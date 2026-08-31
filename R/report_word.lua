-- Convert the completed HTML report to editable Word without recomputing it.
-- Browser controls are omitted; all result rows, figures and AI labels remain.
local function wrap_cell(cell)
  cell.contents = {pandoc.Div(cell.contents, pandoc.Attr('', {}, {['custom-style']='Table Text'}))}
  return cell
end

local function make_table(simple, indices, caption)
  local heads, widths, aligns, rows = {}, {}, {}, {}
  for _,i in ipairs(indices) do
    heads[#heads+1] = simple.headers[i]
    widths[#widths+1] = 1 / #indices
    aligns[#aligns+1] = simple.aligns[i]
  end
  for _,row in ipairs(simple.rows) do
    local selected = {}
    for _,i in ipairs(indices) do selected[#selected+1] = row[i] end
    rows[#rows+1] = selected
  end
  local t = pandoc.utils.from_simple_table(pandoc.SimpleTable(caption, aligns, widths, heads, rows))
  t.attributes['custom-style'] = 'Table'
  for _,row in ipairs(t.head.rows) do for _,cell in ipairs(row.cells) do wrap_cell(cell) end end
  for _,body in ipairs(t.bodies) do for _,row in ipairs(body.body) do for _,cell in ipairs(row.cells) do wrap_cell(cell) end end end
  return t
end

function Div(el)
  if el.identifier == 'header' or el.identifier == 'TOC' then return {} end
  return el.content
end

function Image(el)
  -- Use intrinsic aspect ratio. Width fits portrait pages and height stays below
  -- the printable area even for a tall source image.
  -- mediabag.fetch returns mime type then bytes; image sizing is also handled
  -- by Word's page limits when no dimensions can be read.
  local success, mime, data = pcall(pandoc.mediabag.fetch, el.src)
  local width = 6.5
  if success and data and pandoc.image and pandoc.image.size then
    local sized, size = pcall(pandoc.image.size, data)
    if sized and size.width and size.height and size.height > 0 then
      width = math.min(width, 7.0 * size.width / size.height)
    end
  end
  el.attributes.width = string.format('%.3fin', width)
  el.attributes.height = nil
  el.attributes.style = nil
  return el
end

function Table(el)
  -- R Markdown's report tables have single-level headers and no merged cells.
  -- Wide single-record diagnostics read better as Field/Value tables. Other
  -- wide tables are split into panels with their first two columns repeated.
  local s = pandoc.utils.to_simple_table(el)
  local columns = #s.aligns
  if columns > 8 and #s.rows == 1 then
    local rows = {}
    for i=1,columns do rows[#rows+1] = {s.headers[i], s.rows[1][i]} end
    s = pandoc.SimpleTable(s.caption, {pandoc.AlignLeft,pandoc.AlignLeft}, {.38,.62},
      {{pandoc.Plain('Field')},{pandoc.Plain('Value')}}, rows)
    local result = make_table(s,{1,2},s.caption)
    result.colspecs = {{pandoc.AlignLeft,.38},{pandoc.AlignLeft,.62}}
    return result
  end
  if columns <= 8 then
    local indices={};for i=1,columns do indices[#indices+1]=i end
    local result=make_table(s,indices,s.caption)
    if columns==2 then result.colspecs={{pandoc.AlignLeft,.29},{pandoc.AlignLeft,.71}} end
    return result
  end
  local result = {}
  local panels = math.ceil((columns-2)/6)
  for start=3,columns,6 do
    local indices={1,2}
    for i=start,math.min(start+5,columns) do indices[#indices+1]=i end
    local caption=pandoc.Inlines(s.caption)
    caption:extend(pandoc.Inlines(string.format(' (panel %d of %d; first two columns repeated)', math.floor((start-3)/6)+1, panels)))
    result[#result+1]=make_table(s,indices,caption)
  end
  return result
end

function Para(el)
  -- knitr can place adjacent figures in one HTML paragraph. Separate them so
  -- Word can paginate each figure independently instead of reserving two pages.
  local count=0
  for _,inline in ipairs(el.content) do if inline.t=='Image' then count=count+1 end end
  if count<2 then return nil end
  local result, pending={},{}
  for _,inline in ipairs(el.content) do
    if inline.t=='Image' then
      if #pending>0 then result[#result+1]=pandoc.Para(pending);pending={} end
      result[#result+1]=pandoc.Para({inline})
    elseif inline.t~='SoftBreak' and inline.t~='Space' then pending[#pending+1]=inline end
  end
  if #pending>0 then result[#result+1]=pandoc.Para(pending) end
  return result
end

function CodeBlock(el)
  -- Long session-info lines wrap in Word while preserving their full text.
  local lines={}
  for line in (el.text..'\n'):gmatch('(.-)\n') do
    while #line>95 do
      local cut=line:sub(1,95):match('.*()%s') or 95
      lines[#lines+1]=line:sub(1,cut);line=line:sub(cut+1)
    end
    lines[#lines+1]=line
  end
  el.text=table.concat(lines,'\n')
  return el
end

function Pandoc(doc)
  doc.meta.generator=nil;doc.meta.viewport=nil
  return doc
end
