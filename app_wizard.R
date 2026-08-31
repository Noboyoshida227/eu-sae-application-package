# =====================================================================
#  app_wizard.R -- Step-by-step ("wizard") front end for the EU SAE
#                  Poverty Mapping dashboard.
#
#  WHAT THIS IS
#  ------------
#  The classic dashboard (app.R) presents every setting at once in a
#  single long sidebar. This file presents exactly the same settings
#  as six sequential steps:
#
#      1. Data  ->  2. Mapping  ->  3. Indicator
#                ->  4. Models  ->  5. AI Assistant  ->  6. Review & Run
#
#  HOW IT REUSES app.R
#  -------------------
#  This file does NOT duplicate the pipeline. At launch it parses app.R,
#  evaluates every top-level definition EXCEPT the old `ui` object and
#  the launcher block, and then reuses app.R's `server()` function
#  verbatim. Every input ID is preserved, so all of app.R's server logic
#  works unchanged. Fixes made to app.R are inherited automatically.
#
#  KEY DESIGN CONSTRAINT
#  ---------------------
#  The steps are built with tabsetPanel(type = "hidden"). Hidden tab
#  panels remain in the DOM, so an input on step 2 still exists and holds
#  its value while the user is looking at step 5. Rebuilding steps with
#  renderUI/uiOutput would set every off-screen input to NULL and
#  silently break the pipeline. Do not change this.
#
#  GATING
#  ------
#  Gating is deliberately "soft": Next and the breadcrumb are always
#  clickable. Incomplete steps show an amber notice and an amber chip.
#  Only "Run Analysis" on step 6 is genuinely blocked.
#
#  LAUNCH
#  ------
#      shiny::runApp("app_wizard.R", launch.browser = TRUE)
#  or double-click Start_Here/Start_Wizard.bat
# =====================================================================

library(shiny)

# ---------------------------------------------------------------------
# 1. Load app.R's globals and server() without launching it
# ---------------------------------------------------------------------

# app.R self-launches when run non-interactively. This guard stops that.
Sys.setenv(EU_SAE_APP_LAUNCHED = "1")

.wiz_app_dir <- tryCatch(
  {
    ofiles <- vapply(
      sys.frames(),
      function(env) {
        of <- env$ofile
        if (is.null(of) || length(of) == 0) "" else as.character(of)[1]
      },
      character(1)
    )
    ofiles <- ofiles[nzchar(ofiles)]
    if (length(ofiles) > 0) {
      dirname(normalizePath(tail(ofiles, 1), winslash = "/", mustWork = TRUE))
    } else {
      normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    }
  },
  error = function(e) normalizePath(getwd(), winslash = "/", mustWork = FALSE)
)

.wiz_core <- file.path(.wiz_app_dir, "app.R")
if (!file.exists(.wiz_core)) {
  stop(
    "app_wizard.R must sit in the same folder as app.R. Looked for: ",
    .wiz_core,
    call. = FALSE
  )
}

# Parse app.R and evaluate everything except the pieces that build or
# launch the classic single-page UI.
.wiz_exprs <- parse(.wiz_core, keep.source = FALSE)

.wiz_should_keep <- function(e) {
  if (!is.call(e)) return(TRUE)
  head_sym <- as.character(e[[1]])[1]

  # Drop `ui <- ...`, `.app <- shinyApp(...)`, and the launcher helper.
  if (head_sym %in% c("<-", "=", "<<-") && length(e) >= 2) {
    target <- tryCatch(as.character(e[[2]])[1], error = function(err) "")
    if (target %in% c("ui", ".app", ".run_app_with_port_fallback")) {
      return(FALSE)
    }
  }

  # Drop the trailing auto-launch block.
  if (identical(head_sym, "if")) {
    txt <- paste(deparse(e), collapse = " ")
    if (grepl("EU_SAE_APP_LAUNCHED", txt, fixed = TRUE)) return(FALSE)
  }

  TRUE
}

.wiz_kept <- vapply(as.list(.wiz_exprs), .wiz_should_keep, logical(1))
for (.e in as.list(.wiz_exprs)[.wiz_kept]) {
  eval(.e, envir = globalenv())
}
rm(.e)

if (!exists("server", mode = "function")) {
  stop("Could not load server() from app.R.", call. = FALSE)
}
.wiz_core_server <- get("server", mode = "function")

# Serve www/ under an explicit prefix so static assets resolve however
# the app is launched (runApp on a file, on a directory, or via a
# shinyApp object).
shiny::addResourcePath("wizassets", file.path(.wiz_app_dir, "www"))

# ---------------------------------------------------------------------
# 2. Wizard step definitions
# ---------------------------------------------------------------------

WIZ_STEPS <- list(
  list(id = "data",      num = 1L, short = "Data",         title = "Step 1 - Data inputs"),
  list(id = "mapping",   num = 2L, short = "Mapping",      title = "Step 2 - Variable mapping"),
  list(id = "indicator", num = 3L, short = "Indicator",    title = "Step 3 - Indicator definition"),
  list(id = "models",    num = 4L, short = "Models",       title = "Step 4 - Model options"),
  list(id = "ai",        num = 5L, short = "AI Assistant", title = "Step 5 - AI Assistant (optional)"),
  list(id = "run",       num = 6L, short = "Review & Run", title = "Step 6 - Review & run")
)
WIZ_IDS <- vapply(WIZ_STEPS, function(s) s$id, character(1))

# Small helpers used only by the wizard chrome -------------------------

wiz_step_header <- function(step) {
  tags$div(
    class = "wiz-step-header",
    tags$div(class = "wiz-step-num", sprintf("%d", step$num)),
    tags$div(
      tags$h3(step$title),
      tags$div(class = "wiz-step-sub", uiOutput(paste0("wiz_sub_", step$id), inline = TRUE))
    )
  )
}

wiz_nav <- function(step_id) {
  idx <- match(step_id, WIZ_IDS)
  tags$div(
    class = "wiz-nav",
    if (idx > 1) {
      actionButton(paste0("wiz_back_", step_id), "< Back", class = "btn-default")
    } else {
      tags$span()
    },
    tags$div(class = "wiz-nav-spacer"),
    if (idx < length(WIZ_IDS)) {
      actionButton(paste0("wiz_next_", step_id), "Next >", class = "btn-primary")
    } else {
      tags$span()
    }
  )
}

# A step body: header, amber issue notice, content, nav bar.
wiz_panel <- function(step_id, ...) {
  step <- WIZ_STEPS[[match(step_id, WIZ_IDS)]]
  tabPanel(
    step_id,
    tags$div(
      class = "wiz-step",
      wiz_step_header(step),
      uiOutput(paste0("wiz_issues_", step_id)),
      tags$div(class = "wiz-step-body", ...),
      wiz_nav(step_id)
    )
  )
}

# ---------------------------------------------------------------------
# 3. Shared head: JS handlers + CSS lifted verbatim from app.R, with
#    the wizard chrome appended to the same style block, plus a few
#    extra JS message handlers used only by the wizard.
# ---------------------------------------------------------------------

wiz_head <- tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('fadeTransition', function(msg) {
        var hide = document.getElementById(msg.hide);
        var show = document.getElementById(msg.show);
        hide.style.transition = 'opacity 0.45s ease';
        hide.style.opacity = '0';
        setTimeout(function(){
          hide.style.display = 'none';
          hide.style.opacity = '1';
          show.style.opacity = '0';
          show.style.display = (show.id === 'main_app') ? 'block' : 'flex';
          show.style.transition = 'opacity 0.45s ease';
          setTimeout(function(){ show.style.opacity = '1'; }, 30);
        }, 450);
      });
      Shiny.addCustomMessageHandler('mappingValidity', function(msg) {
        var group = $('#' + msg.id).closest('.form-group');
        if (!group.length) return;
        if (msg.invalid) {
          group.addClass('mapping-invalid');
          group.attr('title', msg.message || 'Column not found in selected dataset');
        } else {
          group.removeClass('mapping-invalid');
          group.removeAttr('title');
        }
      });
      Shiny.addCustomMessageHandler('clearFileInput', function(id) {
        var input = document.getElementById(id);
        if (input) {
          input.value = '';
          $(input).trigger('change');
        }
        var group = $('#' + id).closest('.form-group');
        group.find('.file-caption-name').attr('title', '').text('No file selected');
        group.find('input[type=text]').val('');
      });
    ")),
    tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
    #cover_page {
      position: fixed; top: 0; left: 0; width: 100%; height: 100%;
      background: linear-gradient(160deg, #0f1b3d 0%, #1a2f6b 35%, #1e4d8f 65%, #2a6cb0 100%);
      z-index: 9999; display: flex; flex-direction: column;
      align-items: center; justify-content: center;
      font-family: 'Inter', 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      color: #ffffff; text-align: center;
      animation: fadeIn 1s ease-out;
      overflow: hidden;
    }
    #cover_page::before {
      content: ''; position: absolute; top: -50%; left: -50%;
      width: 200%; height: 200%;
      background: radial-gradient(ellipse at 30% 20%, rgba(109,213,237,0.08) 0%, transparent 50%),
                  radial-gradient(ellipse at 70% 80%, rgba(59,130,200,0.06) 0%, transparent 50%);
      pointer-events: none;
    }
    @keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }
    #cover_page .cover-content {
      position: relative; z-index: 1;
      display: flex; flex-direction: column; align-items: center;
      max-width: 780px; padding: 0 24px;
    }
    #cover_page .cover-illustration {
      margin-bottom: 1.4em;
    }
    #cover_page .cover-illustration img {
      border-radius: 10px;
      box-shadow: 0 12px 48px rgba(0,0,0,0.35), 0 2px 12px rgba(0,0,0,0.2);
      border: 1px solid rgba(255,255,255,0.08);
      max-height: 58vh;
      width: auto;
      object-fit: contain;
    }
    #cover_page .cover-label {
      font-size: 0.78em; font-weight: 600; letter-spacing: 3px;
      text-transform: uppercase; color: rgba(157,213,245,0.85);
      margin-bottom: 0.5em;
    }
    #cover_page h1 {
      font-size: 2.6em; font-weight: 700; margin: 0 0 0.2em;
      letter-spacing: -0.5px; line-height: 1.15;
      background: linear-gradient(180deg, #ffffff 30%, rgba(200,225,255,0.85) 100%);
      -webkit-background-clip: text; -webkit-text-fill-color: transparent;
      background-clip: text;
    }
    #cover_page .subtitle {
      font-size: 1.05em; font-weight: 300; color: rgba(220,235,255,0.85);
      max-width: 560px; line-height: 1.55; margin-bottom: 0.3em;
    }
    #cover_page .cover-divider {
      width: 60px; height: 2px; margin: 0.7em auto 0.8em;
      background: linear-gradient(90deg, transparent, rgba(109,213,237,0.5), transparent);
      border: none;
    }
    #cover_page .tagline {
      font-size: 0.85em; font-weight: 400; color: rgba(180,210,240,0.65);
      letter-spacing: 0.5px; margin-bottom: 1.8em;
    }
    #enter_app_btn {
      font-size: 1em; font-weight: 500; padding: 14px 52px;
      background: linear-gradient(135deg, rgba(109,213,237,0.2), rgba(59,123,213,0.25));
      color: #fff; letter-spacing: 0.8px;
      border: 1.5px solid rgba(109,213,237,0.4);
      border-radius: 50px; cursor: pointer;
      transition: all 0.35s ease;
      backdrop-filter: blur(8px);
      box-shadow: 0 2px 16px rgba(0,0,0,0.15);
    }
    #enter_app_btn:hover {
      background: linear-gradient(135deg, rgba(109,213,237,0.35), rgba(59,123,213,0.4));
      border-color: rgba(109,213,237,0.7);
      transform: translateY(-2px);
      box-shadow: 0 6px 28px rgba(109,213,237,0.2);
    }
    #cover_page .cover-footer {
      margin-top: 1.8em;
      text-align: center; font-size: 0.75em; font-weight: 400;
      color: rgba(180,210,240,0.35); letter-spacing: 0.3px;
    }
    #main_app { display: none; }
    #main_app.visible { display: block; }

    /* ---- Tooltips ---- */
    .tt-wrap {
      position: relative;
      display: inline;
    }
    .tt-wrap .tt-icon {
      display: inline-block;
      width: 15px; height: 15px;
      line-height: 15px;
      text-align: center;
      font-size: 10px; font-weight: 700;
      color: #fff;
      background: #6b8db5;
      border-radius: 50%;
      cursor: help;
      margin-left: 3px;
      vertical-align: middle;
    }
    .tt-wrap .tt-text {
      visibility: hidden;
      opacity: 0;
      position: absolute;
      z-index: 1000;
      left: 0; top: 1.6em;
      width: 300px;
      background: #1a2a4a;
      color: #e0ecf8;
      padding: 10px 14px;
      border-radius: 8px;
      font-size: 0.85em;
      font-weight: 400;
      line-height: 1.5;
      box-shadow: 0 4px 20px rgba(0,0,0,0.3);
      border: 1px solid rgba(109,213,237,0.25);
      transition: opacity 0.2s ease, visibility 0.2s ease;
      pointer-events: none;
    }
    .tt-wrap:hover .tt-text {
      visibility: visible;
      opacity: 1;
    }
    .form-group.mapping-invalid label {
      color: #b00020;
    }
    .form-group.mapping-invalid .selectize-input {
      border-color: #b00020 !important;
      box-shadow: 0 0 0 2px rgba(176,0,32,0.12) !important;
    }
    .form-group.mapping-invalid .selectize-input.focus {
      border-color: #b00020 !important;
      box-shadow: 0 0 0 3px rgba(176,0,32,0.18) !important;
    }
    .mapping-validation-summary {
      color: #8a1f11;
      background: #fff3f1;
      border: 1px solid #f1b8b0;
      border-radius: 6px;
      padding: 8px 10px;
      font-size: 12px;
      margin: 4px 0 12px 0;
    }
    .benchmark-format-note {
      color: #445;
      background: #f7f9fc;
      border: 1px solid #d9e2ef;
      border-radius: 6px;
      padding: 10px 12px;
      font-size: 12px;
      line-height: 1.45;
      margin: -4px 0 12px 0;
    }
    .benchmark-format-note .note-title {
      font-weight: 600;
      color: #24324a;
      margin-bottom: 4px;
    }
    .benchmark-format-note p {
      margin: 0 0 6px 0;
    }
    .benchmark-format-note ul {
      margin: 0;
      padding-left: 18px;
    }
    .benchmark-clear-row {
      margin: 2px 0 8px 0;
    }

    /* ================= Wizard chrome ================= */
    /* Shiny hides hidden-type tab navs itself; belt and braces. */
    .nav-hidden { display: none !important; }
    body { background: #f4f6fa; }
    #main_app .container-fluid { max-width: 1180px; }

    .wiz-topbar {
      display: flex; align-items: center; justify-content: space-between;
      flex-wrap: wrap; gap: 10px;
      padding: 12px 18px; margin: 0 0 14px 0;
      background: linear-gradient(135deg, #14264d 0%, #1e4d8f 100%);
      border-radius: 10px; color: #eaf2fb;
      box-shadow: 0 2px 10px rgba(20,38,77,0.18);
    }
    .wiz-topbar .wiz-title {
      font-size: 1.15em; font-weight: 600; letter-spacing: 0.2px;
    }
    .wiz-topbar .wiz-title small {
      display: block; font-size: 0.62em; font-weight: 400;
      color: rgba(200,222,247,0.75); letter-spacing: 0.4px;
    }
    .wiz-topbar .btn { margin-left: 6px; }

    /* ---- Breadcrumb ---- */
    .wiz-crumbs {
      display: flex; flex-wrap: wrap; gap: 8px;
      margin: 0 0 18px 0;
    }
    .wiz-crumb {
      display: inline-flex; align-items: center; gap: 7px;
      padding: 7px 14px 7px 9px;
      border-radius: 40px; border: 1px solid #d5dde8;
      background: #fff; color: #46536b;
      font-size: 0.85em; font-weight: 500;
      text-decoration: none !important; cursor: pointer;
      transition: all 0.18s ease;
      box-shadow: 0 1px 2px rgba(20,38,77,0.05);
    }
    .wiz-crumb:hover {
      border-color: #9db8d8; color: #1e4d8f;
      transform: translateY(-1px);
      box-shadow: 0 3px 8px rgba(20,38,77,0.1);
    }
    .wiz-crumb .wiz-dot {
      display: inline-flex; align-items: center; justify-content: center;
      width: 21px; height: 21px; border-radius: 50%;
      font-size: 0.78em; font-weight: 700; color: #fff;
      background: #b9c4d4; flex: 0 0 21px;
    }
    .wiz-crumb.done { border-color: #b6dfc0; background: #f2fbf4; color: #22683a; }
    .wiz-crumb.done .wiz-dot { background: #2e7d32; }
    .wiz-crumb.todo { border-color: #f0d8a8; background: #fffaf0; color: #8a5a00; }
    .wiz-crumb.todo .wiz-dot { background: #d99b16; }
    .wiz-crumb.current {
      border-color: #1e4d8f; background: #1e4d8f; color: #fff;
      box-shadow: 0 3px 10px rgba(30,77,143,0.28);
    }
    .wiz-crumb.current .wiz-dot { background: rgba(255,255,255,0.28); color: #fff; }

    /* ---- Step card ---- */
    .wiz-step {
      background: #fff; border: 1px solid #e2e8f1; border-radius: 12px;
      padding: 22px 26px 18px; margin-bottom: 18px;
      box-shadow: 0 2px 14px rgba(20,38,77,0.06);
    }
    .wiz-step-header { display: flex; align-items: flex-start; gap: 14px; margin-bottom: 6px; }
    .wiz-step-header h3 { margin: 0 0 2px 0; font-size: 1.28em; font-weight: 600; color: #16233d; }
    .wiz-step-num {
      flex: 0 0 38px; width: 38px; height: 38px; border-radius: 50%;
      background: linear-gradient(135deg, #1e4d8f, #2a6cb0); color: #fff;
      display: flex; align-items: center; justify-content: center;
      font-size: 1.05em; font-weight: 700;
    }
    .wiz-step-sub { font-size: 0.87em; color: #61708a; line-height: 1.5; }
    .wiz-step-body { margin-top: 16px; }
    .wiz-step-body hr { margin: 18px 0; border-top: 1px solid #eceff4; }
    .wiz-step-body h4 {
      font-size: 0.82em; font-weight: 700; letter-spacing: 1.1px;
      text-transform: uppercase; color: #7c8aa3;
      margin: 22px 0 10px 0;
    }
    .wiz-step-body h4:first-child { margin-top: 0; }

    /* Two-column field grid; collapses on narrow screens */
    .wiz-grid { display: flex; flex-wrap: wrap; gap: 0 28px; }
    .wiz-grid > .wiz-col { flex: 1 1 320px; min-width: 280px; }

    .wiz-note {
      font-size: 12px; color: #55617a; line-height: 1.5;
      background: #f7f9fc; border: 1px solid #e3e9f2;
      border-left: 3px solid #9db8d8;
      border-radius: 6px; padding: 9px 12px; margin: 0 0 14px 0;
    }

    /* ---- Amber issue notice ---- */
    .wiz-issues {
      background: #fffaf0; border: 1px solid #f0d8a8;
      border-left: 3px solid #d99b16;
      border-radius: 6px; padding: 10px 13px; margin: 12px 0 0 0;
      font-size: 0.86em; color: #7d5300;
    }
    .wiz-issues .wiz-issues-title { font-weight: 700; margin-bottom: 4px; }
    .wiz-issues ul { margin: 0; padding-left: 20px; }
    .wiz-issues li { margin: 2px 0; }
    .wiz-ok {
      background: #f2fbf4; border: 1px solid #b6dfc0;
      border-left: 3px solid #2e7d32;
      border-radius: 6px; padding: 9px 13px; margin: 12px 0 0 0;
      font-size: 0.86em; color: #22683a; font-weight: 500;
    }

    /* ---- Nav bar ---- */
    .wiz-nav {
      display: flex; align-items: center;
      margin-top: 22px; padding-top: 16px;
      border-top: 1px solid #eceff4;
    }
    .wiz-nav-spacer { flex: 1 1 auto; }
    .wiz-nav .btn { min-width: 116px; font-weight: 500; }

    /* ---- Step 6 summary table ---- */
    .wiz-summary { width: 100%; font-size: 0.85em; border-collapse: collapse; }
    .wiz-summary th, .wiz-summary td {
      padding: 6px 10px; border-bottom: 1px solid #eef1f6; vertical-align: top;
    }
    .wiz-summary th {
      text-align: left; font-weight: 600; color: #5c6a83;
      width: 42%; white-space: nowrap;
    }
    .wiz-summary td { color: #22314d; }
    .wiz-summary .wiz-sec td {
      padding-top: 14px; font-weight: 700; color: #16233d;
      text-transform: uppercase; font-size: 0.9em; letter-spacing: 0.8px;
      border-bottom: 1px solid #d8dfea;
    }
    .wiz-run-row { display: flex; gap: 10px; flex-wrap: wrap; margin: 6px 0 2px; }
    .wiz-run-row .btn { min-width: 190px; }
  ")),
  tags$script(HTML("
    // ---- Wizard chrome handlers (additional to app.R's) ----
    Shiny.addCustomMessageHandler('wizCrumbState', function(msg) {
      for (var i = 0; i < msg.ids.length; i++) {
        var el = document.getElementById(msg.ids[i]);
        if (!el) continue;
        el.classList.remove('done', 'todo', 'current');
        el.classList.add(msg.states[i]);
      }
    });
    Shiny.addCustomMessageHandler('wizToggleBtn', function(msg) {
      var b = document.getElementById(msg.id);
      if (!b) return;
      if (msg.disabled) {
        b.setAttribute('disabled', 'disabled');
        b.classList.add('disabled');
      } else {
        b.removeAttribute('disabled');
        b.classList.remove('disabled');
      }
    });
    Shiny.addCustomMessageHandler('wizScrollTop', function(msg) {
      window.scrollTo({ top: 0, behavior: 'smooth' });
    });
  "))
)

# ---------------------------------------------------------------------
# 4. Wizard UI
#
#    Every widget below is lifted verbatim from app.R's sidebarPanel --
#    same input IDs, same labels, same tooltips, same conditionalPanel
#    conditions -- only regrouped into six steps and laid out in two
#    columns instead of a narrow sidebar.
# ---------------------------------------------------------------------

ui <- fluidPage(
  wiz_head,

  # ---- Cover page (unchanged from app.R) ----
  div(id = "cover_page",
    div(class = "cover-content",
      div(class = "cover-illustration",
        tags$img(src = "wizassets/eu_poverty_map.png",
                 style = "width: 90%; max-width: 680px;")
      ),
      div(class = "cover-label", "Small Area Estimation Platform"),
      h1("EU Poverty Mapping"),
      div(class = "subtitle",
        "Poverty rate estimation across NUTS-3 areas using Fay\u2013Herriot models with benchmarking and AI-assisted diagnostics"
      ),
      tags$hr(class = "cover-divider"),
      div(class = "tagline",
        "Guided setup in six steps  \u00b7  Univariate & Multivariate FH  \u00b7  Benchmarked Estimates"
      ),
      actionButton("enter_app_btn", "Get Started", class = "btn"),
      div(class = "cover-footer",
        "World Bank Group"
      )
    )
  ),

  # ---- Main app ----
  div(id = "main_app",

    # Top bar -------------------------------------------------------
    tags$div(class = "wiz-topbar",
      tags$div(class = "wiz-title",
        "EU Poverty Mapping",
        tags$small("Guided setup \u00b7 six steps")
      ),
      tags$div(
        actionButton("wiz_jump_run", "Skip to Review & Run", class = "btn-default btn-sm")
      )
    ),

    # Saved setup ---------------------------------------------------
    tags$div(class = "wiz-step", style = "padding: 14px 20px 10px;",
      h4("Saved setup"),
      tags$div(
        style = "font-size: 12px; color: #556; margin: -4px 0 10px 0;",
        "Save or reload dashboard settings so later runs only require small edits. If files were selected with Browse, the app saves local setup copies under app_runs/_last_setup_files for the next session."
      ),
      div(
        style = "display: flex; gap: 6px; flex-wrap: wrap; margin-bottom: 8px;",
        actionButton("load_setup_btn", "Load Last Setup", class = "btn-default btn-sm"),
        actionButton("save_setup_btn", "Save Current Setup", class = "btn-default btn-sm"),
        actionButton("reset_setup_btn", "Reset to Defaults", class = "btn-warning btn-sm")
      ),
      uiOutput("saved_setup_status")
    ),

    # Breadcrumb ----------------------------------------------------
    # Built statically (not via renderUI) so the actionLink input
    # bindings are never torn down and re-created -- rebinding would
    # reset their click counters and fire spurious navigation. Only the
    # CSS classes are updated, via the wizCrumbState message handler.
    tags$div(class = "wiz-crumbs",
      lapply(WIZ_STEPS, function(s) {
        actionLink(
          paste0("wiz_crumb_", s$id),
          label = tagList(tags$span(class = "wiz-dot", as.character(s$num)), s$short),
          class = "wiz-crumb"
        )
      })
    ),

    # Steps ---------------------------------------------------------
    tabsetPanel(
      id = "wizard", type = "hidden",

      # ============================ STEP 1 ============================
      wiz_panel("data",
        tags$div(class = "wiz-note",
          "Start by telling the app which years to analyse and where the input data lives. ",
          "Survey, auxiliary and geometry files are all required; the population file is optional."
        ),
        h4("Analysis settings"),
        tags$div(class = "wiz-grid",
          tags$div(class = "wiz-col",
            textInput("years",
              tip_label("Analysis years (two, comma-separated)", "Enter exactly two years separated by a comma. The pipeline estimates poverty for each year and tests for significant changes between them."),
              value = "2012,2013"),
            textInput("country_name",
              tip_label("Country or territory", "Used only in report and brief titles; it does not affect estimation."),
              value = ""),
            numericInput("analysis_seed",
              tip_label("Analysis seed", "Controls LASSO folds and bootstrap draws so identical inputs and settings reproduce the same stochastic analysis."),
              value = 123, min = 0, step = 1),
            textInput("run_label",
              tip_label("Run label", "Optional short label appended to the saved app_runs folder so outputs from different runs are easier to distinguish."),
              value = "")
          ),
          tags$div(class = "wiz-col",
            tags$div(class = "wiz-note", style = "margin-top: 26px;",
              tags$strong("Two years are required."),
              " The pipeline estimates the indicator separately for each year and then tests whether the change between them is statistically significant."
            )
          )
        ),
        h4("Data inputs"),
        tags$div(
          style = "font-size: 12px; color: #556; margin: -4px 0 10px 0;",
          "Use Browse to select the required input files from any folder on your computer: household survey, auxiliary covariates, and shapefiles/geometries. Survey and auxiliary files can be R, CSV/TSV/DAT, Stata, SPSS, SAS, Python Parquet/Feather, or Excel files. For ESRI shapefiles, upload one .zip containing the shapefile components, especially .shp, .shx, and .dbf; the .dbf usually contains the domain ID needed for maps. Geometry files can also be .rds, GeoPackage, or GeoJSON. If a saved setup is loaded, its active files are shown below each Browse button; browse again only when changing a file."
        )
        ,
        tags$div(class = "wiz-grid",
          tags$div(class = "wiz-col",
            fileInput("survey_file",
              tip_label("Browse Survey data file", "Choose the household survey file from any folder on your computer. Accepted formats: .rds, .RData/.rda, .csv, .tsv, .txt, .dat, .dta, .sav, .zsav, .por, .sas7bdat, .xpt, .parquet, .feather, .xlsx, .xls."),
              accept = c(".rds", ".RData", ".rda", ".csv", ".tsv", ".txt", ".dat",
                         ".dta", ".sav", ".zsav", ".por", ".sas7bdat", ".xpt",
                         ".parquet", ".feather", ".xlsx", ".xls")),
            uiOutput("survey_active_file"),
            fileInput("rhs_file",
              tip_label("Browse Auxiliary data file", "Choose the auxiliary covariates file from any folder on your computer. Accepted formats: .rds, .RData/.rda, .csv, .tsv, .txt, .dat, .dta, .sav, .zsav, .por, .sas7bdat, .xpt, .parquet, .feather, .xlsx, .xls."),
              accept = c(".rds", ".RData", ".rda", ".csv", ".tsv", ".txt", ".dat",
                         ".dta", ".sav", ".zsav", ".por", ".sas7bdat", ".xpt",
                         ".parquet", ".feather", ".xlsx", ".xls")),
            uiOutput("rhs_active_file")
          ),
          tags$div(class = "wiz-col",
            fileInput("shp_file",
              tip_label("Browse Shapefile/geometries file", "Choose the geometry file from any folder on your computer. For ESRI shapefiles, upload a single .zip containing all shapefile components, especially .shp, .shx, and .dbf. A .shp file alone is usually not enough because the .dbf stores the domain ID used to join estimates to map polygons. Other accepted formats: .rds, .RData/.rda, .gpkg, .geojson, .json, .kml, .gml."),
              accept = c(".rds", ".RData", ".rda", ".zip", ".gpkg", ".geojson", ".json", ".kml", ".gml")),
            uiOutput("shp_active_file")
            ,
            fileInput("population_file",
              tip_label("Domain population sizes (optional)",
                        "Optional RDS/CSV/XLSX file with domain population sizes. Supports long domain-year-population format or wide domain-by-year format; leave blank to estimate domain populations from the survey as sum(weight * household size).")),
            uiOutput("population_active_file")
          )
        )
      ),

      # ============================ STEP 2 ============================
      wiz_panel("mapping",
        tags$div(
          style = "font-size: 12px; color: #556; margin: -4px 0 10px 0;",
          "Search the dropdown list of columns loaded from the selected dataset, or type a column name directly. A red field means the typed name is not currently found in the relevant dataset."
        ),
        uiOutput("mapping_validation_summary")
        ,
        tags$div(class = "wiz-grid",
          tags$div(class = "wiz-col",
            h4("Survey columns"),
            mapping_selectize("var_year",
              tip_label("year", "Column name in the survey data that identifies the year. Search the survey columns or type the column name."),
              "year"),
            mapping_selectize("var_domain",
              tip_label("domain", "Column name in the survey data that identifies the small area domain (e.g. NUTS-3 province). Search the survey columns or type the column name."),
              "prov"),
            mapping_selectize("var_psu",
              tip_label("psu", "Column name for the Primary Sampling Unit. Used to compute design-based sampling variances. Search the survey columns or type the column name."),
              "ea_id"),
            mapping_selectize("var_weight",
              tip_label("weight", "Column name for the survey sampling weight. Search the survey columns or type the column name."),
              "weight"),
            mapping_selectize("var_strata",
              tip_label("strata", "Optional strata ID column for survey-design variance estimation. Search the survey columns or type the column name. Leave blank if the survey design has no strata or strata are unavailable."),
              "")
          ),
          tags$div(class = "wiz-col",
            h4("Survey columns (cont.)"),
            mapping_selectize("var_hh_size",
              tip_label("household size", "Column name for household size. Direct poverty-rate estimates use population_weight = weight * household size; when no population file is uploaded, benchmarking also estimates domain populations as sum(weight * household size) by domain and year. Search the survey columns or type the column name."),
              "hhsize"),
            mapping_selectize("var_welfare",
              tip_label("welfare", "Column name for the welfare variable (e.g. income or consumption) used to determine poverty status. Search the survey columns or type the column name."),
              "income")
            ,
            h4("Join keys"),
            mapping_selectize("rhs_domain",
              tip_label("Auxiliary covariates domain field", "Column name in the auxiliary covariates file that identifies the domain. Search the auxiliary columns or type the column name. Used to join covariates to survey data."),
              "prov"),
            mapping_selectize("shp_domain",
              tip_label("Shapefile domain field", "Column name in the shapefiles/geometries file that identifies the domain. Search the geometry columns or type the column name. Used to join estimates to map polygons."),
              "prov")
          )
        )
      ),

      # ============================ STEP 3 ============================
      wiz_panel("indicator",
        tags$div(class = "wiz-note",
          "Define what is being modelled. The options below change depending on whether you choose a poverty measure or mean welfare."
        ),
        tags$div(class = "wiz-grid",
          tags$div(class = "wiz-col",
            # ---- Indicator type ----
            # Top-level choice. Switching to "mean_welfare" hides the
            # poverty-line / FGT inputs (they're irrelevant) and exposes
            # an optional log-transform.
            selectInput("indicator_type",
              tip_label("Indicator",
                        "What is being modelled. 'Poverty (FGT)' uses welfare + a poverty line to compute headcount, gap, or severity. 'Mean welfare' uses the population-weighted mean of the welfare variable directly."),
              choices  = c("Poverty (FGT)" = "poverty",
                           "Mean welfare"  = "mean_welfare"),
              selected = "poverty"),
            
            # ---- Poverty line (only shown for poverty indicator) ----
            conditionalPanel(
              condition = "input.indicator_type == 'poverty'",
              radioButtons("povline_type",
                tip_label("Poverty line source",
                          "Choose whether the poverty line comes from a column in the survey data or from numeric values entered separately for each analysis year."),
                choices  = c("Column in data" = "column", "Numeric value" = "numeric"),
                selected = "column", inline = TRUE),
              conditionalPanel(
                condition = "input.povline_type == 'column' && input.indicator_type == 'poverty'",
                mapping_selectize("var_povline",
                  tip_label("povline", "Column name for the poverty line. Search the survey columns or type the column name."),
                  "povline")
              ),
              conditionalPanel(
                condition = "input.povline_type == 'numeric' && input.indicator_type == 'poverty'",
                uiOutput("povline_numeric_by_year_ui")
              ),
            
              # ---- FGT indicator ----
              selectInput("fgt_alpha",
                tip_label("Poverty measure",
                          "FGT(0) = headcount ratio (share below the line). FGT(1) = poverty gap (average depth of shortfall). FGT(2) = poverty severity (squared gap, emphasises the poorest)."),
                choices = c("FGT(0) \u2013 Headcount ratio" = "0",
                            "FGT(1) \u2013 Poverty gap"      = "1",
                            "FGT(2) \u2013 Poverty severity"  = "2"),
                selected = "0")
            ),
            
            # ---- Mean welfare options (only shown for mean indicator) ----
            # Note: the log/no choice now lives in the UFH "Transformation"
            # dropdown below (which becomes context-aware when the indicator
            # is mean welfare). The standalone "Log-transform welfare"
            # checkbox has been removed in favor of that single source of
            # truth -- it was replicating the same setting in two places.
            conditionalPanel(
              condition = "input.indicator_type == 'mean_welfare'",
              textInput("currency_symbol",
                tip_label("Currency symbol",
                          "Short label appended to axis titles and table headers for mean welfare estimates."),
                value = "EUR")
            )
          ),
          tags$div(class = "wiz-col",
            tags$div(class = "wiz-note", style = "margin-top: 26px;",
              tags$strong("Poverty (FGT)"),
              " needs a welfare variable and a poverty line, and produces the headcount ratio, poverty gap or severity. ",
              tags$strong("Mean welfare"),
              " models the population-weighted mean of the welfare variable directly and hides the poverty-line inputs."
            )
          )
        )
      ),

      # ============================ STEP 4 ============================
      wiz_panel("models",
        tags$div(class = "wiz-note",
          "Every option on this step has a sensible default. Change them only if you have a specific modelling reason \u2014 you can safely continue as-is."
        ),
        h4("Pipeline steps"),
        checkboxGroupInput("steps",
          tip_label("Pipeline steps", "UFH fits a univariate Fay-Herriot model per year. MFH fits multivariate models (MFH1, MFH2, MFH3) that borrow strength across time. Comparison merges both results side by side with maps and precision metrics."),
          choices = c("UFH", "MFH", "Comparison"),
          selected = c("UFH", "MFH", "Comparison")
        )
        ,
        tags$div(class = "wiz-grid",
          tags$div(class = "wiz-col",
            h4("UFH options"),
            # Transformation dropdown: choices are context-aware.
            #   - poverty indicator: arcsin / no
            #   - mean_welfare      : log    / no
            # The actual choice list is updated by an observer on input$indicator_type
            # (see server logic below). We seed it with the poverty defaults here.
            selectInput("ufh_transformation",
              tip_label("Transformation",
                        paste(
                          "Transformation of the direct estimates before model fitting.",
                          "For poverty rates: 'arcsin' constrains estimates to [0,1] and stabilizes variances.",
                          "For mean welfare: 'log' addresses the right-skewness of welfare; the back-transform to currency units is bias-corrected via Duan's smearing (see Bias Correction below).",
                          "'no' fits on the original scale.")),
              choices = c("arcsin", "no"), selected = "arcsin"),
            # Bias correction is meaningful under arcsin AND log. For arcsin, it
            # corrects the non-linearity of arcsine; for log, it corrects the
            # Jensen-inequality bias of exp(eta_hat). The available options
            # depend on the chosen transformation (set dynamically server-side).
            conditionalPanel(
              condition = "input.ufh_transformation == 'arcsin' || input.ufh_transformation == 'log'",
              selectInput("ufh_backtrans",
                tip_label("Bias Correction",
                          paste(
                            "How the model estimates are bias-corrected when back-transforming to the original scale.",
                            "For arcsin: 'bc' integrates sin^2(.) against the predictive density (correct under Gaussianity); 'none' returns the naive sin^2(eta_hat).",
                            "For log: 'bc_sm' applies Duan's smearing estimator -- multiplies exp(eta_hat) by the empirical mean of exp(residuals), which is non-parametric and robust to non-Gaussian residuals; 'none' returns the naive exp(eta_hat) (downward-biased for the mean).")),
                choices = c("bc", "none"), selected = "bc")
            ),
            # Variance smoothing menu is only meaningful when no transformation
            # is used (arcsin/log already stabilize variances).
            conditionalPanel(
              condition = "input.ufh_transformation == 'no'",
            selectInput("ufh_var_choice",
              tip_label("Variance option (UFH)", "Sampling variance input for UFH when no transformation is used. 'sm_out' replaces a direct variance with its smoothed (GVF-based) variance only when the direct variance is missing/non-finite or below 0.001; 'sm_all' replaces all variances; 'direct' retains raw survey variances except safety backfills. When arcsin or log is selected, this choice is ignored because the transformation already stabilizes variances."),
                choices = c("sm_out", "sm_all", "direct"), selected = "sm_out")
            ),
            selectInput("ufh_ic_criterion",
              tip_label("Model selection criterion", "Information criterion used for stepwise covariate selection. BIC penalizes complexity more heavily and tends to select simpler models. AIC favours predictive accuracy."),
              choices = c("AIC", "BIC"), selected = "BIC"),
            checkboxInput("ufh_lasso_enabled",
              tip_label("Use LASSO screening",
                        "Optionally screens numeric covariates with LASSO before the AIC/BIC stepwise stage. The final model is still chosen by stepwise selection."),
              value = FALSE),
            conditionalPanel(
              condition = "input.ufh_lasso_enabled",
              selectInput("ufh_lasso_lambda",
                tip_label("LASSO lambda",
                          "lambda.1se is more conservative; lambda.min may retain more predictors."),
                choices = c("lambda.1se", "lambda.min"), selected = "lambda.1se")
            ),
            textInput("ufh_candidates_y1",
              tip_label("UFH covariates for Year 1 (comma-separated, optional)",
                        "Forces specific covariates into the UFH model for the first analysis year. If left blank, the app selects automatically from auxiliary data."),
              value = ""),
            textInput("ufh_candidates_y2",
              tip_label("UFH covariates for Year 2 (comma-separated, optional)",
                        "Forces specific covariates into the UFH model for the second analysis year. If left blank, the app selects automatically from auxiliary data."),
              value = "")
          ),
          tags$div(class = "wiz-col",
            h4("MFH options"),
            # Transformation choice for MFH. Independent from UFH so the two
            # models can run on different scales if needed (rare, but
            # supported -- e.g. UFH on identity, MFH on log).
            #   - poverty: hidden (MFH never used arcsin; identity scale only)
            #   - mean_welfare: log / no, default log
            # The bias-correction subchoice mirrors the UFH menu but only
            # offers bc_sm (Duan smearing) for log; arcsin is not an MFH
            # option and so 'bc' (integration-based) is not exposed here.
            conditionalPanel(
              condition = "input.indicator_type == 'mean_welfare'",
              selectInput("mfh_transformation",
                tip_label("Transformation (MFH)",
                          paste(
                            "Transformation applied to the MFH model. Independent of the UFH choice above.",
                            "'log' fits MFH on log(welfare) per year, then back-transforms each (domain, year) cell with a per-domain-year smearing factor anchored to the population-weighted arithmetic mean of welfare. MCPE is back-transformed to currency units via a delta-method approximation, so cross-year change analysis stays on the EUR scale.",
                            "'no' fits on the identity scale.")),
                choices = c("log", "no"), selected = "log"),
              conditionalPanel(
                condition = "input.mfh_transformation == 'log'",
                selectInput("mfh_backtrans",
                  tip_label("Bias Correction (MFH)",
                            paste(
                              "Bias correction for the log -> currency back-transform.",
                              "'bc_sm' applies Duan's smearing estimator (multiplies exp(eta_hat) by the empirical mean of exp(residuals)); robust to non-Gaussian residuals.",
                              "'none' returns the naive exp(eta_hat), which is downward-biased for the mean.")),
                  choices = c("bc_sm", "none"), selected = "bc_sm")
              )
            ),
            selectInput("mfh_ic_criterion",
              tip_label("Model selection criterion", "Information criterion used for stepwise covariate selection. AIC favours predictive accuracy; BIC penalizes complexity more and selects sparser models."),
              choices = c("AIC", "BIC"), selected = "AIC"),
            checkboxInput("mfh_lasso_enabled",
              tip_label("Use LASSO screening",
                        "Optionally screens numeric covariates with LASSO before the AIC/BIC stepwise stage. The final model is still chosen by stepwise selection."),
              value = FALSE),
            conditionalPanel(
              condition = "input.mfh_lasso_enabled",
              selectInput("mfh_lasso_lambda",
                tip_label("LASSO lambda",
                          "lambda.1se is more conservative; lambda.min may retain more predictors."),
                choices = c("lambda.1se", "lambda.min"), selected = "lambda.1se")
            ),
            selectInput("mfh_var_choice",
              tip_label("Variance option (MFH)", "Sampling variance input: 'sm_out' replaces a direct variance with its smoothed (GVF-based) variance only when the direct variance is missing/non-finite or below 0.001; 'sm_all' replaces all variances; 'direct' retains raw survey variances except safety backfills. The covariance options below update automatically to match."),
              choices = c("sm_out", "sm_all", "direct"), selected = "sm_out"),
            selectInput("mfh_cov_choice",
              tip_label("Covariance option", "How the covariance of sampling errors over time is estimated. Available options depend on the variance choice above. 'rho_sm_out' multiplies the national average autocorrelation with outlier-smoothed variances (available with sm_out). 'rho_sm_all' multiplies it with fully smoothed variances (available with sm_all). 'rho_dir' multiplies it with direct variances. 'direct' uses the direct variance-covariance matrix. 'zero' assumes no cross-year sampling correlation."),
              choices = c("rho_sm_out", "rho_dir", "direct", "zero"), selected = "rho_sm_out"),
            selectInput("mfh_diag_model",
              tip_label("Selected MFH model", "MFH2 is the package default. If MFH3 is selected, the package follows Molina and Romero: fit MFH3, use MFH2 if MFH3 does not converge or errors, and otherwise use the MFH3 random-effect variance homogeneity test to choose MFH3 or MFH2. MFH1 remains an additional sensitivity model."),
              choices = c(
                "MFH1 (additional sensitivity model)" = "MFH1",
                "MFH2 (default)" = "MFH2",
                "MFH3 candidate (Molina-Romero test and fallback)" = "MFH3"
              ), selected = "MFH2"),
            checkboxInput("fit_mfh3",
              tip_label("Also fit MFH3 for sensitivity", "When MFH1 or MFH2 is selected, this optional checkbox fits MFH3 for comparison diagnostics without changing the selected model. Selecting MFH3 above always fits it and applies the Molina-Romero test and MFH2 fallback."),
              value = FALSE),
            conditionalPanel(
              condition = "input.mfh_diag_model == 'MFH3' || input.fit_mfh3",
              selectInput("mfh_refvar_adjustment",
                tip_label("MFH3 reference-variance test adjustment", "This setting applies only to the pairwise time-variance contrasts returned when MFH3 is fitted. It does not adjust poverty-change tests across geographic domains. Bonferroni is the conservative model-selection default; BH is a less-conservative sensitivity option. With two years there is only one contrast, so both give the same p-value."),
                choices = c("Bonferroni (recommended)" = "bonferroni",
                            "Benjamini-Hochberg (sensitivity)" = "BH"),
                selected = "bonferroni")
            ),
            numericInput("mcpe_bootstrap_replicates",
              tip_label("MCPE bootstrap replicates", "Controls Monte Carlo precision for MFH change inference. 200 is the interactive default; use at least 500 for production after checking Monte Carlo stability."),
              value = 200, min = 50, step = 50),
            textInput("mfh_candidates_y1",
              tip_label("MFH covariates for Year 1 (comma-separated, optional)",
                        "Forces specific covariates into the MFH model for the first analysis year. If left blank, the app selects automatically from auxiliary data."),
              value = ""),
            textInput("mfh_candidates_y2",
              tip_label("MFH covariates for Year 2 (comma-separated, optional)",
                        "Forces specific covariates into the MFH model for the second analysis year. If left blank, the app selects automatically from auxiliary data."),
              value = "")
          )
        ),
        tags$hr(),
        h4("Benchmarking"),
        checkboxInput("do_benchmark",
          tip_label("Apply benchmarking", "If checked, UFH and MFH estimates are benchmarked. If unchecked, uploaded benchmark files and benchmark-level mappings are ignored for the next run."),
          value = FALSE),
        conditionalPanel(
          condition = "input.do_benchmark",
          radioButtons(
            "benchmark_level",
            tip_label(
              "Benchmarking level",
              "National benchmarking makes the population-weighted average of the domain estimates equal the direct national estimate. Grouped benchmarking applies the constraint separately within each selected higher-level area."
            ),
            choices = c(
              "National" = "national",
              "Grouped by a survey variable" = "custom"
            ),
            selected = "national"
          ),
          fileInput("regional_benchmark_file",
            tip_label("Benchmark Target Database (optional)",
                      "Optional RDS/CSV/XLSX file with benchmark targets by year. Leave blank to estimate targets from the survey using population_weight = weight * household size.")),
          tags$div(
            class = "benchmark-format-note",
            tags$div(class = "note-title", "External benchmark file format"),
            tags$p(
              "Upload this file only when you have external benchmark targets. ",
              "Leave it blank to estimate targets from the survey."
            ),
            tags$ul(
              tags$li("National benchmarking: include a year column and a target column."),
              tags$li("Grouped benchmarking: also include the benchmark-level column selected below."),
              tags$li("Target column names can be benchmark, target, value, poverty_rate, rate, or mean."),
              tags$li("Accepted file types: .rds, .RData/.rda, .csv, .txt, .xlsx, or .xls.")
            )
          ),
          tags$div(
            class = "benchmark-clear-row",
            actionButton("clear_benchmark_file",
              "Clear selected file",
              class = "btn-default btn-sm")
          ),
          uiOutput("benchmark_active_file"),
          conditionalPanel(
            condition = "input.benchmark_level == 'custom'",
            mapping_selectize("var_benchmark_level",
              tip_label("Grouped benchmark variable",
                        "Survey column defining the higher-level benchmark groups, such as region, NUTS2, or voivodeship."),
              "")
          )
        )
        ,
        tags$hr(),
        h4("Data assessment"),
        checkboxInput("psu_consistent",
          tip_label("PSU codes are consistent over time", "Check this if the same PSU identifiers refer to the same sampling units across years. Affects how cross-year covariance of sampling errors is estimated."),
          value = FALSE)
      ),

      # ============================ STEP 5 ============================
      wiz_panel("ai",
        tags$div(class = "wiz-note",
          tags$strong("This step is optional."),
          " Leave the assistant disabled and press Next to continue \u2014 the analysis runs identically without it. ",
          "Only aggregate statistics are ever sent to the API; never raw microdata."
        ),
        tags$div(class = "wiz-grid",
          tags$div(class = "wiz-col",
            h4("AI Assistant (optional)"),
            uiOutput("llm_consent_ui"),
            checkboxInput("llm_enabled",
              tip_label("Enable AI Assistant", "Enable an AI-powered assistant that interprets diagnostics, evaluates normality assumptions, and enriches the analysis brief. Supports Anthropic (Claude) and OpenAI (ChatGPT) API keys."),
              value = FALSE),
            conditionalPanel(
              condition = "input.llm_enabled",
              checkboxInput("llm_external_consent",
                "I understand that aggregate estimates, uncertainty measures, diagnostics, and analysis context will be sent to the selected external AI provider. Geographic identifiers are removed from comparison prompts; raw microdata are not sent.",
                value = FALSE),
              passwordInput("api_key",
                tip_label("API Key", "Enter your Anthropic (sk-ant-...) or OpenAI (sk-...) API key. Only aggregate statistics are sent to the API \u2014 never raw microdata."),
                value = ""),
              selectInput("language",
                tip_label("Language", "Switch language of AI-assisted interpretation. All 24 official EU languages are supported, plus Arabic."),
                choices = supported_languages(),
                selected = "en")
            )
          ),
          tags$div(class = "wiz-col")
        )
      ),

      # ============================ STEP 6 ============================
      wiz_panel("run",
        tags$div(class = "wiz-note",
          "Review the settings below, run the readiness check, then start the analysis."
        ),
        uiOutput("wiz_outstanding"),
        h4("Settings summary"),
        uiOutput("wiz_summary"),
        tags$hr(),
        h4("Run"),
        tags$div(class = "wiz-run-row",
          actionButton("check_btn", "1. Check Data Readiness", class = "btn-default",
                       style = "margin-right: 8px;"),
          actionButton("run_btn",   "2. Run Analysis",         class = "btn-primary")
        ),
        uiOutput("wiz_run_hint"),
        tags$hr(),
        tabsetPanel(
          id = "main_tabs",
        
          tabPanel("Preflight",
            h4("Startup Preflight"),
            p("Review these checks before running the pipeline. This panel updates automatically as you change inputs."),
            uiOutput("preflight_ui"),
            h4("Recommended actions"),
            verbatimTextOutput("preflight_actions")
          ),
        
          # ---- Tab: Data Readiness ----
          tabPanel("Data Readiness",
            h4("Data Readiness Assessment"),
            p("Click '1. Check Data Readiness' in the sidebar to generate diagnostics. ",
              "Review the results here before starting the UFH / MFH analysis."),
            verbatimTextOutput("readiness_messages"),
            h4("National Poverty Headcount Rates"),
            tableOutput("readiness_national"),
            h4("Domain Consistency"),
            tableOutput("readiness_domains"),
            h4("Missing Poverty Rates"),
            tableOutput("readiness_missing"),
            h4("Auxiliary Covariate Summary"),
            p("Means, standard errors, observation counts, and correlations with the domain-level target indicator (poverty rate or mean welfare, matching the Indicator selector)."),
            tableOutput("readiness_aux")
          ),
        
          # ---- Tab: Pipeline Status ----
          tabPanel("Status",
            h4("Current step"),
            textOutput("status"),
            h4("Run folder"),
            verbatimTextOutput("run_location"),
            h4("Run log"),
            verbatimTextOutput("logs"),
            h4("Expected outputs"),
            tableOutput("outputs")
          )
        )
      )
    ),

    # Resources footer ----------------------------------------------
    tags$div(class = "wiz-step", style = "padding: 14px 20px 10px;",
      h4("Resources"),
      tags$div(
        style = "font-size: 12px; color: #556; margin: -4px 0 10px 0;",
        "Detailed instructions and methodological notes are maintained outside the app. ",
        "Use these files as the authoritative user guidance:"
      ),
      tags$ul(
        style = "font-size: 12px; color: #556; padding-left: 18px; margin-top: 0;",
        tags$li(tags$code("docs/guidance/guidelines_v5_2_0_rc6_wizard.docx")),
        tags$li(tags$code("docs/MCPE_VALIDATION_STATUS.md")),
        tags$li(tags$code("docs/instructions/EU_SAE_Download_Instructions_5_2_0_rc_6_wizard_3_pointwise.pdf")),
        tags$li(tags$code("docs/instructions/EU_SAE_User_Guide_5_2_0_rc_6_wizard_3_pointwise.pptx")),
        tags$li(tags$code("outputs/final_report.html"), " after a completed run")
      )
    )
  ) # end main_app
)

# ---------------------------------------------------------------------
# 5. Wizard server layer
#
#    wizard_server() first runs app.R's server() verbatim -- registering
#    every output, observer and reactive the pipeline needs -- and then
#    adds the navigation, breadcrumb state and soft-gating on top.
#
#    The gating checks below deliberately re-derive their answers from
#    input$ values rather than reaching into app.R's internal reactives
#    (which are not exported). They are cheap, and keeping them separate
#    means app.R needs no modification at all.
# ---------------------------------------------------------------------

wiz_is_blank <- function(x) {
  !length(x) || is.null(x) || !nzchar(trimws(as.character(x)[1]))
}

# Does a saved-setup entry point at a file that still exists on disk?
wiz_saved_file_exists <- function(key) {
  setup <- tryCatch(read_dashboard_setup(), error = function(e) NULL)
  if (is.null(setup)) return(FALSE)
  ref <- tryCatch(setup$data_files[[key]], error = function(e) NULL)
  if (wiz_is_blank(ref)) return(FALSE)
  isTRUE(tryCatch(setup_file_exists(ref), error = function(e) FALSE))
}

wiz_have_file <- function(file_input, key) {
  !is.null(file_input) || wiz_saved_file_exists(key)
}

wizard_server <- function(input, output, session) {

  # ---- Everything from the classic dashboard, unchanged --------------
  .wiz_core_server(input, output, session)

  # ===================================================================
  # Per-step outstanding issues
  #
  # Each returns character(0) when the step is complete. These drive the
  # amber notices, the breadcrumb colours and the step 6 summary. They
  # never block navigation.
  # ===================================================================

  issues_data <- reactive({
    msgs <- character()
    if (length(parse_years(input$years)) != 2L) {
      msgs <- c(msgs, "Enter exactly two analysis years, separated by a comma (e.g. 2012,2013).")
    }
    seed <- suppressWarnings(as.numeric(input$analysis_seed))
    if (length(seed) != 1L || !is.finite(seed) || seed < 0 || seed != floor(seed)) {
      msgs <- c(msgs, "Analysis seed must be a non-negative whole number.")
    }
    if (!wiz_have_file(input$survey_file, "survey_file")) {
      msgs <- c(msgs, "Survey data file not selected.")
    }
    if (!wiz_have_file(input$rhs_file, "rhs_file")) {
      msgs <- c(msgs, "Auxiliary covariates file not selected.")
    }
    if (!wiz_have_file(input$shp_file, "shp_file")) {
      msgs <- c(msgs, "Shapefile / geometries file not selected.")
    }
    msgs
  })

  issues_mapping <- reactive({
    required <- list(
      var_year    = "year",
      var_domain  = "domain",
      var_psu     = "psu",
      var_weight  = "weight",
      var_hh_size = "household size",
      var_welfare = "welfare",
      rhs_domain  = "auxiliary covariates domain field",
      shp_domain  = "shapefile domain field"
    )
    msgs <- character()
    for (id in names(required)) {
      if (wiz_is_blank(input[[id]])) {
        msgs <- c(msgs, sprintf("No column mapped for %s.", required[[id]]))
      }
    }
    # strata is genuinely optional and is deliberately not checked here.
    msgs
  })

  issues_indicator <- reactive({
    msgs <- character()
    ind <- input$indicator_type %||% "poverty"
    if (identical(ind, "poverty")) {
      if (identical(input$povline_type %||% "column", "column")) {
        if (wiz_is_blank(input$var_povline)) {
          msgs <- c(msgs, "No column mapped for the poverty line.")
        }
      } else {
        yrs <- parse_years(input$years)
        for (yr in yrs) {
          val <- input[[paste0("povline_numeric_", yr)]]
          if (is.null(val) || !is.finite(suppressWarnings(as.numeric(val)))) {
            msgs <- c(msgs, sprintf("No numeric poverty line entered for %s.", yr))
          }
        }
      }
    }
    msgs
  })

  issues_models <- reactive({
    msgs <- character()
    if (length(input$steps) == 0L) {
      msgs <- c(msgs, "Select at least one pipeline step (UFH, MFH or Comparison).")
    }
    if (isTRUE(input$do_benchmark) &&
        identical(input$benchmark_level %||% "national", "custom") &&
        wiz_is_blank(input$var_benchmark_level)) {
      msgs <- c(msgs, "Grouped benchmarking requires a benchmark-level survey variable.")
    }
    n_boot <- suppressWarnings(as.numeric(input$mcpe_bootstrap_replicates))
    if (length(n_boot) != 1L || !is.finite(n_boot) || n_boot < 50 || n_boot != floor(n_boot)) {
      msgs <- c(msgs, "MCPE bootstrap replicates must be a whole number of at least 50.")
    }
    msgs
  })

  issues_ai <- reactive({
    msgs <- character()
    if (isTRUE(input$llm_enabled) && !isTRUE(input$llm_external_consent)) {
      msgs <- c(msgs, "AI Assistant is enabled but external-transfer consent has not been acknowledged.")
    }
    if (isTRUE(input$llm_enabled) && wiz_is_blank(input$api_key)) {
      msgs <- c(msgs, "AI Assistant is enabled but no API key has been entered.")
    }
    msgs
  })

  wiz_issue_map <- reactive({
    list(
      data      = issues_data(),
      mapping   = issues_mapping(),
      indicator = issues_indicator(),
      models    = issues_models(),
      ai        = issues_ai(),
      run       = character()
    )
  })

  # ===================================================================
  # Notices, subtitles and breadcrumb state
  # ===================================================================

  wiz_issue_box <- function(msgs, ok_text) {
    if (length(msgs) == 0L) {
      return(tags$div(class = "wiz-ok", ok_text))
    }
    tags$div(
      class = "wiz-issues",
      tags$div(class = "wiz-issues-title",
               "Still outstanding on this step - you can continue anyway:"),
      tags$ul(lapply(msgs, tags$li))
    )
  }

  local({
    ok_texts <- list(
      data      = "All required data inputs are set.",
      mapping   = "All required columns are mapped.",
      indicator = "The indicator is fully defined.",
      models    = "Model options are set.",
      ai        = "Nothing outstanding - the AI Assistant is optional.",
      run       = "Ready."
    )
    subs <- list(
      data      = "Choose the analysis years and point the app at your survey, auxiliary and geometry files.",
      mapping   = "Tell the app which column in each file holds which variable. Red fields are names not found in the selected dataset.",
      indicator = "Choose what is being modelled: a poverty measure (FGT) or mean welfare.",
      models    = "Fay-Herriot model settings, benchmarking and covariate selection. All have defaults.",
      ai        = "Optional. Enables AI-assisted interpretation of the diagnostics.",
      run       = "Check the settings, run the readiness diagnostics, then start the analysis."
    )
    for (sid in WIZ_IDS) {
      local({
        this_id <- sid
        output[[paste0("wiz_issues_", this_id)]] <- renderUI({
          if (identical(this_id, "run")) return(NULL)
          wiz_issue_box(wiz_issue_map()[[this_id]], ok_texts[[this_id]])
        })
        output[[paste0("wiz_sub_", this_id)]] <- renderUI({
          HTML(subs[[this_id]])
        })
      })
    }
  })

  # Breadcrumb colours. Pushed as CSS classes rather than re-rendering
  # the links, so the action-button bindings survive untouched.
  observe({
    issues <- wiz_issue_map()
    current <- input$wizard %||% "data"
    states <- vapply(WIZ_IDS, function(id) {
      if (identical(id, current)) "current"
      else if (length(issues[[id]]) == 0L) "done"
      else "todo"
    }, character(1))
    session$sendCustomMessage("wizCrumbState", list(
      ids    = as.list(paste0("wiz_crumb_", WIZ_IDS)),
      states = as.list(unname(states))
    ))
  })

  # ===================================================================
  # Navigation
  # ===================================================================

  go_to <- function(step_id) {
    if (!step_id %in% WIZ_IDS) return(invisible(NULL))
    updateTabsetPanel(session, "wizard", selected = step_id)
    session$sendCustomMessage("wizScrollTop", list(x = TRUE))
  }

  for (sid in WIZ_IDS) {
    local({
      this_id <- sid
      idx <- match(this_id, WIZ_IDS)

      observeEvent(input[[paste0("wiz_crumb_", this_id)]], {
        go_to(this_id)
      }, ignoreInit = TRUE)

      if (idx > 1L) {
        observeEvent(input[[paste0("wiz_back_", this_id)]], {
          go_to(WIZ_IDS[idx - 1L])
        }, ignoreInit = TRUE)
      }
      if (idx < length(WIZ_IDS)) {
        observeEvent(input[[paste0("wiz_next_", this_id)]], {
          go_to(WIZ_IDS[idx + 1L])
        }, ignoreInit = TRUE)
      }
    })
  }

  observeEvent(input$wiz_jump_run, { go_to("run") }, ignoreInit = TRUE)

  # Loading a saved setup fills in every step at once, so jump straight
  # to the review step rather than making the user click through.
  observeEvent(input$load_setup_btn, { go_to("run") }, ignoreInit = TRUE)
  observeEvent(input$reset_setup_btn, { go_to("data") }, ignoreInit = TRUE)

  # ===================================================================
  # Step 6: consolidated outstanding list, settings summary, run gate
  # ===================================================================

  output$wiz_outstanding <- renderUI({
    issues <- wiz_issue_map()
    labels <- vapply(WIZ_STEPS, function(s) s$short, character(1))
    names(labels) <- WIZ_IDS

    open_steps <- WIZ_IDS[vapply(WIZ_IDS, function(id) length(issues[[id]]) > 0L, logical(1))]
    if (length(open_steps) == 0L) {
      return(tags$div(class = "wiz-ok", "All steps are complete. You can run the analysis."))
    }
    tags$div(
      class = "wiz-issues",
      tags$div(class = "wiz-issues-title", "Outstanding items"),
      tags$ul(lapply(open_steps, function(id) {
        tags$li(
          tags$strong(paste0(labels[[id]], ": ")),
          paste(issues[[id]], collapse = " ")
        )
      }))
    )
  })

  wiz_fmt <- function(x, blank = "(not set)") {
    if (wiz_is_blank(x)) return(blank)
    paste(as.character(x), collapse = ", ")
  }

  wiz_file_label <- function(file_input, key) {
    if (!is.null(file_input)) return(basename(file_input$name[1]))
    setup <- tryCatch(read_dashboard_setup(), error = function(e) NULL)
    ref <- tryCatch(setup$data_files[[key]], error = function(e) NULL)
    if (!wiz_is_blank(ref) && isTRUE(tryCatch(setup_file_exists(ref), error = function(e) FALSE))) {
      return(paste0(basename(ref), " (from saved setup)"))
    }
    "(not selected)"
  }

  output$wiz_summary <- renderUI({
    row  <- function(k, v) tags$tr(tags$th(k), tags$td(v))
    sec  <- function(k) tags$tr(class = "wiz-sec", tags$td(colspan = 2, k))

    ind <- input$indicator_type %||% "poverty"

    rows <- list(
      sec("1. Data"),
      row("Analysis years", wiz_fmt(paste(parse_years(input$years), collapse = ", "))),
      row("Country or territory", wiz_fmt(input$country_name, "Not specified")),
      row("Analysis seed", wiz_fmt(input$analysis_seed, "123")),
      row("Run label", wiz_fmt(input$run_label, "(none)")),
      row("Survey file", wiz_file_label(input$survey_file, "survey_file")),
      row("Auxiliary file", wiz_file_label(input$rhs_file, "rhs_file")),
      row("Geometry file", wiz_file_label(input$shp_file, "shp_file")),
      row("Population file", wiz_file_label(input$population_file, "population_file")),

      sec("2. Mapping"),
      row("year / domain", paste(wiz_fmt(input$var_year), "/", wiz_fmt(input$var_domain))),
      row("psu / weight", paste(wiz_fmt(input$var_psu), "/", wiz_fmt(input$var_weight))),
      row("strata", wiz_fmt(input$var_strata, "(none)")),
      row("household size / welfare", paste(wiz_fmt(input$var_hh_size), "/", wiz_fmt(input$var_welfare))),
      row("auxiliary / shapefile domain", paste(wiz_fmt(input$rhs_domain), "/", wiz_fmt(input$shp_domain))),

      sec("3. Indicator"),
      row("Indicator", if (identical(ind, "poverty")) "Poverty (FGT)" else "Mean welfare")
    )

    if (identical(ind, "poverty")) {
      rows <- c(rows, list(
        row("Poverty measure", sprintf("FGT(%s)", wiz_fmt(input$fgt_alpha, "0"))),
        row("Poverty line source",
            if (identical(input$povline_type %||% "column", "column")) {
              paste("column:", wiz_fmt(input$var_povline))
            } else {
              yrs <- parse_years(input$years)
              vals <- vapply(yrs, function(yr) {
                sprintf("%s = %s", yr, wiz_fmt(input[[paste0("povline_numeric_", yr)]]))
              }, character(1))
              paste("numeric:", paste(vals, collapse = "; "))
            })
      ))
    } else {
      rows <- c(rows, list(row("Currency symbol", wiz_fmt(input$currency_symbol))))
    }

    rows <- c(rows, list(
      sec("4. Models"),
      row("Pipeline steps", wiz_fmt(input$steps, "(none selected)")),
      row("UFH transformation / criterion",
          paste(wiz_fmt(input$ufh_transformation), "/", wiz_fmt(input$ufh_ic_criterion))),
      row("UFH LASSO screening", if (isTRUE(input$ufh_lasso_enabled)) wiz_fmt(input$ufh_lasso_lambda) else "off"),
      row("MFH model / criterion",
          paste(wiz_fmt(input$mfh_diag_model), "/", wiz_fmt(input$mfh_ic_criterion))),
      row("MFH variance / covariance",
          paste(wiz_fmt(input$mfh_var_choice), "/", wiz_fmt(input$mfh_cov_choice))),
      row("MFH LASSO screening", if (isTRUE(input$mfh_lasso_enabled)) wiz_fmt(input$mfh_lasso_lambda) else "off"),
      row("Fit MFH3", if (isTRUE(input$fit_mfh3) || input$mfh_diag_model %in% c("AUTO", "MFH3")) "yes" else "no"),
      row(
        "MFH3 reference-variance test adjustment",
        if (identical(input$mfh_diag_model, "MFH3") || isTRUE(input$fit_mfh3)) {
          wiz_fmt(input$mfh_refvar_adjustment, "bonferroni")
        } else {
          "not applied (MFH3 not requested)"
        }
      ),
      row("Poverty-change inference", "pointwise primary; BH and Bonferroni sensitivities reported"),
      row("MCPE bootstrap replicates", wiz_fmt(input$mcpe_bootstrap_replicates, "200")),
      row("Benchmarking",
          if (isTRUE(input$do_benchmark)) {
            lvl <- if (identical(input$benchmark_level %||% "national", "national")) {
              "national"
            } else {
              paste("by", wiz_fmt(input$var_benchmark_level, "(variable not selected)"))
            }
            paste("on -", lvl)
          } else "off"),
      row("PSU codes consistent over time", if (isTRUE(input$psu_consistent)) "yes" else "no"),

      sec("5. AI Assistant"),
      row("Enabled", if (isTRUE(input$llm_enabled)) "yes" else "no"),
      row("External-transfer consent", if (isTRUE(input$llm_external_consent)) "acknowledged" else "not acknowledged"),
      row("API key", if (wiz_is_blank(input$api_key)) "(not entered)" else "entered"),
      row("Language", wiz_fmt(input$language, "en"))
    ))

    tags$table(class = "wiz-summary", tags$tbody(rows))
  })

  # Run gate. Only the genuinely fatal prerequisites block the button --
  # the same ones app.R's own run handler requires. Everything else is a
  # hint, so an expert user is never trapped by a validation rule.
  wiz_run_blockers <- reactive({ issues_data() })

  observe({
    session$sendCustomMessage("wizToggleBtn", list(
      id = "run_btn",
      disabled = length(wiz_run_blockers()) > 0L
    ))
  })

  output$wiz_run_hint <- renderUI({
    blockers <- wiz_run_blockers()
    if (length(blockers) > 0L) {
      return(tags$div(
        class = "wiz-issues",
        tags$div(class = "wiz-issues-title", "Run Analysis is disabled until step 1 is complete:"),
        tags$ul(lapply(blockers, tags$li))
      ))
    }
    other <- setdiff(unlist(wiz_issue_map()[setdiff(WIZ_IDS, "data")]), NULL)
    if (length(other) > 0L) {
      return(tags$div(
        class = "wiz-note",
        "You can run now, but some later steps still have outstanding items (listed above). ",
        "Run ", tags$strong("1. Check Data Readiness"), " first if you are unsure."
      ))
    }
    tags$div(
      class = "wiz-note",
      "Tip: run ", tags$strong("1. Check Data Readiness"),
      " before ", tags$strong("2. Run Analysis"), " to catch data problems early."
    )
  })
}

# ---------------------------------------------------------------------
# 6. Launch
#
# Mirrors app.R's behaviour: when sourced non-interactively (i.e. from
# Start_Here/Start_Wizard.bat) the app is launched explicitly; inside RStudio the
# shinyApp object is simply returned so "Run App" works. The guard
# environment variable prevents re-entry if the file is sourced twice.
# ---------------------------------------------------------------------

.wiz_app <- shinyApp(ui, wizard_server)

.wiz_run <- function(preferred = 7788L) {
  for (port in c(preferred, preferred + 1L, preferred + 2L, preferred + 3L)) {
    ok <- tryCatch({
      shiny::runApp(.wiz_app, host = "127.0.0.1", port = port,
                    launch.browser = TRUE)
      TRUE
    }, error = function(e) {
      if (grepl("port|address|in use", conditionMessage(e), ignore.case = TRUE)) {
        message(sprintf("Port %d unavailable, trying the next one...", port))
        FALSE
      } else {
        stop(e)
      }
    })
    if (isTRUE(ok)) return(invisible(TRUE))
  }
  # Last resort: let Shiny pick any free port.
  shiny::runApp(.wiz_app, host = "127.0.0.1", launch.browser = TRUE)
  invisible(TRUE)
}

if (!interactive() && !nzchar(Sys.getenv("EU_SAE_WIZARD_LAUNCHED"))) {
  Sys.setenv(EU_SAE_WIZARD_LAUNCHED = "1")
  .wiz_run()
} else {
  .wiz_app
}
