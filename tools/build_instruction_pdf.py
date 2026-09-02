import re
from pathlib import Path

from reportlab.lib import colors
from reportlab.lib.enums import TA_LEFT
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import ParagraphStyle
from reportlab.lib.units import mm
from reportlab.pdfbase.pdfmetrics import stringWidth
from reportlab.pdfgen import canvas
from reportlab.platypus import Paragraph


# ---------------------------------------------------------------------------
# tools/build_instruction_pdf.py
#
# Regenerates docs/instructions/EU_SAE_Download_Instructions_<version>.pdf.
# The version is read from WIZARD_VERSION at the repository root, so this
# script never needs editing when the version changes - run it (or let
# tools/bump_version.py run it) and the PDF is rebuilt with the right label
# and the right filename.
#
#   python tools/build_instruction_pdf.py
#
# Requires the reportlab package:  pip install reportlab
# ---------------------------------------------------------------------------
ROOT = Path(__file__).resolve().parents[1]
WIZARD_VERSION = (ROOT / "WIZARD_VERSION").read_text(encoding="utf-8").strip()
if not re.fullmatch(r"[A-Za-z0-9._-]+", WIZARD_VERSION):
    raise SystemExit(f"Invalid WIZARD_VERSION: {WIZARD_VERSION!r}")
VERSION_TOKEN = re.sub(r"[.-]", "_", WIZARD_VERSION)          # 5.2.0-rc.6-x -> 5_2_0_rc_6_x
OUT = ROOT / "docs" / "instructions" / f"EU_SAE_Download_Instructions_{VERSION_TOKEN}.pdf"

PAGE_W, PAGE_H = A4
MARGIN = 18 * mm
CONTENT_W = PAGE_W - 2 * MARGIN

NAVY = colors.HexColor("#132B56")
BLUE = colors.HexColor("#3F86C7")
LIGHT_BLUE = colors.HexColor("#EAF4FC")
TEXT = colors.HexColor("#263B5D")
MUTED = colors.HexColor("#687B97")
WARNING_BG = colors.HexColor("#FFF3E6")
WARNING_BORDER = colors.HexColor("#E58A2C")
RULE = colors.HexColor("#D8E2EE")


body_style = ParagraphStyle(
    "body",
    fontName="Helvetica",
    fontSize=7.65,
    leading=9.35,
    textColor=TEXT,
    alignment=TA_LEFT,
    spaceAfter=0,
)

warning_style = ParagraphStyle(
    "warning",
    parent=body_style,
    fontSize=7.35,
    leading=9.0,
    textColor=colors.HexColor("#6B3A08"),
)


def draw_wrapped(c, html, x, y_top, width, style=body_style):
    para = Paragraph(html, style)
    _, height = para.wrap(width, PAGE_H)
    para.drawOn(c, x, y_top - height)
    return y_top - height


def draw_page_header(c, kicker, title, page_no):
    c.setFillColor(NAVY)
    c.rect(0, PAGE_H - 31 * mm, PAGE_W, 31 * mm, fill=1, stroke=0)
    c.setFillColor(BLUE)
    c.rect(0, PAGE_H - 31 * mm, 4 * mm, 31 * mm, fill=1, stroke=0)
    c.setFont("Helvetica", 7.2)
    c.setFillColor(colors.white)
    c.drawString(MARGIN, PAGE_H - 9.2 * mm, f"EU SAE {WIZARD_VERSION} - review candidate")
    c.setFont("Helvetica-Bold", 17.2)
    c.drawString(MARGIN, PAGE_H - 20.0 * mm, title)
    c.setFont("Helvetica", 7.5)
    c.setFillColor(colors.HexColor("#C7D9EF"))
    c.drawString(MARGIN, PAGE_H - 26.3 * mm, kicker)

    c.setStrokeColor(RULE)
    c.setLineWidth(0.6)
    c.line(MARGIN, 14.5 * mm, PAGE_W - MARGIN, 14.5 * mm)
    c.setFont("Helvetica", 6.8)
    c.setFillColor(MUTED)
    c.drawString(MARGIN, 9.5 * mm, "EU SAE application package - review and testing only")
    label = f"Page {page_no}"
    c.drawString(PAGE_W - MARGIN - stringWidth(label, "Helvetica", 6.8), 9.5 * mm, label)


def draw_section(c, y, number, title, html, gap=3.4 * mm):
    c.setFillColor(BLUE)
    c.rect(MARGIN, y - 2.5 * mm, 2.2 * mm, 4.4 * mm, fill=1, stroke=0)
    c.setFont("Helvetica-Bold", 10.0)
    c.setFillColor(NAVY)
    c.drawString(MARGIN + 4.0 * mm, y - 1.1 * mm, f"{number}. {title}")
    y -= 4.5 * mm
    y = draw_wrapped(c, html, MARGIN + 4.0 * mm, y, CONTENT_W - 4.0 * mm)
    return y - gap


def draw_warning(c, y, html):
    para = Paragraph(html, warning_style)
    _, height = para.wrap(CONTENT_W - 8 * mm, PAGE_H)
    box_h = height + 6 * mm
    c.setFillColor(WARNING_BG)
    c.setStrokeColor(WARNING_BORDER)
    c.roundRect(MARGIN, y - box_h, CONTENT_W, box_h, 2.2 * mm, fill=1, stroke=1)
    para.drawOn(c, MARGIN + 4 * mm, y - 3 * mm - height)
    return y - box_h - 4 * mm


def ensure_fit(y, page_no):
    if y < 18 * mm:
        raise RuntimeError(f"Page {page_no} overflowed: bottom y={y:.1f}")


def build_pdf():
    c = canvas.Canvas(str(OUT), pagesize=A4, pageCompression=1)
    c.setTitle("EU SAE Application Package - Download and verification instructions")
    c.setAuthor("EU SAE application package")
    c.setSubject("Review-candidate installation, launch, verification, and release-gate instructions")

    draw_page_header(c, "DOWNLOAD AND VERIFICATION INSTRUCTIONS", "EU SAE Application Package", 1)
    c.setFont("Helvetica-Bold", 9.0)
    c.setFillColor(BLUE)
    c.drawString(MARGIN, PAGE_H - 37.5 * mm, f"Version {WIZARD_VERSION}")
    y = PAGE_H - 43.0 * mm
    y = draw_warning(
        c,
        y,
        "<b>Release-candidate warning.</b> Public sharing remains blocked until Git history/release assets and institutional rights/authority are remediated. Automatic source archives for superseded releases must not be used. A public URL is intentionally not supplied here.",
    )
    y = draw_section(c, y, 1, "Obtain the correct archive",
        f"Obtain the curated, history-free wizard ZIP from the authorized release manager. Its top-level folder must be <b>EU_SAE_wizard_{WIZARD_VERSION}</b>. Do not ZIP the source workspace and do not use an automatic repository source archive.")
    y = draw_section(c, y, 2, "Verify the package",
        f"Open <b>VERSION</b> and confirm 5.2.0-rc.6; open <b>WIZARD_VERSION</b> and confirm {WIZARD_VERSION}. From the extracted package root, run <b>Rscript scripts/verify_release.R</b> to check the full file set and SHA-256 hashes. Compare the ZIP hash with <b>SHA256SUMS.txt</b>. Stop if a listed file is missing or changed, or if an unexpected file is present.")
    y = draw_section(c, y, 3, "Check the expected contents",
        "The clean package contains the application, R helpers, scripts, tests, output placeholders, instructions, guidance, licence/notice files, and the bundled <b>Data/simulated/</b> and <b>Data/Spain/</b> examples. Internal notes, .git, app_runs, user-created Data folders, local libraries, credentials, generated outputs, and local diagnostics must not ship. The Spain survey and auxiliary files retain their GPL-2 terms. The boundary uses documented IGN/CNIG CartoBase ANE under CC BY 4.0; see <b>Data/Spain/README.md</b> and <b>THIRD_PARTY_NOTICES.md</b>. The Spain survey derivation script remains missing. This candidate is not approved for public distribution.")
    y = draw_section(c, y, 4, "Install and start",
        "Install <b>R 4.2 or later</b>. Use the newest R version approved and available in your organization; the absolute latest R release is not required. On a managed computer, keep the approved R installation unless IT directs an upgrade. Run <b>install_packages.R</b> once with internet access.<br/><br/>For HTML and Word reports, install <b>one</b> free Pandoc provider: <link href='https://posit.co/download/rstudio-desktop/' color='#3F86C7'>RStudio Desktop</link>, <link href='https://quarto.org/docs/get-started/' color='#3F86C7'>Quarto</link>, or <link href='https://pandoc.org/installing.html' color='#3F86C7'>standalone Pandoc</link>. You do not need all three. On Windows, open <b>Start_Here</b> and double-click a .bat launcher. On macOS, double-click <b>Start_Wizard.command</b> or <b>Start_Dashboard.command</b>; the first launch may require Control-click, then Open. On Linux, run <b>bash Start_Here/Start_Wizard.command</b> or the Dashboard equivalent. Keep the terminal window open. See <b>Start_Here/README.md</b> for Mac Gatekeeper and executable-permission help.")
    y = draw_section(c, y, 5, "Follow the wizard flow (or dashboard equivalent)",
        "The wizard presents the same inputs as six steps: 1 Data, 2 Mapping, 3 Indicator, 4 Models, 5 AI Assistant (optional consent, key, and language), and 6 Review &amp; Run. On the final step, use <b>1. Check Data Readiness</b> before <b>2. Run Analysis</b>. The breadcrumb is clickable: green means complete, amber means an outstanding item, and blue means the current step. Navigation is soft, but the run control stays disabled until fatal data requirements are met. <b>Load Last Setup</b> is available from every step; API keys and transfer consent are never persisted. The classic dashboard exposes the same controls in one sidebar.")
    ensure_fit(y, 1)
    c.showPage()

    draw_page_header(c, "INPUTS, MODEL OPTIONS, REPRODUCIBILITY, AND OPTIONAL AI", "Prepare and run an analysis", 2)
    y = PAGE_H - 38.0 * mm
    y = draw_section(c, y, 6, "Select approved local inputs",
        "For learning, use either bundled example. <b>Data/simulated/</b> contains fully synthetic survey, auxiliary, and geometry files plus a seeded generator. <b>Data/Spain/</b> contains the guidance example with mapping, attribution, and licence details. Neither example may feed a real estimate. For actual analyses, browse to approved household survey microdata, domain-level auxiliary covariates, geometry, and optional population or benchmark-target files. Confirm provenance, permitted use, sensitivity classification, area codes, and year coverage before running. Results remain under <b>outputs/</b>; see <b>Data/README.md</b> for saved-setup compatibility.")
    y = draw_section(c, y, 7, "Apply the revised variance rule",
        "For UFH and MFH, <b>sm_out</b> replaces a direct sampling variance with its smoothed value only when the direct value is missing/non-finite or strictly below 0.001. A direct variance equal to 0.001 is retained. <b>sm_all</b> replaces all direct variances; <b>direct</b> retains direct values subject to documented safety backfills.")
    y = draw_section(c, y, 8, "Choose the benchmarking level explicitly",
        "After selecting Apply benchmarking, choose National or Grouped. National uses one countrywide group and constrains population-weighted domain estimates to the direct national target. Grouped requires a mapped higher-level survey variable, such as region or NUTS2, and applies the constraint within each group. An external Benchmark Target Database remains optional.")
    y = draw_section(c, y, 9, "Record reproducibility and inference controls",
        "The seed defaults to 123. MFH MCPE bootstrap uses <b>mcpe_nB</b> (interactive default 200; minimum 50); use at least 500 replications for production inference and assess Monte Carlo stability. The principal significance flag and red/gray figures use the pointwise raw test. At alpha = 0.05, significance matches whether the pointwise 95% confidence interval excludes zero. Benjamini-Hochberg fields remain sensitivity outputs and should support any statement about how many domains changed.")
    y = draw_section(c, y, 10, "Understand the model selection rule",
        "MFH2 is the default MFH model. Selecting MFH3 starts the Molina-Romero sequence: an estimation error or non-converged fit moves automatically to MFH2, with the two cases logged separately. A converged MFH3 fit is retained only when its random-effect variance-homogeneity test supports heterogeneity after the selected multiplicity adjustment. MFH1 remains a manual sensitivity model. The optional MFH3 sensitivity fit does not change the selected model; the final report records the model used.")
    y = draw_section(c, y, 11, "Interpret covariates and fallbacks correctly",
        "With LASSO enabled, year-specific UFH or MFH covariate lists define the screening candidate pool. With LASSO disabled, they define the fixed specification. If a boundary MFH fit cannot be benchmarked, Comparison records the reason in <b>benchmark_status.csv</b> and continues with unbenchmarked estimates.")
    y = draw_section(c, y, 12, "Treat AI as a separate external transfer",
        "AI is off by default. Enter a provider key only after selecting the external-transfer acknowledgement and confirming that the provider or gateway is institutionally approved. The application does not send household microdata. Prompts include unpublished aggregate estimates, uncertainty measures, diagnostics, counts, and analysis context; geographic identifiers are removed from top-ranked and extreme-change prompt tables. Aggregate unpublished estimates may still be confidential. AI commentary is advisory and requires documented human review before dissemination.")
    ensure_fit(y, 2)
    c.showPage()

    draw_page_header(c, "RECORDKEEPING AND RELEASE GATE", "Keep the evidence and complete review", 3)
    y = PAGE_H - 42.0 * mm
    y = draw_section(c, y, 13, "Read both reports and keep the evidence",
        "After a successful run, open <b>outputs/final_report.html</b> in a browser or <b>outputs/final_report.docx</b> in Word. Word text and tables are editable, and figures are embedded images. Save a separate working copy before editing; later runs replace current outputs. Both formats contain the same statistical results and labeled AI text when requested. Keep <b>outputs/data/</b> beside the reports for linked Excel workbooks; archived copies are under <b>app_runs/&lt;timestamp&gt;_&lt;run_label&gt;/outputs/</b>.<br/><br/>The clean ZIP excludes generated reports. Signed-change figures are under <b>outputs/figures/change_comparisons/</b>, with source values in <b>outputs/data/change_estimate_comparison.xlsx</b> and <b>EU_SAE_results.xlsx</b>. They compare estimated changes in percentage points, not confidence-interval widths. A tighter spread across domain estimates alone does not establish smaller MSE or a statistically significant variance reduction.<br/><br/>If report conversion fails, the HTML remains. Check the run log, the selected Pandoc provider, xml2, and zip, then regenerate after resolving the warning. Retain the exact ZIP, release_manifest.csv, verification output, VERSION, WIZARD_VERSION, run metadata, input hashes, and review approvals. Passing tests or checksums does not approve the statistical methods, data rights, privacy basis, or official publication.<br/><br/><b>Release gate.</b> Before public release, complete <b>docs/HISTORY_REMEDIATION.md</b> and <b>docs/RELEASE_CHECKLIST.md</b>, obtain institutional approval for documentation/media rights and release authority, re-clone the rewritten repository, rebuild the clean archive, and independently verify its manifest.")
    ensure_fit(y, 3)
    c.save()


if __name__ == "__main__":
    build_pdf()
    print(f"wrote {OUT.relative_to(ROOT)}  ({OUT.stat().st_size:,} bytes)")
