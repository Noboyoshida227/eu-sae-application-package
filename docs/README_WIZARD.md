# EU SAE Dashboard — Wizard edition

Wizard version: **5.2.0-rc.6-wizard.4-crossplatform**  
Underlying EU SAE package: **5.2.0-rc.6**

This is the clean 5.2.0-rc.6 application package with an additional guided
front end. The statistical pipeline and the classic dashboard remain in
`app.R`; `app_wizard.R` presents the same inputs as six sequential steps and
reuses the same server logic.

## Included entry points

| File | Purpose |
|---|---|
| `Start_Here/Start_Wizard.bat` | Launches the six-step wizard on Windows, normally on localhost port 7788 |
| `Start_Here/Start_Wizard.command` | Launches the six-step wizard on macOS or Linux |
| `app_wizard.R` | Wizard front end |
| `Start_Here/Start_Dashboard.bat` | Launches the classic dashboard on Windows, normally on localhost port 7777 |
| `Start_Here/Start_Dashboard.command` | Launches the classic dashboard on macOS or Linux |
| `app.R` | 5.2.0-rc.6 classic dashboard and shared server |

Both dashboards may run at the same time because they use different ports.

## Run the wizard

R 4.2.0 or later is required. Use the newest version that your organization has
approved and made available; the absolute latest R release is not required. On
a managed computer, keep the approved R 4.2+ installation unless IT directs an
upgrade.

Report creation also needs Pandoc. Before the first run, install **one** free
option: [RStudio Desktop](https://posit.co/download/rstudio-desktop/),
[Quarto](https://quarto.org/docs/get-started/), or [standalone
Pandoc](https://pandoc.org/installing.html). You do not need all three.

On Windows, double-click:

```bat
Start_Here/Start_Wizard.bat
```

On macOS, double-click `Start_Here/Start_Wizard.command`. The first launch may
require Control-clicking the file and choosing **Open**. On Linux, run
`bash Start_Here/Start_Wizard.command`. See `Start_Here/README.md` for macOS
Gatekeeper and executable-permission help.

The launcher opens `http://127.0.0.1:7788` and tries successive ports if 7788
is occupied. Keep the launcher window open while using the dashboard. As an
alternative, open `app_wizard.R` in RStudio and select **Run App**.

Two example datasets are included, both for learning and testing only.
`Data/simulated/` holds fully synthetic files built by an included seeded script.
`Data/Spain/` holds the Spain province example used in the guidance notes, derived
from the GPL-2 `sae` package -- see `Data/Spain/README.md` for its attribution,
licence and open provenance item. Neither may feed a real estimate. For actual
analyses, select your own survey, auxiliary-covariate, and geometry files in
step 1.

## Wizard steps

1. **Data** — country or territory, analysis years, reproducibility seed, run
   label, and survey, auxiliary, geometry, and optional population files.
2. **Mapping** — survey columns and the auxiliary/geometry join keys.
3. **Indicator** — poverty (FGT) or mean welfare and its required settings.
4. **Models** — UFH and MFH choices, MCPE bootstrap replicates, benchmarking,
   covariate selection, and PSU consistency.
5. **AI Assistant** — optional external-transfer consent, API key, and output
   language.
6. **Review & Run** — settings summary, preflight/readiness checks, and run.

For UFH and MFH, `sm_out` now has one simple rule: replace a direct variance
only when it is missing/non-finite or strictly below 0.001. A direct variance
equal to 0.001 is retained. Under benchmarking, choose **National** to make the
population-weighted average of the domain estimates equal the direct national
estimate, or choose **Grouped** and map a higher-level survey variable to apply
the constraint separately within each group.

The breadcrumb is clickable. Green indicates a complete step, amber indicates
an outstanding item, and blue indicates the current step.

### MFH3/MFH2 model-selection rule

**MFH2 is the default MFH model.** If the user selects MFH3, the app follows the
Molina-Romero sequence: it fits MFH3 first; if MFH3 errors or does not converge,
the run explicitly records the problem and uses MFH2. If MFH3 converges, the app
evaluates the equality of its time-specific random-effect variances. MFH3 is
retained only when at least one contrast remains significant after the selected
adjustment; otherwise the app uses MFH2. Bonferroni is the default
model-selection adjustment. The raw, Bonferroni-adjusted, and BH-adjusted
p-values are all saved in
`outputs/tables/mfh_variance_structure_selection.csv`. With two years there is
only one contrast, so the adjustments are identical.

This MFH3 control is shown only when MFH3 is selected or requested as a
sensitivity fit. It does **not** adjust poverty-change tests across geographic
domains. Poverty-change figures and headline counts use pointwise p-values and
95% confidence intervals; the final report and exported comparison table also
show BH and Bonferroni domain-level sensitivity results.

MFH1 remains available as a sensitivity model. It uses
time-specific but mutually independent random effects; it does not use an
unstructured random-effects covariance matrix.

The MCPE bootstrap saves execution and Monte Carlo standard-error diagnostics
in `outputs/tables/mfh_mcpe_validation.csv`. These diagnostics support run-level
review but do not replace independent validation of MCPE bias and interval
coverage. See `docs/MCPE_VALIDATION_STATUS.md` for the completed checks, skipped
dependency-dependent tests, and remaining validation work.

### Validation and consent

Navigation is deliberately soft: **Next** remains available when a step is
incomplete, and the wizard lists the outstanding items. The final run control
is disabled until the fatal data prerequisites are present. The shared rc.6
server performs its own configuration and readiness checks before analysis.

AI assistance is optional. When enabled, it requires both an API key and a
separate acknowledgement that aggregate estimates, uncertainty measures,
diagnostics, and analysis context will be sent to the chosen external provider.
Raw microdata are not sent; comparison prompts remove geographic identifiers.
If AI is requested, both final report formats interleave all seven labelled
interpretation blocks with their corresponding statistical sections and records
their statuses and provider/model metadata. Failed sections are shown
explicitly. The statistical results remain authoritative, and every AI block
requires human review before dissemination. The change-significance section
includes the complete pointwise/BH/Bonferroni table, UFH-MFH confidence-interval
width figures, estimated-change distribution and paired-domain figures, and links to the consolidated Excel result tables in
`outputs/data/`.

### Loading a saved setup

**Load Last Setup** is available from every step. After a successful load, the
wizard moves to the review step. API keys and external-transfer consent are not
persisted and must be entered again for each session.

## Architecture and maintenance rule

At launch, `app_wizard.R` parses `app.R`, evaluates its top-level definitions
except the classic `ui` and launcher, and reuses `app.R`'s `server()` function.
The input IDs in the two interfaces must remain in parity.

The steps use `tabsetPanel(type = "hidden")`, which keeps off-screen inputs in
the browser DOM. Do not replace the step bodies with on-demand `renderUI` /
`uiOutput` construction: doing so can make off-screen inputs `NULL` and break
the shared server contract.

## Verification

From this folder, run:

```text
Rscript tests/run_tests.R
Rscript tools/check_ui_parity.R
```

The first command covers the rc.6 package tests plus wizard parsing, version,
and required-control checks. The second renders both interfaces and verifies
that every classic-dashboard element ID appears exactly once in the wizard.

See `docs/WIZARD_RELEASE_NOTICE.txt`, `docs/RELEASE_CHECKLIST.md`, and
`docs/HISTORY_REMEDIATION.md` before public distribution. This is an internal-review candidate,
not evidence that institutional rights or repository-history remediation have
been completed.

## Input folder migration

See `Data/README.md` for the new example layout and saved-setup compatibility.
The user-facing folder is `Data/`; outputs remain under `outputs/`.

## HTML and editable Word outputs

After a successful run, open `outputs/final_report.html` in a browser or
`outputs/final_report.docx` in Word. Both contain the same findings and labeled
AI interpretations when requested. Word text and tables are editable; figures
are embedded images. Save a separate working copy before editing. Later runs
replace current outputs; archived copies are retained under
`app_runs/<timestamp>_<run_label>/outputs/`. Keep `outputs/data/` with the reports
for Excel links. The clean distribution contains no generated reports.

Both formats require Pandoc, supplied by any one of RStudio Desktop, Quarto, or
a standalone Pandoc installation. Word formatting also requires `xml2` and
`zip`, installed by `install_packages.R`. If Word is missing, inspect the run
log, resolve the conversion warning, and regenerate; the completed HTML is
retained.
The new figures are in `outputs/figures/change_comparisons/`, with values in
`outputs/data/change_estimate_comparison.xlsx` and `EU_SAE_results.xlsx`.
They compare signed estimated changes, not CI widths. A tighter distribution
across domains alone does not establish smaller MSE or statistical significance.
