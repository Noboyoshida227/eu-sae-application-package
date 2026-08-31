# EU SAE Application Package 5.2.0-rc.6

## Start here

Install **R 4.2 or later** and run `install_packages.R` once to install the
required packages. Report rendering also requires Pandoc; `sf` needs its
platform-specific system libraries. Keep the launcher window open while using
the application.

| Interface | Launch |
|---|---|
| Guided wizard (recommended on Windows) | Double-click `Start_Here/Start_Wizard.bat`; normally port 7788 |
| Classic dashboard on Windows | Double-click `Start_Here/Start_Dashboard.bat`; normally port 7777 |
| Classic dashboard on macOS/Linux | Run `bash Start_Dashboard.sh` |

On Windows, open the **Start_Here** folder and choose a launcher. Keep this folder inside the extracted package; it runs the application from its parent directory. The macOS/Linux shell launcher stays at the package root.

Ports advance if occupied. There is no supplied wizard shell launcher; manual
launch through R remains possible from the package root using
`source("app_wizard.R")` after installing dependencies.

- [Download and verification instructions](docs/instructions/EU_SAE_Download_Instructions_5_2_0_rc_6_wizard_3_pointwise.pdf)
- [Wizard reference](docs/README_WIZARD.md) and [slide guide](docs/instructions/EU_SAE_User_Guide_5_2_0_rc_6_wizard_3_pointwise.pptx)
- [Methodological guidance](docs/guidance/guidelines_v5_2_0_rc6_wizard.docx)
- [Examples and your own input folders](Data/README.md)


This is a revised release candidate based on the exact v5.1.0 tag. It includes
the Shiny dashboard, UFH/MFH analysis scripts, reporting, audit controls, tests,
and guidance. It is not yet approved for production official statistics; read
`docs/REVISION_STATUS.md` and `docs/RELEASE_CHECKLIST.md` first.

## HTML and Word reports

Completed runs keep `outputs/final_report.html` and also create
`outputs/final_report.docx`. Word contains the same findings, figures, full result
tables and labelled AI interpretations (when requested), with editable text and
tables. Wide tables are arranged into readable panels with identifier columns
repeated; single-record diagnostics use a field/value layout. Figures are
embedded images. The HTML remains the interactive version, and Excel links need
the adjacent `outputs/data/` folder. The Word export uses the same Pandoc already
required for HTML, plus the R packages `xml2` and `zip` installed by
`install_packages.R`. A conversion failure is logged without discarding the HTML.
If Word is missing, check the run log, resolve missing dependencies, and
regenerate the report. Save a separate Word working copy before editing; later
runs replace current outputs. Archived copies are under
`app_runs/<timestamp>_<run_label>/outputs/`. The clean ZIP does not include
generated reports.

Comparison runs also produce a distribution and paired-domain plot of estimated
changes in `outputs/figures/change_comparisons/`, alongside the existing
confidence-interval-width figures. Data and summaries are in
`outputs/data/change_estimate_comparison.xlsx` and `EU_SAE_results.xlsx`.

## Required inputs

Browse to three approved local inputs in the dashboard:

1. household survey microdata;
2. domain-level auxiliary covariates;
3. domain geometry.

Tabular formats: RDS/RData, CSV/TSV/TXT/DAT, Stata, SPSS, SAS, Parquet,
Feather, and Excel. Text defaults to UTF-8; set `SAE_INPUT_ENCODING` before
launch only when a documented source encoding differs. Geometry formats: sf
RDS/RData, zipped ESRI shapefile, GeoPackage, GeoJSON, KML, and GML.

The review candidate includes the documented Spain example and fully synthetic
examples under `Data/Spain/` and `Data/simulated/`. Neither provides real
estimates. This candidate uses documented IGN/CNIG CartoBase ANE boundaries
under CC BY 4.0; survey derivation and other release approvals remain open.
See [data limitations and map attribution](Data/Spain/README.md) and `THIRD_PARTY_NOTICES.md`.

## Reproducibility controls

- `Analysis seed` controls LASSO folds and MFH bootstrap draws.
- `MCPE bootstrap replicates` defaults to 200 for interaction; production runs
  should use at least 500 only after a stability study.
- Each run records VERSION, config, input filenames and SHA-256 hashes, package
  versions, R session information, seed, and MCPE count.
- UFH change inference explicitly states its period-independence assumption.
- The primary change status and red/gray figures use pointwise inference at
  alpha = 0.05, matching whether the pointwise 95% confidence interval excludes
  zero. Change tables and the final report also show Benjamini-Hochberg- and
  Bonferroni-adjusted p-values and flags as supplementary sensitivity
  information. These domain-change adjustments are separate from the MFH3
  reference-variance test adjustment.
- When a custom covariate list is supplied, it is the LASSO candidate pool if
  LASSO is enabled and the fixed model specification if LASSO is disabled.
- If MFH3 is requested but does not converge, the report and run artifacts say
  so explicitly before using MFH2. Computational MFH3 errors are labeled
  separately rather than being reported as non-convergence.
- If a boundary MFH fit cannot produce benchmarked estimates, Comparison
  continues with unbenchmarked estimates and records the reason in
  `outputs/tables/benchmark_status.csv`.

Generated files are placed in `outputs/` and archived under `app_runs/`. Input
files are not copied into a run archive. `Save Current Setup` is the only
user-initiated feature that makes local convenience copies under
`app_runs/_last_setup_files`.

The Comparison step also creates `outputs/data/EU_SAE_results.xlsx`, a
consolidated workbook containing domain/year estimates, the complete pointwise,
BH, and Bonferroni significance table, significance counts, and UFH-MFH
confidence-interval width tables. `outputs/data/ci_width_comparison.xlsx`
contains the interval-width domain table and distribution/paired summaries.
The corresponding distribution and paired-domain figures are integrated into
both `final_report.html` and `final_report.docx`.

## Optional AI

AI is off by default and requires a separate external-transfer acknowledgment.
Comparison prompts omit geographic identifiers, but still contain unpublished
aggregate estimates and diagnostics. Review `docs/SECURITY_AND_AI.md`. An approved
gateway can be selected with `SAE_OPENAI_BASE_URL` or
`SAE_ANTHROPIC_BASE_URL`.

When AI is requested, both report formats place each distinctly labelled AI
interpretation beside the statistical section it discusses. Every expected
section is marked generated, failed, or disabled; partial provider failures
cannot appear as silent gaps. Each block records provider/model and generation
metadata and requires human review before dissemination.
`outputs/data/ai_interpretations.rds` contains the structured audit state. A
separate `comparison_ai_note.html` is no longer produced.

## Verification and release

Run:

```text
Rscript tests/run_tests.R
Rscript scripts/check_dependency_lock.R
Rscript scripts/build_clean_release.R
```

The authoritative builder includes only `scripts/release_inventory.csv` and
excludes internal notes, arbitrary user Data folders, local libraries,
credentials, run histories, generated outputs and non-inventory literature.
It creates fresh staging, a manifest, a ZIP and `SHA256SUMS.txt` under
`dist/reorganized_candidate/`; an existing destination is never overwritten.
The ZIP contains one package folder with unchanged version identifiers.
Run `Rscript scripts/verify_release.R` from an extracted clean package to
verify both its full file set and hashes. Passing verification is not public
release approval. See `docs/RELEASE_CHECKLIST.md` before distribution.

## Main contents

- `app.R`, `app_support.R`, `report.Rmd`, `install_packages.R`
- `R/` application helpers
- `scripts/` analysis and release tools
- `tests/` targeted regression/static tests
- `outputs/{data,tables,figures}/.gitkeep`
- `docs/guidance/Technical notes/` and user instructions
- release, privacy, support, data-rights, and issue-status documentation

Rights and governance: read `LICENSE`, `NOTICE`, `THIRD_PARTY_NOTICES.md`,
`docs/GOVERNANCE.md`, and `docs/HISTORY_REMEDIATION.md`. The MIT grant applies only to
software source code; it does not license documentation, data, media, or
third-party material.

## Reading the change figures

The estimated-change plots show later year minus earlier year in percentage
points for poverty, using the same finite matched domains and unbenchmarked
UFH/MFH estimates. Negative values indicate a decrease. Their spread across
domains is not the MSE of a domain estimate. A tighter MFH distribution is
consistent with temporal borrowing and shrinkage, but does not prove greater
accuracy or a statistically significant variance reduction. Use the separate
CI-width figures, MSE/MCPE diagnostics, and model sensitivity checks to assess
uncertainty. See Section 11.9 of the guidance note.
