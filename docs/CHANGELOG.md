# Changelog

## 5.2.0-rc.6-wizard.4-crossplatform - 2026-09-01

- Added macOS/Linux launchers `Start_Here/Start_Wizard.command` and
  `Start_Here/Start_Dashboard.command`, double-clickable in Finder. The former
  `Start_Dashboard.sh` had Windows line endings (its shebang failed), sat
  outside `Start_Here`, and had no wizard equivalent; it now hands over to the
  new launcher.
- The release builder marks the launchers executable inside the ZIP
  (`R/zip_permissions.R`). An archive written on Windows records no Unix
  permission bits, so the launchers would otherwise arrive unable to run.
- Fixed a platform-dependent gap in `scripts/eblupMFH2_robust.R`: a matrix
  containing non-finite values could pass `chol()` on some R/LAPACK builds and
  be returned as an available, all-NaN inverse instead of being reported
  unavailable. Non-finite input is now rejected before any decomposition.
- Documentation: launch instructions for all three platforms, including the
  macOS first-launch security prompt; the newest organisation-approved R 4.2+
  is sufficient; one Pandoc provider (RStudio Desktop, Quarto, or standalone
  Pandoc) is enough for report rendering.
- Continuous integration installs the packages the test suite needs, and the
  suite skips the Spain example-data checks in a clone (that data is excluded
  from the repository by licence). The suite had never previously run to
  completion; on its first full run it found the `chol()` gap above.
- Release process: `tools/bump_version.py` propagates the version to every
  file and filename that embeds it; `Release.ps1` refuses to build from an
  uncommitted tree, names the release folder by version, requires a CHANGELOG
  entry, and records the commit. See `docs/RELEASING.md`.

## 27 August 2026 — report formats and documentation

- Added editable Word reports alongside HTML, without recomputing estimates.
- Added distribution and paired-domain figures for signed estimated changes;
  retained the existing CI-width figures and exported their source values.
- Updated guidance, download instructions, slide guide, and operational notes
  for both formats, dependencies, archives, and conversion warnings.
- Clarified that dispersion across estimated changes is not prediction MSE.


## 5.2.0-rc.6-wizard.3-pointwise - 2026-08-25

- retained raw pointwise, BH-adjusted, and Bonferroni-adjusted poverty-change
  p-values and flags through the Comparison export and displayed them in the
  combined report while keeping pointwise inference primary;
- renamed and conditionally displayed the MFH3 reference-variance adjustment
  so it cannot be mistaken for geographic-domain change multiplicity; and
- preserved method labels as text rather than factor level numbers in XLSX
  exports;
- added UFH-MFH 95% confidence-interval width distribution and paired-domain
  figures to the combined report, with matched-domain summary statistics;
- replaced the 30-row significance preview with the complete scrollable table
  of pointwise, BH-adjusted, and Bonferroni-adjusted results;
- added `EU_SAE_results.xlsx` and `ci_width_comparison.xlsx` to the standard
  output set and linked them from the report; and
- extended the AI change-significance prompt so future AI-enabled runs discuss
  adjusted counts, interval-width distributions, and why narrower intervals do
  not necessarily produce more significant domains;
- restored MFH2 as the default MFH model in the classic and wizard interfaces;
- made an explicit MFH3 selection invoke the Molina-Romero sequence: fit MFH3,
  fall back to MFH2 on error or non-convergence, and otherwise use the adjusted
  MFH3 reference-variance test to choose MFH3 or MFH2;
- retained MFH1 as a manually selected sensitivity model and retained the
  optional MFH3 sensitivity-fit control without changing the selected model;
  and
- revised the guidance correspondence annex and internal-review documentation
  to state the corrected default and conditional MFH3 procedure.

## 5.2.0-rc.6-wizard.2-pointwise - 2026-08-24

- made the Molina-Romero MFH3/MFH2 sequence the default model rule;
- distinguished an MFH3 estimation error from a returned non-converged fit and
  selected MFH2 automatically in either case;
- used the converged MFH3 `refvarTest` contrasts for variance-structure
  selection, with Bonferroni as the conservative default and raw/BH results
  retained for review;
- corrected the wizard description of MFH1: its random-effects covariance is
  diagonal with time-specific variances, while sampling-error covariance may
  be full;
- corrected the MFH3 MCPE bootstrap to include the model's
  `u0 ~ N(0,1)` initial state before the AR(1) recursion; and
- added bootstrap Monte Carlo standard errors, refit counts, covariance-repair
  counts, and explicit assurance status to the final report and CSV outputs.

## Post-5.2.0-rc.6 working revision - 2026-08-24

- simplified `sm_out` for UFH and MFH: a missing/non-finite direct variance or
  a direct variance strictly below 0.001 is replaced by its smoothed variance;
  direct variances equal to or above 0.001 are retained;
- added an explicit National benchmarking level in both interfaces, alongside
  grouped benchmarking, so the population-weighted domain estimate is
  constrained to the direct national estimate; and
- updated the default Anthropic model to `claude-sonnet-4-6`, made OpenAI and
  Anthropic model IDs configurable by environment variable, and exposed useful
  provider error details without recording API keys;
- interleaved all seven AI interpretation blocks with their corresponding
  statistical sections in `final_report.html`; and
- retired the redundant `comparison_ai_note.html` transition output so a run
  produces one combined human-readable report.

## 5.2.0-rc.6 - 2026-08-23

- deferred the only `final_report.html` render until statistical processing and
  optional AI interpretation generation have both completed;
- merged AI interpretations into a clearly labelled appendix in the final
  report while retaining the standalone companion note for one deprecated
  transition release;
- added `outputs/data/ai_interpretations.rds` with provider/model metadata,
  consent and inclusion flags, timestamps, prompt/schema versions, human-review
  status, and explicit generated/failed/disabled status for all seven sections;
- made partial and total provider failures visible in the report rather than
  leaving silent gaps, with sanitized error summaries and stale-artifact cleanup;
- escaped all provider-generated text before HTML insertion and added a
  warning-only numeric-provenance check; and
- added mocked, network-free regression tests for AI-off, complete, partial-
  failure, secret-redaction, metadata persistence, and hostile-HTML cases.

## 5.2.0-rc.5 - 2026-08-23

- corrected the `msae::eblupMFH3` compatibility patch so missing call arguments
  remain missing and valid MFH3 calls are no longer corrupted;
- distinguished genuine MFH3 non-convergence from computational errors in the
  run log, artifacts, report warning, and fallback reason;
- made custom UFH and MFH covariate lists act as LASSO candidate pools when
  screening is enabled and as fixed specifications when it is disabled;
- allowed Comparison to continue with clearly labeled unbenchmarked estimates
  when a boundary MFH fit cannot supply benchmark inputs, and added
  `benchmark_status.csv`;
- removed avoidable log-transform warnings and made mean-welfare
  back-transformation messages state the selected bias-correction policy; and
- hardened wizard clean-manifest exclusions and expanded regression and option-
  matrix coverage for the corrected behavior.

## 5.2.0-rc.4 - 2026-08-21

- changed the primary domain-change decision rule and figure coloring to the
  pointwise two-sided test at alpha = 0.05, so red points correspond directly
  to pointwise 95% confidence intervals that exclude zero; retained BH-adjusted
  p-values and flags as supplementary sensitivity outputs;
- fixed run-metadata input-manifest construction when optional benchmark and
  population paths are absent, and persisted terminal pipeline errors in the
  per-run log;
- anchored UFH, MFH, Comparison, and final-report paths to the running package
  copy so nested clean/wizard builds cannot source stale parent-repository code;
- made arcsine UFH estimation robust to direct domain rates of exactly zero or
  one by replacing undefined design-effect calculations with the observed
  domain sample size and recording the affected domains in the step log;
- explicitly printed the MFH numerical-diagnostics table in the final report;
- recorded a labeled, low-cost condition-number estimate on successful
  Cholesky paths while retaining exact condition numbers on fallback paths;
- added the condition-number method to CSV and run-metadata diagnostics and
  hardened the no-extended-diagnostics robust-refit status;
- made clean-release document selection version-aware instead of maintaining a
  per-release exclusion list;
- expanded regression tests for ill-conditioned positive-definite matrices,
  report rendering, and qualified or bare unsafe convergence expressions;
- corrected the guidance cover version and refreshed the guidance, download
  instructions, dashboard guide, and technical-note version statements; and
- corrected the dashboard guide's title and contributor metadata.

## 5.2.0-rc.3 - 2026-08-21

- added a structured MFH numerical-diagnostics export and persisted inversion,
  robust-refit, and g3/MSE availability status in run metadata;
- surfaced MFH numerical warnings in the run log and final report;
- strengthened the bootstrap convergence regression test so it detects the
  formerly defective qualified expressions;
- changed the clean builder to allow only the literature inventory README and
  exclude every other file in that directory;
- documented the analysis seed, bootstrap-replication defaults and production
  guidance, and raw/BH-adjusted change-inference fields;
- refreshed Word metadata and the versioned guidance, download instructions,
  and dashboard guide; and
- moved exact condition-number calculation off the successful Cholesky path.

## 5.2.0-rc.2 - 2026-08-21

- scoped the MIT grant to software source code and added rights, governance,
  third-party, data-provenance, and Git-history remediation notices;
- corrected H18: robust MFH2 now reports failed Fisher-information inversion
  and unavailable g3/MSE instead of silently substituting zero;
- corrected M18 labeling and added a complete disposition for all 56 findings;
- made MFH bootstrap loops safe when only one period is supplied and hardened
  convergence checks;
- refreshed the guidance note, download instructions, and dashboard guide for
  the history-free 5.2.0-rc.2 clean distribution and AI-consent workflow;
- fixed root-manifest self-inclusion and excluded stale local diagnostics from
  the clean release.

## 5.2.0-rc.1 - 2026-08-21

Revision candidate based on the exact `v5.1.0` release commit
`cd04d479e0484ae98e3ef8299db29ac1f0f944b7`.

- made optional LASSO screening deterministic and recorded its seed;
- made the MFH MCPE bootstrap count configurable (interactive default: 200);
- replaced the MFH absolute lower-variance cutoff with a configurable relative
  rule, disabled for positive variances by default pending method-owner review;
- stated UFH period-independence change variance explicitly and added
  Benjamini-Hochberg-adjusted change-test results;
- exposed MFH covariance fallback inputs and decisions in output tables;
- stopped the pipeline when input loading fails;
- restricted complete-case filtering to required UFH estimation fields;
- used a shared complete-case mask in the robust MFH2 refit;
- added application version, input hashes, seed, package versions, and session
  information to every run before the report is rendered;
- removed the duplicate dashboard report render;
- added a country/territory field instead of hard-coded Greece labels;
- added explicit external-AI transfer consent, configurable API base URLs,
  neutral prompts, and anonymized geographic ranks/magnitudes;
- strengthened tabular and geometry input validation and ZIP isolation;
- added tests, CI, release-manifest tooling, governance notes, and a clean
  release builder.

This is a release candidate, not an approved production release. See
`docs/REVISION_STATUS.md` and `docs/RELEASE_CHECKLIST.md`.
