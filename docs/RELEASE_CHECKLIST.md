# Release checklist

A release manager must record evidence for every item before changing the
candidate label to a production version.

- [ ] Method owner approves the variance-smoothing policy and simulations.
- [ ] Method owner approves UFH period-independence inference or supplies a
      validated cross-period covariance estimator.
- [ ] MCPE stability study supports the selected `nB` for every MFH variant.
- [ ] Boundary and positive-definiteness cases pass statistical QA datasets.
- [ ] Nonsampled-domain policy is approved and tested.
- [ ] CV/quality thresholds and publication actions are institutionally approved.
- [x] Replacement Spain map has pinned IGN/CNIG sources, CC BY 4.0 attribution, and an offline generator.
- [ ] Example data provenance, permission, disclosure risk, and license are approved (survey derivation remains missing).
- [ ] Documentation and third-party content rights are approved.
- [ ] Git history, tags, GitHub release assets, caches, forks and mirrors are
      remediated as applicable; a clean re-clone passes the object audit in
      `docs/HISTORY_REMEDIATION.md`.
- [ ] `NOTICE`, `THIRD_PARTY_NOTICES.md`, the non-code rights inventory and
      institutional release authority are approved.
- [ ] AI transfer has a lawful basis, approved providers/endpoints, retention
      terms, and a reviewed privacy notice; otherwise AI remains disabled.
- [ ] Windows, macOS, and Linux smoke tests pass on the supported matrix.
- [ ] At least one managed-environment test uses an organization-approved R
      4.2+ version that is not the newest available R release.
- [ ] `Rscript tests/run_tests.R` passes in the release environment.
- [ ] `Rscript scripts/check_dependency_lock.R` passes.
- [ ] A clean distribution is built with `scripts/build_clean_release.R`.
- [ ] Release manifest/checksums are regenerated and independently verified.
- [ ] The Git tag exactly matches VERSION and uses one canonical case.
- [ ] Only the history-free folder produced by `scripts/build_clean_release.R`
      is archived; the source workspace itself is not zipped.
- [ ] Release notes list all known limitations and methodology assumptions.

## Report-format verification

- [ ] A representative completed run creates both `outputs/final_report.html`
      and `outputs/final_report.docx`; Word-conversion warnings are resolved.
- [ ] Word text/tables are editable, figure/table values match HTML, and wide
      tables and figure captions are readable.
- [ ] Estimated-change and CI-width plots are present and clearly distinguished.
- [ ] AI status labels and review warnings appear in both formats.
- [ ] One Pandoc provider (RStudio Desktop, Quarto, or standalone Pandoc), plus
      `xml2` and `zip`, is available on each supported platform.
- [ ] Generated reports and run histories are excluded from the clean ZIP;
      documentation, manifest, and ZIP checksums describe the same candidate.
