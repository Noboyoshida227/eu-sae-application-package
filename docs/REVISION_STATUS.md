# Revision status against the merged issue register

Baseline: EU SAE application package v5.1.0. Candidate: 5.2.0-rc.6.

`Implemented` means the candidate contains a code or documentation change for
the finding; it is not a production-method approval. `Partial` identifies work
done but an unresolved decision or validation. `Open` means no substantive
closure is claimed.

| ID | Status | Disposition in 5.2.0-rc.6 |
|---|---|---|
| B1 | Implemented | LASSO folds use and record a deterministic analysis seed. |
| B2 | Implemented | Removed the unreachable covariance branch; UFH change inference labels period independence and exports raw and BH-adjusted p-values. |
| B3 | Partial | Removed the absolute 0.001 cutoff. Positive lower-tail replacement is disabled by default; any multiplier still needs method-owner approval and simulation. |
| B4 | Implemented | AI prompts are evidence-conditional and no longer assert fixed conclusions or eight combinations. |
| B5 | Partial | Added explicit external-transfer consent, prompt minimization, a prompt-injection guard and configurable gateway URLs. Lawful basis, provider/retention terms and institutional approval remain open. |
| B6 | Partial—release blocker | Non-inventory literature files are excluded. The candidate includes only listed Spain RDS and simulated examples; the replacement Spain map has documented IGN/CNIG CC BY 4.0 provenance; the survey derivation remains missing. Public Git history/release assets remain unremediated and non-code rights remain unapproved. |
| B7 | Implemented | Added targeted tests and CI; this is code QA, not statistical validation. |
| B8 | Implemented | Removed hard-coded Greece labels and added a country/territory input. |
| H1 | Implemented | Input-load failure is blocking. |
| H2 | Partial | MCPE `nB` is configurable, recorded and defaults to 200; representative stability studies are still required. |
| H3 | Partial | MFH change outputs retain independence MSE, covariance-adjusted MSE, covariance estimate and fallback rule; the estimator/fallback needs method validation. |
| H4 | Partial | UFH/MFH results include BH adjustment and explicit variance handling; the multiplicity family and covariance policy need approval. |
| H5 | Open | Transformation/back-transformation policy requires method-owner review and simulation. |
| H6 | Open | Boundary behavior and resulting uncertainty require method-owner decisions and QA cases. |
| H7 | Open | Nonsampled-domain behavior requires an approved policy and validation. |
| H8 | Open | MSE-estimator choice and adequacy require comparative statistical validation. |
| H9 | Implemented | Run metadata and input hashes are captured before one final report render. MFH inversion, robust-refit, condition-number-method, and g3/MSE availability diagnostics are added after the MFH step and explicitly printed in the report. |
| H10 | Implemented | UFH complete-case filtering uses required estimation fields and logs excluded domain IDs. |
| H11 | Partial | Diagnostics are more visible, but no institutionally approved warning/action policy or thresholds exist. |
| H12 | Implemented | Delimited inputs default to UTF-8 with a documented override. |
| H13 | Partial | Quality indicators are exported, but publication thresholds and actions remain unapproved. |
| H14 | Implemented | Duplicate rendering was removed; the final report follows completed run metadata. |
| H15 | Implemented | Missing normality p-values are reported as unavailable rather than failed. |
| H16 | Partial | A direct dependency snapshot/check exists; reproducible cross-platform restoration remains open. |
| H17 | Implemented | Added clean-release tooling, manifests/checksums and complete output placeholders. |
| H18 | Implemented | Robust MFH2 uses guarded symmetric inversions. Cholesky paths record a labeled low-cost condition-number estimate; fallback paths record an exact value when available. If Fisher information cannot be inverted, g3 and MSE are reported unavailable with a warning; zero is no longer silently substituted. Structured diagnostics are written to the run log, metadata, a CSV table and the final report. Numerical validation remains part of H5/H8. |
| H19 | Open | Remote branches/tags and publication workflow require repository-owner action. |
| H20 | Implemented | Added VERSION/changelog and replaced country-specific labels. |
| H21 | Implemented | README plus DOCX/PDF/PPTX were refreshed for 5.2.0-rc.6, clean-build contents, reproducibility controls, pointwise inference and embedded AI interpretation; superseded binaries are excluded by the explicit release inventory. |
| H22 | Open—release blocker | Existing Git history remains large and retains prohibited/superseded binaries. Follow `docs/HISTORY_REMEDIATION.md`; no history rewrite is claimed. |
| M1 | Open | Robustness and estimator behavior still need representative numerical/statistical validation. |
| M2 | Open | No approved change to the issue's method/default policy is claimed. |
| M3 | Implemented | Clean exports omit run histories and input-file copies. |
| M4 | Implemented | Convenience setup copies are made only by the explicit **Save Current Setup** action and are excluded from clean exports. |
| M5 | Partial | Added R-object/type, duplicate-column and ZIP-isolation checks; a full adversarial input review remains open. |
| M6 | Open | No substantive closure is claimed; requires owner decision and validation. |
| M7 | Open | No substantive closure is claimed; requires owner decision and validation. |
| M8 | Open | No substantive closure is claimed; requires owner decision and validation. |
| M9 | Implemented | Approved API gateways can be configured without code changes; provider approval remains under B5. |
| M10 | Open | No substantive closure is claimed; requires owner decision and validation. |
| M11 | Open | No substantive closure is claimed; requires owner decision and validation. |
| M12 | Open | Full UI translation/localization coverage has not been completed. |
| M13 | Implemented | README and the explicit release inventory describe required inputs and include the listed Spain and simulated examples for learning and testing only. |
| M14 | Open | No substantive closure is claimed; requires method-owner validation. |
| M15 | Open | No substantive closure is claimed; requires method-owner validation. |
| M16 | Open | No substantive closure is claimed; requires method-owner validation. |
| M17 | Partial—release blocker | Added scoped LICENSE, NOTICE, third-party inventory and governance file. Institutional copyright/release authority, asset ownership and disclaimer approval remain unresolved. |
| M18 | Implemented | Robust MFH2 refit now applies one shared complete-case mask. |
| L1 | Implemented | Bootstrap time-loop construction now handles `nT = 1`; convergence checks use `isTRUE`. |
| L2 | Open | Canonical remote tag/case cleanup remains repository-owner work. |
| L3 | Open | No substantive closure is claimed. |
| L4 | Implemented | Root junk is excluded from the clean build and covered by ignore/release rules. |
| L5 | Open | No substantive closure is claimed. |
| L6 | Partial | Metadata for the revised guidance and technical notes was refreshed. Ownership fields in unchanged documents and institutional metadata policy still require rights-holder decisions. |
| L7 | Open | Full accessibility remediation remains incomplete. |
| L8 | Open | Remaining statistical/documentation convention requires owner decision. |

## Public-sharing gate

The history-free clean folder may be used for controlled review. Public sharing
remains blocked by B6/H22 (public history and release assets) and M17 (document,
asset and institutional release authority). Passing tests and checksums does not
approve estimators, privacy basis, data rights or official statistical use.
