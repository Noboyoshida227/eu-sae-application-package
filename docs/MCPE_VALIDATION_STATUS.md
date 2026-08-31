# MCPE bootstrap validation status

Date: 2026-08-24  
Build: 5.2.0-rc.6-wizard.3-pointwise

## Purpose and provenance

The package estimates the mean crossed-product error (MCPE) needed for uncertainty about changes over time by parametric bootstrap. Prof. Isabel Molina provided the basic code, Ifeani improved it, and the package maintainer modified it further for MFH1/MFH2/MFH3 dispatch, reproducible seeds, refit handling, diagnostics, and reporting. Responsibility for the adapted implementation and its validation remains with the package team.

## Checks completed in this revision

- Corrected the MFH3 bootstrap data-generating process so the first simulated random effect contains the initial state: `u0 ~ N(0, 1)` and `u1 = rho * u0 + a1`.
- Added a large-sample moment test comparing the empirical MFH3 random-effect covariance with its theoretical covariance. The test uses 100,000 simulations and a 0.02 absolute tolerance.
- Added deterministic tests for the MFH3-selected Molina-Romero procedure and explicit MFH3 non-convergence fallback; MFH2 remains the application default.
- Added run-level Monte Carlo diagnostics: requested and successful replications, attempts, refit failures, covariance-matrix repairs, seed, and Monte Carlo standard errors for MSE and MCPE.
- Confirmed that, in the current two-year workflow, raw, Bonferroni-adjusted, and BH-adjusted MFH reference-variance-test p-values are identical because there is only one variance contrast.
- All targeted package tests passed. The runtime compatibility check for `msae::eblupMFH3()` and the execution test for deterministic LASSO were skipped in the test environment because `msae` and `glmnet` were not installed.

## Validation not yet completed

The completed checks support structural consistency, reproducibility, and monitoring of simulation precision. They do not constitute independent certification of the MCPE estimator. The following work remains:

- an independent end-to-end simulation study for MFH1, MFH2, and MFH3;
- bias and Monte Carlo variability assessment for MSE and MCPE;
- empirical coverage assessment for change confidence intervals;
- assessment of pointwise and multiple-testing error behavior under realistic survey designs;
- runtime compatibility testing with the production versions of `msae` and its dependencies.

## Operational recommendation

This implementation is suitable for internal methodological review with the above limitations disclosed. For production inference, use at least 500 bootstrap replications only after checking Monte Carlo stability, retain the generated MCPE diagnostics, and report sensitivity results when conclusions are close to the significance threshold. Do not describe the current process as independently validated or certified.
