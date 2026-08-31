# Contributing

Thank you for trying the EU SAE Application Package. This tool produces
small-area estimates of poverty and welfare indicators from household survey
data, and the people who use it are producing numbers that matter, so the most
valuable contribution is usually a careful report of something that behaved
unexpectedly.

## Status

This is a **release candidate**. Estimates it produces should be treated as
experimental and put through your usual validation before any publication.
Open items are recorded in `docs/RELEASE_CHECKLIST.md`.

## Reporting a problem

Open an issue and tell us four things:

1. **What you did** -- the wizard step or dashboard control, and the settings.
2. **What you expected** to happen.
3. **What happened** instead, with the exact error text if there was one.
4. **The run label**, shown in the report and in the run folder name
   (`app_runs/<timestamp>_<label>/`).

Attaching `run.log` and `app_config.yml` from that run folder tells us far more
than a description alone. Please do not attach real microdata, and check that
your `app_config.yml` does not contain a file path you would rather not share.

## Suggesting a methodological change

Changes to estimation, variance, or inference need more than a pull request:
say which published method you are following, what the current code does
differently, and what evidence supports the change. Method changes are reviewed
against `docs/MCPE_VALIDATION_STATUS.md` and the release checklist before they
are accepted.

## Making a code change

1. Run the test suite first, so you know it passed before your change:
   `Rscript tests/run_tests.R`
2. Keep the two interfaces in step. `app.R` (dashboard) and `app_wizard.R`
   (wizard) share one analysis engine and one set of control IDs, enforced by
   `tools/check_ui_parity.R`. A control added to one belongs in both.
3. Do not commit generated material -- `outputs/`, `app_runs/`, reports or
   figures. `.gitignore` covers these; if something slips through, that is a
   bug in the ignore rules worth reporting.
4. Run the tests again, and say in the pull request what you ran and what you
   saw.

## Licence

Code in this repository is MIT licensed (see `LICENSE`). Some example data
carries other terms and is documented in `THIRD_PARTY_NOTICES.md`; the GPL-2
Spain survey and auxiliary files are distributed with the release archive
rather than in this repository, so that the repository itself stays
unambiguously MIT. By contributing, you agree that your contribution is
licensed under the MIT licence.

## Contact

Open an issue for anything about the package. For questions about this
organization's repositories, write to github@worldbank.org.
