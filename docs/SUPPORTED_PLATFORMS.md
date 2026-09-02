# Supported-platform status

This review candidate is intended for R 4.2 or later on current Windows,
macOS, and Linux systems. Users may use the newest R version that their
organization has approved and made available; installing the absolute latest R
release is not required. Each R/platform combination still needs a smoke test,
because CRAN binary availability and system-library requirements differ. This
candidate has not yet completed that test matrix and therefore has no
production support claim.

System libraries required by `sf` and CRAN binary availability differ by
platform. Report rendering requires one free Pandoc provider: RStudio Desktop,
Quarto, or standalone Pandoc. The release manager must test
installation, dashboard startup, one UFH run, one MFH run, report rendering,
and clean export on each claimed platform.

Report smoke tests must cover both HTML and Word. Word export uses Pandoc,
`xml2`, and `zip`; conversion warnings must be resolved before acceptance.
Successful Windows conversion does not establish macOS/Linux support.
