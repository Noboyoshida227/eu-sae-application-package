# Supported-platform status

This candidate is intended for R 4.2 or later on current Windows, macOS, and
Linux systems. It has not yet completed the release checklist's three-platform
smoke test and therefore has no production support claim.

System libraries required by `sf`, Pandoc availability for report rendering,
and CRAN binary availability differ by platform. The release manager must test
installation, dashboard startup, one UFH run, one MFH run, report rendering,
and clean export on each claimed platform.

Report smoke tests must cover both HTML and Word. Word export uses Pandoc,
`xml2`, and `zip`; conversion warnings must be resolved before acceptance.
Successful Windows conversion does not establish macOS/Linux support.
