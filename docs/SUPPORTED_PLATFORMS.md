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

## How the application is started on each platform

| Platform | Launcher | Notes |
|---|---|---|
| Windows | `Start_Here\Start_Wizard.bat`, `Start_Here\Start_Dashboard.bat` | Double-click. |
| macOS | `Start_Here/Start_Wizard.command`, `Start_Here/Start_Dashboard.command` | Double-click. The first launch may show a security prompt: Control-click the file and choose **Open**. See `Start_Here/README.md`. |
| Linux | the same `.command` files | `bash Start_Here/Start_Wizard.command` from a terminal. |

The `.command` launchers are stored with the executable permission set inside
the release ZIP, so they run after extraction without a `chmod`. Running one
with `bash` also restores that permission if it was lost.

**Test status.** The macOS launchers have been verified on Linux, which shares
their permission and shebang semantics, but not yet on a physical Mac. The
first Mac launch from an extracted release ZIP is a required smoke test (see
`docs/RELEASE_CHECKLIST.md`).
