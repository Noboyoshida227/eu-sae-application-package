# Start the application

Extract the whole ZIP first, then open this `Start_Here` folder and run the
launcher for your computer. Keep `Start_Here` directly inside the package
folder — the launchers use the folder above as their working directory.

| Your computer | Guided interface (recommended) | Classic dashboard |
|---|---|---|
| **Windows** | `Start_Wizard.bat` | `Start_Dashboard.bat` |
| **macOS** | `Start_Wizard.command` | `Start_Dashboard.command` |
| **Linux** | `bash Start_Wizard.command` | `bash Start_Dashboard.command` |

Double-click the launcher. A terminal window opens, R starts, and your web
browser opens with the application. **Keep that window open while you work** —
closing it stops the application.

The first run takes several minutes while R packages install, and needs an
internet connection. Later runs start much faster.

## Before the first launch

1. Install **R 4.2.0 or later** from <https://cran.r-project.org/>. Use the
   newest R version that your organization has approved and made available;
   installing the absolute latest R release is not required. On a managed
   computer, do not replace your organization's R installation unless IT asks
   you to. On a Mac, choose the `.pkg` installer matching your chip (Apple
   silicon or Intel).
2. For HTML and Word reports, install **one** of these free options. You do not
   need all three:
   - **RStudio Desktop:** <https://posit.co/download/rstudio-desktop/>
   - **Quarto:** <https://quarto.org/docs/get-started/>
   - **Standalone Pandoc:** <https://pandoc.org/installing.html>

The application may open without Pandoc, but report creation will fail. If your
organization limits software or R-package installation, ask IT for an approved
R 4.2+ installation, one option above, and access to the approved R package
repository.

---

## macOS: two things that can block the first launch

**1. "Apple could not verify..." or "unidentified developer".**
macOS marks everything extracted from a downloaded ZIP as untrusted. Right-click
(or Control-click) the launcher, choose **Open**, then confirm **Open** in the
dialog. If your macOS version offers no Open button, go to
 **System Settings → Privacy & Security**, scroll down, and click
**Open Anyway** next to the blocked file. You only do this once per launcher.

**2. Double-clicking does nothing at all.**
The ZIP was built on Windows, which does not record the Unix "executable"
permission, so the launcher may arrive without it. Fix it once, either way:

- Open **Terminal**, type `chmod +x ` (with a trailing space), drag the
  `Start_Here` folder onto the Terminal window, type `/*.command`, and press
  Return. Then double-click the launcher as normal.
- Or open Terminal, type `bash ` (with a trailing space), drag the launcher
  onto the window and press Return. That starts the application *and* repairs
  the permission, so double-clicking works from then on.

---

## If R cannot be found

The launcher searches `PATH`, `R_HOME`, the CRAN framework at
`/Library/Frameworks/R.framework`, Homebrew (`/opt/homebrew`, `/usr/local`) and
the usual system locations. If R lives somewhere unusual, find it with
`which Rscript` in Terminal and start the launcher after setting:

```bash
export EU_SAE_RSCRIPT=/full/path/to/Rscript
```

On Windows the equivalent is `setx EU_SAE_RSCRIPT "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"`.

---

## After a run

Open `../outputs/final_report.html` in a browser, or
`../outputs/final_report.docx` in Word (paths relative to this folder). The Word
text and tables are editable and the figures are embedded — save a separate copy
before editing. If only the HTML appears, check the run log for a
Word-conversion warning.

To make a desktop shortcut, point it at a launcher here; do not move the
launcher out of this folder. See `../docs/README_WIZARD.md` for dependencies and
archived output locations.
