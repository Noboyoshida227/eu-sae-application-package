# Start the application (Windows)

- **Start_Wizard.bat**: recommended guided interface.
- **Start_Dashboard.bat**: classic dashboard.

Double-click either launcher after extracting the whole ZIP. Keep this
`Start_Here` folder directly inside the package folder. The launchers use the
parent package folder as their working directory, so all application, data,
report and output paths remain unchanged.

Keep the launcher window open while using the application. To create a desktop
shortcut, point it to a launcher here; do not move the launcher out of this folder.

For macOS/Linux, run `bash Start_Dashboard.sh` from the package root instead.
There is no supplied wizard shell launcher.

After a completed run, open `../outputs/final_report.html` in a browser or
`../outputs/final_report.docx` in Word (paths relative to this folder). Word
text/tables are editable and figures are embedded. Save a separate copy before
editing. If only HTML appears, inspect the run log for a Word-conversion warning.
See `../docs/README_WIZARD.md` for dependencies and archived output locations.
