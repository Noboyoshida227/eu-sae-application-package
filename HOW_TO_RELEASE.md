# How to revise the package and make a release

*A plain-language guide for the package maintainer. The compact technical
rulebook is `docs/RELEASING.md`; this note explains the same process in
everyday terms.*

---

## 1. Three places, three jobs

There are three places the package exists, and each has one job.

**The package folder — where you edit.**
`C:\Users\noboy\Repos\eu-sae-personal\`
This is the real thing: the app, the scripts, the documents, the launchers.
Every change you ever make is made here. Think of it as the manuscript.

**The `dist` folder — where finished releases are kept.**
`C:\Users\noboy\Repos\eu-sae-personal\dist\`
Each release gets its own sub-folder, named after its version, containing a
complete copy of the package and the zip file you send out. These are printed
copies of the manuscript, frozen on the day they were made. **You never edit
anything in here.** If you want to change something, edit the manuscript and
print a new copy.

**GitHub — the shared, backed-up record.**
`github.com/Noboyoshida227/eu-sae-application-package`
Every time you commit and push in GitHub Desktop, a snapshot of the package
folder goes here. The release page on GitHub is where colleagues download the
zip. Because each release records which commit it came from, anyone can look at
GitHub later and see exactly what code was in any zip they received.

---

## 2. What is in the package folder

| Item | What it is | Do you edit it? |
|---|---|---|
| `app.R`, `app_wizard.R`, `app_support.R` | The application itself | Only for code changes |
| `R\`, `scripts\` | The statistical engine and pipeline | Only for code changes |
| `Start_Here\` | The launchers users double-click, and their README | Rarely |
| `docs\` | All documentation, including the guidelines note, the instructions, the CHANGELOG | Yes — this is where most revisions happen |
| `Data\` | The two example datasets | Rarely |
| `tests\` | Automatic checks that run on GitHub after every push | Rarely |
| `tools\` | Helper programs, including `bump_version.py` | No |
| `Release.ps1` | The program that builds a release | No |
| `WIZARD_VERSION` | A one-line file holding the current version number | No — `bump_version.py` changes it for you |
| `scripts\release_inventory.csv` | The list of files that go into a release | Only when you add a brand-new file |
| `dist\` | Finished releases (see above) | **Never** |
| `tmp\` | Scratch space used by other tools | Never — safe to delete |

---

## 3. The two helper programs

Both are run from PowerShell, from inside the package folder. Open PowerShell
and type this first, every time:

```powershell
cd C:\Users\noboy\Repos\eu-sae-personal
```

### `tools\bump_version.py` — stamps a new version number everywhere

The version number is written into about fifteen places: the app's title bar,
the launchers, the tests, several notes, the text inside the slide deck and the
guidelines note, and the *file names* of the two instruction documents.
This program changes all of them in one go and regenerates the
download-instructions PDF.

```powershell
python tools\bump_version.py 5.2.0-rc.6-wizard.5-greece
```

The last part is the new version. Keep the pattern `5.2.0-rc.6-wizard.N-label`:
increase `N` by one each release, and choose any short label (no spaces) that
reminds you what the round was about — `greece`, `bugfix`, `october`.

It prints every file it touched. Add `--dry-run` at the end to see what it
*would* change without changing anything.

### `Release.ps1` — builds the release folder and the zip

```powershell
powershell -ExecutionPolicy Bypass -File .\Release.ps1
```

It first checks four things and stops with a plain message if any is wrong:

1. everything is committed in GitHub Desktop;
2. this version has not been built before;
3. `docs\CHANGELOG.md` has a note for this version;
4. every file that should ship is present.

Then it creates `dist\release_<version>\` containing the package copy, the zip,
a checksum file, and `RELEASE_INFO.txt` naming the commit it was built from.
About a minute.

---

## 4. The revision cycle

Every release is the same five moves.

**1. Edit.** Change whatever you need in the package folder. Save.

**2. Stamp the new version.**
```powershell
python tools\bump_version.py 5.2.0-rc.6-wizard.5-greece
```

**3. Write down what changed.** Open `docs\CHANGELOG.md` in any text editor
and add a short section at the very top, under a heading that matches the
version you just stamped:
```
## 5.2.0-rc.6-wizard.5-greece - 2026-09-10

- What you changed, and why, in a few lines. Write it for your colleagues;
  you will paste it into the GitHub release page.
```

**4. Commit and push.** Open GitHub Desktop. It lists every changed file.
Write a one-line summary, click **Commit to main**, then **Push origin**.

**5. Build.**
```powershell
powershell -ExecutionPolicy Bypass -File .\Release.ps1
```
When it finishes, the zip is in `dist\release_5.2.0-rc.6-wizard.5-greece\`.

**Then publish.** On GitHub: **Releases → Draft a new release**. In *Choose a
tag* type `v5.2.0-rc.6-wizard.5-greece` and click *Create new tag on publish*.
Paste your CHANGELOG section as the description, add the SHA-256 from
`SHA256SUMS.txt`, attach the zip, tick *pre-release*, publish. Send colleagues
the link.

---

## 5. Things that come up

**"dist\release_… already exists."**
You already built this version. If nobody has received it yet, delete that one
folder and run `Release.ps1` again. If anyone has it, stamp a new version
instead — two different zips must never share a name.

**"The working tree has uncommitted changes."**
You skipped step 4. Commit in GitHub Desktop and run again.

**"CHANGELOG.md has no '## <version>' heading."**
You skipped step 3, or the heading doesn't match the stamped version exactly.

**I added a brand-new file and it isn't in the zip.**
Only files listed in `scripts\release_inventory.csv` are included. Open that
file (it's a plain list of paths) and add a line for the new file.

**I want to change the download-instructions PDF.**
Don't edit the PDF. Edit the text in `tools\build_instruction_pdf.py`; the
next `bump_version.py` run regenerates the PDF from it.

**Setting up on a new computer.**
After downloading the package from GitHub, copy `Data\Spain\survey.rds` and
`auxiliary.rds` in from any previous release zip. They are kept out of GitHub
on purpose (licensing) and `Release.ps1` will tell you if they are missing.

**PowerShell says scripts are disabled.**
The `-ExecutionPolicy Bypass` part of the `Release.ps1` command handles this.
If a plain `python …` line is refused, run
`Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass` once in that
window.

---

## 6. Two things never to do

- Never edit anything inside `dist\`. Edit the package folder and build again.
- Never rebuild a version that has already been sent to someone. Stamp a new
  one.
