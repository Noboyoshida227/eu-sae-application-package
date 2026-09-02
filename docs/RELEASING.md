# Making a release

One version, one commit, one folder, one zip. Everything below exists to make
those four things line up automatically, so that any archive anyone has ever
received can be traced to the exact commit that produced it and rebuilt from it.

## The five rules

1. **The version lives in one place.** `WIZARD_VERSION` is the only file you
   edit. `tools/bump_version.py` copies it everywhere else it is embedded — the
   app's title bar, the launchers, the tests, the release inventory, the notes,
   the text inside the slide deck and the guidelines note, and the *names* of
   the two instruction files — and regenerates the download-instructions PDF.
2. **A release is a commit.** `Release.ps1` refuses to build if anything is
   uncommitted, and writes the commit hash into `RELEASE_INFO.txt`. A zip built
   from uncommitted files matches nothing on GitHub and can never be rebuilt.
3. **One version is built once.** The release folder is
   `dist\release_<version>\`; if it already exists, the version must change.
   Two archives with the same name and different contents cannot be told apart
   by the people who receive them.
4. **Every version has a CHANGELOG entry.** `Release.ps1` and the test suite
   both check for a `## <version>` heading in `docs/CHANGELOG.md`.
5. **Tag the commit.** `v<version>` on the commit named in `RELEASE_INFO.txt`;
   publish the GitHub release from that tag with the zip attached.

## The steps

Edit files at the repository root. Never edit anything under `dist\`.

```powershell
cd C:\Users\noboy\Repos\eu-sae-personal

# 1. bump the version everywhere (use --dry-run first to see what it will touch)
python tools\bump_version.py 5.2.0-rc.6-wizard.5-<short-label>

# 2. write the CHANGELOG entry under  ## 5.2.0-rc.6-wizard.5-<short-label> - <date>
#    (docs\CHANGELOG.md - describe what changed and why)

# 3. commit everything in GitHub Desktop, then push

# 4. build
powershell -ExecutionPolicy Bypass -File .\Release.ps1

# 5. tag and push the tag (Release.ps1 prints these with the right values)
git tag -a v<version> -m "EU SAE <version>" <commit>
git push origin v<version>

# 6. on GitHub: Releases -> Draft a new release -> choose the tag,
#    paste the CHANGELOG entry, attach the .zip, quote the SHA-256,
#    tick "pre-release" while the package is a release candidate.
```

Send recipients the `.zip` from `dist\release_<version>\` together with its
SHA-256 from `SHA256SUMS.txt`.

## What the build enforces

`Release.ps1` stops, with a plain-language message, if:

- the working tree has uncommitted changes (use `-AllowDirty` only for a
  private test build — it is labelled *UNCOMMITTED* and must not be sent out);
- `dist\release_<version>\` already exists;
- `docs/CHANGELOG.md` has no entry for the version;
- any file in `scripts/release_inventory.csv` is missing — in particular
  `Data/Spain/survey.rds` and `auxiliary.rds`, which are kept out of the
  repository on purpose (GPL-2) and must be present locally;
- the built archive lacks a package folder, a zip, or the executable bit on the
  macOS/Linux launchers.

## Things that are easy to get wrong

**Adding a new file.** Only files listed in `scripts/release_inventory.csv` go
into a release. A new file that is not listed is silently left out. Add a row.

**Editing a document in more than one place.** Launch instructions appear in
`Start_Here/README.md`, `README.md`, `docs/README_WIZARD.md`,
`docs/SUPPORTED_PLATFORMS.md`, `docs/WIZARD_RELEASE_NOTICE.txt`, the
download-instructions PDF (generated from `tools/build_instruction_pdf.py`),
the user-guide slides, and the guidelines note. `Start_Here/README.md` is the
authoritative one; keep the others short and pointing to it.

**The download-instructions PDF.** Do not edit the PDF. Edit the text in
`tools/build_instruction_pdf.py` and run it (`bump_version.py` runs it for you).

**Building from a fresh clone.** A clone lacks `Data/Spain/survey.rds` and
`auxiliary.rds` by design. Copy them in from a previous release archive before
building; `Release.ps1` will tell you if they are missing.

**Scratch folders.** `tmp\` and `dist\` are ignored by git and are yours to
delete. Nothing in them is needed to rebuild a release.
