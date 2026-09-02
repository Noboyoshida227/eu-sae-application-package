#!/usr/bin/env python3
"""
tools/bump_version.py - change the package version in ONE command.

    python tools/bump_version.py 5.2.0-rc.6-wizard.4-crossplatform
    python tools/bump_version.py 5.2.0-rc.6-wizard.4-crossplatform --dry-run

The version lives in WIZARD_VERSION, but it is also embedded in the app's
title bar, the launcher, the tests, the release inventory, several notes, the
two instruction files' NAMES, and the text inside the slide deck, the
guidelines note and the download-instructions PDF. Changing it by hand means
finding every one of those, and missing one produces a release whose parts
disagree about what version they are.

This script does all of it, or - if any prerequisite is missing - none of it.

What it changes
  1. WIZARD_VERSION
  2. Every tracked text file that contains the old version, in either form
     ("5.2.0-rc.6-wizard.3-pointwise" or "5_2_0_rc_6_wizard_3_pointwise").
     docs/CHANGELOG.md is deliberately excluded: it is history.
  3. Renames docs/instructions/*_<old token>.pdf / .pptx to the new token.
  4. Rewrites the version text inside the slide deck (.pptx) and the
     guidelines note (.docx) - these are ZIP files of XML underneath.
  5. Regenerates the download-instructions PDF via tools/build_instruction_pdf.py
     (skip with --skip-pdf if reportlab is unavailable here; regenerate later).

What it does NOT do
  - It does not write the CHANGELOG entry. Add one under "## <new version>"
    before building; the test suite checks that it exists.
  - It does not commit. Review the diff, then commit.
"""
import argparse, io, os, re, shutil, subprocess, sys, zipfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
os.chdir(ROOT)

TEXT_EXT = {".R", ".Rmd", ".md", ".txt", ".csv", ".bat", ".command", ".sh",
            ".yml", ".yaml", ".json", ".ps1", ".py", ".lua", ".gitattributes"}
SKIP_DIRS = {".git", "dist", "tmp", "outputs", "app_runs", "r_local_library",
             "node_modules", ".Rproj.user", ".claude"}
SKIP_FILES = {"docs/CHANGELOG.md",           # history - never rewritten
              "tools/bump_version.py"}       # this file quotes versions as examples
OFFICE_TARGETS = [                            # (path pattern, XML parts to patch)
    ("docs/instructions/EU_SAE_User_Guide_{tok}.pptx", r"ppt/slides/slide\d+\.xml"),
    ("docs/guidance/guidelines_v5_2_0_rc6_wizard.docx", r"word/document\.xml"),
]
RENAME_TARGETS = [
    "docs/instructions/EU_SAE_Download_Instructions_{tok}.pdf",
    "docs/instructions/EU_SAE_User_Guide_{tok}.pptx",
]

def token(v):  return re.sub(r"[.-]", "_", v)
def say(msg):  print(msg)
def die(msg):  print("ERROR:", msg); sys.exit(1)

def tracked_text_files():
    for dp, dns, fns in os.walk("."):
        dns[:] = [d for d in dns if d not in SKIP_DIRS and not d.startswith(".git")]
        for f in fns:
            p = Path(dp, f)
            rel = p.as_posix().lstrip("./")
            if rel in SKIP_FILES: continue
            if p.suffix in TEXT_EXT or p.name in ("WIZARD_VERSION", "VERSION", ".gitattributes", ".gitignore"):
                yield p

def patch_office_zip(path, part_pattern, old, new, dry):
    """Rewrite selected XML parts inside a .pptx/.docx, leaving everything else byte-identical."""
    src = zipfile.ZipFile(path)
    buf = io.BytesIO()
    changed = 0
    with zipfile.ZipFile(buf, "w") as dst:
        for info in src.infolist():
            data = src.read(info.filename)
            if re.fullmatch(part_pattern, info.filename):
                n = data.count(old.encode("utf-8"))
                if n:
                    data = data.replace(old.encode("utf-8"), new.encode("utf-8"))
                    changed += n
            dst.writestr(info, data, compress_type=info.compress_type)
    src.close()
    if changed and not dry:
        Path(path).write_bytes(buf.getvalue())
    return changed

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("new_version")
    ap.add_argument("--dry-run", action="store_true", help="report what would change, touch nothing")
    ap.add_argument("--skip-pdf", action="store_true", help="do not regenerate the PDF (reportlab absent here)")
    a = ap.parse_args()

    old = Path("WIZARD_VERSION").read_text(encoding="utf-8").strip()
    new = a.new_version.strip()
    if not re.fullmatch(r"[A-Za-z0-9._-]+", new): die(f"invalid version {new!r} (letters, digits, . _ - only)")
    if new == old: die(f"WIZARD_VERSION is already {old}")
    old_tok, new_tok = token(old), token(new)
    dry = a.dry_run
    say(f"{'DRY RUN - ' if dry else ''}{old}  ->  {new}")
    say(f"{'          ' if dry else ''}{old_tok}  ->  {new_tok}\n")

    # ---- prerequisites: fail before touching anything -------------------
    if not a.skip_pdf:
        try:
            import reportlab  # noqa: F401
        except ImportError:
            die("reportlab is not installed, so the PDF cannot be regenerated.\n"
                "       pip install reportlab   - or -   re-run with --skip-pdf and regenerate the PDF elsewhere.")
    for pat in RENAME_TARGETS:
        p = Path(pat.format(tok=old_tok))
        if not p.exists(): die(f"expected file not found: {p}")
        if Path(pat.format(tok=new_tok)).exists(): die(f"target already exists: {pat.format(tok=new_tok)}")
    for pat, _ in OFFICE_TARGETS:
        if not Path(pat.format(tok=old_tok)).exists(): die(f"expected file not found: {pat.format(tok=old_tok)}")

    # ---- 1 + 2. text files ------------------------------------------------
    say("Text files:")
    n_files = 0
    for p in sorted(tracked_text_files()):
        b = p.read_bytes()
        n1, n2 = b.count(old.encode()), b.count(old_tok.encode())
        if not (n1 or n2): continue
        nb = b.replace(old.encode(), new.encode()).replace(old_tok.encode(), new_tok.encode())
        if not dry: p.write_bytes(nb)
        say(f"  {p.as_posix():<62} {n1 + n2:>3} replacement(s)")
        n_files += 1
    if n_files == 0: die("no text file contained the old version - is WIZARD_VERSION out of step with the tree?")

    # ---- 4. office documents ----------------------------------------------
    say("\nOffice documents (text inside the file):")
    for pat, part in OFFICE_TARGETS:
        p = pat.format(tok=old_tok)
        n = patch_office_zip(p, part, old, new, dry)
        say(f"  {p:<62} {n:>3} replacement(s)")

    # ---- 3. renames ---------------------------------------------------------
    say("\nRenamed:")
    for pat in RENAME_TARGETS:
        src, dst = pat.format(tok=old_tok), pat.format(tok=new_tok)
        if not dry: os.rename(src, dst)
        say(f"  {src}\n    -> {dst}")

    # ---- 5. regenerate the PDF ---------------------------------------------
    if a.skip_pdf:
        say("\nPDF: SKIPPED (--skip-pdf). The renamed PDF still shows the OLD version inside.")
        say("     Run  python tools/build_instruction_pdf.py  where reportlab is installed before releasing.")
    elif dry:
        say("\nPDF: would regenerate via tools/build_instruction_pdf.py")
    else:
        say("\nPDF:")
        r = subprocess.run([sys.executable, "tools/build_instruction_pdf.py"], capture_output=True, text=True)
        if r.returncode != 0: die("PDF regeneration failed:\n" + r.stdout + r.stderr)
        say("  " + r.stdout.strip().replace("\n", "\n  "))

    # ---- leftover scan: partial or truncated forms of the OLD version --------
    # e.g. a launcher header saying "5.2.0-rc.6-wizard.3" without its suffix.
    old_base = re.sub(r"-[A-Za-z]+$", "", old)          # 5.2.0-rc.6-wizard.3-pointwise -> 5.2.0-rc.6-wizard.3
    needles = {old_base, token(old_base)} - {new, new_tok}
    leftovers = []
    if old_base != old:
        for p in sorted(tracked_text_files()):
            b = p.read_bytes() if not dry else p.read_bytes()
            for nd in needles:
                for m in re.finditer(re.escape(nd).encode() + rb"(?![\w.-])", b):
                    line = b.count(b"\n", 0, m.start()) + 1
                    leftovers.append((p.as_posix(), line, nd))
    if leftovers:
        say("\nWARNING - old version still appears in a truncated form; fix these by hand:")
        for f, ln, nd in leftovers: say(f"  {f}:{ln}  contains  {nd}")

    # ---- what is left for a human ------------------------------------------
    say(f"\nDone. {'Nothing was changed.' if dry else 'Now:'}")
    if not dry:
        say(f"  1. Add an entry to docs/CHANGELOG.md under:   ## {new} - <date>")
        say(f"  2. Review the changes in GitHub Desktop and commit.")
        say(f"  3. Run Release.ps1 - it builds dist\\release_{new}\\")

if __name__ == "__main__":
    main()
