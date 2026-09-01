#!/usr/bin/env bash
# ============================================================
#  EU SAE Wizard - One-click launcher (macOS / Linux)
#
#  macOS:  double-click this file. Terminal opens and the app starts.
#          Your default web browser opens automatically.
#          Keep the Terminal window open while using the app.
#          Press Ctrl+C, or close the window, to stop the app.
#
#  Linux:  run  bash "Start_Here/Start_Wizard.command"  from a terminal.
#
#  If double-clicking does nothing, the executable permission was lost
#  when the ZIP was extracted. Open Terminal and run this once:
#      chmod +x "/path/to/package/Start_Here/"*.command
#  (Type  chmod +x  then drag the Start_Here folder in, add /*.command)
#  Running this file once with  bash  also repairs the permission.
# ============================================================

set -o pipefail

# ---- Locate the package folder (the parent of Start_Here) --------------
# Uses only shell builtins (cd, pwd) so it still works if PATH is minimal,
# which is exactly the situation a double-clicked .command starts in.
SELF="${BASH_SOURCE[0]:-$0}"
case "$SELF" in
  */*) SELF_DIR="${SELF%/*}" ;;
  *)   SELF_DIR="." ;;
esac
SCRIPT_DIR="$(cd -- "$SELF_DIR" >/dev/null 2>&1 && pwd -P)"
if [ -z "$SCRIPT_DIR" ]; then
  echo "ERROR: Cannot determine the folder this launcher lives in."
  exit 1
fi
PKG_DIR="$(cd -- "$SCRIPT_DIR/.." >/dev/null 2>&1 && pwd -P)"
if [ -z "$PKG_DIR" ]; then
  echo "ERROR: Cannot open the package folder above Start_Here."
  exit 1
fi
cd "$PKG_DIR" || exit 1

# ---- Self-repair: make the launchers double-clickable next time --------
# A ZIP built on Windows carries no Unix permission bits, so the .command
# files arrive without the executable flag and Finder refuses to run them.
# Running this script by any route fixes both launchers permanently.
chmod +x "$SCRIPT_DIR"/*.command 2>/dev/null || true

# ---- Pause helper: keep the Terminal window readable on error ----------
IS_DOUBLE_CLICK=0
if [ -t 0 ] && [ -t 1 ]; then IS_DOUBLE_CLICK=1; fi
pause_on_exit() {
  if [ "$EU_SAE_LAUNCHER_CHECK_ONLY" = "1" ]; then return 0; fi
  if [ "$IS_DOUBLE_CLICK" = "1" ]; then
    echo ""
    printf 'Press Return to close this window... '
    read -r _dummy || true
  fi
}

# ---- Check the application files before looking for R ------------------
MISSING=""
for F in "app.R" "app_support.R" "install_packages.R" "report.Rmd" \
         "R/input_readers.R" "app_wizard.R"; do
  if [ ! -f "$F" ]; then MISSING="$MISSING  $F"; fi
done
if [ -n "$MISSING" ]; then
  echo "ERROR: Required package file(s) missing:"
  for M in $MISSING; do echo "    $M"; done
  echo ""
  echo "Keep Start_Here inside the complete extracted package folder."
  echo "Current package folder: $PKG_DIR"
  echo ""
  echo "If you extracted only part of the ZIP, extract the whole archive"
  echo "again and run the launcher from inside the extracted folder."
  pause_on_exit
  exit 1
fi

echo "============================================"
echo "   EU SAE Application - Guided Setup (Wizard)"
echo "============================================"
echo ""
echo "Starting up..."
echo "Your web browser will open automatically in a few moments."
echo "(First run may take several minutes while packages install.)"
echo ""
echo "IMPORTANT: Keep this window open while using the application."
echo "Press Ctrl+C, or close this window, to stop it."
echo ""
echo "--------------------------------------------"
echo ""

# ---- Locate Rscript ----------------------------------------------------
RSCRIPT=""

# (1) Explicit override, for managed computers where R sits somewhere odd.
#     Example:  export EU_SAE_RSCRIPT="/opt/R/4.5.2/bin/Rscript"
if [ -n "$EU_SAE_RSCRIPT" ] && [ -x "$EU_SAE_RSCRIPT" ]; then
  RSCRIPT="$EU_SAE_RSCRIPT"
fi

# (2) R_HOME, if configured.
if [ -z "$RSCRIPT" ] && [ -n "$R_HOME" ] && [ -x "$R_HOME/bin/Rscript" ]; then
  RSCRIPT="$R_HOME/bin/Rscript"
fi

# (3) PATH. Note: a double-clicked .command gets a minimal PATH that often
#     excludes /usr/local/bin and /opt/homebrew/bin, so this frequently
#     fails on macOS even when Rscript works fine in Terminal. Steps 4-6
#     are what actually find R in that case.
if [ -z "$RSCRIPT" ] && command -v Rscript >/dev/null 2>&1; then
  RSCRIPT="$(command -v Rscript)"
fi

# (4) The CRAN macOS installer's stable symlink.
if [ -z "$RSCRIPT" ] && [ -x "/Library/Frameworks/R.framework/Resources/bin/Rscript" ]; then
  RSCRIPT="/Library/Frameworks/R.framework/Resources/bin/Rscript"
fi

# (5) Homebrew (Apple Silicon, then Intel) and other common prefixes.
if [ -z "$RSCRIPT" ]; then
  for CAND in /opt/homebrew/bin/Rscript /usr/local/bin/Rscript \
              /usr/bin/Rscript /opt/local/bin/Rscript /snap/bin/Rscript; do
    if [ -x "$CAND" ]; then RSCRIPT="$CAND"; break; fi
  done
fi

# (6) A specific version inside the macOS R framework - highest version wins.
if [ -z "$RSCRIPT" ]; then
  BEST=""
  for CAND in /Library/Frameworks/R.framework/Versions/*/Resources/bin/Rscript; do
    [ -x "$CAND" ] || continue
    if [ -z "$BEST" ]; then
      BEST="$CAND"
    else
      HIGH="$(printf '%s\n%s\n' "$BEST" "$CAND" | sort -V 2>/dev/null | tail -n 1)"
      if [ -n "$HIGH" ]; then BEST="$HIGH"; else BEST="$CAND"; fi
    fi
  done
  RSCRIPT="$BEST"
fi

if [ -z "$RSCRIPT" ]; then
  echo "ERROR: R is not installed on this computer, or could not be found."
  echo ""
  echo "The launcher searched PATH, R_HOME, the CRAN R framework in"
  echo "/Library/Frameworks/R.framework, Homebrew (/opt/homebrew, /usr/local)"
  echo "and the usual system locations."
  echo ""
  echo "Please install R 4.2.0 or later from:  https://cran.r-project.org/"
  echo "On a Mac, choose the .pkg installer that matches your chip"
  echo "(Apple silicon or Intel). Then run this launcher again."
  echo ""
  echo "If R is already installed somewhere unusual, find it with"
  echo "    which Rscript"
  echo "in Terminal, then run this launcher after setting, for example:"
  echo "    export EU_SAE_RSCRIPT=/full/path/to/Rscript"
  pause_on_exit
  exit 1
fi

echo "Using R at: $RSCRIPT"
echo ""

if [ "$EU_SAE_LAUNCHER_CHECK_ONLY" = "1" ]; then
  echo "Launcher check only requested. R and required application files were found."
  echo "Package root: $PKG_DIR"
  exit 0
fi

# ---- Launch -------------------------------------------------------------
#  - Sources install_packages.R if present (installs missing packages on
#    first run).
#  - Then sources app_wizard.R directly. That file contains its own
#    non-interactive launcher, which prefers port 7788 and falls back to
#    the next free local port. Sourcing it rather than calling
#    shiny::runApp(appDir=) avoids a double-runApp nesting that breaks
#    static asset serving (www/eu_poverty_map.png and friends).
"$RSCRIPT" -e "if (file.exists('install_packages.R')) source('install_packages.R'); source('app_wizard.R')"
APP_EXIT=$?

if [ "$APP_EXIT" -ne 0 ]; then
  echo ""
  echo "--------------------------------------------"
  echo "The application exited with an error (code $APP_EXIT)."
  echo "--------------------------------------------"
  echo ""
  echo "If packages failed to install, check your internet connection and"
  echo "try again. On macOS, spatial packages (sf, terra) install as ready-"
  echo "made binaries from CRAN; if R tries to compile them from source,"
  echo "answer 'no' when it asks about source installs."
  pause_on_exit
fi

exit "$APP_EXIT"
