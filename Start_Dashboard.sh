#!/usr/bin/env bash
# ============================================================
#  EU SAE - compatibility launcher (macOS / Linux)
#
#  The real launchers now live in Start_Here, beside the Windows ones:
#
#      Start_Here/Start_Wizard.command      guided interface (recommended)
#      Start_Here/Start_Dashboard.command   classic dashboard
#
#  On macOS, double-click either of those in Finder.
#  This file is kept only so that older instructions still work; it hands
#  over to the classic dashboard launcher.
# ============================================================

SELF="${BASH_SOURCE[0]:-$0}"
case "$SELF" in
  */*) SELF_DIR="${SELF%/*}" ;;
  *)   SELF_DIR="." ;;
esac
HERE="$(cd -- "$SELF_DIR" >/dev/null 2>&1 && pwd -P)"
TARGET="$HERE/Start_Here/Start_Dashboard.command"

if [ ! -f "$TARGET" ]; then
  echo "ERROR: Start_Here/Start_Dashboard.command was not found."
  echo "Keep this file in the package root, with Start_Here beside it."
  exit 1
fi

echo "Handing over to Start_Here/Start_Dashboard.command ..."
echo ""
exec bash "$TARGET" "$@"
