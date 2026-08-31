@echo off
setlocal DisableDelayedExpansion
REM ============================================================
REM  EU SAE Dashboard 5.2.0-rc.6-wizard.3 - One-click launcher (Windows)
REM
REM  Double-click this file to start the step-by-step (wizard) dashboard.
REM  Your default web browser will open with the dashboard.
REM  Keep this window open while using the dashboard.
REM  Close this window to stop the dashboard.
REM ============================================================

title EU SAE Dashboard - Wizard
REM Start_Here must remain directly inside the extracted package folder.
cd /d "%~dp0.." || (
  echo ERROR: Cannot open the package folder above Start_Here.
  if /I not "%EU_SAE_LAUNCHER_CHECK_ONLY%"=="1" pause
  exit /b 1
)
REM Check the application paths before finding R or installing any packages.
for %%F in ("app.R" "app_support.R" "install_packages.R" "report.Rmd" "R\input_readers.R" "app_wizard.R") do (
  if not exist "%%~F" (
    echo ERROR: Required package file missing: %%~F
    echo Keep Start_Here inside the complete extracted package folder.
    echo Current package folder: "%CD%"
    if /I not "%EU_SAE_LAUNCHER_CHECK_ONLY%"=="1" pause
    exit /b 1
  )
)

echo ============================================
echo    EU SAE Dashboard - Guided Setup
echo ============================================
echo.
echo Starting up...
echo Your web browser will open automatically in a few moments.
echo (First run may take several minutes while packages install.)
echo.
echo IMPORTANT: Keep this window open while using the dashboard.
echo Close this window to stop the dashboard.
echo.
echo --------------------------------------------
echo.

REM ---- Locate Rscript.exe -----------------------------------
set "RSCRIPT="

REM (1) Allow an explicit override for managed/company computers.
REM     Example:
REM       setx EU_SAE_RSCRIPT "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
if defined EU_SAE_RSCRIPT (
  if exist "%EU_SAE_RSCRIPT%" set "RSCRIPT=%EU_SAE_RSCRIPT%"
)

REM (2) Try R_HOME if it is configured.
if not defined RSCRIPT (
  if defined R_HOME call :try_r_root "%R_HOME%"
)

REM (3) Try PATH.
for /f "delims=" %%I in ('where Rscript.exe 2^>nul') do (
  if not defined RSCRIPT set "RSCRIPT=%%I"
)

REM (4) Try R.exe on PATH and look next to it for Rscript.exe.
if not defined RSCRIPT (
  for /f "delims=" %%I in ('where R.exe 2^>nul') do (
    if not defined RSCRIPT if exist "%%~dpIRscript.exe" set "RSCRIPT=%%~dpIRscript.exe"
  )
)

REM (5) Try R registry install paths. These are often present even when
REM     Rscript.exe is not on PATH.
if not defined RSCRIPT (
  for %%K in ("HKCU\Software\R-core\R" "HKLM\Software\R-core\R" "HKLM\Software\WOW6432Node\R-core\R") do (
    for /f "tokens=2,*" %%A in ('reg query %%~K /v InstallPath 2^>nul ^| findstr /I "InstallPath"') do (
      if not defined RSCRIPT call :try_r_root "%%B"
    )
  )
)

REM (6) Try common install locations. /O-N checks higher folder names first.
if not defined RSCRIPT (
  for /f "delims=" %%D in ('dir /b /ad /o-n "%ProgramFiles%\R\R-*" 2^>nul') do (
    if not defined RSCRIPT call :try_r_root "%ProgramFiles%\R\%%D"
  )
)
if not defined RSCRIPT (
  for /f "delims=" %%D in ('dir /b /ad /o-n "%SystemDrive%\Program Files (x86)\R\R-*" 2^>nul') do (
    if not defined RSCRIPT call :try_r_root "%SystemDrive%\Program Files (x86)\R\%%D"
  )
)
if not defined RSCRIPT (
  if defined LOCALAPPDATA (
    for /f "delims=" %%D in ('dir /b /ad /o-n "%LOCALAPPDATA%\Programs\R\R-*" 2^>nul') do (
      if not defined RSCRIPT call :try_r_root "%LOCALAPPDATA%\Programs\R\%%D"
    )
  )
)

REM (7) Last resort: use PowerShell to search common roots recursively.
if not defined RSCRIPT (
  for /f "delims=" %%I in ('powershell -NoProfile -ExecutionPolicy Bypass -Command "$roots = @($env:R_HOME, ''C:\Program Files\R'', ''C:\Program Files (x86)\R'', \"$env:LOCALAPPDATA\Programs\R\") | Where-Object { $_ }; $candidates = foreach ($root in $roots) { if (Test-Path -LiteralPath $root) { Get-ChildItem -LiteralPath $root -Directory -Filter ''R-*'' -ErrorAction SilentlyContinue | ForEach-Object { foreach ($rel in @(''bin\Rscript.exe'', ''bin\x64\Rscript.exe'')) { $exe = Join-Path $_.FullName $rel; if (Test-Path -LiteralPath $exe) { $ver = $_.Name -replace ''^R-'', ''''; try { $v = [version]$ver } catch { $v = [version]''0.0.0'' }; [pscustomobject]@{ Version = $v; Exe = $exe } } } } } }; $candidates | Sort-Object Version -Descending | Select-Object -First 1 -ExpandProperty Exe" 2^>nul') do (
    if not defined RSCRIPT set "RSCRIPT=%%I"
  )
)

if not defined RSCRIPT (
  echo ERROR: R is not installed on this computer, or could not be found.
  echo.
  echo The launcher searched PATH, R_HOME, the Windows registry,
  echo Program Files, Program Files ^(x86^), and your local AppData R folder.
  echo.
  echo Please install R 4.2.0 or later from:  https://cran.r-project.org/
  echo Then double-click this file again.
  echo.
  echo If R is already installed in a company-specific folder, ask IT for
  echo the full path to Rscript.exe and set EU_SAE_RSCRIPT to that path.
  echo.
  pause
  exit /b 1
)

echo Using R at: %RSCRIPT%
echo.

if /I "%EU_SAE_LAUNCHER_CHECK_ONLY%"=="1" (
  echo Launcher check only requested. R and required application files were found.
  echo Package root: "%CD%"
  exit /b 0
)

REM ---- Launch the dashboard ---------------------------------
REM  - Sources install_packages.R if present (installs missing packages on first run)
REM  - Then sources app_wizard.R directly. app_wizard.R contains its own
REM    non-interactive launcher, which prefers port 7788 and automatically
REM    falls back to the next free local port. Calling source('app_wizard.R') instead
REM    of shiny::runApp(appDir=...) avoids a double-runApp nesting that
REM    breaks static asset serving (www/eu_poverty_map.png and friends).
"%RSCRIPT%" -e "if (file.exists('install_packages.R')) source('install_packages.R'); source('app_wizard.R')"
set "APP_EXIT=%ERRORLEVEL%"

REM Pause only if R exited with an error so the user can read the message
if not "%APP_EXIT%"=="0" (
  echo.
  echo --------------------------------------------
  echo The dashboard exited with an error.
  echo --------------------------------------------
  pause
)
exit /b %APP_EXIT%

:try_r_root
if defined RSCRIPT goto :eof
set "RROOT=%~1"
if not defined RROOT goto :eof
if exist "%RROOT%\bin\Rscript.exe" (
  set "RSCRIPT=%RROOT%\bin\Rscript.exe"
  goto :eof
)
if exist "%RROOT%\bin\x64\Rscript.exe" (
  set "RSCRIPT=%RROOT%\bin\x64\Rscript.exe"
  goto :eof
)
goto :eof
