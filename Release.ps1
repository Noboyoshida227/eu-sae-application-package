#Requires -Version 5.1
<#
.SYNOPSIS
    Build one dated release of the EU SAE Application Package.

.DESCRIPTION
    Creates dist\release_<yyyyMMdd>\ containing:

        EU_SAE_wizard_<version>\                     the complete package folder
        EU_SAE_wizard_<version>_..._candidate.zip    the archive to distribute
        SHA256SUMS.txt                               its checksum
        RELEASE_INFO.txt                             what this build is
        _archive_verification\                       the builder's own re-check

    One release = one folder = one zip. Nothing inside dist\ is ever edited by
    hand: edit the package files at the repository root, then run this script.
    The root is what GitHub publishes, so the release and the repository always
    describe the same thing.

    If today's folder already exists, a numbered one is used instead
    (release_20260901_2), so a build can never overwrite an earlier release.

.EXAMPLE
    .\Release.ps1
        Build into dist\release_<today>.

.EXAMPLE
    .\Release.ps1 -Tag 20260901_mac_launchers
        Build into dist\release_20260901_mac_launchers.

.NOTES
    Run from the package root:
        cd C:\Users\noboy\Repos\eu-sae-personal
        powershell -ExecutionPolicy Bypass -File .\Release.ps1
#>

[CmdletBinding()]
param(
    [string] $Tag = (Get-Date -Format 'yyyyMMdd')
)

$ErrorActionPreference = 'Stop'

function Write-Step  { param($m) Write-Host ""; Write-Host "== $m" -ForegroundColor Cyan }
function Write-Good  { param($m) Write-Host "   $m" -ForegroundColor Green }
function Write-Bad   { param($m) Write-Host "   $m" -ForegroundColor Red }

# ---- Work from the folder this script lives in --------------------------
Set-Location -LiteralPath $PSScriptRoot

foreach ($required in @('scripts\build_clean_release.R', 'WIZARD_VERSION', 'VERSION')) {
    if (-not (Test-Path -LiteralPath $required)) {
        Write-Bad "Cannot find $required"
        Write-Bad "Keep Release.ps1 in the package root (beside app.R)."
        exit 1
    }
}

$version = (Get-Content -LiteralPath 'WIZARD_VERSION' -Raw).Trim()
Write-Step "Building EU SAE $version"

# ---- Pick a release folder that does not exist yet ----------------------
$target = Join-Path 'dist' "release_$Tag"
$n = 1
while (Test-Path -LiteralPath $target) {
    $n++
    $target = Join-Path 'dist' ("release_{0}_{1}" -f $Tag, $n)
}
Write-Good "Release folder: $target"

# ---- Locate Rscript -----------------------------------------------------
$rscript = $null
$onPath = Get-Command Rscript.exe -ErrorAction SilentlyContinue
if ($onPath) { $rscript = $onPath.Source }

if (-not $rscript -and $env:EU_SAE_RSCRIPT -and (Test-Path -LiteralPath $env:EU_SAE_RSCRIPT)) {
    $rscript = $env:EU_SAE_RSCRIPT
}

if (-not $rscript) {
    $roots = @("$env:ProgramFiles\R", "${env:ProgramFiles(x86)}\R", "$env:LOCALAPPDATA\Programs\R") |
             Where-Object { $_ -and (Test-Path -LiteralPath $_) }
    $found = foreach ($root in $roots) {
        Get-ChildItem -LiteralPath $root -Directory -Filter 'R-*' -ErrorAction SilentlyContinue |
        ForEach-Object {
            $exe = Join-Path $_.FullName 'bin\Rscript.exe'
            if (Test-Path -LiteralPath $exe) {
                $v = try { [version]($_.Name -replace '^R-', '') } catch { [version]'0.0.0' }
                [pscustomobject]@{ Version = $v; Exe = $exe }
            }
        }
    }
    $rscript = $found | Sort-Object Version -Descending | Select-Object -First 1 -ExpandProperty Exe
}

if (-not $rscript) {
    Write-Bad "Rscript.exe not found."
    Write-Bad "Install R from https://cran.r-project.org/, or set EU_SAE_RSCRIPT"
    Write-Bad "to the full path of Rscript.exe."
    exit 1
}
Write-Good "Using R: $rscript"

# ---- Build --------------------------------------------------------------
Write-Step "Running the release builder"
# R writes progress to stderr. With ErrorActionPreference = 'Stop', merging
# stderr into the pipeline can turn ordinary progress messages into a
# terminating error, so relax it just for the native call.
$prevEAP = $ErrorActionPreference
$ErrorActionPreference = 'Continue'
$log = & $rscript 'scripts/build_clean_release.R' $target 2>&1
$buildExit = $LASTEXITCODE
$ErrorActionPreference = $prevEAP
$log | ForEach-Object { Write-Host "   $_" }

if ($buildExit -ne 0) {
    Write-Step "BUILD FAILED"
    Write-Bad "The builder exited with code $buildExit. Nothing was published."
    Write-Bad "The messages above say which file or check failed."
    exit $buildExit
}

# ---- Confirm the release folder really holds a package AND a zip --------
Write-Step "Checking the release folder"

$logText = ($log | Out-String)
$pkgDir  = Get-ChildItem -LiteralPath $target -Directory |
           Where-Object { $_.Name -like 'EU_SAE_wizard_*' } | Select-Object -First 1
$zip     = Get-ChildItem -LiteralPath $target -Filter '*.zip' | Select-Object -First 1

$problems = @()
if (-not $pkgDir) { $problems += "No EU_SAE_wizard_* package folder was created." }
if (-not $zip)    { $problems += "No .zip archive was created." }
if ($logText -notmatch 'Marked executable in archive') {
    $problems += "The macOS/Linux launchers were NOT marked executable in the archive."
}

if ($problems.Count) {
    Write-Step "RELEASE INCOMPLETE"
    $problems | ForEach-Object { Write-Bad $_ }
    Write-Bad ""
    Write-Bad "Do not distribute this build. Send the output above for diagnosis."
    exit 1
}

$fileCount = (Get-ChildItem -LiteralPath $pkgDir.FullName -Recurse -File).Count
$hash      = (Get-FileHash -LiteralPath $zip.FullName -Algorithm SHA256).Hash.ToLower()
$sizeMB    = [math]::Round($zip.Length / 1MB, 2)

Write-Good "Package folder : $($pkgDir.Name)  ($fileCount files)"
Write-Good "Archive        : $($zip.Name)  ($sizeMB MB)"
Write-Good "SHA-256        : $hash"
Write-Good "macOS launchers marked executable inside the archive."

# ---- Leave a note so the folder explains itself later -------------------
$info = @(
    "EU SAE Application Package - release build",
    "",
    "Version        : $version",
    "Built          : $(Get-Date -Format 'yyyy-MM-dd HH:mm')",
    "Built from     : $($PSScriptRoot)",
    "Package folder : $($pkgDir.Name)  ($fileCount files)",
    "Archive        : $($zip.Name)  ($($zip.Length) bytes)",
    "SHA-256        : $hash",
    "",
    "Distribute the .zip in this folder. Recipients can verify it with:",
    "    Windows : Get-FileHash .\$($zip.Name) -Algorithm SHA256",
    "    macOS   : shasum -a 256 $($zip.Name)",
    "",
    "This folder is build output. To change the package, edit the files at the",
    "repository root and run Release.ps1 again - it will create the next folder."
)
$infoPath = Join-Path $target 'RELEASE_INFO.txt'
$info | Set-Content -LiteralPath $infoPath -Encoding UTF8

Write-Step "Done"
Write-Host "   The archive to send out:" -ForegroundColor Green
Write-Host "   $($zip.FullName)" -ForegroundColor White
Write-Host ""
Write-Host "   Folder contents:" -ForegroundColor Green
Get-ChildItem -LiteralPath $target | ForEach-Object {
    $kind = if ($_.PSIsContainer) { "<dir> " } else { "{0,6:N0} KB" -f ($_.Length / 1KB) }
    Write-Host ("   {0,-12} {1}" -f $kind, $_.Name)
}
Write-Host ""
