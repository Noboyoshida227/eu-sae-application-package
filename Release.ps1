#Requires -Version 5.1
<#
.SYNOPSIS
    Build one release of the EU SAE Application Package - and refuse to build
    one that could not be reproduced or told apart later.

.DESCRIPTION
    Creates  dist\release_<WIZARD_VERSION>\  containing the complete package
    folder, the distributable ZIP, its SHA-256, a RELEASE_INFO.txt that names
    the exact git commit, and the builder's own re-extraction check.

    Before building, it checks four things and stops if any fails:

      1. The working tree is clean (everything committed). A release built
         from uncommitted files matches no commit on GitHub and cannot be
         rebuilt. Use -AllowDirty only for a private test build; it is
         labelled as such and must not be distributed.
      2. dist\release_<version>\ does not already exist. One version is
         built once. To build again, change the version
         (python tools\bump_version.py <new>) or remove that folder if it was
         never sent to anyone.
      3. docs\CHANGELOG.md has a "## <version>" heading, so what changed is
         written down where recipients will find it.
      4. The Spain example data and every other inventory file is present.

    The full process is described in docs\RELEASING.md.

.EXAMPLE
    powershell -ExecutionPolicy Bypass -File .\Release.ps1
#>

[CmdletBinding()]
param(
    [switch] $AllowDirty
)

$ErrorActionPreference = 'Stop'
function Write-Step { param($m) Write-Host ""; Write-Host "== $m" -ForegroundColor Cyan }
function Write-Good { param($m) Write-Host "   $m" -ForegroundColor Green }
function Write-Bad  { param($m) Write-Host "   $m" -ForegroundColor Red }
function Write-Warn { param($m) Write-Host "   $m" -ForegroundColor Yellow }

Set-Location -LiteralPath $PSScriptRoot
foreach ($required in @('scripts\build_clean_release.R', 'WIZARD_VERSION', 'docs\CHANGELOG.md')) {
    if (-not (Test-Path -LiteralPath $required)) { Write-Bad "Cannot find $required - run this from the package root."; exit 1 }
}
$version = (Get-Content -LiteralPath 'WIZARD_VERSION' -Raw).Trim()
if ($version -notmatch '^[A-Za-z0-9._-]+$') { Write-Bad "WIZARD_VERSION is not a valid version label: '$version'"; exit 1 }
Write-Step "Release $version"

# ---- locate git (PATH, then GitHub Desktop's bundled copy) ----------------
$git = (Get-Command git.exe -ErrorAction SilentlyContinue).Source
if (-not $git) {
    $bundled = Get-ChildItem -Path "$env:LOCALAPPDATA\GitHubDesktop\app-*\resources\app\git\cmd\git.exe" -ErrorAction SilentlyContinue |
               Sort-Object FullName -Descending | Select-Object -First 1
    if ($bundled) { $git = $bundled.FullName }
}

# ---- 1. clean working tree ------------------------------------------------
$commit = 'unknown'; $branch = 'unknown'; $dirty = $null
if ($git) {
    $dirty  = (& $git status --porcelain 2>$null | Where-Object { $_ -notmatch '^\?\? (dist|tmp)/' })
    $commit = (& $git rev-parse --short=10 HEAD 2>$null)
    $branch = (& $git rev-parse --abbrev-ref HEAD 2>$null)
    $remote = (& $git rev-parse --short=10 '@{upstream}' 2>$null)
    if ($dirty) {
        if ($AllowDirty) {
            Write-Warn "Working tree has uncommitted changes - building a TEST build (-AllowDirty)."
            Write-Warn "It will be labelled UNCOMMITTED and must not be distributed."
        } else {
            Write-Bad "The working tree has uncommitted changes:"
            $dirty | ForEach-Object { Write-Bad "     $_" }
            Write-Bad ""
            Write-Bad "A release must correspond to a commit, or it can never be rebuilt or"
            Write-Bad "identified. Commit (GitHub Desktop) and run again, or use -AllowDirty"
            Write-Bad "for a private test build."
            exit 1
        }
    } else {
        Write-Good "Working tree clean at commit $commit ($branch)"
        if ($remote -and $remote -ne $commit) { Write-Warn "Local $branch is not the same as GitHub ($remote). Push before publishing." }
    }
} else {
    Write-Warn "git not found - cannot confirm the tree is committed. Install Git or GitHub Desktop."
    if (-not $AllowDirty) { Write-Bad "Refusing to build without that check. Use -AllowDirty to override."; exit 1 }
}

# ---- 2. one version, built once ------------------------------------------
$target = Join-Path 'dist' "release_$version"
if (Test-Path -LiteralPath $target) {
    Write-Bad "$target already exists."
    Write-Bad "A version is built once. Either change the version:"
    Write-Bad "     python tools\bump_version.py <new-version>"
    Write-Bad "or, if that build was never sent to anyone, delete the folder and run again."
    exit 1
}
Write-Good "Release folder: $target"

# ---- 3. CHANGELOG entry ---------------------------------------------------
$changelog = Get-Content -LiteralPath 'docs\CHANGELOG.md' -Raw
if ($changelog -notmatch ('(?m)^##\s+' + [regex]::Escape($version))) {
    Write-Bad "docs\CHANGELOG.md has no '## $version' heading."
    Write-Bad "Write down what changed in this version before building it."
    exit 1
}
Write-Good "CHANGELOG has an entry for $version"

# ---- 4. inventory present (a clearer message than the R stop()) ----------
$inventory = Import-Csv -LiteralPath 'scripts\release_inventory.csv'
$missing = $inventory.path | Where-Object { -not (Test-Path -LiteralPath $_ -PathType Leaf) }
if ($missing) {
    Write-Bad "Files listed in scripts\release_inventory.csv are missing:"
    $missing | ForEach-Object { Write-Bad "     $_" }
    if ($missing -match 'Data/Spain/(survey|auxiliary)\.rds') {
        Write-Bad ""
        Write-Bad "Data\Spain\survey.rds and auxiliary.rds are kept OUT of the repository on"
        Write-Bad "purpose (GPL-2). Copy them in from the previous release archive or from"
        Write-Bad "the release manager; see Data\Spain\README.md."
    }
    exit 1
}
Write-Good "All $($inventory.Count) inventory files present"

# ---- locate Rscript -------------------------------------------------------
$rscript = (Get-Command Rscript.exe -ErrorAction SilentlyContinue).Source
if (-not $rscript -and $env:EU_SAE_RSCRIPT -and (Test-Path -LiteralPath $env:EU_SAE_RSCRIPT)) { $rscript = $env:EU_SAE_RSCRIPT }
if (-not $rscript) {
    $roots = @("$env:ProgramFiles\R", "${env:ProgramFiles(x86)}\R", "$env:LOCALAPPDATA\Programs\R") | Where-Object { $_ -and (Test-Path -LiteralPath $_) }
    $found = foreach ($root in $roots) {
        Get-ChildItem -LiteralPath $root -Directory -Filter 'R-*' -ErrorAction SilentlyContinue | ForEach-Object {
            $exe = Join-Path $_.FullName 'bin\Rscript.exe'
            if (Test-Path -LiteralPath $exe) {
                $v = try { [version]($_.Name -replace '^R-', '') } catch { [version]'0.0.0' }
                [pscustomobject]@{ Version = $v; Exe = $exe }
            }
        }
    }
    $rscript = $found | Sort-Object Version -Descending | Select-Object -First 1 -ExpandProperty Exe
}
if (-not $rscript) { Write-Bad "Rscript.exe not found. Install R, or set EU_SAE_RSCRIPT to its full path."; exit 1 }
Write-Good "Using R: $rscript"

# ---- build ----------------------------------------------------------------
Write-Step "Running the release builder"
$prevEAP = $ErrorActionPreference; $ErrorActionPreference = 'Continue'
$log = & $rscript 'scripts/build_clean_release.R' $target 2>&1
$buildExit = $LASTEXITCODE
$ErrorActionPreference = $prevEAP
$log | ForEach-Object { Write-Host "   $_" }
if ($buildExit -ne 0) { Write-Step "BUILD FAILED"; Write-Bad "The builder exited with code $buildExit. Nothing was published."; exit $buildExit }

# ---- confirm the folder holds a package AND a zip AND executable launchers -
Write-Step "Checking the release folder"
$logText = ($log | Out-String)
$pkgDir  = Get-ChildItem -LiteralPath $target -Directory | Where-Object { $_.Name -like 'EU_SAE_wizard_*' } | Select-Object -First 1
$zip     = Get-ChildItem -LiteralPath $target -Filter '*.zip' | Select-Object -First 1
$problems = @()
if (-not $pkgDir) { $problems += "No EU_SAE_wizard_* package folder was created." }
if (-not $zip)    { $problems += "No .zip archive was created." }
if ($logText -notmatch 'Marked executable in archive') { $problems += "The macOS/Linux launchers were NOT marked executable in the archive." }
if ($problems.Count) {
    Write-Step "RELEASE INCOMPLETE"; $problems | ForEach-Object { Write-Bad $_ }
    Write-Bad ""; Write-Bad "Do not distribute this build."; exit 1
}
$fileCount = (Get-ChildItem -LiteralPath $pkgDir.FullName -Recurse -File).Count
$hash      = (Get-FileHash -LiteralPath $zip.FullName -Algorithm SHA256).Hash.ToLower()
$sizeMB    = [math]::Round($zip.Length / 1MB, 2)
Write-Good "Package folder : $($pkgDir.Name)  ($fileCount files)"
Write-Good "Archive        : $($zip.Name)  ($sizeMB MB)"
Write-Good "SHA-256        : $hash"
Write-Good "macOS launchers marked executable inside the archive."

# ---- RELEASE_INFO.txt: the folder explains itself -------------------------
$status = if ($dirty) { "UNCOMMITTED TEST BUILD - do not distribute" } else { "release build" }
$info = @(
    "EU SAE Application Package - $status", "",
    "Version        : $version",
    "Commit         : $commit ($branch)",
    "Built          : $(Get-Date -Format 'yyyy-MM-dd HH:mm')",
    "Built from     : $PSScriptRoot",
    "Package folder : $($pkgDir.Name)  ($fileCount files)",
    "Archive        : $($zip.Name)  ($($zip.Length) bytes)",
    "SHA-256        : $hash", "",
    "Distribute the .zip in this folder. Recipients can verify it with:",
    "    Windows : Get-FileHash .\$($zip.Name) -Algorithm SHA256",
    "    macOS   : shasum -a 256 $($zip.Name)", "",
    "This folder is build output: it is the package as of commit $commit.",
    "To change the package, edit at the repository root, bump the version,",
    "add a CHANGELOG entry, commit, and run Release.ps1 again."
)
$info | Set-Content -LiteralPath (Join-Path $target 'RELEASE_INFO.txt') -Encoding UTF8

Write-Step "Done"
Write-Host "   The archive to send out:" -ForegroundColor Green
Write-Host "   $($zip.FullName)" -ForegroundColor White
if (-not $dirty -and $git) {
    Write-Host ""
    Write-Host "   To record this release permanently, tag the commit and push the tag:" -ForegroundColor Green
    Write-Host "   git tag -a v$version -m `"EU SAE $version`" $commit" -ForegroundColor White
    Write-Host "   git push origin v$version" -ForegroundColor White
    Write-Host ""
    Write-Host "   Then on GitHub: Releases -> Draft a new release -> choose tag v$version," -ForegroundColor Green
    Write-Host "   paste the CHANGELOG entry, attach the .zip, quote the SHA-256, mark as pre-release." -ForegroundColor Green
}
Write-Host ""
