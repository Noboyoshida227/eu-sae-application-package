# Authoritative candidate builder. Run from the package root.
source(file.path("R", "release_controls.R"))
source(file.path("R", "release_packaging.R"))
source(file.path("R", "zip_permissions.R"))
args <- commandArgs(trailingOnly = TRUE)
target <- if (length(args)) args[[1L]] else file.path("dist", "reorganized_candidate")
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
target <- gsub("\\\\", "/", target)
if (any(strsplit(target, "/", fixed = TRUE)[[1L]] == "..")) stop("Build destination must not contain parent-directory traversal.")
if (!grepl("^(/|[A-Za-z]:/)", target)) target <- file.path(root, target)
target <- normalizePath(target, winslash = "/", mustWork = FALSE)
if (!startsWith(target, paste0(root, "/dist/"))) stop("Build destination must be inside this package's dist/ directory.")
existing_parent <- dirname(target)
while (!dir.exists(existing_parent)) existing_parent <- dirname(existing_parent)
resolved_parent <- normalizePath(existing_parent, winslash = "/", mustWork = TRUE)
if (!(identical(resolved_parent, root) || identical(resolved_parent, paste0(root, "/dist")) ||
      startsWith(resolved_parent, paste0(root, "/dist/")))) stop("Build destination resolves outside the package dist/ directory.")
if (file.exists(target) || dir.exists(target)) stop("Refusing to overwrite release destination: ", target)
if (!requireNamespace("zip", quietly = TRUE)) stop("R package 'zip' is required to build and verify the archive.")
paths <- sae_release_inventory(root)
missing <- paths[!file.exists(file.path(root, paths)) | dir.exists(file.path(root, paths))]
if (length(missing)) stop("Missing required release inputs: ", paste(missing, collapse = ", "))
# Preflight the complete source inventory and hashes before creating the target.
hashes <- vapply(file.path(root, paths), sae_sha256_file, character(1))
if (anyNA(hashes)) stop("Source hashing failed.")
wizard_version <- trimws(readLines("WIZARD_VERSION", warn = FALSE)[1L])
if (!grepl("^[A-Za-z0-9._-]+$", wizard_version)) stop("Invalid WIZARD_VERSION.")
package_name <- paste0("EU_SAE_wizard_", wizard_version)
stage <- file.path(target, package_name)
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
for (path in paths) {
  dst <- file.path(stage, path)
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(file.path(root, path), dst, overwrite = FALSE, copy.date = TRUE)) stop("Copy failed: ", path)
}
writeLines(c(
  paste("EU SAE candidate", wizard_version, "- reorganized layout"),
  "Prepared for review only; not authorized for public distribution or official statistics.",
  "Includes only the explicit scripts/release_inventory.csv file list.",
  "Windows: open Start_Here and run Start_Wizard.bat or Start_Dashboard.bat. Keep Start_Here inside the package.",
  "macOS/Linux: open Start_Here and run Start_Wizard.command or Start_Dashboard.command (double-click on macOS). Keep Start_Here inside the package.",
  "Reports: final_report.html and editable final_report.docx; comparison outputs include signed estimated-change figures alongside CI-width figures.",
  "Excluded: internal notes, non-inventory literature, user Data folders, local libraries, credentials, run histories, generated outputs and local diagnostics.",
  "Spain boundaries now use documented IGN/CNIG CartoBase ANE (CC BY 4.0); the survey derivation script remains missing.",
  "MCPE independent validation, platform smoke tests, rights and institutional approval remain release gates.",
  "Review docs/RELEASE_CHECKLIST.md, docs/MCPE_VALIDATION_STATUS.md, docs/HISTORY_REMEDIATION.md and THIRD_PARTY_NOTICES.md.",
  "No Git history is included; this build does not remediate earlier repository objects or releases."
), file.path(stage, "docs", "CLEAN_RELEASE_NOTICE.txt"), useBytes = TRUE)
sae_write_release_manifest(stage)
sae_verify_release(stage)
archive <- file.path(target, paste0(package_name, "_reports_candidate.zip"))
zip::zipr(archive, files = package_name, root = target, include_directories = TRUE)
# A ZIP written on Windows records no Unix permission bits, so the macOS/Linux
# launchers would extract without the executable flag and Finder would refuse to
# run them. Mark them executable before the archive is hashed, so the published
# SHA-256 matches the file recipients actually download. The re-extraction and
# manifest verification below run afterwards and would catch any corruption.
exec_marked <- sae_set_zip_exec_bits(archive, patterns = c("\\.command$", "\\.sh$"))
if (!length(exec_marked)) stop("No launcher entries were marked executable in the archive.")
cat("Marked executable in archive:\n"); cat(paste0("  ", exec_marked, collapse = "\n"), "\n")
checksum <- sae_sha256_file(archive)
if (is.na(checksum)) stop("Archive checksum failed.")
writeLines(paste(checksum, basename(archive), sep = "  "), file.path(target, "SHA256SUMS.txt"))
# Verify the archive contents independently of staging; preserve this QA extraction.
entries <- zip::zip_list(archive)$filename
if (any(!startsWith(entries, paste0(package_name, "/"))) ||
    any(grepl("(^/|(^|/)\\.\\.(/|$)|^[A-Za-z]:)", entries))) stop("Invalid archive topology.")
extraction <- file.path(target, "_archive_verification")
dir.create(extraction)
zip::unzip(archive, exdir = extraction)
sae_verify_release(file.path(extraction, package_name))
cat("Candidate staging:", stage, "\nArchive:", archive,
    "\nSHA256:", checksum, "\nManifest and extracted archive verified.\n")
