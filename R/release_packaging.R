# Shared release inventory, manifest and verification implementation.
sae_release_inventory <- function(root = ".") {
  inventory <- utils::read.csv(file.path(root, "scripts", "release_inventory.csv"),
                               stringsAsFactors = FALSE, check.names = FALSE)
  paths <- inventory$path
  if (!is.character(paths) || !length(paths) || anyNA(paths) || anyDuplicated(tolower(paths)) ||
      any(!nzchar(paths)) || any(grepl("(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.(/|$))", paths))) {
    stop("Invalid release inventory.", call. = FALSE)
  }
  forbidden <- "(^|/)(\\.git|\\.env[^/]*|\\.Renviron|\\.Rhistory|\\.RData|Rplots\\.pdf|app_runs|r_local_library|node_modules|renv|tmp|dist|secrets\\.yml|api_keys\\.yml|package_versions\\.local\\.csv)(/|$)|^docs/internal/|^outputs/(?!.*\\.gitkeep$)|^docs/guidance/literature/(?!README\\.md$)"
  if (any(grepl(forbidden, paths, perl = TRUE, ignore.case = TRUE))) stop("Forbidden file in release inventory.")
  required <- c("VERSION", "WIZARD_VERSION", "app.R", "app_wizard.R", "report.Rmd",
                "README.md", "LICENSE", "NOTICE", "THIRD_PARTY_NOTICES.md",
                "docs/MCPE_VALIDATION_STATUS.md", "docs/SUPPORTED_PLATFORMS.md",
                "Data/README.md", "scripts/release_inventory.csv")
  if (length(setdiff(required, paths))) stop("Release inventory lacks required package files.")
  sort(paths, method = "radix")
}

sae_write_release_manifest <- function(root = ".") {
  paths <- sae_release_inventory(root)
  full <- file.path(root, paths)
  missing <- paths[!file.exists(full) | dir.exists(full)]
  if (length(missing)) stop("Missing release file(s): ", paste(missing, collapse = ", "))
  hashes <- vapply(full, sae_sha256_file, character(1))
  if (anyNA(hashes) || any(!grepl("^[0-9a-f]{64}$", hashes))) stop("SHA-256 computation failed.")
  manifest <- data.frame(path = paths, bytes = unname(file.info(full)$size),
                         sha256 = unname(hashes), stringsAsFactors = FALSE)
  utils::write.csv(manifest, file.path(root, "release_manifest.csv"), row.names = FALSE,
                   fileEncoding = "UTF-8")
  invisible(manifest)
}

sae_verify_release <- function(root = ".") {
  expected <- sae_release_inventory(root)
  actual <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  actual <- gsub("\\\\", "/", actual)
  if (!setequal(actual, c(expected, "release_manifest.csv"))) {
    stop("Release file-set mismatch. Missing: ", paste(setdiff(c(expected, "release_manifest.csv"), actual), collapse = ", "),
         "; unexpected: ", paste(setdiff(actual, c(expected, "release_manifest.csv")), collapse = ", "))
  }
  expected_dirs <- unique(unlist(lapply(expected, function(path) {
    parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return(character())
    vapply(seq_len(length(parts) - 1L), function(n) paste(parts[seq_len(n)], collapse = "/"), character(1))
  })))
  actual_dirs <- list.dirs(root, recursive = TRUE, full.names = FALSE)
  actual_dirs <- gsub("\\\\", "/", actual_dirs[nzchar(actual_dirs)])
  if (!setequal(actual_dirs, expected_dirs)) stop("Release directory-set mismatch, including empty or hidden directories.")
  manifest <- utils::read.csv(file.path(root, "release_manifest.csv"), stringsAsFactors = FALSE)
  if (!identical(manifest$path, expected) || anyDuplicated(manifest$path)) stop("Manifest inventory mismatch.")
  hashes <- vapply(file.path(root, manifest$path), sae_sha256_file, character(1))
  if (anyNA(hashes) || !identical(unname(hashes), manifest$sha256) ||
      !identical(as.numeric(file.info(file.path(root, manifest$path))$size), as.numeric(manifest$bytes))) {
    stop("Release content does not match its manifest.")
  }
  invisible(TRUE)
}
