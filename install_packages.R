# ============================================================
# install_packages.R
# Run this script ONCE before launching app.R to ensure all
# required R packages are installed.
#
# Usage:  source("install_packages.R")
#    or:  Rscript install_packages.R
# ============================================================

cat("=== EU SAE Shiny App - Package Installer ===\n\n")

.user_lib <- Sys.getenv("R_LIBS_USER", unset = "")
if (nzchar(.user_lib)) {
  dir.create(.user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(unique(c(.user_lib, .libPaths())))
}

# --- R version check ----------------------------------------
# Current CRAN dependencies used by the pipeline (notably emdi)
# require R 4.2.0 or later.
.min_r_version <- "4.2.0"
if (getRversion() < .min_r_version) {
  stop(sprintf(
    "R >= %s is required (you have %s). Please update R from https://cran.r-project.org/",
    .min_r_version,
    getRversion()
  ), call. = FALSE)
}

# --- Helper: install if missing or below a minimum version ---
local_version_report <- "package_versions.local.csv"

installed_package_version <- function(pkg) {
  tryCatch(
    as.character(utils::packageVersion(pkg)),
    error = function(e) NA_character_
  )
}

install_if_missing_or_old <- function(pkg, min_version = NA_character_,
                                      repos = "https://cloud.r-project.org") {
  current_version <- installed_package_version(pkg)
  installed <- !is.na(current_version)
  has_min <- is.character(min_version) && length(min_version) == 1 &&
    !is.na(min_version) && nzchar(min_version)

  too_old <- FALSE
  if (installed) {
    too_old <- has_min && utils::compareVersion(current_version, min_version) < 0
  }

  if (!installed) {
    cat("  Installing:", pkg, "\n")
    install.packages(pkg, repos = repos, lib = .libPaths()[1], quiet = TRUE)
  } else if (too_old) {
    cat("  Updating:", pkg, "(installed", current_version,
        "< required", min_version, ")\n")
    install.packages(pkg, repos = repos, lib = .libPaths()[1], quiet = TRUE)
  } else if (has_min) {
    cat("  OK:", pkg, current_version, "(minimum", min_version, ")\n")
  } else {
    cat("  OK:", pkg, current_version, "\n")
  }
}

# --- 1. CRAN packages --------------------------------------
cat("[1/3] Checking CRAN packages...\n")

cran_packages <- c(
  # Shiny app
  "shiny",

  # Data manipulation
  "data.table", "dplyr", "tidyr", "purrr", "stringr",
  "tibble", "rlang", "tidyverse", "reshape2", "haven", "arrow",

  # Small area estimation
  "sae", "msae", "emdi",

  # Survey design
  "survey",


  # Spatial
  "sf", "spdep",

  # Visualisation
  "ggplot2", "patchwork", "viridis", "scales",

  # Tables and reporting
  "gt", "knitr", "rmarkdown", "openxlsx", "writexl", "readxl", "xml2", "zip",

  # Statistical / matrix
  "MASS", "Matrix", "matrixcalc", "moments", "magic", "car", "caret", "glmnet",

  # API / web
  "httr", "jsonlite", "base64enc",

  # Utilities
  "yaml", "here", "tictoc", "conflicted", "pacman", "pins", "digest", "tools"
)

minimum_versions <- c(
  shiny = "1.7.0",
  data.table = "1.14.0",
  dplyr = "1.0.0",
  tidyr = "1.2.0",
  purrr = "1.0.0",
  stringr = "1.5.0",
  tibble = "3.1.0",
  rlang = "1.1.0",
  haven = "2.5.0",
  arrow = "12.0.0",
  sae = "1.3",
  msae = "0.1.5",
  emdi = "2.0.0",
  survey = "4.2",
  sf = "1.0.0",
  spdep = "1.2.0",
  ggplot2 = "3.4.0",
  patchwork = "1.1.0",
  viridis = "0.6.0",
  scales = "1.2.0",
  gt = "0.9.0",
  knitr = "1.40",
  rmarkdown = "2.20",
  openxlsx = "4.2.5",
  writexl = "1.4.0",
  readxl = "1.4.0",
  Matrix = "1.5.0",
  glmnet = "4.1.0",
  httr = "1.4.0",
  jsonlite = "1.8.0",
  yaml = "2.3.0",
  here = "1.0.1"
)

for (pkg in cran_packages) {
  min_version <- if (pkg %in% names(minimum_versions)) {
    minimum_versions[[pkg]]
  } else {
    NA_character_
  }
  install_if_missing_or_old(pkg, min_version)
}

# --- 2. Optional Quarto CLI check ---------------------------
cat("\n[2/3] Checking optional Quarto support...\n")

quarto_path <- if (requireNamespace("quarto", quietly = TRUE)) {
  tryCatch(quarto::quarto_path(), error = function(e) "")
} else {
  ""
}

quarto_path <- if (is.character(quarto_path) && length(quarto_path) > 0) {
  unname(quarto_path[[1]])
} else {
  ""
}

if (nzchar(quarto_path)) {
  cat("  Optional Quarto CLI found:", quarto_path, "\n")
} else {
  cat("  Optional Quarto CLI not found. This dashboard uses rmarkdown for reports, so Quarto is not required.\n")
}

# --- 3. Verify all packages load ---------------------------
cat("\n[3/3] Verifying all packages can be loaded...\n")

failed <- character()
for (pkg in cran_packages) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  if (!ok) failed <- c(failed, pkg)
}

if (length(failed) == 0) {
  package_versions <- data.frame(
    package = cran_packages,
    version = vapply(cran_packages, function(pkg) {
      as.character(utils::packageVersion(pkg))
    }, character(1)),
    required_minimum = vapply(cran_packages, function(pkg) {
      if (pkg %in% names(minimum_versions)) minimum_versions[[pkg]] else NA_character_
    }, character(1)),
    stringsAsFactors = FALSE
  )
  utils::write.csv(package_versions, local_version_report, row.names = FALSE)
  cat("\n  All packages installed successfully.\n")
  cat("  Package versions recorded locally in ", local_version_report, ".\n", sep = "")
  cat("  This local report is for troubleshooting only and is ignored by Git.\n")
  cat("  You can now run the app with:  shiny::runApp('app.R')\n\n")
} else {
  cat("\n  WARNING: The following packages could not be loaded:\n")
  cat("    ", paste(failed, collapse = ", "), "\n")
  cat("  Please install them manually and try again.\n\n")
}
