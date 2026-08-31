# Run from the package root; optional argument selects an extracted clean package.
source(file.path("R", "release_controls.R"))
source(file.path("R", "release_packaging.R"))
args <- commandArgs(trailingOnly = TRUE)
sae_verify_release(if (length(args)) args[[1L]] else ".")
cat("Release inventory, sizes and SHA-256 hashes verified.\n")
