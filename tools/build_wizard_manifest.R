# Compatibility entry point; the explicit inventory is authoritative.
root <- if (basename(getwd()) == "tools") ".." else "."
source(file.path(root, "R", "release_controls.R"))
source(file.path(root, "R", "release_packaging.R"))
args <- commandArgs(trailingOnly = TRUE)
target <- if (length(args)) args[[1L]] else root
sae_write_release_manifest(target)
cat("Release manifest generated from the explicit inventory.\n")
