# Resolve package inputs without replacing a missing user file with example data.
sae_resolve_input_path <- function(path, root = ".", allow_basename = FALSE) {
  if (is.null(path) || length(path) == 0L) return(NULL)
  path <- trimws(as.character(path[[1L]]))
  if (is.na(path) || !nzchar(path)) return(NULL)
  path <- gsub("\\\\", "/", path)
  absolute <- grepl("^([A-Za-z]:|/)", path)
  candidate <- if (absolute) path else file.path(root, path)
  if (file.exists(candidate)) return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  # Absolute references belong to the user, including references to older copies.
  if (absolute) return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
  relative <- sub("^\\./", "", path)
  migrated <- relative
  if (!".." %in% strsplit(relative, "/", fixed = TRUE)[[1L]]) {
    migrated <- sub("^data/", "Data/Spain/", relative)
    migrated <- sub("^example_data/", "Data/simulated/", migrated)
  }
  destination <- file.path(root, migrated)
  if (!identical(migrated, relative) && file.exists(destination)) {
    message("Updated packaged input path: ", relative, " -> ", migrated)
    return(normalizePath(destination, winslash = "/", mustWork = TRUE))
  }
  if (isTRUE(allow_basename) && !grepl("/", relative, fixed = TRUE)) {
    candidates <- file.path(root, c("Data", "Data/Spain", "Data/simulated"), relative)
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) > 1L) stop("Ambiguous saved input filename: ", relative,
                                     ". Browse to the intended file again.", call. = FALSE)
    if (length(candidates) == 1L) return(normalizePath(candidates, winslash = "/", mustWork = TRUE))
  }
  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}
