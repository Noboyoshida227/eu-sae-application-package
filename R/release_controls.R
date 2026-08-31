# Release, provenance, and reproducibility helpers.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

sae_app_version <- function(root = ".") {
  path <- file.path(root, "VERSION")
  if (!file.exists(path)) return("unknown")
  value <- trimws(readLines(path, warn = FALSE, encoding = "UTF-8")[1])
  if (nzchar(value)) value else "unknown"
}

sae_sha256_file <- function(path) {
  if (is.null(path) || length(path) != 1L || !nzchar(path) || !file.exists(path)) {
    return(NA_character_)
  }
  if (isTRUE(file.info(path)$size == 0)) {
    return("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  }
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(file = path, algo = "sha256", serialize = FALSE))
  }
  candidates <- if (.Platform$OS.type == "windows") {
    list(c("certutil", "-hashfile", shQuote(path), "SHA256"))
  } else {
    list(c("sha256sum", path), c("shasum", "-a", "256", path))
  }
  for (cmd in candidates) {
    if (!nzchar(Sys.which(cmd[[1]]))) next
    out <- tryCatch(system2(cmd[[1]], cmd[-1], stdout = TRUE, stderr = FALSE),
                    error = function(e) character())
    hit <- regmatches(out, regexpr("[A-Fa-f0-9]{64}", out, perl = TRUE))
    hit <- hit[nzchar(hit)]
    if (length(hit) > 0L) return(tolower(hit[[1]]))
  }
  NA_character_
}

sae_input_manifest <- function(cfg) {
  paths <- c(
    survey = cfg$data_inputs$survey_path %||% cfg$ufh$survey_path %||% cfg$mfh$survey_path,
    auxiliary = cfg$data_inputs$rhs_path %||% cfg$ufh$rhs_path %||% cfg$mfh$rhs_path,
    geometry = cfg$data_inputs$shp_path %||% cfg$ufh$shp_path %||% cfg$mfh$shp_path,
    benchmark = cfg$benchmarking$target_path %||% cfg$ufh$benchmark_target_path %||% cfg$mfh$benchmark_target_path,
    population = cfg$ufh$population_path %||% cfg$mfh$population_path
  )
  paths <- paths[!vapply(paths, is.null, logical(1))]
  input_roles <- names(paths)
  paths <- as.character(paths)
  names(paths) <- input_roles
  paths <- paths[!is.na(paths) & nzchar(paths)]
  if (length(paths) == 0L) {
    return(data.frame(input_role = character(), file_name = character(),
                      bytes = numeric(), modified_utc = character(),
                      sha256 = character(), hash_status = character()))
  }
  info <- file.info(paths)
  hashes <- vapply(paths, sae_sha256_file, character(1))
  data.frame(
    input_role = names(paths),
    file_name = basename(paths),
    bytes = unname(info$size),
    modified_utc = format(info$mtime, tz = "UTC", usetz = TRUE),
    sha256 = hashes,
    hash_status = ifelse(is.na(hashes), "digest package unavailable", "recorded"),
    stringsAsFactors = FALSE
  )
}

sae_write_run_metadata <- function(cfg, config_path = NULL,
                                   output_dir = file.path("outputs", "data")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  version <- sae_app_version()
  seed <- suppressWarnings(as.integer(cfg$analysis_seed %||% 123L))
  if (!is.finite(seed)) seed <- 123L
  manifest <- sae_input_manifest(cfg)
  utils::write.csv(manifest, file.path(output_dir, "input_manifest.csv"), row.names = FALSE,
                   fileEncoding = "UTF-8")

  pkg_names <- unique(c(
    "R", "shiny", "yaml", "emdi", "sae", "msae", "survey", "sf",
    "glmnet", "rmarkdown", "digest", "httr", "jsonlite"
  ))
  pkg_versions <- vapply(pkg_names, function(pkg) {
    if (identical(pkg, "R")) return(as.character(getRversion()))
    if (requireNamespace(pkg, quietly = TRUE)) as.character(utils::packageVersion(pkg)) else NA_character_
  }, character(1))
  utils::write.csv(
    data.frame(package = pkg_names, version = pkg_versions, stringsAsFactors = FALSE),
    file.path(output_dir, "package_versions.csv"), row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"),
             useBytes = TRUE)

  metadata <- list(
    application_version = version,
    release_baseline = "v5.1.0 (cd04d479e0484ae98e3ef8299db29ac1f0f944b7)",
    generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    run_id = cfg$run_id %||% NA_character_,
    run_label = cfg$run_label %||% NA_character_,
    run_folder = if (!is.null(config_path)) normalizePath(dirname(config_path), winslash = "/", mustWork = FALSE) else NA_character_,
    archived_outputs = if (!is.null(config_path)) normalizePath(file.path(dirname(config_path), "outputs"), winslash = "/", mustWork = FALSE) else NA_character_,
    country_or_territory = cfg$country %||% "Not specified",
    analysis_seed = seed,
    mcpe_bootstrap_replicates = cfg$mfh$mcpe_nB %||% NA_integer_,
    ufh_change_variance_assumption = "period_independence",
    mfh_selected_model = cfg$mfh$diag_model %||% NA_character_,
    benchmarking = cfg$benchmarking %||% list(enabled = FALSE),
    config_file = if (!is.null(config_path)) basename(config_path) else NA_character_,
    ai = list(
      enabled = isTRUE(cfg$ai$enabled),
      provider = cfg$ai$provider %||% NA_character_,
      external_transfer_consent = isTRUE(cfg$ai$external_transfer_consent),
      payload_policy = "aggregate statistics; geographic identifiers removed from comparison prompts"
    ),
    input_manifest = manifest
  )
  saveRDS(metadata, file.path(output_dir, "run_metadata.rds"))
  invisible(metadata)
}

sae_record_mfh_numerical_diagnostics <- function(
    artifacts_path = file.path("outputs", "data", "mfh_artifacts.rds"),
    metadata_path = file.path("outputs", "data", "run_metadata.rds"),
    tables_dir = file.path("outputs", "tables")) {
  if (!file.exists(artifacts_path)) {
    stop("MFH artifacts are unavailable: ", artifacts_path, call. = FALSE)
  }

  artifacts <- readRDS(artifacts_path)
  selected_model <- artifacts$selected_model
  fit <- if (is.list(selected_model)) selected_model$fit else NULL
  diagnostics <- if (is.list(fit)) fit$numerical_diagnostics else NULL
  robust_used <- isTRUE(attr(selected_model, ".robust_refit_used"))
  robust_failed <- isTRUE(attr(selected_model, ".robust_refit_failed"))
  model_name <- as.character(artifacts$diag_model %||% NA_character_)

  component_row <- function(component, item = NULL, status) {
    data.frame(
      model = model_name,
      robust_refit_used = robust_used,
      robust_refit_failed = robust_failed,
      diagnostic_status = status,
      component = component,
      inverse_method = as.character(item$method %||% NA_character_),
      condition_number = suppressWarnings(as.numeric(item$condition_number %||% NA_real_)),
      condition_number_method = as.character(item$condition_number_method %||% NA_character_),
      inverse_available = if (is.null(item$available)) NA else isTRUE(item$available),
      g3_available = if (is.null(diagnostics$g3_available)) NA else isTRUE(diagnostics$g3_available),
      mse_status = if (is.null(diagnostics$g3_available)) {
        "not_reported"
      } else if (isTRUE(diagnostics$g3_available)) {
        "available"
      } else {
        "unavailable_fisher_information"
      },
      stringsAsFactors = FALSE
    )
  }

  if (is.list(diagnostics)) {
    component_names <- c(
      "marginal_covariance", "fixed_effect_information", "fisher_information"
    )
    rows <- do.call(rbind, lapply(component_names, function(component) {
      item <- diagnostics[[component]]
      status <- if (is.list(item) && isTRUE(item$available)) {
        if (identical(as.character(item$method %||% ""), "MASS::ginv")) {
          "generalized_inverse_used"
        } else {
          "available"
        }
      } else {
        "unavailable"
      }
      component_row(component, item, status)
    }))
  } else {
    status <- if (robust_failed) {
      "robust_refit_failed"
    } else if (robust_used) {
      "robust_refit_no_extended_diagnostics"
    } else if (isTRUE(artifacts$model_fit_failed)) {
      "model_fit_failed"
    } else if (isTRUE(artifacts$fit_skipped)) {
      "model_fit_skipped"
    } else {
      "standard_fit_no_extended_diagnostics"
    }
    rows <- component_row("extended_diagnostics", NULL, status)
  }

  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
  table_path <- file.path(tables_dir, "mfh_numerical_diagnostics.csv")
  utils::write.csv(rows, table_path, row.names = FALSE, fileEncoding = "UTF-8")

  metadata <- if (file.exists(metadata_path)) readRDS(metadata_path) else list()
  metadata$mfh_numerical_diagnostics <- list(
    recorded_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    selected_model = model_name,
    robust_refit_used = robust_used,
    robust_refit_failed = robust_failed,
    g3_available = if (is.null(diagnostics$g3_available)) NA else isTRUE(diagnostics$g3_available),
    requires_attention = any(
      rows$diagnostic_status %in% c(
        "generalized_inverse_used", "unavailable", "robust_refit_failed",
        "robust_refit_no_extended_diagnostics", "model_fit_failed",
        "model_fit_skipped"
      )
    ) || any(rows$mse_status == "unavailable_fisher_information"),
    table_file = normalizePath(table_path, winslash = "/", mustWork = FALSE),
    components = rows
  )
  dir.create(dirname(metadata_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(metadata, metadata_path)
  invisible(metadata$mfh_numerical_diagnostics)
}
