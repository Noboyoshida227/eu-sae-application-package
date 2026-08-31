`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

if (!exists("sae_read_table_input", mode = "function")) {
  .input_reader_path <- file.path("R", "input_readers.R")
  if (file.exists(.input_reader_path)) {
    source(.input_reader_path)
  }
}
if (!exists("sae_apply_numeric_poverty_lines", mode = "function")) {
  .pipeline_helper_path <- file.path("R", "pipeline_helpers.R")
  if (file.exists(.pipeline_helper_path)) {
    source(.pipeline_helper_path)
  }
}
if (!exists("sae_write_run_metadata", mode = "function")) {
  .release_control_path <- file.path("R", "release_controls.R")
  if (file.exists(.release_control_path)) source(.release_control_path)
}

source(file.path("R", "report_export.R"))

parse_years <- function(x) {
  if (is.numeric(x)) return(sort(as.integer(x)))
  raw <- trimws(unlist(strsplit(as.character(x %||% ""), ",")))
  raw <- raw[nzchar(raw)]
  yrs <- suppressWarnings(as.integer(raw))
  yrs <- yrs[!is.na(yrs)]
  sort(unique(yrs))
}

validate_app_config <- function(cfg) {
  errs <- character()

  years <- parse_years(cfg$years_keep %||% c(2012L, 2013L))
  if (length(years) != 2) {
    errs <- c(errs, "`years_keep` must contain exactly two years.")
  }

  analysis_seed <- suppressWarnings(as.integer(cfg$analysis_seed %||% 123L))
  if (!is.finite(analysis_seed) || analysis_seed < 0L) {
    errs <- c(errs, "`analysis_seed` must be a non-negative integer.")
  }
  mcpe_nB <- suppressWarnings(as.integer(cfg$mfh$mcpe_nB %||% 200L))
  if (!is.finite(mcpe_nB) || mcpe_nB < 50L) {
    errs <- c(errs, "`mfh.mcpe_nB` must be an integer of at least 50; 500 or more is recommended for production inference.")
  }
  lower_mult <- suppressWarnings(as.numeric(cfg$mfh$variance_lower_multiplier %||% 0))
  if (!is.finite(lower_mult) || lower_mult < 0 || lower_mult >= 1) {
    errs <- c(errs, "`mfh.variance_lower_multiplier` must be in [0, 1). Use 0 to replace only zero/missing lower-tail variances.")
  }

  steps <- cfg$run$steps %||% c("UFH", "MFH", "Comparison")
  bad_steps <- setdiff(steps, c("UFH", "MFH", "Comparison"))
  if (length(bad_steps) > 0) {
    errs <- c(errs, paste0("Invalid run steps: ", paste(bad_steps, collapse = ", ")))
  }

  bench_enabled <- isTRUE(cfg$benchmarking$enabled)
  bench_level <- cfg$benchmarking$level %||% cfg$mfh$benchmark_level %||%
    cfg$ufh$benchmark_level %||% "national"
  if (!bench_level %in% c("national", "custom", "region")) {
    errs <- c(errs, "`benchmarking.level` must be one of: national, custom.")
  }

  mfh_var <- cfg$mfh$var_choice %||% "sm_out"
  if (!mfh_var %in% c("direct", "sm_out", "sm_all")) {
    errs <- c(errs, "`mfh.var_choice` must be one of: direct, sm_out, sm_all.")
  }

  mfh_cov <- cfg$mfh$cov_choice %||% "rho_sm_out"
  if (!mfh_cov %in% c("direct", "rho_dir", "rho_sm_out", "rho_sm_all", "zero")) {
    errs <- c(errs, "`mfh.cov_choice` must be one of: direct, rho_dir, rho_sm_out, rho_sm_all, zero.")
  }
  mfh_model_rule <- toupper(cfg$mfh$diag_model %||% "MFH2")
  if (!mfh_model_rule %in% c("AUTO", "MFH1", "MFH2", "MFH3")) {
    errs <- c(errs, "`mfh.diag_model` must be one of: AUTO, MFH1, MFH2, MFH3.")
  }
  refvar_adjustment <- tolower(cfg$mfh$refvar_adjustment %||% "bonferroni")
  if (!refvar_adjustment %in% c("bonferroni", "bh", "none")) {
    errs <- c(errs, "`mfh.refvar_adjustment` must be one of: bonferroni, bh, none.")
  }
  refvar_alpha <- suppressWarnings(as.numeric(cfg$mfh$refvar_alpha %||% 0.05))
  if (!is.finite(refvar_alpha) || refvar_alpha <= 0 || refvar_alpha >= 1) {
    errs <- c(errs, "`mfh.refvar_alpha` must be strictly between 0 and 1.")
  }
  optional_paths <- c(
    benchmark_target_path = cfg$benchmarking$target_path,
    ufh_benchmark_target_path = cfg$ufh$benchmark_target_path %||% cfg$ufh$regional_benchmark_path,
    mfh_regional_benchmark_path = cfg$mfh$regional_benchmark_path,
    mfh_benchmark_target_path = cfg$mfh$benchmark_target_path,
    mfh_population_path = cfg$mfh$population_path,
    ufh_population_path = cfg$ufh$population_path
  )
  for (nm in names(optional_paths)) {
    p <- optional_paths[[nm]]
    if (!is.null(p) && length(p) > 0 && isTRUE(nzchar(p)) && !file.exists(p)) {
      errs <- c(errs, sprintf("Optional input `%s` does not exist: %s", nm, p))
    }
  }

  # MFH transformation. arcsin is intentionally not exposed for MFH; only
  # "log" (mean welfare) or "no" are accepted. The qmd reads
  # cfg$mfh$log_transform to drive its log-fit branch.
  mfh_trans <- cfg$mfh$transformation %||% "no"
  if (!mfh_trans %in% c("log", "no")) {
    errs <- c(errs, "`mfh.transformation` must be one of: log, no (arcsin is not supported for MFH).")
  }
  mfh_log <- isTRUE(cfg$mfh$log_transform)
  mfh_ind <- cfg$indicator_type %||% "poverty"
  if (mfh_log && !identical(mfh_ind, "mean_welfare")) {
    errs <- c(errs, "`mfh.log_transform = TRUE` is only valid when indicator_type = 'mean_welfare'.")
  }
  # MFH bias-correction method label. Only bc_sm or none are valid for
  # MFH (Duan smearing or naive). bc (integration-based) is for arcsin
  # only and arcsin is not an MFH option.
  mfh_bc_method <- cfg$mfh$bias_correction_method %||% cfg$mfh$backtransformation
  if (!is.null(mfh_bc_method) && !is.na(mfh_bc_method) &&
      !mfh_bc_method %in% c("bc_sm", "naive", "none")) {
    errs <- c(errs, "`mfh.bias_correction_method` must be one of: bc_sm, none (or unset).")
  }
  if (mfh_log && identical(mfh_bc_method, "bc")) {
    errs <- c(errs, "`bc` (integration-based) is only valid for arcsin. Use `bc_sm` (smearing) with MFH log.")
  }

  # The user-facing transformation can be "arcsin" (poverty), "log"
  # (mean welfare), or "no". Note: when the dropdown says "log", the
  # config builder in app.R stores `transformation = "no"` (because the
  # log step is applied in pre-processing, not by emdi::fh) and signals
  # the log fit via `cfg$log_transform = TRUE`. The validator therefore
  # treats "log" here as a valid value for forward-compat and for any
  # configs that record the user-facing choice directly.
  ufh_trans <- cfg$ufh$transformation %||% "arcsin"
  if (!ufh_trans %in% c("arcsin", "log", "no")) {
    errs <- c(errs, "`ufh.transformation` must be one of: arcsin, log, no.")
  }

  # Bias correction has two representations in the config:
  #   ufh$bias_correction        -- LOGICAL (TRUE = correct, FALSE = naive,
  #                                 NA when no transformation is used)
  #   ufh$bias_correction_method -- STRING label ("bc", "bc_sm", "none")
  #   ufh$backtransformation     -- legacy STRING alias ("bc", "bc_sm", "naive", "none")
  # We accept any of them. The "method" field is the source of truth for
  # the user-facing label and is checked here.
  ufh_bc_method <- cfg$ufh$bias_correction_method %||% cfg$ufh$backtransformation
  if (!is.null(ufh_bc_method) && !is.na(ufh_bc_method) &&
      !ufh_bc_method %in% c("bc", "bc_sm", "naive", "none")) {
    errs <- c(errs, "`ufh.bias_correction_method` must be one of: bc, bc_sm, none (or unset).")
  }
  if (identical(ufh_trans, "arcsin") && identical(ufh_bc_method, "bc_sm")) {
    errs <- c(errs, "`bc_sm` is only valid for the log transformation. Use `bc` with arcsin.")
  }
  if (identical(ufh_trans, "log") && identical(ufh_bc_method, "bc")) {
    errs <- c(errs, "`bc` (integration-based) is only valid for arcsin. Use `bc_sm` (smearing) with log.")
  }

  # Variance smoothing option for UFH (mirrors MFH menu). Only meaningful
  # when transformation == "no"; arcsin and log already stabilize variances.
  ufh_var <- cfg$ufh$var_choice %||% "sm_out"
  if (!ufh_var %in% c("direct", "sm_out", "sm_all")) {
    errs <- c(errs, "`ufh.var_choice` must be one of: direct, sm_out, sm_all.")
  }
  ufh_lasso_lambda <- cfg$ufh$lasso_lambda %||% "lambda.1se"
  if (!ufh_lasso_lambda %in% c("lambda.1se", "lambda.min")) {
    errs <- c(errs, "`ufh.lasso_lambda` must be one of: lambda.1se, lambda.min.")
  }
  mfh_lasso_lambda <- cfg$mfh$lasso_lambda %||% "lambda.1se"
  if (!mfh_lasso_lambda %in% c("lambda.1se", "lambda.min")) {
    errs <- c(errs, "`mfh.lasso_lambda` must be one of: lambda.1se, lambda.min.")
  }

  fgt <- as.integer(cfg$fgt_alpha %||% 0L)
  if (!fgt %in% c(0L, 1L, 2L)) {
    errs <- c(errs, "`fgt_alpha` must be 0, 1, or 2.")
  }
  if (fgt > 0L && identical(ufh_trans, "arcsin")) {
    errs <- c(errs, "Arcsin transformation is only valid for FGT(0). Set transformation to 'no' for FGT(1)/FGT(2).")
  }
  # Log is welfare-only; reject if combined with the poverty branch.
  ind_type <- cfg$indicator_type %||% "poverty"
  if (identical(ufh_trans, "log") && !identical(ind_type, "mean_welfare")) {
    errs <- c(errs, "Log transformation is only valid when indicator_type = 'mean_welfare'.")
  }

  ptype <- cfg$povline_type %||% "column"
  if (!ptype %in% c("column", "numeric")) {
    errs <- c(errs, "`povline_type` must be 'column' or 'numeric'.")
  }
  if (identical(ptype, "numeric")) {
    pval <- cfg$povline_value
    ok <- tryCatch({
      sae_validate_numeric_poverty_lines(pval, years)
      TRUE
    }, error = function(e) {
      errs <<- c(errs, conditionMessage(e))
      FALSE
    })
    if (!isTRUE(ok)) {
      errs <- unique(errs)
    }
  }

  list(valid = length(errs) == 0, errors = errs)
}

write_app_config <- function(cfg, path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Install with install.packages('yaml').")
  }
  yaml::write_yaml(cfg, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

required_packages_for_steps <- function(steps) {
  step_packages <- list(
    UFH = c(
      "sf", "data.table", "tidyverse", "car", "sae", "survey", "spdep",
      "MASS", "caret", "purrr", "gt", "scales", "viridis", "emdi",
      "rlang", "dplyr", "ggplot2", "writexl", "readxl", "conflicted",
      "tictoc", "matrixcalc", "knitr", "here", "yaml", "haven",
      "arrow", "glmnet"
    ),
    MFH = c(
      "sf", "data.table", "dplyr", "tidyr", "stringr", "purrr", "tictoc",
      "tidyverse", "car", "msae", "sae", "survey", "spdep", "MASS",
      "caret", "conflicted", "tibble", "emdi", "ggplot2", "gt",
      "viridis", "scales", "patchwork", "knitr", "here", "matrixcalc",
      "Matrix", "magic", "yaml", "haven", "arrow", "glmnet"
    ),
    Comparison = c(
      "dplyr", "tidyr", "ggplot2", "readxl", "writexl", "knitr",
      "patchwork", "sf", "purrr", "stringr", "scales", "emdi",
      "here", "yaml"
    )
  )

  report_packages <- c(
    "rmarkdown", "dplyr", "tidyr", "ggplot2", "readxl", "knitr",
    "patchwork", "sf", "scales", "here"
  )
  unique(c(unlist(step_packages[steps], use.names = FALSE), report_packages))
}

check_pipeline_packages <- function(steps) {
  packages <- required_packages_for_steps(steps)
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) return(invisible(TRUE))

  install_call <- sprintf(
    "install.packages(c(%s))",
    paste(sprintf("'%s'", missing), collapse = ", ")
  )
  stop(sprintf(
    paste0(
      "Missing required R package(s) for the selected analysis steps: %s\n\n",
      "Run source('install_packages.R') from the dashboard folder, or run:\n%s"
    ),
    paste(missing, collapse = ", "),
    install_call
  ), call. = FALSE)
}

split_csv <- function(x) {
  vals <- trimws(unlist(strsplit(x %||% "", ",")))
  vals[nzchar(vals)]
}

resolve_upload <- function(file_input, fallback = NULL) {
  if (is.null(file_input) || is.null(file_input$datapath) || !nzchar(file_input$datapath)) {
    return(fallback)
  }
  normalizePath(file_input$datapath, winslash = "/", mustWork = TRUE)
}

validate_mapped_input_columns <- function(survey_raw, rhs_raw, var_map, rhs_domain,
                                          povline_type = "column",
                                          indicator_type = "poverty") {
  survey_required <- c(
    year = var_map$year,
    domain = var_map$domain,
    psu = var_map$psu,
    weight = var_map$weight,
    hh_size = var_map$hh_size,
    welfare = var_map$welfare
  )
  if (identical(indicator_type, "poverty") && identical(povline_type, "column")) {
    survey_required <- c(survey_required, povline = var_map$povline)
  }
  survey_required <- survey_required[!is.na(survey_required) & nzchar(survey_required)]
  missing_survey <- survey_required[!survey_required %in% names(survey_raw)]

  rhs_year <- if (!is.null(var_map$year) && var_map$year %in% names(rhs_raw)) {
    var_map$year
  } else {
    "year"
  }
  rhs_required <- c(domain = rhs_domain, year = rhs_year)
  rhs_required <- rhs_required[!is.na(rhs_required) & nzchar(rhs_required)]
  missing_rhs <- rhs_required[!rhs_required %in% names(rhs_raw)]

  if (length(missing_survey) > 0 || length(missing_rhs) > 0) {
    parts <- character()
    if (length(missing_survey) > 0) {
      parts <- c(parts, sprintf(
        "survey data missing mapped column(s): %s",
        paste(sprintf("%s='%s'", names(missing_survey), missing_survey), collapse = ", ")
      ))
    }
    if (length(missing_rhs) > 0) {
      parts <- c(parts, sprintf(
        "auxiliary covariates missing mapped column(s): %s",
        paste(sprintf("%s='%s'", names(missing_rhs), missing_rhs), collapse = ", ")
      ))
    }
    stop("Input column mapping error: ", paste(parts, collapse = "; "), call. = FALSE)
  }

  invisible(TRUE)
}

# Helper: read uploaded or default data and harmonize variable names
load_and_harmonize <- function(survey_path, rhs_path, var_map, rhs_domain,
                               povline_type = "column", povline_value = NULL,
                               indicator_type = "poverty") {
  survey_raw <- tryCatch(sae_read_table_input(survey_path, "Survey data"), error = function(e) NULL)
  rhs_raw    <- tryCatch(sae_read_table_input(rhs_path, "Auxiliary covariates"), error = function(e) NULL)

  if (is.null(survey_raw) || is.null(rhs_raw)) return(NULL)

  validate_mapped_input_columns(
    survey_raw = survey_raw,
    rhs_raw = rhs_raw,
    var_map = var_map,
    rhs_domain = rhs_domain,
    povline_type = povline_type,
    indicator_type = indicator_type
  )

  # Build rename vector from var_map
  use_strata <- isTRUE({
    strata_var <- trimws(as.character(var_map$strata %||% ""))
    length(strata_var) == 1 && !is.na(strata_var) && nzchar(strata_var)
  })
  rename_vec <- c()
  if (var_map$domain  != "domain")  rename_vec <- c(rename_vec, domain  = var_map$domain)
  if (var_map$psu     != "psu")     rename_vec <- c(rename_vec, psu     = var_map$psu)
  if (var_map$welfare != "welfare") rename_vec <- c(rename_vec, welfare = var_map$welfare)
  if (var_map$weight  != "weight")  rename_vec <- c(rename_vec, weight  = var_map$weight)
  if (use_strata && !is.null(var_map$strata) && nzchar(var_map$strata) &&
      var_map$strata != "strata") {
    rename_vec <- c(rename_vec, strata = var_map$strata)
  }
  benchmark_level_var <- trimws(as.character(
    var_map$benchmark_level %||% var_map$region %||% ""
  ))
  if (nzchar(benchmark_level_var) &&
      benchmark_level_var %in% names(survey_raw) &&
      benchmark_level_var != "region") {
    rename_vec <- c(rename_vec, region = benchmark_level_var)
  }
  if (!is.null(var_map$hh_size) && nzchar(var_map$hh_size) &&
      var_map$hh_size != "hh_size") {
    rename_vec <- c(rename_vec, hh_size = var_map$hh_size)
  }
  if (var_map$year    != "year")    rename_vec <- c(rename_vec, year    = var_map$year)
  # Only rename povline column when the poverty line comes from data
  if (identical(povline_type, "column") && !is.null(var_map$povline) &&
      var_map$povline != "povline") {
    rename_vec <- c(rename_vec, povline = var_map$povline)
  }

  survey_data <- survey_raw
  for (new_name in names(rename_vec)) {
    old_name <- rename_vec[[new_name]]
    if (old_name %in% names(survey_data)) {
      names(survey_data)[names(survey_data) == old_name] <- new_name
    }
  }
  if (!use_strata && "strata" %in% names(survey_data)) {
    survey_data$strata <- NULL
  }
  if (nzchar(benchmark_level_var) &&
      !"region" %in% names(survey_data) &&
      benchmark_level_var %in% unname(rename_vec)) {
    copied_from <- names(rename_vec)[match(benchmark_level_var, unname(rename_vec))]
    if (!is.na(copied_from) && copied_from %in% names(survey_data)) {
      survey_data$region <- survey_data[[copied_from]]
    }
  }

  # When the poverty line is numeric, create the column. The value may be
  # a legacy scalar or a named year-to-line map.
  if (identical(povline_type, "numeric") && !is.null(povline_value)) {
    survey_data <- sae_apply_numeric_poverty_lines(
      survey_data,
      povline_value = povline_value,
      year_col = "year",
      output_col = "povline"
    )
  }
  if (all(c("weight", "hh_size") %in% names(survey_data))) {
    survey_data$population_weight <- suppressWarnings(
      as.numeric(survey_data$weight) * as.numeric(survey_data$hh_size)
    )
  }
  if ("domain" %in% names(survey_data)) {
    survey_data$domain <- trimws(as.character(survey_data$domain))
  }
  if ("region" %in% names(survey_data)) {
    survey_data$region <- trimws(as.character(survey_data$region))
  }

  rhs_data <- rhs_raw
  if (rhs_domain != "domain" && rhs_domain %in% names(rhs_data)) {
    names(rhs_data)[names(rhs_data) == rhs_domain] <- "domain"
  }
  rhs_year <- if (!is.null(var_map$year) && var_map$year %in% names(rhs_data)) {
    var_map$year
  } else {
    "year"
  }
  if (rhs_year != "year" && rhs_year %in% names(rhs_data)) {
    names(rhs_data)[names(rhs_data) == rhs_year] <- "year"
  }
  if ("domain" %in% names(rhs_data)) {
    rhs_data$domain <- trimws(as.character(rhs_data$domain))
  }

  list(survey = survey_data, rhs = rhs_data)
}

# Helper: compute per-year data summaries for diagnostics / brief
# Now indicator-aware: for "mean_welfare" the per-domain summary is
# the population-weighted domain mean of welfare (optionally on the log scale)
# instead of the FGT.
build_year_summary <- function(survey_data, yr, fgt_alpha = 0L,
                               indicator_type = "poverty",
                               log_transform = FALSE) {
  sv <- survey_data[survey_data$year == yr, ]
  n_domains <- length(unique(sv$domain))

  # Population-weighted per-domain summary so the dashboard agrees with the
  # `survey::svymean()`-based direct estimates the pipeline actually
  # exports. The analysis weight is weight * household size when available.
  analysis_weight <- if ("population_weight" %in% names(sv)) {
    sv$population_weight
  } else if (all(c("weight", "hh_size") %in% names(sv))) {
    suppressWarnings(as.numeric(sv$weight) * as.numeric(sv$hh_size))
  } else {
    sv$weight
  }
  weighted_mean_by_domain <- function(target, domain, weight) {
    domains <- unique(domain)
    out <- setNames(rep(NA_real_, length(domains)), as.character(domains))
    for (d in domains) {
      idx <- domain == d
      ok  <- idx & !is.na(target) & !is.na(weight)
      if (any(ok)) out[as.character(d)] <- stats::weighted.mean(target[ok], weight[ok])
    }
    out
  }

  if (identical(indicator_type, "mean_welfare")) {
    # Always summarize on the arithmetic-mean welfare scale so the
    # dashboard agrees with the exported Direct column (which is now
    # svymean(welfare), not exp(svymean(log welfare))). The model is
    # still fitted on the log scale internally when `log_transform` is
    # TRUE, but that's a model-internal detail and not what the
    # diagnostics tab should display.
    w <- as.numeric(sv$welfare)
    pov_rates <- weighted_mean_by_domain(w, sv$domain, analysis_weight)
  } else {
    fgt_vals <- if (fgt_alpha == 0L) {
      as.numeric(sv$welfare < sv$povline)
    } else {
      pmax(0, (sv$povline - sv$welfare) / sv$povline)^fgt_alpha
    }
    pov_rates <- weighted_mean_by_domain(fgt_vals, sv$domain, analysis_weight)
  }

  diag <- list(
    year                 = as.character(yr),
    model_type           = "UFH",
    convergence          = TRUE,
    n_domains            = n_domains,
    re_shapiro_pvalue    = NA_real_,
    re_shapiro_pass      = NA,
    resid_shapiro_pvalue = NA_real_,
    resid_shapiro_pass   = NA,
    variance_estimate    = NA_real_
  )

  bench <- list(
    estimate_range   = round(range(pov_rates, na.rm = TRUE), 4),
    estimate_median  = round(median(pov_rates, na.rm = TRUE), 4),
    estimate_mean    = round(mean(pov_rates, na.rm = TRUE), 4),
    cv_median        = NA_real_,
    cv_max           = NA_real_,
    n_cv_above_25pct = NA_integer_,
    mse_median       = NA_real_,
    n_domains        = n_domains,
    n_obs            = nrow(sv)
  )

  list(diag = diag, bench = bench)
}

# Read output Excel/CSV files after pipeline run for richer diagnostics.
# Paths follow the Package4 outputs/ layout.
read_pipeline_outputs <- function() {
  ufh_path <- "outputs/data/pov_fh.xlsx"
  mfh_path <- "outputs/data/pov_mfh.xlsx"
  sig_path <- "outputs/tables/statistical_significance_results.csv"

  ufh_shapiro_path <- "outputs/tables/ufh_shapiro_results.csv"
  mfh_shapiro_path <- "outputs/tables/mfh_shapiro_results.csv"

  result <- list()

  if (file.exists(ufh_path) && requireNamespace("readxl", quietly = TRUE)) {
    result$ufh <- tryCatch(readxl::read_excel(ufh_path), error = function(e) NULL)
  }
  if (file.exists(mfh_path) && requireNamespace("readxl", quietly = TRUE)) {
    result$mfh <- tryCatch(readxl::read_excel(mfh_path), error = function(e) NULL)
  }
  if (file.exists(sig_path)) {
    result$significance <- tryCatch(read.csv(sig_path), error = function(e) NULL)
  }
  if (file.exists(ufh_shapiro_path)) {
    result$ufh_shapiro <- tryCatch(read.csv(ufh_shapiro_path), error = function(e) NULL)
  }
  if (file.exists(mfh_shapiro_path)) {
    result$mfh_shapiro <- tryCatch(read.csv(mfh_shapiro_path), error = function(e) NULL)
  }
  result
}

# Enrich diagnostics with Shapiro-Wilk p-values from exported CSV
enrich_diag_with_shapiro <- function(diag, shapiro_df, yr, model_type = "UFH") {
  if (is.null(shapiro_df) || is.null(diag)) return(diag)
  yr_data <- shapiro_df[shapiro_df$year == yr, ]
  if (nrow(yr_data) == 0) return(diag)

  resid_row <- yr_data[yr_data$component == "residual", ]
  re_row    <- yr_data[yr_data$component == "random_effect", ]

  if (nrow(resid_row) > 0 && !is.na(resid_row$p_value[1])) {
    diag$resid_shapiro_pvalue <- resid_row$p_value[1]
    diag$resid_shapiro_pass   <- resid_row$p_value[1] >= 0.05
  }
  if (nrow(re_row) > 0 && !is.na(re_row$p_value[1])) {
    diag$re_shapiro_pvalue <- re_row$p_value[1]
    diag$re_shapiro_pass   <- re_row$p_value[1] >= 0.05
  }
  diag
}

# Build richer diagnostics from pipeline output Excel
enrich_diagnostics_from_output <- function(output_df, yr, model_type = "UFH") {
  if (is.null(output_df)) return(NULL)

  yr_data <- output_df[output_df$year == yr, ]
  if (nrow(yr_data) == 0) return(NULL)

  # Try to extract FH_Bench estimates and CVs
  est_col <- grep("FH_Bench$|MFH_Bench$", names(yr_data), value = TRUE)
  cv_col  <- grep("FH_Bench_CV$|MFH_Bench_CV$", names(yr_data), value = TRUE)
  mse_col <- grep("FH_Bench_MSE$|MFH_Bench_MSE$", names(yr_data), value = TRUE)
  if (length(est_col) == 0 && length(cv_col) == 0 && length(mse_col) == 0) {
    return(NULL)
  }

  bench <- list(n_domains = nrow(yr_data))

  if (length(est_col) > 0) {
    est_vals <- yr_data[[est_col[1]]]
    bench$estimate_range  <- round(range(est_vals, na.rm = TRUE), 4)
    bench$estimate_median <- round(median(est_vals, na.rm = TRUE), 4)
    bench$estimate_mean   <- round(mean(est_vals, na.rm = TRUE), 4)
  }
  if (length(cv_col) > 0) {
    cv_vals <- yr_data[[cv_col[1]]]
    bench$cv_median       <- round(median(cv_vals, na.rm = TRUE), 4)
    bench$cv_max          <- round(max(cv_vals, na.rm = TRUE), 4)
    bench$n_cv_above_25pct <- sum(cv_vals > 0.25, na.rm = TRUE)
  }
  if (length(mse_col) > 0) {
    bench$mse_median <- round(median(yr_data[[mse_col[1]]], na.rm = TRUE), 6)
  }

  bench
}

# ---------------------------------------------------------------------------
# run_pipeline_from_config()
#
# Package4 replacement for the Quarto-based pipeline runner. Instead of
# rendering .qmd files through the Quarto CLI, this version runs standalone
# R scripts (scripts/01_ufh.R, scripts/02_mfh.R, scripts/03_comparison.R) in
# child Rscript processes and renders the final report via rmarkdown::render().
# ---------------------------------------------------------------------------
.pipeline_step_timeout <- function() {
  timeout <- suppressWarnings(as.integer(Sys.getenv("SAE_STEP_TIMEOUT_SEC", "7200")))
  if (!is.finite(timeout) || timeout <= 0) timeout <- 7200L
  timeout
}

.pipeline_write_child_output <- function(step, output) {
  log_dir <- file.path("outputs", "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(log_dir, paste0(tolower(step), "_child_output.log"))
  writeLines(as.character(output), con = log_path, useBytes = TRUE)
  normalizePath(log_path, winslash = "/", mustWork = FALSE)
}

.pipeline_warning_summary <- function(output) {
  output <- trimws(as.character(output))
  output <- output[nzchar(output)]
  if (length(output) == 0) {
    return(character())
  }

  warning_lines <- output[grepl(
    paste(
      c(
        "^Warning messages:$",
        "^There (were|was) [0-9]+ warning",
        "^Warning in ",
        "^Warning:",
        "^[0-9]+:",
        "SAVE WARN"
      ),
      collapse = "|"
    ),
    output
  )]
  warning_lines <- unique(warning_lines)
  warning_lines <- warning_lines[!grepl("^Warning messages:$", warning_lines)]

  if (length(warning_lines) == 0) {
    return(character())
  }

  count_lines <- warning_lines[grepl("^There (were|was) [0-9]+ warning", warning_lines)]
  detail_lines <- setdiff(warning_lines, count_lines)
  detail_lines <- sub("^SAVE WARN .* ::\\s*", "", detail_lines)
  detail_lines <- sub("^[0-9]+:\\s*", "", detail_lines)
  detail_lines <- detail_lines[!grepl("package '.+' was built under R version", detail_lines)]
  detail_lines <- unique(detail_lines)
  detail_lines <- head(detail_lines, 3L)

  if (length(count_lines) == 0 && length(detail_lines) == 0) {
    return(character())
  }

  c(
    if (length(count_lines) > 0) {
      sprintf("Warnings reported: %s. See detailed step log.", count_lines[1])
    } else {
      "Warnings reported; see detailed step log."
    },
    if (length(detail_lines) > 0) paste0("  - ", detail_lines) else character()
  )
}

.pipeline_success_summary <- function(output) {
  output <- trimws(as.character(output))
  output <- output[nzchar(output)]
  if (length(output) == 0) {
    return(character())
  }

  setup_lines <- grepl(
    paste(
      c(
        "^Indicator:",
        "^Analysis years:",
        "^Transformation:",
        "^Bias correction:",
        "^Model-selection criterion:",
        "^Variance smoothing option:",
        "population_weight\\s*=\\s*weight \\* hh_size"
      ),
      collapse = "|"
    ),
    output
  )
  status_lines <- grepl(
    paste(
      c(
        "benchmarking enabled",
        "benchmarking disabled",
        "Benchmarking complete",
        "Benchmark groups:",
        "No population file supplied",
        "estimated domain populations",
        "MFH unavailable"
      ),
      collapse = "|"
    ),
    output,
    ignore.case = TRUE
  )
  key_lines <- output[setup_lines | status_lines]

  unique(head(key_lines, 12L))
}

.pipeline_failure_excerpt <- function(output, log_path) {
  output <- trimws(as.character(output))
  output <- output[nzchar(output)]
  if (length(output) == 0) {
    return(character())
  }

  diagnostic_lines <- output[grepl(
    paste(
      c(
        "^Error", "^Warning", "ERROR", "WARNING", "Execution halted",
        "failed", "cannot", "can't", "not found", "missing", "Traceback"
      ),
      collapse = "|"
    ),
    output,
    ignore.case = TRUE
  )]
  excerpt <- unique(c(diagnostic_lines, tail(output, 80L)))
  if (length(excerpt) > 120L) {
    excerpt <- c(
      head(excerpt, 120L),
      sprintf("... additional failure output omitted from main log; see %s", log_path)
    )
  }
  excerpt
}

.pipeline_log_child_summary <- function(step, output, status, logger = message) {
  output <- as.character(output)
  if (length(output) == 0) {
    return(invisible(NULL))
  }

  log_path <- .pipeline_write_child_output(step, output)
  logger(sprintf("[%s] Detailed step log saved to: %s", step, log_path))

  if (status != 0L) {
    log_lines <- .pipeline_failure_excerpt(output, log_path)
  } else {
    log_lines <- c(
      .pipeline_success_summary(output),
      .pipeline_warning_summary(output)
    )
  }

  for (line in unique(log_lines)) {
    logger(sprintf("[%s] %s", step, line))
  }
  invisible(log_path)
}

.pipeline_run_step <- function(step, script, config_path, logger = message) {
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  if (!file.exists(rscript)) rscript <- "Rscript"

  script_path <- normalizePath(script, winslash = "/", mustWork = TRUE)
  timeout <- .pipeline_step_timeout()
  r_literal <- function(x) {
    paste0("\"", gsub("\"", "\\\\\"", gsub("\\\\", "\\\\\\\\", as.character(x))), "\"")
  }
  wrapper_path <- tempfile(pattern = paste0("sae_step_", step, "_"), fileext = ".R")
  writeLines(
    c(
      "options(warn = 1)",
      sprintf("Sys.setenv(SAE_APP_CONFIG = %s, SAE_APP_STEP = %s)",
              r_literal(config_path), r_literal(step)),
      sprintf("source(%s, local = new.env(parent = globalenv()))",
              r_literal(script_path))
    ),
    con = wrapper_path,
    useBytes = TRUE
  )
  on.exit(unlink(wrapper_path, force = TRUE), add = TRUE)

  output <- tryCatch(
    suppressWarnings(system2(
      command = rscript,
      args = c("--vanilla", wrapper_path),
      stdout = TRUE,
      stderr = TRUE,
      timeout = timeout
    )),
    error = function(e) {
      out <- conditionMessage(e)
      attr(out, "status") <- 1L
      out
    }
  )

  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  status <- as.integer(status)
  child_log_path <- .pipeline_log_child_summary(step, output, status, logger = logger)

  list(
    status = status,
    output = as.character(output),
    child_log_path = child_log_path,
    timed_out = identical(status, 124L)
  )
}

.pipeline_clean_outputs <- function(logger = message) {
  roots <- c("outputs/data", "outputs/tables", "outputs/figures", "outputs/logs")
  for (root in roots) {
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    generated <- list.files(root, full.names = TRUE, recursive = FALSE,
                            all.files = TRUE, no.. = TRUE)
    generated <- generated[basename(generated) != ".gitkeep"]
    if (length(generated) > 0) {
      unlink(generated, recursive = TRUE, force = TRUE)
    }
  }
  unlink(
    c("outputs/final_report.html", "outputs/final_report.docx", "outputs/comparison_ai_note.html"),
    force = TRUE
  )
  logger("Cleared previous generated outputs for this run.")
}

render_final_report <- function(include_ai = FALSE, logger = message,
                                progress_callback = NULL, include_word = TRUE) {
  report_rmd <- "report.Rmd"
  if (!file.exists(report_rmd)) {
    logger("Warning: report.Rmd not found; skipping report rendering.")
    return(invisible(FALSE))
  }

  if (is.function(progress_callback)) {
    progress_callback("start", "Report")
  }

  logger("Rendering final report...")

  # Auto-detect pandoc for batch mode (outside RStudio).
  if (!rmarkdown::pandoc_available()) {
    pandoc_candidates <- c(
      Sys.getenv("RSTUDIO_PANDOC"),
      dirname(Sys.which("pandoc")),
      dirname(Sys.which("quarto")),
      file.path(Sys.getenv("ProgramFiles"), "RStudio",
                "resources", "app", "bin", "quarto", "bin", "tools"),
      file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc"),
      "/Applications/RStudio.app/Contents/Resources/app/bin/quarto/bin/tools",
      "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
      "/Applications/Quarto.app/Contents/Resources/app/bin/tools",
      "/Applications/Quarto.app/Contents/Resources/app/quarto/bin/tools",
      "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools",
      "/usr/lib/rstudio/resources/app/quarto/bin/tools",
      "/usr/local/bin",
      "/opt/homebrew/bin",
      "/usr/bin"
    )
    for (p in pandoc_candidates) {
      if (nzchar(p) && (file.exists(file.path(p, "pandoc.exe")) ||
                        file.exists(file.path(p, "pandoc")))) {
        Sys.setenv(RSTUDIO_PANDOC = p)
        break
      }
    }
  }

  rmarkdown::render(
    input       = report_rmd,
    output_file = file.path(getwd(), "outputs", "final_report.html"),
    params      = list(include_ai = isTRUE(include_ai)),
    encoding    = "UTF-8",
    quiet       = TRUE
  )

  logger(sprintf(
    "Report rendered: outputs/final_report.html (AI interpretations %s)",
    if (isTRUE(include_ai)) "included where available" else "not included"
  ))

  if (isTRUE(include_word)) {
    # Never mistake a prior run's Word report for the newly rendered HTML.
    unlink("outputs/final_report.docx", force = TRUE)
    tryCatch({
      sae_render_word_report()
      logger("Word report rendered: outputs/final_report.docx (editable text and tables).")
    }, error = function(e) {
      logger(paste("Warning: HTML report is ready, but Word export failed:", conditionMessage(e)))
    })
  }
  if (is.function(progress_callback)) {
    progress_callback("complete", "Report")
  }
  invisible(TRUE)
}

run_pipeline_from_config <- function(config_path, logger = message,
                                     progress_callback = NULL,
                                     render_report = TRUE,
                                     include_ai = FALSE) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Install with install.packages('yaml').")
  }

  config_path <- normalizePath(config_path, winslash = "/", mustWork = TRUE)
  cfg <- yaml::read_yaml(config_path)
  v <- validate_app_config(cfg)
  if (!v$valid) {
    stop(paste(v$errors, collapse = "\n"))
  }

  steps <- cfg$run$steps %||% c("UFH", "MFH", "Comparison")
  check_pipeline_packages(steps)

  # Set SAE_APP_CONFIG so the sourced R scripts can read the config
  old_cfg <- Sys.getenv("SAE_APP_CONFIG", unset = "")
  on.exit({
    if (nzchar(old_cfg)) {
      Sys.setenv(SAE_APP_CONFIG = old_cfg)
    } else {
      Sys.unsetenv("SAE_APP_CONFIG")
    }
  }, add = TRUE)
  Sys.setenv(SAE_APP_CONFIG = config_path)

  # Create output directories
  out_dirs <- c("outputs/data", "outputs/tables", "outputs/figures")
  for (d in out_dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  .pipeline_clean_outputs(logger = logger)
  if (exists("sae_write_run_metadata", mode = "function")) {
    sae_write_run_metadata(cfg, config_path = config_path)
    logger("Recorded application version, seed, package versions, session information, and input hashes.")
  }

  # Step-to-script mapping
  step_scripts <- c(
    UFH        = "scripts/01_ufh.R",
    MFH        = "scripts/02_mfh.R",
    Comparison = "scripts/03_comparison.R"
  )

  # Execute each pipeline step in a child R process so crashes and package-level
  # aborts do not take down the Shiny process.
  for (step in steps) {
    script <- step_scripts[[step]]
    if (is.null(script) || !file.exists(script)) {
      stop(sprintf("Script for step '%s' not found: %s", step, script %||% "(unmapped)"))
    }

    if (is.function(progress_callback)) {
      progress_callback("start", step)
    }

    logger(sprintf("Running step: %s (%s)", step, script))
    step_unavailable <- FALSE
    step_result <- .pipeline_run_step(step, script, config_path, logger = logger)
    if (step_result$status != 0L) {
      step_log_hint <- if (!is.null(step_result$child_log_path) &&
                           nzchar(step_result$child_log_path %||% "")) {
        sprintf(" Detailed step log: %s", step_result$child_log_path)
      } else {
        " See run.log for captured output."
      }
      if (identical(step, "MFH") &&
          any(grepl("MFH unavailable:", step_result$output, fixed = TRUE))) {
        step_unavailable <- TRUE
        logger("MFH artifacts were written with unavailable/NA placeholders; continuing to Comparison.")
      } else if (isTRUE(step_result$timed_out)) {
        stop(sprintf(
          "Step '%s' exceeded the timeout (%s seconds).%s",
          step, .pipeline_step_timeout(), step_log_hint
        ))
      } else {
        stop(sprintf(
          "Step '%s' failed with exit status %s.%s",
          step, step_result$status, step_log_hint
        ))
      }
    }
    if (identical(step, "MFH") &&
        exists("sae_record_mfh_numerical_diagnostics", mode = "function") &&
        file.exists(file.path("outputs", "data", "mfh_artifacts.rds"))) {
      numerical_summary <- tryCatch(
        sae_record_mfh_numerical_diagnostics(),
        error = function(e) {
          logger(sprintf("Warning: could not record MFH numerical diagnostics: %s", conditionMessage(e)))
          NULL
        }
      )
      if (!is.null(numerical_summary)) {
        logger("Recorded MFH numerical diagnostics in run metadata and outputs/tables/mfh_numerical_diagnostics.csv.")
        if (isTRUE(numerical_summary$requires_attention)) {
          logger(paste(
            "Warning: MFH numerical diagnostics require review;",
            "check inversion methods and g3/MSE availability before using the estimates."
          ))
        }
      }
    }
    logger(sprintf(
      "Completed step: %s%s",
      step,
      if (step_unavailable) " (marked unavailable)" else ""
    ))

    if (is.function(progress_callback)) {
      progress_callback("complete", step)
    }
  }

  if (isTRUE(render_report)) {
    render_final_report(
      include_ai = include_ai,
      logger = logger,
      progress_callback = progress_callback
    )
  }

  invisible(TRUE)
}
