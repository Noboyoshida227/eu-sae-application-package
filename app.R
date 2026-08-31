library(shiny)

# Shiny's default upload limit is 5 MB, which is too small for many survey,
# auxiliary, and geometry RDS files. Users can override this before launching:
# Sys.setenv(SAE_SHINY_MAX_UPLOAD_MB = "1000")
.upload_limit_mb <- suppressWarnings(as.numeric(Sys.getenv("SAE_SHINY_MAX_UPLOAD_MB", "500")))
if (!is.finite(.upload_limit_mb) || .upload_limit_mb < 5) {
  .upload_limit_mb <- 500
}
options(shiny.maxRequestSize = .upload_limit_mb * 1024^2)

# ---- Startup validation ----
# Check R version (>= 4.2 required by current CRAN dependencies)
if (getRversion() < "4.2.0") {
  stop(sprintf(
    "R >= 4.2.0 is required (you have %s). Please update R from https://cran.r-project.org/",
    getRversion()
  ))
}
if (requireNamespace("here", quietly = TRUE)) here::i_am("app.R")

.detect_app_dir <- function() {
  frame_files <- vapply(
    sys.frames(),
    function(env) {
      ofile <- env$ofile
      if (is.null(ofile) || length(ofile) == 0) "" else as.character(ofile)[1]
    },
    character(1)
  )
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files) > 0) {
    return(dirname(normalizePath(tail(frame_files, 1), winslash = "/", mustWork = TRUE)))
  }

  if (requireNamespace("here", quietly = TRUE)) {
    here_dir <- tryCatch(here::here(), error = function(e) "")
    if (nzchar(here_dir)) {
      return(normalizePath(here_dir, winslash = "/", mustWork = TRUE))
    }
  }

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    editor_path <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(editor_path)) {
      return(dirname(normalizePath(editor_path, winslash = "/", mustWork = TRUE)))
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

.app_dir <- .detect_app_dir()
if (!identical(normalizePath(getwd(), winslash = "/", mustWork = TRUE), .app_dir)) {
  setwd(.app_dir)
}

# Check that all required source files exist before loading
.required_source_files <- c(
  "R/input_readers.R",
  "R/input_paths.R",
  "R/pipeline_helpers.R",
  "R/release_controls.R",
  "app_support.R",
  "R/validation_checks.R",
  "R/multilingual.R",
  "R/llm_assistant.R",
  "R/brief_generator.R",
  "R/normality_evaluator.R",
  "R/comparison_report_ai.R",
  "R/indicator_helpers.R",
  "R/variance_policy.R"
)
.missing_files <- .required_source_files[!file.exists(.required_source_files)]
if (length(.missing_files) > 0) {
  stop(sprintf(
    "Missing required files: %s\nPlease ensure you are running the app from the project root directory and all files are present.",
    paste(.missing_files, collapse = ", ")
  ))
}

# Check critical package dependencies with helpful messages
.critical_packages <- list(
  here       = "install.packages('here')      # needed for analysis scripts",
  yaml       = "install.packages('yaml')",
  dplyr      = "install.packages('dplyr')",
  readxl     = "install.packages('readxl')",
  haven      = "install.packages('haven')      # needed for Stata/SPSS/SAS inputs",
  arrow      = "install.packages('arrow')      # needed for Parquet/Feather inputs",
  glmnet     = "install.packages('glmnet')     # needed for optional LASSO screening",
  rmarkdown  = "install.packages('rmarkdown') # needed for report rendering",
  httr       = "install.packages('httr')      # needed for AI Assistant",
  jsonlite   = "install.packages('jsonlite') # needed for AI Assistant",
  sf         = "install.packages('sf')         # requires GDAL/GEOS/PROJ system libraries on macOS/Linux"
)
.missing_pkgs <- names(.critical_packages)[!vapply(names(.critical_packages), requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing_pkgs) > 0) {
  install_hints <- vapply(.missing_pkgs, function(p) .critical_packages[[p]], character(1))
  warning(sprintf(
    paste0(
      "Some recommended packages are not installed:\n  %s\n",
      "Install them with:\n  %s\n",
      "Or run: source('install_packages.R')"
    ),
    paste(.missing_pkgs, collapse = ", "),
    paste(install_hints, collapse = "\n  ")
  ), immediate. = TRUE)
}

data_dir_path <- function() {
  normalizePath("Data", winslash = "/", mustWork = FALSE)
}

# Check that the package data folder exists. Individual filenames are chosen
# by the user in the dashboard and can vary across countries.
if (!dir.exists(data_dir_path())) {
  warning(sprintf(
    "Optional package data folder not found: %s\nYou can still use Browse in the dashboard to choose survey, auxiliary covariates, and shapefiles/geometries files from another folder on your computer.",
    data_dir_path()
  ), immediate. = TRUE)
}

source("R/input_readers.R")
source("R/input_paths.R")
source("R/pipeline_helpers.R")
source("R/release_controls.R")
source("app_support.R")
source("R/validation_checks.R")
source("R/multilingual.R")
source("R/llm_assistant.R")
source("R/brief_generator.R")
source("R/normality_evaluator.R")
source("R/comparison_report_ai.R")
source("R/variance_policy.R")
source("R/indicator_helpers.R")

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

resolve_data_input <- function(file_input, fallback = NULL) {
  resolve_upload(file_input, fallback)
}

display_data_path <- function(path) {
  path <- path %||% ""
  if (nzchar(path)) path else "(not selected)"
}

filter_to_analysis_years <- function(df, years) {
  if (is.null(df) || !"year" %in% names(df) || length(years) == 0) {
    return(df)
  }
  year_values <- suppressWarnings(as.integer(as.character(df$year)))
  keep <- year_values %in% as.integer(years)
  df[keep %in% TRUE, , drop = FALSE]
}

missing_data_inputs <- function(survey_path, rhs_path, shp_path) {
  paths <- c(Survey = survey_path %||% "",
             Auxiliary = rhs_path %||% "",
             Geometry = shp_path %||% "")
  names(paths)[!nzchar(paths) | !file.exists(paths)]
}

dashboard_setup_path <- function() {
  file.path("app_runs", "_last_dashboard_setup.yml")
}

dashboard_setup_file_dir <- function() {
  file.path("app_runs", "_last_setup_files")
}

dashboard_setup_defaults <- function() {
  list(
    version = 1L,
    saved_at = NULL,
    data_files = list(
      survey_file = "",
      rhs_file = "",
      shp_file = "",
      regional_benchmark_file = "",
      population_file = ""
    ),
    inputs = list(
      years = "2012,2013",
      country_name = "",
      analysis_seed = 123L,
      run_label = "",
      steps = c("UFH", "MFH", "Comparison"),
      do_benchmark = FALSE,
      benchmark_level = "national",
      var_benchmark_level = "",
      var_year = "year",
      var_domain = "prov",
      var_psu = "ea_id",
      var_weight = "weight",
      var_strata = "",
      var_hh_size = "hhsize",
      var_welfare = "income",
      indicator_type = "poverty",
      povline_type = "column",
      var_povline = "povline",
      povline_numeric = 5000,
      povline_numeric_by_year = list(),
      fgt_alpha = "0",
      currency_symbol = "EUR",
      rhs_domain = "prov",
      shp_domain = "prov",
      ufh_transformation = "arcsin",
      ufh_backtrans = "bc",
      ufh_var_choice = "sm_out",
      ufh_ic_criterion = "BIC",
      ufh_lasso_enabled = FALSE,
      ufh_lasso_lambda = "lambda.1se",
      ufh_candidates_y1 = "",
      ufh_candidates_y2 = "",
      mfh_transformation = "no",
      mfh_backtrans = "bc_sm",
      mfh_ic_criterion = "AIC",
      mfh_lasso_enabled = FALSE,
      mfh_lasso_lambda = "lambda.1se",
      mfh_var_choice = "sm_out",
      mfh_cov_choice = "rho_sm_out",
      mfh_diag_model = "MFH2",
      fit_mfh3 = FALSE,
      mfh_refvar_adjustment = "bonferroni",
      mcpe_bootstrap_replicates = 200L,
      mfh_candidates_y1 = "",
      mfh_candidates_y2 = "",
      psu_consistent = FALSE,
      llm_enabled = FALSE,
      llm_external_consent = FALSE,
      language = "en"
    )
  )
}

read_dashboard_setup <- function(path = dashboard_setup_path()) {
  if (!file.exists(path) || !requireNamespace("yaml", quietly = TRUE)) {
    return(NULL)
  }
  tryCatch(yaml::read_yaml(path), error = function(e) NULL)
}

write_dashboard_setup <- function(setup, path = dashboard_setup_path()) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to save dashboard setup.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(setup, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

setup_file_path <- function(file_name) {
  sae_resolve_input_path(file_name, root = .app_dir, allow_basename = TRUE)
}

setup_file_exists <- function(file_name) {
  path <- setup_file_path(file_name)
  !is.null(path) && file.exists(path)
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
  domain_values <- trimws(as.character(sv$domain))
  valid_domain <- !is.na(domain_values) & nzchar(domain_values)
  n_domains <- length(unique(domain_values[valid_domain]))

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
    domain <- trimws(as.character(domain))
    target <- suppressWarnings(as.numeric(target))
    weight <- suppressWarnings(as.numeric(weight))
    domains <- unique(domain[!is.na(domain) & nzchar(domain)])
    out <- setNames(rep(NA_real_, length(domains)), as.character(domains))
    for (d in domains) {
      idx <- !is.na(domain) & domain == d
      ok <- idx & is.finite(target) & is.finite(weight) & weight > 0
      if (any(ok, na.rm = TRUE)) {
        out[as.character(d)] <- stats::weighted.mean(target[ok], weight[ok])
      }
    }
    out
  }
  safe_scalar_summary <- function(x, fn) {
    x <- x[is.finite(x)]
    if (!length(x)) return(NA_real_)
    fn(x)
  }
  safe_range <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(c(NA_real_, NA_real_))
    range(x)
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
    estimate_range   = round(safe_range(pov_rates), 4),
    estimate_median  = round(safe_scalar_summary(pov_rates, median), 4),
    estimate_mean    = round(safe_scalar_summary(pov_rates, mean), 4),
    cv_median        = NA_real_,
    cv_max           = NA_real_,
    n_cv_above_25pct = NA_integer_,
    mse_median       = NA_real_,
    n_domains        = n_domains,
    n_obs            = nrow(sv)
  )

  list(diag = diag, bench = bench)
}

# Read output Excel files after pipeline run for richer diagnostics
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


# Helper: create a label with an inline tooltip icon
tip_label <- function(label_text, tip_text) {
  span(class = "tt-wrap",
    label_text,
    span(class = "tt-icon", "?"),
    span(class = "tt-text", tip_text)
  )
}

mapping_selectize <- function(input_id, label, selected = "",
                              placeholder = "Search or type column name") {
  selected <- selected %||% ""
  selectizeInput(
    input_id,
    label,
    choices = unique(c("", selected)),
    selected = selected,
    multiple = FALSE,
    options = list(
      create = TRUE,
      createOnBlur = TRUE,
      persist = FALSE,
      placeholder = placeholder,
      maxOptions = 1000
    )
  )
}

ui <- fluidPage(
  # ---- Cover Page ----
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('fadeTransition', function(msg) {
        var hide = document.getElementById(msg.hide);
        var show = document.getElementById(msg.show);
        hide.style.transition = 'opacity 0.45s ease';
        hide.style.opacity = '0';
        setTimeout(function(){
          hide.style.display = 'none';
          hide.style.opacity = '1';
          show.style.opacity = '0';
          show.style.display = (show.id === 'main_app') ? 'block' : 'flex';
          show.style.transition = 'opacity 0.45s ease';
          setTimeout(function(){ show.style.opacity = '1'; }, 30);
        }, 450);
      });
      Shiny.addCustomMessageHandler('mappingValidity', function(msg) {
        var group = $('#' + msg.id).closest('.form-group');
        if (!group.length) return;
        if (msg.invalid) {
          group.addClass('mapping-invalid');
          group.attr('title', msg.message || 'Column not found in selected dataset');
        } else {
          group.removeClass('mapping-invalid');
          group.removeAttr('title');
        }
      });
      Shiny.addCustomMessageHandler('clearFileInput', function(id) {
        var input = document.getElementById(id);
        if (input) {
          input.value = '';
          $(input).trigger('change');
        }
        var group = $('#' + id).closest('.form-group');
        group.find('.file-caption-name').attr('title', '').text('No file selected');
        group.find('input[type=text]').val('');
      });
    ")),
    tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
    #cover_page {
      position: fixed; top: 0; left: 0; width: 100%; height: 100%;
      background: linear-gradient(160deg, #0f1b3d 0%, #1a2f6b 35%, #1e4d8f 65%, #2a6cb0 100%);
      z-index: 9999; display: flex; flex-direction: column;
      align-items: center; justify-content: center;
      font-family: 'Inter', 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      color: #ffffff; text-align: center;
      animation: fadeIn 1s ease-out;
      overflow: hidden;
    }
    #cover_page::before {
      content: ''; position: absolute; top: -50%; left: -50%;
      width: 200%; height: 200%;
      background: radial-gradient(ellipse at 30% 20%, rgba(109,213,237,0.08) 0%, transparent 50%),
                  radial-gradient(ellipse at 70% 80%, rgba(59,130,200,0.06) 0%, transparent 50%);
      pointer-events: none;
    }
    @keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }
    #cover_page .cover-content {
      position: relative; z-index: 1;
      display: flex; flex-direction: column; align-items: center;
      max-width: 780px; padding: 0 24px;
    }
    #cover_page .cover-illustration {
      margin-bottom: 1.4em;
    }
    #cover_page .cover-illustration img {
      border-radius: 10px;
      box-shadow: 0 12px 48px rgba(0,0,0,0.35), 0 2px 12px rgba(0,0,0,0.2);
      border: 1px solid rgba(255,255,255,0.08);
      max-height: 58vh;
      width: auto;
      object-fit: contain;
    }
    #cover_page .cover-label {
      font-size: 0.78em; font-weight: 600; letter-spacing: 3px;
      text-transform: uppercase; color: rgba(157,213,245,0.85);
      margin-bottom: 0.5em;
    }
    #cover_page h1 {
      font-size: 2.6em; font-weight: 700; margin: 0 0 0.2em;
      letter-spacing: -0.5px; line-height: 1.15;
      background: linear-gradient(180deg, #ffffff 30%, rgba(200,225,255,0.85) 100%);
      -webkit-background-clip: text; -webkit-text-fill-color: transparent;
      background-clip: text;
    }
    #cover_page .subtitle {
      font-size: 1.05em; font-weight: 300; color: rgba(220,235,255,0.85);
      max-width: 560px; line-height: 1.55; margin-bottom: 0.3em;
    }
    #cover_page .cover-divider {
      width: 60px; height: 2px; margin: 0.7em auto 0.8em;
      background: linear-gradient(90deg, transparent, rgba(109,213,237,0.5), transparent);
      border: none;
    }
    #cover_page .tagline {
      font-size: 0.85em; font-weight: 400; color: rgba(180,210,240,0.65);
      letter-spacing: 0.5px; margin-bottom: 1.8em;
    }
    #enter_app_btn {
      font-size: 1em; font-weight: 500; padding: 14px 52px;
      background: linear-gradient(135deg, rgba(109,213,237,0.2), rgba(59,123,213,0.25));
      color: #fff; letter-spacing: 0.8px;
      border: 1.5px solid rgba(109,213,237,0.4);
      border-radius: 50px; cursor: pointer;
      transition: all 0.35s ease;
      backdrop-filter: blur(8px);
      box-shadow: 0 2px 16px rgba(0,0,0,0.15);
    }
    #enter_app_btn:hover {
      background: linear-gradient(135deg, rgba(109,213,237,0.35), rgba(59,123,213,0.4));
      border-color: rgba(109,213,237,0.7);
      transform: translateY(-2px);
      box-shadow: 0 6px 28px rgba(109,213,237,0.2);
    }
    #cover_page .cover-footer {
      margin-top: 1.8em;
      text-align: center; font-size: 0.75em; font-weight: 400;
      color: rgba(180,210,240,0.35); letter-spacing: 0.3px;
    }
    #main_app { display: none; }
    #main_app.visible { display: block; }

    /* ---- Tooltips ---- */
    .tt-wrap {
      position: relative;
      display: inline;
    }
    .tt-wrap .tt-icon {
      display: inline-block;
      width: 15px; height: 15px;
      line-height: 15px;
      text-align: center;
      font-size: 10px; font-weight: 700;
      color: #fff;
      background: #6b8db5;
      border-radius: 50%;
      cursor: help;
      margin-left: 3px;
      vertical-align: middle;
    }
    .tt-wrap .tt-text {
      visibility: hidden;
      opacity: 0;
      position: absolute;
      z-index: 1000;
      left: 0; top: 1.6em;
      width: 300px;
      background: #1a2a4a;
      color: #e0ecf8;
      padding: 10px 14px;
      border-radius: 8px;
      font-size: 0.85em;
      font-weight: 400;
      line-height: 1.5;
      box-shadow: 0 4px 20px rgba(0,0,0,0.3);
      border: 1px solid rgba(109,213,237,0.25);
      transition: opacity 0.2s ease, visibility 0.2s ease;
      pointer-events: none;
    }
    .tt-wrap:hover .tt-text {
      visibility: visible;
      opacity: 1;
    }
    .form-group.mapping-invalid label {
      color: #b00020;
    }
    .form-group.mapping-invalid .selectize-input {
      border-color: #b00020 !important;
      box-shadow: 0 0 0 2px rgba(176,0,32,0.12) !important;
    }
    .form-group.mapping-invalid .selectize-input.focus {
      border-color: #b00020 !important;
      box-shadow: 0 0 0 3px rgba(176,0,32,0.18) !important;
    }
    .mapping-validation-summary {
      color: #8a1f11;
      background: #fff3f1;
      border: 1px solid #f1b8b0;
      border-radius: 6px;
      padding: 8px 10px;
      font-size: 12px;
      margin: 4px 0 12px 0;
    }
    .benchmark-format-note {
      color: #445;
      background: #f7f9fc;
      border: 1px solid #d9e2ef;
      border-radius: 6px;
      padding: 10px 12px;
      font-size: 12px;
      line-height: 1.45;
      margin: -4px 0 12px 0;
    }
    .benchmark-format-note .note-title {
      font-weight: 600;
      color: #24324a;
      margin-bottom: 4px;
    }
    .benchmark-format-note p {
      margin: 0 0 6px 0;
    }
    .benchmark-format-note ul {
      margin: 0;
      padding-left: 18px;
    }
    .benchmark-clear-row {
      margin: 2px 0 8px 0;
    }
  "))),

  div(id = "cover_page",
    div(class = "cover-content",
      div(class = "cover-illustration",
        tags$img(src = "eu_poverty_map.png",
                 style = "width: 90%; max-width: 680px;")
      ),
      div(class = "cover-label", "Small Area Estimation Platform"),
      h1("EU Poverty Mapping"),
      div(class = "subtitle",
        "Poverty rate estimation across NUTS-3 areas using Fay\u2013Herriot models with benchmarking and AI-assisted diagnostics"
      ),
      tags$hr(class = "cover-divider"),
      div(class = "tagline",
        "Univariate & Multivariate FH  \u00b7  Benchmarked Estimates  \u00b7  Automated Reporting"
      ),
      actionButton("enter_app_btn", "Get Started", class = "btn"),
      div(class = "cover-footer",
        "World Bank Group"
      )
    )
  ),

  # ---- Main App (hidden until cover is dismissed) ----
  div(id = "main_app",
    titlePanel("EU Poverty Mapping App"),
    sidebarLayout(
    sidebarPanel(
      # ---- Analysis settings ----
      textInput("years",
        tip_label("Analysis years (two, comma-separated)", "Enter exactly two years separated by a comma. The pipeline estimates poverty for each year and tests for significant changes between them."),
        value = "2012,2013"),
      textInput("country_name",
        tip_label("Country or territory", "Used only in report and brief titles; it does not affect estimation."),
        value = ""),
      numericInput("analysis_seed",
        tip_label("Analysis seed", "Controls LASSO folds and bootstrap draws so identical inputs and settings reproduce the same stochastic analysis."),
        value = 123, min = 0, step = 1),
      textInput("run_label",
        tip_label("Run label", "Optional short label appended to the saved app_runs folder so outputs from different runs are easier to distinguish."),
        value = ""),
      checkboxGroupInput("steps",
        tip_label("Pipeline steps", "UFH fits a univariate Fay-Herriot model per year. MFH fits multivariate models (MFH1, MFH2, MFH3) that borrow strength across time. Comparison merges both results side by side with maps and precision metrics."),
        choices = c("UFH", "MFH", "Comparison"),
        selected = c("UFH", "MFH", "Comparison")
      ),
      tags$hr(),

      # ---- Reusable setup ----
      h4("Saved setup"),
      tags$div(
        style = "font-size: 12px; color: #556; margin: -4px 0 10px 0;",
        "Save or reload dashboard settings so later runs only require small edits. If files were selected with Browse, the app saves local setup copies under app_runs/_last_setup_files for the next session."
      ),
      div(
        style = "display: flex; gap: 6px; flex-wrap: wrap; margin-bottom: 8px;",
        actionButton("load_setup_btn", "Load Last Setup", class = "btn-default btn-sm"),
        actionButton("save_setup_btn", "Save Current Setup", class = "btn-default btn-sm"),
        actionButton("reset_setup_btn", "Reset to Defaults", class = "btn-warning btn-sm")
      ),
      uiOutput("saved_setup_status"),
      tags$hr(),

      # ---- Resources ----
      h4("Resources"),
      tags$div(
        style = "font-size: 12px; color: #556; margin: -4px 0 10px 0;",
        "Detailed instructions and methodological notes are maintained outside the app. ",
        "Use these files as the authoritative user guidance:"
      ),
      tags$ul(
        style = "font-size: 12px; color: #556; padding-left: 18px; margin-top: 0;",
        tags$li(tags$code("docs/guidance/guidelines_v5_2_0_rc6_wizard.docx")),
        tags$li(tags$code("docs/instructions/EU_SAE_Download_Instructions_5_2_0_rc_6_wizard_3_pointwise.pdf")),
        tags$li(tags$code("docs/instructions/EU_SAE_User_Guide_5_2_0_rc_6_wizard_3_pointwise.pptx")),
        tags$li(tags$code("outputs/final_report.html"), " after a completed run")
      ),
      tags$hr(),

      # ---- Data inputs ----
      h4("Data inputs"),
      tags$div(
        style = "font-size: 12px; color: #556; margin: -4px 0 10px 0;",
        "Use Browse to select the required input files from any folder on your computer: household survey, auxiliary covariates, and shapefiles/geometries. Survey and auxiliary files can be R, CSV/TSV/DAT, Stata, SPSS, SAS, Python Parquet/Feather, or Excel files. For ESRI shapefiles, upload one .zip containing the shapefile components, especially .shp, .shx, and .dbf; the .dbf usually contains the domain ID needed for maps. Geometry files can also be .rds, GeoPackage, or GeoJSON. If a saved setup is loaded, its active files are shown below each Browse button; browse again only when changing a file."
      ),
      fileInput("survey_file",
        tip_label("Browse Survey data file", "Choose the household survey file from any folder on your computer. Accepted formats: .rds, .RData/.rda, .csv, .tsv, .txt, .dat, .dta, .sav, .zsav, .por, .sas7bdat, .xpt, .parquet, .feather, .xlsx, .xls."),
        accept = c(".rds", ".RData", ".rda", ".csv", ".tsv", ".txt", ".dat",
                   ".dta", ".sav", ".zsav", ".por", ".sas7bdat", ".xpt",
                   ".parquet", ".feather", ".xlsx", ".xls")),
      uiOutput("survey_active_file"),
      fileInput("rhs_file",
        tip_label("Browse Auxiliary data file", "Choose the auxiliary covariates file from any folder on your computer. Accepted formats: .rds, .RData/.rda, .csv, .tsv, .txt, .dat, .dta, .sav, .zsav, .por, .sas7bdat, .xpt, .parquet, .feather, .xlsx, .xls."),
        accept = c(".rds", ".RData", ".rda", ".csv", ".tsv", ".txt", ".dat",
                   ".dta", ".sav", ".zsav", ".por", ".sas7bdat", ".xpt",
                   ".parquet", ".feather", ".xlsx", ".xls")),
      uiOutput("rhs_active_file"),
      fileInput("shp_file",
        tip_label("Browse Shapefile/geometries file", "Choose the geometry file from any folder on your computer. For ESRI shapefiles, upload a single .zip containing all shapefile components, especially .shp, .shx, and .dbf. A .shp file alone is usually not enough because the .dbf stores the domain ID used to join estimates to map polygons. Other accepted formats: .rds, .RData/.rda, .gpkg, .geojson, .json, .kml, .gml."),
        accept = c(".rds", ".RData", ".rda", ".zip", ".gpkg", ".geojson", ".json", ".kml", ".gml")),
      uiOutput("shp_active_file"),
      checkboxInput("do_benchmark",
        tip_label("Apply benchmarking", "If checked, UFH and MFH estimates are benchmarked. If unchecked, uploaded benchmark files and benchmark-level mappings are ignored for the next run."),
        value = FALSE),
      conditionalPanel(
        condition = "input.do_benchmark",
        radioButtons(
          "benchmark_level",
          tip_label(
            "Benchmarking level",
            "National benchmarking makes the population-weighted average of the domain estimates equal the direct national estimate. Grouped benchmarking applies the same constraint separately within the selected higher-level areas."
          ),
          choices = c(
            "National" = "national",
            "Grouped by a survey variable" = "custom"
          ),
          selected = "national"
        ),
        fileInput("regional_benchmark_file",
          tip_label("Benchmark Target Database (optional)",
                    "Optional RDS/CSV/XLSX file with benchmark targets by year. Leave blank to estimate targets from the survey using population_weight = weight * household size.")),
        tags$div(
          class = "benchmark-format-note",
          tags$div(class = "note-title", "External benchmark file format"),
          tags$p(
            "Upload this file only when you have external benchmark targets. ",
            "Leave it blank to estimate targets from the survey."
          ),
          tags$ul(
            tags$li("National benchmarking: include a year column and a target column."),
            tags$li("Grouped benchmarking: also include the benchmark-level column selected below."),
            tags$li("Target column names can be benchmark, target, value, poverty_rate, rate, or mean."),
            tags$li("Accepted file types: .rds, .RData/.rda, .csv, .txt, .xlsx, or .xls.")
          )
        ),
        tags$div(
          class = "benchmark-clear-row",
          actionButton("clear_benchmark_file",
            "Clear selected file",
            class = "btn-default btn-sm")
        ),
        uiOutput("benchmark_active_file"),
        conditionalPanel(
          condition = "input.benchmark_level == 'custom'",
          mapping_selectize("var_benchmark_level",
            tip_label("Grouped benchmark variable",
                      "Survey column defining the higher-level benchmark groups, such as region, NUTS2, or voivodeship."),
            "")
        )
      ),
      fileInput("population_file",
        tip_label("Domain population sizes (optional)",
                  "Optional RDS/CSV/XLSX file with domain population sizes. Supports long domain-year-population format or wide domain-by-year format; leave blank to estimate domain populations from the survey as sum(weight * household size).")),
      uiOutput("population_active_file"),
      tags$hr(),

      # ---- Variable mapping ----
      h4("Variable mapping"),
      tags$div(
        style = "font-size: 12px; color: #556; margin: -4px 0 10px 0;",
        "Search the dropdown list of columns loaded from the selected dataset, or type a column name directly. A red field means the typed name is not currently found in the relevant dataset."
      ),
      uiOutput("mapping_validation_summary"),
      mapping_selectize("var_year",
        tip_label("year", "Column name in the survey data that identifies the year. Search the survey columns or type the column name."),
        "year"),
      mapping_selectize("var_domain",
        tip_label("domain", "Column name in the survey data that identifies the small area domain (e.g. NUTS-3 province). Search the survey columns or type the column name."),
        "prov"),
      mapping_selectize("var_psu",
        tip_label("psu", "Column name for the Primary Sampling Unit. Used to compute design-based sampling variances. Search the survey columns or type the column name."),
        "ea_id"),
      mapping_selectize("var_weight",
        tip_label("weight", "Column name for the survey sampling weight. Search the survey columns or type the column name."),
        "weight"),
      mapping_selectize("var_strata",
        tip_label("strata", "Optional strata ID column for survey-design variance estimation. Search the survey columns or type the column name. Leave blank if the survey design has no strata or strata are unavailable."),
        ""),
      mapping_selectize("var_hh_size",
        tip_label("household size", "Column name for household size. Direct poverty-rate estimates use population_weight = weight * household size; when no population file is uploaded, benchmarking also estimates domain populations as sum(weight * household size) by domain and year. Search the survey columns or type the column name."),
        "hhsize"),
      mapping_selectize("var_welfare",
        tip_label("welfare", "Column name for the welfare variable (e.g. income or consumption) used to determine poverty status. Search the survey columns or type the column name."),
        "income"),

      # ---- Indicator type ----
      # Top-level choice. Switching to "mean_welfare" hides the
      # poverty-line / FGT inputs (they're irrelevant) and exposes
      # an optional log-transform.
      selectInput("indicator_type",
        tip_label("Indicator",
                  "What is being modelled. 'Poverty (FGT)' uses welfare + a poverty line to compute headcount, gap, or severity. 'Mean welfare' uses the population-weighted mean of the welfare variable directly."),
        choices  = c("Poverty (FGT)" = "poverty",
                     "Mean welfare"  = "mean_welfare"),
        selected = "poverty"),

      # ---- Poverty line (only shown for poverty indicator) ----
      conditionalPanel(
        condition = "input.indicator_type == 'poverty'",
        radioButtons("povline_type",
          tip_label("Poverty line source",
                    "Choose whether the poverty line comes from a column in the survey data or from numeric values entered separately for each analysis year."),
          choices  = c("Column in data" = "column", "Numeric value" = "numeric"),
          selected = "column", inline = TRUE),
        conditionalPanel(
          condition = "input.povline_type == 'column' && input.indicator_type == 'poverty'",
          mapping_selectize("var_povline",
            tip_label("povline", "Column name for the poverty line. Search the survey columns or type the column name."),
            "povline")
        ),
        conditionalPanel(
          condition = "input.povline_type == 'numeric' && input.indicator_type == 'poverty'",
          uiOutput("povline_numeric_by_year_ui")
        ),

        # ---- FGT indicator ----
        selectInput("fgt_alpha",
          tip_label("Poverty measure",
                    "FGT(0) = headcount ratio (share below the line). FGT(1) = poverty gap (average depth of shortfall). FGT(2) = poverty severity (squared gap, emphasises the poorest)."),
          choices = c("FGT(0) \u2013 Headcount ratio" = "0",
                      "FGT(1) \u2013 Poverty gap"      = "1",
                      "FGT(2) \u2013 Poverty severity"  = "2"),
          selected = "0")
      ),

      # ---- Mean welfare options (only shown for mean indicator) ----
      # Note: the log/no choice now lives in the UFH "Transformation"
      # dropdown below (which becomes context-aware when the indicator
      # is mean welfare). The standalone "Log-transform welfare"
      # checkbox has been removed in favor of that single source of
      # truth -- it was replicating the same setting in two places.
      conditionalPanel(
        condition = "input.indicator_type == 'mean_welfare'",
        textInput("currency_symbol",
          tip_label("Currency symbol",
                    "Short label appended to axis titles and table headers for mean welfare estimates."),
          value = "EUR")
      ),

      mapping_selectize("rhs_domain",
        tip_label("Auxiliary covariates domain field", "Column name in the auxiliary covariates file that identifies the domain. Search the auxiliary columns or type the column name. Used to join covariates to survey data."),
        "prov"),
      mapping_selectize("shp_domain",
        tip_label("Shapefile domain field", "Column name in the shapefiles/geometries file that identifies the domain. Search the geometry columns or type the column name. Used to join estimates to map polygons."),
        "prov"),
      tags$hr(),

      # ---- UFH options ----
      h4("UFH options"),
      # Transformation dropdown: choices are context-aware.
      #   - poverty indicator: arcsin / no
      #   - mean_welfare      : log    / no
      # The actual choice list is updated by an observer on input$indicator_type
      # (see server logic below). We seed it with the poverty defaults here.
      selectInput("ufh_transformation",
        tip_label("Transformation",
                  paste(
                    "Transformation of the direct estimates before model fitting.",
                    "For poverty rates: 'arcsin' constrains estimates to [0,1] and stabilizes variances.",
                    "For mean welfare: 'log' addresses the right-skewness of welfare; the back-transform to currency units is bias-corrected via Duan's smearing (see Bias Correction below).",
                    "'no' fits on the original scale.")),
        choices = c("arcsin", "no"), selected = "arcsin"),
      # Bias correction is meaningful under arcsin AND log. For arcsin, it
      # corrects the non-linearity of arcsine; for log, it corrects the
      # Jensen-inequality bias of exp(eta_hat). The available options
      # depend on the chosen transformation (set dynamically server-side).
      conditionalPanel(
        condition = "input.ufh_transformation == 'arcsin' || input.ufh_transformation == 'log'",
        selectInput("ufh_backtrans",
          tip_label("Bias Correction",
                    paste(
                      "How the model estimates are bias-corrected when back-transforming to the original scale.",
                      "For arcsin: 'bc' integrates sin^2(.) against the predictive density (correct under Gaussianity); 'none' returns the naive sin^2(eta_hat).",
                      "For log: 'bc_sm' applies Duan's smearing estimator -- multiplies exp(eta_hat) by the empirical mean of exp(residuals), which is non-parametric and robust to non-Gaussian residuals; 'none' returns the naive exp(eta_hat) (downward-biased for the mean).")),
          choices = c("bc", "none"), selected = "bc")
      ),
      # Variance smoothing menu is only meaningful when no transformation
      # is used (arcsin/log already stabilize variances).
      conditionalPanel(
        condition = "input.ufh_transformation == 'no'",
        selectInput("ufh_var_choice",
          tip_label("Variance option (UFH)", "Sampling variance input for UFH when no transformation is used. 'sm_out' replaces a direct variance with its smoothed (GVF-based) variance only when the direct variance is missing/non-finite or below 0.001; 'sm_all' replaces all variances; 'direct' retains raw survey variances except safety backfills. When arcsin or log is selected, this choice is ignored because the transformation already stabilizes variances."),
          choices = c("sm_out", "sm_all", "direct"), selected = "sm_out")
      ),
      selectInput("ufh_ic_criterion",
        tip_label("Model selection criterion", "Information criterion used for stepwise covariate selection. BIC penalizes complexity more heavily and tends to select simpler models. AIC favours predictive accuracy."),
        choices = c("AIC", "BIC"), selected = "BIC"),
      checkboxInput("ufh_lasso_enabled",
        tip_label("Use LASSO screening",
                  "Optionally screens numeric covariates with LASSO before the AIC/BIC stepwise stage. If custom Year 1 or Year 2 covariates are entered below, they define the LASSO candidate pool for that year. The final model is still chosen by stepwise selection."),
        value = FALSE),
      conditionalPanel(
        condition = "input.ufh_lasso_enabled",
        selectInput("ufh_lasso_lambda",
          tip_label("LASSO lambda",
                    "lambda.1se is more conservative; lambda.min may retain more predictors."),
          choices = c("lambda.1se", "lambda.min"), selected = "lambda.1se")
      ),
      textInput("ufh_candidates_y1",
        tip_label("UFH covariates for Year 1 (comma-separated, optional)",
                  "If LASSO is on, these variables define the candidate pool for the first analysis year. If LASSO is off, they define the fixed UFH specification. Leave blank to use the eligible auxiliary variables."),
        value = ""),
      textInput("ufh_candidates_y2",
        tip_label("UFH covariates for Year 2 (comma-separated, optional)",
                  "If LASSO is on, these variables define the candidate pool for the second analysis year. If LASSO is off, they define the fixed UFH specification. Leave blank to use the eligible auxiliary variables."),
        value = ""),
      tags$hr(),

      # ---- MFH options ----
      h4("MFH options"),
      # Transformation choice for MFH. Independent from UFH so the two
      # models can run on different scales if needed (rare, but
      # supported -- e.g. UFH on identity, MFH on log).
      #   - poverty: hidden (MFH never used arcsin; identity scale only)
      #   - mean_welfare: log / no, default log
      # The bias-correction subchoice mirrors the UFH menu but only
      # offers bc_sm (Duan smearing) for log; arcsin is not an MFH
      # option and so 'bc' (integration-based) is not exposed here.
      conditionalPanel(
        condition = "input.indicator_type == 'mean_welfare'",
        selectInput("mfh_transformation",
          tip_label("Transformation (MFH)",
                    paste(
                      "Transformation applied to the MFH model. Independent of the UFH choice above.",
                      "'log' fits MFH on log(welfare) per year, then back-transforms each (domain, year) cell with a per-domain-year smearing factor anchored to the population-weighted arithmetic mean of welfare. MCPE is back-transformed to currency units via a delta-method approximation, so cross-year change analysis stays on the EUR scale.",
                      "'no' fits on the identity scale.")),
          choices = c("log", "no"), selected = "log"),
        conditionalPanel(
          condition = "input.mfh_transformation == 'log'",
          selectInput("mfh_backtrans",
            tip_label("Bias Correction (MFH)",
                      paste(
                        "Bias correction for the log -> currency back-transform.",
                        "'bc_sm' applies Duan's smearing estimator (multiplies exp(eta_hat) by the empirical mean of exp(residuals)); robust to non-Gaussian residuals.",
                        "'none' returns the naive exp(eta_hat), which is downward-biased for the mean.")),
            choices = c("bc_sm", "none"), selected = "bc_sm")
        )
      ),
      selectInput("mfh_ic_criterion",
        tip_label("Model selection criterion", "Information criterion used for stepwise covariate selection. AIC favours predictive accuracy; BIC penalizes complexity more and selects sparser models."),
        choices = c("AIC", "BIC"), selected = "AIC"),
      checkboxInput("mfh_lasso_enabled",
        tip_label("Use LASSO screening",
                  "Optionally screens numeric covariates with LASSO before the AIC/BIC stepwise stage. If custom Year 1 or Year 2 covariates are entered below, they define the LASSO candidate pool for that year. The final model is still chosen by stepwise selection."),
        value = FALSE),
      conditionalPanel(
        condition = "input.mfh_lasso_enabled",
        selectInput("mfh_lasso_lambda",
          tip_label("LASSO lambda",
                    "lambda.1se is more conservative; lambda.min may retain more predictors."),
          choices = c("lambda.1se", "lambda.min"), selected = "lambda.1se")
      ),
      selectInput("mfh_var_choice",
        tip_label("Variance option (MFH)", "Sampling variance input: 'sm_out' replaces a direct variance with its smoothed (GVF-based) variance only when the direct variance is missing/non-finite or below 0.001; 'sm_all' replaces all variances; 'direct' retains raw survey variances except safety backfills. The covariance options below update automatically to match."),
        choices = c("sm_out", "sm_all", "direct"), selected = "sm_out"),
      selectInput("mfh_cov_choice",
        tip_label("Covariance option", "How the covariance of sampling errors over time is estimated. Available options depend on the variance choice above. 'rho_sm_out' multiplies the national average autocorrelation with outlier-smoothed variances (available with sm_out). 'rho_sm_all' multiplies it with fully smoothed variances (available with sm_all). 'rho_dir' multiplies it with direct variances. 'direct' uses the direct variance-covariance matrix. 'zero' assumes no cross-year sampling correlation."),
        choices = c("rho_sm_out", "rho_dir", "direct", "zero"), selected = "rho_sm_out"),
      selectInput("mfh_diag_model",
        tip_label("Selected MFH model", "MFH2 is the package default. If MFH3 is selected, the package follows Molina and Romero: fit MFH3, use MFH2 if MFH3 does not converge or errors, and otherwise use the MFH3 reference-variance tests to choose MFH3 or MFH2. MFH1 remains an additional sensitivity model. UFH is produced separately by scripts/01_ufh.R."),
        choices = c(
          "MFH1 (additional sensitivity model)" = "MFH1",
          "MFH2 (default)" = "MFH2",
          "MFH3 candidate (Molina-Romero test and fallback)" = "MFH3"
        ), selected = "MFH2"),
      checkboxInput("fit_mfh3",
        tip_label("Also fit MFH3 for sensitivity", "When MFH1 or MFH2 is selected, this optional checkbox fits MFH3 for comparison diagnostics without changing the selected model. Selecting MFH3 above always fits it and applies the Molina-Romero test and MFH2 fallback."),
        value = FALSE),
      conditionalPanel(
        condition = "input.mfh_diag_model == 'MFH3' || input.fit_mfh3",
        selectInput("mfh_refvar_adjustment",
          tip_label("MFH3 reference-variance test adjustment", "This setting applies only to the pairwise time-variance contrasts returned when MFH3 is fitted. It does not adjust poverty-change tests across geographic domains. Bonferroni is the conservative model-selection default; BH is a less-conservative sensitivity option. With exactly two years there is only one contrast, so raw, Bonferroni, and BH p-values are identical."),
          choices = c(
            "Bonferroni (recommended)" = "bonferroni",
            "Benjamini-Hochberg (sensitivity)" = "bh"
          ), selected = "bonferroni")
      ),
      numericInput("mcpe_bootstrap_replicates",
        tip_label("MCPE bootstrap replicates", "Controls Monte Carlo precision for MFH change inference. 200 is the interactive default; use at least 500 for production after checking Monte Carlo stability."),
        value = 200, min = 50, step = 50),
      textInput("mfh_candidates_y1",
        tip_label("MFH covariates for Year 1 (comma-separated, optional)",
                  "If LASSO is on, these variables define the candidate pool for the first analysis year. If LASSO is off, they define the fixed MFH specification. Leave blank to use the eligible auxiliary variables."),
        value = ""),
      textInput("mfh_candidates_y2",
        tip_label("MFH covariates for Year 2 (comma-separated, optional)",
                  "If LASSO is on, these variables define the candidate pool for the second analysis year. If LASSO is off, they define the fixed MFH specification. Leave blank to use the eligible auxiliary variables."),
        value = ""),
      tags$hr(),

      # ---- Data assessment ----
      h4("Data assessment"),
      checkboxInput("psu_consistent",
        tip_label("PSU codes are consistent over time", "Check this if the same PSU identifiers refer to the same sampling units across years. Affects how cross-year covariance of sampling errors is estimated."),
        value = FALSE),
      tags$hr(),

      # ---- LLM settings ----
      h4("AI Assistant (optional)"),
      uiOutput("llm_consent_ui"),
      checkboxInput("llm_enabled",
        tip_label("Enable AI Assistant", "Enable an AI-powered assistant that interprets diagnostics, evaluates normality assumptions, and enriches the analysis brief. Supports Anthropic (Claude) and OpenAI (ChatGPT) API keys."),
        value = FALSE),
      conditionalPanel(
        condition = "input.llm_enabled",
        checkboxInput("llm_external_consent",
          "I understand that aggregate estimates, uncertainty measures, diagnostics, and analysis context will be sent to the selected external AI provider. Geographic identifiers are removed from comparison prompts; raw microdata are not sent.",
          value = FALSE),
        passwordInput("api_key",
          tip_label("API Key", "Enter your Anthropic (sk-ant-...) or OpenAI (sk-...) API key. Only aggregate statistics are sent to the API \u2014 never raw microdata."),
          value = ""),
        selectInput("language",
          tip_label("Language", "Switch language of AI-assisted interpretation. All 24 official EU languages are supported, plus Arabic."),
          choices = supported_languages(),
          selected = "en")
      ),
      tags$hr(),

      # Two-stage launch: first review readiness, then run the analysis.
      actionButton("check_btn", "1. Check Data Readiness", class = "btn-default",
                   style = "margin-right: 8px;"),
      actionButton("run_btn",   "2. Run Analysis",         class = "btn-primary")
    ),

    mainPanel(
      tabsetPanel(
        id = "main_tabs",

        tabPanel("Preflight",
          h4("Startup Preflight"),
          p("Review these checks before running the pipeline. This panel updates automatically as you change inputs."),
          uiOutput("preflight_ui"),
          h4("Recommended actions"),
          verbatimTextOutput("preflight_actions")
        ),

        # ---- Tab: Data Readiness ----
        tabPanel("Data Readiness",
          h4("Data Readiness Assessment"),
          p("Click '1. Check Data Readiness' in the sidebar to generate diagnostics. ",
            "Review the results here before starting the UFH / MFH analysis."),
          verbatimTextOutput("readiness_messages"),
          h4("National Poverty Headcount Rates"),
          tableOutput("readiness_national"),
          h4("Domain Consistency"),
          tableOutput("readiness_domains"),
          h4("Missing Poverty Rates"),
          tableOutput("readiness_missing"),
          h4("Auxiliary Covariate Summary"),
          p("Means, standard errors, observation counts, and correlations with the domain-level target indicator (poverty rate or mean welfare, matching the Indicator selector)."),
          tableOutput("readiness_aux")
        ),

        # ---- Tab: Pipeline Status ----
        tabPanel("Status",
          h4("Current step"),
          textOutput("status"),
          h4("Run folder"),
          verbatimTextOutput("run_location"),
          h4("Run log"),
          verbatimTextOutput("logs"),
          h4("Expected outputs"),
          tableOutput("outputs")
        )
      )
    )
  ) # end sidebarLayout
  ) # end main_app div
)

server <- function(input, output, session) {
  # ---- Safe accessor for language (defaults to "en" if not yet available) ----
  get_language <- function() input$language %||% "en"

  # ---- Page navigation helpers ----
  fade_transition <- function(hide_id, show_id) {
    session$sendCustomMessage("fadeTransition", list(hide = hide_id, show = show_id))
  }

  # Cover -> Main App
  observeEvent(input$enter_app_btn, { fade_transition("cover_page", "main_app") })

  # ---- Link variance and covariance options for MFH ----
  # When the variance option changes, update the available covariance choices
  # so that the smoothing level is consistent.
  observeEvent(input$mfh_var_choice, {
    vc <- input$mfh_var_choice
    if (vc == "sm_out") {
      cov_choices <- c("rho_sm_out", "direct", "zero")
      default_sel <- "rho_sm_out"
    } else if (vc == "sm_all") {
      cov_choices <- c("rho_sm_all", "direct", "zero")
      default_sel <- "rho_sm_all"
    } else {
      # direct variance: rho_dir uses direct variances, which is consistent
      cov_choices <- c("rho_dir", "direct", "zero")
      default_sel <- "rho_dir"
    }
    updateSelectInput(session, "mfh_cov_choice",
                      choices = cov_choices, selected = default_sel)
  })

  status    <- reactiveVal("Idle")
  logs      <- reactiveVal("")
  run_location <- reactiveVal("No run started yet.")
  output_rows <- reactiveVal(data.frame(
    "Current file" = character(),
    Description = character(),
    Exists = character(),
    "Saved copy" = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  ))

  # Reactive stores for validation, diagnostics, and brief
  validation_result <- reactiveVal(NULL)
  diagnostics_data  <- reactiveVal(NULL)
  brief_result      <- reactiveVal(NULL)
  llm_interp        <- reactiveVal(NULL)
  llm_brief         <- reactiveVal(NULL)
  normality_eval    <- reactiveVal(NULL)
  readiness_result  <- reactiveVal(NULL)
  # TRUE once the user has clicked "Check Data Readiness" in the current session.
  # The UFH / MFH / Comparison pipeline can only start after this is TRUE, so the
  # user is forced to review the preflight and readiness results before running
  # the analysis.
  readiness_checked <- reactiveVal(FALSE)
  restoring_setup   <- reactiveVal(FALSE)
  active_setup_files <- reactiveVal(dashboard_setup_defaults()$data_files)
  benchmark_upload_cleared <- reactiveVal(FALSE)
  setup_status_text  <- reactiveVal("Using default dashboard settings.")

  append_log <- function(msg) {
    stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    logs(paste0(logs(), if (nzchar(logs())) "\n" else "", "[", stamp, "] ", msg))
  }

  # ---- LLM consent text (multilingual) ----
  output$llm_consent_ui <- renderUI({
    t <- translator(get_language())
    helpText(t$get("llm_consent_text"))
  })

  # ---- Create LLM assistant ----
  get_llm <- reactive({
    if (isTRUE(input$llm_enabled) && isTRUE(input$llm_external_consent) &&
        nchar(input$api_key %||% "") > 0) {
      llm_assistant(api_key = input$api_key, provider = detect_llm_provider(input$api_key))
    } else {
      llm_assistant(enabled = FALSE)
    }
  })

  # ---- Benchmarking level helpers ----
  get_benchmark_level_var <- reactive({
    if (!isTRUE(input$do_benchmark)) return("")
    if (identical(input$benchmark_level %||% "national", "national")) return("")
    trimws(input$var_benchmark_level %||% "")
  })

  get_benchmark_level <- reactive({
    level <- input$benchmark_level %||% ""
    if (level %in% c("national", "custom")) return(level)
    if (nzchar(trimws(input$var_benchmark_level %||% ""))) "custom" else "national"
  })

  # ---- Build var_map from inputs ----
  get_var_map <- reactive({
    benchmark_level_var <- get_benchmark_level_var()
    vm <- list(
      year    = input$var_year,
      domain  = input$var_domain,
      psu     = input$var_psu,
      weight  = input$var_weight,
      strata  = input$var_strata %||% "",
      hh_size = input$var_hh_size,
      benchmark_level = benchmark_level_var,
      region  = benchmark_level_var,
      welfare = input$var_welfare,
      poor    = "poor"
    )
    # Poverty line: column name or NULL (when numeric)
    if (identical(input$povline_type, "column")) {
      vm$povline <- input$var_povline
    }
    vm
  })

  get_povline_numeric_by_year <- function(years_vec = parse_years(input$years)) {
    years_vec <- as.integer(years_vec)
    vals <- lapply(years_vec, function(yr) {
      id <- paste0("povline_numeric_", yr)
      val <- input[[id]]
      if (is.null(val)) {
        saved <- dashboard_setup_defaults()$inputs$povline_numeric
        val <- input$povline_numeric %||% saved %||% 5000
      }
      as.numeric(val)
    })
    names(vals) <- as.character(years_vec)
    vals
  }

  output$povline_numeric_by_year_ui <- renderUI({
    years_vec <- parse_years(input$years)
    if (length(years_vec) == 0L) {
      return(helpText("Enter analysis years before setting numeric poverty lines."))
    }
    tagList(lapply(years_vec, function(yr) {
      id <- paste0("povline_numeric_", yr)
      numericInput(
        id,
        tip_label(
          paste("Poverty line", yr),
          "Numeric poverty line used for all households in this analysis year."
        ),
        value = isolate(input[[id]] %||% input$povline_numeric %||% 5000),
        min = 0
      )
    }))
  })

  add_benchmark_metadata <- function(bench_list, source, level_label,
                                     level_variable, enabled) {
    if (is.null(bench_list)) return(bench_list)
    lapply(bench_list, function(b) {
      if (is.null(b)) return(b)
      b$benchmark_enabled <- isTRUE(enabled)
      b$benchmark_source <- source
      b$benchmark_level <- level_label
      b$benchmark_level_variable <- level_variable %||% ""
      b
    })
  }

  benchmark_metadata_lines <- function(b) {
    if (is.null(b)) return(character())
    lines <- character()
    if (!is.null(b$benchmark_enabled)) {
      lines <- c(lines, sprintf(
        "- **Benchmarking:** %s",
        if (isTRUE(b$benchmark_enabled)) "enabled" else "off"
      ))
    }
    if (!is.null(b$benchmark_level) && nzchar(as.character(b$benchmark_level))) {
      lines <- c(lines, sprintf("- **Benchmark level:** %s", b$benchmark_level))
    }
    if (!is.null(b$benchmark_source) && nzchar(as.character(b$benchmark_source))) {
      lines <- c(lines, sprintf("- **Benchmark source:** %s", b$benchmark_source))
    }
    lines
  }

  safe_setup_copy_name <- function(key, file_name) {
    base <- basename(file_name %||% "")
    if (!nzchar(base)) {
      base <- paste0(key, ".rds")
    }
    base <- gsub("[^A-Za-z0-9._-]+", "_", base)
    paste(key, base, sep = "__")
  }

  persist_setup_upload <- function(key, file_input) {
    if (is.null(file_input) ||
        is.null(file_input$datapath) ||
        !nzchar(file_input$datapath %||% "") ||
        !file.exists(file_input$datapath)) {
      return(NULL)
    }
    dir.create(dashboard_setup_file_dir(), recursive = TRUE, showWarnings = FALSE)
    target <- file.path(
      dashboard_setup_file_dir(),
      safe_setup_copy_name(key, file_input$name %||% key)
    )
    ok <- file.copy(file_input$datapath, target, overwrite = TRUE)
    if (!isTRUE(ok)) {
      stop(sprintf("Could not save setup copy for %s.", key), call. = FALSE)
    }
    normalizePath(target, winslash = "/", mustWork = FALSE)
  }

  selected_setup_file_ref <- function(file_input, key) {
    copied <- persist_setup_upload(key, file_input)
    if (!is.null(copied) && nzchar(copied)) {
      return(copied)
    }
    setup_file_ref_for(key)
  }

  selected_setup_file_name <- function(file_input, key) {
    upload_name <- file_input$name %||% ""
    if (nzchar(upload_name)) {
      return(basename(upload_name))
    }
    file_ref <- setup_file_ref_for(key)
    if (nzchar(file_ref)) basename(file_ref) else ""
  }

  setup_file_ref_for <- function(key) {
    active_setup_files()[[key]] %||% ""
  }

  setup_file_name_for <- function(key) {
    file_ref <- setup_file_ref_for(key)
    if (nzchar(file_ref)) basename(file_ref) else ""
  }

  saved_setup_path_for <- function(key) {
    file_ref <- setup_file_ref_for(key)
    path <- setup_file_path(file_ref)
    if (!is.null(path) && file.exists(path)) path else NULL
  }

  collect_dashboard_setup <- function() {
    defaults <- dashboard_setup_defaults()
    inputs <- defaults$inputs

    input_ids <- names(inputs)
    for (id in input_ids) {
      val <- input[[id]]
      if (!is.null(val)) {
        inputs[[id]] <- val
      }
    }
    inputs$povline_numeric_by_year <- get_povline_numeric_by_year(parse_years(inputs$years))
    if (length(inputs$povline_numeric_by_year) > 0L) {
      inputs$povline_numeric <- as.numeric(inputs$povline_numeric_by_year[[1]])
    }

    # Never persist API keys. Remember only whether the AI section was enabled
    # and the language selection; users re-enter credentials per session.
    inputs$api_key <- NULL
    inputs$llm_external_consent <- FALSE

    list(
      version = 1L,
      saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      data_files = list(
        survey_file = selected_setup_file_ref(input$survey_file, "survey_file"),
        rhs_file = selected_setup_file_ref(input$rhs_file, "rhs_file"),
        shp_file = selected_setup_file_ref(input$shp_file, "shp_file"),
        regional_benchmark_file = if (isTRUE(benchmark_upload_cleared())) {
          ""
        } else {
          selected_setup_file_ref(input$regional_benchmark_file, "regional_benchmark_file")
        },
        population_file = selected_setup_file_ref(input$population_file, "population_file")
      ),
      inputs = inputs
    )
  }

  apply_dashboard_setup <- function(setup, label = "saved setup") {
    defaults <- dashboard_setup_defaults()
    setup <- setup %||% defaults
    setup$inputs <- modifyList(defaults$inputs, setup$inputs %||% list())
    setup$data_files <- modifyList(defaults$data_files, setup$data_files %||% list())

    restoring_setup(TRUE)
    on.exit({
      session$onFlushed(function() restoring_setup(FALSE), once = TRUE)
    }, add = TRUE)

    active_setup_files(setup$data_files)
    benchmark_upload_cleared(FALSE)
    setup_status_text(sprintf(
      "Loaded %s%s.",
      label,
      if (!is.null(setup$saved_at) && nzchar(setup$saved_at %||% "")) {
        paste0(" saved at ", setup$saved_at)
      } else {
        ""
      }
    ))

    x <- setup$inputs
    f <- setup$data_files
    update_mapping_select <- function(id, value) {
      value <- value %||% ""
      cols <- if (exists("columns_for_mapping_id", inherits = FALSE)) {
        columns_for_mapping_id(id)
      } else {
        character()
      }
      updateSelectizeInput(
        session,
        id,
        choices = unique(c("", cols, value)),
        selected = value,
        options = list(
          create = TRUE,
          createOnBlur = TRUE,
          persist = FALSE,
          maxOptions = max(1000L, length(cols) + 5L)
        ),
        server = TRUE
      )
    }

    updateTextInput(session, "years", value = x$years %||% defaults$inputs$years)
    updateTextInput(session, "country_name", value = x$country_name %||% "")
    updateNumericInput(session, "analysis_seed",
                       value = as.integer(x$analysis_seed %||% 123L))
    updateTextInput(session, "run_label", value = x$run_label %||% "")
    updateCheckboxGroupInput(session, "steps",
                             selected = x$steps %||% defaults$inputs$steps)
    updateCheckboxInput(session, "do_benchmark",
                        value = isTRUE(x$do_benchmark))
    saved_benchmark_level <- x$benchmark_level %||%
      if (nzchar(x$var_benchmark_level %||% "")) "custom" else "national"
    updateRadioButtons(session, "benchmark_level",
                       selected = saved_benchmark_level)
    update_mapping_select("var_benchmark_level", x$var_benchmark_level %||% "")
    update_mapping_select("var_year", x$var_year %||% "year")
    update_mapping_select("var_domain", x$var_domain %||% "prov")
    update_mapping_select("var_psu", x$var_psu %||% "ea_id")
    update_mapping_select("var_weight", x$var_weight %||% "weight")
    update_mapping_select("var_strata", x$var_strata %||% "")
    update_mapping_select("var_hh_size", x$var_hh_size %||% "hhsize")
    update_mapping_select("var_welfare", x$var_welfare %||% "income")
    updateSelectInput(session, "indicator_type",
                      selected = x$indicator_type %||% "poverty")
    updateRadioButtons(session, "povline_type",
                       selected = x$povline_type %||% "column")
    update_mapping_select("var_povline", x$var_povline %||% "povline")
    .povline_by_year <- x$povline_numeric_by_year %||% list()
    updateSelectInput(session, "fgt_alpha",
                      selected = as.character(x$fgt_alpha %||% "0"))
    updateTextInput(session, "currency_symbol",
                    value = x$currency_symbol %||% "EUR")
    update_mapping_select("rhs_domain", x$rhs_domain %||% "prov")
    update_mapping_select("shp_domain", x$shp_domain %||% "prov")
    updateSelectInput(session, "ufh_ic_criterion",
                      selected = x$ufh_ic_criterion %||% "BIC")
    updateCheckboxInput(session, "ufh_lasso_enabled",
                        value = isTRUE(x$ufh_lasso_enabled))
    updateSelectInput(session, "ufh_lasso_lambda",
                      selected = x$ufh_lasso_lambda %||% "lambda.1se")
    updateTextInput(session, "ufh_candidates_y1",
                    value = x$ufh_candidates_y1 %||% "")
    updateTextInput(session, "ufh_candidates_y2",
                    value = x$ufh_candidates_y2 %||% "")
    updateSelectInput(session, "mfh_ic_criterion",
                      selected = x$mfh_ic_criterion %||% "AIC")
    updateCheckboxInput(session, "mfh_lasso_enabled",
                        value = isTRUE(x$mfh_lasso_enabled))
    updateSelectInput(session, "mfh_lasso_lambda",
                      selected = x$mfh_lasso_lambda %||% "lambda.1se")
    updateSelectInput(session, "mfh_var_choice",
                      selected = x$mfh_var_choice %||% "sm_out")
    updateSelectInput(session, "mfh_diag_model",
                      selected = x$mfh_diag_model %||% "MFH2")
    updateCheckboxInput(session, "fit_mfh3",
                        value = isTRUE(x$fit_mfh3 %||% FALSE))
    updateSelectInput(session, "mfh_refvar_adjustment",
                      selected = x$mfh_refvar_adjustment %||% "bonferroni")
    updateNumericInput(session, "mcpe_bootstrap_replicates",
                       value = as.integer(x$mcpe_bootstrap_replicates %||% 200L))
    updateTextInput(session, "mfh_candidates_y1",
                    value = x$mfh_candidates_y1 %||% "")
    updateTextInput(session, "mfh_candidates_y2",
                    value = x$mfh_candidates_y2 %||% "")
    updateCheckboxInput(session, "psu_consistent",
                        value = isTRUE(x$psu_consistent))
    updateCheckboxInput(session, "llm_enabled",
                        value = isTRUE(x$llm_enabled))
    updateCheckboxInput(session, "llm_external_consent", value = FALSE)
    updateSelectInput(session, "language", selected = x$language %||% "en")

    session$onFlushed(function() {
      updateSelectInput(session, "ufh_transformation",
                        selected = x$ufh_transformation %||% defaults$inputs$ufh_transformation)
      updateSelectInput(session, "ufh_backtrans",
                        selected = x$ufh_backtrans %||% defaults$inputs$ufh_backtrans)
      updateSelectInput(session, "ufh_var_choice",
                        selected = x$ufh_var_choice %||% defaults$inputs$ufh_var_choice)
      updateSelectInput(session, "mfh_transformation",
                        selected = x$mfh_transformation %||% defaults$inputs$mfh_transformation)
      updateSelectInput(session, "mfh_backtrans",
                        selected = x$mfh_backtrans %||% defaults$inputs$mfh_backtrans)
      session$onFlushed(function() {
        updateSelectInput(session, "mfh_cov_choice",
                          selected = x$mfh_cov_choice %||% defaults$inputs$mfh_cov_choice)
      }, once = TRUE)
      session$onFlushed(function() {
        years_for_lines <- parse_years(x$years %||% defaults$inputs$years)
        vals <- sae_normalize_povline_map(
          .povline_by_year %||% (x$povline_numeric %||% 5000),
          years_for_lines
        )
        for (yr in years_for_lines) {
          id <- paste0("povline_numeric_", yr)
          val <- as.numeric(vals[as.character(yr)])
          if (!is.finite(val)) val <- as.numeric(x$povline_numeric %||% 5000)
          updateNumericInput(session, id, value = val)
        }
      }, once = TRUE)
    }, once = TRUE)

    readiness_checked(FALSE)
    session$onFlushed(function() {
      if (exists("refresh_all_mapping_choices", inherits = FALSE)) {
        refresh_all_mapping_choices()
      }
    }, once = TRUE)
    invisible(TRUE)
  }

  save_current_dashboard_setup <- function(reason = "manual") {
    setup <- collect_dashboard_setup()
    path <- write_dashboard_setup(setup)
    active_setup_files(setup$data_files)
    setup_status_text(sprintf(
      "Saved current dashboard setup at %s.",
      setup$saved_at %||% format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
    append_log(sprintf("Saved reusable dashboard setup: %s", path))
    invisible(path)
  }

  # ---- Auto-disable arcsin for FGT(1)/FGT(2) ----
  observeEvent(input$fgt_alpha, {
    if (as.integer(input$fgt_alpha %||% 0) > 0) {
      updateSelectInput(session, "ufh_transformation", selected = "no")
      showNotification(
        "Arcsin transformation disabled: it is only valid for the headcount ratio FGT(0).",
        type = "warning", duration = 6
      )
    }
  })

  # ---- Swap transformation choices when indicator_type changes ----
  # Poverty -> arcsin / no  (default arcsin for FGT(0); 'no' enforced for FGT(1/2))
  # Mean welfare -> log / no (default log)
  # Bias correction options also depend on the chosen transformation
  # (see the second observer below).
  observeEvent(input$indicator_type, {
    if (identical(input$indicator_type, "mean_welfare")) {
      updateSelectInput(session, "ufh_transformation",
                       choices  = c("log", "no"),
                       selected = "log")
      showNotification(
        "Mean welfare selected. Transformation choices: 'log' (default, with Duan smearing back-transform) or 'no' (identity scale).",
        type = "default", duration = 6
      )
    } else {
      sel <- if (as.integer(input$fgt_alpha %||% 0) > 0) "no" else "arcsin"
      updateSelectInput(session, "ufh_transformation",
                       choices  = c("arcsin", "no"),
                       selected = sel)
    }
  })

  # ---- Update UFH bias-correction choices based on transformation ----
  # arcsin -> bc / none      (bc = integration-based correction for arcsine)
  # log    -> bc_sm / none   (bc_sm = Duan smearing estimator)
  # no     -> hidden by conditionalPanel; nothing to update.
  observeEvent(input$ufh_transformation, {
    if (identical(input$ufh_transformation, "arcsin")) {
      updateSelectInput(session, "ufh_backtrans",
                       choices  = c("bc", "none"),
                       selected = "bc")
    } else if (identical(input$ufh_transformation, "log")) {
      updateSelectInput(session, "ufh_backtrans",
                       choices  = c("bc_sm", "none"),
                       selected = "bc_sm")
    }
  })

  # ---- MFH transformation: only meaningful for mean welfare ----
  # When indicator switches away from mean_welfare, force MFH back to
  # identity ("no") so the run config never accidentally carries a
  # log setting into a poverty-rate run. (The dropdown is hidden in
  # that case via conditionalPanel, but its last value would persist
  # in the input state otherwise.)
  observeEvent(input$indicator_type, {
    if (!identical(input$indicator_type, "mean_welfare")) {
      updateSelectInput(session, "mfh_transformation",
                       choices  = c("log", "no"),
                       selected = "no")
    } else {
      updateSelectInput(session, "mfh_transformation",
                       choices  = c("log", "no"),
                       selected = "log")
    }
  }, ignoreInit = TRUE)

  # ---- Outputs ----
  output$status  <- renderText(status())
  output$logs    <- renderText(logs())
  output$run_location <- renderText(run_location())
  output$outputs <- renderTable(output_rows(), striped = TRUE)
  output$saved_setup_status <- renderUI({
    file_line <- function(label, key) {
      ref <- setup_file_ref_for(key)
      if (!nzchar(ref)) {
        return(tags$div(tags$strong(label), ": not set"))
      }
      ok <- setup_file_exists(ref)
      tags$div(
        tags$strong(label), ": ", basename(ref),
        tags$span(
          style = sprintf("color:%s;", if (ok) "#2e7d32" else "#b26a00"),
          if (ok) " (found)" else " (not found; browse again)"
        )
      )
    }
    tags$div(
      style = "font-size: 12px; color: #556; background: #f7f7f7; border: 1px solid #ddd; border-radius: 6px; padding: 8px; margin-bottom: 8px;",
      tags$div(setup_status_text()),
      file_line("Survey", "survey_file"),
      file_line("Auxiliary", "rhs_file"),
      file_line("Shapefiles", "shp_file"),
      if (nzchar(setup_file_name_for("regional_benchmark_file"))) {
        file_line("Benchmark target", "regional_benchmark_file")
      },
      if (nzchar(setup_file_name_for("population_file"))) {
        file_line("Population", "population_file")
      }
    )
  })

  active_file_ui <- function(label, file_input, key, required = FALSE,
                             margin = "-12px 0 12px 0") {
    uploaded <- !is.null(file_input) &&
      !is.null(file_input$name) &&
      nzchar(file_input$name %||% "")
    path <- resolve_upload(file_input, fallback = saved_setup_path_for(key))
    saved_name <- setup_file_name_for(key)

    if (uploaded) {
      msg <- sprintf("Active %s: %s (selected for this session)", label, basename(file_input$name))
      color <- "#2e7d32"
    } else if (!is.null(path) && nzchar(path) && file.exists(path)) {
      msg <- sprintf("Active %s: %s (loaded from saved setup)", label, basename(path))
      color <- "#2e7d32"
    } else if (nzchar(saved_name)) {
      msg <- sprintf("Saved %s file not found: %s. Browse again to select it.", label, basename(saved_name))
      color <- "#b26a00"
    } else if (isTRUE(required)) {
      msg <- sprintf("Active %s: none selected", label)
      color <- "#b26a00"
    } else {
      msg <- sprintf("Active %s: none", label)
      color <- "#666"
    }

    tags$div(
      style = sprintf("font-size: 12px; color: %s; margin: %s;", color, margin),
      msg
    )
  }

  output$survey_active_file <- renderUI({
    active_file_ui("survey file", input$survey_file, "survey_file", required = TRUE)
  })
  output$rhs_active_file <- renderUI({
    active_file_ui("auxiliary file", input$rhs_file, "rhs_file", required = TRUE)
  })
  output$shp_active_file <- renderUI({
    active_file_ui("shapefiles file", input$shp_file, "shp_file", required = TRUE)
  })
  output$benchmark_active_file <- renderUI({
    if (isTRUE(benchmark_upload_cleared())) {
      return(tags$div(
        style = "font-size: 12px; color: #666; margin: 6px 0 12px 0;",
        "Active benchmark target: none"
      ))
    }
    active_file_ui(
      "benchmark target",
      input$regional_benchmark_file,
      "regional_benchmark_file",
      required = FALSE,
      margin = "6px 0 12px 0"
    )
  })
  output$population_active_file <- renderUI({
    active_file_ui("population file", input$population_file, "population_file", required = FALSE)
  })

  read_dataset_columns <- function(path, kind = c("table", "geometry")) {
    kind <- match.arg(kind)
    path <- path %||% ""
    if (!nzchar(path) || !file.exists(path)) {
      return(character())
    }
    cols <- tryCatch(
      sae_read_input_names(path, kind = kind),
      error = function(e) character()
    )
    cols <- unique(trimws(as.character(cols %||% character())))
    cols[nzchar(cols)]
  }

  survey_columns <- reactive({
    read_dataset_columns(resolve_data_input(
      input$survey_file,
      fallback = saved_setup_path_for("survey_file")
    ), kind = "table")
  })

  rhs_columns <- reactive({
    read_dataset_columns(resolve_data_input(
      input$rhs_file,
      fallback = saved_setup_path_for("rhs_file")
    ), kind = "table")
  })

  shp_columns <- reactive({
    read_dataset_columns(resolve_data_input(
      input$shp_file,
      fallback = saved_setup_path_for("shp_file")
    ), kind = "geometry")
  })

  survey_mapping_ids <- c(
    "var_year", "var_domain", "var_psu", "var_weight",
    "var_strata", "var_hh_size", "var_welfare",
    "var_povline", "var_benchmark_level"
  )

  columns_for_mapping_id <- function(id) {
    if (id %in% survey_mapping_ids) {
      survey_columns()
    } else if (identical(id, "rhs_domain")) {
      rhs_columns()
    } else if (identical(id, "shp_domain")) {
      shp_columns()
    } else {
      character()
    }
  }

  update_mapping_choices <- function(id, cols = NULL, selected = NULL) {
    if (is.null(cols)) cols <- columns_for_mapping_id(id)
    current <- if (is.null(selected)) isolate(input[[id]] %||% "") else selected
    current <- trimws(as.character(current %||% ""))
    updateSelectizeInput(
      session,
      id,
      choices = unique(c("", cols, current)),
      selected = current,
      options = list(
        create = TRUE,
        createOnBlur = TRUE,
        persist = FALSE,
        maxOptions = max(1000L, length(cols) + 5L)
      ),
      server = TRUE
    )
  }

  refresh_all_mapping_choices <- function() {
    for (id in survey_mapping_ids) {
      update_mapping_choices(id)
    }
    update_mapping_choices("rhs_domain")
    update_mapping_choices("shp_domain")
    invisible(TRUE)
  }

  observe({
    cols <- survey_columns()
    for (id in survey_mapping_ids) {
      update_mapping_choices(id, cols)
    }
  })

  observe({
    cols <- rhs_columns()
    update_mapping_choices("rhs_domain", cols)
  })

  observe({
    cols <- shp_columns()
    update_mapping_choices("shp_domain", cols)
  })

  mapping_invalid_entries <- reactive({
    entries <- list()
    add_entry <- function(id, label, cols, required = TRUE, enabled = TRUE) {
      if (!isTRUE(enabled) || length(cols) == 0) {
        return()
      }
      value <- trimws(input[[id]] %||% "")
      invalid <- if (nzchar(value)) {
        !(value %in% cols)
      } else {
        isTRUE(required)
      }
      if (isTRUE(invalid)) {
        entries[[length(entries) + 1L]] <<- list(
          id = id,
          label = label,
          value = value
        )
      }
    }

    s_cols <- survey_columns()
    r_cols <- rhs_columns()
    g_cols <- shp_columns()
    add_entry("var_year", "year", s_cols)
    add_entry("var_domain", "domain", s_cols)
    add_entry("var_psu", "psu", s_cols)
    add_entry("var_weight", "weight", s_cols)
    add_entry("var_strata", "strata", s_cols, required = FALSE)
    add_entry("var_hh_size", "household size", s_cols)
    add_entry("var_welfare", "welfare", s_cols)
    add_entry(
      "var_povline", "povline", s_cols,
      required = TRUE,
      enabled = identical(input$indicator_type %||% "poverty", "poverty") &&
        identical(input$povline_type %||% "column", "column")
    )
    add_entry(
      "var_benchmark_level", "benchmark level", s_cols,
      required = TRUE,
      enabled = isTRUE(input$do_benchmark) &&
        identical(input$benchmark_level %||% "national", "custom")
    )
    add_entry("rhs_domain", "auxiliary domain field", r_cols)
    add_entry("shp_domain", "shapefile domain field", g_cols)
    entries
  })

  observe({
    invalid <- mapping_invalid_entries()
    invalid_ids <- vapply(invalid, function(x) x$id, character(1))
    all_ids <- c(
      "var_year", "var_domain", "var_psu", "var_weight",
      "var_strata", "var_hh_size", "var_welfare", "var_povline",
      "var_benchmark_level", "rhs_domain", "shp_domain"
    )
    for (id in all_ids) {
      session$sendCustomMessage(
        "mappingValidity",
        list(
          id = id,
          invalid = id %in% invalid_ids,
          message = "Column not found in selected dataset"
        )
      )
    }
  })

  output$mapping_validation_summary <- renderUI({
    invalid <- mapping_invalid_entries()
    if (length(invalid) == 0) {
      return(NULL)
    }
    labels <- vapply(invalid, function(x) {
      val <- if (nzchar(x$value)) paste0("'", x$value, "'") else "(blank)"
      paste0(x$label, " = ", val)
    }, character(1))
    tags$div(
      class = "mapping-validation-summary",
      tags$strong("Check variable mapping: "),
      paste(labels, collapse = "; "),
      ". Red fields are not found in the selected dataset."
    )
  })

  set_active_setup_file <- function(key, file_name) {
    if (!nzchar(file_name %||% "")) {
      return(invisible(NULL))
    }
    files <- active_setup_files()
    files[[key]] <- basename(file_name)
    active_setup_files(files)
    invisible(files)
  }

  clear_active_setup_file <- function(key) {
    files <- active_setup_files()
    files[[key]] <- ""
    active_setup_files(files)
    invisible(files)
  }

  observeEvent(input$survey_file, {
    if (nzchar(input$survey_file$name %||% "")) {
      set_active_setup_file("survey_file", input$survey_file$name)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$rhs_file, {
    if (nzchar(input$rhs_file$name %||% "")) {
      set_active_setup_file("rhs_file", input$rhs_file$name)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$shp_file, {
    if (nzchar(input$shp_file$name %||% "")) {
      set_active_setup_file("shp_file", input$shp_file$name)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$regional_benchmark_file, {
    if (nzchar(input$regional_benchmark_file$name %||% "")) {
      benchmark_upload_cleared(FALSE)
      set_active_setup_file("regional_benchmark_file", input$regional_benchmark_file$name)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$clear_benchmark_file, {
    benchmark_upload_cleared(TRUE)
    clear_active_setup_file("regional_benchmark_file")
    session$sendCustomMessage("clearFileInput", "regional_benchmark_file")
    showNotification(
      "Benchmark Target Database cleared. Leave Apply benchmarking checked to use survey-direct benchmark targets, or uncheck it to run without benchmarking.",
      type = "message",
      duration = 6
    )
  }, ignoreInit = TRUE)
  observeEvent(input$population_file, {
    if (nzchar(input$population_file$name %||% "")) {
      set_active_setup_file("population_file", input$population_file$name)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$save_setup_btn, {
    tryCatch({
      path <- save_current_dashboard_setup("manual")
      showNotification(
        paste("Dashboard setup saved:", normalizePath(path, winslash = "/", mustWork = FALSE)),
        type = "message", duration = 6
      )
    }, error = function(e) {
      showNotification(paste("Could not save dashboard setup:", e$message),
                       type = "error", duration = 8)
    })
  })

  observeEvent(input$load_setup_btn, {
    setup <- read_dashboard_setup()
    if (is.null(setup)) {
      showNotification(
        "No saved dashboard setup was found. Run the app once or click 'Save Current Setup' first.",
        type = "warning", duration = 8
      )
      return()
    }
    apply_dashboard_setup(setup, "last setup")
    append_log(sprintf(
      "Loaded reusable dashboard setup: %s",
      normalizePath(dashboard_setup_path(), winslash = "/", mustWork = FALSE)
    ))
    showNotification("Loaded the last saved dashboard setup.", type = "message", duration = 5)
  })

  observeEvent(input$reset_setup_btn, {
    apply_dashboard_setup(dashboard_setup_defaults(), "default setup")
    if (file.exists(dashboard_setup_path())) {
      unlink(dashboard_setup_path(), force = TRUE)
    }
    if (dir.exists(dashboard_setup_file_dir())) {
      unlink(dashboard_setup_file_dir(), recursive = TRUE, force = TRUE)
    }
    append_log("Reset dashboard controls to default setup.")
    showNotification(
      "Dashboard controls reset to defaults and the saved local setup was cleared.",
      type = "message", duration = 8
    )
  })

  session$onFlushed(function() {
    setup <- read_dashboard_setup()
    if (!is.null(setup)) {
      apply_dashboard_setup(setup, "last setup")
    }
  }, once = TRUE)

  output$preflight_ui <- renderUI({
    build_check <- function(label, ok, detail) {
      color <- if (isTRUE(ok)) "#2e7d32" else "#b26a00"
      symbol <- if (isTRUE(ok)) "OK" else "Check"
      tags$div(
        style = "margin-bottom: 12px; padding: 10px 12px; border: 1px solid #ddd; border-radius: 6px;",
        tags$div(
          style = sprintf("font-weight: 600; color: %s;", color),
          sprintf("%s: %s", symbol, label)
        ),
        tags$div(detail)
      )
    }

    has_file <- function(path) nzchar(path %||% "") && file.exists(path)
    survey_path <- resolve_data_input(input$survey_file, fallback = saved_setup_path_for("survey_file"))
    rhs_path <- resolve_data_input(input$rhs_file, fallback = saved_setup_path_for("rhs_file"))
    shp_path <- resolve_data_input(input$shp_file, fallback = saved_setup_path_for("shp_file"))

    root_ok <- all(file.exists(c("app_support.R", "scripts/01_ufh.R", "scripts/02_mfh.R", "scripts/03_comparison.R")))
    years_vec <- parse_years(input$years)
    years_ok <- length(years_vec) == 2
    data_ok <- has_file(survey_path) && has_file(rhs_path) && has_file(shp_path)
    sf_ok <- requireNamespace("sf", quietly = TRUE)
    rmarkdown_ok <- requireNamespace("rmarkdown", quietly = TRUE)

    checks <- list(
      build_check(
        "Project root",
        root_ok,
        if (root_ok) {
          sprintf("Working directory looks correct: %s", normalizePath(".", winslash = "/", mustWork = FALSE))
        } else {
          "Required app files are missing from the current working directory. Launch the app from the package root folder."
        }
      ),
      build_check(
        "rmarkdown",
        rmarkdown_ok,
        if (rmarkdown_ok) {
          "Package 'rmarkdown' is available for report rendering."
        } else {
          "Package 'rmarkdown' is not installed. Report rendering will fail. Install with: install.packages('rmarkdown')"
        }
      ),
      build_check(
        "Analysis years",
        years_ok,
        if (years_ok) {
          sprintf("Two analysis years detected: %s", paste(years_vec, collapse = ", "))
        } else {
          "Enter exactly two years separated by a comma."
        }
      ),
      build_check(
        "Input data files",
        data_ok,
        sprintf(
          "Survey: %s | Auxiliary: %s | Geometry: %s",
          if (has_file(survey_path)) "available" else "missing",
          if (has_file(rhs_path)) "available" else "missing",
          if (has_file(shp_path)) "available" else "missing"
        )
      ),
      build_check(
        "Spatial dependency",
        sf_ok,
        if (sf_ok) {
          "Package 'sf' is available."
        } else {
          "Package 'sf' is not available. Mapping and geometry-dependent steps may fail."
        }
      )
    )

    tags$div(checks)
  })

  output$preflight_actions <- renderText({
    actions <- character()

    root_ok <- all(file.exists(c("app_support.R", "scripts/01_ufh.R", "scripts/02_mfh.R", "scripts/03_comparison.R")))
    if (!root_ok) {
      actions <- c(actions, "- Open or launch the app from the package root directory.")
    }

    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      actions <- c(actions, "- Install the `rmarkdown` package for report rendering: install.packages('rmarkdown')")
    }

    years_vec <- parse_years(input$years)
    if (length(years_vec) != 2) {
      actions <- c(actions, "- Set `Analysis years` to exactly two comma-separated years.")
    }

    survey_path <- resolve_data_input(input$survey_file, fallback = saved_setup_path_for("survey_file"))
    rhs_path <- resolve_data_input(input$rhs_file, fallback = saved_setup_path_for("rhs_file"))
    shp_path <- resolve_data_input(input$shp_file, fallback = saved_setup_path_for("shp_file"))
    if (!file.exists(survey_path %||% "")) {
      actions <- c(actions, sprintf("- Choose the household survey file with the Survey data Browse button. Accepted formats include `.rds`, `.csv`, `.dat`, `.dta`, `.sav`, `.sas7bdat`, `.parquet`, `.feather`, and Excel files. Resolved path: `%s`.", display_data_path(survey_path)))
    }
    if (!file.exists(rhs_path %||% "")) {
      actions <- c(actions, sprintf("- Choose the auxiliary covariates file with the Auxiliary data Browse button. Accepted formats include `.rds`, `.csv`, `.dat`, `.dta`, `.sav`, `.sas7bdat`, `.parquet`, `.feather`, and Excel files. Resolved path: `%s`.", display_data_path(rhs_path)))
    }
    if (!file.exists(shp_path %||% "")) {
      actions <- c(actions, sprintf("- Choose the shapefiles/geometries file with the Shapefile/geometries Browse button. For ESRI shapefiles, upload one `.zip` containing `.shp`, `.shx`, and `.dbf`; a standalone `.shp` file is usually not enough. Other accepted formats include `.rds`, `.gpkg`, and `.geojson`. Resolved path: `%s`.", display_data_path(shp_path)))
    }

    if (!requireNamespace("sf", quietly = TRUE)) {
      actions <- c(actions, "- Install the `sf` package and its system libraries before running geometry-dependent outputs.")
    }

    if (.Platform$OS.type == "windows") {
      actions <- c(actions, "- If files are stored in OneDrive, make sure required inputs are fully available offline before running.")
    }

    if (length(actions) == 0) {
      "No obvious blockers detected. You can run the pipeline."
    } else {
      paste(actions, collapse = "\n")
    }
  })


  # ---- Data Readiness tab outputs ----
  output$readiness_messages <- renderText({
    rr <- readiness_result()
    if (is.null(rr)) return("Click '1. Check Data Readiness' in the sidebar to generate diagnostics.")
    paste(rr$messages, collapse = "\n")
  })

  output$readiness_national <- renderTable({
    rr <- readiness_result()
    if (is.null(rr)) return(NULL)
    df <- rr$national_poverty
    # Format and label the headline statistic according to the chosen
    # indicator. For poverty (FGT) it is a rate in [0, 1] shown as a
    # percentage; for mean welfare it is a level in the configured
    # currency (and on the log scale when log_transform is on, in which
    # case formatting it as "%" would mislead).
    ind <- input$indicator_type %||% "poverty"
    use_log <- identical(input$ufh_transformation, "log") && identical(ind, "mean_welfare")
    if (identical(ind, "mean_welfare")) {
      if (use_log) {
        df$national_rate <- sprintf("%.4f", df$national_rate)
        col_label <- "Mean log welfare"
      } else {
        cur <- input$currency_symbol %||% "EUR"
        df$national_rate <- format(round(df$national_rate, 1),
                                    big.mark = ",", nsmall = 1, trim = TRUE)
        col_label <- sprintf("Mean welfare (%s)", cur)
      }
    } else {
      df$national_rate <- sprintf("%.2f%%", df$national_rate * 100)
      col_label <- "Poverty Rate"
    }
    names(df) <- c("Year", col_label, "Households", "Domains")
    df
  }, striped = TRUE, align = "lrrr")

  output$readiness_domains <- renderTable({
    rr <- readiness_result()
    if (is.null(rr)) return(NULL)
    di <- rr$domain_consistency
    rows <- data.frame(
      Dataset  = c("Survey", "Auxiliary"),
      Domains  = c(length(di$survey_domains), length(di$aux_domains)),
      stringsAsFactors = FALSE
    )
    if (!is.null(di$geo_domains)) {
      rows <- rbind(rows, data.frame(Dataset = "Geometries",
                                     Domains = length(di$geo_domains),
                                     stringsAsFactors = FALSE))
    }
    rows <- rbind(rows, data.frame(Dataset = "All Consistent?",
                                   Domains = ifelse(di$all_consistent, "Yes", "No"),
                                   stringsAsFactors = FALSE))
    rows$Domains <- as.character(rows$Domains)
    rows
  }, striped = TRUE)

  output$readiness_missing <- renderTable({
    rr <- readiness_result()
    if (is.null(rr)) return(NULL)
    if (nrow(rr$missing_poverty) == 0) {
      return(data.frame(Result = "No missing poverty rates - all domains have survey-based estimates."))
    }
    names(rr$missing_poverty) <- c("Domain", "Year", "Reason")
    rr$missing_poverty
  }, striped = TRUE)

  output$readiness_aux <- renderTable({
    rr <- readiness_result()
    if (is.null(rr)) return(NULL)
    df <- rr$aux_summary
    cor_label <- attr(df, "cor_target_label") %||% "Corr. w/ Poverty"
    names(df) <- c("Variable", "Mean", "Std. Error", "N", cor_label)
    df
  }, striped = TRUE, digits = 4)

  output$validation_text <- renderText({
    vr <- validation_result()
    if (is.null(vr)) return("No validation run yet. Click 'Run Pipeline' to start.")
    paste(vr$flags, collapse = "\n")
  })

  output$data_summary_text <- renderText({
    vr <- validation_result()
    if (is.null(vr)) return("")
    paste(capture.output(str(vr$summary)), collapse = "\n")
  })

  output$diagnostics_text <- renderText({
    dd <- diagnostics_data()
    if (is.null(dd)) return("No diagnostics available yet. Run the pipeline first.")
    lines <- character()
    for (yr_name in names(dd$diag)) {
      d <- dd$diag[[yr_name]]
      lines <- c(lines, sprintf("--- Year: %s ---", d$year %||% yr_name))
      lines <- c(lines, sprintf("  Model type:  %s", d$model_type %||% "UFH"))
      lines <- c(lines, sprintf("  Domains:     %s", d$n_domains %||% "N/A"))
      lines <- c(lines, sprintf("  Convergence: %s", if (isTRUE(d$convergence)) "Yes" else "N/A"))
      if (!is.na(d$re_shapiro_pvalue %||% NA)) {
        lines <- c(lines, sprintf("  RE normality (Shapiro p): %.4f [%s]",
                                   d$re_shapiro_pvalue,
                                   if (isTRUE(d$re_shapiro_pass)) "PASS" else "FAIL"))
      }
      if (!is.na(d$resid_shapiro_pvalue %||% NA)) {
        lines <- c(lines, sprintf("  Resid normality (Shapiro p): %.4f [%s]",
                                   d$resid_shapiro_pvalue,
                                   if (isTRUE(d$resid_shapiro_pass)) "PASS" else "FAIL"))
      }
      lines <- c(lines, "")

      # Benchmark summary
      b <- dd$bench[[yr_name]]
      if (!is.null(b)) {
        if (!is.null(b$estimate_range)) {
          lines <- c(lines, sprintf("  Estimate range: [%.4f, %.4f]", b$estimate_range[1], b$estimate_range[2]))
        }
        if (!is.na(b$estimate_median %||% NA)) {
          lines <- c(lines, sprintf("  Median estimate: %.4f", b$estimate_median))
        }
        if (!is.na(b$cv_median %||% NA)) {
          lines <- c(lines, sprintf("  Median CV: %.4f", b$cv_median))
        }
        if (!is.na(b$cv_max %||% NA)) {
          lines <- c(lines, sprintf("  Max CV: %.4f", b$cv_max))
        }
        if (!is.na(b$n_cv_above_25pct %||% NA)) {
          lines <- c(lines, sprintf("  Domains with CV > 25%%: %d", b$n_cv_above_25pct))
        }
        lines <- c(lines, "")
      }
    }
    paste(lines, collapse = "\n")
  })

  output$llm_interpretation <- renderText({
    llm_interp() %||% ""
  })

  output$brief_template <- renderText({
    br <- brief_result()
    if (is.null(br)) return("No brief available yet. Run the pipeline first.")
    br$template_brief
  })

  output$brief_llm_text <- renderText({
    llm_brief() %||% ""
  })

  output$normality_eval_text <- renderText({
    normality_eval() %||% ""
  })

  # ---- Invalidate readiness when inputs that affect it change ----
  # If the user swaps data files or changes the variable mapping after clicking
  # "Check Data Readiness", they must re-check before running the analysis.
  # Only the inputs below create reactive dependencies; readiness_checked() is
  # read with isolate() so that setting it to TRUE in check_btn does not
  # immediately re-trigger this observer and flip it back to FALSE.
  #
  # Readiness output depends on more than just the data files and column
  # mapping: it varies with indicator_type, the FGT alpha, and the
  # per-model transformation choices (because log fits trigger
  # log-specific data checks like non-positive welfare). It also
  # depends on which `steps` will run. Touching all of them here forces
  # the user to re-run "Check Data Readiness" if they change any
  # setting that could change the readiness verdict.
  observe({
    if (isTRUE(restoring_setup())) {
      return()
    }
    # Touch each input so this observer reacts to any of them.
    list(
      # Files and variable mapping
      input$survey_file, input$rhs_file, input$shp_file,
      input$regional_benchmark_file, input$population_file,
      input$var_year, input$var_domain, input$var_psu,
      input$var_weight, input$var_strata, input$var_hh_size,
      input$var_benchmark_level, input$var_welfare, input$var_povline,
      input$rhs_domain, input$shp_domain,
      # Poverty-line config
      input$povline_type, get_povline_numeric_by_year(parse_years(input$years)),
      # Indicator and FGT (changes which Test 4 statistic is computed
      # and which log-specific checks fire)
      input$indicator_type, input$fgt_alpha,
      # Per-model transformation choices (drive log-specific data
      # checks; readiness now ORs UFH and MFH log flags)
      input$ufh_transformation, input$ufh_backtrans,
      input$mfh_transformation, input$mfh_backtrans,
      input$ufh_lasso_enabled, input$ufh_lasso_lambda,
      input$mfh_lasso_enabled, input$mfh_lasso_lambda,
      input$analysis_seed, input$mcpe_bootstrap_replicates,
      # Which steps will run (changes the readiness scope/messages)
      input$steps,
      # Benchmarking choices affect required variables and outputs.
      input$do_benchmark,
      input$run_label
    )
    isolate({
      if (isTRUE(readiness_checked())) {
        readiness_checked(FALSE)
        append_log("Data inputs or analysis options changed - please re-run 'Check Data Readiness'.")
      }
    })
  })

  # ---- Check Data Readiness (Stage 1) ----
  # Runs only the validation + data-readiness portion. The user reviews the
  # results in the Preflight and Data Readiness tabs, then clicks Run Analysis.
  observeEvent(input$check_btn, {
    status("Checking data readiness...")
    logs("")
    append_log("Running preflight and data readiness checks...")

    survey_path <- resolve_data_input(input$survey_file, fallback = saved_setup_path_for("survey_file"))
    rhs_path    <- resolve_data_input(input$rhs_file, fallback = saved_setup_path_for("rhs_file"))
    shp_path    <- resolve_data_input(input$shp_file, fallback = saved_setup_path_for("shp_file"))
    append_log(paste("Survey data path:", display_data_path(survey_path)))
    append_log(paste("Auxiliary data path:", display_data_path(rhs_path)))
    append_log(paste("Geometry data path:", display_data_path(shp_path)))
    missing_inputs <- missing_data_inputs(survey_path, rhs_path, shp_path)
    if (length(missing_inputs) > 0) {
      msg <- paste0(
        "Missing data input(s): ", paste(missing_inputs, collapse = ", "),
        ". Choose the files with the Browse buttons before checking readiness."
      )
      status("Data readiness check failed")
      append_log(paste("ERROR:", msg))
      showNotification(msg, type = "error", duration = 10)
      return()
    }
    regional_benchmark_path <- if (isTRUE(input$do_benchmark) && !isTRUE(benchmark_upload_cleared())) {
      resolve_upload(input$regional_benchmark_file, fallback = saved_setup_path_for("regional_benchmark_file"))
    } else {
      NULL
    }
    population_path <- if (isTRUE(input$do_benchmark)) {
      resolve_upload(input$population_file, fallback = saved_setup_path_for("population_file"))
    } else {
      resolve_upload(input$population_file, fallback = saved_setup_path_for("population_file"))
    }
    if (!isTRUE(input$do_benchmark) && !is.null(population_path) && nzchar(population_path %||% "")) {
      append_log("Population file uploaded but benchmarking is off; population file will be saved in the config but not used in this run.")
    }
    var_map     <- get_var_map()
    requested_years <- parse_years(input$years)
    povline_numeric_map <- get_povline_numeric_by_year(requested_years)
    if (identical(input$povline_type %||% "column", "numeric")) {
      append_log(sae_poverty_line_log_message("numeric", povline_numeric_map, requested_years))
    }

    survey_for_validation <- survey_path
    rhs_for_validation    <- rhs_path

    harmonized <- tryCatch(
      load_and_harmonize(survey_for_validation, rhs_for_validation,
                         var_map, input$rhs_domain,
                         povline_type  = input$povline_type %||% "column",
                         povline_value = povline_numeric_map,
                         indicator_type = input$indicator_type %||% "poverty"),
      error = function(e) {
        append_log(paste("ERROR while loading data:", conditionMessage(e)))
        NULL
      }
    )

    if (is.null(harmonized)) {
      status("Data readiness check failed")
      append_log("WARNING: Could not load data for readiness check.")
      showNotification(
        "Could not load data for readiness check. See the Status tab for details.",
        type = "error", duration = 8
      )
      return()
    }

    # Validation flags feed into the Preflight tab.
    validation_survey <- filter_to_analysis_years(harmonized$survey, requested_years)
    validation_rhs <- filter_to_analysis_years(harmonized$rhs, requested_years)
    flags <- validate_inputs(validation_survey, validation_rhs)
    validation_result(flags)
    append_log(sprintf("Validation complete: %d flag(s)", length(flags$flags)))

    # Year-variable checks (pre-harmonization) - mirrors the logic inside run_btn.
    year_msgs <- character()
    survey_raw_check <- tryCatch(sae_read_table_input(survey_for_validation, "Survey data"), error = function(e) NULL)
    rhs_raw_check    <- tryCatch(sae_read_table_input(rhs_for_validation, "Auxiliary covariates"), error = function(e) NULL)
    year_var_name    <- var_map$year

    if (!is.null(survey_raw_check) && !is.null(rhs_raw_check)) {
      survey_has_year <- year_var_name %in% names(survey_raw_check)
      aux_year_var_name <- if (year_var_name %in% names(rhs_raw_check)) year_var_name else "year"
      aux_has_year    <- aux_year_var_name %in% names(rhs_raw_check)

      if (survey_has_year && aux_has_year) {
        year_msgs <- c(year_msgs, sprintf(
          "Test 0a: Both survey data and auxiliary covariates contain the year variable '%s'.",
          year_var_name
        ))
      } else {
        missing_in <- character()
        if (!survey_has_year) missing_in <- c(missing_in, "survey data")
        if (!aux_has_year)    missing_in <- c(missing_in, "auxiliary covariates")
        year_msgs <- c(year_msgs, sprintf(
          "Test 0a: WARNING \u2014 Year variable '%s' is MISSING from: %s.",
          year_var_name, paste(missing_in, collapse = " and ")
        ))
      }

      if (survey_has_year && aux_has_year) {
        survey_years <- sort(unique(survey_raw_check[[year_var_name]]))
        aux_years    <- sort(unique(rhs_raw_check[[aux_year_var_name]]))
        if (identical(as.character(survey_years), as.character(aux_years))) {
          year_msgs <- c(year_msgs, sprintf(
            "Test 0b: Survey and auxiliary covariates cover the same years (%s).",
            paste(survey_years, collapse = ", ")
          ))
        } else {
          in_survey_not_aux <- setdiff(survey_years, aux_years)
          in_aux_not_survey <- setdiff(aux_years, survey_years)
          parts <- character()
          if (length(in_survey_not_aux) > 0)
            parts <- c(parts, sprintf("year(s) %s in survey but not in auxiliary covariates",
                                      paste(in_survey_not_aux, collapse = ", ")))
          if (length(in_aux_not_survey) > 0)
            parts <- c(parts, sprintf("year(s) %s in auxiliary covariates but not in survey",
                                      paste(in_aux_not_survey, collapse = ", ")))
          year_msgs <- c(year_msgs, sprintf(
            "Test 0b: WARNING \u2014 Year mismatch: %s. Survey years: %s. Auxiliary years: %s.",
            paste(parts, collapse = "; "),
            paste(survey_years, collapse = ", "),
            paste(aux_years, collapse = ", ")
          ))
        }
        missing_requested_survey <- setdiff(requested_years, survey_years)
        missing_requested_aux <- setdiff(requested_years, aux_years)
        if (length(missing_requested_survey) > 0 || length(missing_requested_aux) > 0) {
          parts <- character()
          if (length(missing_requested_survey) > 0) {
            parts <- c(parts, sprintf("survey data lacks requested year(s): %s",
                                      paste(missing_requested_survey, collapse = ", ")))
          }
          if (length(missing_requested_aux) > 0) {
            parts <- c(parts, sprintf("auxiliary covariates lack requested year(s): %s",
                                      paste(missing_requested_aux, collapse = ", ")))
          }
          year_msgs <- c(year_msgs, sprintf(
            "Test 0b: ERROR -- Requested analysis years (%s) are not available: %s.",
            paste(requested_years, collapse = ", "),
            paste(parts, collapse = "; ")
          ))
        }
      }
    }

    benchmark_level_var <- get_benchmark_level_var()
    if (isTRUE(input$do_benchmark) &&
        identical(get_benchmark_level(), "custom") &&
        !nzchar(benchmark_level_var)) {
      year_msgs <- c(year_msgs,
                     "Test 0d: ERROR -- Grouped benchmarking requires a benchmark-level survey variable. Select a variable or choose National benchmarking.")
    } else if (isTRUE(input$do_benchmark) && nzchar(benchmark_level_var)) {
      if (is.null(survey_raw_check) || !benchmark_level_var %in% names(survey_raw_check)) {
        year_msgs <- c(year_msgs, sprintf(
          "Test 0d: ERROR -- Grouped benchmarking requires benchmark-level column '%s' in the survey data. Select National benchmarking if a grouping variable is not required.",
          benchmark_level_var
        ))
      }
    }

    geo_read <- tryCatch(
      list(data = sae_read_geometry_input(shp_path, "Geometry data"),
           messages = character()),
      error = function(e) list(
        data = NULL,
        messages = sprintf(
          "Test 2: ERROR -- Geometry data could not be read: %s",
          conditionMessage(e)
        )
      )
    )
    readiness_survey <- filter_to_analysis_years(harmonized$survey, requested_years)
    readiness_rhs <- filter_to_analysis_years(harmonized$rhs, requested_years)
    rr <- tryCatch(
      assess_data_readiness(
        survey_data    = readiness_survey,
        aux_data       = readiness_rhs,
        geo_data       = geo_read$data,
        domain_var     = input$shp_domain,
        save_to        = "outputs/tables",
        fgt_alpha      = as.integer(input$fgt_alpha %||% 0),
        indicator_type = input$indicator_type %||% "poverty",
        # Readiness assesses log-specific issues (non-positive welfare,
        # extreme tails after logging, etc.) whenever EITHER pipeline
        # plans to fit on log. UFH and MFH now have independent
        # transformation choices, so we OR the two flags.
        log_transform  = identical(input$indicator_type, "mean_welfare") &&
                         (identical(input$ufh_transformation, "log") ||
                          identical(input$mfh_transformation, "log"))
      ),
      error = function(e) empty_readiness_result(sprintf(
        "Data readiness ERROR -- %s",
        conditionMessage(e)
      ))
    )
    rr$messages <- c(year_msgs, geo_read$messages, rr$messages)
    readiness_result(rr)
    append_log(sprintf("Data readiness: %d diagnostic messages", length(rr$messages)))

    readiness_errors <- isTRUE(flags$has_errors) ||
      any(grepl("ERROR", c(flags$flags, rr$messages), ignore.case = FALSE))
    readiness_checked(!readiness_errors)
    status(if (readiness_errors) {
      "Data readiness check found blocking errors"
    } else {
      "Data readiness check complete - review the results, then click Run Analysis."
    })
    updateTabsetPanel(session, "main_tabs", selected = "Data Readiness")
    if (readiness_errors) {
      showNotification(
        "Data readiness found blocking errors. Fix the items marked ERROR before running the analysis.",
        type = "error", duration = 10
      )
    } else {
      showNotification(
        "Data readiness check complete. Review the Preflight and Data Readiness tabs, then click '2. Run Analysis'.",
        type = "message", duration = 8
      )
    }
  })

  # ---- Run Pipeline ----
  observeEvent(input$run_btn, {
    # Gate: the user must first click "Check Data Readiness" so they see the
    # preflight and readiness diagnostics before the UFH / MFH / Comparison
    # pipeline runs. This prevents long-running analyses on unreviewed inputs.
    if (!isTRUE(readiness_checked())) {
      showModal(modalDialog(
        title = "Please check data readiness first",
        tags$p(
          "Before running the analysis, please click ",
          tags$strong("1. Check Data Readiness"),
          " in the sidebar and review the Preflight and Data Readiness tabs."
        ),
        tags$p(
          "This ensures your survey data, auxiliary covariates, and geometry ",
          "file are consistent before the UFH / MFH / Comparison pipeline starts."
        ),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      # Also jump to the Data Readiness tab as a hint.
      updateTabsetPanel(session, "main_tabs", selected = "Data Readiness")
      return()
    }

    status("Preparing configuration...")
    logs("")
    llm_interp(NULL)
    llm_brief(NULL)
    normality_eval(NULL)
    run_location("Preparing run folder...")

    # Build ordered list of pipeline stages
    pipeline_stages <- "Validation"
    if ("UFH" %in% input$steps) pipeline_stages <- c(pipeline_stages, "UFH")
    if ("MFH" %in% input$steps) pipeline_stages <- c(pipeline_stages, "MFH")
    if ("Comparison" %in% input$steps) pipeline_stages <- c(pipeline_stages, "Comparison")
    if ("Comparison" %in% input$steps && isTRUE(input$llm_enabled)) pipeline_stages <- c(pipeline_stages, "LLM Interpretation")
    # Always attempt to render the final HTML report after the analysis
    # stages so users get an up-to-date outputs/final_report.html on every
    # pipeline run. Rendering is wrapped in tryCatch below, so a failure
    # logs a warning and the pipeline continues to "Finalizing".
    pipeline_stages <- c(pipeline_stages, "Render Report")
    pipeline_stages <- c(pipeline_stages, "Finalizing")
    n_steps <- length(pipeline_stages)

    progress <- shiny::Progress$new(session, min = 0, max = n_steps)
    progress$set(message = sprintf("[1/%d] Preparing...", n_steps), value = 0)
    on.exit(progress$close())
    step_counter <- 0L

    advance_progress <- function(stage, detail = NULL) {
      step_counter <<- step_counter + 1L
      msg <- sprintf("[%d/%d] %s", step_counter, n_steps, stage)
      progress$set(value = step_counter, message = msg, detail = detail)
    }

    run_id  <- format(Sys.time(), "%Y%m%d_%H%M%S")
    run_label_raw <- trimws(input$run_label %||% "")
    run_label_safe <- gsub("[^A-Za-z0-9_-]+", "_", run_label_raw)
    run_label_safe <- gsub("^_+|_+$", "", run_label_safe)
    run_dir_name <- if (nzchar(run_label_safe)) {
      paste(run_id, run_label_safe, sep = "_")
    } else {
      run_id
    }
    run_dir <- file.path("app_runs", run_dir_name)
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    run_dir_abs <- normalizePath(run_dir, winslash = "/", mustWork = FALSE)
    run_outputs_abs <- normalizePath(file.path(run_dir, "outputs"),
                                     winslash = "/", mustWork = FALSE)
    run_location(paste(
      paste("Run folder:", run_dir_abs),
      paste("Archived outputs:", run_outputs_abs),
      "Current working outputs are also written to: outputs/",
      sep = "\n"
    ))
    append_log(paste("Run folder:", run_dir_abs))
    append_log(paste("Archived outputs will be saved to:", run_outputs_abs))

    survey_path <- resolve_data_input(input$survey_file, fallback = saved_setup_path_for("survey_file"))
    rhs_path    <- resolve_data_input(input$rhs_file, fallback = saved_setup_path_for("rhs_file"))
    shp_path    <- resolve_data_input(input$shp_file, fallback = saved_setup_path_for("shp_file"))
    append_log(paste("Survey data path:", display_data_path(survey_path)))
    append_log(paste("Auxiliary data path:", display_data_path(rhs_path)))
    append_log(paste("Geometry data path:", display_data_path(shp_path)))
    missing_inputs <- missing_data_inputs(survey_path, rhs_path, shp_path)
    if (length(missing_inputs) > 0) {
      msg <- paste0(
        "Missing data input(s): ", paste(missing_inputs, collapse = ", "),
        ". Choose the files with the Browse buttons before running analysis."
      )
      status("Missing data inputs")
      append_log(paste("ERROR:", msg))
      showNotification(msg, type = "error", duration = 10)
      return()
    }
    regional_benchmark_path <- if (isTRUE(input$do_benchmark) && !isTRUE(benchmark_upload_cleared())) {
      resolve_upload(input$regional_benchmark_file, fallback = saved_setup_path_for("regional_benchmark_file"))
    } else {
      NULL
    }
    population_path <- if (isTRUE(input$do_benchmark)) {
      resolve_upload(input$population_file, fallback = saved_setup_path_for("population_file"))
    } else {
      resolve_upload(input$population_file, fallback = saved_setup_path_for("population_file"))
    }
    if (!isTRUE(input$do_benchmark) && !is.null(population_path) && nzchar(population_path %||% "")) {
      append_log("Population file uploaded but benchmarking is off; population file will be saved in the config but not used in this run.")
    }

    years          <- parse_years(input$years)
    povline_numeric_map <- get_povline_numeric_by_year(years)
    if (identical(input$povline_type %||% "column", "numeric")) {
      append_log(sae_poverty_line_log_message("numeric", povline_numeric_map, years))
    }
    ufh_candidates_y1 <- split_csv(input$ufh_candidates_y1)
    ufh_candidates_y2 <- split_csv(input$ufh_candidates_y2)
    mfh_candidates_y1 <- split_csv(input$mfh_candidates_y1)
    mfh_candidates_y2 <- split_csv(input$mfh_candidates_y2)
    var_map        <- get_var_map()
    benchmark_level <- get_benchmark_level()
    benchmark_level_var <- get_benchmark_level_var()
    benchmark_target_uploaded <- isTRUE(input$do_benchmark) &&
      !is.null(regional_benchmark_path) &&
      nzchar(regional_benchmark_path %||% "")
    benchmark_source_label <- if (!isTRUE(input$do_benchmark)) {
      "benchmarking off"
    } else if (benchmark_target_uploaded) {
      "Benchmark Target Database"
    } else {
      "survey direct"
    }
    benchmark_level_label <- if (!isTRUE(input$do_benchmark)) {
      "not applied"
    } else if (identical(benchmark_level, "custom")) {
      paste0("grouped by ", benchmark_level_var)
    } else {
      "national"
    }
    append_log(paste("Benchmark level:", benchmark_level_label))
    append_log(paste("Benchmark source:", benchmark_source_label))

    # ---- Step 1: Validate input data ----
    advance_progress("Validation", "Checking input data")
    status("Validating input data...")
    append_log("Validating input data...")
    survey_for_validation <- survey_path
    rhs_for_validation    <- rhs_path

    harmonized <- tryCatch(
      load_and_harmonize(survey_for_validation, rhs_for_validation,
                         var_map, input$rhs_domain,
                         povline_type  = input$povline_type %||% "column",
                         povline_value = povline_numeric_map,
                         indicator_type = input$indicator_type %||% "poverty"),
      error = function(e) {
        append_log(paste("ERROR while loading data:", conditionMessage(e)))
        NULL
      }
    )

    # Map legacy covariance name if browser cached an old session
    mfh_cov_val <- input$mfh_cov_choice
    if (identical(mfh_cov_val, "rho_sm")) mfh_cov_val <- "rho_sm_out"

    if (!is.null(harmonized)) {
      validation_survey <- filter_to_analysis_years(harmonized$survey, years)
      validation_rhs <- filter_to_analysis_years(harmonized$rhs, years)
      flags <- validate_inputs(validation_survey, validation_rhs)
      validation_result(flags)
      append_log(sprintf("Validation complete: %d flag(s)", length(flags$flags)))

      # Data readiness assessment
      # -- Pre-harmonization checks: year variable in raw data --
      year_msgs <- character()
      survey_raw_check <- tryCatch(sae_read_table_input(survey_for_validation, "Survey data"), error = function(e) NULL)
      rhs_raw_check    <- tryCatch(sae_read_table_input(rhs_for_validation, "Auxiliary covariates"), error = function(e) NULL)
      year_var_name    <- var_map$year  # the user-specified year column name
      requested_years  <- years

      if (!is.null(survey_raw_check) && !is.null(rhs_raw_check)) {
        survey_has_year <- year_var_name %in% names(survey_raw_check)
        aux_year_var_name <- if (year_var_name %in% names(rhs_raw_check)) year_var_name else "year"
        aux_has_year    <- aux_year_var_name %in% names(rhs_raw_check)

        if (survey_has_year && aux_has_year) {
          year_msgs <- c(year_msgs, sprintf(
            "Test 0a: Both survey data and auxiliary covariates contain the year variable '%s'.",
            year_var_name
          ))
        } else {
          missing_in <- character()
          if (!survey_has_year) missing_in <- c(missing_in, "survey data")
          if (!aux_has_year)    missing_in <- c(missing_in, "auxiliary covariates")
          year_msgs <- c(year_msgs, sprintf(
            "Test 0a: WARNING \u2014 Year variable '%s' is MISSING from: %s.",
            year_var_name, paste(missing_in, collapse = " and ")
          ))
        }

        if (survey_has_year && aux_has_year) {
          survey_years <- sort(unique(survey_raw_check[[year_var_name]]))
          aux_years    <- sort(unique(rhs_raw_check[[aux_year_var_name]]))
          if (identical(as.character(survey_years), as.character(aux_years))) {
            year_msgs <- c(year_msgs, sprintf(
              "Test 0b: Survey and auxiliary covariates cover the same years (%s).",
              paste(survey_years, collapse = ", ")
            ))
          } else {
            in_survey_not_aux <- setdiff(survey_years, aux_years)
            in_aux_not_survey <- setdiff(aux_years, survey_years)
            parts <- character()
            if (length(in_survey_not_aux) > 0)
              parts <- c(parts, sprintf("year(s) %s in survey but not in auxiliary covariates",
                                        paste(in_survey_not_aux, collapse = ", ")))
            if (length(in_aux_not_survey) > 0)
              parts <- c(parts, sprintf("year(s) %s in auxiliary covariates but not in survey",
                                        paste(in_aux_not_survey, collapse = ", ")))
            year_msgs <- c(year_msgs, sprintf(
              "Test 0b: WARNING \u2014 Year mismatch: %s. Survey years: %s. Auxiliary years: %s.",
              paste(parts, collapse = "; "),
              paste(survey_years, collapse = ", "),
              paste(aux_years, collapse = ", ")
            ))
          }
          missing_requested_survey <- setdiff(requested_years, survey_years)
          missing_requested_aux <- setdiff(requested_years, aux_years)
          if (length(missing_requested_survey) > 0 || length(missing_requested_aux) > 0) {
            parts <- character()
            if (length(missing_requested_survey) > 0) {
              parts <- c(parts, sprintf("survey data lacks requested year(s): %s",
                                        paste(missing_requested_survey, collapse = ", ")))
            }
            if (length(missing_requested_aux) > 0) {
              parts <- c(parts, sprintf("auxiliary covariates lack requested year(s): %s",
                                        paste(missing_requested_aux, collapse = ", ")))
            }
            year_msgs <- c(year_msgs, sprintf(
              "Test 0b: ERROR -- Requested analysis years (%s) are not available: %s.",
              paste(requested_years, collapse = ", "),
              paste(parts, collapse = "; ")
            ))
          }
        }
      }

      benchmark_level_var <- get_benchmark_level_var()
      if (isTRUE(input$do_benchmark) &&
          identical(get_benchmark_level(), "custom") &&
          !nzchar(benchmark_level_var)) {
        year_msgs <- c(year_msgs,
                       "Test 0d: ERROR -- Grouped benchmarking requires a benchmark-level survey variable. Select a variable or choose National benchmarking.")
      } else if (isTRUE(input$do_benchmark) && nzchar(benchmark_level_var)) {
        if (is.null(survey_raw_check) || !benchmark_level_var %in% names(survey_raw_check)) {
          year_msgs <- c(year_msgs, sprintf(
            "Test 0d: ERROR -- Grouped benchmarking requires benchmark-level column '%s' in the survey data. Select National benchmarking if a grouping variable is not required.",
            benchmark_level_var
          ))
        }
      }

      geo_read <- tryCatch(
        list(data = sae_read_geometry_input(shp_path, "Geometry data"),
             messages = character()),
        error = function(e) list(
          data = NULL,
          messages = sprintf(
            "Test 2: ERROR -- Geometry data could not be read: %s",
            conditionMessage(e)
          )
        )
      )
      readiness_survey <- filter_to_analysis_years(harmonized$survey, requested_years)
      readiness_rhs <- filter_to_analysis_years(harmonized$rhs, requested_years)
      rr <- tryCatch(
        assess_data_readiness(
          survey_data    = readiness_survey,
          aux_data       = readiness_rhs,
          geo_data       = geo_read$data,
          domain_var     = input$shp_domain,
          save_to        = "outputs/tables",
          fgt_alpha      = as.integer(input$fgt_alpha %||% 0),
          indicator_type = input$indicator_type %||% "poverty",
          # OR over UFH and MFH transformation choices -- see longer
          # comment on the first readiness call site above.
          log_transform  = identical(input$indicator_type, "mean_welfare") &&
                           (identical(input$ufh_transformation, "log") ||
                            identical(input$mfh_transformation, "log"))
        ),
        error = function(e) empty_readiness_result(sprintf(
          "Data readiness ERROR -- %s",
          conditionMessage(e)
        ))
      )
      # Prepend year-variable checks to readiness messages
      rr$messages <- c(year_msgs, geo_read$messages, rr$messages)
      readiness_result(rr)
      append_log(sprintf("Data readiness: %d diagnostic messages", length(rr$messages)))
      readiness_errors <- isTRUE(flags$has_errors) ||
        any(grepl("ERROR", c(flags$flags, rr$messages), ignore.case = FALSE))
      if (readiness_errors) {
        status("Data readiness check found blocking errors")
        append_log("ERROR: Data readiness found blocking errors; pipeline was not started.")
        showNotification(
          "Data readiness found blocking errors. Fix the items marked ERROR before running the analysis.",
          type = "error", duration = 10
        )
        return()
      }

      # Generate data properties note (template-based, no LLM)
      data_note <- generate_data_note(
        validation  = flags,
        var_map     = var_map,
        ufh_options = list(
          transformation     = input$ufh_transformation,
          # Bias correction is reported only when a transformation is in
          # play (arcsin or log); otherwise NA -- there is nothing to
          # back-transform.
          backtransformation = if (input$ufh_transformation %in% c("arcsin", "log"))
                                 input$ufh_backtrans else NA,
          # Variance-smoothing choice is only meaningful on the identity
          # scale; arcsin and log both already stabilize variances.
          var_choice         = if (input$ufh_transformation %in% c("arcsin", "log"))
                                 NA else (input$ufh_var_choice %||% "sm_out"),
          lasso_enabled      = isTRUE(input$ufh_lasso_enabled),
          lasso_lambda       = input$ufh_lasso_lambda %||% "lambda.1se",
          candidate_vars_y1  = ufh_candidates_y1,
          candidate_vars_y2  = ufh_candidates_y2
        ),
        mfh_options = list(
          # Transformation and bias correction are MFH-only here --
          # MFH never used arcsin, so the only meaningful values are
          # 'log' / 'no'. The MFH transformation dropdown is hidden
          # when the indicator is poverty (conditionalPanel), but
          # `input$mfh_transformation` keeps its last selected value
          # (default 'log' on app load) -- so we MUST gate on
          # indicator_type here, otherwise a default poverty run
          # would be reported as MFH transformation = 'log' even
          # though the actual MFH config gates log off.
          # Bias correction is only reported when the indicator is
          # mean_welfare AND MFH is set to log; otherwise NA --
          # there is nothing to back-transform.
          transformation     = if (identical(input$indicator_type, "mean_welfare"))
                                 (input$mfh_transformation %||% "no") else "no",
          backtransformation = if (identical(input$indicator_type, "mean_welfare") &&
                                    identical(input$mfh_transformation, "log"))
                                 input$mfh_backtrans else NA,
          var_choice     = input$mfh_var_choice,
          cov_choice     = mfh_cov_val,
          diag_model     = input$mfh_diag_model,
          fit_mfh3       = isTRUE(input$fit_mfh3) ||
                           input$mfh_diag_model %in% c("AUTO", "MFH3"),
          refvar_adjustment = input$mfh_refvar_adjustment %||% "bonferroni",
          refvar_alpha   = 0.05,
          do_benchmark   = isTRUE(input$do_benchmark),
          benchmark_level = benchmark_level,
          benchmark_level_variable = benchmark_level_var,
          benchmark_source = benchmark_source_label,
          benchmark_target_path = regional_benchmark_path,
          population_path         = population_path,
          lasso_enabled           = isTRUE(input$mfh_lasso_enabled),
          lasso_lambda            = input$mfh_lasso_lambda %||% "lambda.1se",
          candidate_vars_y1 = mfh_candidates_y1,
          candidate_vars_y2 = mfh_candidates_y2
        ),
        steps               = input$steps,
        psu_consistent_user = isTRUE(input$psu_consistent)
      )
      dir.create("outputs/data", showWarnings = FALSE, recursive = TRUE)
      dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
      dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

      # Build per-year diagnostics from raw data
      yr_list <- years
      diag_list  <- list()
      bench_list <- list()
      for (yr in yr_list) {
        yr_key <- paste0("y", yr)
        s <- build_year_summary(
          harmonized$survey, yr,
          fgt_alpha      = as.integer(input$fgt_alpha %||% 0),
          indicator_type = input$indicator_type %||% "poverty",
          log_transform  = identical(input$ufh_transformation, "log") &&
                           identical(input$indicator_type, "mean_welfare")
        )
        diag_list[[yr_key]]  <- s$diag
        bench_list[[yr_key]] <- s$bench
      }
      bench_list <- add_benchmark_metadata(
        bench_list,
        source = benchmark_source_label,
        level_label = benchmark_level_label,
        level_variable = benchmark_level_var,
        enabled = isTRUE(input$do_benchmark)
      )
      diagnostics_data(list(diag = diag_list, bench = bench_list))

      # Generate template brief (no LLM)
      llm_off <- llm_assistant(enabled = FALSE)
      br <- generate_analysis_brief(
        diagnostics   = diag_list,
        bench_summary = bench_list,
        input_flags   = flags,
        llm           = llm_off,
        language      = get_language(),
        country       = if (nzchar(trimws(input$country_name %||% ""))) trimws(input$country_name) else "Not specified",
        model_type    = if ("UFH" %in% input$steps) "UFH" else "MFH"
      )
      brief_result(br)
    } else {
      append_log("WARNING: Could not load data for validation.")
      validation_result(list(flags = c("WARNING: Could not load data files for validation."),
                              summary = list(), has_errors = TRUE))
      status("Input loading failed")
      showNotification("Input data could not be loaded. The analysis was not started.",
                       type = "error", duration = 10)
      return()
    }

    # ---- Step 2: Build pipeline config and run ----
    # Variance smoothing option for UFH is only active when no transformation
    # is used. arcsin and log both stabilize variances on their own scale, so
    # the R script pins var_choice to "sm_out" (a no-op for non-NA, non-outlier
    # rows) under those branches.
    ufh_var_val <- if (input$ufh_transformation %in% c("arcsin", "log")) {
      "sm_out"
    } else {
      input$ufh_var_choice %||% "sm_out"
    }

    # Bias correction has two complementary representations in the config:
    #   ufh$bias_correction        -- LOGICAL (TRUE = correct, FALSE = naive).
    #                                 This is the wire format the R script reads
    #                                 to decide whether to apply correction.
    #   ufh$bias_correction_method -- STRING label ("bc", "bc_sm", "none")
    #                                 capturing which corrector the user
    #                                 picked. Used for diagnostics, reports,
    #                                 and forward-compat (e.g. wiring "none"
    #                                 through to skip the smearing step).
    #   ufh$backtransformation     -- legacy string alias ("bc"/"bc_sm"/NULL)
    #                                 kept so older configs / consumers
    #                                 continue to parse.
    # Mapping per UI:
    #   arcsin + "bc"     -> bias_correction = TRUE,  method = "bc"
    #   arcsin + "none"   -> bias_correction = FALSE, method = "none"
    #   log    + "bc_sm"  -> bias_correction = TRUE,  method = "bc_sm"
    #   log    + "none"   -> bias_correction = FALSE, method = "none"
    #   no                -> bias_correction = NA,    method = NA
    ufh_bc_method <- if (input$ufh_transformation %in% c("arcsin", "log")) {
      input$ufh_backtrans %||% (if (identical(input$ufh_transformation, "arcsin")) "bc" else "bc_sm")
    } else {
      NA_character_
    }
    ufh_bc_logical <- if (input$ufh_transformation %in% c("arcsin", "log")) {
      !identical(ufh_bc_method, "none")
    } else {
      NA
    }
    # String alias used by older readers and by emdi's `backtransformation`
    # argument (NULL means "no back-transform").
    ufh_bt_string <- if (identical(ufh_bc_method, "none") || is.na(ufh_bc_method)) {
      NULL
    } else {
      ufh_bc_method
    }

    # The transformation actually passed to emdi::fh(). For "log" we
    # apply log() to the LHS in the R script before fitting, so emdi sees an
    # untransformed (identity-scale) LHS and we tell it transformation = "no".
    ufh_emdi_trans <- if (identical(input$ufh_transformation, "log")) {
      "no"
    } else {
      input$ufh_transformation
    }

    ufh_cfg <- list(
      survey_path             = survey_path,
      rhs_path                = rhs_path,
      shp_path                = shp_path,
      population_path         = population_path,
      do_benchmark            = isTRUE(input$do_benchmark),
      benchmark_level         = benchmark_level,
      benchmark_level_variable = benchmark_level_var,
      benchmark_source        = benchmark_source_label,
      benchmark_target_path   = regional_benchmark_path,
      regional_benchmark_path = regional_benchmark_path,
      var_map                 = var_map,
      rhs_domain              = input$rhs_domain,
      shp_domain              = input$shp_domain,
      years_keep              = years,
      transformation          = ufh_emdi_trans,
      bias_correction         = ufh_bc_logical,
      bias_correction_method  = ufh_bc_method,
      backtransformation      = ufh_bt_string,
      ic_criterion            = input$ufh_ic_criterion,
      var_choice              = ufh_var_val,
      lasso_enabled           = isTRUE(input$ufh_lasso_enabled),
      lasso_lambda            = input$ufh_lasso_lambda %||% "lambda.1se",
      candidate_vars_y1       = if (length(ufh_candidates_y1)) ufh_candidates_y1 else NULL,
      candidate_vars_y2       = if (length(ufh_candidates_y2)) ufh_candidates_y2 else NULL
    )

    # ---- MFH transformation + bias correction ------------------------------
    # The MFH dropdown is independent from UFH. Only meaningful for
    # mean_welfare; for poverty runs we force log_transform = FALSE.
    is_mean <- identical(input$indicator_type, "mean_welfare")
    mfh_log_transform <- is_mean && identical(input$mfh_transformation, "log")
    # Bias-correction representations mirror the UFH layout:
    #   bias_correction        -- LOGICAL wire format read by the R script
    #   bias_correction_method -- STRING label ("bc_sm" / "none" / NA)
    #   backtransformation     -- legacy STRING alias (NULL == none)
    # MFH only ever offers Duan smearing for log; "bc" (integration-based)
    # belongs to arcsin and isn't applicable here.
    mfh_bc_method <- if (mfh_log_transform) {
      input$mfh_backtrans %||% "bc_sm"
    } else {
      NA_character_
    }
    mfh_bc_logical <- if (mfh_log_transform) {
      !identical(mfh_bc_method, "none")
    } else {
      NA
    }
    mfh_bt_string <- if (identical(mfh_bc_method, "none") || is.na(mfh_bc_method)) {
      NULL
    } else {
      mfh_bc_method
    }

    mfh_cfg <- list(
      survey_path             = survey_path,
      rhs_path                = rhs_path,
      shp_path                = shp_path,
      regional_benchmark_path = regional_benchmark_path,
      benchmark_target_path   = regional_benchmark_path,
      population_path         = population_path,
      do_benchmark            = isTRUE(input$do_benchmark),
      benchmark_level         = benchmark_level,
      benchmark_level_variable = benchmark_level_var,
      benchmark_source        = benchmark_source_label,
      var_map                 = var_map,
      ic_criterion            = input$mfh_ic_criterion,
      rhs_domain              = input$rhs_domain,
      shp_domain              = input$shp_domain,
      years_keep              = years,
      var_choice              = input$mfh_var_choice,
      cov_choice              = mfh_cov_val,
      diag_model              = input$mfh_diag_model,
      fit_mfh3                = isTRUE(input$fit_mfh3) ||
                                input$mfh_diag_model %in% c("AUTO", "MFH3"),
      refvar_adjustment       = input$mfh_refvar_adjustment %||% "bonferroni",
      refvar_alpha            = 0.05,
      mcpe_nB                 = as.integer(input$mcpe_bootstrap_replicates %||% 200L),
      # Per-model log/no choice. The MFH R script reads this in preference
      # to the global cfg$log_transform; the global flag remains as a
      # backward-compat shim and is set below to the OR of UFH and MFH.
      log_transform           = mfh_log_transform,
      # MFH never passes log to emdi -- the log step is applied to the
      # LHS in the R script before fitting, so emdi sees an identity-scale
      # outcome. We therefore always store transformation = "no" here.
      transformation          = "no",
      bias_correction         = mfh_bc_logical,
      bias_correction_method  = mfh_bc_method,
      backtransformation      = mfh_bt_string,
      lasso_enabled           = isTRUE(input$mfh_lasso_enabled),
      lasso_lambda            = input$mfh_lasso_lambda %||% "lambda.1se",
      candidate_vars_y1       = if (length(mfh_candidates_y1)) mfh_candidates_y1 else NULL,
      candidate_vars_y2       = if (length(mfh_candidates_y2)) mfh_candidates_y2 else NULL
    )

    # Per-UFH log_transform flag (mirror of the per-MFH one). Lets the
    # UFH R script pick up its own setting independently when both are
    # present; otherwise it falls back to the global cfg$log_transform.
    ufh_cfg$log_transform <- identical(input$ufh_transformation, "log") && is_mean

    cfg <- list(
      years_keep      = years,
      country         = if (nzchar(trimws(input$country_name %||% ""))) trimws(input$country_name) else "Not specified",
      analysis_seed   = as.integer(input$analysis_seed %||% 123L),
      indicator_type  = input$indicator_type %||% "poverty",
      # Global log_transform: TRUE if either UFH or MFH is on log.
      # Kept so that helpers that don't know about per-model flags
      # (R/indicator_helpers.R, generate_data_note, etc.) keep working.
      # Each R script prefers its own per-model flag (cfg$ufh$log_transform
      # or cfg$mfh$log_transform) when present.
      log_transform   = (ufh_cfg$log_transform %||% FALSE) ||
                        (mfh_cfg$log_transform %||% FALSE),
      currency_symbol = input$currency_symbol %||% "EUR",
      fgt_alpha       = as.integer(input$fgt_alpha %||% 0),
      povline_type    = input$povline_type %||% "column",
      povline_value   = if (identical(input$povline_type, "numeric"))
                          povline_numeric_map else input$var_povline,
      run_id          = run_id,
      run_label       = run_label_raw,
      benchmarking    = list(
        enabled = isTRUE(input$do_benchmark),
        level   = benchmark_level,
        level_variable = benchmark_level_var,
        level_label = benchmark_level_label,
        source = benchmark_source_label,
        target_path = regional_benchmark_path
      ),
      data_inputs     = list(
        data_folder = data_dir_path(),
        survey_file = selected_setup_file_name(input$survey_file, "survey_file"),
        rhs_file = selected_setup_file_name(input$rhs_file, "rhs_file"),
        shp_file = selected_setup_file_name(input$shp_file, "shp_file"),
        benchmark_target_file = if (isTRUE(benchmark_upload_cleared())) {
          ""
        } else {
          selected_setup_file_name(input$regional_benchmark_file, "regional_benchmark_file")
        },
        population_file = selected_setup_file_name(input$population_file, "population_file"),
        survey_path = survey_path,
        rhs_path = rhs_path,
        shp_path = shp_path
      ),
      run             = list(steps = input$steps),
      ai              = list(
        enabled = isTRUE(input$llm_enabled) && isTRUE(input$llm_external_consent),
        provider = if (nchar(input$api_key %||% "") > 0) detect_llm_provider(input$api_key) else NA_character_,
        external_transfer_consent = isTRUE(input$llm_external_consent)
      ),
      ufh             = ufh_cfg,
      mfh             = mfh_cfg
    )

    check <- validate_app_config(cfg)
    if (!check$valid) {
      status("Invalid configuration")
      append_log(paste(check$errors, collapse = " | "))
      return()
    }

    cfg_path <- file.path(run_dir, "app_config.yml")
    write_app_config(cfg, cfg_path)
    append_log(paste("Saved config:", normalizePath(cfg_path, winslash = "/", mustWork = FALSE)))

    status("Running pipeline...")
    ok      <- TRUE
    err_msg <- NULL

    # Persist pipeline log to disk so the user can share it for diagnosis
    # when something goes wrong. File lives at app_runs/<run_id>/run.log.
    run_log_path <- file.path(run_dir, "run.log")
    # Initialize the file so it always exists even if no messages arrive.
    tryCatch({
      cat(sprintf("[%s] Pipeline run started (run_id=%s)\n",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"), run_id),
          file = run_log_path, append = FALSE)
    }, error = function(e) NULL)

    pipeline_logger <- function(msg) {
      append_log(msg)
      tryCatch({
        cat(sprintf("[%s] %s\n",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    paste(as.character(msg), collapse = " ")),
            file = run_log_path, append = TRUE)
      }, error = function(e) NULL)
    }

    pipeline_progress <- function(event, label) {
      if (event == "start") {
        detail <- switch(label,
          UFH        = "Univariate Fay-Herriot model",
          MFH        = "Multivariate Fay-Herriot model",
          Comparison = "Comparing UFH and MFH results",
          label
        )
        advance_progress(label, detail)
        status(sprintf("Running %s...", label))
      }
    }

    tryCatch(
      {
        run_pipeline_from_config(
          config_path       = cfg_path,
          logger            = pipeline_logger,
          progress_callback = pipeline_progress,
          render_report     = FALSE
        )
      },
      error = function(e) {
        ok      <<- FALSE
        err_msg <<- conditionMessage(e)
      }
    )

    # ---- Step 2b: Generate structured AI interpretations ----
    # The statistical pipeline finishes first, AI status is then saved as
    # structured data, and final_report.html is rendered exactly once after
    # both layers are ready. AI text is interleaved with the corresponding
    # statistical sections in final_report.html; no separate companion report
    # is produced.
    retired_ai_note <- file.path("outputs", "comparison_ai_note.html")
    if (file.exists(retired_ai_note)) unlink(retired_ai_note)
    ai_requested <- "Comparison" %in% input$steps && isTRUE(input$llm_enabled)
    ai_note_eligible <- ok && ai_requested &&
      isTRUE(input$llm_external_consent) && nchar(input$api_key %||% "") > 0
    append_log(sprintf("AI note check: ok=%s, Comparison=%s, llm_enabled=%s, api_key_set=%s -> eligible=%s",
                        ok, "Comparison" %in% input$steps, isTRUE(input$llm_enabled),
                        nchar(input$api_key %||% "") > 0, ai_note_eligible))
    provider_hint <- if (nchar(input$api_key %||% "") > 0) {
      detect_llm_provider(input$api_key)
    } else {
      NA_character_
    }
    model_hint <- if (identical(provider_hint, "openai")) {
      default_llm_model("openai")
    } else if (identical(provider_hint, "anthropic")) {
      default_llm_model("anthropic")
    } else {
      NA_character_
    }
    disabled_reason <- if (!ai_requested) {
      "AI interpretation was not requested for this run."
    } else if (!isTRUE(input$llm_external_consent)) {
      "AI interpretation was disabled because external-transfer consent was not recorded."
    } else if (nchar(input$api_key %||% "") == 0) {
      "AI interpretation was disabled because no provider API key was supplied."
    } else if (!ok) {
      "AI interpretation was not attempted because the statistical pipeline failed."
    } else {
      "AI interpretation was not available."
    }
    ai_state <- new_comparison_ai_interpretation_state(
      requested = ai_requested,
      enabled = ai_note_eligible,
      provider = provider_hint,
      model = model_hint,
      language = get_language(),
      consent_recorded = isTRUE(input$llm_external_consent),
      include_in_report = ai_requested,
      default_status = "disabled",
      default_reason = disabled_reason
    )

    if (ai_note_eligible) {
      advance_progress("LLM Interpretation", "Generating report interpretations")
      status("Generating AI interpretations...")
      append_log("Generating AI interpretations for the final report...")
      tryCatch({
        llm <- llm_assistant(api_key = input$api_key, provider = detect_llm_provider(input$api_key))
        ai_state <- generate_comparison_ai_interpretations(
          llm             = llm,
          language        = get_language(),
          indicator_type  = input$indicator_type %||% "poverty",
          currency_symbol = input$currency_symbol %||% "EUR",
          # The Comparison report is MFH-driven (per-domain-year smearing
          # back-transform of MFH EBLUPs, MCPE-based change CIs from
          # MFH MCPE), so the integrated AI interpretation must describe the MFH
          # scale. Using the UFH flag here would mislabel a run where
          # UFH and MFH chose different transformations (e.g. UFH=no,
          # MFH=log).
          log_transform   = identical(input$mfh_transformation, "log") &&
                            identical(input$indicator_type, "mean_welfare"),
          consent_recorded = isTRUE(input$llm_external_consent),
          logger          = function(msg) append_log(msg)
        )
        generated_count <- sum(vapply(
          ai_state$sections,
          function(section) identical(section$status, "generated"),
          logical(1)
        ))
        if (generated_count > 0L) {
          append_log(sprintf(
            "%d AI interpretation section(s) prepared for integration into final_report.html.",
            generated_count
          ))
        } else {
          append_log("WARNING: No AI interpretation sections were generated; failure statuses will appear in the final report.")
        }
      }, error = function(e) {
        failure_reason <- sanitize_ai_failure_message(conditionMessage(e))
        ai_state <- new_comparison_ai_interpretation_state(
          requested = TRUE,
          enabled = TRUE,
          provider = provider_hint,
          model = model_hint,
          language = get_language(),
          consent_recorded = TRUE,
          include_in_report = TRUE,
          default_status = "failed",
          default_reason = failure_reason
        )
        append_log(paste("WARNING: AI interpretation generation failed:", failure_reason))
      })
    }

    if (ok) {
      tryCatch({
        save_comparison_ai_interpretations(ai_state)
        statuses <- vapply(ai_state$sections, function(section) section$status, character(1))
        append_log(sprintf(
          "AI interpretation state saved: %d generated, %d failed, %d disabled section(s).",
          sum(statuses == "generated"), sum(statuses == "failed"),
          sum(statuses == "disabled")
        ))
        render_final_report(
          include_ai = ai_requested,
          logger = pipeline_logger,
          progress_callback = pipeline_progress
        )
      }, error = function(e) {
        ok <<- FALSE
        err_msg <<- paste("Final report rendering failed:", conditionMessage(e))
      })
    }

    # ---- Step 3: Check outputs and enrich diagnostics ----
    advance_progress("Finalizing", if (ok) "Checking outputs" else "Pipeline failed")
    status(if (ok) "Finalizing..." else "Run failed")
    files <- c(
      "outputs/final_report.html",
      "outputs/final_report.docx",
      "outputs/data/pov_fh.xlsx",
      "outputs/data/pov_mfh.xlsx",
      "outputs/data/ai_interpretations.rds",
      "outputs/data/pov_comparison_detailed.xlsx",
      "outputs/data/statistical_significance_comparison.xlsx",
      "outputs/data/ci_width_comparison.xlsx",
      "outputs/data/change_estimate_comparison.xlsx",
      "outputs/data/EU_SAE_results.xlsx",
      "outputs/tables/ufh_selection_diagnostics.csv",
      "outputs/tables/mfh_selection_diagnostics.csv",
      "outputs/tables/ufh_model_diagnostics.csv",
      "outputs/tables/mfh_model_diagnostics.csv"
    )
    bench_phrase <- if (isTRUE(input$do_benchmark)) {
      "with benchmarked values, CVs, and MSEs"
    } else {
      "with estimates, CVs, and MSEs (benchmarking off)"
    }
    descriptions <- c(
      "Combined HTML report with UFH, MFH, Comparison results, and clearly labelled AI interpretations when requested",
      "Editable Word version of the same completed report, including figures and full result tables",
      paste("UFH (Fay-Herriot) poverty estimates", bench_phrase),
      paste("MFH poverty estimates", bench_phrase),
      "Structured AI interpretation text, provider/model metadata, consent status, and per-section generation status",
      "Detailed comparison of UFH and MFH poverty estimates with CVs and MSEs for every domain and year",
      "Full pointwise, BH-adjusted, and Bonferroni-adjusted significance results for year-on-year changes",
      "Matched-domain UFH and MFH 95% confidence-interval widths with distribution and paired summaries",
      "Estimated changes by domain, distribution summaries and paired UFH-MFH differences",
      "Consolidated Excel workbook containing estimate, significance, and confidence-interval result tables",
      "UFH covariate-selection diagnostics, including optional LASSO screening",
      "MFH covariate-selection diagnostics, including optional LASSO screening",
      "UFH model-fit diagnostics with OLS companion R2 statistics",
      "MFH model-fit diagnostics with OLS companion R2 statistics"
    )
    exists_status <- ifelse(file.exists(files), "TRUE", "FALSE")
    update_output_table <- function(archive_dir = NULL) {
      exists_status <- ifelse(file.exists(files), "TRUE", "FALSE")

      current_files <- normalizePath(files, winslash = "/", mustWork = FALSE)
      saved_copy <- rep("Pending until run completes", length(files))
      if (!is.null(archive_dir) && dir.exists(archive_dir)) {
        archived_files <- file.path(
          archive_dir,
          sub("^outputs[/\\\\]?", "", files)
        )
        saved_copy <- ifelse(
          file.exists(archived_files),
          normalizePath(archived_files, winslash = "/", mustWork = FALSE),
          "Not archived"
        )
      }

      output_rows(data.frame(
        "Current file" = current_files,
        Description = descriptions,
        Exists = exists_status,
        "Saved copy" = saved_copy,
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    update_output_table()

    # Try to enrich diagnostics from pipeline output files
    pipeline_out <- read_pipeline_outputs()
    dd <- diagnostics_data()
    if (!is.null(dd)) {
      yr_keys <- names(dd$bench)

      # Enrich UFH diagnostics
      ufh_diag  <- list()
      ufh_bench <- list()
      if (!is.null(pipeline_out$ufh)) {
        for (yr_key in yr_keys) {
          yr_val <- as.integer(gsub("^y", "", yr_key))
          enriched <- enrich_diagnostics_from_output(pipeline_out$ufh, yr_val, "UFH")
          ufh_bench[[yr_key]] <- enriched %||% dd$bench[[yr_key]]
          d <- dd$diag[[yr_key]]
          d$model_type <- "UFH"
          d <- enrich_diag_with_shapiro(d, pipeline_out$ufh_shapiro, yr_val, "UFH")
          ufh_diag[[yr_key]] <- d
        }
      }

      # Enrich MFH diagnostics
      mfh_diag  <- list()
      mfh_bench <- list()
      if (!is.null(pipeline_out$mfh)) {
        for (yr_key in yr_keys) {
          yr_val <- as.integer(gsub("^y", "", yr_key))
          enriched <- enrich_diagnostics_from_output(pipeline_out$mfh, yr_val, "MFH")
          mfh_bench[[yr_key]] <- enriched %||% dd$bench[[yr_key]]
          d <- dd$diag[[yr_key]]
          d$model_type <- "MFH"
          d <- enrich_diag_with_shapiro(d, pipeline_out$mfh_shapiro, yr_val, "MFH")
          mfh_diag[[yr_key]] <- d
        }
      }

      ufh_bench <- add_benchmark_metadata(
        ufh_bench,
        source = benchmark_source_label,
        level_label = benchmark_level_label,
        level_variable = benchmark_level_var,
        enabled = isTRUE(input$do_benchmark)
      )
      mfh_bench <- add_benchmark_metadata(
        mfh_bench,
        source = benchmark_source_label,
        level_label = benchmark_level_label,
        level_variable = benchmark_level_var,
        enabled = isTRUE(input$do_benchmark)
      )

      # Store enriched data (keep UFH as primary for backward compat)
      if (length(ufh_bench) > 0) dd$bench <- ufh_bench
      dd$ufh_diag  <- ufh_diag
      dd$ufh_bench <- ufh_bench
      dd$mfh_diag  <- mfh_diag
      dd$mfh_bench <- mfh_bench
      diagnostics_data(dd)

      # Regenerate brief with enriched data (separate UFH/MFH when both available)
      llm_off  <- llm_assistant(enabled = FALSE)
      has_both <- length(ufh_diag) > 0 && length(mfh_diag) > 0
      br <- generate_analysis_brief(
        diagnostics     = dd$diag,
        bench_summary   = dd$bench,
        input_flags     = validation_result(),
        llm             = llm_off,
        language        = get_language(),
        country         = if (nzchar(trimws(input$country_name %||% ""))) trimws(input$country_name) else "Not specified",
        model_type      = if ("UFH" %in% input$steps) "UFH" else "MFH",
        ufh_diagnostics = if (has_both) ufh_diag  else NULL,
        ufh_bench       = if (has_both) ufh_bench else NULL,
        mfh_diagnostics = if (has_both) mfh_diag  else NULL,
        mfh_bench       = if (has_both) mfh_bench else NULL
      )
      brief_result(br)
    }

    # ---- Step 4: Save brief, diagnostics note, and check outputs ----
    dir.create("outputs/data", showWarnings = FALSE, recursive = TRUE)

    br <- brief_result()

    # Save diagnostics note as markdown (separate UFH / MFH sections)
    dd <- diagnostics_data()
    if (!is.null(dd)) {
      diag_lines <- c("# Model Diagnostics Note", "")

      # Helper to format one model's diagnostics block
      format_diag_block <- function(model_label, diag_list, bench_list) {
        bl <- character()
        bl <- c(bl, sprintf("## %s", model_label), "")
        for (yr_name in names(diag_list)) {
          d <- diag_list[[yr_name]]
          bl <- c(bl, sprintf("### Year: %s", d$year %||% yr_name), "")
          bl <- c(bl, sprintf("- **Domains:** %s", d$n_domains %||% "N/A"))
          bl <- c(bl, sprintf("- **Convergence:** %s",
                               if (isTRUE(d$convergence)) "Yes" else "N/A"))
          if (!is.na(d$re_shapiro_pvalue %||% NA)) {
            bl <- c(bl, sprintf("- **RE normality (Shapiro p):** %.4f [%s]",
                                 d$re_shapiro_pvalue,
                                 if (isTRUE(d$re_shapiro_pass)) "PASS" else "FAIL"))
          }
          if (!is.na(d$resid_shapiro_pvalue %||% NA)) {
            bl <- c(bl, sprintf("- **Resid normality (Shapiro p):** %.4f [%s]",
                                 d$resid_shapiro_pvalue,
                                 if (isTRUE(d$resid_shapiro_pass)) "PASS" else "FAIL"))
          }
          bl <- c(bl, "",
            sprintf("*Review Q-Q plots and kernel density plots in the %s HTML report", model_label),
            "to visually confirm normality of random effects and residuals.*", "")

          b <- bench_list[[yr_name]]
          if (!is.null(b)) {
            bl <- c(bl, "#### Benchmark Summary", "")
            bl <- c(bl, benchmark_metadata_lines(b))
            if (!is.null(b$estimate_range))
              bl <- c(bl, sprintf("- **Estimate range:** [%.4f, %.4f]",
                                   b$estimate_range[1], b$estimate_range[2]))
            if (!is.na(b$estimate_median %||% NA))
              bl <- c(bl, sprintf("- **Median estimate:** %.4f", b$estimate_median))
            if (!is.na(b$cv_median %||% NA))
              bl <- c(bl, sprintf("- **Median CV:** %.4f", b$cv_median))
            if (!is.na(b$cv_max %||% NA))
              bl <- c(bl, sprintf("- **Max CV:** %.4f", b$cv_max))
            if (!is.na(b$mse_median %||% NA))
              bl <- c(bl, sprintf("- **Median MSE:** %.6f", b$mse_median))
            if (!is.na(b$n_cv_above_25pct %||% NA))
              bl <- c(bl, sprintf("- **Domains with CV > 25%%:** %d", b$n_cv_above_25pct))
            bl <- c(bl, "")
          }
        }
        bl
      }

      has_both <- !is.null(dd$ufh_diag) && length(dd$ufh_diag) > 0 &&
                  !is.null(dd$mfh_diag) && length(dd$mfh_diag) > 0

      if (has_both) {
        diag_lines <- c(diag_lines,
          format_diag_block("UFH (Univariate Fay-Herriot)", dd$ufh_diag, dd$ufh_bench),
          format_diag_block("MFH (Multivariate Fay-Herriot)", dd$mfh_diag, dd$mfh_bench))

        # Comparison section
        diag_lines <- c(diag_lines, "## UFH vs MFH Comparison", "")
        for (yr_name in names(dd$ufh_bench)) {
          ub <- dd$ufh_bench[[yr_name]]
          mb <- dd$mfh_bench[[yr_name]]
          if (!is.null(ub) && !is.null(mb)) {
            diag_lines <- c(diag_lines, sprintf("### %s", yr_name))
            diag_lines <- c(diag_lines, sprintf("| Metric | UFH | MFH |"))
            diag_lines <- c(diag_lines, "|--------|-----|-----|")
            diag_lines <- c(diag_lines, sprintf("| Median CV | %.4f | %.4f |",
                                                 ub$cv_median %||% NA, mb$cv_median %||% NA))
            diag_lines <- c(diag_lines, sprintf("| Max CV | %.4f | %.4f |",
                                                 ub$cv_max %||% NA, mb$cv_max %||% NA))
            if (!is.na(ub$mse_median %||% NA) && !is.na(mb$mse_median %||% NA))
              diag_lines <- c(diag_lines, sprintf("| Median MSE | %.6f | %.6f |",
                                                   ub$mse_median, mb$mse_median))
            diag_lines <- c(diag_lines, sprintf("| Domains CV>25%% | %s | %s |",
                                                 ub$n_cv_above_25pct %||% "N/A",
                                                 mb$n_cv_above_25pct %||% "N/A"))
            diag_lines <- c(diag_lines, "")
          }
        }
        diag_lines <- c(diag_lines,
          "*The model with lower CV, lower MSE, and better-aligned Q-Q plots is generally preferred.",
          "MFH borrows strength across time periods and may improve estimates for domains",
          "with small samples, but requires the multivariate normality assumption to hold.*",
          "")
      } else {
        # Single model fallback
        for (yr_name in names(dd$diag)) {
          d <- dd$diag[[yr_name]]
          diag_lines <- c(diag_lines, sprintf("## Year: %s", d$year %||% yr_name), "")
          diag_lines <- c(diag_lines, sprintf("- **Model type:** %s", d$model_type %||% "UFH"))
          diag_lines <- c(diag_lines, sprintf("- **Domains:** %s", d$n_domains %||% "N/A"))
          diag_lines <- c(diag_lines, sprintf("- **Convergence:** %s",
                                               if (isTRUE(d$convergence)) "Yes" else "N/A"))
          if (!is.na(d$re_shapiro_pvalue %||% NA))
            diag_lines <- c(diag_lines, sprintf("- **RE normality (Shapiro p):** %.4f [%s]",
                                                 d$re_shapiro_pvalue,
                                                 if (isTRUE(d$re_shapiro_pass)) "PASS" else "FAIL"))
          if (!is.na(d$resid_shapiro_pvalue %||% NA))
            diag_lines <- c(diag_lines, sprintf("- **Resid normality (Shapiro p):** %.4f [%s]",
                                                 d$resid_shapiro_pvalue,
                                                 if (isTRUE(d$resid_shapiro_pass)) "PASS" else "FAIL"))
          diag_lines <- c(diag_lines, "")

          b <- dd$bench[[yr_name]]
          if (!is.null(b)) {
            diag_lines <- c(diag_lines, "### Benchmark Summary", "")
            diag_lines <- c(diag_lines, benchmark_metadata_lines(b))
            if (!is.null(b$estimate_range))
              diag_lines <- c(diag_lines, sprintf("- **Estimate range:** [%.4f, %.4f]",
                                                   b$estimate_range[1], b$estimate_range[2]))
            if (!is.na(b$estimate_median %||% NA))
              diag_lines <- c(diag_lines, sprintf("- **Median estimate:** %.4f", b$estimate_median))
            if (!is.na(b$cv_median %||% NA))
              diag_lines <- c(diag_lines, sprintf("- **Median CV:** %.4f", b$cv_median))
            if (!is.na(b$cv_max %||% NA))
              diag_lines <- c(diag_lines, sprintf("- **Max CV:** %.4f", b$cv_max))
            if (!is.na(b$n_cv_above_25pct %||% NA))
              diag_lines <- c(diag_lines, sprintf("- **Domains with CV > 25%%:** %d", b$n_cv_above_25pct))
            diag_lines <- c(diag_lines, "")
          }
        }
      }

      # diagnostics kept in memory only, not written to disk
    }

    # run_pipeline_from_config() records run metadata before analysis and
    # renders final_report.html exactly once after all requested steps.

    if (ok) {
      archive_dir <- file.path(run_dir, "outputs")
      tryCatch({
        if (dir.exists(archive_dir)) {
          unlink(archive_dir, recursive = TRUE, force = TRUE)
        }
        dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
        output_files <- list.files("outputs", all.files = TRUE, no.. = TRUE,
                                   full.names = TRUE)
        if (length(output_files) > 0) {
          file.copy(output_files, archive_dir, recursive = TRUE,
                    overwrite = TRUE, copy.date = TRUE)
        }
        update_output_table(archive_dir)
        run_location(paste(
          paste("Run folder:", run_dir_abs),
          paste("Archived outputs:", normalizePath(archive_dir, winslash = "/", mustWork = FALSE)),
          "Status: completed successfully",
          sep = "\n"
        ))
        append_log(paste(
          "Archived run outputs:",
          normalizePath(archive_dir, winslash = "/", mustWork = FALSE)
        ))
      }, error = function(e) {
        append_log(paste("WARNING: Could not archive run outputs:", e$message))
      })
      status("Completed successfully")
      progress$set(value = n_steps, detail = "Complete")
      tryCatch({
        save_current_dashboard_setup("successful run")
      }, error = function(e) {
        append_log(paste("WARNING: Could not save reusable dashboard setup:", e$message))
      })
      append_log("Pipeline finished.")
    } else {
      status("Run failed")
      run_location(paste(
        paste("Run folder:", run_dir_abs),
        "Archived outputs: not created because the run failed",
        "Check run.log in the run folder for details.",
        sep = "\n"
      ))
      progress$set(value = n_steps, detail = "Failed")
      pipeline_logger(paste("ERROR:", err_msg))
    }
  })

  # ---- LLM: Interpret Diagnostics ----
  observeEvent(input$interpret_btn, {
    dd  <- diagnostics_data()
    llm <- get_llm()
    if (is.null(dd) || !isTRUE(llm$enabled)) {
      llm_interp("No diagnostics available or LLM not enabled.")
      return()
    }

    llm_interp("Requesting interpretation from Claude...")

    tryCatch({
      lang <- language_label(get_language())

      # Build text for both models when available
      has_both <- !is.null(dd$ufh_diag) && length(dd$ufh_diag) > 0 &&
                  !is.null(dd$mfh_diag) && length(dd$mfh_diag) > 0

      if (has_both) {
        ufh_diag_text  <- paste(capture.output(str(dd$ufh_diag)),  collapse = "\n")
        ufh_bench_text <- paste(capture.output(str(dd$ufh_bench)), collapse = "\n")
        mfh_diag_text  <- paste(capture.output(str(dd$mfh_diag)),  collapse = "\n")
        mfh_bench_text <- paste(capture.output(str(dd$mfh_bench)), collapse = "\n")

        interp_prompt <- paste(
          "Here are diagnostics for BOTH UFH and MFH models in a Small Area Estimation analysis.\n\n",
          "=== UFH (Univariate Fay-Herriot) ===\n",
          "DIAGNOSTICS:\n", ufh_diag_text,
          "\nBENCHMARK SUMMARIES:\n", ufh_bench_text,
          "\n\n=== MFH (Multivariate Fay-Herriot) ===\n",
          "DIAGNOSTICS:\n", mfh_diag_text,
          "\nBENCHMARK SUMMARIES:\n", mfh_bench_text,
          "\n\nPlease provide a structured interpretation:\n",
          "1. UFH Assessment: convergence, normality (discuss Q-Q plots and Shapiro-Wilk),",
          "   precision (CV), accuracy (MSE)\n",
          "2. MFH Assessment: same structure, noting any gains from borrowing strength\n",
          "3. Model Comparison: which approach is better for this data and why\n",
          "4. Domains that may need attention\n",
          "5. Actionable recommendations"
        )
      } else {
        all_diag_text  <- paste(capture.output(str(dd$diag)),  collapse = "\n")
        all_bench_text <- paste(capture.output(str(dd$bench)), collapse = "\n")

        interp_prompt <- paste(
          "Here are the model diagnostics for a Small Area Estimation analysis:\n\n",
          "DIAGNOSTICS:\n", all_diag_text,
          "\n\nBENCHMARK SUMMARIES:\n", all_bench_text,
          "\n\nPlease provide a concise interpretation covering:",
          "1. Whether model assumptions appear satisfied (discuss Q-Q plots and normality)",
          "2. Any diagnostic concerns or red flags",
          "3. Domains that may need attention",
          "4. Actionable recommendations"
        )
      }

      result <- llm$query(
        prompt = interp_prompt,
        system_prompt = paste(
          "You are a statistician helping interpret Small Area Estimation (SAE)",
          "model diagnostics. Be concise but thorough. Focus on actionable insights.",
          "When discussing normality, reference both Shapiro-Wilk test results and",
          "Q-Q plot interpretation (alignment with diagonal, tail behavior, outliers).",
          sprintf("Respond in %s.", lang)
        )
      )

      if (!is.null(result)) {
        llm_interp(result)
      } else {
        llm_interp("LLM request failed. Check your API key and network connection.")
      }
    }, error = function(e) {
      llm_interp(sprintf("Error generating interpretation: %s", e$message))
    })
  })

  # ---- LLM: Evaluate Normality ----
  observeEvent(input$eval_normality_btn, {
    llm <- get_llm()
    if (!isTRUE(llm$enabled)) {
      normality_eval("LLM not enabled. Set API key and enable AI Assistant.")
      return()
    }

    vision_mode <- isTRUE(input$use_vision)
    mode_label <- if (vision_mode) "vision (with plots)" else "text-only"
    normality_eval(paste0("Evaluating normality (", mode_label, " mode)..."))

    tryCatch({
      # Find the emdi fh model objects from pipeline output
      # They are saved as RDS files by the pipeline
      ufh_model_path <- "outputs/data/fh_model_y1.rds"
      ufh_model_path2 <- "outputs/data/fh_model_y2.rds"

      eval_parts <- character()

      for (yr_info in list(
        list(path = ufh_model_path,  label = "UFH Year 1"),
        list(path = ufh_model_path2, label = "UFH Year 2")
      )) {
        if (file.exists(yr_info$path)) {
          fh_mod <- tryCatch(readRDS(yr_info$path), error = function(e) NULL)
          if (!is.null(fh_mod) && inherits(fh_mod, "fh")) {
            detected_provider <- detect_llm_provider(input$api_key)
            result <- evaluate_normality(
              fh_model   = fh_mod,
              api_key    = input$api_key,
              provider   = detected_provider,
              model      = default_llm_model(detected_provider),
              language   = get_language(),
              use_vision = vision_mode
            )
            part <- paste0(
              "=== ", yr_info$label, " ===\n",
              "Standardized Residuals:\n",
              "  Normality holds: ", result$standardized_residuals$normality_holds, "\n",
              "  Shapiro: ", result$standardized_residuals$shapiro_assessment, "\n",
              "  Visual: ", result$standardized_residuals$visual_assessment, "\n",
              if (length(result$standardized_residuals$concerns) > 0)
                paste0("  Concerns: ", paste(result$standardized_residuals$concerns, collapse = "; "), "\n")
              else "",
              "\nRandom Effects:\n",
              "  Normality holds: ", result$random_effects$normality_holds, "\n",
              "  Shapiro: ", result$random_effects$shapiro_assessment, "\n",
              "  Visual: ", result$random_effects$visual_assessment, "\n",
              if (length(result$random_effects$concerns) > 0)
                paste0("  Concerns: ", paste(result$random_effects$concerns, collapse = "; "), "\n")
              else "",
              "\nRecommendation: ", result$overall_recommendation, "\n"
            )
            eval_parts <- c(eval_parts, part)
          }
        }
      }

      if (length(eval_parts) == 0) {
        normality_eval(paste(
          "No saved emdi model objects found. The normality evaluation requires",
          "the fh model objects to be saved as RDS files during pipeline execution.",
          "\n\nTo enable this, the pipeline should save: saveRDS(fh_model, 'outputs/data/fh_model_y1.rds')",
          "\n\nAlternatively, the Shapiro-Wilk results from the CSV exports are",
          "already included in the standard AI interpretation above."
        ))
      } else {
        normality_eval(paste(eval_parts, collapse = "\n\n"))
      }

    }, error = function(e) {
      normality_eval(sprintf("Error evaluating normality: %s", e$message))
    })
  })

  # ---- LLM: Generate Enriched Brief ----
  observeEvent(input$brief_llm_btn, {
    dd  <- diagnostics_data()
    llm <- get_llm()
    vr  <- validation_result()
    if (is.null(dd) || !isTRUE(llm$enabled)) {
      llm_brief("No diagnostics available or LLM not enabled.")
      return()
    }

    llm_brief("Generating AI-enriched brief...")

    tryCatch({
      has_both <- !is.null(dd$ufh_diag) && length(dd$ufh_diag) > 0 &&
                  !is.null(dd$mfh_diag) && length(dd$mfh_diag) > 0
      br <- generate_analysis_brief(
        diagnostics     = dd$diag,
        bench_summary   = dd$bench,
        input_flags     = vr,
        llm             = llm,
        language        = get_language(),
        country         = if (nzchar(trimws(input$country_name %||% ""))) trimws(input$country_name) else "Not specified",
        model_type      = if ("UFH" %in% input$steps) "UFH" else "MFH",
        ufh_diagnostics = if (has_both) dd$ufh_diag  else NULL,
        ufh_bench       = if (has_both) dd$ufh_bench else NULL,
        mfh_diagnostics = if (has_both) dd$mfh_diag  else NULL,
        mfh_bench       = if (has_both) dd$mfh_bench else NULL
      )

      if (!is.null(br$llm_brief)) {
        llm_brief(br$llm_brief)
      } else {
        llm_brief("LLM request failed. Check your API key and network connection.")
      }
    }, error = function(e) {
      llm_brief(sprintf("Error generating brief: %s", e$message))
    })
  })
}

.app <- shinyApp(ui, server)

.run_app_with_port_fallback <- function(app_dir, host = "127.0.0.1",
                                        preferred = 7777L,
                                        max_tries = 100L,
                                        launch.browser = TRUE) {
  env_port <- suppressWarnings(as.integer(Sys.getenv("EU_SAE_APP_PORT", unset = "")))
  candidates <- preferred + seq.int(0L, max_tries - 1L)
  if (!is.na(env_port) && env_port > 0L) {
    candidates <- unique(c(env_port, candidates))
  }

  for (port in candidates) {
    message("Launching Shiny app at http://", host, ":", port, " ...")
    message("Open that URL in your browser. Press Ctrl+C in this terminal to stop.")
    ok <- tryCatch({
      shiny::runApp(
        appDir = app_dir,
        host = host,
        port = port,
        launch.browser = launch.browser
      )
      TRUE
    }, error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("address already in use|Failed to create server|createTcpServer",
                msg, ignore.case = TRUE)) {
        message("Port ", port, " is already in use; trying the next port.")
        FALSE
      } else {
        stop(e)
      }
    })
    if (isTRUE(ok)) return(invisible(TRUE))
  }
  stop("Could not find an available local port for the dashboard.", call. = FALSE)
}

# When run from RStudio's "Run App" button or via shiny::runApp(),
# the app object is auto-launched. When run via `Rscript app.R` from a
# terminal, R is non-interactive and we must launch the server explicitly.
#
# IMPORTANT: we pass appDir (a directory) to shiny::runApp rather than the
# .app object. Passing the shinyApp object directly skips Shiny's automatic
# serving of the www/ folder, which 404s the landing-page choropleth
# (www/eu_poverty_map.png) and any other static assets. Passing appDir
# causes Shiny to re-source this file as part of normal app loading; the
# Sys.getenv guard below prevents that from re-entering this branch and
# causing infinite recursion.
if (!interactive() && !nzchar(Sys.getenv("EU_SAE_APP_LAUNCHED"))) {
  Sys.setenv(EU_SAE_APP_LAUNCHED = "1")
  .run_app_with_port_fallback(
    app_dir = .app_dir,
    host = "127.0.0.1",
    preferred = 7777L,
    launch.browser = TRUE
  )
} else {
  .app
}
