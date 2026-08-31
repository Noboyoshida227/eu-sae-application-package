# ============================================================================
# EU SAE Package4 -- 01_ufh.R
# Univariate Fay-Herriot (UFH) Pipeline
#
# Converted from qmd/40-fh_v2.qmd (computation only, no prose/knitr)
# All paths use here::here() anchored to the project root.
# Figures saved to outputs/figures/, data to outputs/data/,
# tables to outputs/tables/.
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(tidyverse)
  library(car)
  library(sae)
  library(survey)
  library(spdep)
  library(MASS)
  library(caret)
  library(purrr)
  library(gt)
  library(scales)
  library(viridis)
  library(emdi)
  library(rlang)
  library(dplyr)
  library(ggplot2)
  library(writexl)
  library(readxl)
  library(conflicted)
  library(tictoc)
  library(matrixcalc)
  library(knitr)
  library(here)
})

# Anchor all here::here() calls to this package copy, even when it is nested
# inside another Git checkout containing older EU SAE files.
here::i_am("scripts/01_ufh.R")

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag",    "dplyr")
conflicted::conflict_prefer("first",  "dplyr")
conflicted::conflict_prefer("recode", "dplyr")

options(survey.lonely.psu = "adjust")

source(here::here("scripts", "ufh_functions.R"))
source(here::here("scripts", "population_helpers.R"))
source(here::here("R", "input_readers.R"))
source(here::here("R", "input_paths.R"))
source(here::here("R", "pipeline_helpers.R"))
source(here::here("R", "variance_policy.R"))


# ============================================================
# App configuration and data loading
#
# When launched from the Shiny dashboard, all options (data paths,
# variable mapping, years, transformation, IC criterion, regressors)
# are read from the config YAML. When run standalone, sensible
# defaults are used.
# ============================================================

# ---- Helper: return config value or default ----
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
}

cfg_or_default <- function(x, default) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) default else x
}

existing_default_path <- function(...) {
  candidates <- c(...)
  existing <- candidates[file.exists(candidates)]
  path <- if (length(existing) > 0) existing[[1]] else candidates[[1]]
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

resolve_config_path <- function(path, default_path, label) {
  path <- sae_resolve_input_path(cfg_or_default(path, default_path), root = here::here())
  if (!is.null(path) && file.exists(path)) return(path)
  stop(sprintf(
    "%s file does not exist: %s\nUpload the file again or update app_config.yml to a stable path.",
    label, path
  ), call. = FALSE)
}

# ---- Read app config (if available) ----
cfg_path <- Sys.getenv("SAE_APP_CONFIG", unset = "")
ufh_cfg  <- list()
.app_cfg <- list()
if (nzchar(cfg_path) && file.exists(cfg_path)) {
  .app_cfg <- yaml::read_yaml(cfg_path)
  if (!is.null(.app_cfg$ufh)) ufh_cfg <- .app_cfg$ufh
}

# ---- Indicator: poverty (FGT) or mean welfare ----------------------------
# Read indicator-related fields from the top-level config. Default is the
# legacy poverty (FGT) behaviour so existing configs continue to work.
if (file.exists(here::here("R", "indicator_helpers.R"))) source(here::here("R", "indicator_helpers.R"))

indicator_type   <- cfg_or_default(.app_cfg$indicator_type,  "poverty")
# UFH-specific log_transform (preferred). Falls back to the global
# cfg$log_transform for back-compat with older app_config.yml files
# that didn't carry per-model flags.
.ufh_log_transform_raw <- if (!is.null(.app_cfg$ufh$log_transform)) {
  .app_cfg$ufh$log_transform
} else {
  cfg_or_default(.app_cfg$log_transform, FALSE)
}
log_transform    <- isTRUE(.ufh_log_transform_raw) &&
                    identical(indicator_type, "mean_welfare")
.safe_log_positive <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & x > 0
  out[ok] <- log(x[ok])
  out
}
currency_symbol  <- cfg_or_default(.app_cfg$currency_symbol, "EUR")
fgt_alpha        <- as.integer(cfg_or_default(.app_cfg$fgt_alpha, 0L))
povline_type     <- cfg_or_default(.app_cfg$povline_type, "column")
povline_cfg      <- cfg_or_default(.app_cfg$povline_value, "povline")

ind_lab <- indicator_label(indicator_type, fgt_alpha,
                            log_transform = log_transform,
                            currency_symbol = currency_symbol)
# pov_lab kept for backward-compat with downstream chunks that read it
pov_lab <- ind_lab

.safe_cv <- function(se, estimate, eps = 1e-12) {
  ifelse(is.finite(se) & is.finite(estimate) & abs(estimate) > eps,
         se / abs(estimate), NA_real_)
}
.safe_cv_from_mse <- function(mse, estimate, eps = 1e-12) {
  .safe_cv(sqrt(pmax(mse, 0)), estimate, eps = eps)
}

if (identical(indicator_type, "poverty")) {
  cat("Indicator: Poverty --", ind_lab$fgt, "-", ind_lab$short, "\n")
  if (povline_type == "numeric") {
    cat(sae_poverty_line_log_message("numeric", povline_cfg,
                                     cfg_or_default(ufh_cfg$years_keep, c(2012L, 2013L))),
        "\n")
  }
} else {
  cat("Indicator: Mean welfare", if (log_transform) "(log-fit, back-transformed)" else "(identity scale)", "\n")
}

# ---- Data paths (from config or defaults) ----
.default_survey_path <- here::here("Data", "Spain", "survey.rds")
.default_rhs_path <- here::here("Data", "Spain", "auxiliary.rds")
.default_shp_path <- here::here("Data", "Spain", "shapefile.rds")
survey_path <- resolve_config_path(ufh_cfg$survey_path, .default_survey_path, "Survey")
rhs_path <- resolve_config_path(ufh_cfg$rhs_path, .default_rhs_path, "Auxiliary covariates")
shp_path <- resolve_config_path(ufh_cfg$shp_path, .default_shp_path, "Geometry")
population_path <- cfg_or_default(ufh_cfg$population_path, "")
do_benchmark <- isTRUE(ufh_cfg$do_benchmark)
benchmark_level <- cfg_or_default(
  ufh_cfg$benchmark_level,
  .app_cfg$benchmarking$level %||% "national"
)
if (!benchmark_level %in% c("national", "custom", "region")) benchmark_level <- "national"
benchmark_target_path <- cfg_or_default(
  ufh_cfg$benchmark_target_path %||% ufh_cfg$regional_benchmark_path,
  .app_cfg$benchmarking$target_path %||% ""
)
benchmark_level_var_cfg <- trimws(as.character(cfg_or_default(
  ufh_cfg$benchmark_level_variable,
  .app_cfg$benchmarking$level_variable %||% ""
)))
change_alpha <- suppressWarnings(as.numeric(cfg_or_default(
  ufh_cfg$change_alpha,
  .app_cfg$change_alpha %||% 0.05
)))
if (!is.finite(change_alpha) || change_alpha <= 0 || change_alpha >= 1) {
  change_alpha <- 0.05
}

cat("UFH data paths:\n")
cat("  survey:", survey_path, "\n")
cat("  rhs:   ", rhs_path, "\n")
cat("  shp:   ", shp_path, "\n")

survey_raw <- sae_read_table_input(survey_path, "Survey data")  # household-level survey data (incl. region)
rhs_dt_raw <- sae_read_table_input(rhs_path, "Auxiliary covariates")    # domain-level covariates
shp_dt     <- sae_read_geometry_input(shp_path, "Geometry data")    # province geometries
.map_attribution <- sae_boundary_attribution(shp_dt)

# Domain label columns are kept until after canonical domain names are built
# so exports can include geographic names when they are available.

# ---- Variable name mapping (from config or defaults) ----
default_var_map <- list(
  year = "year", domain = "prov", psu = "ea_id",
  weight = "weight", strata = "", hh_size = "hhsize",
  benchmark_level = "", region = "",
  welfare = "income", povline = "povline", poor = "poor"
)
if (!is.null(ufh_cfg$var_map)) {
  var_map <- modifyList(default_var_map, ufh_cfg$var_map)
} else {
  var_map <- default_var_map
}
use_strata <- isTRUE({
  strata_var <- trimws(as.character(var_map$strata %||% ""))
  length(strata_var) == 1 && !is.na(strata_var) && nzchar(strata_var)
})
if (!use_strata) var_map$strata <- ""
mapped_benchmark_level_var <- benchmark_level_var_cfg
if (!nzchar(mapped_benchmark_level_var)) {
  mapped_benchmark_level_var <- trimws(as.character(
    var_map$benchmark_level %||% var_map$region %||% ""
  ))
}
if (identical(benchmark_level, "region") && !nzchar(mapped_benchmark_level_var)) {
  mapped_benchmark_level_var <- "region"
}
if (benchmark_level %in% c("custom", "region")) {
  if (!nzchar(mapped_benchmark_level_var)) {
    benchmark_level <- "national"
  }
  var_map$benchmark_level <- mapped_benchmark_level_var
  var_map$region <- mapped_benchmark_level_var
} else {
  var_map$benchmark_level <- ""
  var_map$region <- ""
}

# ---- Harmonize variable names ----
rename_cols <- c(
  year = var_map$year, domain = var_map$domain, psu = var_map$psu,
  weight = var_map$weight, welfare = var_map$welfare
)
if (!is.null(var_map$strata) && nzchar(var_map$strata) &&
    var_map$strata %in% names(survey_raw) && var_map$strata != "strata") {
  rename_cols <- c(rename_cols, strata = var_map$strata)
}
if (!is.null(var_map$benchmark_level) && nzchar(var_map$benchmark_level) &&
    var_map$benchmark_level %in% names(survey_raw) &&
    var_map$benchmark_level != "region" &&
    !var_map$benchmark_level %in% unname(rename_cols)) {
  rename_cols <- c(rename_cols, region = var_map$benchmark_level)
}
# Only rename povline column when it comes from data and we are running
# the poverty indicator. Mean-welfare runs ignore the poverty line entirely.
if (identical(indicator_type, "poverty") &&
    povline_type == "column" && !is.null(var_map$povline)) {
  rename_cols <- c(rename_cols, povline = var_map$povline)
}
if (!is.null(var_map$hh_size) && nzchar(var_map$hh_size) &&
    var_map$hh_size %in% names(survey_raw) && var_map$hh_size != "hh_size") {
  rename_cols <- c(rename_cols, hh_size = var_map$hh_size)
}
survey_raw <- sae_resolve_rename_collisions(survey_raw, rename_cols, context = "UFH mapping")
survey_all <- survey_raw %>% rename(!!!rename_cols)
if (!use_strata && "strata" %in% names(survey_all)) {
  survey_all <- survey_all %>% select(-any_of("strata"))
}
if (!is.null(var_map$benchmark_level) && nzchar(var_map$benchmark_level) &&
    !"region" %in% names(survey_all) &&
    var_map$benchmark_level %in% unname(rename_cols)) {
  copied_from <- names(rename_cols)[match(var_map$benchmark_level, unname(rename_cols))]
  if (!is.na(copied_from) && copied_from %in% names(survey_all)) {
    survey_all$region <- survey_all[[copied_from]]
  }
}
# When the poverty line is a numeric constant, create the column (poverty only)
if (identical(indicator_type, "poverty") && povline_type == "numeric") {
  survey_all <- sae_apply_numeric_poverty_lines(
    survey_all,
    povline_value = povline_cfg,
    year_col = "year",
    years_keep = as.integer(cfg_or_default(ufh_cfg$years_keep, c(2012L, 2013L))),
    output_col = "povline"
  )
}
# For mean-welfare runs, ensure a placeholder povline column exists so any
# downstream code path that mentions `povline` does not blow up.
if (!identical(indicator_type, "poverty") && !"povline" %in% names(survey_all)) {
  survey_all$povline <- NA_real_
}
survey_all <- sae_add_population_weight(
  survey_all,
  weight_col = "weight",
  hh_size_col = "hh_size",
  output_col = "population_weight",
  context = "UFH direct estimation"
)
cat("UFH direct estimates use population_weight = weight * hh_size.\n")
survey_all <- survey_all %>%
  mutate(domain = trimws(as.character(domain)))
if ("region" %in% names(survey_all)) {
  survey_all <- survey_all %>%
    mutate(region = trimws(as.character(region)))
}

rhs_domain_col <- cfg_or_default(ufh_cfg$rhs_domain, var_map$domain)
shp_domain_col <- cfg_or_default(ufh_cfg$shp_domain, var_map$domain)
rhs_year_col <- if (!is.null(var_map$year) && var_map$year %in% names(rhs_dt_raw)) {
  var_map$year
} else {
  "year"
}

rhs_dt_raw <- rhs_dt_raw %>%
  rename(domain = all_of(rhs_domain_col)) %>%
  { if (rhs_year_col != "year" && rhs_year_col %in% names(.)) rename(., year = all_of(rhs_year_col)) else . } %>%
  mutate(domain = trimws(as.character(domain)))
shp_dt     <- shp_dt %>%
  rename(domain = all_of(shp_domain_col)) %>%
  mutate(domain = trimws(as.character(domain)))

domain_metadata <- sae_build_domain_metadata(shp_dt, rhs_dt_raw, survey_all)
rhs_dt_raw <- sae_drop_domain_label_columns(rhs_dt_raw)
shp_dt <- sae_drop_domain_label_columns(shp_dt)
# rename/mutate on sf may discard custom attributes; restore the selected credit.
attr(shp_dt, "boundary_attribution") <- .map_attribution
survey_all <- sae_drop_domain_label_columns(survey_all)

# ---- Analysis years (from config or defaults) ----
years_keep <- as.integer(cfg_or_default(ufh_cfg$years_keep, c(2012L, 2013L)))
cat("Analysis years:", paste(years_keep, collapse = ", "), "\n")
missing_survey_years <- setdiff(years_keep, sort(unique(as.integer(survey_all$year))))
missing_rhs_years <- setdiff(years_keep, sort(unique(as.integer(rhs_dt_raw$year))))
if (length(missing_survey_years) > 0 || length(missing_rhs_years) > 0) {
  stop(
    "Requested analysis years are not available. ",
    "Requested: ", paste(years_keep, collapse = ", "),
    ". Missing in survey: ",
    if (length(missing_survey_years) > 0) paste(missing_survey_years, collapse = ", ") else "none",
    ". Missing in auxiliary covariates: ",
    if (length(missing_rhs_years) > 0) paste(missing_rhs_years, collapse = ", ") else "none",
    "."
  )
}

# ---- Transformation and bias correction (from config or defaults) ----
ufh_transformation <- cfg_or_default(ufh_cfg$transformation, "arcsin")
# Arcsin is only valid for variables in [0, 1].
if (!identical(indicator_type, "poverty") && identical(ufh_transformation, "arcsin")) {
  cat("NOTE: Arcsin transformation disabled -- only valid on [0, 1].\n")
  ufh_transformation <- "no"
}
if (fgt_alpha > 0L && identical(ufh_transformation, "arcsin")) {
  cat("NOTE: Arcsin transformation disabled -- only valid for FGT(0).\n")
  ufh_transformation <- "no"
}
# Bias correction: user-facing logical option (TRUE = bias-corrected, FALSE = naive)
# Map to emdi's backtransformation argument: TRUE -> "bc", FALSE -> "naive".
# Bias correction is ONLY meaningful under arcsin -- when transformation = "no",
# there is nothing to back-transform, so we pass NULL to emdi::fh().
ufh_bias_correction <- as.logical(cfg_or_default(ufh_cfg$bias_correction, TRUE))
# Backward compatibility: support old 'backtransformation' key
if (is.null(ufh_cfg$bias_correction) && !is.null(ufh_cfg$backtransformation)) {
  ufh_bias_correction <- (ufh_cfg$backtransformation == "bc")
  message("Note: config key 'backtransformation' is deprecated. Use 'bias_correction: true/false' instead.")
}
if (identical(ufh_transformation, "arcsin")) {
  ufh_backtrans_method <- if (ufh_bias_correction) "bc" else "naive"
} else {
  # No transformation -> no back-transform needed. NULL tells emdi::fh() /
  # step_wrapper_fh() to skip the back-transform step entirely.
  ufh_backtrans_method <- NULL
}
cat("Transformation:", ufh_transformation, "\n")
if (isTRUE(log_transform)) {
  cat("Bias correction:", if (isTRUE(ufh_bias_correction)) {
    "TRUE (Duan smearing, bc_sm)"
  } else {
    "FALSE (naive exponential back-transform)"
  }, "\n")
} else if (identical(ufh_transformation, "arcsin")) {
  cat("Bias correction:", ufh_bias_correction, "\n")
} else {
  cat("Bias correction: (not applicable; no transformation selected)\n")
}

# ---- IC criterion (from config or default) ----
ufh_ic_criterion <- if (!is.null(ufh_cfg$ic_criterion) &&
                        ufh_cfg$ic_criterion %in% c("AIC", "BIC")) {
  ufh_cfg$ic_criterion
} else {
  "BIC"
}
cat("Model-selection criterion:", ufh_ic_criterion, "\n")
ufh_lasso_enabled <- isTRUE(ufh_cfg$lasso_enabled)
ufh_lasso_lambda <- cfg_or_default(ufh_cfg$lasso_lambda, "lambda.1se")
analysis_seed <- suppressWarnings(as.integer(cfg_or_default(.app_cfg$analysis_seed, 123L)))
if (!is.finite(analysis_seed) || analysis_seed < 0L) analysis_seed <- 123L
if (!ufh_lasso_lambda %in% c("lambda.1se", "lambda.min")) {
  warning("Unknown UFH lasso_lambda '", ufh_lasso_lambda,
          "' - falling back to 'lambda.1se'.")
  ufh_lasso_lambda <- "lambda.1se"
}
cat("LASSO screening (UFH):",
    if (ufh_lasso_enabled) paste("enabled,", ufh_lasso_lambda) else "disabled",
    "\n")

# ---- Variance smoothing choice (only meaningful when arcsin is NOT used) ----
# Options mirror the MFH menu:
#   "sm_out"  - smooth only non-finite or below-0.001 variances (default)
#   "sm_all"  - replace ALL sampling variances with smoothed values
#   "direct"  - use raw direct variances as-is (NA/0 still backfilled)
# When arcsin is selected the qmd keeps the legacy behavior (backfill NA/0
# only) regardless of this setting; arcsin already stabilizes variances.
ufh_var_choice <- cfg_or_default(ufh_cfg$var_choice, "sm_out")
if (!ufh_var_choice %in% c("direct", "sm_out", "sm_all")) {
  warning("Unknown ufh_cfg$var_choice '", ufh_var_choice,
          "' - falling back to 'sm_out'.")
  ufh_var_choice <- "sm_out"
}
if (identical(ufh_transformation, "arcsin")) {
  cat("Variance smoothing option: (ignored under arcsin transformation)\n")
} else {
  cat("Variance smoothing option:", ufh_var_choice, "\n")
}

# ---- User-specified covariate pools per year ----
# Support both legacy single-list (ufh_cfg$regressors / ufh_cfg$candidate_vars)
# and new per-year keys (ufh_cfg$candidate_vars_y1 / candidate_vars_y2).
# When LASSO is enabled, these lists are candidate pools for LASSO + stepwise
# selection.  When LASSO is disabled, they are treated as fixed formulas.
ufh_regressors_y1 <- NULL
ufh_regressors_y2 <- NULL

if (!is.null(ufh_cfg$candidate_vars_y1) && length(ufh_cfg$candidate_vars_y1) > 0) {
  ufh_regressors_y1 <- as.character(unlist(ufh_cfg$candidate_vars_y1))
  cat("User-specified regressors for year 1:", paste(ufh_regressors_y1, collapse = ", "), "\n")
}
if (!is.null(ufh_cfg$candidate_vars_y2) && length(ufh_cfg$candidate_vars_y2) > 0) {
  ufh_regressors_y2 <- as.character(unlist(ufh_cfg$candidate_vars_y2))
  cat("User-specified regressors for year 2:", paste(ufh_regressors_y2, collapse = ", "), "\n")
}

# Legacy fallback: single regressors/candidate_vars list applied to both years
if (is.null(ufh_regressors_y1) && is.null(ufh_regressors_y2)) {
  legacy <- ufh_cfg$regressors %||% ufh_cfg$candidate_vars
  if (!is.null(legacy) && length(legacy) > 0) {
    legacy <- as.character(unlist(legacy))
    ufh_regressors_y1 <- legacy
    ufh_regressors_y2 <- legacy
    cat("User-specified regressors (both years):", paste(legacy, collapse = ", "), "\n")
  }
}

# Validate user-specified regressors against actual RHS columns
available_covs <- setdiff(names(rhs_dt_raw), c("domain", "year",
                          var_map$domain, var_map$year))

validate_regressors <- function(vars, label) {
  if (is.null(vars) || length(vars) == 0) return(NULL)
  found   <- vars[vars %in% available_covs]
  missing <- vars[!vars %in% available_covs]
  if (length(missing) > 0) {
    warning(sprintf("UFH %s: dropping unknown covariates: %s",
                    label, paste(missing, collapse = ", ")))
  }
  if (length(found) == 0) return(NULL)
  found
}

ufh_regressors_y1 <- validate_regressors(ufh_regressors_y1, "Year 1")
ufh_regressors_y2 <- validate_regressors(ufh_regressors_y2, "Year 2")

# Build per-year formula overrides only when LASSO is disabled.  With LASSO
# enabled the user-selected variables remain the candidate pool.
ufh_formula_override_y1 <- NULL
if (!isTRUE(ufh_lasso_enabled) &&
    !is.null(ufh_regressors_y1) && length(ufh_regressors_y1) > 0) {
  ufh_formula_override_y1 <- as.formula(
    paste("direct_povrate ~", paste(ufh_regressors_y1, collapse = " + "))
  )
  cat("Year 1 fixed formula:", deparse(ufh_formula_override_y1), "\n")
}
ufh_formula_override_y2 <- NULL
if (!isTRUE(ufh_lasso_enabled) &&
    !is.null(ufh_regressors_y2) && length(ufh_regressors_y2) > 0) {
  ufh_formula_override_y2 <- as.formula(
    paste("direct_povrate ~", paste(ufh_regressors_y2, collapse = " + "))
  )
  cat("Year 2 fixed formula:", deparse(ufh_formula_override_y2), "\n")
}

# ---- Container for per-year results ----
fh_results_list <- list()

# ---- EUR-scale plot helpers for mean-welfare log-fit runs --------------
# emdi's compare_plot() and map_plot() pull values straight off the
# fitted model object, which for mean_welfare + log_transform stays on
# the log scale even though the exported pov_fh data frame is in EUR.
# To keep the rendered HTML on the same scale as the export, we use
# these custom ggplot equivalents when log_transform is TRUE. For
# poverty / non-log runs the original emdi helpers are used unchanged.
.compare_plot_eur <- function(pov_fh_year, year_label) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(invisible(NULL))
  tidyr_ns <- asNamespace("tidyr")
  long <- tidyr_ns$pivot_longer(
    pov_fh_year |> dplyr::select(domain, Direct, FH, FH_Bench),
    cols = c(Direct, FH, FH_Bench),
    names_to = "method", values_to = "value"
  )
  ggplot2::ggplot(long,
                  ggplot2::aes(x = reorder(as.factor(domain), value),
                                y = value, color = method, group = method)) +
    ggplot2::geom_point(size = 2, alpha = 0.85) +
    ggplot2::geom_line(alpha = 0.4) +
    ggplot2::scale_color_manual(values = c(Direct = "black",
                                            FH    = "#1f77b4",
                                            FH_Bench = "#d62728")) +
    ggplot2::labs(
      title = paste0("Direct vs FH vs FH-Benchmarked -- Year ", year_label),
      subtitle = "Back-transformed to EUR (mean welfare, log-fit)",
      x = "Domain (sorted by value)",
      y = "Estimate (EUR)",
      color = "Method"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         hjust = 1, size = 7))
}

.map_plot_eur <- function(pov_fh_year, shp, year_label, value_col = "FH_Bench") {
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)) return(invisible(NULL))
  shp_join <- shp |>
    dplyr::left_join(
      pov_fh_year |> dplyr::select(domain, dplyr::all_of(value_col)) |>
        dplyr::rename(.value = dplyr::all_of(value_col)),
      by = "domain"
    )
  ggplot2::ggplot(shp_join) +
    ggplot2::geom_sf(ggplot2::aes(fill = .value), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
    ggplot2::labs(caption = sae_map_caption(.map_attribution), 
      title = paste0(value_col, " -- Year ", year_label),
      subtitle = "Back-transformed to EUR (mean welfare, log-fit)",
      fill = "EUR"
    ) +
    ggplot2::theme_void(base_size = 12)
}

.use_eur_plots <- identical(indicator_type, "mean_welfare") && isTRUE(log_transform)

make_sae_design <- function(data) {
  if (isTRUE(use_strata) && "strata" %in% names(data) && any(!is.na(data$strata))) {
    data$strata <- sae_strata_codes(data$strata, context = "UFH survey design")
    survey::svydesign(
      ids = ~psu,
      strata = ~strata,
      weights = ~population_weight,
      data = data,
      nest = TRUE
    )
  } else {
    survey::svydesign(ids = ~psu, weights = ~population_weight, data = data)
  }
}

.read_optional_benchmark_table <- function(path) {
  if (is.null(path) || length(path) == 0 || !nzchar(path)) return(NULL)
  if (!file.exists(path)) stop("Benchmark Target Database not found: ", path)
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") {
    readRDS(path)
  } else if (ext %in% c("rda", "rdata")) {
    load_env <- new.env(parent = emptyenv())
    loaded <- load(path, envir = load_env)
    if (length(loaded) != 1) {
      stop("Benchmark Target Database .RData/.rda file must contain exactly one object.")
    }
    load_env[[loaded[1]]]
  } else if (ext %in% c("csv", "txt")) {
    read.csv(path, check.names = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read Excel benchmark target inputs.")
    }
    readxl::read_excel(path)
  } else {
    stop("Unsupported Benchmark Target Database format: ", path)
  }
}

.pick_col <- function(nms, candidates) {
  candidates <- candidates[nzchar(candidates)]
  lower_nms <- tolower(nms)
  hit <- match(tolower(candidates), lower_nms)
  hit <- hit[!is.na(hit)]
  if (length(hit) == 0) NULL else nms[hit[1]]
}

.targets_from_optional_benchmark <- function(path, group_vec, years_keep,
                                             level_col = "") {
  obj <- .read_optional_benchmark_table(path)
  if (is.null(obj)) return(NULL)

  group_ids <- sort(unique(as.character(group_vec[!is.na(group_vec)])))
  year_chr <- as.character(years_keep)

  if (is.matrix(obj)) {
    mat <- obj
    if (is.null(rownames(mat))) stop("Benchmark target matrix must have group row names.")
    missing_groups <- setdiff(group_ids, rownames(mat))
    if (length(missing_groups) > 0) {
      stop("Benchmark target matrix is missing group(s): ",
           paste(missing_groups, collapse = ", "))
    }
    mat <- mat[group_ids, , drop = FALSE]
    if (ncol(mat) != length(years_keep)) {
      stop("Benchmark target matrix must have one column per analysis year.")
    }
    storage.mode(mat) <- "double"
    colnames(mat) <- year_chr
    if (any(!is.finite(mat))) stop("Benchmark target matrix contains missing/non-finite targets.")
    return(mat)
  }

  df <- as.data.frame(obj, check.names = FALSE)
  nms <- names(df)
  group_col <- .pick_col(nms, c(level_col, "benchmark_level", "level",
                                "group", "group_id", "region", "reg", "region_id"))
  year_col <- .pick_col(nms, c("year", "time", "period"))
  value_col <- .pick_col(nms, c("benchmark", "target", "B_r",
                                "benchmark_target", "regional_benchmark",
                                "direct", "direct_rate", "poverty_rate"))
  if (is.null(group_col)) {
    if (length(group_ids) == 1L) {
      group_col <- ".benchmark_level"
      df[[group_col]] <- group_ids[1]
    } else {
      stop("Benchmark Target Database must contain a benchmark-level column ",
           "(for example '",
           if (nzchar(level_col)) level_col else "benchmark_level",
           "') or matrix row names.")
    }
  }

  mat <- matrix(NA_real_, nrow = length(group_ids), ncol = length(years_keep),
                dimnames = list(group_ids, year_chr))

  if (!is.null(year_col) && !is.null(value_col)) {
    for (tt in seq_along(years_keep)) {
      idx <- as.character(df[[year_col]]) == year_chr[tt]
      ids <- as.character(df[[group_col]][idx])
      vals <- as.numeric(df[[value_col]][idx])
      mat[intersect(group_ids, ids), tt] <- vals[match(intersect(group_ids, ids), ids)]
    }
  } else {
    for (tt in seq_along(years_keep)) {
      candidates <- c(
        year_chr[tt],
        paste0("B_r_", year_chr[tt]),
        paste0("benchmark_", year_chr[tt]),
        paste0("target_", year_chr[tt]),
        paste0("direct_", year_chr[tt]),
        paste0("poor_", year_chr[tt])
      )
      col <- .pick_col(nms, candidates)
      if (is.null(col)) stop("Benchmark Target Database is missing a column for year ", year_chr[tt], ".")
      ids <- as.character(df[[group_col]])
      mat[group_ids, tt] <- as.numeric(df[[col]][match(group_ids, ids)])
    }
  }

  if (any(!is.finite(mat))) {
    stop("Benchmark Target Database is missing targets for at least one benchmark level/year.")
  }
  mat
}

# ---- Benchmark grouping map ----------------------------------------------
if (do_benchmark) {
  if (identical(benchmark_level, "national")) {
    region_map <- survey_all |>
      select(domain) |>
      distinct() |>
      mutate(region = "national")
  } else if ("region" %in% names(survey_all)) {
    region_map <- survey_all |>
      select(domain, region) |>
      distinct()
  } else {
    stop("Grouped benchmarking requires mapped benchmark-level column '",
         var_map$benchmark_level,
         "' in the survey data. Leave the benchmark-level mapping blank for national benchmarking.")
  }
  bench_desc <- if (identical(benchmark_level, "national")) {
    "national"
  } else {
    paste0("grouped by ", var_map$benchmark_level)
  }
  cat("UFH benchmarking enabled at level:", bench_desc, "\n")
} else {
  region_map <- NULL
  cat("UFH benchmarking disabled by configuration.\n")
  if (nzchar(population_path %||% "")) {
    cat("Population file was provided but UFH benchmarking is disabled; population file is not used in this step.\n")
  }
}

# ---- Invalidate stale UFH output artifacts -------------------------------
# If a previous UFH render failed mid-way, its output files from an even
# EARLIER successful render will still be in output/UFH/ and will be picked
# up by the Comparison step -- making Comparison look like UFH "didn't
# change" even when the config did.
#
# To prevent silent staleness, delete the output artifacts at the start of
# every UFH render. If this render succeeds, they get re-written fresh;
# if it fails, they stay missing and the downstream Comparison render
# fails loudly with a clear "file not found" rather than silently using
# yesterday's numbers.
.ufh_outputs_to_clean <- c(
  here::here("outputs", "data", "pov_fh.xlsx"),
  here::here("outputs", "data", "fh_model_y1.rds"),
  here::here("outputs", "data", "fh_model_y2.rds"),
  here::here("outputs", "tables", "statistical_significance_results.csv"),
  here::here("outputs", "tables", "statistical_significance_results_unbench.csv"),
  here::here("outputs", "tables", "ufh_shapiro_results.csv"),
  here::here("outputs", "tables", "ufh_selection_diagnostics.csv"),
  here::here("outputs", "tables", "ufh_model_diagnostics.csv")
)
for (.f in .ufh_outputs_to_clean) {
  if (file.exists(.f)) {
    tryCatch(file.remove(.f),
             warning = function(w) NULL,
             error   = function(e) NULL)
  }
}
cat("Cleaned stale UFH output artifacts before render.\n")


# ============================================================
# Helper function: run the full FH pipeline for a single year
#
# This function encapsulates Steps 1-8 so that each year can be
# executed in its own chunk while avoiding code duplication.
# It returns a list with the final results data frame and the
# benchmarked model object.
# ============================================================

run_fh_year <- function(yr, survey_all, rhs_dt_raw, shp_dt,
                        years_keep, region_map,
                        transformation = "arcsin",
                        backtransformation = "bc",
                        formula_override = NULL,
                        candidate_vars_override = NULL,
                        var_choice = "sm_out",
                        do_benchmark = FALSE,
                        population_path = "",
                        benchmark_target_path = "",
                        benchmark_level_variable = "",
                        lasso_enabled = FALSE,
                        lasso_lambda = "lambda.1se") {

  # Back-transformation is only meaningful under arcsin. If the caller passes
  # "bc"/"naive" together with transformation = "no", silently drop it so we
  # pass NULL to emdi::fh() and step_wrapper_fh() -- matches the UI semantics.
  if (!identical(transformation, "arcsin")) {
    backtransformation <- NULL
  }

  # ---- Filter to single year ----
  survey_dt <- survey_all |> filter(year == yr)
  rhs_dt    <- rhs_dt_raw |> filter(year == yr)

  # ---- Candidate auxiliary variables ----
  candidate_vars <- colnames(rhs_dt)[!colnames(rhs_dt) %in% c("domain", "year")]
  candidate_vars <- sae_numeric_candidates(
    rhs_dt, candidate_vars, context = paste0("UFH year ", yr))
  if (!is.null(candidate_vars_override) && length(candidate_vars_override) > 0L) {
    candidate_vars <- intersect(as.character(candidate_vars_override), candidate_vars)
    cat("  [year ", yr, "] user-selected LASSO candidate pool: ",
        paste(candidate_vars, collapse = ", "), "\n", sep = "")
  }

  # ---- Sample sizes ----
  sampsize_dt <- survey_dt |>
    group_by(domain) |>
    summarize(N = n())

  # ---- Build the LHS the model is fitted on ----
  # For "poverty"      -- pov_indicator is FGT(Î±) computed from welfare+povline
  # For "mean_welfare" -- pov_indicator is welfare itself (or log(welfare))
  # The downstream `direct_povrate` column is named for back-compat: it
  # holds whichever direct estimate corresponds to the chosen indicator.
  if (identical(indicator_type, "poverty")) {
    survey_dt <- survey_dt |>
      mutate(pov_indicator = compute_fgt(welfare, povline, fgt_alpha))
  } else {
    survey_dt <- survey_dt |>
      mutate(pov_indicator = if (isTRUE(log_transform)) {
        .safe_log_positive(welfare)
      } else {
        as.numeric(welfare)
      })
  }

  # ---- Direct estimation ----
  # Direct poverty/indicator estimates are person-weighted by expanding each
  # sampled household by household size: population_weight = weight * hh_size.
  design_obj <- make_sae_design(survey_dt)
  var_dt <- survey::svyby(~pov_indicator, by = ~domain, design = design_obj,
                          FUN = survey::svymean, na.rm = TRUE)

  direct_dt <- var_dt |>
    rename(direct_povrate = "pov_indicator", SD = "se") |>
    mutate(vardir = SD^2,
           CV = .safe_cv(SD, direct_povrate)) |>
    merge(sampsize_dt, by = "domain")

  # When fitting on the log scale we additionally compute the population-
  # weighted arithmetic mean of welfare per domain. This is what the UI
  # promises ("Mean welfare = population-weighted mean of welfare") and is
  # used to anchor the per-domain smearing in the back-transform: with
  # smear_d = direct_arith_d / exp(direct_povrate_d), the back-transformed
  # Direct column equals svymean(welfare) exactly, and FH/FH_Bench EBLUPs
  # are scaled by the same per-domain factor (standard assumption: within-
  # domain variability of welfare is similar between direct and model).
  if (identical(indicator_type, "mean_welfare") && isTRUE(log_transform)) {
    var_dt_arith <- survey::svyby(~welfare, by = ~domain, design = design_obj,
                                   FUN = survey::svymean, na.rm = TRUE)
    arith_dt <- var_dt_arith |>
      dplyr::rename(direct_arith = "welfare", SD_arith = "se") |>
      dplyr::select(domain, direct_arith, SD_arith)
    direct_dt <- direct_dt |> dplyr::left_join(arith_dt, by = "domain")
  }

  # Design effect:
  #   FGT(0): Bernoulli SRS variance p(1-p)/N
  #   FGT(1)/FGT(2) and mean welfare: sample variance / N
  if (identical(indicator_type, "poverty") && fgt_alpha == 0L) {
    direct_dt <- direct_dt |>
      mutate(var_SRS = direct_povrate * (1 - direct_povrate) / N)
  } else {
    domain_svar <- survey_dt |>
      group_by(domain) |>
      summarize(var_SRS = var(pov_indicator, na.rm = TRUE) / n(), .groups = "drop")
    direct_dt <- direct_dt |>
      left_join(domain_svar, by = "domain") |>
      dplyr::mutate(var_SRS = as.numeric(var_SRS))
  }

  .ess <- sae_effective_sample_size(
    sample_size = direct_dt$N,
    design_variance = direct_dt$vardir,
    srs_variance = direct_dt$var_SRS
  )
  direct_dt$deff <- .ess$deff
  direct_dt$n_eff <- .ess$n_eff
  direct_dt$effective_sample_size_fallback <-
    .ess$effective_sample_size_fallback
  if (any(.ess$effective_sample_size_fallback)) {
    .fallback_domains <- as.character(
      direct_dt$domain[.ess$effective_sample_size_fallback]
    )
    cat(sprintf(
      paste0(
        "  [year %s] arcsine effective sample size used observed N for ",
        "%d boundary/degenerate domain(s): %s.\n"
      ),
      yr, length(.fallback_domains), paste(.fallback_domains, collapse = ", ")
    ))
  }

  # ---- Variance smoothing ----
  #   Three strategies, selected by `var_choice`:
  #     "direct"  - keep raw direct variances (still backfill NA/0 for safety).
  #     "sm_out"  - replace missing/non-finite or < 0.001 direct variances.
  #     "sm_all"  - replace ALL variances with smoothed values.
  #   When transformation == "arcsin", variance smoothing is upstream of the
  #   arcsin/BC pipeline anyway, so the safe default ("sm_out") is used.
  # Restrict filtering to fields required for estimation. Optional labels or
  # diagnostics must not silently remove an otherwise estimable domain.
  required_complete <- intersect(c("domain", "direct_povrate", "vardir", "N"),
                                 names(direct_dt))
  keep_complete <- stats::complete.cases(direct_dt[, required_complete, drop = FALSE])
  dropped_domains <- unique(as.character(direct_dt$domain[!keep_complete]))
  if (length(dropped_domains) > 0L) {
    cat(sprintf("  [year %s] excluded %d domain(s) missing required estimation fields: %s\n",
                yr, length(dropped_domains), paste(dropped_domains, collapse = ", ")))
  }
  direct_dt <- direct_dt[keep_complete, ]

  var_smooth <- varsmoothie_king(domain     = direct_dt[["domain"]],
                                 direct_var = direct_dt$vardir,
                                 sampsize   = direct_dt$N)

  direct_dt <- var_smooth |>
    merge(direct_dt, by.x = "Domain", by.y = "domain")

  # When arcsin transformation is active, variance smoothing is not
  # user-configurable: arcsin already stabilizes variances. We preserve the
  # legacy behavior, which only backfills NA / exactly-zero variances so that
  # emdi::fh() does not error out.
  if (identical(transformation, "arcsin")) {
    direct_dt <- direct_dt |>
      dplyr::mutate(
        vardir_new = if_else(is.na(vardir) | vardir == 0, var_smooth, vardir),
        vardir     = vardir_new,
        SD         = sqrt(vardir_new),
        CV         = .safe_cv(SD, direct_povrate)
      ) |>
      dplyr::select(-vardir_new)
    if (any(!is.finite(direct_dt$n_eff) | direct_dt$n_eff <= 0)) {
      stop(sprintf(
        "Year %s has non-finite or non-positive effective sample sizes after boundary handling.",
        yr
      ), call. = FALSE)
    }
    cat("  [year ", yr, "] arcsin transformation: variance smoothing ",
        "applied only to NA / zero variances.\n", sep = "")
  } else {
    direct_dt <- switch(
      var_choice,
      "direct" = direct_dt |>
        dplyr::mutate(
          # Still backfill NA / zero variances to avoid emdi::fh() errors.
          vardir_new = if_else(is.na(vardir) | vardir == 0, var_smooth, vardir),
          vardir     = vardir_new,
          SD         = sqrt(vardir_new),
          CV         = .safe_cv(SD, direct_povrate)
        ) |>
        dplyr::select(-vardir_new),
      "sm_out" = direct_dt |>
        dplyr::mutate(
          vardir_new = sae_sm_out_variance(vardir, var_smooth),
          vardir     = vardir_new,
          SD         = sqrt(vardir_new),
          CV         = .safe_cv(SD, direct_povrate)
        ) |>
        dplyr::select(-vardir_new),
      "sm_all" = direct_dt |>
        dplyr::mutate(
          vardir = var_smooth,
          SD     = sqrt(var_smooth),
          CV     = .safe_cv(SD, direct_povrate)
        ),
      stop("Unknown var_choice '", var_choice,
           "' in run_fh_year (expected 'direct', 'sm_out', or 'sm_all').")
    )
    cat("  [year ", yr, "] variance smoothing option applied: ",
        var_choice, "\n", sep = "")
  }

  # ---- Combine direct estimates with auxiliary data ----
  fh_dt <- merge(direct_dt, rhs_dt,
                 by.x = "Domain", by.y = "domain", all = TRUE)

  # ---- Model selection ----
  selection_diag <- NULL
  if (is.null(formula_override)) {
    lasso <- sae_lasso_screen(
      dt = fh_dt,
      xvars = candidate_vars,
      y = "direct_povrate",
      enabled = lasso_enabled,
      lambda_choice = lasso_lambda,
      label = paste("UFH", yr),
      seed = analysis_seed + as.integer(yr)
    )
    selection_diag <- lasso$diagnostics
    candidate_vars_for_stepwise <- lasso$vars
    cat(sprintf("UFH %s LASSO screen: %s\n", yr,
                selection_diag$note[[1]]))
    if (nzchar(selection_diag$selected_variables[[1]])) {
      cat("UFH", yr, "LASSO-selected variables:",
          selection_diag$selected_variables[[1]], "\n")
    }
    if (length(candidate_vars_for_stepwise) == 0L) {
      fh_formula <- direct_povrate ~ 1
      cat("No safe covariates remain for stepwise selection; using intercept-only UFH formula.\n")
    } else {
      fh_step <- step_wrapper_fh(dt = fh_dt,
                                 xvars = candidate_vars_for_stepwise,
                                 y = "direct_povrate",
                                 cor_thresh = 0.7,
                                 criteria = ufh_ic_criterion,
                                 vardir = "vardir",
                                 transformation = transformation,
                                 backtransformation = backtransformation,
                                 eff_smpsize = "n_eff")
      fh_formula <- formula(fh_step$fixed)
      cat("Auto-selected model formula:\n")
    }
  } else {
    fh_formula <- formula_override
    selection_diag <- data.frame(
      model = paste("UFH", yr),
      outcome = "direct_povrate",
      lasso_enabled = FALSE,
      lambda_choice = lasso_lambda,
      n_candidates = length(attr(terms(fh_formula), "term.labels")),
      n_numeric_candidates = NA_integer_,
      n_complete_cases = NA_integer_,
      n_selected = length(attr(terms(fh_formula), "term.labels")),
      selected_variables = paste(attr(terms(fh_formula), "term.labels"), collapse = ", "),
      fallback_used = FALSE,
      note = "User-specified covariates supplied; fixed formula used and automated selection skipped.",
      stringsAsFactors = FALSE
    )
    cat("Using override formula:\n")
  }
  print(fh_formula)

  # ---- Model estimation ----
  # emdi::fh() requires mse_type = "analytical" when transformation = "no"
  # (with method in {reml, ml} and no correlation structure). Bootstrap MSE
  # is only valid under the arcsin transformation.
  #
  # IMPORTANT: Use bquote() to inline LITERAL values of mse_type / B into the
  # stored fh() call. bench_regional()'s bootstrap later invokes update() on
  # this model, and update() re-evaluates the original call in its own scope.
  # If we passed local variable names (e.g. .mse_type_fh) into fh(), those
  # names would be unresolvable from bench_regional's frame and every
  # bootstrap iteration would silently fail, producing NA MSEs for FH_Bench.
  .mse_type_fh <- if (identical(transformation, "arcsin")) "boot" else "analytical"
  .B_fh        <- if (identical(transformation, "arcsin")) c(200, 0) else c(0, 0)

  set.seed(123)
  fh_model <- eval(bquote(
    fh(fixed              = fh_formula,
       vardir             = "vardir",
       combined_data      = fh_dt,
       domains            = "Domain",
       method             = "reml",
       transformation     = transformation,
       backtransformation = backtransformation,
       eff_smpsize        = "n_eff",
       MSE                = TRUE,
       mse_type           = .(.mse_type_fh),
       B                  = .(.B_fh))
  ))

  fh_bench <- NULL
  pop_dt <- NULL
  fh_dt <- fh_dt |> rename(domain = Domain)
  if (do_benchmark) {
    # ---- Regional/national benchmarking ----
    # Each benchmark group's target = population-weighted average of direct
    # domain rates. A per-group ratio factor is applied so that the weighted
    # mean of benchmarked FH estimates equals that target.
    population_mat <- sae_resolve_population_matrix(
      population_path = population_path,
      survey_data = survey_all,
      domain_vec = fh_dt$domain,
      years_keep = years_keep,
      hh_size_col = "hh_size",
      context = paste0("UFH year ", yr)
    )
    pop_dt <- tibble::tibble(
      domain = fh_dt$domain,
      Nd = as.numeric(population_mat[as.character(fh_dt$domain), as.character(yr)])
    )

    fh_dt <- fh_dt |>
      left_join(pop_dt |> mutate(ratio_n = Nd / sum(Nd)), by = "domain") |>
      left_join(region_map, by = "domain")

    external_benchmark_mat <- .targets_from_optional_benchmark(
      benchmark_target_path,
      group_vec = region_map$region,
      years_keep = years_keep,
      level_col = benchmark_level_variable
    )

    # Benchmark-level targets: uploaded targets if provided; otherwise the
    # population-weighted average of direct domain rates.
    direct_region_benchmarks <- fh_dt |>
      filter(!is.na(direct_povrate)) |>
      group_by(region) |>
      summarize(
        n_provs = n(),
        B_r     = weighted.mean(direct_povrate, Nd, na.rm = TRUE),
        source  = "survey direct",
        .groups = "drop"
      )
    if (!is.null(external_benchmark_mat)) {
      external_region_benchmarks <- tibble::tibble(
        region = rownames(external_benchmark_mat),
        B_r = as.numeric(external_benchmark_mat[, as.character(yr)]),
        source = "Benchmark Target Database"
      )
      region_benchmarks <- direct_region_benchmarks |>
        select(region, n_provs) |>
        left_join(external_region_benchmarks, by = "region")
      cat("Using Benchmark Target Database for UFH benchmarking:", benchmark_target_path, "\n")
    } else {
      region_benchmarks <- direct_region_benchmarks
    }
    cat("\nUFH benchmark targets (yr =", yr, "):\n")
    print(region_benchmarks)

    region_df <- fh_dt |>
      left_join(region_benchmarks |> select(region, B_r), by = "region") |>
      mutate(direct_povrate = if_else(is.finite(B_r), B_r, direct_povrate)) |>
      select(domain, region, Nd, direct_povrate)
    fh_bench  <- bench_regional(
      model = fh_model,
      region_df = region_df,
      MSE = TRUE,
      B = 200,
      seed = 123
    )

    pov_fh <- as.data.frame(estimators(fh_bench, MSE = TRUE, CV = TRUE))
  } else {
    pov_fh <- as.data.frame(estimators(fh_model, MSE = TRUE, CV = TRUE))
  }

  # ---- Prepare results ----
  pov_fh <- pov_fh |>
    rename(domain := "Domain") |>
    mutate(year := yr)
  if (!do_benchmark) {
    pov_fh$FH_Bench <- pov_fh$FH
    pov_fh$FH_Bench_MSE <- pov_fh$FH_MSE
    pov_fh$FH_Bench_CV <- pov_fh$FH_CV
    attr(pov_fh, "benchmarking_applied") <- FALSE
  } else {
    attr(pov_fh, "benchmarking_applied") <- TRUE
  }

  # ---- Back-transform from log to original scale (mean welfare only) ----
  # When bias correction is ON (bc_sm, the default), we use a per-domain
  # smearing factor anchored to the population-weighted arithmetic mean welfare:
  #   smear_d  = direct_arith_d / exp(direct_povrate_d)
  # so the FH/FH_Bench EBLUPs target the arithmetic mean (Duan-style
  # smearing). Variance is propagated by the delta method using the
  # per-domain back-transformed point estimate as the multiplier:
  # Var(cÂ·exp(Î·)) â‰ˆ (cÂ·exp(Î·))Â² Â· Var(Î·).
  #
  # When bias correction is OFF (user explicitly picked "none"), we
  # set smear_d = 1, which makes FH/FH_Bench = exp(Î·Ì‚) -- the naive,
  # downward-biased back-transform. The Direct column is still
  # replaced with the population-weighted arithmetic mean of welfare
  # because that quantity is unbiased and identifies "mean welfare"
  # by definition; the user is opting out of bias correction for the
  # MODEL EBLUP, not redefining what "Direct" means.
  if (identical(indicator_type, "mean_welfare") && isTRUE(log_transform)) {
    # Variance smoothing earlier in run_fh_year() merges direct_dt against
    # var_smooth using by.x = "Domain", by.y = "domain", which renames the
    # join column. Normalise to lowercase `domain` here so the smearing
    # lookup and the region merge below both work regardless of which
    # branch was taken upstream.
    .direct_norm <- direct_dt
    if ("Domain" %in% names(.direct_norm) && !"domain" %in% names(.direct_norm)) {
      names(.direct_norm)[names(.direct_norm) == "Domain"] <- "domain"
    }

    # `ufh_bias_correction` is set in the outer scope (chunk near top of
    # the qmd) from cfg$ufh$bias_correction. TRUE -> bc_sm (smearing),
    # FALSE -> none (naive exp). Default is TRUE if missing.
    .apply_smearing <- isTRUE(ufh_bias_correction)
    smear_lookup <- .direct_norm |>
      dplyr::mutate(smear_d = if (.apply_smearing) {
        ifelse(
          is.finite(direct_arith) & is.finite(direct_povrate) & abs(direct_povrate) < 50,
          direct_arith / exp(direct_povrate),
          1
        )
      } else {
        # No bias correction: naive exp(eta) for FH/FH_Bench.
        rep(1, dplyr::n())
      }) |>
      dplyr::select(domain, smear_d, direct_arith, SD_arith)
    if (!.apply_smearing) {
      cat(sprintf("UFH back-transform (year %s): bias correction = none (naive exp); FH/FH_Bench target the geometric mean.\n", yr))
    }

    pov_fh <- pov_fh |>
      dplyr::left_join(smear_lookup, by = "domain")

    for (col in c("Direct", "FH", "FH_Bench")) {
      mse_col <- paste0(col, "_MSE")
      cv_col  <- paste0(col, "_CV")
      if (!col %in% names(pov_fh)) next
      eta <- pov_fh[[col]]
      if (col == "Direct") {
        # Replace with the actual population-weighted arithmetic mean and its variance
        pov_fh[[col]] <- pov_fh$direct_arith
        if (mse_col %in% names(pov_fh)) {
          pov_fh[[mse_col]] <- (pov_fh$SD_arith)^2
        }
      } else {
        # Per-domain anchored back-transform of the model EBLUP
        pov_fh[[col]] <- pov_fh$smear_d * exp(eta)
        if (mse_col %in% names(pov_fh)) {
          pov_fh[[mse_col]] <- (pov_fh[[col]])^2 * pov_fh[[mse_col]]
        }
      }
      if (cv_col %in% names(pov_fh) && mse_col %in% names(pov_fh)) {
        pov_fh[[cv_col]] <- .safe_cv_from_mse(pov_fh[[mse_col]], pov_fh[[col]])
      }
    }
    # ---- Re-benchmark on the EUR scale ---------------------------------
    # The FH_Bench column we just back-transformed was produced by a ratio
    # benchmark applied on the log scale. After exponentiation the result
    # is no longer guaranteed to satisfy the regional aggregation
    # constraint on the original currency scale (population-weighted
    # regional mean of FH_Bench should equal the regional weighted mean
    # of arithmetic-mean direct welfare). Override FH_Bench by computing
    # a fresh ratio adjustment on the EUR scale using the back-transformed
    # FH and Direct columns. The corresponding MSE is left as the
    # delta-method propagated value from the log-scale bootstrap -- that
    # is an approximation; redoing the bootstrap on the EUR scale is on
    # the future-work list.
    if (do_benchmark && !is.null(region_map) &&
        "FH" %in% names(pov_fh) && "FH_Bench" %in% names(pov_fh)) {
      reg_lookup <- .direct_norm |>
        dplyr::left_join(region_map, by = "domain") |>
        dplyr::left_join(pop_dt, by = "domain") |>
        dplyr::left_join(
          dplyr::tibble(domain = pov_fh$domain, FH_eur = pov_fh$FH),
          by = "domain"
        )
      if ("Nd" %in% names(reg_lookup)) {
        region_lambda <- reg_lookup |>
          dplyr::filter(!is.na(direct_arith), !is.na(FH_eur), !is.na(Nd)) |>
          dplyr::group_by(region) |>
          dplyr::summarise(
            B_eur     = stats::weighted.mean(direct_arith, Nd, na.rm = TRUE),
            FH_bar    = stats::weighted.mean(FH_eur,       Nd, na.rm = TRUE),
            lambda_eur = ifelse(abs(FH_bar) > 1e-8, B_eur / FH_bar, 1),
            .groups   = "drop"
          )
        lambda_for_pov_fh <- region_lambda$lambda_eur[match(
          reg_lookup$region[match(pov_fh$domain, reg_lookup$domain)],
          region_lambda$region
        )]
        lambda_for_pov_fh[!is.finite(lambda_for_pov_fh)] <- 1
        # Cache the OLD FH_Bench (log-scale-derived bench in EUR) before
        # overwriting; FH_Bench_MSE was tied to that point estimate, not
        # to the unbenchmarked FH. Mirrors the MFH fix.
        FH_Bench_old <- pov_fh$FH_Bench
        pov_fh$FH_Bench <- lambda_for_pov_fh * pov_fh$FH
        if ("FH_Bench_MSE" %in% names(pov_fh)) {
          ratio_sq <- (pov_fh$FH_Bench / pmax(abs(FH_Bench_old), 1e-8))^2
          ratio_sq[!is.finite(ratio_sq)] <- 1
          pov_fh$FH_Bench_MSE <- pov_fh$FH_Bench_MSE * ratio_sq
        }
        if ("FH_Bench_CV" %in% names(pov_fh) && "FH_Bench_MSE" %in% names(pov_fh)) {
          pov_fh$FH_Bench_CV <- .safe_cv_from_mse(pov_fh$FH_Bench_MSE,
                                                  pov_fh$FH_Bench)
        }
        attr(pov_fh, "bench_scale") <- "EUR"
      }
    }

    pov_fh <- pov_fh |> dplyr::select(-any_of(c("smear_d", "direct_arith", "SD_arith")))
    attr(pov_fh, "log_back_transformed") <- TRUE
    attr(pov_fh, "smearing_anchor")       <- "per_domain_arithmetic"
  }

  # Return objects needed for diagnostics and downstream analysis
  list(
    pov_fh     = pov_fh,
    fh_model   = fh_model,
    fh_formula = fh_formula,
    fh_bench   = fh_bench,
    fh_dt      = fh_dt,
    direct_dt  = direct_dt,
    selection_diagnostics = selection_diag
  )
}

plot_mse_comparison <- function(pov_fh_year, year_label) {
  mse_long <- pov_fh_year %>%
    select(domain, Direct_MSE, FH_MSE, FH_Bench_MSE) %>%
    pivot_longer(-domain, names_to = "Method", values_to = "MSE") %>%
    mutate(
      Method = factor(
        Method,
        levels = c("Direct_MSE", "FH_MSE", "FH_Bench_MSE"),
        labels = c("Direct", "FH", "FH Benchmarked")
      )
    )

  ggplot(mse_long, aes(x = reorder(domain, MSE), y = MSE, color = Method)) +
    geom_point(size = 2, alpha = 0.85) +
    scale_color_manual(
      values = c("Direct" = "black", "FH" = "#1f77b4", "FH Benchmarked" = "#d62728")
    ) +
    labs(
      title = paste0("MSE Comparison by Domain - Year ", year_label),
      x = "Domain (ordered by increasing MSE within method)",
      y = "MSE",
      color = "Estimator"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
}
build_mse_decomposition <- function(res_year) {
  if (!isTRUE(do_benchmark) ||
      !"region" %in% names(res_year$fh_dt) ||
      !"Nd" %in% names(res_year$fh_dt)) {
    return(
      res_year$pov_fh %>%
        select(domain, Direct_MSE, FH_MSE, FH_Bench_MSE) %>%
        mutate(
          region = "not benchmarked",
          Bench_gt_Direct = FH_Bench_MSE > Direct_MSE,
          Bench_gt_FH = FH_Bench_MSE > FH_MSE,
          Bench_minus_Direct = FH_Bench_MSE - Direct_MSE,
          Bench_minus_FH = FH_Bench_MSE - FH_MSE,
          Bench_to_Direct = FH_Bench_MSE / Direct_MSE,
          Bench_to_FH = FH_Bench_MSE / FH_MSE,
          lambda_r = NA_real_,
          abs_lambda_shift = NA_real_
        )
    )
  }

  region_diag <- res_year$fh_dt %>%
    select(domain, region, Nd, direct_povrate) %>%
    distinct()

  fh_ind <- res_year$fh_model$ind %>%
    select(Domain, FH) %>%
    rename(domain = Domain)

  lambda_by_region <- region_diag %>%
    left_join(fh_ind, by = "domain") %>%
    group_by(region) %>%
    summarize(
      B_r = weighted.mean(direct_povrate, Nd, na.rm = TRUE),
      FH_bar_r = weighted.mean(FH, Nd, na.rm = TRUE),
      lambda_r = if_else(abs(FH_bar_r) < 1e-8, NA_real_, B_r / FH_bar_r),
      .groups = "drop"
    )

  res_year$pov_fh %>%
    select(domain, Direct_MSE, FH_MSE, FH_Bench_MSE) %>%
    left_join(region_diag %>% select(domain, region), by = "domain") %>%
    left_join(lambda_by_region, by = "region") %>%
    mutate(
      Bench_gt_Direct = FH_Bench_MSE > Direct_MSE,
      Bench_gt_FH = FH_Bench_MSE > FH_MSE,
      Bench_minus_Direct = FH_Bench_MSE - Direct_MSE,
      Bench_minus_FH = FH_Bench_MSE - FH_MSE,
      Bench_to_Direct = FH_Bench_MSE / Direct_MSE,
      Bench_to_FH = FH_Bench_MSE / FH_MSE,
      abs_lambda_shift = abs(lambda_r - 1)
    ) %>%
    arrange(desc(Bench_to_Direct))
}


survey_all |> glimpse()


rhs_dt_raw |> glimpse()


shp_dt |> glimpse()


fh_table <- tibble::tibble(
  Dataset = c("`survey_dt`", "`rhs_dt`", "`shp_dt`"),
  `Unit of Observation` = c(
    "Individual (or Household)",
    "Target Area (e.g. Province)",
    "Target Area (Spatial)"
  ),
  `Required Variables` = c(
    "`target area identifiers`, `weights`, `cluster identifier`, `welfare variable`, `poverty line`",
    "`target area identifiers`, `covariates` (e.g. `gen`, `educ1`, `schyrs`, etc.)",
    "`target area identifiers`, `geometries` (e.g. `geometry` column from `sf`)"
  )
)

fh_table %>%
 gt() %>%
  tab_header(
    title = md("**Data Input Checklist for the Univariate Fay-Herriot Model**"),
    subtitle = md("*Datasets, levels, and Required variables*")
  ) %>%
  cols_label(
    Dataset = "Dataset Name",
    `Unit of Observation` = "Unit of Observation",
    `Required Variables` = "Required Variables"
  ) %>%
  tab_options(
    table.font.names = "Arial",
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12,
    table.font.size = 12,
    column_labels.font.weight = "bold",
    data_row.padding = px(4),
    table.border.top.width = px(2),
    table.border.bottom.width = px(2),
    heading.align = "left"
  ) %>%
  fmt_markdown(columns = everything())


res_y1 <- run_fh_year(yr = years_keep[1],
                      survey_all = survey_all,
                      rhs_dt_raw = rhs_dt_raw,
                      shp_dt = shp_dt,
                      years_keep = years_keep,
                      region_map = region_map,
                      transformation = ufh_transformation,
                      backtransformation = ufh_backtrans_method,
                      formula_override = ufh_formula_override_y1,
                      candidate_vars_override = ufh_regressors_y1,
                      var_choice = ufh_var_choice,
                      do_benchmark = do_benchmark,
                      population_path = population_path,
                      benchmark_target_path = benchmark_target_path,
                      benchmark_level_variable = var_map$benchmark_level,
                      lasso_enabled = ufh_lasso_enabled,
                      lasso_lambda = ufh_lasso_lambda)

fh_results_list[[as.character(years_keep[1])]] <- res_y1$pov_fh


cat("Final selected model formula (Year", years_keep[1], "):\n")
print(res_y1$fh_formula)








summary(res_y1$fh_model)


.coef_df <- res_y1$fh_model$model$coefficients
.coef_tbl <- data.frame(
  Variable    = rownames(.coef_df),
  Estimate    = .coef_df$coefficients,
  Std.Error   = .coef_df$std.error,
  z.value     = .coef_df$t.value,
  p.value     = .coef_df$p.value,
  Signif      = ifelse(.coef_df$p.value < 0.001, "***",
                ifelse(.coef_df$p.value < 0.01,  "**",
                ifelse(.coef_df$p.value < 0.05,  "*",
                ifelse(.coef_df$p.value < 0.1,   ".",  "")))),
  check.names = FALSE
)
knitr::kable(
  .coef_tbl,
  digits    = c(0, 6, 6, 3, 4, 0),
  align     = c("l", "r", "r", "r", "r", "c"),
  caption   = paste0("UFH model coefficients and significance -- Year ", years_keep[1]),
  row.names = FALSE
)
cat("\n*Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1*\n")


# emdi's plot.emdi() and compare.emdi() contain hardcoded readline() calls.
# trace() injects a local readline shadow into each method body so the
# pipeline doesn't hang waiting for console input.
tryCatch({
  trace("plot.emdi", tracer = quote(readline <- function(...) ""),
        where = asNamespace("emdi"), print = FALSE)
}, error = function(e) message("Note: could not trace plot.emdi: ", e$message))
tryCatch({
  trace("compare.emdi", tracer = quote(readline <- function(...) ""),
        where = asNamespace("emdi"), print = FALSE)
}, error = function(e) message("Note: could not trace compare.emdi: ", e$message))

pdf(here::here("outputs", "figures", "ufh_diagnostics_y1.pdf"),
    width = 10, height = 8)
tryCatch(plot(res_y1$fh_model),
         error = function(e) message("Note: emdi diagnostic plot (Y1) skipped: ", e$message))
dev.off()

if (.use_eur_plots) {
  .p_cmp_y1 <- .compare_plot_eur(res_y1$pov_fh, years_keep[1])
  ggsave(here::here("outputs", "figures", "ufh_compare_y1.png"),
         .p_cmp_y1, width = 10, height = 8, dpi = 150)
} else {
  pdf(here::here("outputs", "figures", "ufh_compare_y1.pdf"),
      width = 10, height = 8)
  tryCatch(compare_plot(res_y1$fh_model, MSE = TRUE, CV = TRUE),
           error = function(e) message("Note: compare_plot (Y1) skipped: ", e$message))
  dev.off()
}

pdf(nullfile())
tryCatch(compare(res_y1$fh_model),
         error = function(e) message("Note: compare (Y1) skipped: ", e$message))
dev.off()



# --- 3-way point-estimate comparison: Direct vs FH vs FH Benchmarked ---
# Order domains by increasing Direct poverty rate
domain_order_y1 <- res_y1$pov_fh %>% arrange(Direct) %>% pull(domain)

bench_comp_y1 <- res_y1$pov_fh %>%
  select(domain, Direct, FH, FH_Bench) %>%
  mutate(domain = factor(domain, levels = domain_order_y1)) %>%
  pivot_longer(-domain, names_to = "Method", values_to = "Estimate") %>%
  mutate(Method = factor(Method,
                         levels = c("Direct", "FH", "FH_Bench"),
                         labels = c("Direct", "FH", "FH Benchmarked")))

.p_bench_comp_y1 <- ggplot(bench_comp_y1, aes(x = domain, y = Estimate, color = Method, shape = Method)) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_color_manual(
    values = c("Direct" = "black", "FH" = "#1f77b4", "FH Benchmarked" = "#d62728")
  ) +
  labs(
    title = paste0(paste0("Comparison of ", pov_lab$short, " Estimates by Domain \u2013 Year "), years_keep[1]),
    x = "Domain (ordered by increasing Direct estimate)",
    y = "Poverty Rate Estimate",
    color = "Method", shape = "Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
print(.p_bench_comp_y1)
ggsave(here::here("outputs", "figures", "ufh_bench_compare_y1.png"),
       .p_bench_comp_y1, width = 14, height = 6, dpi = 150)



# --- Summary table: Direct, FH, FH Benchmarked ---
suppressWarnings({
  print(kable(
    res_y1$pov_fh %>%
      select(domain, Direct, FH, FH_Bench, Direct_CV, FH_CV, FH_Bench_CV),
    digits = 4,
    caption = paste0("Direct, FH, and Benchmarked FH Estimates with CVs (Year ", years_keep[1], ")")
  ))
})



rmse_y1 <- res_y1$pov_fh %>%
  mutate(
    Direct_RMSE   = sqrt(Direct_MSE),
    FH_RMSE       = sqrt(FH_MSE),
    FH_Bench_RMSE = sqrt(FH_Bench_MSE)
  )

# Order domains by increasing Direct RMSE
rmse_order_y1 <- rmse_y1 %>% arrange(Direct_RMSE) %>% pull(domain)

rmse_y1_long <- rmse_y1 %>%
  select(domain, Direct_RMSE, FH_RMSE, FH_Bench_RMSE) %>%
  mutate(domain = factor(domain, levels = rmse_order_y1)) %>%
  pivot_longer(-domain, names_to = "Method", values_to = "RMSE") %>%
  mutate(Method = factor(Method,
                         levels = c("Direct_RMSE", "FH_RMSE", "FH_Bench_RMSE"),
                         labels = c("Direct", "FH", "FH Benchmarked")))

.p_rmse_y1 <- ggplot(rmse_y1_long, aes(x = domain, y = RMSE, color = Method)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(
    values = c("Direct" = "black", "FH" = "#1f77b4", "FH Benchmarked" = "#d62728")
  ) +
  labs(
    title   = paste0("RMSE by Domain \u2013 Year ", years_keep[1]),
    x       = "Domain (ordered by increasing Direct RMSE)",
    y       = "RMSE",
    color   = "Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
print(.p_rmse_y1)
ggsave(here::here("outputs", "figures", "ufh_rmse_y1.png"),
       .p_rmse_y1, width = 14, height = 6, dpi = 150)



# Order domains by increasing Direct CV
cv_order_y1 <- res_y1$pov_fh %>% arrange(Direct_CV) %>% pull(domain)

cv_y1_long <- res_y1$pov_fh %>%
  select(domain, Direct_CV, FH_CV, FH_Bench_CV) %>%
  mutate(domain = factor(domain, levels = cv_order_y1)) %>%
  pivot_longer(-domain, names_to = "Method", values_to = "CV") %>%
  mutate(Method = factor(Method,
                         levels = c("Direct_CV", "FH_CV", "FH_Bench_CV"),
                         labels = c("Direct", "FH", "FH Benchmarked")))

.p_cv_y1 <- ggplot(cv_y1_long, aes(x = domain, y = CV, color = Method)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(
    values = c("Direct" = "black", "FH" = "#1f77b4", "FH Benchmarked" = "#d62728")
  ) +
  labs(
    title   = paste0("CV by Domain \u2013 Year ", years_keep[1]),
    x       = "Domain (ordered by increasing Direct CV)",
    y       = "Coefficient of Variation (CV)",
    color   = "Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
print(.p_cv_y1)
ggsave(here::here("outputs", "figures", "ufh_cv_y1.png"),
       .p_cv_y1, width = 14, height = 6, dpi = 150)



.p_mse_y1 <- plot_mse_comparison(res_y1$pov_fh, years_keep[1])
print(.p_mse_y1)
ggsave(here::here("outputs", "figures", "ufh_mse_y1.png"),
       .p_mse_y1, width = 14, height = 6, dpi = 150)

mse_decomp_y1 <- build_mse_decomposition(res_y1)

# UFH Lambda summary by region
if (isTRUE(do_benchmark)) {
  lambda_by_region_y1 <- mse_decomp_y1 %>%
    group_by(region) %>%
    summarize(lambda_r = first(lambda_r), .groups = "drop") %>%
    mutate(abs_shift = round(abs(lambda_r - 1), 6),
           lambda_r  = round(lambda_r, 6))
  cat("\nUFH Lambda factors by region (Year", years_keep[1], "):\n")
  print(as.data.frame(lambda_by_region_y1))
  cat("Mean |lambda - 1|:", round(mean(lambda_by_region_y1$abs_shift), 6), "\n")
} else {
  cat("\nUFH benchmarking not applied; lambda factor summary skipped for Year",
      years_keep[1], ".\n")
}

cat("Year", years_keep[1], "- Domains with FH_Bench_MSE > Direct_MSE:",
    sum(mse_decomp_y1$Bench_gt_Direct, na.rm = TRUE), "\n")

suppressWarnings({
  print(kable(
    mse_decomp_y1 %>%
      filter(Bench_gt_Direct) %>%
      select(domain, region, Direct_MSE, FH_MSE, FH_Bench_MSE,
             Bench_to_Direct, Bench_to_FH, lambda_r, abs_lambda_shift),
    digits = 4,
    caption = paste0(
      "Domains with FH_Bench_MSE > Direct_MSE and their regional lambda_r (",
      years_keep[1], ")"
    )
  ))
})

suppressWarnings({
  print(kable(
    mse_decomp_y1 %>%
      select(domain, region, Direct_MSE, FH_MSE, FH_Bench_MSE,
             Bench_minus_Direct, Bench_minus_FH,
             Bench_to_Direct, Bench_to_FH,
             lambda_r, abs_lambda_shift),
    digits = 4,
    caption = paste0("MSE decomposition by domain (", years_keep[1], ")")
  ))
})


if (.use_eur_plots) {
  .p_map_y1 <- .map_plot_eur(res_y1$pov_fh, shp_dt, years_keep[1])
  ggsave(here::here("outputs", "figures", "ufh_map_y1.png"),
         .p_map_y1, width = 10, height = 8, dpi = 150)
} else {
  .map_model_y1 <- if (isTRUE(do_benchmark) && !is.null(res_y1$fh_bench)) {
    res_y1$fh_bench
  } else {
    res_y1$fh_model
  }
  domain_ord <- match(shp_dt[["domain"]], .map_model_y1$ind$Domain)
  if (any(is.na(domain_ord))) {
    stop(
      "UFH map year ", years_keep[1],
      " cannot match all geometry domains to FH estimates. Unmatched geometry domains: ",
      paste(head(shp_dt[["domain"]][is.na(domain_ord)], 10), collapse = ", ")
    )
  }
  map_tab <- data.frame(pop_data_id = .map_model_y1$ind$Domain[domain_ord],
                        shape_id = shp_dt[["domain"]])
  png(here::here("outputs", "figures", "ufh_map_y1.png"),
      width = 10, height = 8, units = "in", res = 150)
  map_plot(object = .map_model_y1, MSE = TRUE, map_obj = shp_dt,
           map_dom_id = "domain", map_tab = map_tab)
  dev.off()
}


res_y2 <- run_fh_year(yr = years_keep[2],
                      survey_all = survey_all,
                      rhs_dt_raw = rhs_dt_raw,
                      shp_dt = shp_dt,
                      years_keep = years_keep,
                      region_map = region_map,
                      transformation = ufh_transformation,
                      backtransformation = ufh_backtrans_method,
                      formula_override = ufh_formula_override_y2,
                      candidate_vars_override = ufh_regressors_y2,
                      var_choice = ufh_var_choice,
                      do_benchmark = do_benchmark,
                      population_path = population_path,
                      benchmark_target_path = benchmark_target_path,
                      benchmark_level_variable = var_map$benchmark_level,
                      lasso_enabled = ufh_lasso_enabled,
                      lasso_lambda = ufh_lasso_lambda)

fh_results_list[[as.character(years_keep[2])]] <- res_y2$pov_fh


cat("Final selected model formula (Year", years_keep[2], "):\n")
print(res_y2$fh_formula)








summary(res_y2$fh_model)


.coef_df2 <- res_y2$fh_model$model$coefficients
.coef_tbl2 <- data.frame(
  Variable    = rownames(.coef_df2),
  Estimate    = .coef_df2$coefficients,
  Std.Error   = .coef_df2$std.error,
  z.value     = .coef_df2$t.value,
  p.value     = .coef_df2$p.value,
  Signif      = ifelse(.coef_df2$p.value < 0.001, "***",
                ifelse(.coef_df2$p.value < 0.01,  "**",
                ifelse(.coef_df2$p.value < 0.05,  "*",
                ifelse(.coef_df2$p.value < 0.1,   ".",  "")))),
  check.names = FALSE
)
knitr::kable(
  .coef_tbl2,
  digits    = c(0, 6, 6, 3, 4, 0),
  align     = c("l", "r", "r", "r", "r", "c"),
  caption   = paste0("UFH model coefficients and significance -- Year ", years_keep[2]),
  row.names = FALSE
)
cat("\n*Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1*\n")


pdf(here::here("outputs", "figures", "ufh_diagnostics_y2.pdf"),
    width = 10, height = 8)
tryCatch(plot(res_y2$fh_model), error = function(e)
  message("Note: emdi diagnostic plot (Y2) skipped: ", e$message))
dev.off()

if (.use_eur_plots) {
  .p_cmp_y2 <- .compare_plot_eur(res_y2$pov_fh, years_keep[2])
  ggsave(here::here("outputs", "figures", "ufh_compare_y2.png"),
         .p_cmp_y2, width = 10, height = 8, dpi = 150)
} else {
  pdf(here::here("outputs", "figures", "ufh_compare_y2.pdf"),
      width = 10, height = 8)
  tryCatch(compare_plot(res_y2$fh_model, MSE = TRUE, CV = TRUE),
           error = function(e) message("Note: compare_plot (Y2) skipped: ", e$message))
  dev.off()
}

pdf(nullfile())
tryCatch(compare(res_y2$fh_model),
         error = function(e) message("Note: compare (Y2) skipped: ", e$message))
dev.off()

# Remove readline trace from emdi methods
tryCatch(untrace("plot.emdi", where = asNamespace("emdi")),
         error = function(e) NULL)
tryCatch(untrace("compare.emdi", where = asNamespace("emdi")),
         error = function(e) NULL)



# --- 3-way point-estimate comparison: Direct vs FH vs FH Benchmarked ---
# Order domains by increasing Direct poverty rate
domain_order_y2 <- res_y2$pov_fh %>% arrange(Direct) %>% pull(domain)

bench_comp_y2 <- res_y2$pov_fh %>%
  select(domain, Direct, FH, FH_Bench) %>%
  mutate(domain = factor(domain, levels = domain_order_y2)) %>%
  pivot_longer(-domain, names_to = "Method", values_to = "Estimate") %>%
  mutate(Method = factor(Method,
                         levels = c("Direct", "FH", "FH_Bench"),
                         labels = c("Direct", "FH", "FH Benchmarked")))

.p_bench_comp_y2 <- ggplot(bench_comp_y2, aes(x = domain, y = Estimate, color = Method, shape = Method)) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_color_manual(
    values = c("Direct" = "black", "FH" = "#1f77b4", "FH Benchmarked" = "#d62728")
  ) +
  labs(
    title = paste0(paste0("Comparison of ", pov_lab$short, " Estimates by Domain \u2013 Year "), years_keep[2]),
    x = "Domain (ordered by increasing Direct estimate)",
    y = "Poverty Rate Estimate",
    color = "Method", shape = "Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
print(.p_bench_comp_y2)
ggsave(here::here("outputs", "figures", "ufh_bench_compare_y2.png"),
       .p_bench_comp_y2, width = 14, height = 6, dpi = 150)



# --- Summary table: Direct, FH, FH Benchmarked ---
suppressWarnings({
  print(kable(
    res_y2$pov_fh %>%
      select(domain, Direct, FH, FH_Bench, Direct_CV, FH_CV, FH_Bench_CV),
    digits = 4,
    caption = paste0("Direct, FH, and Benchmarked FH Estimates with CVs (Year ", years_keep[2], ")")
  ))
})



rmse_y2 <- res_y2$pov_fh %>%
  mutate(
    Direct_RMSE   = sqrt(Direct_MSE),
    FH_RMSE       = sqrt(FH_MSE),
    FH_Bench_RMSE = sqrt(FH_Bench_MSE)
  )

# Order domains by increasing Direct RMSE
rmse_order_y2 <- rmse_y2 %>% arrange(Direct_RMSE) %>% pull(domain)

rmse_y2_long <- rmse_y2 %>%
  select(domain, Direct_RMSE, FH_RMSE, FH_Bench_RMSE) %>%
  mutate(domain = factor(domain, levels = rmse_order_y2)) %>%
  pivot_longer(-domain, names_to = "Method", values_to = "RMSE") %>%
  mutate(Method = factor(Method,
                         levels = c("Direct_RMSE", "FH_RMSE", "FH_Bench_RMSE"),
                         labels = c("Direct", "FH", "FH Benchmarked")))

.p_rmse_y2 <- ggplot(rmse_y2_long, aes(x = domain, y = RMSE, color = Method)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(
    values = c("Direct" = "black", "FH" = "#1f77b4", "FH Benchmarked" = "#d62728")
  ) +
  labs(
    title   = paste0("RMSE by Domain \u2013 Year ", years_keep[2]),
    x       = "Domain (ordered by increasing Direct RMSE)",
    y       = "RMSE",
    color   = "Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
print(.p_rmse_y2)
ggsave(here::here("outputs", "figures", "ufh_rmse_y2.png"),
       .p_rmse_y2, width = 14, height = 6, dpi = 150)



# Order domains by increasing Direct CV
cv_order_y2 <- res_y2$pov_fh %>% arrange(Direct_CV) %>% pull(domain)

cv_y2_long <- res_y2$pov_fh %>%
  select(domain, Direct_CV, FH_CV, FH_Bench_CV) %>%
  mutate(domain = factor(domain, levels = cv_order_y2)) %>%
  pivot_longer(-domain, names_to = "Method", values_to = "CV") %>%
  mutate(Method = factor(Method,
                         levels = c("Direct_CV", "FH_CV", "FH_Bench_CV"),
                         labels = c("Direct", "FH", "FH Benchmarked")))

.p_cv_y2 <- ggplot(cv_y2_long, aes(x = domain, y = CV, color = Method)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(
    values = c("Direct" = "black", "FH" = "#1f77b4", "FH Benchmarked" = "#d62728")
  ) +
  labs(
    title   = paste0("CV by Domain \u2013 Year ", years_keep[2]),
    x       = "Domain (ordered by increasing Direct CV)",
    y       = "Coefficient of Variation (CV)",
    color   = "Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
print(.p_cv_y2)
ggsave(here::here("outputs", "figures", "ufh_cv_y2.png"),
       .p_cv_y2, width = 14, height = 6, dpi = 150)



.p_mse_y2 <- plot_mse_comparison(res_y2$pov_fh, years_keep[2])
print(.p_mse_y2)
ggsave(here::here("outputs", "figures", "ufh_mse_y2.png"),
       .p_mse_y2, width = 14, height = 6, dpi = 150)

mse_decomp_y2 <- build_mse_decomposition(res_y2)

# UFH Lambda summary by region
if (isTRUE(do_benchmark)) {
  lambda_by_region_y2 <- mse_decomp_y2 %>%
    group_by(region) %>%
    summarize(lambda_r = first(lambda_r), .groups = "drop") %>%
    mutate(abs_shift = round(abs(lambda_r - 1), 6),
           lambda_r  = round(lambda_r, 6))
  cat("\nUFH Lambda factors by region (Year", years_keep[2], "):\n")
  print(as.data.frame(lambda_by_region_y2))
  cat("Mean |lambda - 1|:", round(mean(lambda_by_region_y2$abs_shift), 6), "\n")
} else {
  cat("\nUFH benchmarking not applied; lambda factor summary skipped for Year",
      years_keep[2], ".\n")
}

cat("Year", years_keep[2], "- Domains with FH_Bench_MSE > Direct_MSE:",
    sum(mse_decomp_y2$Bench_gt_Direct, na.rm = TRUE), "\n")

suppressWarnings({
  print(kable(
    mse_decomp_y2 %>%
      filter(Bench_gt_Direct) %>%
      select(domain, region, Direct_MSE, FH_MSE, FH_Bench_MSE,
             Bench_to_Direct, Bench_to_FH, lambda_r, abs_lambda_shift),
    digits = 4,
    caption = paste0(
      "Domains with FH_Bench_MSE > Direct_MSE and their regional lambda_r (",
      years_keep[2], ")"
    )
  ))
})

suppressWarnings({
  print(kable(
    mse_decomp_y2 %>%
      select(domain, region, Direct_MSE, FH_MSE, FH_Bench_MSE,
             Bench_minus_Direct, Bench_minus_FH,
             Bench_to_Direct, Bench_to_FH,
             lambda_r, abs_lambda_shift),
    digits = 4,
    caption = paste0("MSE decomposition by domain (", years_keep[2], ")")
  ))
})


if (.use_eur_plots) {
  .p_map_y2 <- .map_plot_eur(res_y2$pov_fh, shp_dt, years_keep[2])
  ggsave(here::here("outputs", "figures", "ufh_map_y2.png"),
         .p_map_y2, width = 10, height = 8, dpi = 150)
} else {
  .map_model_y2 <- if (isTRUE(do_benchmark) && !is.null(res_y2$fh_bench)) {
    res_y2$fh_bench
  } else {
    res_y2$fh_model
  }
  domain_ord <- match(shp_dt[["domain"]], .map_model_y2$ind$Domain)
  if (any(is.na(domain_ord))) {
    stop(
      "UFH map year ", years_keep[2],
      " cannot match all geometry domains to FH estimates. Unmatched geometry domains: ",
      paste(head(shp_dt[["domain"]][is.na(domain_ord)], 10), collapse = ", ")
    )
  }
  map_tab <- data.frame(pop_data_id = .map_model_y2$ind$Domain[domain_ord],
                        shape_id = shp_dt[["domain"]])
  png(here::here("outputs", "figures", "ufh_map_y2.png"),
      width = 10, height = 8, units = "in", res = 150)
  map_plot(object = .map_model_y2, MSE = TRUE, map_obj = shp_dt,
           map_dom_id = "domain", map_tab = map_tab)
  dev.off()
}


dir.create(here::here("outputs", "data"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
domain_population <- sae_domain_population_summary(survey_all, years_keep)
poverty_line_export <- if (identical(indicator_type, "poverty")) {
  sae_poverty_line_summary(survey_all, years_keep)
} else {
  NULL
}

ufh_selection_diagnostics <- bind_rows(
  res_y1$selection_diagnostics,
  res_y2$selection_diagnostics
)
write.csv(
  ufh_selection_diagnostics,
  here::here("outputs", "tables", "ufh_selection_diagnostics.csv"),
  row.names = FALSE
)

ufh_model_diagnostics <- bind_rows(
  sae_model_fit_diagnostics(
    method = "UFH",
    year = years_keep[1],
    formula = res_y1$fh_formula,
    data = res_y1$fh_dt,
    mixed_model = res_y1$fh_model,
    random_variance = tryCatch(as.numeric(res_y1$fh_model$model$variance),
                               error = function(e) NA_real_),
    note = "R2 columns are OLS companion diagnostics for the selected formula."
  ),
  sae_model_fit_diagnostics(
    method = "UFH",
    year = years_keep[2],
    formula = res_y2$fh_formula,
    data = res_y2$fh_dt,
    mixed_model = res_y2$fh_model,
    random_variance = tryCatch(as.numeric(res_y2$fh_model$model$variance),
                               error = function(e) NA_real_),
    note = "R2 columns are OLS companion diagnostics for the selected formula."
  )
)
write.csv(
  ufh_model_diagnostics,
  here::here("outputs", "tables", "ufh_model_diagnostics.csv"),
  row.names = FALSE
)

pov_fh_combined <- bind_rows(res_y1$pov_fh, res_y2$pov_fh) %>%
  mutate(
    Direct_RMSE   = sqrt(Direct_MSE),
    FH_RMSE       = sqrt(FH_MSE),
    FH_Bench_RMSE = sqrt(FH_Bench_MSE)
  )
pov_fh_combined <- sae_enrich_result_table(
  pov_fh_combined,
  domain_metadata = domain_metadata,
  population = domain_population,
  poverty_lines = poverty_line_export
)
.pov_fh_export <- pov_fh_combined
if (!isTRUE(do_benchmark)) {
  .pov_fh_export <- .pov_fh_export %>%
    select(-any_of(c("FH_Bench", "FH_Bench_MSE", "FH_Bench_CV", "FH_Bench_RMSE")))
}
writexl::write_xlsx(.pov_fh_export, path = here::here("outputs", "data", "pov_fh.xlsx"))
cat("Combined results exported to: output/UFH/pov_fh.xlsx\n")

# Save emdi model objects for AI-assisted normality evaluation
saveRDS(res_y1$fh_model, here::here("outputs", "data", "fh_model_y1.rds"))
saveRDS(res_y2$fh_model, here::here("outputs", "data", "fh_model_y2.rds"))
cat("Model objects saved for normality evaluation.\n")


# ============================================================
# Export Shapiro-Wilk test results for the app/brief generator
# Extract from emdi summary objects for both years
# ============================================================

extract_shapiro_from_fh <- function(fh_model, yr) {
  s <- summary(fh_model)
  # emdi summary stores normality table with Shapiro_W and Shapiro_p
  norm_tbl <- s$normality

  # norm_tbl has rows: "Standardized_Residuals", "Random_Effects"
  # and columns: Skewness, Kurtosis, Shapiro_W, Shapiro_p
  resid_p <- norm_tbl["Standardized_Residuals", "Shapiro_p"]
  resid_w <- norm_tbl["Standardized_Residuals", "Shapiro_W"]
  re_p    <- norm_tbl["Random_Effects", "Shapiro_p"]
  re_w    <- norm_tbl["Random_Effects", "Shapiro_W"]

  data.frame(
    year      = c(yr, yr),
    component = c("residual", "random_effect"),
    W         = c(resid_w, re_w),
    p_value   = c(resid_p, re_p),
    model_type = "UFH"
  )
}

ufh_shapiro_results <- tryCatch(
  bind_rows(
    extract_shapiro_from_fh(res_y1$fh_model, years_keep[1]),
    extract_shapiro_from_fh(res_y2$fh_model, years_keep[2])
  ),
  error = function(e) {
    # Fallback: compute Shapiro tests directly from model residuals
    make_row <- function(fh_model, yr) {
      re  <- fh_model$model$random_effects
      res <- fh_model$model$std_real_residuals
      sw_re  <- if (!is.null(re) && length(re) >= 3 && length(unique(re)) >= 3) tryCatch(shapiro.test(re), error = function(e) list(statistic = c(W = NA), p.value = NA)) else list(statistic = c(W = NA), p.value = NA)
      sw_res <- if (!is.null(res) && length(res) >= 3 && length(unique(res)) >= 3) tryCatch(shapiro.test(res), error = function(e) list(statistic = c(W = NA), p.value = NA)) else list(statistic = c(W = NA), p.value = NA)
      data.frame(
        year      = c(yr, yr),
        component = c("residual", "random_effect"),
        W         = c(sw_res$statistic[[1]], sw_re$statistic[[1]]),
        p_value   = c(sw_res$p.value, sw_re$p.value),
        model_type = "UFH"
      )
    }
    bind_rows(
      make_row(res_y1$fh_model, years_keep[1]),
      make_row(res_y2$fh_model, years_keep[2])
    )
  }
)

write.csv(ufh_shapiro_results, file = here::here("outputs", "tables", "ufh_shapiro_results.csv"), row.names = FALSE)
cat("Shapiro-Wilk results exported to: output/UFH/ufh_shapiro_results.csv\n")


# Retrieve per-year results from the list
pov_year1 <- fh_results_list[[as.character(years_keep[1])]]
pov_year2 <- fh_results_list[[as.character(years_keep[2])]]

cat("Year 1 data dimensions:", dim(pov_year1), "\n")
cat("Year 2 data dimensions:", dim(pov_year2), "\n")

# ---- Defensive column check ----------------------------------------------
# Verify that both per-year data frames expose all the columns we rely on
# downstream. Under some configurations (e.g. certain transformation /
# variance-smoothing combinations) the column set returned by
# emdi::estimators() may differ, so we fail early with a clear message
# rather than silently producing an undefined `merged_data`/`results`.
.required_cols <- c("domain", "FH", "FH_MSE", "FH_Bench",
                    "FH_Bench_MSE", "Direct", "year")
.missing_y1 <- setdiff(.required_cols, names(pov_year1))
.missing_y2 <- setdiff(.required_cols, names(pov_year2))
if (length(.missing_y1) > 0 || length(.missing_y2) > 0) {
  cat("\n[significance-test] ERROR: required columns are missing.\n")
  cat("  Year 1 (", years_keep[1], ") has columns: ",
      paste(names(pov_year1), collapse = ", "), "\n", sep = "")
  cat("  Year 2 (", years_keep[2], ") has columns: ",
      paste(names(pov_year2), collapse = ", "), "\n", sep = "")
  if (length(.missing_y1) > 0)
    cat("  Missing in Year 1: ", paste(.missing_y1, collapse = ", "), "\n", sep = "")
  if (length(.missing_y2) > 0)
    cat("  Missing in Year 2: ", paste(.missing_y2, collapse = ", "), "\n", sep = "")
  stop("Cannot run significance testing: missing required columns ",
       "in per-year FH results.")
}

# Merge datasets by domain -- include both FH (unbenchmarked) and FH_Bench
merged_data <- pov_year1 %>%
  select(domain, FH, FH_MSE, FH_Bench, FH_Bench_MSE, Direct, year) %>%
  rename(
    FH_y1           = FH,
    FH_MSE_y1       = FH_MSE,
    FH_Bench_y1     = FH_Bench,
    FH_Bench_MSE_y1 = FH_Bench_MSE,
    Direct_y1       = Direct
  ) %>%
  inner_join(
    pov_year2 %>%
      select(domain, FH, FH_MSE, FH_Bench, FH_Bench_MSE, Direct, year) %>%
      rename(
        FH_y2           = FH,
        FH_MSE_y2       = FH_MSE,
        FH_Bench_y2     = FH_Bench,
        FH_Bench_MSE_y2 = FH_Bench_MSE,
        Direct_y2       = Direct
      ),
    by = "domain"
  )

suppressWarnings({
  print(kable(head(merged_data), digits = 4,
              caption = "Merged Poverty Estimates (First 6 Domains)"))
})

results_unbench <- merged_data %>%
  mutate(
    diff = FH_y2 - FH_y1,
    # UFH fits are period-specific and do not estimate cross-period prediction
    # covariance. State the independence assumption explicitly instead of
    # exposing an unreachable covariance branch that is always FALSE.
    variance_assumption = "period_independence",
    mse = FH_MSE_y1 + FH_MSE_y2,
    mse = ifelse(is.finite(mse) & mse >= 0, mse, NA_real_),
    se = sqrt(mse),
    zq = diff / se,
    alpha = change_alpha,
    z_critical = qnorm(1 - alpha / 2),
    lb = diff - z_critical * se,
    ub = diff + z_critical * se,
    p_value = 2 * stats::pnorm(abs(zq), lower.tail = FALSE),
    p_value_bh = stats::p.adjust(p_value, method = "BH"),
    p_value_bonferroni = stats::p.adjust(p_value, method = "bonferroni"),
    significant_unadjusted = p_value < alpha,
    significant_bh = p_value_bh < alpha,
    significant_bonferroni = p_value_bonferroni < alpha,
    significance_rule = "pointwise_unadjusted",
    significant = significant_unadjusted,
    index = row_number()
  ) %>%
  select(
    domain, diff, mse, variance_assumption, alpha, zq, p_value, p_value_bh,
    p_value_bonferroni, significant_unadjusted, significant_bh,
    significant_bonferroni, significance_rule, significant, lb, ub, index,
    FH_y1, FH_y2, Direct_y1, Direct_y2
  )

cat("=== Without Benchmarking (FH) ===\n")
cat("Total domains analyzed:", nrow(results_unbench), "\n")
cat("Statistically significant changes:", sum(results_unbench$significant), "\n")
cat("Non-significant changes:", sum(!results_unbench$significant), "\n\n")

cat("Positive changes (increase in poverty):", sum(results_unbench$diff > 0), "\n")
cat("  - Significant increases:", sum(results_unbench$diff > 0 & results_unbench$significant), "\n\n")

cat("Negative changes (decrease in poverty):", sum(results_unbench$diff < 0), "\n")
cat("  - Significant decreases:", sum(results_unbench$diff < 0 & results_unbench$significant), "\n")


# ---- Defensive column check ----------------------------------------------
# The merged_data must contain FH_Bench_{y1,y2} and their MSEs for the
# benchmarked z-test to work. If any are missing (e.g. because a non-default
# var_choice/transformation path produced a different column set), we fall
# back to the unbenchmarked results and emit a clear diagnostic, so the
# render still completes.
.bench_required <- c("FH_Bench_y1", "FH_Bench_y2",
                     "FH_Bench_MSE_y1", "FH_Bench_MSE_y2")
.bench_missing <- setdiff(.bench_required, names(merged_data))

if (length(.bench_missing) > 0) {
  cat("\n[significance-test-bench] WARNING: required columns missing in ",
      "merged_data: ", paste(.bench_missing, collapse = ", "), ".\n", sep = "")
  cat("  merged_data has columns: ",
      paste(names(merged_data), collapse = ", "), "\n", sep = "")
  cat("  Falling back to unbenchmarked results for downstream analysis.\n")
  # Build a `results` object with the same schema so downstream chunks don't fail.
  results <- results_unbench %>%
    mutate(
      FH_Bench_y1 = NA_real_,
      FH_Bench_y2 = NA_real_
    ) %>%
    select(
      domain, diff, mse, variance_assumption, alpha, zq, p_value, p_value_bh,
      p_value_bonferroni, significant_unadjusted, significant_bh,
      significant_bonferroni, significance_rule, significant, lb, ub, index,
      FH_Bench_y1, FH_Bench_y2, Direct_y1, Direct_y2
    )
} else {
  results <- merged_data %>%
    mutate(
      diff = FH_Bench_y2 - FH_Bench_y1,
      variance_assumption = "period_independence",
      mse = FH_Bench_MSE_y1 + FH_Bench_MSE_y2,
      mse = ifelse(is.finite(mse) & mse >= 0, mse, NA_real_),
      se = sqrt(mse),
      zq = diff / se,
      alpha = change_alpha,
      z_critical = qnorm(1 - alpha / 2),
      lb = diff - z_critical * se,
      ub = diff + z_critical * se,
      p_value = 2 * stats::pnorm(abs(zq), lower.tail = FALSE),
      p_value_bh = stats::p.adjust(p_value, method = "BH"),
      p_value_bonferroni = stats::p.adjust(p_value, method = "bonferroni"),
      significant_unadjusted = p_value < alpha,
      significant_bh = p_value_bh < alpha,
      significant_bonferroni = p_value_bonferroni < alpha,
      significance_rule = "pointwise_unadjusted",
      significant = significant_unadjusted,
      index = row_number()
    ) %>%
    select(
      domain, diff, mse, variance_assumption, alpha, zq, p_value, p_value_bh,
      p_value_bonferroni, significant_unadjusted, significant_bh,
      significant_bonferroni, significance_rule, significant, lb, ub, index,
      FH_Bench_y1, FH_Bench_y2, Direct_y1, Direct_y2
    )
}

cat("\n=== With Benchmarking (FH_Bench) ===\n")
cat("Total domains analyzed:", nrow(results), "\n")
cat("Statistically significant changes:", sum(results$significant), "\n")
cat("Non-significant changes:", sum(!results$significant), "\n\n")

cat("Positive changes (increase in poverty):", sum(results$diff > 0), "\n")
cat("  - Significant increases:", sum(results$diff > 0 & results$significant), "\n\n")

cat("Negative changes (decrease in poverty):", sum(results$diff < 0), "\n")
cat("  - Significant decreases:", sum(results$diff < 0 & results$significant), "\n")


sig_changes_unbench <- results_unbench %>%
  filter(significant) %>%
  arrange(desc(diff)) %>%
  mutate(
    direction = ifelse(diff > 0, "Increase", "Decrease"),
    CI_95 = paste0("[", round(lb, 4), ", ", round(ub, 4), "]")
  ) %>%
  select(domain, diff, zq, CI_95, direction, FH_y1, FH_y2)

if (nrow(sig_changes_unbench) > 0) {
  suppressWarnings({
    print(kable(sig_changes_unbench,
          digits = 4,
          col.names = c("Domain", "Difference", "Z-statistic", "95% CI",
                        "Direction",
                        paste0("FH ", years_keep[1]),
                        paste0("FH ", years_keep[2])),
          caption = "Significant Changes Without Benchmarking (Î± = 0.05)"))
  })
} else {
  cat("No statistically significant changes detected (unbenchmarked).\n")
}


sig_changes <- results %>%
  filter(significant) %>%
  arrange(desc(diff)) %>%
  mutate(
    direction = ifelse(diff > 0, "Increase", "Decrease"),
    CI_95 = paste0("[", round(lb, 4), ", ", round(ub, 4), "]")
  ) %>%
  select(domain, diff, zq, CI_95, direction, FH_Bench_y1, FH_Bench_y2)

if (nrow(sig_changes) > 0) {
  suppressWarnings({
    print(kable(sig_changes,
          digits = 4,
          col.names = c("Domain", "Difference", "Z-statistic", "95% CI",
                        "Direction",
                        paste0("FH_Bench ", years_keep[1]),
                        paste0("FH_Bench ", years_keep[2])),
          caption = "Significant Changes With Benchmarking (Î± = 0.05)"))
  })
} else {
  cat("No statistically significant changes detected (benchmarked).\n")
}


desc_stats <- data.frame(
  Statistic = c("Mean difference", "Median difference", "Std. deviation",
                "Minimum", "Maximum", "Range"),
  FH = c(
    mean(results_unbench$diff), median(results_unbench$diff), sd(results_unbench$diff),
    min(results_unbench$diff), max(results_unbench$diff),
    max(results_unbench$diff) - min(results_unbench$diff)
  ),
  FH_Bench = c(
    mean(results$diff), median(results$diff), sd(results$diff),
    min(results$diff), max(results$diff),
    max(results$diff) - min(results$diff)
  )
)

suppressWarnings({
  print(kable(desc_stats, digits = 6,
              col.names = c("Statistic", "FH (Unbenchmarked)", "FH Benchmarked"),
              caption = "Descriptive Statistics of Poverty Rate Changes"))
})


comparison_df <- tibble(
  domain = results_unbench$domain,
  sig_unbench = results_unbench$significant,
  sig_bench   = results$significant
) %>%
  mutate(
    status = case_when(
      sig_unbench & sig_bench   ~ "Significant in both",
      !sig_unbench & !sig_bench ~ "Not significant in either",
      sig_unbench & !sig_bench  ~ "Significant only without benchmarking",
      !sig_unbench & sig_bench  ~ "Significant only with benchmarking"
    )
  )

cat("=== Impact of Benchmarking on Significance Conclusions ===\n\n")
status_tbl <- table(comparison_df$status)
for (s in names(status_tbl)) {
  cat(sprintf("  %s: %d domains\n", s, status_tbl[s]))
}

# Show domains where significance changed
changed <- comparison_df %>% filter(sig_unbench != sig_bench)
if (nrow(changed) > 0) {
  cat("\nDomains where benchmarking changed the significance conclusion:\n")
  change_detail <- changed %>%
    left_join(
      tibble(domain = results_unbench$domain,
             diff_unbench = results_unbench$diff,
             diff_bench = results$diff),
      by = "domain"
    )
  suppressWarnings({
    print(kable(change_detail, digits = 4,
                caption = "Domains with Different Significance Conclusions"))
  })
} else {
  cat("\nBenchmarking did not change any significance conclusions.\n")
}


.p_sig_unbench <- ggplot(results_unbench, aes(x = domain, y = diff, color = significant)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.3, alpha = 0.5) +
  scale_color_manual(
    values = c("FALSE" = "gray60", "TRUE" = "red"),
    labels = c("Not Significant", "Significant"),
    name = "Pointwise\nSignificance"
  ) +
  labs(
    title = paste(pov_lab$short, "Changes Between", years_keep[1], "and", years_keep[2],
                  "(Without Benchmarking)"),
    subtitle = "FH estimates with 95% Confidence Intervals",
    x = "Domain",
    y = paste0("Change in Poverty Rate (", years_keep[2], " - ", years_keep[1], ")"),
    caption = "Red points indicate pointwise significance (unadjusted p < 0.05); bars are pointwise 95% confidence intervals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )
ggsave(here::here("outputs", "figures", "ufh_significance_unbench.png"),
       .p_sig_unbench, width = 14, height = 6, dpi = 300)

.p_sig_bench <- ggplot(results, aes(x = domain, y = diff, color = significant)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.3, alpha = 0.5) +
  scale_color_manual(
    values = c("FALSE" = "gray60", "TRUE" = "red"),
    labels = c("Not Significant", "Significant"),
    name = "Pointwise\nSignificance"
  ) +
  labs(
    title = paste(pov_lab$short, "Changes Between", years_keep[1], "and", years_keep[2],
                  "(With Benchmarking)"),
    subtitle = "FH Benchmarked estimates with 95% Confidence Intervals",
    x = "Domain",
    y = paste0("Change in Poverty Rate (", years_keep[2], " - ", years_keep[1], ")"),
    caption = "Red points indicate pointwise significance (unadjusted p < 0.05); bars are pointwise 95% confidence intervals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )
ggsave(here::here("outputs", "figures", "ufh_significance_bench.png"),
       .p_sig_bench, width = 14, height = 6, dpi = 300)


.p_hist_unbench <- ggplot(results_unbench, aes(x = diff, fill = significant)) +
  geom_histogram(bins = 20, alpha = 0.7, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(
    values = c("FALSE" = "lightblue", "TRUE" = "coral"),
    labels = c("Not Significant", "Significant"),
    name = "Pointwise\nSignificance"
  ) +
  labs(
    title = "Distribution of Poverty Rate Changes (Without Benchmarking)",
    x = paste0("Change in Poverty Rate (", years_keep[2], " - ", years_keep[1], ")"),
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
print(.p_hist_unbench)
ggsave(here::here("outputs", "figures", "ufh_change_hist_unbench.png"),
       .p_hist_unbench, width = 10, height = 6, dpi = 150)

.p_hist_bench <- ggplot(results, aes(x = diff, fill = significant)) +
  geom_histogram(bins = 20, alpha = 0.7, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(
    values = c("FALSE" = "lightblue", "TRUE" = "coral"),
    labels = c("Not Significant", "Significant"),
    name = "Pointwise\nSignificance"
  ) +
  labs(
    title = "Distribution of Poverty Rate Changes (With Benchmarking)",
    x = paste0("Change in Poverty Rate (", years_keep[2], " - ", years_keep[1], ")"),
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
print(.p_hist_bench)
ggsave(here::here("outputs", "figures", "ufh_change_hist_bench.png"),
       .p_hist_bench, width = 10, height = 6, dpi = 150)



longpov_dt <- pov_fh_combined |>
  select(domain, year, modelpov = FH_Bench) |>
  mutate(
    domain = trimws(as.character(domain)),
    year = as.numeric(year),
    modelpov = as.numeric(modelpov)
  )

growth_dt <- longpov_dt |>
  group_by(domain) |>
  group_modify(~ {
    x <- .x[is.finite(.x$year) & is.finite(.x$modelpov), , drop = FALSE]
    denom <- mean(x$modelpov, na.rm = TRUE)
    growth <- if (nrow(x) >= 2 && is.finite(denom) && abs(denom) > 1e-12) {
      unname(coef(lm(modelpov ~ year, data = x))[2] / denom)
    } else {
      NA_real_
    }
    tibble(growth_rate = growth)
  }) |>
  ungroup()

shp_dt <- shp_dt |>
  mutate(domain = trimws(as.character(domain))) |>
  left_join(growth_dt, by = "domain")

# Cap growth rates at 1 to reduce the influence of extreme outliers
shp_dt <- shp_dt |>
  mutate(growth_rate_capped = pmin(growth_rate, 1))

.growth_limit_min <- min(shp_dt$growth_rate_capped, na.rm = TRUE)
if (!is.finite(.growth_limit_min)) .growth_limit_min <- 0
.growth_limit_min <- min(.growth_limit_min, 0.5)
if (.growth_limit_min >= 0.5) .growth_limit_min <- 0.5 - 1e-6

.p_growth_ufh <- ggplot(shp_dt) +
  geom_sf(aes(fill = growth_rate_capped), color = NA) +
  scale_fill_viridis(
    name   = "Growth Rate (capped at 1)",
    limits = c(.growth_limit_min, 0.5),
    oob    = squish,
    option = "magma"
  ) +
  labs(
    title    = paste("Spatial Distribution of", pov_lab$short, "Growth Rates"),
    subtitle = "Growth rates capped at 1 to reduce outlier influence",
    caption = sae_map_caption(.map_attribution, "Data source: Author calculation")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 14),
    legend.title    = element_text(size = 16),
    plot.title      = element_text(size = 18, face = "bold"),
    plot.subtitle   = element_text(size = 14),
    plot.caption    = element_text(size = 12),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    panel.grid      = element_blank()
  )
print(.p_growth_ufh)
ggsave(here::here("outputs", "figures", "ufh_growth_rate_map.png"),
       .p_growth_ufh, width = 12, height = 10, dpi = 150)


write_csv(results_unbench, here::here("outputs", "tables", "statistical_significance_results_unbench.csv"))
if (isTRUE(do_benchmark)) {
  write_csv(results, here::here("outputs", "tables", "statistical_significance_results.csv"))
}
cat("Results saved to:\n")
cat("  - outputs/tables/statistical_significance_results_unbench.csv (FH, without benchmarking)\n")
if (isTRUE(do_benchmark)) {
  cat("  - outputs/tables/statistical_significance_results.csv (FH_Bench, with benchmarking)\n")
}


sessionInfo()
