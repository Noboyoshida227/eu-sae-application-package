# ============================================================
# pipeline_helpers.R -- Shared helpers for model selection,
# diagnostics, poverty-line handling, and export enrichment
# ============================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
}

sae_as_numeric_or_na <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

sae_scalar_numeric_or_na <- function(x, column = NULL) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  if (!is.null(column) && is.data.frame(x) && column %in% names(x)) {
    x <- x[[column]]
  }
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0L || !is.finite(x[[1]])) NA_real_ else x[[1]]
}

sae_normalize_povline_map <- function(povline_value, years_keep = NULL) {
  years_chr <- as.character(years_keep %||% character())

  if (is.null(povline_value) || length(povline_value) == 0) {
    out <- numeric(0)
  } else if (is.list(povline_value) && !is.data.frame(povline_value)) {
    out <- sae_as_numeric_or_na(unlist(povline_value, use.names = TRUE))
  } else {
    out <- sae_as_numeric_or_na(povline_value)
    names(out) <- names(povline_value)
  }

  if (length(out) == 1L && length(years_chr) > 0L &&
      (is.null(names(out)) || !nzchar(names(out)[1] %||% ""))) {
    out <- stats::setNames(rep(out[[1]], length(years_chr)), years_chr)
  }

  if (length(out) > 0L && (is.null(names(out)) || any(!nzchar(names(out))))) {
    if (length(years_chr) == length(out)) {
      names(out) <- years_chr
    }
  }

  out
}

sae_validate_numeric_poverty_lines <- function(povline_value, years_keep) {
  years_chr <- as.character(years_keep)
  vals <- sae_normalize_povline_map(povline_value, years_chr)

  if (length(years_chr) == 0L) {
    stop("Numeric poverty-line validation requires at least one analysis year.",
         call. = FALSE)
  }
  if (length(vals) == 1L && length(years_chr) > 1L) {
    vals <- stats::setNames(rep(vals[[1]], length(years_chr)), years_chr)
  }

  missing_years <- setdiff(years_chr, names(vals))
  if (length(missing_years) > 0L) {
    stop(
      "A positive numeric poverty line is required for each analysis year. Missing: ",
      paste(missing_years, collapse = ", "),
      call. = FALSE
    )
  }

  vals <- vals[years_chr]
  bad <- !is.finite(vals) | vals <= 0
  if (any(bad)) {
    stop(
      "Numeric poverty lines must be positive finite values. Check year(s): ",
      paste(years_chr[bad], collapse = ", "),
      call. = FALSE
    )
  }
  vals
}

sae_apply_numeric_poverty_lines <- function(data, povline_value,
                                            year_col = "year",
                                            years_keep = NULL,
                                            output_col = "povline") {
  if (!year_col %in% names(data)) {
    stop("Cannot apply year-specific poverty lines because year column '",
         year_col, "' is missing.", call. = FALSE)
  }
  if (is.null(years_keep)) {
    supplied <- sae_normalize_povline_map(povline_value, NULL)
    years <- if (!is.null(names(supplied)) && any(nzchar(names(supplied)))) {
      sae_as_numeric_or_na(names(supplied))
    } else {
      sort(unique(sae_as_numeric_or_na(data[[year_col]])))
    }
  } else {
    years <- years_keep
  }
  vals <- sae_validate_numeric_poverty_lines(povline_value, years)
  yr_chr <- as.character(sae_as_numeric_or_na(data[[year_col]]))
  data[[output_col]] <- as.numeric(vals[yr_chr])
  data
}

sae_poverty_line_log_message <- function(povline_type, povline_value, years_keep) {
  if (!identical(povline_type, "numeric")) {
    return("Poverty line source: column in survey data.")
  }
  vals <- sae_validate_numeric_poverty_lines(povline_value, years_keep)
  paste(
    "Poverty lines by year:",
    paste(sprintf("%s=%s", names(vals), format(vals, trim = TRUE)),
          collapse = ", ")
  )
}

sae_weighted_mean <- function(x, w) {
  x <- sae_as_numeric_or_na(x)
  w <- sae_as_numeric_or_na(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

sae_domain_population_summary <- function(survey_data,
                                          years_keep = NULL,
                                          domain_col = "domain",
                                          year_col = "year",
                                          weight_col = "population_weight") {
  if (is.null(survey_data) ||
      !all(c(domain_col, year_col, weight_col) %in% names(survey_data))) {
    return(data.frame(domain = character(), year = integer(),
                      population = numeric(), stringsAsFactors = FALSE))
  }
  df <- data.frame(
    domain = trimws(as.character(survey_data[[domain_col]])),
    year = sae_as_numeric_or_na(survey_data[[year_col]]),
    weight = sae_as_numeric_or_na(survey_data[[weight_col]]),
    stringsAsFactors = FALSE
  )
  if (!is.null(years_keep) && length(years_keep) > 0L) {
    df <- df[df$year %in% as.integer(years_keep), , drop = FALSE]
  }
  df <- df[is.finite(df$year) & nzchar(df$domain) & is.finite(df$weight), ,
           drop = FALSE]
  if (nrow(df) == 0L) {
    return(data.frame(domain = character(), year = integer(),
                      population = numeric(), stringsAsFactors = FALSE))
  }
  out <- stats::aggregate(
    weight ~ domain + year,
    data = df,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  names(out)[names(out) == "weight"] <- "population"
  out$year <- as.integer(out$year)
  out
}

sae_poverty_line_summary <- function(survey_data,
                                     years_keep = NULL,
                                     domain_col = "domain",
                                     year_col = "year",
                                     povline_col = "povline",
                                     weight_col = "population_weight") {
  if (is.null(survey_data) ||
      !all(c(domain_col, year_col, povline_col) %in% names(survey_data))) {
    return(data.frame(domain = character(), year = integer(),
                      poverty_line_used = numeric(),
                      poverty_line_min = numeric(),
                      poverty_line_max = numeric(),
                      stringsAsFactors = FALSE))
  }
  weight <- if (weight_col %in% names(survey_data)) {
    survey_data[[weight_col]]
  } else {
    rep(1, nrow(survey_data))
  }
  df <- data.frame(
    domain = trimws(as.character(survey_data[[domain_col]])),
    year = sae_as_numeric_or_na(survey_data[[year_col]]),
    povline = sae_as_numeric_or_na(survey_data[[povline_col]]),
    weight = sae_as_numeric_or_na(weight),
    stringsAsFactors = FALSE
  )
  if (!is.null(years_keep) && length(years_keep) > 0L) {
    df <- df[df$year %in% as.integer(years_keep), , drop = FALSE]
  }
  df <- df[is.finite(df$year) & nzchar(df$domain) & is.finite(df$povline), ,
           drop = FALSE]
  if (nrow(df) == 0L) {
    return(data.frame(domain = character(), year = integer(),
                      poverty_line_used = numeric(),
                      poverty_line_min = numeric(),
                      poverty_line_max = numeric(),
                      stringsAsFactors = FALSE))
  }
  split_key <- interaction(df$domain, df$year, drop = TRUE, lex.order = TRUE)
  parts <- split(df, split_key)
  out <- do.call(rbind, lapply(parts, function(part) {
    data.frame(
      domain = part$domain[[1]],
      year = as.integer(part$year[[1]]),
      poverty_line_used = sae_weighted_mean(part$povline, part$weight),
      poverty_line_min = min(part$povline, na.rm = TRUE),
      poverty_line_max = max(part$povline, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

sae_domain_name_columns <- function(df, include_generic = TRUE) {
  if (is.null(df) || !length(names(df))) return(character())
  specific <- c(
    "geographic_name", "domain_name", "area_name", "province_name",
    "prov_name", "provlab",
    "NUTS_NAME", "nuts_name", "shapeName", "shape_name"
  )
  generic <- c("label", "name", "NAME", "Name")
  candidates <- if (isTRUE(include_generic)) c(specific, generic) else specific
  intersect(candidates, names(df))
}

sae_build_domain_metadata <- function(...) {
  sources <- list(...)
  rows <- list()
  for (src in sources) {
    if (is.null(src) || !"domain" %in% names(src)) next
    name_cols <- sae_domain_name_columns(src)
    if (length(name_cols) == 0L) next
    nm <- name_cols[[1]]
    part <- data.frame(
      domain = trimws(as.character(src$domain)),
      geographic_name = trimws(as.character(src[[nm]])),
      stringsAsFactors = FALSE
    )
    part <- part[nzchar(part$domain) & nzchar(part$geographic_name) &
                   !is.na(part$geographic_name), , drop = FALSE]
    if (nrow(part) > 0L) rows[[length(rows) + 1L]] <- unique(part)
  }
  if (length(rows) == 0L) {
    return(data.frame(domain = character(), geographic_name = character(),
                      stringsAsFactors = FALSE))
  }
  all_rows <- do.call(rbind, rows)
  all_rows <- all_rows[!duplicated(all_rows$domain), , drop = FALSE]
  rownames(all_rows) <- NULL
  all_rows
}

sae_drop_domain_label_columns <- function(df) {
  label_cols <- sae_domain_name_columns(df, include_generic = FALSE)
  label_cols <- setdiff(label_cols, c("domain", "geographic_name"))
  out <- df[, setdiff(names(df), label_cols), drop = FALSE]
  # sf column subsetting can drop custom attributes required for map credits.
  for (key in c("boundary_attribution", "boundary_provenance")) {
    attr(out, key) <- attr(df, key, exact = TRUE)
  }
  out
}

sae_add_precision_columns <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  out <- as.data.frame(df, stringsAsFactors = FALSE)

  add_rmse <- function(mse_col, rmse_col) {
    if (!mse_col %in% names(out) || rmse_col %in% names(out)) return()
    mse <- sae_as_numeric_or_na(out[[mse_col]])
    out[[rmse_col]] <<- sqrt(pmax(mse, 0))
  }
  add_cv <- function(mse_col, estimate_col, cv_col) {
    if (!all(c(mse_col, estimate_col) %in% names(out)) ||
        cv_col %in% names(out)) return()
    mse <- sae_as_numeric_or_na(out[[mse_col]])
    estimate <- sae_as_numeric_or_na(out[[estimate_col]])
    se <- sqrt(pmax(mse, 0))
    out[[cv_col]] <<- ifelse(is.finite(se) & is.finite(estimate) &
                                abs(estimate) > 1e-12,
                              se / abs(estimate), NA_real_)
  }

  for (mse_col in names(out)) {
    if (grepl("_MSE$", mse_col)) {
      stem <- sub("_MSE$", "", mse_col)
      add_rmse(mse_col, paste0(stem, "_RMSE"))
      add_cv(mse_col, stem, paste0(stem, "_CV"))
    } else if (grepl("^mse_", mse_col)) {
      stem <- sub("^mse_", "", mse_col)
      add_rmse(mse_col, paste0("rmse_", stem))
      add_cv(mse_col, paste0("rate_", stem), paste0("cv_", stem))
    } else if (identical(mse_col, "direct_mse")) {
      add_rmse(mse_col, "direct_rmse")
      add_cv(mse_col, "direct_rate", "direct_cv")
    } else if (identical(mse_col, "mse")) {
      add_rmse(mse_col, "rmse")
    }
  }
  out
}

sae_enrich_result_table <- function(df,
                                    domain_metadata = NULL,
                                    population = NULL,
                                    poverty_lines = NULL) {
  if (is.null(df) || nrow(df) == 0L || !"domain" %in% names(df)) return(df)
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  out$.sae_row_order <- seq_len(nrow(out))
  out$geographic_identifier <- out$domain

  if (!is.null(domain_metadata) && nrow(domain_metadata) > 0L) {
    out <- merge(out, unique(domain_metadata), by = "domain", all.x = TRUE,
                 sort = FALSE)
  }
  if (!"geographic_name" %in% names(out)) out$geographic_name <- NA_character_

  if (!is.null(population) && nrow(population) > 0L &&
      all(c("domain", "year") %in% names(out)) &&
      all(c("domain", "year", "population") %in% names(population))) {
    out <- merge(out, unique(population), by = c("domain", "year"),
                 all.x = TRUE, sort = FALSE)
  }

  if (!is.null(poverty_lines) && nrow(poverty_lines) > 0L &&
      all(c("domain", "year") %in% names(out)) &&
      all(c("domain", "year") %in% names(poverty_lines))) {
    poverty_lines <- as.data.frame(poverty_lines, stringsAsFactors = FALSE)
    if ("poverty_line" %in% names(poverty_lines) &&
        !"poverty_line_used" %in% names(poverty_lines)) {
      names(poverty_lines)[names(poverty_lines) == "poverty_line"] <-
        "poverty_line_used"
    }
    out <- merge(out, unique(poverty_lines), by = c("domain", "year"),
                 all.x = TRUE, sort = FALSE)
  }

  out <- sae_add_precision_columns(out)
  out <- out[order(out$.sae_row_order), , drop = FALSE]
  out$.sae_row_order <- NULL
  first_cols <- intersect(
    c("geographic_identifier", "geographic_name", "domain", "year",
      "population", "poverty_line_used", "poverty_line_min",
      "poverty_line_max"),
    names(out)
  )
  out[, c(first_cols, setdiff(names(out), first_cols)), drop = FALSE]
}

sae_deparse_formula <- function(x) paste(deparse(x), collapse = "")

sae_cv_foldid <- function(n, nfolds, seed = 123L) {
  n <- as.integer(n)
  nfolds <- as.integer(nfolds)
  seed <- suppressWarnings(as.integer(seed))
  if (!is.finite(seed)) seed <- 123L
  if (n < 1L || nfolds < 2L || nfolds > n) {
    stop("Invalid cross-validation dimensions.", call. = FALSE)
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  sample(rep(seq_len(nfolds), length.out = n))
}

sae_lasso_screen <- function(dt, xvars, y, enabled = FALSE,
                             lambda_choice = "lambda.1se",
                             label = "model", seed = 123L) {
  df <- as.data.frame(dt, stringsAsFactors = FALSE)
  original <- unique(xvars[xvars %in% names(df)])
  diag <- data.frame(
    model = label,
    outcome = y,
    lasso_enabled = isTRUE(enabled),
    lambda_choice = lambda_choice %||% "lambda.1se",
    seed = suppressWarnings(as.integer(seed)),
    n_candidates = length(original),
    n_numeric_candidates = NA_integer_,
    n_complete_cases = NA_integer_,
    n_selected = NA_integer_,
    selected_variables = "",
    stepwise_candidates = "",
    fallback_used = FALSE,
    note = "",
    stringsAsFactors = FALSE
  )
  set_stepwise <- function(vars) {
    vars <- unique(vars[vars %in% names(df)])
    diag$stepwise_candidates <<- paste(vars, collapse = ", ")
    vars
  }
  if (!isTRUE(enabled)) {
    diag$n_numeric_candidates <- sum(vapply(df[, original, drop = FALSE],
                                            is.numeric, logical(1)))
    diag$note <- "LASSO screening disabled; stepwise used original candidate set."
    return(list(vars = set_stepwise(original), diagnostics = diag))
  }
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required for LASSO screening. Run install_packages.R and try again.",
         call. = FALSE)
  }
  numeric_vars <- original[vapply(df[, original, drop = FALSE],
                                  is.numeric, logical(1))]
  numeric_vars <- numeric_vars[vapply(numeric_vars, function(v) {
    x <- sae_as_numeric_or_na(df[[v]])
    x <- x[is.finite(x)]
    length(x) > 0L && length(unique(x)) > 1L
  }, logical(1))]
  diag$n_numeric_candidates <- length(numeric_vars)
  if (length(numeric_vars) == 0L) {
    diag$fallback_used <- TRUE
    diag$note <- "No nonconstant numeric candidate predictors were available for LASSO; stepwise will use an intercept-only model."
    return(list(vars = set_stepwise(character()), diagnostics = diag))
  }
  use_df <- as.data.frame(df[, c(y, numeric_vars), drop = FALSE])
  use_df <- use_df[stats::complete.cases(use_df), , drop = FALSE]
  diag$n_complete_cases <- nrow(use_df)
  if (nrow(use_df) < 5L) {
    diag$fallback_used <- TRUE
    diag$note <- "Too few complete cases for LASSO; stepwise used cleaned numeric candidate set."
    return(list(vars = set_stepwise(numeric_vars), diagnostics = diag))
  }
  complete_nzv <- vapply(use_df[, numeric_vars, drop = FALSE], function(x) {
    x <- sae_as_numeric_or_na(x)
    x <- x[is.finite(x)]
    length(unique(x)) > 1L
  }, logical(1))
  numeric_vars <- numeric_vars[complete_nzv]
  diag$n_numeric_candidates <- length(numeric_vars)
  if (length(numeric_vars) == 0L) {
    diag$fallback_used <- TRUE
    diag$note <- "All numeric candidates were constant after complete-case filtering; stepwise will use an intercept-only model."
    return(list(vars = set_stepwise(character()), diagnostics = diag))
  }
  use_df <- use_df[, c(y, numeric_vars), drop = FALSE]
  if (length(numeric_vars) < 2L) {
    diag$fallback_used <- TRUE
    diag$note <- "Only one cleaned numeric candidate was available; LASSO skipped and stepwise used that candidate."
    return(list(vars = set_stepwise(numeric_vars), diagnostics = diag))
  }

  x <- as.matrix(use_df[, numeric_vars, drop = FALSE])
  yv <- sae_as_numeric_or_na(use_df[[y]])
  if (!any(is.finite(yv)) || length(unique(yv[is.finite(yv)])) < 2L) {
    diag$fallback_used <- TRUE
    diag$note <- "Outcome had insufficient variation for LASSO; stepwise used cleaned numeric candidate set."
    return(list(vars = set_stepwise(numeric_vars), diagnostics = diag))
  }
  nfolds <- min(10L, max(3L, floor(nrow(use_df) / 2L)))
  nfolds <- min(nfolds, nrow(use_df))
  seed <- suppressWarnings(as.integer(seed))
  if (!is.finite(seed)) seed <- 123L
  diag$seed <- seed
  foldid <- sae_cv_foldid(nrow(use_df), nfolds, seed)
  fit <- tryCatch(
    glmnet::cv.glmnet(x, yv, alpha = 1, family = "gaussian",
                      standardize = TRUE, nfolds = nfolds, foldid = foldid),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    diag$fallback_used <- TRUE
    diag$note <- paste("LASSO failed; stepwise used cleaned numeric candidate set:",
                       conditionMessage(fit))
    return(list(vars = set_stepwise(numeric_vars), diagnostics = diag))
  }
  s <- if (lambda_choice %in% c("lambda.min", "lambda.1se")) {
    lambda_choice
  } else {
    "lambda.1se"
  }
  cf <- stats::coef(fit, s = s)
  selected <- rownames(cf)[as.numeric(cf[, 1]) != 0]
  selected <- setdiff(selected, "(Intercept)")
  selected <- intersect(selected, numeric_vars)
  diag$n_selected <- length(selected)
  diag$selected_variables <- paste(selected, collapse = ", ")
  if (length(selected) == 0L) {
    diag$fallback_used <- TRUE
    diag$note <- "LASSO selected no predictors; stepwise used cleaned numeric candidate set."
    return(list(vars = set_stepwise(numeric_vars), diagnostics = diag))
  }
  diag$note <- "LASSO-screened candidate set passed to AIC/BIC stepwise selection."
  list(vars = set_stepwise(selected), diagnostics = diag)
}

sae_model_fit_diagnostics <- function(method, year, formula, data,
                                      mixed_model = NULL,
                                      random_variance = NA_real_,
                                      note = "") {
  rhs_terms <- attr(stats::terms(formula), "term.labels")
  out <- data.frame(
    method = method,
    year = as.integer(year),
    formula = sae_deparse_formula(formula),
    n_domains = NA_integer_,
    n_predictors = length(rhs_terms),
    AIC = NA_real_,
    BIC = NA_real_,
    logLik = NA_real_,
    residual_sd = NA_real_,
    random_effect_variance = random_variance,
    ols_companion_r2 = NA_real_,
    ols_companion_adj_r2 = NA_real_,
    note = note,
    stringsAsFactors = FALSE
  )
  fit <- tryCatch(stats::lm(formula, data = data), error = function(e) NULL)
  if (!is.null(fit)) {
    sm <- summary(fit)
    out$n_domains <- stats::nobs(fit)
    out$AIC <- sae_scalar_numeric_or_na(
      tryCatch(stats::AIC(fit), error = function(e) NA_real_),
      column = "AIC"
    )
    out$BIC <- sae_scalar_numeric_or_na(
      tryCatch(stats::BIC(fit), error = function(e) NA_real_),
      column = "BIC"
    )
    out$logLik <- sae_scalar_numeric_or_na(
      tryCatch(stats::logLik(fit), error = function(e) NA_real_)
    )
    out$residual_sd <- sm$sigma %||% NA_real_
    out$ols_companion_r2 <- sm$r.squared %||% NA_real_
    out$ols_companion_adj_r2 <- sm$adj.r.squared %||% NA_real_
  }
  if (!is.null(mixed_model)) {
    mixed_aic <- sae_scalar_numeric_or_na(
      tryCatch(stats::AIC(mixed_model), error = function(e) NA_real_),
      column = "AIC"
    )
    mixed_bic <- sae_scalar_numeric_or_na(
      tryCatch(stats::BIC(mixed_model), error = function(e) NA_real_),
      column = "BIC"
    )
    mixed_loglik <- sae_scalar_numeric_or_na(
      tryCatch(stats::logLik(mixed_model), error = function(e) NA_real_)
    )
    if (is.finite(mixed_aic)) out$AIC <- mixed_aic
    if (is.finite(mixed_bic)) out$BIC <- mixed_bic
    if (is.finite(mixed_loglik)) out$logLik <- mixed_loglik
  }
  out
}

# Effective sample size used by the arcsine Fay-Herriot transformation.
# A design effect cannot be estimated when both the design-based and SRS
# variances are zero (most commonly when a sampled domain has a direct rate of
# exactly 0 or 1). In that boundary case, use the observed domain sample size,
# which gives the standard arcsine working variance 1 / (4 * N), instead of
# allowing 0 / 0 to propagate as NaN into the covariance matrix.
sae_effective_sample_size <- function(sample_size, design_variance,
                                      srs_variance) {
  sample_size <- suppressWarnings(as.numeric(sample_size))
  design_variance <- suppressWarnings(as.numeric(design_variance))
  srs_variance <- suppressWarnings(as.numeric(srs_variance))

  if (!(length(sample_size) == length(design_variance) &&
        length(sample_size) == length(srs_variance))) {
    stop("sample_size, design_variance, and srs_variance must have equal lengths.",
         call. = FALSE)
  }
  if (any(!is.finite(sample_size) | sample_size <= 0)) {
    stop("Effective-sample-size calculation requires finite positive domain sample sizes.",
         call. = FALSE)
  }

  design_effect <- design_variance / srs_variance
  n_eff <- sample_size / design_effect
  fallback_used <- !is.finite(design_effect) | design_effect <= 0 |
                   !is.finite(n_eff) | n_eff <= 0

  design_effect[fallback_used] <- 1
  n_eff[fallback_used] <- sample_size[fallback_used]

  data.frame(
    deff = design_effect,
    n_eff = n_eff,
    effective_sample_size_fallback = fallback_used,
    stringsAsFactors = FALSE
  )
}

# ---- Mapping-collision resolution -------------------------------------------
# `rename(!!!rename_map)` fails with an opaque tidyselect error when a column
# already bears a canonical name while a *different* column is mapped to it
# (e.g. the file has a `strata` column but the user mapped `region` to strata).
# The user's mapping is the instruction, so the mapped column takes the
# canonical name and the pre-existing column is set aside as `<name>_original`,
# announced in the step log. A column that the same map renames away needs no
# treatment - the simultaneous rename handles it correctly.
sae_resolve_rename_collisions <- function(df, rename_map, context = "") {
  targets <- names(rename_map)
  sources <- unname(rename_map)
  for (i in seq_along(rename_map)) {
    target <- targets[[i]]
    source_col <- sources[[i]]
    if (identical(target, source_col)) next
    if (target %in% names(df) && !target %in% sources) {
      alt <- paste0(target, "_original")
      while (alt %in% names(df) || alt %in% targets) alt <- paste0(alt, "_")
      cat(sprintf(
        paste0("NOTE%s: the input already has a column named '%s', but the ",
               "variable mapping assigns '%s' to that role. Keeping the ",
               "mapped column '%s' and renaming the pre-existing '%s' to ",
               "'%s'. If the pre-existing column was the one you wanted, ",
               "select it in the variable mapping instead.\n"),
        if (nzchar(context)) paste0(" [", context, "]") else "",
        target, source_col, source_col, target, alt))
      names(df)[names(df) == target] <- alt
    }
  }
  df
}

# ---- Covariate type guard ---------------------------------------------------
# Fay-Herriot covariates pass through a correlation screen and a linear model,
# so a non-numeric column in the auxiliary file (a region label, a name, a date
# read as text) aborts variable selection with the opaque error
# `cor(xmat) : 'x' must be numeric`. Keep only columns a model can use, and say
# which ones were set aside and why.
sae_numeric_candidates <- function(df, vars, context = "") {
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0L) return(vars)
  usable <- vapply(vars, function(v) {
    col <- df[[v]]
    (is.numeric(col) || is.logical(col)) && !inherits(col, "sfc")
  }, logical(1))
  dropped <- vars[!usable]
  if (length(dropped) > 0L) {
    cat(sprintf(
      paste0("NOTE%s: %d auxiliary column(s) are not numeric and cannot serve ",
             "as Fay-Herriot covariates: %s. They are ignored for variable ",
             "selection. Recode them as numeric indicators if the model should ",
             "use them.\n"),
      if (nzchar(context)) paste0(" [", context, "]") else "",
      length(dropped), paste(dropped, collapse = ", ")))
  }
  vars[usable]
}

# ---- Stratum identifier normalisation ---------------------------------------
# survey::degf() derives design degrees of freedom via as.numeric(strata).
# Character stratum labels ("R1", "North_urban") coerce to NA there, which emits
# "NAs introduced by coercion" and can distort the degrees of freedom used for
# every direct-estimate confidence interval. Stable integer codes preserve the
# stratification exactly while keeping survey's internals numeric.
.sae_notified <- new.env(parent = emptyenv())
sae_strata_codes <- function(x, context = "") {
  if (is.numeric(x)) return(x)
  codes <- as.integer(factor(as.character(x)))
  key <- paste0("strata:", context)
  if (is.null(.sae_notified[[key]])) {
    cat(sprintf(
      paste0("NOTE%s: stratum labels are not numeric; converted to %d integer ",
             "stratum codes for the survey design (stratification unchanged).\n"),
      if (nzchar(context)) paste0(" [", context, "]") else "",
      length(unique(codes[!is.na(codes)]))))
    assign(key, TRUE, envir = .sae_notified)
  }
  codes
}

# Boundary attribution belongs to the selected geometry, never to a hard-coded country.
sae_boundary_attribution <- function(x) {
  value <- if (is.character(x)) x else attr(x, "boundary_attribution", exact = TRUE)
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) return(NULL)
  trimws(value)
}
sae_map_caption <- function(boundary, caption = NULL) {
  parts <- c(caption, sae_boundary_attribution(boundary))
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (!length(parts)) return(NULL)
  paste(parts, collapse = "\n")
}
