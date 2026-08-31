# Helpers used when a user selects MFH3. They select between MFH2 and MFH3
# following the workflow described by Molina and Romero (2025). These
# functions are deliberately independent of msae so that the decision rule
# can be unit-tested without fitting a model.

sae_mfh_model_converged <- function(model) {
  is.list(model) && is.list(model$fit) && isTRUE(model$fit$convergence)
}

.sae_refvar_column <- function(x, patterns) {
  nm <- names(x)
  if (is.null(nm)) return(NULL)
  normalized <- tolower(gsub("[^a-z0-9]+", "", nm))
  for (pattern in patterns) {
    hit <- which(grepl(pattern, normalized, perl = TRUE))
    if (length(hit)) return(nm[[hit[[1L]]]])
  }
  NULL
}

sae_mfh3_refvar_test_table <- function(model_mfh3, alpha = 0.05) {
  raw <- if (is.list(model_mfh3) && is.list(model_mfh3$fit)) {
    model_mfh3$fit$refvarTest
  } else {
    NULL
  }
  if (is.null(raw) || length(raw) == 0L || all(is.na(raw))) {
    return(data.frame())
  }

  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  p_col <- .sae_refvar_column(raw, c("^pvalue$", "^pval$", "pvalue"))
  if (is.null(p_col)) return(data.frame())
  comparison_col <- .sae_refvar_column(
    raw, c("^refvar$", "comparison", "contrast", "pair")
  )
  statistic_col <- .sae_refvar_column(
    raw, c("^ttest$", "statistic", "^z$", "^t$")
  )

  p_raw <- suppressWarnings(as.numeric(raw[[p_col]]))
  comparison <- if (!is.null(comparison_col)) {
    as.character(raw[[comparison_col]])
  } else {
    paste0("variance contrast ", seq_len(nrow(raw)))
  }
  statistic <- if (!is.null(statistic_col)) {
    suppressWarnings(as.numeric(raw[[statistic_col]]))
  } else {
    rep(NA_real_, nrow(raw))
  }

  out <- data.frame(
    comparison = comparison,
    statistic = statistic,
    p_value_raw = p_raw,
    stringsAsFactors = FALSE
  )
  finite <- is.finite(out$p_value_raw)
  out$p_value_bonferroni <- NA_real_
  out$p_value_bh <- NA_real_
  if (any(finite)) {
    out$p_value_bonferroni[finite] <- stats::p.adjust(
      out$p_value_raw[finite], method = "bonferroni"
    )
    out$p_value_bh[finite] <- stats::p.adjust(
      out$p_value_raw[finite], method = "BH"
    )
  }
  out$significant_raw <- finite & out$p_value_raw < alpha
  out$significant_bonferroni <- finite & out$p_value_bonferroni < alpha
  out$significant_bh <- finite & out$p_value_bh < alpha
  out
}

sae_choose_mfh_variance_structure <- function(model_mfh3,
                                              alpha = 0.05,
                                              adjustment = "bonferroni",
                                              fit_error = NULL) {
  adjustment <- match.arg(tolower(adjustment),
                          c("bonferroni", "bh", "none"))
  alpha <- as.numeric(alpha)[1L]
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a finite number strictly between 0 and 1.",
         call. = FALSE)
  }

  empty_tests <- data.frame()
  error_message <- if (inherits(fit_error, "condition")) {
    conditionMessage(fit_error)
  } else if (!is.null(fit_error) && length(fit_error)) {
    as.character(fit_error)[1L]
  } else {
    NA_character_
  }

  if (is.null(model_mfh3)) {
    code <- if (is.na(error_message)) "mfh3_unavailable" else "mfh3_error"
    reason <- if (is.na(error_message)) {
      "MFH3 did not return a model object; MFH2 was selected."
    } else {
      paste0("MFH3 raised an estimation error; MFH2 was selected. Error: ",
             error_message)
    }
    return(list(
      requested_model = "MFH3_CANDIDATE",
      selected_model = "MFH2",
      reason_code = code,
      reason = reason,
      mfh3_converged = FALSE,
      test_available = FALSE,
      adjustment = adjustment,
      alpha = alpha,
      tests = empty_tests
    ))
  }

  if (!sae_mfh_model_converged(model_mfh3)) {
    return(list(
      requested_model = "MFH3_CANDIDATE",
      selected_model = "MFH2",
      reason_code = "mfh3_nonconvergence",
      reason = paste0(
        "MFH3 returned a model object but did not report successful ",
        "convergence; MFH2 was selected automatically."
      ),
      mfh3_converged = FALSE,
      test_available = FALSE,
      adjustment = adjustment,
      alpha = alpha,
      tests = empty_tests
    ))
  }

  tests <- sae_mfh3_refvar_test_table(model_mfh3, alpha = alpha)
  if (!nrow(tests) || !any(is.finite(tests$p_value_raw))) {
    return(list(
      requested_model = "MFH3_CANDIDATE",
      selected_model = "MFH2",
      reason_code = "refvar_test_unavailable",
      reason = paste0(
        "MFH3 converged, but its random-effect variance homogeneity test ",
        "was unavailable; MFH2 was selected conservatively."
      ),
      mfh3_converged = TRUE,
      test_available = FALSE,
      adjustment = adjustment,
      alpha = alpha,
      tests = tests
    ))
  }

  decision_column <- switch(
    adjustment,
    bonferroni = "p_value_bonferroni",
    bh = "p_value_bh",
    none = "p_value_raw"
  )
  reject <- any(tests[[decision_column]] < alpha, na.rm = TRUE)
  selected <- if (reject) "MFH3" else "MFH2"
  reason_code <- if (reject) {
    "heteroscedasticity_supported"
  } else {
    "homoscedasticity_not_rejected"
  }
  reason <- if (reject) {
    sprintf(
      "MFH3 converged and at least one variance contrast remained significant after %s adjustment (alpha = %.3f); MFH3 was selected.",
      adjustment, alpha
    )
  } else {
    sprintf(
      "MFH3 converged, but no variance contrast remained significant after %s adjustment (alpha = %.3f); MFH2 was selected.",
      adjustment, alpha
    )
  }

  list(
    requested_model = "MFH3_CANDIDATE",
    selected_model = selected,
    reason_code = reason_code,
    reason = reason,
    mfh3_converged = TRUE,
    test_available = TRUE,
    adjustment = adjustment,
    alpha = alpha,
    tests = tests
  )
}

sae_mfh_selection_export <- function(selection, requested_model = NULL) {
  tests <- selection$tests
  if (is.null(tests) || !nrow(tests)) {
    tests <- data.frame(
      comparison = NA_character_,
      statistic = NA_real_,
      p_value_raw = NA_real_,
      p_value_bonferroni = NA_real_,
      p_value_bh = NA_real_,
      significant_raw = NA,
      significant_bonferroni = NA,
      significant_bh = NA,
      stringsAsFactors = FALSE
    )
  }
  if (is.null(requested_model) || !length(requested_model)) {
    requested_model <- selection$requested_model
  }
  tests$requested_model <- requested_model
  tests$selected_model <- selection$selected_model
  tests$mfh3_converged <- selection$mfh3_converged
  tests$test_available <- selection$test_available
  tests$decision_adjustment <- selection$adjustment
  tests$alpha <- selection$alpha
  tests$reason_code <- selection$reason_code
  tests$reason <- selection$reason
  tests[, c(
    "requested_model", "selected_model", "mfh3_converged",
    "test_available", "decision_adjustment", "alpha", "reason_code",
    "reason", "comparison", "statistic", "p_value_raw",
    "p_value_bonferroni", "p_value_bh", "significant_raw",
    "significant_bonferroni", "significant_bh"
  )]
}
