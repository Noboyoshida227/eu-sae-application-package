#' Parametric Bootstrap MCPE for MFH1 Model (Using existing EBLUP)
#'
#' Computes Mean Squared Error (MSE) and Model-based Covariance Prediction
#' Error (MCPE) for multivariate Fay-Herriot type 1 (MFH1) models using
#' parametric bootstrap. Mirrors the structure of
#' \code{pbmcpeMFH2_with_existing()} but assumes random effects are
#' independent across time (no AR(1)), with variance that may differ by
#' time period.
#'
#' Returns the same shape as \code{pbmcpeMFH2_with_existing()} so the
#' Comparison report and downstream code don't need to branch on model.
#'
#' @inheritParams pbmcpeMFH2_with_existing

library(MASS)
library(Matrix)
library(matrixcalc)

if (!exists("sae_simulate_mfh_random_effects", mode = "function")) {
  .mfh_helper <- if (requireNamespace("here", quietly = TRUE)) {
    here::here("R", "mfh_bootstrap_helpers.R")
  } else {
    file.path("R", "mfh_bootstrap_helpers.R")
  }
  source(.mfh_helper)
  rm(.mfh_helper)
}

pbmcpeMFH1_with_existing <- function(formula, vardir, domain_var,
                                     existing_model, nB = 200, data,
                                     max_attempts = NULL, seed = 123L, ...) {
  # max_attempts: hard cap on total bootstrap iterations to prevent the
  # loop from spinning forever when refits keep failing to converge (see
  # the corresponding pbmcpeMFH2 wrapper for the longer rationale).
  if (is.null(max_attempts)) max_attempts <- 5L * nB
  if (!is.null(seed)) set.seed(seed)

  nD <- nrow(data)
  nT <- length(formula)
  M <- nD * nT

  domain_ids <- data[[domain_var]]
  if (is.null(domain_ids)) {
    stop("domain_var '", domain_var, "' not found in data")
  }

  X_list <- lapply(formula, function(f) model.matrix(f, data))
  p_list <- sapply(X_list, ncol)

  result <- existing_model
  if (!isTRUE(result$fit$convergence)) stop("Existing model did not converge")

  beta_list <- list()
  estcoef_mat <- result$fit$estcoef
  start_row <- 1
  for (t in 1:nT) {
    end_row <- start_row + p_list[t] - 1
    beta_list[[t]] <- estcoef_mat[start_row:end_row, 1]
    start_row <- end_row + 1
  }

  # MFH1: refvar is (potentially) a vector of length nT; rho is unused.
  varu2_vec <- sae_mfh_refvar_vector(result$fit$refvar, nT, "MFH1")

  sigmaedts <- as.matrix(data[, vardir, drop = FALSE])
  storage.mode(sigmaedts) <- "double"
  Sigmaed <- array(0, c(nT, nT, nD))
  near_pd_adjustments <- 0L
  for (d in 1:nD) {
    idx <- nT + 1
    for (t1 in seq_len(max(0L, nT - 1L))) {
      Sigmaed[t1, t1, d] <- sigmaedts[d, t1]
      for (t2 in seq.int(t1 + 1L, nT)) {
        Sigmaed[t1, t2, d] <- sigmaedts[d, idx]
        Sigmaed[t2, t1, d] <- sigmaedts[d, idx]
        idx <- idx + 1
      }
    }
    Sigmaed[nT, nT, d] <- sigmaedts[d, nT]
    if (!is.positive.definite(Sigmaed[,,d])) {
      Sigmaed[,,d] <- as.matrix(nearPD(Sigmaed[,,d], keepDiag = TRUE)$mat)
      near_pd_adjustments <- near_pd_adjustments + 1L
    }
  }

  mcpedt <- matrix(0, nrow = nD, ncol = nT + nT * (nT - 1) / 2)
  mcpedt_sq <- matrix(0, nrow = nD, ncol = nT + nT * (nT - 1) / 2)
  countfail <- 0
  b <- 1
  attempts <- 0L

  while (b <= nB) {
    attempts <- attempts + 1L
    if (attempts > max_attempts) {
      warning(sprintf(
        "pbmcpeMFH1_with_existing(): hit max_attempts=%d after %d successful and %d failed bootstrap refits; aborting bootstrap. Returning NULL so the caller can fall back to a no-MCPE path.",
        max_attempts, b - 1L, countfail
      ))
      return(NULL)
    }
    message(sprintf("Bootstrap %d (MFH1)", b))

    # Independent random effects across time (no AR(1))
    udt_mat_draw <- sae_simulate_mfh_random_effects(
      "MFH1", nD = nD, nT = nT, refvar = varu2_vec
    )
    udt_b <- as.numeric(t(udt_mat_draw))

    edt_b <- rep(0, M)
    meandt_b <- rep(0, M)
    i <- 1
    for (d in seq_len(nD)) {
      edt_b[i:(i + nT - 1)] <- mvrnorm(1, mu = rep(0, nT),
                                       Sigma = Sigmaed[,,d])
      for (tt in seq_len(nT)) {
        meandt_b[i + tt - 1] <- X_list[[tt]][d, ] %*% beta_list[[tt]]
      }
      i <- i + nT
    }

    mudt_b <- meandt_b + udt_b
    ydt_b  <- mudt_b + edt_b

    ydt.mat  <- matrix(0, nrow = nD, ncol = nT)
    mudt.mat <- matrix(0, nrow = nD, ncol = nT)
    for (tt in 1:nT) {
      ydt.mat[, tt]  <- ydt_b[seq(from = tt, to = M, by = nT)]
      mudt.mat[, tt] <- mudt_b[seq(from = tt, to = M, by = nT)]
    }

    ydt.df <- setNames(as.data.frame(ydt.mat), paste0("Y", 1:nT))

    formula.b <- lapply(1:nT, function(t) {
      rhs <- paste(attr(terms(formula[[t]]), "term.labels"), collapse = " + ")
      as.formula(paste0("Y", t, " ~ ", rhs))
    })

    used_vars <- unique(unlist(lapply(formula, all.vars)))
    data.b <- cbind(ydt.df, data[, used_vars, drop = FALSE], sigmaedts)

    result.b <- tryCatch({
      eblupMFH1(formula = formula.b, vardir = vardir, data = data.b)
    }, error = function(e) NULL)

    if (is.null(result.b) || !isTRUE(result.b$fit$convergence)) {
      countfail <- countfail + 1
      next
    }

    dif <- result.b$eblup - mudt.mat
    dif.b <- matrix(0, nrow = nD, ncol = nT + nT * (nT - 1) / 2)
    pos <- nT
    for (t1 in seq_len(max(0L, nT - 1L))) {
      dif.b[, t1] <- dif[, t1]^2
      for (t2 in seq.int(t1 + 1L, nT)) {
        pos <- pos + 1
        dif.b[, pos] <- dif[, t1] * dif[, t2]
      }
    }
    dif.b[, nT] <- dif[, nT]^2

    mcpedt <- mcpedt + dif.b
    mcpedt_sq <- mcpedt_sq + dif.b^2
    b <- b + 1
  }

  n_valid <- b - 1L
  if (n_valid <= 0L) {
    warning("pbmcpeMFH1_with_existing(): no successful bootstrap refits; returning NULL.")
    return(NULL)
  }
  mcpedt.b <- mcpedt / n_valid
  mcpedt.mcse <- sae_bootstrap_mcse(mcpedt, mcpedt_sq, n_valid)

  mse  <- mcpedt.b[, 1:nT, drop = FALSE]
  mse_mcse <- mcpedt.mcse[, 1:nT, drop = FALSE]
  mcpe <- if (nT >= 2)
    mcpedt.b[, (nT + 1):(nT + nT * (nT - 1) / 2), drop = FALSE]
  else NULL
  mcpe_mcse <- if (nT >= 2)
    mcpedt.mcse[, (nT + 1):(nT + nT * (nT - 1) / 2), drop = FALSE]
  else NULL

  if (!is.null(mcpe)) {
    colnames(mcpe) <- apply(combn(nT, 2), 2,
                            function(pair) paste0("(", pair[1], ",", pair[2], ")"))
    colnames(mcpe_mcse) <- colnames(mcpe)
  }

  list(
    domain = domain_ids,
    eblup  = result$eblup,
    mse    = mse,
    mse_mcse = mse_mcse,
    mcpe   = mcpe,
    mcpe_mcse = mcpe_mcse,
    fails  = countfail,
    n_success = n_valid,
    attempts = attempts,
    target_replicates = nB,
    near_pd_adjustments = near_pd_adjustments,
    seed = seed
  )
}
