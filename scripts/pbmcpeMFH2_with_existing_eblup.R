#' Parametric Bootstrap MCPE for MFH2 Model (Using existing EBLUP)
#'
#' Computes Mean Squared Error (MSE) and Model-based Covariance Prediction Error (MCPE) 
#' for multivariate Fay-Herriot type 2 (MFH2) models using parametric bootstrap.
#' This version uses pre-computed EBLUPs from an existing model fit to ensure consistency.
#'
#' @param formula A list of formulas, one for each time point or outcome (e.g., list(y1 ~ x1 + x2, y2 ~ x1)).
#' @param vardir A character vector of column names in \code{data} specifying the sampling error variances
#'   and covariances. The first \code{nT} elements correspond to the variances, followed by
#'   \code{nT*(nT - 1)/2} covariances (in row-wise order).
#' @param domain_var Character string specifying the column name in \code{data} that contains domain identifiers.
#' @param existing_model An existing MFH2 model object (from eblupMFH2) to use for EBLUPs and parameters.
#' @param nB Number of bootstrap replications. Default is 100.
#' @param data A data frame containing all variables used in \code{formula} and \code{vardir}.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{domain}{Domain identifiers from the input data.}
#'   \item{eblup}{The EBLUP estimates from the existing model (for consistency).}
#'   \item{mse}{Estimated mean squared errors for each domain and time point.}
#'   \item{mcpe}{Estimated MCPEs (covariances between errors) across outcomes.}
#'   \item{fails}{Number of bootstrap iterations that failed to converge.}
#' }
#' @importFrom MASS mvrnorm
#' @importFrom Matrix nearPD
#' @import matrixcalc
#' @export

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

pbmcpeMFH2_with_existing <- function(formula, vardir, domain_var, existing_model,
                                     nB = 200, data, max_attempts = NULL,
                                     seed = 123L, ...) {
  # max_attempts: upper bound on total bootstrap iterations (successes +
  # failures). Prevents the loop from spinning forever when refits keep
  # failing to converge -- e.g., when var_choice = "direct" feeds the
  # bootstrap sampling-error variances that destabilise eblupMFH2's REML.
  # Default = 5 * nB (i.e., we tolerate up to 4*nB consecutive failures).
  if (is.null(max_attempts)) max_attempts <- 5L * nB
  if (!is.null(seed)) set.seed(seed)
  
  nD <- nrow(data)
  nT <- length(formula)
  M <- nD * nT
  
  # Store domain identifiers at the start
  domain_ids <- data[[domain_var]]
  if (is.null(domain_ids)) {
    stop("domain_var '", domain_var, "' not found in data")
  }
  
  # Build list of design matrices
  X_list <- lapply(formula, function(f) model.matrix(f, data))
  p_list <- sapply(X_list, ncol)
  
  # Use the existing model instead of refitting
  result <- existing_model
  if (!isTRUE(result$fit$convergence)) stop("Existing model did not converge")
  
  # Extract beta coefficients from matrix
  beta_list <- list()
  estcoef_mat <- result$fit$estcoef
  start_row <- 1
  for (t in 1:nT) {
    end_row <- start_row + p_list[t] - 1
    beta_list[[t]] <- estcoef_mat[start_row:end_row, 1]
    start_row <- end_row + 1
  }
  
  varu2 <- sae_mfh_refvar_vector(result$fit$refvar, nT, "MFH2")
  rho <- sae_mfh_rho_scalar(result$fit$rho)
  
  # Prepare sampling error covariance matrices
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
        "pbmcpeMFH2_with_existing(): hit max_attempts=%d after %d successful and %d failed bootstrap refits; aborting bootstrap. This typically indicates that eblupMFH2() is not converging on the perturbed samples (often the case with var_choice = 'direct'). Returning NULL so the caller can fall back to a no-MCPE path.",
        max_attempts, b - 1L, countfail
      ))
      return(NULL)
    }
    message(sprintf("Bootstrap %d", b))
    
    udt_mat_draw <- sae_simulate_mfh_random_effects(
      "MFH2", nD = nD, nT = nT, refvar = varu2, rho = rho
    )
    udt_b <- as.numeric(t(udt_mat_draw))
    edt_b <- rep(0, M)
    meandt_b <- rep(0, M)
    i <- 1
    for (d in 1:nD) {
      edt_b[i:(i + nT - 1)] <- mvrnorm(1, mu = rep(0, nT), Sigma = Sigmaed[,,d])
      for (t in 1:nT) {
        meandt_b[i + t - 1] <- X_list[[t]][d, ] %*% beta_list[[t]]
      }
      i <- i + nT
    }
    
    mudt_b <- meandt_b + udt_b
    ydt_b <- mudt_b + edt_b
    
    ydt.mat <- matrix(0, nrow = nD, ncol = nT)
    mudt.mat <- matrix(0, nrow = nD, ncol = nT)
    for (t in 1:nT) {
      ydt.mat[, t] <- ydt_b[seq(from = t, to = M, by = nT)]
      mudt.mat[, t] <- mudt_b[seq(from = t, to = M, by = nT)]
    }
    
    ydt.df <- setNames(as.data.frame(ydt.mat), paste0("Y", 1:nT))
    
    # Rebuild formulas with same RHS
    formula.b <- lapply(1:nT, function(t) {
      rhs <- paste(attr(terms(formula[[t]]), "term.labels"), collapse = " + ")
      as.formula(paste0("Y", t, " ~ ", rhs))
    })
    
    used_vars <- unique(unlist(lapply(formula, all.vars)))
    data.b <- cbind(ydt.df, data[, used_vars, drop = FALSE], sigmaedts)
    
    result.b <- tryCatch({
      eblupMFH2(formula = formula.b, vardir = vardir, data = data.b)
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
    warning("pbmcpeMFH2_with_existing(): no successful bootstrap refits; returning NULL.")
    return(NULL)
  }
  mcpedt.b <- mcpedt / n_valid
  mcpedt.mcse <- sae_bootstrap_mcse(mcpedt, mcpedt_sq, n_valid)
  
  mse  <- mcpedt.b[, 1:nT, drop = FALSE]
  mse_mcse <- mcpedt.mcse[, 1:nT, drop = FALSE]
  mcpe <- mcpedt.b[, (nT + 1):(nT + nT * (nT - 1) / 2), drop = FALSE]
  mcpe_mcse <- mcpedt.mcse[, (nT + 1):(nT + nT * (nT - 1) / 2), drop = FALSE]
  
  # Optional: always name columns (nice for downstream code)
  if (nT >= 2) {
    colnames(mcpe) <- apply(combn(nT, 2), 2, function(pair) paste0("(", pair[1], ",", pair[2], ")"))
    colnames(mcpe_mcse) <- colnames(mcpe)
  }
  
  list(
    domain = domain_ids,
    eblup  = result$eblup,  # Use EBLUP from existing model
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
