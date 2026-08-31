# Random-effect simulation helpers used by the MFH parametric bootstrap.
# Keeping the data-generating process in one place makes it possible to test
# its implied covariance independently of the msae refitting routines.

sae_mfh_rho_scalar <- function(rho) {
  if (is.null(rho)) stop("The fitted model does not contain rho.", call. = FALSE)
  value <- if (is.matrix(rho) || is.data.frame(rho)) {
    suppressWarnings(as.numeric(rho[, 1L])[1L])
  } else {
    suppressWarnings(as.numeric(rho)[1L])
  }
  if (!is.finite(value) || abs(value) >= 1) {
    stop("rho must be finite and strictly between -1 and 1.", call. = FALSE)
  }
  value
}

sae_mfh_refvar_vector <- function(refvar, nT, model) {
  value <- suppressWarnings(as.numeric(refvar))
  if (!length(value) || any(!is.finite(value)) || any(value < 0)) {
    stop(model, " random-effect variance estimates must be finite and non-negative.",
         call. = FALSE)
  }
  if (length(value) == 1L && model == "MFH2") return(value)
  if (length(value) == 1L) value <- rep(value, nT)
  if (length(value) != nT) {
    stop(model, " refvar length ", length(value),
         " is incompatible with nT = ", nT, ".", call. = FALSE)
  }
  value
}

sae_mfh_random_effect_covariance <- function(model, refvar, rho = NULL, nT) {
  model <- toupper(as.character(model)[1L])
  nT <- as.integer(nT)[1L]
  if (!is.finite(nT) || nT < 1L) stop("nT must be at least one.", call. = FALSE)

  if (model == "MFH1") {
    variances <- sae_mfh_refvar_vector(refvar, nT, model)
    return(diag(variances, nT, nT))
  }

  rho <- sae_mfh_rho_scalar(rho)
  if (model == "MFH2") {
    innovation_variance <- sae_mfh_refvar_vector(refvar, nT, model)[1L]
    idx <- seq_len(nT)
    return(innovation_variance * outer(idx, idx, function(i, j) {
      rho^abs(i - j) / (1 - rho^2)
    }))
  }

  if (model == "MFH3") {
    innovation_variances <- sae_mfh_refvar_vector(refvar, nT, model)
    # Molina-Romero/Benavent-Morales specification:
    # u_0 ~ N(0, 1), u_t = rho * u_{t-1} + a_t,
    # a_t ~ N(0, sigma_t^2).
    loading <- matrix(0, nT, nT + 1L)
    for (tt in seq_len(nT)) {
      loading[tt, 1L] <- rho^tt
      for (jj in seq_len(tt)) loading[tt, jj + 1L] <- rho^(tt - jj)
    }
    innovation_cov <- diag(c(1, innovation_variances), nT + 1L)
    return(loading %*% innovation_cov %*% t(loading))
  }

  stop("Unsupported MFH model: ", model, call. = FALSE)
}

sae_simulate_mfh_random_effects <- function(model, nD, nT, refvar,
                                            rho = NULL) {
  model <- toupper(as.character(model)[1L])
  nD <- as.integer(nD)[1L]
  nT <- as.integer(nT)[1L]
  if (!is.finite(nD) || nD < 1L) stop("nD must be at least one.", call. = FALSE)

  if (model == "MFH1") {
    variances <- sae_mfh_refvar_vector(refvar, nT, model)
    draws <- matrix(
      stats::rnorm(nD * nT,
                   sd = rep(sqrt(variances), times = nD)),
      nrow = nD, ncol = nT, byrow = TRUE
    )
    return(draws)
  }

  rho <- sae_mfh_rho_scalar(rho)
  if (model == "MFH2") {
    innovation_variance <- sae_mfh_refvar_vector(refvar, nT, model)[1L]
    innovations <- matrix(
      stats::rnorm(nD * nT, sd = sqrt(innovation_variance)),
      nrow = nD, ncol = nT, byrow = TRUE
    )
    draws <- matrix(0, nD, nT)
    draws[, 1L] <- innovations[, 1L] / sqrt(1 - rho^2)
    if (nT > 1L) {
      for (tt in 2:nT) draws[, tt] <- rho * draws[, tt - 1L] + innovations[, tt]
    }
    return(draws)
  }

  if (model == "MFH3") {
    innovation_variances <- sae_mfh_refvar_vector(refvar, nT, model)
    innovations <- matrix(
      stats::rnorm(nD * nT,
                   sd = rep(sqrt(innovation_variances), times = nD)),
      nrow = nD, ncol = nT, byrow = TRUE
    )
    u0 <- stats::rnorm(nD, sd = 1)
    draws <- matrix(0, nD, nT)
    draws[, 1L] <- rho * u0 + innovations[, 1L]
    if (nT > 1L) {
      for (tt in 2:nT) draws[, tt] <- rho * draws[, tt - 1L] + innovations[, tt]
    }
    return(draws)
  }

  stop("Unsupported MFH model: ", model, call. = FALSE)
}

sae_bootstrap_mcse <- function(sum_values, sum_squares, n) {
  if (n <= 1L) return(matrix(NA_real_, nrow(sum_values), ncol(sum_values)))
  means <- sum_values / n
  variances <- (sum_squares - n * means^2) / (n - 1L)
  sqrt(pmax(variances, 0) / n)
}
