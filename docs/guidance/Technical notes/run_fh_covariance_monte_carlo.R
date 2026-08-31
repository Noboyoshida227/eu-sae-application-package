# Monte Carlo simulation for FH/MFH covariance specification
#
# This script implements the simulation described in Section 11 and Annex A of
# FH_prediction_covariance_paper_with_annex_REVISED_section11_annexA.docx.
#
# Default full run:
#   Rscript "docs/guidance/Technical notes/run_fh_covariance_monte_carlo.R"
#
# Fast smoke test:
#   Rscript "docs/guidance/Technical notes/run_fh_covariance_monte_carlo.R" --reps=5 --scenarios=1
#
# Useful options:
#   --reps=500
#   --seed=777
#   --fixed-seed=12345
#   --scenarios=all          # or comma-separated scenario numbers, 1 through 9
#   --cores=1                # parallelizes over scenarios when > 1
#   --outdir=simulation_outputs

options(stringsAsFactors = FALSE)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

m <- 100L
TT <- 3L
beta_true <- c(1, 2)
A_true <- 1
miss_frac <- 0.20

get_script_dir <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if (length(file_arg) > 0L) {
    script_file <- sub("^--file=", "", file_arg[[1L]])
    return(dirname(normalizePath(script_file, mustWork = FALSE)))
  }

  script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(script_file)) {
    return(dirname(normalizePath(script_file, mustWork = FALSE)))
  }

  getwd()
}
script_dir <- get_script_dir()
default_output_dir <- file.path(script_dir, "simulation_outputs")

parse_args <- function(args) {
  get_opt <- function(name, default) {
    prefix <- paste0("--", name, "=")
    hit <- grep(paste0("^", prefix), args, value = TRUE)
    if (length(hit) == 0L) {
      return(default)
    }
    sub(prefix, "", hit[[length(hit)]], fixed = TRUE)
  }

  list(
    reps = as.integer(get_opt("reps", "500")),
    seed = as.integer(get_opt("seed", "777")),
    fixed_seed = as.integer(get_opt("fixed-seed", "12345")),
    scenarios = get_opt("scenarios", "all"),
    cores = as.integer(get_opt("cores", "1")),
    outdir = get_opt("outdir", default_output_dir)
  )
}

ar1 <- function(rho, n = TT) {
  idx <- seq_len(n)
  rho ^ abs(outer(idx, idx, "-"))
}

make_fixed <- function(seed = 12345L) {
  set.seed(seed)
  z <- matrix(runif(m * TT, min = 0, max = 1), nrow = m, ncol = TT)
  dvar <- matrix(runif(m * TT, min = 0.5, max = 1.5), nrow = m, ncol = TT)
  x <- array(1, dim = c(m, TT, 2L))
  x[, , 2L] <- z
  list(z = z, dvar = dvar, x = x)
}

make_sampling_covariances <- function(dvar, rho_e) {
  re <- ar1(rho_e)
  dfull <- array(0, dim = c(nrow(dvar), TT, TT))
  ddiag <- array(0, dim = c(nrow(dvar), TT, TT))

  for (i in seq_len(nrow(dvar))) {
    s_half <- diag(sqrt(dvar[i, ]), TT, TT)
    dfull[i, , ] <- s_half %*% re %*% s_half
    ddiag[i, , ] <- diag(dvar[i, ], TT, TT)
  }

  list(full = dfull, diag = ddiag)
}

invert_array <- function(v_array) {
  n <- dim(v_array)[1L]
  out <- array(NA_real_, dim = dim(v_array))
  for (i in seq_len(n)) {
    out[i, , ] <- solve(v_array[i, , ])
  }
  out
}

add_g_to_array <- function(g, d_array) {
  out <- d_array
  for (i in seq_len(dim(out)[1L])) {
    out[i, , ] <- g + d_array[i, , ]
  }
  out
}

gls_beta <- function(x, y, vinv) {
  n <- dim(x)[1L]
  p <- dim(x)[3L]
  xt_v_x <- matrix(0, nrow = p, ncol = p)
  xt_v_y <- numeric(p)

  for (i in seq_len(n)) {
    xi <- x[i, , ]
    vi <- vinv[i, , ]
    yi <- y[i, ]
    xt_v_x <- xt_v_x + crossprod(xi, vi %*% xi)
    xt_v_y <- xt_v_y + as.vector(crossprod(xi, vi %*% yi))
  }

  as.vector(solve(xt_v_x, xt_v_y))
}

neg_loglik_mfh <- function(par, y, x, d_array) {
  a <- exp(par[1L])
  rho <- tanh(par[2L])
  if (!is.finite(a) || a > 1e4) {
    return(1e12)
  }

  g <- a * ar1(rho)
  v_array <- add_g_to_array(g, d_array)
  n <- dim(v_array)[1L]
  log_det <- 0
  vinv <- array(NA_real_, dim = dim(v_array))

  for (i in seq_len(n)) {
    chol_i <- try(chol(v_array[i, , ]), silent = TRUE)
    if (inherits(chol_i, "try-error")) {
      return(1e12)
    }
    log_det <- log_det + 2 * sum(log(diag(chol_i)))
    vinv[i, , ] <- chol2inv(chol_i)
  }

  b <- gls_beta(x, y, vinv)
  quad <- 0
  for (i in seq_len(n)) {
    xi <- x[i, , ]
    ri <- y[i, ] - as.vector(xi %*% b)
    quad <- quad + as.numeric(crossprod(ri, vinv[i, , ] %*% ri))
  }

  0.5 * (log_det + quad)
}

fit_mfh <- function(y, x, d_array) {
  starts <- list(c(log(1.0), atanh(0.3)), c(log(0.5), atanh(0.7)))
  best <- NULL

  for (start in starts) {
    res <- try(
      optim(
        par = start,
        fn = neg_loglik_mfh,
        y = y,
        x = x,
        d_array = d_array,
        method = "Nelder-Mead",
        control = list(maxit = 300, reltol = 1e-8)
      ),
      silent = TRUE
    )

    if (!inherits(res, "try-error") && is.finite(res$value)) {
      if (is.null(best) || res$value < best$value) {
        best <- res
      }
    }
  }

  if (is.null(best)) {
    stop("MFH likelihood optimization failed for both starting points.")
  }

  c(A = exp(best$par[1L]), rho_u = tanh(best$par[2L]), objective = best$value)
}

ufh_moment <- function(y_obs, x_obs, d_obs, n_p = 2L) {
  n <- length(y_obs)

  criterion <- function(a) {
    w <- 1 / (a + d_obs)
    xt_w_x <- crossprod(x_obs, x_obs * w)
    xt_w_y <- as.vector(crossprod(x_obs, y_obs * w))
    b <- as.vector(solve(xt_w_x, xt_w_y))
    r <- y_obs - as.vector(x_obs %*% b)
    sum((r ^ 2) * w) - (n - n_p)
  }

  lower <- 0
  upper <- 20
  if (criterion(lower) <= 0) {
    a_hat <- 0
  } else {
    for (iter in seq_len(60L)) {
      mid <- 0.5 * (lower + upper)
      if (criterion(mid) > 0) {
        lower <- mid
      } else {
        upper <- mid
      }
    }
    a_hat <- 0.5 * (lower + upper)
  }

  w <- 1 / (a_hat + d_obs)
  xt_w_x <- crossprod(x_obs, x_obs * w)
  xt_w_y <- as.vector(crossprod(x_obs, y_obs * w))
  b_hat <- as.vector(solve(xt_w_x, xt_w_y))

  list(A = a_hat, beta = b_hat)
}

beta_for_mfh <- function(y, x, a, rho, d_array) {
  g <- a * ar1(rho)
  v <- add_g_to_array(g, d_array)
  gls_beta(x, y, invert_array(v))
}

mfh_predict_observed <- function(y, x, beta, a, rho, d_array) {
  n <- dim(x)[1L]
  g <- a * ar1(rho)
  pred <- matrix(NA_real_, nrow = n, ncol = TT)

  for (i in seq_len(n)) {
    xi <- x[i, , ]
    xb <- as.vector(xi %*% beta)
    vi <- g + d_array[i, , ]
    pred[i, ] <- xb + as.vector(g %*% solve(vi, y[i, ] - xb))
  }

  pred
}

mfh_predict_missing_final <- function(y_obs, x_obs, x_miss, beta, a, rho, d_oo) {
  n <- nrow(y_obs)
  g <- a * ar1(rho)
  c_mo <- g[TT, seq_len(TT - 1L)]
  goo <- g[seq_len(TT - 1L), seq_len(TT - 1L)]
  pred <- numeric(n)

  for (i in seq_len(n)) {
    xb_obs <- as.vector(x_obs[i, , ] %*% beta)
    xb_miss <- as.numeric(crossprod(x_miss[i, ], beta))
    voo <- goo + d_oo[i, , ]
    pred[i] <- xb_miss + as.numeric(c_mo %*% solve(voo, y_obs[i, ] - xb_obs))
  }

  pred
}

mvnorm_rows_common <- function(n, sigma) {
  matrix(rnorm(n * ncol(sigma)), nrow = n, ncol = ncol(sigma)) %*% chol(sigma)
}

mvnorm_rows_area_specific <- function(cov_array) {
  n <- dim(cov_array)[1L]
  out <- matrix(NA_real_, nrow = n, ncol = TT)
  for (i in seq_len(n)) {
    out[i, ] <- as.vector(matrix(rnorm(TT), nrow = 1L) %*% chol(cov_array[i, , ]))
  }
  out
}

mean_se <- function(x) {
  c(mean = mean(x), se = stats::sd(x) / sqrt(length(x)))
}

summarize_dev <- function(x) {
  c(mean = mean(x), p90 = as.numeric(stats::quantile(x, probs = 0.9, names = FALSE)))
}

scenario_grid <- data.frame(
  scenario = seq_len(9L),
  group = c("Base", "A", "A", "B", "B", "C", "C", "C", "C"),
  rho_u = c(0.0, 0.0, 0.0, 0.4, 0.8, 0.4, 0.4, 0.8, 0.8),
  rho_e = c(0.0, 0.4, 0.8, 0.0, 0.0, 0.4, 0.8, 0.4, 0.8)
)

run_scenario <- function(row, reps, seed, fixed_seed) {
  t0 <- proc.time()[["elapsed"]]
  fixed <- make_fixed(fixed_seed)
  z <- fixed$z
  dvar <- fixed$dvar
  x <- fixed$x

  rho_u <- row$rho_u
  rho_e <- row$rho_e

  d_cov <- make_sampling_covariances(dvar, rho_e)
  dfull <- d_cov$full
  ddiag <- d_cov$diag
  g_true <- A_true * ar1(rho_u)

  set.seed(seed)
  n_miss <- as.integer(miss_frac * m)

  metrics <- list(
    syn_o = numeric(reps),
    ufh_o = numeric(reps),
    mfhd_o = numeric(reps),
    mfhf_o = numeric(reps),
    orc_o = numeric(reps),
    syn_m = numeric(reps),
    mfhd_m = numeric(reps),
    mfhf_m = numeric(reps),
    orc_m = numeric(reps),
    Ad = numeric(reps),
    rd = numeric(reps),
    Af = numeric(reps),
    rf = numeric(reps),
    Aufh = numeric(reps),
    bias_syn_o = numeric(reps),
    bias_ufh_o = numeric(reps),
    bias_mfhd_o = numeric(reps),
    bias_mfhf_o = numeric(reps),
    bias_syn_m = numeric(reps),
    bias_mfhd_m = numeric(reps),
    bias_mfhf_m = numeric(reps)
  )

  devs <- list(
    dsyn_o = numeric(0L),
    dufh_o = numeric(0L),
    dmfhd_o = numeric(0L),
    dmfhf_o = numeric(0L),
    dsyn_m = numeric(0L),
    dmfhd_m = numeric(0L),
    dmfhf_m = numeric(0L)
  )

  for (rep in seq_len(reps)) {
    u <- mvnorm_rows_common(m, g_true)
    e <- mvnorm_rows_area_specific(dfull)
    theta <- beta_true[1L] + beta_true[2L] * z + u
    y <- theta + e

    miss_areas <- sample.int(m, n_miss, replace = FALSE)
    comp <- setdiff(seq_len(m), miss_areas)

    obs_mask <- matrix(TRUE, nrow = m, ncol = TT)
    obs_mask[miss_areas, TT] <- FALSE
    y_obs <- y[obs_mask]
    d_obs <- dvar[obs_mask]
    x_obs <- cbind(1, z[obs_mask])

    ufh_fit <- ufh_moment(y_obs, x_obs, d_obs)
    metrics$Aufh[rep] <- ufh_fit$A

    syn <- ufh_fit$beta[1L] + ufh_fit$beta[2L] * z
    gamma <- ufh_fit$A / (ufh_fit$A + dvar)
    ufh <- syn + gamma * (y - syn)

    yc <- y[comp, , drop = FALSE]
    xc <- x[comp, , , drop = FALSE]

    fit_d <- fit_mfh(yc, xc, ddiag[comp, , , drop = FALSE])
    fit_f <- fit_mfh(yc, xc, dfull[comp, , , drop = FALSE])
    metrics$Ad[rep] <- fit_d[["A"]]
    metrics$rd[rep] <- fit_d[["rho_u"]]
    metrics$Af[rep] <- fit_f[["A"]]
    metrics$rf[rep] <- fit_f[["rho_u"]]

    b_d <- beta_for_mfh(yc, xc, fit_d[["A"]], fit_d[["rho_u"]], ddiag[comp, , , drop = FALSE])
    b_f <- beta_for_mfh(yc, xc, fit_f[["A"]], fit_f[["rho_u"]], dfull[comp, , , drop = FALSE])

    theta_c <- theta[comp, , drop = FALSE]
    pred_mfhd <- mfh_predict_observed(
      yc, xc, b_d, fit_d[["A"]], fit_d[["rho_u"]],
      ddiag[comp, , , drop = FALSE]
    )
    pred_mfhf <- mfh_predict_observed(
      yc, xc, b_f, fit_f[["A"]], fit_f[["rho_u"]],
      dfull[comp, , , drop = FALSE]
    )
    pred_orc <- mfh_predict_observed(
      yc, xc, beta_true, A_true, rho_u,
      dfull[comp, , , drop = FALSE]
    )

    metrics$syn_o[rep] <- mean((syn[comp, ] - theta_c) ^ 2)
    metrics$ufh_o[rep] <- mean((ufh[comp, ] - theta_c) ^ 2)
    metrics$mfhd_o[rep] <- mean((pred_mfhd - theta_c) ^ 2)
    metrics$mfhf_o[rep] <- mean((pred_mfhf - theta_c) ^ 2)
    metrics$orc_o[rep] <- mean((pred_orc - theta_c) ^ 2)
    metrics$bias_syn_o[rep] <- mean(syn[comp, ] - theta_c)
    metrics$bias_ufh_o[rep] <- mean(ufh[comp, ] - theta_c)
    metrics$bias_mfhd_o[rep] <- mean(pred_mfhd - theta_c)
    metrics$bias_mfhf_o[rep] <- mean(pred_mfhf - theta_c)

    devs$dsyn_o <- c(devs$dsyn_o, abs(as.vector(syn[comp, ] - pred_orc)))
    devs$dufh_o <- c(devs$dufh_o, abs(as.vector(ufh[comp, ] - pred_orc)))
    devs$dmfhd_o <- c(devs$dmfhd_o, abs(as.vector(pred_mfhd - pred_orc)))
    devs$dmfhf_o <- c(devs$dmfhf_o, abs(as.vector(pred_mfhf - pred_orc)))

    theta_m <- theta[miss_areas, TT]
    y_m_obs <- y[miss_areas, seq_len(TT - 1L), drop = FALSE]
    x_m_obs <- x[miss_areas, seq_len(TT - 1L), , drop = FALSE]
    x_miss <- x[miss_areas, TT, ]
    doo_d <- ddiag[miss_areas, seq_len(TT - 1L), seq_len(TT - 1L), drop = FALSE]
    doo_f <- dfull[miss_areas, seq_len(TT - 1L), seq_len(TT - 1L), drop = FALSE]

    pm_d <- mfh_predict_missing_final(
      y_m_obs, x_m_obs, x_miss, b_d, fit_d[["A"]], fit_d[["rho_u"]], doo_d
    )
    pm_f <- mfh_predict_missing_final(
      y_m_obs, x_m_obs, x_miss, b_f, fit_f[["A"]], fit_f[["rho_u"]], doo_f
    )
    pm_o <- mfh_predict_missing_final(
      y_m_obs, x_m_obs, x_miss, beta_true, A_true, rho_u, doo_f
    )

    metrics$syn_m[rep] <- mean((syn[miss_areas, TT] - theta_m) ^ 2)
    metrics$mfhd_m[rep] <- mean((pm_d - theta_m) ^ 2)
    metrics$mfhf_m[rep] <- mean((pm_f - theta_m) ^ 2)
    metrics$orc_m[rep] <- mean((pm_o - theta_m) ^ 2)
    metrics$bias_syn_m[rep] <- mean(syn[miss_areas, TT] - theta_m)
    metrics$bias_mfhd_m[rep] <- mean(pm_d - theta_m)
    metrics$bias_mfhf_m[rep] <- mean(pm_f - theta_m)

    devs$dsyn_m <- c(devs$dsyn_m, abs(syn[miss_areas, TT] - pm_o))
    devs$dmfhd_m <- c(devs$dmfhd_m, abs(pm_d - pm_o))
    devs$dmfhf_m <- c(devs$dmfhf_m, abs(pm_f - pm_o))
  }

  v_true <- add_g_to_array(g_true, dfull)
  v_inv <- invert_array(v_true)
  leading_obs <- numeric(m)
  leading_miss <- numeric(m)
  c_mo <- g_true[TT, seq_len(TT - 1L)]
  g_oo <- g_true[seq_len(TT - 1L), seq_len(TT - 1L)]
  for (i in seq_len(m)) {
    m_i <- g_true - g_true %*% v_inv[i, , ] %*% g_true
    leading_obs[i] <- mean(diag(m_i))
    v_oo <- g_oo + dfull[i, seq_len(TT - 1L), seq_len(TT - 1L)]
    leading_miss[i] <- A_true - as.numeric(c_mo %*% solve(v_oo, c_mo))
  }

  out <- data.frame(
    scenario = row$scenario,
    group = row$group,
    rho_u = rho_u,
    rho_e = rho_e,
    reps = reps,
    time_sec = proc.time()[["elapsed"]] - t0,
    lead_obs = mean(leading_obs),
    lead_miss = mean(leading_miss)
  )

  for (nm in names(metrics)) {
    ms <- mean_se(metrics[[nm]])
    out[[nm]] <- ms[["mean"]]
    out[[paste0(nm, "_se")]] <- ms[["se"]]
  }

  for (nm in names(devs)) {
    ds <- summarize_dev(devs[[nm]])
    out[[paste0(nm, "_mean")]] <- ds[["mean"]]
    out[[paste0(nm, "_p90")]] <- ds[["p90"]]
  }

  out
}

format_value_se <- function(value, se) {
  sprintf("%.3f (%.3f)", value, se)
}

write_summary_tables <- function(results, outdir) {
  observed <- data.frame(
    group = results$group,
    rho_u = results$rho_u,
    rho_e = results$rho_e,
    synthetic = format_value_se(results$syn_o, results$syn_o_se),
    ufh_eblup = format_value_se(results$ufh_o, results$ufh_o_se),
    mfh_diag = format_value_se(results$mfhd_o, results$mfhd_o_se),
    mfh_full = format_value_se(results$mfhf_o, results$mfhf_o_se),
    oracle_blup = format_value_se(results$orc_o, results$orc_o_se),
    leading_term = sprintf("%.3f", results$lead_obs)
  )
  utils::write.csv(observed, file.path(outdir, "table_observed_mse.csv"), row.names = FALSE)

  missing <- data.frame(
    group = results$group,
    rho_u = results$rho_u,
    rho_e = results$rho_e,
    synthetic_ufh = format_value_se(results$syn_m, results$syn_m_se),
    mfh_diag = format_value_se(results$mfhd_m, results$mfhd_m_se),
    mfh_full = format_value_se(results$mfhf_m, results$mfhf_m_se),
    oracle_blup = format_value_se(results$orc_m, results$orc_m_se),
    leading_term = sprintf("%.3f", results$lead_miss)
  )
  utils::write.csv(missing, file.path(outdir, "table_missing_mse.csv"), row.names = FALSE)

  dev_observed <- data.frame(
    group = results$group,
    rho_u = results$rho_u,
    rho_e = results$rho_e,
    synthetic = sprintf("%.3f (%.3f)", results$dsyn_o_mean, results$dsyn_o_p90),
    ufh_eblup = sprintf("%.3f (%.3f)", results$dufh_o_mean, results$dufh_o_p90),
    mfh_diag = sprintf("%.3f (%.3f)", results$dmfhd_o_mean, results$dmfhd_o_p90),
    mfh_full = sprintf("%.3f (%.3f)", results$dmfhf_o_mean, results$dmfhf_o_p90)
  )
  utils::write.csv(
    dev_observed,
    file.path(outdir, "table_observed_point_deviation.csv"),
    row.names = FALSE
  )

  dev_missing <- data.frame(
    group = results$group,
    rho_u = results$rho_u,
    rho_e = results$rho_e,
    synthetic_ufh = sprintf("%.3f (%.3f)", results$dsyn_m_mean, results$dsyn_m_p90),
    mfh_diag = sprintf("%.3f (%.3f)", results$dmfhd_m_mean, results$dmfhd_m_p90),
    mfh_full = sprintf("%.3f (%.3f)", results$dmfhf_m_mean, results$dmfhf_m_p90)
  )
  utils::write.csv(
    dev_missing,
    file.path(outdir, "table_missing_point_deviation.csv"),
    row.names = FALSE
  )

  params <- data.frame(
    group = results$group,
    rho_u = results$rho_u,
    rho_e = results$rho_e,
    A_diag = format_value_se(results$Ad, results$Ad_se),
    rho_u_diag = format_value_se(results$rd, results$rd_se),
    A_full = format_value_se(results$Af, results$Af_se),
    rho_u_full = format_value_se(results$rf, results$rf_se)
  )
  utils::write.csv(params, file.path(outdir, "table_mfh_parameter_estimates.csv"), row.names = FALSE)

  bias <- data.frame(
    group = results$group,
    rho_u = results$rho_u,
    rho_e = results$rho_e,
    bias_syn_o = results$bias_syn_o,
    bias_ufh_o = results$bias_ufh_o,
    bias_mfhd_o = results$bias_mfhd_o,
    bias_mfhf_o = results$bias_mfhf_o,
    bias_syn_m = results$bias_syn_m,
    bias_mfhd_m = results$bias_mfhd_m,
    bias_mfhf_m = results$bias_mfhf_m
  )
  utils::write.csv(bias, file.path(outdir, "table_prediction_bias.csv"), row.names = FALSE)
}

select_scenarios <- function(scenarios_arg) {
  if (tolower(scenarios_arg) == "all") {
    return(seq_len(nrow(scenario_grid)))
  }
  idx <- as.integer(strsplit(scenarios_arg, ",", fixed = TRUE)[[1L]])
  if (any(is.na(idx)) || any(!idx %in% scenario_grid$scenario)) {
    stop("--scenarios must be 'all' or comma-separated scenario numbers from 1 through 9.")
  }
  idx
}

run_all <- function() {
  cfg <- parse_args(commandArgs(trailingOnly = TRUE))
  if (is.na(cfg$reps) || cfg$reps < 1L) {
    stop("--reps must be a positive integer.")
  }
  if (is.na(cfg$cores) || cfg$cores < 1L) {
    stop("--cores must be a positive integer.")
  }

  selected <- select_scenarios(cfg$scenarios)
  rows <- split(scenario_grid[selected, , drop = FALSE], seq_along(selected))
  outdir <- normalizePath(cfg$outdir, mustWork = FALSE)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  message("Running ", length(rows), " scenario(s), ", cfg$reps, " replication(s) each.")
  message("Output directory: ", outdir)

  if (cfg$cores > 1L && length(rows) > 1L) {
    cl <- parallel::makeCluster(min(cfg$cores, length(rows)))
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(
      cl,
      varlist = setdiff(ls(envir = .GlobalEnv), c("cfg", "cl")),
      envir = .GlobalEnv
    )
    result_list <- parallel::parLapply(
      cl,
      rows,
      function(row) run_scenario(row, reps = cfg$reps, seed = cfg$seed, fixed_seed = cfg$fixed_seed)
    )
  } else {
    result_list <- lapply(
      rows,
      function(row) run_scenario(row, reps = cfg$reps, seed = cfg$seed, fixed_seed = cfg$fixed_seed)
    )
  }

  results <- do.call(rbind, result_list)
  results <- results[order(results$scenario), ]
  row.names(results) <- NULL

  utils::write.csv(results, file.path(outdir, "all_results.csv"), row.names = FALSE)
  write_summary_tables(results, outdir)

  message("Done. Main results written to ", file.path(outdir, "all_results.csv"))
  invisible(results)
}

if (sys.nframe() == 0L) {
  run_all()
}
