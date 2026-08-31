# Shared variance-selection policy for UFH and MFH.
#
# Under sm_out, the direct sampling variance is retained unless it is
# missing/non-finite or below the fixed threshold. This intentionally does
# not replace large direct variances.

SAE_SM_OUT_VARIANCE_THRESHOLD <- 0.001

sae_sm_out_variance <- function(direct_variance,
                                smoothed_variance,
                                threshold = SAE_SM_OUT_VARIANCE_THRESHOLD) {
  direct_variance <- as.numeric(direct_variance)
  smoothed_variance <- as.numeric(smoothed_variance)

  target_length <- max(length(direct_variance), length(smoothed_variance))
  if (!length(direct_variance) || !length(smoothed_variance) ||
      target_length %% length(direct_variance) != 0L ||
      target_length %% length(smoothed_variance) != 0L) {
    stop("Direct and smoothed variance vectors must have compatible lengths.",
         call. = FALSE)
  }

  direct_variance <- rep(direct_variance, length.out = target_length)
  smoothed_variance <- rep(smoothed_variance, length.out = target_length)

  threshold <- suppressWarnings(as.numeric(threshold)[1])
  if (!is.finite(threshold) || threshold < 0) {
    stop("The sm_out variance threshold must be a finite non-negative number.",
         call. = FALSE)
  }

  replace <- !is.finite(direct_variance) | direct_variance < threshold
  direct_variance[replace] <- smoothed_variance[replace]
  direct_variance
}

