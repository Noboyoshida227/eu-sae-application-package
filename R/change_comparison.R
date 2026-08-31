# Descriptive comparison of unbenchmarked changes, without refitting models.
sae_change_comparison <- function(significance, years, indicator_type = "poverty",
                                  currency_symbol = "EUR", fgt_alpha = 0L) {
  required <- c("domain", "method", "diff")
  if (!all(required %in% names(significance))) stop("Change comparison lacks domain, method or diff.")
  if (length(years) != 2L || anyNA(years) || years[2] <= years[1]) stop("Changes require two ascending years.")
  multiplier <- if (identical(indicator_type, "poverty")) 100 else 1
  unit <- if (identical(indicator_type, "poverty")) "percentage points" else currency_symbol
  label <- if (!identical(indicator_type, "poverty")) "mean-welfare" else if (fgt_alpha == 0L) "poverty-rate" else sprintf("FGT(%s)", fgt_alpha)
  extract <- function(method, prefix) {
    d <- as.data.frame(significance[as.character(significance$method) == method, , drop = FALSE])
    d$domain <- trimws(as.character(d$domain))
    if (anyNA(d$domain) || any(!nzchar(d$domain)) || anyDuplicated(d$domain)) stop("Each method must have one nonmissing row per domain.")
    out <- data.frame(domain = d$domain, change = as.numeric(d$diff) * multiplier)
    for (field in intersect(c("lb", "ub", "significant_unadjusted", "significant_bh", "significant_bonferroni"), names(d))) {
      out[[field]] <- if (field %in% c("lb", "ub")) as.numeric(d[[field]]) * multiplier else d[[field]]
    }
    names(out)[-1L] <- paste0(prefix, "_", names(out)[-1L])
    out
  }
  ufh <- extract("FH", "UFH"); mfh <- extract("MFH", "MFH")
  paired <- merge(ufh, mfh, by = "domain", sort = FALSE)
  matched <- nrow(paired)
  paired <- paired[is.finite(paired$UFH_change) & is.finite(paired$MFH_change), , drop = FALSE]
  ord <- suppressWarnings(as.numeric(paired$domain))
  paired <- paired[order(is.na(ord), ord, paired$domain), , drop = FALSE]
  paired$MFH_minus_UFH_change <- paired$MFH_change - paired$UFH_change
  paired$year_from <- rep(years[1], nrow(paired)); paired$year_to <- rep(years[2], nrow(paired))
  paired$unit <- rep(unit, nrow(paired)); paired$benchmark_status <- rep("Unbenchmarked", nrow(paired))
  long <- rbind(data.frame(domain = paired$domain, method = rep("UFH", nrow(paired)), change = paired$UFH_change),
                data.frame(domain = paired$domain, method = rep("MFH", nrow(paired)), change = paired$MFH_change))
  long$method <- factor(long$method, levels = c("UFH", "MFH"))
  summarise_values <- function(x, method) data.frame(
    method = method, domains = length(x),
    minimum = if (length(x)) min(x) else NA_real_,
    percentile_25 = if (length(x)) unname(quantile(x, .25)) else NA_real_,
    median = if (length(x)) median(x) else NA_real_,
    mean = if (length(x)) mean(x) else NA_real_,
    percentile_75 = if (length(x)) unname(quantile(x, .75)) else NA_real_,
    maximum = if (length(x)) max(x) else NA_real_, unit = unit)
  distribution <- rbind(summarise_values(paired$UFH_change, "UFH"), summarise_values(paired$MFH_change, "MFH"))
  gap <- paired$MFH_minus_UFH_change
  summary <- data.frame(
    year_from = years[1], year_to = years[2], matched_domains = matched,
    finite_paired_domains = nrow(paired), excluded_nonfinite_pairs = matched - nrow(paired),
    UFH_only_domains = sum(!ufh$domain %in% mfh$domain),
    MFH_only_domains = sum(!mfh$domain %in% ufh$domain),
    MFH_lower_domains = sum(gap < 0), MFH_higher_domains = sum(gap > 0), equal_domains = sum(gap == 0),
    median_MFH_minus_UFH_change = if (length(gap)) median(gap) else NA_real_,
    mean_MFH_minus_UFH_change = if (length(gap)) mean(gap) else NA_real_, unit = unit)
  list(domain = paired, long = long, distribution = distribution, paired = summary,
       label = label, unit = unit, years = years)
}

sae_plot_change_distribution <- function(comparison) {
  if (!nrow(comparison$domain)) return(NULL)
  ggplot2::ggplot(comparison$long, ggplot2::aes(x = method, y = change, color = method, fill = method)) +
    ggplot2::geom_hline(yintercept = 0, color = "#64748B", linetype = "dashed") +
    ggplot2::geom_boxplot(width = .42, alpha = .12, outlier.shape = NA, linewidth = .8) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = .08, height = 0, seed = 123), alpha = .55, size = 2.2) +
    ggplot2::stat_summary(fun = median, geom = "text", ggplot2::aes(label = sprintf("Median %+.2f", ggplot2::after_stat(y))),
                          position = ggplot2::position_nudge(x = .26), hjust = 0, vjust = .5,
                          size = 3.8, fontface = "bold", show.legend = FALSE) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(.45, .85))) +
    ggplot2::scale_color_manual(values = c(UFH = "#2563EB", MFH = "#D97706")) +
    ggplot2::scale_fill_manual(values = c(UFH = "#2563EB", MFH = "#D97706")) +
    ggplot2::labs(title = paste("Distribution of estimated", comparison$label, "changes"),
      subtitle = sprintf("%s minus %s | Unbenchmarked estimates across %d matched domains", comparison$years[2], comparison$years[1], nrow(comparison$domain)),
      x = NULL, y = sprintf("Estimated change (%s)", comparison$unit),
      caption = "Each point is one domain; boxes show the median and interquartile range.\nNegative values indicate a decrease. These are estimates, not confidence-interval widths.") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(face = "bold"), panel.grid.major.x = ggplot2::element_blank())
}

sae_plot_change_paired <- function(comparison) {
  d <- comparison$domain
  if (!nrow(d)) return(NULL)
  d$comparison <- ifelse(d$MFH_minus_UFH_change < 0, "MFH lower", "MFH higher or equal")
  label_rows <- head(order(abs(d$MFH_minus_UFH_change), decreasing = TRUE), 4)
  d$label <- ifelse(seq_len(nrow(d)) %in% label_rows, d$domain, "")
  limits <- range(c(0, d$UFH_change, d$MFH_change)); span <- diff(limits)
  if (!is.finite(span) || span == 0) span <- 1
  limits <- limits + c(-.10, .10) * span
  ggplot2::ggplot(d, ggplot2::aes(x = UFH_change, y = MFH_change)) +
    ggplot2::geom_hline(yintercept = 0, color = "#CBD5E1", linewidth = .4) +
    ggplot2::geom_vline(xintercept = 0, color = "#CBD5E1", linewidth = .4) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#64748B") +
    ggplot2::geom_point(ggplot2::aes(color = comparison, shape = comparison), size = 2.8, alpha = .78) +
    ggplot2::geom_text(data = d[nzchar(d$label), , drop = FALSE], ggplot2::aes(label = label),
                       nudge_x = .016 * span, nudge_y = .016 * span, size = 3.5, check_overlap = TRUE) +
    ggplot2::scale_color_manual(values = c(`MFH lower` = "#D97706", `MFH higher or equal` = "#2563EB")) +
    ggplot2::scale_shape_manual(values = c(`MFH lower` = 16, `MFH higher or equal` = 1)) +
    ggplot2::coord_equal(xlim = limits, ylim = limits) +
    ggplot2::labs(title = paste("Domain-level comparison of estimated", comparison$label, "changes"),
      subtitle = sprintf("%s minus %s | %d matched domains | Unbenchmarked\nBelow equality: a lower MFH change estimate", comparison$years[2], comparison$years[1], nrow(d)),
      x = sprintf("UFH estimated change (%s)", comparison$unit),
      y = sprintf("MFH estimated change (%s)", comparison$unit), color = NULL, shape = NULL,
      caption = "Negative values indicate a decrease. Labels identify the four largest method gaps.\nA lower estimate does not imply greater accuracy or statistical significance.") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16), legend.position = "top")
}
