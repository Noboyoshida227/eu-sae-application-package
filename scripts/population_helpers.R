# Shared population helpers for benchmarking.
#
# If the user supplies a population file, read it as domain-year population
# totals. Otherwise, estimate domain-year populations from the household survey
# using population_weight = weight * household size.

sae_read_optional_population_table <- function(path) {
  if (is.null(path) || length(path) == 0 || !nzchar(path)) return(NULL)
  if (!file.exists(path)) stop("Population input not found: ", path)

  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") {
    readRDS(path)
  } else if (ext %in% c("rda", "rdata")) {
    load_env <- new.env(parent = emptyenv())
    loaded <- load(path, envir = load_env)
    if (length(loaded) != 1) {
      stop("Population .RData/.rda file must contain exactly one object.")
    }
    load_env[[loaded[1]]]
  } else if (ext %in% c("csv", "txt")) {
    read.csv(path, check.names = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read Excel population inputs.")
    }
    readxl::read_excel(path)
  } else {
    stop("Unsupported population input format: ", path)
  }
}

sae_pick_col <- function(nms, candidates) {
  lower_nms <- tolower(nms)
  hit <- match(tolower(candidates), lower_nms)
  hit <- hit[!is.na(hit)]
  if (length(hit) == 0) NULL else nms[hit[1]]
}

sae_add_population_weight <- function(survey_data,
                                      weight_col = "weight",
                                      hh_size_col = "hh_size",
                                      output_col = "population_weight",
                                      context = "estimation") {
  missing <- c(weight_col, hh_size_col)[!c(weight_col, hh_size_col) %in% names(survey_data)]
  if (length(missing) > 0) {
    stop(
      "Population-weighted ", context, " requires ",
      "population_weight = ", weight_col, " * ", hh_size_col, ". ",
      "Missing required survey column(s): ",
      paste(missing, collapse = ", "),
      ". Map the household-size variable in the app."
    )
  }

  w <- suppressWarnings(as.numeric(survey_data[[weight_col]]))
  h <- suppressWarnings(as.numeric(survey_data[[hh_size_col]]))
  pop_weight <- w * h
  bad <- !is.finite(pop_weight) | pop_weight <= 0
  if (all(bad)) {
    stop(
      "Population-weighted ", context, " could not proceed because all ",
      "computed population weights are missing, non-finite, or non-positive. ",
      "Check ", weight_col, " and ", hh_size_col, "."
    )
  }
  if (any(bad)) {
    stop(
      "Population-weighted ", context, " found ",
      sum(bad), " row(s) with missing, non-finite, or non-positive ",
      "population_weight = ", weight_col, " * ", hh_size_col, ". ",
      "Please clean the weight and household-size columns before running."
    )
  }

  survey_data[[output_col]] <- pop_weight
  survey_data
}

sae_validate_population_matrix <- function(mat, label = "Population") {
  bad <- !is.finite(mat) | mat <= 0
  if (any(bad)) {
    idx <- which(bad, arr.ind = TRUE)
    examples <- utils::head(sprintf(
      "%s/%s",
      rownames(mat)[idx[, 1]],
      colnames(mat)[idx[, 2]]
    ), 10)
    stop(
      label,
      " contains missing, non-finite, or non-positive population totals for ",
      "domain/year cell(s): ",
      paste(examples, collapse = ", "),
      if (nrow(idx) > length(examples)) ", ..." else ""
    )
  }
  mat
}

sae_population_matrix_from_file <- function(path, domain_vec, years_keep) {
  obj <- sae_read_optional_population_table(path)
  if (is.null(obj)) return(NULL)

  nD <- length(domain_vec)
  nT <- length(years_keep)
  domain_chr <- as.character(domain_vec)
  year_chr <- as.character(years_keep)

  if (is.matrix(obj)) {
    mat <- obj
    if (is.null(rownames(mat))) stop("Population matrix must have domain row names.")
    missing_domains <- setdiff(domain_chr, rownames(mat))
    if (length(missing_domains) > 0) {
      stop("Population matrix is missing domain(s): ",
           paste(missing_domains, collapse = ", "))
    }
    mat <- mat[domain_chr, , drop = FALSE]
    if (ncol(mat) != nT) {
      stop("Population matrix must have one column per analysis year.")
    }
    storage.mode(mat) <- "double"
    colnames(mat) <- year_chr
    return(sae_validate_population_matrix(mat, "Population matrix"))
  }

  df <- as.data.frame(obj, check.names = FALSE)
  nms <- names(df)
  domain_col <- sae_pick_col(nms, c("domain", "prov", "area", "area_id"))
  year_col <- sae_pick_col(nms, c("year", "time", "period"))
  value_col <- sae_pick_col(nms, c("Nd", "N_d", "population", "pop", "N"))

  if (!is.null(domain_col) && !is.null(year_col) && !is.null(value_col)) {
    mat <- matrix(NA_real_, nrow = nD, ncol = nT,
                  dimnames = list(domain_chr, year_chr))
    for (tt in seq_along(years_keep)) {
      idx <- as.character(df[[year_col]]) == year_chr[tt]
      vals <- suppressWarnings(as.numeric(df[[value_col]][idx]))
      ids <- as.character(df[[domain_col]][idx])
      common <- intersect(domain_chr, ids)
      mat[common, tt] <- vals[match(common, ids)]
    }
    return(sae_validate_population_matrix(mat, "Population table"))
  }

  if (!is.null(domain_col)) {
    mat <- matrix(NA_real_, nrow = nD, ncol = nT,
                  dimnames = list(domain_chr, year_chr))
    for (tt in seq_along(years_keep)) {
      candidates <- c(
        year_chr[tt],
        paste0("Nd_", year_chr[tt]),
        paste0("N_", year_chr[tt]),
        paste0("population_", year_chr[tt]),
        paste0("pop_", year_chr[tt])
      )
      col <- sae_pick_col(nms, candidates)
      if (is.null(col)) {
        stop("Population file is missing a column for year ", year_chr[tt], ".")
      }
      ids <- as.character(df[[domain_col]])
      mat[domain_chr, tt] <- suppressWarnings(as.numeric(df[[col]][match(domain_chr, ids)]))
    }
    return(sae_validate_population_matrix(mat, "Population table"))
  }

  stop("Population input must be a matrix with domain row names, a long table ",
       "(domain/year/population), or a wide table with one row per domain.")
}

sae_population_matrix_from_survey <- function(survey_data,
                                              domain_vec,
                                              years_keep,
                                              domain_col = "domain",
                                              year_col = "year",
                                              weight_col = "weight",
                                              hh_size_col = "hh_size") {
  required <- c(domain_col, year_col, weight_col, hh_size_col)
  missing <- required[!required %in% names(survey_data)]
  if (length(missing) > 0) {
    stop(
      "No population file was supplied, so benchmarking must estimate ",
      "domain populations from the survey using weight * household size. ",
      "Missing required survey column(s): ",
      paste(missing, collapse = ", "),
      ". Map the household-size variable in the app or supply a population file."
    )
  }

  domain_chr <- as.character(domain_vec)
  year_chr <- as.character(years_keep)
  mat <- matrix(NA_real_, nrow = length(domain_chr), ncol = length(year_chr),
                dimnames = list(domain_chr, year_chr))

  w <- suppressWarnings(as.numeric(survey_data[[weight_col]]))
  h <- suppressWarnings(as.numeric(survey_data[[hh_size_col]]))
  pop_weight <- w * h
  df <- data.frame(
    domain = as.character(survey_data[[domain_col]]),
    year = as.character(survey_data[[year_col]]),
    pop_weight = pop_weight,
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$pop_weight), , drop = FALSE]

  if (nrow(df) == 0) {
    stop("Survey-derived population weights are all missing/non-finite. ",
         "Check the weight and household-size columns.")
  }

  agg <- stats::aggregate(pop_weight ~ domain + year, data = df, FUN = sum)
  for (tt in seq_along(year_chr)) {
    rows <- agg$year == year_chr[tt]
    ids <- agg$domain[rows]
    vals <- agg$pop_weight[rows]
    common <- intersect(domain_chr, ids)
    mat[common, tt] <- vals[match(common, ids)]
  }

  sae_validate_population_matrix(mat, "Survey-derived population totals")
}

sae_resolve_population_matrix <- function(population_path,
                                          survey_data,
                                          domain_vec,
                                          years_keep,
                                          hh_size_col = "hh_size",
                                          context = "benchmarking") {
  if (!is.null(population_path) && length(population_path) > 0 &&
      nzchar(population_path)) {
    mat <- sae_population_matrix_from_file(population_path, domain_vec, years_keep)
    cat("Using external domain population sizes for ", context, ": ",
        population_path, "\n", sep = "")
    return(mat)
  }

  mat <- sae_population_matrix_from_survey(
    survey_data = survey_data,
    domain_vec = domain_vec,
    years_keep = years_keep,
    hh_size_col = hh_size_col
  )
  cat("No population file supplied; estimated domain populations for ",
      context, " as sum(weight * household size) by domain/year.\n", sep = "")
  mat
}
