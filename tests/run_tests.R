failures <- character()
check <- function(ok, label) {
  if (!isTRUE(ok)) failures <<- c(failures, label)
  cat(if (isTRUE(ok)) "PASS" else "FAIL", "-", label, "\n")
}
read_all <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

r_files <- c(list.files("R", "\\.R$", full.names = TRUE),
             list.files("scripts", "\\.R$", full.names = TRUE),
             "app.R", "app_wizard.R", "app_support.R", "install_packages.R",
             "tools/check_ui_parity.R", "tools/build_wizard_manifest.R")
parse_ok <- vapply(r_files, function(path) {
  tryCatch({ parse(path, encoding = "UTF-8"); TRUE }, error = function(e) {
    cat("Parse error in", path, ":", conditionMessage(e), "\n"); FALSE
  })
}, logical(1))
check(all(parse_ok), "all R sources parse")
check(identical(trimws(readLines("VERSION", warn = FALSE)[1]), "5.2.0-rc.6"), "VERSION is the candidate version")
check(identical(trimws(readLines("WIZARD_VERSION", warn = FALSE)[1]),
                "5.2.0-rc.6-wizard.4-crossplatform"),
      "WIZARD_VERSION identifies the rc.6 wizard overlay")
wizard_version <- trimws(readLines("WIZARD_VERSION", warn = FALSE)[1])
changelog_text <- read_all("docs/CHANGELOG.md")
check(grepl(paste0("## ", wizard_version), changelog_text, fixed = TRUE),
      "CHANGELOG has an entry for the current WIZARD_VERSION")
check(!grepl("\n\\+", changelog_text),
      "CHANGELOG contains no pasted diff markers")

app_text <- read_all("app.R")
wizard_text <- read_all("app_wizard.R")
support_text <- read_all("app_support.R")
mfh_text <- read_all("scripts/02_mfh.R")
ufh_text <- read_all("scripts/01_ufh.R")
comparison_text <- read_all("scripts/03_comparison.R")
wizard_manifest_text <- read_all("tools/build_wizard_manifest.R")
ai_text <- read_all("R/comparison_report_ai.R")
report_text <- read_all("report.Rmd")
wizard_required_ids <- c("country_name", "analysis_seed", "benchmark_level",
                         "mfh_diag_model", "fit_mfh3", "mfh_refvar_adjustment",
                         "mcpe_bootstrap_replicates", "llm_external_consent")
wizard_has_required_ids <- vapply(
  wizard_required_ids,
  function(id) grepl(sprintf('\\"%s\\"', id), wizard_text),
  logical(1)
)
check(all(wizard_has_required_ids),
      "wizard exposes all rc.6 dashboard controls")
check(grepl('mfh_diag_model = "MFH2"', app_text, fixed = TRUE) &&
        grepl('fit_mfh3 = FALSE', app_text, fixed = TRUE) &&
        grepl('"MFH2 (default)" = "MFH2"', app_text, fixed = TRUE) &&
        grepl('"MFH2 (default)" = "MFH2"', wizard_text, fixed = TRUE) &&
        !grepl('"Automatic: MFH3 test, otherwise MFH2"',
               paste(app_text, wizard_text), fixed = TRUE),
      "MFH2 is the default MFH choice in both interfaces")
check(grepl('diag_model_requested %in% c("AUTO", "MFH3")', mfh_text,
            fixed = TRUE) &&
        grepl("sae_choose_mfh_variance_structure", mfh_text, fixed = TRUE),
      "selecting MFH3 invokes Molina-Romero selection and fallback")
check(grepl('significance_rule = "pointwise_unadjusted"', ufh_text, fixed = TRUE) &&
        grepl("significant = significant_unadjusted", ufh_text, fixed = TRUE) &&
        grepl("p_value_bonferroni", ufh_text, fixed = TRUE) &&
        grepl("significant_bonferroni", ufh_text, fixed = TRUE) &&
        !grepl("significant = p_value_bh < alpha", ufh_text, fixed = TRUE),
      "UFH keeps pointwise significance primary and reports BH/Bonferroni sensitivities")
check(grepl('significance_rule = "pointwise_unadjusted"', mfh_text, fixed = TRUE) &&
        grepl("significant = significant_unadjusted", mfh_text, fixed = TRUE) &&
        grepl("p_value_bonferroni", mfh_text, fixed = TRUE) &&
        grepl("significant_bonferroni", mfh_text, fixed = TRUE) &&
        grepl("$significant <- comp12_obj$df$significant_unadjusted", mfh_text, fixed = TRUE),
      "MFH keeps pointwise significance primary and reports BH/Bonferroni sensitivities")
check(grepl("bars are pointwise 95% confidence intervals", comparison_text, fixed = TRUE),
      "comparison figures disclose the pointwise significance rule")
check(grepl("p_value = p_raw", comparison_text, fixed = TRUE) &&
        grepl("p_value_bh = p_bh", comparison_text, fixed = TRUE) &&
        grepl("p_value_bonferroni = p_bonferroni", comparison_text, fixed = TRUE) &&
        grepl("Bonferroni-adjusted p-value", report_text, fixed = TRUE),
      "comparison export and report retain raw, BH, and Bonferroni change results")
check(grepl("MFH3 reference-variance test adjustment", app_text, fixed = TRUE) &&
        grepl("MFH3 reference-variance test adjustment", wizard_text, fixed = TRUE) &&
        grepl("input.mfh_diag_model == 'MFH3' || input.fit_mfh3", app_text, fixed = TRUE) &&
        grepl("input.mfh_diag_model == 'MFH3' || input.fit_mfh3", wizard_text, fixed = TRUE),
      "MFH3 variance-test adjustment is clearly labelled and conditionally displayed")
check(grepl("candidate_vars_override", ufh_text, fixed = TRUE) &&
        grepl("!isTRUE(ufh_lasso_enabled)", ufh_text, fixed = TRUE),
      "UFH custom covariates are LASSO pools or fixed specifications as selected")
check(grepl("!isTRUE(mfh_lasso_enabled)", mfh_text, fixed = TRUE) &&
        grepl("candidate_vars_per_year", mfh_text, fixed = TRUE),
      "MFH custom covariates are LASSO pools or fixed specifications as selected")
check(grepl("mfh3_failure_type", mfh_text, fixed = TRUE) &&
        grepl("mfh3_nonconvergence", read_all("R/mfh_model_selection.R"), fixed = TRUE) &&
        grepl("mfh3_error", read_all("R/mfh_model_selection.R"), fixed = TRUE),
      "MFH3 non-convergence and computational errors are distinguished")
check(grepl(".mcpe_unavailable_reason", mfh_text, fixed = TRUE) &&
        !grepl("When `var_choice = \\\"direct\\\"`, `eblupMFH2()` often", mfh_text, fixed = TRUE),
      "MCPE-unavailable message reflects the selected model and actual condition")
check(grepl("benchmark_status.csv", comparison_text, fixed = TRUE) &&
        grepl("Benchmarking Unavailable", comparison_text, fixed = TRUE),
      "Comparison records and reports unavailable boundary benchmarking")
source("R/release_controls.R")
source("R/release_packaging.R")
inventory <- sae_release_inventory(".")
check(all(c("Data/Spain/survey.rds", "Data/Spain/auxiliary.rds", "Data/Spain/shapefile.rds",
            "Data/simulated/survey_example.csv", "Data/simulated/auxiliary_example.csv",
            "Data/simulated/geometry_example.geojson") %in% inventory) &&
        !any(grepl("^(app_runs/|docs/internal/)|Rplots|package_versions.local", inventory)) &&
        identical(inventory[startsWith(inventory, "docs/guidance/literature/")],
                  "docs/guidance/literature/README.md"),
      "release inventory contains approved examples and excludes internal/generated/literature files")
check(grepl("sae_write_release_manifest", wizard_manifest_text, fixed = TRUE),
      "wizard manifest delegates to the shared inventory implementation")
wizard_resources <- c(
  "docs/guidance/guidelines_v5_2_0_rc6_wizard.docx",
  "docs/MCPE_VALIDATION_STATUS.md",
  "docs/instructions/EU_SAE_Download_Instructions_5_2_0_rc_6_wizard_4_crossplatform.pdf",
  "docs/instructions/EU_SAE_User_Guide_5_2_0_rc_6_wizard_4_crossplatform.pptx"
)
check(all(vapply(wizard_resources, file.exists, logical(1))) &&
        all(vapply(wizard_resources, grepl, logical(1), x = wizard_text,
                   fixed = TRUE)) &&
        !grepl("v5_1_0", wizard_text, fixed = TRUE),
      "wizard resources reference only shipped rc.6 guidance")
brief_text <- read_all("R/brief_generator.R")
check(!grepl('country\\s*=\\s*"Greece"', paste(app_text, brief_text)), "no hard-coded Greece report country")
check(!grepl("nB\\s*=\\s*50([,)]|\\s)", mfh_text), "MCPE is not hard-coded to 50")
source("R/variance_policy.R")
variance_direct <- c(NA, -0.1, 0, 0.000999, 0.001, 0.002, 10, Inf)
variance_smoothed <- seq(0.01, 0.08, by = 0.01)
variance_expected <- c(variance_smoothed[1:4], 0.001, 0.002, 10,
                       variance_smoothed[8])
check(isTRUE(all.equal(
        sae_sm_out_variance(variance_direct, variance_smoothed),
        variance_expected
      )) &&
        grepl("sae_sm_out_variance(vardir, var_smooth)", ufh_text, fixed = TRUE) &&
        grepl("sae_sm_out_variance(v1, v1_sm_all)", mfh_text, fixed = TRUE) &&
        !grepl("upper_mult", mfh_text, fixed = TRUE),
      "UFH and MFH sm_out replace only non-finite or below-0.001 direct variances")
check(grepl('"National" = "national"', app_text, fixed = TRUE) &&
        grepl('"National" = "national"', wizard_text, fixed = TRUE) &&
        grepl('benchmark_level = "national"', app_text, fixed = TRUE),
      "classic and wizard interfaces expose national benchmarking explicitly")
source("scripts/bench_regional_mfh.R")
.bench_est <- matrix(c(0.10, 0.20, 0.30, 0.40, 0.15, 0.25), nrow = 3)
.bench_dir <- matrix(c(0.12, 0.18, 0.33, 0.36, 0.14, 0.29), nrow = 3)
.bench_pop <- matrix(c(100, 200, 300, 110, 210, 310), nrow = 3)
rownames(.bench_est) <- rownames(.bench_dir) <- rownames(.bench_pop) <- c("a", "b", "c")
colnames(.bench_est) <- colnames(.bench_dir) <- colnames(.bench_pop) <- c("1", "2")
.bench_result <- bench_regional_mfh(
  eblup_mat = .bench_est, domain_vec = c("a", "b", "c"),
  region_vec = rep("national", 3), Nd_vec = .bench_pop[, 1],
  Nd_mat = .bench_pop, direct_mat = .bench_dir, MSE = FALSE
)
.bench_actual <- colSums(.bench_result$eblup_bench * .bench_pop) / colSums(.bench_pop)
.bench_target <- colSums(.bench_dir * .bench_pop) / colSums(.bench_pop)
check(isTRUE(all.equal(.bench_actual, .bench_target, tolerance = 1e-12)),
      "national MFH benchmarking reproduces the population-weighted direct target")
source("R/llm_assistant.R")
old_anthropic_model <- Sys.getenv("SAE_ANTHROPIC_MODEL", unset = NA_character_)
Sys.unsetenv("SAE_ANTHROPIC_MODEL")
check(identical(default_llm_model("anthropic"), "claude-sonnet-4-6") &&
        !grepl("claude-sonnet-4-20250514", paste(app_text, read_all("R/llm_assistant.R"),
                                                  read_all("R/normality_evaluator.R"))),
      "Anthropic integration no longer uses the retired Claude Sonnet 4 model")
if (!is.na(old_anthropic_model)) Sys.setenv(SAE_ANTHROPIC_MODEL = old_anthropic_model)
check(length(gregexpr("rmarkdown::render\\(", app_text, fixed = FALSE)[[1]][gregexpr("rmarkdown::render\\(", app_text)[[1]] > 0]) == 0,
      "dashboard does not duplicate report rendering")
check(grepl("rmarkdown::render", support_text, fixed = TRUE), "pipeline performs final report rendering")
check(!grepl("MFH detects more significant domains", ai_text, fixed = TRUE), "AI prompt has no fixed MFH superiority conclusion")
check(grepl("Bonferroni-adjusted p-values", ai_text, fixed = TRUE) &&
        grepl("median paired percent reduction", ai_text, fixed = TRUE) &&
        grepl("narrower interval does not guarantee significance", ai_text, fixed = TRUE),
      "AI significance prompt covers adjusted counts and CI-width interpretation")
check(grepl("top_ranked_values", ai_text, fixed = TRUE) && !grepl("top_domains", ai_text, fixed = TRUE),
      "AI comparison prompt anonymizes top geographic ranks")
check(grepl("render_report     = FALSE", app_text, fixed = TRUE) &&
        grepl("render_final_report(", app_text, fixed = TRUE),
      "dashboard defers the single final report render until AI status is ready")
check(grepl("params      = list(include_ai", support_text, fixed = TRUE) &&
        grepl("render_final_report <- function", support_text, fixed = TRUE),
      "report helper receives the AI inclusion parameter")
check(dir.exists("outputs/figures"), "figures output directory exists after clean or completed runs")

source("R/input_readers.R")
bad_rds <- tempfile(fileext = ".rds")
saveRDS(letters, bad_rds)
rejected <- inherits(try(sae_read_table_input(bad_rds, "test"), silent = TRUE), "try-error")
unlink(bad_rds)
check(rejected, "non-tabular RDS input is rejected")

source("R/release_controls.R")
check(identical(sae_app_version(), "5.2.0-rc.6"), "release helper reads VERSION")
source("R/comparison_report_ai.R")
section_keys <- names(comparison_ai_sections())
saved_prompt_builder <- build_comparison_ai_prompts
build_comparison_ai_prompts <- function(...) {
  c(list(system = "mock system prompt"),
    setNames(as.list(paste0("mock prompt ", section_keys)), section_keys))
}
mock_all <- list(
  enabled = TRUE,
  provider = "mock-provider",
  model = "mock-model",
  query = function(prompt, system_prompt = NULL) paste("Interpretation for", prompt)
)
all_state <- generate_comparison_ai_interpretations(
  mock_all, consent_recorded = TRUE, logger = function(...) NULL
)
all_status <- vapply(all_state$sections, function(x) x$status, character(1))
check(all(all_status == "generated") &&
        identical(all_state$provider, "mock-provider") &&
        identical(all_state$model, "mock-model"),
      "structured AI state records complete generation and provider metadata")
numeric_ok <- comparison_ai_numeric_qa("The estimate is 12.5.",
                                       "Input estimate: 12.5000")
numeric_warning <- comparison_ai_numeric_qa("The estimate is 99.",
                                            "Input estimate: 12.5")
check(identical(numeric_ok$status, "passed") &&
        identical(numeric_warning$status, "warning") &&
        identical(numeric_warning$unverified_numbers, 99),
      "AI numeric provenance check flags unsupported numbers without blocking the report")

mock_partial <- mock_all
mock_partial$query <- function(prompt, system_prompt = NULL) {
  if (grepl("normality", prompt, fixed = TRUE)) {
    stop("provider failure for sk-test-secret-12345678")
  }
  paste("Interpretation for", prompt)
}
partial_state <- generate_comparison_ai_interpretations(
  mock_partial, consent_recorded = TRUE, logger = function(...) NULL
)
partial_status <- vapply(partial_state$sections, function(x) x$status, character(1))
check(identical(partial_status[["normality"]], "failed") &&
        sum(partial_status == "generated") == length(section_keys) - 1L &&
        !grepl("sk-test-secret", partial_state$sections$normality$failure_reason,
               fixed = TRUE),
      "partial AI failure is explicit and provider secrets are redacted")

off_state <- new_comparison_ai_interpretation_state()
off_status <- vapply(off_state$sections, function(x) x$status, character(1))
check(all(off_status == "disabled") && !isTRUE(off_state$requested),
      "AI-off state records all sections as disabled")

html_state <- all_state
html_state$sections$overview$text <- "<script>alert('unsafe')</script>"
appendix_html <- comparison_ai_appendix_html(html_state, include_ai = TRUE)
check(!grepl("<script>", appendix_html, fixed = TRUE) &&
        grepl("&lt;script&gt;", appendix_html, fixed = TRUE) &&
        grepl("Human review is required", appendix_html, fixed = TRUE),
      "integrated AI blocks escape provider text and require human review")

ai_tmp <- tempfile("ai-state-")
dir.create(ai_tmp)
ai_state_file <- file.path(ai_tmp, "ai_interpretations.rds")
ai_meta_file <- file.path(ai_tmp, "run_metadata.rds")
saveRDS(list(application_version = sae_app_version()), ai_meta_file)
save_comparison_ai_interpretations(all_state, ai_state_file, ai_meta_file)
saved_ai_state <- readRDS(ai_state_file)
saved_ai_meta <- readRDS(ai_meta_file)
check(identical(saved_ai_state$schema_version, 1L) &&
        length(saved_ai_meta$ai_interpretations$generated_sections) == length(section_keys),
      "AI state and per-section run metadata are saved together")
unlink(ai_tmp, recursive = TRUE)
build_comparison_ai_prompts <- saved_prompt_builder
manifest_inputs <- setNames(
  vapply(c("survey", "auxiliary", "geometry"), function(role) {
    path <- tempfile(pattern = paste0("manifest-", role, "-"), fileext = ".rds")
    saveRDS(data.frame(value = 1), path)
    path
  }, character(1)),
  c("survey", "auxiliary", "geometry")
)
manifest_cfg <- list(
  data_inputs = list(
    survey_path = manifest_inputs[["survey"]],
    rhs_path = manifest_inputs[["auxiliary"]],
    shp_path = manifest_inputs[["geometry"]]
  ),
  benchmarking = list(target_path = NULL),
  ufh = list(population_path = NULL),
  mfh = list()
)
input_manifest <- sae_input_manifest(manifest_cfg)
check(nrow(input_manifest) == 3L &&
        identical(as.character(input_manifest$input_role),
                  c("survey", "auxiliary", "geometry")) &&
        all(!is.na(input_manifest$sha256)),
      "input manifest preserves roles when optional paths are absent")
unlink(unname(manifest_inputs))
check(grepl('pipeline_logger(paste("ERROR:", err_msg))', app_text, fixed = TRUE),
      "pipeline failures are persisted to the run log")
root_anchor_specs <- c(
  "scripts/01_ufh.R" = 'here::i_am("scripts/01_ufh.R")',
  "scripts/02_mfh.R" = 'here::i_am("scripts/02_mfh.R")',
  "scripts/03_comparison.R" = 'here::i_am("scripts/03_comparison.R")'
)
check(all(vapply(names(root_anchor_specs), function(path) {
  grepl(root_anchor_specs[[path]], read_all(path), fixed = TRUE)
}, logical(1))),
      "pipeline scripts anchor here paths to the running package copy")

robust_mfh2_text <- read_all("scripts/eblupMFH2_robust.R")
check(!grepl("if (all(is.finite(FI))) FI[i, j] else 0", robust_mfh2_text, fixed = TRUE),
      "robust MFH2 does not silently replace unavailable Fisher inverse terms with zero")
check(grepl("MSE is returned as NA rather than understated", robust_mfh2_text, fixed = TRUE),
      "robust MFH2 reports unavailable g3/MSE")
source("scripts/eblupMFH2_robust.R")
inv_pd <- .safe_sym_inverse(diag(2), "test positive-definite matrix")
inv_ill <- .safe_sym_inverse(diag(c(1, 1e-10)), "test ill-conditioned positive-definite matrix")
inv_singular <- suppressWarnings(.safe_sym_inverse(matrix(1, 2, 2), "test singular matrix"))
inv_bad <- suppressWarnings(.safe_sym_inverse(matrix(c(NA, 0, 0, 1), 2), "test non-finite matrix"))
check(isTRUE(inv_pd$available) && identical(inv_pd$method, "chol2inv"),
      "positive-definite inverse uses Cholesky")
check(isTRUE(inv_ill$available) && identical(inv_ill$method, "chol2inv") &&
        is.finite(inv_ill$condition_number) && inv_ill$condition_number > 1e8 &&
        identical(inv_ill$condition_number_method,
                  "chol_factor_kappa_squared_estimate"),
      "ill-conditioned Cholesky path records a condition-number estimate")
check(isTRUE(inv_singular$available) && identical(inv_singular$method, "MASS::ginv"),
      "singular Fisher-style matrix uses an explicit generalized inverse")
check(!isTRUE(inv_bad$available) && is.null(inv_bad$inverse),
      "non-finite matrix is reported unavailable")

bootstrap_files <- c(
  "scripts/mcpe_functions.R",
  "scripts/pbmcpeMFH1_with_existing_eblup.R",
  "scripts/pbmcpeMFH2_with_existing_eblup.R",
  "scripts/pbmcpeMFH3_with_existing_eblup.R",
  "scripts/bench_regional_mfh.R"
)
bootstrap_text <- paste(vapply(bootstrap_files, read_all, character(1)), collapse = "\n")
check(!grepl("1:(nT - 1)", bootstrap_text, fixed = TRUE) &&
        !grepl("for (tt in 2:nT)", bootstrap_text, fixed = TRUE) &&
        !grepl("for(t in 2:nT)", bootstrap_text, fixed = TRUE),
      "MFH bootstrap time loops are safe when nT equals one")
unsafe_convergence_patterns <- c(
  "!\\s*[[:alnum:]_.]+\\$fit\\$convergence",
  "!\\s*fit\\$convergence"
)
check(!any(vapply(unsafe_convergence_patterns, grepl, logical(1),
                  x = bootstrap_text, perl = TRUE)),
      "MFH bootstrap convergence checks handle missing or non-logical values")

if (requireNamespace("msae", quietly = TRUE)) {
  source("R/msae_compat.R")
  data("datasae3", package = "msae")
  patched_mfh3 <- sae_patch_msae_function("eblupMFH3")
  mfh3_formula <- list(
    f1 = Y1 ~ X1 + X2,
    f2 = Y2 ~ X1 + X2,
    f3 = Y3 ~ X1 + X2
  )
  mfh3_vardir <- c("v1", "v2", "v3", "v12", "v13", "v23")
  mfh3_fit <- try(patched_mfh3(mfh3_formula, vardir = mfh3_vardir,
                                data = datasae3, MAXITER = 100,
                                PRECISION = 1e-4), silent = TRUE)
  check(!inherits(mfh3_fit, "try-error") && is.list(mfh3_fit) &&
          isTRUE(mfh3_fit$fit$convergence),
        "MFH3 compatibility patch preserves missing arguments and convergence")
} else {
  cat("SKIP - MFH3 compatibility execution (msae not installed)\n")
}

diagnostic_tmp <- tempfile("mfh-diagnostics-")
dir.create(file.path(diagnostic_tmp, "data"), recursive = TRUE)
dir.create(file.path(diagnostic_tmp, "tables"), recursive = TRUE)
dummy_model <- list(fit = list(numerical_diagnostics = list(
  marginal_covariance = list(method = "chol2inv", condition_number = 12,
                             condition_number_method = "chol_factor_kappa_squared_estimate",
                             available = TRUE),
  fixed_effect_information = list(method = "MASS::ginv", condition_number = Inf,
                                  condition_number_method = "exact_kappa", available = TRUE),
  fisher_information = list(method = NA_character_, condition_number = Inf,
                            condition_number_method = "exact_kappa", available = FALSE),
  g3_available = FALSE
)))
attr(dummy_model, ".robust_refit_used") <- TRUE
saveRDS(list(selected_model = dummy_model, diag_model = "MFH2"),
        file.path(diagnostic_tmp, "data", "mfh_artifacts.rds"))
saveRDS(list(application_version = sae_app_version()),
        file.path(diagnostic_tmp, "data", "run_metadata.rds"))
diag_summary <- sae_record_mfh_numerical_diagnostics(
  artifacts_path = file.path(diagnostic_tmp, "data", "mfh_artifacts.rds"),
  metadata_path = file.path(diagnostic_tmp, "data", "run_metadata.rds"),
  tables_dir = file.path(diagnostic_tmp, "tables")
)
diag_rows <- read.csv(file.path(diagnostic_tmp, "tables", "mfh_numerical_diagnostics.csv"))
diag_meta <- readRDS(file.path(diagnostic_tmp, "data", "run_metadata.rds"))
check(nrow(diag_rows) == 3L &&
        identical(as.character(diag_rows$mse_status), rep("unavailable_fisher_information", 3L)) &&
        "condition_number_method" %in% names(diag_rows) &&
        isTRUE(diag_summary$requires_attention),
      "MFH numerical diagnostics expose inverse methods and unavailable g3/MSE")
check(is.list(diag_meta$mfh_numerical_diagnostics) &&
        identical(diag_meta$mfh_numerical_diagnostics$selected_model, "MFH2"),
      "MFH numerical diagnostics are persisted in run metadata")
unlink(diagnostic_tmp, recursive = TRUE)

builder_text <- read_all("scripts/build_clean_release.R")
check(grepl("sae_release_inventory", builder_text, fixed = TRUE) &&
        grepl("sae_verify_release", builder_text, fixed = TRUE) &&
        grepl("Refusing to overwrite", builder_text, fixed = TRUE),
      "clean builder uses explicit inventory, verifies contents and refuses overwriting")
check(identical(gsub("rc_", "rc", gsub("[.-]", "_", "5.2.0-rc.6"),
                       fixed = TRUE), "5_2_0_rc6"),
      "release version maps to the document filename token")
check(!any(startsWith(inventory, "wizard_build/")),
      "clean builder excludes the separate wizard work area")
report_text <- read_all("report.Rmd")
check(grepl("print(kable(mfh_numerical_diag", report_text, fixed = TRUE),
      "MFH numerical diagnostics table is explicitly printed")
check(grepl('here::i_am("report.Rmd")', report_text, fixed = TRUE),
      "final report anchors here paths to the running package copy")
ai_report_keys <- c("overview", "normality", "rates", "precision",
                    "change_significance", "poverty_maps", "change_maps")
check(grepl("params:", report_text, fixed = TRUE) &&
        grepl("include_ai: false", report_text, fixed = TRUE) &&
        !grepl("AI-Assisted Interpretation Appendix", report_text, fixed = TRUE) &&
        all(vapply(ai_report_keys, function(key) {
          grepl(sprintf('comparison_ai_section_html(ai_state, "%s"', key),
                report_text, fixed = TRUE)
        }, logical(1))),
      "final report interleaves labelled AI blocks and supports statistical-only rendering")
check(grepl("Complete statistical significance comparison", report_text, fixed = TRUE) &&
        !grepl("head(sig_display, 30)", report_text, fixed = TRUE) &&
        grepl("full-significance-table", report_text, fixed = TRUE),
      "final report displays the complete pointwise/BH/Bonferroni table in a scrollable pane")
check(grepl("ci_width_distribution_ufh_mfh.png", report_text, fixed = TRUE) &&
        grepl("ci_width_paired_ufh_mfh.png", report_text, fixed = TRUE) &&
        grepl("ci_width_comparison.xlsx", report_text, fixed = TRUE) &&
        grepl("EU_SAE_results.xlsx", report_text, fixed = TRUE),
      "final report integrates CI-width figures and Excel result links")
check(!grepl("render_comparison_ai_note", app_text, fixed = TRUE) &&
        !grepl('"outputs/comparison_ai_note.html"', app_text, fixed = TRUE),
      "dashboard produces one combined human-readable report")

source("R/pipeline_helpers.R")
ess_boundary <- sae_effective_sample_size(
  sample_size = c(12, 20, 30),
  design_variance = c(0, 0.01, 0),
  srs_variance = c(0, 0.005, 0.02)
)
check(identical(ess_boundary$effective_sample_size_fallback,
                c(TRUE, FALSE, TRUE)) &&
        identical(ess_boundary$n_eff, c(12, 10, 30)) &&
        all(is.finite(ess_boundary$n_eff)) &&
        all(ess_boundary$n_eff > 0),
      "effective sample size is finite for zero-rate and degenerate domains")
ess_bad_n <- inherits(
  try(sae_effective_sample_size(c(0, 10), c(0, 0.01), c(0, 0.01)),
      silent = TRUE),
  "try-error"
)
check(ess_bad_n, "effective sample size rejects non-positive domain sample sizes")
fold_a <- sae_cv_foldid(41L, 7L, seed = 77L)
fold_b <- sae_cv_foldid(41L, 7L, seed = 77L)
check(identical(fold_a, fold_b) && setequal(unique(fold_a), seq_len(7L)),
      "cross-validation folds are deterministic for a fixed seed")
if (requireNamespace("glmnet", quietly = TRUE)) {
  set.seed(1)
  d <- data.frame(y = rnorm(40), x1 = rnorm(40), x2 = rnorm(40), x3 = rnorm(40))
  a <- sae_lasso_screen(d, c("x1", "x2", "x3"), "y", TRUE, seed = 77L)
  b <- sae_lasso_screen(d, c("x1", "x2", "x3"), "y", TRUE, seed = 77L)
  check(identical(a$diagnostics$selected_variables, b$diagnostics$selected_variables),
        "LASSO screening is deterministic for a fixed seed")
} else {
  cat("SKIP - deterministic LASSO execution (glmnet not installed)\n")
}

check(identical(stats::p.adjust(c(0.01, 0.03, 0.2), "BH"), c(0.03, 0.045, 0.2)),
      "BH multiplicity adjustment reference")

source("R/mfh_model_selection.R")
.mfh3_nonconverged <- list(fit = list(
  convergence = FALSE,
  refvarTest = data.frame(refvar = "Y1 vs Y2", `T-test` = 2,
                          `p-value` = 0.01, check.names = FALSE)
))
.mfh_nonconv_choice <- sae_choose_mfh_variance_structure(.mfh3_nonconverged)
check(identical(.mfh_nonconv_choice$selected_model, "MFH2") &&
        identical(.mfh_nonconv_choice$reason_code, "mfh3_nonconvergence"),
      "MFH3 non-convergence automatically selects MFH2")

.mfh3_heterogeneous <- list(fit = list(
  convergence = TRUE,
  refvarTest = data.frame(
    refvar = c("Y1 vs Y2", "Y1 vs Y3", "Y2 vs Y3"),
    `T-test` = c(2.6, 1.4, 0.8),
    `p-value` = c(0.01, 0.08, 0.20),
    check.names = FALSE
  )
))
.mfh_hetero_choice <- sae_choose_mfh_variance_structure(
  .mfh3_heterogeneous, adjustment = "bonferroni"
)
check(identical(.mfh_hetero_choice$selected_model, "MFH3") &&
        isTRUE(all.equal(.mfh_hetero_choice$tests$p_value_bonferroni,
                         c(0.03, 0.24, 0.60))) &&
        isTRUE(all.equal(.mfh_hetero_choice$tests$p_value_bh,
                         c(0.03, 0.12, 0.20))),
      "MFH3 variance test exports raw, Bonferroni, and BH decisions")

.mfh3_homogeneous <- list(fit = list(
  convergence = TRUE,
  refvarTest = data.frame(refvar = "Y1 vs Y2", `T-test` = 0.5,
                          `p-value` = 0.62, check.names = FALSE)
))
.mfh_homo_choice <- sae_choose_mfh_variance_structure(.mfh3_homogeneous)
check(identical(.mfh_homo_choice$selected_model, "MFH2") &&
        identical(.mfh_homo_choice$tests$p_value_raw,
                  .mfh_homo_choice$tests$p_value_bonferroni) &&
        identical(.mfh_homo_choice$tests$p_value_raw,
                  .mfh_homo_choice$tests$p_value_bh),
      "two-year MFH3 test selects MFH2 and has no multiplicity difference")

source("R/mfh_bootstrap_helpers.R")
set.seed(90210)
.mfh3_refvar <- c(0.20, 0.40, 0.60)
.mfh3_rho <- 0.50
.mfh3_theory <- sae_mfh_random_effect_covariance(
  "MFH3", .mfh3_refvar, .mfh3_rho, nT = 3L
)
.mfh3_draws <- sae_simulate_mfh_random_effects(
  "MFH3", nD = 100000L, nT = 3L,
  refvar = .mfh3_refvar, rho = .mfh3_rho
)
check(isTRUE(all.equal(.mfh3_theory[1, 1],
                       .mfh3_rho^2 + .mfh3_refvar[1], tolerance = 1e-12)) &&
        max(abs(stats::cov(.mfh3_draws) - .mfh3_theory)) < 0.02,
      "MFH3 bootstrap DGP matches the Molina-Romero initial-state covariance")

.mcpe_wrapper_text <- paste(
  read_all("scripts/pbmcpeMFH1_with_existing_eblup.R"),
  read_all("scripts/pbmcpeMFH2_with_existing_eblup.R"),
  read_all("scripts/pbmcpeMFH3_with_existing_eblup.R")
)
check(grepl("sae_bootstrap_mcse", .mcpe_wrapper_text, fixed = TRUE) &&
        !grepl("udt_b[i] <- adt_b[i]", .mcpe_wrapper_text, fixed = TRUE),
      "MFH bootstrap reports Monte Carlo error and MFH3 includes its initial state")


# Path migration tests do not load example data or the statistical pipeline.
source("R/input_paths.R")
path_fixture <- tempfile("sae_input_paths_")
dir.create(file.path(path_fixture, "Data", "Spain"), recursive = TRUE)
dir.create(file.path(path_fixture, "Data", "simulated"), recursive = TRUE)
writeLines("Spain fixture", file.path(path_fixture, "Data", "Spain", "survey.rds"))
writeLines("Simulated fixture", file.path(path_fixture, "Data", "simulated", "survey_example.csv"))
expected_spain <- normalizePath(file.path(path_fixture, "Data", "Spain", "survey.rds"), winslash = "/")
expected_simulated <- normalizePath(file.path(path_fixture, "Data", "simulated", "survey_example.csv"), winslash = "/")
check(identical(sae_resolve_input_path("data/survey.rds", path_fixture), expected_spain) &&
        identical(sae_resolve_input_path("example_data\\survey_example.csv", path_fixture), expected_simulated),
      "legacy package-relative example references migrate to the same file")
check(identical(sae_resolve_input_path("survey.rds", path_fixture, allow_basename = TRUE), expected_spain),
      "unambiguous saved bare filename resolves after reorganization")
missing_absolute <- paste0(normalizePath(path_fixture, winslash = "/"), "/old_copy/data/survey.rds")
check(identical(sae_resolve_input_path(missing_absolute, path_fixture, TRUE), missing_absolute) &&
        !file.exists(sae_resolve_input_path("customer/survey.rds", path_fixture, TRUE)),
      "missing absolute and explicit user paths are never replaced with examples")
if (!dir.exists(file.path(path_fixture, "data"))) dir.create(file.path(path_fixture, "data"))
writeLines("Original user file", file.path(path_fixture, "data", "survey.rds"))
check(identical(sae_resolve_input_path("data/survey.rds", path_fixture),
                normalizePath(file.path(path_fixture, "data", "survey.rds"), winslash = "/")),
      "existing user path takes precedence over migration")
writeLines("Ambiguous fixture", file.path(path_fixture, "Data", "simulated", "survey.rds"))
check(inherits(try(sae_resolve_input_path("survey.rds", path_fixture, TRUE), silent = TRUE), "try-error"),
      "ambiguous saved filenames require user selection")
# The fixture is owned by this test and must remain in R's temporary directory.
stopifnot(startsWith(normalizePath(path_fixture, winslash = "/"),
                    paste0(normalizePath(tempdir(), winslash = "/"), "/")))
unlink(path_fixture, recursive = TRUE)

# IGN boundary and attribution regression checks.
source("R/input_readers.R")
# Data/Spain/auxiliary.rds is deliberately excluded from the repository
# (GPL-2; see THIRD_PARTY_NOTICES.md), so these checks run only where the
# Spain example data is present - a full working copy or an extracted release
# archive - and are skipped in a fresh clone such as CI.
if (file.exists("Data/Spain/auxiliary.rds") &&
    file.exists("Data/Spain/shapefile.rds")) {
  ign_geometry <- sae_read_geometry_input("Data/Spain/shapefile.rds")
  ign_aux <- readRDS("Data/Spain/auxiliary.rds")
  ign_names <- unique(as.data.frame(ign_aux)[, c("prov", "provlab")])
  ign_names <- ign_names[order(ign_names$prov), ]
  check(identical(names(ign_geometry), c("prov", "provlab", "geometry")) &&
        identical(ign_geometry$prov, 1:52) &&
        identical(ign_geometry$provlab, as.character(ign_names$provlab)),
        "IGN boundaries preserve all 52 province IDs and existing names")
  if (requireNamespace("sf", quietly = TRUE)) {
    check(all(sf::st_is_valid(ign_geometry)) && !any(sf::st_is_empty(ign_geometry)) &&
          sf::st_crs(ign_geometry)$epsg == 4326L,
          "IGN boundary geometry is valid, nonempty and WGS84")
  } else {
    message("SKIP: IGN boundary geometry check (sf not installed).")
  }
  ign_credit <- "Obra derivada de CartoBase ANE 2006-2024 CC-BY 4.0 ign.es"
  check(identical(sae_map_caption(ign_geometry, "Estimates: authors"),
                  paste("Estimates: authors", ign_credit, sep = "\n")),
        "map caption preserves estimate credit and adds selected boundary attribution")
  ign_renamed <- dplyr::mutate(dplyr::rename(ign_geometry, domain = prov),
                               domain = as.character(domain))
  attr(ign_renamed, "boundary_attribution") <- sae_boundary_attribution(ign_geometry)
  ign_renamed <- sae_drop_domain_label_columns(ign_renamed)
  check(identical(sae_boundary_attribution(ign_renamed), ign_credit),
        "boundary attribution survives pipeline domain-label cleanup")
  uncredited_geometry <- ign_geometry
  attr(uncredited_geometry, "boundary_attribution") <- NULL
  check(is.null(sae_map_caption(uncredited_geometry)) &&
        identical(sae_map_caption(uncredited_geometry, "User data"), "User data"),
        "uncredited user geometry never receives IGN attribution")
  attr(uncredited_geometry, "boundary_attribution") <- "Different provider"
  check(identical(sae_map_caption(uncredited_geometry), "Different provider"),
        "map attribution follows the selected provider rather than the Spain example")
} else {
  message("SKIP: IGN boundary and attribution checks (Spain example data not present).")
}

check(!inherits(try(parse("Data/Spain/generate_spain_boundary.R", encoding = "UTF-8"),
                     silent = TRUE), "try-error"),
      "offline IGN boundary generator parses")


# Estimated-change figures use paired signed changes, not interval widths.
source("R/change_comparison.R")
change_fixture <- data.frame(domain=c("2","1","1","2"),
  method=c("FH","FH","MFH","MFH"), diff=c(-.02,.03,.01,-.01),
  lb=c(-.04,.01,-.01,-.03), ub=c(0,.05,.03,.01))
change_result <- sae_change_comparison(change_fixture, c(2012,2013))
check(identical(change_result$domain$domain, c("1","2")) &&
      isTRUE(all.equal(change_result$domain$UFH_change,c(3,-2))) &&
      isTRUE(all.equal(change_result$domain$MFH_change,c(1,-1))) &&
      isTRUE(all.equal(change_result$domain$MFH_minus_UFH_change,c(-2,1))),
      "change comparison joins by domain and converts signed changes to percentage points")
change_missing <- change_fixture
change_missing$diff[4] <- Inf
change_missing <- rbind(change_missing,data.frame(domain="3",method="FH",diff=.2,lb=.1,ub=.3))
change_filtered <- sae_change_comparison(change_missing,c(2012,2013))
check(nrow(change_filtered$domain)==1L && all(change_filtered$distribution$domains==1L) &&
      change_filtered$paired$excluded_nonfinite_pairs==1L && change_filtered$paired$UFH_only_domains==1L,
      "both change plots and summaries use the same finite pairs and disclose exclusions")
check(inherits(try(sae_change_comparison(rbind(change_fixture,change_fixture[1,]),c(2012,2013)),silent=TRUE),"try-error"),
      "duplicate domain-method observations cannot create Cartesian comparisons")
change_empty <- sae_change_comparison(change_fixture[change_fixture$method=="FH",],c(2012,2013))
check(nrow(change_empty$domain)==0L && is.null(sae_plot_change_paired(change_empty)) &&
      is.null(sae_plot_change_distribution(change_empty)),
      "UFH-only runs do not fabricate MFH changes or figures")
change_currency <- sae_change_comparison(change_fixture,c(2012,2013),"mean_welfare","EUR")
check(identical(change_currency$unit,"EUR") &&
      isTRUE(all.equal(change_currency$domain$UFH_change,c(.03,-.02))),
      "mean-welfare changes retain currency units without percentage scaling")
set.seed(19); change_seed <- .Random.seed
invisible(ggplot2::ggplot_build(sae_plot_change_distribution(change_result)))
check(identical(change_seed,.Random.seed),"descriptive plot jitter does not alter the analysis RNG state")
change_all_zero <- change_fixture;change_all_zero$diff <- 0
check(!inherits(try(ggplot2::ggplot_build(sae_plot_change_paired(sae_change_comparison(change_all_zero,c(2012,2013)))),silent=TRUE),"try-error"),
      "paired change axes remain valid when every change is zero")

# Word export remains independent of analysis and preserves all table cells.
source("R/report_export.R")
if (rmarkdown::pandoc_available() && requireNamespace("xml2", quietly=TRUE) && requireNamespace("zip", quietly=TRUE)) {
  word_fixture <- tempfile("sae-word-test-")
  dir.create(word_fixture)
  word_html <- file.path(word_fixture,"report with spaces.html")
  word_docx <- file.path(word_fixture,"report with spaces.docx")
  word_cells <- paste0("<td>value_",seq_len(10),"</td>",collapse="")
  writeLines(paste0('<html><head><title>Fixture</title></head><body><h1>Findings</h1>',
    '<p>AI-generated; not validated.</p><table><tr>',
    paste0('<th>field_',seq_len(10),'</th>',collapse=''),'</tr><tr>',word_cells,'</tr></table></body></html>'),word_html)
  word_before <- readBin(word_html,"raw",n=file.info(word_html)$size)
  sae_render_word_report(word_html,word_docx,root=getwd())
  word_parts <- zip::zip_list(word_docx)$filename
  zip::unzip(word_docx,files="word/document.xml",exdir=word_fixture)
  word_xml <- xml2::read_xml(file.path(word_fixture,"word/document.xml"))
  word_ns <- c(w="http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  word_text <- xml2::xml_text(xml2::xml_find_all(word_xml,"//w:t",word_ns))
  check(all(c("[Content_Types].xml","word/document.xml") %in% word_parts) &&
        all(paste0("value_",seq_len(10)) %in% word_text) &&
        length(xml2::xml_find_all(word_xml,"//w:tbl/w:tr",word_ns))==11L &&
        "AI-generated; not validated." %in% word_text,
        "Word export contains editable transposed tables, all cells, and AI status labels")
  word_width <- xml2::xml_find_first(word_xml,"//w:tbl/w:tblPr/w:tblW",word_ns)
  check(xml2::xml_attr(word_width,"w:w",word_ns)=="9360" &&
        xml2::xml_attr(word_width,"w:type",word_ns)=="dxa" &&
        !"Table of Contents" %in% word_text &&
        identical(word_before,readBin(word_html,"raw",n=file.info(word_html)$size)),
        "Word export sets fixed page-width tables and leaves the HTML byte-identical")
  check(inherits(try(sae_render_word_report(word_html,word_docx,root=word_fixture),silent=TRUE),"try-error") &&
        identical(word_before,readBin(word_html,"raw",n=file.info(word_html)$size)),
        "missing Word resources fail clearly without modifying the HTML")
  stopifnot(startsWith(normalizePath(word_fixture,winslash="/"),paste0(normalizePath(tempdir(),winslash="/"),"/")))
  unlink(word_fixture,recursive=TRUE)
} else message("SKIP: Word integration fixture needs Pandoc, xml2 and zip.")

if (length(failures) > 0L) {
  stop("Tests failed: ", paste(failures, collapse = "; "), call. = FALSE)
}
cat("All targeted tests passed.\n")
