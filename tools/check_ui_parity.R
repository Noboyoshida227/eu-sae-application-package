# Verification: render app.R's original UI and the wizard UI side by
# side (no pipeline packages required) and prove the two contain exactly
# the same set of element IDs -- i.e. the regrouping into six steps
# dropped nothing and duplicated nothing.
suppressMessages(library(shiny))
options(encoding = "UTF-8")
`%||%` <- function(a, b) if (is.null(a)) b else a
supported_languages <- function() c(English = "en", French = "fr")

# Run from the package root:  Rscript tools/check_ui_parity.R
root <- if (basename(getwd()) == "tools") ".." else "."
core <- file.path(root, "app.R")

# Pull only the UI helpers and the `ui` object out of app.R.
for (e in as.list(parse(core, keep.source = FALSE, encoding = "UTF-8"))) {
  if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") &&
      as.character(e[[2]])[1] %in% c("tip_label", "mapping_selectize", "ui")) {
    eval(e, envir = globalenv())
  }
}
ui_core <- ui

for (e in as.list(parse(file.path(root, "app_wizard.R"), keep.source = FALSE,
                        encoding = "UTF-8"))) {
  if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") &&
      as.character(e[[2]])[1] %in% c("WIZ_STEPS", "WIZ_IDS", "wiz_step_header",
                                     "wiz_nav", "wiz_panel", "wiz_head", "ui")) {
    eval(e, envir = globalenv())
  }
}
ui_wiz <- ui

element_ids <- function(u) {
  h <- as.character(htmltools::renderTags(u)$html)
  # (?<!-) avoids matching data-tabsetid="..." as an id
  m <- regmatches(h, gregexpr('(?<![-a-z])id="[^"]+"', h, perl = TRUE))[[1]]
  ids <- gsub('^id="|"$', "", m)
  # Shiny mints random tab-<n>-<k> ids per render; not comparable.
  ids[!grepl("^tab-[0-9]+-[0-9]+$", ids)]
}

core_ids <- element_ids(ui_core)
wiz_ids  <- element_ids(ui_wiz)

wiz_only  <- setdiff(unique(wiz_ids), unique(core_ids))
core_only <- setdiff(unique(core_ids), unique(wiz_ids))
dup_core  <- { t <- table(core_ids); names(t)[t > 1] }
dup_wiz   <- { t <- table(wiz_ids);  names(t)[t > 1] }

cat("\n================ UI PARITY CHECK ================\n")
cat(sprintf("app.R      : %3d elements with ids (%d unique)\n", length(core_ids), length(unique(core_ids))))
cat(sprintf("app_wizard : %3d elements with ids (%d unique)\n", length(wiz_ids), length(unique(wiz_ids))))
cat("\nDROPPED by the wizard (present in app.R, absent here):\n  ",
    if (length(core_only)) paste(core_only, collapse = ", ") else "none", "\n")
cat("\nADDED by the wizard (wizard chrome -- expected):\n  ",
    if (length(wiz_only)) paste(wiz_only, collapse = ", ") else "none", "\n")
cat("\nDuplicate ids in app.R     :", if (length(dup_core)) paste(dup_core, collapse = ", ") else "none", "\n")
cat("Duplicate ids in app_wizard:", if (length(dup_wiz)) paste(dup_wiz, collapse = ", ") else "none", "\n")
cat("=================================================\n")

stopifnot(length(core_only) == 0, length(dup_wiz) == 0)
cat("PASS: every app.R element survives the regrouping, exactly once.\n")
