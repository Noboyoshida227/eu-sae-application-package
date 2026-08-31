# Compatibility helpers for msae under recent R versions.
#
# Some msae functions end with a convergence check of the form
#   kit >= MAXITER && diff >= PRECISION
# where `diff` is a vector.  Recent R versions reject a vector on the right
# side of `&&` when the left side is TRUE.  Patch only that expression, and
# preserve missing arguments in the original function calls.  Replacing a
# missing argument with an explicit NULL corrupts eblupMFH3 and produces
# `incorrect number of arguments to "="`.

sae_fix_msae_convergence_ast <- function(expr) {
  if (!is.call(expr)) return(expr)

  if (identical(expr[[1]], as.name("&&")) && length(expr) == 3L) {
    rhs <- expr[[3]]
    if (is.call(rhs) && identical(rhs[[1]], as.name(">=")) &&
        length(rhs) == 3L && identical(rhs[[2]], as.name("diff")) &&
        identical(rhs[[3]], as.name("PRECISION"))) {
      expr[[3]] <- call("any", rhs)
      return(expr)
    }
  }

  for (i in seq_along(expr)) {
    # Calls can contain truly missing arguments.  Recursing into one and
    # assigning the returned NULL changes the call's meaning.
    if (!is.null(expr[[i]])) {
      expr[[i]] <- sae_fix_msae_convergence_ast(expr[[i]])
    }
  }
  expr
}

sae_patch_msae_function <- function(name) {
  fn <- getFromNamespace(name, "msae")
  body(fn) <- sae_fix_msae_convergence_ast(body(fn))
  environment(fn) <- asNamespace("msae")
  fn
}

sae_install_msae_compat_functions <- function(
    names = c("eblupMFH1", "eblupMFH2", "eblupMFH3", "eblupUFH"),
    envir = .GlobalEnv) {
  for (name in names) {
    assign(name, sae_patch_msae_function(name), envir = envir)
  }
  invisible(names)
}
