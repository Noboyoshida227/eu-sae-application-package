lock_path <- "dependencies.lock.csv"
if (!file.exists(lock_path)) stop("Missing ", lock_path, call. = FALSE)
lock <- utils::read.csv(lock_path, stringsAsFactors = FALSE)
actual <- vapply(lock$package, function(pkg) {
  if (identical(pkg, "R")) return(as.character(getRversion()))
  if (requireNamespace(pkg, quietly = TRUE)) as.character(utils::packageVersion(pkg)) else NA_character_
}, character(1))
result <- data.frame(
  package = lock$package,
  expected = lock$version,
  actual = actual,
  status = ifelse(is.na(actual), "MISSING",
                  ifelse(actual == lock$version, "MATCH", "DIFFERENT")),
  stringsAsFactors = FALSE
)
utils::write.csv(result, "dependency_lock_check.csv", row.names = FALSE)
print(result, row.names = FALSE)
if (any(result$status != "MATCH")) {
  stop("Dependency lock check failed. See dependency_lock_check.csv.", call. = FALSE)
}
cat("Dependency lock check passed.\n")
