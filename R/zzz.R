.onLoad <- function(libname, pkgname) {
  op <- options()
  op_matrixset <- list(
    matrixset.warn_class_change = TRUE
  )
  toset <- !(names(op_matrixset) %in% names(op))
  if (any(toset)) options(op_matrixset[toset])

  # cash_status <<- CashStatus$new()

  invisible()
}
