#' @export
remove_row_annotation <- function(.ms, ...) UseMethod("remove_row_annotation")

#' @export
remove_row_annotation.matrixset <- function(.ms, ...)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  row_info <- .ms$row_info

  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = row_info, allow_rename = FALSE)
  pos <- unname(pos)

  traits <- colnames(row_info)

  tag_pos <- which(traits == .rowtag(.ms))
  if (any(pos == tag_pos))
    stop("row tag (i.e., row info containing row names) can't be removed")

  gr_vars <- attr(.ms, "row_group_vars")
  if (!is.null(gr_vars)) {
    gr_pos <- match(gr_vars, traits)
    if (any(pos == gr_pos))
      stop("annotation used for grouping can't be removed")
  }

  row_info <- row_info[, -pos]
  rwtr <- colnames(row_info)

  .ms$row_info <- row_info
  attr(.ms, "row_traits") <- rwtr

  .ms
}




#' @export
remove_column_annotation <- function(.ms, ...) UseMethod("remove_column_annotation")

#' @export
remove_column_annotation.matrixset <- function(.ms, ...)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  col_info <- .ms$column_info

  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = col_info, allow_rename = FALSE)
  pos <- unname(pos)

  traits <- colnames(col_info)

  tag_pos <- which(traits == .coltag(.ms))
  if (any(pos == tag_pos))
    stop("column tag (i.e., column info containing column names) can't be removed")

  gr_vars <- attr(.ms, "col_group_vars")
  if (!is.null(gr_vars)) {
    gr_pos <- match(gr_vars, traits)
    if (any(pos == gr_pos))
      stop("annotation used for grouping can't be removed")
  }

  col_info <- col_info[, -pos]
  cwtr <- colnames(col_info)

  .ms$column_info <- col_info
  attr(.ms, "col_traits") <- cwtr

  .ms
}
