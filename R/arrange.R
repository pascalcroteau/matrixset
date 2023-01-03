#' Re-order rows or columns of a `matrixset`
#'
#' @description
#' Orders the rows ([arrange_row()]) or columns ([arrange_column()]) by
#' annotation values.
#'
#' The mechanic is based on sorting the annotation data frame via `dplyr`'s
#' [dplyr::arrange()].
#'
#' This means, for instance, that grouping is ignored by default. You must
#' either specify the grouping annotation in the sorting annotation, or use
#' `.by_group = TRUE`.
#'
#' The handling of locales and handling of missing values is also governed by
#' dplyr's `arrange()`.
#'
#' @param .ms          A `matrixset` object
#' @param ...          Name of traits to base sorting upon. Tidy selection is
#'                     supported. Use [dplyr::desc()] to sort an annotation in
#'                     descending order.
#' @param .by_group    `logical`. Defaults to `FALSE` and even if `TRUE`, has no
#'                     impact on ungrouped margin. Otherwise, grouping
#'                     annotation is used first for sorting.
#'
#' @returns
#' A `matrixset` with re-ordered rows or columns, including updated row or
#' column meta info.
#'
#' @examples
#' ms1 <- remove_row_annotation(student_results, class, teacher)
#'
#' # this would not work
#' # remove_row_annotation(row_group_by(student_results, class), class)
#' @name arrange



#' @rdname arrange
#' @export
arrange_row <- function(.ms, ..., .by_group = FALSE) UseMethod("arrange_row")

#' @export
arrange_row.matrixset <- function(.ms, ..., .by_group = FALSE)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  row_info <- .ms$row_info
  row_info <- dplyr::arrange(row_info, ..., .by_group = .by_group)

  new_order <- row_info[[.rowtag(.ms)]]

  matrix_set <- .ms$matrix_set
  .ms$matrix_set <- lapply(matrix_set, function(m) m[new_order, , drop = FALSE])

  .ms$row_info <- row_info
  attr(.ms, "row_names") <- new_order

  meta <- get_group_info(row_info, class(.ms), "row")
  attrs <- set_group_attrs(attributes(.ms), meta$attrs, "row")
  attributes(.ms) <- attrs
  class(.ms) <- meta$class
  if (is.null(meta$attrs$group_vars)) class(.ms) <- "matrixset"

  .ms
}





#' @rdname arrange
#' @export
arrange_column <- function(.ms, ..., .by_group = FALSE) UseMethod("arrange_column")

#' @export
arrange_column.matrixset <- function(.ms, ..., .by_group = FALSE)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  col_info <- .ms$column_info
  col_info <- dplyr::arrange(col_info, ..., .by_group = .by_group)

  new_order <- col_info[[.coltag(.ms)]]

  matrix_set <- .ms$matrix_set
  .ms$matrix_set <- lapply(matrix_set, function(m) m[, new_order, drop = FALSE])

  .ms$column_info <- col_info
  attr(.ms, "col_names") <- new_order

  meta <- get_group_info(col_info, class(.ms), "col")
  attrs <- set_group_attrs(attributes(.ms), meta$attrs, "col")
  attributes(.ms) <- attrs
  class(.ms) <- meta$class
  if (is.null(meta$attrs$group_vars)) class(.ms) <- "matrixset"

  .ms
}

