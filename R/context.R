

#' Contexts dependent functions
#'
#' @description
#' These functions are designed to work inside certain `matrixset` functions, to
#' have access to current group/matrix/row/column. Because of that, they will
#' not work in a general context.
#'
#' The functions within which the context functions will work are [apply_matrix()],
#' [apply_row()] and [apply_column()] - as well as their *_dfl/*dfw variant.
#'
#' Note that "current" refers to the current matrix/group/row/column, as
#' applicable, and possibly combined.
#'
#' The context functions are:
#'
#' * `current_n_row()` and `current_n_column()`. They each give the number of rows
#'     and columns, respectively, of the current matrix.
#'
#'     They are the context equivalent of [nrow()] and [ncol()].
#'
#' * `current_row_name()` and `current_column_name()`. They provide the current
#'     row/column name. They are the context equivalent of [rownames()] and
#'     [colnames()].
#'
#' * `current_row_info()` and `current_column_info()`. They give access to the
#'     current row/column annotation data frame. The are the context equivalent
#'     of [row_info()] and [column_info()].
#'
#' * `row_pos()` and `column_pos()`. They give the current row/column indices.
#'     The indices are the the ones before matrix subsetting.
#'
#' * `row_rel_pos()` and `column_rel_pos()`. They give the row/column indices
#'     relative to the current matrix. They are equivalent to
#'     `seq_len(current_n_row())`/`seq_len(current_n_column())`.
#'
#' @returns
#' See each individual functions for returned value when used in proper context.
#' If used out of context, an error condition is issued.
#'
#' @examples
#' # this will fail (as it should), because it is used out of context
#' is(try(current_n_row(), silent = TRUE), "try-error")
#'
#' # this is one way to know the number of students per class in 'student_results'
#' student_results |>
#,     row_group_by(class) |>
#'     apply_matrix_dfl(n = ~ current_n_row(), .matrix = 1)
#'
#' @name context
NULL





context_message <- function(fn)
{

  stop(paste(encodeString(fn, quote = "`"), "can only be used within matrixset"),
       call. = FALSE)
}


#' @rdname context
#' @export
current_row_info <- function()
{
  context_message("current_row_info()")
}


#' @rdname context
#' @export
current_column_info <- function()
{
  context_message("current_column_info()")
}


#' @rdname context
#' @export
current_n_row <- function()
{
  context_message("current_n_row()")
}


#' @rdname context
#' @export
current_n_column <- function()
{
  context_message("current_n_column()")
}


#' @rdname context
#' @export
current_row_name <- function()
{
  context_message("current_row_name()")
}


#' @rdname context
#' @export
row_pos <- function()
{
  context_message("row_pos()")
}


#' @rdname context
#' @export
row_rel_pos <- function()
{
  context_message("row_rel_pos()")
}


#' @rdname context
#' @export
current_column_name <- function()
{
  context_message("current_column_name()")
}


#' @rdname context
#' @export
column_pos <- function()
{
  context_message("column_pos()")
}


#' @rdname context
#' @export
column_rel_pos <- function()
{
  context_message("column_rel_pos()")
}


