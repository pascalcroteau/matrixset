

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
#' @name context
NULL




context_enclos <- function(fn, env)
{
  FN <- env[[fn]]
  if (is.null(FN)) stop(paste(encodeString(fn, quote = "`"), "can only be used within matrixset"),
                        call. = FALSE)
  FN
}


#' @rdname context
#' @export
current_row_info <- function()
{
  context_enclos("current_row_info()", emptyenv())
}


#' @rdname context
#' @export
current_column_info <- function()
{
  context_enclos("current_column_info()", emptyenv())
}


#' @rdname context
#' @export
current_n_row <- function()
{
  context_enclos("current_n_row()", emptyenv())
}


#' @rdname context
#' @export
current_n_column <- function()
{
  context_enclos("current_n_column()", emptyenv())
}


#' @rdname context
#' @export
current_row_name <- function()
{
  context_enclos("current_row_name()", emptyenv())
}


#' @rdname context
#' @export
row_pos <- function()
{
  context_enclos("row_pos()", emptyenv())
}


#' @rdname context
#' @export
row_rel_pos <- function()
{
  context_enclos("row_rel_pos()", emptyenv())
}


#' @rdname context
#' @export
current_column_name <- function()
{
  context_enclos("current_column_name()", emptyenv())
}


#' @rdname context
#' @export
column_pos <- function()
{
  context_enclos("column_pos()", emptyenv())
}


#' @rdname context
#' @export
column_rel_pos <- function()
{
  context_enclos("column_rel_pos()", emptyenv())
}
