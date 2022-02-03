
context_frame <- new.env(parent = emptyenv())
context_add <- function(env) context_frame[["mask"]] <- env
context_del <- function() rm(list="mask", pos = context_frame)
context_env <- function(fun) {
  e <- context_frame[["mask"]]
  if (is.null(e)) stop(paste(encodeString(fun, quote = "`"), "can only be used within"))
  e
}



#' Contexts dependent functions
#'
#' @description
#' These functions are designed to work inside certain `matrixset` functions, to
#' have access to current group/matrix/row/column. Because of that, they will
#' not work in a general context.
#'
#' The functions within which the context functions will work are [apply_mat()],
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
#' * `current_row_info()` and `current_column_info()`. They give access to the
#'     current row/column annotation data frame. The are the context equivlent
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


#' @rdname context
#' @export
current_row_info <- function()
{
  context_env("current_row_info()")$enclos$current_row_info()
}


#' @rdname context
#' @export
current_column_info <- function()
{
  context_env("current_column_info()")$enclos$current_column_info()
}


#' @rdname context
#' @export
current_n_row <- function()
{
  context_env("current_n_row()")$enclos$current_n_row()
}


#' @rdname context
#' @export
current_n_column <- function()
{
  context_env("current_n_column()")$enclos$current_n_col()
}


#' @rdname context
#' @export
row_pos <- function()
{
  context_env("row_pos()")$enclos$row_pos()
}


#' @rdname context
#' @export
row_rel_pos <- function()
{
  context_env("row_rel_pos()")$enclos$row_rel_pos()
}


#' @rdname context
#' @export
column_pos <- function()
{
  context_env("column_pos()")$enclos$col_pos()
}


#' @rdname context
#' @export
column_rel_pos <- function()
{
  context_env("column_rel_pos()")$enclos$col_rel_pos()
}

