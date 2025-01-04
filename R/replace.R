

#           missing (i, j, k)
# []         3  T  T  T
# [1]        3  F  T  T
# [, ]       4  T  T  T
# [1, ]      4  F  T  T
# [, 1]      4  T  F  T
# [1, 1]     4  F  F  T
# [, , ]     5  T  T  T
# [1, , ]    5  F  T  T
# [, 1, ]    5  T  F  T
# [, , 1]    5  T  T  F
# [1, 1, ]   5  F  F  T
# [1, , 1]   5  F  T  F
# [, 1, 1]   5  T  F  F
# [1, 1, 1]  5  F  F  F


# this in-house data frame allows controlling col compatibility
#' @export
`[<-.ms_data_frame` <- function(x, i, j, value)
{
  same <- assess_cols(x, value, j)
  if (!all(same))
    stop("", call. = FALSE)

  NextMethod()
}



neg_index_to_pos <- function(idx, n)
{
  if (all(idx > 0)) return(idx)

  if (!all(idx < 0)) stop("can't mix negative and positive subscripts")

  all_idx <- 1:n
  all_idx[idx]
}



assess_cols <- function(x, value, j)
{
  xcls <- lapply(x, typeof)
  vcls <- lapply(value, typeof)

  xnms <- names(x)
  vnms <- names(value)

  if (!missing(j)) {
    xcls <- xcls[j]
    vcls <- vcls[j]
    xnms <- xnms[j]
    vnms <- vnms[j]
  }

  if (!all(xnms == vnms))
    warning("Not all replacement traits share the same name")

  unname(
    mapply(function(x, y) {
      isTRUE(all.equal(x,y, check.attributes = FALSE))
    },
    xcls, vcls)
  )
}


assess_vec_compat <- function(value, nr, nc, i, j)
{
  len_i <- if (is.null(i)) nr else length(i)
  len_j <- if (is.null(j)) nc else length(j)
  vectorish <- len_i == 1L || len_j == 1L

  if (vectorish) {
    if (length(value) != max(len_i, len_j))
      stop("number of items to replace is not a multiple of replacement length")
  } else stop("dimension of items to replace is not vector compatible")
}



assess_mat_compat <- function(value, nr, nc, i, j)
{
  len_i <- if (is.null(i)) nr else length(i)
  len_j <- if (is.null(j)) nc else length(j)

  if (!isTRUE(identical(c(len_i, len_j), dim(value))))
    stop("value dimension is not the same as matrixset target")
}



# i and/or j NULL
replace_matrix_set <- function(old, new, nr, nc, i, j)
{
  nmats <- length(old)
  mats <- lapply(1:nmats,
                 function(idx) {
                   OLD <- old[[idx]]
                   NEW <- new[[idx]]

                   all_i <- is.null(i) || length(i) == nr
                   all_j <- is.null(j) || length(j) == nc

                   if (is.null(OLD)) {
                     if (all_i && all_j) OLD <- NEW
                     else if (!is.null(NEW)) stop("Can only replace NULL by another NULL, or a matrix with full rows and columns (i.e. matches matrixset dimensions)")
                   } else if (is.null(NEW)) {
                     if (all_i && all_j)
                       OLD <- NULL
                     else stop("can only replace the whole matrix by NULL, not a subset")
                   } else {
                     if (all_i) {
                       if (all_j) OLD[] <- NEW
                       else OLD[, j] <- NEW
                     } else {
                       if (all_j) OLD[i, ] <- NEW
                       else OLD[i, j] <- NEW
                     }

                   }

                   OLD
                 })
  names(mats) <- names(old)
  mats
}

#' Replace Parts of a matrixset
#'
#' @description
#' Replace whole or parts of some - or all - matrices of a `matrixset`.
#'
#' @details
#' If `matrix` is left unspecified (or given as `NULL`), all matrices will be
#' replaced by `value`. How replacement exactly occurs depends on `value` itself.
#'
#' If `value` is a single atomic `vector` (this excludes lists) or `matrix`,
#' relevant subscripts of all requested matrices will be replaced by the same
#' `value`. This is conditional to the dimensions being compatible.
#'
#' Alternatively, `value` can be a list of atomic vectors/matrices. If `value`
#' has a single element, the same rules as above apply. Otherwise, the length
#' of `value` must match the number of matrices for which subscripts have to be
#' replaced.
#'
#' If the list elements are named, the names are matched to the names of the
#' matrices that need replacement - in which case `value` needs not to be the
#' same length.
#'
#' A final possibility for `value` is for it to be `NULL`. In this case, target
#' matrices are turned to `NULL`.
#'
#' @section vector `value`:
#' Contrarily to `matrix` replacement, when submitting an atomic `vector`
#' `value`, dimensions must match exactly.
#'
#' @section Replacing `NULL` matrices:
#' Replacing subscripts of `NULL` matrices is not possible, unless `value` is
#' itself `NULL`, or a matrix the same dimensions (number of rows and columns)
#' as `x`. If `x` has dimnames, `value` must have the same dimnames.
#'
#' @param x    `matrixset` object from which to replace element(s)
#' @param i,j    Indices specifying elements to replace. Indices are numeric or
#'     character vectors or empty (`NULL`). Note that treating `NULL` as empty
#'     differs from the usual replacement, where it is treated as `integer(0)`.
#'     Here a `NULL` (empty) results in selecting all rows or columns.
#'
#'     Numeric values are coerced to integer as by [as.integer()] (and hence
#'     truncated towards zero).
#'
#'     Character vectors will be matched to the dimnames of the object.
#'
#'     Can also be logical vectors, indicating elements/slices to replace Such
#'     vectors are **NOT** recycled, which is an important difference with usual
#'     matrix replacement. It means that the logical vector must match the
#'     object dimension in length.
#'
#'     Can also be negative integers, indicating elements/slices to leave out of
#'     the replacement.
#'
#'     When indexing, a single argument `i` can be a matrix with two columns.
#'     This is treated as if the first column was the `i` index and the second
#'     column the `j` index.
#'
#' @param matrix    index specifying matrix or matrices to replace. Index is
#'                  numeric or character vectors or empty (\code{NULL}). Note
#'                  that treating \code{NULL} as empty differs from the usual
#'                  replacement, where it is treated as \code{integer(0)}. Here
#'                  a \code{NULL} (empty) results in replacing all matrices.
#'
#'    Numeric values are coerced to integer as by [as.integer()] (and hence
#'    truncated towards zero).
#'
#'    Character vectors will be matched to the matrix names of the object.
#'
#'    Can also be logical vectors, indicating elements/slices to replace. Such
#'    vectors are *NOT* recycled, which is an important difference with usual
#'    matrix replacement. It means that the `logical` vector must match the
#'    number of matrices in length.
#'
#'    Can also be negative integers, indicating elements/slices to leave out of
#'    the replacement.
#'
#' @param value    object to use as replacement value
#'
#' @returns
#' A `matrixset`, with proper elements replaced.
#'
#' @examples
#' # an hypothetical example of students that failed 3 courses and their results
#' # after remedial class
#'
#' # you can replace a line for all matrices at once. In the example, the "wrong"
#' # tag refers to the fact that the 'failure' results do not make sense after
#' # replacement
#' student_results_wrong <- student_results
#' student_results_wrong["student 2",,] <- c(0.81, 0.88, 0.71) # obviously, integer index works too
#' # note how all matrices had the same replacement
#' student_results_wrong
#'
#' # this already makes more sense in the context of the example
#' student_results[2,,] <- list(c(0,0.45,0.1), c(0.81, 0.88, 0.71))
#' student_results
#'
#' # or even these two equivalent commands
#' student_results["student 2",,"remedial"] <- c(0.77, 0.83, 0.75)
#' student_results[2,,2] <- matrix(c(0.77, 0.83, 0.75), 1, 3)
#'
#'
#' @export
`[<-.matrixset` <- function(x, i=NULL, j=NULL, matrix = NULL, value = NULL)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  if (!all(names(sys.call()) %in% c("", "matrix", "value")))
    stop("named arguments are forbidden", call. = FALSE)

  has_i <- !missing(i)
  has_j <- !missing(j)
  has_matrix <- !missing(matrix)
  nA <- nargs()

  idx_err <- "incorrect number of dimensions"

  if (nA == 3L) {
    if (has_i || has_j || has_matrix)
      stop(idx_err, call. = FALSE)
  } else if (nA == 4L) stop(idx_err, call. = FALSE)

  nmat <- .nmatrix(x)
  match_matrix_names <- if (is.character(matrix)) TRUE else FALSE
  if (!is.null(matrix)) {
    matrix <- index_to_integer(matrix, nmat, .matrixnames(x))
    matrix <- neg_index_to_pos(matrix, nmat)
  }
  mat_nms <- .matrixnames(x)
  if (!is.null(matrix)) mat_nms <- mat_nms[matrix]

  if (is_matrixish(i) && !is.null(j))
    stop("index j must not be provided if i is a matrix")

  if (is_matrixish(i)) {
    ij <- matrix_to_indices(i)
    i <- ij[["i"]]
    j <- ij[["j"]]
  }

  if (!is.null(i)) i <- index_to_integer(i, nrow(x), rownames(x))
  if (!is.null(j)) j <- index_to_integer(j, ncol(x), colnames(x))

  value_rowinfo <- NULL
  value_colinfo <- NULL

  if (is_matrixset(value)) {
    if (!is.null(i) && length(i) != nrow(value))
      stop("incompatible number of rows for replacement")

    if (!is.null(j) && length(j) != ncol(value))
      stop("incompatible number of columns for replacement")

    if (length(.rowtraits(x)) != length(.rowtraits(value)))
      stop("incompatible number of row traits for replacement")

    if (length(.coltraits(x)) != length(.coltraits(value)))
      stop("incompatible number of column traits for replacement")

    RN <- rownames(x)
    if (!is.null(i)) RN <- RN[i]
    if (!all(RN == rownames(value)))
      warning("Not all rownames match with replacement value")

    CN <- colnames(x)
    if (!is.null(j)) CN <- CN[i]
    if (!all(CN == colnames(value)))
      warning("Not all colnames match with replacement value")

    val_mats <- match(mat_nms, .matrixnames(value), 0)
    if (!all(val_mats > 0))
      stop("Can't replace with value because matrices do not match")

    if (!is.null(matrix)) value <- value[,,matrix=mat_nms]

    value_rowinfo <- value$row_info
    value_colinfo <- value$column_info

    value <- value$matrix_set
  } else if (is_matrixish(value) || is.null(value) || (is.vector(value) && !is.list(value))) {
    if (is.vector(value)) assess_vec_compat(value, nrow(x), ncol(x), i, j)
    if (is_matrixish(value)) assess_mat_compat(value, nrow(x), ncol(x), i, j)
    NM <- if (is.null(matrix)) nmat else length(matrix)
    value <- rep(list(value), NM)
    names(value) <- mat_nms
  } else if (is.list(value)) {

    value_nms <- names(value)

    if (match_matrix_names) {

      if (is.null(value_nms) || any(value_nms == ""))
        stop("The elements of 'value' must be named")

      is_null <- vapply(value, is.null, FALSE)
      is_matrix <- vapply(value, is_matrixish, FALSE)
      is_valid <- is_null | is_matrix
      if (!all(is_valid))
        stop("Elements of value must be NULL or a matrix")

      val_mats <- match(mat_nms, value_nms, 0)
      if (!all(val_mats > 0))
        stop("Can't replace with value because list names do not match")
      value <- value[mat_nms]

    } else {
      if ((!is.null(matrix) && length(value) != length(matrix)) || is.null(matrix) && length(value) != nmat)
        stop("list is the not same length as the number of matrix elements to replace")

      if (!is.null(value_nms)) {
        if (!all(mat_nms == value_nms))
          warning("not all list names match the matrix names.")
      }
    }

  } else stop("wrong value type")


  x_matrix_set <- x$matrix_set

  if (is.null(matrix))
    x_matrix_set <- replace_matrix_set(x_matrix_set, value,
                                       nrow(x), ncol(x), i, j)
  else x_matrix_set[matrix] <- replace_matrix_set(x_matrix_set[matrix], value,
                                                  nrow(x), ncol(x), i, j)

  all_null <- all(vapply(x_matrix_set, is.null, FALSE))

  if (all_null) {
    nm <- logical(0)
    meta <- tibble::tibble(tmp_name = nm,)

    names(meta) <- .rowtag(x)
    x$row_info <- meta

    names(meta) <- .coltag(x)
    x$column_info <- meta
  } else {

    if (!is.null(value_rowinfo)) {
      old_class <- class(x$row_info)
      class(x$row_info) <- c("ms_data_frame", old_class)
      if (is.null(i)) x$row_info[] <- value_rowinfo else x$row_info[i, ] <- value_rowinfo
      class(x$row_info) <- old_class
    }

    if (!is.null(value_colinfo)) {
      old_class <- class(x$column_info)
      class(x$column_info) <- c("ms_data_frame", old_class)
      if (is.null(j)) x$column_info[] <- value_colinfo else x$column_info[j, ] <- value_colinfo
      class(x$column_info) <- old_class
    }

  }

  x_matrix_info <- info_matrices(x_matrix_set)
  x$matrix_set <- x_matrix_set

  row_names <-  x_matrix_info[["row_names"]]
  if (is.null(row_names)) row_names <- character(0)
  # if (is.null(row_names)) row_names <- make_names(seq_len(n_row), "")
  col_names <-  x_matrix_info[["col_names"]]
  if (is.null(col_names)) col_names <- character(0)
  # if (is.null(row_names)) col_names <- make_names(seq_len(n_col), "")

  attr(x, "n_row") <- x_matrix_info[["n_row"]]
  attr(x, "n_col") <- x_matrix_info[["n_col"]]
  attr(x, "row_names") <- row_names
  attr(x, "col_names") <- col_names
  attr(x, "row_traits") <- colnames(x$row_info)
  attr(x, "col_traits") <- colnames(x$column_info)

  x
}

