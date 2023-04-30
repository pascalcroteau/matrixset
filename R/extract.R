
# []       2  T  T  T
# [, ]     3  T  T  T
# [1, ]    3  F  T  T
# [, 1]    3  T  F  T
# [, , ]   4  T  T  T
# [1, ,]   4  F  T  T
# [, 1,]   4  T  F  T
# [, ,1]   4  T  T  F
# [1,1,]   4  F  F  T
# [1,,1]   4  F  T  F
# [,1,1]   4  T  F  F
# [1,1,1]  4  F  F  F
# [, , , ] 5  T  T  T
# [1, , ,] 5  F  T  T
# [, 1, ,] 5  T  F  T
# [, , 1,] 5  T  T  F
# [1,1, ,] 5  F  F  T
# [1,,1 ,] 5  F  T  F
# [,1,1, ] 5  T  F  F
# [1,1,1,] 5  F  F  F


CashStatus <- R6::R6Class("CashStatus",

                           public = list(
                             set = function(cl) {
                               private$allowed <- c(rlang::as_string(cl[[1]]), private$allowed)
                             },

                             allow = function(cl) rlang::as_string(cl[[1]]) %in% private$allowed,

                             clear = function(cl) {
                               fn <- rlang::as_string(cl[[1]])
                               idx <- match(fn, private$allowed, 0)
                               if (idx > 0) {
                                 alw <- private$allowed
                                 alw <- alw[-idx]
                                 if (length(alw) == 0L) alw <- ""
                                 private$allowed <- alw
                               }
                             }
                           ),

                           private = list(
                             allowed = ""
                           ))

cash_status <- CashStatus$new()


# check if args are provided properly in terms of indexes i,j (that should be
# unnamed and thus positioned based)
assess_subset_args <- function(Nargs, matrix_pos, name_pos)
{
  extra <- c("drop", "keep_annotation", "warn_class_change")

  if ((Nargs == 3L && name_pos == 0) || (name_pos > 0 && name_pos <= 5)) {
    stop("incorrect number of dimensions", call. = FALSE)
  }

  if (matrix_pos > 0 && matrix_pos != 5)
    stop("incorrect position for 'matrix' argument")
}

# convert indexes that can be non-integers to integers
index_to_integer <- function(idx, n, names)
{

  if (is.null(idx)) {
    idx <- seq_len(n)
  } else {
    if (anyNA(idx))
      stop("NAs are not allowed for indexing matrixset objects")

    if (typeof(idx) == "logical") {
      len <- length(idx)
      if (len != n) {
        stop("logical subscript not of appropriate length")
      }

      idx <- which(idx)
      if (length(idx) == 0L)
        stop("logical subscript has no TRUEs")
    }
    else if (typeof(idx) == "character") {

      idx <- match(idx, names, 0)
      if (!any(idx > 0))
        stop("character subscript has no matches")
      idx <- idx[idx > 0]
    }
    else if (is.numeric(idx)) {
      if (typeof(idx) == "double") idx <- as.integer(idx)

      if (!any(idx != 0)) stop("wrong subscript specification: nothing to subset")
      idx <- idx[idx != 0]
      if (any(idx > n)) stop("subscript out of bounds")
    } else stop("subscript type not handled")
  }

  idx
}




matrix_to_indices <- function(mat)
{
  if (ncol(mat) != 2)
    stop("matrix for extraction must have 2 columns")

  list(i = as.vector(mat[, 1]), j = as.vector(mat[, 2]))
}


# workhorse doing the actual subsetting
#' @importFrom methods is
matset_subset <- function(x, i, j, matrix, drop, keep_annotation,
                          warn_class_change)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  nmat <- .nmatrix(x)
  if (!is.null(matrix))
    matrix <- index_to_integer(matrix, nmat, .matrixnames(x))

  if (is_matrixish(i) && !is.null(j))
    stop("index j must not be provided if i is a matrix")

  if (is_matrixish(i)) {
    ij <- matrix_to_indices(i)
    i <- ij[["i"]]
    j <- ij[["j"]]
  }

  if (!is.null(i)) i <- index_to_integer(i, nrow(x), rownames(x))
  if (!is.null(j)) j <- index_to_integer(j, ncol(x), colnames(x))

  xsub <- x$matrix_set
  if (!is.null(matrix)) xsub <- xsub[matrix]


  if (is.null(i)) {
    if (!is.null(j)) xsub <- lapply(xsub, function(X) X[, j, drop = drop])
  } else if (is.null(j)) {
    xsub <- lapply(xsub, function(X) X[i, , drop = drop])
  } else xsub <- lapply(xsub, function(X) X[i, j, drop = drop])

  all_matrices <- all(vapply(xsub,
                             function(u) is.null(u) || is.matrix(u) || is(u, "Matrix"),
                             FALSE))
  if (all_matrices)
  {
    if (keep_annotation) {
      xsub_rowinfo <- x$row_info
      xsub_colinfo <- x$column_info

      if (max(dim(xsub_rowinfo)) > 0 && !is.null(i))
        xsub_rowinfo <- xsub_rowinfo[i,, drop = FALSE]
      if (max(dim(xsub_colinfo)) > 0 && !is.null(j))
        xsub_colinfo <- xsub_colinfo[j,, drop = FALSE]


      row_tag <- .rowtag(x)
      col_tag <- .coltag(x)

      xsub <- matrixset(xsub, row_info = xsub_rowinfo, column_info = xsub_colinfo,
                        row_key = row_tag, row_tag = row_tag, column_key = col_tag,
                        column_tag = col_tag)
    } else if (warn_class_change)
      warning("Result object is no longer a matrixset")

  } else {
    if (warn_class_change)
      warning("Result object is no longer a matrixset")
  }

  xsub
}






#' Subsetting  matrixsets
#'
#' @description
#' Extract parts of a matrixset, where indexes refers to rows and columns.
#'
#' @details
#' Indexes `i` and `j` are given as for a regular [matrix()]
#' (note however that factors are currently not allowed for indexing).
#' Which matrices are extracted (all or a subset) is specified via argument
#' `"matrix"`.
#'
#' Missing values (`NA`) are not allowed for indexing, as it results in
#' unknown selection. Character indexes use exact matching, not partial.
#'
#' The default arguments for `"drop"` and `"keep_annotation"` are
#' chosen so that the object resulting from the extraction is still a
#' `matrixset`.
#'
#' Setting `"keep_annotation"` to `FALSE` automatically results in a class
#' change (a list of matrix) and a warning is issued (see argument
#' `warn_class_change`, however).
#'
#' Setting `drop` to `TRUE` may also result to a change of class,
#' depending on the provided indices (the same way matrix may result to a vector
#' when `drop` is `TRUE`).
#'
#' The subsetting operator `[[` is a convenient wrapper for `[(,,matrix)`.
#'
#' There is no `$` subsetting operator for the `matrixset` object.
#'
#' @param x                    `matrixset` object from which to extract
#'                             element(s)
#' @param i,j                  rows (`i`) and columns (`j`) to extract from
#'    matrices of `x`, as indices. These are either `numeric` or `character`
#'    values.
#'
#'    To extract every rows or columns, use `i = NULL` or `j = NULL`, which is
#'    the default for both. Note that treating `NULL` as empty differs from the
#'    usual extraction, where it is treated as `integer(0)`.
#'
#'    Numeric values are coerced to integer through [as.integer()], which means
#'    they are truncated towards zero.
#'
#'    Character vectors will be matched to the dimnames of the object.
#'
#'    Indices an also be logical vectors, stating for each element if it is
#'    extracted (`TRUE`) or rejected (`FALSE`). Logical vectors are *NOT*
#'    recycled, which is an important difference with usual matrix extraction.
#'    It means that the `logical` vector must match the object dimension in
#'    length.
#'
#'    Can also be negative integers, in which case they are indices of elements
#'    to leave out of the selection.
#'
#'    When indexing, a single argument `i` can be a matrix with two columns.
#'    This is treated as if the first column was the `i` index and the second
#'    column the `j` index.
#' @param matrix               index specifying matrix or matrices to extract.
#'                             Index is numeric or character vectors or empty
#'                             (\code{NULL}). Note that treating \code{NULL} as
#'                             empty differs from the usual extraction, where it
#'                             is treated as \code{integer(0)}. Here a
#'                             \code{NULL} (empty) results in selecting all
#'                             matrices.
#'
#'    See arguments `i,j`, as the same rules are followed.
#' @param drop                 If `TRUE`, the `drop` option of matrix extraction
#'                             is used. See [`[`()]. Note that the default for
#'                             `matrixset` objects is \code{FALSE}.
#' @param keep_annotation      `logical` specifying if the resulting object
#'                             should keep the annotations (meta info, or
#'                             traits, as per `matrixset` notation) as part
#'                             of the object. The default (`TRUE`), combined
#'                             with the default `drop = FALSE`, guarantees that
#'                             the resulting object is a `matrixset` object. If
#'                             `keep_annotation` is `FALSE`, the resulting
#'                             object will be a list of matrix, and a warning
#'                             will be issued, unless `warn_class_change` is
#'                             `FALSE`.
#' @param warn_class_change    `logical` that determines if a warning
#'                             should be issued when the extraction result is
#'                             not a `matrixset`. The default is to use
#'                             the global option `"matrixset.warn_class_change"`,
#'                             which is `FALSE` by default. If one wants
#'                             to silence permanently this warning, this is the
#'                             option to change.
#'
#'
#' @section Grouped matrixset:
#' When subsetting a grouped `matrixset` (by rows and/or columns), when the
#' resulting object is still a `matrixset`, the grouping structure will be
#' updated based on the resulting data.
#'
#' @returns
#' The resulting object type depends on the subsetting options. By default, a
#' `matrixset` object will be returned. This object will have the following
#' properties:
#' * Rows and/or columns are a subset of the input (based on what has been
#' subsetted), but appear in the same order.
#' * Annotations, or traits, are subsetted appropriately.
#' * The number of groups may be reduced.
#' * Currently, attributes are _not_ preserved.
#'
#' If `keep_annotation` is `FALSE`, the resulting object will be a list.
#' Typically, it will be a list of `matrix`, but if `drop` is `TRUE`, some
#' list elements could be vectors.
#'
#' @examples
#' lst <- list(a = matrix(1:6, 2, 3), b = matrix(101:106, 2, 3), c = NULL)
#' rownames(lst$a) <- rownames(lst$b) <- c("r1", "r2")
#' colnames(lst$a) <- colnames(lst$b) <- c("c1", "c2", "c3")
#' ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
#' ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
#' matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
#'
#' # this doesn't subset anything, just returns matset again
#' matset[]
#'
#' # this extracts the first row of every matrix. Note how each matrices is
#' # still a matrix, so you still end up with a matrixset object. Note also
#' # that you need placeholder for j and matrix index, even when not provided
#' matset[1, , ]
#'
#' # similar idea
#' matset[,2, ]
#' matset[1,2,]
#'
#' # it obviously works with vector indexes
#' matset[1:2, c(1,3),]
#'
#' # you can extract the matrices this - even without the 'annoying' warning
#' matset[, , , keep_annotation = FALSE]
#' matset[, , , keep_annotation = FALSE, warn_class_change = FALSE]
#'
#' # extracts subsetted matrices (no annotations)
#' matset[1, , , keep_annotation = FALSE, warn_class_change = FALSE]
#'
#' # a bit more in line with how R subsets matrices
#' matset[1, , , drop = TRUE, warn_class_change = FALSE]
#'
#' # you can obviously get some of the matrices only
#' matset[,,1]
#' matset[c(1,2),,1:2]
#'
#' # to showcase other kind of indexes. These are all equivalents
#' matset[1,,]
#' matset["r1", ,]
#' matset[c(TRUE, FALSE), ,]
#' matset[-2, ,] # equivalent because there are only 2 rows
#'
#' # this is also equivalent
#' matset[,,1]
#' matset[[1]]
#'
#' @name subsetting
NULL



#' @rdname subsetting
#' @export
`[.matrixset` <- function(x, i=NULL, j=NULL, matrix = NULL,
                          drop = FALSE, keep_annotation = TRUE,
                          warn_class_change = getOption("matrixset.warn_class_change"))
{
  extra <- c("drop", "keep_annotation", "warn_class_change")
  mscall <- sys.call()
  arg_names <- names(mscall)
  if (!all(arg_names %in% c("", "matrix", extra)))
    stop("named arguments are forbidden", call. = FALSE)
  named_extra <- any(arg_names %in% extra)
  pos_matrix <- match("matrix", arg_names, 0)
  pos_extra <- if (named_extra) min(which(arg_names %in% extra)) else 0

  nA <- nargs()
  assess_subset_args(nA, pos_matrix, pos_extra)

  # matset_subset(x, i, j, matrix, drop, nA, keep_annotation, warn_class_change)
  matset_subset(x, i, j, matrix, drop, keep_annotation, warn_class_change)


}



#' @rdname subsetting
#' @export
`[.row_grouped_ms` <- function(x, i=NULL, j=NULL, matrix = NULL,
                               drop = FALSE, keep_annotation = TRUE,
                               warn_class_change = getOption("matrixset.warn_class_change"))
{
  xsub <- NextMethod()
  if (is_matrixset(xsub)) {
    grs <- row_groups(x)
    xsub <- row_group_by(xsub, !!!grs)
    class(xsub) <- class(x)
  }

  xsub
}



#' @rdname subsetting
#' @export
`[.col_grouped_ms` <- function(x, i=NULL, j=NULL, matrix = NULL,
                               drop = FALSE, keep_annotation = TRUE,
                               warn_class_change = getOption("matrixset.warn_class_change"))
{
  xsub <- NextMethod()
  if (is_matrixset(xsub)) {
    grs <- column_groups(x)
    xsub <- column_group_by(xsub, !!!grs)
    class(xsub) <- class(x)
  }

  xsub
}



#' @rdname subsetting
#' @export
`[.dual_grouped_ms` <- function(x, i=NULL, j=NULL, matrix = NULL,
                                drop = FALSE, keep_annotation = TRUE,
                                warn_class_change = getOption("matrixset.warn_class_change"))
{
  xsub <- NextMethod()
  if (is_matrixset(xsub)) {
    grs <- column_groups(x)
    xsub <- column_group_by(xsub, !!!grs)

    grs <- row_groups(x)
    xsub <- row_group_by(xsub, !!!grs)

    class(xsub) <- class(x)
  }

  xsub
}


#' @rdname subsetting
#' @export
`$.matrixset` <- function(x, matrix)
{
  if (!cash_status$allow(sys.call(sys.parent())))
    stop("$ operator is invalid for matrixsets")

  .subset2(x, matrix)
}


#' @rdname subsetting
#' @export
`[[.matrixset` <- function(x, matrix)
{
  x[,, matrix = matrix]
}





