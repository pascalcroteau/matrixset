#' Add matrices to the `matrixset` object
#'
#' @description
#' Matrices to add must be of the same dimension and dimnames as `.ms`.
#'
#' Either a named list of matrices can be supplied, or matrices can be specified
#' separaely.
#'
#' @param .ms    A `matrixset` object.
#' @param ...     A single list of matrices (must be a named list), or
#'                individual matrices, e.g. `mat1 = m1`, `mat2 = m2`, etc.
#'                `NULL` elements are accepted. This allows to create a
#'                placeholder that can be filled later on.
#'
#' @returns
#' A `matrixset` with updated matrices.
#'
#' @examples
#' m1 <- matrix(1:60, 20, 3)
#' dimnames(m1) <- dimnames(student_results)
#' m2 <- matrix(101:160, 20, 3)
#' dimnames(m2) <- dimnames(student_results)
#'
#' ms <- add_matrix(student_results, m1=m1, m2=m2)
#' ms2 <- add_matrix(student_results, list(m1=m1, m2=m2))
#'
#' @export
add_matrix <- function(.ms, ...) UseMethod("add_matrix")
#' @export
add_matrix.matrixset <- function(.ms, ...)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  matrix_set <- matrices_from_dots(...)
  names_matrix <- list_names(matrix_set)

  if (any(idx <- duplicated(names_matrix, fromLast = TRUE))) {
    matrix_set <- matrix_set[!idx]
    names_matrix <- names_matrix[!idx]
    warning("Matrices share the same name. Only the last provided will be kept")
  }

  old_set <- .ms$matrix_set
  old_names <- names(old_set)

  if (any(names_matrix %in% old_names))
    stop("some new matrices have the same name as other original matrices. Use 'mutate_matrix()' to update original matrices")

  matrix_set <- c(old_set, matrix_set)

  matrix_info <- info_matrices(matrix_set, NULL)

  n_matrix <- length(matrix_set)

  .ms$matrix_set <- matrix_set
  attr(.ms, "matrix_names") <- names(matrix_set)
  attr(.ms, "n_matrix") <- n_matrix
  .ms
}





#' Remove one or more matrices of the `matrixset` object
#'
#' @description
#' This is a special case of the `[` method, with the benefit of being explicit
#' about what action is taken.
#'
#' @section Usage inside [mutate_matrix()]:
#' In most cases, both arguments of the function are mandatory. However, if you
#' want to declare that a matrix should be removed via the [mutate_matrix()]
#' function, the `remove_matrix()` must be called without arguments. There is
#' an example that illustrates that.
#'
#' @param .ms           A `matrixset` object. Leave empty only if `remove_matrix()`
#'                      is used inside `mutate_matrix()`.
#' @param matrix        index specifying matrix or matrices to remove. Index is
#'                      *posivie* numeric or character vectors. Tidy select is
#'                      also supported .Leave empty only if `remove_matrix()`
#'                      is used inside `mutate_matrix()`.
#'
#' @returns
#' A `matrixset` with updated matrices.
#'
#' @examples
#' ms1 <- remove_matrix(student_results, "remedial")
#' ms2 <- remove_matrix(student_results, 2)
#' ms3 <- mutate_matrix(student_results, remedial = remove_matrix())
#'
#' @export
remove_matrix <- function(.ms, matrix) UseMethod("remove_matrix")
#' @export
remove_matrix.matrixset <- function(.ms, matrix)
{
  has_ms <- !missing(.ms)
  has_matrix <- !missing(matrix)
  nA <- nargs()

  if (nA == 0L)
    stop("This special case of 'remove_matrix' can only be called inside 'mutate_mat()'")

  if (nA < 2) {
    if (!has_ms) stop("argument \".ms\" is missing")
    if (!has_matrix) stop("argument \"matrix\" is missing")
  }

  matrix <- rlang::enquo(matrix)

  nms <- .matrixnames(.ms)
  names(nms) <- nms

  matrix <- tidyselect::eval_select(matrix, nms)
  matrix <- unname(matrix)


  # nmat <- .nmatrix(.ms)
  # matrix <- index_to_integer(matrix, nmat, .matrixnames(.ms))
  # if (any(neg <- matrix < 0)) stop("negative indices are not allowed to remove matrices")
  matrix <- -matrix

  .ms[,,matrix]
}


.mutate_mat <- function(quos, .ms, .env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  .mats <- .ms$matrix_set
  .rowinf <- .ms$row_info
  .colinf <- .ms$column_info

  if (length(same <- intersect(names(.rowinf), names(.colinf))) > 0) {
    warning(paste0("The following traits are found in both rows and columns:\n  ",
                   paste(encodeString(same, quote = "\""), collapse = ", "),
                   ".\n  If any of these are needed, you should use context functions to make sure to use the correct ones."))
  }

  top <- new.env()
  flush_matrix <- function() ._NULL_
  assign("remove_matrix", flush_matrix, top)

  middle <- new.env(parent = top)
  middle <- list2env(.rowinf, middle)
  middle <- list2env(.colinf, middle)

  bottom <- new.env(parent = middle)
  bottom <- list2env(.mats, bottom)

  mask <- rlang::new_data_mask(bottom, top = top)
  mask$.data <- rlang::as_data_pronoun(mask)

  for (i in seq_along(quos)) {
    assign(names(quos)[i], rlang::eval_tidy(quos[[i]], mask, .env), bottom)
  }

  as.list(bottom)

}


#' Create/modify/delete matrices from a `matrixset` object
#'
#' @description
#' Applies functions that takes matrices as input and return similar matrices.
#' The difinition of similar is that the new matrix has the same dimension and
#' dimnames as `.ms`.
#'
#' If the returned matrix is assigned to a new matrix, this matrix is added to the
#' `matrixset` object. If it is assigned to an already existing matrix, it
#' overwrites the matrix of the same name.
#'
#' Setting a matrix value to `NULL` will ***not*** delete the matrix, but will
#' create an empty slot (`NULL`) for the matrix.
#'
#' To delete a matrix, use the function [remove_matrix()]. See examples below.
#'
#' Note that matrices are created sequentially and can be used by other
#' name-value pairs. There is an example that showcases this.
#'
#' @param .ms    A `matrixset` object.
#' @param ...    Name-value pairs, ala `dplyr`'s [dplyr::mutate()]. The value
#'               can be one of:
#'
#'  * a `matrix`, with same dimension and dimnames as `.ms`.
#'  * `NULL`, which will turn the matrix as an empty placeholder.
#'  * [remove_matrix()], to remove the matrix
#'
#' @returns
#' A `matrixset` with updated matrices.
#'
#' @examples
#' # Notice how FC can be used as soon as created
#' ms <- mutate_matrix(student_results,
#'                     FC = remedial/failure,
#'                     foo = NULL,
#'                     logFC = log2(FC),
#'                     FC = remove_matrix())
#' # this is NULL
#' matrix_elm(ms, "foo")
#'
#' # running this would return an error, since FC was deleted
#' # matrix_elm(ms, "FC")
#'
#' @export
mutate_matrix <- function(.ms, ...)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  quosures <- rlang::enquos(..., .named = FALSE, .ignore_empty = "all")

  new_nms <- names(quosures)
  if (any(new_nms == "")) {
    stop("mew matrices must be named")
  }

  nms <- .matrixnames(.ms)
  all_nms <- unique(c(nms, new_nms), fromLast = TRUE)

  for (i in seq_along(quosures)) {
    quosures[[i]] <- norm_call(quosures[[i]], NULL, FALSE)
  }

  candidates <- .mutate_mat(quosures, .ms, rlang::caller_env())
  candidates <- candidates[all_nms]

  is_null <- vapply(candidates, is_null_obj, logical(1))
  if (any(is_null)) {
    candidates <- candidates[!is_null]
    if (!length(candidates))
      stop("all matrices are NULL. This is currently not supported for 'mutate_mat'")
    all_nms <- names(candidates)
  }

  matrix_info <- info_matrices(candidates, NULL)

  n_matrix <- length(candidates)

  .ms$matrix_set <- candidates[all_nms]
  attr(.ms, "matrix_names") <- all_nms
  attr(.ms, "n_matrix") <- n_matrix
  .ms
}
