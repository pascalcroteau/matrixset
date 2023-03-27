

#' Convert matrixset to data frame
#'
#' @description
#' Converts a `matrixset` to a `data.frame` (a `tibble`, more specifically), in
#' a long format.
#'
#' When `as_list` is `TRUE`, each matrix is converted separately. Row/column
#' annotation is included if requested.
#'
#' @param .ms                `matrixset` object to convert to `data.frame`
#' @param add_row_info       `logical`, to include row annotation or not
#' @param add_column_info    `logical`, to include column annotation or not
#' @param as_list            `logical`. By default (`FALSE`), a single tibble
#'                           is returned with matrices as columns. When `TRUE`,
#'                           the list structure, an element by converted matrix,
#'                           is kept.
#' @param .matrix            matrix indices of which matrix to include in the
#'                           conversion. The default, `NULL`, means all the
#'                           matrices are used.
#'
#'    If not `NULL`, index is numeric or character vectors.
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
#' @returns
#' A tibble, or if `as_list` is `TRUE`, A `list` of data frames, an element per
#' converted matrix
#'
#' @examples
#' # includes both annotation
#' ms_to_df(student_results)
#'
#' # includes only row annotation
#' ms_to_df(student_results, add_column_info = FALSE)
#'
#' @export
ms_to_df <- function(.ms, add_row_info = TRUE, add_column_info = TRUE,
                     as_list = FALSE, .matrix = NULL)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))


  if (is.null(.matrix)) {
    mats <- .ms$matrix_set
  } else {
    .matrix <- index_to_integer(.matrix, nmatrix(.ms), matrixnames(.ms))
    mats <- .ms$matrix_set[.matrix]
  }


  rnms <- rownames(.ms)
  cnms <- colnames(.ms)
  rtag <- .rowtag(.ms)
  ctag <- .coltag(.ms)


  if (add_row_info) {
    rinf <- .ms$row_info
  }

  if (add_column_info) {
    cinf <- .ms$column_info
  }


  msdf <- lapply(mats, function(m) {
    d <- tibble::as_tibble(m)
    d[[rtag]] <- rnms

    d <- tidyr::pivot_longer(d, tidyselect::all_of(cnms),
                             names_to = ctag,
                             values_to = ".vals")
    if (add_row_info) d <- dplyr::left_join(d, rinf, by = rtag)
    if (add_column_info) d <- dplyr::left_join(d, cinf, by = ctag)
    d
  })

  if (as_list) msdf else dplyr::bind_rows(msdf, .id = ".matrix")
}



