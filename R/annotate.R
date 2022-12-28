#' Remove meta info of a `matrixset`
#'
#' @description
#' Deletes row or column annotation (i.e., trait).
#'
#' The tag is a special trait that can't be removed. The tag is the column name
#' of the meta data frame that holds the row or column names. The tag identity
#' of the' object can be obtained via [row_tag()] or [column_tag()].
#'
#' @param .ms    A `matrixset` object
#' @param ...    Name of traits to remove. Tidy selection is supported.
#'
#' @section Groups:
#' Removing a trait that is used for grouping is not allowed and will not work.
#'
#' @returns
#' A `matrixset` with updated row or column meta info.
#'
#' @examples
#' ms1 <- remove_row_annotation(student_results, class, teacher)
#'
#' # this would not work
#' # remove_row_annotation(row_group_by(student_results, class), class)
#' @name remove_anno



#' @rdname remove_anno
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



#' @rdname remove_anno
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




#' Create/modify/delete annotations of a `matrixset` object
#'
#' @description
#' An annotation is a trait that is stored in the meta (row or column) data frame
#' of the `.ms` object.
#'
#' Creating an annotation is done as when applying a `mutate()` on a data frame.
#' Thus, annotations can be created from already existing annotations.
#'
#' The usage is the same as for [dplyr::mutate()], so see this function for
#' instructions on how to create/modify or delete traits.
#'
#' The only difference is that the tag is a special annotation that can't be
#' deleted or modify (with one exception in case of modification). The tag is
#' the column name of the meta data frame that holds the row or column names.
#' The tag identity of the' object can be obtained via [row_tag()] or
#' [column_tag()]. To modify a tag, see `rownames<-()` or `colnames<-()`.
#'
#' @param .ms    A `matrixset` object.
#' @param ...    Name-value pairs, ala `dplyr`'s [dplyr::mutate()].
#'
#' @returns
#' A `matrixset` with updated meta info.
#'
#' @examples
#' # You can create annotation from scrath or using already existing annotation
#' ms <- annotate_row(student_results,
#'                    dummy = 1,
#'                    passed = ifelse(previous_year_score >= 0.6, TRUE, FALSE))
#'
#' # There is no direct access to matrix content, but here is an example on how
#' # it can be done
#' ms <- annotate_row(student_results,
#'                    mn_fail = apply_matrix_dfl(student_results, mn=~ rowMeans(.m1),
#'                                               .matrix_wise = FALSE)$mn)
#'
#' @name annotate



#' @rdname annotate
#' @export
annotate_row <- function(.ms, ...) UseMethod("annotate_row")

#' @export
annotate_row.matrixset <- function(.ms, ...)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  quos <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")
  if (.rowtag(.ms) %in% names(quos))
    stop("row tag (i.e., row info containing row names) can't be removed")

  row_info <- .ms$row_info
  row_info <- dplyr::mutate(row_info, ...)

  tr <- colnames(row_info)

  n <- nrow(.ms)
  if ((ni <- nrow(row_info)) != n) {
    stop(paste("the number of rows is modified by the annotation,",
               "which is against the 'matrixset' paradigm."))
  }

  .ms$row_info <- row_info
  attr(.ms, "row_traits") <- tr

  meta <- get_group_info(row_info, class(.ms), "row")
  attrs <- set_group_attrs(attributes(.ms), meta$attrs, "row")
  attributes(.ms) <- attrs
  class(.ms) <- meta$class
  if (is.null(meta$attrs$group_vars)) class(.ms) <- "matrixset"

  .ms
}





#' @rdname annotate
#' @export
annotate_column <- function(.ms, ...) UseMethod("annotate_column")

#' @export
annotate_column.matrixset <- function(.ms, ...)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  quos <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")
  if (.coltag(.ms) %in% names(quos))
    stop("column tag (i.e., column info containing column names) can't be removed")

  col_info <- .ms$column_info
  col_info <- dplyr::mutate(col_info, ...)

  tr <- colnames(col_info)

  n <- nrow(.ms)
  if ((ni <- nrow(col_info)) != n) {
    stop(paste("the number of columns is modified by the annotation,",
               "which is against the 'matrixset' paradigm."))
  }

  .ms$column_info <- col_info
  attr(.ms, "row_traits") <- tr

  meta <- get_group_info(col_info, class(.ms), "col")
  attrs <- set_group_attrs(attributes(.ms), meta$attrs, "col")
  attributes(.ms) <- attrs
  class(.ms) <- meta$class
  if (is.null(meta$attrs$group_vars)) class(.ms) <- "matrixset"

  .ms
}





