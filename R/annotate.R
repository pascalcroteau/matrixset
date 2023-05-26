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
#' # this doesn't work because "class" is used for grouping
#' ms2 <- tryCatch(remove_row_annotation(row_group_by(student_results, class), class),
#'                 error = function(e) e)
#' is(ms2, "error") #TRUE
#' ms2$message
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
#' @seealso
#' [annotate_row_from_apply()]/[annotate_column_from_apply()], a version that
#' allows access to the `matrixset` matrices.
#'
#' @examples
#' # You can create annotation from scrath or using already existing annotation
#' ms <- annotate_row(student_results,
#'                    dummy = 1,
#'                    passed = ifelse(previous_year_score >= 0.6, TRUE, FALSE))
#'
#' # There is a direct access to matrix content with annotate_row_from_apply(),
#' # but here is an example on how it can be done with annotate_row()
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

  n <- ncol(.ms)
  if ((ni <- nrow(col_info)) != n) {
    stop(paste("the number of columns is modified by the annotation,",
               "which is against the 'matrixset' paradigm."))
  }

  .ms$column_info <- col_info
  attr(.ms, "col_traits") <- tr

  meta <- get_group_info(col_info, class(.ms), "col")
  attrs <- set_group_attrs(attributes(.ms), meta$attrs, "col")
  attributes(.ms) <- attrs
  class(.ms) <- meta$class
  if (is.null(meta$attrs$group_vars)) class(.ms) <- "matrixset"

  .ms
}






#' Apply functions to a single matrix of a matrixset and store results as annotation
#'
#' @description
#' This is in essence [apply_row_dfw()]/[apply_column_dfw()], but with the
#' results saved as new annotations. As such, the usage is almost identical to
#' these functions, except that only a single matrix can be used, and must be
#' specified (matrix specification differs also slightly).
#'
#' @details
#' A conscious choice was made to provide this functionality only for
#' `apply_*_dfw()`, as this is the only version for which the output dimension
#' is guaranteed to respect the `matrixset` paradigm.
#'
#' On that note, see the section 'Grouped `matrixset`'.
#'
#' @section Grouped `matrixset`:
#' In the context of grouping, the `apply_*_dfw()` functions stack the results
#' for each group value.
#'
#' In the case of `annotate_*_from_matrix()`, a [tidyr::pivot_wider()] is
#' further applied to ensure compatibility of the dimension.
#'
#' The `pivot_wider()` arguments `names_prefix`, `names_sep`, `names_glue`,
#' `names_sort`, `names_vary ` and `names_expand` can help you control the final
#' annotation trait names.
#'
#' @param .ms    `matrixset` object
#' @param .matrix   a tidyselect matrix name: matrix name as a bare name or a
#'                  character.
#' @param ...    expressions, separated by commas. They can be specified in one of
#'     the following way:
#'
#'    * a function name, e.g., `mean`.
#'    * a function call, where you can use `.m` to represent the current matrix
#'       (for `apply_matrix`), `.i` to represent the current row (for `apply_row`)
#'       and `.j` for the current column (`apply_column`). Bare names of object
#'       traits can be used as well. For instance, `lm(.i ~ program)`.
#'
#'       The pronouns are also available for the multivariate version, under
#'       certain circumstances, but they have a different meaning. See the
#'       "Multivariate" section for more details.
#'    * a formula expression. The pronouns `.m`, `.i` and `.j` can be used as
#'       well. See examples to see the usefulness of this.
#'
#'    The expressions can be named; these names will be used to provide names to
#'    the results.
#' @param names_prefix,names_sep,names_glue,names_sort,names_vary,names_expand    See
#'   the same arguments of [tidyr::pivot_wider()]
#'
#' @returns
#' A `matrixset` with updated meta info.
#'
#' @seealso
#' [annotate_row()]/[annotate_column()].
#'
#' @examples
#' # This is the same example as in annotate_row(), but with the "proper" way
#' # of doing it
#' ms <- annotate_row_from_apply(student_results, "failure", mn = mean)
#'
#' @name annotate_from_matrix





#' @rdname annotate_from_matrix
#' @export
annotate_row_from_apply <- function(.ms, .matrix, ...,  names_prefix = "",
                                     names_sep = "_", names_glue = NULL,
                                     names_sort = FALSE, names_vary = "fastest",
                                     names_expand = FALSE)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  matnms <- matrixnames(.ms)
  .matrix <- names(tidyselect::eval_select(rlang::enquo(.matrix),
                                           setNames(matnms, matnms)))

  anns <- apply_row_dfw(.ms = .ms, ..., .matrix = .matrix, .matrix_wise = TRUE,
                        .input_list = FALSE)[[.matrix]]

  row_tag <- .rowtag(.ms)
  row_info <- .ms$row_info

  gr_vars <- column_group_vars(.ms)
  if (!is.null(gr_vars)) {
    nms <- colnames(anns)
    drop_cols <- c(row_tag, gr_vars)
    nms <- nms[-which(nms %in% drop_cols)]
    anns <- tidyr::pivot_wider(anns,
                               names_from = tidyselect::all_of(gr_vars),
                               values_from = tidyselect::all_of(nms),
                               names_prefix = names_prefix,
                               names_sep = names_sep,
                               names_glue = names_glue,
                               names_sort = names_sort,
                               names_vary = names_vary,
                               names_expand = names_expand)
  }


  row_info <- dplyr::left_join(row_info, anns, by = row_tag)
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




#' @rdname annotate_from_matrix
#' @export
annotate_column_from_apply <- function(.ms, .matrix, ..., names_prefix = "",
                                        names_sep = "_", names_glue = NULL,
                                        names_sort = FALSE,
                                        names_vary = "fastest",
                                        names_expand = FALSE)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  matnms <- matrixnames(.ms)
  .matrix <- names(tidyselect::eval_select(rlang::enquo(.matrix),
                                           setNames(matnms, matnms)))

  anns <- apply_column_dfw(.ms = .ms, ..., .matrix = .matrix, .matrix_wise = TRUE,
                        .input_list = FALSE)[[.matrix]]

  col_tag <- .coltag(.ms)
  col_info <- .ms$column_info

  gr_vars <- row_group_vars(.ms)
  if (!is.null(gr_vars)) {
    nms <- colnames(anns)
    drop_cols <- c(col_tag, gr_vars)
    nms <- nms[-which(nms %in% drop_cols)]
    anns <- tidyr::pivot_wider(anns,
                               names_from = tidyselect::all_of(gr_vars),
                               values_from = tidyselect::all_of(nms),
                               names_prefix = names_prefix,
                               names_sep = names_sep,
                               names_glue = names_glue,
                               names_sort = names_sort,
                               names_vary = names_vary,
                               names_expand = names_expand)
  }

  col_info <- dplyr::left_join(col_info, anns, by = col_tag)
  tr <- colnames(col_info)

  n <- ncol(.ms)
  if ((ni <- nrow(col_info)) != n) {
    stop(paste("the number of columns is modified by the annotation,",
               "which is against the 'matrixset' paradigm."))
  }

  .ms$column_info <- col_info
  attr(.ms, "col_traits") <- tr

  meta <- get_group_info(col_info, class(.ms), "col")
  attrs <- set_group_attrs(attributes(.ms), meta$attrs, "col")
  attributes(.ms) <- attrs
  class(.ms) <- meta$class
  if (is.null(meta$attrs$group_vars)) class(.ms) <- "matrixset"


  .ms
}

