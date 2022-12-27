


norm_filt_expr <- function(...)
{
  expr <- rlang::enquos(...)
  expr <- lapply(expr, function(e) rlang::quo_get_expr(e))
  expr <- if (length(expr) > 1) rlang::call2("&", !!!expr) else expr[[1]]
  expr
}

#' Subset rows using annotation values
#'
#' @description
#' The [filter_row()] function subsets the rows of all matrices of a
#' `matrixset`, retaining all rows that satisfy given condition(s). The function
#' `filter_row` works like `dplyr`'s [dplyr::filter()].
#'
#' @details
#' The conditions are given as expressions in `...`, which are applied to
#' columns of the annotation data frame (`row_info`) to determine which rows
#' should be retained.
#'
#' It can be applied to both grouped and ungrouped `matrixset` (see
#' [row_group_by()]), and section \sQuote{Grouped matrixsets}.
#'
#' @section Grouped matrixsets:
#' Column grouping ([column_group_by()]) has no impact on row filtering.
#'
#' The impact of row grouping ([row_group_by()]) on row filtering depends on
#' the conditions. Often, row grouping will not have any impact, but as soon as
#' an aggregating, lagging or ranking function is involved, then the results
#' will differ.
#'
#' For instance, the two following are not equivalent (except by pure
#' coincidence).
#'
#' `student_results %>% filter_row(previous_year_score > mean(previous_year_score))`
#'
#' And it's grouped equivalent:
#' `student_results %>% row_group_by(class) %>% filter_row(previous_year_score > mean(previous_year_score))`
#'
#' In the ungrouped version, the mean of `previous_year_score` is taken globally
#' and `filter_row` keeps rows with `previous_year_score` greater than this
#' global average. In the grouped version, the average is calculated within each
#' `class` and the kept rows are the ones with `previous_year_score` greater
#' than the within-class average.
#'
#' @param .ms          `matrixset` object to subset based on the filtering
#'                     conditions
#' @param ...          Condition, or expression, that returns a logical value,
#'                     used to determine if rows are kept or discarded. The
#'                     expression may refer to row annotations - columns of
#'                     the `row_info` component of `.ms` More than one
#'                     condition can be supplied and if multiple
#'                     expressions are included, they are combined with the `&`
#'                     operator. Only rows for which all conditions evaluate to
#'                     TRUE are kept.
#' @param .preserve    `logical`, relevant only if `.ms` is row grouped. When
#'                     `.preserve` is `FALSE` (the default), the row grouping
#'                     is updated based on the new `matrixset` resulting from
#'                     the filtering. Otherwise, the row grouping is kept as is.
#'
#' @returns
#' A `matrixset`, with possibly a subset of the rows of the original object.
#' Groups will be updated if `.preserve` is `TRUE`.
#'
#' @examples
#' # Filtering using one condition
#' filter_row(student_results, class == "classA")
#'
#' # Filetring using multiple conditions. These are equivalent
#' filter_row(student_results, class == "classA" & previous_year_score > 0.75)
#' filter_row(student_results, class == "classA", previous_year_score > 0.75)
#'
#' # The potential difference between grouped and non-grouped.
#' filter_row(student_results, previous_year_score > mean(previous_year_score))
#' student_results |>
#'   row_group_by(teacher) |>
#'   filter_row(previous_year_score > mean(previous_year_score))
#'
#
#' @export
filter_row <- function(.ms, ..., .preserve = FALSE) UseMethod("filter_row")


#' @export
filter_row.matrixset <- function(.ms, ..., .preserve = FALSE)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  filter_expr <- norm_filt_expr(...)

  assess_all_vars(filter_expr, c(.rowtag(.ms), .rowtraits(.ms)),
                  rlang::caller_env())

  tmp_info <- dplyr::mutate(.ms$row_info, ._idx_ = !!filter_expr)
  idx <- tmp_info$._idx_

  if (!any(idx)) return(.ms)

  .ms[idx,,]
}



#' @export
filter_row.row_grouped_ms <- function(.ms, ..., .preserve = FALSE)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  filter_expr <- norm_filt_expr(...)

  tmp_info <- dplyr::filter(.ms$row_info, !!filter_expr, .preserve = .preserve)
  tag <- .rowtag(.ms)
  idx <- match(tmp_info[[tag]], .ms$row_info[[tag]])

  # .ms <- .ms[idx,,]
  .ms <- matset_subset(.ms, i=idx, j=NULL, matrix=NULL, drop=FALSE,
                       keep_annotation=TRUE, warn_class_change=FALSE)

  # group_meta <- dplyr::group_data(tmp_info)
  # group_indices <- dplyr::group_indices(tmp_info)
  # group_rows <- dplyr::group_rows(tmp_info)
  # group_keys <- dplyr::group_keys(tmp_info)
  # group_vars <- dplyr::group_vars(tmp_info)
  # level_drop <- dplyr::group_by_drop_default(tmp_info)
  #
  # .ms$row_info <- tmp_info
  #
  # attr(.ms, "row_group_meta") <- group_meta
  # attr(.ms, "row_group_indices") <- group_indices
  # attr(.ms, "row_group_rows") <- group_rows
  # attr(.ms, "row_group_keys") <- group_keys
  # attr(.ms, "row_group_vars") <- group_vars
  # attr(.ms, "row_group_level_drop") <- level_drop

  meta <- get_group_info(tmp_info, class(.ms), "row")
  .ms$row_info <- tmp_info
  attrs <- set_group_attrs(attributes(.ms), meta$attrs, "row")
  attributes(.ms) <- attrs
  class(.ms) <- meta$class

  .ms

}




#' @export
filter_row.dual_grouped_ms <- function(.ms, ..., .preserve = FALSE)
{
  filter_row.row_grouped_ms(.ms, ..., .preserve)
}



#' Subset columns using annotation values
#'
#' @description
#' The [filter_column()] function subsets the columns of all matrices of a
#' `matrixset`, retaining all columns that satisfy given condition(s). The
#' function `filter_column` works like `dplyr`'s [dplyr::filter()].
#'
#' @details
#' The conditions are given as expressions in `...`, which are applied to
#' columns of the annotation data frame (`column_info`) to determine which
#' columns should be retained.
#'
#' It can be applied to both grouped and ungrouped `matrixset` (see
#' [column_group_by()]), and section \sQuote{Grouped matrixsets}.
#'
#' @section Grouped matrixsets:
#' Row grouping ([row_group_by()]) has no impact on column filtering.
#'
#' The impact of column grouping ([column_group_by()]) on column filtering
#' depends on the conditions. Often, column grouping will not have any impact,
#' but as soon as an aggregating, lagging or ranking function is involved, then
#' the results will differ.
#'
#' For instance, the two following are not equivalent (except by pure
#' coincidence).
#'
#' `student_results %>% filter_column(school_average > mean(school_average))`
#'
#' And it's grouped equivalent:
#' `student_results %>% column_group_by(program) %>% filter_column(school_average > mean(school_average))`
#'
#' In the ungrouped version, the mean of `school_average` is taken globally
#' and `filter_column` keeps columns with `school_average` greater than this
#' global average. In the grouped version, the average is calculated within each
#' `class` and the kept columns are the ones with `school_average` greater
#' than the within-class average.
#'
#' @param .ms          `matrixset` object to subset based on the filtering
#'                     conditions
#' @param ...          Condition, or expression, that returns a logical value,
#'                     used to determine if columns are kept or discarded. The
#'                     expression may refer to column annotations - columns of
#'                     the `column_info` component of `.ms` More than one
#'                     condition can be supplied and if multiple
#'                     expressions are included, they are combined with the `&`
#'                     operator. Only columns for which all conditions evaluate
#'                     to TRUE are kept.
#' @param .preserve    `logical`, relevant only if `.ms` is column grouped. When
#'                     `.preserve` is `FALSE` (the default), the column grouping
#'                     is updated based on the new `matrixset` resulting from
#'                     the filtering. Otherwise, the column grouping is kept as
#'                     is.
#'
#' @returns
#' A `matrixset`, with possibly a subset of the columns of the original object.
#' Groups will be updated if `.preserve` is `TRUE`.
#'
#' @examples
#' # Filtering using one condition
#' filter_column(student_results, program == "Applied Science")
#'
#' # Filetring using multiple conditions. These are equivalent
#' filter_column(student_results, program == "Applied Science" & school_average > 0.8)
#' filter_column(student_results, program == "Applied Science", school_average > 0.8)
#'
#' # The potential difference between grouped and non-grouped.
#' filter_column(student_results, school_average > mean(school_average))
#' student_results |>
#'   column_group_by(program) |>
#'   filter_column(school_average > mean(school_average))
#'
#' @export
filter_column <- function(.ms, ..., .preserve = FALSE) UseMethod("filter_column")


#' @export
filter_column.matrixset <- function(.ms, ..., .preserve = FALSE)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  filter_expr <- norm_filt_expr(...)

  tmp_info <- dplyr::mutate(.ms$column_info, ._idx_ = !!filter_expr)
  idx <- tmp_info$._idx_

  if (!any(idx)) return(.ms)

  .ms[,idx,]
}



#' @export
filter_column.col_grouped_ms <- function(.ms, ..., .preserve = FALSE)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)

  filter_expr <- norm_filt_expr(...)

  tmp_info <- dplyr::filter(.ms$column_info, !!filter_expr,
                            .preserve = .preserve)
  tag <- .coltag(.ms)
  idx <- match(tmp_info[[tag]], .ms$column_info[[tag]])

  # .ms <- .ms[,idx,]
  #
  # group_meta <- dplyr::group_data(tmp_info)
  # group_indices <- dplyr::group_indices(tmp_info)
  # group_rows <- dplyr::group_rows(tmp_info)
  # group_keys <- dplyr::group_keys(tmp_info)
  # group_vars <- dplyr::group_vars(tmp_info)
  # level_drop <- dplyr::group_by_drop_default(tmp_info)
  #
  # .ms$column_info <- tmp_info
  #
  # attr(.ms, "col_group_meta") <- group_meta
  # attr(.ms, "col_group_indices") <- group_indices
  # attr(.ms, "col_group_rows") <- group_rows
  # attr(.ms, "col_group_keys") <- group_keys
  # attr(.ms, "col_group_vars") <- group_vars
  # attr(.ms, "col_group_level_drop") <- level_drop


  .ms <- matset_subset(.ms, i=NULL, j=idx, matrix=NULL, drop=FALSE,
                       keep_annotation=TRUE, warn_class_change=FALSE)

  meta <- get_group_info(tmp_info, class(.ms), "col")
  .ms$column_info <- tmp_info
  attrs <- set_group_attrs(attributes(.ms), meta$attrs, "col")
  attributes(.ms) <- attrs
  class(.ms) <- meta$class

  .ms

}


#' @export
filter_column.dual_grouped_ms <- function(.ms, ..., .preserve = FALSE)
{
  filter_column.col_grouped_ms(.ms, ..., .preserve)
}




