

get_group_info <- function(info, class, dim)
{
  group_vars <- dplyr::group_vars(info)

  if (length(group_vars))
  {
    group_meta <- dplyr::group_data(info)
    group_indices <- dplyr::group_indices(info)
    group_rows <- dplyr::group_rows(info)
    group_keys <- dplyr::group_keys(info)
    level_drop <- dplyr::group_by_drop_default(info)

    if (dim == "row") {
      new_class <- if (any(c("dual_grouped_ms", "col_grouped_ms") %in% class)) "dual_grouped_ms" else "row_grouped_ms"
    } else {
      new_class <- if (any(c("dual_grouped_ms", "row_grouped_ms") %in% class)) "dual_grouped_ms" else "col_grouped_ms"
    }

    new_class <- c(new_class, "matrixset")
  } else {

    group_vars <- NULL
    group_meta <- NULL
    group_indices <- NULL
    group_rows <- NULL
    group_keys <- NULL
    level_drop <- NULL
    new_class <- class

  }

  list(attrs = list(group_meta=group_meta, group_indices=group_indices,
                    group_rows=group_rows, group_keys=group_keys,
                    group_vars=group_vars, group_level_drop=level_drop),
       class = new_class)
}


set_group_attrs <- function(old_attrs, new_attrs, dim)
{
  names(new_attrs) <- paste(dim, names(new_attrs), sep = "_")
  attrs <- c(old_attrs, new_attrs)
  attrs <- attrs[!duplicated(names(attrs), fromLast = TRUE)]
  attrs
}



#' Group rows/columns of a matrixset by one or more variables
#'
#' @description
#' Applying [row_group_by()] or [column_group_by()] to a `matrixset` object
#' registers this object as one where certain operations are performed per
#' (row or column) group.
#'
#' To (partly) remove grouping, use [row_ungroup()] or [column_ungroup()].
#'
#' These functions are the `matrixset` equivalent of `dplyr`'s
#' [dplyr::group_by()] and [dplyr::ungroup()]
#'
#' @param .ms     A `matrixset` object
#' @param ...     In [row_group_by()] or [column_group_by()], annotation
#'                variables to use for grouping. These variables are the ones
#'                returned by [row_traits()] or [column_traits()]. In [row_ungroup()]
#'                or [column_ungroup()], variables to remove from grouping. If
#'                not provided, grouping is removed altogether.
#' @param .add    `logical`. The default, `FALSE`, means that previous groups
#'                are overwritten. Setting `.add` to `TRUE` will add to the
#'                existing groups.
#' @param .drop   `logical`. When grouping by `factor` annotations, should
#'                levels that do not appear in the data be dropped? The default
#'                is `TRUE`, unless `.ms` has been previously grouped with
#'                `.drop = FALSE`.
#'
#' @returns
#' A grouped `matrixset` with class `row_grouped_ms`, unless `.ms` was already
#' column-grouped via [column_group_by()], in which case a `dual_grouped_ms`
#' `matrixset` is returned.
#'
#' If the combination of `...` and `.add` yields an empty set of grouping
#' columns, a regular `matrixset`or a `col_grouped_ms`, as appropriate, will be
#' returned.
#'
#' @examples
#' by_class <- row_group_by(student_results, class)
#'
#' # On it's own, a grouped `matrixset` looks like a regular `matrixset`, except
#' # that the grouping structure is listed
#' by_class
#'
#' # Grouping changes how some functions operates
#' filter_row(by_class, previous_year_score > mean(previous_year_score))
#'
#' # You can group by expressions: you end-up grouping by the new annotation:
#' row_group_by(student_results, sqrt_score = sqrt(previous_year_score))
#'
#' # By default, grouping overrides existing grouping
#' row_group_vars(row_group_by(by_class, teacher))
#'
#' # Use .add = TRUE to instead append
#' row_group_vars(row_group_by(by_class, teacher, .add = TRUE))
#' # To removing grouping, use ungroup
#' row_ungroup(by_class)
#'
#' @name group_by


#' @rdname group_by
#' @export
row_group_by <- function(.ms, ..., .add = FALSE, .drop = row_group_by_drop_default(.ms))
  UseMethod("row_group_by")


#' @export
row_group_by.matrixset <- function(.ms, ..., .add = FALSE, .drop = row_group_by_drop_default(.ms))
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)
  group_info <- dplyr::group_by(.ms$row_info, ..., .add = .add, .drop = .drop)

  meta <- get_group_info(group_info, class(.ms), "row")
  .ms$row_info <- group_info
  attrs <- set_group_attrs(attributes(.ms), meta$attrs, "row")
  attributes(.ms) <- attrs
  class(.ms) <- meta$class

  # group_vars <- dplyr::group_vars(group_info)
  #
  # if (length(group_vars))
  # {
  #   group_meta <- dplyr::group_data(group_info)
  #   group_indices <- dplyr::group_indices(group_info)
  #   group_rows <- dplyr::group_rows(group_info)
  #   group_keys <- dplyr::group_keys(group_info)
  #   level_drop <- dplyr::group_by_drop_default(group_info)
  #
  #   .ms$row_info <- group_info
  #
  #   attr(.ms, "row_group_meta") <- group_meta
  #   attr(.ms, "row_group_indices") <- group_indices
  #   attr(.ms, "row_group_rows") <- group_rows
  #   attr(.ms, "row_group_keys") <- group_keys
  #   attr(.ms, "row_group_vars") <- group_vars
  #   attr(.ms, "row_group_level_drop") <- level_drop
  #
  #   cms <- class(.ms)
  #   new_class <- if ("col_grouped_ms" %in% cms) "dual_grouped_ms" else "row_grouped_ms"
  #   class(.ms) <- c(new_class, "matrixset")
  # }

  .ms
}



#' @rdname group_by
#' @export
column_group_by <- function(.ms, ..., .add = FALSE, .drop = column_group_by_drop_default(.ms))
  UseMethod("column_group_by")


#' @export
column_group_by.matrixset <- function(.ms, ..., .add = FALSE, .drop = column_group_by_drop_default(.ms))
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)
  group_info <- dplyr::group_by(.ms$column_info, ..., .add = .add, .drop = .drop)
  # group_vars <- dplyr::group_vars(group_info)
  #
  #
  # if (length(group_vars))
  # {
  #   group_meta <- dplyr::group_data(group_info)
  #   group_indices <- dplyr::group_indices(group_info)
  #   group_rows <- dplyr::group_rows(group_info)
  #   group_keys <- dplyr::group_keys(group_info)
  #   level_drop <- dplyr::group_by_drop_default(group_info)
  #
  #   .ms$column_info <- group_info
  #
  #   attr(.ms, "col_group_meta") <- group_meta
  #   attr(.ms, "col_group_indices") <- group_indices
  #   attr(.ms, "col_group_rows") <- group_rows
  #   attr(.ms, "col_group_keys") <- group_keys
  #   attr(.ms, "col_group_vars") <- group_vars
  #   attr(.ms, "col_group_level_drop") <- level_drop
  #
  #   cms <- class(.ms)
  #   new_class <- if ("row_grouped_ms" %in% cms) "dual_grouped_ms" else "col_grouped_ms"
  #   class(.ms) <- c(new_class, "matrixset")
  # }

  meta <- get_group_info(group_info, class(.ms), "col")
  .ms$column_info <- group_info
  attrs <- set_group_attrs(attributes(.ms), meta$attrs, "col")
  attributes(.ms) <- attrs
  class(.ms) <- meta$class

  .ms
}



#' Default value for .drop argument of function row_group_by()
#'
#' Default value for `.drop` argument of function [row_group_by()]
#'
#' @param .ms    a `matrixset` object
#'
#' @returns
#' Returns `TRUE` for row-ungrouped `matrixset`s. For row-grouped objects, the
#' default is also `TRUE` unless `.ms` has been previously grouped with
#' `.drop = FALSE`.
#'
#' @examples
#' sr_grouped <- row_group_by(student_results, class, .drop = FALSE)
#' row_group_by_drop_default(sr_grouped)
#'
#'
#'
#' @export
row_group_by_drop_default <- function(.ms) UseMethod("row_group_by_drop_default")

#' @export
row_group_by_drop_default.matrixset <- function(.ms)
{
  drop_attr <- attr(.ms, "row_group_level_drop")
  if (is.null(drop_attr)) return(TRUE)
  drop_attr
}



#' Default value for .drop argument of function column_group_by()
#'
#' Default value for `.drop` argument of function [column_group_by()]
#'
#' @param .ms    a `matrixset` object
#'
#' @returns
#' Returns `TRUE` for column-ungrouped `matrixset`s. For column-grouped objects,
#' the default is also `TRUE` unless `.ms` has been previously grouped with
#' `.drop = FALSE`.
#'
#' @examples
#' sr <- row_group_by(student_results, class, .drop = FALSE)
#' row_group_by_drop_default(sr)
#'
#' @export
column_group_by_drop_default <- function(.ms) UseMethod("column_group_by_drop_default")


#' @export
column_group_by_drop_default.matrixset <- function(.ms)
{
  drop_attr <- attr(.ms, "col_group_level_drop")
  if (is.null(drop_attr)) return(TRUE)
  drop_attr
}



#' @rdname group_by
#' @export
row_ungroup <- function(.ms, ...) UseMethod("row_ungroup")
#' @export
row_ungroup.matrixset <- function(.ms, ...) .ms
#' @export
row_ungroup.col_grouped_ms <- function(.ms, ...) .ms

#' @export
row_ungroup.row_grouped_ms <- function(.ms, ...)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)
  group_info <- dplyr::ungroup(.ms$row_info, ...)
  group_vars <- dplyr::group_vars(group_info)

  if (length(group_vars)) {

    group_meta <- dplyr::group_data(group_info)
    group_indices <- dplyr::group_indices(group_info)
    group_rows <- dplyr::group_rows(group_info)
    group_keys <- dplyr::group_keys(group_info)
    level_drop <- dplyr::group_by_drop_default(group_info)

    attr(.ms, "row_group_meta") <- group_meta
    attr(.ms, "row_group_indices") <- group_indices
    attr(.ms, "row_group_rows") <- group_rows
    attr(.ms, "row_group_keys") <- group_keys
    attr(.ms, "row_group_vars") <- group_vars
    attr(.ms, "row_group_level_drop") <- level_drop

  } else {

    attr(.ms, "row_group_meta") <- NULL
    attr(.ms, "row_group_indices") <- NULL
    attr(.ms, "row_group_rows") <- NULL
    attr(.ms, "row_group_keys") <- NULL
    attr(.ms, "row_group_vars") <- NULL
    attr(.ms, "row_group_level_drop") <- NULL

    cms <- class(.ms)
    class(.ms) <- if ("dual_grouped_ms" %in% cms) c("col_grouped_ms", "matrixset") else "matrixset"

  }

  .ms$row_info <- group_info

  .ms
}

#' @export
row_ungroup.dual_grouped_ms <- function(.ms, ...)
  row_ungroup.row_grouped_ms(.ms, ...)




#' @rdname group_by
#' @export
column_ungroup <- function(.ms, ...) UseMethod("column_ungroup")
#' @export
column_ungroup.matrixset <- function(.ms, ...) .ms
#' @export
column_ungroup.row_grouped_ms <- function(.ms, ...) .ms

#' @export
column_ungroup.col_grouped_ms <- function(.ms, ...)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  assess_tidyable(.ms)
  group_info <- dplyr::ungroup(.ms$column_info, ...)
  group_vars <- dplyr::group_vars(group_info)

  if (length(group_vars)) {

    group_meta <- dplyr::group_data(group_info)
    group_indices <- dplyr::group_indices(group_info)
    group_rows <- dplyr::group_rows(group_info)
    group_keys <- dplyr::group_keys(group_info)
    level_drop <- dplyr::group_by_drop_default(group_info)

    attr(.ms, "col_group_meta") <- group_meta
    attr(.ms, "col_group_indices") <- group_indices
    attr(.ms, "col_group_rows") <- group_rows
    attr(.ms, "col_group_keys") <- group_keys
    attr(.ms, "col_group_vars") <- group_vars
    attr(.ms, "col_group_level_drop") <- level_drop

  } else {

    attr(.ms, "col_group_meta") <- NULL
    attr(.ms, "col_group_indices") <- NULL
    attr(.ms, "col_group_rows") <- NULL
    attr(.ms, "col_group_keys") <- NULL
    attr(.ms, "col_group_vars") <- NULL
    attr(.ms, "col_group_level_drop") <- NULL

    cms <- class(.ms)
    class(.ms) <- if ("dual_grouped_ms" %in% cms) c("row_grouped_ms", "matrixset") else "matrixset"

  }

  .ms$column_info <- group_info

  .ms
}

#' @export
column_ungroup.dual_grouped_ms <- function(.ms, ...)
  column_ungroup.col_grouped_ms(.ms, ...)



# ADD GROUP_META, WHICH IS DPLYR'S GROUP_DATA

#' Matrixset group metadata
#'
#' @description
#' * [row_group_meta()] and [column_group_meta()] returns the grouping structure,
#'   in a data frame format. See `dplyr`'s [dplyr::group_data()], from which the
#'   functions are based. Returns `NULL` for ungrouped `matrixset`s.
#' * [row_group_keys()] and [column_group_keys()] retrieve the grouping data,
#'     while the locations (row or column indices) are retrieved with
#'     [row_group_where()] and [column_group_where()].
#' * [row_group_indices()] and [column_group_indices()] each return an integer
#'     vector the same length as the number of rows or columns of `.ms`, and
#'     gives the group that each row or column belongs to.
#' * [row_group_vars()] and [column_group_vars()] give names of grouping
#'    variables as character vector; [row_groups()] and [column_groups()] give
#'    the names as a list of symbols.
#'
#' @param .ms    a `matrixset` object
#'
#' @name meta



#' @rdname meta
#' @export
row_group_meta <- function(.ms) UseMethod("row_group_meta")
#' @export
row_group_meta.matrixset <- function(.ms) NULL
#' @export
row_group_meta.row_grouped_ms <- function(.ms) attr(.ms, "row_group_meta")
#' @export
row_group_meta.dual_grouped_ms <- function(.ms) attr(.ms, "row_group_meta")



#' @rdname meta
#' @export
row_group_vars <- function(.ms) UseMethod("row_group_vars")
#' @export
row_group_vars.matrixset <- function(.ms) NULL
#' @export
row_group_vars.row_grouped_ms <- function(.ms) attr(.ms, "row_group_vars")
#' @export
row_group_vars.dual_grouped_ms <- function(.ms) attr(.ms, "row_group_vars")




#' @rdname meta
#' @export
row_group_keys <- function(.ms) UseMethod("row_group_keys")
#' @export
row_group_keys.matrixset <- function(.ms) NULL
#' @export
row_group_keys.row_grouped_ms <- function(.ms) attr(.ms, "row_group_keys")
#' @export
row_group_keys.dual_grouped_ms <- function(.ms) attr(.ms, "row_group_keys")




#' @rdname meta
#' @export
row_group_where <- function(.ms) UseMethod("row_group_where")
#' @export
row_group_where.matrixset <- function(.ms) NULL
#' @export
row_group_where.row_grouped_ms <- function(.ms) attr(.ms, "row_group_rows")
#' @export
row_group_where.dual_grouped_ms <- function(.ms) attr(.ms, "row_group_rows")



#' @rdname meta
#' @export
row_group_indices <- function(.ms) UseMethod("row_group_indices")
#' @export
row_group_indices.matrixset <- function(.ms) NULL
#' @export
row_group_indices.row_grouped_ms <- function(.ms) attr(.ms, "row_group_indices")
#' @export
row_group_indices.dual_grouped_ms <- function(.ms) attr(.ms, "row_group_indices")




#' @rdname meta
#' @export
row_groups <- function(.ms) UseMethod("row_groups")
#' @export
row_groups.matrixset <- function(.ms) NULL
#' @export
row_groups.row_grouped_ms <- function(.ms) rlang::syms(row_group_vars(.ms))
#' @export
row_groups.dual_grouped_ms <- function(.ms) rlang::syms(row_group_vars(.ms))



#' @rdname meta
#' @export
column_group_meta <- function(.ms) UseMethod("column_group_meta")
#' @export
column_group_meta.matrixset <- function(.ms) NULL
#' @export
column_group_meta.col_grouped_ms <- function(.ms) attr(.ms, "col_group_meta")
#' @export
column_group_meta.dual_grouped_ms <- function(.ms) attr(.ms, "col_group_meta")



#' @rdname meta
#' @export
column_group_vars <- function(.ms) UseMethod("column_group_vars")
#' @export
column_group_vars.matrixset <- function(.ms) NULL
#' @export
column_group_vars.col_grouped_ms <- function(.ms) attr(.ms, "col_group_vars")
#' @export
column_group_vars.dual_grouped_ms <- function(.ms) attr(.ms, "col_group_vars")




#' @rdname meta
#' @export
column_group_keys <- function(.ms) UseMethod("column_group_keys")
#' @export
column_group_keys.matrixset <- function(.ms) NULL
#' @export
column_group_keys.col_grouped_ms <- function(.ms) attr(.ms, "col_group_keys")
#' @export
column_group_keys.dual_grouped_ms <- function(.ms) attr(.ms, "col_group_keys")




#' @rdname meta
#' @export
column_group_where <- function(.ms) UseMethod("column_group_where")
#' @export
column_group_where.matrixset <- function(.ms) NULL
#' @export
column_group_where.col_grouped_ms <- function(.ms) attr(.ms, "col_group_rows")
#' @export
column_group_where.dual_grouped_ms <- function(.ms) attr(.ms, "col_group_rows")



#' @rdname meta
#' @export
column_group_indices <- function(.ms) UseMethod("column_group_indices")
#' @export
column_group_indices.matrixset <- function(.ms) NULL
#' @export
column_group_indices.col_grouped_ms <- function(.ms) attr(.ms, "col_group_indices")
#' @export
column_group_indices.dual_grouped_ms <- function(.ms) attr(.ms, "col_group_indices")




#' @rdname meta
#' @export
column_groups <- function(.ms) UseMethod("column_groups")
#' @export
column_groups.matrixset <- function(.ms) NULL
#' @export
column_groups.col_grouped_ms <- function(.ms) rlang::syms(column_group_vars(.ms))
#' @export
column_groups.dual_grouped_ms <- function(.ms) rlang::syms(column_group_vars(.ms))






















