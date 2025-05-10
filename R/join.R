



# build_glue <- function(ng)
# {
#   if (is.null(ng) || (is.logical(ng) && isFALSE(ng))) return(NULL)
#   if (is.logical(ng)) ng <- "{.tag}"
#   has_tag <- stringr::str_detect(ng, "\\{\\.tag\\}")
#   if (!has_tag) ng <- paste("{.tag}", ng, sep = "_")
#   ng
# }



# unique_names <- function(tplt, tag, dat)
# {
#   if (tplt == "{.tag}") return(make_unique(dat[[tag]]))
#   id_unq <- unique_id(dat[[tag]], nrow(dat))
#   ldat <- dat[id_unq > 0, ]
#   # dat[[".tag"]] <- dat[[tag]]
#   ldat <- list2env(ldat, parent = emptyenv())
#   ldat[[".tag"]] <- ldat[[tag]]
#   new_tag <- stringr::str_glue_data(tplt, .x=ldat)
#   if (vctrs::vec_unique_count(new_tag) != length(new_tag))
#     stop(paste("'by' does not result in unique", tag))
#
#   new_dat_tag <- dat[[tag]]
#   new_dat_tag[id_unq > 0] <- new_tag
#   new_dat_tag
# }





#' @importFrom stats setNames
# set_by_null <- function (by, x_nms, y_nms, x_tag, y_tag)
# {
#   if (is.null(by)) {
#     if (x_tag %in% x_nms && !is.null(y_tag) && y_tag %in% y_nms) {
#       by <- setNames(y_tag, x_tag)
#     } else {
#       by <- intersect(x_nms, y_nms)
#       if (length(by) == 0) {
#         msg <- "`by` must be supplied when `x` and `y` have no common variables."
#         stop(msg)
#       }
#     }
#   }
#   by
# }


# assess_by_vars_margin <- function(vars, nms)
# {
#   dup <- duplicated(vars)
#   if (any(dup)) {
#     msg <- "Join variables must be unique."
#     stop(msg)
#   }
#
#   if (!all(is_in <- vars %in% nms)) {
#     msg <- if (sum(!is_in) > 1) "variables" else "variable"
#     vars_quoted <- encodeString(vars[!is_in], quote = "\"")
#     msg <- paste(msg, paste(vars_quoted, collapse = ", "), "is not a known trait")
#     stop(msg)
#   }
#   NULL
# }
#
#
# set_by_vars <- function(by, x_nms, y_nms)
# {
#   if (is.character(by) || is.list(by)) {
#     by_nms <- names(by)
#     by <- unname(by)
#
#     if (is.list(by) && !all(sapply(by, is.character)))
#       stop("`by` must be a list of character when a list")
#
#     if (is.list(by) && !all(sapply(by, function(x) length(x) == 1L)))
#       stop("elements of `by` must be of length 1 when it is a list")
#
#     by <- unlist(by)
#
#     # by_x <- rlang::`%||%`(by_nms, by)
#     by_x <- by_nms %OR% by
#     by_y <- by
#
#   } else {
#     msg <- "`by` must be a (named) character vector, list, or NULL"
#     stop(msg)
#   }
#
#   assess_by_vars_margin(by_x, x_nms)
#   assess_by_vars_margin(by_y, y_nms)
#
#   setNames(by_y, by_x)
#
# }



#' join_names <- function(obj, mrg = NULL) UseMethod("join_names")
#'
#' #' @export
#' join_names.matrixset <- function(obj, mrg = NULL)
#' {
#'   if (mrg == "row") {
#'     c(.rowtag(obj), .rowtraits(obj))
#'   } else {
#'     c(.coltag(obj), .coltraits(obj))
#'   }
#' }
#' #' @export
#' join_names.data.frame <- function(obj, mrg = NULL) colnames(obj)


#' join_tag <- function(obj, mrg = NULL) UseMethod("join_tag")
#'
#' #' @export
#' join_tag.matrixset <- function(obj, mrg = NULL)
#' {
#'   if (mrg == "row") {
#'     .rowtag(obj)
#'   } else {
#'     .coltag(obj)
#'   }
#' }
#' #' @export
#' join_tag.data.frame <- function(obj, mrg = NULL) NULL


#' join_info <- function(obj, mrg = NULL) UseMethod("join_info")
#' #' @export
#' join_info.matrixset <- function(obj, mrg = NULL)
#' {
#'   cl <- sys.call()
#'   cash_status$set(cl)
#'   on.exit(cash_status$clear(cl))
#'
#'
#'   if (mrg == "row") {
#'     obj$row_info
#'   } else {
#'     obj$column_info
#'   }
#' }
#' #' @export
#' join_info.data.frame <- function(obj, mrg = NULL) obj



# margin_names <- function(obj, mrg)
# {
#   if (mrg == "row") {
#     list(nms = rownames(obj), compl = colnames(obj))
#   } else {
#     list(nms = colnames(obj), compl = rownames(obj))
#   }
# }



# set_adjust <- function(adj, y_ms)
# {
#   adj_type <- typeof(adj)
#   adjust <- switch (adj_type,
#                     "logical" = adj,
#                     "character" = TRUE,
#                     stop("adjust parameter must be a logical or a character vector of length 1.")
#                     )
#
#   if (length(adj) > 1) {
#     warning("'adjust' is of length > 1. Keeping the first element only.")
#     adjust <- adjust[1]
#   }
#
#   adjust_how <- if (is.character(adj)) {
#     match_option(adj, adjust_opts)
#   } else adjust_opts["x_only"]
#
#   if (adjust && adjust_how != adjust_opts["x_only"] && !y_ms) {
#     adjust_how <- adjust_opts["x_only"]
#     warning(stringr::str_glue("'adjust' has been forced to '{ADJ}' because 'y' is a data.frame",
#                               ADJ = adjust_opts["x_only"]))
#   }
#
#   list(adjust = adjust, adjust_how = adjust_how)
# }



# fill_matrix <- function(m, margin, nr, nc, old_names, all_names, compl_names)
# {
#   if (is.null(m)) return(NULL)
#
#   is_Matrix <- is(m, "Matrix")
#
#   d <- dim(m)
#   if (d[1]*d[2] == 0L) {
#     na_val <- rlang::eval_tidy(rlang::call2(storage.mode(m), 1))
#     na_val[] <- NA
#   } else {
#     na_val <- m[1,1]
#     na_val[] <- NA
#   }
#
#   new_names <- setdiff(all_names, old_names)
#   pos <- match(old_names, all_names)
#
#   if (margin == "row") {
#
#     newm <- MATRIX(na_val, nrow = length(all_names), ncol = nc, is_Matrix)
#     rownames(newm) <- all_names
#     colnames(newm) <- compl_names
#     if (is_Matrix) {
#       newm[] <- na_val
#       newm <- methods::as(newm, class(m))
#     }
#     newm[pos, ] <- m
#
#
#   } else {
#
#     newm <- MATRIX(na_val, nrow = nr, ncol = length(all_names), is_Matrix)
#     rownames(newm) <- compl_names
#     colnames(newm) <- all_names
#     if (is_Matrix) {
#       newm[] <- na_val
#       newm <- methods::as(newm, class(m))
#     }
#     newm[, pos] <- m
#
#
#   }
#
#   newm
#
# }
# fill_matrix <- function(m, margin, nr, nc, old_names, joined_names, compl_names,
#                         joined_names_unique)
# {
#   if (is.null(m)) return(NULL)
#
#   is_Matrix <- is(m, "Matrix")
#
#   d <- dim(m)
#   if (d[1]*d[2] == 0L) {
#     na_val <- rlang::eval_tidy(rlang::call2(storage.mode(m), 1))
#     na_val[] <- NA
#   } else {
#     na_val <- m[1,1]
#     na_val[] <- NA
#   }
#
#   new_names <- setdiff(joined_names, old_names)
#   pos <- if (is.null(joined_names_unique)) {
#     match(old_names, joined_names)
#   } else {
#     common <- intersect(joined_names, old_names)
#     unlist(lapply(common, function(.x) which(.x == joined_names)))
#   }
#
#   if (margin == "row") {
#
#     # newm <- MATRIX(na_val, nrow = length(joined_names), ncol = nc, is_Matrix)
#     # rownames(newm) <- joined_names
#     # colnames(newm) <- compl_names
#     # if (is_Matrix) {
#     #   newm[] <- na_val
#     #   newm <- methods::as(newm, class(m))
#     # }
#     newm <- new_empty_matrix(na_val,
#                              nrow = length(joined_names),
#                              ncol = nc,
#                              rownms = joined_names,
#                              colnms = compl_names,
#                              is_Matrix)
#     newm[pos, ] <- if (is.null(joined_names_unique)) {
#       m
#     } else {
#       m[unlist(lapply(joined_names, function(.x) which(.x == old_names))), ]
#     }
#     if (!is.null(joined_names_unique)) rownames(newm) <- joined_names_unique
#
#
#   } else {
#
#     # newm <- MATRIX(na_val, nrow = nr, ncol = length(joined_names), is_Matrix)
#     # rownames(newm) <- compl_names
#     # colnames(newm) <- joined_names
#     # if (is_Matrix) {
#     #   newm[] <- na_val
#     #   newm <- methods::as(newm, class(m))
#     # }
#     newm <- new_empty_matrix(na_val,
#                              nrow = nr,
#                              ncol = length(joined_names),
#                              rownms = compl_names,
#                              colnms = joined_names,
#                              is_Matrix)
#     newm[, pos] <- if (is.null(joined_names_unique)) {
#       m
#     } else {
#       m[, unlist(lapply(joined_names, function(.x) which(.x == old_names)))]
#     }
#     if (!is.null(joined_names_unique)) colnames(newm) <- joined_names_unique
#
#
#   }
#
#   newm
#
# }



# fill_from_y <- function(m, Y, margin, new_names, compl_names, all_names)
# {
#   if (is.null(m)) return(NULL)
#   if (is.null(Y)) return(m)
#
#   pos <- match(new_names, all_names)
#
#   if (margin == "row") {
#
#     x_col <- match(colnames(Y), compl_names, 0)
#     y_col <- match(compl_names, colnames(Y), 0)
#     if (any(x_col > 0) && any(y_col > 0)) {
#       y <- Y[new_names, y_col, drop = FALSE]
#       m[pos, x_col] <- y
#     }
#
#
#   } else {
#
#     x_row <- match(rownames(Y), compl_names, 0)
#     y_row <- match(compl_names, rownames(Y), 0)
#     if (any(x_row > 0) && any(y_row > 0)) {
#       y <- Y[y_row, new_names, drop = FALSE]
#       m[x_row, pos] <- y
#     }
#
#   }
#
#   m
#
# }




# sub_matrix <- function(m, margin, old_names, all_names, compl_names)
# {
#   if (is.null(m)) return(NULL)
#   d <- dim(m)
#   if ((d[1] == 0L && margin == "row") || (d[2] == 0L && margin == "col")) return(m)
#
#   pos <- match(all_names, old_names, 0)
#   if (margin == "row") m[pos, , drop = FALSE] else m[, pos, drop = FALSE]
#
# }








#' MSJoiner: A class o perform join operations
#'
#' An internal class used to perform join operations between a `matrixset`
#' object and an external data frame — which may also be a metadata data frame
#' from another `matrixset` — targeting either row or column metadata.
#'
#' This class encapsulates the logic required to:
#'
#' - Validate inputs and join keys.
#' - Handle suffixes and column name conflicts.
#' - Perform `left_join`, `right_join`, `inner_join`, and `full_join` operations.
#' - Update the `matrixset` object with the joined metadata.
#'
#' @docType class
#' @noRd
#' @name MatrixAdjuster
MSJoiner <- R6::R6Class(
  "MSJoiner",

  public = list(




    #' @description
    #' Initializes an `MSJoiner` object to join metadata from a data frame or
    #' another `matrixset` onto a `matrixset`, targeting either row or column
    #' metadata.
    #'
    #' It validates inputs, resolves join keys, manages conflicts, and supports
    #' optional  expansion of metadata and matrix dimensions.
    #'
    #' @param x               A `matrixset` object. The primary object to which
    #'                        metadata will be joined.
    #' @param y               A `data.frame` or another `matrixset`. The source
    #'                        of metadata.
    #' @param margin          `character`; either `"row"` or `"col"`, indicating
    #'                        the dimension to join on.
    #' @param by              `character` vector of key column(s) for the join.
    #'                        If `NULL`, defaults depend on `y`:
    #'                        - If `matrixset`: uses the meta tag column
    #'                          (e.g., `.rowname` or `.colname`).
    #'                        - If `data.frame`: uses a natural join as in
    #'                          [dplyr::join()]. Cross-joins are not
    #'                          supported.
    #' @param adjust          `logical` or `character`. If `FALSE` (default),
    #'                        prevents changes to metadata dimensions. If
    #'                        `TRUE`, allows expanding metadata (and matrices)
    #'                        by padding with `NA` (or with `0` for S4-class
    #'                        `Matrix` objects).
    #'
    #'                        As a string, must be one of `r flatten_or(adjust_opts)`:
    #'                        - "`r adjust_opts["x_only"]`": equivalent to `TRUE`.
    #'                        - "`r adjust_opts["from_y"]`": uses values from
    #'                          `y` if it is a `matrixset` for padding, but only
    #'                          for overlapping names and traits.
    #' @param name_template   Controls renaming of conflicting tag names.
    #'                        - `NULL` (default): disallows non-unique tag
    #'                          names.
    #'                        - `TRUE`: ensures uniqueness by appending numeric
    #'                          suffixes.
    #'                        - A string: a [glue](https://glue.tidyverse.org)
    #'                          template using variables from `y`. The special
    #'                          variable `.tag` refers to the original tag name.
    #'                          Currently, only curly-brace (`{}`) syntax is
    #'                          supported.
    #'
    #'                        Only non-unique names are modified. Requires
    #'                        `adjust = TRUE`.
    #'
    #' @noRd
    initialize = function(x, y, margin, by, adjust, name_template) {

      private$._x <- x
      private$._y <- y
      private$._y_is_ms <- is_matrixset(y)

      private$._margin = margin
      private$._by = by

      private$._set_adjust(adjust)
      private$._set_name_template(name_template)
      private$._set_meta()

    },






    #' @description
    #' Performs the merge while ensuring that `matrixset` conventions are respected.
    #' Also issues a warning if any trait classes change as a result of the merge.
    #'
    #' @param type          A character string specifying the type of join.
    #'                      One of `"left"`, `"right"`, `"full"`, `"semi"`, or
    #'                      `"anti"`.
    #' @param suffix        A character vector of length 2. Suffixes added to
    #'                      duplicated non-join traits from `x` and `y` to
    #'                      disambiguate them.
    #' @param na_matches    Specifies how `NA` values are matched:
    #'                      * `"na"` (default): treats two `NA` or two `NaN`
    #'                        values as equal, similar to `%in%`, `match()`,
    #'                        and `merge()`.
    #'                      * `"never"`: treats `NA` or `NaN` values as
    #'                        distinct, meaning they will not match each other
    #'                        or any other values. This mimics joins from
    #'                        database systems or `base::merge(incomparables = NA)`.
    #'
    #' @noRd
    join = function(type, suffix = c(".x", ".y"), na_matches = "never") {

      private$._join(type, suffix, na_matches)
      private$._assess_and_assign_meta(suffix[1])
      private$._warn_if_class_change()
      private$._set_matrixset()
      private$._set_group_structure()

    }


  ),


  active = list(

    #' @field matrix_set       A list of updated matrices after the merge.
    #' @field info             The updated metadata data frame for the margin
    #'                         used in the merge.
    #' @field n_row            Number of rows in the `matrixset` after merging.
    #' @field n_col            Number of columns in the `matrixset` after
    #'                         merging.
    #' @field traits           Updated row or column traits after the merge.
    #' @field margin_names     New row or column names for the merged margin.
    #' @field new_tag          The updated tag name for the margin used in the
    #'                         merge.
    #' @field new_class        Updated S3 class (when merging affects group
    #'                         structure).
    #' @field new_group_attrs  Updated row or column group attributes after
    #'                         merging.

    matrix_set = function() private$new_matrix_set_,
    info = function() private$new_info_,
    n_row = function() private$n_row_,
    n_col = function() private$n_col_,
    traits = function() private$new_traits_,
    margin_names = function() private$margin_names_,
    new_tag = function() private$x_tag_,
    new_class = function() private$new_class_,
    new_group_attrs = function() private$new_group_attrs_
  ),



  private = list(

    new_info_ = NULL,                 # The updated metadata data frame for the
                                      # margin used in the merge.
    new_matrix_set_ = NULL,           # A list of updated matrices after the
                                      # merge.
    n_row_ = NULL,                    # Number of rows in the `matrixset` after
                                      # merging.
    n_col_ = NULL,                    # Number of columns in the `matrixset`
                                      # after merging.
    new_traits_ = NULL,               #
    ._n_margin = NULL,                #
    margin_names_ = NULL,             #
    ._margin_names_unique = NULL,     #
    ._margin_names_new = NULL,        #
    new_class_ = NULL,                #
    new_group_attrs_ = NULL,          #

    ._x = NULL,                       #
    ._y = NULL,                       #
    ._y_is_ms = logical(),            #

    ._x_traits = NULL,                #
    ._y_traits = NULL,                #
    x_tag_ = NULL,                    #
    ._y_tag = NULL,                   #

    ._margin = NULL,                  #

    ._by = NULL,                      #

    ._adjust = NULL,                  #
    ._adjust_method = NULL,           #

    ._name_template = NULL,           #
    ._duplication_accepted = NULL,    #
    ._unique_names_post = TRUE,       #







    #' Private Method
    #'
    #' Determines whether dimension adjustment is allowed and, if so, how it
    #' should be handled. Valid options are listed in `adjust_opts`.
    #'
    #' The actual adjustment is performed later in the process.
    #'
    #' @param adj   A `logical` or `character` value. If a string, it must be
    #'              one of `r flatten_or(adjust_opts)`.
    #'
    #' @returns
    #' Invisibly returns `NULL`. This function is called for its side effects.
    ._set_adjust = function(adj)
    {
      adj_type <- typeof(adj)
      adjust <- switch (adj_type,
                        "logical" = adj,
                        "character" = TRUE,
                        stop("adjust parameter must be a logical or a character vector of length 1.")
      )

      if (length(adj) > 1) {
        warning("'adjust' is of length > 1. Keeping the first element only.")
        adjust <- adjust[1]
      }

      adjust_how <- if (is.character(adj)) {
        match_option(adj, adjust_opts)
      } else adjust_opts["x_only"]

      if (adjust && adjust_how != adjust_opts["x_only"] && !private$._y_is_ms) {
        adjust_how <- adjust_opts["x_only"]
        warning(stringr::str_glue("'adjust' has been forced to '{ADJ}' because 'y' is a data.frame",
                                  ADJ = adjust_opts["x_only"]))
      }

      private$._adjust <- adjust
      private$._adjust_method <- adjust_how

    },





    #' Private Method
    #'
    #' Sets the glue name template, unless `tplt` is `NULL` or `FALSE`, in which
    #' case no action is taken.
    #'
    #' If `tplt` is a [glue](https://glue.tidyverse.org) template, the special
    #' variable `.tag` refers to the original tag name. If `.tag` is not present
    #' in the template, it is automatically added as a prefix.
    #'
    #' Currently, only the curly-brace (`{}`) syntax is supported.
    #'
    #' @param tplt   `NULL`, `logical`, or `character` (a
    #'               [glue](https://glue.tidyverse.org) template).
    #'
    #' @returns
    #' Invisibly returns `NULL`. This function is used for its side effects.
    ._set_name_template = function(tplt) {

      if (is.null(tplt) || (is.logical(tplt) && isFALSE(tplt)))
        return(invisible())

      if (is.logical(tplt)) {
        private$._name_template <- "{.tag}"
        return(invisible())
      }

      has_tag <- stringr::str_detect(tplt, "\\{\\.tag\\}")
      if (!has_tag) tplt <- paste("{.tag}", tplt, sep = "_")

      private$._name_template <- tplt

    },





    ._set_traits = function() {

      if (private$._margin == "row") {

        private$._x_traits <- c(.rowtag(private$._x), .rowtraits(private$._x))
        private$._y_traits <- if (private$._y_is_ms) {
          c(.rowtag(private$._y), .rowtraits(private$._y))
        } else {
          colnames(private$._y)
        }

        return()
      }



      private$._x_traits <- c(.coltag(private$._x), .coltraits(private$._x))
      private$._y_traits <- if (private$._y_is_ms) {
        c(.coltag(private$._y), .coltraits(private$._y))
      } else {
        colnames(private$._y)
      }

    },




    ._set_tags = function() {

      if (private$._margin == "row") {

        private$x_tag_ <- .rowtag(private$._x)
        if (private$._y_is_ms) private$._y_tag <- .rowtag(private$._y)

        return()
      }


      private$x_tag_ <- .coltag(private$._x)
      if (private$._y_is_ms) private$._y_tag <- .coltag(private$._y)

    },




    ._assess_by_list_validity = function() {

      if (is.list(private$._by) && !all(sapply(private$._by, is.character)))
        stop("`by` must be a list of character when a list")

      if (is.list(private$._by) && !all(sapply(private$._by, function(x) length(x) == 1L)))
        stop("elements of `by` must be of length 1 when it is a list")

    },





    # assess_by_vars_margin <- function(vars, nms)
    # {
    #   dup <- duplicated(vars)
    #   if (any(dup)) {
    #     msg <- "Join variables must be unique."
    #     stop(msg)
    #   }
    #
    #   if (!all(is_in <- vars %in% nms)) {
    #     msg <- if (sum(!is_in) > 1) "variables" else "variable"
    #     vars_quoted <- encodeString(vars[!is_in], quote = "\"")
    #     msg <- paste(msg, paste(vars_quoted, collapse = ", "), "is not a known trait")
    #     stop(msg)
    #   }
    #   NULL
    # }


    # ._assess_by_vars = function(by, margin) {
    #
    #   dup <- duplicated(by)
    #
    #   if (any(dup)) {
    #     msg <- "Join variables must be unique."
    #     stop(msg)
    #   }
    #
    #   nms <- if (margin == "row") private$._x_traits else private$._y_traits
    #
    #   if (!all(is_in <- by %in% nms)) {
    #
    #     msg_tplt <- "variable{S_MARK} {VAR_TXT} {IS_OR_ARE} not {MAYBE_A} known trait{S_MARK}"
    #     vars_quoted <- encodeString(by[!is_in], quote = "\"")
    #     msg <- stringr::str_glue(msg_tplt,
    #                              S_MARK = if (sum(!is_in) > 1) "s" else "",
    #                              VAR_TXT = paste(vars_quoted, collapse = ", "),
    #                              IS_OR_ARE = if (sum(!is_in) > 1) "are" else "is",
    #                              MAYBE_A = if (sum(!is_in) > 1) "" else "a")
    #
    #     stop(msg)
    #   }
    #
    #
    # },
    ._assess_by_vars = function(by, obj) {

      dup <- duplicated(by)

      if (any(dup)) {
        msg <- "Join variables must be unique."
        stop(msg)
      }

      nms <- if (obj == "x") private$._x_traits else private$._y_traits

      if (!all(is_in <- by %in% nms)) {

        msg_tplt <- "variable{S_MARK} {VAR_TXT} {IS_OR_ARE} not {MAYBE_A} known trait{S_MARK}"
        vars_quoted <- encodeString(by[!is_in], quote = "\"")
        msg <- stringr::str_glue(msg_tplt,
                                 S_MARK = if (sum(!is_in) > 1) "s" else "",
                                 VAR_TXT = paste(vars_quoted, collapse = ", "),
                                 IS_OR_ARE = if (sum(!is_in) > 1) "are" else "is",
                                 MAYBE_A = if (sum(!is_in) > 1) "" else "a")

        stop(msg)
      }


    },




    ._set_by_param = function() {

      if (is.null(private$._by)) {

        by <- NULL

        if (private$x_tag_ %in% private$._x_traits &&
            !is.null(private$._y_tag) && private$._y_tag %in% private$._y_traits) {

          by <- private$._y_tag
          names(by) <- private$x_tag_

        } else {

          by <- intersect(private$._x_traits, private$._y_traits)
          if (length(by) == 0) {
            msg <- "`by` must be supplied when `x` and `y` have no common variables."
            stop(msg)
          }

        }

        private$._by <- by

      }



      if (is.character(private$._by) || is.list(private$._by)) {

        private$._assess_by_list_validity()

        by_nms <- names(private$._by)
        by <- unname(private$._by)

        # if (is.list(by) && !all(sapply(by, is.character)))
        #   stop("`by` must be a list of character when a list")
        #
        # if (is.list(by) && !all(sapply(by, function(x) length(x) == 1L)))
        #   stop("elements of `by` must be of length 1 when it is a list")

        by <- unlist(by)

        # by_x <- rlang::`%||%`(by_nms, by)
        by_x <- by_nms %OR% by
        by_y <- by

      } else {
        msg <- "`by` must be a (named) character vector, list, or NULL"
        stop(msg)
      }

      # private$._assess_by_vars(by_x, "row")
      # private$._assess_by_vars(by_y, "col")
      private$._assess_by_vars(by_x, "x")
      private$._assess_by_vars(by_y, "y")


      by <- by_y
      names(by) <- by_x

      private$._by <- by
    },




    ._set_meta = function() {

      private$._duplication_accepted <- !is.null(private$._name_template)
      private$._set_traits()
      private$._set_tags()
      private$._set_by_param()
    },





    # ._warn_if_class_change = function() {
    #
    #   info_id <- if (private$._margin == "row") "row_info" else "column_info"
    #
    #   var_class_orig <- lapply(.subset2(private$._x, info_id), data.class)
    #   var_class <- lapply(private$new_info_, data.class)
    #
    #   ## THIS DOESN'T WORK AS IT DOESN'T ACCOUNT FOR POSSIBLE TRAIT LABEL CHANGE!!!
    #   var_class_orig_kept <- var_class_orig[names(var_class_orig) %in% names(var_class)]
    #   var_class_still <- var_class[names(var_class) %in% names(var_class_orig)]
    #
    #   if (length(var_class_orig_kept) && length(var_class_still) &&
    #       !identical(var_class_orig_kept, var_class_still)) {
    #
    #     idx <- purrr::map2_lgl(var_class_orig_kept, var_class_still,
    #                            function(x, y) !identical(x, y))
    #     chg_vars <- names(idx[idx])
    #
    #     warning(paste0("some traits have changed type (",
    #                    stringr::str_flatten(sQuote(chg_vars), collapse = ", "),
    #                    ")"),
    #             call. = FALSE)
    #   }
    #
    # },
    ._warn_if_class_change = function() {

      info_id <- if (private$._margin == "row") "row_info" else "column_info"

      var_class_orig <- lapply(.subset2(private$._x, info_id), data.class)
      var_class <- lapply(private$new_info_, data.class)

      var_class_orig_kept <- var_class_orig[private$._x_traits %in% private$new_traits_]
      var_class_still <- var_class[private$new_traits_ %in% private$._x_traits]

      if (length(var_class_orig_kept) && length(var_class_still) &&
          !identical(var_class_orig_kept, var_class_still)) {

        idx <- purrr::map2_lgl(var_class_orig_kept, var_class_still,
                               function(x, y) !identical(x, y))
        chg_vars <- names(idx[idx])

        warning(paste0("some traits have changed type (",
                       stringr::str_flatten(sQuote(chg_vars), collapse = ", "),
                       ")"),
                call. = FALSE)
      }

    },



    ._join = function(type, suffix, na_matches) {

      join_fn <- getFromNamespace(paste0(type, "_join"), "dplyr")

      info_id <- if (private$._margin == "row") "row_info" else "column_info"
      y_info_sym <- if (private$._y_is_ms) {
        substitute(.subset2(private$._y, what), list(what = info_id))
      } else quote(private$._y)

      args <- list(as.name("join_fn"),
                   substitute(.subset2(private$._x, what), list(what = info_id)),
                   y_info_sym,
                   by = quote(private$._by),
                   na_matches = as.name('na_matches'))
      if (!(type %in% filt_join_opts)) args <- c(args, suffix = as.name("suffix"))

      join_call <- as.call(args)

      private$new_info_ <- eval(join_call)

    },




    ._assess_and_assign_meta = function(suffix) {


      private$._n_margin <- nrow(private$new_info_)
      private$new_traits_ <- colnames(private$new_info_)

      private$._handle_trait_name_change(suffix)
      private$._assess_names_unique()


      if (private$._margin == "row") {
        mrg_names <- rownames(private$._x)
        mrg_names_comp <- colnames(private$._x)
      } else {
        mrg_names <- colnames(private$._x)
        mrg_names_comp <- rownames(private$._x)
      }
      mrg_names_new <- private$new_info_[[private$x_tag_]]



      private$margin_names_ <- mrg_names_new

      if (private$._unique_names_post) {

        private$._margin_names_unique <- mrg_names_new
        new_names_unique <- NULL

      } else {

        if (!private$._duplication_accepted)
          stop(
            paste("'by' does not result in unique", private$._margin, "names")
          )


        private$._margin_names_unique <- private$._differentiate_names(mrg_names_new,
                                                                       private$._n_margin)
        new_names_unique <- setdiff(private$._margin_names_unique , mrg_names)

        warning(
          paste0(private$._margin, " names (", private$x_tag_,
                 ") have changed following matrix adjustment"),
          call. = FALSE)


      }


      private$._margin_names_new <- setdiff(mrg_names_new, mrg_names)
      lost_names <- setdiff(mrg_names, mrg_names_new)

      has_new_names <- length(private$._margin_names_new) > 0
      has_new_names_unique <- length(new_names_unique) > 0
      has_lost_names <- length(lost_names) > 0



      if (has_new_names || has_new_names_unique || has_lost_names)
      {
        if (!private$._adjust) {

          tag <- if (private$._margin == "col") "columns" else "rows"
          msg <- paste("the number of", tag,
                       "is modified by the join operation, which is against",
                       "the 'matrixset' paradigm. Use 'adjust' to still",
                       "perform the operation.")
          stop(msg)

        }

      }


        # joined_names <- info[[x_tag]]           <--- OK   = mrg_names_new
        # nms <- margin_names(.ms_x, margin)      <--- OK   = irrelevant
        # x_names <- nms$nms                      <--- OK   = mrg_names
        # compl_names <- nms$compl                <--- OK   = mrg_names_comp
        #
        #  OK
        #  |
        #  |
        #  V
        # joined_names_unique <- if (not_unique) {     <------ margin_names_unique
        #   warning(paste0(margin, " names (", x_tag, ") have changed following matrix adjustment"), call. = FALSE)
        #   unique_names(names_glue, x_tag, info)
        # } else  NULL
        #
        #
        # new_names <- setdiff(joined_names, x_names)
        # new_names_unique <- setdiff(joined_names_unique, x_names)
        # lost_names <- setdiff(x_names, joined_names)
        #
        # has_new_names <- length(new_names) > 0
        # has_new_names_unique <- length(new_names_unique) > 0
        # has_lost_names <- length(lost_names) > 0
        #
        #
        # if (has_new_names || has_new_names_unique || has_lost_names)
      #   {
      #     if (adjust) {
      #       nr <- if (margin == "row") ni else nrow(.ms_x)
      #       nc <- if (margin == "row") ncol(.ms_x) else ni
      #       mats <- .ms_x$matrix_set
      #
      #       if (has_new_names || has_new_names_unique) {
      #
      #         if (not_unique) {
      #           info[[x_tag]] <- joined_names_unique
      #         }
      #
      #         if (adjust_from_y) Y <- .ms_y$matrix_set
      #         mats_nms <- names(mats)
      #         mats <- lapply(mats_nms,
      #                        function(m_nm) {
      #                          m <- mats[[m_nm]]
      #                          newm <- fill_matrix(m, margin, nr, nc, x_names,
      #                                              joined_names, compl_names,
      #                                              joined_names_unique)
      #                          if (adjust_from_y) {
      #                            if (m_nm %in% names(Y)) {
      #                              y <- Y[[m_nm]]
      #                              newm <- fill_from_y(newm, y, margin, new_names,
      #                                                  compl_names, joined_names)
      #                            }
      #                          }
      #                          newm
      #                        })
      #         names(mats) <- mats_nms
      #
      #       }
      #
      #
      #       if (has_lost_names) {
      #         mats <- lapply(mats,
      #                        function(m) sub_matrix(m, margin, x_names, joined_names,
      #                                               compl_names))
      #       }
      #
      #
      #       .ms_x$matrix_set <- mats
      #       if (margin == "row") {
      #         attr(.ms_x, "n_row") <- ni
      #         attr(.ms_x, "row_names") <- if (is.null(joined_names_unique)) {
      #           joined_names
      #         } else joined_names_unique
      #       } else {
      #         attr(.ms_x, "n_col") <- ni
      #         attr(.ms_x, "col_names") <- if (is.null(joined_names_unique)) {
      #           joined_names
      #         } else joined_names_unique
      #       }
      #
      #
      #     } else {
      #       tag <- if (margin == "col") "columns" else "rows"
      #       stop(paste("the number of", tag, "is modified by the join operation, which is against the 'matrixset' paradigm. Use 'adjust' to still perform the operation."))
      #     }
      #
      #   }


      # tag <- if (private$._margin == "row") "rows" else "columns"

      # if (cond) {
      #   msd <- paste("the number of", tag, "is modified by the join operation,",
      #                "which is against the 'matrixset' paradigm. Use 'adjust'",
      #                "to still perform the operation.")
      #   stop(msg)
      # }


    },




    ._handle_trait_name_change = function(suffix) {

      tr <- private$new_traits_
      # tr <- colnames(private$new_info_ )

      if (any(notin <- !(private$._x_traits %in% tr))) {
        notin <- private$._x_traits[notin]
        chg_to <- paste0(notin, suffix)
        if (any(gone <- !(chg_to %in% tr))) {
          gone_away <- notin[gone]
          chg_to <- chg_to[!gone]
        }

        # somehow the column with the names had a name change
        if (private$x_tag_ %in% notin) {
          if (private$x_tag_ %in% gone_away)
            stop("The column holding the margin names has disapeared")

          private$x_tag_ <- chg_to[notin == private$x_tag_]
        }

        chg_to_msg <- paste(paste("", paste(shQuote(notin[!gone]), shQuote(chg_to),
                                            sep = " -> ")), collapse = "\n")

        warning(paste0("some traits have changed name:\n", chg_to_msg),
                call. = FALSE)
        if (any(gone)) {
          warning(paste0("some traits have disappeared: ",
                         stringr::str_flatten_comma(sQuote(gone_away)),
                         call. = FALSE))
        }
      }

    },




    ._assess_names_unique = function() {


      # n_mrg <- nrow(private$._target_info$info)
      # n_mrg <- nrow(private$new_info_)
      n_mrg <- private$._n_margin
      mrg_names <- private$new_info_[[private$x_tag_]]

      not_unique <- FALSE
      if (n_mrg > 0) {

        n_tag <- table(mrg_names)
        n_tag <- unique(n_tag)

        if (length(n_tag) > 1 || n_tag > 1) {
          not_unique <- TRUE
        }
      }

      private$._unique_names_post <- !not_unique

      # mrg_nm_id <- paste(private$._target_info$margin, "names_", sep = "_")
      # mrg_nm_unq_id <- paste(private$._target_info$margin, "names_unique_", sep = "_")
      #
      # private[[mrg_nm_unq_id]] <- if (not_unique) {
      #   private$._differentiate_names(mrg_names, n_mrg)
      # } else mrg_names
      #
      # private[[mrg_nm_id]] <- mrg_names


    },





    # ._assess_names_uniqueness = function() {
    #
    #
    #   # n_mrg <- nrow(private$._target_info$info)
    #   # n_mrg <- nrow(private$new_info_)
    #   n_mrg <- private$._n_margin
    #   mrg_names <- private$new_info_[[private$x_tag_]]
    #
    #   not_unique <- FALSE
    #   if (n_mrg > 0) {
    #
    #     n_tag <- table(mrg_names)
    #     n_tag <- unique(n_tag)
    #
    #     if (length(n_tag) > 1 || n_tag > 1) {
    #       if (!private$._duplication_accepted)
    #         stop(
    #           paste("'by' does not result in unique", private$._margin, "names")
    #         )
    #
    #       warning(
    #         paste0(private$._margin, " names (", private$x_tag_,
    #                ") have changed following matrix adjustment"),
    #         call. = FALSE)
    #       not_unique <- TRUE
    #     }
    #   }
    #
    #   private$._unique_names_post <- !not_unique
    #
    #   # mrg_nm_id <- paste(private$._target_info$margin, "names_", sep = "_")
    #   # mrg_nm_unq_id <- paste(private$._target_info$margin, "names_unique_", sep = "_")
    #   #
    #   # private[[mrg_nm_unq_id]] <- if (not_unique) {
    #   #   private$._differentiate_names(mrg_names, n_mrg)
    #   # } else mrg_names
    #   #
    #   # private[[mrg_nm_id]] <- mrg_names
    #
    #
    # },





    ._differentiate_names = function(names, n_names) {

      if (private$._name_template == "{.tag}") return(make_unique(names))


      id_unq <- unique_id(names, n_names)

      working_df <- private$new_info_[id_unq > 0, ]
      # working_df <- list2env(working_df, parent = emptyenv())
      working_df[[".tag"]] <- working_df[[private$x_tag_]]
      new_tag <- stringr::str_glue_data(private$._name_template, .x=working_df)
      if (length(unique(new_tag)) != length(new_tag))
        stop(paste("'by' does not result in unique", private$x_tag_))

      new_dat_tag <- names
      new_dat_tag[id_unq > 0] <- new_tag
      new_dat_tag

    },





    ._target_meta = function() {

      enclos = new.env(parent = emptyenv())

      # enclos$info <- private$new_info_
      enclos$size <- nrow(private$new_info_)
      enclos$margin <- private$._margin
      enclos$margin_comp <- if (private$._margin == "row") "col" else "row"
      # enclos$tag <- private$x_tag_
      # enclos$duplication_accepted <- !is.null(private$._name_template)
      # enclos$name_template <- private$._name_template


      # mrg_names <- private$new_info_[[private$x_tag_]]
      #
      # enclos$margin_names_unique <- if (private$._unique_names_post) {
      #   mrg_names
      # } else private$._differentiate_names(mrg_names, nrow(private$new_info_))
      #
      #
      # mrg_comp_names <- if (private$._margin == "row") {
      #   colnames(private$._x)
      # } else {
      #   rownames(private$._x)
      # }
      #
      # enclos$margin_names <- mrg_names
      # enclos$margin_comp_names <- mrg_comp_names
      # enclos$margin_comp_names_unique <- mrg_comp_names



      enclos$margin_names_unique <- private$._margin_names_unique

      mrg_comp_names <- if (private$._margin == "row") {
        colnames(private$._x)
      } else {
        rownames(private$._x)
      }

      enclos$margin_names <- private$margin_names_
      enclos$margin_comp_names <- mrg_comp_names
      enclos$margin_comp_names_unique <- mrg_comp_names

      enclos
    },







    ._set_matrixset = function() {

      if (private$._adjust) {

        matrix_meta <- MatrixMeta$new(.subset2(private$._x, "matrix_set"),
                                      adjust = TRUE,
                                      target_info = private$._target_meta())

        adj_from_y <- private$._adjust_method == adjust_opts["from_y"]


        private$n_row_ <- matrix_meta$n_row
        private$n_col_ <- matrix_meta$n_col

        private$margin_names_ <- private$._margin_names_unique
        # private$margin_names_ <-  private$new_info_[[private$x_tag_]]
        # private$margin_names_ <- if (private$._margin == "row") {
        #   matrix_meta$row_names_unique
        # } else {
        #   matrix_meta$col_names_unique
        # }
        # if (is.null(private$margin_names_)) private$margin_names_ <- character(0)
        private$new_info_[[private$x_tag_]] <- private$margin_names_


        private$new_matrix_set_ <- if (matrix_meta$need_adapt) {
          MatrixAdjuster$new(.subset2(private$._x, "matrix_set"),
                             matrix_meta,
                             expand = if (adj_from_y) {
                               .subset2(private$._y, "matrix_set")
                             } else NA
          )$adjusted_mats
        } else {
          .subset2(private$._x, "matrix_set")
        }


        return()
      }

      private$n_row_ <- nrow(private$._x)
      private$n_col_ <- ncol(private$._x)

      # private$margin_names_ <- if (private$._margin == "row") {
      #   rownames(private$._x)
      # } else {
      #   colnames(private$._x)
      # }

      private$new_matrix_set_ <- .subset2(private$._x, "matrix_set")


      # attributes(.ms_x) <- attrs
      # class(.ms_x) <- meta$class
      # if (is.null(meta$attrs$group_vars)) class(.ms_x) <- "matrixset"
      # if (!is.null(meta$attrs$group_vars)) {
      #
      # } else class(.ms_x) <- "matrixset"



    },




    ._set_group_structure = function() {

      info_id <- if (private$._margin == "row") "row_info" else "column_info"

      meta_orig <- get_group_info(.subset2(private$._x, info_id),
                                  class(private$._x), private$._margin)
      meta <- get_group_info(private$new_info_, class(private$._x),
                             private$._margin)
      # meta_comp <- get_group_info(.subset2(private$._x, info_comp_id),
      #                             class(private$._x), margin_comp)
      if (!identical(meta_orig$attrs$group_keys, meta$attrs$group_keys))
        warning("grouping structure of '.ms_x' has changed.", call. = FALSE)
      # attrs <- set_group_attrs(attributes(private$._x), meta$attrs,
      #                          private$._margin)

      new_class <- meta$class
      if (is.null(meta$attrs$group_vars)) {
        # new_class <- "matrixset"
        demoted_class <- if (private$._margin == "row") {
          "col_grouped_ms"
        }  else {
          "row_grouped_ms"
        }
        new_class <- if ("dual_grouped_ms" %in% meta_orig$class) {
          c(demoted_class, "matrixset")
        }  else "matrixset"
      }

      names(meta$attrs) <- paste(private$._margin, names(meta$attrs), sep = "_")
      # names(meta_comp$attrs) <- paste(margin_comp, names(meta_comp$attrs),
      #                                 sep = "_")

      private$new_class_ <- new_class
      private$new_group_attrs_ <- meta$attrs

    }




#BUILD NEW OBJECT!!!!!!


  )
)





.join_info <- function(type, margin, .ms_x, .ms_y, by = NULL,
                       suffix = c(".x", ".y"), na_matches = "never",
                       adjust = FALSE, names_glue = NULL)
{
  ms_joiner <- MSJoiner$new(x = .ms_x, y = .ms_y, margin = margin, by = by,
                            adjust = adjust, name_template = names_glue)
  ms_joiner$join(type = type)


  # PROBABLY NEED TO DISABLE SOME CHECKS IN CREATING MATRIXSET OBJECT SINCE SOME
  # HAVE BEEN DONE ALREADY

  if (margin == "row") {

    # print(
    #   as.call(c(list(as.name(".matrixset"),
    #          quote(ms_joiner$matrix_set),
    #          row_info = quote(ms_joiner$info),
    #          col_info = quote(column_info(.ms_x)),
    #          n_row = quote(ms_joiner$n_row),
    #          n_col = quote(ms_joiner$n_col),
    #          matrix_names = quote(names(ms_joiner$matrix_set)),
    #          n_matrix = quote(length(ms_joiner$matrix_set)),
    #          row_traits = quote(ms_joiner$traits),
    #          col_traits = quote(c(.coltag(.ms_x), .coltraits(.ms_x))),
    #          row_names = quote(ms_joiner$margin_names),
    #          col_names = quote(colnames(.ms_x)),
    #          row_tag = quote(ms_joiner$new_tag),
    #          col_tag = quote(.coltag(.ms_x))),
    #     ms_joiner$new_group_attrs
    #     ))
    #
    #
    #
    #   )

    meta_comp <- get_group_info(.subset2(.ms_x, "column_info"), class(.ms_x),
                                "col")
    comp_group_attrs <- meta_comp$attrs
    names(comp_group_attrs) <- paste("col", names(comp_group_attrs), sep = "_")

    return(

      # .matrixset(ms_joiner$matrix_set,
      #            row_info = ms_joiner$info,
      #            col_info = column_info(.ms_x),
      #            n_row = ms_joiner$n_row,
      #            n_col = ms_joiner$n_col,
      #            matrix_names = names(ms_joiner$matrix_set),
      #            n_matrix = length(ms_joiner$matrix_set),
      #            row_traits = ms_joiner$traits,
      #            col_traits = c(.coltag(.ms_x), .coltraits(.ms_x)),
      #            row_names = ms_joiner$margin_names,
      #            col_names = colnames(.ms_x),
      #            row_tag = ms_joiner$new_tag,
      #            col_tag = .coltag(.ms_x))
      eval(as.call(c(list(as.name(".matrixset"),
                          quote(ms_joiner$matrix_set),
                          row_info = quote(ms_joiner$info),
                          col_info = quote(column_info(.ms_x)),
                          n_row = quote(ms_joiner$n_row),
                          n_col = quote(ms_joiner$n_col),
                          matrix_names = quote(names(ms_joiner$matrix_set)),
                          n_matrix = quote(length(ms_joiner$matrix_set)),
                          row_traits = quote(ms_joiner$traits),
                          col_traits = quote(c(.coltag(.ms_x), .coltraits(.ms_x))),
                          row_names = quote(ms_joiner$margin_names),
                          col_names = quote(colnames(.ms_x)),
                          row_tag = quote(ms_joiner$new_tag),
                          col_tag = quote(.coltag(.ms_x))),
                     .class = quote(ms_joiner$new_class),
                     ms_joiner$new_group_attrs,
                     comp_group_attrs)
      )
      )

    )
  }

  # print(list(ms_joiner$matrix_set,
  #            row_info = row_info(.ms_x),
  #            col_info = ms_joiner$info,,
  #            n_row = ms_joiner$n_row,
  #            n_col = ms_joiner$n_col,
  #            matrix_names = names(ms_joiner$matrix_set),
  #            n_matrix = length(ms_joiner$matrix_set),
  #            row_traits = c(.rowtag(.ms_x), .rowtraits(.ms_x)),
  #            col_traits = ms_joiner$traits,
  #            row_names = rownames(private$.ms_x),
  #            col_names = ms_joiner$margin_names,
  #            row_tag = .rowtag(.ms_x),
  #            col_tag = ms_joiner$new_tag))

  .matrixset(ms_joiner$matrix_set,
             row_info = row_info(.ms_x),
             col_info = ms_joiner$info,
             n_row = ms_joiner$n_row,
             n_col = ms_joiner$n_col,
             matrix_names = names(ms_joiner$matrix_set),
             n_matrix = length(ms_joiner$matrix_set),
             row_traits = c(.rowtag(.ms_x), .rowtraits(.ms_x)),
             col_traits = ms_joiner$traits,
             row_names = rownames(.ms_x),
             col_names = ms_joiner$margin_names,
             row_tag = .rowtag(.ms_x),
             col_tag = ms_joiner$new_tag)

  # GROUPING!!!

  ## WARN WHEN MATRIX CHANGE S4 TYPE!!!
}








# .join_info <- function(type, margin, .ms_x, .ms_y, by = NULL,
#                        suffix = c(".x", ".y"), na_matches = "never",
#                        adjust = FALSE, names_glue = NULL)
# {
#   cl <- sys.call()
#   cash_status$set(cl)
#   on.exit(cash_status$clear(cl))
#
#   adjust_meta <- set_adjust(adjust, is_matrixset(.ms_y))
#   adjust <- adjust_meta$adjust
#   adjust_how <- adjust_meta$adjust_how
#   adjust_from_y <- adjust_how != adjust_opts["x_only"]
#
#   names_glue <- build_glue(names_glue)
#   accept_dupl <- !is.null(names_glue)
#
#   x_nms <- join_names(.ms_x, margin)
#   y_nms <- join_names(.ms_y, margin)
#   x_tag <- join_tag(.ms_x, margin)
#   y_tag <- join_tag(.ms_y, margin)
#
#   by <- set_by_null(by, x_nms, y_nms, x_tag, y_tag)
#   by <- set_by_vars(by, x_nms, y_nms)
#
#   info_x <- join_info(.ms_x, margin)
#   info_y <- join_info(.ms_y, margin)
#
#   meta_orig <- get_group_info(info_x, class(.ms_x), margin)
#   var_class_orig <- lapply(info_x, class)
#
#
#   args <- list("info_x", "info_y", by = as.name("by"),
#                na_matches = as.name('na_matches'))
#   if (!(type %in% filt_join_opts)) args <- c(args, suffix = as.name("suffix"))
#   join_call <- rlang::call2(paste0(type, "_join"), !!!rlang::syms(args),
#                             .ns = "dplyr")
#   info <- rlang::eval_tidy(join_call)
#
#   var_class <- lapply(info, class)
#   var_class_orig_kept <- var_class_orig[names(var_class_orig) %in% names(var_class)]
#   var_class_still <- var_class[names(var_class) %in% names(var_class_orig)]
#   if (length(var_class_orig_kept) && length(var_class_still) &&
#       !identical(var_class_orig_kept, var_class_still)) {
#     idx <- purrr::map2_lgl(var_class_orig_kept, var_class_still,
#                            function(x, y) !identical(x, y))
#     chg_vars <- names(idx[idx])
#     warning(paste0("some traits have changed type (",
#                    stringr::str_flatten(sQuote(chg_vars), collapse = ", "),
#                    ")"),
#             call. = FALSE)
#   }
#
#   ni <- nrow(info)
#
#   not_unique <- FALSE
#   if (ni > 0) {
#     # make sure the new key (row/col name has no duplicates)
#     ntag <- dplyr::count(info, !!as.name(x_tag))
#     ntag <- if(x_tag == "n") {
#       ntag[["nn"]]
#     } else {
#       ntag[["n"]]
#     }
#     ntag <- unique(ntag)
#
#     if (length(ntag) > 1 || ntag > 1) {
#       if (!accept_dupl) stop(paste("'by' does not result in unique", margin, "names"))
#       not_unique <- TRUE
#     }
#   }
#
#
#   joined_names <- info[[x_tag]]
#   nms <- margin_names(.ms_x, margin)
#   x_names <- nms$nms
#   compl_names <- nms$compl
#
#
#   joined_names_unique <- if (not_unique) {
#     warning(paste0(margin, " names (", x_tag, ") have changed following matrix adjustment"), call. = FALSE)
#     unique_names(names_glue, x_tag, info)
#   } else  NULL
#
#
#   new_names <- setdiff(joined_names, x_names)
#   new_names_unique <- setdiff(joined_names_unique, x_names)
#   lost_names <- setdiff(x_names, joined_names)
#
#   has_new_names <- length(new_names) > 0
#   has_new_names_unique <- length(new_names_unique) > 0
#   has_lost_names <- length(lost_names) > 0
#
#   if (has_new_names || has_new_names_unique || has_lost_names)
#   {
#     if (adjust) {
#       nr <- if (margin == "row") ni else nrow(.ms_x)
#       nc <- if (margin == "row") ncol(.ms_x) else ni
#       mats <- .ms_x$matrix_set
#
#       if (has_new_names || has_new_names_unique) {
#
#         if (not_unique) {
#           info[[x_tag]] <- joined_names_unique
#         }
#
#         if (adjust_from_y) Y <- .ms_y$matrix_set
#         mats_nms <- names(mats)
#         mats <- lapply(mats_nms,
#                        function(m_nm) {
#                          m <- mats[[m_nm]]
#                          newm <- fill_matrix(m, margin, nr, nc, x_names,
#                                              joined_names, compl_names,
#                                              joined_names_unique)
#                          if (adjust_from_y) {
#                            if (m_nm %in% names(Y)) {
#                              y <- Y[[m_nm]]
#                              newm <- fill_from_y(newm, y, margin, new_names,
#                                                  compl_names, joined_names)
#                            }
#                          }
#                          newm
#                        })
#         names(mats) <- mats_nms
#
#       }
#
#
#       if (has_lost_names) {
#         mats <- lapply(mats,
#                        function(m) sub_matrix(m, margin, x_names, joined_names,
#                                               compl_names))
#       }
#
#
#       .ms_x$matrix_set <- mats
#       if (margin == "row") {
#         attr(.ms_x, "n_row") <- ni
#         attr(.ms_x, "row_names") <- if (is.null(joined_names_unique)) {
#           joined_names
#         } else joined_names_unique
#       } else {
#         attr(.ms_x, "n_col") <- ni
#         attr(.ms_x, "col_names") <- if (is.null(joined_names_unique)) {
#           joined_names
#         } else joined_names_unique
#       }
#
#
#     } else {
#       tag <- if (margin == "col") "columns" else "rows"
#       stop(paste("the number of", tag, "is modified by the join operation, which is against the 'matrixset' paradigm. Use 'adjust' to still perform the operation."))
#     }
#
#   }
#
#
#
#   tr <- colnames(info)
#
#   if (any(notin <- !(x_nms %in% tr))) {
#     notin <- x_nms[notin]
#     chg_to <- paste0(notin, suffix[1])
#     if (any(gone <- !(chg_to %in% tr))) {
#       gone_away <- chg_to[gone]
#       chg_to <- chg_to[!gone]
#     }
#
#     chg_to_msg <- paste(paste("", paste(shQuote(notin[!gone]), shQuote(chg_to),
#                                         sep = " -> ")), collapse = "\n")
#
#     warning(paste0("some traits have changed name:\n", chg_to_msg),
#             call. = FALSE)
#     if (any(gone)) {
#       warning(paste0("some traits have disappeared: ",
#                      stringr::str_flatten_comma(sQuote(gone_away)),
#                      call. = FALSE))
#     }
#   }
#
#   if (margin == "row") {
#     .ms_x$row_info <- info
#     attr(.ms_x, "row_traits") <- tr
#   } else {
#     .ms_x$column_info <- info
#     attr(.ms_x, "col_traits") <- tr
#   }
#
#
#   meta <- get_group_info(info, class(.ms_x), margin)
#   if (!identical(meta_orig$attrs$group_keys, meta$attrs$group_keys))
#     warning("grouping structure of '.ms_x' has changed.", call. = FALSE)
#   attrs <- set_group_attrs(attributes(.ms_x), meta$attrs, margin)
#   attributes(.ms_x) <- attrs
#   class(.ms_x) <- meta$class
#   if (is.null(meta$attrs$group_vars)) class(.ms_x) <- "matrixset"
#   # if (!is.null(meta$attrs$group_vars)) {
#   #
#   # } else class(.ms_x) <- "matrixset"
#
#
#   .ms_x
#
# }


# Add meta info from another `matrixset` or a `data.frame`
#
# @description
# The operation is done through a join operation between the row meta info
# data.frame ([join_row_info()]) of `.ms` and `y` (or its row meta info
# data.frame if it is a `matrixset` object). The function [join_column_info()]
# does the equivalent operation for column meta info.
#
# The default join operation is a `r join_opts["default"]` join
# (type == `r sQuote(join_opts["default"])`), but most of dplyr's
# joins are available (`r flatten_or(join_opts)`).
#
# The `matrixset` paradigm of unique row/column names is enforced so if a
# `.ms` data.frame row matches multiple ones in `y`, this results in an
# error.
#
# @param .ms           A `matrixset` object
# @param y             A `matrixset` object or a `data.frame`.
# @param type          Joining type, one of `r sQuote(join_opts["default"])`,
#                      `r flatten_or(join_opts[join_opts != join_opts["default"]])`.
# @param by            The names of the variable to join by.
#                      The default, `NULL`, results in slightly different
#                      behavior depending if `y` is a `matrixset` or a
#                      `data.frame`.
#                      If a `matrixset`, the meta info tag of each object (the
#                      tag is the column that holds the row names/column names
#                      in the meta info data frame - typically ".rowname" or
#                      ".colname" unless specified otherwise at `matrixset`
#                      creation) is used for `by`.
#                      If a `data.frame`, a natural join is used. For more
#                      details, see `dplyr`'s [dplyr::join()].
#                      Note that the cross-join is not available.
# @param adjust        A logical. By default (`FALSE`), the join operation is
#                      not permitted to filter or augment the number of rows of
#                      the meta info data frame.
#                      If `TRUE`, this will be allowed. In the case where the
#                      data frame is augmented, the matrices of `.ms`
#                      will be augmented accordingly by padding with `NA`s (
#                      except for the `NULL` matrices).
#
#    Alternatively, `adjust` can be a single string, one of
#    `r flatten_or(adjust_opts)`. Choosing "`r adjust_opts["x_only"]`"
#    is equivalent to `TRUE`. When choosing "`r adjust_opts["from_y"]`",
#    padding is done using values from `y`, but only
#
#    1. if `y` is a `matrixset`
#    2. for `y` matrices that are named the same in `x`
#    3. If padding rows, only columns common between `x` and `y` will use `y`
#      values. The same logic is applied when padding columns.
#
#    Other values are padded with `NA`.
# @param suffix        Suffixes added to disambiguate trait variables. See
#                      `dplyr`'s [dplyr::join()].
# @param na_matches    How to handle missing values when matching. See
#                      `dplyr`'s [dplyr::join()].
#
# @section Groups:
# When `y` is a `matrixset`, only groups from `.ms` are used, if any. Group
# update is the same as in `dplyr`.
#
# @returns
# A `matrixset` with updated row or column meta info, with all `.ms` traits and
# `y` traits. If some traits share the same names - and were not included in
# `by` - `suffix`es will be appended to these names.
#
# If adjustment was allowed, the dimensions of the new `matrixset` may differ
# from the original one.
#
# @examples
# ms1 <- remove_row_annotation(student_results, class, teacher)
# ms <- join_row_info(ms1, student_results)
#
# ms <- join_row_info(ms1, student_results, by = c(".rowname", "previous_year_score"))
#
# # This will throw an error
# ms2 <- remove_row_annotation(filter_row(student_results, class %in% c("classA", "classC")),
#                              class, teacher, previous_year_score)
# ms <- ms <- tryCatch(join_row_info(ms2, student_results, type = "full"),
#                      error = function(e) e)
# is(ms, "error") # TRUE
# ms$message
#
# # Now it works.
# ms <- join_row_info(ms2, student_results, type = "full", adjust = TRUE)
# dim(ms2)
# dim(ms)
# matrix_elm(ms, 1)
#
# @name join

#' Add meta info from another `matrixset` or a `data.frame`
#'
#' @description
#' The operation is done through a join operation between the row meta info
#' data.frame ([join_row_info()]) of `.ms` and `y` (or its row meta info
#' data.frame if it is a `matrixset` object). The function [join_column_info()]
#' does the equivalent operation for column meta info.
#'
#' The default join operation is a `r join_opts["default"]` join
#' (type == `r sQuote(join_opts["default"])`), but most of dplyr's
#' joins are available (`r flatten_or(join_opts)`).
#'
#' The `matrixset` paradigm of unique row/column names is enforced so if a
#' `.ms` data.frame row matches multiple ones in `y`, the default behavior is
#' to issue a condition error.
#'
#' This can be modified by setting new tag names via the argument `names_glue`.
#'
#' @param .ms           A `matrixset` object
#' @param y             A `matrixset` object or a `data.frame`.
#' @param type          Joining type, one of `r sQuote(join_opts["default"])`,
#'                      `r flatten_or(join_opts[join_opts != join_opts["default"]])`.
#' @param by            The names of the variable to join by.
#'                      The default, `NULL`, results in slightly different
#'                      behavior depending if `y` is a `matrixset` or a
#'                      `data.frame`.
#'                      If a `matrixset`, the meta info tag of each object (the
#'                      tag is the column that holds the row names/column names
#'                      in the meta info data frame - typically ".rowname" or
#'                      ".colname" unless specified otherwise at `matrixset`
#'                      creation) is used for `by`.
#'                      If a `data.frame`, a natural join is used. For more
#'                      details, see `dplyr`'s [dplyr::join()].
#'                      Note that the cross-join is not available.
#' @param adjust        A logical. By default (`FALSE`), the join operation is
#'                      not permitted to filter or augment the number of rows of
#'                      the meta info data frame.
#'                      If `TRUE`, this will be allowed. In the case where the
#'                      data frame is augmented, the matrices of `.ms`
#'                      will be augmented accordingly by padding with `NA`s (
#'                      except for the `NULL` matrices).
#'
#'    Alternatively, `adjust` can be a single string, one of
#'    `r flatten_or(adjust_opts)`. Choosing "`r adjust_opts["x_only"]`"
#'    is equivalent to `TRUE`. When choosing "`r adjust_opts["from_y"]`",
#'    padding is done using values from `y`, but only
#'
#'    1. if `y` is a `matrixset`
#'    2. for `y` matrices that are named the same in `x`
#'    3. If padding rows, only columns common between `x` and `y` will use `y`
#'      values. The same logic is applied when padding columns.
#'
#'    Other values are padded with `NA`.
#' @param names_glue    a parameter that may allow multiple matches. By default,
#'                      (`NULL`), no multiple matches are allowed since the
#'                      resulting tag names will no longer be unique.
#'
#'    The value of `names_glue` can be `logical`, with the value `FALSE` being
#'    equivalent to `NULL`. If `TRUE`, then the resulting new tag names will be
#'    enforced to be unique by adding a number index, i.e. a number index will
#'    be glued to the tag names (hence the argument name).
#'
#'    Finally, `names_glue` can be a string, where you supply a glue
#'    specification that uses the variable names found in `y` (columns for data
#'    frames, traits for matrixsets) columns to create a custom new tag name. A
#'    special value `.tag` allows you to access the original tag name. Note that
#'    currently only the curly brackets ({}) can be used in the glue
#'    specification.
#'
#'    When making the unique tag names, *only* the non-unique names are modified.
#'    Also, `adjust = TRUE` must be enforced for `names_glue` to work.
#' @param suffix        Suffixes added to disambiguate trait variables. See
#'                      `dplyr`'s [dplyr::join()].
#' @param na_matches    How to handle missing values when matching. See
#'                      `dplyr`'s [dplyr::join()].
#'
#' @section Groups:
#' When `y` is a `matrixset`, only groups from `.ms` are used, if any. Group
#' update is the same as in `dplyr`.
#'
#' @returns
#' A `matrixset` with updated row or column meta info, with all `.ms` traits and
#' `y` traits. If some traits share the same names - and were not included in
#' `by` - `suffix`es will be appended to these names.
#'
#' If adjustment was allowed, the dimensions of the new `matrixset` may differ
#' from the original one.
#'
#' @examples
#' ms1 <- remove_row_annotation(student_results, class, teacher)
#' ms <- join_row_info(ms1, student_results)
#'
#' ms <- join_row_info(ms1, student_results, by = c(".rowname", "previous_year_score"))
#'
#' # This will throw an error
#' ms2 <- remove_row_annotation(filter_row(student_results, class %in% c("classA", "classC")),
#'                              class, teacher, previous_year_score)
#' ms <- tryCatch(join_row_info(ms2, student_results, type = "full"),
#'                error = function(e) e)
#' is(ms, "error") # TRUE
#' ms$message
#'
#' # Now it works.
#' ms <- join_row_info(ms2, student_results, type = "full", adjust = TRUE)
#' dim(ms2)
#' dim(ms)
#' matrix_elm(ms, 1)
#'
#' # Similarly, this will fail because tag names are no longer unique
#' meta <- tibble::tibble(sample = c("student 2", "student 2"),
#'                       msr = c("height", "weight"),
#'                       value = c(145, 32))
#' ms <- tryCatch(join_row_info(student_results, meta, by = c(".rowname"="sample")),
#'                error = function(e) e)
#' is(ms, "error") # TRUE
#' ms$message
#'
#' # This works, by forcing the tag names to be unique. Notice that we suppress
#' # the warning for now. We'll come back to it.
#' suppressWarnings(
#'    join_row_info(student_results, meta, by = c(".rowname"="sample"),
#'                  adjust = TRUE, names_glue = TRUE)
#' )
#' # Here's the warning: we're being told there was a change in tag names
#' (purrr::quietly(join_row_info)(student_results, meta,
#'                                by = c(".rowname"="sample"), adjust = TRUE,
#'                                names_glue = TRUE))$warnings
#'
#' # You can have better control on how the tag change occurs, for instance by
#' # appending the msr value to the name
#' suppressWarnings(
#'    join_row_info(student_results, meta, by = c(".rowname"="sample"),
#'                  adjust = TRUE, names_glue = "{.tag}_{msr}")
#' )
#' # In this specific example, the {.tag} was superfluous, since the default is
#' # to append after the tag name
#' suppressWarnings(
#'    join_row_info(student_results, meta, by = c(".rowname"="sample"),
#'                  adjust = TRUE, names_glue = "{msr}")
#' )
#' # But the keyword is useful if you want to shuffle order
#' suppressWarnings(
#'    join_row_info(student_results, meta, by = c(".rowname"="sample"),
#'                  adjust = TRUE, names_glue = "{msr}.{.tag}")
#' )
#'
#' # You are warned when there is a change in traits
#' meta <- tibble::tibble(sample = c("student 2", "student 2"),
#'                        class = c("classA", "classA"),
#'                        msr = c("height", "weight"),
#'                        value = c(145, 32))
#' (purrr::quietly(join_row_info)(student_results, meta,
#'                                by = c(".rowname"="sample"), adjust = TRUE,
#'                                names_glue = TRUE))$warnings[2]
#'
#' # Groups are automatically adjusted
#' sr_gr <- row_group_by(student_results, class)
#' gr_orig <- row_group_meta(row_group_by(student_results, class)) |> tidyr::unnest(.rows)
#' suppressWarnings(
#'   new_gr <- join_row_info(sr_gr, meta, by = c(".rowname" = "sample", "class"),
#'                           adjust = TRUE, names_glue = TRUE) |>
#'    row_group_meta() |> tidyr::unnest(.rows)
#' )
#' list(gr_orig, new_gr)
#'
#' # In the example above, the join operation changed the class of 'class',
#' # which in turn changed the grouping meta info. You are warned of both.
#' (purrr::quietly(join_row_info)(sr_gr, meta,
#'                                by = c(".rowname"="sample", "class"),
#'                                adjust = TRUE,  names_glue = TRUE))$warnings
#'
#' # A change in trait name that was used for grouping will result in losing the
#' # grouping. You are warning of the change in grouping structure.
#' (purrr::quietly(join_row_info)(sr_gr, meta,
#'                                by = c(".rowname"="sample"),
#'                                adjust = TRUE,  names_glue = TRUE))$warnings
#'
#' @name join



# # @rdname join
# # @export
# join_row_info <- function(.ms, y, type = "left", by = NULL, adjust = FALSE,
#                           suffix = c(".x", ".y"), na_matches = c("na", "never"))
#   UseMethod("join_row_info")
#
# # @export
# join_row_info.matrixset <- function(.ms, y, type = "left", by = NULL,
#                                     adjust = FALSE, suffix = c(".x", ".y"),
#                                     na_matches = c("na", "never"))
# {
#   na_matches <- match.arg(na_matches)
#   .join_info(type, "row", .ms, y, by = by, suffix = suffix,
#              na_matches = na_matches, adjust = adjust)
# }
#
#
#
# # @rdname join
# # @export
# join_column_info <- function(.ms, y, type = "left", by = NULL, adjust = FALSE,
#                              suffix = c(".x", ".y"), na_matches = c("na", "never"))
#   UseMethod("join_column_info")
#
# # @export
# join_column_info.matrixset <- function(.ms, y, type = "left", by = NULL,
#                                        adjust = FALSE, suffix = c(".x", ".y"),
#                                        na_matches = c("na", "never"))
# {
#   na_matches <- match.arg(na_matches)
#   .join_info(type, "col", .ms, y, by = by, suffix = suffix,
#              na_matches = na_matches, adjust = adjust)
# }


#' @rdname join
#' @export
join_row_info <- function(.ms, y, type = "left", by = NULL, adjust = FALSE,
                          names_glue = NULL, suffix = c(".x", ".y"),
                          na_matches = c("na", "never"))
  UseMethod("join_row_info")

#' @export
join_row_info.matrixset <- function(.ms, y, type = "left", by = NULL,
                                    adjust = FALSE, names_glue = NULL,
                                    suffix = c(".x", ".y"),
                                    na_matches = c("na", "never"))
{
  na_matches <- match.arg(na_matches)
  .join_info(type, "row", .ms, y, by = by, suffix = suffix,
             na_matches = na_matches, adjust = adjust, names_glue = names_glue)
}



#' @rdname join
#' @export
join_column_info <- function(.ms, y, type = "left", by = NULL, adjust = FALSE,
                             names_glue = NULL, suffix = c(".x", ".y"),
                             na_matches = c("na", "never"))
  UseMethod("join_column_info")

#' @export
join_column_info.matrixset <- function(.ms, y, type = "left", by = NULL,
                                       adjust = FALSE, names_glue = NULL,
                                       suffix = c(".x", ".y"),
                                       na_matches = c("na", "never"))
{
  na_matches <- match.arg(na_matches)
  .join_info(type, "col", .ms, y, by = by, suffix = suffix,
             na_matches = na_matches, adjust = adjust, names_glue = names_glue)
}



