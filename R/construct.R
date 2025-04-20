

normalize_expand <- function(expand, nms_matrix, nmatrix)
{
  if (is.null(expand)) return(expand)

  if (is.logical(expand)) {
    if (length(expand) > 1)
      warning("More than one logical value provided to 'expand'. Keeping the first one only")
    expand <- expand[1]

    expand <- if (is.na(expand)) {
      rep(list(NA), nmatrix)
    } else if (!expand) {
      NULL
    } else {
      expand
    }

    # expand <- if (is.na(expand) || expand) rep(list(NA), nmatrix) else NULL
  } else if (is.vector(expand)) {
    if (is.list(expand)) {
      expd_nms <- names(expand)
      if (!is.null(expd_nms)) {
        if (any(expd_nms == ""))
          stop("Empty expansion list names are not allowed")
        nms_idx <- match(nms_matrix, expd_nms, 0)
        nms_idx <- nms_idx[nms_idx > 0]
        if (length(nms_idx) == 0L)
          stop("No expansion list name matches the matrix names")
        expand <- expand[nms_idx]
        if (length(expand) == 1L && nmatrix != 1)
          stop("Number of expansion fill values is not compatible with the number of matrices")
      }

      if (length(expand) == 1L) expand <- rep(expand, nmatrix)
      if (length(expand) != nmatrix)
        stop("Number of expansion fill values is not compatible with the number of matrices")
    } else {
      if (length(expand) > 1)
        warning("More than one value provided to 'expand'. Keeping the first one only")
      expand <- expand[1]
      expand <- rep(list(expand), nmatrix)
    }
  }
  expand
}


# info_dim <- function(lst, side, expand)
# {
#   fn_names <- get(paste0(side, "names"), pos = "package:base", mode = "function")
#   fn_n <- get(paste0("n", side), pos = "package:base", mode = "function")
#   side_label <- if (side == "row") "row" else "column"
#
#   need_expand <- FALSE
#
#   # a note on empty dimnames: row/col names of empty dimnames is NULL; thus
#   # unique(NULL) is NULL and length(NULL) is 0. so length(unique(NULL)) and
#   # length(NULL) are both 0, thus equal. So empty dimnames is seen as not needing
#   # expansion, as it should
#
#   nms <- lapply(lst, fn_names)
#   .names <- unique(nms)
#   n_nms <- length(.names)
#   if (n_nms != 1) {
#     if (!expand)
#       stop(paste("All matrices must have the same", side_label, "names (NULL accepted)"))
#   }
#
#   lapply(.names,
#          function(.nms) {
#            .names_unq <- unique(.nms)
#
#            if (length(.names_unq) < length(.nms))
#              stop(paste(stringr::str_to_title(side_label), "names must be unique"))
#
#            if (any(.nms == ""))
#              stop(paste("Empty", side_label, "names are not allowed"))
#          })
#
#   .names_flat <- unlist(.names)
#   .names <- unique(.names_flat)
#   nexp <- length(.names)
#   if (nexp == 0L && expand)
#     stop("matrices must have dimnames for expansion")
#   if (n_nms > 1 && expand) need_expand <- TRUE
#
#   n_s <- vapply(lst, fn_n, 0)
#   n <- unique(n_s)
#   N <- length(n)
#   if (!expand && N != 1)
#     stop(paste0("All matrices must have the same number of ", side_label, "s"))
#
#   if (expand && need_expand) n <- nexp
#
#   # if (expand) {
#   #   n <- length(.names)
#   # } else {
#   #   n_s <- vapply(lst, fn_n, 0)
#   #   n <- unique(n_s)
#   #   N <- length(n)
#   #   if (N != 1)
#   #     stop(paste0("All matrices must have the same number of ", side_label, "s"))
#   # }
#
#   list(nms=.names, n=n, need_expand = need_expand)
# }





list_names <- function(lst)
{
  if (is.null(lst)) return(NULL)
  lst_nms <- names(lst)
  if (is.null(lst_nms) || any(lst_nms == ""))
    stop("The list elements must be named")
  if (any(duplicated(lst_nms))) stop("matrix names must be unique")
  lst_nms
}





#' MatrixMeta: Class to determine various matrix characteristics
#'
#' This class identifies the key characteristics of a list of matrices, in order
#' to either build a new matrixset or modify an existing one — for example,
#' by expanding or reducing the set.
#'
#' When modification is allowed, only a subset of the matrices may require
#' changes. For example, if two out of three matrices share the same
#' characteristics, but the third has fewer rows, only the third matrix would
#' need to be expanded.
#'
#' @docType class
#' @noRd
#' @name MatrixMeta
MatrixMeta <- R6::R6Class(
  "MatrixMeta",

  public = list(


    #' @description
    #' Creates a new MatrixMeta object
    #'
    #' @param mat_lst: A list of matrices from which to extract characteristics,
    #'                 before any adjustments such as expansion or shrinkage.
    #' @param match_names:
    #'                 [TRUE|FALSE] If TRUE, row and column names are allowed to
    #'                 be in a different order across the provided matrices.
    #' @param adjust:  [TRUE|FALSE] TRUE: Matrices can be reshaped if necessary.
    #'                 If a shape adjustment is needed, the new dimensions and
    #'                 their names will be returned.. Otherwise, if
    #'                 `adjust = FALSE`, an error is raised instead of allowing
    #'                 a shape change.
    #' @param target_info:
    #'                 An `environment` (or any object supporting the `$`
    #'                 operator, such as a `list`) containing metadata for
    #'                 `MatrixMeta`, based on a reference `data.frame` resulting
    #'                 from a join operation.
    #'
    #'                 This information reflects the post-join target structure,
    #'                 which may differ from the structure inferred from
    #'                 `mat_lst` (e.g., due to row expansion).
    #'
    #'                 This data frame is a margin (row or column) info of a
    #'                 secondary matrixset object.
    #'                 The mandatory elements are:
    #'                 - size:     the number of rows in the reference  data
    #'                             frame info
    #'                 - margin:   Either "row" or "col", identifying which
    #'                             margin info the reference data frame
    #'                             represents.
    #'                 - margin_comp:
    #'                             The complementary margin ("row" or "col").
    #'                             For example, if margin is "row", margin_comp
    #'                             is "col", and vice versa.
    #'                 - margin_names:
    #'                             The names of the margin (e.g., if margin =
    #'                             "row", it will be the rownames).
    #'                 - margin_names_unique:
    #'                             Margin names after disambiguation. Typically
    #'                             the same as margin_names, but may differ in
    #'                             join operations where duplicate names occur.
    #'                 - margin_comp_names:
    #'                             Like margin_names, but for the complementary
    #'                             margin.
    #'                 - margin_comp_names_unique:
    #'                             Like margin_names_unique, but for the
    #'                             complementary margin.
    #'
    #' @noRd
    initialize = function(mat_lst, match_names = FALSE, adjust = FALSE,
                          target_info = NULL) {

      private$._matrix_list = mat_lst
      if (!is.null(target_info)) private$._target_info <- target_info
      private$._adjust <- adjust
      private$._init_params()
      private$._match_names <- adjust || match_names
      private$._set_meta()

    }

  ),

  active = list(


    # row_names and row_names_unique (and similarly for col) are typically
    # equivalent. But they can  differ in context of join operations, when
    # multiple matches occur and names are duplicated.

    #' @field n_row               number of rows in each matrices
    #' @field n_col               number of columns in each matrices
    #' @field matrix_names        names of the matrices
    #' @field row_names           row names for each matrix
    #' @field row_names_unique    row names after disambiguation.
    #'                            Typically identical to `row_names`, but
    #'                            may differ in join operations where duplicate
    #'                            names occur.
    #' @field col_names           column names for each matrix
    #' @field col_names_unique    column names after disambiguation.
    #'                            Typically identical to column_names, but
    #'                            may differ in join operations where duplicate
    #'                            names occur.
    #' @field need_row_expand_per_mat
    #'                            Logical vector indicating whether each matrix
    #'                            requires row expansion.
    #' @field need_col_expand_per_mat
    #'                            Logical vector indicating whether each matrix
    #'                            requires column expansion.
    #' @field need_row_shrink_per_mat
    #'                            Logical vector indicating whether each matrix
    #'                            requires row shrinkage.
    #' @field need_col_shrink_per_mat
    #'                            Logical vector indicating whether each matrix
    #'                            requires column shrinkage.
    #' @field need_adapt          \[`TRUE`|`FALSE`\] `TRUE` if any matrix
    #'                            requires adaptation of any kind (expansion,
    #'                            shrinkage, or reordering).
    #' @field need_adjust_per_mat Logical vector indicating whether each matrix
    #'                            requires any adaptation of any kind (expansion,
    #'                            shrinkage, or reordering)

    n_row = function() private$n_row_,
    n_col = function() private$n_col_,
    matrix_names = function() names(private$._matrix_list),
    row_names = function() private$row_names_,
    row_names_unique = function() private$row_names_unique_,
    col_names = function() private$col_names_,
    col_names_unique = function() private$col_names_unique_,
    need_row_expand_per_mat = function() private$need_row_expand_per_mat_,
    need_col_expand_per_mat = function() private$need_col_expand_per_mat_,
    need_row_shrink_per_mat = function() private$._need_shrink_per_mat("row"),
    need_col_shrink_per_mat = function() private$._need_shrink_per_mat("col"),
    need_adapt = function() {
      private$._need_expansion() || private$._need_shrinkage() ||
        private$._need_ordering()
    },
    need_row_adapt_per_mat = function() private$._need_adapt_per_mat("row"),
    need_col_adapt_per_mat = function() private$._need_adapt_per_mat("col"),
    need_adjust_per_mat = function() private$need_row_adjust_per_mat_ |
      private$need_col_adjust_per_mat_

  ),

  private = list(

    ._matrix_list = NULL,                # internal storage for the matrices
    ._n_matrix = 0L,                     # number of matrices
    ._target_info = NULL,                # internal storage of the metadata
                                         # about the target annotation data
                                         # frame
    ._target_length = 0L,                # number of rows in the annotation data
                                         # frame, or 0 if none.

    n_row_ = 0,                          # internal storage of the number of
                                         # rows of each matrix. Exposed via the
                                         # active binding n_row()
    n_col_ =  0,                         # internal storage of the number of
                                         # columns of each matrix. Exposed via
                                         # the active binding n_col()
    row_names_ = NULL,                   # internal storage of each matrix row
                                         # names (may contain duplicates).
                                         # Exposed via the active binding
                                         # row_names()
    row_names_unique_ = NULL,            # internal storage of each
                                         # Disambiguated matrix row names.
                                         # Exposed via the active binding
                                         # row_names_unique()
    col_names_ = NULL,                   # internal storage of each matrix
                                         # column names (may contain
                                         # duplicates). Exposed via the active
                                         # binding column_names()
    col_names_unique_ = NULL,            # internal storage of each
                                         # Disambiguated matrix column names.
                                         # Exposed via the active binding
                                         # col_names_unique()


    ._adjust = FALSE,                    # Internal; whether any matrix
                                         # adjustment is allowed (expansion,
                                         # shrinkage, or reordering).
    ._match_names = FALSE,               # Internal; whether reordering based on
                                         # dimension names is allowed.


    need_row_expand_per_mat_ = NULL,     # Internal; Logical vector indicating
                                         # whether each matrix requires row
                                         # expansion. Exposed via the active
                                         # binding need_row_expand_per_mat()
    need_col_expand_per_mat_ = NULL,     # Internal; Logical vector indicating
                                         # whether each matrix requires column
                                         # expansion. Exposed via the active
                                         # binding need_col_expand_per_mat()
    need_row_adjust_per_mat_ = NULL,     # Internal; Logical vector indicating
                                         # whether each matrix requires row
                                         # adjustment.
    need_col_adjust_per_mat_ = NULL,     # Internal; Logical vector indicating
                                         # whether each matrix requires column
                                         # adjustment.
    need_row_order_per_mat_ = NULL,      # Internal; Logical vector indicating
                                         # whether each matrix requires row
                                         # reordering. Exposed via the active
                                         # binding need_row_order_per_mat()
    need_col_order_per_mat_ = NULL,      # Internal; Logical vector indicating
                                         # whether each matrix requires column
                                         # reordering. Exposed via the active
                                         # binding need_col_order_per_mat()


    ._margin_names_per_mat = NULL,         # utility object to store the
                                           # margin names for each matrix
    ._margin_unq_names_per_mat = NULL,     # utility object to store the
                                           # unique margin names for
                                           # each matrix (i.e., unique values of
                                           # ._margin_names_per_mat)
    ._margin_names_across_mats = NULL,     # utility object to store all unique
                                           # margin names across all matrices
                                           # (union of
                                           # ._margin_unq_names_per_mat).





    #' Private method
    #'
    #' Checks whether at least one matrix requires adjustment along the
    #' specified margin.
    #'
    #' @param margin    Either `"row"` or `"col"`. Margin to check for
    #'                  adjustment needs.
    #'
    #' @returns
    #' Logical; `TRUE` if adjustment is needed, `FALSE` otherwise.
    ._cumul_need_adjust = function(margin) {
      itm <- paste("need", margin, "adjust_per_mat_", sep = "_")
      any(private[[itm]])
    },




    #' Private method
    #'
    #' Checks whether at least one matrix requires shrinkage along the
    #' specified margin.
    #'
    #' @param margin    Either `"row"` or `"col"`. Margin to check for
    #'                  shrinkage needs.
    #'
    #' @returns
    #' Logical; `TRUE` if shrinkage is needed, `FALSE` otherwise.
    ._need_shrink_per_mat = function(margin) {
      itm_exp <- paste("need", margin, "expand_per_mat_", sep = "_")
      itm_adj <- paste("need", margin, "adjust_per_mat_", sep = "_")
      private[[itm_adj]] & !private[[itm_exp]]
    },




    #' Private method
    #'
    #' Checks whether at least one matrix requires adaptation of any kind (
    #' expansion, shrinkage or reordering) along the specified margin.
    #'
    #' @param margin    Either `"row"` or `"col"`. Margin to check for
    #'                  adaptation needs.
    #'
    #' @returns
    #' Logical; `TRUE` if adaptation is needed, `FALSE` otherwise.
    ._need_adapt_per_mat = function(margin) {

      itm_exp <- paste("need", margin, "expand_per_mat_", sep = "_")
      itm_ord <- paste("need", margin, "order_per_mat_", sep = "_")

      private[[itm_exp]] | private[[itm_ord]] |
        private$._need_shrink_per_mat("row")

    },





    #' Private method
    #'
    #' Checks if any matrix requires expansion, row or column.
    #'
    #' @returns
    #' Logical; `TRUE` if expansion is needed, `FALSE` otherwise.
    ._need_expansion = function() {
      any(private$need_row_expand_per_mat_) ||
        any(private$need_col_expand_per_mat_)
    },



    #' Private method
    #'
    #' Checks if any matrix requires shrinkage, row or column.
    #'
    #' @returns
    #' Logical; `TRUE` if shrinkage is needed, `FALSE` otherwise.
    ._need_shrinkage = function() {
      any(private$._need_shrink_per_mat("row")) ||
        any(private$._need_shrink_per_mat("col"))

    },



    #' Private method
    #'
    #' Checks if any matrix requires reordering, row or column.
    #'
    #' @returns
    #' Logical; `TRUE` if reordering is needed, `FALSE` otherwise.
    ._need_ordering = function() {
      any(private$need_row_order_per_mat_) ||
        any(private$need_col_order_per_mat_)
    },




    #' Private method
    #'
    #' Initiate internal parameters to default values
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._init_params = function() {
      n <- length(private$._matrix_list)

      private$._n_matrix <- n
      private$need_row_expand_per_mat_ <- logical(n)
      private$need_col_expand_per_mat_ <- logical(n)
      private$need_row_adjust_per_mat_ <- logical(n)
      private$need_col_adjust_per_mat_ <- logical(n)

      private$._target_length <- if (!is.null(private$._target_info) &&
                                     private$._adjust) {
        private$._target_info$size
      } else 0L
    },




    #' Private method
    #'
    #' Derives matrix metadata, optionally incorporating information from a
    #' target annotation data frame.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._set_meta = function() {

      if (is.list(private$._matrix_list)) {

        is_null <- vapply(private$._matrix_list, is.null, FALSE)
        if (!all(is_null)) {

          is_matrix <- vapply(private$._matrix_list, is_matrixish, FALSE)
          is_valid <- is_null | is_matrix
          if (!all(is_valid))
            stop("Elements must be NULL or a matrix")

          use_mat <- if (any(is_null)) !is_null else NULL

          private$._set_dimname("row", use_mat)
          private$._set_dimname("col", use_mat)

        }

      }

    },





    #' Private method
    #'
    #' Sets the appropriate dimnames based on the specified `margin`.
    #'
    #' @param margin        Character string; either "row" or "col", specifying
    #'                      the margin to apply the dimnames to.
    #' @param mat_subset    A logical vector the same length as the number of
    #'                      matrices, indicating which ones should contribute to
    #'                      the dimnames.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._set_dimname = function(margin, mat_subset) {

      private$._get_margin_names_across_mats(mat_subset, margin)

      private$._assess_order_need(margin)
      private$._assess_expansion_need(margin)

      n_adjust <- private$._adjusted_length(margin)

      # controlling on target_info is to make sure we are here from construction,
      # not joining
      if (n_adjust == 0L && private$._adjust && is.null(private$._target_info))
        stop("matrices must have dimnames for expansion")

      # With margin names provided, the following will have been tested already,
      # but since we (currently) allow for NULL names, this step is necessary in
      # this special case. Also, because of NULL names, assessing the number of
      # rows/columns will not work, so we must use a more direct approach
      private$._assess_num_margin_names_across_mats(mat_subset, margin)


      # when expansion comes from join operation, we get dimnames from the
      # annotation info
      if (!private$._set_names_from_info()) {

        private[[paste(margin, "names_unique_", sep = "_")]] <- private$._margin_names_across_mats
        private[[paste(margin, "names_", sep = "_")]] <- private$._margin_names_across_mats

      }


      if (private$._cumul_need_adjust(margin)) {
        private[[paste0("n_", margin, "_")]] <- n_adjust
      }

    },




    #' Private method
    #'
    #' Determines and sets dimnames for the given `margin` across matrices,
    #' and validates consistency when size adjustment is disallowed.
    #'
    #' @param mat_subset    A logical vector the same length as the number of
    #'                      matrices, indicating which ones should contribute to
    #'                      the dimnames.
    #' @param margin        Character string; either "row" or "col", specifying
    #'                      the margin to apply the dimnames to.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._get_margin_names_across_mats = function(mat_subset, margin) {

      margin_names <- paste0(margin, "names")
      margin_names_fn <- get(margin_names, pos = "package:base",
                             mode = "function")
      margin_label <- if (margin == "row") "row" else "column"

      mrg_nms_per_mat_full <- lapply(private$._matrix_list, margin_names_fn)
      mrg_nms_per_mat <- if (is.null(mat_subset)) {
        mrg_nms_per_mat_full
      } else {
        mrg_nms_per_mat_full[mat_subset]
      }

      private$._margin_names_per_mat <- mrg_nms_per_mat_full


      private$._margin_unq_names_per_mat <- unique(mrg_nms_per_mat_full)

      n_margin_names_across_mats <- length(unique(mrg_nms_per_mat))

      if (n_margin_names_across_mats != 1) {
        if (!private$._match_names && !private$._adjust) {
          stop(paste("All matrices must have the same", margin_label,
                     "names (NULL accepted)"))
        }


      }

      private$._assess_margin_names_across_mats(margin_label)

      private$._margin_names_across_mats <- unique(unlist(private$._margin_unq_names_per_mat))


    },






    #' Private method
    #'
    #' Checks whether margin names are unique and non-empty.
    #'
    #' @param margin_label     A string used in error messages to identify the
    #'                         source of the issue when an assessment fails.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._assess_margin_names_across_mats = function(margin_label) {

      lapply(private$._margin_unq_names_per_mat,
             function(.nms) {
               .names_unq <- unique(.nms)

               if (length(.names_unq) < length(.nms))
                 stop(paste(stringr::str_to_title(margin_label),
                            "names must be unique"))

               if (any(.nms == ""))
                 stop(paste("Empty", margin_label, "names are not allowed"))
             })

    },





    #' Private Method
    #'
    #' Checks each matrix to determine whether reordering is needed along the
    #' specified margin.
    #'
    #' @param margin   Margin to check: either `"row"` or `"col"`.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._assess_order_need = function(margin) {

      ord_name <- paste("need", margin, "order_per_mat_", sep = "_")
      private[[ord_name]] <- vapply(private$._margin_names_per_mat,
                                    private$._assess_ordering,
                                    FALSE)
    },




    #' Private Method
    #'
    #' Checks each matrix to determine whether expansion is needed along the
    #' specified margin. Also updates per-matrix adjustment flags accordingly,
    #' as expansion is a form of adjustment.
    #'
    #' @param margin   Margin to check: either `"row"` or `"col"`.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._assess_expansion_need = function(margin) {

      exp_name <- paste("need", margin, "expand_per_mat_", sep = "_")
      adj_name <- paste("need", margin, "adjust_per_mat_", sep = "_")


      for (m in seq_len(private$._n_matrix)) {
        n_nms <- length(private$._margin_names_per_mat[[m]])


        if (!is.null(private$._target_info) &&
            private$._target_info$margin == margin) {

          if (n_nms != private$._target_length) {
            private[[adj_name]][m] <- TRUE

            if (n_nms < private$._target_length) {
              private[[exp_name]][m] <- TRUE
              next
            }

            private[[exp_name]][m] <- FALSE
            next
          }

        }


        # this scenario here can only happens at matrixset creation, where
        # only expansion is possible - thus no shrinking
        if (n_nms < length(private$._margin_names_across_mats)) {
          private[[exp_name]][m] <- TRUE
          private[[adj_name]][m] <- TRUE

          next
        }

        private[[adj_name]][m] <- FALSE # NEEDED??????

      }

    },






    #' Private Method
    #'
    #' Determines whether reordering is needed by comparing the given `nms`
    #' vector  to `._margin_names_across_mats`.
    #'
    #' @param nms   A `character` vector of margin names to evaluate.
    #'
    #' @returns
    #' Logical; `TRUE` if reordering is needed, `FALSE` otherwise.
    ._assess_ordering = function(nms) {

      if (length(nms) < length(private$._margin_names_across_mats)) return(TRUE)
      if (any(nms != private$._margin_names_across_mats)) return(TRUE)
      FALSE

    },




    #' Private Method
    #'
    #' Returns the size of the specified `margin`. If adjustment is required,
    #' this is the target size.
    #'
    #' Size counting is based on the observed margin names.
    #'
    #' @param margin   `"row"` or `"col"`; the margin to measure.
    #'
    #' @returns
    #' A `numeric` value representing the margin size.
    ._adjusted_length = function(margin) {

      n_adjust <- length(private$._margin_names_across_mats)

      if (is.null(private$._target_info) || !private$._adjust) return(n_adjust)


      exp_name <- paste("need", margin, "expand_per_mat_", sep = "_")
      collapse_fun <- if (any(private[[exp_name]])) max else min

      collapse_fun(n_adjust, private$._target_length)
    },





    #' Private Method
    #'
    #' Counts the `margin` size for each eligible matrix. If adjustment is not
    #' allowed, checks whether all sizes are identical across matrices.
    #'
    #' The count is based on actual margin sizes, not dimnames.
    #'
    #' @param mat_subset  Logical vector indicating which matrices to include.
    #' @param margin      `"row"` or `"col"`; the margin to assess.
    #'
    #' #' @returns
    #' Nothing; used for its side effects.
    ._assess_num_margin_names_across_mats = function(mat_subset, margin) {

      n_margin <- paste0("n", margin)
      n_margin_fn <- get(n_margin, pos = "package:base", mode = "function")
      margin_label <- if (margin == "row") "row" else "column"

      n_mrg_for_each <- if (is.null(mat_subset)) {
        vapply(private$._matrix_list, n_margin_fn, 0)
      } else {
        vapply(private$._matrix_list[mat_subset], n_margin_fn, 0)
      }
      n_mrg <- as.integer(unique(n_mrg_for_each))
      N <- length(n_mrg)
      if (!private$._adjust && N != 1)
        stop(paste0("All matrices must have the same number of ", margin_label, "s"))

      private[[paste0("n_", margin, "_")]] <- n_mrg
    },





    #' Private Method
    #'
    #' In the context of a join operation (not matrix construction), updates
    #' margin names using metadata from the target annotation data frame, if
    #' available.
    #'
    #' @returns
    #' `TRUE` if updates were made, `FALSE` otherwise (e.g., no target
    #' annotation data).
    ._set_names_from_info = function() {

      if (!is.null(private$._target_info) && private$._adjust) {

        mrg_nm_id <- paste(private$._target_info$margin, "names_", sep = "_")
        mrg_nm_unq_id <- paste(private$._target_info$margin, "names_unique_",
                               sep = "_")


        mrg_comp_nm_id <- paste(private$._target_info$margin_comp, "names_",
                                sep = "_")
        mrg_comp_nm_unq_id <- paste(private$._target_info$margin_comp,
                                    "names_unique_", sep = "_")

        private[[mrg_nm_unq_id]] <- private$._target_info$margin_names_unique
        private[[mrg_nm_id]] <- private$._target_info$margin_names


        private[[mrg_comp_nm_unq_id]] <- private$._target_info$margin_comp_names_unique
        private[[mrg_comp_nm_id]] <- private$._target_info$margin_comp_names

        return(TRUE)
      }

      return(FALSE)

    }

  )
)







# get, from matrix list, the number of rows and columns. Assess if all matrices
# of same dim. Get also the matrix rownames and colnames. Assess if all matrices
# have the same names and if names are valid
# lst -  list of matrix
# info_matrices <- function(lst = NULL, expand = NULL)
# {
#   n_row <- NULL
#   n_col <- NULL
#   need_expand <- FALSE
#
#   if (is.null(lst)) {
#     n_row <- 0
#     n_col <-  0
#     row_names <- NULL
#     col_names <- NULL
#   } else if (is.list(lst)) {
#
#     # lst_nms <- names(lst)
#     # if (is.null(lst_nms) || any(lst_nms == ""))
#     #   stop("The list elements must be named")
#
#     is_null <- vapply(lst, is.null, FALSE)
#     if (all(is_null)) {
#       n_row <- 0
#       n_col <-  0
#       row_names <- NULL
#       col_names <- NULL
#     } else {
#       is_matrix <- vapply(lst, is_matrixish, FALSE)
#       is_valid <- is_null | is_matrix
#       if (!all(is_valid))
#         stop("Elements must be NULL or a matrix")
#
#       row_meta <- info_dim(lst[!is_null], "row", !is.null(expand))
#       col_meta <- info_dim(lst[!is_null], "col", !is.null(expand))
#
#       n_row <- row_meta$n
#       n_col <- col_meta$n
#       row_names <- row_meta$nms
#       col_names <- col_meta$nms
#       row_expand <- row_meta$need_expand
#       col_expand <- col_meta$need_expand
#       need_expand <- row_expand || col_expand
#
#     }
#   }
#
#   return(list(n_row = as.integer(n_row), n_col = as.integer(n_col),
#               row_names = row_names, col_names = col_names,
#               need_expand = need_expand))
# }



# get matrix list from dots.
matrices_from_dots <- function(...)
{
  mats <- list(...)
  if (length(mats) == 1L) {
    m <- mats[[1]]
    if (is.null(m) || is.list(m)) mats <- m
  }

  mats
}



# MATRIX <- function(dat, nrow, ncol, is_Matrix = FALSE)
# {
#   if (is_Matrix) {
#     Matrix::Matrix(0, nrow=nrow, ncol=ncol)
#   } else {
#     matrix(dat, nrow=nrow, ncol=ncol)
#   }
# }


#
# set_expand_value <- function(is_Matrix, exp_val)
# {
#   if (is.logical(exp_val) && !is.na(exp_val)) {
#     if (is_Matrix) return(0) else return(NA)
#   }
#   exp_val
#   # if (is_Matrix && (is.logical(exp_val) && !is.na(exp_val))) 0 else exp_val
# }





new_empty_matrix <- function(dat, nrow, ncol, rownms, colnms, is_Matrix = FALSE)
{
  if (is_Matrix) {

    m <- Matrix::Matrix(dat, nrow = nrow, ncol = ncol)

    if (is.na(dat) || ((is.integer(dat) && dat != 0L) ||
                       abs(dat) > .Machine$double.eps^.5)) {

      m <- methods::as(m, "generalMatrix")
    }

  } else m <- matrix(dat, nrow=nrow, ncol=ncol)

  rownames(m) <- rownms
  colnames(m) <- colnms

  m
}





#' MatrixAdjuster: A class for adjusting matrices
#'
#' Based on characteristics extracted from an instance of a `MatrixMeta`
#' object, this class performs necessary adjustments on the matrices in the
#' input list.
#'
#' To ensure general usability, no adjustment is performed if it is not needed,
#' as indicated by the `expand` flag.
#'
#' @docType class
#' @noRd
#' @name MatrixAdjuster
MatrixAdjuster <- R6::R6Class(
  "MatrixAdjuster",

  public = list(



    #' @description
    #' Initializes a new `MatrixAdjuster` object.
    #'
    #' @param matrix_list    A list of matrices to be adjusted.
    #' @param matrix_info    An instance of a `MatrixMeta` object, providing
    #'                       the necessary metadata to guide the adjustment
    #'                       process.
    #' @param expand         Controls whether input matrices should be expanded,
    #'                       and specifies the padding values to use if
    #'                       expansion is required.
    #'
    #'                       If `NULL` or `FALSE`, no expansion is performed.
    #'
    #'                       If `TRUE`, expansion is enabled using default
    #'                       padding values: `NA` for base R matrices, and `0`
    #'                       for matrices from the `Matrix` package (which will
    #'                       convert them to sparse matrices).
    #'
    #'                       You can also provide a single custom padding value
    #'                       (e.g., `-1`) to apply the same padding to all
    #'                       matrices.
    #'
    #'                       Alternatively, you may pass a list of padding
    #'                       values. If the list is unnamed, it must have the
    #'                       same length as `matrix_list`, and the values will
    #'                       be applied in order. If the list is named, its
    #'                       names will be matched to those of the matrices.
    #'
    #'                       Finally, in the case of adjustments resulting from
    #'                       a join operation, `expand` can be a list of
    #'                       matrices from which padding values will be derived.

    #' @noRd
    initialize = function(matrix_list, matrix_info, expand) {

      private$._matrix_list <- matrix_list
      private$._n_matrix <- length(matrix_list)
      private$._target_info <- matrix_info
      private$._set_expand_param(expand)
      private$._adjust()

    }
  ),


  active = list(


    #' @field adjusted_mats  A list of matrices that have been either resized,
    #'                       reordered, or both.

    adjusted_mats = function() private$adjusted_mats_

  ),


  private = list(


    ._matrix_list = NULL,                # Internal storage for the list of
                                         # matrices to be adjusted.
    ._n_matrix = 0L,                     # Number of matrices to adjust.
    adjusted_mats_ = NULL,               # Internal storage for the list of
                                         # matrices that have been resized,
                                         # reordered, or both.
    ._target_info = NULL,                # An instance of a `MatrixMeta` object,
                                         # containing the metadata needed to
                                         # guide the adjustment process.
    ._expand_param = NULL,               # Stores the `expand` parameter value
                                         # unless the adjustment originates from
                                         # a join operation. In such cases, it
                                         # will be set to `NA` to initialize the
                                         # adjusted matrices.
    ._expand_outer_source = NULL,        # Stores the `expand` value used when
                                         # the adjustment comes from a join
                                         # operation.
    ._expand_from_outer = FALSE,         # Indicates whether the adjustment
                                         # request is the result of a join
                                         # operation (`TRUE`) or a user-defined
                                         # request (`FALSE`).
    ._expand_from_outer_possible = FALSE,# Will be `FALSE` if
                                         # `.expand_from_outer` is `TRUE`, or if
                                         # it's not possible to fulfill the
                                         # request — for instance, when the
                                         # external source does not provide the
                                         # required matrix.
    ._outer_indexes = NULL,              # When padding a matrix using values
                                         # from an outer matrix, this stores the
                                         # row and column indices from both the
                                         # inner and outer matrices, needed to
                                         # align them properly.
    ._align_indexes = NULL,





    #' Private Method
    #'
    #' Determines and sets the values of `._expand_param`,
    #' `._expand_outer_source`, and  `._expand_from_outer` based on the input
    #' `expand` strategy.
    #'
    #' If `expand` is a list of matrices, it indicates that the expansion was
    #' triggered by a join operation. In such cases, the three internal
    #' variables are set accordingly:
    #' - `._expand_param` is set to `NA` (as a placeholder) rather than storing
    #'   the actual padding value(s),
    #' - `._expand_outer_source` stores the expansion parameters used during the
    #'   join,
    #' - `._expand_from_outer` is set to `TRUE`.
    #'
    #' When `expand` is not a list of matrices (i.e., the expansion is
    #' user-defined), the variables are set as follows:
    #' - `._expand_param` stores the value of `expand`,
    #' - `._expand_outer_source` is set to `NULL`,
    #' - `._expand_from_outer` is set to `FALSE`.
    #'
    #' @param expand The expansion strategy to apply. This value determines how
    #'               the internal expansion-related fields are initialized. See
    #'               the descriptions of `._expand_param`,
    #'               `._expand_outer_source`, and `._expand_from_outer` for more
    #'               details.
    ._set_expand_param = function(expand) {

      if (!is.list(expand)) {
        private$._expand_param <- expand
        return()
      }


      is_mtrx <- vapply(expand, is_matrixish, FALSE)
      if (any(is_mtrx)) {
        private$._expand_param <- NA
        private$._expand_outer_source <- expand
        private$._expand_from_outer <- TRUE
        return()
      }


      # if (is.null(expand$matrix_set)) {
      #   private$._expand_param <- expand
      #   return()
      # }

      private$._expand_param <- expand

    },






    #' This method applies a two-step strategy: it first initializes the adjusted matrix
    #' with padding values, and then overwrites the relevant elements with the actual values
    #' from the original matrix.
    ._adjust = function() {


      if (!private$._target_info$need_adapt)
        return()


      private$._init()


      for (m in seq_len(private$._n_matrix)) {

        if (is.null(private$._matrix_list[[m]])) next

        padding_val <- private$._set_expand_value(m)
        private$._init_matrix(m, padding_val)

        m_y <- private$._outer_matrix_idx(m)

        old_rnms <- rownames(private$._matrix_list[[m]])
        old_cnms <- colnames(private$._matrix_list[[m]])


        private$._init_align_to_outer_indexes(m, m_y, old_rnms, old_cnms)
        private$._init_align_indexes(m, old_rnms, old_cnms)


        adjust_order_rows <- private$._need_adjustment("row", m)
        adjust_order_cols <- private$._need_adjustment("col", m)
        shrink_rows <- private$._need_shrinkage("row", m)
        shrink_cols <- private$._need_shrinkage("col", m)


        if (adjust_order_rows) {


          if (adjust_order_cols) {

            if (shrink_rows) {

              if (shrink_cols) {

                private$adjusted_mats_[[m]] <-
                  private$._matrix_list[[m]][private$._align_indexes$roi,
                                             private$._align_indexes$coi]
                next
              }

              private$adjusted_mats_[[m]][, private$._align_indexes$cai] <-
                private$._matrix_list[[m]][private$._align_indexes$roi,
                                           private$._align_indexes$coi]
              next

            }


            if (shrink_cols) {

              private$adjusted_mats_[[m]][private$._align_indexes$rai, ] <-
                private$._matrix_list[[m]][private$._align_indexes$roi,
                                           private$._align_indexes$coi]
              next
            }

            private$adjusted_mats_[[m]][private$._align_indexes$rai,
                                        private$._align_indexes$cai] <-
              private$._matrix_list[[m]][private$._align_indexes$roi,
                                         private$._align_indexes$coi]
            next

          }



          if (shrink_rows) {

            private$adjusted_mats_[[m]] <-
              private$._matrix_list[[m]][private$._align_indexes$roi, ]
            next

          }

          private$adjusted_mats_[[m]][private$._align_indexes$rai, ] <-
            private$._matrix_list[[m]][private$._align_indexes$roi, ]

          if (private$._expand_from_outer_possible) {
            private$adjusted_mats_[[m]][private$._outer_indexes$rti_y,
                                        private$._outer_indexes$cti_y] <-
              private$._expand_outer_source[[m_y]][private$._outer_indexes$roi_y,
                                                   private$._outer_indexes$coi_y]
          }
          next


        }



        if (adjust_order_cols) {

          if (shrink_cols) {

            private$adjusted_mats_[[m]] <-
              private$._matrix_list[[m]][, private$._align_indexes$coi]
            next
          }

          private$adjusted_mats_[[m]][, private$._align_indexes$cai] <-
            private$._matrix_list[[m]][, private$._align_indexes$coi]
          next

        }

      }


    },





    ._init = function() {

      # private$expanded_mats_ <- vector('list', length(private$._matrix_list))
      private$adjusted_mats_ <- vector('list', private$._n_matrix)
      names(private$adjusted_mats_) <- names(private$._matrix_list)

    },







    ._set_expand_value = function(mat_idx)
    {
      exp_val <- private$._expand_param
      as_Matrix <- is(private$._matrix_list[[mat_idx]], "Matrix")

      exp_val_i <- if (is.logical(exp_val)) exp_val else exp_val[[mat_idx]]

      if (is.logical(exp_val_i) && !is.na(exp_val_i)) {
        if (as_Matrix) return(0) else return(NA)
      }
      exp_val_i

    },





    ._init_matrix = function(mat_idx, padding_val) {

      # if (private$._target_info$need_expand_per_mat[mat_idx]) {
      if (private$._target_info$need_adjust_per_mat[mat_idx]) {


        as_Matrix <- is(private$._matrix_list[[mat_idx]], "Matrix")

        if (as_Matrix) {

          private$._init_S4Matrix(mat_idx, padding_val)
          return()

        }

        # private$expanded_mats_[[mat_idx]] <-
        #   matrix(padding_val, nrow = private$._target_info$n_row,
        #          ncol = private$._target_info$n_col)
        private$adjusted_mats_[[mat_idx]] <- new_empty_matrix(padding_val,
                                                              nrow = private$._target_info$n_row,
                                                              ncol = private$._target_info$n_col,
                                                              rownms = private$._target_info$row_names_unique,
                                                              colnms = private$._target_info$col_names_unique,
                                                              is_Matrix = FALSE)

        return()
      }



      # if (private$._target_info$need_shrink_per_mat[mat_idx]) {
      #
      #
      #   as_Matrix <- is(private$._matrix_list[[mat_idx]], "Matrix")
      #
      #   if (as_Matrix) {
      #
      #     private$._init_S4Matrix(mat_idx, padding_val)
      #     return()
      #
      #   }
      #
      #   # private$expanded_mats_[[mat_idx]] <-
      #   #   matrix(padding_val, nrow = private$._target_info$n_row,
      #   #          ncol = private$._target_info$n_col)
      #   private$expanded_mats_[[mat_idx]] <- new_empty_matrix(padding_val,
      #                                                         nrow = private$._target_info$n_row,
      #                                                         ncol = private$._target_info$n_col,
      #                                                         rownms = private$._target_info$row_names_unique,
      #                                                         colnms = private$._target_info$col_names_unique,
      #                                                         is_Matrix = FALSE)
      #
      #   return()
      # }





      private$adjusted_mats_[[mat_idx]] <- private$._matrix_list[[mat_idx]]

      rownames(private$adjusted_mats_[[mat_idx]]) <- private$._target_info$row_names_unique
      colnames(private$adjusted_mats_[[mat_idx]]) <- private$._target_info$col_names_unique


      # if (!private$._target_info$need_expand_per_mat[mat_idx]) {
      #
      #   private$expanded_mats_[[mat_idx]] <- private$._matrix_list[[mat_idx]]
      #
      #   rownames(private$expanded_mats_[[mat_idx]]) <- private$._target_info$row_names_unique
      #   colnames(private$expanded_mats_[[mat_idx]]) <- private$._target_info$col_names_unique
      #
      # } else {
      #
      #   as_Matrix <- is(private$._matrix_list[[mat_idx]], "Matrix")
      #
      #   if (as_Matrix) {
      #
      #     private$._init_S4Matrix(mat_idx, padding_val)
      #
      #   } else {
      #
      #     # private$expanded_mats_[[mat_idx]] <-
      #     #   matrix(padding_val, nrow = private$._target_info$n_row,
      #     #          ncol = private$._target_info$n_col)
      #     private$expanded_mats_[[mat_idx]] <- new_empty_matrix(padding_val,
      #                      nrow = private$._target_info$n_row,
      #                      ncol = private$._target_info$n_col,
      #                      rownms = private$._target_info$row_names_unique,
      #                      colnms = private$._target_info$col_names_unique,
      #                      is_Matrix = FALSE)
      #
      #   }
      #
      # }

      # rownames(private$expanded_mats_[[mat_idx]]) <- private$._target_info$row_names
      # colnames(private$expanded_mats_[[mat_idx]]) <- private$._target_info$col_names

    },





    #' Private Method
    #'
    #' Determines the row and column indices from both the original (to be
    #' padded) and the padding source (outer) matrices, in order to align them
    #' correctly.
    #'
    #' The four sets of indices are stored in variables following the naming
    #' convention: [r|c][o|t]i, where:
    #'   - 'r' or 'c' indicates row or column,
    #'   - 'o' or 't' stands for original (outer/padding source) or target (matrix
    #'     to be padded),
    #'   - 'i' denotes index.
    #'
    #' Alignment is performed such that, assuming the matrices are named `origin`
    #' and `target`, the following replacement holds:
    #' `origin[rti_y, cti_y] <- target[roi_y, coi_y]`.
    #' For example, `rti_y` contains the row indices in `origin` that align with
    #' the rows of `target`, and so on.
    #'
    #' @param m           Index matrix for the target (original) matrix to be
    #'                    padded.
    #' @param m_y         Index matrix for the source (outer) matrix providing
    #'                    padding values.
    #' @param old_rnms    Row names of the target matrix.
    #' @param old_cnms    Column names of the target matrix.
    #'
    ._init_align_to_outer_indexes = function(m, m_y, old_rnms, old_cnms) {

      private$._outer_indexes$coi_y <- NULL
      private$._outer_indexes$roi_y <- NULL
      private$._outer_indexes$cti_y <- NULL
      private$._outer_indexes$rti_y <- NULL


      if (private$._expand_from_outer_possible) {

        y_cnms <- colnames(private$._expand_outer_source[[m_y]])
        if (private$._target_info$need_col_expand_per_mat[m])
          y_cnms[y_cnms %in% old_cnms] <- NA_character_
        coi_y <- match(private$._target_info$col_names, y_cnms, 0)
        private$._outer_indexes$coi_y <- coi_y[coi_y > 0]

        private$._outer_indexes$cti_y <- which(private$._target_info$col_names %in% y_cnms)


        y_rnms <- rownames(private$._expand_outer_source[[m_y]])
        if(private$._target_info$need_row_expand_per_mat[m])
          y_rnms[y_rnms %in% old_rnms] <- NA_character_
        roi_y <- match(private$._target_info$row_names, y_rnms, 0)
        private$._outer_indexes$roi_y <- roi_y[roi_y > 0]

        private$._outer_indexes$rti_y <- which(private$._target_info$row_names %in% y_rnms)
      }

    },




    #' Private Method
    #'
    #' This method determines the row and column indices from both the adjusted
    #' matrix (i.e., the padded result) and the original matrix, in order to
    #' copy the appropriate values to the correct positions.
    #'
    #' The four sets of indices are stored in variables following the naming
    #' convention: [r|c][o|a]i, where:
    #'   - 'r' or 'c' indicates row or column,
    #'   - 'o' or 'a' stands for original or adjusted,
    #'   - 'i' denotes index.
    #'
    #' Alignment is performed such that, assuming the matrices are named `adjusted`
    #' and `origin`, the following assignment works:
    #' `adjusted[rai, cai] <- origin[roi, coi]`.
    #' For instance, `rai` contains the row indices in `adjusted` that align with
    #' the rows of `origin`.
    #'
    #' @param m           Index matrix for the matrix being adjusted.
    #' @param old_rnms    Row names of the original matrix.
    #' @param old_cnms    Column names of the original matrix.

    ._init_align_indexes = function(m, old_rnms, old_cnms) {

      private$._align_indexes$coi <- NULL
      private$._align_indexes$roi <- NULL
      private$._align_indexes$cai <- NULL
      private$._align_indexes$rai <- NULL

      adjust_order_rows <- private$._need_adjustment("row", m)
      adjust_order_cols <- private$._need_adjustment("col", m)
      shrink_rows <- private$._need_shrinkage("row", m)
      shrink_cols <- private$._need_shrinkage("col", m)


      if (adjust_order_cols) {

        private$._align_indexes$coi <- match(private$._target_info$col_names,
                                             old_cnms, 0)

        if (!shrink_cols)
          private$._align_indexes$cai <- which(private$._target_info$col_names %in% old_cnms)

      }



      if (adjust_order_rows) {

        private$._align_indexes$roi <- match(private$._target_info$row_names,
                                             old_rnms, 0)

        if (!shrink_rows)
          private$._align_indexes$rai <- which(private$._target_info$row_names %in% old_rnms)
      }

    },






    ._init_S4Matrix = function(mat_idx, padding_val) {

      NR <- nrow(private$._matrix_list[[mat_idx]])
      NC <- ncol(private$._matrix_list[[mat_idx]])

      # private$expanded_mats_[[mat_idx]] <- Matrix::Matrix(0,
      #                                                     nrow = private$._target_info$n_row,
      #                                                     ncol = private$._target_info$n_col)
      # private$expanded_mats_[[mat_idx]][] <- padding_val

      if (is.na(padding_val) && is.logical(padding_val)) padding_val <- NA_real_

      private$adjusted_mats_[[mat_idx]] <- new_empty_matrix(padding_val,
                                                            nrow = private$._target_info$n_row,
                                                            ncol = private$._target_info$n_col,
                                                            rownms = private$._target_info$row_names_unique,
                                                            colnms = private$._target_info$col_names_unique,
                                                            is_Matrix = TRUE)

      # private$expanded_mats_[[mat_idx]] <- Matrix::Matrix(padding_val,
      #                                                     nrow = private$._target_info$n_row,
      #                                                     ncol = private$._target_info$n_col)
      #
      #
      # if (is.na(padding_val) || ((is.integer(padding_val) && padding_val != 0L) ||
      #     abs(padding_val) > .Machine$double.eps^.5)) {
      #   private$expanded_mats_[[mat_idx]] <- methods::as(private$expanded_mats_[[mat_idx]],
      #                                                    "generalMatrix")
      # }

      if ((NR < private$._target_info$n_row || NC < private$._target_info$n_col) &&
          ((is.integer(padding_val) && padding_val %.==.% 0L) ||
           # (is.numeric(padding_val) && abs(padding_val) < .Machine$double.eps^.5)))
           (is.numeric(padding_val) && abs(padding_val) %.<.% .Machine$double.eps^.5)))
        private$adjusted_mats_[[mat_idx]] <- methods::as(private$adjusted_mats_[[mat_idx]],
                                                         "sparseMatrix")

    },




#     ._finish_S4Matrix = function(mat_idx, padding_val, comp_ri, comp_ci) {
#
#       as_Matrix <- is(private$._matrix_list[[mat_idx]], "Matrix")
#       if (!as_Matrix) return()
#
#       ri_from <- seq_len(private$._target_info$n_row)
#       ci_from <- seq_len(private$._target_info$n_col)
#
#       ri <- if (is.null(comp_ri)) {
#         ri_from
#       } else if (length(comp_ri) == private$._target_info$n_row) {
#         NULL
#       } else {
#         setdiff(ri_from, comp_ri)
#       }
#
#
#       ci <- if (is.null(comp_ci)) {
#         ci_from
#       } else if (length(comp_ci) == private$._target_info$n_col) {
#         NULL
#       } else {
#         setdiff(ci_from, comp_ci)
#       }
#
# print(list(ri, comp_ri, ci,comp_ci))
#       if (is.null(ri)) {
#         if (is.null(ci)) return()
#
#         private$expanded_mats_[[mat_idx]][, ci] <- padding_val
#         return()
#       }
#
#
#       if (is.null(ci)) {
#         # private$expanded_mats_[[mat_idx]][ri, ] <- padding_val
#         return()
#       }
#
#       # private$expanded_mats_[[mat_idx]][ri, ci] <- padding_val
#       # print(c(ci, comp_ci))
#
#     },







    ._need_adjustment = function(margin, mat_idx) {

      # look_for_expand <- paste("need", margin, "expand_per_mat", sep = "_")
      # look_for_shrink <- paste("need", margin, "shrink_per_mat", sep = "_")
      # # look_for_EXPAND <- paste("need", margin, "EXPAND_per_mat", sep = "_")
      # look_for_order <- paste("need", margin, "order_per_mat", sep = "_")
      #
      # need_expand <- private$._target_info[[look_for_expand]][mat_idx]
      # need_shrink <- private$._target_info[[look_for_shrink]][mat_idx]
      # # need_EXPAND <- private$._target_info[[look_for_EXPAND]][mat_idx]
      # # need_shrink <- private$need_expand & private$!need_EXPAND
      # need_order <- private$._target_info[[look_for_order]][mat_idx]
      #
      # need_expand || need_shrink || need_order
      # # need_adjust || need_order

      look_for_adapt <- paste("need", margin, "adapt_per_mat", sep = "_")
      # print(list(need_expand, need_shrink, need_order,
      #            need_expand || need_shrink || need_order,
      #            private$._target_info[[look_for_adjust]][mat_idx]))
      private$._target_info[[look_for_adapt]][mat_idx]
    },



    ._need_shrinkage = function(margin, mat_idx) {

      look_for_shrink <- paste("need", margin, "shrink_per_mat", sep = "_")
      need_shrink <- private$._target_info[[look_for_shrink]][mat_idx]

      need_shrink

    },





     ._outer_matrix_idx = function(idx) {

       if (!private$._expand_from_outer) {
         private$._expand_from_outer_possible <- FALSE
         return(NULL)
       }

       mat_name <- names(private$._matrix_list)[idx]
       out_idx <- match(mat_name, names(private$._expand_outer_source), 0L)

       if (out_idx == 0L) {
         private$._expand_from_outer_possible <- FALSE
         return(NULL)
       }

       private$._expand_from_outer_possible <- TRUE
       out_idx
     }





    # ._set_target_pos = function(margin, orig_nms) {
    #
    #   target_nms <- paste0(margin, "_names")
    #   which(private$._target_info[[target_nms]] %in% orig_nms)
    #
    # },



  )
)




#
# expand_matrices <- function(matrix_list, matrix_info, expand)
# {
#   need_expand <- matrix_info$need_expand
#   if (!need_expand) return(matrix_list)
#
#   nmatrix <- length(matrix_list)
#
#   expand_list <- vector('list', nmatrix)
#   names(expand_list) <- names(matrix_list)
#
#   nr <- matrix_info$n_row
#   nc <- matrix_info$n_col
#   rnms <- matrix_info$row_names
#   cnms <- matrix_info$col_names
#   for (l in 1:nmatrix) {
#     NR <- nrow(matrix_list[[l]])
#     NC <- ncol(matrix_list[[l]])
#     if (!is.null(matrix_list[[l]])) {
#       is_Matrix <- is(matrix_list[[l]], "Matrix")
#
#       if (NR != nr || NC != nc) {
#         expand_value <- set_expand_value(is_Matrix, if (is.logical(expand)) expand else expand[[l]])
#         expand_list[[l]] <- MATRIX(expand_value, nrow = nr, ncol = nc, is_Matrix)
#       } else {
#         expand_list[[l]] <- matrix_list[[l]]
#       }
#
#       rownames(expand_list[[l]]) <- rnms
#       colnames(expand_list[[l]]) <- cnms
#       if (is_Matrix) {
#         expand_list[[l]][] <- expand_value
#         expand_list[[l]] <- methods::as(expand_list[[l]], class(matrix_list[[l]]))
#         if ((NR < nr || NC < nc) &&
#             ((is.integer(expand_value) && expand_value == 0L) ||
#              (is.numeric(expand_value) && abs(expand_value) < .Machine$double.eps^.5)))
#           expand_list[[l]] <- methods::as(expand_list[[l]], "sparseMatrix")
#       }
#       old_rnms <- rownames(matrix_list[[l]])
#       old_cnms <- colnames(matrix_list[[l]])
#       ridx <- rnms %in% old_rnms
#       cidx <- cnms %in% old_cnms
#       ri <- match(rnms, old_rnms, 0)
#       ci <- match(cnms, old_cnms, 0)
#       if (any(ridx) && any(cidx) && (any(ri != seq_along(ri)) || any(ci != seq_along(ci)))) {
#         expand_list[[l]][rnms[ridx], cnms[cidx]] <- matrix_list[[l]][ri, ci]
#       }
#
#
#       # is_Matrix <- is(matrix_list[[l]], "Matrix")
#       # expand_value <- set_expand_value(is_Matrix, if (is.logical(expand)) expand else expand[[l]])
#       # expand_list[[l]] <- MATRIX(expand_value, nrow = nr, ncol = nc, is_Matrix)
#       # # expand_list[[l]] <- MATRIX(expand[[l]], nrow = nr, ncol = nc, is_Matrix)
#       # rownames(expand_list[[l]]) <- rnms
#       # colnames(expand_list[[l]]) <- cnms
#       # if (is_Matrix) {
#       #   # expand_list[[l]][] <- expand[[l]]
#       #   expand_list[[l]][] <- expand_value
#       #   expand_list[[l]] <- methods::as(expand_list[[l]], class(matrix_list[[l]]))
#       #   if ((NR < nr || NC < nc) &&
#       #       ((is.integer(expand_value) && expand_value == 0L) ||
#       #        (is.numeric(expand_value) && abs(expand_value) < .Machine$double.eps^.5)))
#       #     expand_list[[l]] <- methods::as(expand_list[[l]], "sparseMatrix")
#       # }
#       # old_rnms <- rownames(matrix_list[[l]])
#       # old_cnms <- colnames(matrix_list[[l]])
#       # ridx <- rnms %in% old_rnms
#       # cidx <- cnms %in% old_cnms
#       # ri <- match(rnms, old_rnms, 0)
#       # ci <- match(cnms, old_cnms, 0)
#       # if (any(ridx) && any(cidx)) {
#       #   expand_list[[l]][rnms[ridx], cnms[cidx]] <- matrix_list[[l]][ri, ci]
#       # }
#     }
#   }
#
#   expand_list
# }



# the 'adjust' parameter allows to validate some elements of 'meta' without
# transforming it (adjusting) to make it conform to the matrixset call. This is
# useful if it is already known that no adjustment is necessary and thus save
# some (mostly negligeable, though) time
set_meta <- function(side, meta, info, key, tag, adjust)
{
  side_name <- if (side == "row") "row_names_unique" else "col_names_unique"
  side_names <- info[[side_name]]
  side_label <- if (side == "row") "rows" else "columns"

  null_meta <- is.null(meta)

  if (null_meta) {
    nm <- if (is.null(side_names)) logical(0) else side_names
    meta <- data.frame(tmp_name = nm, stringsAsFactors = FALSE,
                       check.names = FALSE)
    names(meta) <- key
  }

  if (!is.data.frame(meta))
    stop(paste0("If provided, ", side, "_info must be a data frame"))

  col_names <- colnames(meta)

  if (!(key %in% col_names))
    stop(paste("There are no columns called", key, "in", side_label, "meta"))

  if (!is.null(side_names)) {
    index <- match(side_names, meta[[key]], 0)
    tmp_idx <- match(meta[[key]], side_names, 0)
    if (length(index) == 0L || any(index == 0L))
      stop(paste("Not all", side_label, "names are provided in meta info"))
    if (length(unique(tmp_idx)) != length(tmp_idx))
      stop(paste("some keys defined more than once in", side, "meta info"))
    if (adjust && any(tmp_idx == 0L))
      warning(paste0("Some meta info are not found in ", side_label, ". They will be ignored."))
    if (adjust && length(index) > 0) meta <- meta[index, , drop = FALSE]
  }

  # can't name key as tag if tag is already a column in the meta info. Unless
  # key was already called tag
  if (tag %in% col_names && key != tag)
    stop(paste("Column", tag, "already exists in", side, "meta info"))

  col_names[which(col_names == key)] <- tag
  colnames(meta) <- col_names

  meta
}



.matrixset <- function(matrix_set, row_info, col_info, n_row, n_col,
                       matrix_names, n_matrix, row_traits, col_traits,
                       row_names, col_names, row_tag, col_tag,
                       .class = "matrixset", row_group_meta = NULL,
                       row_group_indices = NULL, row_group_rows = NULL,
                       row_group_keys = NULL, row_group_vars = NULL,
                       row_group_level_drop = NULL, col_group_meta = NULL,
                       col_group_indices = NULL, col_group_rows = NULL,
                       col_group_keys = NULL, col_group_vars = NULL,
                       col_group_level_drop = NULL)
{

  structure(list(matrix_set = matrix_set,
                 row_info = row_info,
                 column_info = col_info),
            class = .class,
            n_row = n_row,
            n_col = n_col,
            matrix_names = matrix_names,
            n_matrix = n_matrix,
            row_traits = row_traits,
            col_traits = col_traits,
            row_names = row_names,
            col_names = col_names,
            row_tag = row_tag,
            col_tag = col_tag,
            row_group_meta = row_group_meta,
            row_group_indices = row_group_indices,
            row_group_rows = row_group_rows,
            row_group_keys = row_group_keys,
            row_group_vars = row_group_vars,
            row_group_level_drop = row_group_level_drop,
            col_group_meta = col_group_meta,
            col_group_indices = col_group_indices,
            col_group_rows = col_group_rows,
            col_group_keys = col_group_keys,
            col_group_vars = col_group_vars,
            col_group_level_drop = col_group_level_drop
            )
}




#' Matrix Set
#'
#' @description
#' Creates a matrix set, possibly annotated for rows and/or columns. These
#' annotations are referred as traits.
#'
#' @details
#' A `matrixset` is a collection of matrices that share the same dimensions and,
#' if applicable, dimnames. It is designed to hold different measures for the
#' same rows/columns. For example, each matrix could be a different time point
#' for the same subjects.
#'
#' Traits, which are annotations, can be provided in the form of data frames
#' for rows and/or columns. If traits are provided, the `data.frame` must
#' contain only one entry per row/column (see examples).
#'
#' Row or column names are not mandatory to create a proper `matrixset`. The
#' only way for this to work however is to leave traits (annotations) empty.
#' If provided, each matrices must have the same dimnames as well.
#'
#' If dimnames are missing, note that most of the operations for matrixsets
#' won't be available. For instance, operations that use traits will not work,
#' e.g., [filter_row()].
#'
#' It is allowed for matrix elements of a `matrixset` to be `NULL` - see
#' examples.
#'
#' @section Matrix Expansion:
#' The concept of matrix expansion allows to provide input matrices that do not
#' share the same dimensions.
#'
#' This works by taking the union of the dimnames and padding, if necessary,
#' each matrix with a special value for the missing rows/columns.
#'
#' Because the dimnames are used, they must necessarily be non-`NULL` in the
#' provided matrices.
#'
#' An interesting side-effect is that one can use this option to match the
#' dimnames and provide a common row/column order among the matrices.
#'
#' For base matrices, the padding special value is, by default
#' (`expand = TRUE`), `NA`. For the special matrices (Matrix package), the
#' default value is `0`. For these special matrices, padding with 0 forces
#' conversion to sparse matrix.
#'
#' The default value can be changed by providing any value (e.g, `-1`) to
#' `expand`, in which case the same padding value is used for all matrices.
#'
#' If different padding values are needed for each matrices, a list can be
#' provided to `expand`. If the list is unnamed, it must match the number of
#' input matrices in length and the padding values are assigned to the matrices
#' in order.
#'
#' A named list can be provided as well. In that case, `expand` names and
#' matrix names are matched. All matrices must have a match in the `expand` list
#' (more `expand` values can be provided, though).
#'
#' @param ...         A single list of matrices (must be a named list), or
#'                    individual matrices, e.g. `mat1 = m1`, `mat2 = m2`, etc.
#'                    `NULL` elements are accepted. This allows to create a
#'                    placeholder that can be filled later on.
#' @param match_names By default (`FALSE`), provided matrices must have their
#'                    dimnames matching exactly (i.e., in the same order), even
#'                    if their share all of their margin names.
#'                    Names will be matched to allow the creation of the
#'                    `matrixset` if `match_names` is set to `TRUE`.
#' @param expand      By default (`NULL`), input matrix expansion is disabled.
#'                    Setting this parameter to `TRUE` will enable the expansion
#'                    feature. See the section \sQuote{Matrix Expansion} for more
#'                    details of what it is, as well as other possible options
#'                    for `expand`. The section will also detail how the default
#'                    expansion value is dependent on the matrix types.
#' @param row_info    a data frame, used to annotate matrix rows. The link
#'                    between the matrix row names and the data frame is given
#'                    in column "rowname". A different column can be used if one
#'                    provides a different `row_key`.
#' @param column_info a data frame, used to annotate matrix columns. The link
#'                    between the matrix column names and the data frame is given
#'                    in column "colname". A different column can be used if one
#'                    provides a different `column_key`.
#' @param row_key     column name in `row_info`` data frame that will
#'                    link the row names with the row information. A string is
#'                    expected.
#' @param column_key  column name in `col_info` data frame that will
#'                    link the column names with the row information. A string is
#'                    expected.
#' @param row_tag     A string, giving the row annotation data frame column that
#'                    will link the row names to the data frame. While
#'                    `row_key` specifies the column name of the data frame
#'                    at input, `row_tag` specifies the column name that
#'                    will be used throughout in the `matrixset` object.
#' @param column_tag  A string, giving the column annotation data frame column
#'                    that will link the row names to the data frame. While
#'                    `column_key` specifies the column name of the data
#'                    frame at input, `column_tag` specifies the column
#'                    name that will be used throughout in the `matrixset`
#'                    object.
#'
#' @return
#' Returns a `matrixset`, a collection of matrices (see \sQuote{Details}).
#'
#' @seealso
#' [as_matrixset()]
#'
#' @examples
#' # A single NULL element will create an empty matrixset (it doesn't hold
#' # any matrices)
#' lst <- NULL
#' matrixset(lst)
#'
#' # This will hold to empty matrices
#' lst <- list(a = NULL, b = NULL)
#' matrixset(lst)
#' # this is equivalent
#' matrixset(a = NULL, b = NULL)
#'
#' # A basic example
#' lst <- list(a = matrix(0, 2, 3))
#' matrixset(lst)
#' # equivalent
#' matrixset(a = matrix(0, 2, 3))
#'
#' # can mix with NULL too
#' lst <- list(a = NULL, b = matrix(0, 2, 3), c = matrix(0, 2, 3))
#' matset <- matrixset(lst)
#'
#' # dimnames are also considered to be traits
#' lst <- list(a = NULL, b = matrix(0, 2, 3), c = matrix(0, 2, 3))
#' rownames(lst$b) <- c("r1", "r2")
#' rownames(lst$c) <- c("r1", "r2")
#' matrixset(lst)
#'
#' # You don't have to annotate both rows and columns. But you need to provide
#' # the appropriate dimnames when you provide traits
#' lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
#' rownames(lst$a) <- c("r1", "r2")
#' rownames(lst$b) <- c("r1", "r2")
#' colnames(lst$a) <- c("c1", "c2", "c3")
#' colnames(lst$b) <- c("c1", "c2", "c3")
#' ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
#' matset <- matrixset(lst, row_info = ri)
#'
#' # You can provide a column name that contains the keys
#' ri <- data.frame(foo = c("r1", "r2"), g = 1:2)
#' matset <- matrixset(lst, row_info = ri, row_key = "foo")
#'
#' lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
#' rownames(lst$a) <- c("r1", "r2")
#' rownames(lst$b) <- c("r1", "r2")
#' colnames(lst$a) <- c("c1", "c2", "c3")
#' colnames(lst$b) <- c("c1", "c2", "c3")
#' ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
#' ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
#' matset <- matrixset(lst, row_info = ri, column_info = ci)
#'
#' # This is not allowed, because the row trait data frame has more than one
#' # entry for "r1"
#' lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
#' rownames(lst$a) <- c("r1", "r2")
#' rownames(lst$b) <- c("r1", "r2")
#' colnames(lst$a) <- c("c1", "c2", "c3")
#' colnames(lst$b) <- c("c1", "c2", "c3")
#' ri <- data.frame(rowname = c("r1", "r2", "r1"), g = 1:3)
#' ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
#' ans <- tryCatch(matrixset(lst, row_info = ri, column_info = ci),
#'                 error = function(e) e)
#' is(ans, "error")
#'
#' @export
matrixset <- function(..., match_names = FALSE, expand = NULL, row_info = NULL,
                      column_info = NULL, row_key = "rowname",
                      column_key = "colname", row_tag = ".rowname",
                      column_tag = ".colname")
{
  matrix_set <- matrices_from_dots(...)

  names_matrix <- list_names(matrix_set)
  n_matrix <- length(matrix_set)
  expand <- normalize_expand(expand, names_matrix, n_matrix)

  # matrix_info <- info_matrices(matrix_set, expand)
  matrix_info <- MatrixMeta$new(matrix_set, match_names,
                                (is.logical(expand) && expand) || !is.null(expand) )
  if (match_names || !is.null(expand)) {
    matrix_set <- MatrixAdjuster$new(matrix_set, matrix_info, expand)$adjusted_mats
  }
    # matrix_set <- expand_matrices(matrix_set, matrix_info, expand)
# foo <- MatrixAdjuster$new(matrix_set, matrix_info, expand)
  row_info <- set_meta("row", row_info, matrix_info, row_key, row_tag, TRUE)
  col_info <- set_meta("col", column_info, matrix_info, column_key, column_tag, TRUE)

  rwtr <- colnames(row_info)
  cltr <- colnames(col_info)
  if (any(rwtr %in% cltr))
    stop("Rows and columns can't share annotation names")

  # matset <- list(matrix_set = matrix_set,
  #                row_info = if (is.null(row_info)) NULL else tibble::as_tibble(row_info),
  #                column_info = if (is.null(col_info)) NULL else tibble::as_tibble(col_info))

  row_names <-  matrix_info[["row_names_unique"]]
  if (is.null(row_names)) row_names <- character(0)
  # if (is.null(row_names)) row_names <- make_names(seq_len(matrix_info[["n_row"]]), "")
  col_names <-  matrix_info[["col_names_unique"]]
  if (is.null(col_names)) col_names <- character(0)
  # if (is.null(row_names)) col_names <- make_names(seq_len(matrix_info[["n_col"]]), "")


  # matset <- list(matrix_set = matrix_set,
  #                row_info = if (is.null(row_info)) NULL else tibble::as_tibble(row_info),
  #                column_info = if (is.null(col_info)) NULL else tibble::as_tibble(col_info))
  #
  #
  # structure(matset,
  #           class = "matrixset",
  #           n_row = matrix_info[["n_row"]],
  #           n_col = matrix_info[["n_col"]],
  #           matrix_names = names(matrix_set),
  #           n_matrix = n_matrix,
  #           row_traits = rwtr,
  #           col_traits = cltr,
  #           row_names = row_names,
  #           col_names = col_names,
  #           row_tag = row_tag,
  #           col_tag = column_tag)
  #

  .matrixset(matrix_set,
             row_info = if (is.null(row_info)) NULL else tibble::as_tibble(row_info),
             col_info = if (is.null(col_info)) NULL else tibble::as_tibble(col_info),
             n_row = matrix_info[["n_row"]],
             n_col = matrix_info[["n_col"]],
             matrix_names = names(matrix_set),
             n_matrix = n_matrix,
             row_traits = rwtr,
             col_traits = cltr,
             row_names = row_names,
             col_names = col_names,
             row_tag = row_tag,
             col_tag = column_tag)

}




#' Coerce object into `matrixset`
#'
#' @description
#' Turns object into a `matrixset`. See specific methods for more details
#'
#' @section Methods:
#' * `matrix`
#'
#'     The `matrix` method is very similar to calling the `matrixset`
#'       construction function, with some key differences:
#'     * A matrix name will be provided automatically by `as_matrixset`. The
#'       name is "..1".
#'     * Because only matrix is provided, the `expand` argument is not available
#'
#' * `list`
#'
#'     The `list` method is nearly identical to calling the `matrixset`
#'     construction function. It only differs in that unnamed `list` element
#'     will be padded with a name. The new padded names are the element index,
#'     prefixed by "..". Already existing names will be made unique as well. If
#'     name modification needs to be performed, a warning will be issued.
#'
#' @param x           an object to coerce to `matrixset`. See methods.
#' @param match_names By default (`FALSE`), provided matrices must have their
#'                    dimnames matching exactly (i.e., in the same order), even
#'                    if their share all of their margin names.
#'                    Names will be matched to allow the creation of the
#'                    `matrixset` if `match_names` is set to `TRUE`.
#' @param expand      By default (`NULL`), input matrix expansion is disabled.
#'                    Setting this parameter to `TRUE` will enable the expansion
#'                    feature. See the section \sQuote{Matrix Expansion} of
#'                    [matrixset()] for more details of what it is, as well as
#'                    other possible options for `expand`. Note as well that this
#'                    argument is not available for all methods.
#' @param row_info    a data frame, used to annotate matrix rows. The link
#'                    between the matrix row names and the data frame is given
#'                    in column "rowname". A different column can be used if one
#'                    provides a different `row_key`.
#' @param column_info a data frame, used to annotate matrix columns. The link
#'                    between the matrix column names and the data frame is given
#'                    in column "colname". A different column can be used if one
#'                    provides a different `column_key`.
#' @param row_key     column name in `row_info` data frame that will
#'                    link the row names with the row information. A string is
#'                    expected.
#' @param column_key  column name in `col_info` data frame that will
#'                    link the column names with the row information. A string is
#'                    expected.
#' @param row_tag     A string, giving the row annotation data frame column that
#'                    will link the row names to the data frame. While
#'                    `row_key` specifies the column name of the data frame
#'                    at input, `row_tag` specifies the column name that
#'                    will be used throughout in the `matrixset` object.
#' @param column_tag  A string, giving the column annotation data frame column
#'                    that will link the row names to the data frame. While
#'                    `column_key` specifies the column name of the data
#'                    frame at input, `column_tag` specifies the column
#'                    name that will be used throughout in the `matrixset`
#'                    object.
#'
#' @returns
#' Returns a `matrixset` - see [matrixset()].
#'
#' @examples
#' # We're showing how 'as_matrixset' can differ. But first, show how they can
#' # yield the same result. Note that the list is named
#' lst <- list(a = matrix(1:6, 2, 3), b = matrix(101:106, 2, 3))
#' identical(matrixset(lst), as_matrixset(lst))
#'
#' # Now it will differ: the list is unnamed. In fact, 'matrixset' will fail
#' lst <- list(matrix(1:6, 2, 3), matrix(101:106, 2, 3))
#' is(try(matrixset(lst), silent = TRUE), "try-error")
#' as_matrixset(lst)
#'
#' # You need to name the matrix to use 'matrixset'. A name is provided for you
#' # with 'as_matrixset'. But you can't control what it is.
#' as_matrixset(matrix(1:6, 2, 3))
#'
#' @export
as_matrixset <- function(x, match_names = FALSE, expand = NULL, row_info = NULL,
                         column_info = NULL, row_key = "rowname",
                         column_key = "colname", row_tag = ".rowname",
                         column_tag = ".colname")
  UseMethod("as_matrixset")

#' @export
as_matrixset.default <- function(x, match_names = FALSE, expand = NULL,
                                 row_info = NULL, column_info = NULL,
                                 row_key = "rowname", column_key = "colname",
                                 row_tag = ".rowname", column_tag = ".colname")
{
  if (methods::is(x, "Matrix")) {

    matrixset("..1" = x, row_info = row_info, column_info = column_info,
              row_key = row_key, column_key = column_key, row_tag = row_tag,
              column_tag = column_tag)

  } else stop(paste("objects of class", class(x), "are not supported"))
}


#' @export
as_matrixset.matrix <- function(x, match_names = FALSE, expand = NULL,
                                row_info = NULL, column_info = NULL,
                                row_key = "rowname", column_key = "colname",
                                row_tag = ".rowname", column_tag = ".colname")
{
  matrixset("..1" = x, row_info = row_info, column_info = column_info,
            row_key = row_key, column_key = column_key, row_tag = row_tag,
            column_tag = column_tag)
}

#' @export
as_matrixset.list <- function(x, match_names = FALSE, expand = NULL,
                              row_info = NULL, column_info = NULL,
                              row_key = "rowname", column_key = "colname",
                              row_tag = ".rowname", column_tag = ".colname")
{
  lst_nms <- make_names(x, "")
  names(x) <- lst_nms

  matrixset(x, match_names = match_names, expand = expand, row_info = row_info,
            column_info = column_info, row_key = row_key,
            column_key = column_key, row_tag = row_tag, column_tag = column_tag)
}


