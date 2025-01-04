

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


info_dim <- function(lst, side, expand)
{
  fn_names <- get(paste0(side, "names"), pos = "package:base", mode = "function")
  fn_n <- get(paste0("n", side), pos = "package:base", mode = "function")
  side_label <- if (side == "row") "row" else "column"

  need_expand <- FALSE

  # a note on empty dimnames: row/col names of empty dimnames is NULL; thus
  # unique(NULL) is NULL and length(NULL) is 0. so length(unique(NULL)) and
  # length(NULL) are both 0, thus equal. So empty dimnames is seen as not needing
  # expansion, as it should

  nms <- lapply(lst, fn_names)
  .names <- unique(nms)
  n_nms <- length(.names)
  if (n_nms != 1) {
    if (!expand)
      stop(paste("All matrices must have the same", side_label, "names (NULL accepted)"))
  }

  lapply(.names,
         function(.nms) {
           .names_unq <- unique(.nms)

           if (length(.names_unq) < length(.nms))
             stop(paste(stringr::str_to_title(side_label), "names must be unique"))

           if (any(.nms == ""))
             stop(paste("Empty", side_label, "names are not allowed"))
         })

  .names_flat <- unlist(.names)
  .names <- unique(.names_flat)
  nexp <- length(.names)
  if (nexp == 0L && expand)
    stop("matrices must have dimnames for expansion")
  if (n_nms > 1 && expand) need_expand <- TRUE

  n_s <- vapply(lst, fn_n, 0)
  n <- unique(n_s)
  N <- length(n)
  if (!expand && N != 1)
    stop(paste0("All matrices must have the same number of ", side_label, "s"))

  if (expand && need_expand) n <- nexp

  # if (expand) {
  #   n <- length(.names)
  # } else {
  #   n_s <- vapply(lst, fn_n, 0)
  #   n <- unique(n_s)
  #   N <- length(n)
  #   if (N != 1)
  #     stop(paste0("All matrices must have the same number of ", side_label, "s"))
  # }

  list(nms=.names, n=n, need_expand = need_expand)
}





list_names <- function(lst)
{
  if (is.null(lst)) return(NULL)
  lst_nms <- names(lst)
  if (is.null(lst_nms) || any(lst_nms == ""))
    stop("The list elements must be named")
  if (any(duplicated(lst_nms))) stop("matrix names must be unique")
  lst_nms
}





MatrixMeta <- R6::R6Class(
  "MatrixMeta",

  public = list(

    initialize = function(mat_lst, match_names = FALSE, expand = FALSE) {

      private$._matrix_list = mat_lst
      # private$._init_vecs()
      private$._expand <- expand
      private$._match_names <- expand || match_names
      private$._set_meta()

    }

  ),

  active = list(

    n_row = function() private$n_row_,
    n_col = function() private$n_col_,
    row_names = function() private$row_names_,
    col_names = function() private$col_names_,
    # need_row_expand = function() private$need_row_expand_,
    # need_row_expand = function() any(private$need_row_expand_per_mat_),
    need_row_expand = function() private$._cumul_need_expand("row"),
    # need_col_expand = function() private$need_col_expand_,
    # need_col_expand = function() any(private$need_col_expand_per_mat_),
    need_col_expand = function() private$._cumul_need_expand("col"),
    need_row_expand_per_mat = function() private$need_row_expand_per_mat_,
    need_col_expand_per_mat = function() private$need_col_expand_per_mat_,
    need_row_order_per_mat = function() private$need_row_order_per_mat_,
    need_col_order_per_mat = function() private$need_col_order_per_mat_,
    # need_expand = function() private$need_row_expand_ || private$need_col_expand_,
    need_expand = function() any(private$need_row_expand_per_mat_) || any(private$need_col_expand_per_mat_),
    need_order = function() any(private$need_row_order_per_mat_) || any(private$need_col_order_per_mat_),
    need_expand_per_mat = function() private$need_row_expand_per_mat_ | private$need_col_expand_per_mat_

  ),

  private = list(

    ._matrix_list = NULL,

    n_row_ = 0,
    n_col_ =  0,
    row_names_ = NULL,
    col_names_ = NULL,

    ._expand = FALSE,
    ._match_names = FALSE,
    # need_row_expand_ = FALSE,
    # need_col_expand_ = FALSE,
    need_row_expand_per_mat_ = NULL,
    need_col_expand_per_mat_ = NULL,
    need_row_order_per_mat_ = NULL,
    need_col_order_per_mat_ = NULL,


    ._margin_names_across_mats = NULL,     # utility object to store the
                                           # different margin names across
                                           # matrices




    # ._init_vecs = function() {
    #   n <- length(private$._matrix_list)
    #
    #   private$need_row_expand_per_mat_ = logical(n)
    #   private$need_col_expand_per_mat_ = logical(n)
    # },



    ._cumul_need_expand = function(margin) {
      itm <- paste("need", margin, "expand_per_mat_", sep = "_")
      any(private[[itm]])
    },



    ._assess_ordering = function(nms) {

      if (length(nms) < length(private$._margin_names_across_mats)) return(TRUE)
      if (any(nms != private$._margin_names_across_mats)) return(TRUE)
      FALSE

    },



    ._assess_expansion = function(nms) {

      if (length(nms) < length(private$._margin_names_across_mats)) return(TRUE)
      # if (any(nms != private$._margin_names_across_mats)) return(TRUE)
      FALSE

    },



    ._assess_margin_names_across_mats = function(margin_label) {

      lapply(private$._margin_names_across_mats,
             function(.nms) {
               .names_unq <- unique(.nms)

               if (length(.names_unq) < length(.nms))
                 stop(paste(stringr::str_to_title(margin_label), "names must be unique"))

               if (any(.nms == ""))
                 stop(paste("Empty", margin_label, "names are not allowed"))
             })

    },



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

      # mrg_nms_per_mat <- if (is.null(mat_subset)) {
      #   lapply(private$._matrix_list, margin_names_fn)
      # } else {
      #   lapply(private$._matrix_list[mat_subset], margin_names_fn)
      # }

      private$._margin_names_across_mats <- unique(mrg_nms_per_mat)

      n_margin_names_across_mats <- length(private$._margin_names_across_mats)

      if (n_margin_names_across_mats != 1) {
        if (!private$._match_names && !private$._expand) {
          stop(paste("All matrices must have the same", margin_label,
                     "names (NULL accepted)"))
        }

        # private[[paste("need", margin, "expand_", sep = "_")]] <- TRUE
      }

      private$._assess_margin_names_across_mats(margin_label)

      private$._margin_names_across_mats <- unique(unlist(private$._margin_names_across_mats))

      exp_name <- paste("need", margin, "expand_per_mat_", sep = "_")
      ord_name <- paste("need", margin, "order_per_mat_", sep = "_")
      private[[ord_name]] <- vapply(mrg_nms_per_mat_full,
                                    private$._assess_ordering,
                                    FALSE)
      private[[exp_name]] <- vapply(mrg_nms_per_mat_full,
                                    private$._assess_expansion,
                                    FALSE)
    },



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
      if (!private$._expand && N != 1)
        stop(paste0("All matrices must have the same number of ", margin_label, "s"))

      private[[paste0("n_", margin, "_")]] <- n_mrg
    },



    ._set_dimname = function(margin, mat_subset) {

      private$._get_margin_names_across_mats(mat_subset, margin)

      # private$._margin_names_across_mats <- unique(unlist(private$._margin_names_across_mats))
      n_expand <- length(private$._margin_names_across_mats)

      if (n_expand == 0L && private$._expand)
        stop("matrices must have dimnames for expansion")

      # With margin names provided, the following will have been tested already,
      # but since we (currently) allow for NULL names, this step is necessary in
      # this special case. Also, because of NULL names, assessing the number of
      # rows/columns will not work, so we must use a more direct approach
      private$._assess_num_margin_names_across_mats(mat_subset, margin)


      private[[paste(margin, "names_", sep = "_")]] <- private$._margin_names_across_mats
      # if (private[[paste("need", margin, "expand_", sep = "_")]]) {
      if (private$._cumul_need_expand(margin)) {
        private[[paste0("n_", margin, "_")]] <- n_expand
      }

    },




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

    }

  )
)







# get, from matrix list, the number of rows and columns. Assess if all matrices
# of same dim. Get also the matrix rownames and colnames. Assess if all matrices
# have the same names and if names are valid
# lst -  list of matrix
info_matrices <- function(lst = NULL, expand = NULL)
{
  n_row <- NULL
  n_col <- NULL
  need_expand <- FALSE

  if (is.null(lst)) {
    n_row <- 0
    n_col <-  0
    row_names <- NULL
    col_names <- NULL
  } else if (is.list(lst)) {

    # lst_nms <- names(lst)
    # if (is.null(lst_nms) || any(lst_nms == ""))
    #   stop("The list elements must be named")

    is_null <- vapply(lst, is.null, FALSE)
    if (all(is_null)) {
      n_row <- 0
      n_col <-  0
      row_names <- NULL
      col_names <- NULL
    } else {
      is_matrix <- vapply(lst, is_matrixish, FALSE)
      is_valid <- is_null | is_matrix
      if (!all(is_valid))
        stop("Elements must be NULL or a matrix")

      row_meta <- info_dim(lst[!is_null], "row", !is.null(expand))
      col_meta <- info_dim(lst[!is_null], "col", !is.null(expand))

      n_row <- row_meta$n
      n_col <- col_meta$n
      row_names <- row_meta$nms
      col_names <- col_meta$nms
      row_expand <- row_meta$need_expand
      col_expand <- col_meta$need_expand
      need_expand <- row_expand || col_expand

    }
  }

  return(list(n_row = as.integer(n_row), n_col = as.integer(n_col),
              row_names = row_names, col_names = col_names,
              need_expand = need_expand))
}



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



MATRIX <- function(dat, nrow, ncol, is_Matrix = FALSE)
{
  if (is_Matrix) {
    Matrix::Matrix(0, nrow=nrow, ncol=ncol)
  } else {
    matrix(dat, nrow=nrow, ncol=ncol)
  }
}



set_expand_value <- function(is_Matrix, exp_val)
{
  if (is.logical(exp_val) && !is.na(exp_val)) {
    if (is_Matrix) return(0) else return(NA)
  }
  exp_val
  # if (is_Matrix && (is.logical(exp_val) && !is.na(exp_val))) 0 else exp_val
}




MatrixAdjuster <- R6::R6Class(
  "MatrixAdjuster",

  public = list(

    initialize = function(matrix_list, matrix_info, expand) {

      private$._matrix_list <- matrix_list
      private$._target_info <- matrix_info
      private$._expand_param <- expand
      private$._expand()

    }
  ),


  active = list(

    expanded_mats = function() private$expanded_mats_

  ),


  private = list(

    ._matrix_list = NULL,
    expanded_mats_ = NULL,
    ._target_info = NULL,
    ._expand_param = NULL,
    ._expand_value = NULL,



    ._init = function() {

      private$expanded_mats_ <- vector('list', length(private$._matrix_list))
      names(private$expanded_mats_) <- names(private$._matrix_list)

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




    ._init_S4Matrix = function(mat_idx, padding_val) {

      NR <- nrow(private$._matrix_list[[mat_idx]])
      NC <- ncol(private$._matrix_list[[mat_idx]])

      private$expanded_mats_[[mat_idx]] <- Matrix::Matrix(0,
                                                          nrow = private$._target_info$n_row,
                                                          ncol = private$._target_info$n_col)
      private$expanded_mats_[[mat_idx]][] <- padding_val

      private$expanded_mats_[[mat_idx]] <- methods::as(private$expanded_mats_[[mat_idx]],
                                                       class(private$._matrix_list[[mat_idx]]))

      if ((NR < private$._target_info$n_row || NC < private$._target_info$n_col) &&
          ((is.integer(padding_val) && padding_val == 0L) ||
           (is.numeric(padding_val) && abs(padding_val) < .Machine$double.eps^.5)))
        private$expanded_mats_[[mat_idx]] <- methods::as(private$expanded_mats_[[mat_idx]],
                                                         "sparseMatrix")

    },




    ._init_matrix = function(mat_idx) {

      if (!private$._target_info$need_expand_per_mat[mat_idx]) {

        private$expanded_mats_[[mat_idx]] <- private$._matrix_list[[mat_idx]]

      } else {

        as_Matrix <- is(private$._matrix_list[[mat_idx]], "Matrix")
        padding_val <- private$._set_expand_value(mat_idx)

        if (as_Matrix) {

          private$._init_S4Matrix(mat_idx, padding_val)

        } else {

          private$expanded_mats_[[mat_idx]] <-
            matrix(padding_val, nrow = private$._target_info$n_row,
                   ncol = private$._target_info$n_col)


        }

      }

      rownames(private$expanded_mats_[[mat_idx]]) <- private$._target_info$row_names
      colnames(private$expanded_mats_[[mat_idx]]) <- private$._target_info$col_names

    },



    ._need_adjustment = function(margin, mat_idx) {

      look_for_expand <- paste("need", margin, "expand_per_mat", sep = "_")
      look_for_order <- paste("need", margin, "order_per_mat", sep = "_")

      need_expand <- private$._target_info[[look_for_expand]][mat_idx]
      need_order <- private$._target_info[[look_for_order]][mat_idx]

      need_expand || need_order

    },



    ._expand = function() {

      if (!private$._target_info$need_expand && !private$._target_info$need_order)
        return()


      private$._init()


      for (m in seq_along(private$._matrix_list)) {

        if (is.null(private$._matrix_list[[m]])) next

        private$._init_matrix(m)

        expand_order_rows <- private$._need_adjustment("row", m)
        expand_order_cols <- private$._need_adjustment("col", m)

        if (expand_order_cols) {
          old_cnms <- colnames(private$._matrix_list[[m]])

          coi <- match(private$._target_info$col_names, old_cnms, 0)
          cti <- which(private$._target_info$col_names %in% old_cnms)
        }



        if (expand_order_rows) {

          old_rnms <- rownames(private$._matrix_list[[m]])

          roi <- match(private$._target_info$row_names, old_rnms, 0)
          rti <- which(private$._target_info$row_names %in% old_rnms)

          if (expand_order_cols) {

            private$expanded_mats_[[m]][rti, cti] <- private$._matrix_list[[m]][roi, coi]
            next

          }

          private$expanded_mats_[[m]][rti, ] <- private$._matrix_list[[m]][roi, ]
          next
        }


        if (expand_order_cols) {

          private$expanded_mats_[[m]][, cti] <- private$._matrix_list[[m]][, coi]
          next

        }


      }


    }


  )
)





expand_matrices <- function(matrix_list, matrix_info, expand)
{
  need_expand <- matrix_info$need_expand
  if (!need_expand) return(matrix_list)

  nmatrix <- length(matrix_list)

  expand_list <- vector('list', nmatrix)
  names(expand_list) <- names(matrix_list)

  nr <- matrix_info$n_row
  nc <- matrix_info$n_col
  rnms <- matrix_info$row_names
  cnms <- matrix_info$col_names
  for (l in 1:nmatrix) {
    NR <- nrow(matrix_list[[l]])
    NC <- ncol(matrix_list[[l]])
    if (!is.null(matrix_list[[l]])) {
      is_Matrix <- is(matrix_list[[l]], "Matrix")

      if (NR != nr || NC != nc) {
        expand_value <- set_expand_value(is_Matrix, if (is.logical(expand)) expand else expand[[l]])
        expand_list[[l]] <- MATRIX(expand_value, nrow = nr, ncol = nc, is_Matrix)
      } else {
        expand_list[[l]] <- matrix_list[[l]]
      }

      rownames(expand_list[[l]]) <- rnms
      colnames(expand_list[[l]]) <- cnms
      if (is_Matrix) {
        expand_list[[l]][] <- expand_value
        expand_list[[l]] <- methods::as(expand_list[[l]], class(matrix_list[[l]]))
        if ((NR < nr || NC < nc) &&
            ((is.integer(expand_value) && expand_value == 0L) ||
             (is.numeric(expand_value) && abs(expand_value) < .Machine$double.eps^.5)))
          expand_list[[l]] <- methods::as(expand_list[[l]], "sparseMatrix")
      }
      old_rnms <- rownames(matrix_list[[l]])
      old_cnms <- colnames(matrix_list[[l]])
      ridx <- rnms %in% old_rnms
      cidx <- cnms %in% old_cnms
      ri <- match(rnms, old_rnms, 0)
      ci <- match(cnms, old_cnms, 0)
      if (any(ridx) && any(cidx) && (any(ri != seq_along(ri)) || any(ci != seq_along(ci)))) {
        expand_list[[l]][rnms[ridx], cnms[cidx]] <- matrix_list[[l]][ri, ci]
      }


      # is_Matrix <- is(matrix_list[[l]], "Matrix")
      # expand_value <- set_expand_value(is_Matrix, if (is.logical(expand)) expand else expand[[l]])
      # expand_list[[l]] <- MATRIX(expand_value, nrow = nr, ncol = nc, is_Matrix)
      # # expand_list[[l]] <- MATRIX(expand[[l]], nrow = nr, ncol = nc, is_Matrix)
      # rownames(expand_list[[l]]) <- rnms
      # colnames(expand_list[[l]]) <- cnms
      # if (is_Matrix) {
      #   # expand_list[[l]][] <- expand[[l]]
      #   expand_list[[l]][] <- expand_value
      #   expand_list[[l]] <- methods::as(expand_list[[l]], class(matrix_list[[l]]))
      #   if ((NR < nr || NC < nc) &&
      #       ((is.integer(expand_value) && expand_value == 0L) ||
      #        (is.numeric(expand_value) && abs(expand_value) < .Machine$double.eps^.5)))
      #     expand_list[[l]] <- methods::as(expand_list[[l]], "sparseMatrix")
      # }
      # old_rnms <- rownames(matrix_list[[l]])
      # old_cnms <- colnames(matrix_list[[l]])
      # ridx <- rnms %in% old_rnms
      # cidx <- cnms %in% old_cnms
      # ri <- match(rnms, old_rnms, 0)
      # ci <- match(cnms, old_cnms, 0)
      # if (any(ridx) && any(cidx)) {
      #   expand_list[[l]][rnms[ridx], cnms[cidx]] <- matrix_list[[l]][ri, ci]
      # }
    }
  }

  expand_list
}



# the 'adjust' parameter allows to validate some elements of 'meta' without
# transforming it (adjusting) to make it conform to the matrixset call. This is
# useful if it is already known that no adjustment is necessary and thus save
# some (mostly negligeable, though) time
set_meta <- function(side, meta, info, key, tag, adjust)
{
  side_name <- if (side == "row") "row_names" else "col_names"
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
    matrix_set <- MatrixAdjuster$new(matrix_set, matrix_info, expand)$expanded_mats
  }
    # matrix_set <- expand_matrices(matrix_set, matrix_info, expand)
# foo <- MatrixAdjuster$new(matrix_set, matrix_info, expand)
  row_info <- set_meta("row", row_info, matrix_info, row_key, row_tag, TRUE)
  col_info <- set_meta("col", column_info, matrix_info, column_key, column_tag, TRUE)

  rwtr <- colnames(row_info)
  cltr <- colnames(col_info)
  if (any(rwtr %in% cltr))
    stop("Rows and columns can't share annotation names")

  matset <- list(matrix_set = matrix_set,
                 row_info = if (is.null(row_info)) NULL else tibble::as_tibble(row_info),
                 column_info = if (is.null(col_info)) NULL else tibble::as_tibble(col_info))

  row_names <-  matrix_info[["row_names"]]
  if (is.null(row_names)) row_names <- character(0)
  # if (is.null(row_names)) row_names <- make_names(seq_len(matrix_info[["n_row"]]), "")
  col_names <-  matrix_info[["col_names"]]
  if (is.null(col_names)) col_names <- character(0)
  # if (is.null(row_names)) col_names <- make_names(seq_len(matrix_info[["n_col"]]), "")

  structure(matset,
            class = "matrixset",
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


