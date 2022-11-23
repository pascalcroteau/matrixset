


# mrg == TRUE means that functions are applied to margin (row or column).
# Otherwise the whole matrix is taken as input
set_msub_expr <- function(rgr, cgr, mrg = TRUE)
{
  if (mrg) {
    if (rgr) {
      if (cgr) quote(.m[gr, gc]) else quote(.m[gr, ])
    } else {
      if (cgr) quote(.m[, gc]) else quote(.m)
    }
  } else {
    if (rgr) {
      if (cgr) quote(.m[gr, gc, drop = FALSE]) else quote(.m[gr, , drop = FALSE])
    } else {
      if (cgr) quote(.m[, gc, drop = FALSE]) else quote(.m)
    }
  }

}



ms_mask <- R6::R6Class("ms_mask",

                      public = list(

                        # .gridx is the result of row_group_where (or column)
                        initialize = function(MARGIN, .mats, .rowinf, .colinf,
                                              ENV, .gridx_row = NULL,
                                              .gridx_col = NULL) {

                          frame <- rlang::caller_env(2)
                          context_add(frame)

                          private$.margin <- MARGIN
                          private$.row_info_names <- names(.rowinf)
                          private$.col_info_names <- names(.colinf)

                          private$.enclos_dat <- vector("list", length(.mats))
                          names(private$.enclos_dat) <- names(.mats)

                          label <- if (MARGIN == "row") ".i" else if (MARGIN == "col") ".j" else ".m"
                          private$.dat_names <- c(label, ".__is_null_")

                          grouped_row <- !is.null(.gridx_row)
                          grouped_col <- !is.null(.gridx_col)

                          # .mexpr <- set_msub_expr(grouped_row, grouped_col)
                          .mexpr <- set_msub_expr(grouped_row, grouped_col, MARGIN != "mat")

                          if (!grouped_row) .gridx_row <- list(1L)
                          if (!grouped_col) .gridx_col <- list(1L)

                          private$.row_info <- vector("list", length(.gridx_row))
                          private$.col_info <- vector("list", length(.gridx_col))

                          private$.row_idx <- vector("list", length(.gridx_row))
                          private$.col_idx <- vector("list", length(.gridx_col))



                          for (midx in seq_along(.mats)) {
                            private$.enclos_dat[[midx]] <- vector("list", length(.gridx_row))

                            .m <- .mats[[midx]]
                            for (grrow in seq_along(.gridx_row)) {
                              private$.enclos_dat[[midx]][[grrow]] <- vector("list", length(.gridx_col))

                              gr <- if (grouped_row) .gridx_row[[grrow]] else seq_len(nrow(.rowinf))
                              private$.row_info[[grrow]] <- if (grouped_row) as.list(.rowinf[gr, ]) else as.list(.rowinf)
                              private$.row_idx[[grrow]] <- gr

                              for (grcol in seq_along(.gridx_col)) {
                                gc <- if (grouped_col) .gridx_col[[grcol]] else seq_len(nrow(.colinf))
                                # .mexpr <- set_msub_expr(grouped_row, grouped_col)
                                private$.enclos_dat[[midx]][[grrow]][[grcol]] <- c(list(eval(.mexpr)),
                                                                                      is.null(.m))
                                names(private$.enclos_dat[[midx]][[grrow]][[grcol]]) <- private$.dat_names
                                if (grrow == 1) {
                                  private$.col_info[[grcol]] <- if (grouped_col) as.list(.colinf[gc, ]) else as.list(.colinf)
                                  private$.col_idx[[grcol]] <- gc
                                }
                              }
                            }
                          }

                          private$.enclos <- new.env()
                          private$.mask <- rlang::new_data_mask(private$.enclos)
                          private$.mask$.data <- rlang::as_data_pronoun(private$.mask)

                          private$.env <- ENV

                        },


                        update = function(mat, gr_idx_row = NULL,
                                          gr_idx_col = NULL) {

                          if (is.null(gr_idx_row)) gr_idx_row <- 1
                          if (is.null(gr_idx_col)) gr_idx_col <- 1

                          new_mat <- mat != private$.prev_mat
                          new_group_row <- gr_idx_row != private$.prev_gr_row
                          new_group_col <- gr_idx_col != private$.prev_gr_col

                          if (new_group_row) {
                            private$.prev_gr_row <- gr_idx_row

                            for (nm in private$.row_info_names)
                              assign(nm, private$.row_info[[gr_idx_row]][[nm]], private$.enclos)
                          }


                          if (new_group_col) {
                            private$.prev_gr_col <- gr_idx_col

                            for (nm in private$.col_info_names)
                              assign(nm, private$.col_info[[gr_idx_col]][[nm]], private$.enclos)
                          }



                          if (new_mat || new_group_row || new_group_col) {

                            if (new_mat) private$.prev_mat <- mat

                            for (nm in private$.dat_names)
                              assign(nm, private$.enclos_dat[[mat]][[gr_idx_row]][[gr_idx_col]][[nm]], private$.enclos)
                          }

                        },



                        eval = function(quoS, .simplify = FALSE) {
                          v <- lapply(quoS,
                                      function(q) {
                                        if (eval(quote(.__is_null_), private$.enclos)) {
                                          if (.simplify) ._NULL_ else NULL
                                        } else {
                                          rlang::eval_tidy(q, private$.mask, private$.env)
                                        }
                                      })
                          names(v) <- names(quoS)

                          is_vect <- sapply(v, is.vector)
                          lens <- mapply(function(vl, lgl) if (is_null_obj(vl)) -1 else if(lgl) length(vl) else 0, v, is_vect)

                          list(v=v, lens=lens)
                        },



                        current_row_info = function() {
                          gr_idx <- private$.prev_gr_row
                          if (gr_idx == 0) gr_idx <- 1
                          tibble::as_tibble(private$.row_info[[gr_idx]])
                        },


                        current_column_info = function() {
                          gr_idx <- private$.prev_gr_col
                          if (gr_idx == 0) gr_idx <- 1
                          tibble::as_tibble(private$.col_info[[gr_idx]])
                        },


                        current_n_row = function() {
                          gr_idx <- private$.prev_gr_row
                          if (gr_idx == 0) gr_idx <- 1
                          length(private$.row_info[[gr_idx]][[1]])
                        },


                        current_n_col = function() {
                          gr_idx <- private$.prev_gr_col
                          if (gr_idx == 0) gr_idx <- 1
                          length(private$.col_info[[gr_idx]][[1]])
                        },


                        row_pos = function() {
                          gr_idx <- private$.prev_gr_row
                          if (gr_idx == 0) gr_idx <- 1
                          private$.row_idx[[gr_idx]]
                        },


                        row_rel_pos = function() {
                          seq_len(self$current_n_row())
                        },


                        col_pos = function() {
                          gr_idx <- private$.prev_gr_col
                          if (gr_idx == 0) gr_idx <- 1
                          private$.col_idx[[gr_idx]]
                        },


                        col_rel_pos = function() {
                          seq_len(self$current_n_col())
                        },


                        clean = function() context_del()

                      ),

                      private = list(
                        .margin = NULL,
                        .row_info = NULL,
                        .col_info = NULL,
                        .row_info_names = NULL,
                        .col_info_names = NULL,
                        .row_idx = NULL,
                        .col_idx = NULL,
                        .prev_mat = 0,
                        .prev_gr_row = 0,
                        .prev_gr_col = 0,
                        .enclos_dat = NULL,
                        .dat_names = NULL,
                        .enclos = NULL,
                        .mask = NULL,
                        .env = NULL
                      )

)




# set_mats_sub_expr <- function(rgr, cgr, mrg = TRUE)
# {
#   if (mrg) {
#     if (rgr) {
#       if (cgr) quote(lapply(seq_mats, function(.m) .mats[[.m]][gr, gc])) else quote(lapply(seq_mats, function(.m) .mats[[.m]][gr, ]))
#     } else {
#       if (cgr) quote(lapply(seq_mats, function(.m) .mats[[.m]][, gc])) else quote(.mats)
#     }
#   } else {
#     if (rgr) {
#       if (cgr) quote(lapply(seq_mats, function(.m) .mats[[.m]][gr, gc, drop = FALSE])) else quote(lapply(seq_mats, function(.m) .mats[[.m]][gr, , drop = FALSE]))
#     } else {
#       if (cgr) quote(lapply(seq_mats, function(.m) .mats[[.m]][, gc, drop = FALSE])) else quote(.mats)
#     }
#   }
#
# }



set_mats_sub_expr <- function(rgr, cgr, margin = NULL)
{
  mrg <- margin != "mat"


  if (mrg) {
    if (rgr) {
      if (cgr) {
        nms_expr <- if (margin == "row") quote(colnames(M)[gc]) else quote(rownames(M)[gr])
        substitute(
          lapply(seq_mats, function(.m) {
            M <- .mats[[.m]]
            Mout <- M[gr, gc]
            names(Mout) <- nms
            Mout
          })
        , list(nms = nms_expr))
      }  else {
        quote(
          lapply(seq_mats, function(.m) .mats[[.m]][gr, ])
          )
      }
    } else {
      if (cgr) quote(lapply(seq_mats, function(.m) .mats[[.m]][, gc])) else quote(.mats)
    }
  } else {
    if (rgr) {
      if (cgr) quote(lapply(seq_mats, function(.m) .mats[[.m]][gr, gc, drop = FALSE])) else quote(lapply(seq_mats, function(.m) .mats[[.m]][gr, , drop = FALSE]))
    } else {
      if (cgr) quote(lapply(seq_mats, function(.m) .mats[[.m]][, gc, drop = FALSE])) else quote(.mats)
    }
  }

}





ms_mult_mask <- R6::R6Class("ms_mult_mask",

                       public = list(

                         # .gridx is the result of row_group_where (or column)
                         initialize = function(MARGIN, .mats, .rowinf, .colinf,
                                               ENV, as_list = FALSE,
                                               .gridx_row = NULL,
                                               .gridx_col = NULL) {

                           frame <- rlang::caller_env(2)
                           context_add(frame)

                           private$.margin <- MARGIN
                           private$.row_info_names <- names(.rowinf)
                           private$.col_info_names <- names(.colinf)

                           nmats <- length(.mats)
                           seq_mats <- seq_len(nmats)

                           label <- if (MARGIN == "row") ".i" else if (MARGIN == "col") ".j" else ".m"

                           if (as_list) {
                             mat_nms <- names(.mats)
                             private$.dat_names <- label
                           } else {
                             mat_nms <- label
                             mat_nms <- paste0(mat_nms, seq_mats)
                             private$.dat_names <- mat_nms
                           }

                           names(seq_mats) <- mat_nms

                           grouped_row <- !is.null(.gridx_row)
                           grouped_col <- !is.null(.gridx_col)

                           # .mexpr <- set_mats_sub_expr(grouped_row, grouped_col, MARGIN != "mat")
                           .mexpr <- set_mats_sub_expr(grouped_row, grouped_col, MARGIN)

                           if (!grouped_row) .gridx_row <- list(1L)
                           if (!grouped_col) .gridx_col <- list(1L)

                           private$.enclos_dat <- vector("list", length(.gridx_row))

                           private$.row_info <- vector("list", length(.gridx_row))
                           private$.col_info <- vector("list", length(.gridx_col))

                           private$.row_idx <- vector("list", length(.gridx_row))
                           private$.col_idx <- vector("list", length(.gridx_col))


                           for (grrow in seq_along(.gridx_row)) {
                             private$.enclos_dat[[grrow]] <- vector("list", length(.gridx_col))

                             gr <- if (grouped_row) .gridx_row[[grrow]] else seq_len(nrow(.rowinf))
                             private$.row_info[[grrow]] <- if (grouped_row) as.list(.rowinf[gr, ]) else as.list(.rowinf)
                             private$.row_idx[[grrow]] <- gr

                             for (grcol in seq_along(.gridx_col)) {
                               gc <- if (grouped_col) .gridx_col[[grcol]] else seq_len(nrow(.colinf))

                               mat_lst <- eval(.mexpr)
                               if (as_list) {
                                 private$.enclos_dat[[grrow]][[grcol]] <- list(mat_lst)
                                 names(private$.enclos_dat[[grrow]][[grcol]]) <- label
                               } else {
                                 private$.enclos_dat[[grrow]][[grcol]] <- mat_lst
                               }

                               if (grrow == 1) {
                                 private$.col_info[[grcol]] <- if (grouped_col) as.list(.colinf[gc, ]) else as.list(.colinf)
                                 private$.col_idx[[grcol]] <- gc
                               }
                             }
                           }


                           private$.enclos <- new.env()
                           private$.mask <- rlang::new_data_mask(private$.enclos)
                           private$.mask$.data <- rlang::as_data_pronoun(private$.mask)

                           private$.env <- ENV

                         },


                         update = function(gr_idx_row = NULL,
                                           gr_idx_col = NULL) {

                           if (is.null(gr_idx_row)) gr_idx_row <- 1
                           if (is.null(gr_idx_col)) gr_idx_col <- 1

                           new_group_row <- gr_idx_row != private$.prev_gr_row
                           new_group_col <- gr_idx_col != private$.prev_gr_col

                           if (new_group_row) {
                             private$.prev_gr_row <- gr_idx_row

                             for (nm in private$.row_info_names)
                               assign(nm, private$.row_info[[gr_idx_row]][[nm]], private$.enclos)
                           }


                           if (new_group_col) {
                             private$.prev_gr_col <- gr_idx_col

                             for (nm in private$.col_info_names)
                               assign(nm, private$.col_info[[gr_idx_col]][[nm]], private$.enclos)
                           }



                           if (new_group_row || new_group_col) {

                             for (nm in private$.dat_names)
                               assign(nm, private$.enclos_dat[[gr_idx_row]][[gr_idx_col]][[nm]], private$.enclos)
                           }


                         },



                         eval = function(quoS, .simplify = FALSE) {
                           v <- lapply(quoS,
                                       function(q) {
                                         rlang::eval_tidy(q, private$.mask, private$.env)
                                       })
                           names(v) <- names(quoS)

                           is_vect <- sapply(v, is.vector)
                           lens <- mapply(function(vl, lgl) if (is.null(vl)) -1 else if(lgl) length(vl) else 0, v, is_vect)

                           list(v=v, lens=lens)
                         },



                         current_row_info = function() {
                           gr_idx <- private$.prev_gr_row
                           if (gr_idx == 0) gr_idx <- 1
                           tibble::as_tibble(private$.row_info[[gr_idx]])
                         },


                         current_column_info = function() {
                           gr_idx <- private$.prev_gr_col
                           if (gr_idx == 0) gr_idx <- 1
                           tibble::as_tibble(private$.col_info[[gr_idx]])
                         },


                         current_n_row = function() {
                           gr_idx <- private$.prev_gr_row
                           if (gr_idx == 0) gr_idx <- 1
                           length(private$.row_info[[gr_idx]][[1]])
                         },


                         current_n_col = function() {
                           gr_idx <- private$.prev_gr_col
                           if (gr_idx == 0) gr_idx <- 1
                           length(private$.col_info[[gr_idx]][[1]])
                         },


                         row_pos = function() {
                           gr_idx <- private$.prev_gr_row
                           if (gr_idx == 0) gr_idx <- 1
                           private$.row_idx[[gr_idx]]
                         },


                         row_rel_pos = function() {
                           seq_len(self$current_n_row())
                         },


                         col_pos = function() {
                           gr_idx <- private$.prev_gr_col
                           if (gr_idx == 0) gr_idx <- 1
                           private$.col_idx[[gr_idx]]
                         },


                         col_rel_pos = function() {
                           seq_len(self$current_n_col())
                         },


                         clean = function() context_del()

                       ),

                       private = list(
                         .margin = NULL,
                         .row_info = NULL,
                         .col_info = NULL,
                         .row_info_names = NULL,
                         .col_info_names = NULL,
                         .row_idx = NULL,
                         .col_idx = NULL,
                         .prev_gr_row = 0,
                         .prev_gr_col = 0,
                         .enclos_dat = NULL,
                         .dat_names = NULL,
                         .enclos = NULL,
                         .mask = NULL,
                         .env = NULL
                       )

)









norm_call <- function(quo, var, .convert_name = TRUE)
{
  expr <- rlang::quo_get_expr(quo)

  if (rlang::is_formula(expr)) {
    expr <- rlang::f_rhs(expr)
    if (!(is.call(expr) && is(expr, "{"))) expr <- rlang::call2("{", expr)
  }

  colon <- FALSE
  if (rlang::is_call(expr, "::")) {
    pkg <- expr[[2]]
    expr <- expr[[3]]
    colon <- TRUE
  }
  if (is.name(expr)) {
    if (!.convert_name)
      stop("function names are not accepted in this context", call. = FALSE)
    expr <- rlang::call2(expr, !!!rlang::syms(var))
  }
  if (colon) expr <- call("::", pkg, expr)
  rlang::quo_set_expr(quo, expr)
}





#' @importFrom rlang :=
eval_fun <- function(margin, ms, ..., matidx, row_first, .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  if (is.null(ms$matrix_set)) return(NULL)

  wide <- NULL
  if (!is.logical(.simplify)) {
    if (.simplify == "wide") wide <- TRUE else wide <- FALSE
    .simplify <- TRUE
  }



  if (margin == "row") {
    gr_idx_row <- as.list(seq_len(nrow(ms)))
    gr_idx_col <- column_group_where(ms)
    gr_lbl_row <- rownames(ms)
    gr_lbl_col <- NULL
  } else if (margin == "col") {
    gr_idx_row <- row_group_where(ms)
    gr_idx_col <- as.list(seq_len(ncol(ms)))
    gr_lbl_row <- NULL
    gr_lbl_col <- colnames(ms)
  } else {
    gr_idx_row <- row_group_where(ms)
    gr_idx_col <- column_group_where(ms)
    gr_lbl_row <- NULL
    gr_lbl_col <- NULL
  }


  if (is.null(matidx)) {
    nmat <- .nmatrix(ms)
    matnms <- matrixnames(ms)
    enclos <- ms_mask$new(margin, ms$matrix_set, ms$row_info,
                          ms$column_info, env, gr_idx_row, gr_idx_col)
  } else {
    matidx <- index_to_integer(matidx, nmatrix(ms), matrixnames(ms))
    nmat <- length(matidx)
    matnms <- matrixnames(ms)[matidx]
    enclos <- ms_mask$new(margin, ms$matrix_set[matidx], ms$row_info,
                          ms$column_info, env, gr_idx_row, gr_idx_col)
  }

  on.exit(enclos$clean(), add = TRUE)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")
  var_lab <- switch(margin, "row" = ".i", "col"=".j", "mat" = ".m")

  for (i in seq_along(quosures)) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }


  v <- vector("list", nmat)
  names(v) <- matnms


  ngroup_row <- length(gr_idx_row)
  grouprow <- vector("list", ngroup_row)
  if (ngroup_row == 0L) ngroup_row <- 1L
  if (!is.null(gr_lbl_row)) names(grouprow) <- gr_lbl_row

  ngroup_col <- length(gr_idx_col)
  groupcol <- vector("list", ngroup_col)
  if (ngroup_col == 0L) ngroup_col <- 1L
  if (!is.null(gr_lbl_col)) names(groupcol) <- gr_lbl_col


  seq1 <- if (row_first) seq(ngroup_row) else seq(ngroup_col)
  seq2 <- if (row_first) seq(ngroup_col) else seq(ngroup_row)


  for (k in 1:nmat)
  {
    # for (gr_row in seq(ngroup_row))
    for (gr1 in seq1)
    {
      # for (gr_col in seq(ngroup_col))
      for (gr2 in seq2)
      {
        gr_row <- if (row_first) gr1 else gr2
        gr_col <- if (row_first) gr2 else gr1

        l <- NULL
        enclos$update(k, gr_row, gr_col)
        pts <- enclos$eval(quosures, .simplify)
        l <- union(l, unique(pts$lens))
        vals <- pts$v


        if (.simplify) {

          if (length(l) == 1L && l > 0) {

            if (wide) {

              vals <- purrr::imap_dfc(vals, function(v, nm) {
                if (l > 1) {
                  names(v) <- concat(nm, make_names(v, ""), sep = " ")
                  tibble::tibble_row(!!!v)
                } else tibble::tibble(!!as.name(nm) := v)
              })

            } else {

              vals <- purrr::imap_dfc(vals, function(v, nm) {
                if (l > 1) {
                  nms <- make_names(v, "")
                  names(v) <- NULL
                  tibble::tibble(!!as.name(paste0(nm, ".name")) := nms,
                                 !!as.name(nm) := v)
                }
                else tibble::tibble(!!as.name(nm) := v)

              })
            }
          } else {
            if (length(l) > 1)
              stop("vectors must be of the same length", call. = FALSE)
            if (l == 0L) stop("function results must be non-empty vectors")
          }
        }

        if (row_first) {
          if (is.null(gr_idx_col)) groupcol <- vals else groupcol[[gr_col]] <- vals
        } else {
          if (is.null(gr_idx_row)) grouprow <- vals else grouprow[[gr_row]] <- vals
        }

      }
      if (row_first) {
        if (is.null(gr_idx_row)) grouprow <- groupcol else grouprow[[gr_row]] <- groupcol
      } else {
        if (is.null(gr_idx_col)) groupcol <- grouprow else groupcol[[gr_col]] <- grouprow
      }

    }
    v[[k]] <- if (row_first) grouprow else groupcol
  }

  v
}







#' @importFrom rlang :=
eval_fun_mult <- function(margin, ms, ..., matidx, row_first, list_input,
                          .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  if (is.null(ms$matrix_set)) return(NULL)

  wide <- NULL
  if (!is.logical(.simplify)) {
    if (.simplify == "wide") wide <- TRUE else wide <- FALSE
    .simplify <- TRUE
  }



  if (margin == "row") {
    gr_idx_row <- as.list(seq_len(nrow(ms)))
    gr_idx_col <- column_group_where(ms)
    gr_lbl_row <- rownames(ms)
    gr_lbl_col <- NULL
  } else if (margin == "col") {
    gr_idx_row <- row_group_where(ms)
    gr_idx_col <- as.list(seq_len(ncol(ms)))
    gr_lbl_row <- NULL
    gr_lbl_col <- colnames(ms)
  } else {
    gr_idx_row <- row_group_where(ms)
    gr_idx_col <- column_group_where(ms)
    gr_lbl_row <- NULL
    gr_lbl_col <- NULL
  }



  if (is.null(matidx)) {
    nmat <- .nmatrix(ms)
    matnms <- matrixnames(ms)
    enclos <- ms_mult_mask$new(margin, ms$matrix_set, ms$row_info,
                               ms$column_info, env, as_list = list_input,
                               gr_idx_row, gr_idx_col)
  } else {
    matidx <- index_to_integer(matidx, nmatrix(ms), matrixnames(ms))
    nmat <- length(matidx)
    matnms <- matrixnames(ms)[matidx]
    enclos <- ms_mult_mask$new(margin, ms$matrix_set[matidx], ms$row_info,
                               ms$column_info, env, as_list = list_input,
                               gr_idx_row, gr_idx_col)
  }

  on.exit(enclos$clean(), add = TRUE)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")
  var_lab <- switch(margin, "row" = ".i", "col"=".j", "mat" = ".m")
  if (!list_input) {
    seq_mat <- seq(nmat)
    var_lab <- paste0(var_lab, seq_mat)
  }

  for (i in seq_along(quosures)) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }

  ngroup_row <- length(gr_idx_row)
  grouprow <- vector("list", ngroup_row)
  ngroup_col <- length(gr_idx_col)
  groupcol <- vector("list", ngroup_col)

  if (margin == "row") {
    grouped_col <- ngroup_col > 0
    grouped_row <- FALSE
  } else if (margin == "col") {
    grouped_col <- FALSE
    grouped_row <- ngroup_row > 0
  } else {
    grouped_col <- ngroup_col > 0
    grouped_row <- ngroup_row > 0
  }
  grouped <- grouped_row || grouped_col

  if (ngroup_row == 0L) ngroup_row <- 1L
  if (!is.null(gr_lbl_row)) names(grouprow) <- gr_lbl_row
  if (ngroup_col == 0L) ngroup_col <- 1L
  if (!is.null(gr_lbl_col)) names(groupcol) <- gr_lbl_col


  seq1 <- if (row_first) seq(ngroup_row) else seq(ngroup_col)
  seq2 <- if (row_first) seq(ngroup_col) else seq(ngroup_row)



  # for (gr_row in seq(ngroup_row))
  for (gr1 in seq1)
  {
    # for (gr_col in seq(ngroup_col))
    for (gr2 in seq2)
    {
      gr_row <- if (row_first) gr1 else gr2
      gr_col <- if (row_first) gr2 else gr1

      l <- NULL
      enclos$update(gr_row, gr_col)
      pts <- enclos$eval(quosures, .simplify)
      l <- union(l, unique(pts$lens))
      vals <- pts$v


      if (.simplify) {

        if (length(l) == 1L && l > 0) {

          if (wide) {

            vals <- purrr::imap_dfc(vals, function(v, nm) {
              if (l > 1 || grouped) {
                names(v) <- concat(nm, make_names(v, ""), sep = " ")
                tibble::tibble_row(!!!v)
              } else {
                if (is.vector(v) && !is.list(v)) v <-  unname(v)
                tibble::tibble(!!as.name(nm) := v)
              }
            })

          } else {

            vals <- purrr::imap_dfc(vals, function(v, nm) {
              if (l > 1 || grouped) {
                nms <- make_names(v, "")
                names(v) <- NULL
                tibble::tibble(!!as.name(paste0(nm, ".name")) := nms,
                               !!as.name(nm) := v)
              }
              else {
                if (is.vector(v) && !is.list(v)) v <-  unname(v)
                tibble::tibble(!!as.name(nm) := v)
              }

            })
          }
        } else {
          if (length(l) > 1)
            stop("vectors must be of the same length", call. = FALSE)
          if (l == 0L) stop("function results must be non-empty vectors")
        }
      }

      if (row_first) {
        if (is.null(gr_idx_col)) groupcol <- vals else groupcol[[gr_col]] <- vals
      } else {
        if (is.null(gr_idx_row)) grouprow <- vals else grouprow[[gr_row]] <- vals
      }

    }
    if (row_first) {
      if (is.null(gr_idx_row)) grouprow <- groupcol else grouprow[[gr_row]] <- groupcol
    } else {
      if (is.null(gr_idx_col)) groupcol <- grouprow else groupcol[[gr_col]] <- grouprow
    }

  }

  v <- if (row_first) grouprow else groupcol
  v
}









#' Apply functions to each matrix of a matrixset
#'
#' @description
#' The `apply_mat` function applies functions to each matrix of a `matrixset`.
#' The `apply_row`/`apply_column` functions do the same but separately for the
#' row/column. The functions can be applied to all matrices or only a subset.
#'
#' The `dfl`/`dfw` versions differ in their output format and when possibe,
#' always return a [tibble()].
#'
#' Empty matrices are simply left unevaluated. How that impacts the returned
#' result depends on which flavor of apply_* has been used. See \sQuote{Value}
#' for more details.
#'
#' If `.matrix_wise` is `FALSE`, the function (or expression) is multivariate in
#' the sense that all matrices are accessible at once, as opposed to each of them
#' in turn.
#'
#' See section "Multivariate".
#'
#' @section Pronouns:
#' The `rlang` pronouns `.data` and `.env` are available. Two scenarios for
#' which they can be useful are:
#'  * The annotation names are stored in a character variable. You can make use
#'      of the variable by using `.data[[var]]`. See the example for an
#'      illustration of this.
#'  * You want to make use of a global variable that has the same name as an
#'      annotation. You can use `.env[[var]]` or `.env$var` to make sure to use
#'      the proper variable.
#'
#' The matrixset package defines its own pronouns: `.m`, `.i` and `.j`, which
#' are discussed in the function specification argument (`...`).
#'
#' It is not necessary to import any of the pronouns (or load `rlang` in the
#' case of `.data` and `.env`) in a interactive session.
#'
#' It is useful however when writing a package to avoid the `R CMD check` notes.
#' As needed, you can import `.data` and `.env` (from `rlang`) or any of `.i`,
#' `.j` or `.m` from `matrixset`.
#'
#' @section Multivariate:
#' The default behavior is to apply a function or expression to a single
#' matrix and each matrices of the `matrixset` object are provided sequentially
#' to the function/expression.
#'
#' If `.matrix_wise` is `FALSE`, all matrices are provided at once to the
#' functions/expressions. They can be provided in two fashions:
#'  * separately (default behavior). Each matrix can be referred by `.m1`, ...,
#'      `.mn`, where `n` is the number of matrices. Note that this is the number
#'       as determined by `.matrix`.
#'
#'       For `apply_row` (and dfl/dfw variants), use `.i1`, `.i2` and so on
#'       instead. What the functions/expressions have access to in this case is
#'       the first row of the first matrix, the first row of the second matrix
#'       and so on. Then, continuing the loop, the second row of each matrix
#'       will be accessible, and so on
#'
#'       Similarly, use `.j1` and so on for the `apply_column` family.
#'
#'       Anonymous functions will be understood as a function with multiple
#'       arguments. In the example `apply_row(ms, mean, .matrix_wise = FALSE)`,
#'       if there are 3 matrices in the `ms` object, `mean` is understood as
#'       `mean(.i1, .i2, .i3)`. Note that this would fail because of the `mean`
#'       function.
#'
#'  * In a list (`.list_input = TRUE`). The list will have an element per matrix.
#'     The list can be referred using the same pronouns (`.m`, `.i`, `.j`), and
#'     the matrix, by the matrix names or position.
#'
#' For the multivariate setting, empty matrices are given as is, so it is
#' important that provided functions can deal with such a scenario. An
#' alternative is to skip the empty matrices with the `.matrix` argument.
#'
#' @section Grouped matrixsets:
#' If groups have been defined, functions will be evaluated within them. When
#' both row and column grouping has been registered, functions are evaluated at
#' each cross-combination of row/column groups.
#'
#' The output format is different when the `.ms` matrixset object is grouped.
#' A list for every matrix is still returned, but each of these lists now holds
#' a tibble.
#'
#' Each tibble has a column called `.vals`, where the function results are
#' stored. This column is a list, one element per group. The group labels are
#' given by the other columns of the tibble. For a given group, things are like
#' the ungrouped version: further sub-lists for rows/columns - if applicable -
#' and function values.
#'
#' The dfl/dfw versions are more similar in their output format to their
#' ungrouped version. The format is almost identical, except that additional
#' columns are reported to identify the group labels.
#'
#' See the examples.
#'
#'
#' @param .ms    `matrixset` object
#' @param ...    expressions, separated by commas. They can be specified in one of
#'     the following way:
#'
#'    * a function name, e.g., `mean`.
#'    * a function call, where you can use `.m` to represent the current matrix
#'       (for `apply_mat`), `.i` to represent the current row (for `apply_row`)
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
#'
#' @param .matrix   matrix indices of which matrix to apply functions to. The
#'                  default, `NULL`, means all the matrices are used.
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
#' @param .matrix_wise    `logical`. By default (`TRUE`), matrices are provided
#'    one by one, in turn, to the functions/expressions. But if `.matrix_wise` is
#'    `FALSE`, the functions/expressions have access to all matrices. See
#'    "Multivariate" for details, including how to reference the matrices.
#'
#' @param .input_list    `logical`. If multivariate (`.matrix_wise ==  FALSE`),
#'    the matrices are provided as a single list, where each element is a matrix
#'    (or matrix row or column). The list elements are the matrix names.
#'
#' @returns
#' A list for every matrix in the matrixset object. Each list is itself a list.
#' For `apply_mat`, it is a list of the function values - `NULL` if the matrix
#' was empty. Otherwise, it is a list with one element for each row/column -
#' these elements will be `NULL` if the corresponding matrix was empty. And
#' finally, for `apply_row`/`apply_column`, each of these sub-list is a list,
#' the results of each function.
#'
#' If each function returns a `vector` of the same dimension, you can use either
#' the `_dfl` or the `_dfw` version. What they do is to return a list of
#' `tibble`s. The `dfl` version will stack the function results in a long format
#' while the `dfw` version will put them side-by-side, in a wide format. An
#' empty matrix will be returned for empty input matrices.
#'
#' If the functions returned vectors of more than one element, there will be a
#' column to store the values and one for the function ID (dfl), or one column
#' per combination of function/result (dfw)
#'
#' See the grouping section to learn about the result format in the grouping
#' context.
#'
#' @examples
#' # The firs example takes the whole matrix average, while the second takes
#' # every row average
#' (mn_mat <- apply_mat(student_results, mean))
#' (mn_row <- apply_row(student_results, mean))
#'
#' # More than one function can be provided. It's a good idea in this case to
#' # name them
#' (mn_col <- apply_column(student_results, avr=mean, med=median))
#'
#' # the dfl/dfw versions returns nice tibbles - if the functions return values
#' # of the same length.
#' (mn_l <- apply_column_dfl(student_results, avr=mean, med=median))
#' (mn_w <- apply_column_dfw(student_results, avr=mean, med=median))
#'
#' # There is no difference between the two versions for length-1 vector results.
#' # hese will differ, however
#' (rg_l <- apply_column_dfl(student_results, rg=range))
#' (rg_w <- apply_column_dfw(student_results, rg=range))
#'
#' # More complex examples can be used, by using pronouns and data annotation
#' (vals <- apply_column(student_results, avr=mean, avr_trim=mean(.j, trim=.05),
#'                                       reg=lm(.j ~ teacher)))
#'
#' # You can wrap complex function results, such as for lm, into a list, to use
#' # the dfl/dfr version
#' (vals_tidy <- apply_column_dfw(student_results, avr=mean, avr_trim=mean(.j, trim=.05),
#'                                                reg=list(lm(.j ~ teacher))))
#'
#' # You can provide complex expressions by using formulas
#' (r <- apply_column(student_results,
#'                                   res= ~ {
#'                                     log_score <- log(.j)
#'                                     p <- predict(lm(log_score ~ teacher + class))
#'                                     .j - exp(p)
#'                                   }))
#'
#' # the .data pronoun can be useful to use names stored in variables
#' fn <- function(nm) {
#'   if (!is.character(nm) && length(nm) != 1) stop("this example won't work")
#'   apply_column(student_results, lm(.j ~ .data[[nm]]))
#' }
#' fn("teacher")
#'
#' # You can use variables that are outside the scope of the matrixset object.
#' # You don't need to do anything special if that variable is not named as an
#' # annotation
#' pass_grade <- 0.5
#' (passed <- apply_row_dfw(student_results, pass = ~ .i >= pass_grade))
#'
#' # use .env if shares an annotation name
#' previous_year_score <- 0.5
#' (passed <- apply_row_dfw(student_results, pass = ~ .i >= .env$previous_year_score))
#'
#' # Grouping structure makes looping easy. Look at the output format
#' cl_prof_gr <- row_group_by(student_results, class, teacher)
#' (gr_summ <- apply_column(cl_prof_gr, avr=mean, med=median))
#' (gr_summ_tidy <- apply_column_dfw(cl_prof_gr, avr=mean, med=median))
#' # to showcase how we can play with format
#' (gr_summ_tidy_long <- apply_column_dfl(cl_prof_gr, summ = ~ c(avr=mean(.j), med=median(.j))))
#'
#' # It is even possible to combine groupings
#' cl_prof_program_gr <- column_group_by(cl_prof_gr, program)
#' (mat_summ <- apply_mat(cl_prof_program_gr, avr = mean, med = median, rg = range))
#' # it doesn' make much sense, but this is to showcase format
#' (summ_gr <- apply_mat(cl_prof_program_gr, avr = mean, med = median, rg = range))
#' (summ_gr_long <- apply_column_dfl(cl_prof_program_gr,
#'                                  ct = ~ c(avr = mean(.j), med = median(.j)),
#'                                  rg = range))
#' (summ_gr_wide <- apply_column_dfw(cl_prof_program_gr,
#'                                  ct = c(avr = mean(.j), med = median(.j)),
#'                                  rg = range))
#'
#' @name loop
NULL




#' @rdname loop
#' @export
apply_row <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                      .input_list = FALSE)
  if (.matrix_wise) {
    .apply_row(.ms, ..., .matrix = .matrix)
  } else {
    .mapply_row(.ms, ..., .matrix = .matrix, .list_input = .input_list)
  }

#' @rdname loop
#' @export
apply_row_dfl <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                          .input_list = FALSE)
  if (.matrix_wise) {
    .apply_row_dfl(.ms, ..., .matrix = .matrix)
  } else {
    .mapply_row_dfl(.ms, ..., .matrix = .matrix, .list_input = .input_list)
  }

#' @rdname loop
#' @export
apply_row_dfw <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                          .input_list = FALSE)
  if (.matrix_wise) {
    .apply_row_dfw(.ms, ..., .matrix = .matrix)
  } else {
    .mapply_row_dfw(.ms, ..., .matrix = .matrix, .list_input = .input_list)
  }


#' @rdname loop
#' @export
apply_column <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                         .input_list = FALSE)
  if (.matrix_wise) {
    .apply_column(.ms, ..., .matrix = .matrix)
  } else {
    .mapply_column(.ms, ..., .matrix = .matrix, .list_input = .input_list)
  }

#' @rdname loop
#' @export
apply_column_dfl <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                             .input_list = FALSE)
  if (.matrix_wise) {
    .apply_column_dfl(.ms, ..., .matrix = .matrix)
  } else {
    .mapply_column_dfl(.ms, ..., .matrix = .matrix, .list_input = .input_list)
  }

#' @rdname loop
#' @export
apply_column_dfw <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                             .input_list = FALSE)
  if (.matrix_wise) {
    .apply_column_dfw(.ms, ..., .matrix = .matrix)
  } else {
    .mapply_column_dfw(.ms, ..., .matrix = .matrix, .list_input = .input_list)
  }


#' @rdname loop
#' @export
apply_mat <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                      .input_list = FALSE)
  if (.matrix_wise) {
    .apply_mat(.ms, ..., .matrix = .matrix)
  }


#' @rdname loop
#' @export
apply_mat_dfl <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                          .input_list = FALSE)
  if (.matrix_wise) {
    .apply_mat_dfl(.ms, ..., .matrix = .matrix)
  }


#' @rdname loop
#' @export
apply_mat_dfw <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                          .input_list = FALSE)
  if (.matrix_wise) {
    .apply_mat_dfw(.ms, ..., .matrix = .matrix)
  }






.apply_row <- function(.ms, ..., .matrix = NULL)
  UseMethod(".apply_row")



.apply_row.NULL <- function(.ms, ..., .matrix = NULL) NULL




.apply_row.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_fun(margin="row", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
           .simplify = FALSE, env=rlang::caller_env())
}




.apply_row.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  NextMethod()
}




.apply_row.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  ans <- column_group_meta(.ms)
  vals <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                   row_first = FALSE, .simplify = FALSE,
                   env=rlang::caller_env())
  lapply(vals, function(v) {
    ans$.rows <- NULL
    ans$.vals <- v
    ans
  })
}





.apply_row.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  ans <- column_group_meta(.ms)
  vals <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                   row_first = FALSE, .simplify = FALSE,
                   env=rlang::caller_env())
  lapply(vals, function(v) {
    ans$.rows <- NULL
    ans$.vals <- v
    ans
  })
}





.apply_column <- function(.ms, ..., .matrix = NULL)
  UseMethod(".apply_column")




.apply_column.NULL <- function(.ms, ..., .matrix = NULL) NULL




.apply_column.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_fun(margin="col", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
           .simplify = FALSE, env=rlang::caller_env())
}




.apply_column.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  NextMethod()
}




.apply_column.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  ans_tmp <- row_group_meta(.ms)
  vals <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                   row_first = TRUE, .simplify = FALSE,
                   env=rlang::caller_env())
  ans <- lapply(vals, function(v) {
    ans_tmp$.vals <- v
    ans_tmp$.rows <- NULL
    ans_tmp
  })
  ans
}




.apply_column.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  ans_tmp <- row_group_meta(.ms)
  vals <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                   row_first = TRUE, .simplify = FALSE,
                   env=rlang::caller_env())
  ans <- lapply(vals, function(v) {
    ans_tmp$.vals <- v
    ans_tmp$.rows <- NULL
    ans_tmp
  })
  ans
}






.apply_row_dfl <- function(.ms, ..., .matrix = NULL)
  UseMethod(".apply_row_dfl")



.apply_row_dfl.NULL <- function(.ms, ..., .matrix = NULL) NULL




.apply_row_dfl.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       row_first = TRUE, .simplify = "long",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  lapply(eval_obj, function(vals) {
    dplyr::bind_rows(vals, .id = .rowtag(.ms))
  })

}




.apply_row_dfl.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  NextMethod()
}




.apply_row_dfl.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       row_first = FALSE, .simplify = "long",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- column_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr,
                                                                      .id = .rowtag(.ms))))
  })
}




.apply_row_dfl.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       row_first = FALSE, .simplify = "long",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- column_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr,
                                                                      .id = .rowtag(.ms))))
  })
}







.apply_column_dfl <- function(.ms, ..., .matrix = NULL)
  UseMethod(".apply_column_dfl")



.apply_column_dfl.NULL <- function(.ms, ..., .matrix = NULL) NULL




.apply_column_dfl.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       row_first = TRUE, .simplify = "long",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  lapply(eval_obj, function(vals) {
    dplyr::bind_rows(vals, .id = .coltag(.ms))
  })

}





.apply_column_dfl.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  NextMethod()
}




.apply_column_dfl.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       row_first = TRUE, .simplify = "long",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- row_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr,
                                                                      .id = .coltag(.ms))))
  })
}




.apply_column_dfl.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       row_first = TRUE, .simplify = "long",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- row_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr,
                                                                      .id = .coltag(.ms))))
  })
}







.apply_row_dfw <- function(.ms, ..., .matrix = NULL)
  UseMethod(".apply_row_dfw")




.apply_row_dfw.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       row_first = TRUE, .simplify = "wide",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  lapply(eval_obj, function(vals) {
    dplyr::bind_rows(vals, .id = .rowtag(.ms))
  })

}




.apply_row_dfw.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  NextMethod()
}




.apply_row_dfw.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       row_first = FALSE, .simplify = "wide",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- column_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr,
                                                                      .id = .rowtag(.ms))))
  })
}




.apply_row_dfw.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       row_first = FALSE, .simplify = "wide",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- column_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr,
                                                                      .id = .rowtag(.ms))))
  })
}





.apply_column_dfw <- function(.ms, ..., .matrix = NULL)
  UseMethod(".apply_column_dfw")



.apply_column_dfw.NULL <- function(.ms, ..., .matrix = NULL) NULL




.apply_column_dfw.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       row_first = TRUE, .simplify = "wide",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  lapply(eval_obj, function(vals) {
    dplyr::bind_rows(vals, .id = .coltag(.ms))
  })

}




.apply_column_dfw.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  NextMethod()
}




.apply_column_dfw.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       row_first = TRUE, .simplify = "wide",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- row_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr,
                                                                      .id = .coltag(.ms))))
  })
}





.apply_column_dfw.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       row_first = TRUE, .simplify = "wide",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- row_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr,
                                                                      .id = .coltag(.ms))))
  })
}




.apply_mat <- function(.ms, ..., .matrix = NULL)
  UseMethod(".apply_mat")



.apply_mat.NULL <- function(.ms, ..., .matrix = NULL) NULL




.apply_mat.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
           .simplify = FALSE, env=rlang::caller_env())
}




.apply_mat.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  ans <- row_group_meta(.ms)
  vals <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
                   .simplify = FALSE, env=rlang::caller_env())
  lapply(vals, function(v) {
    ans$.vals <- v
    ans$.rows <- NULL
    ans
  })
}




.apply_mat.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  ans <- column_group_meta(.ms)
  vals <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
                   .simplify = FALSE, env=rlang::caller_env())
  lapply(vals, function(v) {
    ans$.vals <- v
    ans$.rows <- NULL
    ans
  })
}





.apply_mat.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  meta_row <- row_group_keys(.ms)
  meta_col <- column_group_keys(.ms)

  ngr_row <- nrow(meta_row)
  ngr_col <- nrow(meta_col)

  rep_idx_row <- rep(seq(ngr_row), each = ngr_col)
  rep_idx_col <- rep(seq(ngr_col), ngr_row)

  meta <- meta_row[rep_idx_row, ]
  meta_col <- meta_col[rep_idx_col, ]
  for (nm in names(meta_col)) meta[[nm]] <- meta_col[[nm]]


  vals <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
                   .simplify = FALSE, env=rlang::caller_env())
  lapply(vals, function(v) {
    meta$.vals <- unlist(v, recursive = FALSE)
    meta
  })

}






.apply_mat_dfl <- function(.ms, ..., .matrix = NULL)
  UseMethod(".apply_mat_dfl")



.apply_mat_dfl.NULL <- function(.ms, ..., .matrix = NULL) NULL





.apply_mat_dfl.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
                       .simplify = "long", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  eval_obj
}




.apply_mat_dfl.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
                       .simplify = "long", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- row_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr)))
  })
}




.apply_mat_dfl.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="mat", ms=.ms, ..., matidx=.matrix,
                       row_first = TRUE, .simplify = "long",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- column_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr)))
  })
}


#'


.apply_mat_dfl.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  meta_row <- row_group_keys(.ms)
  meta_col <- column_group_keys(.ms)

  ngr_row <- nrow(meta_row)
  ngr_col <- nrow(meta_col)

  rep_idx_row <- rep(seq(ngr_row), each = ngr_col)
  rep_idx_col <- rep(seq(ngr_col), ngr_row)

  meta <- meta_row[rep_idx_row, ]
  meta_col <- meta_col[rep_idx_col, ]
  for (nm in names(meta_col)) meta[[nm]] <- meta_col[[nm]]

  nmeta <- nrow(meta)


  vals <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
                   .simplify = "long", env=rlang::caller_env())

  res <- lapply(vals, function(v) unlist(v, recursive = FALSE))
  nres <- lapply(res, function(r) unique(sapply(r, function(a) nrow(a))))


  purrr::map2(res, nres,
              function(r, n) {
                idx <- rep(seq(nmeta), each = n)
                dplyr::bind_cols(meta[idx, ], dplyr::bind_rows(r))
              })

}











.apply_mat_dfw <- function(.ms, ..., .matrix = NULL)
  UseMethod(".apply_mat_dfw")



.apply_mat_dfw.NULL <- function(.ms, ..., .matrix = NULL) NULL





.apply_mat_dfw.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
                       .simplify = "wide", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  eval_obj
}




.apply_mat_dfw.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
                       .simplify = "wide", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- row_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr)))
  })
}




.apply_mat_dfw.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="mat", ms=.ms, ..., matidx=.matrix,
                       row_first = TRUE, .simplify = "wide",
                       env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- column_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr)))
  })
}


#'


.apply_mat_dfw.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  meta_row <- row_group_keys(.ms)
  meta_col <- column_group_keys(.ms)

  ngr_row <- nrow(meta_row)
  ngr_col <- nrow(meta_col)

  rep_idx_row <- rep(seq(ngr_row), each = ngr_col)
  rep_idx_col <- rep(seq(ngr_col), ngr_row)

  meta <- meta_row[rep_idx_row, ]
  meta_col <- meta_col[rep_idx_col, ]
  for (nm in names(meta_col)) meta[[nm]] <- meta_col[[nm]]


  vals <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
                   .simplify = "wide", env=rlang::caller_env())


  lapply(vals,
         function(v) {
           dplyr::bind_cols(meta, dplyr::bind_rows(v))
         })

}
















.mapply_row <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
  UseMethod(".mapply_row")



.mapply_row.NULL <- function(.ms, ..., .matrix = NULL, .list_input = FALSE) NULL




.mapply_row.matrixset <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_fun_mult(margin="row", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
                list_input = .list_input, .simplify = FALSE,
                env = rlang::caller_env())
}




.mapply_row.row_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  NextMethod()
}




.mapply_row.col_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  ans <- column_group_meta(.ms)
  vals <- eval_fun_mult(margin="row", ms=.ms, ..., matidx=.matrix,
                        row_first = FALSE, list_input = .list_input,
                        .simplify = FALSE, env=rlang::caller_env())
  ans$.rows <- NULL
  ans$.vals <- vals
  ans
}




.mapply_row.dual_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  ans <- column_group_meta(.ms)
  vals <- eval_fun_mult(margin="row", ms=.ms, ..., matidx=.matrix,
                        row_first = FALSE, list_input = .list_input,
                        .simplify = FALSE, env=rlang::caller_env())
  ans$.rows <- NULL
  ans$.vals <- vals
  ans
}





.mapply_column <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
  UseMethod(".mapply_column")




.mapply_column.NULL <- function(.ms, ..., .matrix = NULL, .list_input = FALSE) NULL




.mapply_column.matrixset <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_fun_mult(margin="col", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
                list_input = .list_input, .simplify = FALSE,
                env = rlang::caller_env())
}




.mapply_column.col_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  NextMethod()
}




.mapply_column.row_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  ans <- row_group_meta(.ms)
  vals <- eval_fun_mult(margin="col", ms=.ms, ..., matidx=.matrix,
                        row_first = TRUE, list_input = .list_input,
                        .simplify = FALSE,
                        env=rlang::caller_env())
  ans$.rows <- NULL
  ans$.vals <- vals
  ans
}




.mapply_column.dual_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  ans_tmp <- row_group_meta(.ms)
  vals <- eval_fun_mult(margin="col", ms=.ms, ..., matidx=.matrix,
                        row_first = TRUE, list_input = .list_input,
                        .simplify = FALSE,
                        env=rlang::caller_env())
  ans$.rows <- NULL
  ans$.vals <- vals
  ans
}






.mapply_row_dfl <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
  UseMethod(".mapply_row_dfl")



.mapply_row_dfl.NULL <- function(.ms, ..., .matrix = NULL, .list_input = FALSE) NULL




.mapply_row_dfl.matrixset <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="row", ms=.ms, ..., matidx=.matrix,
                            row_first = TRUE, list_input = .list_input,
                            .simplify = "long",
                            env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  dplyr::bind_rows(eval_obj, .id = .rowtag(.ms))

}




.mapply_row_dfl.row_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  NextMethod()
}




.mapply_row_dfl.col_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="row", ms=.ms, ..., matidx=.matrix,
                            row_first = FALSE, list_input = .list_input,
                            .simplify = "long",
                            env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- column_group_keys(.ms)

  purrr::map2_dfr(eval_obj, seq_along(eval_obj),
                  function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                   dplyr::bind_rows(gr,
                                                                    .id = .rowtag(.ms))))

}




.mapply_row_dfl.dual_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="row", ms=.ms, ..., matidx=.matrix,
                            row_first = FALSE, list_input = .list_input,
                            .simplify = "long",
                            env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- column_group_keys(.ms)

  purrr::map2_dfr(eval_obj, seq_along(eval_obj),
                  function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                   dplyr::bind_rows(gr,
                                                                    .id = .rowtag(.ms))))
}







.mapply_column_dfl <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
  UseMethod(".mapply_column_dfl")



.mapply_column_dfl.NULL <- function(.ms, ..., .matrix = NULL, .list_input = FALSE) NULL




.mapply_column_dfl.matrixset <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="col", ms=.ms, ..., matidx=.matrix,
                            row_first = TRUE, list_input = .list_input,
                            .simplify = "long",
                            env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  dplyr::bind_rows(eval_obj, .id = .coltag(.ms))

}





.mapply_column_dfl.col_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  NextMethod()
}




.mapply_column_dfl.row_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="col", ms=.ms, ..., matidx=.matrix,
                            row_first = TRUE, list_input = .list_input,
                            .simplify = "long",
                            env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- row_group_keys(.ms)

  purrr::map2_dfr(eval_obj, seq_along(eval_obj),
                  function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                   dplyr::bind_rows(gr,
                                                                    .id = .coltag(.ms))))

}




.mapply_column_dfl.dual_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="col", ms=.ms, ..., matidx=.matrix,
                            row_first = TRUE, list_input = .list_input,
                            .simplify = "long",
                            env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- row_group_keys(.ms)

  purrr::map2_dfr(eval_obj, seq_along(eval_obj),
                  function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                   dplyr::bind_rows(gr,
                                                                    .id = .coltag(.ms))))
}







.mapply_row_dfw <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
  UseMethod(".mapply_row_dfw")




.mapply_row_dfw.matrixset <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="row", ms=.ms, ..., matidx=.matrix,
                            row_first = TRUE, list_input = .list_input,
                            .simplify = "wide", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  dplyr::bind_rows(eval_obj, .id = .rowtag(.ms))

}




.apply_row_dfw.row_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  NextMethod()
}




.mapply_row_dfw.col_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="row", ms=.ms, ..., matidx=.matrix,
                            row_first = FALSE, list_input = .list_input,
                            .simplify = "wide", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- column_group_keys(.ms)

  purrr::map2_dfr(eval_obj, seq_along(eval_obj), function(gr, i) {
    dplyr::bind_cols(group_inf[i, ],
                     dplyr::bind_rows(gr,
                                      .id = .rowtag(.ms)))
  })

}




.mapply_row_dfw.dual_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="row", ms=.ms, ..., matidx=.matrix,
                            row_first = FALSE, list_input = .list_input,
                            .simplify = "wide", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- column_group_keys(.ms)

  purrr::map2_dfr(eval_obj, seq_along(eval_obj), function(gr, i) {
    dplyr::bind_cols(group_inf[i, ],
                     dplyr::bind_rows(gr,
                                      .id = .rowtag(.ms)))
  })
}





.mapply_column_dfw <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
  UseMethod(".mapply_column_dfw")



.mapply_column_dfw.NULL <- function(.ms, ..., .matrix = NULL, .list_input = FALSE) NULL




.mapply_column_dfw.matrixset <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="col", ms=.ms, ..., matidx=.matrix,
                            row_first = TRUE, list_input = .list_input,
                            .simplify = "wide", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  dplyr::bind_rows(eval_obj, .id = .coltag(.ms))

}




.mapply_column_dfw.col_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  NextMethod()
}




.mapply_column_dfw.row_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="col", ms=.ms, ..., matidx=.matrix,
                            row_first = TRUE, list_input = .list_input,
                            .simplify = "wide", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- row_group_keys(.ms)

  purrr::map2_dfr(eval_obj, seq_along(eval_obj),
                  function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                   dplyr::bind_rows(gr,
                                                                    .id = .coltag(.ms))))

}



.mapply_column_dfw.dual_grouped_ms <- function(.ms, ..., .matrix = NULL, .list_input = FALSE)
{
  eval_obj <- eval_fun_mult(margin="col", ms=.ms, ..., matidx=.matrix,
                            row_first = TRUE, list_input = .list_input,
                            .simplify = "wide", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- row_group_keys(.ms)

  purrr::map2_dfr(eval_obj, seq_along(eval_obj),
                  function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                   dplyr::bind_rows(gr,
                                                                    .id = .coltag(.ms))))
}




#'
#'
#' .apply_mat <- function(.ms, ..., .matrix = NULL)
#'   UseMethod(".apply_mat")
#'
#'
#'
#' .apply_mat.NULL <- function(.ms, ..., .matrix = NULL) NULL
#'
#'
#'
#'
#' .apply_mat.matrixset <- function(.ms, ..., .matrix = NULL)
#' {
#'   eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
#'            .simplify = FALSE, env=rlang::caller_env())
#' }
#'
#'
#'
#'
#' .apply_mat.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
#' {
#'   ans <- row_group_meta(.ms)
#'   vals <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
#'                    .simplify = FALSE, env=rlang::caller_env())
#'   lapply(vals, function(v) {
#'     ans$.vals <- v
#'     ans$.rows <- NULL
#'     ans
#'   })
#' }
#'
#'
#'
#'
#' .apply_mat.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
#' {
#'   ans <- column_group_meta(.ms)
#'   vals <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
#'                    .simplify = FALSE, env=rlang::caller_env())
#'   lapply(vals, function(v) {
#'     ans$.vals <- v
#'     ans$.rows <- NULL
#'     ans
#'   })
#' }
#'
#'
#'
#'
#'
#' .apply_mat.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
#' {
#'   meta_row <- row_group_keys(.ms)
#'   meta_col <- column_group_keys(.ms)
#'
#'   ngr_row <- nrow(meta_row)
#'   ngr_col <- nrow(meta_col)
#'
#'   rep_idx_row <- rep(seq(ngr_row), each = ngr_col)
#'   rep_idx_col <- rep(seq(ngr_col), ngr_row)
#'
#'   meta <- meta_row[rep_idx_row, ]
#'   meta_col <- meta_col[rep_idx_col, ]
#'   for (nm in names(meta_col)) meta[[nm]] <- meta_col[[nm]]
#'
#'
#'   vals <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
#'                    .simplify = FALSE, env=rlang::caller_env())
#'   lapply(vals, function(v) {
#'     meta$.vals <- unlist(v, recursive = FALSE)
#'     meta
#'   })
#'
#' }
#'
#'
#'
#'
#'
#'
#' .apply_mat_dfl <- function(.ms, ..., .matrix = NULL)
#'   UseMethod(".apply_mat_dfl")
#'
#'
#'
#' .apply_mat_dfl.NULL <- function(.ms, ..., .matrix = NULL) NULL
#'
#'
#'
#'
#'
#' .apply_mat_dfl.matrixset <- function(.ms, ..., .matrix = NULL)
#' {
#'   eval_obj <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
#'                        .simplify = "long", env=rlang::caller_env())
#'
#'   if (is.null(eval_obj)) return(NULL)
#'
#'   eval_obj
#' }
#'
#'
#'
#'
#' .apply_mat_dfl.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
#' {
#'   eval_obj <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
#'                        .simplify = "long", env=rlang::caller_env())
#'
#'   if (is.null(eval_obj)) return(NULL)
#'
#'   group_inf <- row_group_keys(.ms)
#'
#'   lapply(eval_obj, function(mats) {
#'
#'     purrr::map2_dfr(mats, seq_along(mats),
#'                     function(gr, i) dplyr::bind_cols(group_inf[i, ],
#'                                                      dplyr::bind_rows(gr)))
#'   })
#' }
#'
#'
#'
#'
#' .apply_mat_dfl.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
#' {
#'   eval_obj <- eval_fun(margin="mat", ms=.ms, ..., matidx=.matrix,
#'                        row_first = TRUE, .simplify = "long",
#'                        env=rlang::caller_env())
#'
#'   if (is.null(eval_obj)) return(NULL)
#'
#'   group_inf <- column_group_keys(.ms)
#'
#'   lapply(eval_obj, function(mats) {
#'
#'     purrr::map2_dfr(mats, seq_along(mats),
#'                     function(gr, i) dplyr::bind_cols(group_inf[i, ],
#'                                                      dplyr::bind_rows(gr)))
#'   })
#' }
#'
#'
#' #'
#'
#'
#' .apply_mat_dfl.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
#' {
#'   meta_row <- row_group_keys(.ms)
#'   meta_col <- column_group_keys(.ms)
#'
#'   ngr_row <- nrow(meta_row)
#'   ngr_col <- nrow(meta_col)
#'
#'   rep_idx_row <- rep(seq(ngr_row), each = ngr_col)
#'   rep_idx_col <- rep(seq(ngr_col), ngr_row)
#'
#'   meta <- meta_row[rep_idx_row, ]
#'   meta_col <- meta_col[rep_idx_col, ]
#'   for (nm in names(meta_col)) meta[[nm]] <- meta_col[[nm]]
#'
#'   nmeta <- nrow(meta)
#'
#'
#'   vals <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
#'                    .simplify = "long", env=rlang::caller_env())
#'
#'   res <- lapply(vals, function(v) unlist(v, recursive = FALSE))
#'   nres <- lapply(res, function(r) unique(sapply(r, function(a) nrow(a))))
#'
#'
#'   purrr::map2(res, nres,
#'               function(r, n) {
#'                 idx <- rep(seq(nmeta), each = n)
#'                 dplyr::bind_cols(meta[idx, ], dplyr::bind_rows(r))
#'               })
#'
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' .apply_mat_dfw <- function(.ms, ..., .matrix = NULL)
#'   UseMethod(".apply_mat_dfw")
#'
#'
#'
#' .apply_mat_dfw.NULL <- function(.ms, ..., .matrix = NULL) NULL
#'
#'
#'
#'
#'
#' .apply_mat_dfw.matrixset <- function(.ms, ..., .matrix = NULL)
#' {
#'   eval_obj <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
#'                        .simplify = "wide", env=rlang::caller_env())
#'
#'   if (is.null(eval_obj)) return(NULL)
#'
#'   eval_obj
#' }
#'
#'
#'
#'
#' .apply_mat_dfw.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
#' {
#'   eval_obj <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
#'                        .simplify = "wide", env=rlang::caller_env())
#'
#'   if (is.null(eval_obj)) return(NULL)
#'
#'   group_inf <- row_group_keys(.ms)
#'
#'   lapply(eval_obj, function(mats) {
#'
#'     purrr::map2_dfr(mats, seq_along(mats),
#'                     function(gr, i) dplyr::bind_cols(group_inf[i, ],
#'                                                      dplyr::bind_rows(gr)))
#'   })
#' }
#'
#'
#'
#'
#' .apply_mat_dfw.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
#' {
#'   eval_obj <- eval_fun(margin="mat", ms=.ms, ..., matidx=.matrix,
#'                        row_first = TRUE, .simplify = "wide",
#'                        env=rlang::caller_env())
#'
#'   if (is.null(eval_obj)) return(NULL)
#'
#'   group_inf <- column_group_keys(.ms)
#'
#'   lapply(eval_obj, function(mats) {
#'
#'     purrr::map2_dfr(mats, seq_along(mats),
#'                     function(gr, i) dplyr::bind_cols(group_inf[i, ],
#'                                                      dplyr::bind_rows(gr)))
#'   })
#' }
#'
#'
#' #'
#'
#'
#' .apply_mat_dfw.dual_grouped_ms <- function(.ms, ..., .matrix = NULL)
#' {
#'   meta_row <- row_group_keys(.ms)
#'   meta_col <- column_group_keys(.ms)
#'
#'   ngr_row <- nrow(meta_row)
#'   ngr_col <- nrow(meta_col)
#'
#'   rep_idx_row <- rep(seq(ngr_row), each = ngr_col)
#'   rep_idx_col <- rep(seq(ngr_col), ngr_row)
#'
#'   meta <- meta_row[rep_idx_row, ]
#'   meta_col <- meta_col[rep_idx_col, ]
#'   for (nm in names(meta_col)) meta[[nm]] <- meta_col[[nm]]
#'
#'
#'   vals <- eval_fun("mat", ms=.ms, ..., matidx=.matrix, row_first = TRUE,
#'                    .simplify = "wide", env=rlang::caller_env())
#'
#'
#'   lapply(vals,
#'          function(v) {
#'            dplyr::bind_cols(meta, dplyr::bind_rows(v))
#'          })
#'
#' }












