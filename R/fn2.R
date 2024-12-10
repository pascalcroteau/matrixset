


`%.==.%` <- function(x, y)
{
  if (is.null(x)) FALSE else if (is.null(y)) FALSE else x == y
}






flatten_and_name <- function(x, y) {

  n <- length(x)
  nms <- names(x)
  r <- c()
  for (i in 1:n) {
    nmsi <- if (is.null(y[[i]])) nms[i] else paste(nms[i], y[[i]])
    r <- c(r, setNames(x[[i]], nmsi))
  }
  r

}






LoopStruct <- R6::R6Class(
  "LoopStruct",

  public = list(

    initialize = function(.ms, margin, mat_subset)
    {
      private$matrix_subset_ <- !is.null(mat_subset)
      private$._set_matrix_idx(.ms, mat_subset)
      private$._set_matrix_eval_status(.ms)

      row_wise <- margin %.==.% 1
      col_wise <- margin %.==.% 2

      row_grp_meta <- attr(.ms, "row_group_meta")
      col_grp_meta <- attr(.ms, "col_group_meta")

      row_grouped <- !is.null(row_grp_meta) && !row_wise
      col_grouped <- !is.null(col_grp_meta) && !col_wise

      if (!row_grouped) row_grp_meta <- NULL
      if (!col_grouped) col_grp_meta <- NULL

      n_group_row <- if (row_grouped) nrow(row_grp_meta) else NULL
      n_group_col <- if (col_grouped) nrow(col_grp_meta) else NULL

      private$row_wise_ <- row_wise
      private$col_wise_ <- col_wise
      private$row_group_df_ <- row_grp_meta
      private$col_group_df_ <- col_grp_meta
      private$row_grouped_ <- row_grouped
      private$col_grouped_ <- col_grouped

      private$set_row_groups(.ms)
      private$set_col_groups(.ms)

    }

  ),

  active = list(

    row_group_df = function() private$row_group_df_,
    col_group_df = function() private$col_group_df_,
    row_grouped = function() private$row_grouped_,
    col_grouped = function() private$col_grouped_,
    row_groups_for_loop = function() private$row_groups_for_loop_,
    col_groups_for_loop = function() private$col_groups_for_loop_,

    matrix_subsetting = function() private$matrix_subset_,
    matrix_idx = function() private$matrix_idx_,
    matrix_eval = function() private$matrix_eval_,

    looping = function() {
      !is.null(private$row_groups_for_loop_) ||
        !is.null(private$col_groups_for_loop_)
    },
    row_looping = function() !is.null(private$row_groups_for_loop_),
    col_looping = function() !is.null(private$col_groups_for_loop_),
    col_looping_first = function() {
      !private$row_grouped_ &&
        (private$col_grouped_ || is.null(private$row_groups_for_loop_))
    }


  ),

  private = list(

    matrix_subset_ = FALSE,
    matrix_idx_ = NULL,
    matrix_eval_ = NULL,
    row_wise_ = NULL,
    col_wise_ = NULL,
    row_group_df_ = NULL,
    col_group_df_ = NULL,
    row_grouped_ = NULL,
    col_grouped_ = NULL,
    row_groups_for_loop_ = NULL,
    col_groups_for_loop_ = NULL,




    ._set_matrix_idx = function(.ms, matidx)
    {
      if (is.null(matidx)) {
        nmat <- .nmatrix(.ms)
        matidx <- seq_len(nmat)
        matnms <- matrixnames(.ms)
      } else {
        matnms <- .matrixnames(.ms)
        matidx <- index_to_integer(matidx, .nmatrix(.ms), matnms)
        matnms <- matnms[matidx]
      }

      matidx <- stats::setNames(matidx, matnms)
      private$matrix_idx_ <- matidx

    },



    ._set_matrix_eval_status = function(.ms)
    {
      mats <- .subset2(.ms, "matrix_set")
      mat_eval <- !vapply(mats, is.null, FALSE)
      private$matrix_eval_ <- mat_eval[private$matrix_idx_]
    },




    set_row_groups = function(.ms) {

      if (private$row_wise_) {

        row_grs <- as.list(seq_len(nrow(.ms)))
        names(row_grs) <- rownames(.ms)
        private$row_groups_for_loop_ <- row_grs

      } else if (private$row_grouped_) {

        private$row_groups_for_loop_ <- private$row_group_df_$.rows

      }

    },



    set_col_groups = function(.ms) {

      if (private$col_wise_) {

        col_grs <- as.list(seq_len(ncol(.ms)))
        names(col_grs) <- colnames(.ms)
        private$col_groups_for_loop_ <- col_grs

      } else if (private$col_grouped_) {

        private$col_groups_for_loop_ <- private$col_group_df_$.rows

      }

    }

  )

)






EvalScope <- R6::R6Class(
  "EvalScope",

  public = list(

    initialize = function(.ms, margin, multi, as_list, loop_struct, env) {


      private$._context_env <- new.env(parent = env)
      private$._enclos_env <- new.env(parent = private$._context_env)
      private$._enclos_env$.data <- new.env()
      private$._enclos_env$.env <- env

      private$._set_context()

      private$._ms <- .ms
      private$._mats <- .subset2(.ms, "matrix_set")
      private$._row_inf <- .subset2(.ms, "row_info")
      private$._col_inf <- .subset2(.ms, "column_info")

      private$._margin <- margin
      private$._matrix_wise <- !multi
      private$._matrix_list <- as_list
      private$._loop_struct <- loop_struct

      private$._set_bindings()
    },



    register_function = function(fn) {
      private$._fn <- fn
    },




    eval = function() {

      eval(private$._fn, envir = private$._enclos_env)

    }



  ),


  active = list(

    i = function(new_i) private$i_ <- new_i,
    j = function(new_j) private$j_ <- new_j,
    k = function(new_k) private$k_ <- new_k

  ),


  private = list(

    ._context_env = NULL,
    ._enclos_env = NULL,

    ._ms = NULL,
    ._mats = NULL,
    i_ = NULL,
    j_ = NULL,
    k_ = NULL,

    ._margin = NULL,
    ._matrix_wise = TRUE,
    ._matrix_list = FALSE,

    ._loop_struct = NULL,

    ._row_inf = NULL,
    ._col_inf = NULL,

    ._fn = NULL,



    ._mat_whole = quote(private$._mats),
    ._mat_subset_k = quote(private$._mats[[._k_]]),
    ._mat_subset_k_lst = quote(private$._mats[._k_]),

    ._M_subset_i_drop = quote(._M_[private$i_, ]),
    ._M_subset_i_no_drop = quote(._M_[private$i_, , drop = FALSE]),
    ._M_subset_i_then_j = quote(._M_[private$i_, ][private$j_]),
    ._M_subset_j_drop = quote(._M_[, private$j_]),
    ._M_subset_j_no_drop = quote(._M_[, private$j_, drop = FALSE]),
    ._M_subset_j_then_i = quote(._M_[, private$j_][private$i_]),
    ._M_subset_ij = quote(._M_[private$i_, private$j_, drop = FALSE]),



    ._set_context = function() {

      private$._context_env$current_n_row <- function() {
        if (is.null(private$i_)) nrow(private$._ms) else length(private$i_)
      }

      private$._context_env$current_n_column <- function() {
        if (is.null(private$j_)) ncol(private$._ms) else length(private$j_)
      }



      private$._context_env$current_row_name <- function() {
        if (is.null(private$i_)) {
          rownames(private$._ms)
        } else {
          rownames(private$._ms)[private$i_]
        }
      }

      private$._context_env$current_column_name <- function() {
        if (is.null(private$j_)) {
          colnames(private$._ms)
        } else {
          colnames(private$._ms)[private$i_]
        }
      }



      private$._context_env$current_row_info <- function() {
        if (is.null(private$i_)) {
          .subset2(student_results, "row_info")
        } else {
          .subset2(student_results, "row_info")[private$i_, ]
        }
      }

      private$._context_env$current_column_info <- function() {
        if (is.null(private$j_)) {
          .subset2(student_results, "column_info")
        } else {
          .subset2(student_results, "column_info")[private$j_, ]
        }
      }



      private$._context_env$row_pos <- function() {
        if (is.null(private$i_)) {
          seq_len(nrow(private$._ms))
        } else {
          private$i_
        }
      }

      private$._context_env$column_pos <- function() {
        if (is.null(private$j_)) {
          seq_len(ncol(private$._ms))
        } else {
          private$j_
        }
      }



      private$._context_env$row_rel_pos <- function() {
        cnr <- if (is.null(private$i_)) nrow(private$._ms) else length(private$i_)
        seq_len(cnr)
      }

      private$._context_env$column_rel_pos <- function() {
        cnc <- if (is.null(private$j_)) ncol(private$._ms) else length(private$j_)
        seq_len(cnc)
      }


    },



    ._generate_mat_subset_expr = function(mat_expr, k_expr, sub_expr) {
      do.call("substitute",
              list(sub_expr,
                   list(._M_ = do.call(substitute,
                                       list(mat_expr,
                                            list(._k_ = k_expr))
                                       )
                        )
                   )
              )
    },


    ._generate_mat_subset_within_loop_expr = function(seq_expr, mat_expr, k_expr,
                                                      sub_expr) {
      do.call(substitute, list(substitute(lapply(._ms_, function(m) ._m_expr_),
                                          list(._ms_ = do.call(substitute,
                                                               list(seq_expr,
                                                                    list(._k_ = k_expr))
                                          ),
                                               ._m_expr_ = sub_expr)),
                               list(._M_ = mat_expr)))
    },




    ._generate_row_subset = function(k) {
      if (private$._loop_struct$col_looping)
        return(private$._generate_mat_subset_expr(private$._mat_subset_k,
                                                  k,
                                                  private$._M_subset_i_then_j))

      private$._generate_mat_subset_expr(private$._mat_subset_k,
                                         k,
                                         private$._M_subset_i_drop)
    },






    ._generate_row_subset_multi = function() {

      if (private$._loop_struct$matrix_subsetting) {

        if (private$._loop_struct$col_looping)
          return(
            private$._generate_mat_subset_within_loop_expr(private$._mat_subset_k_lst,
                                                           quote(m),
                                                           quote(private$k_),
                                                           private$._M_subset_i_then_j)
          )

        return(
          private$._generate_mat_subset_within_loop_expr(private$._mat_subset_k_lst,
                                                         quote(m),
                                                         quote(private$k_),
                                                         private$._M_subset_i_drop)
        )
      }

      if (private$._loop_struct$col_looping)
        return(
          private$._generate_mat_subset_within_loop_expr(private$._mat_whole,
                                                         quote(m),
                                                         quote(private$k_),
                                                         private$._M_subset_i_then_j)
        )

      private$._generate_mat_subset_within_loop_expr(private$._mat_whole,
                                                     quote(m),
                                                     quote(private$k_),
                                                     private$._M_subset_i_drop)

    },






    ._generate_col_subset = function(k) {
      if (private$._loop_struct$row_looping)
        return(private$._generate_mat_subset_expr(private$._mat_subset_k,
                                                  k,
                                                  private$._M_subset_j_then_i))

      private$._generate_mat_subset_expr(private$._mat_subset_k,
                                         k,
                                         private$._M_subset_j_drop)
    },





    ._generate_col_subset_multi = function() {

      if (private$._loop_struct$matrix_subsetting) {

        if (private$._loop_struct$row_looping)
          return(
            private$._generate_mat_subset_within_loop_expr(private$._mat_subset_k_lst,
                                                           quote(m),
                                                           quote(private$k_),
                                                           private$._M_subset_j_then_i)
          )

        return(
          private$._generate_mat_subset_within_loop_expr(private$._mat_subset_k_lst,
                                                         quote(m),
                                                         quote(private$k_),
                                                         private$._M_subset_j_drop)
        )
      }

      if (private$._loop_struct$row_looping)
        return(
          private$._generate_mat_subset_within_loop_expr(private$._mat_whole,
                                                         quote(m),
                                                         quote(private$k_),
                                                         private$._M_subset_j_then_i)
        )

      private$._generate_mat_subset_within_loop_expr(private$._mat_whole,
                                                     quote(m),
                                                     quote(private$k_),
                                                     private$._M_subset_j_drop)

    },




    ._generate_mat_subset = function(k) {

      if (private$._loop_struct$row_looping) {

        if (private$._loop_struct$col_looping)
          return(private$._generate_mat_subset_expr(private$._mat_subset_k,
                                                    k,
                                                    private$._M_subset_ij))

        return(private$._generate_mat_subset_expr(private$._mat_subset_k,
                                                  k,
                                                  private$._M_subset_i_no_drop))
      }


      if (private$._loop_struct$col_looping)
        return(private$._generate_mat_subset_expr(private$._mat_subset_k,
                                                  k,
                                                  private$._M_subset_j_no_drop))

      do.call(substitute, list(private$._mat_subset_k,
                               list(._k_ = k)))
    },





    ._generate_mat_subset_multi = function() {

      if (private$._loop_struct$matrix_subsetting) {

        if (private$._loop_struct$row_looping) {

          if (private$._loop_struct$col_looping)
            return(private$._mat_subset_multi_with_k_with_i_with_j_expr)

          return(private$._mat_subset_multi_with_k_with_i_no_j_expr)

        }

        if (private$._loop_struct$col_looping)
          return(private$._mat_subset_multi_with_k_no_i_with_j_expr)

        return(private$._mat_subset_multi_with_k_no_i_no_j_expr)

      }


      if (private$._loop_struct$row_looping) {

        if (private$._loop_struct$col_looping)
          return(private$._mat_subset_multi_no_k_with_i_with_j_expr)

        return(private$._mat_subset_multi_no_k_with_i_no_j_expr)

      }

      if (private$._loop_struct$col_looping)
        return(private$._mat_subset_multi_no_k_no_i_with_j_expr)

      return(private$._mat_subset_multi_no_k_no_i_no_j_expr)

    },





    ._set_bindings_from_single_mat = function() {

      active_name <- if (private$._margin == 1) {
        var_lab_row
      } else if (private$._margin == 2) {
        var_lab_col
      } else var_lab_mat
      not_active_name <- setdiff(c(var_lab_row, var_lab_col, var_lab_mat),
                                 active_name)


      fn_body <- if (private$._margin == 1) {
        private$._generate_row_subset(quote(private$k_))
      } else if (private$._margin == 2) {
        private$._generate_col_subset(quote(private$k_))
      } else {
        private$._generate_mat_subset(quote(private$k_))
      }

      fn <- function() {}
      body(fn) <- fn_body

      makeActiveBinding(active_name, fn, env = private$._enclos_env)

      for (v in not_active_name) {
        msg <- glue::glue("object {OBJ} not found", OBJ = v)
        fn <- function() {}
        fn_body <- substitute(stop(MSG, call. = FALSE), list(MSG=msg))
        body(fn) <- fn_body
        makeActiveBinding(v, fn, env = private$._enclos_env)
      }
    },




    ._set_bindings_from_multi_mat_sep = function() {

      active_name <- if (private$._margin == 1) {
        var_lab_row
      } else if (private$._margin == 2) {
        var_lab_col
      } else var_lab_mat

      not_active_name <- setdiff(c(var_lab_row, var_lab_col, var_lab_mat),
                                 active_name)


      idx <- if (private$._loop_struct$matrix_subsetting) {
        private$._loop_struct$matrix_idx
      } else {
        seq_along(private$._mats)
      }
      fields <- names(private$._mats)[idx]



      fn <- function() {}

      for (mi in seq_along(idx)) {

        field <- paste0(active_name, mi)

        fn_body <- if (private$._margin == 1) {
          private$._generate_row_subset(idx[mi])
        } else if (private$._margin == 2) {
          private$._generate_col_subset(idx[mi])
        } else {
          private$._generate_mat_subset(idx[mi])
        }


        body(fn) <- fn_body

        makeActiveBinding(field, fn, env = private$._enclos_env)
        makeActiveBinding(field, fn, env = private$._enclos_env$.data)


        # for (v in not_active_name) {
        #   msg <- glue::glue("object {OBJ} not found", OBJ = v)
        #   fn <- function() {}
        #   fn_body <- substitute(stop(MSG, call. = FALSE), list(MSG=msg))
        #   body(fn) <- fn_body
        #   makeActiveBinding(v, fn, env = private$._enclos_env)
        # }
      }
    },




    ._set_bindings_from_multi_mat = function() {

      active_name <- if (private$._margin == 1) {
        var_lab_row
      } else if (private$._margin == 2) {
        var_lab_col
      } else var_lab_mat
      not_active_name <- setdiff(c(var_lab_row, var_lab_col, var_lab_mat),
                                 active_name)

      if (!private$._matrix_list) {
        return(private$._set_bindings_from_multi_mat_sep())
      }


      fn_body <- if (private$._margin == 1) {
        private$._generate_row_subset_multi()
      } else if (private$._margin == 2) {
        private$._generate_col_subset_multi()
      } else {
        private$._generate_mat_subset_multi()
      }

      fn <- function() {}
      body(fn) <- fn_body

      makeActiveBinding(active_name, fn, env = private$._enclos_env)


    },





    ._set_bindings_from_inf = function(info) {

      info_name <- paste(".", info, "inf", sep = "_")
      fields <- names(private[[info_name]])

      inf_idx <- if (info == "row") quote(private$i_) else quote(private$j_)

      fn <- function() {}

      for (field in fields) {
        fn_body <- substitute(
          {
            if (is.null(idx)) {
              return(private[[info]][[fld]])
            }
            private[[info]][[fld]][idx]
          },
          list(info = info_name,
               fld = field,
               idx = inf_idx)
        )
        body(fn) <- fn_body

        makeActiveBinding(field, fn, env = private$._enclos_env)
        makeActiveBinding(field, fn, env = private$._enclos_env$.data)
      }

    },



    ._set_bindings = function() {

      private$._set_bindings_from_inf("row")
      private$._set_bindings_from_inf("col")

      if (!private$._matrix_wise) {

        private$._set_bindings_from_multi_mat()
        return()

      }

      private$._set_bindings_from_single_mat()

    }

  )

  )


# for (v in vars) {
#   EvalScope$set("active", v {
#     f <- function() {}
#     body(f) <- rlang::quo_get_expr(rlang::quo(private$))
#     f
#   })
# }








Applyer <- R6::R6Class(
  "Applyer",

  public = list(

    initialize = function(.ms, matidx, margin, fns, multi, as_list, simplify,
                          force_name, env) {

      private$._loop_struct <- LoopStruct$new(.ms, margin, matidx)

      private$._margin <- margin
      private$._multi_mat <- multi

      private$._set_matrix_meta(.ms)

      private$._tag <- switch(margin,
                              "1" = .rowtag(.ms),
                              "2" = .coltag(.ms),
                              NULL)

      private$._set_fn_meta(fns)

      private$._simplify <- simplify
      private$._force_name <- force_name

      private$._scope <- EvalScope$new(.ms, margin, multi, as_list,
                                       private$._loop_struct, env)
    },


    eval = function() private$eval_()



  ),


  private = list(

    ._scope = NULL,

    ._margin = NULL,
    ._multi_mat = NULL,

    ._mat_n = NULL,
    ._mat_names = NULL,

    ._tag = NULL,

    ._loop_struct = NULL,

    ._fns = list(function(x) x),
    ._fns_n = 0,
    ._fn_names = NULL,

    ._fns_outcome = NULL,
    ._fns_outcome_formatted = NULL,
    ._fns_outcome_names = NULL,
    ._fn_out_lens = NULL,

    ._mat_outcome = NULL,
    ._row_outcome = NULL,
    ._col_outcome = NULL,

    ._wide_order = NULL,

    ._simplify = "no",
    ._force_name = FALSE,





    ._set_matrix_meta = function(.ms)
    {
      matidx <- private$._loop_struct$matrix_idx
      nmat <- length(matidx)

      matnms <- .matrixnames(.ms)
      matnms <- matnms[matidx]

      private$._mat_n <- nmat
      private$._mat_names <- matnms

      private$._reset_mat_outcome()

    },




    ._reset_fns_outcome_names = function() {

      private$._fns_outcome_names <- vector('list', private$._fns_n)

    },



    ._set_fn_meta = function(fns) {

      private$._fns <- fns
      private$._fns_n <- length(fns)
      private$._fn_names <- names(fns)

      private$._fns_outcome <- vector('list', length(fns))
      names(private$._fns_outcome) <- names(fns)

      private$._fn_out_lens <- integer(length(fns))

      private$._reset_fns_outcome_names()

      n <- private$._fns_n
      rder <- as.vector(rbind(1:n, n+(1:n)))
      private$._wide_order <- rder
    },



    ._reset_mat_outcome = function() {

      if (private$._mat_n > 0L) {
        private$._mat_outcome <- vector('list', private$._mat_n)
        names(private$._mat_outcome) <- private$._mat_names
      }

    },


    ._reset_row_outcome = function() {

      private$._row_outcome <- vector('list', length(private$._loop_struct$row_groups_for_loop))
      names(private$._row_outcome) <- names(private$._loop_struct$row_groups_for_loop)

    },




    ._reset_col_outcome = function() {

      private$._col_outcome <- vector('list', length(private$._loop_struct$col_groups_for_loop))
      names(private$._col_outcome) <- names(private$._loop_struct$col_groups_for_loop)

    },



    # level 1 is almost always loop on row. Exception only if there is column
    # grouping - but not row grouping (or not rowwise)



    eval_ = function() {
      if (private$._multi_mat) {
        return(private$._eval_multi())
      }

      private$._eval_by_matrix()
      private$._mat_outcome
    },



    ._eval_by_matrix = function() {

      if (private$._mat_n == 0L) return(invisible())

      for (midx in seq_len(private$._mat_n)) {

        if (!private$._loop_struct$matrix_eval[midx]) return(NULL)

        private$._scope$k <- private$._loop_struct$matrix_idx[midx]

        if (!private$._loop_struct$looping) {

          private$._eval_fns()
          private$._mat_outcome[[midx]] <- private$._format_list_of_evals(private$._fns_outcome_formatted, FALSE)
          next
        }


        if (private$._loop_struct$col_looping_first) {

          private$._eval_by_col_groups(inner = FALSE)
          private$._mat_outcome[[midx]] <- private$._col_outcome
          next

        }

        private$._eval_by_row_groups(inner = FALSE)
        private$._mat_outcome[[midx]] <- private$._row_outcome

        if (private$._margin == 0 && private$._loop_struct$row_grouped &&
            private$._loop_struct$col_grouped && private$._simplify == "no") {

          private$._mat_outcome[[midx]] <- tidyr::unnest(private$._mat_outcome[[midx]],
                                                         cols = .vals)
        }

      }



    },





    ._eval_multi = function() {

      if (private$._mat_n == 0L) return(invisible())

      private$._scope$k <- private$._loop_struct$matrix_idx

      if (!private$._loop_struct$looping) {

        private$._eval_fns()
        return(
          private$._format_list_of_evals(private$._fns_outcome_formatted, FALSE)
        )
      }


      if (private$._loop_struct$col_looping_first) {

        private$._eval_by_col_groups(inner = FALSE)
        return(private$._col_outcome)
      }

      private$._eval_by_row_groups(inner = FALSE)

      if (private$._margin == 0 && private$._loop_struct$row_grouped &&
          private$._loop_struct$col_grouped && private$._simplify == "no") {

        return(tidyr::unnest(private$._row_outcome, cols = .vals))
      }

      private$._row_outcome

    },





    ._eval_by_row_groups = function(inner = FALSE) {

      private$._reset_row_outcome()

      for (ridx in seq_along(private$._loop_struct$row_groups_for_loop)) {

        if (private$._loop_struct$row_grouped)
          private$._reset_fns_outcome_names()

        private$._scope$i <- private$._loop_struct$row_groups_for_loop[[ridx]]

        if (inner ||is.null(private$._loop_struct$col_groups_for_loop)) {
          private$._eval_fns(private$._loop_struct$row_grouped)
          private$._row_outcome[[ridx]] <- private$._fns_outcome_formatted
          next
        }

        private$._eval_by_col_groups(!inner)
        private$._row_outcome[[ridx]] <- private$._col_outcome

      }

      private$._format_margin_outcome("row")

    },



    ._eval_by_col_groups = function(inner = TRUE) {

      private$._reset_col_outcome()

      for (cidx in seq_along(private$._loop_struct$col_groups_for_loop)) {

        if (private$._loop_struct$col_grouped)
          private$._reset_fns_outcome_names()

        private$._scope$j <- private$._loop_struct$col_groups_for_loop[[cidx]]

        if (inner || is.null(private$._loop_struct$row_groups_for_loop)) {
          private$._eval_fns(private$._loop_struct$col_grouped)
          private$._col_outcome[[cidx]] <- private$._fns_outcome_formatted
          next
        }

        private$._eval_by_row_groups(!inner)
        private$._col_outcome[[cidx]] <- private$._row_outcome
      }


      private$._format_margin_outcome("col")

    },




    ._format_margin_outcome = function(margin) {

      grouped <- private$._loop_struct[[paste(margin, "grouped", sep = "_")]]
      outcome_id <- paste(".", margin, "outcome", sep = "_")

      if (grouped) {
        outcome_tmp <- private$._loop_struct[[paste(margin, "group_df", sep = "_")]]

        outcome_tmp$.rows <- lapply(private[[outcome_id]], function(o) {
          private$._format_list_of_evals(o, grouped = TRUE)
        })

        private[[outcome_id]] <- dplyr::rename(outcome_tmp, .vals = .rows)
        if (private$._simplify != "no") {
          private[[outcome_id]] <- tidyr::unnest(private[[outcome_id]], cols = .vals)
        }
        return()

      }

      private[[outcome_id]] <- private$._format_list_of_evals(private[[outcome_id]], FALSE)

    },





    ._format_list_of_evals = function(lst, grouped) {

      if (private$._simplify == "no") return(lst)

      if (grouped) return(dplyr::bind_rows(lst))

      dplyr::bind_rows(lst, .id = private$._tag)

    },




    ._eval_fns = function(grouped = FALSE) {

      for (fidx in seq_len(private$._fns_n)) {

        fn <- private$._fns[[fidx]]
        private$._scope$register_function(fn)
        private$._fns_outcome[[fidx]] <- private$._scope$eval()
        private$._format_fn_outcome(fidx)

      }

      private$._format_list_of_fns(grouped)

    },




    ._set_out_lens = function(idx) {

      private$._fn_out_lens[idx] <- length(private$._fns_outcome[[idx]])

    },




    ._set_fns_names = function(idx) {

      private$._set_out_lens(idx)

      if ((!private$._fn_out_lens[idx] == 1L || private$._force_name) &&
          is.null(out_names <- private$._fns_outcome_names[[idx]])) {

        out_names <- make_names(private$._fns_outcome[[idx]], .name = "")
        private$._fns_outcome_names[[idx]] <- out_names
      }

    },




    ._format_fn_outcome = function(idx) {

      private$._set_fns_names(idx)

      if (private$._simplify == "no") return()

      # with length == 1 and no name forcing, it long/wide format is irrelevant
      if (private$._fn_out_lens[idx] == 1L && !private$._force_name) {
        return()
      }


      if (private$._simplify == "long") {
        private$._fns_outcome[[idx]] <- unname(private$._fns_outcome[[idx]])
        return()
      }


      # wide
      invisible()

    },




    ._assess_length = function() {

      lens <- private$._fn_out_lens

      any0 <- any(l0 <- lens == 0L)
      if (any0) lens <- lens[-l0]
      multi <- if (length(lens) > 0) {
        length(unique(lens)) > 1
      } else FALSE

      if (multi) stop("vectors must be of the same length", call. = FALSE)

      invisible()

    },




    ._format_list_of_fns = function(grouped) {


      if (private$._simplify == "no") {
        private$._fns_outcome_formatted <- private$._fns_outcome
        return()
      }

      private$._assess_length()


      # with length == 1 and no name forcing, it long/wide format is irrelevant
      if (unique(private$._fn_out_lens) == 1L && !private$._force_name) {
        private$._fns_outcome_formatted <- private$._fns_outcome
        return()
      }


      if (private$._simplify == "long") {
        nms <- setNames(private$._fns_outcome_names,
                        paste0(names(private$._fns_outcome), ".name"))
        private$._fns_outcome_formatted <- c(nms, private$._fns_outcome)[private$._wide_order]

        if (grouped) {
          private$._fns_outcome_formatted <- list(private$._fns_outcome_formatted)
        }
        return()
      }

      # wide
      private$._fns_outcome_formatted <- list_row(
        flatten_and_name(private$._fns_outcome,
                         private$._fns_outcome_names))

      if (grouped) {
        private$._fns_outcome_formatted <- list(private$._fns_outcome_formatted)
      }

    }

  )
)





quo_is_formula <- function(quo)
{
  rlang::is_formula(rlang::quo_get_expr(quo))
}



quo_is_function <- function(quo)
{
  if (!rlang::quo_is_symbolic(quo)) return(FALSE)
  test_for_fn <- tryCatch(rlang::eval_tidy(quo), error = function(e) e)
  if (inherits(test_for_fn, "error")) return(FALSE)
  rlang::is_function(test_for_fn)
}



get_fn_names <- function(quos)
{
  nmfn <- names(quos)
  is_a_formula <- vapply(quos, quo_is_formula, FALSE)
  if (any(form_idx <- is_a_formula)) {
    nmfn[form_idx] <- gsub("^~", "", nmfn[form_idx])
  }
  nmfn
}



# get_fn_meta <- function(q)
# {
#   nmfn <- names(q)
#   nfn <- length(q)
#   seq_fn <- stats::setNames(seq_len(nfn), nmfn)
#
#   list(fn_names = nmfn, fn_n = nfn, fn_seq = seq_fn)
# }






get_var_tag <- function(margin)
{
  if (is.null(margin) || margin == 0) {
    var_lab_mat
  } else if (margin == 1) {
    var_lab_row
  } else {
    var_lab_col
  }
}





as_fn_expr <- function(x, dot_arg, env = globalenv(), arg = rlang::caller_arg(x),
                       call = rlang::caller_env())
{

  if (quo_is_function(x)) {
    return(rlang::call2(rlang::quo_get_expr(x), as.name(dot_arg)))

  }

  if (quo_is_formula(x)) {

    fn <- rlang::as_function(rlang::eval_tidy(x), env = env, arg = arg, call = call)
    return(body(fn))

  }

  rlang::abort("")

}





# Assess if the name (nms) attributed to function result in apply_* (e.g. avr in
# apply_row_dfl(ms_object, avr = mean)) matches the matrixset object's relevant
# tag (tag). In the example, the relevant tag is the row tag (because apply_row),
# which unless changed by user, is .rowname.
#
# Note that this name is used as column name in the dfl/dfw versions of the
# apply functions. Consequently, the test is relevant only for these apply
# versions.
#
# @param nms         string, the column name with function result
# @param tag         string, the relevant tag
# @param simplify    logical (no default). If TRUE, the test is performed,
#                    otherwise it is skipped.
#
# @returns
# invisible NULL, unless nms == tag, in which case an error condition is issued.
assess_fun_names <- function(nms, tag, simplify)
{
  if (is.na(tag) || !simplify) return(invisible(NULL))
  if (any(nms == tag))
    stop(paste("the function results can't be named", shQuote(tag)))
}





# margin = NULL: mult
#          0: whole matrix
#          1: rowwise
#          2: colwise
eval_function <- function(.ms, ..., margin = NULL, matidx = NULL,
                          .matrix_wise = TRUE, .input_list = FALSE,
                          .simplify = "no", .force_name = FALSE,
                          env = rlang::caller_env(2))
{
  var_tag <- get_var_tag(margin)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")
  # fn_meta <- get_fn_meta(quosures)
  fn_names <- get_fn_names(quosures)


  fns <- lapply(quosures, function(q) {
    as_fn_expr(q, var_tag)
  })


  # make sure we're not giving it the same name as tag. The check is skipped if
  # simplify is FALSE (not dfl/dfw), as this restriction is necessary only to
  # make sure the result tibble in dfl/dfw have column name conflict.
  if (!is.null(margin) && (margin == 0 || margin == 1)) {
    # assess_fun_names(nmfn, .rowtag(.ms), simplify_bool)
    # assess_fun_names(fn_meta$fn_names, .rowtag(.ms), .simplify != "no")
    assess_fun_names(fn_names, .rowtag(.ms), .simplify != "no")
  }
  if (!is.null(margin) && (margin == 0 || margin == 2)) {
    # assess_fun_names(nmfn, .coltag(.ms), simplify_bool)
    # assess_fun_names(fn_meta$fn_names, .coltag(.ms), .simplify != "no")
    assess_fun_names(fn_names, .coltag(.ms), .simplify != "no")
  }

  names(fns) <- fn_names


  applyer <- Applyer$new(.ms, matidx, margin, fns, !.matrix_wise, .input_list,
                         .simplify, .force_name, env)


  # group_meta <- get_group_meta(margin, .ms)


  # applyer$set_row_groups(group_meta$row_group_df$.rows)
  # applyer$set_col_groups(group_meta$col_group_df$.rows)
  # applyer$set_row_groups()
  # applyer$set_col_groups()

  applyer$eval()

}







#' @rdname loop
#' @export
apply_row <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                      .input_list = FALSE)
  UseMethod("apply_row")

#' @export
apply_row.matrixset <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                                .input_list = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin = 1, matidx = .matrix,
                .matrix_wise = .matrix_wise, .input_list = .input_list)
  # if (.matrix_wise) {
  #   # warn_if(.matrix_wise, .input_list)
  #   eval_function(.ms, ..., margin = 1, matidx = .matrix)
  # } #else {
  #   eval_fun_margin_mult(.ms, mrg="row", var_lab=var_lab_row, ...,
  #                        matidx = .matrix, as_list_mat = .input_list,
  #                        .simplify = FALSE, env = rlang::caller_env())
  # }
}



#' @rdname loop
#' @export
apply_row_dfl <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                          .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_row_dfl")

#' @export
apply_row_dfl.matrixset <- function(.ms, ..., .matrix = NULL,
                                    .matrix_wise = TRUE, .input_list = FALSE,
                                    .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin=1, matidx = .matrix, .simplify = "long",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
  # appl <- if (.matrix_wise) {
  #   eval_function(.ms, ..., margin=1, matidx = .matrix, .simplify = "long",
  #                 .force_name = .force_name)
  # } #else {
  #   eval_fun_margin_mult(.ms, mrg="row", var_lab=var_lab_row, ...,
  #                        matidx = .matrix, as_list_mat = .input_list,
  #                        .simplify = TRUE, env = rlang::caller_env())
  # }
  # appl
  # tblize_lg(appl, .rowtag(.ms), mult = !.matrix_wise, force_name = .force_name)
}



#' @rdname loop
#' @export
apply_row_dfw <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                          .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_row_dfw")

#' @export
apply_row_dfw.matrixset <- function(.ms, ..., .matrix = NULL,
                                    .matrix_wise = TRUE, .input_list = FALSE,
                                    .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin = 1, matidx = .matrix, .simplify = "wide",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
  # appl <- if (.matrix_wise) {
  #   eval_function(.ms, ..., margin = 1, matidx = .matrix, .simplify = "wide",
  #                 .force_name = .force_name)
    # eval_fun_margin(.ms, mrg="row", var_lab=var_lab_row, ..., matidx = .matrix,
    #                 .simplify = TRUE, env = rlang::caller_env())
  # } #else {
  #   eval_fun_margin_mult(.ms, mrg="row", var_lab=var_lab_row, ...,
  #                        matidx = .matrix, as_list_mat = .input_list,
  #                        .simplify = TRUE, env = rlang::caller_env())
  # }
  # tblize_wd(appl, .rowtag(.ms), mult = !.matrix_wise, force_name = .force_name)
  # appl
}





#' @rdname loop
#' @export
apply_column <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                         .input_list = FALSE)
  UseMethod("apply_column")

#' @export
apply_column.matrixset <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                                   .input_list = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin=2, matidx = .matrix,
                .matrix_wise = .matrix_wise, .input_list = .input_list)
  # if (.matrix_wise) {
  #   warn_if(.matrix_wise, .input_list)
  #   eval_function(.ms, ..., margin=2, matidx = .matrix)
  #   # eval_fun_margin(.ms, mrg="col", var_lab=var_lab_col, ..., matidx = .matrix,
  #   #                 .simplify = FALSE, env = rlang::caller_env())
  # } #else {
  #   eval_fun_margin_mult(.ms, mrg="col", var_lab=var_lab_col, ...,
  #                        matidx = .matrix, as_list_mat = .input_list,
  #                        .simplify = FALSE, env = rlang::caller_env())
  # }
}



#' @rdname loop
#' @export
apply_column_dfl <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                             .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_column_dfl")

#' @export
apply_column_dfl.matrixset <- function(.ms, ..., .matrix = NULL,
                                       .matrix_wise = TRUE, .input_list = FALSE,
                                       .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin=2, matidx = .matrix, .simplify = "long",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
  # appl <- if (.matrix_wise) {
  #   eval_function(.ms, ..., margin=2, matidx = .matrix, .simplify = "long",
  #                 .force_name = .force_name)
  #   # eval_fun_margin(.ms, mrg="col", var_lab=var_lab_col, ..., matidx = .matrix,
  #   #                 .simplify = TRUE, env = rlang::caller_env())
  # }
  # appl
  #else {
  #   eval_fun_margin_mult(.ms, mrg="col", var_lab=var_lab_col, ...,
  #                        matidx = .matrix, as_list_mat = .input_list,
  #                        .simplify = TRUE, env = rlang::caller_env())
  # }
  # tblize_lg(appl, .coltag(.ms), mult = !.matrix_wise, force_name = .force_name)
}




#' @rdname loop
#' @export
apply_column_dfw <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                             .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_column_dfw")

#' @export
apply_column_dfw.matrixset <- function(.ms, ..., .matrix = NULL,
                                       .matrix_wise = TRUE, .input_list = FALSE,
                                       .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin=2, matidx = .matrix, .simplify = "wide",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
  # appl <- if (.matrix_wise) {
  #   eval_function(.ms, ..., margin=2, matidx = .matrix, .simplify = "wide",
  #                 .force_name = .force_name)
  #   # eval_fun_margin(.ms, mrg="col", var_lab=var_lab_col, ..., matidx = .matrix,
  #   #                 .simplify = TRUE, env = rlang::caller_env())
  # }
  # appl
  #else {
  #   eval_fun_margin_mult(.ms, mrg="col", var_lab=var_lab_col, ...,
  #                        matidx = .matrix, as_list_mat = .input_list,
  #                        .simplify = TRUE, env = rlang::caller_env())
  # }
  # tblize_wd(appl, .coltag(.ms), mult = !.matrix_wise, force_name = .force_name)
}




#' @rdname loop
#' @export
apply_matrix <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                         .input_list = FALSE)
  UseMethod("apply_matrix")


#' @export
apply_matrix.matrixset <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                                   .input_list = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin = 0, matidx = .matrix,
                .matrix_wise = .matrix_wise, .input_list = .input_list)
  # if (.matrix_wise) {
  #   # eval_fun_matrix(.ms, ..., matidx = .matrix, .simplify = FALSE,
  #   #                 env = rlang::caller_env())
  #   eval_function(.ms, ..., margin = 0, matidx = .matrix)
  # } #else {
  #   eval_fun_matrix_mult(.ms, ..., matidx=.matrix, as_list_mat=.input_list,
  #                        .simplify=FALSE, env=rlang::caller_env())
  # }
}





#' @rdname loop
#' @export
apply_matrix_dfl <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                             .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_matrix_dfl")


#' @export
apply_matrix_dfl.matrixset <- function(.ms, ..., .matrix = NULL,
                                       .matrix_wise = TRUE, .input_list = FALSE,
                                       .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin = 0, matidx = .matrix, .simplify = "long",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
  # appl <- if (.matrix_wise) {
  #   # eval_fun_matrix(.ms, ..., matidx = .matrix, .simplify = TRUE,
  #   #                 env = rlang::caller_env())
  #   eval_function(.ms, ..., margin = 0, matidx = .matrix, .simplify = "long",
  #                 .force_name = .force_name)
  # } #else {
  #   eval_fun_matrix_mult(.ms, ..., matidx=.matrix, as_list_mat=.input_list,
  #                        .simplify=TRUE, env=rlang::caller_env())
  # }
  # tblize_lg(appl, "", matrix = TRUE, mult = !.matrix_wise,
  #           force_name = .force_name)
  # appl
}






#' @rdname loop
#' @export
apply_matrix_dfw <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                             .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_matrix_dfw")


#' @export
apply_matrix_dfw.matrixset <- function(.ms, ..., .matrix = NULL,
                                       .matrix_wise = TRUE, .input_list = FALSE,
                                       .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin = 0, matidx = .matrix, .simplify = "wide",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
  # appl <- if (.matrix_wise) {
  #   # eval_fun_matrix(.ms, ..., matidx = .matrix, .simplify = TRUE,
  #   #                 env = rlang::caller_env())
  #   eval_function(.ms, ..., margin = 0, matidx = .matrix, .simplify = "wide",
  #                 .force_name = .force_name)
  # } #else {
  # #   eval_fun_matrix_mult(.ms, ..., matidx=.matrix, as_list_mat=.input_list,
  # #                        .simplify=TRUE, env=rlang::caller_env())
  # # }
  # # tblize_wd(appl, "", matrix = TRUE, mult = !.matrix_wise,
  # #           force_name = .force_name)
  # appl
}



