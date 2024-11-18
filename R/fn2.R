


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






EvalScope <- R6::R6Class(
  "EvalScope",

  public = list(

    initialize = function(.ms, margin, env) {

      private$._enclos_env <- new.env()
      private$._enclos_env$.data <- new.env()
      private$._enclos_env$.env <- env

      private$._mats <- .subset2(.ms, "matrix_set")
      private$._row_inf <- .subset2(.ms, "row_info")
      private$._col_inf <- .subset2(.ms, "column_info")

      private$._set_bindings(margin)
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

    ._enclos_env = NULL,

    ._mats = NULL,
    i_ = NULL,
    j_ = NULL,
    k_ = NULL,

    ._reshape = NULL,

    ._row_inf = NULL,
    ._col_inf = NULL,

    ._fn = NULL,


    ._row_subset_expr = quote(
      {
        if (is.null(private$j_)) {
          return(private$._mats[[private$k_]][private$i_, ])
        }
        private$._mats[[private$k_]][private$i_, private$j_]
      }
    ),



    ._col_subset_expr = quote(
      {
        if (is.null(private$i_)) {
          return(private$._mats[[private$k_]][, private$j_])
        }
        private$._mats[[private$k_]][private$i_, private$j_]
      }
    ),



    ._mat_subset_expr = quote(
      {
        if (is.null(private$i_)) {

          if (is.null(private$j_)) {
            return(private$._mats[[private$k_]])
          }

          return(private$._mats[[private$k_]][, private$j_, drop = FALSE])
        }

        if (is.null(private$j_)) {
          return(private$._mats[[private$k_]][private$i_, , drop = FALSE])
        }

        return(private$._mats[[private$k_]][private$i_, private$j_, drop = FALSE])
      }
    ),





    ._set_bindings_from_single_mat = function(info) {

      active_name <- if (info == 1) {
        var_lab_row
      } else if (info == 2) {
        var_lab_col
      } else var_lab_mat
      not_active_name <- setdiff(c(var_lab_row, var_lab_col, var_lab_mat),
                                 active_name)


      # fn_body <- if (info == 1) {
      #   quote(
      #     {
      #       if (is.null(private$j_)) {
      #         return(private$._mats[[private$k_]][private$i_, ])
      #       }
      #       private$._mats[[private$k_]][private$i_, private$j_]
      #     }
      #   )
      # } else if (info == 2) {
      #   quote(
      #     {
      #       if (is.null(private$i_)) {
      #         return(private$._mats[[private$k_]][, private$j_])
      #       }
      #       private$._mats[[private$k_]][private$i_, private$j_]
      #     }
      #   )
      # } else {
      #
      #   quote(
      #     {
      #       if (is.null(private$i_)) {
      #
      #         if (is.null(private$j_)) {
      #           return(private$._mats[[private$k_]])
      #         }
      #
      #         return(private$._mats[[private$k_]][, private$j_, drop = FALSE])
      #       }
      #
      #       if (is.null(private$j_)) {
      #         return(private$._mats[[private$k_]][private$i_, , drop = FALSE])
      #       }
      #
      #       return(private$._mats[[private$k_]][private$i_, private$j_, drop = FALSE])
      #     }
      #   )
      # }
      fn_body <- if (info == 1) {
        private$._row_subset_expr
      } else if (info == 2) {
        private$._col_subset_expr
      } else {
        private$._mat_subset_expr
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



    ._set_bindings = function(margin) {

      private$._set_bindings_from_single_mat(margin)

      private$._set_bindings_from_inf("row")
      private$._set_bindings_from_inf("col")

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

    initialize = function(.ms, matidx, margin, fns, simplify, force_name, env) {
      private$._scope <- EvalScope$new(.ms, margin, env)

      private$._margin <- margin

      private$._set_matrix_meta(.ms, matidx)

      private$._nr <- nrow(.ms)
      private$._nc <- ncol(.ms)

      private$._row_names <- rownames(.ms)
      private$._col_names <- colnames(.ms)

      private$._tag <- switch(margin,
                              "1" = .rowtag(.ms),
                              "2" = .coltag(.ms),
                              NULL)

      private$._set_group_meta(.ms, margin)
      private$._set_fn_meta(fns)

      # private$._fns <- fns
      private$._simplify <- simplify
      private$._force_name <- force_name
    },



    set_row_groups = function() {

      if (private$._row_wise) {

        row_grs <- as.list(seq_len(private$._nr))
        names(row_grs) <- private$._row_names
        private$._row_groups_for_loop <- row_grs

      } else if (private$._row_grouped) {

        private$._row_groups_for_loop <- private$._row_group_df$.rows

      }

    },



    set_col_groups = function() {

      if (private$._col_wise) {

        col_grs <- as.list(seq_len(private$._nc))
        names(col_grs) <- private$._col_names
        private$._col_groups_for_loop <- col_grs

      } else if (private$._col_grouped) {

        private$._col_groups_for_loop <- private$._col_group_df$.rows

      }

    },





    eval = function() private$eval_()



  ),


  private = list(

    ._scope = NULL,

    ._margin = NULL,


    ._mat_n = NULL,
    ._mat_idx = NULL,
    ._mat_seq = NULL,
    ._mat_names = NULL,

    ._nr = NULL,
    ._nc = NULL,
    ._row_names = NULL,
    ._col_names = NULL,
    ._tag = NULL,

    ._row_wise = NULL,
    ._col_wise = NULL,
    ._row_group_df = NULL,
    ._col_group_df = NULL,
    ._row_grouped = NULL,
    ._col_grouped = NULL,
    ._n_group_row = NULL,
    ._n_group_col = NULL,
    ._row_groups_for_loop = NULL,
    ._col_groups_for_loop = NULL,

    ._fns = list(function(x) x),
    ._fns_n = 0,
    ._fn_names = NULL,

    ._fns_outcome = NULL,
    ._fns_outcome_formatted = NULL,
    ._fns_outcome_names = NULL,
    ._fn_out_lens = NULL,

    ._row_outcome = NULL,
    ._col_outcome = NULL,

    ._wide_order = NULL,

    ._simplify = "no",
    ._force_name = FALSE,




    ._set_matrix_meta = function(.ms, matidx)
    {
      if (is.null(matidx)) {
        nmat <- .nmatrix(.ms)
        matidx <- seq_len(nmat)
        seq_mats <- matidx
        matnms <- matrixnames(.ms)
      } else {
        matnms <- .matrixnames(.ms)
        matidx <- index_to_integer(matidx, .nmatrix(.ms), matnms)
        nmat <- length(matidx)
        seq_mats <- seq_len(nmat)
        matnms <- matnms[matidx]
      }
      matidx <- stats::setNames(matidx, matnms)
      seq_mats <- stats::setNames(seq_mats, matnms)

      private$._mat_n <- nmat
      private$._mat_idx <- matidx
      private$._mat_seq <- seq_mats
      private$._mat_names <- matnms

    },




    ._set_group_meta = function(.ms, margin)
    {
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

      private$._row_wise <- row_wise
      private$._col_wise <- col_wise
      private$._row_group_df <- row_grp_meta
      private$._col_group_df <- col_grp_meta
      private$._row_grouped <- row_grouped
      private$._col_grouped <- col_grouped
      private$._n_group_row <- n_group_row
      private$._n_group_col <- n_group_col

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



    ._reset_row_outcome = function() {

      private$._row_outcome <- vector('list', length(private$._row_groups_for_loop))
      names(private$._row_outcome) <- names(private$._row_groups_for_loop)

    },




    ._reset_col_outcome = function() {

      private$._col_outcome <- vector('list', length(private$._col_groups_for_loop))
      names(private$._col_outcome) <- names(private$._col_groups_for_loop)

    },



    # level 1 is almost always loop on row. Exception only if there is column
    # grouping - but not row grouping (or not rowwise)



    eval_ = function() {
      private$._eval_by_matrix()
    },



    ._eval_by_matrix = function() {

      lapply(private$._mat_idx,
             function(mat_idx) {

               private$._scope$k <- mat_idx

               if (is.null(private$._row_groups_for_loop) &&
                   is.null(private$._col_groups_for_loop)) {

                 # mat_outcomes <- private$._eval_fns()
                 private$._eval_fns()
                 mat_outcomes <- private$._fns_outcome_formatted
                 return(private$._format_list_of_evals(mat_outcomes, FALSE))
               }

               if (!private$._row_grouped &&
                   (private$._col_grouped || is.null(private$._row_groups_for_loop))) {
                 # return(private$._eval_by_col_groups(inner = FALSE))
                 private$._eval_by_col_groups(inner = FALSE)
                 mat_outcomes <- private$._col_outcome
                 return(mat_outcomes)
               }

               # mat_outcomes <- private$._eval_by_row_groups(inner = FALSE)
               private$._eval_by_row_groups(inner = FALSE)
               mat_outcomes <- private$._row_outcome

               if (private$._margin == 0 && private$._row_grouped &&
                   private$._col_grouped && private$._simplify == "no") {

                 mat_outcomes <- tidyr::unnest(mat_outcomes, cols = .vals)
                 }


               mat_outcomes
             })

    },



    ._eval_by_row_groups = function(inner = FALSE) {

      private$._reset_row_outcome()

      for (ridx in seq_along(private$._row_groups_for_loop)) {

        if (private$._row_grouped) private$._reset_fns_outcome_names()

        private$._scope$i <- private$._row_groups_for_loop[[ridx]]

        if (inner ||is.null(private$._col_groups_for_loop)) {
          private$._eval_fns(private$._row_grouped)
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

      for (cidx in seq_along(private$._col_groups_for_loop)) {

        if (private$._col_grouped) private$._reset_fns_outcome_names()

        private$._scope$j <- private$._col_groups_for_loop[[cidx]]

        if (inner || is.null(private$._row_groups_for_loop)) {
          private$._eval_fns(private$._col_grouped)
          private$._col_outcome[[cidx]] <- private$._fns_outcome_formatted
          next
        }

        private$._eval_by_row_groups(!inner)
        private$._col_outcome[[cidx]] <- private$._row_outcome
      }


      private$._format_margin_outcome("col")

    },




    ._format_margin_outcome = function(margin) {


      grouped <- private[[paste(".", margin, "grouped", sep = "_")]]
      outcome_id <- paste(".", margin, "outcome", sep = "_")

      if (grouped) {
        outcome_tmp <- private[[paste(".", margin, "group_df", sep = "_")]]

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
        private$._set_out_lens(fidx)
        private$._set_fns_names(fidx)
        private$._format_fn_outcome(fidx)

      }

      private$._format_list_of_fns(grouped)

    },




    ._set_out_lens = function(idx) {

      private$._fn_out_lens[idx] <- length(private$._fns_outcome[[idx]])

    },




    ._set_fns_names = function(idx) {

      if ((!private$._fn_out_lens[idx] == 1L || private$._force_name) &&
          is.null(out_names <- private$._fns_outcome_names[[idx]])) {

        out_names <- make_names(private$._fns_outcome[[idx]], .name = "")
        private$._fns_outcome_names[[idx]] <- out_names
      }

    },




    # ._format_fn_outcome = function(res) {
    ._format_fn_outcome = function(idx) {

      if (private$._simplify == "no") return()

      # with length == 1 and no name forcing, it long/wide format is irrelevant
      # private$._fn_out_lens[idx] <- length(private$._fns_outcome[[idx]])
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




    # ._format_list_of_fns = function(fn_list, grouped) {
    ._format_list_of_fns = function(grouped) {


      if (private$._simplify == "no") {
        private$._fns_outcome_formatted <- private$._fns_outcome
        return()
      }

      private$._assess_length()


      if (private$._simplify == "long") {
        nms <- setNames(private$._fns_outcome_names,
                        paste0(names(private$._fns_outcome), ".name"))
        # n <- private$._fns_n
        # rder <- as.vector(rbind(1:n, n+(1:n))) # PUT THIS ELSEWHERE AS IT IS DONE FOR EVERY LOOP
        # private$._fns_outcome_formatted <- c(nms, private$._fns_outcome)[rder]
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



get_fn_meta <- function(q)
{
  nmfn <- names(q)
  nfn <- length(q)
  seq_fn <- stats::setNames(seq_len(nfn), nmfn)

  list(fn_names = nmfn, fn_n = nfn, fn_seq = seq_fn)
}





# get_group_meta <- function(margin, .ms)
# {
#   row_wise <- margin %.==.% 1
#   col_wise <- margin %.==.% 2
#
#   row_grp_meta <- attr(.ms, "row_group_meta")
#   col_grp_meta <- attr(.ms, "col_group_meta")
#
#   row_grouped <- !is.null(row_grp_meta) && !row_wise
#   col_grouped <- !is.null(col_grp_meta) && !col_wise
#
#   if (!row_grouped) row_grp_meta <- NULL
#   if (!col_grouped) col_grp_meta <- NULL
#
#   n_group_row <- if (row_grouped) nrow(row_grp_meta) else NULL
#   n_group_col <- if (col_grouped) nrow(col_grp_meta) else NULL
#
#   list(row_wise = row_wise,
#        col_wise = col_wise,
#        row_group_df = row_grp_meta,
#        col_group_df = col_grp_meta,
#        row_grouped = row_grouped,
#        col_grouped = col_grouped,
#        n_group_row = n_group_row,
#        n_group_col =  n_group_col)
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
eval_function <- function(.ms, ..., margin = NULL, matidx = NULL, .simplify = "no",
                          .force_name = FALSE, env = rlang::caller_env(2))
{
  var_tag <- get_var_tag(margin)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")
  fn_meta <- get_fn_meta(quosures)


  fns <- lapply(quosures, function(q) {
    as_fn_expr(q, var_tag)
  })


  # make sure we're not giving it the same name as tag. The check is skipped if
  # simplify is FALSE (not dfl/dfw), as this restriction is necessary only to
  # make sure the result tibble in dfl/dfw have column name conflict.
  if (!is.null(margin) && (margin == 0 || margin == 1)) {
    # assess_fun_names(nmfn, .rowtag(.ms), simplify_bool)
    assess_fun_names(fn_meta$fn_names, .rowtag(.ms), .simplify != "no")
  }
  if (!is.null(margin) && (margin == 0 || margin == 2)) {
    # assess_fun_names(nmfn, .coltag(.ms), simplify_bool)
    assess_fun_names(fn_meta$fn_names, .coltag(.ms), .simplify != "no")
  }


  applyer <- Applyer$new(.ms, matidx, margin, fns, .simplify,
                         .force_name, env)


  # group_meta <- get_group_meta(margin, .ms)


  # applyer$set_row_groups(group_meta$row_group_df$.rows)
  # applyer$set_col_groups(group_meta$col_group_df$.rows)
  applyer$set_row_groups()
  applyer$set_col_groups()

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
  if (.matrix_wise) {
    # warn_if(.matrix_wise, .input_list)
    eval_function(.ms, ..., margin = 1, matidx = .matrix)
  } #else {
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
  appl <- if (.matrix_wise) {
    eval_function(.ms, ..., margin=1, matidx = .matrix, .simplify = "long",
                  .force_name = .force_name)
  } #else {
  #   eval_fun_margin_mult(.ms, mrg="row", var_lab=var_lab_row, ...,
  #                        matidx = .matrix, as_list_mat = .input_list,
  #                        .simplify = TRUE, env = rlang::caller_env())
  # }
  appl
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
  appl <- if (.matrix_wise) {
    eval_function(.ms, ..., margin = 1, matidx = .matrix, .simplify = "wide",
                  .force_name = .force_name)
    # eval_fun_margin(.ms, mrg="row", var_lab=var_lab_row, ..., matidx = .matrix,
    #                 .simplify = TRUE, env = rlang::caller_env())
  } #else {
  #   eval_fun_margin_mult(.ms, mrg="row", var_lab=var_lab_row, ...,
  #                        matidx = .matrix, as_list_mat = .input_list,
  #                        .simplify = TRUE, env = rlang::caller_env())
  # }
  # tblize_wd(appl, .rowtag(.ms), mult = !.matrix_wise, force_name = .force_name)
  appl
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
  if (.matrix_wise) {
    warn_if(.matrix_wise, .input_list)
    eval_function(.ms, ..., margin=2, matidx = .matrix)
    # eval_fun_margin(.ms, mrg="col", var_lab=var_lab_col, ..., matidx = .matrix,
    #                 .simplify = FALSE, env = rlang::caller_env())
  } #else {
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
  appl <- if (.matrix_wise) {
    eval_function(.ms, ..., margin=2, matidx = .matrix, .simplify = "long",
                  .force_name = .force_name)
    # eval_fun_margin(.ms, mrg="col", var_lab=var_lab_col, ..., matidx = .matrix,
    #                 .simplify = TRUE, env = rlang::caller_env())
  }
  appl
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
  appl <- if (.matrix_wise) {
    eval_function(.ms, ..., margin=2, matidx = .matrix, .simplify = "wide",
                  .force_name = .force_name)
    # eval_fun_margin(.ms, mrg="col", var_lab=var_lab_col, ..., matidx = .matrix,
    #                 .simplify = TRUE, env = rlang::caller_env())
  }
  appl
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
  if (.matrix_wise) {
    # eval_fun_matrix(.ms, ..., matidx = .matrix, .simplify = FALSE,
    #                 env = rlang::caller_env())
    eval_function(.ms, ..., margin = 0, matidx = .matrix)
  } #else {
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
  appl <- if (.matrix_wise) {
    # eval_fun_matrix(.ms, ..., matidx = .matrix, .simplify = TRUE,
    #                 env = rlang::caller_env())
    eval_function(.ms, ..., margin = 0, matidx = .matrix, .simplify = "long",
                  .force_name = .force_name)
  } #else {
  #   eval_fun_matrix_mult(.ms, ..., matidx=.matrix, as_list_mat=.input_list,
  #                        .simplify=TRUE, env=rlang::caller_env())
  # }
  # tblize_lg(appl, "", matrix = TRUE, mult = !.matrix_wise,
  #           force_name = .force_name)
  appl
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
  appl <- if (.matrix_wise) {
    # eval_fun_matrix(.ms, ..., matidx = .matrix, .simplify = TRUE,
    #                 env = rlang::caller_env())
    eval_function(.ms, ..., margin = 0, matidx = .matrix, .simplify = "wide",
                  .force_name = .force_name)
  } #else {
  #   eval_fun_matrix_mult(.ms, ..., matidx=.matrix, as_list_mat=.input_list,
  #                        .simplify=TRUE, env=rlang::caller_env())
  # }
  # tblize_wd(appl, "", matrix = TRUE, mult = !.matrix_wise,
  #           force_name = .force_name)
  appl
}



