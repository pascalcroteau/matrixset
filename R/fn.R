
# GROUPS VIA UPDATE?
# I'D SAY A GROUP INDEX THAT WOULD APPLY TO BOTH PRIVATE$.COL_DATA$.I (AND ROW WHEN APPRROPRIATE)
# AND ENCOLS


# THINK ABOUT 'CURRENT'

# ENCLOS <- R6::R6Class("ENCLOS",
#
#                       public = list(
#
#                         initialize = function(MARGIN, .mats, .rowinf, .colinf, ENV) {
#                           private$.margin <- MARGIN
#                           private$.matrices <- .mats
#                           private$.row_data <- .rowinf
#                           private$.col_data <- .colinf
#                           private$.env <- ENV
#                         },
#
#                         update = function(mat, idx, gr_idx = NULL) {
#                           if (private$.margin == "row") {
#                             private$.col_data$.i <- private$.matrices[[mat]][idx,]
#                             if (is.null(gr_idx)) {
#                               private$.enclos <- c(as.list(private$.row_data[idx,,drop = FALSE]),
#                                                    as.list(private$.col_data))
#                             } else {
#                               private$.enclos <- c(as.list(private$.row_data[idx,,drop = FALSE]),
#                                                    as.list(private$.col_data[gr_idx,,drop = FALSE]))
#                             }
#
#                           } else if (private$.margin == "col") {
#                             private$.row_data$.j <- private$.matrices[[mat]][,idx]
#                             if (is.null(gr_idr)) {
#                               private$.enclos <- c(as.list(private$.col_data[idx,,drop = FALSE]),
#                                                    as.list(private$.row_data))
#                             } else {
#                               private$.enclos <- c(as.list(private$.col_data[idx,,drop = FALSE]),
#                                                    as.list(private$.row_data[gr_idx,,drop=FALSE]))
#                             }
#                           }
#                         },
#
#                         eval = function(quoS) {
#                           mask <- rlang::as_data_mask(private$.enclos)
#                           v <- lapply(quoS,
#                                  function(q) rlang::eval_tidy(q, mask, private$.env))
#                           names(v) <- names(quoS)
#
#                           is_vect <- sapply(v, is.vector)
#                           lens <- mapply(function(vl, lgl) if (lgl) length(vl) else -1, v, is_vect)
#
#                           list(v=v, lens=lens)
#                         }
#
#                       ),
#
#                       private = list(
#                         .margin = NULL,
#                         .matrices = NULL,
#                         .row_data = NULL,
#                         .col_data = NULL,
#                         .enclos = NULL,
#                         .env = NULL
#                       )
#
#                       )





ENCLOS <- R6::R6Class("ENCLOS",

                      public = list(

                        # .gridx is the result of row_group_where (or column)
                        initialize = function(MARGIN, .mats, .n, .names,
                                              .rowinf, .colinf, ENV,
                                              .gridx = NULL) {

                          private$.margin <- MARGIN

                          idx <- setNames(seq(.n), .names)

                          if (is.null(.gridx)) {

                            if (MARGIN == "row") {
                              private$.enclos_dat <- list(
                                lapply(.mats, function(.m) {
                                  lapply(idx, function(i) c(.i=list(.m[i, ]),
                                                            .__is_null_ = is.null(.m),
                                                            as.list(.rowinf[i, ])))
                                })
                              )

                              private$.info <- list(as.list(.colinf))

                            } else if (MARGIN == "col") {

                              private$.enclos_dat <- list(
                                lapply(.mats, function(.m)
                                {
                                  lapply(idx, function(j) c(.j=list(.m[, j]),
                                                            .__is_null_ = is.null(.m),
                                                            as.list(.colinf[j, ])))
                                })
                              )

                              private$.info <- list(as.list(.rowinf))

                            }

                          } else {
                            if (MARGIN == "row") {
                              private$.enclos_dat <- lapply(.gridx, function(gr) {
                                lapply(.mats, function(.m)
                                {
                                  lapply(idx, function(i) c(.i=list(.m[i, gr]),
                                                            .__is_null_ = is.null(.m),
                                                            as.list(.rowinf[i, ])))
                                })
                              })

                              private$.info <- lapply(.gridx, function(gr) {
                                as.list(.colinf[gr, ])
                              })

                            } else if (MARGIN == "col") {
                              private$.enclos_dat <- lapply(.gridx, function(gr) {
                                lapply(.mats, function(.m)
                                {
                                  lapply(idx, function(j) c(.j=list(.m[gr, j]),
                                                            .__is_null_ = is.null(.m),
                                                            as.list(.colinf[j, ])))
                                })
                              })

                              private$.info <- lapply(.gridx, function(gr) {
                                as.list(.rowinf[gr, ])
                              })

                            }
                          }

                          private$.enclos <- list2env(
                            c(private$.info[[1]],
                              private$.enclos_dat[[1]][[1]][[1]])
                          )
                          private$.mask <- rlang::new_data_mask(private$.enclos)
                          private$.mask$.data <- rlang::as_data_pronoun(private$.mask)


                          private$.info_names <- names(private$.info[[1]])
                          private$.dat_names <- names(private$.enclos_dat[[1]][[1]][[1]])
                          private$.env <- ENV
                        },


                        update = function(mat, idx, gr_idx = NULL) {

                          if (is.null(gr_idx)) gr_idx <- 1

                          if (gr_idx > 1) {
                            for (nm in private$.info_names)
                              assign(nm, private$.info[[gr_idx]][[nm]], private$.enclos)

                            for (nm in private$.dat_names)
                              assign(nm, private$.enclos_dat[[gr_idx]][[mat]][[idx]][[nm]], private$.enclos)
                          } else if (idx > 1 || mat > 1) {
                            for (nm in private$.dat_names)
                              assign(nm, private$.enclos_dat[[gr_idx]][[mat]][[idx]][[nm]], private$.enclos)
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
                          # lens <- mapply(function(vl, lgl) if (lgl) length(vl) else -1, v, is_vect)
                          lens <- mapply(function(vl, lgl) if (is_null_obj(vl)) -1 else length(vl), v, is_vect)

                          list(v=v, lens=lens)
                        }

                      ),

                      private = list(
                        .margin = NULL,
                        .info = NULL,
                        .info_names = NULL,
                        .dat_names = NULL,
                        .enclos_dat = NULL,
                        .enclos = NULL,
                        .mask = NULL,
                        .env = NULL
                      )

                      )






norm_call <- function(quo, var)
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
  if (is.name(expr)) expr <- rlang::call2(expr, as.name(var))
  if (colon) expr <- call("::", pkg, expr)
  rlang::quo_set_expr(quo, expr)
}



eval_fun <- function(margin, ms, ..., matidx, .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  if (is.null(ms$matrix_set)) return(list(vals = NULL, lens = NULL))

  if (margin == "row") {
    n <- nrow(ms)
    nms <- rownames(ms)
    gr_idx <- column_group_where(ms)
  } else if (margin == "col") {
    n <- ncol(ms)
    nms <- colnames(ms)
    gr_idx <- row_group_where(ms)
  }

  if (is.null(matidx)) {
    nmat <- .nmatrix(ms)
    matnms <- matrixnames(ms)
    enclos <- ENCLOS$new(margin, ms$matrix_set, n, nms, ms$row_info,
                         ms$column_info, env, gr_idx)
    # enclos <- ENCLOS$new(margin, ms$matrix_set, ms$row_info, ms$column_info,
    #                      env)
  } else {
    matidx <- index_to_integer(matidx, nmatrix(ms), matrixnames(ms))
    nmat <- length(matidx)
    matnms <- matrixnames(ms)[matidx]
    enclos <- ENCLOS$new(margin, ms$matrix_set[matidx], n, nms, ms$row_info,
                         ms$column_info, env, gr_idx)
    # enclos <- ENCLOS$new(margin, ms$matrix_set[matidx], ms$row_info,
    #                      ms$column_info, env)
  }

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")
  var_lab <- switch(margin, "row" = ".i", "col"=".j")

  for (i in seq_along(quosures)) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }

  group_lbl <- switch(margin,
                      "row" = column_group_keys(ms),
                      "col" = row_group_keys(ms))

  rowv <- vector("list", nmat)
  names(rowv) <- matnms

  parts <- vector("list", n)
  names(parts) <- nms

  lens <- vector("list", nmat)

  if (!is.null(group_lbl)) {
    ngroup <- nrow(group_lbl)
    grouprow <- vector("list", ngroup)
  } else ngroup <- 1

  for (gr in seq(ngroup))
  {
    for (k in 1:nmat)
    {
      l <- NULL
      for (i in 1:n)
      {
        if (is.null(group_lbl)) enclos$update(k, i) else enclos$update(k, i, gr)
        pts <- enclos$eval(quosures, .simplify)
        parts[[i]] <- pts$v
        l <- union(l, unique(pts$lens))
      }
      rowv[[k]] <- parts
      lens[[k]] <- l
    }

    if (!is.null(group_lbl)) grouprow[[gr]] <- list(row_vals = rowv, lens = lens)
  }

  if (!is.null(group_lbl)) {
    rowv <- purrr::map(grouprow, 1)
    lens <- purrr::map(grouprow, 2)
  }

  list(vals=rowv, lens=lens)
}


#' Apply functions to each matrix row/column of a matrixset
#'
#' @description
#' The `row_loop`/`column_loop` functions apply functions to each matrix
#' row/column of a `matrixset`. The functions can be applied to all matrices or
#' only a subset.
#'
#' The `dfl`/`dfw` versions differ in their output format.
#'
#' @section vector `value`:
#' Contrarily to `matrix` replacement, when submitting an atomic `vector`
#' `value`, dimensions must match exactly.
#'
#'
#' @param .ms    `matrixset` object
#' @param ...    functions, separated by commas. They can be specified in one of
#'     the following way:
#'
#'    * a function name, e.g., `mean`.
#'    * a function call, where you can use `.i` to represent the current row
#'       (for `row_loop`) and `.j` for the current column (`column_loop`). Bare
#'       names of object traits can be used as well. For instance,
#'       `lm(.i ~ program)`.
#'    * a formula expression. See examples to see the usefulness of this.
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
#' @returns
#' A list for every matrix in the matrixset object. Each list is itself a list,
#' one element for each row/column. And finally, each of these sub-list is a
#' list, the results of each function.
#'
#' If each function returns a vector of the same dimension, you can use either
#' the `_dfl` or the `_dfw` version. What they do is to return a list of
#' `tibble`s. The `dfl` version will stack the function results, in a long
#' format while the `dfw` version will put them side-by-side, in a wide format.
#'
#' See the grouping section to learn about the result format in the grouping
#' context.
#'
#' @examples
#' # an hypothetical example of students that failed 3 courses and their results
#' # after remedial class
#' set.seed(1121)
#' failure <- matrix(runif(20*3,0,.5), 20, 3)
#' remedial <- matrix(c(c(runif(10, 0.55, 0.75), runif(10, 0.65, 0.8)),
#'                      runif(20, 0.65, 0.90),
#'                      c(runif(10, 0.6, 0.8), runif(10, 0.65, 0.95))), 20, 3)
#' rownames(failure) <- rownames(remedial) <- paste("student", 1:20)
#' colnames(failure) <- colnames(remedial) <- c("Mathematics", "English", "Science")
#' student_info <- data.frame(student = paste("student", 1:20),
#'                            class = gl(4,5,labels = paste0("class", LETTERS[1:4])),
#'                            teacher = gl(2,10,labels = paste0("Professor", 1:2)))
#' student_results <- matrixset(failure = failure, remedial = remedial,
#'                              row_info = student_info, row_key = "student")
#'
#' # you can replace a line for all matrices at once. In the example, the "wrong"
#' # tag refers to the fact that the 'failure' results do not make sense after
#' # replacement
#' student_results_wrong <- student_results
#' student_results_wrong["student 2",,] <- c(0.81, 0.88, 0.71) # obviously, integer index works too
#' # note how all matrices had the same replacement
#' student_results_wrong
#'
#' # this already makes more sense in the context of the example
#' student_results[2,,] <- list(c(0,0.45,0.1), c(0.81, 0.88, 0.71))
#' student_results
#'
#' # or even these two equivalent commands
#' student_results["student 2",,"remedial"] <- c(0.77, 0.83, 0.75)
#' student_results[2,,2] <- matrix(c(0.77, 0.83, 0.75), 1, 3)
#'
#' @name loop
#' @export
row_loop <- function(.ms, ..., .matrix = NULL)
  UseMethod("row_loop")


#' @rdname loop
#' @export
row_loop.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       .simplify = FALSE, env=rlang::caller_env())
  eval_obj$vals
}


#' @rdname loop
#' @export
row_loop.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  NextMethod()
}


#' @rdname loop
#' @export
row_loop.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  vals <- column_group_meta(.ms)
  vals$.rows <- NextMethod()
  vals
}



#' @rdname loop
#' @export
column_loop <- function(.ms, ..., .matrix = NULL)
  UseMethod("column_loop")


#' @rdname loop
#' @export
column_loop.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       .simplify = FALSE, env=rlang::caller_env())
  eval_obj$vals
}


#' @rdname loop
#' @export
column_loop.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  NextMethod()
}


#' @rdname loop
#' @export
column_loop.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  vals <- row_group_meta(.ms)
  vals$.columns <- NextMethod()
  vals$.rows <- NULL
  vals
}




#' @rdname loop
#' @export
row_loop_dfl <- function(.ms, ..., .matrix = NULL, .prefix = "", .sep = " ")
  UseMethod("row_loop_dfl")


#' @rdname loop
#' @export
row_loop_dfl.matrixset <- function(.ms, ..., .matrix = NULL, .prefix = "",
                                   .sep = " ")
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       .simplify = TRUE, env=rlang::caller_env())

  if (is.null(eval_obj$vals)) return(NULL)

  purrr::map2(eval_obj$vals, eval_obj$lens,
              function(vals, lens) {
                lens_unq <- unique(lens)
                if (length(lens_unq) > 1)
                  stop("vectors must be of the same length", call. = FALSE)
                if (lens_unq > 1) {
                  purrr::map_dfr(vals,
                                 function(u) {
                                   purrr::imap_dfc(u, function(v, nm) {
                                     names(v) <- make_names(v, "")
                                     tibble::enframe(v,
                                                     name = paste0(.prefix, nm, ".name"),
                                                     value = nm)}
                                   )
                                 }, .id = .rowtag(.ms))

                } else if (lens_unq == 0L) {
                  stop("function results must be non-empty vectors")
                } else {
                  tbls <- purrr::map(vals, tibble::as_tibble)
                  dplyr::bind_rows(tbls , .id = .rowtag(.ms) )
                }
              })


}





#' @rdname loop
#' @export
column_loop_dfl <- function(.ms, ..., .matrix = NULL, .prefix = "", .sep = " ")
  UseMethod("column_loop_dfl")


#' @rdname loop
#' @export
column_loop_dfl.matrixset <- function(.ms, ..., .matrix = NULL, .prefix = "",
                                      .sep = " ")
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       .simplify = TRUE, env=rlang::caller_env())

  if (is.null(eval_obj$vals)) return(NULL)

  purrr::map2(eval_obj$vals, eval_obj$lens,
              function(vals, lens) {
                lens_unq <- unique(lens)
                if (length(lens_unq) > 1)
                  stop("vectors must be of the same length", call. = FALSE)
                if (lens_unq > 1) {
                  purrr::map_dfr(vals,
                                 function(u) {
                                   purrr::imap_dfc(u, function(v, nm) {
                                     names(v) <- make_names(v, "")
                                     tibble::enframe(v,
                                                     name = paste0(.prefix, nm, ".name"),
                                                     value = nm)}
                                   )
                                 }, .id = .coltag(.ms))

                } else if (lens_unq == 0L) {
                  stop("function results must be non-empty vectors")
                } else {
                  tbls <- purrr::map(vals, tibble::as_tibble)
                  dplyr::bind_rows(tbls , .id = .coltag(.ms) )
                }
              })


}






#' @rdname loop
#' @export
row_loop_dfw <- function(.ms, ..., .matrix = NULL, .prefix = "", .sep = " ")
  UseMethod("row_loop_dfw")


#' @rdname loop
#' @export
row_loop_dfw.matrixset <- function(.ms, ..., .matrix = NULL, .prefix = "",
                                   .sep = " ")
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       .simplify = TRUE, env=rlang::caller_env())

  if (is.null(eval_obj$vals)) return(NULL)

  purrr::map2(eval_obj$vals, eval_obj$lens,
              function(vals, lens) {
                lens_unq <- unique(lens)
                if (length(lens_unq) > 1)
                  stop("vectors must be of the same length", call. = FALSE)

                if (any(lens == 0L)) {
                  stop("function results must be non-empty vectors")
                } else if (any(lens > 1)) {

                  purrr::map_dfr(vals,
                                 function(u) {
                                   purrr::imap_dfc(u, function(v, nm) {
                                     # names(v) <- concat(nm, make_names(v, ""), sep = .sep)

                                     # tibble::as_tibble_row(v)
                                     names(v) <- concat(nm, make_names(v, ""), sep = .sep)
                                     df <- tibble::enframe(v, value = nm)
                                     tidyr::pivot_wider(df, names_from = "name",
                                                        values_from = tidyselect::all_of(nm))
                                   }
                                   )
                                 }, .id = .rowtag(.ms))

                } else {
                  tbls <- purrr::map(vals, tibble::as_tibble)
                  dplyr::bind_rows(tbls , .id = .rowtag(.ms))
                }
              })

}



#' @rdname loop
#' @export
column_loop_dfw <- function(.ms, ..., .matrix = NULL, .prefix = "", .sep = " ")
  UseMethod("column_loop_dfw")


#' @rdname loop
#' @export
column_loop_dfw.matrixset <- function(.ms, ..., .matrix = NULL, .prefix = "",
                                      .sep = " ")
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       .simplify = TRUE, env=rlang::caller_env())

  if (is.null(eval_obj$vals)) return(NULL)

  purrr::map2(eval_obj$vals, eval_obj$lens,
              function(vals, lens) {
                lens_unq <- unique(lens)
                if (length(lens_unq) > 1)
                  stop("vectors must be of the same length", call. = FALSE)

                if (any(lens == 0L)) {
                  stop("function results must be non-empty vectors")
                } else if (any(lens > 1)) {

                  purrr::map_dfr(vals,
                                 function(u) {
                                   purrr::imap_dfc(u, function(v, nm) {
                                     # names(v) <- concat(nm, make_names(v, ""), sep = .sep)

                                     # tibble::as_tibble_row(v)
                                     names(v) <- concat(nm, make_names(v, ""), sep = .sep)
                                     df <- tibble::enframe(v, value = nm)
                                     tidyr::pivot_wider(df, names_from = "name",
                                                        values_from = tidyselect::all_of(nm))
                                   }
                                   )
                                 }, .id = .coltag(.ms))

                } else {
                  tbls <- purrr::map(vals, tibble::as_tibble)
                  dplyr::bind_rows(tbls , .id = .coltag(.ms))
                }
              })

}





