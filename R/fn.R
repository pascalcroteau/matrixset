
# GROUPS VIA UPDATE?
# I'D SAY A GROUP INDEX THAT WOULD APPLY TO BOTH PRIVATE$.COL_DATA$.I (AND ROW WHEN APPRROPRIATE)
# AND ENCOLS


# THINK ABOUT 'CURRENT'





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
                              private$.enclos_dat <- lapply(.mats, function(.m) {
                                list(
                                  lapply(idx, function(i) c(.i=list(.m[i, ]),
                                                            .__is_null_ = is.null(.m),
                                                            as.list(.rowinf[i, ])))
                                )
                              })

                              private$.info <- list(as.list(.colinf))

                            } else if (MARGIN == "col") {

                              private$.enclos_dat <- lapply(.mats, function(.m)
                              {
                                list(
                                  lapply(idx, function(j) c(.j=list(.m[, j]),
                                                            .__is_null_ = is.null(.m),
                                                            as.list(.colinf[j, ])))
                                )
                              })

                              private$.info <- list(as.list(.rowinf))

                            }

                          } else {
                            if (MARGIN == "row") {
                              private$.enclos_dat <- lapply(.mats, function(.m) {
                                lapply(.gridx, function(gr)
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
                              private$.enclos_dat <- lapply(.mats, function(.m) {
                                lapply(.gridx, function(gr)
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
                              assign(nm, private$.enclos_dat[[mat]][[gr_idx]][[idx]][[nm]], private$.enclos)
                          } else if (idx > 1 || mat > 1) {
                            for (nm in private$.dat_names)
                              assign(nm, private$.enclos_dat[[mat]][[gr_idx]][[idx]][[nm]], private$.enclos)
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
                          lens <- mapply(function(vl, lgl) if (is_null_obj(vl)) -1 else if(lgl) length(vl) else 0, v, is_vect)

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





#' @importFrom rlang :=
eval_fun <- function(margin, ms, ..., matidx, .simplify, env)
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

  # lens <- vector("list", nmat)

  if (!is.null(group_lbl)) {
    ngroup <- nrow(group_lbl)
    grouprow <- vector("list", ngroup)
  } else {
    ngroup <- 1
  }


  for (k in 1:nmat)
  {
    for (gr in seq(ngroup))
    {
      l <- NULL
      for (i in 1:n)
      {
        if (is.null(group_lbl)) enclos$update(k, i) else enclos$update(k, i, gr)
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

        parts[[i]] <- vals
      }

      if (is.null(group_lbl)) grouprow <- parts else grouprow[[gr]] <- parts

    }
    rowv[[k]] <- grouprow
  }

  rowv
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


#' @export
row_loop.NULL <- function(.ms, ..., .matrix = NULL) NULL


#' @rdname loop
#' @export
row_loop.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
           .simplify = FALSE, env=rlang::caller_env())
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
  ans <- column_group_meta(.ms)
  vals <- NextMethod()
  lapply(vals, function(v) {
    ans$.rows <- v
    ans
  })
}



#' @rdname loop
#' @export
column_loop <- function(.ms, ..., .matrix = NULL)
  UseMethod("column_loop")



#' @export
column_loop.NULL <- function(.ms, ..., .matrix = NULL) NULL


#' @rdname loop
#' @export
column_loop.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
           .simplify = FALSE, env=rlang::caller_env())
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
  ans_tmp <- row_group_meta(.ms)
  vals <- NextMethod()
  ans <- lapply(vals, function(v) {
    ans_tmp$.columns <- v
    ans_tmp$.rows <- NULL
    ans_tmp
  })
  ans
}




#' @rdname loop
#' @export
row_loop_dfl <- function(.ms, ..., .matrix = NULL)
  UseMethod("row_loop_dfl")


#' @export
row_loop_dfl.NULL <- function(.ms, ..., .matrix = NULL) NULL


#' @rdname loop
#' @export
row_loop_dfl.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       .simplify = "long", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  lapply(eval_obj, function(vals) {
    dplyr::bind_rows(vals, .id = .rowtag(.ms))
  })

}


#' @rdname loop
#' @export
row_loop.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  NextMethod()
}


#' @rdname loop
#' @export
row_loop_dfl.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       .simplify = "long", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- column_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr,
                                                                      .id = .rowtag(.ms))))
  })
}





#' @rdname loop
#' @export
column_loop_dfl <- function(.ms, ..., .matrix = NULL)
  UseMethod("column_loop_dfl")


#' @export
column_loop_dfl.NULL <- function(.ms, ..., .matrix = NULL) NULL


#' @rdname loop
#' @export
column_loop_dfl.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       .simplify = "long", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  lapply(eval_obj, function(vals) {
    dplyr::bind_rows(vals, .id = .coltag(.ms))
  })

}



#' @rdname loop
#' @export
column_loop.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  NextMethod()
}


#' @rdname loop
#' @export
column_loop_dfl.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       .simplify = "long", env=rlang::caller_env())

  if (is.null(eval_obj)) return(NULL)

  group_inf <- row_group_keys(.ms)

  lapply(eval_obj, function(mats) {

    purrr::map2_dfr(mats, seq_along(mats),
                    function(gr, i) dplyr::bind_cols(group_inf[i, ],
                                                     dplyr::bind_rows(gr,
                                                                      .id = .coltag(.ms))))
  })
}





#' @rdname loop
#' @export
row_loop_dfw <- function(.ms, ..., .matrix = NULL)
  UseMethod("row_loop_dfw")


#' @rdname loop
#' @export
row_loop_dfw.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ..., matidx=.matrix,
                       .simplify = "wide", env=rlang::caller_env())

  if (is.null(eval_obj$vals)) return(NULL)

  lapply(eval_obj$vals, function(vals) {
    dplyr::bind_rows(vals, .id = .rowtag(.ms))
  })

}



#' @rdname loop
#' @export
column_loop_dfw <- function(.ms, ..., .matrix = NULL)
  UseMethod("column_loop_dfw")


#' @rdname loop
#' @export
column_loop_dfw.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="col", ms=.ms, ..., matidx=.matrix,
                       .simplify = "wide", env=rlang::caller_env())

  if (is.null(eval_obj$vals)) return(NULL)

  lapply(eval_obj$vals, function(vals) {
    dplyr::bind_rows(vals, .id = .coltag(.ms))
  })

}





