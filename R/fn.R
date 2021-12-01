
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

                          if (is.null(.gridx)) {

                            idx <- setNames(seq(.n), .names)

                            if (MARGIN == "row") {
                              private$.enclos_dat <- list(
                                lapply(.mats, function(.m) {
                                  lapply(idx, function(i) c(.i=list(.m[i, ]),
                                                            as.list(.rowinf[i, ])))
                                })
                              )

                              private$.info <- list(as.list(.colinf))

                            } else if (MARGIN == "col") {

                              private$.enclos_dat <- list(
                                lapply(.mats, function(.m)
                                {
                                  lapply(idx, function(j) c(.j=list(.m[, j]),
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


                        eval = function(quoS) {
                          v <- lapply(quoS,
                                 function(q) rlang::eval_tidy(q, private$.mask, private$.env))
                          names(v) <- names(quoS)

                          is_vect <- sapply(v, is.vector)
                          lens <- mapply(function(vl, lgl) if (lgl) length(vl) else -1, v, is_vect)

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


eval_fun <- function(margin, ms, ..., matidx, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  if (is.null(matidx)) {
    nmat <- .nmatrix(ms)
    matnms <- matrixnames(ms)
    enclos <- ENCLOS$new(margin, ms$matrix_set, nrow(ms), rownames(ms),
                        ms$row_info, ms$column_info, env,
                        column_group_where(ms))
    # enclos <- ENCLOS$new(margin, ms$matrix_set, ms$row_info, ms$column_info,
    #                      env)
  } else {
    matidx <- index_to_integer(matidx, nmatrix(ms), matrixnames(ms))
    nmat <- length(matidx)
    matnms <- matrixnames(ms)[matidx]
    enclos <- ENCLOS$new(margin, ms$matrix_set[matidx], nrow(ms), rownames(ms),
                         ms$row_info, ms$column_info, env,
                         column_group_where(ms))
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

  parts <- vector("list", nrow(ms))
  names(parts) <- rownames(ms)

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
      for (i in 1:nrow(ms))
      {
        if (is.null(group_lbl)) enclos$update(k, i) else enclos$update(k, i, gr)
        pts <- enclos$eval(quosures)
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


#' Apply functions to each matrix row of a matrixset
#'
#' @description
#' The `row_loop` functions transform their input by applying a function to each
#' matrix row of a `matrixset`. The function can be applied to all matrices or
#' only a subset.
#'
#' @details
#' If `matrix` is left unspecified (or given as `NULL`), all matrices will be
#' replaced by `value`. How replacement exactly occurs depends on `value` itself.
#'
#' If `value` is a single atomic `vector` (this excludes lists) or `matrix`,
#' relevant subscripts of all requested matrices will be replaced by the same
#' `value`. This is conditional to the dimensions being compatible.
#'
#' Alternatively, `value` can be a list of atomic vectors/matrices. If `value`
#' has a single element, the same rules as above apply. Otherwise, the length
#' of `value` must match the number of matrices for which subscripts have to be
#' replaced.
#'
#' If the list elements are named, the names are matched to the names of the
#' matrices that need replacement - in which case `value` needs not to be the
#' same length.
#'
#' A final possibility for `value` is for it to be `NULL`. In this case, target
#' matrices are turned to `NULL`.
#'
#' @section vector `value`:
#' Contrarily to `matrix` replacement, when submitting an atomic `vector`
#' `value`, dimensions must match exactly.
#'
#' @section Replacing `NULL` matrices:
#' Replacing subscripts of `NULL` matrices is not possible, unless `value` is
#' itself `NULL`, or a matrix the same dimensions (number of rows and columns)
#' as `x`. If `x` has dimnames, `value` must have the same dimnames.
#'
#' @param x    `matrixset` object from which to replace element(s)
#' @param i,j    Indices specifying elements to replace. Indices are numeric or
#'     character vectors or empty (`NULL`). Note that treating `NULL` as empty
#'     differs from the usual replacement, where it is treated as `integer(0)`.
#'     Here a `NULL` (empty) results in selecting all rows or columns.
#'
#'     Numeric values are coerced to integer as by [as.integer()] (and hence
#'     truncated towards zero).
#'
#'     Character vectors will be matched to the dimnames of the object.
#'
#'     Can also be logical vectors, indicating elements/slices to replace Such
#'     vectors are **NOT** recycled, which is an important difference with usual
#'     matrix replacement. It means that the logical vector must match the
#'     object dimension in length.
#'
#'     Can also be negative integers, indicating elements/slices to leave out of
#'     the replacement.
#'
#'     When indexing, a single argument `i` can be a matrix with two columns.
#'     This is treated as if the first column was the `i` index and the second
#'     column the `j` index.
#'
#' @param matrix    index specifying matrix or matrices to replace. Index is
#'                  numeric or character vectors or empty (\code{NULL}). Note
#'                  that treating \code{NULL} as empty differs from the usual
#'                  replacement, where it is treated as \code{integer(0)}. Here
#'                  a \code{NULL} (empty) results in replacing all matrices.
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
#' @param value    object to use as replacement value
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
#'
#' @noRd
row_loop <- function(.ms, ..., .matrix = NULL, .prefix = "", .sep = " ")
  UseMethod("row_loop")



row_loop.matrixset <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ...,
                       matidx=.matrix, env=rlang::caller_env())
  eval_obj$vals
}



row_loop.row_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  NextMethod()
}




row_loop.col_grouped_ms <- function(.ms, ..., .matrix = NULL)
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ...,
                       matidx=.matrix, env=rlang::caller_env())
  vals <- column_group_meta(.ms)
  vals$.rows <- eval_obj$vals
  vals
}





row_loop_tbl.matrixset <- function(.ms, ..., .matrix = NULL, .prefix = "",
                                   .sep = " ")
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ...,
                       matidx=.matrix, env=rlang::caller_env())

  purrr::map2(eval_obj$vals, eval_obj$lens,
              function(vals, lens) {
                lens_unq <- unique(lens)
                if (length(lens_unq) > 1)
                  stop("vectors must be of the same length")
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

                } else if (lens_unq < 1) {
                  stop("function results must be non-empty vectors")
                } else {
                  tbls <- purrr::map(vals, tibble::as_tibble)
                  dplyr::bind_rows(tbls , .id = .rowtag(.ms) )
                }
              })


}




row_loop_tbw.matrixset <- function(.ms, ..., .matrix = NULL, .prefix = "",
                                   .sep = " ")
{
  eval_obj <- eval_fun(margin="row", ms=.ms, ...,
                       matidx=.matrix, env=rlang::caller_env())

  purrr::map2(eval_obj$vals, eval_obj$lens,
              function(vals, lens) {
                if (any(lens < 1)) {
                  stop("function results must be non-empty vectors")
                } else if (any(lens > 1)) {

                  purrr::map_dfr(vals,
                                 function(u) {
                                   purrr::imap_dfc(u, function(v, nm) {
                                     names(v) <- concat(nm, make_names(v, ""), sep = .sep)

                                     tibble::as_tibble_row(v)
                                   }
                                   )
                                 }, .id = .rowtag(.ms))

                } else {
                  tbls <- purrr::map(vals, tibble::as_tibble)
                  dplyr::bind_rows(tbls , .id = .rowtag(.ms))
                }
              })

}





