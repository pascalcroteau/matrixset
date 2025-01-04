


.matrixnames <- function(x) attr(x, "matrix_names")
.nmatrix <- function(x) attr(x, "n_matrix")
.rowtag <- function(x) attr(x, "row_tag")
.coltag <- function(x) attr(x, "col_tag")
.rowtraits <- function(x)
{
  rt <- attr(x, "row_traits")
  rt[rt != .rowtag(x)]
}
.coltraits <- function(x)
{
  ct <- attr(x, "col_traits")
  ct[ct != .coltag(x)]
}

.is_row_grouped <- function(x) !is.null(attr(x, "row_group_vars"))
.is_col_grouped <- function(x) !is.null(attr(x, "col_group_vars"))


#' Matrixset properties
#'
#' @description
#' Utility functions to extract relevant information from a `matrixset` object.
#'
#' @details
#' `ìs_matrixset` tests if its argument is a proper `matrixset` object.
#'
#' `dim` retrieves the dimension of the `matrixset` matrices (which are the
#' same for reach). Similarly,`nrow`  returns the number of rows for each
#' matrices, and `ncol` returns the number of columns.
#'
#' `dimnames` retrieves the dimnames of the `matrixset` matrices (which are the
#' same for reach). Similarly, `rownames` (`colnames`) will retrieve row
#' (column) names.
#'
#' `matrixnames` retrieves the matrix names, or `NULL` if the matrices are not
#' named.
#'
#' `nmatrix` returns the number of matrices of a `matrixset`.
#'
#' `row_traits` returns the object's row traits; these are the column names of
#' the row annotation data frame.
#'
#' `column_traits` returns the object's column traits; these are the column
#' names of the column annotation data frame.
#'
#' `row_info` extracts the row annotation data frame. `column_info` does
#' the same thing for column annotation.
#'
#' `row_tag` returns the column name of `row_info` that stores the `matrixset`'s
#' row names. `column_tag` returns the column name of `column_info` that stores
#' the `matrixset`'s column names.
#'
#' The replacement methods for `row_traits`/`row_info` and `column_traits`/`column_info`
#' can potentially change meta variables that were used for grouping. There is
#' always an attempt to keep the original groups, but they will be updated if it
#' is possible - a message is issued when that happens - and otherwise removed
#' altogether, with a warning.
#'
#' `matrix_elm` extracts a single matrix. It's a wrapper to `x[,,matrix]`, but
#' returns the matrix element. The replacement method `matrix_elm` is also a
#' wrapper to `x[,,matrix] <-`.
#'
#'
#' @param x    `matrixset` object from which to retrieve information, or object
#'      to test for being a `matrixset`.
#' @param matrix   index specifying matrix or matrices to extract. Index is
#'                 numeric or character vectors or empty (`NULL`). Note that
#'                 treating `NULL` as empty differs from the usual extraction,
#'                 where it is treated as `integer(0)`. Here a `NULL` (empty)
#'                 results in selecting all matrices.
#' @param value    valid value for replacement
#'
#' @returns
#' `ìs_matrixset` returns a `logical`.
#'
#' `dim` returns a length-2 vector; `nrow` and `ncol` return length-1 vector.
#'
#' `dimnames` returns a length-2 list; one component for each dimnames (row and
#' column). `rownames` and `colnames` each returns a `character` vector of
#' names.
#'
#' `matrixnames` a`character` vector of matrix names, or `NULL`.
#'
#' `nmatrix` returns an `ìnteger`.
#'
#' `row_traits` and `column_traits` returns a `character` vector.
#'
#' `row_tag` and `column_tag` returns a `character` vector.
#'
#' `row_info` extracts the row annotation data frame. `column_info` does
#' the same thing for column annotation.
#'
#' @examples
#' is_matrixset(student_results)
#' dim(student_results)
#' c(nrow(student_results), ncol(student_results))
#' dimnames(student_results)
#' list(rownames(student_results), colnames(student_results))
#' matrixnames(student_results)
#' nmatrix(student_results)
#' list(row_traits(student_results), column_traits(student_results))
#' row_info(student_results)
#' column_info(student_results)
#'
#' @name properties
NULL





#' @rdname properties
#' @export
dim.matrixset <- function(x) c(attr(x, "n_row"), attr(x, "n_col"))

#' @rdname properties
#' @export
dimnames.matrixset <- function(x) list(attr(x, "row_names"), attr(x, "col_names"))


#' @rdname properties
#' @export
`dimnames<-.matrixset` <- function (x, value)
{
  d <- dim(x)
  if (!is.list(value) || length(value) != 2L)
    stop("invalid 'dimnames' given for data frame")

  orig_val <- dimnames(x)

  lov1 <- length(orig_val[[1L]])
  lov2 <- length(orig_val[[2L]])

  value[[1L]] <- as.character(value[[1L]])
  value[[2L]] <- as.character(value[[2L]])

  lv1 <- length(value[[1L]])
  lv2 <- length(value[[2L]])

  if (lov1 && d[[1L]] != lv1)
    stop("incompatible row names length.")

  if (lov2 && d[[2L]] != lv2)
    stop("incompatible column names length.")

  if (lv1) .row_names_ms(x) <- value[[1L]]
  if (lv2) .col_names_ms(x) <- value[[2L]]
  x
}



# @export
.row_names_ms <- function(x) rownames(x)
# @export
.col_names_ms <- function(x) colnames(x)



# @export
`.row_names_ms<-` <- function(x, value)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  if (.is_row_grouped(x) && .rowtag(x) %in% row_group_vars(x))
    stop("can't modify row names as they are used for grouping")

  if (is.null(value)) stop("unsetting row values is not currently supported")

  if (!is.character(value)) value <- as.character(value)
  if (anyNA(value) || any(value == "")) stop("missing values in row names are not allowed.")

  x_matrix_set <- x$matrix_set
  x_matrix_set <- lapply(x_matrix_set, function(X) {
    if (!is.null(X)) rownames(X) <- value
    X
  })

  x$matrix_set <- x_matrix_set

  rt <- .rowtag(x)
  if (nrow(x$row_info)) {
    x$row_info[[rt]] <- value
  } else {
    ri <- tibble::tibble(foo = value)
    ri[[rt]]<- ri$foo
    ri$foo <- NULL
    x$row_info <- ri
  }

  attr(x, "row_names") <- value

  x
}

# @export
`.col_names_ms<-` <- function(x, value)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  if (.is_col_grouped(x) && .coltag(x) %in% column_group_vars(x))
    stop("can't modify column names as they are used for grouping")

  if (is.null(value)) stop("unsetting column values is not currently supported")

  if (!is.character(value)) value <- as.character(value)
  if (anyNA(value) || any(value == "")) stop("missing values in column names are not allowed.")

  x_matrix_set <- x$matrix_set
  x_matrix_set <- lapply(x_matrix_set, function(X) {
    if (!is.null(X)) colnames(X) <- value
    X
  })

  x$matrix_set <- x_matrix_set

  ct <- .coltag(x)
  if (nrow(x$column_info)) {
    x$column_info[[ct]] <- value
  } else {
    ci <- tibble::tibble(foo = value)
    ci[[ct]]<- ci$foo
    ci$foo <- NULL
    x$column_info <- ci
  }

  attr(x, "col_names") <- value

  x
}




#' @rdname properties
#' @export
matrixnames <- function(x) UseMethod("matrixnames")
#' @export
matrixnames.default <- function(x)
  stop(paste("'matrixnames' is not defined for objects of class", class(x)))
#' @export
matrixnames.matrixset <- function(x) .matrixnames(x)




#' @rdname properties
#' @export
`matrixnames<-` <- function(x, value) UseMethod("matrixnames<-")
#' @export
`matrixnames<-.default` <- function(x, value)
  stop(paste("'matrixnames<-' is not defined for objects of class", class(x)))
#' @export
`matrixnames<-.matrixset` <- function(x, value)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  if (is.null(value) || any(value == ""))
    stop("The values for new matrix names must not be empty")

  names(x$matrix_set) <- value
  attr(x, "matrix_names") <- value
  x
}



#' @rdname properties
#' @export
matrix_elm <- function(x, matrix) UseMethod("matrix_elm")
#' @export
matrix_elm.default <- function(x, matrix)
  stop(paste("'matrix_elm' is not defined for objects of class", class(x)))
#' @export
matrix_elm.matrixset <- function(x, matrix)
{
  if ((is.logical(matrix) && sum(matrix) != 1) || length(matrix) > 1)
    stop("a single matrix can be extracted")

  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  x[,,matrix,keep_annotation=FALSE, warn_class_change = FALSE][[1]]
}




#' @rdname properties
#' @export
`matrix_elm<-` <- function(x, matrix, value) UseMethod("matrix_elm<-")
#' @export
`matrix_elm<-.default` <- function(x, matrix, value)
  stop(paste("'matrix_elm' is not defined for objects of class", class(x)))
#' @export
`matrix_elm<-.matrixset` <- function(x, matrix, value)
{
  if ((is.logical(matrix) && sum(matrix) != 1) || length(matrix) > 1)
    stop("a single matrix can be extracted")

  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  x[,,matrix] <- value
  x
}





#' @rdname properties
#' @export
nmatrix <- function(x) UseMethod("nmatrix")
#' @export
nmatrix.default <- function(x)
  stop(paste("'nmatrix' is not defined for objects of class", class(x)))
#' @export
nmatrix.matrixset <- function(x) .nmatrix(x)



#' @rdname properties
#' @export
row_traits <- function(x) UseMethod("row_traits")
#' @export
row_traits.default <- function(x)
  stop(paste("'rowtraits' is not defined for objects of class", class(x)))
#' @export
row_traits.matrixset <- function(x) .rowtraits(x)


#' @rdname properties
#' @export
column_traits <- function(x) UseMethod("column_traits")
#' @export
column_traits.default <- function(x)
  stop(paste("'coltraits' is not defined for objects of class", class(x)))
#' @export
column_traits.matrixset <- function(x) .coltraits(x)



#' @rdname properties
#' @export
`row_traits<-` <- function(x, value) UseMethod("row_traits<-")
#' @export
`row_traits<-.default` <- function(x, value)
  stop(paste("'row_traits' is not defined for objects of class", class(x)))
#' @export
`row_traits<-.matrixset` <- function(x, value)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  if (is.null(value) || any(value == ""))
    stop("The values for new row traits must not be empty")

  rt <- attr(x, "row_traits")
  not_tag <- rt != .rowtag(x)

  if (!any(not_tag))
    stop("There aren't any row traits to modify (aside from the row names). To modify row names, use rownames()")

  rt[not_tag] <- value
  colnames(x$row_info) <- rt
  attr(x, "row_traits") <- rt
  x
}


#' @export
`row_traits<-.row_grouped_ms` <- function(x, value)
{
  vars_chr <- row_group_vars(x)

  x <- NextMethod()

  regroup <- all(vars_chr %in% colnames(value))
  if (!regroup) {
    x <- row_ungroup(x)
    warning("Row groups lost after row traits update", call. = FALSE)
  }

  x
}

#' @export
`row_traits<-.dual_grouped_ms` <- function(x, value)
{
  class(x) <- c("row_grouped_ms", "matrixset")
  row_traits(x) <- value
  class(x) <- c("dual_grouped_ms", "matrixset")
  x
}





#' @rdname properties
#' @export
column_traits <- function(x) UseMethod("column_traits")
#' @export
column_traits.default <- function(x)
  stop(paste("'column_traits' is not defined for objects of class", class(x)))
#' @export
column_traits.matrixset <- function(x) .coltraits(x)



#' @rdname properties
#' @export
`column_traits<-` <- function(x, value) UseMethod("column_traits<-")
#' @export
`column_traits<-.default` <- function(x, value)
  stop(paste("'column_traits' is not defined for objects of class", class(x)))
#' @export
`column_traits<-.matrixset` <- function(x, value)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  if (is.null(value) || any(value == ""))
    stop("The values for new column traits must not be empty")

  ct <- attr(x, "col_traits")
  not_tag <- ct != .coltag(x)

  if (!any(not_tag))
    stop("There aren't any column traits to modify (aside from the column names). To modify column names, use colnames()")


  ct[not_tag] <- value
  colnames(x$column_info) <- ct
  attr(x, "col_traits") <- ct
  x
}



#' @export
`column_traits<-.col_grouped_ms` <- function(x, value)
{
  vars_chr <- column_group_vars(x)

  x <- NextMethod()

  regroup <- all(vars_chr %in% colnames(value))
  if (!regroup) {
    x <- column_ungroup(x)
    warning("Column groups lost after column traits update", call. = FALSE)
  }

  x
}


#' @export
`column_traits<-.dual_grouped_ms` <- function(x, value)
{
  class(x) <- c("col_grouped_ms", "matrixset")
  column_traits(x) <- value
  class(x) <- c("dual_grouped_ms", "matrixset")
  x
}





#' @rdname properties
#' @export
row_tag <- function(x) UseMethod("row_tag")
#' @export
row_tag.default <- function(x)
  stop(paste("'row_tag' is not defined for objects of class", class(x)))
#' @export
row_tag.matrixset <- function(x) .rowtag(x)


#' @rdname properties
#' @export
column_tag <- function(x) UseMethod("column_tag")
#' @export
column_tag.default <- function(x)
  stop(paste("'column_tag' is not defined for objects of class", class(x)))
#' @export
column_tag.matrixset <- function(x) .coltag(x)





#' @rdname properties
#' @export
row_info <- function(x) UseMethod("row_info")
#' @export
row_info.default <- function(x)
  stop(paste("'row_info' is not defined for objects of class", class(x)))
#' @export
row_info.matrixset <- function(x)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))
  x$row_info
}




#' @rdname properties
#' @export
`row_info<-` <- function(x, value) UseMethod("row_info<-")
#' @export
`row_info<-.default` <- function(x, value)
  stop(paste("'row_info' is not defined for objects of class", class(x)))

#' @export
`row_info<-.matrixset` <- function(x, value)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  row_key <- .rowtag(x)

  n_row <- nrow(x)
  n_col <- ncol(x)
  row_names <- rownames(x)
  col_names <- colnames(x)
  row_tag <- .rowtag(x)
  matrix_info <- list(n_row = as.integer(n_row), n_col = as.integer(n_col),
                      row_names = row_names, col_names = col_names,
                      need_expand = FALSE)

  row_info <- set_meta("row", value, matrix_info, row_key, row_tag, FALSE)
  rwtr <- colnames(row_info)

  x$row_info <- tibble::as_tibble(row_info)
  attr(x, "row_traits") <- rwtr

  x
}



#' @export
`row_info<-.row_grouped_ms` <- function(x, value)
{
  vars_sym <- row_groups(x)
  vars_chr <- row_group_vars(x)

  x <- row_ungroup(x)
  x <- NextMethod()

  regroup <- all(vars_chr %in% colnames(value))
  if (regroup) {
    x <- row_group_by(x, !!!vars_sym)
    message("Row groups re-calculated after row annotation update")
  } else warning("Row groups lost after row annotation update", call. = FALSE)

  x
}

#' @export
`row_info<-.dual_grouped_ms` <- function(x, value)
{
  class(x) <- c("row_grouped_ms", "matrixset")
  row_info(x) <- value
  class(x) <- c("dual_grouped_ms", "matrixset")
  x
}



#' @rdname properties
#' @export
column_info <- function(x) UseMethod("column_info")
#' @export
column_info.default <- function(x)
  stop(paste("'column_info' is not defined for objects of class", class(x)))
#' @export
column_info.matrixset <- function(x)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))
  x$column_info
}






#' @rdname properties
#' @export
`column_info<-` <- function(x, value) UseMethod("column_info<-")
#' @export
`column_info<-.default` <- function(x, value)
  stop(paste("'column_info' is not defined for objects of class", class(x)))

#' @export
`column_info<-.matrixset` <- function(x, value)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  col_key <- .coltag(x)

  n_row <- nrow(x)
  n_col <- ncol(x)
  row_names <- rownames(x)
  col_names <- colnames(x)
  col_tag <- .coltag(x)
  matrix_info <- list(n_row = as.integer(n_row), n_col = as.integer(n_col),
                      row_names = row_names, col_names = col_names,
                      need_expand = FALSE)

  col_info <- set_meta("col", value, matrix_info, col_key, col_tag, FALSE)
  cltr <- colnames(col_info)

  x$column_info <- tibble::as_tibble( col_info)
  attr(x, "col_traits") <- cltr

  x
}


#' @export
`column_info<-.col_grouped_ms` <- function(x, value)
{
  vars_sym <- column_groups(x)
  vars_chr <- column_group_vars(x)

  x <- column_ungroup(x)
  x <- NextMethod()

  regroup <- all(vars_chr %in% colnames(value))
  if (regroup) {
    x <- column_group_by(x, !!!vars_sym)
    message("Column groups re-calculated after column annotation update")
  } else warning("Column groups lost after column annotation update", call. = FALSE)

  x
}

#' @export
`column_info<-.dual_grouped_ms` <- function(x, value)
{
  class(x) <- c("col_grouped_ms", "matrixset")
  column_info(x) <- value
  class(x) <- c("dual_grouped_ms", "matrixset")
  x
}







#' @rdname properties
#' @export
is_matrixset <- function(x)
{
  is_ms <- inherits(x, "matrixset")

  obj_attrs <- c("n_row", "n_col", "matrix_names", "n_matrix", "row_traits",
                 "col_traits", "row_names", "col_names", "row_tag", "col_tag")

  nms_attrs <- names(attributes(x))
  has_all_attrs <- all(obj_attrs %in% nms_attrs)

  is_ms && has_all_attrs
}





coll <- function(s)
{
  n <- length(s)
  if (n == 1L) return(s)
  s_sub <- s[1:(n-1)]
  out <- paste(s_sub, collapse = ", ")
  out <- paste(out, "and", s[n])
  out
}





print_matrix <- function(m, nrow_print = NULL, ncol_print = NULL,
                         class_print = NULL)
  UseMethod("print_matrix")

#' @export
print_matrix.NULL <- function(m, nrow_print = NULL, ncol_print = NULL,
                              class_print = NULL)
  print(NULL)


#' @export
print_matrix.matrix <- function(m, nrow_print = NULL, ncol_print = NULL,
                                class_print = NULL)
{
  nr <- nrow(m)
  nc <- ncol(m)
  if (is.null(nrow_print)) nrow_print <- nr
  if (is.null(ncol_print)) ncol_print <- nc
  row_shrinked <- FALSE
  col_shrinked <- FALSE

  mout <- m

  row_dimmed <- FALSE
  col_dimmed <- FALSE
  if (is.null(rownames(m)) && nr > 0) {
    rownames(mout) <- paste0("..", seq(nr))
    row_dimmed <- TRUE
  }
  if (is.null(colnames(m)) && nc > 0) {
    colnames(mout) <- paste0("..", seq(nc))
    col_dimmed <- TRUE
  }

  if (nr > 3 || nc > 4)
  {
    if (nr > 3) {
      ir <- c(1:2, nr)
      row_shrinked <- TRUE
    }

    if (nc > 4) {
      ic <- c(1:3, nc)
      col_shrinked <- TRUE
    }
  }

  mout <- format(round2(mout), trim = TRUE)

  if (row_shrinked) {
    if (col_shrinked) {
      mout <- mout[ir, ic]
      colnames(mout)[3] <- "..."
      mout[, 3] <- "..."
    } else mout <- mout[ir, , drop = FALSE]

    rownames(mout)[2] <- "..."
    mout[2, ] <- "..."

  } else if (col_shrinked) {
    mout <- mout[, ic, drop = FALSE]
    colnames(mout)[3] <- "..."
    mout[, 3] <- "..."
  }

  NR <- min(nr, 3)
  rn <- style_dim(rownames(mout), row_dimmed)
  cn <- colnames(mout)

  len <- if (nr) apply(nchar(mout), 2, max) else stats::setNames(rep(0L, ncol(mout)), cn)
  len <- pmax(len, stringr::str_width(cn))
  lr <- if (length(rn)) max(stringr::str_width(rn)) else 0L
  len <- c(lr, len)


  lout <- vector('list', NR+1)
  lout[[1]] <- paste(purrr::map2_chr(len,
                                     c(style_dim("", row_dimmed),
                                       style_dim(cn, col_dimmed)),
                                     ~ stringr::str_pad(.y, .x)),
                     collapse = " ")

  for (i in (seq_len(NR)+1))
  {
    l <-  purrr::map2_chr(len[-1], mout[i-1, ],
                          ~ stringr::str_pad(.y, .x))
    lout[[i]] <- paste(c(stringr::str_pad(rn[i-1], len[1]), l), collapse = " ")
  }


  if (is.null(class_print)) {
    class_print <- if (length(m)) vctrs::vec_ptype_abbr(m[[1]]) else vctrs::vec_ptype_abbr(m)
  }
  header <- paste("A", nrow_print, times, ncol_print, enclose(class_print),
                  "matrix")

  writeLines(pillar::style_subtle(header))
  cli::cat_line(lout)

}





#' @export
print_matrix.Matrix <- function(m, nrow_print = NULL, ncol_print = NULL,
                                class_print = NULL)
{
  M <- m

  nc <- ncol(m)
  nr <- nrow(m)

  if (is.null(rownames(M))) rownames(M) <- paste0("..", seq(nr))
  if (is.null(colnames(M))) colnames(M) <- paste0("..", seq(nc))

  if (nc > 4) M <- M[, c(1:4, nc)]
  if (nr > 3) M <- M[c(1:3, nr), ]
  M <- as.matrix(M)
  print_matrix(M, nr, nc, class(m))
}



#' Print a matrixset
#'
#' @description
#' When printing a `matrixset`:
#'
#' * The number of matrices and their dimension is shown
#' * Prints each matrix of the object, showing its type and dimension. Full
#'   matrices are shown only for those with 3 rows or less. Otherwise, only
#'   the first and last row is shown. The same also applies for the columns.
#' * An exception to the point above: if the number of matrices is greater than
#'   `n_matrices`, the first `n_matrices` are displayed, while the others will
#'   be named only.
#' * The row and column annotations (`row_info`/`column_info`) are displayed as
#'   `tibble` objects.
#'
#' @param x             `matrixset` object to print
#' @param ...           currently not used
#' @param n_matrices    Number of matrices to display
#'
#' @returns
#' Invisibly, the `matrixset` object.
#'
#' @examples
#' print(student_results)
#' print(mrm_plus2015)
#'
#' @export
print.matrixset <- function(x, ..., n_matrices = 2)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  out <- x$matrix_set
  nmats <- length(out)
  if (nmats > n_matrices) {
    out <- out[1:n_matrices]
    extra_matrices <- nmats - n_matrices
  }

  header <- paste("matrixset of", nmats, nrow(x), times, ncol(x), "matrices\n")
  cat(pillar::style_subtle(header))

  x_vars <- row_group_vars(x)
  if (!is.null(x_vars)) {
    cat(pillar::style_subtle(paste("Row groups: ", paste(x_vars, collapse = ", "))))
    cat("\n")
  }

  x_vars <- column_group_vars(x)
  if (!is.null(x_vars)) {
    cat(pillar::style_subtle(paste("Column groups: ", paste(x_vars, collapse = ", "))))
    cat("\n")
  }

  cat("\n")

  no <- length(out)
  if (no) {
    if (no > 1) {
      lapply(seq_len(no-1), function(i) {
        cat(paste("matrix_set:", names(out)[i], "\n"))
        print_matrix(out[[i]])
        cat("\n")
      })

      cat(paste("matrix_set:", names(out)[no], "\n"))
      print_matrix(out[[no]])
    } else {

      cat(paste("matrix_set:", names(out)[1], "\n"))
      print_matrix(out[[1]])

    }

    # lapply(1:no, function(i) {
    #   cat(paste("matrix_set:", names(out)[i], "\n"))
    #   print_matrix(out[[i]])
    #   cat("\n")
    # })
  } else {
    cat(paste("matrix_set:\n"))
    print(NULL)
    cat("\n")
  }


  if (nmats > n_matrices)
    cat(paste("And", extra_matrices, "other matrices named", coll(dQuote(attr(x, "matrix_names"))[(n_matrices+1):nmats])))

  cat("\n\nrow_info:\n")
  print(tibble::as_tibble(x$row_info))

  cat("\n\ncolumn_info:\n")
  print(tibble::as_tibble(x$column_info))

  invisible(x)
}

