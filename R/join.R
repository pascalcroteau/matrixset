# ONCE `BY` IS KNOWN, MUST CHECK THAT UNIQUELY DEFINES EACH ROW/COL

#' @importFrom stats setNames
set_by_null <- function (by, x_nms, y_nms, x_tag, y_tag)
{
  if (is.null(by)) {
    if (x_tag %in% x_nms && !is.null(y_tag) && y_tag %in% y_nms) {
      by <- setNames(y_tag, x_tag)
    } else {
      by <- intersect(x_nms, y_nms)
      if (length(by) == 0) {
        msg <- "`by` must be supplied when `x` and `y` have no common variables."
        stop(msg)
      }
    }
  }
  by
}


assess_by_vars_margin <- function(vars, nms)
{
  dup <- duplicated(vars)
  if (any(dup)) {
    msg <- "Join variables must be unique."
    stop(msg)
  }

  if (!all(is_in <- vars %in% nms)) {
    msg <- if (sum(!is_in) > 1) "variables" else "variable"
    vars_quoted <- encodeString(vars[!is_in], quote = "\"")
    msg <- paste(msg, paste(vars_quoted, collapse = ", "), "is not a known trait")
    stop(msg)
  }
  NULL
}


set_by_vars <- function(by, x_nms, y_nms)
{
  if (is.character(by) || is.list(by)) {
    by_nms <- names(by)
    by <- unname(by)

    if (is.list(by) && !all(sapply(by, is.character)))
      stop("`by` must be a list of character when a list")

    if (is.list(by) && !all(sapply(by, function(x) length(x) == 1L)))
      stop("elements of `by` must be of length 1 when it is a list")

    by <- unlist(by)

    by_x <- rlang::`%||%`(by_nms, by)
    by_y <- by

  } else {
    msg <- "`by` must be a (named) character vector, list, or NULL"
    stop(msg)
  }

  assess_by_vars_margin(by_x, x_nms)
  assess_by_vars_margin(by_y, y_nms)

  setNames(by_y, by_x)

}



join_names <- function(obj, mrg = NULL) UseMethod("join_names")
join_names.matrixset <- function(obj, mrg = NULL)
{
  if (mrg == "row") {
    c(.rowtag(obj), .rowtraits(obj))
  } else {
    c(.coltag(obj), .coltraits(obj))
  }
}
join_names.data.frame <- function(obj, mrg = NULL) colnames(obj)


join_tag <- function(obj, mrg = NULL) UseMethod("join_tag")
join_tag.matrixset <- function(obj, mrg = NULL)
{
  if (mrg == "row") {
    .rowtag(obj)
  } else {
    .coltag(obj)
  }
}
join_tag.data.frame <- function(obj, mrg = NULL) NULL


join_info <- function(obj, mrg = NULL) UseMethod("join_info")
join_info.matrixset <- function(obj, mrg = NULL)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))


  if (mrg == "row") {
    obj$row_info
  } else {
    obj$column_info
  }
}
join_info.data.frame <- function(obj, mrg = NULL) obj



margin_names <- function(obj, mrg)
{
  if (mrg == "row") {
    list(nms = rownames(obj), compl = colnames(obj))
  } else {
    list(nms = colnames(obj), compl = rownames(obj))
  }
}



fill_matrix <- function(m, margin, nr, nc, old_names, all_names, compl_names)
{
  if (is.null(m)) return(NULL)
  d <- dim(m)
  if (d[1]*d[2] == 0L) {
    na_val <- rlang::eval_tidy(rlang::call2(storage.mode(m), 1))
    na_val[] <- NA
  }

  na_val <- m[1,1]
  na_val[] <- NA

  new_names <- setdiff(all_names, old_names)
  pos <- match(old_names, all_names)

  if (margin == "row") {

    newm <- matrix(na_val, length(all_names), nc)
    rownames(newm) <- all_names
    colnames(newm) <- compl_names
    newm[pos, ] <- m

  } else {

    newm <- matrix(na_val, nr, length(all_names))
    rownames(newm) <- compl_names
    colnames(newm) <- all_names
    newm[, pos] <- m

  }

  newm

}



sub_matrix <- function(m, margin, old_names, all_names, compl_names)
{
  if (is.null(m)) return(NULL)
  d <- dim(m)
  if ((d[1] == 0L && margin == "row") || (d[2] == 0L && margin == "col")) return(m)

  pos <- match(all_names, old_names, 0)
  if (margin == "row") m[pos, , drop = FALSE] else m[, pos, drop = FALSE]

}






.join_info <- function(type, margin, .ms_x, .ms_y, by = NULL,
                       suffix = c(".x", ".y"), na_matches = "never",
                       adjust = FALSE)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  x_nms <- join_names(.ms_x, margin)
  y_nms <- join_names(.ms_y, margin)
  x_tag <- join_tag(.ms_x, margin)
  y_tag <- join_tag(.ms_y, margin)

  by <- set_by_null(by, x_nms, y_nms, x_tag, y_tag)
  by <- set_by_vars(by, x_nms, y_nms)

  info_x <- join_info(.ms_x, margin)
  info_y <- join_info(.ms_y, margin)


  args <- list("info_x", "info_y", by = as.name("by"),
               suffix = as.name("suffix"), na_matches = as.name('na_matches'))
  join_call <- rlang::call2(paste0(type, "_join"), !!!rlang::syms(args),
                            .ns = "dplyr")
  info <- rlang::eval_tidy(join_call)

  # make sure the new key (row/col name has no duplicates)
  ntag <- dplyr::count(info, !!as.name(x_tag))
  ntag <- if(x_tag == "n") {
    ntag[["nn"]]
  } else {
    ntag[["n"]]
  }
  ntag <- unique(ntag)

  if (length(ntag) > 1 || ntag > 1)
    stop(paste("'by' does not result in unique", margin, "names"))

  tr <- colnames(info)

  n <- rlang::eval_tidy(rlang::call2(as.name(paste0("n", margin)), as.name(".ms_x")))
  if ((ni <- nrow(info)) != n) {
    if (adjust) {

      nms <- margin_names(.ms_x, margin)
      old_names <- nms$nms
      compl_names <- nms$compl
      all_names <- info[[x_tag]]

      nr <- if (margin == "row") ni else nrow(.ms_x)
      nc <- if (margin == "row") ncol(.ms_x) else ni
      mats <- .ms_x$matrix_set

      if (ni > n) {
        mats <- lapply(mats,
                       function(m) fill_matrix(m, margin, nr, nc, old_names,
                                               all_names, compl_names))
      } else {
        mats <- lapply(mats,
                       function(m) sub_matrix(m, margin, old_names, all_names,
                                              compl_names))
      }

      .ms_x$matrix_set <- mats
      if (margin == "row") {
        attr(.ms_x, "n_row") <- ni
        attr(.ms_x, "row_names") <- all_names
      } else {
        attr(.ms_x, "n_col") <- ni
        attr(.ms_x, "col_names") <- all_names
      }

    } else {
      tag <- if (margin == "col") "columns" else "rows"
      stop(paste("the number of", tag, "is modified by the join operation, which is against the 'matrixset' paradigm. Use 'adjust' to still perform the operation."))
    }
  }



  if (margin == "row") {
    .ms_x$row_info <- info
    attr(.ms_x, "row_traits") <- tr
  } else {
    .ms_x$column_info <- info
    attr(.ms_x, "col_traits") <- tr
  }


  meta <- get_group_info(info, class(.ms_x), margin)
  attrs <- set_group_attrs(attributes(.ms_x), meta$attrs, margin)
  attributes(.ms_x) <- attrs
  class(.ms_x) <- meta$class
  if (is.null(meta$attrs$group_vars)) class(.ms_x) <- "matrixset"
  # if (!is.null(meta$attrs$group_vars)) {
  #
  # } else class(.ms_x) <- "matrixset"


  .ms_x

}


#' Add meta info from another `matrixset` or a `data.frame`
#'
#' @description
#' The operation is done through a join operation between the row meta info
#' data.frame ([join_row_info()]) of `.ms` and `y` (or its row meta info
#' data.frame if it is a `matrixset` object). The function [join_column_info()]
#' does the equivalent operation for column meta info.
#'
#' The default join operation is a left join (type == `"left"`), but all dplyr's
#' mutating joins are available (`"left"`, `"inner"`, `"right"` and `"full"`).
#'
#' The `matrixset` paradigm of unique row/column names is enforced so if a
#' `.ms` data.frame row matches multiple ones in `y`, this results in an
#' error.
#'
#' @param .ms           A `matrixset` object
#' @param y             A `matrixset` object or a `data.frame`.
#' @param type          Joining type, one of `"left"` (default), `"inner"`,
#'                      `"right"` or `"full"`
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
#'                      except for the `NULL`matrices).
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
#' ms <- ms <- tryCatch(join_row_info(ms2, student_results, type = "full"),
#'                      error = function(e) e)
#' is(ms, "error") # TRUE
#' ms$message
#'
#' # Now it works.
#' ms <- join_row_info(ms2, student_results, type = "full", adjust = TRUE)
#' dim(ms2)
#' dim(ms)
#' matrix_elm(ms, 1)
#'
#' @name join



#' @rdname join
#' @export
join_row_info <- function(.ms, y, type = "left", by = NULL, adjust = FALSE,
                          suffix = c(".x", ".y"), na_matches = c("na", "never"))
  UseMethod("join_row_info")

#' @export
join_row_info.matrixset <- function(.ms, y, type = "left", by = NULL,
                                    adjust = FALSE, suffix = c(".x", ".y"),
                                    na_matches = c("na", "never"))
{
  na_matches <- match.arg(na_matches)
  .join_info(type, "row", .ms, y, by = by, suffix = suffix,
             na_matches = na_matches, adjust = adjust)
}



#' @rdname join
#' @export
join_column_info <- function(.ms, y, type = "left", by = NULL, adjust = FALSE,
                             suffix = c(".x", ".y"), na_matches = c("na", "never"))
  UseMethod("join_column_info")

#' @export
join_column_info.matrixset <- function(.ms, y, type = "left", by = NULL,
                                       adjust = FALSE, suffix = c(".x", ".y"),
                                       na_matches = c("na", "never"))
{
  na_matches <- match.arg(na_matches)
  .join_info(type, "column", .ms, y, by = by, suffix = suffix,
             na_matches = na_matches, adjust = adjust)
}





