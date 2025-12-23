



build_glue <- function(ng)
{
  if (is.null(ng) || (is.logical(ng) && isFALSE(ng))) return(NULL)
  if (is.logical(ng)) ng <- "{.tag}"
  has_tag <- stringr::str_detect(ng, "\\{\\.tag\\}")
  if (!has_tag) ng <- paste("{.tag}", ng, sep = "_")
  ng
}



unique_names <- function(tplt, tag, dat)
{
  if (tplt == "{.tag}") return(make_unique(dat[[tag]]))
  id_unq <- unique_id(dat[[tag]], nrow(dat))
  ldat <- dat[id_unq > 0, ]
  # dat[[".tag"]] <- dat[[tag]]
  ldat <- list2env(ldat, parent = emptyenv())
  ldat[[".tag"]] <- ldat[[tag]]
  new_tag <- stringr::str_glue_data(tplt, .x=ldat)
  if (vctrs::vec_unique_count(new_tag) != length(new_tag))
    stop(paste("'by' does not result in unique", tag))

  new_dat_tag <- dat[[tag]]
  new_dat_tag[id_unq > 0] <- new_tag
  new_dat_tag
}





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

    # by_x <- rlang::`%||%`(by_nms, by)
    by_x <- by_nms %OR% by
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

#' @export
join_names.matrixset <- function(obj, mrg = NULL)
{
  if (mrg == "row") {
    c(.rowtag(obj), .rowtraits(obj))
  } else {
    c(.coltag(obj), .coltraits(obj))
  }
}
#' @export
join_names.data.frame <- function(obj, mrg = NULL) colnames(obj)


join_tag <- function(obj, mrg = NULL) UseMethod("join_tag")

#' @export
join_tag.matrixset <- function(obj, mrg = NULL)
{
  if (mrg == "row") {
    .rowtag(obj)
  } else {
    .coltag(obj)
  }
}
#' @export
join_tag.data.frame <- function(obj, mrg = NULL) NULL


join_info <- function(obj, mrg = NULL) UseMethod("join_info")
#' @export
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
#' @export
join_info.data.frame <- function(obj, mrg = NULL) obj



margin_names <- function(obj, mrg)
{
  if (mrg == "row") {
    list(nms = rownames(obj), compl = colnames(obj))
  } else {
    list(nms = colnames(obj), compl = rownames(obj))
  }
}



set_adjust <- function(adj, y_ms)
{
  adj_type <- typeof(adj)
  adjust <- switch (adj_type,
                    "logical" = adj,
                    "character" = TRUE,
                    stop("adjust parameter must be a logical or a character vector of length 1.")
                    )

  if (length(adj) > 1) {
    warning("'adjust' is of length > 1. Keeping the first element only.")
    adjust <- adjust[1]
  }

  adjust_how <- if (is.character(adj)) {
    match_option(adj, adjust_opts)
  } else adjust_opts["x_only"]

  if (adjust && adjust_how != adjust_opts["x_only"] && !y_ms) {
    adjust_how <- adjust_opts["x_only"]
    warning(stringr::str_glue("'adjust' has been forced to '{ADJ}' because 'y' is a data.frame",
                              ADJ = adjust_opts["x_only"]))
  }

  list(adjust = adjust, adjust_how = adjust_how)
}



# fill_matrix <- function(m, margin, nr, nc, old_names, all_names, compl_names)
# {
#   if (is.null(m)) return(NULL)
#
#   is_Matrix <- is(m, "Matrix")
#
#   d <- dim(m)
#   if (d[1]*d[2] == 0L) {
#     na_val <- rlang::eval_tidy(rlang::call2(storage.mode(m), 1))
#     na_val[] <- NA
#   } else {
#     na_val <- m[1,1]
#     na_val[] <- NA
#   }
#
#   new_names <- setdiff(all_names, old_names)
#   pos <- match(old_names, all_names)
#
#   if (margin == "row") {
#
#     newm <- MATRIX(na_val, nrow = length(all_names), ncol = nc, is_Matrix)
#     rownames(newm) <- all_names
#     colnames(newm) <- compl_names
#     if (is_Matrix) {
#       newm[] <- na_val
#       newm <- methods::as(newm, class(m))
#     }
#     newm[pos, ] <- m
#
#
#   } else {
#
#     newm <- MATRIX(na_val, nrow = nr, ncol = length(all_names), is_Matrix)
#     rownames(newm) <- compl_names
#     colnames(newm) <- all_names
#     if (is_Matrix) {
#       newm[] <- na_val
#       newm <- methods::as(newm, class(m))
#     }
#     newm[, pos] <- m
#
#
#   }
#
#   newm
#
# }
fill_matrix <- function(m, margin, nr, nc, old_names, joined_names, compl_names,
                        joined_names_unique)
{
  if (is.null(m)) return(NULL)

  is_Matrix <- is(m, "Matrix")

  d <- dim(m)
  if (d[1]*d[2] == 0L) {
    na_val <- rlang::eval_tidy(rlang::call2(storage.mode(m), 1))
    na_val[] <- NA
  } else {
    na_val <- m[1,1]
    na_val[] <- NA
  }

  new_names <- setdiff(joined_names, old_names)
  pos <- if (is.null(joined_names_unique)) {
    match(old_names, joined_names)
  } else {
    common <- intersect(joined_names, old_names)
    unlist(lapply(common, function(.x) which(.x == joined_names)))
  }

  class_m <- class(m)
  if (margin == "row") {

    newm <- MATRIX(na_val, nrow = length(joined_names), ncol = nc, is_Matrix)
    rownames(newm) <- joined_names
    colnames(newm) <- compl_names
    if (is_Matrix) {
      newm[] <- na_val
      newm <- if (class_m == "dgeMatrix"){
        methods::as(methods::as(newm, "generalMatrix"), "unpackedMatrix")
      } else methods::as(newm, class_m)
    }
    newm[pos, ] <- if (is.null(joined_names_unique)) {
      m
    } else {
      m[unlist(lapply(joined_names, function(.x) which(.x == old_names))), ]
    }
    if (!is.null(joined_names_unique)) rownames(newm) <- joined_names_unique


  } else {

    newm <- MATRIX(na_val, nrow = nr, ncol = length(joined_names), is_Matrix)
    rownames(newm) <- compl_names
    colnames(newm) <- joined_names
    if (is_Matrix) {
      newm[] <- na_val
      newm <- if (class_m == "dgeMatrix"){
        methods::as(methods::as(newm, "generalMatrix"), "unpackedMatrix")
      } else methods::as(newm, class_m)
    }
    newm[, pos] <- if (is.null(joined_names_unique)) {
      m
    } else {
      m[, unlist(lapply(joined_names, function(.x) which(.x == old_names)))]
    }
    if (!is.null(joined_names_unique)) colnames(newm) <- joined_names_unique


  }

  newm

}



fill_from_y <- function(m, Y, margin, new_names, compl_names, all_names)
{
  if (is.null(m)) return(NULL)
  if (is.null(Y)) return(m)

  pos <- match(new_names, all_names)

  if (margin == "row") {

    x_col <- match(colnames(Y), compl_names, 0)
    y_col <- match(compl_names, colnames(Y), 0)
    if (any(x_col > 0) && any(y_col > 0)) {
      y <- Y[new_names, y_col, drop = FALSE]
      m[pos, x_col] <- y
    }


  } else {

    x_row <- match(rownames(Y), compl_names, 0)
    y_row <- match(compl_names, rownames(Y), 0)
    if (any(x_row > 0) && any(y_row > 0)) {
      y <- Y[y_row, new_names, drop = FALSE]
      m[x_row, pos] <- y
    }

  }

  m

}




sub_matrix <- function(m, margin, old_names, all_names, compl_names)
{
  if (is.null(m)) return(NULL)
  d <- dim(m)
  if ((d[1] == 0L && margin == "row") || (d[2] == 0L && margin == "col")) return(m)

  pos <- match(all_names, old_names, 0)
  if (margin == "row") m[pos, , drop = FALSE] else m[, pos, drop = FALSE]

}






# .join_info <- function(type, margin, .ms_x, .ms_y, by = NULL,
#                        suffix = c(".x", ".y"), na_matches = "never",
#                        adjust = FALSE)
# {
#   cl <- sys.call()
#   cash_status$set(cl)
#   on.exit(cash_status$clear(cl))
#
#   adjust_meta <- set_adjust(adjust, is_matrixset(.ms_y))
#   adjust <- adjust_meta$adjust
#   adjust_how <- adjust_meta$adjust_how
#   adjust_from_y <- adjust_how != adjust_opts["x_only"]
#
#   x_nms <- join_names(.ms_x, margin)
#   y_nms <- join_names(.ms_y, margin)
#   x_tag <- join_tag(.ms_x, margin)
#   y_tag <- join_tag(.ms_y, margin)
#
#   by <- set_by_null(by, x_nms, y_nms, x_tag, y_tag)
#   by <- set_by_vars(by, x_nms, y_nms)
#
#   info_x <- join_info(.ms_x, margin)
#   info_y <- join_info(.ms_y, margin)
#
#
#   # args <- list("info_x", "info_y", by = as.name("by"),
#   #              suffix = as.name("suffix"), na_matches = as.name('na_matches'))
#   args <- list("info_x", "info_y", by = as.name("by"),
#                na_matches = as.name('na_matches'))
#   if (!(type %in% filt_join_opts)) args <- c(args, suffix = as.name("suffix"))
#   join_call <- rlang::call2(paste0(type, "_join"), !!!rlang::syms(args),
#                             .ns = "dplyr")
#   info <- rlang::eval_tidy(join_call)
#
#   ni <- nrow(info)
#
#   if (ni > 0) {
#     # make sure the new key (row/col name has no duplicates)
#     ntag <- dplyr::count(info, !!as.name(x_tag))
#     ntag <- if(x_tag == "n") {
#       ntag[["nn"]]
#     } else {
#       ntag[["n"]]
#     }
#     ntag <- unique(ntag)
#
#     if (length(ntag) > 1 || ntag > 1)
#       stop(paste("'by' does not result in unique", margin, "names"))
#   }
#
#   tr <- colnames(info)
#
#   n <- rlang::eval_tidy(rlang::call2(as.name(paste0("n", margin)), as.name(".ms_x")))
#   if ((ni <- nrow(info)) != n) {
#     if (adjust) {
#
#       nms <- margin_names(.ms_x, margin)
#       old_names <- nms$nms
#       compl_names <- nms$compl
#       all_names <- info[[x_tag]]
#       if (adjust_from_y) new_names <- setdiff(all_names, old_names)
#
#       nr <- if (margin == "row") ni else nrow(.ms_x)
#       nc <- if (margin == "row") ncol(.ms_x) else ni
#       mats <- .ms_x$matrix_set
#
#       if (ni > n) {
#         # mats <- lapply(mats,
#         #                function(m) fill_matrix(m, margin, nr, nc, old_names,
#         #                                        all_names, compl_names))
#         if (adjust_from_y) Y <- .ms_y$matrix_set
#         mats_nms <- names(mats)
#         mats <- lapply(mats_nms,
#                        function(m_nm) {
#                          m <- mats[[m_nm]]
#                          newm <- fill_matrix(m, margin, nr, nc, old_names,
#                                              all_names, compl_names)
#                          if (adjust_from_y) {
#                            if (m_nm %in% names(Y)) {
#                              y <- Y[[m_nm]]
#                              newm <- fill_from_y(newm, y, margin, new_names,
#                                                  compl_names, all_names)
#                            }
#                          }
#                          newm
#                        })
#         names(mats) <- mats_nms
#       } else {
#         mats <- lapply(mats,
#                        function(m) sub_matrix(m, margin, old_names, all_names,
#                                               compl_names))
#       }
#
#       .ms_x$matrix_set <- mats
#       if (margin == "row") {
#         attr(.ms_x, "n_row") <- ni
#         attr(.ms_x, "row_names") <- all_names
#       } else {
#         attr(.ms_x, "n_col") <- ni
#         attr(.ms_x, "col_names") <- all_names
#       }
#
#     } else {
#       tag <- if (margin == "col") "columns" else "rows"
#       stop(paste("the number of", tag, "is modified by the join operation, which is against the 'matrixset' paradigm. Use 'adjust' to still perform the operation."))
#     }
#   }
#
#
#
#   if (margin == "row") {
#     .ms_x$row_info <- info
#     attr(.ms_x, "row_traits") <- tr
#   } else {
#     .ms_x$column_info <- info
#     attr(.ms_x, "col_traits") <- tr
#   }
#
#
#   meta <- get_group_info(info, class(.ms_x), margin)
#   attrs <- set_group_attrs(attributes(.ms_x), meta$attrs, margin)
#   attributes(.ms_x) <- attrs
#   class(.ms_x) <- meta$class
#   if (is.null(meta$attrs$group_vars)) class(.ms_x) <- "matrixset"
#   # if (!is.null(meta$attrs$group_vars)) {
#   #
#   # } else class(.ms_x) <- "matrixset"
#
#
#   .ms_x
#
# }
.join_info <- function(type, margin, .ms_x, .ms_y, by = NULL,
                       suffix = c(".x", ".y"), na_matches = "never",
                       adjust = FALSE, names_glue = NULL)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  adjust_meta <- set_adjust(adjust, is_matrixset(.ms_y))
  adjust <- adjust_meta$adjust
  adjust_how <- adjust_meta$adjust_how
  adjust_from_y <- adjust_how != adjust_opts["x_only"]

  names_glue <- build_glue(names_glue)
  accept_dupl <- !is.null(names_glue)

  x_nms <- join_names(.ms_x, margin)
  y_nms <- join_names(.ms_y, margin)
  x_tag <- join_tag(.ms_x, margin)
  y_tag <- join_tag(.ms_y, margin)

  by <- set_by_null(by, x_nms, y_nms, x_tag, y_tag)
  by <- set_by_vars(by, x_nms, y_nms)

  info_x <- join_info(.ms_x, margin)
  info_y <- join_info(.ms_y, margin)

  meta_orig <- get_group_info(info_x, class(.ms_x), margin)
  var_class_orig <- lapply(info_x, class)


  args <- list("info_x", "info_y", by = as.name("by"),
               na_matches = as.name('na_matches'))
  if (!(type %in% filt_join_opts)) args <- c(args, suffix = as.name("suffix"))
  join_call <- rlang::call2(paste0(type, "_join"), !!!rlang::syms(args),
                            .ns = "dplyr")
  info <- rlang::eval_tidy(join_call)

  var_class <- lapply(info, class)
  var_class_orig_kept <- var_class_orig[names(var_class_orig) %in% names(var_class)]
  var_class_still <- var_class[names(var_class) %in% names(var_class_orig)]
  if (length(var_class_orig_kept) && length(var_class_still) &&
      !identical(var_class_orig_kept, var_class_still)) {
    idx <- purrr::map2_lgl(var_class_orig_kept, var_class_still,
                           function(x, y) !identical(x, y))
    chg_vars <- names(idx[idx])
    warning(paste0("some traits have changed type (",
                   stringr::str_flatten(sQuote(chg_vars), collapse = ", "),
                   ")"),
            call. = FALSE)
  }

  ni <- nrow(info)

  not_unique <- FALSE
  if (ni > 0) {
    # make sure the new key (row/col name has no duplicates)
    ntag <- dplyr::count(info, !!as.name(x_tag))
    ntag <- if(x_tag == "n") {
      ntag[["nn"]]
    } else {
      ntag[["n"]]
    }
    ntag <- unique(ntag)

    if (length(ntag) > 1 || ntag > 1) {
      if (!accept_dupl) stop(paste("'by' does not result in unique", margin, "names"))
      not_unique <- TRUE
    }
  }


  joined_names <- info[[x_tag]]
  nms <- margin_names(.ms_x, margin)
  x_names <- nms$nms
  compl_names <- nms$compl


  joined_names_unique <- if (not_unique) {
    warning(paste0(margin, " names (", x_tag, ") have changed following matrix adjustment"), call. = FALSE)
    unique_names(names_glue, x_tag, info)
  } else  NULL


  new_names <- setdiff(joined_names, x_names)
  new_names_unique <- setdiff(joined_names_unique, x_names)
  lost_names <- setdiff(x_names, joined_names)

  has_new_names <- length(new_names) > 0
  has_new_names_unique <- length(new_names_unique) > 0
  has_lost_names <- length(lost_names) > 0

  if (has_new_names || has_new_names_unique || has_lost_names)
  {
    if (adjust) {
      nr <- if (margin == "row") ni else nrow(.ms_x)
      nc <- if (margin == "row") ncol(.ms_x) else ni
      mats <- .ms_x$matrix_set

      if (has_new_names || has_new_names_unique) {

        if (not_unique) {
          info[[x_tag]] <- joined_names_unique
        }

        if (adjust_from_y) Y <- .ms_y$matrix_set
        mats_nms <- names(mats)
        mats <- lapply(mats_nms,
                       function(m_nm) {
                         m <- mats[[m_nm]]
                         newm <- fill_matrix(m, margin, nr, nc, x_names,
                                             joined_names, compl_names,
                                             joined_names_unique)
                         if (adjust_from_y) {
                           if (m_nm %in% names(Y)) {
                             y <- Y[[m_nm]]
                             newm <- fill_from_y(newm, y, margin, new_names,
                                                 compl_names, joined_names)
                           }
                         }
                         newm
                       })
        names(mats) <- mats_nms

      }


      if (has_lost_names) {
        mats <- lapply(mats,
                       function(m) sub_matrix(m, margin, x_names, joined_names,
                                              compl_names))
      }


      .ms_x$matrix_set <- mats
      if (margin == "row") {
        attr(.ms_x, "n_row") <- ni
        attr(.ms_x, "row_names") <- if (is.null(joined_names_unique)) {
          joined_names
        } else joined_names_unique
      } else {
        attr(.ms_x, "n_col") <- ni
        attr(.ms_x, "col_names") <- if (is.null(joined_names_unique)) {
          joined_names
        } else joined_names_unique
      }


    } else {
      tag <- if (margin == "col") "columns" else "rows"
      stop(paste("the number of", tag, "is modified by the join operation, which is against the 'matrixset' paradigm. Use 'adjust' to still perform the operation."))
    }

  }



  tr <- colnames(info)

  if (any(notin <- !(x_nms %in% tr))) {
    notin <- x_nms[notin]
    chg_to <- paste0(notin, suffix[1])
    if (any(gone <- !(chg_to %in% tr))) {
      gone_away <- chg_to[gone]
      chg_to <- chg_to[!gone]
    }

    chg_to_msg <- paste(paste("", paste(shQuote(notin[!gone]), shQuote(chg_to),
                                        sep = " -> ")), collapse = "\n")

    warning(paste0("some traits have changed name:\n", chg_to_msg),
            call. = FALSE)
    if (any(gone)) {
      warning(paste0("some traits have disappeared: ",
                     stringr::str_flatten_comma(sQuote(gone_away)),
                     call. = FALSE))
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
  if (!identical(meta_orig$attrs$group_keys, meta$attrs$group_keys))
    warning("grouping structure of '.ms_x' has changed.", call. = FALSE)
  attrs <- set_group_attrs(attributes(.ms_x), meta$attrs, margin)
  attributes(.ms_x) <- attrs
  class(.ms_x) <- meta$class
  if (is.null(meta$attrs$group_vars)) class(.ms_x) <- "matrixset"
  # if (!is.null(meta$attrs$group_vars)) {
  #
  # } else class(.ms_x) <- "matrixset"


  .ms_x

}


# Add meta info from another `matrixset` or a `data.frame`
#
# @description
# The operation is done through a join operation between the row meta info
# data.frame ([join_row_info()]) of `.ms` and `y` (or its row meta info
# data.frame if it is a `matrixset` object). The function [join_column_info()]
# does the equivalent operation for column meta info.
#
# The default join operation is a `r join_opts["default"]` join
# (type == `r sQuote(join_opts["default"])`), but most of dplyr's
# joins are available (`r flatten_or(join_opts)`).
#
# The `matrixset` paradigm of unique row/column names is enforced so if a
# `.ms` data.frame row matches multiple ones in `y`, this results in an
# error.
#
# @param .ms           A `matrixset` object
# @param y             A `matrixset` object or a `data.frame`.
# @param type          Joining type, one of `r sQuote(join_opts["default"])`,
#                      `r flatten_or(join_opts[join_opts != join_opts["default"]])`.
# @param by            The names of the variable to join by.
#                      The default, `NULL`, results in slightly different
#                      behavior depending if `y` is a `matrixset` or a
#                      `data.frame`.
#                      If a `matrixset`, the meta info tag of each object (the
#                      tag is the column that holds the row names/column names
#                      in the meta info data frame - typically ".rowname" or
#                      ".colname" unless specified otherwise at `matrixset`
#                      creation) is used for `by`.
#                      If a `data.frame`, a natural join is used. For more
#                      details, see `dplyr`'s [dplyr::join()].
#                      Note that the cross-join is not available.
# @param adjust        A logical. By default (`FALSE`), the join operation is
#                      not permitted to filter or augment the number of rows of
#                      the meta info data frame.
#                      If `TRUE`, this will be allowed. In the case where the
#                      data frame is augmented, the matrices of `.ms`
#                      will be augmented accordingly by padding with `NA`s (
#                      except for the `NULL` matrices).
#
#    Alternatively, `adjust` can be a single string, one of
#    `r flatten_or(adjust_opts)`. Choosing "`r adjust_opts["x_only"]`"
#    is equivalent to `TRUE`. When choosing "`r adjust_opts["from_y"]`",
#    padding is done using values from `y`, but only
#
#    1. if `y` is a `matrixset`
#    2. for `y` matrices that are named the same in `x`
#    3. If padding rows, only columns common between `x` and `y` will use `y`
#      values. The same logic is applied when padding columns.
#
#    Other values are padded with `NA`.
# @param suffix        Suffixes added to disambiguate trait variables. See
#                      `dplyr`'s [dplyr::join()].
# @param na_matches    How to handle missing values when matching. See
#                      `dplyr`'s [dplyr::join()].
#
# @section Groups:
# When `y` is a `matrixset`, only groups from `.ms` are used, if any. Group
# update is the same as in `dplyr`.
#
# @returns
# A `matrixset` with updated row or column meta info, with all `.ms` traits and
# `y` traits. If some traits share the same names - and were not included in
# `by` - `suffix`es will be appended to these names.
#
# If adjustment was allowed, the dimensions of the new `matrixset` may differ
# from the original one.
#
# @examples
# ms1 <- remove_row_annotation(student_results, class, teacher)
# ms <- join_row_info(ms1, student_results)
#
# ms <- join_row_info(ms1, student_results, by = c(".rowname", "previous_year_score"))
#
# # This will throw an error
# ms2 <- remove_row_annotation(filter_row(student_results, class %in% c("classA", "classC")),
#                              class, teacher, previous_year_score)
# ms <- ms <- tryCatch(join_row_info(ms2, student_results, type = "full"),
#                      error = function(e) e)
# is(ms, "error") # TRUE
# ms$message
#
# # Now it works.
# ms <- join_row_info(ms2, student_results, type = "full", adjust = TRUE)
# dim(ms2)
# dim(ms)
# matrix_elm(ms, 1)
#
# @name join

#' Add meta info from another `matrixset` or a `data.frame`
#'
#' @description
#' The operation is done through a join operation between the row meta info
#' data.frame ([join_row_info()]) of `.ms` and `y` (or its row meta info
#' data.frame if it is a `matrixset` object). The function [join_column_info()]
#' does the equivalent operation for column meta info.
#'
#' The default join operation is a `r join_opts["default"]` join
#' (type == `r sQuote(join_opts["default"])`), but most of dplyr's
#' joins are available (`r flatten_or(join_opts)`).
#'
#' The `matrixset` paradigm of unique row/column names is enforced so if a
#' `.ms` data.frame row matches multiple ones in `y`, the default behavior is
#' to issue a condition error.
#'
#' This can be modified by setting new tag names via the argument `names_glue`.
#'
#' @param .ms           A `matrixset` object
#' @param y             A `matrixset` object or a `data.frame`.
#' @param type          Joining type, one of `r sQuote(join_opts["default"])`,
#'                      `r flatten_or(join_opts[join_opts != join_opts["default"]])`.
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
#'                      except for the `NULL` matrices).
#'
#'    Alternatively, `adjust` can be a single string, one of
#'    `r flatten_or(adjust_opts)`. Choosing "`r adjust_opts["x_only"]`"
#'    is equivalent to `TRUE`. When choosing "`r adjust_opts["from_y"]`",
#'    padding is done using values from `y`, but only
#'
#'    1. if `y` is a `matrixset`
#'    2. for `y` matrices that are named the same in `x`
#'    3. If padding rows, only columns common between `x` and `y` will use `y`
#'      values. The same logic is applied when padding columns.
#'
#'    Other values are padded with `NA`.
#' @param names_glue    a parameter that may allow multiple matches. By default,
#'                      (`NULL`), no multiple matches are allowed since the
#'                      resulting tag names will no longer be unique.
#'
#'    The value of `names_glue` can be `logical`, with the value `FALSE` being
#'    equivalent to `NULL`. If `TRUE`, then the resulting new tag names will be
#'    enforced to be unique by adding a number index, i.e. a number index will
#'    be glued to the tag names (hence the argument name).
#'
#'    Finally, `names_glue` can be a string, where you supply a glue
#'    specification that uses the variable names found in `y` (columns for data
#'    frames, traits for matrixsets) columns to create a custom new tag name. A
#'    special value `.tag` allows you to access the original tag name. Note that
#'    currently only the curly brackets ({}) can be used in the glue
#'    specification.
#'
#'    When making the unique tag names, *only* the non-unique names are modified.
#'    Also, `adjust = TRUE` must be enforced for `names_glue` to work.
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
#' ms <- tryCatch(join_row_info(ms2, student_results, type = "full"),
#'                error = function(e) e)
#' is(ms, "error") # TRUE
#' ms$message
#'
#' # Now it works.
#' ms <- join_row_info(ms2, student_results, type = "full", adjust = TRUE)
#' dim(ms2)
#' dim(ms)
#' matrix_elm(ms, 1)
#'
#' # Similarly, this will fail because tag names are no longer unique
#' meta <- tibble::tibble(sample = c("student 2", "student 2"),
#'                       msr = c("height", "weight"),
#'                       value = c(145, 32))
#' ms <- tryCatch(join_row_info(student_results, meta, by = c(".rowname"="sample")),
#'                error = function(e) e)
#' is(ms, "error") # TRUE
#' ms$message
#'
#' # This works, by forcing the tag names to be unique. Notice that we suppress
#' # the warning for now. We'll come back to it.
#' suppressWarnings(
#'    join_row_info(student_results, meta, by = c(".rowname"="sample"),
#'                  adjust = TRUE, names_glue = TRUE)
#' )
#' # Here's the warning: we're being told there was a change in tag names
#' (purrr::quietly(join_row_info)(student_results, meta,
#'                                by = c(".rowname"="sample"), adjust = TRUE,
#'                                names_glue = TRUE))$warnings
#'
#' # You can have better control on how the tag change occurs, for instance by
#' # appending the msr value to the name
#' suppressWarnings(
#'    join_row_info(student_results, meta, by = c(".rowname"="sample"),
#'                  adjust = TRUE, names_glue = "{.tag}_{msr}")
#' )
#' # In this specific example, the {.tag} was superfluous, since the default is
#' # to append after the tag name
#' suppressWarnings(
#'    join_row_info(student_results, meta, by = c(".rowname"="sample"),
#'                  adjust = TRUE, names_glue = "{msr}")
#' )
#' # But the keyword is useful if you want to shuffle order
#' suppressWarnings(
#'    join_row_info(student_results, meta, by = c(".rowname"="sample"),
#'                  adjust = TRUE, names_glue = "{msr}.{.tag}")
#' )
#'
#' # You are warned when there is a change in traits
#' meta <- tibble::tibble(sample = c("student 2", "student 2"),
#'                        class = c("classA", "classA"),
#'                        msr = c("height", "weight"),
#'                        value = c(145, 32))
#' (purrr::quietly(join_row_info)(student_results, meta,
#'                                by = c(".rowname"="sample"), adjust = TRUE,
#'                                names_glue = TRUE))$warnings[2]
#'
#' # Groups are automatically adjusted
#' sr_gr <- row_group_by(student_results, class)
#' gr_orig_tmp <- row_group_meta(row_group_by(student_results, class))
#' gr_orig <- tidyr::unnest(gr_orig_tmp, .rows)
#' suppressWarnings(
#'   {
#'     jn <- join_row_info(sr_gr, meta, by = c(".rowname" = "sample", "class"),
#'                       adjust = TRUE, names_glue = TRUE)
#'     new_gr_tmp <- row_group_meta(jn)
#'     new_gr <- tidyr::unnest(new_gr_tmp, .rows)
#'   }
#' )
#' list(gr_orig, new_gr)
#'
#' # In the example above, the join operation changed the class of 'class',
#' # which in turn changed the grouping meta info. You are warned of both.
#' (purrr::quietly(join_row_info)(sr_gr, meta,
#'                                by = c(".rowname"="sample", "class"),
#'                                adjust = TRUE,  names_glue = TRUE))$warnings
#'
#' # A change in trait name that was used for grouping will result in losing the
#' # grouping. You are warning of the change in grouping structure.
#' (purrr::quietly(join_row_info)(sr_gr, meta,
#'                                by = c(".rowname"="sample"),
#'                                adjust = TRUE,  names_glue = TRUE))$warnings
#'
#' @name join



# # @rdname join
# # @export
# join_row_info <- function(.ms, y, type = "left", by = NULL, adjust = FALSE,
#                           suffix = c(".x", ".y"), na_matches = c("na", "never"))
#   UseMethod("join_row_info")
#
# # @export
# join_row_info.matrixset <- function(.ms, y, type = "left", by = NULL,
#                                     adjust = FALSE, suffix = c(".x", ".y"),
#                                     na_matches = c("na", "never"))
# {
#   na_matches <- match.arg(na_matches)
#   .join_info(type, "row", .ms, y, by = by, suffix = suffix,
#              na_matches = na_matches, adjust = adjust)
# }
#
#
#
# # @rdname join
# # @export
# join_column_info <- function(.ms, y, type = "left", by = NULL, adjust = FALSE,
#                              suffix = c(".x", ".y"), na_matches = c("na", "never"))
#   UseMethod("join_column_info")
#
# # @export
# join_column_info.matrixset <- function(.ms, y, type = "left", by = NULL,
#                                        adjust = FALSE, suffix = c(".x", ".y"),
#                                        na_matches = c("na", "never"))
# {
#   na_matches <- match.arg(na_matches)
#   .join_info(type, "col", .ms, y, by = by, suffix = suffix,
#              na_matches = na_matches, adjust = adjust)
# }


#' @rdname join
#' @export
join_row_info <- function(.ms, y, type = "left", by = NULL, adjust = FALSE,
                          names_glue = NULL, suffix = c(".x", ".y"),
                          na_matches = c("na", "never"))
  UseMethod("join_row_info")

#' @export
join_row_info.matrixset <- function(.ms, y, type = "left", by = NULL,
                                    adjust = FALSE, names_glue = NULL,
                                    suffix = c(".x", ".y"),
                                    na_matches = c("na", "never"))
{
  na_matches <- match.arg(na_matches)
  .join_info(type, "row", .ms, y, by = by, suffix = suffix,
             na_matches = na_matches, adjust = adjust, names_glue = names_glue)
}



#' @rdname join
#' @export
join_column_info <- function(.ms, y, type = "left", by = NULL, adjust = FALSE,
                             names_glue = NULL, suffix = c(".x", ".y"),
                             na_matches = c("na", "never"))
  UseMethod("join_column_info")

#' @export
join_column_info.matrixset <- function(.ms, y, type = "left", by = NULL,
                                       adjust = FALSE, names_glue = NULL,
                                       suffix = c(".x", ".y"),
                                       na_matches = c("na", "never"))
{
  na_matches <- match.arg(na_matches)
  .join_info(type, "col", .ms, y, by = by, suffix = suffix,
             na_matches = na_matches, adjust = adjust, names_glue = names_glue)
}



