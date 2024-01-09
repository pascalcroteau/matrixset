


# TEST USING COLUMN TRAIT IN COLUMN LOOP (AND ROW)
# test same context as margin loop




norm_call <- function(quo, var, .convert_name = TRUE)
{
  expr <- rlang::quo_get_expr(quo)

  if (rlang::is_formula(expr)) {
    expr <- rlang::f_rhs(expr)
    if (!(is.call(expr) && is(expr, "{"))) expr <- rlang::call2("{", expr)
  }

  colon <- FALSE
  expr_orig <- expr
  if (rlang::is_call(expr, "::")) {
    # pkg <- expr[[2]]
    expr <- expr[[3]]
    colon <- TRUE
  }
  if (is.name(expr)) {
    if (!.convert_name)
      stop("function names are not accepted in this context", call. = FALSE)
    # expr <- rlang::call2(expr, !!!rlang::syms(var))
    expr <- if (colon) rlang::call2(expr_orig, !!!rlang::syms(var)) else rlang::call2(expr, !!!rlang::syms(var))
  }
  # if (colon) expr <- call("::", pkg, expr)
  rlang::quo_set_expr(quo, expr)
}



assess_fun_names <- function(nms, tag, simplify)
{
  if (is.na(tag) || !simplify) return(invisible(NULL))
  if (any(nms == tag))
    stop(paste("the function results can't be named", shQuote(tag)))
}





.tag <- function(mg) switch(mg, "row" = .rowtag, "col" = .coltag)
margin <- function(mg) switch (mg, "row" = nrow, "col" = ncol)
margin_nms <- function(mg) switch (mg, "row" = rownames, "col" = colnames)
margin_info <- function(mg) switch (mg, "row" = column_info, "col" = row_info)
margin_info_compl <- function(mg) switch (mg, "row" = row_info, "col" = column_info)
group_where <- function(mg) switch (mg, "row" = column_group_where, "col" = row_group_where)
group_meta <- function(mg) switch (mg, "row" = column_group_meta, "col" = row_group_meta)
margin_group_vars <- function(mg) switch (mg, "row" = column_group_vars, "col" = row_group_vars)



set_env_expr <- quote({
  top <- new.env()
  middle <- new.env(parent = top)
  funs <- new.env(parent = middle)
  bottom <- new.env(parent = funs)
  mask <- rlang::new_data_mask(bottom, top = top)
  mask$.data <- rlang::as_data_pronoun(mask)
})


context_matidx <- quote({
  if (is.null(matidx)) {
    nmat <- .nmatrix(ms)
    matidx <- seq_len(nmat)
    seq_mats <- matidx
    matnms <- matrixnames(ms)
  } else {
    matidx <- index_to_integer(matidx, nmatrix(ms), matrixnames(ms))
    nmat <- length(matidx)
    seq_mats <- seq_len(nmat)
    matnms <- matrixnames(ms)[matidx]
  }
  seq_mats <- stats::setNames(seq_mats, matnms)
})



funs_context_expr <- quote({
  funs[["current_row_info"]] <- function() {
    funs$.__row_info
  }

  funs[["current_column_info"]] <- function() {
    funs$.__column_info
  }

  funs[["current_n_row"]] <- function() {
    nrow(funs$.__row_info)
  }

  funs[["current_n_column"]] <- function() {
    nrow(funs$.__column_info)
  }

  funs[["current_row_name"]] <- function() {
    funs$.__row_name
  }

  funs[["row_pos"]] <- function() {
    funs$.__row_idx
  }

  funs[["row_rel_pos"]] <- function() {
    seq_len(nrow(funs$.__row_info))
  }

  funs[["current_column_name"]] <- function() {
    funs$.__column_name
  }

  funs[["column_pos"]] <- function() {
    funs$.__column_idx
  }

  funs[["column_rel_pos"]] <- function() {
    seq_len(nrow(funs$.__column_info))
  }
})


context_enclos_expr <- quote({
  context_enclos("current_row_info", funs)
  context_enclos("current_column_info", funs)
  context_enclos("current_n_row", funs)
  context_enclos("current_n_column", funs)
  context_enclos("current_row_name", funs)
  context_enclos("current_column_name", funs)
  context_enclos("row_pos", funs)
  context_enclos("row_rel_pos", funs)
  context_enclos("column_pos", funs)
  context_enclos("column_rel_pos", funs)
})




context_funs_assign_expr <- function(mrg, inf_sym, idx_expr, name_expr)
{
  switch(mrg,
         row = substitute({
           funs[[".__row_info"]] <- inf
           funs[[".__row_idx"]] <- idx
           funs[[".__row_name"]] <- nms
         },
         list(inf = inf_sym,
              idx = idx_expr,
              nms = name_expr)),

         substitute({
           funs[[".__column_info"]] <- inf
           funs[[".__column_idx"]] <- idx
           funs[[".__column_name"]] <- nms
         },
         list(inf = inf_sym,
              idx = idx_expr,
              nms = name_expr)))
}




eval_fun_margin <- function(ms, mrg, var_lab, ..., matidx, .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  nmat <- NULL
  seq_mats <- NULL
  matnms <- NULL
  mask <- NULL

  if (is.null(ms$matrix_set)) return(NULL)

  N <- margin(mrg)(ms)
  inf <- margin_info(mrg)(ms)
  inf_compl <- margin_info_compl(mrg)(ms)

  mrg_compl <- switch(mrg, row = "col", "row")

  eval(context_matidx)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")


  nmfn <- names(quosures)
  assess_fun_names(nmfn, .tag(mrg)(ms), .simplify)

  nfn <- length(quosures)
  seq_fn <- stats::setNames(seq_len(nfn), nmfn)
  for (i in seq_fn) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }

  eval(set_env_expr)

  eval(funs_context_expr)
  eval(context_enclos_expr)




  v <- vector('list', nmat)
  names(v) <- matnms

  vm <- vector('list', N)
  names(vm) <- margin_nms(mrg)(ms)

  vf <- vector('list', nfn)
  names(vf) <- nmfn

  top <- list2env(inf, top)

  eval(context_funs_assign_expr(mrg_compl, quote(inf),
                                quote(seq_len(margin(mrg_compl)(ms))),
                                quote(margin_nms(mrg_compl)(ms))))

  for (k in seq_mats) {

    M <- ms$matrix_set[[matidx[[k]]]]

    for (j in 1:N) {

      inf_compl_j <- inf_compl[j, , drop = FALSE]
      top <- list2env(inf_compl_j, top)

      eval(context_funs_assign_expr(mrg, quote(inf_compl_j), quote(j),
                                    quote(margin_nms(mrg)(ms)[j])))

      if (!is.null(M)) {
        mat <- if (mrg == "row") M[j, , drop = TRUE] else M[, j, drop = TRUE]
        middle[[var_lab]] <- mat
      }

      for (vidx in seq_fn) {

        if (is.null(M)) {
          vf[vidx] <- list(NULL)
        } else {
          vf[[vidx]] <- rlang::eval_tidy(quosures[[vidx]], mask, env)
        }

      }
      vm[[j]] <- vf
    }

    v[[k]] <- vm

  }


  v

}





eval_fun_margin_grp <- function(ms, mrg, var_lab, ..., matidx, .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  nmat <- NULL
  seq_mats <- NULL
  matnms <- NULL
  mask <- NULL

  if (is.null(ms$matrix_set)) return(NULL)

  N <- margin(mrg)(ms)
  inf <- margin_info(mrg)(ms)
  inf_compl <- margin_info_compl(mrg)(ms)

  gr_idx <- group_where(mrg)(ms)

  ngroup <- length(gr_idx)

  mrg_compl <- switch(mrg, row = "col", "row")

  eval(context_matidx)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")


  nmfn <- names(quosures)
  assess_fun_names(nmfn, .tag(mrg)(ms), .simplify)

  nfn <- length(quosures)
  seq_fn <- stats::setNames(seq_len(nfn), nmfn)
  for (i in seq_fn) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }

  eval(set_env_expr)

  eval(funs_context_expr)
  eval(context_enclos_expr)



  v <- vector('list', ngroup)

  vmt <- vector('list', nmat)
  names(vmt) <- matnms

  vm <- vector('list', N)
  names(vm) <- margin_nms(mrg)(ms)

  vf <- vector('list', nfn)
  names(vf) <- nmfn

  for (gr in 1:ngroup) {

    idx <- gr_idx[[gr]]

    inf_idx <- inf[idx, , drop = FALSE]
    top <- list2env(inf_idx, top)

    eval(context_funs_assign_expr(mrg_compl, quote(inf_idx), quote(idx),
                                  quote(margin_nms(mrg_compl)(ms)[idx])))


    for (k in seq_mats) {

      Mfull <- ms$matrix_set[[matidx[[k]]]]

      if (!is.null(Mfull)) {
        M <- if (mrg == "row") Mfull[, idx, drop = FALSE] else Mfull[idx, , drop = FALSE]
      }

      for (j in 1:N) {

        inf_compl_j <- inf_compl[j, , drop = FALSE]
        top <- list2env(inf_compl_j, top)

        eval(context_funs_assign_expr(mrg, quote(inf_compl_j), quote(j),
                                      quote(margin_nms(mrg)(ms)[j])))

        if (is.null(Mfull)) {
          for (vidx in seq_fn) vf[vidx] <- list(NULL)
        } else {

          mat <- if (mrg == "row") M[j, , drop = TRUE] else M[, j, drop = TRUE]
          middle[[var_lab]] <- mat

          for (vidx in seq_fn) {
            vf[[vidx]] <- rlang::eval_tidy(quosures[[vidx]], mask, env)
          }

        }

        vm[[j]] <- vf

      }

      vmt[[k]] <- vm

    }

    v[[gr]] <- vmt

  }


  ans <- group_meta(mrg)(ms)
  ans$.rows <- NULL
  v <- lapply(seq_mats, function(k) {
    ans$.vals <- lapply(1:ngroup, function(gr) {
      v[[gr]][[k]]
    })
    ans
  })

  if (.simplify) attr(v, "group_vars") <- margin_group_vars(mrg)(ms)

  v

}





eval_fun_margin_mult <- function(ms, mrg, var_lab, ..., matidx, as_list_mat,
                                 .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  nmat <- NULL
  seq_mats <- NULL
  matnms <- NULL
  mask <- NULL

  if (is.null(ms$matrix_set)) return(NULL)

  N <- margin(mrg)(ms)
  inf <- margin_info(mrg)(ms)
  inf_compl <- margin_info_compl(mrg)(ms)

  mrg_compl <- switch(mrg, row = "col", "row")

  eval(context_matidx)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")

  if (!as_list_mat) mat_lab <- paste0(var_lab, seq_mats)

  nmfn <- names(quosures)
  assess_fun_names(nmfn, .tag(mrg)(ms), .simplify)

  nfn <- length(quosures)
  seq_fn <- stats::setNames(seq_len(nfn), nmfn)
  for (i in seq_fn) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }

  eval(set_env_expr)

  eval(funs_context_expr)
  eval(context_enclos_expr)

  mat_list <- vector("list", nmat)
  names(mat_list) <- matnms

  val <- vector("list", N)
  names(val) <- margin_nms(mrg)(ms)

  v <- vector('list', nfn)
  names(v) <- nmfn

  top <- list2env(inf, top)

  eval(context_funs_assign_expr(mrg_compl, quote(inf),
                                quote(seq_len(margin(mrg_compl)(ms))),
                                quote(margin_nms(mrg_compl)(ms))))


  for (j in 1:N) {

    inf_compl_j <- inf_compl[j, , drop = FALSE]

    eval(context_funs_assign_expr(mrg, quote(inf_compl_j), quote(j),
                                  quote(margin_nms(mrg)(ms)[j])))

    top <- list2env(inf_compl_j, top)

    for (k in seq_mats) {

      M <- ms$matrix_set[[matidx[[k]]]]


      if (!is.null(M)) {
        mat_list[[k]] <- if (mrg == "row") M[j, , drop = TRUE] else M[, j, drop = TRUE]
      }

    }

    if (as_list_mat) {
      middle[[var_lab]] <- mat_list
    } else {
      names(mat_list) <- mat_lab
      middle <- list2env(mat_list, envir = middle)
    }


    for (vidx in seq_fn)
      v[[vidx]] <- rlang::eval_tidy(quosures[[vidx]], mask, env)
    val[[j]] <- v


  }


  val

}





eval_fun_margin_grp_mult <- function(ms, mrg, var_lab, ..., matidx, as_list_mat,
                                     .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  nmat <- NULL
  seq_mats <- NULL
  matnms <- NULL
  mask <- NULL

  if (is.null(ms$matrix_set)) return(NULL)

  N <- margin(mrg)(ms)
  inf <- margin_info(mrg)(ms)
  inf_compl <- margin_info_compl(mrg)(ms)

  gr_idx <- group_where(mrg)(ms)
  ngroup <- length(gr_idx)

  mrg_compl <- switch(mrg, row = "col", "row")

  eval(context_matidx)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")

  if (!as_list_mat) mat_lab <- paste0(var_lab, seq_mats)

  nmfn <- names(quosures)
  assess_fun_names(nmfn, .tag(mrg)(ms), .simplify)

  nfn <- length(quosures)
  seq_fn <- stats::setNames(seq_len(nfn), nmfn)
  for (i in seq_fn) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }

  eval(set_env_expr)

  eval(funs_context_expr)
  eval(context_enclos_expr)


  mat_list <- vector("list", nmat)
  names(mat_list) <- matnms

  val <- vector('list', nfn)
  names(val) <- nmfn

  vj <- vector('list', N)
  names(vj) <- margin_nms(mrg)(ms)

  v <- vector('list', ngroup)

  for (gr in 1:ngroup) {

    idx <- gr_idx[[gr]]

    inf_idx <- inf[idx, , drop = FALSE]
    top <- list2env(inf_idx, top)

    eval(context_funs_assign_expr(mrg_compl, quote(inf_idx), quote(idx),
                                  quote(margin_nms(mrg_compl)(ms)[idx])))


    for (j in 1:N) {

      inf_compl_j <- inf_compl[j, , drop = FALSE]

      eval(context_funs_assign_expr(mrg, quote(inf_compl_j), quote(j),
                                    quote(margin_nms(mrg)(ms)[j])))

      top <- list2env(inf_compl_j, top)


      for (k in seq_mats) {

        M <- ms$matrix_set[[matidx[[k]]]]

        if (!is.null(M)) {
          mat_list[[k]] <- if (mrg == "row") M[j, , drop = TRUE][idx] else M[, j, drop = TRUE][idx]
        }

      }

      if (as_list_mat) {
        middle[[var_lab]] <- mat_list
      } else {
        names(mat_list) <- mat_lab
        middle <- list2env(mat_list, envir = middle)
      }


      for (vidx in seq_fn)
        val[[vidx]] <- rlang::eval_tidy(quosures[[vidx]], mask, env)
      vj[[j]] <- val

    }

    v[[gr]] <- vj

  }


  ans <- group_meta(mrg)(ms)
  ans$.rows <- NULL
  ans$.vals <- v


  if (.simplify) attr(ans, "group_vars") <- margin_group_vars(mrg)(ms)


  ans

}






eval_fun_matrix <- function(ms, ..., matidx, .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  nmat <- NULL
  seq_mats <- NULL
  matnms <- NULL
  mask <- NULL

  if (is.null(ms$matrix_set)) return(NULL)

  var_lab <- var_lab_mat

  nr <- nrow(ms)
  nc <- ncol(ms)
  rinf <- row_info(ms)
  cinf <- column_info(ms)

  eval(context_matidx)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")


  nmfn <- names(quosures)
  assess_fun_names(nmfn, .rowtag(ms), .simplify)
  assess_fun_names(nmfn, .coltag(ms), .simplify)

  nfn <- length(quosures)
  seq_fn <- stats::setNames(seq_len(nfn), nmfn)
  for (i in seq_fn) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }

  eval(set_env_expr)

  eval(funs_context_expr)
  eval(context_enclos_expr)


  v <- vector('list', nmat)
  names(v) <- matnms

  vf <- vector('list', nfn)
  names(vf) <- nmfn

  top <- list2env(cinf, top)
  top <- list2env(rinf, top)
  eval(context_funs_assign_expr("col", quote(cinf), quote(seq_len(ncol(ms))),
                                quote(colnames(ms))))
  eval(context_funs_assign_expr("row", quote(rinf), quote(seq_len(nrow(ms))),
                                quote(rownames(ms))))


  for (k in seq_mats) {

    M <- ms$matrix_set[[matidx[[k]]]]


    if (!is.null(M)) {
      middle[[var_lab]] <- M
    }

    for (vidx in seq_fn) {

      if (is.null(M)) {
        vf[vidx] <- list(NULL)
      } else {
        vf[[vidx]] <- rlang::eval_tidy(quosures[[vidx]], mask, env)
      }

    }

    v[[k]] <- vf

  }


  v

}




#sgms = singly grouped matrixset
eval_fun_matrix_sgms <- function(ms, mrg, ..., matidx, .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  nmat <- NULL
  seq_mats <- NULL
  matnms <- NULL
  mask <- NULL

  if (is.null(ms$matrix_set)) return(NULL)

  var_lab <- var_lab_mat
  mrg_compl <- switch(mrg, row = "col", col = "row", "oops")
  if (mrg_compl == "oops") stop("wrong margin specification")

  nr <- nrow(ms)
  nc <- ncol(ms)
  inf <- switch(mrg, row = row_info(ms), column_info(ms))
  inf_compl <- switch(mrg_compl, row = row_info(ms), column_info(ms))


  gr_idx <- switch(mrg, "row"=row_group_where(ms), column_group_where(ms))
  ngroup <- length(gr_idx)

  eval(context_matidx)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")


  nmfn <- names(quosures)
  assess_fun_names(nmfn, .rowtag(ms), .simplify)
  assess_fun_names(nmfn, .coltag(ms), .simplify)

  nfn <- length(quosures)
  seq_fn <- stats::setNames(seq_len(nfn), nmfn)
  for (i in seq_fn) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }


  eval(set_env_expr)

  eval(funs_context_expr)
  eval(context_enclos_expr)

  v <- vector('list', ngroup)

  vmt <- vector('list', nmat)
  names(vmt) <- matnms

  vf <- vector('list', nfn)
  names(vf) <- nmfn

  for (gr in 1:ngroup) {

    idx <- gr_idx[[gr]]

    inf_idx <- inf[idx, , drop = FALSE]
    top <- list2env(inf_idx, top)

    eval(context_funs_assign_expr(mrg, quote(inf_idx), quote(idx),
                                  quote(margin_nms(mrg)(ms)[idx])))

    top <- list2env(inf_compl, top)

    eval(context_funs_assign_expr(mrg_compl, quote(inf_compl),
                                  quote(seq_len(margin(mrg_compl)(ms))),
                                  quote(margin_nms(mrg_compl)(ms))))


    for (k in seq_mats) {

      M <- ms$matrix_set[[matidx[[k]]]]
      M <- switch(mrg, row=M[idx, , drop = FALSE], M[, idx, drop = FALSE])


      if (!is.null(M)) {
        middle[[var_lab]] <- M
      }

      for (vidx in seq_fn) {

        if (is.null(M)) {
          vf[vidx] <- list(NULL)
        } else {
          vf[[vidx]] <- rlang::eval_tidy(quosures[[vidx]], mask, env)
        }

      }

      vmt[[k]] <- vf

    }
    v[[gr]] <- vmt
  }


  ans <- group_meta(mrg_compl)(ms)
  ans$.rows <- NULL
  v <- lapply(seq_mats, function(k) {
    ans$.vals <- lapply(1:ngroup, function(gr) {
      v[[gr]][[k]]
    })
    ans
  })

  if (.simplify) attr(v, "group_vars") <- margin_group_vars(mrg_compl)(ms)

  v

}




eval_fun_matrix_dgms <- function(ms, ..., matidx, .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  nmat <- NULL
  seq_mats <- NULL
  matnms <- NULL
  mask <- NULL

  if (is.null(ms$matrix_set)) return(NULL)

  var_lab <- var_lab_mat

  nr <- nrow(ms)
  nc <- ncol(ms)
  rinf <- row_info(ms)
  cinf <- column_info(ms)

  gr_idx_row <- row_group_where(ms)
  gr_idx_col <- column_group_where(ms)
  ngroup_row <- length(gr_idx_row)
  ngroup_col <- length(gr_idx_col)

  eval(context_matidx)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")


  nmfn <- names(quosures)
  assess_fun_names(nmfn, .rowtag(ms), .simplify)
  assess_fun_names(nmfn, .coltag(ms), .simplify)

  nfn <- length(quosures)
  seq_fn <- stats::setNames(seq_len(nfn), nmfn)
  for (i in seq_fn) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }

  eval(set_env_expr)

  eval(funs_context_expr)
  eval(context_enclos_expr)

  ans_col <- column_group_meta(ms)
  ans_col$.rows <- NULL

  v <- vector('list', ngroup_row)
  vr <- vector('list', ngroup_col)

  vmt <- vector('list', nmat)
  names(vmt) <- matnms

  vf <- vector('list', nfn)
  names(vf) <- nmfn

  for (grr in 1:ngroup_row) {


    for (grc in 1:ngroup_col) {

      idx_row <- gr_idx_row[[grr]]
      idx_col <- gr_idx_col[[grc]]

      inf_idx <- rinf[idx_row, , drop = FALSE]
      inf_compl <- cinf[idx_col, , drop = FALSE]
      top <- list2env(inf_idx, top)
      top <- list2env(inf_compl, top)

      eval(context_funs_assign_expr("row", quote(inf_idx), quote(idx_row),
                                    quote(rownames(ms)[idx_row])))
      eval(context_funs_assign_expr("col", quote(inf_compl), quote(idx_col),
                                    quote(colnames(ms)[idx_col])))

      for (k in seq_mats) {

        M <- ms$matrix_set[[matidx[[k]]]]


        if (!is.null(M)) {
          M <- M[idx_row, idx_col, drop = FALSE]
          middle[[var_lab]] <- M
        }

        for (vidx in seq_fn) {

          if (is.null(M)) {
            vf[vidx] <- list(NULL)
          } else {
            vf[[vidx]] <- rlang::eval_tidy(quosures[[vidx]], mask, env)
          }

        }

        vmt[[k]] <- vf

      }

      vr[[grc]] <- vmt

    }

    vtmp <- lapply(seq_mats, function(k) {
      ans_col$.vals <- lapply(1:ngroup_col, function(gr) {
        vr[[gr]][[k]]
      })
      ans_col
    })
    v[[grr]] <- vtmp


  }


  ans <- row_group_meta(ms)
  ans$.rows <- NULL
  vans <- lapply(seq_mats, function(k) {
    ans$.___tmp___ <- lapply(1:ngroup_row, function(gr) {
      v[[gr]][[k]]
    })
    tidyr::unnest(ans, ".___tmp___")
  })

  if (.simplify) attr(vans, "group_vars") <- margin_group_vars("row")(ms)

  vans

}





eval_fun_matrix_mult <- function(ms, ..., matidx, as_list_mat, .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  nmat <- NULL
  seq_mats <- NULL
  matnms <- NULL
  mask <- NULL

  if (is.null(ms$matrix_set)) return(NULL)

  var_lab <- var_lab_mat

  nr <- nrow(ms)
  nc <- ncol(ms)
  rinf <- row_info(ms)
  cinf <- column_info(ms)

  eval(context_matidx)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")

  if (!as_list_mat) mat_lab <- paste0(var_lab, seq_mats)

  nmfn <- names(quosures)
  assess_fun_names(nmfn, .rowtag(ms), .simplify)
  assess_fun_names(nmfn, .coltag(ms), .simplify)

  nfn <- length(quosures)
  seq_fn <- stats::setNames(seq_len(nfn), nmfn)
  for (i in seq_fn) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }


  eval(set_env_expr)

  eval(funs_context_expr)
  eval(context_enclos_expr)



  mat_list <- vector("list", nmat)
  names(mat_list) <- matnms

  v <- vector('list', nfn)
  names(v) <- nmfn


  top <- list2env(cinf, top)
  top <- list2env(rinf, top)
  eval(context_funs_assign_expr("col", quote(cinf), quote(seq_len(ncol(ms))),
                                quote(colnames(ms))))
  eval(context_funs_assign_expr("row", quote(rinf), quote(seq_len(nrow(ms))),
                                quote(rownames(ms))))


  for (k in seq_mats) {
    M <- ms$matrix_set[[matidx[[k]]]]
    if (!is.null(M)) mat_list[[k]] <- M
  }

  if (as_list_mat) {
    middle[[var_lab]] <- mat_list
  } else {
    names(mat_list) <- mat_lab
    middle <- list2env(mat_list, envir = middle)
  }


  for (vidx in seq_fn)
    v[[vidx]] <- rlang::eval_tidy(quosures[[vidx]], mask, env)

  v

}





#sgms = singly grouped matrixset
eval_fun_matrix_sgms_mult <- function(ms, mrg, ..., matidx, as_list_mat,
                                      .simplify, env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  nmat <- NULL
  seq_mats <- NULL
  matnms <- NULL
  mask <- NULL

  if (is.null(ms$matrix_set)) return(NULL)

  var_lab <- var_lab_mat
  mrg_compl <- switch(mrg, row = "col", col = "row", "oops")
  if (mrg_compl == "oops") stop("wrong margin specification")

  nr <- nrow(ms)
  nc <- ncol(ms)
  inf <- switch(mrg, row = row_info(ms), column_info(ms))
  inf_compl <- switch(mrg_compl, row = row_info(ms), column_info(ms))

  gr_idx <- switch(mrg, "row"=row_group_where(ms), column_group_where(ms))
  ngroup <- length(gr_idx)

  eval(context_matidx)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")

  if (!as_list_mat) mat_lab <- paste0(var_lab, seq_mats)

  nmfn <- names(quosures)
  assess_fun_names(nmfn, .rowtag(ms), .simplify)
  assess_fun_names(nmfn, .coltag(ms), .simplify)

  nfn <- length(quosures)
  seq_fn <- stats::setNames(seq_len(nfn), nmfn)
  for (i in seq_fn) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }


  eval(set_env_expr)

  eval(funs_context_expr)
  eval(context_enclos_expr)

  mat_list <- vector("list", nmat)
  names(mat_list) <- matnms

  val <- vector("list", ngroup)

  v <- vector('list', nfn)
  names(v) <- nmfn

  for (gr in 1:ngroup) {
    idx <- gr_idx[[gr]]

    inf_idx <- inf[idx, , drop = FALSE]
    top <- list2env(inf_idx, top)

    eval(context_funs_assign_expr(mrg, quote(inf_idx), quote(idx),
                                  quote(margin_nms(mrg)(ms)[idx])))

    top <- list2env(inf_compl, top)

    eval(context_funs_assign_expr(mrg_compl, quote(inf_compl),
                                  quote(seq_len(margin(mrg_compl)(ms))),
                                  quote(margin_nms(mrg_compl)(ms))))

    for (k in seq_mats) {

      M <- ms$matrix_set[[matidx[[k]]]]
      M <- switch(mrg, row=M[idx, , drop = FALSE], M[, idx, drop = FALSE])

      if (!is.null(M)) mat_list[[k]] <- M
    }

    if (as_list_mat) {
      middle[[var_lab]] <- mat_list
    } else {
      names(mat_list) <- mat_lab
      middle <- list2env(mat_list, envir = middle)
    }


    for (vidx in seq_fn)
      v[[vidx]] <- rlang::eval_tidy(quosures[[vidx]], mask, env)

    val[[gr]] <- v
  }


  ans <- group_meta(mrg_compl)(ms)
  ans$.rows <- NULL
  ans$.vals <- val

  if (.simplify) attr(ans, "group_vars") <- margin_group_vars(mrg_compl)(ms)

  ans

}






eval_fun_matrix_dgms_mult <- function(ms, ..., matidx, as_list_mat, .simplify,
                                      env)
{
  cl <- sys.call()
  cash_status$set(cl)
  on.exit(cash_status$clear(cl))

  nmat <- NULL
  seq_mats <- NULL
  matnms <- NULL
  mask <- NULL

  if (is.null(ms$matrix_set)) return(NULL)

  var_lab <- var_lab_mat

  nr <- nrow(ms)
  nc <- ncol(ms)
  rinf <- row_info(ms)
  cinf <- column_info(ms)

  gr_idx_row <- row_group_where(ms)
  gr_idx_col <- column_group_where(ms)
  ngroup_row <- length(gr_idx_row)
  ngroup_col <- length(gr_idx_col)

  eval(context_matidx)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")

  if (!as_list_mat) mat_lab <- paste0(var_lab, seq_mats)

  nmfn <- names(quosures)
  assess_fun_names(nmfn, .rowtag(ms), .simplify)
  assess_fun_names(nmfn, .coltag(ms), .simplify)

  nfn <- length(quosures)
  seq_fn <- stats::setNames(seq_len(nfn), nmfn)
  for (i in seq_fn) {
    quosures[[i]] <- norm_call(quosures[[i]], var_lab)
  }


  eval(set_env_expr)

  eval(funs_context_expr)
  eval(context_enclos_expr)

  mat_list <- vector("list", nmat)
  names(mat_list) <- matnms

  v <- vector('list', ngroup_row)
  vr <- vector('list', ngroup_col)

  val <- vector('list', nfn)
  names(val) <- nmfn

  for (grr in 1:ngroup_row) {


    for (grc in 1:ngroup_col) {

      idx_row <- gr_idx_row[[grr]]
      idx_col <- gr_idx_col[[grc]]

      inf_idx <- rinf[idx_row, , drop = FALSE]
      inf_compl <- cinf[idx_col, , drop = FALSE]
      top <- list2env(inf_idx, top)
      top <- list2env(inf_compl, top)

      eval(context_funs_assign_expr("row", quote(inf_idx), quote(idx_row),
                                    quote(rownames(ms)[idx_row])))
      eval(context_funs_assign_expr("col", quote(inf_compl), quote(idx_col),
                                    quote(colnames(ms)[idx_col])))


      for (k in seq_mats) {

        M <- ms$matrix_set[[matidx[[k]]]]
        M <- M[idx_row, idx_col, drop = FALSE]

        if (!is.null(M)) mat_list[[k]] <- M
      }


      if (as_list_mat) {
        middle[[var_lab]] <- mat_list
      } else {
        names(mat_list) <- mat_lab
        middle <- list2env(mat_list, envir = middle)
      }


      for (k in seq_mats) {

        M <- ms$matrix_set[[matidx[[k]]]]


        if (!is.null(M)) {
          M <- M[idx_row, idx_col, drop = FALSE]
          middle[[var_lab]] <- M
        }

        for (vidx in seq_fn) {

          if (is.null(M)) {
            val[vidx] <- list(NULL)
          } else {
            val[[vidx]] <- rlang::eval_tidy(quosures[[vidx]], mask, env)
          }

        }

        vr[[grc]] <- val


        ans <- column_group_meta(ms)
        ans$.rows <- NULL
        ans$.vals <- vr

      }

      v[[grr]] <- ans

    }


  }


  ans <- row_group_meta(ms)
  ans$.rows <- NULL
  ans$.___tmp___ <- v
  ans <- tidyr::unnest(ans, ".___tmp___")

  if (.simplify) attr(ans, "group_vars") <- row_group_vars(ms)
  ans
}








make_longer_val <- function(.v, force_name)
{
  tbls <- purrr::imap(.v,
                      ~ {
                        ln <- length(.x)
                        if (ln > 1 && is.null(names(.x))) names(.x) <- make_names(.x, "")
                        if (ln > 1L || force_name) {
                          stats::setNames(tibble::tibble(names(.x), unname(.x)),
                                          c(paste0(.y, ".name"), .y))
                        } else {
                          stats::setNames(tibble::tibble(.x), .y)
                        }
                      })
  dplyr::bind_cols(tbls)
}




make_longer <- function(val, force_name)
{
  lapply(val, function(x) {
    len <- unique(sapply(x, length))

    if (length(len) > 1) stop("vectors must be of the same length",
                              call. = FALSE)

    is_null <- sapply(x, is.null)
    if (any(is_null)) x[is_null] <- list(logical(0))

    make_longer_val(x, force_name)

  })
}






make_wider_val <- function(.v, force_name)
{
  tbls <- purrr::imap(.v,
                      ~ {
                        ln <- length(.x)
                        if (ln > 1L || force_name) {
                          nms <- names(.x)
                          if (is.null(nms) || any(nms == ""))
                            names(.x) <- make_names(.x, "")
                          names(.x) <- paste(.y, names(.x))
                          tibble::new_tibble(list_row(.x))
                        } else if (ln > 0L) {
                          stats::setNames(tibble::tibble(.x), .y)
                        } else {
                          mpty <- tibble::tibble()
                          mpty[[.y]] <- logical(0)
                          mpty
                        }
                      })
  dplyr::bind_cols(tbls)
}



make_wider <- function(val, force_name)
{
  lapply(val, function(x) {
    len <- unique(sapply(x, length))

    if (length(len) > 1) stop("vectors must be of the same length",
                              call. = FALSE)

    is_null <- sapply(x, is.null)
    if (any(is_null)) x[is_null] <- list(logical(0))

    make_wider_val(x, force_name)

  })
}




tblize_lg <- function(fna, mrg_tag, matrix = FALSE, mult = FALSE, force_name = FALSE)
{
  bind_long <- function(v, force_name, mrg_tag)
    dplyr::bind_rows(make_longer(v, force_name), .id = mrg_tag)

  if (is.null(fna)) return(NULL)
  grp_vars <- attr(fna, "group_vars")

  if (is.null(grp_vars)) {

    if (matrix) {
      if (mult) {
        make_longer_val(fna, force_name)
      } else make_longer(fna, force_name)
    } else {

      if (mult) {
        bind_long(fna, force_name, mrg_tag)
      } else {
        lapply(fna, function(m) {
          bind_long(m, force_name, mrg_tag)
        })
      }

    }

  } else {

    if (matrix) {
      if (mult) {
        unfold <- lapply(fna$.vals, function(v) make_longer_val(v, force_name))
        fna$.vals <- unfold
        tidyr::unnest(fna, ".vals")
      } else {
        lapply(fna, function(v) {
          .vals <- make_longer(v$.vals, force_name)
          v$.vals <- .vals
          tidyr::unnest(v, ".vals")
        })
      }

    } else {

      if (mult) {
        unfold <- lapply(fna$.vals, function(v) bind_long(v, force_name, mrg_tag))
        fna$.vals <- unfold
        tidyr::unnest(fna, ".vals")

      } else {
        lapply(fna, function(m) {
          unfold <- lapply(m$.vals, function(v) bind_long(v, force_name, mrg_tag))
          m$.vals <- unfold
          tidyr::unnest(m, ".vals")
        })
      }

    }

  }

}





tblize_wd <- function(fna, mrg_tag, matrix = FALSE, mult = FALSE, force_name = FALSE)
{
  bind_wide <- function(v, force_name, mrg_tag)
    dplyr::bind_rows(make_wider(v, force_name), .id = mrg_tag)

  if (is.null(fna)) return(NULL)
  grp_vars <- attr(fna, "group_vars")
  if (is.null(grp_vars)) {
    if (matrix) {
      if (mult) {
        make_wider_val(fna, force_name)
      } else {
        make_wider(fna, force_name)
      }

    } else {
      if (mult) {
        bind_wide(fna, force_name, mrg_tag)
      } else {
        lapply(fna, function(m) {
          bind_wide(m, force_name, mrg_tag)
        })
      }

    }

  } else {
    if (matrix) {
      if (mult) {
        unfold <- lapply(fna$.vals, function(v) make_wider_val(v, force_name))
        fna$.vals <- unfold
        tidyr::unnest(fna, ".vals")
      } else {
        lapply(fna, function(m) {
          .vals <- make_wider(m$.vals, force_name)
          m$.vals <- .vals
          tidyr::unnest(m, ".vals")
        })
      }

    } else {
      if (mult) {
        unfold <- lapply(fna$.vals, function(v) bind_wide(v, force_name, mrg_tag))
        fna$.vals <- unfold
        tidyr::unnest(fna, ".vals")
      } else {
        lapply(fna, function(m) {
          unfold <- lapply(m$.vals, function(v) bind_wide(v, force_name, mrg_tag))
          m$.vals <- unfold
          tidyr::unnest(m, ".vals")
        })
      }

    }

  }

}




#' Apply functions to each matrix of a matrixset
#'
#' @description
#' The `apply_matrix` function applies functions to each matrix of a `matrixset`.
#' The `apply_row`/`apply_column` functions do the same but separately for each
#' row/column. The functions can be applied to all matrices or only a subset.
#'
#' The `dfl`/`dfw` versions differ in their output format and when possible,
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
#' The matrixset package defines its own pronouns: `r var_lab_mat`, `r var_lab_row` and `r var_lab_col`, which
#' are discussed in the function specification argument (`...`).
#'
#' It is not necessary to import any of the pronouns (or load `rlang` in the
#' case of `.data` and `.env`) in a interactive session.
#'
#' It is useful however when writing a package to avoid the `R CMD check` notes.
#' As needed, you can import `.data` and `.env` (from `rlang`) or any of `r var_lab_mat`,
#' `r var_lab_row` or `r var_lab_col` from `matrixset`.
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
#'    * a function call, where you can use `r var_lab_mat` to represent the current matrix
#'       (for `apply_matrix`), `r var_lab_row` to represent the current row (for `apply_row`)
#'       and `r var_lab_col` for the current column (`apply_column`). Bare names of object
#'       traits can be used as well. For instance, `lm(.i ~ program)`.
#'
#'       The pronouns are also available for the multivariate version, under
#'       certain circumstances, but they have a different meaning. See the
#'       "Multivariate" section for more details.
#'    * a formula expression. The pronouns `r var_lab_mat`, `r var_lab_row` and
#'       `r var_lab_col` can be used as well. See examples to see the usefulness
#'       of this.
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
#' @param .force_name    `logical`. Used only for the simplified output versions
#'    (dfl/dfw). By default (`FALSE`), function IDs will be provided only if the
#'    function outcome is a vector of length 2 or more. If `.force_name` is
#'    `TRUE` then function IDs are provided in all situations.
#'
#'    This can be useful in situation of grouping. As the functions are
#'    evaluated independently within each group, there could be situations where
#'    function outcomes are of length 1 for some groups and lenght 2 or more in
#'    other groups.
#'
#'    See examples.
#'
#' @returns
#' A list for every matrix in the matrixset object. Each list is itself a list.
#' For `apply_matrix`, it is a list of the function values - `NULL` if the matrix
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
#' (mn_mat <- apply_matrix(student_results, mean))
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
#' (mat_summ <- apply_matrix(cl_prof_program_gr, avr = mean, med = median, rg = range))
#' # it doesn' make much sense, but this is to showcase format
#' (summ_gr <- apply_matrix(cl_prof_program_gr, avr = mean, med = median, rg = range))
#' (summ_gr_long <- apply_column_dfl(cl_prof_program_gr,
#'                                  ct = ~ c(avr = mean(.j), med = median(.j)),
#'                                  rg = range))
#' (summ_gr_wide <- apply_column_dfw(cl_prof_program_gr,
#'                                  ct = c(avr = mean(.j), med = median(.j)),
#'                                  rg = range))
#'
#'
#' # This is an example where you may want to use the .force_name argument
#' (apply_matrix_dfl(column_group_by(student_results, program), FC = colMeans(.m)))
#' (apply_matrix_dfl(column_group_by(student_results, program), FC = colMeans(.m),
#'                   .force_name = TRUE))
#'
#' @name loop
NULL



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
    warn_if(.matrix_wise, .input_list)
    eval_fun_margin(.ms, mrg="row", var_lab=var_lab_row, ..., matidx = .matrix,
                    .simplify = FALSE, env = rlang::caller_env())
  } else {
    eval_fun_margin_mult(.ms, mrg="row", var_lab=var_lab_row, ...,
                         matidx = .matrix, as_list_mat = .input_list,
                         .simplify = FALSE, env = rlang::caller_env())
  }
}

#' @export
apply_row.row_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                     .matrix_wise = TRUE, .input_list = FALSE)
{
  NextMethod()
}

#' @export
apply_row.col_grouped_ms <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                                .input_list = FALSE)
{
  if (.matrix_wise) {
    warn_if(.matrix_wise, .input_list)
    eval_fun_margin_grp(.ms, mrg="row", var_lab=var_lab_row, ...,
                        matidx = .matrix, .simplify = FALSE,
                        env = rlang::caller_env())
  } else {
    eval_fun_margin_grp_mult(.ms, mrg="row", var_lab=var_lab_row, ...,
                             matidx = .matrix, as_list_mat = .input_list,
                             .simplify = FALSE, env = rlang::caller_env())
  }
}

#' @export
apply_row.dual_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                     .matrix_wise = TRUE, .input_list = FALSE)
{
  apply_row.col_grouped_ms(.ms, ..., .matrix = .matrix,
                           .matrix_wise = .matrix_wise,
                           .input_list = .input_list)
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
    eval_fun_margin(.ms, mrg="row", var_lab=var_lab_row, ..., matidx = .matrix,
                    .simplify = TRUE, env = rlang::caller_env())
  } else {
    eval_fun_margin_mult(.ms, mrg="row", var_lab=var_lab_row, ...,
                         matidx = .matrix, as_list_mat = .input_list,
                         .simplify = TRUE, env = rlang::caller_env())
  }
  tblize_lg(appl, .rowtag(.ms), mult = !.matrix_wise, force_name = .force_name)
}

#' @export
apply_row_dfl.row_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                         .matrix_wise = TRUE,
                                         .input_list = FALSE,
                                         .force_name = FALSE)
{
  NextMethod()
}

#' @export
apply_row_dfl.col_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                         .matrix_wise = TRUE,
                                         .input_list = FALSE,
                                         .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  appl <- if (.matrix_wise) {
    eval_fun_margin_grp(.ms, mrg="row", var_lab=var_lab_row, ...,
                        matidx = .matrix, .simplify = TRUE,
                        env = rlang::caller_env())
  } else {
    eval_fun_margin_grp_mult(.ms, mrg="row", var_lab=var_lab_row, ...,
                             matidx = .matrix, as_list_mat = .input_list,
                             .simplify = TRUE, env = rlang::caller_env())
  }
  tblize_lg(appl, .rowtag(.ms), mult = !.matrix_wise, force_name = .force_name)
}

#' @export
apply_row_dfl.dual_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                          .matrix_wise = TRUE,
                                          .input_list = FALSE,
                                          .force_name = FALSE)
{
  apply_row_dfl.col_grouped_ms(.ms, ..., .matrix = .matrix,
                           .matrix_wise = .matrix_wise,
                           .input_list = .input_list)
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
    eval_fun_margin(.ms, mrg="row", var_lab=var_lab_row, ..., matidx = .matrix,
                    .simplify = TRUE, env = rlang::caller_env())
  } else {
    eval_fun_margin_mult(.ms, mrg="row", var_lab=var_lab_row, ...,
                         matidx = .matrix, as_list_mat = .input_list,
                         .simplify = TRUE, env = rlang::caller_env())
  }
  tblize_wd(appl, .rowtag(.ms), mult = !.matrix_wise, force_name = .force_name)
}

#' @export
apply_row_dfw.row_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                         .matrix_wise = TRUE,
                                         .input_list = FALSE,
                                         .force_name = FALSE)
{
  NextMethod()
}

#' @export
apply_row_dfw.col_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                         .matrix_wise = TRUE,
                                         .input_list = FALSE,
                                         .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  appl <- if (.matrix_wise) {
    eval_fun_margin_grp(.ms, mrg="row", var_lab=var_lab_row, ...,
                        matidx = .matrix, .simplify = TRUE,
                        env = rlang::caller_env())
  } else {
    eval_fun_margin_grp_mult(.ms, mrg="row", var_lab=var_lab_row, ...,
                             matidx = .matrix, as_list_mat = .input_list,
                             .simplify = TRUE, env = rlang::caller_env())
  }
  tblize_wd(appl, .rowtag(.ms), mult = !.matrix_wise, force_name = .force_name)
}

#' @export
apply_row_dfw.dual_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                          .matrix_wise = TRUE,
                                          .input_list = FALSE,
                                          .force_name = FALSE)
{
  apply_row_dfw.col_grouped_ms(.ms, ..., .matrix = .matrix,
                               .matrix_wise = .matrix_wise,
                               .input_list = .input_list)
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
    eval_fun_margin(.ms, mrg="col", var_lab=var_lab_col, ..., matidx = .matrix,
                    .simplify = FALSE, env = rlang::caller_env())
  } else {
    eval_fun_margin_mult(.ms, mrg="col", var_lab=var_lab_col, ...,
                         matidx = .matrix, as_list_mat = .input_list,
                         .simplify = FALSE, env = rlang::caller_env())
  }
}

#' @export
apply_column.row_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                        .matrix_wise = TRUE, .input_list = FALSE)
{
  if (.matrix_wise) {
    warn_if(.matrix_wise, .input_list)
    eval_fun_margin_grp(.ms, mrg="col", var_lab=var_lab_col, ...,
                        matidx = .matrix, .simplify = FALSE,
                        env = rlang::caller_env())
  } else {
    eval_fun_margin_grp_mult(.ms, mrg="col", var_lab=var_lab_col, ...,
                             matidx = .matrix, as_list_mat = .input_list,
                             .simplify = FALSE, env = rlang::caller_env())
  }
}

#' @export
apply_column.col_grouped_ms <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                                        .input_list = FALSE)
{
  NextMethod()
}

#' @export
apply_column.dual_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                         .matrix_wise = TRUE, .input_list = FALSE)
{
  apply_column.row_grouped_ms(.ms, ..., .matrix = .matrix,
                              .matrix_wise = .matrix_wise,
                              .input_list = .input_list)
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
    eval_fun_margin(.ms, mrg="col", var_lab=var_lab_col, ..., matidx = .matrix,
                    .simplify = TRUE, env = rlang::caller_env())
  } else {
    eval_fun_margin_mult(.ms, mrg="col", var_lab=var_lab_col, ...,
                         matidx = .matrix, as_list_mat = .input_list,
                         .simplify = TRUE, env = rlang::caller_env())
  }
  tblize_lg(appl, .coltag(.ms), mult = !.matrix_wise, force_name = .force_name)
}

#' @export
apply_column_dfl.row_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                            .matrix_wise = TRUE,
                                            .input_list = FALSE,
                                            .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  appl <- if (.matrix_wise) {
    eval_fun_margin_grp(.ms, mrg="col", var_lab=var_lab_col, ...,
                        matidx = .matrix, .simplify = TRUE,
                        env = rlang::caller_env())
  } else {
    eval_fun_margin_grp_mult(.ms, mrg="col", var_lab=var_lab_col, ...,
                             matidx = .matrix, as_list_mat = .input_list,
                             .simplify = TRUE, env = rlang::caller_env())
  }
  tblize_lg(appl, .coltag(.ms), mult = !.matrix_wise, force_name = .force_name)
}

#' @export
apply_column_dfl.col_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                            .matrix_wise = TRUE,
                                            .input_list = FALSE,
                                            .force_name = FALSE)
{
  NextMethod()
}

#' @export
apply_column_dfl.dual_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                             .matrix_wise = TRUE,
                                             .input_list = FALSE,
                                             .force_name = FALSE)
{
  apply_column_dfl.row_grouped_ms(.ms, ..., .matrix = .matrix,
                                  .matrix_wise = .matrix_wise,
                                  .input_list = .input_list)
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
    eval_fun_margin(.ms, mrg="col", var_lab=var_lab_col, ..., matidx = .matrix,
                    .simplify = TRUE, env = rlang::caller_env())
  } else {
    eval_fun_margin_mult(.ms, mrg="col", var_lab=var_lab_col, ...,
                         matidx = .matrix, as_list_mat = .input_list,
                         .simplify = TRUE, env = rlang::caller_env())
  }
  tblize_wd(appl, .coltag(.ms), mult = !.matrix_wise, force_name = .force_name)
}

#' @export
apply_column_dfw.row_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                            .matrix_wise = TRUE,
                                            .input_list = FALSE,
                                            .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  appl <- if (.matrix_wise) {
    eval_fun_margin_grp(.ms, mrg="col", var_lab=var_lab_col, ...,
                        matidx = .matrix, .simplify = TRUE,
                        env = rlang::caller_env())
  } else {
    eval_fun_margin_grp_mult(.ms, mrg="col", var_lab=var_lab_col, ...,
                             matidx = .matrix, as_list_mat = .input_list,
                             .simplify = TRUE, env = rlang::caller_env())
  }
  tblize_wd(appl, .coltag(.ms), mult = !.matrix_wise, force_name = .force_name)
}

#' @export
apply_column_dfw.col_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                            .matrix_wise = TRUE,
                                            .input_list = FALSE,
                                            .force_name = FALSE)
{
  NextMethod()
}

#' @export
apply_column_dfw.dual_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                             .matrix_wise = TRUE,
                                             .input_list = FALSE,
                                             .force_name = FALSE)
{
  apply_column_dfw.row_grouped_ms(.ms, ..., .matrix = .matrix,
                                  .matrix_wise = .matrix_wise,
                                  .input_list = .input_list)
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
    eval_fun_matrix(.ms, ..., matidx = .matrix, .simplify = FALSE,
                    env = rlang::caller_env())
  } else {
    eval_fun_matrix_mult(.ms, ..., matidx=.matrix, as_list_mat=.input_list,
                         .simplify=FALSE, env=rlang::caller_env())
  }
}


#' @export
apply_matrix.row_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                        .matrix_wise = TRUE, .input_list = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  if (.matrix_wise) {
    eval_fun_matrix_sgms(.ms, "row", ..., matidx = .matrix, .simplify = FALSE,
                         env = rlang::caller_env())

  } else {
    eval_fun_matrix_sgms_mult(.ms, "row", ..., matidx=.matrix,
                              as_list_mat=.input_list, .simplify=FALSE,
                              env=rlang::caller_env())
  }
}


#' @export
apply_matrix.col_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                        .matrix_wise = TRUE, .input_list = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  if (.matrix_wise) {
    eval_fun_matrix_sgms(.ms, "col", ..., matidx = .matrix, .simplify = FALSE,
                         env = rlang::caller_env())

  } else {
    eval_fun_matrix_sgms_mult(.ms, "col", ..., matidx=.matrix,
                              as_list_mat=.input_list, .simplify=FALSE,
                              env=rlang::caller_env())
  }
}


#' @export
apply_matrix.dual_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                        .matrix_wise = TRUE, .input_list = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  if (.matrix_wise) {
    eval_fun_matrix_dgms(.ms, ..., matidx = .matrix, .simplify = FALSE,
                         env = rlang::caller_env())

  } else {
    eval_fun_matrix_dgms_mult(.ms, ..., matidx=.matrix, as_list_mat=.input_list,
                              .simplify=FALSE, env=rlang::caller_env())
  }
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
    eval_fun_matrix(.ms, ..., matidx = .matrix, .simplify = TRUE,
                    env = rlang::caller_env())
  } else {
    eval_fun_matrix_mult(.ms, ..., matidx=.matrix, as_list_mat=.input_list,
                         .simplify=TRUE, env=rlang::caller_env())
  }
  tblize_lg(appl, "", matrix = TRUE, mult = !.matrix_wise,
            force_name = .force_name)
}


#' @export
apply_matrix_dfl.row_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                            .matrix_wise = TRUE,
                                            .input_list = FALSE,
                                            .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  appl <- if (.matrix_wise) {
    eval_fun_matrix_sgms(.ms, "row", ..., matidx = .matrix, .simplify = TRUE,
                         env = rlang::caller_env())

  } else {
    eval_fun_matrix_sgms_mult(.ms, "row", ..., matidx=.matrix,
                              as_list_mat=.input_list, .simplify=TRUE,
                              env=rlang::caller_env())
  }
  tblize_lg(appl, "", matrix = TRUE, mult = !.matrix_wise,
            force_name = .force_name)
}


#' @export
apply_matrix_dfl.col_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                            .matrix_wise = TRUE,
                                            .input_list = FALSE,
                                            .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  appl <- if (.matrix_wise) {
    eval_fun_matrix_sgms(.ms, "col", ..., matidx = .matrix, .simplify = TRUE,
                         env = rlang::caller_env())

  } else {
    eval_fun_matrix_sgms_mult(.ms, "col", ..., matidx=.matrix,
                              as_list_mat=.input_list, .simplify=TRUE,
                              env=rlang::caller_env())
  }
  tblize_lg(appl, "", matrix = TRUE, mult = !.matrix_wise,
            force_name = .force_name)
}


#' @export
apply_matrix_dfl.dual_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                             .matrix_wise = TRUE,
                                             .input_list = FALSE,
                                             .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  appl <- if (.matrix_wise) {
    eval_fun_matrix_dgms(.ms, ..., matidx = .matrix, .simplify = TRUE,
                         env = rlang::caller_env())

  } else {
    eval_fun_matrix_dgms_mult(.ms, ..., matidx=.matrix, as_list_mat=.input_list,
                              .simplify=TRUE, env=rlang::caller_env())
  }
  tblize_lg(appl, "", matrix = TRUE, mult = !.matrix_wise,
            force_name = .force_name)
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
    eval_fun_matrix(.ms, ..., matidx = .matrix, .simplify = TRUE,
                    env = rlang::caller_env())
  } else {
    eval_fun_matrix_mult(.ms, ..., matidx=.matrix, as_list_mat=.input_list,
                         .simplify=TRUE, env=rlang::caller_env())
  }
  tblize_wd(appl, "", matrix = TRUE, mult = !.matrix_wise,
            force_name = .force_name)
}


#' @export
apply_matrix_dfw.row_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                            .matrix_wise = TRUE,
                                            .input_list = FALSE,
                                            .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  appl <- if (.matrix_wise) {
    eval_fun_matrix_sgms(.ms, "row", ..., matidx = .matrix, .simplify = TRUE,
                         env = rlang::caller_env())

  } else {
    eval_fun_matrix_sgms_mult(.ms, "row", ..., matidx=.matrix,
                              as_list_mat=.input_list, .simplify=TRUE,
                              env=rlang::caller_env())
  }
  tblize_wd(appl, "", matrix = TRUE, mult = !.matrix_wise,
            force_name = .force_name)
}


#' @export
apply_matrix_dfw.col_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                            .matrix_wise = TRUE,
                                            .input_list = FALSE,
                                            .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  appl <- if (.matrix_wise) {
    eval_fun_matrix_sgms(.ms, "col", ..., matidx = .matrix, .simplify = TRUE,
                         env = rlang::caller_env())

  } else {
    eval_fun_matrix_sgms_mult(.ms, "col", ..., matidx=.matrix,
                              as_list_mat=.input_list, .simplify=TRUE,
                              env=rlang::caller_env())
  }
  tblize_wd(appl, "", matrix = TRUE, mult = !.matrix_wise,
            force_name = .force_name)
}


#' @export
apply_matrix_dfw.dual_grouped_ms <- function(.ms, ..., .matrix = NULL,
                                             .matrix_wise = TRUE,
                                             .input_list = FALSE,
                                             .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  appl <- if (.matrix_wise) {
    eval_fun_matrix_dgms(.ms, ..., matidx = .matrix, .simplify = TRUE,
                         env = rlang::caller_env())

  } else {
    eval_fun_matrix_dgms_mult(.ms, ..., matidx=.matrix, as_list_mat=.input_list,
                              .simplify=TRUE, env=rlang::caller_env())
  }
  tblize_wd(appl, "", matrix = TRUE, mult = !.matrix_wise,
            force_name = .force_name)
}







