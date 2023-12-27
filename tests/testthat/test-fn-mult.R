test_that("matrixset general loop works", {
#
  expect_identical(apply_row(matrixset(NULL), mean, .matrix_wise = FALSE), NULL)
  expect_identical(apply_column(matrixset(NULL), mean, .matrix_wise = FALSE), NULL)


  expect_error(apply_row(student_results, list, .matrix_wise = FALSE),
               "object '\\.i' not found")


  lst <- apply_row(student_results, list, .matrix_wise = FALSE, .input_list = TRUE)
  lst_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
   list(list=list(M))
  })

  expect_equal(lst, lst_ref)


  lst <- apply_row(student_results, FC = ~ .i[[2]]/.i[[1]], .matrix_wise = FALSE, .input_list = TRUE)
  lst_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    list(FC=M[[2]]/M[[1]])
  })

  expect_equal(lst, lst_ref)



  expect_error(apply_column(student_results, list, .matrix_wise = FALSE),
               "object '\\.j' not found")

  expect_error(apply_column_dfl(student_results, .colname = ~ mean, .matrix_wise = FALSE))
  expect_error(apply_column_dfw(student_results, .colname = ~ mean, .matrix_wise = FALSE))
  expect_error(apply_row_dfl(student_results, .rowname = ~ mean, .matrix_wise = FALSE))
  expect_error(apply_row_dfw(student_results, .rowname = ~ mean, .matrix_wise = FALSE))


  expect_warning(apply_matrix(student_results, mean, .input_list = TRUE),
                 "`\\.input_list` is TRUE but so is `\\.matrix_wise`\\. `\\.input_list` will be ignored\\.")
  expect_warning(apply_row(student_results, mean, .input_list = TRUE),
                 "`\\.input_list` is TRUE but so is `\\.matrix_wise`\\. `\\.input_list` will be ignored\\.")
  expect_warning(apply_column(student_results, mean, .input_list = TRUE),
                 "`\\.input_list` is TRUE but so is `\\.matrix_wise`\\. `\\.input_list` will be ignored\\.")
  expect_warning(apply_matrix_dfl(student_results, mean, .input_list = TRUE),
                 "`\\.input_list` is TRUE but so is `\\.matrix_wise`\\. `\\.input_list` will be ignored\\.")
  expect_warning(apply_row_dfl(student_results, mean, .input_list = TRUE),
                 "`\\.input_list` is TRUE but so is `\\.matrix_wise`\\. `\\.input_list` will be ignored\\.")
  expect_warning(apply_column_dfl(student_results, mean, .input_list = TRUE),
                 "`\\.input_list` is TRUE but so is `\\.matrix_wise`\\. `\\.input_list` will be ignored\\.")
  expect_warning(apply_matrix_dfw(student_results, mean, .input_list = TRUE),
                 "`\\.input_list` is TRUE but so is `\\.matrix_wise`\\. `\\.input_list` will be ignored\\.")
  expect_warning(apply_row_dfw(student_results, mean, .input_list = TRUE),
                 "`\\.input_list` is TRUE but so is `\\.matrix_wise`\\. `\\.input_list` will be ignored\\.")
  expect_warning(apply_column_dfw(student_results, mean, .input_list = TRUE),
                 "`\\.input_list` is TRUE but so is `\\.matrix_wise`\\. `\\.input_list` will be ignored\\.")




  lst <- apply_column(student_results, list, .matrix_wise = FALSE, .input_list = TRUE)
  lst_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    list(list=list(M))
  })

  expect_equal(lst, lst_ref)


  lst <- apply_column(student_results, FC = ~ purrr::map(.j, ~ .x/.j[[1]]), .matrix_wise = FALSE, .input_list = TRUE)
  lst_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    list(FC=purrr::map(M, ~ .x/M[[1]]))
  })

  expect_equal(lst, lst_ref)




  ctr <- function(..., fun) {
    x <- list(...)
    apply(do.call(rbind, x), 2, fun)
  }
  ct <- apply_row(student_results, mn=ctr(.i1, .i2, fun = mean), md=ctr(.i1, .i2, fun = median), .matrix_wise = FALSE)
  ct_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- unname(M)
    list(mn=ctr(M[[1]], M[[2]], fun = mean), md=ctr(M[[1]], M[[2]], fun = median))
  })

  expect_equal(ct, ct_ref)


  js <- rlang::syms(paste0(".j", 1:2))
  ct <- apply_column(student_results, mn=ctr(!!!js, fun = mean), md=ctr(!!!js, fun = median), .matrix_wise = FALSE)
  ct_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- unname(M)
    M <- lapply(M, function(m) m[,1])
    list(mn=ctr(M[[1]], M[[2]], fun = mean), md=ctr(M[[1]], M[[2]], fun = median))
  })

  expect_equal(ct, ct_ref)



  ctr_dot <- function(..., fun) {
    x <- list(...)
    apply(do.call(rbind, x), 2, fun)
  }
  ctr2 <- function(l, fun) rlang::eval_tidy(rlang::quo(ctr_dot(!!!l, fun = fun)))
  ct <- apply_row(student_results, mn=ctr2(.i, fun = mean), md=ctr2(.i, fun = median), .matrix_wise = FALSE, .input_list = TRUE)
  ct_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- unname(M)
    list(mn=ctr(M[[1]], M[[2]], fun = mean), md=ctr(M[[1]], M[[2]], fun = median))
  })

  expect_equal(ct, ct_ref)



  ct <- apply_column(student_results, mn=ctr2(.j, fun = mean), md=ctr2(.j, fun = median), .matrix_wise = FALSE, .input_list = TRUE)
  ct_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- unname(M)
    M <- lapply(M, function(m) m[,1])
    list(mn=ctr(M[[1]], M[[2]], fun = mean), md=ctr(M[[1]], M[[2]], fun = median))
  })

  expect_equal(ct, ct_ref)




  rg <- apply_row(student_results, reg = lm(.i2/.i1 ~ program), .matrix_wise = FALSE)
  rg_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    meta <- column_info(student_results)
    meta <- tibble::column_to_rownames(meta, ".colname")
    meta$.i1 <- M[[1]]
    meta$.i2 <- M[[2]]
    list(reg=eval(quote(lm(.i2/.i1 ~ program)), envir = meta))
  })

  expect_equal(rg, rg_ref, ignore_attr = TRUE)



  rg <- apply_column(student_results, reg = lm(.j2/.j1 ~ teacher+class), .matrix_wise = FALSE)
  rg_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    meta <- row_info(student_results)
    meta <- tibble::column_to_rownames(meta, ".rowname")
    meta$.j1 <- M[[1]]
    meta$.j2 <- M[[2]]
    list(reg=eval(quote(lm(.j2/.j1 ~ teacher+class)), envir = meta))
  })

  expect_equal(rg, rg_ref, ignore_attr = TRUE)




  rg <- apply_row(student_results, reg = lm(.i1 ~ program), .matrix = 2, .matrix_wise = FALSE)
  rg_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- M[[2]][1,]
    M <- unname(M)
    meta <- column_info(student_results)
    meta <- tibble::column_to_rownames(meta, ".colname")
    meta$.i1 <- M
    list(reg=eval(quote(lm(.i1 ~ program)), envir = meta))
  })

  expect_equal(rg, rg_ref, ignore_attr = TRUE)



  rg <- apply_column(student_results, reg = lm(.j1 ~ teacher+class), .matrix = 2, .matrix_wise = FALSE)
  rg_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- M[[2]][,1]
    M <- unname(M)
    meta <- row_info(student_results)
    meta <- tibble::column_to_rownames(meta, ".rowname")
    meta$.j1 <- M
    list(reg=eval(quote(lm(.j1 ~ teacher+class)), envir = meta))
  })

  expect_equal(rg, rg_ref, ignore_attr = TRUE)





#
#   # .data
  rg <- apply_row(student_results, reg = coef(lm(.i2/.i1 ~ .data[["program"]])), .matrix_wise = FALSE)
  rg_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    meta <- column_info(student_results)
    meta <- tibble::column_to_rownames(meta, ".colname")
    meta$.i1 <- M[[1]]
    meta$.i2 <- M[[2]]
    list(reg=eval(quote(coef(lm(.i2/.i1 ~ program))), envir = meta))
  })

  expect_equal(rg, rg_ref, ignore_attr = TRUE)



  rg <- apply_column(student_results, reg = coef(lm(.j2/.j1 ~ .data[["teacher"]]+class)), .matrix_wise = FALSE)
  rg_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    meta <- row_info(student_results)
    meta <- tibble::column_to_rownames(meta, ".rowname")
    meta$.j1 <- M[[1]]
    meta$.j2 <- M[[2]]
    list(reg=eval(quote(coef(lm(.j2/.j1 ~ teacher+class))), envir = meta))
  })

  expect_equal(rg, rg_ref, ignore_attr = TRUE)




  program <- 0.5
  rg <- apply_row(student_results, reg = .i2 - .i1 - .env[["program"]], .matrix_wise = FALSE)
  rg_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    list(reg=M[[2]] - M[[1]] - 0.5)
  })

  expect_equal(rg, rg_ref, ignore_attr = TRUE)



  teacher <- 0.5
  rg <- apply_column(student_results, reg = .j2 - .j1 - .env[["teacher"]], .matrix_wise = FALSE)
  rg_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    list(reg=M[[2]] - M[[1]] - 0.5)
  })

  expect_equal(rg, rg_ref, ignore_attr = TRUE)







  expect_error(apply_row(column_group_by(student_results, program), list,
                         .matrix_wise = FALSE),
               "object '\\.i' not found")


  lst <- apply_row(column_group_by(student_results, program), list,
                   .matrix_wise = FALSE, .input_list = TRUE)
  grs <- column_group_meta(column_group_by(student_results, program))
  ans <- grs
  ans$.rows <- NULL
  lst_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
      M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) {M <- m[1,]; names(M) <- colnames(m); M})
      list(list=list(M))
    })
  })
  ans$.vals <- lst_ref

  expect_equal(lst, ans)


  lst <- apply_row(column_group_by(student_results, program), FC = ~ .i[[2]]/.i[[1]], .matrix_wise = FALSE, .input_list = TRUE)
  grs <- column_group_meta(column_group_by(student_results, program))
  ans <- grs
  ans$.rows <- NULL
  lst_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
      M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) {M <- m[1,]; names(M) <- colnames(m); M})
      M <- unname(M)
      list(FC=M[[2]]/M[[1]])
    })
  })
  ans$.vals <- lst_ref

  expect_equal(lst, ans)



  lst <- apply_column(row_group_by(student_results, teacher, class), list,
                      .matrix_wise = FALSE, .input_list = TRUE)
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  ans <- grs
  ans$.rows <- NULL
  lst_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[,1])
      list(list=list(M))
    })
  })
  ans$.vals <- lst_ref

  expect_equal(lst, ans)


  lst <- apply_column(row_group_by(student_results, teacher, class),
                      FC = ~ purrr::map(.j, ~ .x/.j[[1]]), .matrix_wise = FALSE,
                      .input_list = TRUE)
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  ans <- grs
  ans$.rows <- NULL
  lst_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[,1])
      list(FC=purrr::map(M, ~ .x/M[[1]]))
    })
  })
  ans$.vals <- lst_ref

  expect_equal(lst, ans)




  ctr <- function(..., fun) {
    x <- list(...)
    apply(do.call(rbind, x), 2, fun)
  }
  ct <- apply_row(column_group_by(student_results, program), mn=ctr(.i1, .i2, fun = mean), md=ctr(.i1, .i2, fun = median), .matrix_wise = FALSE)
  grs <- column_group_meta(column_group_by(student_results, program))
  ans <- grs
  ans$.rows <- NULL
  ct_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
      M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- unname(M)
      M <- lapply(M, function(m) {M <- m[1,]; names(M) <- colnames(m); M})
      list(mn=ctr(M[[1]], M[[2]], fun = mean), md=ctr(M[[1]], M[[2]], fun = median))
    })
  })
  ans$.vals <- ct_ref

  expect_equal(ct, ans)



  js <- rlang::syms(paste0(".j", 1:2))
  ct <- apply_column(row_group_by(student_results, teacher, class), mn=ctr(!!!js, fun = mean), md=ctr(!!!js, fun = median), .matrix_wise = FALSE)
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  ans <- grs
  ans$.rows <- NULL
  ct_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- unname(M)
      M <- lapply(M, function(m) m[,1])
      list(mn=ctr(M[[1]], M[[2]], fun = mean), md=ctr(M[[1]], M[[2]], fun = median))
    })
  })
  ans$.vals <- ct_ref

  expect_equal(ct, ans)




  ctr_dot <- function(..., fun) {
    x <- list(...)
    apply(do.call(rbind, x), 2, fun)
  }
  ctr2 <- function(l, fun) rlang::eval_tidy(rlang::quo(ctr_dot(!!!l, fun = fun)))
  ct <- apply_row(column_group_by(student_results, program), mn=ctr2(.i, fun = mean), md=ctr2(.i, fun = median), .matrix_wise = FALSE, .input_list = TRUE)
  grs <- column_group_meta(column_group_by(student_results, program))
  ans <- grs
  ans$.rows <- NULL
  ct_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
      M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- unname(M)
      M <- lapply(M, function(m) {M <- m[1,]; names(M) <- colnames(m); M})
      list(mn=ctr(M[[1]], M[[2]], fun = mean), md=ctr(M[[1]], M[[2]], fun = median))
    })
  })
  ans$.vals <- ct_ref

  expect_equal(ct, ans)



  ct <- apply_column(row_group_by(student_results, teacher, class), mn=ctr2(.j, fun = mean), md=ctr2(.j, fun = median), .matrix_wise = FALSE, .input_list = TRUE)
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  ans <- grs
  ans$.rows <- NULL
  ct_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- unname(M)
      M <- lapply(M, function(m) m[,1])
      list(mn=ctr(M[[1]], M[[2]], fun = mean), md=ctr(M[[1]], M[[2]], fun = median))
    })
  })
  ans$.vals <- ct_ref

  expect_equal(ct, ans)




  rg <- apply_row(column_group_by(student_results, program), reg = lm(.i2/.i1 ~ school_average), .matrix_wise = FALSE)
  grs <- column_group_meta(column_group_by(student_results, program))
  ans <- grs
  ans$.rows <- NULL
  rg_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
      M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[1,])
      M <- unname(M)
      meta <- column_info(student_results)
      meta <- tibble::column_to_rownames(meta, ".colname")
      meta <- dplyr::slice(meta, gr)
      meta$.i1 <- M[[1]]
      meta$.i2 <- M[[2]]
      list(reg=eval(quote(lm(.i2/.i1 ~ school_average)), envir = meta))
    })
  })
  ans$.vals <- rg_ref

  expect_equal(rg, ans, ignore_attr = TRUE)



  rg <- apply_column(row_group_by(student_results, teacher), reg = lm(.j2/.j1 ~ class), .matrix_wise = FALSE)
  grs <- row_group_meta(row_group_by(student_results, teacher))
  ans <- grs
  ans$.rows <- NULL
  rg_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[,1])
      M <- unname(M)
      meta <- row_info(student_results)
      meta <- tibble::column_to_rownames(meta, ".rowname")
      meta <- dplyr::slice(meta, gr)
      meta$.j1 <- M[[1]]
      meta$.j2 <- M[[2]]
      list(reg=eval(quote(lm(.j2/.j1 ~ class)), envir = meta))
    })
  })
  ans$.vals <- rg_ref

  expect_equal(rg, ans, ignore_attr = TRUE)




  ct <- apply_row(row_group_by(column_group_by(student_results, program), teacher),
                  mn=ctr2(.i, fun = mean), md=ctr2(.i, fun = median),
                  .matrix_wise = FALSE, .input_list = TRUE)
  grs <- column_group_meta(column_group_by(student_results, program))
  ans <- grs
  ans$.rows <- NULL
  ct_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
      M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- unname(M)
      M <- lapply(M, function(m) {M <- m[1,]; names(M) <- colnames(m); M})
      list(mn=ctr(M[[1]], M[[2]], fun = mean), md=ctr(M[[1]], M[[2]], fun = median))
    })
  })
  ans$.vals <- ct_ref

  expect_equal(ct, ans)



  ct <- apply_column(row_group_by(column_group_by(student_results, program), teacher),
                     mn=ctr2(.j, fun = mean), md=ctr2(.j, fun = median),
                     .matrix_wise = FALSE, .input_list = TRUE)
  grs <- row_group_meta(row_group_by(student_results, teacher))
  ans <- grs
  ans$.rows <- NULL
  ct_ref <- lapply(grs$.rows, function(gr) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- unname(M)
      M <- lapply(M, function(m) m[,1])
      list(mn=ctr(M[[1]], M[[2]], fun = mean), md=ctr(M[[1]], M[[2]], fun = median))
    })
  })
  ans$.vals <- ct_ref

  expect_equal(ct, ans)



})





test_that("matrixset 'long' loop works", {

  # with null
  expect_identical(apply_row_dfl(matrixset(NULL), mean, .matrix_wise = FALSE), NULL)
  expect_identical(apply_column_dfl(matrixset(NULL), mean, .matrix_wise = FALSE), NULL)




  fc <- apply_row_dfl(student_results, ~.i2/.i1, .matrix_wise = FALSE)
  fc_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    tibble::enframe(M[[2]]/M[[1]], name = "~.i2/.i1.name", value = "~.i2/.i1")
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".rowname")

  expect_equal(fc, fc_ref)


  fc <- apply_row_dfl(student_results, FC = ~ .i[[2]]/.i[[1]], .matrix_wise = FALSE, .input_list = TRUE)
  fc_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    tibble::enframe(M[[2]]/M[[1]], name = "FC.name", value = "FC")
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".rowname")

  expect_equal(fc, fc_ref)



  fc <- apply_column_dfl(student_results, ~.j2/.j1, .matrix_wise = FALSE)
  fc_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    tibble::enframe(M[[2]]/M[[1]], name = "~.j2/.j1.name", value = "~.j2/.j1")
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".colname")

  expect_equal(fc, fc_ref)


  fc <- apply_column_dfl(student_results, FC = ~ .j[[2]]/.j[[1]], .matrix_wise = FALSE, .input_list = TRUE)
  fc_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    tibble::enframe(M[[2]]/M[[1]], name = "FC.name", value = "FC")
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".colname")

  expect_equal(fc, fc_ref)



  fc <- apply_row_dfl(student_results, ~.i2/.i1, ~ log2(.i2/.i1), .matrix_wise = FALSE)
  fc_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    d1 <- tibble::enframe(M[[2]]/M[[1]], name = "~.i2/.i1.name", value = "~.i2/.i1")
    d2 <- tibble::enframe(log2(M[[2]]/M[[1]]), name = "~log2(.i2/.i1).name", value = "~log2(.i2/.i1)")
    dplyr::bind_cols(d1, d2)
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".rowname")

  expect_equal(fc, fc_ref)


  fc <- apply_row_dfl(student_results, FC = ~ .i[[2]]/.i[[1]], logFC = ~ log2(.i[[2]]/.i[[1]]), .matrix_wise = FALSE, .input_list = TRUE)
  fc_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    d1 <- tibble::enframe(M[[2]]/M[[1]], name = "FC.name", value = "FC")
    d2 <- tibble::enframe(log2(M[[2]]/M[[1]]), name = "logFC.name", value = "logFC")
    dplyr::bind_cols(d1, d2)
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".rowname")

  expect_equal(fc, fc_ref)



  fc <- apply_column_dfl(student_results, ~.j2/.j1, ~log2(.j2/.j1), .matrix_wise = FALSE)
  fc_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    d1 <- tibble::enframe(M[[2]]/M[[1]], name = "~.j2/.j1.name", value = "~.j2/.j1")
    d2 <- tibble::enframe(log2(M[[2]]/M[[1]]), name = "~log2(.j2/.j1).name", value = "~log2(.j2/.j1)")
    dplyr::bind_cols(d1, d2)
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".colname")

  expect_equal(fc, fc_ref)


  fc <- apply_column_dfl(student_results, FC = ~ .j[[2]]/.j[[1]], logFC = ~ log2(.j[[2]]/.j[[1]]), .matrix_wise = FALSE, .input_list = TRUE)
  fc_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    d1 <- tibble::enframe(M[[2]]/M[[1]], name = "FC.name", value = "FC")
    d2 <- tibble::enframe(log2(M[[2]]/M[[1]]), name = "logFC.name", value = "logFC")
    dplyr::bind_cols(d1, d2)
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".colname")

  expect_equal(fc, fc_ref)



  fc <- apply_row_dfl(student_results, FC = ~ mean(.i2/.i1), logFC = ~ mean(log2(.i2/.i1)), .matrix_wise = FALSE)
  fc_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    d1 <- tibble::enframe(mean(M[[2]]/M[[1]]), name = NULL, value = "FC")
    d2 <- tibble::enframe(mean(log2(M[[2]]/M[[1]])), name = NULL, value = "logFC")
    dplyr::bind_cols(d1, d2)
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".rowname")

  expect_equal(fc, fc_ref)



  fc <- apply_column_dfl(student_results, FC = ~ mean(.j2/.j1), logFC = ~ mean(log2(.j2/.j1)), .matrix_wise = FALSE)
  fc_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    d1 <- tibble::enframe(mean(M[[2]]/M[[1]]), name = NULL, value = "FC")
    d2 <- tibble::enframe(mean(log2(M[[2]]/M[[1]])), name = NULL, value = "logFC")
    dplyr::bind_cols(d1, d2)
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".colname")

  expect_equal(fc, fc_ref)



  # grouped
  # grfc <- apply_row_dfl(column_group_by(student_results, program), ~ .i2/.i1,
  #                       .matrix_wise = FALSE, .force_name = TRUE)
  # grs <- column_group_meta(column_group_by(student_results, program))
  # fc_ref <- grs
  # fc_ref_tmp <- lapply(grs$.rows, function(gr) {
  #   tmp <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
  #     M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
  #     M <- lapply(M, function(m) m[1,])
  #     M <- unname(M)
  #     tbl <- tibble::enframe(M[[2]]/M[[1]], name = "~.i2/.i1.name", value = "~.i2/.i1")
  #     tbl[["~.i2/.i1.name"]] <- ifelse(as.character(tbl[["~.i2/.i1.name"]]) == "1", "English", tbl[["~.i2/.i1.name"]])
  #     tbl
  #   })
  #   dplyr::bind_rows(tmp, .id = ".rowname")
  # })
  # fc_ref$.rows <- NULL
  # fc_ref$.vals <- fc_ref_tmp
  # fc_ref <- tidyr::unnest(fc_ref, cols=c(.vals))
  #
  # expect_equal(grfc, fc_ref)




  grfc <- apply_column_dfl(row_group_by(student_results, class), ~ .j2/.j1, .matrix_wise = FALSE)
  grs <- row_group_meta(row_group_by(student_results, class))
  fc_ref <- grs
  fc_ref_tmp <- lapply(grs$.rows, function(gr) {
    tmp <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr, cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[,1])
      M <- unname(M)
      tbl <- tibble::enframe(M[[2]]/M[[1]], name = "~.j2/.j1.name", value = "~.j2/.j1")
      tbl[["~.j2/.j1.name"]] <- ifelse(as.character(tbl[["~.j2/.j1.name"]]) == "1", NA_character_, tbl[["~.j2/.j1.name"]])
      tbl
    })
    dplyr::bind_rows(tmp, .id = ".colname")
  })
  fc_ref$.rows <- NULL
  fc_ref$.vals <- fc_ref_tmp
  fc_ref <- tidyr::unnest(fc_ref, cols=c(.vals))

  expect_equal(grfc, fc_ref)



  grfc <- apply_row_dfl(column_group_by(student_results, program), mean_FC = ~ mean(.i2/.i1), .matrix_wise = FALSE)
  grs <- column_group_meta(column_group_by(student_results, program))
  fc_ref <- grs
  fc_ref_tmp <- lapply(grs$.rows, function(gr) {
    tmp <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
      M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[1,])
      M <- unname(M)
      tbl <- tibble::enframe(mean(M[[2]]/M[[1]]), name = NULL, value = "mean_FC")
      tbl
    })
    dplyr::bind_rows(tmp, .id = ".rowname")
  })
  fc_ref$.rows <- NULL
  fc_ref$.vals <- fc_ref_tmp
  fc_ref <- tidyr::unnest(fc_ref, cols=c(.vals))

  expect_equal(grfc, fc_ref)



  grfc <- apply_column_dfl(row_group_by(student_results, class), mean_FC = ~ mean(.j2/.j1), .matrix_wise = FALSE)
  grs <- row_group_meta(row_group_by(student_results, class))
  fc_ref <- grs
  fc_ref_tmp <- lapply(grs$.rows, function(gr) {
    tmp <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr, cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[,1])
      M <- unname(M)
      tbl <- tibble::enframe(mean(M[[2]]/M[[1]]), name = NULL, value = "mean_FC")
      tbl
    })
    dplyr::bind_rows(tmp, .id = ".colname")
  })
  fc_ref$.rows <- NULL
  fc_ref$.vals <- fc_ref_tmp
  fc_ref <- tidyr::unnest(fc_ref, cols=c(.vals))

  expect_equal(grfc, fc_ref)



  grfc <- apply_row_dfl(column_group_by(student_results, program), mean_FC = ~ mean(.i2/.i1), min_FC = ~ min(.i2/.i1), .matrix_wise = FALSE)
  grs <- column_group_meta(column_group_by(student_results, program))
  fc_ref <- grs
  fc_ref_tmp <- lapply(grs$.rows, function(gr) {
    tmp <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
      M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[1,])
      M <- unname(M)
      tbl <- tibble::tibble(mean_FC=mean(M[[2]]/M[[1]]),
                            min_FC=min(M[[2]]/M[[1]]))
      tbl
    })
    dplyr::bind_rows(tmp, .id = ".rowname")
  })
  fc_ref$.rows <- NULL
  fc_ref$.vals <- fc_ref_tmp
  fc_ref <- tidyr::unnest(fc_ref, cols=c(.vals))

  expect_equal(grfc, fc_ref)



  grfc <- apply_column_dfl(row_group_by(student_results, class), mean_FC = ~ mean(.j2/.j1), min_FC = ~ min(.j2/.j1), .matrix_wise = FALSE)
  grs <- row_group_meta(row_group_by(student_results, class))
  fc_ref <- grs
  fc_ref_tmp <- lapply(grs$.rows, function(gr) {
    tmp <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr, cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[,1])
      M <- unname(M)
      tbl <- tibble::tibble(mean_FC=mean(M[[2]]/M[[1]]),
                            min_FC=min(M[[2]]/M[[1]]))
      tbl
    })
    dplyr::bind_rows(tmp, .id = ".colname")
  })
  fc_ref$.rows <- NULL
  fc_ref$.vals <- fc_ref_tmp
  fc_ref <- tidyr::unnest(fc_ref, cols=c(.vals))

  expect_equal(grfc, fc_ref)




  grfc <- apply_row_dfl(row_group_by(column_group_by(student_results, program), teacher),
                        mean_FC = ~ mean(.i2/.i1), min_FC = ~ min(.i2/.i1),
                        .matrix_wise = FALSE)
  grs <- column_group_meta(column_group_by(student_results, program))
  fc_ref <- grs
  fc_ref_tmp <- lapply(grs$.rows, function(gr) {
    tmp <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
      M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[1,])
      M <- unname(M)
      tbl <- tibble::tibble(mean_FC=mean(M[[2]]/M[[1]]),
                            min_FC=min(M[[2]]/M[[1]]))
      tbl
    })
    dplyr::bind_rows(tmp, .id = ".rowname")
  })
  fc_ref$.rows <- NULL
  fc_ref$.vals <- fc_ref_tmp
  fc_ref <- tidyr::unnest(fc_ref, cols=c(.vals))

  expect_equal(grfc, fc_ref)



  grfc <- apply_column_dfl(column_group_by(row_group_by(student_results, class), program),
                           mean_FC = ~ mean(.j2/.j1),
                           min_FC = ~ min(.j2/.j1), .matrix_wise = FALSE)
  grs <- row_group_meta(row_group_by(student_results, class))
  fc_ref <- grs
  fc_ref_tmp <- lapply(grs$.rows, function(gr) {
    tmp <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr, cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[,1])
      M <- unname(M)
      tbl <- tibble::tibble(mean_FC=mean(M[[2]]/M[[1]]),
                            min_FC=min(M[[2]]/M[[1]]))
      tbl
    })
    dplyr::bind_rows(tmp, .id = ".colname")
  })
  fc_ref$.rows <- NULL
  fc_ref$.vals <- fc_ref_tmp
  fc_ref <- tidyr::unnest(fc_ref, cols=c(.vals))

  expect_equal(grfc, fc_ref)

})








test_that("matrixset 'wide' loop works", {

  # with null
  expect_identical(apply_row_dfw(matrixset(NULL), mean, .matrix_wise = FALSE), NULL)
  expect_identical(apply_column_dfw(matrixset(NULL), mean, .matrix_wise = FALSE), NULL)




  fc <- apply_row_dfw(student_results, FC = ~ .i2/.i1, logFC = log2(.i2/.i1), .matrix_wise = FALSE)
  fc_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    tbl <- tibble::tibble(.colname = colnames(student_results),
                          FC=unname(M[[2]]/M[[1]]), logFC=unname(log2(M[[2]]/M[[1]])))
    tidyr::pivot_wider(tbl, names_from = ".colname", values_from = c("FC", "logFC"), names_sep = " ")
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".rowname")

  expect_equal(fc, fc_ref)


  fc <- apply_row_dfw(student_results, FC = ~ .i[[2]]/.i[[1]], logFC = log2(.i[[2]]/.i[[1]]), .matrix_wise = FALSE, .input_list = TRUE)
  fc_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    tbl <- tibble::tibble(.colname = colnames(student_results),
                          FC=unname(M[[2]]/M[[1]]), logFC=unname(log2(M[[2]]/M[[1]])))
    tidyr::pivot_wider(tbl, names_from = ".colname", values_from = c("FC", "logFC"), names_sep = " ")
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".rowname")

  expect_equal(fc, fc_ref)


  fc <- apply_row_dfw(student_results, FC = ~ mean(.i2/.i1), logFC = mean(log2(.i2/.i1)), .matrix_wise = FALSE)
  fc_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    tbl <- tibble::tibble(FC=mean(M[[2]]/M[[1]]), logFC=mean(log2(M[[2]]/M[[1]])))
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".rowname")

  expect_equal(fc, fc_ref)


  fc <- apply_column_dfw(student_results, FC = ~ .j2/.j1, logFC = log2(.j2/.j1), .matrix_wise = FALSE)
  fc_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    tbl <- tibble::tibble(.rowname = rownames(student_results),
                          FC=unname(M[[2]]/M[[1]]), logFC=unname(log2(M[[2]]/M[[1]])))
    tidyr::pivot_wider(tbl, names_from = ".rowname", values_from = c("FC", "logFC"), names_sep = " ")
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".colname")

  expect_equal(fc, fc_ref)


  fc <- apply_column_dfw(student_results, FC = ~ .j[[2]]/.j[[1]], logFC = log2(.j[[2]]/.j[[1]]), .matrix_wise = FALSE, .input_list = TRUE)
  fc_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    tbl <- tibble::tibble(.rowname = rownames(student_results),
                          FC=unname(M[[2]]/M[[1]]), logFC=unname(log2(M[[2]]/M[[1]])))
    tidyr::pivot_wider(tbl, names_from = ".rowname", values_from = c("FC", "logFC"), names_sep = " ")
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".colname")

  expect_equal(fc, fc_ref)


  fc <- apply_column_dfw(student_results, FC = ~ mean(.j2/.j1), logFC = mean(log2(.j2/.j1)), .matrix_wise = FALSE)
  fc_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    tbl <- tibble::tibble(FC=mean(M[[2]]/M[[1]]), logFC=mean(log2(M[[2]]/M[[1]])))
  })
  fc_ref <- dplyr::bind_rows(fc_ref, .id = ".colname")

  expect_equal(fc, fc_ref)



  # showcase > 1 length answer
  summ <- apply_row_dfw(student_results, qtl = ~ quantile(.i2/.i1, prob = c(.25,.75)), rg = range(.i2/.i1), .matrix_wise = FALSE)
  summ_ref <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
    M <- student_results[r,,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[1,])
    M <- unname(M)
    tbl <- c(quantile(M[[2]]/M[[1]], prob = c(.25, .75)), range(M[[2]]/M[[1]]))
    names(tbl)[1:2] <- paste("qtl", names(tbl)[1:2])
    names(tbl)[3:4] <- c("1", "2")
    tbl <- tibble::as_tibble_row(tbl)
    tbl
  })
  summ_ref <- dplyr::bind_rows(summ_ref, .id = ".rowname")
  colnames(summ_ref)[4:5] <- c("rg ..1", "rg ..2")

  expect_equal(summ, summ_ref)



  summ <- apply_column_dfw(student_results, qtl = ~ quantile(.j2/.j1, prob = c(.25,.75)), rg = range(.j2/.j1), .matrix_wise = FALSE)
  summ_ref <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
    M <- student_results[,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
    M <- lapply(M, function(m) m[,1])
    M <- unname(M)
    tbl <- c(quantile(M[[2]]/M[[1]], prob = c(.25, .75)), range(M[[2]]/M[[1]]))
    names(tbl)[1:2] <- paste("qtl", names(tbl)[1:2])
    names(tbl)[3:4] <- c("1", "2")
    tbl <- tibble::as_tibble_row(tbl)
    tbl
  })
  summ_ref <- dplyr::bind_rows(summ_ref, .id = ".colname")
  colnames(summ_ref)[4:5] <- c("rg ..1", "rg ..2")

  expect_equal(summ, summ_ref)



  # error
  expect_error(apply_row_dfw(student_results, ~mean(.i2/.i1), ~range(.i2/.i1),.matrix_wise = FALSE),
               "vectors must be of the same length")


  expect_error(apply_column_dfw(student_results, ~mean(.j2/.j1), ~range(.j2/.j1),.matrix_wise = FALSE),
               "vectors must be of the same length")







  # grouped
  grfc <- apply_row_dfw(column_group_by(student_results, program), FC_mean = ~mean(.i2/.i1), ~min(.i2/.i1), .matrix_wise = FALSE)
  grs <- column_group_meta(column_group_by(student_results, program))
  fc_ref_tmp <- lapply(grs$.rows, function(gr) {
    tmp <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
      M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[1,])
      M <- unname(M)
      tbl <- tibble::tibble(mean(M[[2]]/M[[1]]),
                            min(M[[2]]/M[[1]]))
      colnames(tbl) <- paste0(c("FC_mean ", "~min(.i2/.i1) "), dots_for_names, "1")
      tbl
    })
    dplyr::bind_rows(tmp, .id = ".rowname")
  })
  names(fc_ref_tmp) <- grs$program
  fc_ref <- dplyr::bind_rows(fc_ref_tmp, .id = "program")
  colnames(fc_ref) <- gsub(paste0(" ", dots_for_names_expr, "1"), "", colnames(fc_ref))
  expect_identical(grfc, fc_ref)



  grfc <- apply_row_dfw(column_group_by(student_results, program),
                        FC_mean = ~mean(.i[[2]]/.i[[1]]),
                        ~min(.i[[2]]/.i[[1]]), .matrix_wise = FALSE, .input_list = TRUE)
  colnames(fc_ref)[4] <- "~min(.i[[2]]/.i[[1]])"
  expect_identical(grfc, fc_ref)


  # grfc <- apply_row_dfw(column_group_by(student_results, program), FC = ~.i2/.i1,
  #                       .matrix_wise = FALSE, .force_name = TRUE)
  # grs <- column_group_meta(column_group_by(student_results, program))
  # fc_ref_tmp <- lapply(grs$.rows, function(gr) {
  #   tmp <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)), function(r) {
  #     M <- student_results[r,gr,,keep_annotation=FALSE, warn_class_change=FALSE]
  #     nms <- unlist(unique(lapply(M, colnames)))
  #     M <- lapply(M, function(m) {mm <- m[1,]; names(mm) <- nms; mm})
  #     M <- unname(M)
  #     tbl <- tibble::enframe(M[[2]]/M[[1]])
  #     tbl <- tidyr::pivot_wider(tbl, names_from = "name", values_from = "value", names_sep = " ")
  #     colnames(tbl) <- paste("FC", colnames(tbl))
  #     tbl
  #   })
  #   dplyr::bind_rows(tmp, .id = ".rowname")
  # })
  # names(fc_ref_tmp) <- grs$program
  # fc_ref <- dplyr::bind_rows(fc_ref_tmp, .id = "program")
  # expect_identical(grfc, fc_ref)



  grfc <- apply_column_dfw(row_group_by(student_results, class), FC = ~ .j2/.j1, .matrix_wise = FALSE)
  grs <- row_group_meta(row_group_by(student_results, class))
  fc_ref_tmp <- lapply(grs$.rows, function(gr) {
    tmp <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr, cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[,1])
      M <- unname(M)
      tbl <- tibble::enframe(M[[2]]/M[[1]])
      tbl <- tidyr::pivot_wider(tbl, names_from = "name", values_from = "value", names_sep = " ")
      colnames(tbl) <- paste("FC", colnames(tbl))
      tbl
    })
    dplyr::bind_rows(tmp, .id = ".colname")
  })
  names(fc_ref_tmp) <- grs$class
  fc_ref <- dplyr::bind_rows(fc_ref_tmp, .id = "class")
  fc_ref$class <- factor(fc_ref$class, levels = levels(grs$class))

  expect_equal(grfc, fc_ref)


  grfc <- apply_column_dfw(row_group_by(student_results, class), FC_mean = ~ mean(.j2/.j1), FC_min = min(.j2/.j1), .matrix_wise = FALSE)
  grs <- row_group_meta(row_group_by(student_results, class))
  fc_ref_tmp <- lapply(grs$.rows, function(gr) {
    tmp <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)), function(cl) {
      M <- student_results[gr,cl,,keep_annotation=FALSE, warn_class_change=FALSE]
      M <- lapply(M, function(m) m[,1])
      M <- unname(M)
      tbl <- tibble::tibble(mean(M[[2]]/M[[1]]),
                            min(M[[2]]/M[[1]]))
      colnames(tbl) <- paste0(c("FC_mean ", "FC_min "), dots_for_names, "1")
      tbl
    })
    dplyr::bind_rows(tmp, .id = ".colname")
  })
  names(fc_ref_tmp) <- grs$class
  fc_ref <- dplyr::bind_rows(fc_ref_tmp, .id = "class")
  fc_ref$class <- factor(fc_ref$class, levels = levels(grs$class))
  colnames(fc_ref) <- gsub(paste0(" ", dots_for_names_expr, "1"), "", colnames(fc_ref))
  expect_identical(grfc, fc_ref)


  grfc <- apply_column_dfw(row_group_by(student_results, class),
                           FC_mean = ~mean(.j[[2]]/.j[[1]]),
                           ~min(.j[[2]]/.j[[1]]), .matrix_wise = FALSE, .input_list = TRUE)
  colnames(fc_ref)[4] <- "~min(.j[[2]]/.j[[1]])"
  expect_identical(grfc, fc_ref)

#DUAL GROUPING

})








test_that("matrixset matrix loop works", {

  expect_identical(apply_matrix(matrixset(NULL), mean, .matrix_wise = FALSE), NULL)

  expect_error(apply_matrix(student_results, rbind, .matrix_wise = FALSE),
               "object '\\.m' not found")

  # rb <- apply_matrix(student_results, rbind, .matrix_wise = FALSE, .input_list = TRUE)
  # rb_ref <- do.call(rbind, student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE])
  # rb_ref <- list(rbind = rb_ref)
  # expect_equal(rb, rb_ref)


  fc <- apply_matrix(student_results, FC = .m2/.m1, .matrix_wise = FALSE)
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref <- list(FC = m[[2]]/m[[1]])
  expect_equal(fc, fc_ref)



  fc <- apply_matrix_dfl(student_results, FC = rowMeans(.m2/.m1),
                      logFC = rowMeans(log2(.m2/.m1)), .matrix_wise = FALSE)
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref = list(FC = rowMeans(m[[2]]/m[[1]]), logFC = rowMeans(log2(m[[2]]/m[[1]])))
  fc_ref <- tibble::tibble(FC.name = names(fc_ref$FC),
                           FC = unname(fc_ref$FC),
                           logFC.name = names(fc_ref$logFC),
                           logFC = unname(fc_ref$logFC))
  expect_equal(fc, fc_ref)



  fc <- apply_matrix_dfw(student_results, FC = rowMeans(.m2/.m1),
                     logFC = rowMeans(log2(.m2/.m1)), .matrix_wise = FALSE)
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref = c(rowMeans(m[[2]]/m[[1]]), rowMeans(log2(m[[2]]/m[[1]])))
  names(fc_ref) <- paste(rep(c("FC", "logFC"), each = 20), names(fc_ref))
  fc_ref <- tibble::tibble(!!!fc_ref)
  expect_equal(fc, fc_ref)



  # grouped


  # rb <- apply_matrix(row_group_by(student_results, teacher), rbind,
  #                 .matrix_wise = FALSE)
  # m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  # rb_ref <- row_group_meta(row_group_by(student_results, teacher))
  # v <- purrr::map(rb_ref$.rows, ~ list(rbind=do.call(rbind, lapply(m, function(u) u[.x, ]))))
  # rb_ref$.vals <- v
  # rb_ref$.rows <- NULL
  # expect_equal(rb, rb_ref)



  fc <- apply_matrix(row_group_by(student_results, teacher), FC = .m2/.m1,
                  .matrix_wise = FALSE)
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref <- row_group_meta(row_group_by(student_results, teacher))
  v <- purrr::map(fc_ref$.rows, ~ {M <- lapply(m, function(u) u[.x, ]); list(FC=M[[2]]/M[[1]])})
  fc_ref$.vals <- v
  fc_ref$.rows <- NULL
  expect_equal(fc, fc_ref)



  fc <- apply_matrix_dfl(row_group_by(student_results, teacher), FC = colMeans(.m2/.m1),
                      .matrix_wise = FALSE)
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref <- row_group_meta(row_group_by(student_results, teacher))
  v <- purrr::map(fc_ref$.rows, ~ {M <- lapply(m, function(u) u[.x, ]); list(FC=colMeans(M[[2]]/M[[1]]))})
  fc_ref$.vals <- v
  fc_ref$.rows <- NULL
  fc_ref <- tidyr::unnest_longer(tidyr::unnest_wider(fc_ref, .vals), FC)
  fc_ref <- dplyr::select(fc_ref, teacher, FC.name=FC_id, FC)
  names(fc_ref$FC) <- NULL
  expect_equal(fc, fc_ref)



  fc <- apply_matrix_dfw(row_group_by(student_results, teacher), FC = colMeans(.m2/.m1),
                      .matrix_wise = FALSE)
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref <- row_group_meta(row_group_by(student_results, teacher))
  v <- purrr::map(fc_ref$.rows, ~ {M <- lapply(m, function(u) u[.x, ]); list(FC=colMeans(M[[2]]/M[[1]]))})
  fc_ref$.vals <- v
  fc_ref$.rows <- NULL
  fc_ref <- tidyr::unnest_wider(tidyr::unnest_wider(fc_ref, .vals), FC)
  colnames(fc_ref)[2:4] <- paste("FC", colnames(fc_ref)[2:4])
  expect_equal(fc, fc_ref)





  # rb <- apply_matrix(column_group_by(student_results, program), rbind,
  #                 .matrix_wise = FALSE)
  # m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  # rb_ref <- column_group_meta(column_group_by(student_results, program))
  # v <- purrr::map(rb_ref$.rows, ~ list(rbind=do.call(rbind, lapply(m, function(u) u[, .x, drop = FALSE]))))
  # rb_ref$.vals <- v
  # rb_ref$.rows <- NULL
  # expect_equal(rb, rb_ref)



  fc <- apply_matrix(column_group_by(student_results, program), FC = .m2/.m1,
                  .matrix_wise = FALSE)
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref <- column_group_meta(column_group_by(student_results, program))
  v <- purrr::map(fc_ref$.rows, ~ {M <- lapply(m, function(u) u[, .x, drop = FALSE]); list(FC=M[[2]]/M[[1]])})
  fc_ref$.vals <- v
  fc_ref$.rows <- NULL
  expect_equal(fc, fc_ref)



  fc <- apply_matrix_dfl(column_group_by(student_results, program), FC = rowMeans(.m2/.m1),
                      .matrix_wise = FALSE)
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref <- column_group_meta(column_group_by(student_results, program))
  v <- purrr::map(fc_ref$.rows, ~ {M <- lapply(m, function(u) u[, .x, drop = FALSE]); list(FC=rowMeans(M[[2]]/M[[1]]))})
  fc_ref$.vals <- v
  fc_ref$.rows <- NULL
  fc_ref <- tidyr::unnest_longer(tidyr::unnest_wider(fc_ref, .vals), FC)
  fc_ref <- dplyr::select(fc_ref, program, FC.name=FC_id, FC)
  names(fc_ref$FC) <- NULL
  expect_equal(fc, fc_ref)



  fc <- apply_matrix_dfw(column_group_by(student_results, program), FC = rowMeans(.m2/.m1),
                      .matrix_wise = FALSE)
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref <- column_group_meta(column_group_by(student_results, program))
  v <- purrr::map(fc_ref$.rows, ~ {M <- lapply(m, function(u) u[, .x, drop = FALSE]); list(FC=rowMeans(M[[2]]/M[[1]]))})
  fc_ref$.vals <- v
  fc_ref$.rows <- NULL
  fc_ref <- tidyr::unnest_wider(tidyr::unnest_wider(fc_ref, .vals), FC)
  colnames(fc_ref)[2:21] <- paste("FC", colnames(fc_ref)[2:21])
  expect_equal(fc, fc_ref)




  fc <- apply_matrix(column_group_by(row_group_by(student_results, teacher, class), program),
                     .m2/.m1, .matrix_wise = FALSE)
  grs_row <- row_group_meta(row_group_by(student_results, teacher, class))
  grs_col <- column_group_meta(column_group_by(student_results, program))
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref <- m[[2]]/m[[1]]
  grmn_ref <- lapply(grs_row$.rows, function(grr) {
    lapply(grs_col$.rows, function(grc) {
      fc_ref[grr, grc, drop = FALSE]
    })
  })
  grmn_ref <- unlist(grmn_ref, recursive = FALSE)
  grmn_ref <- purrr::map(grmn_ref, ~ list(`.m2/.m1` = .x))
  fc_ref <- tidyr::expand_grid(grs_row[, 1:2], grs_col[,1])
  fc_ref$.vals <- grmn_ref
  expect_identical(fc, fc_ref)




  fc <- apply_matrix_dfl(column_group_by(row_group_by(student_results, teacher, class), program),
                         rowMeans(.m2/.m1), .matrix_wise = FALSE)
  grs_row <- row_group_meta(row_group_by(student_results, teacher, class))
  grs_col <- column_group_meta(column_group_by(student_results, program))
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref <- m[[2]]/m[[1]]
  grmn_ref <- lapply(grs_row$.rows, function(grr) {
    lapply(grs_col$.rows, function(grc) {
      rowMeans(fc_ref[grr, grc, drop = FALSE])
    })
  })
  grmn_ref <- unlist(grmn_ref, recursive = FALSE)
  fc_ref <- tidyr::expand_grid(grs_row[, 1:2], grs_col[,1])
  fc_ref$.vals <- grmn_ref
  fc_ref <- tidyr::unnest_longer(fc_ref, .vals)
  fc_ref <- dplyr::select(fc_ref,
                          teacher, class, program,
                          `rowMeans(.m2/.m1).name` = .vals_id,
                          `rowMeans(.m2/.m1)` = .vals)
  fc_ref$`rowMeans(.m2/.m1)` <- unname(fc_ref$`rowMeans(.m2/.m1)`)
  expect_identical(fc, fc_ref)



  fc <- apply_matrix_dfw(column_group_by(row_group_by(student_results, teacher, class), program),
                         FC = colMeans(.m2/.m1), .matrix_wise = FALSE, .force_name = TRUE)
  grs_row <- row_group_meta(row_group_by(student_results, teacher, class))
  grs_col <- column_group_meta(column_group_by(student_results, program))
  m <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  fc_ref <- m[[2]]/m[[1]]
  grmn_ref <- lapply(grs_row$.rows, function(grr) {
    lapply(grs_col$.rows, function(grc) {
      colMeans(fc_ref[grr, grc, drop = FALSE])
    })
  })
  grmn_ref <- unlist(grmn_ref, recursive = FALSE)
  fc_ref <- tidyr::expand_grid(grs_row[, 1:2], grs_col[,1])
  fc_ref$.vals <- grmn_ref
  fc_ref <- tidyr::unnest_wider(fc_ref, .vals)
  fc_ref <- dplyr::select(fc_ref,
                          teacher, class, program,
                          `FC Mathematics`=Mathematics,
                          `FC Science`=Science, `FC English`=English)
  expect_identical(fc, fc_ref)


})




