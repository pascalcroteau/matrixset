test_that("matrixset general loop works", {

  student_results_M <- mutate_matrix(student_results,
                                     failure = Matrix::Matrix(matrix_elm(student_results, 1)),
                                     remedial = Matrix::Matrix(matrix_elm(student_results, 2)))


  student_results2 <- student_results_M
  matrix_elm(student_results2,2) <- NULL
  mn <- apply_row(student_results2, mean)
  M <- matrix_elm(student_results2,1)
  mn_ref <- list(failure=apply(M, 1, function(u) list(mean=mean(u)),simplify = FALSE))
  mn_ref <- c(mn_ref, list(remedial=lapply(mn_ref$failure, function(u) lapply(u, function(v) NULL))))

  expect_equal(mn, mn_ref)




  mn <- apply_row(student_results_M, mean)
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     apply(M, 1, function(u) list(mean=mean(u)),simplify = FALSE)
                   })
  names(mn_ref) <- matrixnames(student_results_M)

  expect_equal(mn, mn_ref)



  mn <- apply_column(student_results_M, mean)
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     apply(M, 2, function(u) list(mean=mean(u)),simplify = FALSE)
                   })
  names(mn_ref) <- matrixnames(student_results_M)

  expect_equal(mn, mn_ref)



  mn <- apply_row(student_results_M, mn=mean)
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     apply(M, 1, function(u) list(mn=mean(u)),simplify = FALSE)
                   })
  names(mn_ref) <- matrixnames(student_results_M)

  expect_equal(mn, mn_ref)



  mn <- apply_column(student_results_M, mn=mean)
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     apply(M, 2, function(u) list(mn=mean(u)),simplify = FALSE)
                   })
  names(mn_ref) <- matrixnames(student_results_M)

  expect_equal(mn, mn_ref)



  ct <- apply_row(student_results_M, mn=mean, md=median(.i))
  ct_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     apply(M, 1, function(u) list(mn=mean(u),
                                                  md=median(u)),
                           simplify = FALSE)
                   })
  names(ct_ref) <- matrixnames(student_results_M)

  expect_equal(ct, ct_ref)



  ct <- apply_column(student_results_M, mn=mean, md=median(.j))
  ct_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     apply(M, 2, function(u) list(mn=mean(u),
                                                  md=median(u)),
                           simplify = FALSE)
                   })
  names(ct_ref) <- matrixnames(student_results_M)

  expect_equal(ct, ct_ref)



  ct <- apply_row(student_results_M, mn=mean, reg = lm(.i ~ national_average + program))
  ct_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     meta <- column_info(student_results_M)
                     meta <- tibble::column_to_rownames(meta, ".colname")
                     ans <- lapply(1:nrow(student_results_M),
                                   function(i) {
                                     meta$.i <- M[i,]
                                     list(mn=mean(M[i,]),
                                          reg=eval(quote(lm(.i ~ national_average + program)), envir = meta))
                                   })
                     names(ans) <- rownames(student_results_M)
                     ans
                   })
  names(ct_ref) <- matrixnames(student_results_M)

  expect_equal(ct, ct_ref, ignore_attr = TRUE)




  ct <- apply_column(student_results_M, mn=mean, reg = lm(.j ~ teacher + class))
  ct_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     meta <- row_info(student_results_M)
                     meta <- tibble::column_to_rownames(meta, ".rowname")
                     ans <- lapply(1:ncol(student_results_M),
                                   function(j) {
                                     meta$.j <- M[,j]
                                     list(mn=mean(M[,j]),
                                          reg=eval(quote(lm(.j ~ teacher + class)), envir = meta))
                                   })
                     names(ans) <- colnames(student_results_M)
                     ans
                   })
  names(ct_ref) <- matrixnames(student_results_M)

  expect_equal(ct, ct_ref, ignore_attr = TRUE)




  ct <- apply_row(student_results_M, mn=mean, reg = lm(.i ~ national_average + program),
                  .matrix = 2)
  ct_ref <- lapply(2,
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     meta <- column_info(student_results_M)
                     meta <- tibble::column_to_rownames(meta, ".colname")
                     ans <- lapply(1:nrow(student_results_M),
                                   function(i) {
                                     meta$.i <- M[i,]
                                     list(mn=mean(M[i,]),
                                          reg=eval(quote(lm(.i ~ national_average + program)), envir = meta))
                                   })
                     names(ans) <- rownames(student_results_M)
                     ans
                   })
  names(ct_ref) <- matrixnames(student_results_M)[2]

  expect_equal(ct, ct_ref, ignore_attr = TRUE)





  ct <- apply_column(student_results_M, mn=mean, reg = lm(.j ~ teacher + class),
                     .matrix = 2)
  ct_ref <- lapply(2,
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     meta <- row_info(student_results_M)
                     meta <- tibble::column_to_rownames(meta, ".rowname")
                     ans <- lapply(1:ncol(student_results_M),
                                   function(j) {
                                     meta$.j <- M[,j]
                                     list(mn=mean(M[,j]),
                                          reg=eval(quote(lm(.j ~ teacher + class)), envir = meta))
                                   })
                     names(ans) <- colnames(student_results_M)
                     ans
                   })
  names(ct_ref) <- matrixnames(student_results_M)[2]

  expect_equal(ct, ct_ref, ignore_attr = TRUE)





  e <- apply_row(student_results_M,
                 mn = {
                   ii <- .i
                   mean(ii)
                 })

  e_ref <- lapply(seq(nmatrix(student_results_M)),
                  function(m) {
                    M <- matrix_elm(student_results_M,m)
                    apply(M, 1, function(u) list(mn=mean(u)),simplify = FALSE)
                  })
  names(e_ref) <- matrixnames(student_results_M)

  expect_equal(e, e_ref, ignore_attr = TRUE)





  e <- apply_column(student_results_M,
                    mn = {
                      jj <- .j
                      mean(jj)
                    })

  e_ref <- lapply(seq(nmatrix(student_results_M)),
                  function(m) {
                    M <- matrix_elm(student_results_M,m)
                    apply(M, 2, function(u) list(mn=mean(u)),simplify = FALSE)
                  })
  names(e_ref) <- matrixnames(student_results_M)

  expect_equal(e, e_ref, ignore_attr = TRUE)






  e <- apply_row(student_results_M,
                 s={
                   .i + school_average + previous_year_score
                 })

  e_ref <- lapply(seq(nmatrix(student_results_M)),
                  function(m) {
                    M <- matrix_elm(student_results_M,m)
                    row_meta <- row_info(student_results_M)
                    col_meta <- column_info(student_results_M)
                    s <- lapply(1:nrow(M), function(i) list(s=M[i,] + row_meta$previous_year_score[i] + col_meta$school_average))
                    names(s) <- rownames(student_results_M)
                    s
                  })
  names(e_ref) <- matrixnames(student_results_M)

  expect_equal(e, e_ref, ignore_attr = TRUE)





  e <- apply_column(student_results_M,
                    s={
                      .j + school_average + previous_year_score
                    })

  e_ref <- lapply(seq(nmatrix(student_results_M)),
                  function(m) {
                    M <- matrix_elm(student_results_M,m)
                    row_meta <- row_info(student_results_M)
                    col_meta <- column_info(student_results_M)
                    s <- lapply(1:ncol(M), function(j) list(s=M[,j] + row_meta$previous_year_score + col_meta$school_average[j]))
                    names(s) <- colnames(student_results_M)
                    s
                  })
  names(e_ref) <- matrixnames(student_results_M)

  expect_equal(e, e_ref, ignore_attr = TRUE)





  # .data
  rg <- apply_row(student_results_M, reg = unname(coef(lm(.i ~ .data[["national_average"]] + program))))
  rg_ref <- apply_row(student_results_M, reg = unname(coef(lm(.i ~ national_average + program))))
  expect_identical(rg, rg_ref)



  rg <- apply_column(student_results_M, reg = unname(coef(lm(.j ~ .data[["teacher"]] + class))))
  rg_ref <- apply_column(student_results_M, reg = unname(coef(lm(.j ~ teacher + class))))
  expect_identical(rg, rg_ref)




  previous_year_score <- 0.5
  avr <- apply_row(student_results_M, av=mean(c(.i, previous_year_score)))
  avr_ref <- lapply(seq(nmatrix(student_results_M)),
                    function(m) {
                      M <- matrix_elm(student_results_M,m)
                      row_meta <- row_info(student_results_M)
                      s <- lapply(1:nrow(M), function(i) list(av=mean(c(M[i,], row_meta$previous_year_score[i]))))
                      names(s) <- rownames(student_results_M)
                      s
                    })
  names(avr_ref) <- matrixnames(student_results_M)
  expect_identical(avr, avr_ref)



  school_average <- 0.5
  avr <- apply_column(student_results_M, av=mean(c(.j, school_average)))
  avr_ref <- lapply(seq(nmatrix(student_results_M)),
                    function(m) {
                      M <- matrix_elm(student_results_M,m)
                      col_meta <- column_info(student_results_M)
                      s <- lapply(1:ncol(M), function(j) list(av=mean(c(M[,j], col_meta$school_average[j]))))
                      names(s) <- colnames(student_results_M)
                      s
                    })
  names(avr_ref) <- matrixnames(student_results_M)
  expect_identical(avr, avr_ref)



  previous_year_score <- 0.5
  avr <- apply_row(student_results_M, av=mean(c(.i, .env$previous_year_score)))
  avr_ref <- lapply(seq(nmatrix(student_results_M)),
                    function(m) {
                      M <- matrix_elm(student_results_M,m)
                      s <- lapply(1:nrow(M), function(i) list(av=mean(c(M[i,], previous_year_score))))
                      names(s) <- rownames(student_results_M)
                      s
                    })
  names(avr_ref) <- matrixnames(student_results_M)
  expect_identical(avr, avr_ref)




  school_average <- 0.5
  avr <- apply_column(student_results_M, av=mean(c(.j, .env$school_average)))
  avr_ref <- lapply(seq(nmatrix(student_results_M)),
                    function(m) {
                      M <- matrix_elm(student_results_M,m)
                      s <- lapply(1:ncol(M), function(j) list(av=mean(c(M[,j], school_average))))
                      names(s) <- colnames(student_results_M)
                      s
                    })
  names(avr_ref) <- matrixnames(student_results_M)
  expect_identical(avr, avr_ref)





  # 1-row/1 column
  avr <- apply_column(student_results_M[,1,], avr = mean(.j))
  avr_ref <- lapply(seq(nmatrix(student_results_M)),
                    function(m) {
                      M <- matrix_elm(student_results_M,m)
                      apply(M[,1,drop=FALSE], 2, function(u) list(avr=mean(u)),simplify = FALSE)
                    })
  names(avr_ref) <- matrixnames(student_results_M)

  expect_equal(avr, avr_ref)




  avr <- apply_column(student_results_M[1,1,], avr = mean(.j))
  avr_ref <- lapply(seq(nmatrix(student_results_M)),
                    function(m) {
                      M <- matrix_elm(student_results_M,m)
                      apply(M[1,1,drop=FALSE], 2, function(u) list(avr=mean(u)),simplify = FALSE)
                    })
  names(avr_ref) <- matrixnames(student_results_M)

  expect_equal(avr, avr_ref)




  avr <- apply_row(student_results_M[1,,], avr = mean(.i))
  avr_ref <- lapply(seq(nmatrix(student_results_M)),
                    function(m) {
                      M <- matrix_elm(student_results_M,m)
                      apply(M[1,,drop=FALSE], 1, function(u) list(avr=mean(u)),simplify = FALSE)
                    })
  names(avr_ref) <- matrixnames(student_results_M)

  expect_equal(avr, avr_ref)



  avr <- apply_row(student_results_M[1,1,], avr = mean(.i))
  avr_ref <- lapply(seq(nmatrix(student_results_M)),
                    function(m) {
                      M <- matrix_elm(student_results_M,m)
                      apply(M[1,1,drop=FALSE], 1, function(u) list(avr=mean(u)),simplify = FALSE)
                    })
  names(avr_ref) <- matrixnames(student_results_M)

  expect_equal(avr, avr_ref)






  # grouuped
  #
  grmn <- apply_row(column_group_by(student_results_M, program), mean)
  grs <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     ans$.rows <- NULL
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mean=mean(u)),simplify = FALSE)
                     })
                     ans$.vals <- grmn_ref
                     ans
                   })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column(row_group_by(student_results_M, teacher, class), mean)
  grs <- row_group_meta(row_group_by(student_results_M, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results_M)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results_M,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mean=mean(u)),simplify = FALSE)
    })
    ans$.vals <- mn_ref
    ans
  })
  names(grmn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, grmn_ref)




  grmn <- apply_row(column_group_by(student_results_M, program), mn=mean)
  grs <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     ans$.rows <- NULL
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mn=mean(u)),simplify = FALSE)
                     })
                     ans$.vals <- grmn_ref
                     ans
                   })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column(row_group_by(student_results_M, teacher, class), mn=mean)
  grs <- row_group_meta(row_group_by(student_results_M, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results_M)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results_M,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mn=mean(u)),simplify = FALSE)
    })
    ans$.vals <- mn_ref
    ans
  })
  names(grmn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, grmn_ref)

})





test_that("matrixset 'long' loop works", {

  student_results_M <- mutate_matrix(student_results,
                                     failure = Matrix::Matrix(matrix_elm(student_results, 1)),
                                     remedial = Matrix::Matrix(matrix_elm(student_results, 2)))


  student_results2 <- student_results_M
  matrix_elm(student_results2,2) <- NULL
  ct <- apply_row_dfl(student_results2, mn=mean, md=median)
  M <- matrix_elm(student_results2,1)
  ct_ref <- list(failure=t(apply(M, 1, function(u) c(mn=mean(u), md=median(u)),simplify = TRUE)))
  ct_ref$failure <- tibble::as_tibble(ct_ref$failure, rownames = ".rowname")
  ct_ref$remedial <- tibble::tibble(.rowname = character(), mn = logical(), md = logical())
  expect_identical(ct, ct_ref)



  student_results2 <- student_results_M
  matrix_elm(student_results2,2) <- NULL
  ct <- apply_column_dfl(student_results2, mn=mean, md=median)
  M <- matrix_elm(student_results2,1)
  ct_ref <- list(failure=t(apply(M, 2, function(u) c(mn=mean(u), md=median(u)),simplify = TRUE)))
  ct_ref$failure <- tibble::as_tibble(ct_ref$failure, rownames = ".colname")
  ct_ref$remedial <- tibble::tibble(.colname = character(), mn = logical(), md = logical())
  expect_identical(ct, ct_ref)



  ct <- apply_row_dfl(student_results_M, mn=mean, md=median)
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  ct_ref <- lapply(M,
                   function(m) tibble::tibble(.rowname = rownames(m),
                                              mn=unname(Matrix::rowMeans(m)),
                                              md = unname(apply(m,1,median))))
  expect_equal(ct, ct_ref)



  ct <- apply_column_dfl(student_results_M, mn=mean, md=median)
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  ct_ref <- lapply(M,
                   function(m) tibble::tibble(.colname = colnames(m),
                                              mn=unname(Matrix::colMeans(m)),
                                              md = unname(apply(m,2,median))))
  expect_equal(ct, ct_ref)



  # showcase > 1 length answer
  summ <- apply_row_dfl(student_results_M, mn=c(mean(.i), median(.i)), rg=range(.i))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       a <- list(tibble::as_tibble(t(apply(m,1, function(x) c(one=mean(x), two=median(x)))), rownames = ".rowname"),
                                 tibble::as_tibble(t(apply(m,1, function(x) setNames(range(x), c("one", "two")))), rownames = ".rowname"))
                       a <- lapply(a,
                                   function(u) tidyr::pivot_longer(u,
                                                                   names_to = "mn.name",
                                                                   values_to = "mn",
                                                                   cols = c("one", "two")))
                       colnames(a[[2]])[2:3] <- c("rg.name", "rg")
                       suppressMessages(a <- dplyr::bind_cols(a))
                       a$`.rowname...4` <- NULL
                       a <- dplyr::rename(a, `.rowname` = `.rowname...1`)
                       a$mn.name <- ifelse(a$mn.name == "one", "..1", "..2")
                       a$rg.name <- ifelse(a$rg.name == "one", "..1", "..2")
                       a
                     })
  expect_identical(summ, summ_ref)



  summ <- apply_column_dfl(student_results_M, mn=c(mean(.j), median(.j)), rg=range(.j))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       a <- list(tibble::as_tibble(t(apply(m,2, function(x) c(one=mean(x), two=median(x)))), rownames = ".colname"),
                                 tibble::as_tibble(t(apply(m,2, function(x) setNames(range(x), c("one", "two")))), rownames = ".colname"))
                       a <- lapply(a,
                                   function(u) tidyr::pivot_longer(u,
                                                                   names_to = "mn.name",
                                                                   values_to = "mn",
                                                                   cols = c("one", "two")))
                       colnames(a[[2]])[2:3] <- c("rg.name", "rg")
                       suppressMessages(a <- dplyr::bind_cols(a))
                       a$`.colname...4` <- NULL
                       a <- dplyr::rename(a, `.colname` = `.colname...1`)
                       a$mn.name <- ifelse(a$mn.name == "one", "..1", "..2")
                       a$rg.name <- ifelse(a$rg.name == "one", "..1", "..2")
                       a
                     })
  expect_identical(summ, summ_ref)



  summ <- apply_row_dfl(student_results_M, mn=c(mn=mean(.i), md=median(.i)), rg=range(.i))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       a <- list(tibble::as_tibble(t(apply(m,1, function(x) c(one=mean(x), two=median(x)))), rownames = ".rowname"),
                                 tibble::as_tibble(t(apply(m,1, function(x) setNames(range(x), c("one", "two")))), rownames = ".rowname"))
                       a <- lapply(a,
                                   function(u) tidyr::pivot_longer(u,
                                                                   names_to = "mn.name",
                                                                   values_to = "mn",
                                                                   cols = c("one", "two")))
                       colnames(a[[2]])[2:3] <- c("rg.name", "rg")
                       suppressMessages(a <- dplyr::bind_cols(a))
                       a$`.rowname...4` <- NULL
                       a <- dplyr::rename(a, `.rowname` = `.rowname...1`)
                       a$mn.name <- ifelse(a$mn.name == "one", "mn", "md")
                       a$rg.name <- ifelse(a$rg.name == "one", "..1", "..2")
                       a
                     })
  expect_identical(summ, summ_ref)




  summ <- apply_column_dfl(student_results_M, mn=c(mn=mean(.j), md=median(.j)), rg=range(.j))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       a <- list(tibble::as_tibble(t(apply(m,2, function(x) c(one=mean(x), two=median(x)))), rownames = ".colname"),
                                 tibble::as_tibble(t(apply(m,2, function(x) setNames(range(x), c("one", "two")))), rownames = ".colname"))
                       a <- lapply(a,
                                   function(u) tidyr::pivot_longer(u,
                                                                   names_to = "mn.name",
                                                                   values_to = "mn",
                                                                   cols = c("one", "two")))
                       colnames(a[[2]])[2:3] <- c("rg.name", "rg")
                       suppressMessages(a <- dplyr::bind_cols(a))
                       a$`.colname...4` <- NULL
                       a <- dplyr::rename(a, `.colname` = `.colname...1`)
                       a$mn.name <- ifelse(a$mn.name == "one", "mn", "md")
                       a$rg.name <- ifelse(a$rg.name == "one", "..1", "..2")
                       a
                     })
  expect_identical(summ, summ_ref)




  # error
  expect_error(apply_row_dfl(student_results_M,
                             mn=mean,
                             reg = lm(.i ~ national_average + program)),
               "vectors must be of the same length")




  expect_error(apply_column_dfl(student_results_M,
                                mn=mean,
                                reg = lm(.j ~ teacher + previous_year_score)),
               "vectors must be of the same length")




  # the trick
  summ <- apply_row_dfl(student_results_M, mn=mean, reg = list(lm(.i ~ national_average + program)))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- column_info(student_results_M)
  summ_ref <- lapply(M,
                     function(m) {
                       tibble::tibble(.rowname = rownames(m),
                                      mn=unname(Matrix::rowMeans(m)),
                                      reg = unname(apply(m,1,function(x) {
                                        meta$.i <- x
                                        eval(quote(lm(.i ~ national_average + program)), envir = meta)
                                      })))
                     })
  expect_equal(summ, summ_ref, ignore_attr = TRUE)




  summ <- apply_column_dfl(student_results_M, mn=mean, reg = list(lm(.j ~ teacher + previous_year_score)))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- row_info(student_results_M)
  summ_ref <- lapply(M,
                     function(m) {
                       tibble::tibble(.colname = colnames(m),
                                      mn=unname(Matrix::colMeans(m)),
                                      reg = unname(apply(m,2,function(x) {
                                        meta$.j <- x
                                        eval(quote(lm(.j ~ teacher + previous_year_score)), envir = meta)
                                      })))
                     })
  expect_equal(summ, summ_ref, ignore_attr = TRUE)




  summ <- apply_row_dfl(student_results_M, reg = list(lm(.i ~ national_average), lm(.i ~ program)))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- column_info(student_results_M)
  summ_ref <- lapply(M,
                     function(m) {
                       u <- tibble::tibble(
                         .rowname = rownames(student_results_M),
                         a=unname(apply(m,1,function(x) {
                           meta$.i <- x
                           eval(quote(lm(.i ~ national_average)), envir = meta)
                         })),
                         b=unname(apply(m,1,function(x) {
                           meta$.i <- x
                           eval(quote(lm(.i ~ program)), envir = meta)
                         })))
                       u <- tidyr::pivot_longer(u, names_to = "reg.name",
                                                values_to = "reg",
                                                cols = c("a", "b"))
                       u$reg.name <- ifelse(u$reg.name == "a", "..1", "..2")
                       u
                     })
  expect_equal(summ, summ_ref, ignore_attr = TRUE)




  summ <- apply_column_dfl(student_results_M, reg = list(lm(.j ~ class), lm(.j ~ teacher)))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- row_info(student_results_M)
  summ_ref <- lapply(M,
                     function(m) {
                       u <- tibble::tibble(
                         .colname = colnames(student_results_M),
                         a=unname(apply(m,2,function(x) {
                           meta$.j <- x
                           eval(quote(lm(.j ~ class)), envir = meta)
                         })),
                         b=unname(apply(m,2,function(x) {
                           meta$.j <- x
                           eval(quote(lm(.j ~ teacher)), envir = meta)
                         })))
                       u <- tidyr::pivot_longer(u, names_to = "reg.name",
                                                values_to = "reg",
                                                cols = c("a", "b"))
                       u$reg.name <- ifelse(u$reg.name == "a", "..1", "..2")
                       u
                     })
  expect_equal(summ, summ_ref, ignore_attr = TRUE)





  # this should fail
  expect_error(apply_row_dfl(student_results_M, mn=mean(.i), rg=range(.i)),
               "vectors must be of the same length")


  expect_error(apply_column_dfl(student_results_M, mn=mean(.j), rg=range(.j)),
               "vectors must be of the same length")


  expect_error(apply_column_dfl(student_results_M, .colname = mean))
  expect_error(apply_column_dfw(student_results_M, .colname = mean))
  expect_error(apply_row_dfl(student_results_M, .rowname = mean))
  expect_error(apply_row_dfw(student_results_M, .rowname = mean))




  # grouped
  grmn <- apply_row_dfl(column_group_by(student_results_M, program), mean, median)
  grs <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mean=mean(u), median = median(u)),simplify = FALSE)
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_longer(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_wider(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) {
    u <- u[, c(1,4,2,3)]
    colnames(u)[2] <- ".rowname"
    u
  })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column_dfl(row_group_by(student_results_M, teacher, class), mean, median)
  grs <- row_group_meta(row_group_by(student_results_M, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results_M)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results_M,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mean=mean(u), median = median(u)),simplify = FALSE)
    })
    ans$.columns <- mn_ref
    ans
  })
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_longer(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_wider(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) {
    u <- u[, c(1,2,5,3,4)]
    colnames(u)[3] <- ".colname"
    u
  })
  names(grmn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, grmn_ref)




  grmn <- apply_row_dfl(column_group_by(student_results_M, program), mn=mean, md=median(.i))
  grs <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mn=mean(u), md = median(u)),simplify = FALSE)
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_longer(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_wider(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) {
    u <- u[, c(1,4,2,3)]
    colnames(u)[2] <- ".rowname"
    u
  })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column_dfl(row_group_by(student_results_M, teacher, class), mn=mean, md=median(.j))
  grs <- row_group_meta(row_group_by(student_results_M, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results_M)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results_M,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mn=mean(u), md = median(u)),simplify = FALSE)
    })
    ans$.columns <- mn_ref
    ans
  })
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_longer(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_wider(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) {
    u <- u[, c(1,2,5,3,4)]
    colnames(u)[3] <- ".colname"
    u
  })
  names(grmn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, grmn_ref)



  grmn <- apply_row_dfl(column_group_by(student_results_M, program), ct=c(mean(.i), median(.i)))
  grs <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) tibble::tibble(ct.name = c("..1", "..2"), ct=c(mean(u), median(u))),simplify = FALSE)
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_longer(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_wider(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest(u, c(ct.name, ct)))
  mn_ref <- lapply(mn_ref, function(u) {
    u <- u[, c(1,4,2,3)]
    colnames(u)[2] <- ".rowname"
    u
  })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, mn_ref)



  grmn <- apply_column_dfl(row_group_by(student_results_M, teacher, class), ct=c(mean(.j), median(.j)))
  grs <- row_group_meta(row_group_by(student_results_M, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results_M)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results_M,m)
      apply(M[gr, , drop = FALSE], 2, function(u) tibble::tibble(ct.name = c("..1", "..2"), ct=c(mean(u), median(u))),simplify = FALSE)
    })
    ans$.columns <- mn_ref
    ans
  })
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_longer(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_wider(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest(u, c(ct.name, ct)))
  grmn_ref <- lapply(grmn_ref, function(u) {
    u <- u[, c(1,2,5,3,4)]
    colnames(u)[3] <- ".colname"
    u
  })
  names(grmn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, grmn_ref)

})








test_that("matrixset 'wide' loop works", {

  student_results_M <- mutate_matrix(student_results,
                                     failure = Matrix::Matrix(matrix_elm(student_results, 1)),
                                     remedial = Matrix::Matrix(matrix_elm(student_results, 2)))


  student_results2 <- student_results_M
  matrix_elm(student_results2,2) <- NULL
  ct <- apply_row_dfw(student_results2, mn=mean, md=median)
  M <- matrix_elm(student_results2,1)
  ct_ref <- list(failure=t(apply(M, 1, function(u) c(mn=mean(u), md=median(u)),simplify = TRUE)))
  ct_ref$failure <- tibble::as_tibble(ct_ref$failure, rownames = ".rowname")
  ct_ref$remedial <- tibble::tibble(.rowname = character(), mn = logical(), md = logical())
  expect_identical(ct, ct_ref)



  student_results2 <- student_results_M
  matrix_elm(student_results2,2) <- NULL
  ct <- apply_column_dfw(student_results2, mn=mean, md=median)
  M <- matrix_elm(student_results2,1)
  ct_ref <- list(failure=t(apply(M, 2, function(u) c(mn=mean(u), md=median(u)),simplify = TRUE)))
  ct_ref$failure <- tibble::as_tibble(ct_ref$failure, rownames = ".colname")
  ct_ref$remedial <- tibble::tibble(.colname = character(), mn = logical(), md = logical())
  expect_identical(ct, ct_ref)



  ct <- apply_row_dfw(student_results_M, mn=mean, md=median)
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  ct_ref <- lapply(M,
                   function(m) tibble::tibble(.rowname = rownames(m),
                                              mn=unname(Matrix::rowMeans(m)),
                                              md = unname(apply(m,1,median))))
  expect_equal(ct, ct_ref)



  ct <- apply_column_dfw(student_results_M, mn=mean, md=median)
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  ct_ref <- lapply(M,
                   function(m) tibble::tibble(.colname = colnames(m),
                                              mn=unname(Matrix::colMeans(m)),
                                              md = unname(apply(m,2,median))))
  expect_equal(ct, ct_ref)



  # showcase > 1 length answer
  summ <- apply_row_dfw(student_results_M, mn=c(mean(.i), median(.i)), rg=range(.i))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       dplyr::bind_cols(tibble::as_tibble(t(apply(m,1, function(x) c(`mn ..1`=mean(x), `mn ..2`=median(x)))), rownames = ".rowname"),
                                        tibble::as_tibble(t(apply(m,1, function(x) setNames(range(x), c("rg ..1", "rg ..2"))))))
                     })
  expect_identical(summ, summ_ref)




  summ <- apply_column_dfw(student_results_M, mn=c(mean(.j), median(.j)), rg=range(.j))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       dplyr::bind_cols(tibble::as_tibble(t(apply(m,2, function(x) c(`mn ..1`=mean(x), `mn ..2`=median(x)))), rownames = ".colname"),
                                        tibble::as_tibble(t(apply(m,2, function(x) setNames(range(x), c("rg ..1", "rg ..2"))))))
                     })
  expect_identical(summ, summ_ref)




  summ <- apply_row_dfw(student_results_M, mn=c(mn=mean(.i), md=median(.i)), rg=range(.i))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       dplyr::bind_cols(tibble::as_tibble(t(apply(m,1, function(x) c(`mn mn`=mean(x), `mn md`=median(x)))), rownames = ".rowname"),
                                        tibble::as_tibble(t(apply(m,1, function(x) setNames(range(x), c("rg ..1", "rg ..2"))))))
                     })
  expect_identical(summ, summ_ref)




  summ <- apply_column_dfw(student_results_M, mn=c(mn=mean(.j), md=median(.j)), rg=range(.j))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       dplyr::bind_cols(tibble::as_tibble(t(apply(m,2, function(x) c(`mn mn`=mean(x), `mn md`=median(x)))), rownames = ".colname"),
                                        tibble::as_tibble(t(apply(m,2, function(x) setNames(range(x), c("rg ..1", "rg ..2"))))))
                     })
  expect_identical(summ, summ_ref)




  # error
  expect_error(apply_row_dfw(student_results_M,
                             mn=mean,
                             reg = lm(.i ~ national_average + program)),
               "vectors must be of the same length")




  expect_error(apply_column_dfw(student_results_M,
                                mn=mean,
                                reg = lm(.j ~ teacher + previous_year_score)),
               "vectors must be of the same length")




  # the trick
  summ <- apply_row_dfw(student_results_M, mn=mean, reg = list(lm(.i ~ national_average + program)))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- column_info(student_results_M)
  summ_ref <- lapply(M,
                     function(m) {
                       tibble::tibble(.rowname = rownames(m),
                                      mn=unname(Matrix::rowMeans(m)),
                                      reg = unname(apply(m,1,function(x) {
                                        meta$.i <- x
                                        eval(quote(lm(.i ~ national_average + program)), envir = meta)
                                      })))
                     })
  expect_equal(summ, summ_ref, ignore_attr = TRUE)




  summ <- apply_column_dfw(student_results_M, mn=mean, reg = list(lm(.j ~ teacher + previous_year_score)))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- row_info(student_results_M)
  summ_ref <- lapply(M,
                     function(m) {
                       tibble::tibble(.colname = colnames(m),
                                      mn=unname(Matrix::colMeans(m)),
                                      reg = unname(apply(m,2,function(x) {
                                        meta$.j <- x
                                        eval(quote(lm(.j ~ teacher + previous_year_score)), envir = meta)
                                      })))
                     })
  expect_equal(summ, summ_ref, ignore_attr = TRUE)



  summ <- apply_row_dfw(student_results_M, reg = list(national = lm(.i ~ national_average), program = lm(.i ~ program)))
  M <- student_results_M[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- column_info(student_results_M)
  summ_ref <- lapply(M,
                     function(m) {
                       tibble::tibble(.rowname = rownames(m),
                                      `reg national` = unname(apply(m,1,function(x) {
                                        meta$.i <- x
                                        eval(quote(lm(.i ~ national_average)), envir = meta)
                                      })),
                                      `reg program` = unname(apply(m,1,function(x) {
                                        meta$.i <- x
                                        eval(quote(lm(.i ~ program)), envir = meta)
                                      })))
                     })
  expect_equal(summ, summ_ref, ignore_attr = TRUE)


  #
  # # this should fail
  expect_error(apply_row_dfw(student_results_M, mn=mean(.i), rg=range(.i)),
               "vectors must be of the same length")


  expect_error(apply_column_dfw(student_results_M, mn=mean(.j), rg=range(.j)),
               "vectors must be of the same length")







  # grouped
  grmn <- apply_row_dfw(column_group_by(student_results_M, program), mean, median)
  grs <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mean=mean(u), median = median(u)),simplify = FALSE)
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_longer(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_wider(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) {
    u <- u[, c(1,4,2,3)]
    colnames(u)[2] <- ".rowname"
    u
  })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column_dfw(row_group_by(student_results_M, teacher, class), mean, median)
  grs <- row_group_meta(row_group_by(student_results_M, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results_M)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results_M,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mean=mean(u), median = median(u)),simplify = FALSE)
    })
    ans$.columns <- mn_ref
    ans
  })
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_longer(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_wider(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) {
    u <- u[, c(1,2,5,3,4)]
    colnames(u)[3] <- ".colname"
    u
  })
  names(grmn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, grmn_ref)




  grmn <- apply_row_dfw(column_group_by(student_results_M, program), mn=mean, md=median(.i))
  grs <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mn=mean(u), md = median(u)),simplify = FALSE)
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_longer(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_wider(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) {
    u <- u[, c(1,4,2,3)]
    colnames(u)[2] <- ".rowname"
    u
  })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column_dfw(row_group_by(student_results_M, teacher, class), mn=mean, md=median(.j))
  grs <- row_group_meta(row_group_by(student_results_M, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results_M)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results_M,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mn=mean(u), md = median(u)),simplify = FALSE)
    })
    ans$.columns <- mn_ref
    ans
  })
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_longer(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_wider(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) {
    u <- u[, c(1,2,5,3,4)]
    colnames(u)[3] <- ".colname"
    u
  })
  names(grmn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, grmn_ref)



  grmn <- apply_row_dfw(column_group_by(student_results_M, program), ct=c(mean(.i), median(.i)))
  grs <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) tibble::tibble(ct.name = c("ct ..1", "ct ..2"), ct=c(mean(u), median(u))),simplify = FALSE)
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_longer(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_wider(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest(u, c(ct.name, ct)))
  mn_ref <- lapply(mn_ref, function(u) tidyr::pivot_wider(u, names_from = "ct.name", values_from = "ct"))
  mn_ref <- lapply(mn_ref, function(u) {
    colnames(u)[2] <- ".rowname"
    u
  })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, mn_ref)



  grmn <- apply_column_dfw(row_group_by(student_results_M, teacher, class), ct=c(mean(.j), median(.j)))
  grs <- row_group_meta(row_group_by(student_results_M, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results_M)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results_M,m)
      apply(M[gr, , drop = FALSE], 2, function(u) tibble::tibble(ct.name = c("d..1", "d..2"), ct=c(mean(u), median(u))),simplify = FALSE)
    })
    ans$.columns <- mn_ref
    ans
  })
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_longer(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_wider(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest(u, c(ct.name, ct)))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::pivot_wider(u, names_from = "ct.name", values_from = "ct"))
  grmn_ref <- lapply(grmn_ref, function(u) {
    colnames(u)[3:5] <- c(".colname", "ct ..1", "ct ..2")
    u
  })
  names(grmn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, grmn_ref)



  grmn <- apply_row_dfw(column_group_by(student_results_M, program),
                        ct=c(mn=mean(.i), md=median(.i)),
                        rg=range,
                        fit=list(lm(.i ~ 1), lm(.i ~ school_average)))
  grs <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       meta <- column_info(student_results_M)
                       rows <- lapply(seq(nrow(M)), function(i) {
                         u <- M[i, gr]
                         info <- meta[gr, ]
                         info$.i <- u
                         tibble::tibble(ct.name = c("ct mn", "ct md"),
                                        ct=c(mean(u), median(u)),
                                        rg.name=c("rg ..1", "rg ..2"),
                                        rg=range(u),
                                        fit.name = c("fit ..1", "fit ..2"),
                                        fit=list(eval(quote(lm(.i ~ 1)), info),
                                                 eval(quote(lm(.i ~ school_average)), info)))
                       })
                       names(rows) <- rownames(student_results_M)
                       dplyr::bind_rows(rows, .id = ".rowname")
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::pivot_wider(u, names_from = c("ct.name", "rg.name", "fit.name"), values_from = c("ct", "rg", "fit")))
  mn_ref <- lapply(mn_ref, function(u) {
    colnames(u)[3:8] <- c("ct mn", "ct md", "rg ..1", "rg ..2", "fit ..1", "fit ..2")
    u
  })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, mn_ref, ignore_attr = TRUE)



  grmn <- apply_column_dfw(row_group_by(student_results_M, teacher, class),
                           ct=c(mn=mean(.j), md=median(.j)),
                           rg=range,
                           fit=list(lm(.j ~ 1), lm(.j ~ previous_year_score)))
  grs <- row_group_meta(row_group_by(student_results_M, teacher, class))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       meta <- row_info(student_results_M)
                       cols <- lapply(seq(ncol(M)), function(j) {
                         u <- M[gr, j]
                         info <- meta[gr, ]
                         info$.j <- u
                         tibble::tibble(ct.name = c("ct mn", "ct md"),
                                        ct=c(mean(u), median(u)),
                                        rg.name=c("rg ..1", "rg ..2"),
                                        rg=range(u),
                                        fit.name = c("fit ..1", "fit ..2"),
                                        fit=list(eval(quote(lm(.j ~ 1)), info),
                                                 eval(quote(lm(.j ~ previous_year_score)), info)))
                       })
                       names(cols) <- colnames(student_results_M)
                       dplyr::bind_rows(cols, .id = ".colname")
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::pivot_wider(u, names_from = c("ct.name", "rg.name", "fit.name"), values_from = c("ct", "rg", "fit")))
  mn_ref <- lapply(mn_ref, function(u) {
    colnames(u)[3:8] <- c("ct mn", "ct md", "rg ..1", "rg ..2", "fit ..1", "fit ..2")
    u
  })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_identical(grmn, mn_ref, ignore_attr = TRUE)

})








test_that("matrixset matrix loop works", {

  student_results_M <- mutate_matrix(student_results,
                                     failure = Matrix::Matrix(matrix_elm(student_results, 1)),
                                     remedial = Matrix::Matrix(matrix_elm(student_results, 2)))

  student_results2 <- student_results_M
  matrix_elm(student_results2,2) <- NULL
  mn <- apply_matrix(student_results2, Matrix::mean)
  M <- matrix_elm(student_results2,1)
  mn_ref <- list(failure=list(`Matrix::mean`=Matrix::mean(M)), remedial=list(`Matrix::mean`=NULL))

  expect_equal(mn, mn_ref)




  mn <- apply_matrix(student_results_M, Matrix::mean)
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     list(`Matrix::mean`=Matrix::mean(M))
                   })
  names(mn_ref) <- matrixnames(student_results_M)

  expect_equal(mn, mn_ref)



  mn <- apply_matrix(student_results_M, mn=Matrix::mean)
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     list(mn=Matrix::mean(M))
                   })
  names(mn_ref) <- matrixnames(student_results_M)

  expect_equal(mn, mn_ref)



  ct <- apply_matrix(student_results_M, mn=Matrix::mean, md=Matrix::mean(.m))
  ct_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     M <- matrix_elm(student_results_M,m)
                     list(mn=Matrix::mean(M), md=Matrix::mean(M))
                   })
  names(ct_ref) <- matrixnames(student_results_M)

  expect_equal(ct, ct_ref)



  e <- apply_matrix(student_results_M,
                    mn = {
                      mm <- .m
                      Matrix::mean(mm)
                    })

  e_ref <- lapply(seq(nmatrix(student_results_M)),
                  function(m) {
                    M <- matrix_elm(student_results_M,m)
                    list(mn=Matrix::mean(M))
                  })
  names(e_ref) <- matrixnames(student_results_M)

  expect_equal(e, e_ref, ignore_attr = TRUE)





  # grouped


  grmn <- apply_matrix(column_group_by(student_results_M, program), Matrix::mean)
  grs <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       list(`Matrix::mean`=Matrix::mean(M[, gr]))
                     })
                     ans$.rows <- NULL
                     ans$.vals <- grmn_ref
                     ans
                   })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_equal(grmn, mn_ref)




  grmn <- apply_matrix(column_group_by(student_results_M, program), mn=Matrix::mean)
  grs <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       list(mn=Matrix::mean(M[, gr]))
                     })
                     ans$.rows <- NULL
                     ans$.vals <- grmn_ref
                     ans
                   })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_equal(grmn, mn_ref)




  grmn <- apply_matrix(row_group_by(student_results_M, teacher, class), Matrix::mean, rg=range)
  grs <- row_group_meta(row_group_by(student_results_M, teacher, class))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results_M,m)
                       list(`Matrix::mean`=Matrix::mean(M[gr, ]), rg=range(M[gr, ]))
                     })
                     ans$.rows <- NULL
                     ans$.vals <- grmn_ref
                     ans
                   })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_equal(grmn, mn_ref)



  grmn <- apply_matrix(column_group_by(row_group_by(student_results_M, teacher, class), program), mn=Matrix::mean, rg=range)
  grs_row <- row_group_meta(row_group_by(student_results_M, teacher, class))
  grs_col <- column_group_meta(column_group_by(student_results_M, program))
  mn_ref <- lapply(seq(nmatrix(student_results_M)),
                   function(m) {
                     ans <- grs
                     M <- matrix_elm(student_results_M,m)
                     grmn_ref <- lapply(grs_row$.rows, function(grr) {
                       lapply(grs_col$.rows, function(grc) {
                         list(mn=Matrix::mean(M[grr, grc]), rg=range(M[grr, grc]))
                       })
                     })
                     unlist(grmn_ref, recursive = FALSE)
                     # grmn_ref
                     # ans$.rows <- NULL
                     # ans$.mats <- grmn_ref
                     # ans
                   })
  names(mn_ref) <- matrixnames(student_results_M)
  expect_equal(grmn$failure$.vals, mn_ref$failure)
  expect_equal(grmn$remedial$.vals, mn_ref$remedial)



})




