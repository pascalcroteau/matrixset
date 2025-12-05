test_that("matrixset general loop works", {

  withr::local_options(lifecycle_verbosity = "quiet")

  expect_identical(apply_row(matrixset(NULL), mean), NULL)
  expect_identical(apply_column(matrixset(NULL), mean), NULL)

  student_results2 <- student_results
  matrix_elm(student_results2,2) <- NULL
  mn <- apply_row(student_results2, mean)
  M <- matrix_elm(student_results2,1)
  mn_ref <- list(failure=apply(M, 1, function(u) list(mean=mean(u)),simplify = FALSE))
  mn_ref <- c(mn_ref, list(remedial = NULL))
  # mn_ref <- c(mn_ref, list(remedial=lapply(mn_ref$failure, function(u) lapply(u, function(v) NULL))))

  expect_equal(mn, mn_ref)





  create_expected <- function(midx, margin, ...)
  {
    fns <- rlang::enquos(..., .named = TRUE)

    if (margin == "row") {
      extract_info <- column_info
      extract_info_compl <- row_info
      tag_name <- ".colname"
      tag_name_compl <- ".rowname"
      pronoun <- ".i"
      N <- nrow
      mrgnames <- rownames
    } else {
      extract_info <- row_info
      extract_info_compl <- column_info
      tag_name <- ".rowname"
      tag_name_compl <- ".colname"
      pronoun <- ".j"
      N <- ncol
      mrgnames <- colnames
    }

    meta <-  extract_info(student_results)
    meta <- tibble::column_to_rownames(meta, tag_name)

    meta_compl <- extract_info_compl(student_results)
    meta_compl <- tibble::column_to_rownames(meta_compl, tag_name_compl)

    meta_env <- list2env(meta)

    meta_mask <- rlang::new_data_mask(meta_env)

    expected <- lapply(midx, function(m) {
      M <- matrix_elm(student_results, m)

      ans <- lapply(1:N(student_results),
                    function(i) {
                      meta_env[[pronoun]] <- if (margin == "row") M[i,] else M[,i]
                      meta_env <- list2env(meta_compl[i, ], envir = meta_env)

                      lapply(fns, function(f) rlang::eval_tidy(f, data = meta_mask))

                    })
      names(ans) <- mrgnames(student_results)
      ans
    })

    names(expected) <- matrixnames(student_results)[midx]

    expected

  }



  testcase <- list(# single function, not named, no params, all matrices
                   quote(apply_row(student_results, mean)),
                   # single function, named, no params, all matrices
                   quote(apply_row(student_results, mn=mean)),
                   # two functions, named, mix params/no params, all matrices
                   quote(apply_row(student_results, mn=mean, md=~median(.i))),
                   quote(apply_row(student_results, mn=mean,
                                   reg = ~lm(.i ~ national_average + program))),
                   quote(apply_row(student_results, mn=mean,
                                   reg = ~lm(.i ~ national_average + program),
                                   .matrix = 2)),
                   quote(apply_row(student_results,
                                   mn = ~{
                                     ii <- .i
                                     mean(ii)
                                   })),
                   quote(apply_row(student_results,
                                   s=~{
                                     .i + school_average + previous_year_score
                                   })),




                   quote(apply_column(student_results, mean)),
                   quote(apply_column(student_results, mn=mean)),
                   quote(apply_column(student_results, mn=mean, md=~median(.j))),
                   quote(apply_column(student_results, mn=mean,
                                      reg = ~lm(.j ~ teacher + class))),
                   quote(apply_column(student_results, mn=mean,
                                      reg = ~lm(.j ~ teacher + class),
                                      .matrix = 2)),
                   quote(apply_column(student_results,
                                      mn = ~{
                                        jj <- .j
                                        mean(jj)
                                      })),
                   quote(apply_column(student_results,
                                      s=~{
                                        .j + school_average + previous_year_score
                                      }))
                   )


  expected_expr <- list(quote(create_expected(seq(nmatrix(student_results)),
                                              "row", mean = mean(.i))),
                        quote(create_expected(seq(nmatrix(student_results)),
                                              "row", mn=mean(.i))),
                        quote(create_expected(seq(nmatrix(student_results)),
                                              "row", mn=mean(.i), md=median(.i))),
                        quote(create_expected(seq(nmatrix(student_results)),
                                              "row", mn=mean(.i),
                                              reg = lm(.i ~ national_average + program))),
                        quote(create_expected(2, "row", mn=mean(.i),
                                              reg = lm(.i ~ national_average + program))),
                        quote(create_expected(seq(nmatrix(student_results)),
                                              "row",
                                              mn = {
                                                ii <- .i
                                                mean(ii)
                                              })),
                        quote(create_expected(seq(nmatrix(student_results)),
                                              "row",
                                              s={
                                                .i + school_average + previous_year_score
                                              })),



                        quote(create_expected(seq(nmatrix(student_results)),
                                              "col", mean = mean(.j))),
                        quote(create_expected(seq(nmatrix(student_results)),
                                              "col", mn=mean(.j))),
                        quote(create_expected(seq(nmatrix(student_results)),
                                              "col", mn=mean(.j), md=median(.j))),
                        quote(create_expected(seq(nmatrix(student_results)),
                                              "col", mn=mean(.j),
                                              reg = lm(.j ~ teacher + class))),
                        quote(create_expected(2, "col", mn=mean(.j),
                                              reg = lm(.j ~ teacher + class))),
                        quote(create_expected(seq(nmatrix(student_results)),
                                              "col",
                                              mn = {
                                                jj <- .j
                                                mean(jj)
                                              })),
                        quote(create_expected(seq(nmatrix(student_results)),
                                              "col",
                                              s={
                                                .j + school_average + previous_year_score
                                              }))
                        )

  for (case in seq_along(testcase))
  {
    testthis <- eval(testcase[[case]])
    expected <- eval(expected_expr[[case]])
    expect_equal(testthis, expected, ignore_attr = TRUE)
  }



  # mn <- apply_row(student_results, mean)
  # mn_ref <- lapply(seq(nmatrix(student_results)),
  #                  function(m) {
  #                    M <- matrix_elm(student_results,m)
  #                    apply(M, 1, function(u) list(mean=mean(u)),simplify = FALSE)
  #                  })
  # names(mn_ref) <- matrixnames(student_results)
  #
  # expect_equal(mn, mn_ref)
  #
  #
  #
  # mn <- apply_row(student_results, mn=mean)
  # mn_ref <- lapply(seq(nmatrix(student_results)),
  #                  function(m) {
  #                    M <- matrix_elm(student_results,m)
  #                    apply(M, 1, function(u) list(mn=mean(u)),simplify = FALSE)
  #                  })
  # names(mn_ref) <- matrixnames(student_results)
  #
  # expect_equal(mn, mn_ref)
  #
  #
  #
  # ct <- apply_row(student_results, mn=mean, md=~median(.i))
  # ct_ref <- lapply(seq(nmatrix(student_results)),
  #                  function(m) {
  #                    M <- matrix_elm(student_results,m)
  #                    apply(M, 1, function(u) list(mn=mean(u),
  #                                                 md=median(u)),
  #                          simplify = FALSE)
  #                  })
  # names(ct_ref) <- matrixnames(student_results)
  #
  # expect_equal(ct, ct_ref)
  #
  #
  #
  # ct <- apply_row(student_results, mn=mean, reg = ~lm(.i ~ national_average + program))
  # ct_ref <- lapply(seq(nmatrix(student_results)),
  #                  function(m) {
  #                    M <- matrix_elm(student_results,m)
  #                    meta <- column_info(student_results)
  #                    meta <- tibble::column_to_rownames(meta, ".colname")
  #                    ans <- lapply(1:nrow(student_results),
  #                                  function(i) {
  #                                    meta$.i <- M[i,]
  #                                    list(mn=mean(M[i,]),
  #                                         reg=eval(quote(lm(.i ~ national_average + program)), envir = meta))
  #                                  })
  #                    names(ans) <- rownames(student_results)
  #                    ans
  #                  })
  # names(ct_ref) <- matrixnames(student_results)
  #
  # expect_equal(ct, ct_ref, ignore_attr = TRUE)
  #
  #
  #
  # ct <- apply_row(student_results, mn=mean, reg = ~ lm(.i ~ national_average + program),
  #                 .matrix = 2)
  # ct_ref <- lapply(2,
  #                  function(m) {
  #                    M <- matrix_elm(student_results,m)
  #                    meta <- column_info(student_results)
  #                    meta <- tibble::column_to_rownames(meta, ".colname")
  #                    ans <- lapply(1:nrow(student_results),
  #                                  function(i) {
  #                                    meta$.i <- M[i,]
  #                                    list(mn=mean(M[i,]),
  #                                         reg=eval(quote(lm(.i ~ national_average + program)), envir = meta))
  #                                  })
  #                    names(ans) <- rownames(student_results)
  #                    ans
  #                  })
  # names(ct_ref) <- matrixnames(student_results)[2]
  #
  # expect_equal(ct, ct_ref, ignore_attr = TRUE)
  #
  #
  #
  #
  #
  # mn <- apply_column(student_results, mean)
  # mn_ref <- lapply(seq(nmatrix(student_results)),
  #                  function(m) {
  #                    M <- matrix_elm(student_results,m)
  #                    apply(M, 2, function(u) list(mean=mean(u)),simplify = FALSE)
  #                  })
  # names(mn_ref) <- matrixnames(student_results)
  #
  # expect_equal(mn, mn_ref)
  #
  #
  #
  # mn <- apply_column(student_results, mn=mean)
  # mn_ref <- lapply(seq(nmatrix(student_results)),
  #                  function(m) {
  #                    M <- matrix_elm(student_results,m)
  #                    apply(M, 2, function(u) list(mn=mean(u)),simplify = FALSE)
  #                  })
  # names(mn_ref) <- matrixnames(student_results)
  #
  # expect_equal(mn, mn_ref)
  #
  #
  #
  #
  # ct <- apply_column(student_results, mn=mean, md=~median(.j))
  # ct_ref <- lapply(seq(nmatrix(student_results)),
  #                  function(m) {
  #                    M <- matrix_elm(student_results,m)
  #                    apply(M, 2, function(u) list(mn=mean(u),
  #                                                 md=median(u)),
  #                          simplify = FALSE)
  #                  })
  # names(ct_ref) <- matrixnames(student_results)
  #
  # expect_equal(ct, ct_ref)
  #
  #
  #
  #
  # ct <- apply_column(student_results, mn=mean, reg = ~lm(.j ~ teacher + class))
  # ct_ref <- lapply(seq(nmatrix(student_results)),
  #                  function(m) {
  #                    M <- matrix_elm(student_results,m)
  #                    meta <- row_info(student_results)
  #                    meta <- tibble::column_to_rownames(meta, ".rowname")
  #                    ans <- lapply(1:ncol(student_results),
  #                                  function(j) {
  #                                    meta$.j <- M[,j]
  #                                    list(mn=mean(M[,j]),
  #                                         reg=eval(quote(lm(.j ~ teacher + class)), envir = meta))
  #                                  })
  #                    names(ans) <- colnames(student_results)
  #                    ans
  #                  })
  # names(ct_ref) <- matrixnames(student_results)
  #
  # expect_equal(ct, ct_ref, ignore_attr = TRUE)
  #
  #
  #
  #
  #
  # ct <- apply_column(student_results, mn=mean, reg = ~lm(.j ~ teacher + class),
  #                   .matrix = 2)
  # ct_ref <- lapply(2,
  #                  function(m) {
  #                    M <- matrix_elm(student_results,m)
  #                    meta <- row_info(student_results)
  #                    meta <- tibble::column_to_rownames(meta, ".rowname")
  #                    ans <- lapply(1:ncol(student_results),
  #                                  function(j) {
  #                                    meta$.j <- M[,j]
  #                                    list(mn=mean(M[,j]),
  #                                         reg=eval(quote(lm(.j ~ teacher + class)), envir = meta))
  #                                  })
  #                    names(ans) <- colnames(student_results)
  #                    ans
  #                  })
  # names(ct_ref) <- matrixnames(student_results)[2]
  #
  # expect_equal(ct, ct_ref, ignore_attr = TRUE)
  #
  #
  #
  #
  #
  # e <- apply_row(student_results,
  #               mn = ~ {
  #                 ii <- .i
  #                 mean(ii)
  #               })
  #
  # e_ref <- lapply(seq(nmatrix(student_results)),
  #                 function(m) {
  #                   M <- matrix_elm(student_results,m)
  #                   apply(M, 1, function(u) list(mn=mean(u)),simplify = FALSE)
  #                 })
  # names(e_ref) <- matrixnames(student_results)
  #
  # expect_equal(e, e_ref, ignore_attr = TRUE)
  #
  #
  #
  #
  #
  # e <- apply_column(student_results,
  #               mn = ~ {
  #                 jj <- .j
  #                 mean(jj)
  #               })
  #
  # e_ref <- lapply(seq(nmatrix(student_results)),
  #                 function(m) {
  #                   M <- matrix_elm(student_results,m)
  #                   apply(M, 2, function(u) list(mn=mean(u)),simplify = FALSE)
  #                 })
  # names(e_ref) <- matrixnames(student_results)
  #
  # expect_equal(e, e_ref, ignore_attr = TRUE)
  #
  #
  #
  #
  #
  #
  # e <- apply_row(student_results,
  #               s=~{
  #                 .i + school_average + previous_year_score
  #               })
  #
  # e_ref <- lapply(seq(nmatrix(student_results)),
  #                 function(m) {
  #                   M <- matrix_elm(student_results,m)
  #                   row_meta <- row_info(student_results)
  #                   col_meta <- column_info(student_results)
  #                   s <- lapply(1:nrow(M), function(i) list(s=M[i,] + row_meta$previous_year_score[i] + col_meta$school_average))
  #                   names(s) <- rownames(student_results)
  #                   s
  #                 })
  # names(e_ref) <- matrixnames(student_results)
  #
  # expect_equal(e, e_ref, ignore_attr = TRUE)
  #
  #
  #
  #
  #
  # e <- apply_column(student_results,
  #               s=~{
  #                 .j + school_average + previous_year_score
  #               })
  #
  # e_ref <- lapply(seq(nmatrix(student_results)),
  #                 function(m) {
  #                   M <- matrix_elm(student_results,m)
  #                   row_meta <- row_info(student_results)
  #                   col_meta <- column_info(student_results)
  #                   s <- lapply(1:ncol(M), function(j) list(s=M[,j] + row_meta$previous_year_score + col_meta$school_average[j]))
  #                   names(s) <- colnames(student_results)
  #                   s
  #                 })
  # names(e_ref) <- matrixnames(student_results)
  #
  # expect_equal(e, e_ref, ignore_attr = TRUE)





  # .data
  rg <- apply_row(student_results, reg = ~unname(coef(lm(.i ~ .data[["national_average"]] + program))))
  rg_ref <- apply_row(student_results, reg = ~unname(coef(lm(.i ~ national_average + program))))
  expect_identical(rg, rg_ref)



  rg <- apply_column(student_results, reg = ~ unname(coef(lm(.j ~ .data[["teacher"]] + class))))
  rg_ref <- apply_column(student_results, reg = ~ unname(coef(lm(.j ~ teacher + class))))
  expect_identical(rg, rg_ref)




  previous_year_score <- 0.5
  avr <- apply_row(student_results, av=~mean(c(.i, previous_year_score)))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                  function(m) {
                    M <- matrix_elm(student_results,m)
                    row_meta <- row_info(student_results)
                    s <- lapply(1:nrow(M), function(i) list(av=mean(c(M[i,], row_meta$previous_year_score[i]))))
                    names(s) <- rownames(student_results)
                    s
                  })
  names(avr_ref) <- matrixnames(student_results)
  expect_identical(avr, avr_ref)



  school_average <- 0.5
  avr <- apply_column(student_results, av=~mean(c(.j, school_average)))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                    function(m) {
                      M <- matrix_elm(student_results,m)
                      col_meta <- column_info(student_results)
                      s <- lapply(1:ncol(M), function(j) list(av=mean(c(M[,j], col_meta$school_average[j]))))
                      names(s) <- colnames(student_results)
                      s
                    })
  names(avr_ref) <- matrixnames(student_results)
  expect_identical(avr, avr_ref)



  previous_year_average <- 0.5
  avr <- apply_row(student_results, av=~mean(c(.i, previous_year_average)))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                    function(m) {
                      M <- matrix_elm(student_results,m)
                      row_meta <- row_info(student_results)
                      s <- lapply(1:nrow(M), function(i) list(av=mean(c(M[i,], previous_year_average))))
                      names(s) <- rownames(student_results)
                      s
                    })
  names(avr_ref) <- matrixnames(student_results)
  expect_identical(avr, avr_ref)



  prior_school_average <- 0.5
  avr <- apply_column(student_results, av=~mean(c(.j, prior_school_average)))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                    function(m) {
                      M <- matrix_elm(student_results,m)
                      col_meta <- column_info(student_results)
                      s <- lapply(1:ncol(M), function(j) list(av=mean(c(M[,j], prior_school_average))))
                      names(s) <- colnames(student_results)
                      s
                    })
  names(avr_ref) <- matrixnames(student_results)
  expect_identical(avr, avr_ref)



  previous_year_score <- 0.5
  avr <- apply_row(student_results, av=~mean(c(.i, .env$previous_year_score)))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                    function(m) {
                      M <- matrix_elm(student_results,m)
                      s <- lapply(1:nrow(M), function(i) list(av=mean(c(M[i,], previous_year_score))))
                      names(s) <- rownames(student_results)
                      s
                    })
  names(avr_ref) <- matrixnames(student_results)
  expect_identical(avr, avr_ref)




  school_average <- 0.5
  avr <- apply_column(student_results, av=~mean(c(.j, .env$school_average)))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                    function(m) {
                      M <- matrix_elm(student_results,m)
                      s <- lapply(1:ncol(M), function(j) list(av=mean(c(M[,j], school_average))))
                      names(s) <- colnames(student_results)
                      s
                    })
  names(avr_ref) <- matrixnames(student_results)
  expect_identical(avr, avr_ref)





  # 1-row/1 column
  avr <- apply_column(student_results[,1,], avr = ~mean(.j))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     M <- matrix_elm(student_results,m)
                     apply(M[,1,drop=FALSE], 2, function(u) list(avr=mean(u)),simplify = FALSE)
                   })
  names(avr_ref) <- matrixnames(student_results)

  expect_equal(avr, avr_ref)




  avr <- apply_column(student_results[1,1,], avr = ~ mean(.j))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                    function(m) {
                      M <- matrix_elm(student_results,m)
                      apply(M[1,1,drop=FALSE], 2, function(u) list(avr=mean(u)),simplify = FALSE)
                    })
  names(avr_ref) <- matrixnames(student_results)

  expect_equal(avr, avr_ref)




  avr <- apply_row(student_results[1,,], avr = ~ mean(.i))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                    function(m) {
                      M <- matrix_elm(student_results,m)
                      apply(M[1,,drop=FALSE], 1, function(u) list(avr=mean(u)),simplify = FALSE)
                    })
  names(avr_ref) <- matrixnames(student_results)

  expect_equal(avr, avr_ref)



  avr <- apply_row(student_results[1,1,], avr = ~ mean(.i))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                    function(m) {
                      M <- matrix_elm(student_results,m)
                      apply(M[1,1,drop=FALSE], 1, function(u) list(avr=mean(u)),simplify = FALSE)
                    })
  names(avr_ref) <- matrixnames(student_results)

  expect_equal(avr, avr_ref)






  # grouped
  #
  grmn <- apply_row(column_group_by(student_results, program), mean)
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     ans$.rows <- NULL
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mean=mean(u)),simplify = FALSE)
                     })
                     ans$.vals <- grmn_ref
                     ans
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column(row_group_by(student_results, teacher, class), mean)
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mean=mean(u)),simplify = FALSE)
    })
    ans$.vals <- mn_ref
    ans
  })
  names(grmn_ref) <- matrixnames(student_results)
  expect_identical(grmn, grmn_ref)




  grmn <- apply_row(column_group_by(student_results, program), mn=mean)
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     ans$.rows <- NULL
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mn=mean(u)),simplify = FALSE)
                     })
                     ans$.vals <- grmn_ref
                     ans
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column(row_group_by(student_results, teacher, class), mn=mean)
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mn=mean(u)),simplify = FALSE)
    })
    ans$.vals <- mn_ref
    ans
  })
  names(grmn_ref) <- matrixnames(student_results)
  expect_identical(grmn, grmn_ref)



  grmn <- apply_row(row_group_by(column_group_by(student_results, program),
                                 class, teacher), mn=mean)
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     ans$.rows <- NULL
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mn=mean(u)),simplify = FALSE)
                     })
                     ans$.vals <- grmn_ref
                     ans
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column(column_group_by(row_group_by(student_results, teacher, class),
                                       program), mn=mean)
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mn=mean(u)),simplify = FALSE)
    })
    ans$.vals <- mn_ref
    ans
  })
  names(grmn_ref) <- matrixnames(student_results)
  expect_identical(grmn, grmn_ref)

})





test_that("matrixset 'long' loop works", {

  withr::local_options(lifecycle_verbosity = "quiet")

  # with null
  expect_identical(apply_row_dfl(matrixset(NULL), mean), NULL)
  expect_identical(apply_column_dfl(matrixset(NULL), mean), NULL)


  student_results2 <- student_results
  matrix_elm(student_results2,2) <- NULL
  ct <- apply_row_dfl(student_results2, mn=mean, md=median)
  M <- matrix_elm(student_results2,1)
  ct_ref <- list(failure=t(apply(M, 1, function(u) c(mn=mean(u), md=median(u)),simplify = TRUE)))
  ct_ref$failure <- tibble::as_tibble(ct_ref$failure, rownames = ".rowname")
  ct_ref <- c(ct_ref, remedial = list(NULL))
  # ct_ref$remedial <- tibble::tibble(.rowname = character(), mn = logical(), md = logical())
  expect_identical(ct, ct_ref)



  student_results2 <- student_results
  matrix_elm(student_results2,2) <- NULL
  ct <- apply_column_dfl(student_results2, mn=mean, md=median)
  M <- matrix_elm(student_results2,1)
  ct_ref <- list(failure=t(apply(M, 2, function(u) c(mn=mean(u), md=median(u)),simplify = TRUE)))
  ct_ref$failure <- tibble::as_tibble(ct_ref$failure, rownames = ".colname")
  ct_ref <- c(ct_ref, remedial = list(NULL))
  # ct_ref$remedial <- tibble::tibble(.colname = character(), mn = logical(), md = logical())
  expect_identical(ct, ct_ref)



  ct <- apply_row_dfl(student_results, mn=mean, md=median)
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  ct_ref <- lapply(M,
                   function(m) tibble::tibble(.rowname = rownames(m),
                                              mn=unname(rowMeans(m)),
                                              md = unname(apply(m,1,median))))
  expect_equal(ct, ct_ref)



  ct <- apply_column_dfl(student_results, mn=mean, md=median)
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  ct_ref <- lapply(M,
                   function(m) tibble::tibble(.colname = colnames(m),
                                              mn=unname(colMeans(m)),
                                              md = unname(apply(m,2,median))))
  expect_equal(ct, ct_ref)



  # showcase > 1 length answer
  summ <- apply_row_dfl(student_results, mn=~c(mean(.i), median(.i)), rg=~range(.i))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
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



  summ <- apply_column_dfl(student_results, mn=~c(mean(.j), median(.j)), rg=~range(.j))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
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



  summ <- apply_row_dfl(student_results, mn=~c(mn=mean(.i), md=median(.i)), rg=~range(.i))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
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




  summ <- apply_column_dfl(student_results, mn=~c(mn=mean(.j), md=median(.j)), rg=~range(.j))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
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
  expect_error(apply_row_dfl(student_results,
                            mn=mean,
                            reg = ~lm(.i ~ national_average + program)),
               "vectors must be of the same length")




  expect_error(apply_column_dfl(student_results,
                            mn=mean,
                            reg = ~lm(.j ~ teacher + previous_year_score)),
               "vectors must be of the same length")




  # the trick
  summ <- apply_row_dfl(student_results, mn=mean, reg = ~list(lm(.i ~ national_average + program)))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- column_info(student_results)
  summ_ref <- lapply(M,
                     function(m) {
                       tibble::tibble(.rowname = rownames(m),
                                                 mn=unname(rowMeans(m)),
                                                 reg = unname(apply(m,1,function(x) {
                                                   meta$.i <- x
                                                   eval(quote(lm(.i ~ national_average + program)), envir = meta)
                                                 })))
                       })
  expect_equal(summ, summ_ref, ignore_attr = TRUE)




  summ <- apply_column_dfl(student_results, mn=mean, reg = ~list(lm(.j ~ teacher + previous_year_score)))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- row_info(student_results)
  summ_ref <- lapply(M,
                     function(m) {
                       tibble::tibble(.colname = colnames(m),
                                      mn=unname(colMeans(m)),
                                      reg = unname(apply(m,2,function(x) {
                                        meta$.j <- x
                                        eval(quote(lm(.j ~ teacher + previous_year_score)), envir = meta)
                                      })))
                     })
  expect_equal(summ, summ_ref, ignore_attr = TRUE)




  summ <- apply_row_dfl(student_results, reg = ~list(lm(.i ~ national_average), lm(.i ~ program)))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- column_info(student_results)
  summ_ref <- lapply(M,
                     function(m) {
                       u <- tibble::tibble(
                         .rowname = rownames(student_results),
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




  summ <- apply_column_dfl(student_results, reg = ~ list(lm(.j ~ class), lm(.j ~ teacher)))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- row_info(student_results)
  summ_ref <- lapply(M,
                     function(m) {
                       u <- tibble::tibble(
                         .colname = colnames(student_results),
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
  expect_error(apply_row_dfl(student_results, mn=~mean(.i), rg=~range(.i)),
               "vectors must be of the same length")


  expect_error(apply_column_dfl(student_results, mn=~mean(.j), rg=~range(.j)),
               "vectors must be of the same length")


  expect_error(apply_column_dfl(student_results, .colname = mean))
  expect_error(apply_column_dfw(student_results, .colname = mean))
  expect_error(apply_row_dfl(student_results, .rowname = mean))
  expect_error(apply_row_dfw(student_results, .rowname = mean))




  # grouped
  grmn <- apply_row_dfl(column_group_by(student_results, program), mean, median)
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mean=mean(u), median = median(u)),simplify = FALSE)
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_longer(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_wider(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) {
   u <- u[, c("program", ".rows_id", "mean", "median")]
   colnames(u)[2] <- ".rowname"
   u
  })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column_dfl(row_group_by(student_results, teacher, class), mean, median)
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mean=mean(u), median = median(u)),simplify = FALSE)
    })
    ans$.columns <- mn_ref
    ans
  })
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_longer(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_wider(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) {
    u <- u[, c("teacher", "class", ".columns_id", "mean", "median")]
    colnames(u)[3] <- ".colname"
    u
  })
  names(grmn_ref) <- matrixnames(student_results)
  expect_identical(grmn, grmn_ref)




  grmn <- apply_row_dfl(column_group_by(student_results, program), mn=mean, md=~median(.i))
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mn=mean(u), md = median(u)),simplify = FALSE)
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_longer(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_wider(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) {
    u <- u[, c("program", ".rows_id", "mn", "md")]
    colnames(u)[2] <- ".rowname"
    u
  })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column_dfl(row_group_by(student_results, teacher, class), mn=mean, md=~median(.j))
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mn=mean(u), md = median(u)),simplify = FALSE)
    })
    ans$.columns <- mn_ref
    ans
  })
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_longer(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_wider(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) {
    u <- u[, c("teacher", "class", ".columns_id", "mn", "md")]
    colnames(u)[3] <- ".colname"
    u
  })
  names(grmn_ref) <- matrixnames(student_results)
  expect_identical(grmn, grmn_ref)



  grmn <- apply_row_dfl(column_group_by(student_results, program), ct=~c(mean(.i), median(.i)))
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) tibble::tibble(ct.name = c("..1", "..2"), ct=c(mean(u), median(u))),simplify = FALSE)
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_longer(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_wider(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest(u, c(ct.name, ct)))
  mn_ref <- lapply(mn_ref, function(u) {
    u <- u[, c("program", ".rows_id", "ct.name", "ct")]
    colnames(u)[2] <- ".rowname"
    u
  })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)



  grmn <- apply_column_dfl(row_group_by(student_results, teacher, class), ct=~c(mean(.j), median(.j)))
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results,m)
      apply(M[gr, , drop = FALSE], 2, function(u) tibble::tibble(ct.name = c("..1", "..2"), ct=c(mean(u), median(u))),simplify = FALSE)
    })
    ans$.columns <- mn_ref
    ans
  })
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_longer(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_wider(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest(u, c(ct.name, ct)))
  grmn_ref <- lapply(grmn_ref, function(u) {
    u <- u[, c("teacher", "class", ".columns_id", "ct.name", "ct")]
    colnames(u)[3] <- ".colname"
    u
  })
  names(grmn_ref) <- matrixnames(student_results)
  expect_identical(grmn, grmn_ref)

})








test_that("matrixset 'wide' loop works", {

  withr::local_options(lifecycle_verbosity = "quiet")

  # with null
  expect_identical(apply_row_dfw(matrixset(NULL), mean), NULL)
  expect_identical(apply_column_dfw(matrixset(NULL), mean), NULL)


  student_results2 <- student_results
  matrix_elm(student_results2,2) <- NULL
  ct <- apply_row_dfw(student_results2, mn=mean, md=median)
  M <- matrix_elm(student_results2,1)
  ct_ref <- list(failure=t(apply(M, 1, function(u) c(mn=mean(u), md=median(u)),simplify = TRUE)))
  ct_ref$failure <- tibble::as_tibble(ct_ref$failure, rownames = ".rowname")
  ct_ref <- c(ct_ref, remedial = list(NULL))
  # ct_ref$remedial <- tibble::tibble(.rowname = character(), mn = logical(), md = logical())
  expect_identical(ct, ct_ref)



  student_results2 <- student_results
  matrix_elm(student_results2,2) <- NULL
  ct <- apply_column_dfw(student_results2, mn=mean, md=median)
  M <- matrix_elm(student_results2,1)
  ct_ref <- list(failure=t(apply(M, 2, function(u) c(mn=mean(u), md=median(u)),simplify = TRUE)))
  ct_ref$failure <- tibble::as_tibble(ct_ref$failure, rownames = ".colname")
  ct_ref <- c(ct_ref, remedial = list(NULL))
  # ct_ref$remedial <- tibble::tibble(.colname = character(), mn = logical(), md = logical())
  expect_identical(ct, ct_ref)



  ct <- apply_row_dfw(student_results, mn=mean, md=median)
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  ct_ref <- lapply(M,
                   function(m) tibble::tibble(.rowname = rownames(m),
                                              mn=unname(rowMeans(m)),
                                              md = unname(apply(m,1,median))))
  expect_equal(ct, ct_ref)



  ct <- apply_column_dfw(student_results, mn=mean, md=median)
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  ct_ref <- lapply(M,
                   function(m) tibble::tibble(.colname = colnames(m),
                                              mn=unname(colMeans(m)),
                                              md = unname(apply(m,2,median))))
  expect_equal(ct, ct_ref)



  # showcase > 1 length answer
  summ <- apply_row_dfw(student_results, mn=~c(mean(.i), median(.i)), rg=~range(.i))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       dplyr::bind_cols(tibble::as_tibble(t(apply(m,1, function(x) c(`mn ..1`=mean(x), `mn ..2`=median(x)))), rownames = ".rowname"),
                                        tibble::as_tibble(t(apply(m,1, function(x) setNames(range(x), c("rg ..1", "rg ..2"))))))
                     })
  expect_identical(summ, summ_ref)




  summ <- apply_column_dfw(student_results, mn=~c(mean(.j), median(.j)), rg=~range(.j))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       dplyr::bind_cols(tibble::as_tibble(t(apply(m,2, function(x) c(`mn ..1`=mean(x), `mn ..2`=median(x)))), rownames = ".colname"),
                                        tibble::as_tibble(t(apply(m,2, function(x) setNames(range(x), c("rg ..1", "rg ..2"))))))
                     })
  expect_identical(summ, summ_ref)




  summ <- apply_row_dfw(student_results, mn=~c(mn=mean(.i), md=median(.i)), rg=~range(.i))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       dplyr::bind_cols(tibble::as_tibble(t(apply(m,1, function(x) c(`mn mn`=mean(x), `mn md`=median(x)))), rownames = ".rowname"),
                                        tibble::as_tibble(t(apply(m,1, function(x) setNames(range(x), c("rg ..1", "rg ..2"))))))
                     })
  expect_identical(summ, summ_ref)




  summ <- apply_column_dfw(student_results, mn=~c(mn=mean(.j), md=median(.j)), rg=~range(.j))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  summ_ref <- lapply(M,
                     function(m) {
                       dplyr::bind_cols(tibble::as_tibble(t(apply(m,2, function(x) c(`mn mn`=mean(x), `mn md`=median(x)))), rownames = ".colname"),
                                        tibble::as_tibble(t(apply(m,2, function(x) setNames(range(x), c("rg ..1", "rg ..2"))))))
                     })
  expect_identical(summ, summ_ref)




  # error
  expect_error(apply_row_dfw(student_results,
                            mn=mean,
                            reg = ~lm(.i ~ national_average + program)),
               "vectors must be of the same length")




  expect_error(apply_column_dfw(student_results,
                               mn=mean,
                               reg = ~lm(.j ~ teacher + previous_year_score)),
               "vectors must be of the same length")




  # the trick
  summ <- apply_row_dfw(student_results, mn=mean, reg = ~list(lm(.i ~ national_average + program)))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- column_info(student_results)
  summ_ref <- lapply(M,
                     function(m) {
                       tibble::tibble(.rowname = rownames(m),
                                      mn=unname(rowMeans(m)),
                                      reg = unname(apply(m,1,function(x) {
                                        meta$.i <- x
                                        eval(quote(lm(.i ~ national_average + program)), envir = meta)
                                      })))
                     })
  expect_equal(summ, summ_ref, ignore_attr = TRUE)




  summ <- apply_column_dfw(student_results, mn=mean, reg = ~list(lm(.j ~ teacher + previous_year_score)))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- row_info(student_results)
  summ_ref <- lapply(M,
                     function(m) {
                       tibble::tibble(.colname = colnames(m),
                                      mn=unname(colMeans(m)),
                                      reg = unname(apply(m,2,function(x) {
                                        meta$.j <- x
                                        eval(quote(lm(.j ~ teacher + previous_year_score)), envir = meta)
                                      })))
                     })
  expect_equal(summ, summ_ref, ignore_attr = TRUE)



  summ <- apply_row_dfw(student_results, reg = ~list(national = lm(.i ~ national_average), program = lm(.i ~ program)))
  M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  meta <- column_info(student_results)
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



  # summ <- apply_column_dfw(student_results, reg = list(teacher = lm(.j ~ teacher), score = lm(.j ~ previous_year_score)))
  # M <- student_results[,,,keep_annotation = FALSE, warn_class_change = FALSE]
  # meta <- column_info(student_results)
  # summ_ref <- lapply(M,
  #                    function(m) {
  #                      tibble::tibble(.rowname = rownames(m),
  #                                     `reg national` = unname(apply(m,1,function(x) {
  #                                       meta$.i <- x
  #                                       eval(quote(lm(.i ~ national_average)), envir = meta)
  #                                     })),
  #                                     `reg program` = unname(apply(m,1,function(x) {
  #                                       meta$.i <- x
  #                                       eval(quote(lm(.i ~ program)), envir = meta)
  #                                     })))
  #                    })
  # expect_equal(summ, summ_ref, ignore_attr = TRUE)


  #
  # # this should fail
  expect_error(apply_row_dfw(student_results, mn=~mean(.i), rg=~range(.i)),
               "vectors must be of the same length")


  expect_error(apply_column_dfw(student_results, mn=~mean(.j), rg=~range(.j)),
               "vectors must be of the same length")







  # grouped
  grmn <- apply_row_dfw(column_group_by(student_results, program), mean, median)
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mean=mean(u), median = median(u)),simplify = FALSE)
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_longer(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_wider(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) {
    u <- u[, c("program", ".rows_id", "mean", "median")]
    colnames(u)[2] <- ".rowname"
    u
  })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column_dfw(row_group_by(student_results, teacher, class), mean, median)
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mean=mean(u), median = median(u)),simplify = FALSE)
    })
    ans$.columns <- mn_ref
    ans
  })
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_longer(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_wider(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) {
    u <- u[, c("teacher", "class", ".columns_id", "mean", "median")]
    colnames(u)[3] <- ".colname"
    u
  })
  names(grmn_ref) <- matrixnames(student_results)
  expect_identical(grmn, grmn_ref)




  grmn <- apply_row_dfw(column_group_by(student_results, program), mn=mean, md=~median(.i))
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       apply(M[, gr, drop = FALSE], 1, function(u) list(mn=mean(u), md = median(u)),simplify = FALSE)
                     })
                     ans$.rows <- grmn_ref
                     ans
                   })
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_longer(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) tidyr::unnest_wider(u, .rows))
  mn_ref <- lapply(mn_ref, function(u) {
    u <- u[, c("program", ".rows_id", "mn", "md")]
    colnames(u)[2] <- ".rowname"
    u
  })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)




  grmn <- apply_column_dfw(row_group_by(student_results, teacher, class), mn=mean, md=~median(.j))
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results,m)
      apply(M[gr, , drop = FALSE], 2, function(u) list(mn=mean(u), md = median(u)),simplify = FALSE)
    })
    ans$.columns <- mn_ref
    ans
  })
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_longer(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) tidyr::unnest_wider(u, .columns))
  grmn_ref <- lapply(grmn_ref, function(u) {
    u <- u[, c("teacher", "class", ".columns_id", "mn", "md")]
    colnames(u)[3] <- ".colname"
    u
  })
  names(grmn_ref) <- matrixnames(student_results)
  expect_identical(grmn, grmn_ref)



  grmn <- apply_row_dfw(column_group_by(student_results, program), ct=~c(mean(.i), median(.i)))
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
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
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)



  grmn <- apply_column_dfw(row_group_by(student_results, teacher, class), ct=~c(mean(.j), median(.j)))
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  grmn_ref <- lapply(seq(nmatrix(student_results)), function(m) {
    ans <- grs
    ans$.rows <- NULL
    mn_ref <- lapply(grs$.rows, function(gr) {
      M <- matrix_elm(student_results,m)
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
  names(grmn_ref) <- matrixnames(student_results)
  expect_identical(grmn, grmn_ref)



  grmn <- apply_row_dfw(column_group_by(student_results, program),
                       ct=~c(mn=mean(.i), md=median(.i)),
                       rg=range,
                       fit=~list(lm(.i ~ 1), lm(.i ~ school_average)))
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       meta <- column_info(student_results)
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
                       names(rows) <- rownames(student_results)
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
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref, ignore_attr = TRUE)



  grmn <- apply_column_dfw(row_group_by(student_results, teacher, class),
                          ct=~c(mn=mean(.j), md=median(.j)),
                          rg=range,
                          fit=~list(lm(.j ~ 1), lm(.j ~ previous_year_score)))
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       meta <- row_info(student_results)
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
                       names(cols) <- colnames(student_results)
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
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref, ignore_attr = TRUE)

})








test_that("matrixset matrix loop works", {

  withr::local_options(lifecycle_verbosity = "quiet")

  expect_identical(apply_matrix(matrixset(NULL), mean), NULL)

  student_results2 <- student_results
  matrix_elm(student_results2,2) <- NULL
  mn <- apply_matrix(student_results2, mean)
  M <- matrix_elm(student_results2,1)
  # mn_ref <- list(failure=list(mean=mean(M)), remedial=list(mean=NULL))
  mn_ref <- list(failure=list(mean=mean(M)), remedial=NULL)

  expect_equal(mn, mn_ref)




  mn <- apply_matrix(student_results, mean)
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     M <- matrix_elm(student_results,m)
                     list(mean=mean(M))
                   })
  names(mn_ref) <- matrixnames(student_results)

  expect_equal(mn, mn_ref)



  mn <- apply_matrix(student_results, mn=mean)
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     M <- matrix_elm(student_results,m)
                     list(mn=mean(M))
                   })
  names(mn_ref) <- matrixnames(student_results)

  expect_equal(mn, mn_ref)



  ct <- apply_matrix(student_results, mn=mean, md=~median(.m))
  ct_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     M <- matrix_elm(student_results,m)
                     list(mn=mean(M), md=median(M))
                   })
  names(ct_ref) <- matrixnames(student_results)

  expect_equal(ct, ct_ref)



  ct <- apply_matrix(student_results, mn=mean, reg = ~lm(.m ~ teacher + class))
  ct_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     .m <- matrix_elm(student_results,m)
                     meta <- row_info(student_results)
                     meta <- tibble::column_to_rownames(meta, ".rowname")
                     ans <- list(mn=mean(.m),
                                 reg=eval(quote(lm(.m ~ teacher + class)), envir = meta))
                     ans
                   })
  names(ct_ref) <- matrixnames(student_results)

  expect_equal(ct, ct_ref, ignore_attr = TRUE)



  ct <- apply_matrix(student_results, mn=mean, reg = ~lm(.m ~ teacher + class),
                 .matrix = 2)
  ct_ref <- lapply(2,
                   function(m) {
                     .m <- matrix_elm(student_results,m)
                     meta <- row_info(student_results)
                     meta <- tibble::column_to_rownames(meta, ".rowname")
                     ans <- list(mn=mean(.m),
                                 reg=eval(quote(lm(.m ~ teacher + class)), envir = meta))
                     ans
                   })
  names(ct_ref) <- matrixnames(student_results)[2]

  expect_equal(ct, ct_ref, ignore_attr = TRUE)



  e <- apply_matrix(student_results,
                   mn = ~{
                     mm <- .m
                     mean(mm)
                   })

  e_ref <- lapply(seq(nmatrix(student_results)),
                  function(m) {
                    M <- matrix_elm(student_results,m)
                    list(mn=mean(M))
                  })
  names(e_ref) <- matrixnames(student_results)

  expect_equal(e, e_ref, ignore_attr = TRUE)



  e <- apply_matrix(student_results,
                   s=~{
                     .m + school_average + previous_year_score
                   })

  e_ref <- lapply(seq(nmatrix(student_results)),
                  function(m) {
                    .m <- matrix_elm(student_results,m)
                    row_meta <- row_info(student_results)
                    col_meta <- column_info(student_results)
                    s <- list(s=.m + row_meta$previous_year_score + col_meta$school_average)
                    s
                  })
  names(e_ref) <- matrixnames(student_results)

  expect_equal(e, e_ref, ignore_attr = TRUE)




  fc <- apply_matrix_dfl(student_results, FC = ~rowMeans(.m),
                         FC_rob = ~apply(.m, 1, median))
  fc_ref <- lapply(seq(nmatrix(student_results)),
                  function(m) {
                    .m <- matrix_elm(student_results,m)
                    .fc <- do.call(cbind,
                                   list(FC = rowMeans(.m), FC_rob = apply(.m, 1, median)))
                    .fc <- tibble::as_tibble(.fc, rownames = "FC.name")
                    .fc <- dplyr::mutate(.fc, FC_rob.name = FC.name)
                    .fc[, c("FC.name", "FC", "FC_rob.name", "FC_rob")]
                  })
  names(fc_ref) <- matrixnames(student_results)
  expect_identical(fc, fc_ref)


  expect_error(apply_matrix_dfl(student_results, FC = ~rowMeans(.m),
                                FC_rob = ~apply(.m, 2, median)),
               "vectors must be of the same length")




  fc <- apply_matrix_dfw(student_results, FC = ~rowMeans(.m),
                         FC_rob = ~apply(.m, 1, median))
  fc_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     .m <- matrix_elm(student_results,m)
                     .fc <- do.call(cbind,
                                    list(FC = rowMeans(.m), FC_rob = apply(.m, 1, median)))
                     .fc <- tibble::as_tibble(.fc, rownames = "FC.name")
                     .fc <- dplyr::mutate(.fc, FC_rob.name = FC.name)
                     .fc <- tidyr::pivot_wider(.fc,
                                               names_from = c(FC.name, FC_rob.name),
                                               values_from = c(FC, FC_rob),
                                               names_glue = "{.value} {FC.name}")
                     # .fc[, c("FC.name", "FC", "FC_rob.name", "FC_rob")]
                   })
  names(fc_ref) <- matrixnames(student_results)
  expect_identical(fc, fc_ref)




  # .data
  rg <- apply_matrix(student_results, reg = ~unname(coef(lm(.m ~ .data[["teacher"]] + class))))
  rg_ref <- apply_matrix(student_results, reg = ~unname(coef(lm(.m ~ teacher + class))))
  expect_identical(rg, rg_ref)



  previous_year_score <- 0.5
  avr <- apply_matrix(student_results, av=~mean(c(.m, previous_year_score)))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                    function(m) {
                      M <- matrix_elm(student_results,m)
                      row_meta <- row_info(student_results)
                      s <- list(av=mean(c(M, row_meta$previous_year_score)))
                      s
                    })
  names(avr_ref) <- matrixnames(student_results)
  expect_identical(avr, avr_ref)



  previous_year_score <- 0.5
  avr <- apply_matrix(student_results, av=~mean(c(.m, .env$previous_year_score)))
  avr_ref <- lapply(seq(nmatrix(student_results)),
                    function(m) {
                      M <- matrix_elm(student_results,m)
                      s <- list(av=mean(c(M, previous_year_score)))
                      s
                    })
  names(avr_ref) <- matrixnames(student_results)
  expect_identical(avr, avr_ref)




  # grouped


  grmn <- apply_matrix(column_group_by(student_results, program), mean)
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       list(mean=mean(M[, gr]))
                     })
                     ans$.rows <- NULL
                     ans$.vals <- grmn_ref
                     ans
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)




  grmn <- apply_matrix(column_group_by(student_results, program), mn=mean)
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       list(mn=mean(M[, gr]))
                     })
                     ans$.rows <- NULL
                     ans$.vals <- grmn_ref
                     ans
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)




  grmn <- apply_matrix(row_group_by(student_results, teacher, class), mean, rg=range)
  grs <- row_group_meta(row_group_by(student_results, teacher, class))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       list(mean=mean(M[gr, ]), rg=range(M[gr, ]))
                     })
                     ans$.rows <- NULL
                     ans$.vals <- grmn_ref
                     ans
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)



  grmn <- apply_matrix(column_group_by(row_group_by(student_results, teacher, class), program), mean, rg=range)
  grs_row <- row_group_meta(row_group_by(student_results, teacher, class))
  grs_col <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     M <- matrix_elm(student_results,m)
                     grmn_ref <- lapply(grs_row$.rows, function(grr) {
                       lapply(grs_col$.rows, function(grc) {
                         list(mean=mean(M[grr, grc]), rg=range(M[grr, grc]))
                       })
                     })
                     unlist(grmn_ref, recursive = FALSE)
                     # grmn_ref
                     # ans$.rows <- NULL
                     # ans$.mats <- grmn_ref
                     # ans
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn$failure$.vals, mn_ref$failure)
  expect_identical(grmn$remedial$.vals, mn_ref$remedial)



  grmn <- apply_matrix_dfl(column_group_by(student_results, program), mn=mean)
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       mean(M[, gr])
                     })
                     ans$.rows <- NULL
                     ans$mn <- grmn_ref
                     ans %>% tidyr::unnest(mn)
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)


  grmn <- apply_matrix_dfl(column_group_by(student_results, program), FC = ~colMeans(.m),
                           FC_rob = ~apply(.m, 2, median), .force_name = TRUE)
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       Mgr <- M[, gr, drop = FALSE]
                       colMeans(Mgr)
                     })
                     ans$FC <- grmn_ref

                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       Mgr <- M[, gr, drop = FALSE]
                       apply(Mgr, 2, median)

                     })
                     ans$FC_rob <- grmn_ref
                     ans$.rows <- NULL
                     ans %>% tidyr::unnest_longer(c(FC, FC_rob)) %>%
                       dplyr::select(program, FC.name = FC_id, FC,
                                     FC_rob.name = FC_rob_id, FC_rob) %>%
                       dplyr::mutate(FC = unname(FC), FC_rob = unname(FC_rob))
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)


  grmn <- apply_matrix_dfl(column_group_by(student_results, program), FC = ~rowMeans(.m))
  grs <- column_group_meta(column_group_by(student_results, program))
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       rowMeans(M[, gr, drop = FALSE])
                     })
                     ans$.rows <- NULL
                     ans$FC <- grmn_ref
                     ans %>% tidyr::unnest_longer(FC) %>%
                       dplyr::select(program, FC.name=FC_id, FC) %>%
                       dplyr::mutate(FC=unname(FC))
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)


  grfc <- apply_matrix_dfl(row_group_by(student_results, class, teacher), FC = ~colMeans(.m))
  grs <- row_group_meta(row_group_by(student_results, class, teacher))
  fc_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       colMeans(M[gr, , drop = FALSE])
                     })
                     ans$.rows <- NULL
                     ans$FC <- grmn_ref
                     ans %>% tidyr::unnest_longer(FC) %>%
                       dplyr::select(class, teacher, FC.name=FC_id, FC) %>%
                       dplyr::mutate(FC=unname(FC))
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)



  grfc <- apply_matrix_dfl(row_group_by(student_results, class, teacher), FC = ~rowMeans(.m),
                           FC_rob = ~apply(.m, 1, mean))
  grs <- row_group_meta(row_group_by(student_results, class, teacher))
  fc_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs
                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       rowMeans(M[gr, , drop = FALSE])
                     })
                     ans$FC <- grmn_ref

                     grmn_ref <- lapply(grs$.rows, function(gr) {
                       M <- matrix_elm(student_results,m)
                       apply(M[gr, , drop = FALSE], 1, mean)
                     })
                     ans$.rows <- NULL
                     ans$FC_rob <- grmn_ref
                     ans %>% tidyr::unnest_longer(c(FC, FC_rob)) %>%
                       dplyr::select(class, teacher, FC.name=FC_id, FC,
                                     FC_rob.name=FC_rob_id, FC_rob) %>%
                       dplyr::mutate(FC=unname(FC), FC_rob=unname(FC_rob))
                   })
  names(mn_ref) <- matrixnames(student_results)
  expect_identical(grmn, mn_ref)


  grfc <- apply_matrix_dfl(column_group_by(row_group_by(student_results, teacher), program),
                           FC = ~colMeans(.m),
                           FC_rob = ~apply(.m, 2, median), .force_name = TRUE)
  grs <- column_group_by(row_group_by(student_results, teacher), program)
  grs_row <- row_group_meta(grs)
  grs_col <- column_group_meta(grs)
  fc_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     ans <- grs_row
                     M <- matrix_elm(student_results,m)
                     ans$.rows <- NULL
                     grmn_ref <- lapply(grs_row$.rows, function(grr) {
                       lapply(grs_col$.rows, function(grc) {
                         colMeans(M[grr, grc, drop = FALSE])
                       })
                     })
                     ans$FC <- grmn_ref

                     grmn_ref <- lapply(grs_row$.rows, function(grr) {
                       lapply(grs_col$.rows, function(grc) {
                         apply(M[grr, grc, drop = FALSE], 2, median)
                       })
                     })
                     ans$FC_rob <- grmn_ref

                     ans %>%
                       tidyr::unnest(c(FC, FC_rob)) %>%
                       tidyr::unnest_longer(c(FC, FC_rob)) %>%
                       dplyr::left_join(column_info(student_results) %>%
                                          dplyr::select(FC_id=.colname, program),
                                        by = "FC_id") %>%
                       dplyr::select(teacher, program, FC.name=FC_id, FC,
                                     FC_rob.name=FC_rob_id, FC_rob) %>%
                       dplyr::mutate(FC=unname(FC), FC_rob=unname(FC_rob))
                   })
  names(fc_ref) <- matrixnames(student_results)
  expect_identical(grfc, fc_ref)





  #dfw
  grmn <- apply_matrix_dfw(column_group_by(student_results, program), mn=mean)
  mn_ref <- apply_matrix_dfl(column_group_by(student_results, program), mn=mean)
  expect_identical(grmn, mn_ref)




  grmn <- apply_matrix_dfw(column_group_by(student_results, program), FC = ~colMeans(.m),
                           FC_rob = ~apply(.m, 2, median), .force_name = TRUE)
  mn_ref <- apply_matrix_dfl(column_group_by(student_results, program), FC = ~colMeans(.m),
                             FC_rob = ~apply(.m, 2, median), .force_name = TRUE) %>%
    lapply(function(m) {
      m %>% tidyr::pivot_wider(names_from = c(FC.name, FC_rob.name),
                               values_from = c(FC, FC_rob),
                               names_glue = "{.value} {FC.name}")
    })
  nms <- unlist(unique(lapply(grmn, names)))
  nms_ref <- unlist(unique(lapply(mn_ref, names)))
  expect_true(all(nms %in% nms_ref))
  expect_true(all(nms_ref %in% nms))
  mn_ref <- purrr::map2(mn_ref, grmn, ~ .x[, names(.y)])
  expect_identical(grmn, mn_ref)


  fc <- apply_matrix_dfw(column_group_by(student_results, program), FC = ~rowMeans(.m))
  fc_ref <- apply_matrix_dfl(column_group_by(student_results, program), FC = ~rowMeans(.m)) %>%
    lapply(function(m) {
      m %>% tidyr::pivot_wider(names_from = FC.name, values_from = FC,
                               names_glue = "{.value} {FC.name}")
    })
  expect_identical(fc, fc_ref)



  fc <- apply_matrix_dfw(row_group_by(student_results, class, teacher),
                         FC = ~colMeans(.m))
  fc_ref <- apply_matrix_dfl(row_group_by(student_results, class, teacher),
                             FC = ~colMeans(.m)) %>%
    lapply(function(m) {
      m %>% tidyr::pivot_wider(names_from = FC.name, values_from = FC,
                               names_glue = "{.value} {FC.name}")
    })
  expect_identical(fc, fc_ref)


  mn <- apply_matrix_dfw(row_group_by(student_results, class, teacher), FC = ~rowMeans(.m),
                         FC_rob = ~rowMeans(.m))
  mn_ref <- apply_matrix_dfl(row_group_by(student_results, class, teacher), FC = ~rowMeans(.m),
                             FC_rob = ~rowMeans(.m)) %>%
    lapply(function(m) {
      m %>% tidyr::pivot_wider(names_from = c(FC.name, FC_rob.name),
                               values_from = c(FC, FC_rob),
                               names_glue = "{.value} {FC.name}")
    })
  nms <- unlist(unique(lapply(mn, names)))
  nms_ref <- unlist(unique(lapply(mn_ref, names)))
  expect_true(all(nms %in% nms_ref))
  expect_true(all(nms_ref %in% nms))
  mn_ref <- purrr::map2(mn_ref, mn, ~ .x[, names(.y)])
  expect_identical(mn, mn_ref)


  ct <- apply_matrix_dfw(column_group_by(row_group_by(student_results, teacher), program),
                         FC = ~colMeans(.m),
                         FC_rob = ~apply(.m, 2, median), .force_name = TRUE)
  ct_ref <- apply_matrix_dfl(column_group_by(row_group_by(student_results, teacher), program),
                             FC = ~colMeans(.m),
                             FC_rob = ~apply(.m, 2, median), .force_name = TRUE) %>%
    lapply(function(m) {
      m %>% tidyr::pivot_wider(names_from = c(FC.name, FC_rob.name),
                               values_from = c(FC, FC_rob),
                               names_glue = "{.value} {FC.name}")
    })
  nms <- unlist(unique(lapply(ct, names)))
  nms_ref <- unlist(unique(lapply(ct_ref, names)))
  expect_true(all(nms %in% nms_ref))
  expect_true(all(nms_ref %in% nms))
  ct_ref <- purrr::map2(ct_ref, ct, ~ .x[, names(.y)])
  expect_identical(ct, ct_ref)

})




