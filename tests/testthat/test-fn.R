test_that("matrixset general loop works", {

  mn <- row_loop(student_results, mean)
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     M <- matrix_elm(student_results,m)
                     apply(M, 1, function(u) list(mean=mean(u)),simplify = FALSE)
                   })
  names(mn_ref) <- matrixnames(student_results)

  expect_equal(mn, mn_ref)



  mn <- row_loop(student_results, mn=mean)
  mn_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     M <- matrix_elm(student_results,m)
                     apply(M, 1, function(u) list(mn=mean(u)),simplify = FALSE)
                   })
  names(mn_ref) <- matrixnames(student_results)

  expect_equal(mn, mn_ref)



  ct <- row_loop(student_results, mn=mean, md=median(.i))
  ct_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     M <- matrix_elm(student_results,m)
                     apply(M, 1, function(u) list(mn=mean(u),
                                                  md=median(u)),
                           simplify = FALSE)
                   })
  names(ct_ref) <- matrixnames(student_results)

  expect_equal(ct, ct_ref)



  ct <- row_loop(student_results, mn=mean, reg = lm(.i ~ national_average + program))
  ct_ref <- lapply(seq(nmatrix(student_results)),
                   function(m) {
                     M <- matrix_elm(student_results,m)
                     meta <- column_info(student_results)
                     meta <- tibble::column_to_rownames(meta, ".colname")
                     ans <- lapply(1:nrow(student_results),
                            function(i) {
                              meta$.i <- M[i,]
                              list(mn=mean(M[i,]),
                                   reg=eval(quote(lm(.i ~ national_average + program)), envir = meta))
                            })
                     names(ans) <- rownames(student_results)
                     ans
                   })
  names(ct_ref) <- matrixnames(student_results)

  expect_equal(ct, ct_ref, ignore_attr = TRUE)



  ct <- row_loop(student_results, mn=mean, reg = lm(.i ~ national_average + program),
                 .matrix = 2)
  ct_ref <- lapply(2,
                   function(m) {
                     M <- matrix_elm(student_results,m)
                     meta <- column_info(student_results)
                     meta <- tibble::column_to_rownames(meta, ".colname")
                     ans <- lapply(1:nrow(student_results),
                                   function(i) {
                                     meta$.i <- M[i,]
                                     list(mn=mean(M[i,]),
                                          reg=eval(quote(lm(.i ~ national_average + program)), envir = meta))
                                   })
                     names(ans) <- rownames(student_results)
                     ans
                   })
  names(ct_ref) <- matrixnames(student_results)[2]

  expect_equal(ct, ct_ref, ignore_attr = TRUE)




  e <- row_loop(student_results,
                mn = {
                  ii <- .i
                  mean(ii)
                })

  e_ref <- lapply(seq(nmatrix(student_results)),
                  function(m) {
                    M <- matrix_elm(student_results,m)
                    apply(M, 1, function(u) list(mn=mean(u)),simplify = FALSE)
                  })
  names(mn_ref) <- matrixnames(student_results)

  expect_equal(ct, ct_ref, ignore_attr = TRUE)




  e <- row_loop(student_results,
                s={
                  .i + school_average + previous_year_score
                })

  e_ref <- lapply(seq(nmatrix(student_results)),
                  function(m) {
                    M <- matrix_elm(student_results,m)
                    row_meta <- row_info(student_results)
                    col_meta <- column_info(student_results)
                    s <- lapply(1:nrow(M), function(i) i + row_meta$previous_year_score[i] + col_meta$school_average)
                    names(s) <- rownames(student_results)
                    s
                  })
  names(mn_ref) <- matrixnames(student_results)

  expect_equal(ct, ct_ref, ignore_attr = TRUE)






  # .data

})
