test_that("matrixset filtering works for Matrix", {

  amat <- Matrix::Matrix(1:24, 4, 6)
  bmat <- Matrix::Matrix(101:124, 4, 6)
  rownames(amat) <- rownames(bmat) <- paste0("r", 1:4)
  colnames(amat) <- colnames(bmat) <- paste0("c", 1:6)
  ri <- data.frame(rowname = paste0("r", 1:4), g = 1:4, h = letters[1:4],
                   stringsAsFactors = F)
  ci <- data.frame(colname = paste0("c", 1:6), gg = 1:6,
                   foo = c("a", "a", "a", "b", "b", "b"),
                   bar = c("u", "v", "u", "v", "u", "v"),
                   stringsAsFactors = F)
  matset <- matrixset(a=amat, b=bmat, row_info = ri, column_info = ci,
                      row_tag = "ROW", column_tag = "COL")
  mat_filt2 <- matrixset(a=amat[3:4,], b=bmat[3:4,], row_info=ri[3:4,],
                         column_info=ci, row_tag = "ROW", column_tag = "COL")
  mat_filt2_null <- matrixset(a=amat[3:4,], b=NULL, row_info=ri[3:4,],
                              column_info=ci, row_tag = "ROW", column_tag = "COL")
  mat_filtmax <- matrixset(a=amat[4,,drop=FALSE], b=bmat[4,,drop=FALSE],
                           row_info=ri[4,], column_info=ci, row_tag = "ROW",
                           column_tag = "COL")
  mat_filtgr <- matrixset(a=amat[, c(2,3,5,6)], b=bmat[, c(2,3,5,6)],
                          row_info=ri,
                          column_info = ci[c(2,3,5,6), ],
                          row_tag = "ROW", column_tag = "COL")
  mat_filtgr <- column_group_by(mat_filtgr, foo)

  expect_identical(filter_row(matset, g > 2), mat_filt2)
  expect_identical(filter_row(matset, g == max(g)), mat_filtmax)



  matset2 <- matset
  matset2[,,2] <- NULL
  expect_identical(filter_row(matset2, g > 2), mat_filt2_null)


  expect_identical(filter_column(column_group_by(matset, foo), gg >= mean(gg)),
                   mat_filtgr)



  srri <- row_info(student_results)
  mns <- tapply(srri$previous_year_score, srri$class, mean)
  idx <- srri$previous_year_score >= mns[srri$class]
  stud_res_filt <- row_group_by(student_results[idx,,], class)
  expect_identical(filter_row(row_group_by(student_results, class), previous_year_score >= mean(previous_year_score)),
                   stud_res_filt)



})



