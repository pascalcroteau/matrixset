test_that("matrixset properties works for Matrix", {


  m <- Matrix::Matrix(1:10, 2, 5)
  ms <- matrixset(foo = m)
  try(rownames(ms) <- c("a", "b"), silent = TRUE)
  expect_identical(rownames(ms), c("a", "b"))
  ms <- matrixset(foo = m)
  try(colnames(ms) <- letters[1:5], silent = TRUE)
  expect_identical(colnames(ms), letters[1:5])
  ms <- matrixset(foo = m)
  try(rownames(ms) <- c("a", "b"), silent = TRUE)
  try(colnames(ms) <- letters[1:5], silent = TRUE)
  expect_identical(rownames(ms), c("a", "b"))
  expect_identical(colnames(ms), letters[1:5])
  ms <- matrixset(foo = m)
  try(dimnames(ms) <- list(c("a", "b"), letters[1:5]), silent = TRUE)
  expect_identical(rownames(ms), c("a", "b"))
  expect_identical(colnames(ms), letters[1:5])
  expect_identical(dimnames(ms), list(c("a", "b"), letters[1:5]))

  rownames(m) <- c("a", "b")
  ms <- matrixset(foo = m)
  try(rownames(ms) <- c("c", "d"), silent = TRUE)
  expect_identical(rownames(ms), c("c", "d"))
  ms <- matrixset(foo = m)
  try(colnames(ms) <- letters[1:5], silent = TRUE)
  expect_identical(colnames(ms), letters[1:5])
  ms <- matrixset(foo = m)
  try(dimnames(ms) <- list(c("c", "d"), letters[1:5]), silent = TRUE)
  expect_identical(rownames(ms), c("c", "d"))
  expect_identical(colnames(ms), letters[1:5])
  expect_identical(dimnames(ms), list(c("c", "d"), letters[1:5]))


  rownames(m) <- NULL
  colnames(m) <- letters[1:5]
  ms <- matrixset(foo = m)
  try(colnames(ms) <- letters[1:5], silent = TRUE)
  expect_identical(colnames(ms), letters[1:5])
  ms <- matrixset(foo = m)
  try(rownames(ms) <- c("c", "d"), silent = TRUE)
  expect_identical(rownames(ms), c("c", "d"))
  ms <- matrixset(foo = m)
  try(dimnames(ms) <- list(c("c", "d"), letters[1:5]), silent = TRUE)
  expect_identical(colnames(ms), letters[1:5])
  expect_identical(rownames(ms), c("c", "d"))
  expect_identical(dimnames(ms), list(c("c", "d"), letters[1:5]))


  student_results_M <- mutate_matrix(student_results,
                                     failure = Matrix::Matrix(matrix_elm(student_results, 1)),
                                     remedial = Matrix::Matrix(matrix_elm(student_results, 2)))


  student_results2 <- student_results_M
  student_results3 <- student_results_M

  row_traits(student_results3) <- c("cl", "prof", "prev_Y_score")

  ri <- row_info(student_results_M)
  colnames(ri) <- c("rn", "cl", "prof", "prev_Y_score")

  expect_error(row_info(student_results2) <- ri,
               "There are no columns called \\.rowname in rows meta")

  colnames(ri)[1] <- ".rowname"
  row_info(student_results2) <- ri

  expect_equal(student_results3, student_results2)
  expect_equal(row_traits(student_results2), row_traits(student_results3))

  expect_error(row_info(student_results2) <- ri[1:10,],
               "Not all rows names are provided in meta info")

  ri2 <- ri
  ri2[,4] <- sample(unlist(ri[,4]))

  student_results2 <- student_results_M
  row_info(student_results2) <- ri2
  expect_equal(row_info(student_results2), ri2)
  expect_equal(row_traits(student_results2), colnames(ri2)[-1])

  student_results2 <- student_results_M
  row_info(student_results2)[,4] <- ri2[, 4]
  expect_equal(row_info(student_results2), ri2, ignore_attr = TRUE)

  student_results2 <- student_results_M
  expect_error(row_info(student_results2) <- ri2[1:10,],
               "Not all rows names are provided in meta info")
  expect_error(row_info(student_results2)[11:20,] <- ri2[1:10,],
               "Not all rows names are provided in meta info")
  ri3 <- ri
  ri3[1:10,] <- ri2[1:10,]
  colnames(ri3) <- c(".rowname", row_traits(student_results_M))
  row_info(student_results2)[1:10,] <- ri2[1:10,]
  expect_equal(row_info(student_results2), ri3)






  student_results2 <- row_group_by(student_results_M, class, teacher)


  ri <- row_info(student_results_M)
  colnames(ri) <- c("rn", "cl", "prof", "prev_Y_score")

  expect_error(row_info(student_results2) <- ri,
               "There are no columns called \\.rowname in rows meta")

  colnames(ri)[1] <- ".rowname"
  expect_warning(row_info(student_results2) <- ri,
                 "Row groups lost after row annotation update")

  student_results2 <- row_group_by(student_results_M, class, teacher)
  suppressWarnings(row_info(student_results2) <- ri)
  expect_null(row_groups(student_results2))
  expect_equal(row_info(student_results2), ri)
  expect_equal(row_traits(student_results2), colnames(ri)[-1])


  ri <- row_info(student_results_M)
  student_results2 <- row_group_by(student_results_M, class, teacher)
  meta <- row_group_meta(student_results2)
  ri2 <- ri
  ri2[,4] <- sample(unlist(ri[,4]))
  expect_message(row_info(student_results2) <- ri2,
                 "Row groups re-calculated after row annotation update")
  expect_equal(row_group_meta(student_results2), meta)
  expect_equal(row_info(student_results2), ri2, ignore_attr = TRUE)
  expect_equal(row_traits(student_results2), colnames(ri)[-1])


  student_results2 <- row_group_by(student_results_M, class, teacher)
  meta <- row_group_meta(student_results2)
  suppressMessages(row_info(student_results2)[,4] <- ri2[, 4])
  expect_equal(row_group_meta(student_results2), meta)
  expect_equal(row_info(student_results2), ri2, ignore_attr = TRUE)


  student_results2 <- row_group_by(student_results_M, class, teacher)
  meta <- row_group_meta(student_results2)
  expect_error(row_info(student_results2) <- ri2[1:10,],
               "Not all rows names are provided in meta info")
  expect_error(row_info(student_results2)[11:20,] <- ri2[1:10,],
               "Not all rows names are provided in meta info")
  ri3 <- ri
  ri3[1:10,] <- ri2[1:10,]
  colnames(ri3) <- c(".rowname", row_traits(student_results_M))
  suppressMessages(row_info(student_results2)[1:10,] <- ri2[1:10,])
  expect_equal(row_group_meta(student_results2), meta)
  expect_equal(row_info(student_results2), ri3, ignore_attr = TRUE)


})
