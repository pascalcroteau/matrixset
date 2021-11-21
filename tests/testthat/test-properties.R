test_that("matrixset properties works", {

  student_results2 <- student_results
  student_results3 <- student_results

  row_traits(student_results3) <- c("cl", "prof", "prev_Y_score")

  ri <- row_info(student_results)
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

  student_results2 <- student_results
  row_info(student_results2) <- ri2
  expect_equal(row_info(student_results2), ri2)
  expect_equal(row_traits(student_results2), colnames(ri2)[-1])

  student_results2 <- student_results
  row_info(student_results2)[,4] <- ri2[, 4]
  expect_equal(row_info(student_results2), ri2, ignore_attr = TRUE)

  student_results2 <- student_results
  expect_error(row_info(student_results2) <- ri2[1:10,],
               "Not all rows names are provided in meta info")
  expect_error(row_info(student_results2)[11:20,] <- ri2[1:10,],
               "Not all rows names are provided in meta info")
  ri3 <- ri
  ri3[1:10,] <- ri2[1:10,]
  colnames(ri3) <- c(".rowname", row_traits(student_results))
  row_info(student_results2)[1:10,] <- ri2[1:10,]
  expect_equal(row_info(student_results2), ri3)






  student_results2 <- row_group_by(student_results, class, teacher)


  ri <- row_info(student_results)
  colnames(ri) <- c("rn", "cl", "prof", "prev_Y_score")

  expect_error(row_info(student_results2) <- ri,
               "There are no columns called \\.rowname in rows meta")

  colnames(ri)[1] <- ".rowname"
  expect_warning(row_info(student_results2) <- ri,
                 "Row groups lost after row annotation update")

  student_results2 <- row_group_by(student_results, class, teacher)
  suppressWarnings(row_info(student_results2) <- ri)
  expect_null(row_groups(student_results2))
  expect_equal(row_info(student_results2), ri)
  expect_equal(row_traits(student_results2), colnames(ri)[-1])


  ri <- row_info(student_results)
  student_results2 <- row_group_by(student_results, class, teacher)
  meta <- row_group_meta(student_results2)
  ri2 <- ri
  ri2[,4] <- sample(unlist(ri[,4]))
  expect_message(row_info(student_results2) <- ri2,
                 "Row groups re-calculated after row annotation update")
  expect_equal(row_group_meta(student_results2), meta)
  expect_equal(row_info(student_results2), ri2, ignore_attr = TRUE)
  expect_equal(row_traits(student_results2), colnames(ri)[-1])


  student_results2 <- row_group_by(student_results, class, teacher)
  meta <- row_group_meta(student_results2)
  suppressMessages(row_info(student_results2)[,4] <- ri2[, 4])
  expect_equal(row_group_meta(student_results2), meta)
  expect_equal(row_info(student_results2), ri2, ignore_attr = TRUE)


  student_results2 <- row_group_by(student_results, class, teacher)
  meta <- row_group_meta(student_results2)
  expect_error(row_info(student_results2) <- ri2[1:10,],
               "Not all rows names are provided in meta info")
  expect_error(row_info(student_results2)[11:20,] <- ri2[1:10,],
               "Not all rows names are provided in meta info")
  ri3 <- ri
  ri3[1:10,] <- ri2[1:10,]
  colnames(ri3) <- c(".rowname", row_traits(student_results))
  suppressMessages(row_info(student_results2)[1:10,] <- ri2[1:10,])
  expect_equal(row_group_meta(student_results2), meta)
  expect_equal(row_info(student_results2), ri3, ignore_attr = TRUE)


})
