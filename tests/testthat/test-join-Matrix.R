test_that("matrixset row info join works for Matrix", {

  student_results_M <- mutate_matrix(student_results,
                                     failure = Matrix::Matrix(matrix_elm(student_results, 1)),
                                     remedial = Matrix::Matrix(matrix_elm(student_results, 2)))


  ms1 <- remove_row_annotation(student_results_M, class, teacher)
  suppressWarnings(ms <- join_row_info(ms1, student_results_M))
  ms_ref <- student_results_M
  ri <- row_info(ms_ref)
  ri <- dplyr::mutate(ri, previous_year_score.y = previous_year_score)
  ri <- dplyr::rename(ri, previous_year_score.x = previous_year_score)
  ri <- ri[, c(".rowname", "previous_year_score.x", "class", "teacher", "previous_year_score.y")]
  row_info(ms_ref) <- ri
  expect_identical(ms, ms_ref)



  ms <- join_row_info(ms1, student_results_M, by = c(".rowname", "previous_year_score"))
  ms_ref <- student_results_M
  ri <- row_info(ms_ref)
  ri <- ri[, c(".rowname", "previous_year_score", "class", "teacher")]
  row_info(ms_ref) <- ri
  expect_identical(ms, ms_ref)



  expect_error(join_row_info(ms1, student_results_M, by = c(".rowname", "previous_score")),
               "variable \"previous_score\" is not a known trait")



  ms1 <- remove_row_annotation(filter_row(student_results_M, class %in% c("classA", "classC")),
                               class, teacher)
  suppressWarnings(ms <- join_row_info(ms1, student_results_M))
  ms_ref <- filter_row(student_results_M, class %in% c("classA", "classC"))
  ri <- row_info(ms_ref)
  ri <- dplyr::mutate(ri, previous_year_score.y = previous_year_score)
  ri <- dplyr::rename(ri, previous_year_score.x = previous_year_score)
  ri <- ri[, c(".rowname", "previous_year_score.x", "class", "teacher", "previous_year_score.y")]
  row_info(ms_ref) <- ri
  expect_identical(ms, ms_ref)



  expect_error(join_row_info(ms1, student_results_M, type = "full"),
               "the number of rows is modified by the join operation, which is against the 'matrixset' paradigm\\. Use 'adjust' to still perform the operation\\.")



  ms2 <- remove_row_annotation(ms1, previous_year_score)
  ms <- join_row_info(ms2, student_results_M, type = "full", adjust = TRUE)
  m <- student_results_M[c(1:5, 11:15), , , keep_annotation = FALSE, warn_class_change = FALSE]
  mNA <- matrix(NA_real_, 10, 3)
  rownames(mNA) <- paste("student", c(6:10, 16:20))
  colnames(mNA) <- colnames(student_results_M)
  m <- lapply(m, function(M) rbind(M, mNA))
  ri <- row_info(student_results_M)
  ri <- ri[c(1:5, 11:15, 6:10, 16:20), ]
  ci <- column_info(student_results_M)
  ms_ref <- matrixset(m, row_info = ri, column_info = ci, row_key = ".rowname",
                      column_key = ".colname")
  expect_identical(ms, ms_ref)



  expect_error(join_row_info(student_results_M, ms1, type = "inner"),
               "the number of rows is modified by the join operation, which is against the 'matrixset' paradigm\\. Use 'adjust' to still perform the operation\\.")


  ms2 <- remove_row_annotation(student_results_M, previous_year_score)
  ms <- join_row_info(ms2, ms1, type = "inner", adjust = TRUE)
  ms_ref <- filter_row(student_results_M, class %in% c("classA", "classC"))
  expect_identical(ms, ms_ref)



  ms3 <- remove_row_annotation(student_results_M, class)
  ms <- join_row_info(row_group_by(ms3, teacher), student_results_M, by = c(".rowname", "previous_year_score", "teacher"))
  ms_ref <- student_results_M
  ri <- row_info(ms_ref)
  ri <- ri[, c(".rowname", "teacher", "previous_year_score", "class")]
  row_info(ms_ref) <- ri
  ms_ref <- row_group_by(ms_ref, teacher)
  expect_identical(ms, ms_ref)



  ms3 <- remove_row_annotation(student_results_M, class)
  suppressWarnings(ms <- join_row_info(row_group_by(ms3, teacher), student_results_M, by = c(".rowname", "previous_year_score")))
  ms_ref <- student_results_M
  ri <- row_info(ms_ref)
  ri$teacher.x <- ri$teacher
  ri$teacher.y <- ri$teacher
  ri <- ri[, c(".rowname", "teacher.x", "previous_year_score", "class", "teacher.y")]
  row_info(ms_ref) <- ri
  expect_identical(ms, ms_ref)

})
