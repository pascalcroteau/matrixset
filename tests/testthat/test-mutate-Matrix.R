test_that("matrixset matrix mutation works for Matrix", {

  student_results_M <- mutate_matrix(student_results,
                                     failure = Matrix::Matrix(matrix_elm(student_results, 1)),
                                     remedial = Matrix::Matrix(matrix_elm(student_results, 2)))


  ms <- mutate_matrix(student_results_M,
                      FC = remedial/failure,
                      foo = NULL,
                      logFC = log(FC),
                      FC = remove_matrix())

  expect_null(matrix_elm(ms, "foo"))
  ms <- remove_matrix(ms, "foo")

  expect_error(add_matrix(student_results_M, remedial = matrix()),
               "some new matrices have the same name as other original matrices\\. Use 'mutate_matrix\\(\\)' to update original matrices")

  ms2 <- add_matrix(student_results_M, logFC=matrix_elm(ms, "logFC"))
  expect_identical(ms, ms2)

  expect_error(add_matrix(student_results_M, foo = 1:5),
               "Elements must be NULL or a matrix")

  expect_error(mutate_matrix(student_results_M, rem = remedial[1:10, ]),
               "All matrices must have the same row names")

  expect_error(mutate_matrix(student_results_M, rem = 1:10),
               "Elements must be NULL or a matrix")


  ms <- mutate_matrix(student_results_M,
                      FC = remedial/failure,
                      logFC = log(FC))
  ms2 <- add_matrix(student_results_M, ms[,,c("FC", "logFC"),keep_annotation=FALSE, warn_class_change = FALSE])
  expect_identical(ms, ms2)
})
