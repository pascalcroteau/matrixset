test_that("matrixset matrix mutation works", {

  ms <- mutate_matrix(student_results,
                      FC = remedial/failure,
                      foo = NULL,
                      logFC = log(FC),
                      FC = remove_matrix())

  expect_null(matrix_elm(ms, "foo"))
  ms <- remove_matrix(ms, "foo")

  expect_error(add_matrix(student_results, remedial = matrix()),
               "some new matrices have the same name as other original matrices\\. Use 'mutate_matrix\\(\\)' to update original matrices")

  ms2 <- add_matrix(student_results, logFC=matrix_elm(ms, "logFC"))
  expect_identical(ms, ms2)

  expect_error(add_matrix(student_results, foo = 1:5),
               "Elements must be NULL or a matrix")

  expect_error(mutate_matrix(student_results, rem = remedial[1:10, ]),
               "All matrices must have the same row names")

  expect_error(mutate_matrix(student_results, rem = 1:10),
               "Elements must be NULL or a matrix")


  ms <- mutate_matrix(student_results,
                      FC = remedial/failure,
                      logFC = log(FC))
  ms2 <- add_matrix(student_results, ms[,,c("FC", "logFC"),keep_annotation=FALSE, warn_class_change = FALSE])
  expect_identical(ms, ms2)


  ms <- mutate_matrix(student_results,
                      bar = matrix(1,
                                   nrow=current_n_row(),
                                   ncol=current_n_column()) |>
                        magrittr::set_rownames(current_row_name()) |>
                        magrittr::set_colnames(current_column_name()))
  ms2 <- add_matrix(student_results,
                    bar=matrix(1,
                           nrow=nrow(student_results),
                           ncol=ncol(student_results)) |>
                      magrittr::set_rownames(rownames(student_results)) |>
                      magrittr::set_colnames(colnames(student_results)))
  expect_identical(ms, ms2)



  ms2 <- mutate_matrix(student_results,
                       bar = {
                         foo <- function(n) {
                           matrix(n,
                                  nrow=current_n_row(),
                                  ncol=current_n_column()) |>
                             magrittr::set_rownames(current_row_name()) |>
                             magrittr::set_colnames(current_column_name())
                         }
                         foo(1)
                       } )
  expect_identical(ms, ms2)


  ms <- mutate_matrix(student_results,
                      bar = remedial)
  ms2 <- add_matrix(student_results,
                    stats::setNames(ms[,,"remedial",keep_annotation=FALSE, warn_class_change = FALSE], "bar"))
  expect_identical(ms, ms2)
})
