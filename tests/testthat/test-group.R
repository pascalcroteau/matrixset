test_that("matrixset grouping works", {

  expect_identical(row_group_by(row_group_by(student_results, class), teacher),
                   row_group_by(student_results, teacher))

  expect_identical(row_group_by(row_group_by(student_results, class), teacher, .add = TRUE),
                   row_group_by(student_results, class, teacher))

  expect_identical(row_group_by(student_results, teacher),
                   row_ungroup(row_group_by(student_results, class, teacher), class))

  expect_identical(student_results,
                   row_ungroup(row_group_by(student_results, class, teacher)))

})
