test_that("matrixset creation works with Matrix", {


  lst <- list(a = Matrix::Matrix(0, 2, 3))
  matset <- matrixset(lst)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = Matrix::Matrix(0, 2, 3))
  matset <- matrixset(a = Matrix::Matrix(0, 2, 3))
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = NULL, b = Matrix::Matrix(0, 2, 3))
  matset <- matrixset(lst)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = NULL, b = matrix(0, 2, 3), c = Matrix::Matrix(0, 2, 3))
  matset <- matrixset(lst)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = NULL, b = matrix(0, 2, 3), c = Matrix::Matrix(0, 2, 3))
  matset <- matrixset(a = NULL, b = matrix(0, 2, 3), c = Matrix::Matrix(0, 2, 3))
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  matset <- matrixset(lst)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = c("r1", "r2")))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), c("r1", "r2"))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  matset <- matrixset(lst)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = c("r1", "r2")))
  expect_equal(column_info(matset), tibble::tibble(.colname = c("c1", "c2", "c3")))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), c("r1", "r2"))
  expect_equal(attr(matset, "col_names"), c("c1", "c2", "c3"))


  lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- tibble::tibble(rowname = c("r1", "r2"), g = 1:2)
  matset <- matrixset(lst, row_info = ri)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), setNames(ri, c(".rowname", "g")))
  expect_equal(column_info(matset), tibble::tibble(.colname = c("c1", "c2", "c3")))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), c("r1", "r2"))
  expect_equal(attr(matset, "col_names"), c("c1", "c2", "c3"))


  lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- tibble::tibble(rowname = c("r1", "r2"), g = 1:2)
  matset <- matrixset(lst, row_info = ri, row_tag = "foo_bar")
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), setNames(ri, c("foo_bar", "g")))
  expect_equal(column_info(matset), tibble::tibble(.colname = c("c1", "c2", "c3")))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), c("r1", "r2"))
  expect_equal(attr(matset, "col_names"), c("c1", "c2", "c3"))


  lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- tibble::tibble(rowname = c("r1", "r2"), g = 1:2)
  ci <- tibble::tibble(colname = c("c1", "c2", "c3"), h = 1:3)
  matset <- matrixset(lst, row_info = ri, column_info = ci)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), setNames(ri, c(".rowname", "g")))
  expect_equal(column_info(matset), setNames(ci, c(".colname", "h")))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), c("r1", "r2"))
  expect_equal(attr(matset, "col_names"), c("c1", "c2", "c3"))


  lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- tibble::tibble(rowname = c("r1", "r2"), g = 1:2)
  ci <- tibble::tibble(colname = c("c1", "c2", "c3"), h = 1:3)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), setNames(ri, c("foo", "g")))
  expect_equal(column_info(matset), setNames(ci, c("bar", "h")))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), c("r1", "r2"))
  expect_equal(attr(matset, "col_names"), c("c1", "c2", "c3"))



  lst <- list(a = Matrix::Matrix(1:6, 2, 3), b = matrix(letters[1:6], 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- tibble::tibble(rowname = c("r1", "r2"), g = 1:2)
  ci <- tibble::tibble(colname = c("c1", "c2", "c3"), h = 1:3)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo",
                      column_tag = "bar")
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), setNames(ri, c("foo", "g")))
  expect_equal(column_info(matset), setNames(ci, c("bar", "h")))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), c("r1", "r2"))
  expect_equal(attr(matset, "col_names"), c("c1", "c2", "c3"))


})




test_that("matrixset creation with expansion works for Matrix", {


  m1 <- matrix(1:6, 2, 3)
  m1e <- matrix(c(1,2,0,3,4,0,5,6,0,0,0,0,0,0,0),3,5)
  m1e2 <- matrix(c(1,2,NA,NA,NA,3,4,NA,NA,NA,5,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),5,5)
  m1e20 <- matrix(c(1,2,0,0,0,3,4,0,0,0,5,6,0,0,0,0,0,0,0,0,0,0,0,0,0),5,5)
  storage.mode(m1e) <- "integer"
  storage.mode(m1e2) <- "integer"
  m2 <- matrix(101:115, 3, 5)
  m3 <- matrix(7:12, 3, 2)
  m3e <- matrix(c(rep(0,15), 0,0,7:9,0,0,10:12), 5, 5)
  m3e2 <- matrix(c(rep(-1,15), -1,-1,7:9,-1,-1,10:12), 5, 5)
  rownames(m1) <- c("r1", "r2")
  rownames(m1e) <- paste0("r", 1:3)
  rownames(m1e2) <- paste0("r", 1:5)
  rownames(m1e20) <- paste0("r", 1:5)
  rownames(m2) <- paste0("r", c(1,3,2))
  rownames(m3) <- paste0("r", 3:5)
  rownames(m3e) <- paste0("r", 1:5)
  rownames(m3e2) <- paste0("r", 1:5)
  colnames(m1) <- paste0("c", 1:3)
  colnames(m1e) <- paste0("c", 1:5)
  colnames(m1e2) <- paste0("c", 1:5)
  colnames(m1e20) <- paste0("c", 1:5)
  colnames(m2) <- paste0("c", c(1,2,4,3,5))
  colnames(m3) <- paste0("c", 4:5)
  colnames(m3e) <- paste0("c", 1:5)
  colnames(m3e2) <- paste0("c", 1:5)
  m1 <- Matrix::Matrix(m1)
  m1e <- Matrix::Matrix(m1e)
  m1e2 <- Matrix::Matrix(m1e2)
  m1e20 <- Matrix::Matrix(m1e20)
  m2 <- Matrix::Matrix(m2)
  m3 <- Matrix::Matrix(m3)
  m3e <- Matrix::Matrix(m3e)
  m3e2 <- Matrix::Matrix(m3e2)

  ms <- matrixset(a=m1e, b=m2[c(1,3,2), c(1,2,4,3,5)])
  ms2e <- matrixset(a=m1e2, b=m3e)
  ms2e0 <- matrixset(a=m1e20, b=m3e)
  ms3e <- matrixset(a=m1e2, b=m3e2)
  expect_identical(matrixset(a=m1, b=m2, expand = TRUE), ms)
  expect_identical(matrixset(a=m1, b=m3, expand = TRUE), ms2e0)
  expect_identical(matrixset(a=m1, b=m3, expand = list(NA, -1)), ms3e)
  expect_identical(matrixset(a=m1, b=m3, expand = list(a=NA, b=-1)), ms3e)
  expect_identical(matrixset(a=m1, b=m3, expand = list(b=-1, c=NA, a=NA)), ms3e)


# test warning of multiple expand in vector form


})


#
#
# test_that("matrixset properly fails", {
#
#
#   testthat::expect_error(matrixset(Matrix::Matrix(0,2,3)), "The list elements must be named")
#
#   expect_error(matrixset(a=matrix(0,2,3),a=Matrix::Matrix(1,2,3)),
#                "matrix names must be unique")
#
#
#   lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 3, 3))
#   testthat::expect_error(matrixset(lst), "All matrices must have the same number of rows")
#
#
#   lst <- list(a = matrix(0, 3, 2), b = Matrix::Matrix(0, 3, 3))
#   testthat::expect_error(matrixset(lst), "All matrices must have the same number of columns")
#
#
#   lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3))
#   rownames(lst$a) <- c("r1", "r2")
#   testthat::expect_error(matrixset(lst), "All matrices must have the same row names \\(NULL accepted\\)")
#
#
#   lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3), c = NULL)
#   rownames(lst$a) <- c("r1", "r2")
#   rownames(lst$b) <- c("r1", "r2")
#   colnames(lst$a) <- c("c1", "c2", "c3")
#   testthat::expect_error(matrixset(lst), "All matrices must have the same column names \\(NULL accepted\\)")
#
#
#   lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3), c = NULL)
#   rownames(lst$a) <- c("r1", "r1")
#   rownames(lst$b) <- c("r1", "r1")
#   testthat::expect_error(matrixset(lst), "Row names must be unique")
#
#
#   lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3), c = NULL)
#   rownames(lst$a) <- c("r1", "r2")
#   rownames(lst$b) <- c("r1", "r2")
#   colnames(lst$a) <- c("c1", "c2", "c1")
#   colnames(lst$b) <- c("c1", "c2", "c1")
#   testthat::expect_error(matrixset(lst), "Column names must be unique")
#
#
#   lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3), c = NULL)
#   rownames(lst$a) <- c("r1", "")
#   rownames(lst$b) <- c("r1", "")
#   testthat::expect_error(matrixset(lst), "Empty row names are not allowed")
#
#
#   lst <- list(a = matrix(0, 2, 3), b = Matrix::Matrix(0, 2, 3), c = NULL)
#   rownames(lst$a) <- c("r1", "r2")
#   rownames(lst$b) <- c("r1", "r2")
#   colnames(lst$a) <- c("c1", "", "c3")
#   colnames(lst$b) <- c("c1", "", "c3")
#   testthat::expect_error(matrixset(lst), "Empty column names are not allowed")
#
#
# })
#
#
#
#
# test_that("matrixset creation with expansion properly fails", {
#
#
#   m1 <- Matrix::Matrix(1:6, 2, 3)
#   m2 <- Matrix::Matrix(101:115, 3, 5)
#
#
#   expect_error(matrixset(a=m1, b=m2, expand = TRUE),
#                "matrices must have dimnames for expansion")
#
#   rownames(m1) <- paste0("r", 1:2)
#   rownames(m2) <- paste0("r", 1:3)
#   colnames(m1) <-  paste0("c", 1:3)
#   colnames(m2) <- paste0("c", 1:5)
#
#   expect_error(matrixset(a=m1, b=m2, expand=list(a=NA, -1)),
#                "Empty expansion list names are not allowed")
#
#   expect_error(matrixset(a=m1, b=m2, expand=list(c=NA, d=-1)),
#                "No expansion list name matches the matrix names")
#
#   expect_error(matrixset(a=m1, b=m2, expand=list(a=NA)),
#                "Number of expansion fill values is not compatible with the number of matrices")
# })
#

