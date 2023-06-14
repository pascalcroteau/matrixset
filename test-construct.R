test_that("matrixset creation works", {


  lst <- NULL
  matset <- matrixset(lst)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], NULL)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 0)
  expect_equal(attr(matset, "n_col"), 0)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = NULL, b = NULL)
  matset <- matrixset(lst)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 0)
  expect_equal(attr(matset, "n_col"), 0)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = NULL, b = NULL)
  matset <- matrixset(a = NULL, b = NULL)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 0)
  expect_equal(attr(matset, "n_col"), 0)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = matrix(0, 2, 3))
  matset <- matrixset(lst)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = matrix(0, 2, 3))
  matset <- matrixset(a = matrix(0, 2, 3))
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = NULL, b = matrix(0, 2, 3))
  matset <- matrixset(lst)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = NULL, b = matrix(0, 2, 3), c = matrix(0, 2, 3))
  matset <- matrixset(lst)
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = NULL, b = matrix(0, 2, 3), c = matrix(0, 2, 3))
  matset <- matrixset(a = NULL, b = matrix(0, 2, 3), c = matrix(0, 2, 3))
  expect_equal(matset[,,, keep_annotation = F, warn_class_change = F], lst)
  expect_equal(row_info(matset), tibble::tibble(.rowname = logical()))
  expect_equal(column_info(matset), tibble::tibble(.colname = logical()))
  expect_equal(attr(matset, "n_row"), 2)
  expect_equal(attr(matset, "n_col"), 3)
  expect_equal(attr(matset, "row_names"), character(0))
  expect_equal(attr(matset, "col_names"), character(0))


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
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


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
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


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
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


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
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


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
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


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
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



  lst <- list(a = matrix(1:6, 2, 3), b = matrix(letters[1:6], 2, 3), c = NULL)
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




test_that("matrixset creation with expansion works", {


  m1 <- matrix(1:6, 2, 3)
  m1e <- matrix(c(1,2,NA,3,4,NA,5,6,NA,NA,NA,NA,NA,NA,NA),3,5)
  m1e2 <- matrix(c(1,2,NA,NA,NA,3,4,NA,NA,NA,5,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),5,5)
  storage.mode(m1e) <- "integer"
  storage.mode(m1e2) <- "integer"
  m2 <- matrix(101:115, 3, 5)
  m3 <- matrix(7:12, 3, 2)
  m3e <- matrix(c(rep(NA,15), NA,NA,7:9,NA,NA,10:12), 5, 5)
  m3e2 <- matrix(c(rep(-1,15), -1,-1,7:9,-1,-1,10:12), 5, 5)
  rownames(m1) <- c("r1", "r2")
  rownames(m1e) <- paste0("r", 1:3)
  rownames(m1e2) <- paste0("r", 1:5)
  rownames(m2) <- paste0("r", c(1,3,2))
  rownames(m3) <- paste0("r", 3:5)
  rownames(m3e) <- paste0("r", 1:5)
  rownames(m3e2) <- paste0("r", 1:5)
  colnames(m1) <- paste0("c", 1:3)
  colnames(m1e) <- paste0("c", 1:5)
  colnames(m1e2) <- paste0("c", 1:5)
  colnames(m2) <- paste0("c", c(1,2,4,3,5))
  colnames(m3) <- paste0("c", 4:5)
  colnames(m3e) <- paste0("c", 1:5)
  colnames(m3e2) <- paste0("c", 1:5)

  ms <- matrixset(a=m1e, b=m2[c(1,3,2), c(1,2,4,3,5)])
  ms2e <- matrixset(a=m1e2, b=m3e)
  ms3e <- matrixset(a=m1e2, b=m3e2)
  expect_identical(matrixset(a=m1, b=m2, expand = TRUE), ms)
  expect_identical(matrixset(a=m1, b=m3, expand = TRUE), ms2e)
  expect_identical(matrixset(a=m1, b=m3, expand = list(NA, -1)), ms3e)
  expect_identical(matrixset(a=m1, b=m3, expand = list(a=NA, b=-1)), ms3e)
  expect_identical(matrixset(a=m1, b=m3, expand = list(b=-1, c=NA, a=NA)), ms3e)


# test warning of multiple expand in vector form


})




test_that("matrixset properly fails", {


  lst <- list()
  testthat::expect_error(matrixset(lst), "The list elements must be named")


  lst <- list(NA)
  testthat::expect_error(matrixset(lst), "The list elements must be named")


  lst <- list(NULL)
  testthat::expect_error(matrixset(lst), "The list elements must be named")


  lst <- list(a = logical(10))
  testthat::expect_error(matrixset(lst), "Elements must be NULL or a matrix")


  lst <- list(a = logical(10), b = NULL)
  testthat::expect_error(matrixset(lst), "Elements must be NULL or a matrix")


  lst <- list(a = NULL, b = logical(10))
  testthat::expect_error(matrixset(lst), "Elements must be NULL or a matrix")


  testthat::expect_error(matrixset(matrix(0,2,3)), "The list elements must be named")

  expect_error(matrixset(a=matrix(0,2,3),a=matrix(1,2,3)),
               "matrix names must be unique")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 3, 3))
  testthat::expect_error(matrixset(lst), "All matrices must have the same number of rows")


  lst <- list(a = matrix(0, 3, 2), b = matrix(0, 3, 3))
  testthat::expect_error(matrixset(lst), "All matrices must have the same number of columns")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3))
  rownames(lst$a) <- c("r1", "r2")
  testthat::expect_error(matrixset(lst), "All matrices must have the same row names \\(NULL accepted\\)")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  testthat::expect_error(matrixset(lst), "All matrices must have the same column names \\(NULL accepted\\)")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r1")
  rownames(lst$b) <- c("r1", "r1")
  testthat::expect_error(matrixset(lst), "Row names must be unique")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c1")
  colnames(lst$b) <- c("c1", "c2", "c1")
  testthat::expect_error(matrixset(lst), "Column names must be unique")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "")
  rownames(lst$b) <- c("r1", "")
  testthat::expect_error(matrixset(lst), "Empty row names are not allowed")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "", "c3")
  colnames(lst$b) <- c("c1", "", "c3")
  testthat::expect_error(matrixset(lst), "Empty column names are not allowed")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- data.frame(rowname = c("r1", "r3"), g = 1:2)
  testthat::expect_error(matrixset(lst, row_info = ri), "Not all rows names are provided in meta info")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- data.frame(row_name = c("r1", "r2"), g = 1:2)
  testthat::expect_error(matrixset(lst, row_info = ri), "There are no columns called rowname in rows meta")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(colname = c("c1", "c4", "c3"), g = 1:3)
  testthat::expect_error(matrixset(lst, row_info = ri, column_info = ci), "Not all columns names are provided in meta info")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(col_name = c("c1", "c2", "c3"), g = 1:3)
  testthat::expect_error(matrixset(lst, row_info = ri, column_info = ci), "There are no columns called colname in columns meta")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(col_name = c("c1", "c2", "c3"), g = 1:3)
  testthat::expect_error(matrixset(lst, row_info = ri, column_info = ci,
                                   row_tag = "g"),
                         "Column g already exists in row meta info")


  lst <- list(a = matrix(0, 2, 3), b = matrix(0, 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- data.frame(rowname = c("r1", "r2", "r1"), g = 1:3)
  ci <- data.frame(colname = c("c1", "c2", "c3"), g = 1:3)
  expect_error(matrixset(lst, row_info = ri, column_info = ci),
               "some keys defined more than once in row meta info")


})




test_that("matrixset creation with expansion properly fails", {


  m1 <- matrix(1:6, 2, 3)
  m2 <- matrix(101:115, 3, 5)


  expect_error(matrixset(a=m1, b=m2, expand = TRUE),
               "matrices must have dimnames for expansion")

  rownames(m1) <- paste0("r", 1:2)
  rownames(m2) <- paste0("r", 1:3)
  colnames(m1) <-  paste0("c", 1:3)
  colnames(m2) <- paste0("c", 1:5)

  expect_error(matrixset(a=m1, b=m2, expand=list(a=NA, -1)),
               "Empty expansion list names are not allowed")

  expect_error(matrixset(a=m1, b=m2, expand=list(c=NA, d=-1)),
               "No expansion list name matches the matrix names")

  expect_error(matrixset(a=m1, b=m2, expand=list(a=NA)),
               "Number of expansion fill values is not compatible with the number of matrices")
})


