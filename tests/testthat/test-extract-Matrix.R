
test_that("matrixset extraction works", {

  as_dge <- function(m) methods::as(methods::as(m, "generalMatrix"), "unpackedMatrix")

  lst <- list(a = Matrix::Matrix(1:6, 2, 3), b = Matrix::Matrix(101:106, 2, 3), c = NULL)
  lst1 <- list(a = Matrix::Matrix(seq(1,5,2),1,3), b = Matrix::Matrix(seq(101,105,2),1,3), c = NULL)
  lst2 <- list(a = Matrix::Matrix(3:4,2,1), b = Matrix::Matrix(103:104,2,1), c = NULL)
  # lst12 <- list(a = methods::as(Matrix::Matrix(3), "dgeMatrix"),
  #               b = methods::as(Matrix::Matrix(103), "dgeMatrix"), c = NULL)
  lst12 <- list(a = as_dge(Matrix::Matrix(3)),
                b = as_dge(Matrix::Matrix(103)), c = NULL)
  lst1213 <- list(a = Matrix::Matrix(c(1,2,5,6),2,2), b = Matrix::Matrix(c(101,102,105,106),2,2), c = NULL)
  rownames(lst$a) <- rownames(lst$b) <- c("r1", "r2")
  rownames(lst1$a) <- rownames(lst1$b) <- "r1"
  rownames(lst12$a) <- rownames(lst12$b) <- "r1"
  rownames(lst2$a) <- rownames(lst2$b) <- c("r1", "r2")
  rownames(lst1213$a) <- rownames(lst1213$b) <- c("r1", "r2")
  colnames(lst$a) <- colnames(lst$b) <- c("c1", "c2", "c3")
  colnames(lst1$a) <- colnames(lst1$b) <- c("c1", "c2", "c3")
  colnames(lst2$a) <- colnames(lst2$b) <- "c2"
  colnames(lst12$a) <- colnames(lst12$b) <- "c2"
  colnames(lst1213$a) <- colnames(lst1213$b) <- c("c1", "c3")
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ri1 <- ri[1,,drop=FALSE]
  ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
  ci2 <- ci[2,,drop=FALSE]
  ci13 <- ci[c(1,3),,drop=FALSE]
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
  matset1 <- matrixset(lst1, row_info = ri1, column_info = ci, row_tag = "foo", column_tag = "bar")
  matset2 <- matrixset(lst2, row_info = ri, column_info = ci2, row_tag = "foo", column_tag = "bar")
  matset12 <- matrixset(lst12, row_info = ri1, column_info = ci2, row_tag = "foo", column_tag = "bar")
  matset1213 <- matrixset(lst1213, row_info = ri, column_info = ci13, row_tag = "foo", column_tag = "bar")


  expect_equal(matset, matset[])
  expect_equal(matset[1, ,], matset1)
  expect_equal(matset[,2, ], matset2, ignore_attr = TRUE)
  expect_equal(matset[1,2,], matset12, ignore_attr = TRUE)
  expect_equal(matset[1:2,c(1,3),], matset1213, ignore_attr = TRUE)
  expect_equal(matset[1, , , keep_annotation = FALSE, warn_class_change = FALSE],
               lst1)
  expect_equal(matset[, 2, , keep_annotation = FALSE, warn_class_change = FALSE],
               lst2)
  expect_equal(matset[1, 2, , keep_annotation = FALSE, warn_class_change = FALSE],
               lst12)
  expect_equal(matset[1:2, c(1,3), , keep_annotation = FALSE, warn_class_change = FALSE],
               lst1213)
  expect_equal(matset[1, , , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[1,]))
  expect_equal(matset[, 2, , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[,2]))
  expect_equal(matset[1, 2, , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[1,2]))
  expect_equal(matset[1:2, c(1,3), , drop = TRUE], matset1213, ignore_attr = TRUE)

  expect_equal(matset[matrix(c(1:2, 1, 3), 2), ,], matset1213, ignore_attr = TRUE)

  # test char
  expect_equal(matset["r1", ,], matset1)
  expect_equal(matset[, "c2", ], matset2, ignore_attr = TRUE)
  expect_equal(matset["r1","c2",], matset12, ignore_attr = TRUE)
  expect_equal(matset[c("r1", "r2"), c("c1","c3"),], matset1213, ignore_attr = TRUE)
  expect_equal(matset["r1", , , keep_annotation = FALSE, warn_class_change = FALSE],
               lst1)
  expect_equal(matset[, "c2", , keep_annotation = FALSE, warn_class_change = FALSE],
               lst2)
  expect_equal(matset["r1", "c2", , keep_annotation = FALSE, warn_class_change = FALSE],
               lst12)
  expect_equal(matset[c("r1", "r2"), c("c1","c3"), , keep_annotation = FALSE,
                      warn_class_change = FALSE], lst1213)
  expect_equal(matset["r1", , , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[1,]))
  expect_equal(matset[, "c2", , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[,2]))
  expect_equal(matset["r1", "c2", , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[1,2]))
  expect_equal(matset[c("r1", "r2"), c("c1","c3"), , drop = TRUE], matset1213, ignore_attr = TRUE)

  # test logical
  expect_equal(matset[c(T, F), ,], matset1)
  expect_equal(matset[, c(F,T,F),], matset2, ignore_attr = TRUE)
  expect_equal(matset[c(T,F),c(F,T,F),], matset12, ignore_attr = TRUE)
  expect_equal(matset[c(T, T), c(T,F,T),], matset1213, ignore_attr = TRUE)
  expect_equal(matset[c(T,F), , , keep_annotation = FALSE, warn_class_change = FALSE],
               lst1)
  expect_equal(matset[, c(F,T,F), , keep_annotation = FALSE,
                      warn_class_change = FALSE], lst2)
  expect_equal(matset["r1", "c2", , keep_annotation = FALSE,
                      warn_class_change = FALSE], lst12)
  expect_equal(matset[c(T, T), c(T,F,T), , keep_annotation = FALSE,
                      warn_class_change = FALSE], lst1213)
  expect_equal(matset[c(T,F), , , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[1,]))
  expect_equal(matset[, c(F,T,F), , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[,2]))
  expect_equal(matset[c(T,F), c(F,T,F), , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[1,2]))
  expect_equal(matset[c(T,T), c(T,F,T), , drop = TRUE], matset1213, ignore_attr = TRUE)

  # test neg index (removal)
  expect_equal(matset[-2, ,], matset1)
  expect_equal(matset[,-c(1,3),], matset2, ignore_attr = TRUE)
  expect_equal(matset[-2,-c(1,3),], matset12, ignore_attr = TRUE)
  expect_equal(matset[1:2,-2,], matset1213, ignore_attr = TRUE)
  expect_equal(matset[-2, , , keep_annotation = FALSE, warn_class_change = FALSE],
               lst1)
  expect_equal(matset[, -c(1,3), , keep_annotation = FALSE, warn_class_change = FALSE],
               lst2)
  expect_equal(matset[-2, -c(1,3), , keep_annotation = FALSE, warn_class_change = FALSE],
               lst12)
  expect_equal(matset[1:2, -2, , keep_annotation = FALSE, warn_class_change = FALSE],
               lst1213)
  expect_equal(matset[-2, , , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[1,]))
  expect_equal(matset[, -c(1,3), , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[,2]))
  expect_equal(matset[-2, -c(1,3), , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[1,2]))
  expect_equal(matset[1:2, -2, , drop = TRUE], matset1213, ignore_attr = TRUE)

})



test_that("grouped matrixset extraction works for Matrix", {

  as_dge <- function(m) methods::as(methods::as(m, "generalMatrix"), "unpackedMatrix")

  lst <- list(a = Matrix::Matrix(1:24, 4, 6), b = Matrix::Matrix(101:124, 4, 6), c = NULL)
  lst1 <- list(a = Matrix::Matrix(seq(1,21,4),1,6), b = Matrix::Matrix(seq(101,121,4),1,6), c = NULL)
  lst2 <- list(a = Matrix::Matrix(5:8,4,1), b = Matrix::Matrix(105:108,4,1), c = NULL)
  # lst12 <- list(a = methods::as(Matrix::Matrix(5), "dgeMatrix"),
  #               b = methods::as(Matrix::Matrix(105), "dgeMatrix"), c = NULL)
  lst12 <- list(a = as_dge(Matrix::Matrix(5)),
                b = as_dge(Matrix::Matrix(105)), c = NULL)
  rownames(lst12$a) <- rownames(lst12$b) <- "r1"
  colnames(lst12$a) <- colnames(lst12$b) <- "c2"
  lst1213 <- list(a = Matrix::Matrix(c(1,2,9,10),2,2),
                  b = Matrix::Matrix(c(101,102,109,110),2,2), c = NULL)
  rownames(lst1$a) <- rownames(lst1$b) <- "r1"
  colnames(lst1$a) <- colnames(lst1$b) <- paste0("c", 1:6)
  rownames(lst$a) <- rownames(lst$b) <- paste0("r", 1:4)
  colnames(lst$a) <- colnames(lst$b) <- paste0("c", 1:6)
  rownames(lst2$a) <- rownames(lst2$b) <- paste0("r", 1:4)
  colnames(lst2$a) <- colnames(lst2$b) <- "c2"
  rownames(lst1213$a) <- rownames(lst1213$b) <- c("r1", "r2")
  colnames(lst1213$a) <- colnames(lst1213$b) <- c("c1", "c3")
  ri <- data.frame(rowname = paste0("r", 1:4), g = 1:4, h = letters[1:4])
  ci <- data.frame(colname = paste0("c", 1:6),
                   gg = 1:6, foo = rep(c("a", "b"), each=3),
                   bar = rep(c("u","v"), 3))
  ci13 <- ci[c(1,3),,drop=FALSE]
  matset <- matrixset(lst, row_info = ri, column_info = ci,
                      row_tag = "ROW", column_tag = "COL")
  matset1213 <- matrixset(lst1213, row_info = ri[1:2,], column_info = ci13,
                          row_tag = "ROW", column_tag = "COL")

  gms <- row_group_by(matset, g, h)
  expect_identical(row_group_by(matset[1:2,,], g, h), gms[1:2,,])



  expect_equal(gms, gms[])
  expect_equal(row_group_by(matset[1, ,], g, h), gms[1, ,])
  expect_equal(row_group_by(matset[,2, ], g, h), gms[,2, ])
  expect_equal(row_group_by(matset[1,2,], g, h), gms[1,2,])
  expect_equal(row_group_by(matset[1:2,c(1,3),], g, h), gms[1:2,c(1,3),])
  expect_equal(gms[1, , , keep_annotation = FALSE, warn_class_change = FALSE],
               lst1)
  expect_equal(gms[, 2, , keep_annotation = FALSE, warn_class_change = FALSE],
               lst2)
  expect_equal(gms[1, 2, , keep_annotation = FALSE, warn_class_change = FALSE],
               lst12)
  expect_equal(gms[1:2, c(1,3), , keep_annotation = FALSE, warn_class_change = FALSE],
               lst1213)
  expect_equal(gms[1, , , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[1,]))
  expect_equal(gms[, 2, , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[,2]))
  expect_equal(gms[1, 2, , drop = TRUE, warn_class_change = FALSE],
               lapply(lst, function(u) u[1,2]))
  expect_equal(gms[1:2, c(1,3), , drop = TRUE], matset1213, ignore_attr = TRUE)

  expect_equal(gms[matrix(c(1:2, 1, 3), 2), ,], matset1213, ignore_attr = TRUE)

})




# class change warning
test_that("matrixset extraction warns properly for Matrix", {

  as_dge <- function(m) methods::as(methods::as(m, "generalMatrix"), "unpackedMatrix")

  lst <- list(a = Matrix::Matrix(1:6, 2, 3),
              b = Matrix::Matrix(101:106, 2, 3), c = NULL)
  lst1 <- list(a = Matrix::Matrix(seq(1,5,2),1,3),
               b = Matrix::Matrix(seq(101,105,2),1,3), c = NULL)
  lst2 <- list(a = Matrix::Matrix(3:4,2,1),
               b = Matrix::Matrix(103:104,2,1), c = NULL)
  # lst12 <- list(a = methods::as(Matrix::Matrix(3), "dgeMatrix"),
  #               b = methods::as(Matrix::Matrix(103), "dgeMatrix"), c = NULL)
  lst12 <- list(a = as_dge(Matrix::Matrix(3)),
                b = as_dge(Matrix::Matrix(103)), c = NULL)
  lst1213 <- list(a = Matrix::Matrix(c(1,2,5,6),2,2),
                  b = Matrix::Matrix(c(101,102,105,106),2,2), c = NULL)
  rownames(lst$a) <- rownames(lst$b) <- c("r1", "r2")
  rownames(lst1$a) <- rownames(lst1$b) <- "r1"
  rownames(lst12$a) <- rownames(lst12$b) <- "r1"
  rownames(lst2$a) <- rownames(lst2$b) <- c("r1", "r2")
  rownames(lst1213$a) <- rownames(lst1213$b) <- c("r1", "r2")
  colnames(lst$a) <- colnames(lst$b) <- c("c1", "c2", "c3")
  colnames(lst1$a) <- colnames(lst1$b) <- c("c1", "c2", "c3")
  colnames(lst2$a) <- colnames(lst2$b) <- "c2"
  colnames(lst12$a) <- colnames(lst12$b) <- "c2"
  colnames(lst1213$a) <- colnames(lst1213$b) <- c("c1", "c3")
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ri1 <- ri[1,,drop=FALSE]
  ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
  ci2 <- ci[2,,drop=FALSE]
  ci13 <- ci[c(1,3),,drop=FALSE]
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
  matset1 <- matrixset(lst1, row_info = ri1, column_info = ci, row_tag = "foo", column_tag = "bar")
  matset2 <- matrixset(lst2, row_info = ri, column_info = ci2, row_tag = "foo", column_tag = "bar")
  matset12 <- matrixset(lst12, row_info = ri1, column_info = ci2, row_tag = "foo", column_tag = "bar")
  matset1213 <- matrixset(lst1213, row_info = ri, column_info = ci13, row_tag = "foo", column_tag = "bar")


  warn_msg <- "Result object is no longer a matrixset"
  expect_warning(matset[1, , , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[, 2, , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[1, 2, , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[1:2, c(1,3), , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[1, , , drop = TRUE], warn_msg)
  expect_warning(matset[, 2, , drop = TRUE, ], warn_msg)
  expect_warning(matset[1, 2, , drop = TRUE, ], warn_msg)

  # test char
  expect_warning(matset["r1", , , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[, "c2", , keep_annotation = FALSE], warn_msg)
  expect_warning(matset["r1", "c2", , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[c("r1", "r2"), c("c1","c3"), , keep_annotation = FALSE], warn_msg)
  expect_warning(matset["r1", , , drop = TRUE], warn_msg)
  expect_warning(matset[, "c2", , drop = TRUE], warn_msg)
  expect_warning(matset["r1", "c2", , drop = TRUE], warn_msg)

  # test logical
  expect_warning(matset[c(T,F), , , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[, c(F,T,F), , keep_annotation = FALSE], warn_msg)
  expect_warning(matset["r1", "c2", , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[c(T, T), c(T,F,T), , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[c(T,F), , , drop = TRUE], warn_msg)
  expect_warning(matset[, c(F,T,F), , drop = TRUE], warn_msg)
  expect_warning(matset[c(T,F), c(F,T,F), , drop = TRUE], warn_msg)

  # test neg index (removal)
  expect_warning(matset[-2, , , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[, -c(1,3), , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[-2, -c(1,3), , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[1:2, -2, , keep_annotation = FALSE], warn_msg)
  expect_warning(matset[-2, , , drop = TRUE], warn_msg)
  expect_warning(matset[, -c(1,3), , drop = TRUE], warn_msg)
  expect_warning(matset[-2, -c(1,3), , drop = TRUE], warn_msg)

})



test_that("matrixset extraction reports errors properly for Matrix", {

  lst <- list(a = Matrix::Matrix(1:6, 2, 3),
              b = Matrix::Matrix(101:106, 2, 3), c = NULL)
  matset_nonm <- matrixset(lst)
  rownames(lst$a) <- rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")

  expect_error(matset[,], "incorrect number of dimensions")
  expect_error(matset[,], "incorrect number of dimensions")
  expect_error(matset[1,], "incorrect number of dimensions")
  expect_error(matset[,1], "incorrect number of dimensions")
  expect_error(matset[1, keep_annotation=TRUE,drop=FALSE], "incorrect number of dimensions")

  expect_error(matset[T, , ], "logical subscript not of appropriate length")
  expect_error(matset[c(T,T,T), , ], "logical subscript not of appropriate length")
  # expect_error(matset[c(F,F), ,], "logical subscript has no TRUEs")
  expect_error(matset["r3", ,], "character subscript has no matches")
  expect_error(matset[list(1,2),,], "subscript type not handled")
  expect_error(matset_nonm["r1",,], "character subscript has no matches")
  expect_error(matset[matrix(c(1,2,2,3), 2), 2, ], "index j must not be provided if i is a matrix")
  expect_error(matset[matrix(1:6, 2, 3), ,], "matrix for extraction must have 2 columns")
  expect_error(matset[NA, ,], "NAs are not allowed for indexing matrixset objects")
  expect_error(matset[,,matrix=NA], "NAs are not allowed for indexing matrixset objects")
  expect_error(matset[,,matrix=0], "wrong subscript specification: nothing to subset")
  expect_error(matset[0, ,], "wrong subscript specification: nothing to subset")



})


