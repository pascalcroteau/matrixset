# replace by NULL everywhere                        done
# replace by NULL some places                       done
# replace all matrices by same matrix               done
# replace some matrices by same matrix              done
# replace all matrices by list (no NULLs)           done
# replace some matrices by list (no NULLS)          done
# replace all matrices by list (with NULLs)         done
# replace some matrices by list (with NULLS)        done
# replace all matrices by matrixset                 done
# replace some matrices by matrixset                done


# replace by vectors
# TEST MATRIX i


test_that("matrixset replacement works", {

  amat <- matrix(1:6, 2, 3)
  bmat <- matrix(101:106, 2, 3)
  repamat <- matrix(c(11,2,13,4,15,6),2,3)
  repbmat <- matrix(c(11,102,13,104,15,106),2,3)
  rownames(amat) <- rownames(bmat) <- rownames(repamat)<- rownames(repbmat) <- c("r1", "r2")
  colnames(amat) <- colnames(bmat) <- colnames(repamat)<- colnames(repbmat) <- c("c1", "c2", "c3")
  lst <- list(a = amat, b = bmat, c = NULL)
  ri <- tibble::tibble(rowname = c("r1", "r2"), g = 1:2)
  ci <- tibble::tibble(colname = c("c1", "c2", "c3"), h = 1:3)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
  matset_NULL <- matrixset(list(a=NULL, b=NULL, c=NULL),
                           row_tag = "foo", column_tag = "bar")
  matset_bNULL <- matrixset(list(a=amat, b=NULL, c=NULL), row_info = ri,
                            column_info = ci, row_tag = "foo", column_tag = "bar")
  matseta <- matrixset(list(a=amat, b=amat, c=amat), row_info = ri,
                       column_info = ci, row_tag = "foo", column_tag = "bar")
  matset10s <- matrixset(list(a = repamat, b = repbmat),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  matset10sc <- matrixset(list(a = repamat, b = repbmat, c = NULL),
                          row_info = ri, column_info = ci,
                          row_tag = "foo", column_tag = "bar")
  matsetnewb <- matrixset(list(a = amat, b = repbmat, c = NULL),
                          row_info = ri, column_info = ci,
                          row_tag = "foo", column_tag = "bar")


  matset2 <- matset
  matset2[] <- NULL
  expect_equal(matset2, matset_NULL)

  matset2 <- matset
  matset2[,,matrix = "b"] <- NULL
  expect_equal(matset2, matset_bNULL)

  matset2 <- matset
  matset2[] <- amat
  expect_equal(matset2, matseta)

  matset2 <- matset[,,matrix = -3]
  matset2[1, ,] <- matrix(c(11,13,15),1,3)
  expect_equal(matset2, matset10s)

  matset2 <- matset[,,matrix = -3]
  matset2["r1", ,] <- matrix(c(11,13,15),1,3)
  expect_equal(matset2, matset10s)

  matset2 <- matset[,,matrix = -3]
  matrep <- matrix(c(11,13,15),1,3)
  rownames(matrep) <- "r1"
  matset2["r1", ,] <- matrep
  expect_equal(matset2, matset10s)





  matset2 <- matset
  matset2[1, , matrix=c("b", "a")] <- matrix(c(11,13,15),1,3)
  expect_equal(matset2, matset10sc)

  matset2 <- matset
  matset2[1, , matrix=c("b", "a")] <- c(11,13,15)
  expect_equal(matset2, matset10sc)

  matset2 <- matset
  matset2[1, , matrix=c("b", "a")] <- matrix(c(11,13,15),1,3)
  expect_equal(matset2, matset10sc)

  matset2 <- matset
  matset2[1, , matrix="b"] <- matrix(c(11,13,15),1,3)
  expect_equal(matset2, matsetnewb)


  matset2 <- matset[,,matrix = -3]
  suppressWarnings(matset2[1, ,] <- list(b = matrix(c(11,13,15),1,3),
                                        a = matrix(c(1001,1003,1005),1,3)))
  amatnew <- amat
  bmatnew <- bmat
  amatnew[1, ] <- matrix(c(11,13,15),1,3)
  bmatnew[1, ] <- matrix(c(1001,1003,1005),1,3)
  newmatset <- matrixset(list(a = amatnew, b = bmatnew),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  expect_equal(matset2, newmatset)


  matset2 <- matset[,,matrix = -3]
  suppressWarnings(matset2["r1", ,] <- list(b = matrix(c(11,13,15),1,3),
                                        a = matrix(c(1001,1003,1005),1,3)))
  amatnew <- amat
  bmatnew <- bmat
  amatnew[1, ] <- matrix(c(11,13,15),1,3)
  bmatnew[1, ] <- matrix(c(1001,1003,1005),1,3)
  newmatset <- matrixset(list(a = amatnew, b = bmatnew),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  expect_equal(matset2, newmatset)


  matset2 <- matset
  matset2[1, , matrix=c(2,1)] <- list(b = matrix(c(11,13,15),1,3),
                                           a = matrix(c(1001,1003,1005),1,3))
  amatnew <- amat
  bmatnew <- bmat
  amatnew[1, ] <- matrix(c(1001,1003,1005),1,3)
  bmatnew[1, ] <- matrix(c(11,13,15),1,3)
  newmatset <- matrixset(list(a = amatnew, b = bmatnew, c = NULL),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  expect_equal(matset2, newmatset)


  matset2 <- matset
  matset2[1, , matrix=c("b", "a")] <- list(b = matrix(c(11,13,15),1,3),
                                           a = matrix(c(1001,1003,1005),1,3))
  amatnew <- amat
  bmatnew <- bmat
  amatnew[1, ] <- matrix(c(1001,1003,1005),1,3)
  bmatnew[1, ] <- matrix(c(11,13,15),1,3)
  newmatset <- matrixset(list(a = amatnew, b = bmatnew, c = NULL),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  expect_equal(matset2, newmatset)


  matset2 <- matset
  matset2[1, , matrix="b"] <- list(b = matrix(c(11,13,15),1,3),
                                   a = matrix(c(1001,1003,1005),1,3))
  bmatnew <- bmat
  bmatnew[1, ] <- matrix(c(11,13,15),1,3)
  newmatset <- matrixset(list(a = amat, b = bmatnew, c = NULL),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  expect_equal(matset2, newmatset)


  matset2 <- matset
  matset2[1, , matrix="b"] <- list(b = matrix(c(11,13,15),1,3))
  bmatnew <- bmat
  bmatnew[1, ] <- matrix(c(11,13,15),1,3)
  newmatset <- matrixset(list(a = amat, b = bmatnew, c = NULL),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  expect_equal(matset2, newmatset)


  matset2 <- matset[,,matrix = -3]
  suppressWarnings( matset2[] <- list(b = matrix(11:16,2,3),
                    a = NULL))
  bmatnew <- bmat
  bmatnew[] <- matrix(11:16,2,3)
  newmatset <- matrixset(list(a = bmatnew, b = NULL),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  expect_equal(matset2, newmatset)


  matset2 <- matset
  matset2[,,matrix=c("b", "a")] <- list(b = matrix(11:16,2,3),
                                      a = NULL,
                                      c = matrix(1001:1006,2,3))
  bmatnew <- bmat
  bmatnew[] <- matrix(11:16,2,3)
  newmatset <- matrixset(list(a = NULL, b = bmatnew, c = NULL),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  expect_equal(matset2, newmatset)


  matset2 <- matset
  matset2[,,matrix="b"] <- list(b = matrix(11:16,2,3),
                              a = NULL)
  bmatnew <- bmat
  bmatnew[] <- matrix(11:16,2,3)
  newmatset <- matrixset(list(a = amat, b = bmatnew, c = NULL),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  expect_equal(matset2, newmatset)


  matset2 <- matset
  matset2[,,matrix="b"] <- list(a = matrix(11:16,2,3),
                              b = NULL)
  newmatset <- matrixset(list(a = amat, b = NULL, c = NULL),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  expect_equal(matset2, newmatset)


  matset2 <- matset
  matset2[,,matrix="b"] <- list(b = NULL)
  newmatset <- matrixset(list(a = amat, b = NULL, c = NULL),
                         row_info = ri, column_info = ci,
                         row_tag = "foo", column_tag = "bar")
  expect_equal(matset2, newmatset)



  lst <- list(a = matrix(1:6, 2, 3), b = matrix(letters[1:6], 2, 3))
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
  matset[1, , ] <- 0:2



  lst <- list(a = matrix(1:6, 2, 3), b = matrix(letters[1:6], 2, 3), c = NULL)
  rownames(lst$a) <- c("r1", "r2")
  rownames(lst$b) <- c("r1", "r2")
  colnames(lst$a) <- c("c1", "c2", "c3")
  colnames(lst$b) <- c("c1", "c2", "c3")
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
  matset[1, , -3] <- 0:2


})


test_that("matrixset replacement by matrixset works", {

  amat <- matrix(1:6, 2, 3)
  bmat <- matrix(101:106, 2, 3)
  amat2 <- matrix(1001:1006, 2, 3)
  bmat2 <- matrix(10001:10006, 2, 3)
  rownames(amat) <- rownames(bmat) <- rownames(amat2) <- rownames(bmat2) <- c("r1", "r2")
  colnames(amat) <- colnames(bmat) <- colnames(amat2) <- colnames(bmat2) <- c("c1", "c2", "c3")
  lst <- list(a = amat, b = bmat, c = NULL)
  lst2 <- list(a = amat2, b = bmat2, c = NULL)
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
  ri2 <- data.frame(rowname = c("r1", "r2"), g = 11:12)
  ci2 <- data.frame(colname = c("c1", "c2", "c3"), h = 11:13)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
  matset2 <- matrixset(lst2, row_info = ri2, column_info = ci2, row_tag = "foo", column_tag = "bar")
  matsetc <- matrixset(lst2, row_info = ri2[, 1:2], column_info = ci2[, 1:2],
                       row_tag = "foo", column_tag = "bar")
  matsetc1 <- matrixset(purrr::map2(lst, lst2,
                                    function(x, y) {x[1, ] <- y[1, ]; x}),
                        row_info = rbind(ri2[1, 1:2], ri[2,]),
                        column_info = ci2[, 1:2],
                        row_tag = "foo", column_tag = "bar")

  matsetr <- matset
  matsetr[] <- matset2
  expect_equal(matsetr, matsetc)

  matsetr <- matset
  matsetr[1, ,] <- matset2[1, ,]
  expect_equal(matsetr, matsetc1)

  matsetr <- matset
  matsetr["r1", ,] <- matset2[1, ,]
  expect_equal(matsetr, matsetc1)

})


test_that("matrixset replacement by matrixset fails when it should", {

  amat <- matrix(1:6, 2, 3)
  bmat <- matrix(101:106, 2, 3)
  amat2 <- matrix(1001:1006, 2, 3)
  bmat2 <- matrix(10001:10006, 2, 3)
  rownames(amat) <- rownames(bmat) <- rownames(amat2) <- rownames(bmat2) <- c("r1", "r2")
  colnames(amat) <- colnames(bmat) <- colnames(amat2) <- colnames(bmat2) <- c("c1", "c2", "c3")
  lst <- list(a = amat, b = bmat, c = NULL)
  lst2 <- list(a = amat2, b = bmat2, c = bmat2)
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
  ri2 <- data.frame(rowname = c("r1", "r2"), g = 11:12)
  ci2 <- data.frame(colname = c("c1", "c2", "c3"), h = 11:13)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
  matset2 <- matrixset(lst2, row_info = ri2, column_info = ci2, row_tag = "goo", column_tag = "war")

  matset2 <- matrixset(lst2,
                       row_info = data.frame(rowname = c("r1", "r2"), g = 11:12, h = 21:22),
                       column_info = data.frame(colname = c("c1", "c2", "c3"), gg = 11:13, hh = 21:23),
                       row_tag = "foo", column_tag = "bar")
  matsetr <- matset
  expect_error(matsetr[] <- matset2, "incompatible number of row traits for replacement")

  expect_error(matsetr[, , -3] <- 1:3, "dimension of items to replace is not vector compatible")
  expect_error(matsetr[, 1:2, -3] <- 1:3, "dimension of items to replace is not vector compatible")
  expect_error(matsetr[1:2, 1:2, -3] <- 1:3, "dimension of items to replace is not vector compatible")
  expect_error(matsetr[1:2, , -3] <- 1:3, "dimension of items to replace is not vector compatible")
  expect_error(matsetr[, 1, -3] <- 1:3, "number of items to replace is not a multiple of replacement length")
  expect_error(matsetr[1, , -3] <- 1:2, "number of items to replace is not a multiple of replacement length")
  expect_error(matsetr[1:2, 1, -3] <- 1:3, "number of items to replace is not a multiple of replacement length")


  matset2 <- matrixset(lst2, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
  matsetr <- matset
  expect_error(matsetr[1, ,] <- matset2[1, ,], "Can only replace NULL by another NULL, or a matrix with full rows and columns \\(i\\.e\\. matches matrixset dimensions\\)")

})


test_that("matrixset replacement reports warnings", {

  amat <- matrix(1:6, 2, 3)
  bmat <- matrix(101:106, 2, 3)
  rownames(amat) <- rownames(bmat) <- c("r1", "r2")
  colnames(amat) <- colnames(bmat) <- c("c1", "c2", "c3")
  lst <- list(a = amat, b = bmat, c = NULL)
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")

  matset2 <- matset[,,matrix = -3]
  expect_warning(matset2[1, ,] <- list(b = matrix(c(11,13,15),1,3),
                                      a = matrix(c(1001,1003,1005),1,3)),
                 "not all list names match the matrix names\\.")


  matset2 <- matset
  expect_warning(matset2[1, , matrix=1:2] <- list(b = matrix(c(11,13,15),1,3),
                                      a = matrix(c(1001,1003,1005),1,3)),
                 "not all list names match the matrix names\\.")



  amat <- matrix(1:6, 2, 3)
  bmat <- matrix(101:106, 2, 3)
  amat2 <- matrix(1001:1006, 2, 3)
  bmat2 <- matrix(10001:10006, 2, 3)
  rownames(amat) <- rownames(bmat) <- rownames(amat2) <- rownames(bmat2) <- c("r1", "r2")
  colnames(amat) <- colnames(bmat) <- colnames(amat2) <- colnames(bmat2) <- c("c1", "c2", "c3")
  lst <- list(a = amat, b = bmat, c = NULL)
  lst2 <- list(a = amat2, b = bmat2, c = NULL)
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
  ri2 <- data.frame(rowname = c("r1", "r2"), g = 11:12)
  ci2 <- data.frame(colname = c("c1", "c2", "c3"), h = 11:13)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
  matset2 <- matrixset(lst2, row_info = ri2, column_info = ci2, row_tag = "foo", column_tag = "bar")
  matsetc <- matrixset(lst2, row_info = ri2[, 1:2], column_info = ci2[, 1:2],
                       row_tag = "foo", column_tag = "bar")
  matsetc1 <- matrixset(purrr::map2(lst, lst2,
                                    function(x, y) {x[1, ] <- y[1, ]; x}),
                        row_info = rbind(ri2[1, 1:2], ri[2,]),
                        column_info = ci2[, 1:2],
                        row_tag = "foo", column_tag = "bar")

  matsetr <- matset
  expect_warning(matsetr["r1", ,] <- matset2[2, ,],
                 "Not all rownames match with replacement value")


  amat <- matrix(1:6, 2, 3)
  bmat <- matrix(101:106, 2, 3)
  amat2 <- matrix(1001:1006, 2, 3)
  bmat2 <- matrix(10001:10006, 2, 3)
  rownames(amat) <- rownames(bmat) <- rownames(amat2) <- rownames(bmat2) <- c("r1", "r2")
  colnames(amat) <- colnames(bmat) <- colnames(amat2) <- colnames(bmat2) <- c("c1", "c2", "c3")
  lst <- list(a = amat, b = bmat, c = NULL)
  lst2 <- list(a = amat2, b = bmat2, c = bmat2)
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
  ri2 <- data.frame(rowname = c("r1", "r2"), g = 11:12)
  ci2 <- data.frame(colname = c("c1", "c2", "c3"), h = 11:13)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")
  matset2 <- matrixset(lst2, row_info = ri2, column_info = ci2, row_tag = "goo", column_tag = "war")

  matsetr <- matset
  suppressWarnings(
    expect_warning(matsetr[] <- matset2, "Not all replacement traits share the same name")
  )

  matset2 <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "goo", column_tag = "war")
  matsetr <- matset
  suppressWarnings(
    expect_warning(matsetr[] <- matset2, "Not all replacement traits share the same name")
  )


})


test_that("matrixset replacement reports errors", {


  amat <- matrix(1:6, 2, 3)
  bmat <- matrix(101:106, 2, 3)
  repamat <- matrix(c(11,2,13,4,15,6),2,3)
  repbmat <- matrix(c(11,102,13,104,15,106),2,3)
  rownames(amat) <- rownames(bmat) <- c("r1", "r2")
  colnames(amat) <- colnames(bmat) <- c("c1", "c2", "c3")
  lst <- list(a = amat, b = bmat, c = NULL)
  ri <- data.frame(rowname = c("r1", "r2"), g = 1:2)
  ci <- data.frame(colname = c("c1", "c2", "c3"), h = 1:3)
  matset <- matrixset(lst, row_info = ri, column_info = ci, row_tag = "foo", column_tag = "bar")


  expect_error(matset[,] <- 1, "incorrect number of dimensions")
  expect_error(matset[,] <- 1, "incorrect number of dimensions")
  expect_error(matset[1,] <- 1, "incorrect number of dimensions")
  expect_error(matset[,1] <- 1, "incorrect number of dimensions")


  matset2 <- matset[,,matrix = -3]
  expect_error(suppressWarnings(matset2[1, ,] <- list(b = matrix(c(11,13,15),1,3),
                                    a = NULL)),
               "can only replace the whole matrix by NULL, not a subset")


  matset2 <- matset
  expect_error(matset2[1, ,] <- matrix(c(11,13,15),1,3),
               "Can only replace NULL by another NULL, or a matrix with full rows and columns \\(i\\.e\\. matches matrixset dimensions\\)")


  matset2 <- matset
  expect_error(matset2[1, , matrix="b"] <- list(a = matrix(c(1001,1003,1005),1,3)),
               "Can't replace with value because list names do not match")

  matset2 <- matset
  expect_error(matset2[,,matrix=2] <- list(b = matrix(11:16,2,3),
                              a = NULL),
               "list is the not same length as the number of matrix elements to replace")

  expect_error(matset2[0, ,] <- matrix(c(11,13,15),1,3),
               "wrong subscript specification: nothing to subset")

})


