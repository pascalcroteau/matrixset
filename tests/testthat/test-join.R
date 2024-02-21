test_that("matrixset row info join works", {

  ms1 <- remove_row_annotation(student_results, class, teacher)
  suppressWarnings(ms <- join_row_info(ms1, student_results))
  ms_ref <- student_results
  ri <- row_info(ms_ref)
  ri <- dplyr::mutate(ri, previous_year_score.y = previous_year_score)
  ri <- dplyr::rename(ri, previous_year_score.x = previous_year_score)
  ri <- ri[, c(".rowname", "previous_year_score.x", "class", "teacher", "previous_year_score.y")]
  row_info(ms_ref) <- ri
  expect_identical(ms, ms_ref)



  ms <- join_row_info(ms1, student_results, by = c(".rowname", "previous_year_score"))
  ms_ref <- student_results
  ri <- row_info(ms_ref)
  ri <- ri[, c(".rowname", "previous_year_score", "class", "teacher")]
  row_info(ms_ref) <- ri
  expect_identical(ms, ms_ref)



  expect_error(join_row_info(ms1, student_results, by = c(".rowname", "previous_score")),
               "variable \"previous_score\" is not a known trait")


  meta <- tibble::tibble(sample = c("student 2", "student 2"),
                         msr = c("height", "weight"),
                         value = c(145, 32))
  expect_error(join_row_info(student_results, meta, by = c(".rowname"="sample")),
               "'by' does not result in unique row names")


  expect_warning(join_row_info(student_results, meta, by = c(".rowname"="sample"),
                               adjust = TRUE, names_glue = TRUE),
                 "row names \\(\\.rowname\\) have changed following matrix adjustment")


  suppressWarnings(ms <- join_row_info(student_results, meta, by = c(".rowname"="sample"),
                      adjust = TRUE, names_glue = TRUE))
  ms_ref_inf <- dplyr::left_join(row_info(student_results), meta,
                                 by = c(".rowname"="sample"))
  nms <- ms_ref_inf$.rowname
  idx <- match(nms, rownames(student_results))
  unq_id <- unique_id(nms, length(nms))
  nms[unq_id > 0] <- paste(nms[unq_id > 0], unq_id[unq_id > 0], sep = "..")
  ms_ref_inf$.rowname <- nms
  m <- student_results[,,, keep_annotation = FALSE, warn_class_change = FALSE]
  m <- lapply(seq_along(m), function(mi) {
    M <- m[[mi]][idx, ]
    rownames(M) <- nms
    M
  })
  names(m) <- matrixnames(student_results)
  ms_ref <- matrixset(m, row_info = ms_ref_inf, row_key = ".rowname",
                      column_info = column_info(student_results),
                      column_key = ".colname")
  expect_identical(ms, ms_ref)


  suppressWarnings(ms <- join_row_info(student_results, meta, by = c(".rowname"="sample"),
                      adjust = TRUE, names_glue = "{.tag}"))
  expect_identical(ms, ms_ref)

  suppressWarnings(ms <- join_row_info(student_results, meta, by = c(".rowname"="sample"),
                      adjust = TRUE, names_glue = "{.tag}_{msr}"))
  ms_ref_inf <- dplyr::left_join(row_info(student_results), meta,
                                 by = c(".rowname"="sample"))
  nms <- ms_ref_inf$.rowname
  idx <- match(nms, rownames(student_results))
  unq_id <- unique_id(nms, length(nms))
  nms[unq_id > 0] <- paste(nms[unq_id > 0], ms_ref_inf$msr[unq_id > 0], sep = "_")
  ms_ref_inf$.rowname <- nms
  m <- student_results[,,, keep_annotation = FALSE, warn_class_change = FALSE]
  m <- lapply(seq_along(m), function(mi) {
    M <- m[[mi]][idx, ]
    rownames(M) <- nms
    M
  })
  names(m) <- matrixnames(student_results)
  ms_ref <- matrixset(m, row_info = ms_ref_inf, row_key = ".rowname",
                      column_info = column_info(student_results),
                      column_key = ".colname")
  expect_identical(ms, ms_ref)

  expect_error(purrr::quietly(join_row_info)(student_results, meta, by = c(".rowname"="sample"),
                             adjust = TRUE, names_glue = "{.tag}_{msr2}"),
               "object 'msr2' not found")


  meta <- tibble::tibble(sample = c("student 2", "student 2"),
                         class = c("classA", "classA"),
                         msr = c("height", "weight"),
                         value = c(145, 32))
  suppressWarnings(ms <- join_row_info(student_results, meta, by = c(".rowname"="sample"),
                      adjust = TRUE, names_glue = TRUE))
  ms_ref_inf <- dplyr::left_join(row_info(student_results), meta,
                                 by = c(".rowname"="sample"))
  nms <- ms_ref_inf$.rowname
  idx <- match(nms, rownames(student_results))
  unq_id <- unique_id(nms, length(nms))
  nms[unq_id > 0] <- paste(nms[unq_id > 0], unq_id[unq_id > 0], sep = "..")
  ms_ref_inf$.rowname <- nms
  m <- student_results[,,, keep_annotation = FALSE, warn_class_change = FALSE]
  m <- lapply(seq_along(m), function(mi) {
    M <- m[[mi]][idx, ]
    rownames(M) <- nms
    M
  })
  names(m) <- matrixnames(student_results)
  ms_ref <- matrixset(m, row_info = ms_ref_inf, row_key = ".rowname",
                      column_info = column_info(student_results),
                      column_key = ".colname")
  expect_identical(ms, ms_ref)


  suppressWarnings(ms <- join_row_info(student_results, meta, by = c(".rowname"="sample"),
                      adjust = TRUE, names_glue = "{.tag}"))
  expect_identical(ms, ms_ref)

  w <- purrr::quietly(join_row_info)(student_results, meta,
                                     by = c(".rowname"="sample", "class"),
                                     adjust = TRUE, names_glue = TRUE)
  w <- w$warnings
  expect_true(any(w == "some traits have changed type ('class')"))


  suppressWarnings(ms <- join_row_info(student_results, meta, by = c(".rowname"="sample", "class"),
                      adjust = TRUE, names_glue = TRUE))
  ms_ref_inf <- dplyr::left_join(row_info(student_results), meta,
                                 by = c(".rowname"="sample", "class"))
  nms <- ms_ref_inf$.rowname
  idx <- match(nms, rownames(student_results))
  unq_id <- unique_id(nms, length(nms))
  nms[unq_id > 0] <- paste(nms[unq_id > 0], unq_id[unq_id > 0], sep = "..")
  ms_ref_inf$.rowname <- nms
  m <- student_results[,,, keep_annotation = FALSE, warn_class_change = FALSE]
  m <- lapply(seq_along(m), function(mi) {
    M <- m[[mi]][idx, ]
    rownames(M) <- nms
    M
  })
  names(m) <- matrixnames(student_results)
  ms_ref <- matrixset(m, row_info = ms_ref_inf, row_key = ".rowname",
                      column_info = column_info(student_results),
                      column_key = ".colname")
  expect_identical(ms, ms_ref)


  suppressWarnings(ms <- join_row_info(student_results, meta, by = c(".rowname"="sample"),
                      adjust = TRUE, names_glue = "{.tag}_{msr}"))
  ms_ref_inf <- dplyr::left_join(row_info(student_results), meta,
                                 by = c(".rowname"="sample"))
  nms <- ms_ref_inf$.rowname
  idx <- match(nms, rownames(student_results))
  unq_id <- unique_id(nms, length(nms))
  nms[unq_id > 0] <- paste(nms[unq_id > 0], ms_ref_inf$msr[unq_id > 0], sep = "_")
  ms_ref_inf$.rowname <- nms
  m <- student_results[,,, keep_annotation = FALSE, warn_class_change = FALSE]
  m <- lapply(seq_along(m), function(mi) {
    M <- m[[mi]][idx, ]
    rownames(M) <- nms
    M
  })
  names(m) <- matrixnames(student_results)
  ms_ref <- matrixset(m, row_info = ms_ref_inf, row_key = ".rowname",
                      column_info = column_info(student_results),
                      column_key = ".colname")
  expect_identical(ms, ms_ref)


  w <- purrr::quietly(join_row_info)(row_group_by(student_results, class), meta,
                by = c(".rowname"="sample"), adjust = TRUE, names_glue = TRUE)
  w <- w$warnings
  expect_true(any(w == "grouping structure of '.ms_x' has changed."))


  ms1 <- remove_column_annotation(student_results, national_average, school_average)
  suppressWarnings(ms <- join_column_info(ms1, student_results))
  ms_ref <- student_results
  ci <- column_info(ms_ref)
  ci <- dplyr::mutate(ci, program.y = program)
  ci <- dplyr::rename(ci, program.x = program)
  ci <- ci[, c(".colname", "program.x", "national_average", "school_average", "program.y")]
  column_info(ms_ref) <- ci
  expect_identical(ms, ms_ref)



  meta <- tibble::tibble(field = c("Mathematics", "Mathematics"),
                         level = c("regular", "advanced"),
                         value = c(100, 250))
  expect_error(join_column_info(student_results, meta, by = c(".colname"="field")),
               "'by' does not result in unique col names")

  expect_warning(join_column_info(student_results, meta, by = c(".colname"="field"),
                                  adjust = TRUE, names_glue = TRUE),
                 "col names \\(\\.colname\\) have changed following matrix adjustment")

  suppressWarnings(ms <- join_column_info(student_results, meta, by = c(".colname"="field"),
                         adjust = TRUE, names_glue = TRUE))
  ms_ref_inf <- dplyr::left_join(column_info(student_results), meta,
                                 by = c(".colname"="field"))
  nms <- ms_ref_inf$.colname
  idx <- match(nms, colnames(student_results))
  unq_id <- unique_id(nms, length(nms))
  nms[unq_id > 0] <- paste(nms[unq_id > 0], unq_id[unq_id > 0], sep = "..")
  ms_ref_inf$.colname <- nms
  m <- student_results[,,, keep_annotation = FALSE, warn_class_change = FALSE]
  m <- lapply(seq_along(m), function(mi) {
    M <- m[[mi]][, idx]
    colnames(M) <- nms
    M
  })
  names(m) <- matrixnames(student_results)
  ms_ref <- matrixset(m, row_info = row_info(student_results),
                      row_key = ".rowname",
                      column_info = ms_ref_inf,
                      column_key = ".colname")
  expect_identical(ms, ms_ref)


  suppressWarnings(ms <- join_column_info(student_results, meta, by = c(".colname"="field"),
                         adjust = TRUE, names_glue = "{.tag}"))
  expect_identical(ms, ms_ref)

  suppressWarnings(ms <- join_column_info(student_results, meta, by = c(".colname"="field"),
                         adjust = TRUE, names_glue = "{.tag}_{level}"))
  ms_ref_inf <- dplyr::left_join(column_info(student_results), meta,
                                 by = c(".colname"="field"))
  nms <- ms_ref_inf$.colname
  idx <- match(nms, colnames(student_results))
  unq_id <- unique_id(nms, length(nms))
  nms[unq_id > 0] <- paste(nms[unq_id > 0], ms_ref_inf$level[unq_id > 0], sep = "_")
  ms_ref_inf$.colname <- nms
  m <- student_results[,,, keep_annotation = FALSE, warn_class_change = FALSE]
  m <- lapply(seq_along(m), function(mi) {
    M <- m[[mi]][, idx]
    colnames(M) <- nms
    M
  })
  names(m) <- matrixnames(student_results)
  ms_ref <- matrixset(m, row_info = row_info(student_results),
                      row_key = ".rowname",
                      column_info = ms_ref_inf, column_key = ".colname")
  expect_identical(ms, ms_ref)

  expect_error(purrr::quietly(join_column_info)(student_results, meta, by = c(".colname"="field"),
                                adjust = TRUE, names_glue = "{.tag}_{level2}"),
               "object 'level2' not found")


  meta <- tibble::tibble(field = c("Mathematics", "Mathematics"),
                         program = c("Applied Science", "Applied Science"),
                         level = c("regular", "advanced"),
                         value = c(100, 250))
  suppressWarnings(ms <- join_column_info(student_results, meta, by = c(".colname"="field"),
                                          adjust = TRUE, names_glue = TRUE))
  ms_ref_inf <- dplyr::left_join(column_info(student_results), meta,
                                 by = c(".colname"="field"))
  nms <- ms_ref_inf$.colname
  idx <- match(nms, colnames(student_results))
  unq_id <- unique_id(nms, length(nms))
  nms[unq_id > 0] <- paste(nms[unq_id > 0], unq_id[unq_id > 0], sep = "..")
  ms_ref_inf$.colname <- nms
  m <- student_results[,,, keep_annotation = FALSE, warn_class_change = FALSE]
  m <- lapply(seq_along(m), function(mi) {
    M <- m[[mi]][, idx]
    colnames(M) <- nms
    M
  })
  names(m) <- matrixnames(student_results)
  ms_ref <- matrixset(m, row_info = row_info(student_results),
                      row_key = ".rowname", column_info = ms_ref_inf,
                      column_key = ".colname")
  expect_identical(ms, ms_ref)


  suppressWarnings(ms <- join_column_info(student_results, meta, by = c(".colname"="field"),
                                          adjust = TRUE, names_glue = "{.tag}"))
  expect_identical(ms, ms_ref)

  suppressWarnings(ms <- join_column_info(student_results, meta, by = c(".colname"="field"),
                                          adjust = TRUE, names_glue = "{.tag}_{level}"))
  ms_ref_inf <- dplyr::left_join(column_info(student_results), meta,
                                 by = c(".colname"="field"))
  nms <- ms_ref_inf$.colname
  idx <- match(nms, colnames(student_results))
  unq_id <- unique_id(nms, length(nms))
  nms[unq_id > 0] <- paste(nms[unq_id > 0], ms_ref_inf$level[unq_id > 0], sep = "_")
  ms_ref_inf$.colname <- nms
  m <- student_results[,,, keep_annotation = FALSE, warn_class_change = FALSE]
  m <- lapply(seq_along(m), function(mi) {
    M <- m[[mi]][, idx]
    colnames(M) <- nms
    M
  })
  names(m) <- matrixnames(student_results)
  ms_ref <- matrixset(m, row_info = row_info(student_results),
                      row_key = ".rowname", column_info = ms_ref_inf,
                      column_key = ".colname")
  expect_identical(ms, ms_ref)




  ms1 <- remove_row_annotation(filter_row(student_results, class %in% c("classA", "classC")),
                               class, teacher)
  suppressWarnings(ms <- join_row_info(ms1, student_results))
  ms_ref <- filter_row(student_results, class %in% c("classA", "classC"))
  ri <- row_info(ms_ref)
  ri <- dplyr::mutate(ri, previous_year_score.y = previous_year_score)
  ri <- dplyr::rename(ri, previous_year_score.x = previous_year_score)
  ri <- ri[, c(".rowname", "previous_year_score.x", "class", "teacher", "previous_year_score.y")]
  row_info(ms_ref) <- ri
  expect_identical(ms, ms_ref)



  expect_error(join_row_info(ms1, student_results, type = "full"),
               "the number of rows is modified by the join operation, which is against the 'matrixset' paradigm\\. Use 'adjust' to still perform the operation\\.")



  ms2 <- remove_row_annotation(ms1, previous_year_score)
  ms <- join_row_info(ms2, student_results, type = "full", adjust = TRUE)
  m <- student_results[c(1:5, 11:15), , , keep_annotation = FALSE, warn_class_change = FALSE]
  mNA <- matrix(NA_real_, 10, 3)
  rownames(mNA) <- paste("student", c(6:10, 16:20))
  colnames(mNA) <- colnames(student_results)
  m <- lapply(m, function(M) rbind(M, mNA))
  ri <- row_info(student_results)
  ri <- ri[c(1:5, 11:15, 6:10, 16:20), ]
  ci <- column_info(student_results)
  ms_ref <- matrixset(m, row_info = ri, column_info = ci, row_key = ".rowname",
                      column_key = ".colname")
  expect_identical(ms, ms_ref)



  ms1 <- remove_column_annotation(filter_column(student_results, program == "Applied Science"),
                                  program)
  suppressWarnings(ms <- join_column_info(ms1, student_results))
  ms_ref <- filter_column(student_results, program == "Applied Science")
  ci <- column_info(ms_ref)
  ci <- dplyr::mutate(ci, national_average.y = national_average,
                      school_average.y = school_average)
  ci <- dplyr::rename(ci, national_average.x = national_average,
                      school_average.x = school_average)
  ci <- ci[, c(".colname", "national_average.x", "school_average.x",
               "national_average.y", "school_average.y", "program")]
  column_info(ms_ref) <- ci
  expect_identical(ms, ms_ref)






  ms1 <- remove_row_annotation(filter_row(student_results, class %in% c("classA", "classC")),
                               class, teacher)


  expect_error(join_row_info(ms1, student_results, type = "full"),
               "the number of rows is modified by the join operation, which is against the 'matrixset' paradigm\\. Use 'adjust' to still perform the operation\\.")



  ms2 <- remove_row_annotation(ms1, previous_year_score)
  ms <- join_row_info(ms2, student_results, type = "full", adjust = TRUE)
  m <- student_results[c(1:5, 11:15), , , keep_annotation = FALSE, warn_class_change = FALSE]
  mNA <- matrix(NA_real_, 10, 3)
  rownames(mNA) <- paste("student", c(6:10, 16:20))
  colnames(mNA) <- colnames(student_results)
  m <- lapply(m, function(M) rbind(M, mNA))
  ri <- row_info(student_results)
  ri <- ri[c(1:5, 11:15, 6:10, 16:20), ]
  ci <- column_info(student_results)
  ms_ref <- matrixset(m, row_info = ri, column_info = ci, row_key = ".rowname",
                      column_key = ".colname")
  expect_identical(ms, ms_ref)






expect_warning(join_row_info(ms2, row_info(student_results), type = "full", adjust = "from_y"),
               "'adjust' has been forced to")

ms <- join_row_info(ms2, row_info(student_results), type = "full", adjust = TRUE)
expect_identical(ms, ms_ref)

expect_error(join_row_info(ms2, student_results, type = "full", adjust = "from_x"),
             "'from_x' is not valid")

ms <- join_row_info(ms2, student_results, type = "full", adjust = "pad_x")
m <- student_results[c(1:5, 11:15), , , keep_annotation = FALSE, warn_class_change = FALSE]
mNA <- matrix(NA_real_, 10, 3)
rownames(mNA) <- paste("student", c(6:10, 16:20))
colnames(mNA) <- colnames(student_results)
m <- lapply(m, function(M) rbind(M, mNA))
ri <- row_info(student_results)
ri <- ri[c(1:5, 11:15, 6:10, 16:20), ]
ci <- column_info(student_results)
ms_ref <- matrixset(m, row_info = ri, column_info = ci, row_key = ".rowname",
                    column_key = ".colname")
expect_identical(ms, ms_ref)



  ms2 <- remove_row_annotation(ms1, previous_year_score)
  ms <- join_row_info(ms2, student_results[, -3, ], type = "full", adjust = "from_y")
  m <- student_results[c(1:5, 11:15), , , keep_annotation = FALSE, warn_class_change = FALSE]
  nms <- paste("student", c(6:10, 16:20))
  mats <- student_results[nms, 1:2, , keep_annotation = FALSE, warn_class_change=FALSE]
  mats <- lapply(mats, function(m) {
    m <- cbind(m, NA)
    colnames(m)[3] <- "Science"
    m
  })
  m <- lapply(1:length(m), function(i) rbind(m[[i]], mats[[i]]))
  names(m) <- names(mats)
  ri <- row_info(student_results)
  ri <- ri[c(1:5, 11:15, 6:10, 16:20), ]
  ci <- column_info(student_results)
  ms_ref <- matrixset(m, row_info = ri, column_info = ci, row_key = ".rowname",
                      column_key = ".colname")
  expect_identical(ms, ms_ref)


  expect_error(join_row_info(student_results, ms1, type = "inner"),
               "the number of rows is modified by the join operation, which is against the 'matrixset' paradigm\\. Use 'adjust' to still perform the operation\\.")


  ms2 <- remove_row_annotation(student_results, previous_year_score)
  ms <- join_row_info(ms2, ms1, type = "inner", adjust = TRUE)
  ms_ref <- filter_row(student_results, class %in% c("classA", "classC"))
  expect_identical(ms, ms_ref)



  ms3 <- remove_row_annotation(student_results, class)
  ms <- join_row_info(row_group_by(ms3, teacher), student_results, by = c(".rowname", "previous_year_score", "teacher"))
  ms_ref <- student_results
  ri <- row_info(ms_ref)
  ri <- ri[, c(".rowname", "teacher", "previous_year_score", "class")]
  row_info(ms_ref) <- ri
  ms_ref <- row_group_by(ms_ref, teacher)
  expect_identical(ms, ms_ref)



  ms3 <- remove_row_annotation(student_results, class)
  suppressWarnings(ms <- join_row_info(row_group_by(ms3, teacher), student_results, by = c(".rowname", "previous_year_score")))
  ms_ref <- student_results
  ri <- row_info(ms_ref)
  ri$teacher.x <- ri$teacher
  ri$teacher.y <- ri$teacher
  ri <- ri[, c(".rowname", "teacher.x", "previous_year_score", "class", "teacher.y")]
  row_info(ms_ref) <- ri
  expect_identical(ms, ms_ref)



  ms1 <- filter_row(student_results, class %in% c("classA", "classC"))
  ms <- join_row_info(remove_row_annotation(student_results, class, teacher),
                      ms1, type = "anti", adjust = TRUE)
  ms_ref <- remove_row_annotation(filter_row(student_results, class %in% c("classB", "classD")),
                                  class, teacher)
  expect_identical(ms, ms_ref)


  expect_identical(join_row_info(remove_row_annotation(student_results, class),
                                 ms1, type = "semi", adjust = TRUE),
                   remove_row_annotation(ms1, class))


  expect_identical(join_row_info(ms1, student_results, type = "anti", adjust = TRUE),
                   filter_row(ms1, class == "none"))
})
