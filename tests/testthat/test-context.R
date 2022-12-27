test_that("context functions work", {

  rowinf <- apply_row(student_results, ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(setNames(seq(nrow(rowmeta)), rowmeta$.rowname),
                function(i) list(`~current_row_info()` = rowmeta[i, ]))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_row(row_group_by(student_results, teacher, class), ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(setNames(seq(nrow(rowmeta)), rowmeta$.rowname),
                function(i) list(`~current_row_info()` = rowmeta[i, ]))
  expect_identical(rowinf$failure, rml, ignore_attr = TRUE)
  expect_identical(rowinf$remedial, rml, ignore_attr = TRUE)


  rowinf <- apply_row(column_group_by(student_results,program), ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(setNames(seq(nrow(rowmeta)), rowmeta$.rowname),
                function(i) list(`~current_row_info()` = rowmeta[i, ]))
  expect_identical(rowinf$failure$.vals[[1]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[2]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[1]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[2]], rml, ignore_attr = TRUE)



  rowinf <- apply_row(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(setNames(seq(nrow(rowmeta)), rowmeta$.rowname),
                function(i) list(`~current_row_info()` = rowmeta[i, ]))
  expect_identical(rowinf$failure$.vals[[1]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[2]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[1]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[2]], rml, ignore_attr = TRUE)







  colinf <- apply_row(student_results, ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
                function(i) list(`~current_column_info()` = colmeta))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_row(row_group_by(student_results, teacher, class), ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
                function(i) list(`~current_column_info()` = colmeta))
  expect_identical(colinf$failure, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial, cml, ignore_attr = TRUE)


  colinf <- apply_row(column_group_by(student_results,program), ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <-  lapply(list(c(1,3), 2), function(j) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
           function(i) list(`~current_column_info()` = colmeta[j, ]))
  })
  expect_identical(colinf$failure$.vals[[1]], cml[[1]], ignore_attr = TRUE)
  expect_identical(colinf$failure$.vals[[2]], cml[[2]], ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals[[1]], cml[[1]], ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals[[2]], cml[[2]], ignore_attr = TRUE)


  colinf <- apply_row(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <-  lapply(list(c(1,3), 2), function(j) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
           function(i) list(`~current_column_info()` = colmeta[j, ]))
  })
  expect_identical(colinf$failure$.vals[[1]], cml[[1]], ignore_attr = TRUE)
  expect_identical(colinf$failure$.vals[[2]], cml[[2]], ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals[[1]], cml[[1]], ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals[[2]], cml[[2]], ignore_attr = TRUE)






  rowinf <- apply_row(student_results, ~ current_n_row())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~current_n_row()` = 1L))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_row(row_group_by(student_results, teacher, class), ~ current_n_row())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~current_n_row()` = 1L))
  expect_identical(rowinf$failure, rml, ignore_attr = TRUE)
  expect_identical(rowinf$remedial, rml, ignore_attr = TRUE)


  rowinf <- apply_row(column_group_by(student_results,program), ~ current_n_row())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~current_n_row()` = 1L))
  expect_identical(rowinf$failure$.vals[[1]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[2]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[1]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[2]], rml, ignore_attr = TRUE)


  rowinf <- apply_row(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ current_n_row())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~current_n_row()` = 1L))
  expect_identical(rowinf$failure$.vals[[1]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[2]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[1]], rml, ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[2]], rml, ignore_attr = TRUE)





  colinf <- apply_row(student_results, ~ current_n_column())
  cml <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
                function(i) list(`~current_n_column()` = 3L))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_row(row_group_by(student_results, teacher, class), ~ current_n_column())
  cml <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
                function(i) list(`~current_n_column()` = 3L))
  expect_identical(colinf$failure, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial, cml, ignore_attr = TRUE)


  colinf <- apply_row(column_group_by(student_results,program), ~ current_n_column())
  cml <-  lapply(list(c(1,3), 2), function(j) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
           function(i) list(`~current_n_column()` = length(j)))
  })
  expect_identical(colinf$failure$.vals[[1]], cml[[1]], ignore_attr = TRUE)
  expect_identical(colinf$failure$.vals[[2]], cml[[2]], ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals[[1]], cml[[1]], ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals[[2]], cml[[2]], ignore_attr = TRUE)


  colinf <- apply_row(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ current_n_column())
  cml <-  lapply(list(c(1,3), 2), function(j) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
           function(i) list(`~current_n_column()` = length(j)))
  })
  expect_identical(colinf$failure$.vals[[1]], cml[[1]], ignore_attr = TRUE)
  expect_identical(colinf$failure$.vals[[2]], cml[[2]], ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals[[1]], cml[[1]], ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals[[2]], cml[[2]], ignore_attr = TRUE)





  rowinf <- apply_row(student_results, ~ row_pos())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~row_pos()` = i))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_row(row_group_by(student_results, teacher, class), ~ row_pos())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~row_pos()` = i))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_row(column_group_by(student_results,program), ~ row_pos())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~row_pos()` = i))
  expect_identical(rowinf$failure$.vals[[1]], rml)
  expect_identical(rowinf$failure$.vals[[2]], rml)
  expect_identical(rowinf$remedial$.vals[[1]], rml)
  expect_identical(rowinf$remedial$.vals[[2]], rml)


  rowinf <- apply_row(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ row_pos())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~row_pos()` = i))
  expect_identical(rowinf$failure$.vals[[1]], rml)
  expect_identical(rowinf$failure$.vals[[2]], rml)
  expect_identical(rowinf$remedial$.vals[[1]], rml)
  expect_identical(rowinf$remedial$.vals[[2]], rml)





  colinf <- apply_row(student_results, ~ column_pos())
  cml <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
                function(i) list(`~column_pos()` = 1L:3L))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_row(row_group_by(student_results, teacher, class), ~ column_pos())
  cml <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
                function(i) list(`~column_pos()` = 1L:3L))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_row(column_group_by(student_results,program), ~ column_pos())
  cml <-  lapply(list(c(1L,3L), 2L), function(j) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
           function(i) list(`~column_pos()` = j))
  })
  expect_identical(colinf$failure$.vals[[1]], cml[[1]])
  expect_identical(colinf$failure$.vals[[2]], cml[[2]])
  expect_identical(colinf$remedial$.vals[[1]], cml[[1]])
  expect_identical(colinf$remedial$.vals[[2]], cml[[2]])


  colinf <- apply_row(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ column_pos())
  cml <-  lapply(list(c(1L,3L), 2L), function(j) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
           function(i) list(`~column_pos()` = j))
  })
  expect_identical(colinf$failure$.vals[[1]], cml[[1]])
  expect_identical(colinf$failure$.vals[[2]], cml[[2]])
  expect_identical(colinf$remedial$.vals[[1]], cml[[1]])
  expect_identical(colinf$remedial$.vals[[2]], cml[[2]])





  rowinf <- apply_row(student_results, ~ row_rel_pos())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~row_rel_pos()` = 1L))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_row(row_group_by(student_results, teacher, class), ~ row_rel_pos())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~row_rel_pos()` = 1L))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_row(column_group_by(student_results,program), ~ row_rel_pos())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~row_rel_pos()` = 1L))
  expect_identical(rowinf$failure$.vals[[1]], rml)
  expect_identical(rowinf$failure$.vals[[2]], rml)
  expect_identical(rowinf$remedial$.vals[[1]], rml)
  expect_identical(rowinf$remedial$.vals[[2]], rml)


  rowinf <- apply_row(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ row_rel_pos())
  rml <- lapply(setNames(seq(nrow(rowmeta)), rownames(student_results)),
                function(i) list(`~row_rel_pos()` = 1L))
  expect_identical(rowinf$failure$.vals[[1]], rml)
  expect_identical(rowinf$failure$.vals[[2]], rml)
  expect_identical(rowinf$remedial$.vals[[1]], rml)
  expect_identical(rowinf$remedial$.vals[[2]], rml)





  colinf <- apply_row(student_results, ~ column_rel_pos())
  cml <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
                function(i) list(`~column_rel_pos()` = 1L:3L))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_row(row_group_by(student_results, teacher, class), ~ column_rel_pos())
  cml <- lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
                function(i) list(`~column_rel_pos()` = 1L:3L))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_row(column_group_by(student_results,program), ~ column_rel_pos())
  cml <-  lapply(list(c(1L,3L), 2L), function(j) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
           function(i) list(`~column_rel_pos()` = seq_along((j))))
  })
  expect_identical(colinf$failure$.vals[[1]], cml[[1]])
  expect_identical(colinf$failure$.vals[[2]], cml[[2]])
  expect_identical(colinf$remedial$.vals[[1]], cml[[1]])
  expect_identical(colinf$remedial$.vals[[2]], cml[[2]])


  colinf <- apply_row(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ column_rel_pos())
  cml <-  lapply(list(c(1L,3L), 2L), function(j) {
    lapply(setNames(seq(nrow(student_results)), rownames(student_results)),
           function(i) list(`~column_rel_pos()` = seq_along(j)))
  })
  expect_identical(colinf$failure$.vals[[1]], cml[[1]])
  expect_identical(colinf$failure$.vals[[2]], cml[[2]])
  expect_identical(colinf$remedial$.vals[[1]], cml[[1]])
  expect_identical(colinf$remedial$.vals[[2]], cml[[2]])














  rowinf <- apply_column(student_results, ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~current_row_info()` = rowmeta))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_column(row_group_by(student_results, teacher, class), ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(list(seq.int(5),
                     seq.int(6,10),
                     seq.int(11,15),
                     seq.int(16,20)),
                function(i) {
                  lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                         function(j) list(`~current_row_info()` = rowmeta[i, ]))
                })
  expect_identical(rowinf$failure$.vals[[1]], rml[[1]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[2]], rml[[2]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[3]], rml[[3]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[4]], rml[[4]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[1]], rml[[1]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[2]], rml[[2]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[3]], rml[[3]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[4]], rml[[4]], ignore_attr = TRUE)


  rowinf <- apply_column(column_group_by(student_results,program), ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~current_row_info()` = rowmeta))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_column(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(list(seq.int(5),
                     seq.int(6,10),
                     seq.int(11,15),
                     seq.int(16,20)),
                function(i) {
                  lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                         function(j) list(`~current_row_info()` = rowmeta[i, ]))
                })
  expect_identical(rowinf$failure$.vals[[1]], rml[[1]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[2]], rml[[2]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[3]], rml[[3]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[4]], rml[[4]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[1]], rml[[1]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[2]], rml[[2]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[3]], rml[[3]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[4]], rml[[4]], ignore_attr = TRUE)







  colinf <- apply_column(student_results, ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~current_column_info()` = colmeta[i, ]))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_column(row_group_by(student_results, teacher, class), ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <- lapply(list(seq.int(5),
                     seq.int(6,10),
                     seq.int(11,15),
                     seq.int(16,20)),
                function(i) {
                  lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                         function(j) list(`~current_column_info()` = colmeta[j, ]))
                }
  )
  expect_identical(colinf$failure$.vals, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals, cml, ignore_attr = TRUE)


  colinf <- apply_column(column_group_by(student_results,program), ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~current_column_info()` = colmeta[i, ]))
  expect_identical(colinf$failure, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial, cml, ignore_attr = TRUE)


  colinf <- apply_column(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <- lapply(list(seq.int(5),
                     seq.int(6,10),
                     seq.int(11,15),
                     seq.int(16,20)),
                function(i) {
                  lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                         function(j) list(`~current_column_info()` = colmeta[j, ]))
                }
  )
  expect_identical(colinf$failure$.vals, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals, cml, ignore_attr = TRUE)






  rowinf <- apply_column(student_results, ~ current_n_row())
  rml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~current_n_row()` = 20L))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_column(row_group_by(student_results, teacher, class), ~ current_n_row())
  rml <- lapply(1:4,
                function(i) {
                  lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                         function(j) list(`~current_n_row()` = 5L))
                }
  )
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)


  rowinf <- apply_column(column_group_by(student_results,program), ~ current_n_row())
  rml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~current_n_row()` = 20L))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_column(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ current_n_row())
  rml <- lapply(1:4,
                function(i) {
                  lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                         function(j) list(`~current_n_row()` = 5L))
                }
  )
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)






  colinf <- apply_column(student_results, ~ current_n_column())
  cml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~current_n_column()` = 1L))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_column(row_group_by(student_results, teacher, class), ~ current_n_column())
  cml <- lapply(1:4, function(i) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
           function(j) list(`~current_n_column()` = 1L))
  })
  expect_identical(colinf$failure$.vals, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals, cml, ignore_attr = TRUE)


  colinf <- apply_column(column_group_by(student_results,program), ~ current_n_column())
  cml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~current_n_column()` = 1L))
  expect_identical(colinf$failure, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial, cml, ignore_attr = TRUE)


  colinf <- apply_column(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ current_n_column())
  cml <- lapply(1:4, function(i) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
           function(j) list(`~current_n_column()` = 1L))
  })
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)





  rowinf <- apply_column(student_results, ~ row_pos())
  rml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~row_pos()` = seq.int(20)))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_column(row_group_by(student_results, teacher, class), ~ row_pos())
  rml <- lapply(list(seq.int(5),
                     seq.int(6,10),
                     seq.int(11,15),
                     seq.int(16,20)), function(i) {
                       lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                              function(j) list(`~row_pos()` = i))
                     })
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)


  rowinf <- apply_column(column_group_by(student_results,program), ~ row_pos())
  rml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~row_pos()` = seq.int(20)))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_column(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ row_pos())
  rml <- lapply(list(seq.int(5),
                     seq.int(6,10),
                     seq.int(11,15),
                     seq.int(16,20)), function(i) {
                       lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                              function(j) list(`~row_pos()` = i))
                     })
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)






  colinf <- apply_column(student_results, ~ column_pos())
  cml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~column_pos()` = i))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_column(row_group_by(student_results, teacher, class), ~ column_pos())
  cml <- lapply(1:4, function(i) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
           function(j) list(`~column_pos()` = j))
  })
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)


  colinf <- apply_column(column_group_by(student_results,program), ~ column_pos())
  cml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~column_pos()` = i))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_column(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ column_pos())
  cml <- lapply(1:4, function(i) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
           function(j) list(`~column_pos()` = j))
  })
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)






  rowinf <- apply_column(student_results, ~ row_rel_pos())
  rml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~row_rel_pos()` = seq.int(20)))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_column(row_group_by(student_results, teacher, class), ~ row_rel_pos())
  rml <- lapply(1:4, function(i) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
           function(j) list(`~row_rel_pos()` = seq.int(5)))
  })
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)


  rowinf <- apply_column(column_group_by(student_results,program), ~ row_rel_pos())
  rml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~row_rel_pos()` = seq.int(20)))
  expect_identical(rowinf$failure, rml)
  expect_identical(rowinf$remedial, rml)


  rowinf <- apply_column(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ row_rel_pos())
  rml <- lapply(1:4, function(i) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
           function(j) list(`~row_rel_pos()` = seq.int(5)))
  })
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)






  colinf <- apply_column(student_results, ~ column_rel_pos())
  cml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~column_rel_pos()` = 1L))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_column(row_group_by(student_results, teacher, class), ~ column_rel_pos())
  cml <- lapply(1:4, function(i) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
           function(j) list(`~column_rel_pos()` = 1L))
  })
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)


  colinf <- apply_column(column_group_by(student_results,program), ~ column_rel_pos())
  cml <- lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
                function(i) list(`~column_rel_pos()` = 1L))
  expect_identical(colinf$failure, cml)
  expect_identical(colinf$remedial, cml)


  colinf <- apply_column(column_group_by(row_group_by(student_results, teacher, class), program),
                      ~ column_rel_pos())
  cml <- lapply(1:4, function(i) {
    lapply(setNames(seq(ncol(student_results)), colnames(student_results)),
           function(j) list(`~column_rel_pos()` = 1L))
  })
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)

















  rowinf <- apply_matrix(student_results, ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(setNames(seq(nmatrix(student_results)), matrixnames(student_results)),
                function(i) list(`~current_row_info()` = rowmeta))
  expect_identical(rowinf, rml)


  rowinf <- apply_matrix(row_group_by(student_results, teacher, class), ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(list(seq.int(5),
                     seq.int(6,10),
                     seq.int(11,15),
                     seq.int(16,20)),
                function(i) {
                  list(`~current_row_info()` = rowmeta[i, ])
                })
  expect_identical(rowinf$failure$.vals[[1]], rml[[1]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[2]], rml[[2]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[3]], rml[[3]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[4]], rml[[4]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[1]], rml[[1]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[2]], rml[[2]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[3]], rml[[3]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[4]], rml[[4]], ignore_attr = TRUE)


  rowinf <- apply_matrix(column_group_by(student_results,program), ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(1:2,
                function(i) {
                  list(`~current_row_info()` = rowmeta)
                })
  expect_identical(rowinf$failure$.vals[[1]], rml[[1]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[2]], rml[[2]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[1]], rml[[1]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[2]], rml[[2]], ignore_attr = TRUE)


  rowinf <- apply_matrix(column_group_by(row_group_by(student_results, teacher, class), program),
                         ~ current_row_info())
  rowmeta <- row_info(student_results)
  rml <- lapply(list(seq.int(5),
                     seq.int(5),
                     seq.int(6,10),
                     seq.int(6,10),
                     seq.int(11,15),
                     seq.int(11,15),
                     seq.int(16,20),
                     seq.int(16,20)),
                function(i) {
                  list(`~current_row_info()` = rowmeta[i, ])
                })
  expect_identical(rowinf$failure$.vals[[1]], rml[[1]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[2]], rml[[2]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[3]], rml[[3]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[4]], rml[[4]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[5]], rml[[5]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[6]], rml[[6]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[7]], rml[[7]], ignore_attr = TRUE)
  expect_identical(rowinf$failure$.vals[[8]], rml[[8]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[1]], rml[[1]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[2]], rml[[2]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[3]], rml[[3]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[4]], rml[[4]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[5]], rml[[5]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[6]], rml[[6]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[7]], rml[[7]], ignore_attr = TRUE)
  expect_identical(rowinf$remedial$.vals[[8]], rml[[8]], ignore_attr = TRUE)







  colinf <- apply_matrix(student_results, ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <- lapply(setNames(seq(nmatrix(student_results)), matrixnames(student_results)),
                function(i) list(`~current_column_info()` = colmeta))
  expect_identical(colinf, cml)


  colinf <- apply_matrix(row_group_by(student_results, teacher, class), ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <- lapply(list(seq.int(5),
                     seq.int(6,10),
                     seq.int(11,15),
                     seq.int(16,20)),
                function(i) {
                  list(`~current_column_info()` = colmeta)
                }
  )
  expect_identical(colinf$failure$.vals, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals, cml, ignore_attr = TRUE)


  colinf <- apply_matrix(column_group_by(student_results,program), ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <- lapply(list(c(1, 3), 2),
                function(i) list(`~current_column_info()` = colmeta[i, ]))
  expect_identical(colinf$failure$.vals, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals, cml, ignore_attr = TRUE)


  colinf <- apply_matrix(column_group_by(row_group_by(student_results, teacher, class), program),
                         ~ current_column_info())
  colmeta <- column_info(student_results)
  cml <- lapply(rep(list(c(1,3),2), 4),
                function(i) {
                  list(`~current_column_info()` = colmeta[i, ])
                }
  )
  expect_identical(colinf$failure$.vals, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals, cml, ignore_attr = TRUE)






  rowinf <- apply_matrix(student_results, ~ current_n_row())
  rml <- lapply(setNames(seq(nmatrix(student_results)), matrixnames(student_results)),
                function(i) list(`~current_n_row()` = 20L))
  expect_identical(rowinf, rml)


  rowinf <- apply_matrix(row_group_by(student_results, teacher, class), ~ current_n_row())
  rml <- lapply(1:4,
                function(i) {
                  list(`~current_n_row()` = 5L)
                }
  )
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)


  rowinf <- apply_matrix(column_group_by(student_results,program), ~ current_n_row())
  rml <- lapply(1:2,
                function(i) list(`~current_n_row()` = 20L))
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)


  rowinf <- apply_matrix(column_group_by(row_group_by(student_results, teacher, class), program),
                         ~ current_n_row())
  rml <- lapply(1:8,
                function(i) {
                  list(`~current_n_row()` = 5L)
                }
  )
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)






  colinf <- apply_matrix(student_results, ~ current_n_column())
  cml <- lapply(setNames(seq(nmatrix(student_results)), matrixnames(student_results)),
                function(i) list(`~current_n_column()` = 3L))
  expect_identical(colinf, cml)


  colinf <- apply_matrix(row_group_by(student_results, teacher, class), ~ current_n_column())
  cml <- lapply(1:4, function(i) {
    list(`~current_n_column()` = 3L)
  })
  expect_identical(colinf$failure$.vals, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals, cml, ignore_attr = TRUE)


  colinf <- apply_matrix(column_group_by(student_results,program), ~ current_n_column())
  cml <- lapply(list(c(1,3), 2),
                function(i) list(`~current_n_column()` = length(i)))
  expect_identical(colinf$failure$.vals, cml, ignore_attr = TRUE)
  expect_identical(colinf$remedial$.vals, cml, ignore_attr = TRUE)


  colinf <- apply_matrix(column_group_by(row_group_by(student_results, teacher, class), program),
                         ~ current_n_column())
  cml <- lapply(rep(list(c(1,3),2), 4), function(i) {
    list(`~current_n_column()` = length(i))
  })
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)





  rowinf <- apply_matrix(student_results, ~ row_pos())
  rml <- lapply(setNames(seq(nmatrix(student_results)), matrixnames(student_results)),
                function(i) list(`~row_pos()` = seq.int(20)))
  expect_identical(rowinf, rml)


  rowinf <- apply_matrix(row_group_by(student_results, teacher, class), ~ row_pos())
  rml <- lapply(list(seq.int(5),
                     seq.int(6,10),
                     seq.int(11,15),
                     seq.int(16,20)), function(i) {
                       list(`~row_pos()` = i)
                     })
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)


  rowinf <- apply_matrix(column_group_by(student_results,program), ~ row_pos())
  rml <- lapply(1:2,
                function(i) list(`~row_pos()` = seq.int(20)))
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)


  rowinf <- apply_matrix(column_group_by(row_group_by(student_results, teacher, class), program),
                         ~ row_pos())
  rml <- lapply(list(seq.int(5),
                     seq.int(5),
                     seq.int(6,10),
                     seq.int(6,10),
                     seq.int(11,15),
                     seq.int(11,15),
                     seq.int(16,20),
                     seq.int(16,20)), function(i) {
                       list(`~row_pos()` = i)
                     })
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)






  colinf <- apply_matrix(student_results, ~ column_pos())
  cml <- lapply(setNames(seq(nmatrix(student_results)), matrixnames(student_results)),
                function(i) list(`~column_pos()` = seq.int(3)))
  expect_identical(colinf, cml)


  colinf <- apply_matrix(row_group_by(student_results, teacher, class), ~ column_pos())
  cml <- lapply(1:4, function(i) {
    list(`~column_pos()` = seq.int(3))
  })
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)


  colinf <- apply_matrix(column_group_by(student_results,program), ~ column_pos())
  cml <- lapply(list(c(1L, 3L), 2L),
                function(i) list(`~column_pos()` = i))
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)


  colinf <- apply_matrix(column_group_by(row_group_by(student_results, teacher, class), program),
                         ~ column_pos())
  cml <- lapply(rep(list(c(1L, 3L), 2L), 4), function(i) {
    list(`~column_pos()` = i)
  })
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)






  rowinf <- apply_matrix(student_results, ~ row_rel_pos())
  rml <- lapply(setNames(seq(nmatrix(student_results)), matrixnames(student_results)),
                function(i) list(`~row_rel_pos()` = seq.int(20)))
  expect_identical(rowinf, rml)


  rowinf <- apply_matrix(row_group_by(student_results, teacher, class), ~ row_rel_pos())
  rml <- lapply(1:4, function(i) {
    list(`~row_rel_pos()` = seq.int(5))
  })
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)


  rowinf <- apply_matrix(column_group_by(student_results,program), ~ row_rel_pos())
  rml <- lapply(1:2,
                function(i) list(`~row_rel_pos()` = seq.int(20)))
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)


  rowinf <- apply_matrix(column_group_by(row_group_by(student_results, teacher, class), program),
                         ~ row_rel_pos())
  rml <- lapply(1:8, function(i) {
    list(`~row_rel_pos()` = seq.int(5))
  })
  expect_identical(rowinf$failure$.vals, rml)
  expect_identical(rowinf$remedial$.vals, rml)






  colinf <- apply_matrix(student_results, ~ column_rel_pos())
  cml <- lapply(setNames(seq(nmatrix(student_results)), matrixnames(student_results)),
                function(i) list(`~column_rel_pos()` = 1L:3L))
  expect_identical(colinf, cml)


  colinf <- apply_matrix(row_group_by(student_results, teacher, class), ~ column_rel_pos())
  cml <- lapply(1:4, function(i) {
    list(`~column_rel_pos()` = 1L:3L)
  })
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)


  colinf <- apply_matrix(column_group_by(student_results,program), ~ column_rel_pos())
  cml <- lapply(list(1L:2L, 1L),
                function(i) list(`~column_rel_pos()` = i))
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)


  colinf <- apply_matrix(column_group_by(row_group_by(student_results, teacher, class), program),
                         ~ column_rel_pos())
  cml <- lapply(rep(list(1L:2L, 1L), 4), function(i) {
    list(`~column_rel_pos()` = i)
  })
  expect_identical(colinf$failure$.vals, cml)
  expect_identical(colinf$remedial$.vals, cml)


})
