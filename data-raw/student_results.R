set.seed(1121)
failure <- matrix(runif(20*3,0,.5), 20, 3)
remedial <- matrix(c(c(runif(10, 0.55, 0.75), runif(10, 0.65, 0.8)),
                     runif(20, 0.65, 0.90),
                     c(runif(10, 0.6, 0.8), runif(10, 0.65, 0.95))), 20, 3)
rownames(failure) <- rownames(remedial) <- paste("student", 1:20)
colnames(failure) <- colnames(remedial) <- c("Mathematics", "English", "Science")
student_info <- data.frame(student = paste("student", 1:20),
                           class = gl(4,5,labels = paste0("class", LETTERS[1:4])),
                           teacher = gl(2,10,labels = paste0("Professor", 1:2)),
                           previous_year_score = runif(20, 0.5, 0.9))
course_info <- data.frame(course = c("Mathematics", "English", "Science"),
                          national_average = runif(3, c(0.5, 0.7, 0.6), c(0.85, 0.99, 0.95)),
                          school_average = runif(3, c(0.6, 0.75, 0.65), c(0.8, 0.99, 0.9)),
                          program = c("Applied Science", "Literature", "Applied Science"))
student_results <- matrixset(failure = failure, remedial = remedial,
                             row_info = student_info, row_key = "student",
                             col_info = course_info, col_key = "course")
