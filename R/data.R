
#' Fake Final Exam Results of School Students Before and After Remedial Courses
#'
#' @format A `matrixset` of 20 rows and 3 columns
#' The object contains two matrices, one for the failure results (matrix named
#' `failure`) and one for the results after remedial classes (matrix named
#' `remedial`). Each matrix has results for 20 students and 3 classes:
#' * Mathematics
#' * English
#' * Science
#'
#' The object has been annotated both for rows (students) and columns (courses).
#' Each students has been annotated for the following information:
#' \describe{
#'   \item{class}{Group, or class, in which the student was part of}
#'   \item{teacher}{Professor that gave the remedial course}
#'   \item{previous_year_score}{Score the student had in the previous level of
#'   the same class}
#' }
#'
#' Each course has been annotated for the following information:
#' \describe{
#'   \item{national_average}{National average of all students for the course}
#'   \item{school_average}{Average of the school's students for the course}
#'   \item{program}{Program in which the course is given}
#' }
#'
"student_results"
