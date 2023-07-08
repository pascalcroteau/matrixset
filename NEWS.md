# matrixset 0.2.0

## Main new Feature

All `matrixset` objects can now store matrices of class `Matrix`, which allows
sparse and other special matrices.

All `matrixset` extractors or other manipulation functions will work as well.

This is a great way to annotate/manipulate special matrices.

## Other New Features

* There is now a check in apply_row_\*/apply_column_\* family functions to
  prevent names of function result to be identical to tags (typically, .rowname
  or .colname). ([#1](https://github.com/pascalcroteau/matrixset/issues/1))
* In the apply_* family functions, there is now a warning when specifying 
  `.input_list` while `.matrix_wise` is `FALSE`. This answers
   [#5](https://github.com/pascalcroteau/matrixset/issues/5)
   
## Bug Correction

* It is now possible to replace row/col/dimnames of `matrixset` objects with
  partially or completely `NULL` dimnames. This fixes
  [#2](https://github.com/pascalcroteau/matrixset/issues/2)
* When printing, the wrong columns were used for printing truncation. This is
  now fixed.
* padding was sometimes off when printing, which is now resolved. This fixes
  [#6](https://github.com/pascalcroteau/matrixset/issues/6)

# matrixset 0.1.1

* Changed some examples that were commented out by wrapping them in tryCatch
* Lines of the description field of the DESCRIPTION file no longer go over 80
  characters. Also removed sets of consecutive space/linefeed.

# matrixset 0.1.0

CRAN submission.

# matrixset 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
