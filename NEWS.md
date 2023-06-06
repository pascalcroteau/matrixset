# matrixset (development version)

* There is now a check in apply_row_\*/apply_column_\* family functions to
  prevent names of function result to be identical to tags (typically, .rowname
  or .colname). ([#1](https://github.com/pascalcroteau/matrixset/issues/1))
* It is now possible to replace row/col/dimnames of `matrixset` objects with
  partially or completely `NULL` dimnames. This fixes
  [#2](https://github.com/pascalcroteau/matrixset/issues/2)

# matrixset 0.1.1

* Changed some examples that were commented out by wrapping them in tryCatch
* Lines of the description field of the DESCRIPTION file no longer go over 80
  characters. Also removed sets of consecutive space/linefeed.

# matrixset 0.1.0

CRAN submission.

# matrixset 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
