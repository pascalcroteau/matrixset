# matrixset 0.4.0

## Changes to `apply_matrix()`, `apply_row()` and `apply_column()`

Note that the changes also apply to the dfl/dfw variants.

### Behavior changes

* The functions are transitioning into taking only functions and formulas and
    no longer expressions. This change was made since the usage of formula and
    expression was almost identical, and the formula is clearer. The 
    documentation has been updated to reflect this and the lifecyle as well.
    
    While this is currently still allowed, a warning will be issued that a
    formula should be used instead.
* The function labels - what is used as column names when simplifying the 
    results - no longer includes the "~" in the name when an expression was
    provided as a formula
* The `NULL` matrices are no longer formatted during the simplification process.
    From now on, `NULL` is always returned for these matrices.
    
    For now, this is a soft deprecation and a lifecycle warning is issued.

### Internal changes

Yet again, the functions have been refactored to improve both on clarity and
performance. The functions are now implemented using R6 classes. A substantial
performance has been noticed, between 2x to 20x faster, depending on the apply
flavor.

## Improved functionality

It is now possible to expand `matrixset` objects following a join operation even
if the resulting tag names are no longer unique. 

This is achieved by the introduction of the `names_glue` argument, which when 
`NULL` (the default), the old behavior of preventing such join is enforced - 
thus keeping back-compatibility. 

Otherwise, the `names_glue` will provide instructions to the join functions on 
how to make the resulting tag names unique.

## Bug correction

* Class adjustment now works for dual grouping.
* Joining now works when the `by` argument is not named for every variable.

## Internal change

New tests implemented for applying functions.

# matrixset 0.3.0

## Improved performance

A complete rewrite of the `apply_` functions have been performed. The initial
goal was to have clearer codes to maintain, but from a user perspective, an 
improvement on speed should be observed as well.

On another positive note, the opportunity of the rewrite was taken to correct
some bugs (see below).

## Improved functionality

* The `mutate_matrix()` function now has access to context functions. This
  implements [#09](https://github.com/pascalcroteau/matrixset/issues/9).
* The `apply_` function family gained a new `.force_name` argument, available
  for the `dfl/dfw` versions. This allows better control in getting meaningful
  IDs to the outcomes.
  
  For instance, using `.force_name` solves the issue
  [#12](https://github.com/pascalcroteau/matrixset/issues/12)
* Functions `join_row_info()` and `join_column_info()` now have more `adjust`
  possibilities to expand matrix sizes when the join operation results in matrix
  expansion. This implements 
  [#10](https://github.com/pascalcroteau/matrixset/issues/10).

## Bug correction

* The function `mutate_matrix()` can now accept statements such as 
  `mutate_matrix(ms, foo=bar)`, which was impossible before.
* `apply_matrix_df*` now works when subgroup yields 1 x m or n x 1 matrices.
  This solves [#13](https://github.com/pascalcroteau/matrixset/issues/13)
* [#12](https://github.com/pascalcroteau/matrixset/issues/12) has been resolved
  by the addition of the `.force_name` argument.
* A miss-parametrization of function `join_column_info()` made it impossible to
  be executed. This has now been fixed.
* `matrixset` objects with 0 rows can now be printed.
* sub-setting `matrixset`s that yields 0-row objects is now possible.

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
