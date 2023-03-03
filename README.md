
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matrixset

<!-- badges: start -->

[![R-CMD-check](https://github.com/pascalcroteau/matrixset/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pascalcroteau/matrixset/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A `matrixset` a container of matrices, each having the same number of
rows and columns and the same dimnames. Moreover, each dimname must
uniquely identify elements.

While there is minimal support for `NULL` dimnames (and that is bound to
change at some point in the future), it is strongly recommended to
provide meaningful dimnames. One of the main reason for this is that
annotation is impossible with `NULL` dimnames.

In addition, as alluded above, a `matrixset` can store independent row
and column annotations. This meta information is stored, and available,
in the form of data frames - one for row information and one for column.
The annotation names are referred to as traits.

This latter feature makes `matrixset` especially attractive even if it
stores only a single matrix, because several methods have been
developped to manipulate `matrixset`s, accounting for annotations.

## Why a matrixset?

Many problems that `matrixset` can tackle could be solved via a
`data.frame` and more specifically using the `tidyverse` suite.

Two reasons for which you may want to use a `matrixset` instead are:

- object size. The `data.frame` needed to store the same information as
  a `matrixset` can be significantly bigger
- You actually need a matrix format, for example for running a PCA.

## Installation

You can install the development version of matrixset from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pascalcroteau/matrixset")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(matrixset)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
