
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matrixset

<!-- badges: start -->

[![R-CMD-check](https://github.com/pascalcroteau/matrixset/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pascalcroteau/matrixset/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/matrixset)](https://CRAN.R-project.org/package=matrixset)
<!-- badges: end -->

A `matrixset` is a container of matrices (both `matrix` and types from
`Matrix` package), each having the same number of rows and columns and
the same dimnames. Moreover, each dimname must uniquely identify
elements.

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

Three reasons for which you may want to use a `matrixset` instead are:

- object size. The `data.frame` needed to store the same information as
  a `matrixset` can be significantly bigger
- You can store sparse matrices and other special matrices of package
  `Matrix`. This is one of very few ways (maybe even the only way) to
  annotate special matrices. In addition, dealing with special matrices
  make the object size argument even more striking when comparing to the
  data frame strategy.
- You actually need a matrix format, for example for running a PCA.

## Installation

The easiest way to install `matrixset` is from CRAN:

``` r
install.packages("matrixset")
```

Or you can install the development version of matrixset from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pascalcroteau/matrixset")
```

## Example

In addition to store multiple matrices that share the same attributes, a
`matrixset` object’s strength is it’s annotation feature. You create an
object from existing matrices and annotation `data.frame`.

``` r
library(MASS)
library(tidyverse)
animals <- as.matrix(Animals)
head(animals)
#>                     body brain
#> Mountain beaver     1.35   8.1
#> Cow               465.00 423.0
#> Grey wolf          36.33 119.5
#> Goat               27.66 115.0
#> Guinea pig          1.04   5.5
#> Dipliodocus     11700.00  50.0


animal_info <- MASS::Animals %>% 
  rownames_to_column("Animal") %>% 
  mutate(is_extinct = case_when(Animal %in% c("Dipliodocus", "Triceratops", "Brachiosaurus") ~ TRUE,
                                TRUE ~ FALSE),
         class = case_when(Animal %in% c("Mountain beaver", "Guinea pig", "Golden hamster", "Mouse", "Rabbit", "Rat") ~ "Rodent",
                           Animal %in% c("Potar monkey", "Gorilla", "Human", "Rhesus monkey", "Chimpanzee") ~ "Primate",
                           Animal %in% c("Cow", "Goat", "Giraffe", "Sheep") ~ "Ruminant",
                           Animal %in% c("Asian elephant", "African elephant") ~ "Elephantidae",
                           Animal %in% c("Grey wolf") ~ "Canine",
                           Animal %in% c("Cat", "Jaguar") ~ "Feline",
                           Animal %in% c("Donkey", "Horse") ~ "Equidae",
                           Animal == "Pig" ~ "Sus",
                           Animal == "Mole" ~ "Talpidae",
                           Animal == "Kangaroo" ~ "Macropodidae",
                           TRUE ~ "Dinosaurs")) %>% 
  select(-body, -brain)
animal_info
#>              Animal is_extinct        class
#> 1   Mountain beaver      FALSE       Rodent
#> 2               Cow      FALSE     Ruminant
#> 3         Grey wolf      FALSE       Canine
#> 4              Goat      FALSE     Ruminant
#> 5        Guinea pig      FALSE       Rodent
#> 6       Dipliodocus       TRUE    Dinosaurs
#> 7    Asian elephant      FALSE Elephantidae
#> 8            Donkey      FALSE      Equidae
#> 9             Horse      FALSE      Equidae
#> 10     Potar monkey      FALSE      Primate
#> 11              Cat      FALSE       Feline
#> 12          Giraffe      FALSE     Ruminant
#> 13          Gorilla      FALSE      Primate
#> 14            Human      FALSE      Primate
#> 15 African elephant      FALSE Elephantidae
#> 16      Triceratops       TRUE    Dinosaurs
#> 17    Rhesus monkey      FALSE      Primate
#> 18         Kangaroo      FALSE Macropodidae
#> 19   Golden hamster      FALSE       Rodent
#> 20            Mouse      FALSE       Rodent
#> 21           Rabbit      FALSE       Rodent
#> 22            Sheep      FALSE     Ruminant
#> 23           Jaguar      FALSE       Feline
#> 24       Chimpanzee      FALSE      Primate
#> 25              Rat      FALSE       Rodent
#> 26    Brachiosaurus       TRUE    Dinosaurs
#> 27             Mole      FALSE     Talpidae
#> 28              Pig      FALSE          Sus
```

You can create the object and then do some operations.

``` r
library(matrixset)
animals_ms <- matrixset(msr = animals, row_info = animal_info, row_key = "Animal")
animals_ms %>% 
    apply_row_dfl(rg = ~ range(.i),
                  qt = ~ quantile(.i, probs = c(.25, .75)))   
#> $msr
#> # A tibble: 56 × 5
#>    .rowname        rg.name     rg qt.name     qt
#>    <chr>           <chr>    <dbl> <chr>    <dbl>
#>  1 Mountain beaver ..1       1.35 25%       3.04
#>  2 Mountain beaver ..2       8.1  75%       6.41
#>  3 Cow             ..1     423    25%     434.  
#>  4 Cow             ..2     465    75%     454.  
#>  5 Grey wolf       ..1      36.3  25%      57.1 
#>  6 Grey wolf       ..2     120.   75%      98.7 
#>  7 Goat            ..1      27.7  25%      49.5 
#>  8 Goat            ..2     115    75%      93.2 
#>  9 Guinea pig      ..1       1.04 25%       2.16
#> 10 Guinea pig      ..2       5.5  75%       4.38
#> # ℹ 46 more rows


animals_ms %>% 
    row_group_by(class) %>% 
    apply_column_dfl(avr = mean)
#> $msr
#> # A tibble: 22 × 3
#>    class        .colname     avr
#>    <chr>        <chr>      <dbl>
#>  1 Canine       body        36.3
#>  2 Canine       brain      120. 
#>  3 Dinosaurs    body     36033. 
#>  4 Dinosaurs    brain       91.5
#>  5 Elephantidae body      4600. 
#>  6 Elephantidae brain     5158. 
#>  7 Equidae      body       354. 
#>  8 Equidae      brain      537  
#>  9 Feline       body        51.6
#> 10 Feline       brain       91.3
#> # ℹ 12 more rows
```
