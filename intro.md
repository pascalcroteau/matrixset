A Gentle Introduction to matrixset
================

## What is a matrixset?

It’s a container of matrices, each having the same number of rows and
columns and the same dimnames. Moreover, each dimname must uniquely
identify elements.

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

## How to create a matrixset?

To illustrate how to build a `matrixset`, we will use the animal dataset
from the MASS package.

``` r
animals <- as.matrix(MASS::Animals)
head(animals)
#>                     body brain
#> Mountain beaver     1.35   8.1
#> Cow               465.00 423.0
#> Grey wolf          36.33 119.5
#> Goat               27.66 115.0
#> Guinea pig          1.04   5.5
#> Dipliodocus     11700.00  50.0
```

This is a matrix with 28 rows and 2 columns.

There is two ways to create a `matrixset` object: `matrixset()` or
`as_matrixset()`.

Let’s first take a look at `matrixset()`.

``` r
animals_ms <- matrixset(animals)
#> Error in list_names(matrix_set): The list elements must be named
```

Oops! Looks like something didn’t work. That’s because matrices must be
named when building a `matrixset`.

``` r
animals_ms <- matrixset(msr = animals)
animals_ms
#> matrixset of 1 28 × 2 matrices
#> 
#> matrix_set: msr 
#> A 28 × 2 <dbl> matrix
#>                     body    brain
#> Mountain beaver     1.35     8.10
#>             ...      ...      ...
#>             Pig   192.00   180.00
#> 
#> 
#> row_info:
#> # A tibble: 28 × 1
#>    .rowname       
#>    <chr>          
#>  1 Mountain beaver
#>  2 Cow            
#>  3 Grey wolf      
#>  4 Goat           
#>  5 Guinea pig     
#>  6 Dipliodocus    
#>  7 Asian elephant 
#>  8 Donkey         
#>  9 Horse          
#> 10 Potar monkey   
#> # … with 18 more rows
#> 
#> 
#> column_info:
#> # A tibble: 2 × 1
#>   .colname
#>   <chr>   
#> 1 body    
#> 2 brain
```

The `as_matrixset()` function will provide a generic name.

``` r
as_matrixset(animals)
#> matrixset of 1 28 × 2 matrices
#> 
#> matrix_set: ..1 
#> A 28 × 2 <dbl> matrix
#>                     body    brain
#> Mountain beaver     1.35     8.10
#>             ...      ...      ...
#>             Pig   192.00   180.00
#> 
#> 
#> row_info:
#> # A tibble: 28 × 1
#>    .rowname       
#>    <chr>          
#>  1 Mountain beaver
#>  2 Cow            
#>  3 Grey wolf      
#>  4 Goat           
#>  5 Guinea pig     
#>  6 Dipliodocus    
#>  7 Asian elephant 
#>  8 Donkey         
#>  9 Horse          
#> 10 Potar monkey   
#> # … with 18 more rows
#> 
#> 
#> column_info:
#> # A tibble: 2 × 1
#>   .colname
#>   <chr>   
#> 1 body    
#> 2 brain
```

It is recommended to use the `matrixset()` function and provide a
meaningful matrix name, as some functionalities will make use of matrix
names. It will also be easier to refer to the matrices when there is
more than one.

We saw in the `animals_ms` printout that default row annotation
(`row_info`) and column annotation (`column_info`) were created at the
time of object building.

Meta info always contains the row names and column names. We will see
later how to add more annotation.

Let’s see an example on how to include more than one matrix.

``` r
log_animals <- log(animals)
ms <- matrixset(msr = animals, log_msr = log_animals)
ms
#> matrixset of 2 28 × 2 matrices
#> 
#> matrix_set: msr 
#> A 28 × 2 <dbl> matrix
#>                     body    brain
#> Mountain beaver     1.35     8.10
#>             ...      ...      ...
#>             Pig   192.00   180.00
#> matrix_set: log_msr 
#> A 28 × 2 <dbl> matrix
#>                  body brain
#> Mountain beaver  0.30  2.09
#>             ...   ...   ...
#>             Pig  5.26  5.19
#> 
#> 
#> row_info:
#> # A tibble: 28 × 1
#>    .rowname       
#>    <chr>          
#>  1 Mountain beaver
#>  2 Cow            
#>  3 Grey wolf      
#>  4 Goat           
#>  5 Guinea pig     
#>  6 Dipliodocus    
#>  7 Asian elephant 
#>  8 Donkey         
#>  9 Horse          
#> 10 Potar monkey   
#> # … with 18 more rows
#> 
#> 
#> column_info:
#> # A tibble: 2 × 1
#>   .colname
#>   <chr>   
#> 1 body    
#> 2 brain
```

It may happen that the matrices are stored in a list. No problem, the
list can be used as input for `matrixset`.

``` r
ms2 <- matrixset(list(msr = animals, log_msr = log_animals))
identical(ms, ms2)
#> [1] TRUE
```

It is also possible to add a matrix to an already existing object.

``` r
animals_ms <- add_matrix(animals_ms, log_msr = log_animals)
identical(ms, animals_ms)
#> [1] TRUE
```

## How to annotate

One way is to do it at the object creation step. Suppose we have the
following animal information (which are rows in our object).

``` r
library(tidyverse)
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

We can then use the `matrixset()` function (it works with
`as_matrixset()` too).

``` r
ms <- matrixset(msr = animals, log_msr = log_animals, row_info = animal_info,
                row_key = "Animal")
ms
#> matrixset of 2 28 × 2 matrices
#> 
#> matrix_set: msr 
#> A 28 × 2 <dbl> matrix
#>                     body    brain
#> Mountain beaver     1.35     8.10
#>             ...      ...      ...
#>             Pig   192.00   180.00
#> matrix_set: log_msr 
#> A 28 × 2 <dbl> matrix
#>                  body brain
#> Mountain beaver  0.30  2.09
#>             ...   ...   ...
#>             Pig  5.26  5.19
#> 
#> 
#> row_info:
#> # A tibble: 28 × 3
#>    .rowname        is_extinct class       
#>    <chr>           <lgl>      <chr>       
#>  1 Mountain beaver FALSE      Rodent      
#>  2 Cow             FALSE      Ruminant    
#>  3 Grey wolf       FALSE      Canine      
#>  4 Goat            FALSE      Ruminant    
#>  5 Guinea pig      FALSE      Rodent      
#>  6 Dipliodocus     TRUE       Dinosaurs   
#>  7 Asian elephant  FALSE      Elephantidae
#>  8 Donkey          FALSE      Equidae     
#>  9 Horse           FALSE      Equidae     
#> 10 Potar monkey    FALSE      Primate     
#> # … with 18 more rows
#> 
#> 
#> column_info:
#> # A tibble: 2 × 1
#>   .colname
#>   <chr>   
#> 1 body    
#> 2 brain
```

Notice how we used the `row_key` argument to specify how to link the two
objects together.

Another option is the `row_info<-()` function. Note that here you need a
column with the same tag as in the `matrixset` object.

``` r
row_info(ms2) <- animal_info %>% rename(.rowname = Animal)
identical(ms, ms2)
#> [1] TRUE
```

We’ll finish the annotation section by introducing the `join` and the
`annotate` methods.

``` r
animals_ms <- animals_ms %>% 
  join_row_info(animal_info, by = c(".rowname" = "Animal"))
identical(ms, animals_ms)
#> [1] TRUE
animals_ms <- animals_ms %>% 
  annotate_column(unit = case_when(.colname == "body" ~ "kg",
                                   TRUE ~ "g")) 
animals_ms
#> matrixset of 2 28 × 2 matrices
#> 
#> matrix_set: msr 
#> A 28 × 2 <dbl> matrix
#>                     body    brain
#> Mountain beaver     1.35     8.10
#>             ...      ...      ...
#>             Pig   192.00   180.00
#> matrix_set: log_msr 
#> A 28 × 2 <dbl> matrix
#>                  body brain
#> Mountain beaver  0.30  2.09
#>             ...   ...   ...
#>             Pig  5.26  5.19
#> 
#> 
#> row_info:
#> # A tibble: 28 × 3
#>    .rowname        is_extinct class       
#>    <chr>           <lgl>      <chr>       
#>  1 Mountain beaver FALSE      Rodent      
#>  2 Cow             FALSE      Ruminant    
#>  3 Grey wolf       FALSE      Canine      
#>  4 Goat            FALSE      Ruminant    
#>  5 Guinea pig      FALSE      Rodent      
#>  6 Dipliodocus     TRUE       Dinosaurs   
#>  7 Asian elephant  FALSE      Elephantidae
#>  8 Donkey          FALSE      Equidae     
#>  9 Horse           FALSE      Equidae     
#> 10 Potar monkey    FALSE      Primate     
#> # … with 18 more rows
#> 
#> 
#> column_info:
#> # A tibble: 2 × 2
#>   .colname unit 
#>   <chr>    <chr>
#> 1 body     kg   
#> 2 brain    g
```

The join operation joins the second meta info (`animal_info` in our
example) to the relevant `matrixset` meta info.

The `annotate` operation works like dplyr’s `mutate()` does: you can
modify existing annotation, create new ones or even remove them. It can
use already existing annotation as well.

``` r
animals_ms %>% 
  annotate_column(us_unit = case_when(unit == "kg" ~ "lb",
                                   TRUE ~ "oz")) %>% 
  column_info()
#> # A tibble: 2 × 3
#>   .colname unit  us_unit
#>   <chr>    <chr> <chr>  
#> 1 body     kg    lb     
#> 2 brain    g     oz
```

The only annotation that can’t be modify or deleted via `annotate` is
the the tag (typically, `.rowname` and `.colname`).

``` r
animals_ms %>% annotate_column(.colname = NULL)
#> Error in annotate_column.matrixset(., .colname = NULL): column tag (i.e., column info containing column names) can't be removed
```

## What to do with a matrixset

Many of the `array` operations are available for `matrixset`:

- Subseting operations `[` and `[[`. The subset `[` works like that of
  an array. The first argument is for rows, the second for columns and
  the third for matrices. It will return a `matrixset`, even if there is
  a single matrix returned as part of the subset. The `keep_annotation`
  argument (that always should be used after the 3 row/col/matrix
  arguments) will return only the matrix list.

  The operation `x[[m]]` is a wrapper to `x[,,m]`.

- Replacement method `[<-` Replaces matrix or matrices, in full or in
  part, by new values.

- `dimnames()` and the related `rownames()` and `colnames()`.

- The replacement versions: `dimnames<-()` and the related
  `rownames<-()` and `colnames<-()`. Note that `rownames<-()` and
  `colnames<-()` are what should be used for names replacement. The
  special annotation tag is modified this way.

- `dim()` and the related `nrow()` and `ncol()`

Other property methods specific to the `matrixset` object are available:

- `matrixnames()` (and the replacement version, `matrixnames<-()`) and
  `nmatrix()`
- Component extraction functions:
  - `matrix_elm()`: extracts a single matrix of the object, and returns
    it as a matrix
  - `row_info()` and `column_info()`: extracts the data frames that
    stores the annotation
  - `row_traits()` and `column_traits()`: returns the annotation names.
    The tag (column that stores the row/column names) is *not* returned.
- Component replacement functions
  - `matrix_elm<-()`: replaces a single matrix of the object
  - `row_info()<-` and `column_info()<-`: relaces whole annotation data
    frame
  - row_traits and `column_traits()`: replace annotation names. Tags
    *cannot* be replaced
- expansion/reduction functions `add_matrix()` and `remove_matrix()`.

Finally, many functions to manipulate or use the `matrixset` object,
often taking inspiration from the `tidyverse`, have been made available

- `mutate_matrix()`: create, modify or delete matrices. Supports tidy
  evaluation and tidy selection (for matrix names **and** annotations)
- `apply_matrix()`, `apply_row()` and `apply_column()` (and all of their
  declinations): apply functions/expressions to matrices, their rows or
  their columns. They have intuitive and direct access to annotations.
- `arrange_row()` and `arrange_column()`: reorder rows and columns.
  Supports tidy selection for annotation
- `filter_row()` and `filter_column()`: subset rows or columns of
  matrices based on conditions that may use annotations.

All of these functions (except for `mutate_matrix()`) can be performed
within groups - groups that could be defined using annotation. This is
done by a prior call to `row_group_by()` and/or `column_group_by()`.
