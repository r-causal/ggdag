# Convert a `tidy_dagitty` object to tbl

Convert a `tidy_dagitty` object to tbl

## Usage

``` r
as.tbl.tidy_dagitty(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'tidy_dagitty'
as_tibble(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  an object of class `tidy_dagitty`

- row.names:

  NULL or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  logical. If TRUE, setting row names and converting column names (to
  syntactic names: see make.names) is optional. Note that all of R's
  base package
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  use optional only for column names treatment, basically with the
  meaning of `data.frame(*, check.names = !optional)`

- ...:

  optional arguments passed to
  [`dplyr::as_tibble()`](https://dplyr.tidyverse.org/reference/reexports.html)
