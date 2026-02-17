# Dplyr verb methods for `tidy_dagitty` objects

Dplyr verb methods for `tidy_dagitty` objects.

## Usage

``` r
# S3 method for class 'tidy_dagitty'
select(.data, ...)

# S3 method for class 'tidy_dagitty'
filter(.data, ...)

# S3 method for class 'tidy_dagitty'
mutate(.data, ...)

# S3 method for class 'tidy_dagitty'
summarise(.data, ...)

# S3 method for class 'tidy_dagitty'
arrange(.data, ...)

# S3 method for class 'tidy_dagitty'
group_by(.data, ...)

# S3 method for class 'tidy_dagitty'
ungroup(x, ...)

# S3 method for class 'tidy_dagitty'
transmute(.data, ...)

# S3 method for class 'tidy_dagitty'
distinct(.data, ..., .keep_all = FALSE)

# S3 method for class 'tidy_dagitty'
full_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

# S3 method for class 'tidy_dagitty'
inner_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

# S3 method for class 'tidy_dagitty'
left_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

# S3 method for class 'tidy_dagitty'
right_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

# S3 method for class 'tidy_dagitty'
anti_join(x, y, by = NULL, copy = FALSE, ...)

# S3 method for class 'tidy_dagitty'
semi_join(x, y, by = NULL, copy = FALSE, ...)

# S3 method for class 'tidy_dagitty'
slice(.data, ..., .dots = list())

# S3 method for class 'tidy_dagitty'
select_(.data, ..., .dots = list())

# S3 method for class 'tidy_dagitty'
filter_(.data, ..., .dots = list())

# S3 method for class 'tidy_dagitty'
mutate_(.data, ..., .dots = list())

# S3 method for class 'tidy_dagitty'
summarise_(.data, ..., .dots = list())

# S3 method for class 'tidy_dagitty'
arrange_(.data, ..., .dots = list())

# S3 method for class 'tidy_dagitty'
slice_(.data, ..., .dots = list())
```

## Arguments

- .data:

  data object of class `tidy_dagitty`

- ...:

  other arguments passed to the `dplyr` function

- .dots, x, y, by, copy, suffix, .keep_all:

  see corresponding function in package `dplyr`

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
tidy_dagitty(m_bias()) |>
  group_by(name) |>
  summarize(n = n())
#> # A tibble: 5 × 2
#>   name      n
#>   <chr> <int>
#> 1 a         2
#> 2 b         2
#> 3 m         1
#> 4 x         1
#> 5 y         1
```
