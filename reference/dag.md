# Create a dagitty DAG

A convenience wrapper for
[`dagitty::dagitty()`](https://rdrr.io/pkg/dagitty/man/dagitty.html).

## Usage

``` r
dag(...)
```

## Arguments

- ...:

  a character vector in the style of dagitty. See
  `dagitty::`[`dagitty`](https://rdrr.io/pkg/dagitty/man/dagitty.html)
  for details.

## Value

a `dagitty`

## Examples

``` r
dag("{x m} -> y")
#> dag {
#> m
#> x
#> y
#> m -> y
#> x -> y
#> }
```
