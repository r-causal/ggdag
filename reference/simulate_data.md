# Simulate Data from Structural Equation Model

This is a thin wrapper for the
[`simulateSEM()`](https://rdrr.io/pkg/dagitty/man/simulateSEM.html)function
in `dagitty` that works with tidied dagitty objects. It treats the input
DAG as a structural equation model, generating random path coefficients
and simulating corresponding data. See
[`dagitty::simulateSEM()`](https://rdrr.io/pkg/dagitty/man/simulateSEM.html)
for details.

## Usage

``` r
simulate_data(
  .tdy_dag,
  b.default = NULL,
  b.lower = -0.6,
  b.upper = 0.6,
  eps = 1,
  N = 500,
  standardized = TRUE
)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- b.default:

  default path coefficient applied to arrows for which no coefficient is
  defined in the model syntax.

- b.lower:

  lower bound for random path coefficients, applied if b.default = NULL.

- b.upper:

  upper bound for path coefficients.

- eps:

  residual variance (only meaningful if standardized=FALSE).

- N:

  number of samples to generate.

- standardized:

  whether a standardized output is desired (all variables have variance
  1).

## Value

a `tbl`with N values for each variable in .tdy_dag

## Examples

``` r
dagify(y ~ z, x ~ z) |>
  tidy_dagitty() |>
  simulate_data()
#> # A tibble: 500 × 3
#>          x       y      z
#>      <dbl>   <dbl>  <dbl>
#>  1 -0.0939  1.11   -0.447
#>  2 -0.101  -0.177   0.270
#>  3 -0.953   1.97    0.963
#>  4  1.25   -1.15   -0.130
#>  5 -0.106  -0.601   0.505
#>  6 -0.991   0.0746 -0.394
#>  7 -0.0142 -0.0875 -0.165
#>  8  2.43   -0.180  -2.14 
#>  9  0.763  -0.520  -1.28 
#> 10  1.66    0.924  -0.270
#> # ℹ 490 more rows
```
