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
#>          x       y       z
#>      <dbl>   <dbl>   <dbl>
#>  1  0.0559  1.92   -0.600 
#>  2  0.673   0.399  -0.487 
#>  3  0.403   0.515  -0.778 
#>  4  0.258   0.156  -0.0667
#>  5 -0.486   0.406  -0.113 
#>  6  1.08    0.986  -1.18  
#>  7 -0.382  -0.867   0.710 
#>  8  0.479  -0.0955  0.980 
#>  9 -0.968  -1.77    0.475 
#> 10 -0.543  -0.443  -0.525 
#> # ℹ 490 more rows
```
