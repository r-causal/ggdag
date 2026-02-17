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
#>  1 -0.341   0.112   1.52 
#>  2 -0.0760  0.753  -1.67 
#>  3 -1.39    0.338  -0.225
#>  4 -0.340   0.588  -0.764
#>  5 -0.0306  0.897  -1.10 
#>  6 -0.650  -1.65    0.765
#>  7  0.470   0.0719 -0.651
#>  8  0.536  -1.26   -0.296
#>  9  0.850  -0.358   1.35 
#> 10 -0.560  -0.596  -0.452
#> # ℹ 490 more rows
```
