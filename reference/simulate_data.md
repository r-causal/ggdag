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
#>         x       y      z
#>     <dbl>   <dbl>  <dbl>
#>  1  1.28   1.03   -1.09 
#>  2 -0.409 -1.51    1.28 
#>  3 -1.23   0.465   0.221
#>  4 -0.788  0.861  -0.670
#>  5 -0.239 -0.906   1.08 
#>  6 -0.115  0.0196 -2.04 
#>  7  0.102  0.857   1.28 
#>  8  0.615  0.857   0.904
#>  9  0.724  0.854   0.123
#> 10 -0.680 -0.698  -2.35 
#> # ℹ 490 more rows
```
