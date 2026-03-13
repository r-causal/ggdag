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
#>            x      y       z
#>        <dbl>  <dbl>   <dbl>
#>  1 -0.000336  0.419  0.0724
#>  2  1.37     -1.10  -0.978 
#>  3  1.26      1.35  -1.99  
#>  4  1.70      0.431 -0.917 
#>  5 -0.225     1.12   0.383 
#>  6 -0.203    -1.13   0.928 
#>  7  0.488    -0.629 -1.60  
#>  8  0.624     0.868 -1.16  
#>  9  0.800    -0.997 -2.03  
#> 10  1.85     -0.323  0.846 
#> # ℹ 490 more rows
```
