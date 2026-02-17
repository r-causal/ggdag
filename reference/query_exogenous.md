# Query Exogenous Variables

Identify exogenous (parentless) variables in a DAG.

## Usage

``` r
query_exogenous(.tdy_dag)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

## Value

A tibble with columns:

- `node`: The exogenous variable

- `n_descendants`: Number of descendant nodes

## Examples

``` r
library(ggdag)
dag <- dagify(
  y ~ x + z,
  x ~ w
)

query_exogenous(dag)
#> # A tibble: 2 × 2
#>   node  n_descendants
#>   <chr>         <int>
#> 1 w                 2
#> 2 z                 1
```
