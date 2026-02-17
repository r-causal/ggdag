# Query Variable Status

Query the status of variables in a DAG (exposure, outcome, or latent).

## Usage

``` r
query_status(.tdy_dag, .var = NULL)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- .var:

  Character vector of variables to query. If NULL, returns status for
  all nodes.

## Value

A tibble with columns:

- `name`: The variable name

- `status`: The variable status (exposure, outcome, latent, or NA)

## Examples

``` r
library(ggdag)
dag <- dagify(
  l ~ x + y,
  y ~ x,
  exposure = "x",
  outcome = "y",
  latent = "l"
)

query_status(dag)
#> # A tibble: 3 × 2
#>   name  status  
#>   <chr> <chr>   
#> 1 l     latent  
#> 2 x     exposure
#> 3 y     outcome 
query_status(dag, .var = "x")
#> # A tibble: 1 × 2
#>   name  status  
#>   <chr> <chr>   
#> 1 x     exposure
```
