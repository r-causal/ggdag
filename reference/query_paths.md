# Query Paths in a DAG

Find all paths between specified nodes in a DAG and determine if they
are open or closed given a conditioning set.

## Usage

``` r
query_paths(
  .tdy_dag,
  from = NULL,
  to = NULL,
  directed = FALSE,
  limit = 100,
  conditioned_on = NULL
)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- from:

  Character vector of starting nodes. If NULL, uses exposure from DAG.

- to:

  Character vector of ending nodes. If NULL, uses outcome from DAG.

- directed:

  Logical. If TRUE, only considers directed paths.

- limit:

  Maximum number of paths to return. Default is 100.

- conditioned_on:

  Character vector of variables to condition on.

## Value

A tibble with columns:

- `path_id`: Integer identifier for each path

- `from`: Starting node

- `to`: Ending node

- `path`: Character string representation of the path

- `path_type`: Character classification as "backdoor" or "direct"

- `variables`: List column containing all variables in the path

- `open`: Logical indicating if the path is open

## Examples

``` r
library(ggdag)
dag <- dagify(
  y ~ x + z,
  x ~ w,
  z ~ w,
  exposure = "x",
  outcome = "y"
)

query_paths(dag)
#> # A tibble: 2 × 7
#>   path_id from  to    path             path_type variables open 
#>     <int> <chr> <chr> <chr>            <chr>     <list>    <lgl>
#> 1       1 x     y     x -> y           direct    <chr [2]> TRUE 
#> 2       2 x     y     x <- w -> z -> y backdoor  <chr [4]> TRUE 
query_paths(dag, conditioned_on = "z")
#> # A tibble: 2 × 7
#>   path_id from  to    path             path_type variables open 
#>     <int> <chr> <chr> <chr>            <chr>     <list>    <lgl>
#> 1       1 x     y     x -> y           direct    <chr [2]> TRUE 
#> 2       2 x     y     x <- w -> z -> y backdoor  <chr [4]> FALSE
```
