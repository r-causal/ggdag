# Query Node Ancestors

Find ancestor nodes for specified variables in a DAG.

## Usage

``` r
query_ancestors(.tdy_dag, .var = NULL)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- .var:

  Character vector of variables to query. If NULL, returns parents for
  all nodes.

## Value

A tibble with columns:

- `node`: The node

- `ancestor_set`: String representation of ancestor nodes

- `ancestors`: List column containing ancestor nodes

- `n_ancestors`: Number of ancestors

## Examples

``` r
library(ggdag)
dag <- dagify(
  y ~ x + z,
  x ~ w
)

query_ancestors(dag)
#> # A tibble: 4 × 4
#>   node  ancestor_set ancestors n_ancestors
#>   <chr> <chr>        <list>          <int>
#> 1 w     NA           <chr [1]>           0
#> 2 x     {w}          <chr [1]>           1
#> 3 y     {w, x, z}    <chr [3]>           3
#> 4 z     NA           <chr [1]>           0
query_ancestors(dag, .var = "y")
#> # A tibble: 1 × 4
#>   node  ancestor_set ancestors n_ancestors
#>   <chr> <chr>        <list>          <int>
#> 1 y     {w, x, z}    <chr [3]>           3
```
