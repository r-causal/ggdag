# Query Node Children

Find child nodes for specified variables in a DAG.

## Usage

``` r
query_children(.tdy_dag, .var = NULL)
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

- `child_set`: String representation of child nodes

- `children`: List column containing child nodes

- `n_children`: Number of children

## Examples

``` r
library(ggdag)
dag <- dagify(
  y ~ x + z,
  x ~ w
)

query_children(dag)
#> # A tibble: 4 × 4
#>   node  child_set children  n_children
#>   <chr> <chr>     <list>         <int>
#> 1 w     {x}       <chr [1]>          1
#> 2 x     {y}       <chr [1]>          1
#> 3 y     NA        <chr [1]>          0
#> 4 z     {y}       <chr [1]>          1
query_children(dag, .var = "x")
#> # A tibble: 1 × 4
#>   node  child_set children  n_children
#>   <chr> <chr>     <list>         <int>
#> 1 x     {y}       <chr [1]>          1
```
