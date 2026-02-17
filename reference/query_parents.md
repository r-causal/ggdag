# Query Node Parents

Find parent nodes for specified variables in a DAG.

## Usage

``` r
query_parents(.tdy_dag, .var = NULL)
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

- `parent_set`: String representation of parent nodes

- `parents`: List column containing parent nodes

- `n_parents`: Number of parents

## Examples

``` r
library(ggdag)
dag <- dagify(
  y ~ x + z,
  x ~ w
)

query_parents(dag)
#> # A tibble: 4 × 4
#>   node  parent_set parents   n_parents
#>   <chr> <chr>      <list>        <int>
#> 1 w     NA         <chr [1]>         0
#> 2 x     {w}        <chr [1]>         1
#> 3 y     {x, z}     <chr [2]>         2
#> 4 z     NA         <chr [1]>         0
query_parents(dag, .var = "y")
#> # A tibble: 1 × 4
#>   node  parent_set parents   n_parents
#>   <chr> <chr>      <list>        <int>
#> 1 y     {x, z}     <chr [2]>         2
```
