# Query Node Descendants

Find descendant nodes for specified variables in a DAG.

## Usage

``` r
query_descendants(.tdy_dag, .var = NULL)
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

- `descendant_set`: String representation of descendant nodes

- `descendants`: List column containing descendant nodes

- `n_descendants`: Number of descendants

## Examples

``` r
library(ggdag)
dag <- dagify(
  y ~ x + z,
  x ~ w
)

query_descendants(dag)
#> # A tibble: 4 × 4
#>   node  descendant_set descendants n_descendants
#>   <chr> <chr>          <list>              <int>
#> 1 w     {x, y}         <chr [2]>               2
#> 2 x     {y}            <chr [1]>               1
#> 3 y     NA             <chr [1]>               0
#> 4 z     {y}            <chr [1]>               1
query_descendants(dag, .var = "w")
#> # A tibble: 1 × 4
#>   node  descendant_set descendants n_descendants
#>   <chr> <chr>          <list>              <int>
#> 1 w     {x, y}         <chr [2]>               2
```
