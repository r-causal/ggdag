# Query Markov Blanket

Find the Markov blanket for specified variables in a DAG. The Markov
blanket includes parents, children, and parents of children
(co-parents).

## Usage

``` r
query_markov_blanket(.tdy_dag, .var = NULL)
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

- `blanket`: String representation of Markov blanket nodes

- `blanket_vars`: List column containing Markov blanket nodes

- `blanket_size`: Size of the Markov blanket

## Examples

``` r
library(ggdag)
dag <- dagify(
  y ~ x + z,
  x ~ w,
  z ~ w
)

query_markov_blanket(dag)
#> # A tibble: 4 × 4
#>   node  blanket   blanket_vars blanket_size
#>   <chr> <chr>     <list>              <int>
#> 1 w     {x, z}    <chr [2]>               2
#> 2 x     {w, y, z} <chr [3]>               3
#> 3 y     {x, z}    <chr [2]>               2
#> 4 z     {w, x, y} <chr [3]>               3
query_markov_blanket(dag, .var = "x")
#> # A tibble: 1 × 4
#>   node  blanket   blanket_vars blanket_size
#>   <chr> <chr>     <list>              <int>
#> 1 x     {w, y, z} <chr [3]>               3
```
