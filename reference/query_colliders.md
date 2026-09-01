# Query Collider Nodes

Identify all collider nodes in a DAG. A collider is a node that two or
more edges point into. Bidirected edges count toward that total, so a
node with one directed parent and one bidirected partner is a collider.

## Usage

``` r
query_colliders(.tdy_dag)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

## Value

A tibble with columns:

- `node`: The collider node

- `parent_set`: String representation of the directed parents

- `parents`: List column containing the directed parents. A bidirected
  partner contributes an arrowhead but is not a parent, so a collider
  formed by bidirected edges can have fewer than two parents here.

- `is_activated`: Logical indicating if the collider is conditioned on

## Examples

``` r
library(ggdag)
dag <- dagify(
  z ~ x + y,
  w ~ z
)

query_colliders(dag)
#> # A tibble: 1 × 4
#>   node  parent_set parents   is_activated
#>   <chr> <chr>      <list>    <lgl>       
#> 1 z     {x, y}     <chr [2]> FALSE       
```
