# Query D-connection

Test whether sets of variables are d-connected in a DAG given a
conditioning set. This is the complement of d-separation.

## Usage

``` r
query_dconnected(.tdy_dag, from, to, conditioned_on = NULL)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- from:

  Character vector of nodes or a list of node sets.

- to:

  Character vector of nodes or a list of node sets.

- conditioned_on:

  Character vector of conditioning variables.

## Value

A tibble with columns:

- `from_set`: String representation of source nodes

- `from`: List column of source nodes

- `to_set`: String representation of target nodes

- `to`: List column of target nodes

- `conditioning_set`: String representation of conditioning variables

- `conditioned_on`: List column of conditioning variables

- `dconnected`: Logical indicating d-connection

## Examples

``` r
library(ggdag)
dag <- dagify(
  y ~ x + z,
  x ~ w,
  z ~ w
)

query_dconnected(dag, from = "x", to = "z")
#> # A tibble: 1 × 7
#>   from_set from      to_set to        conditioning_set conditioned_on dconnected
#>   <chr>    <list>    <chr>  <list>    <chr>            <list>         <lgl>     
#> 1 {x}      <chr [1]> {z}    <chr [1]> {}               <chr [0]>      TRUE      
query_dconnected(dag, from = "x", to = "z", conditioned_on = "w")
#> # A tibble: 1 × 7
#>   from_set from      to_set to        conditioning_set conditioned_on dconnected
#>   <chr>    <list>    <chr>  <list>    <chr>            <list>         <lgl>     
#> 1 {x}      <chr [1]> {z}    <chr [1]> {w}              <chr [1]>      FALSE     
```
