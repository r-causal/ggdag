# Test node relationships

These functions test relationships between nodes in a DAG:

- `is_parent()` tests whether one node is a parent of another

- `is_child()` tests whether one node is a child of another

- `is_ancestor()` tests whether one node is an ancestor of another

- `is_descendant()` tests whether one node is a descendant of another

- `is_adjacent()` tests whether two nodes are adjacent (connected by an
  edge)

## Usage

``` r
is_parent(.dag, .var, .node)

is_child(.dag, .var, .node)

is_ancestor(.dag, .var, .node)

is_descendant(.dag, .var, .node)

is_adjacent(.dag, .var, .node)
```

## Arguments

- .dag:

  A `tidy_dagitty` or `dagitty` object

- .var:

  A character string specifying the variable to test

- .node:

  A character string specifying the reference node

## Value

A logical value indicating whether the relationship holds

## Examples

``` r
dag <- dagify(
  y ~ x + z,
  x ~ z
)

is_parent(dag, "z", "x")
#> [1] TRUE
is_child(dag, "x", "z")
#> [1] TRUE
is_ancestor(dag, "z", "y")
#> [1] TRUE
is_descendant(dag, "y", "z")
#> [1] TRUE
is_adjacent(dag, "x", "y")
#> [1] TRUE
```
