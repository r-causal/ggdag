# Saturate or prune an existing DAG

`dag_saturate()` takes a tidy DAG object and, optionally using existing
coordinates, saturates the DAG based on time ordering of the nodes. To
create a saturated DAG from scratch, see
[`as_tidy_dagitty.list()`](https://r-causal.github.io/ggdag/reference/as_tidy_dagitty.md).
`dag_prune()` takes an existing DAG and removes edges. This is most
useful when used together with saturated DAG.

## Usage

``` r
dag_saturate(
  .tdy_dag,
  use_existing_coords = FALSE,
  layout = "time_ordered",
  seed = NULL,
  ...
)

dag_prune(.tdy_dag, edges)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- use_existing_coords:

  Logical, indicating whether to use existing node coordinates.

- layout:

  a layout available in `ggraph`. See
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)
  for details. Alternatively, `"time_ordered"` will use
  [`time_ordered_coords()`](https://r-causal.github.io/ggdag/reference/time_ordered_coords.md)
  to algorithmically sort the graph by time. You can also pass the
  result of
  [`time_ordered_coords()`](https://r-causal.github.io/ggdag/reference/time_ordered_coords.md)
  directly: either the function returned when called with no arguments,
  or the coordinate tibble returned when called with arguments.

- seed:

  a numeric seed for reproducible layout generation

- ...:

  optional arguments passed to
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)

- edges:

  A named character vector where the name is the starting node and the
  value is the end node, e.g. `c("x" = "y")` will remove the edge going
  from `x` to `y`.

## Value

A `tidy_dagitty` object

## See also

[`as_tidy_dagitty.list()`](https://r-causal.github.io/ggdag/reference/as_tidy_dagitty.md)

## Examples

``` r
# Example usage:
dag <- dagify(y ~ x, x ~ z)
saturated_dag <- dag_saturate(dag)

saturated_dag |>
  ggdag(edge_type = "arc")


saturated_dag |>
  dag_prune(c("x" = "y")) |>
  ggdag(edge_type = "arc")
```
