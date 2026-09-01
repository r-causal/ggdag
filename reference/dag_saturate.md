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

  The edges to remove, in either of two forms. A named character vector
  where the name is the starting node and the value is the end node,
  e.g. `c("x" = "y")` removes the edge going from `x` to `y`. Or a data
  frame with a `name` and a `to` column, which says the same thing, and
  an optional `direction` column of `"->"`, `"<->"`, or `"--"`, which
  names one of the edges a pair of nodes holds.

## Value

A `tidy_dagitty` object

## Details

Bidirected edges carry no time-ordering information, so `dag_saturate()`
assigns time order from the directed edges alone and then passes the
input's bidirected edges through to the saturated DAG unchanged. A
saturated model therefore never implies an independence that the input
denies.

`dag_prune()` errors if `edges` is empty, and if it names an edge the
DAG does not contain, including an edge written in the reverse
direction. A node whose every edge is pruned is kept as an isolated
node.

A pair of nodes can hold a directed edge and a bidirected edge at the
same time, and endpoints alone name both of them. `dag_prune()` errors
on such a pair rather than pruning both; name the direction as well,
with the data frame form of `edges`, to prune one of them.

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
