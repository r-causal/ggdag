# Set curvature for multiple edges at once

`set_curve_edges()` replaces all edge curvatures on a `dagitty` or
`tidy_dagitty` object from a data frame. Use
[`curve_edge()`](https://r-causal.github.io/ggdag/reference/curve_edge.md)
to set a single edge.

## Usage

``` r
set_curve_edges(.dag, edges)
```

## Arguments

- .dag:

  A `dagitty` or `tidy_dagitty` object.

- edges:

  A data frame with columns `from`, `to`, and `curvature`.

## Value

The modified `.dag` object with updated curvatures.

## Curvature sign convention

Per-edge curvature is drawn by the **ggarrow** edge engine only. Set it
with `edge_engine = "ggarrow"` on
[`geom_dag()`](https://r-causal.github.io/ggdag/reference/geom_dag.md)
and the `ggdag_*()` quick plots, or globally with
`ggdag_options_set(edge_engine = "ggarrow")`; the default **ggraph**
engine draws every edge with the curvature of its own edge type and
warns when it is handed a per-edge value it cannot draw.

ggarrow follows the
[`grid::curveGrob()`](https://rdrr.io/r/grid/grid.curve.html)
convention, so positive curvature curves *below* (to the right of) a
left-to-right edge and negative curvature curves *above* it. ggdag
passes the value through untouched.

A bidirected edge has no direction of its own, so ggdag draws it from
the endpoint dagitty stores first. Naming its endpoints the other way
round, as in `curve_edge(dag, "y", "x", 0.5)` for an edge stored as
`x <-> y`, curves the edge to the same side of the page: the sign is
flipped to match the direction the edge is drawn in.

## Examples

``` r
dag <- dagify(y ~ x + m, m ~ x)
edges <- data.frame(
  from = c("x", "m"),
  to = c("y", "y"),
  curvature = c(0.3, -0.4)
)
dag <- set_curve_edges(dag, edges)
```
