# Add or update curvature for a single edge

`curve_edge()` sets the curvature for a single edge on a `dagitty` or
`tidy_dagitty` object. Use
[`set_curve_edges()`](https://r-causal.github.io/ggdag/reference/set_curve_edges.md)
to set multiple edges at once.

## Usage

``` r
curve_edge(.dag, from, to, curvature = 0.3)
```

## Arguments

- .dag:

  A `dagitty` or `tidy_dagitty` object.

- from:

  Character. The name of the source node.

- to:

  Character. The name of the target node.

- curvature:

  Numeric. The curvature value for the edge.

## Value

The modified `.dag` object with updated curvature.

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
dag <- curve_edge(dag, from = "m", to = "y", curvature = 0.5)
```
