# Mark an edge as curved in dagify formulas

Use `curved()` inside
[`dagify()`](https://r-causal.github.io/ggdag/reference/dagify.md)
formulas to specify per-edge curvature. This function should only be
used inside
[`dagify()`](https://r-causal.github.io/ggdag/reference/dagify.md)
formulas; calling it directly will result in an error, similar to
[`dplyr::n()`](https://dplyr.tidyverse.org/reference/context.html).

## Usage

``` r
curved(var, curvature = 0.3)
```

## Arguments

- var:

  A variable name (unquoted) representing the parent node.

- curvature:

  A numeric curvature value. Positive values curve edges in one
  direction, negative in the other. Default is `0.3`.

## Value

This function is not intended to be called directly. It is detected in
the formula AST by
[`dagify()`](https://r-causal.github.io/ggdag/reference/dagify.md).

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
# Curve the edge from m to y
dagify(
  y ~ x + curved(m, 0.5),
  m ~ x
)
#> dag {
#> m
#> x
#> y
#> m -> y
#> x -> m
#> x -> y
#> }
```
