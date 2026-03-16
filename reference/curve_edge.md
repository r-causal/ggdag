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

The curvature value is passed directly to the active edge rendering
engine. The **ggraph** engine (default) and **ggarrow** engine interpret
the sign differently:

- **ggraph**: positive curvature curves *above* (to the left of) a
  left-to-right edge.

- **ggarrow** / **grid**: positive curvature curves *below* (to the
  right of) a left-to-right edge, following
  [`grid::curveGrob()`](https://rdrr.io/r/grid/grid.curve.html)
  convention.

This means the same `curvature` value will render as a mirror image
depending on the engine. ggdag does not negate or transform the value;
each engine uses its native convention.

## Examples

``` r
dag <- dagify(y ~ x + m, m ~ x)
dag <- curve_edge(dag, from = "m", to = "y", curvature = 0.5)
```
