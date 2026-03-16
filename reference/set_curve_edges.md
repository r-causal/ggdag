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
edges <- data.frame(
  from = c("x", "m"),
  to = c("y", "y"),
  curvature = c(0.3, -0.4)
)
dag <- set_curve_edges(dag, edges)
```
