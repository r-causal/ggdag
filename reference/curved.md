# Mark an edge as curved in dagify formulas

Use `curved()` inside
[`dagify()`](https://r-causal.github.io/ggdag/reference/dagify.md)
formulas to specify per-edge curvature. This function should only be
used inside
[`dagify()`](https://r-causal.github.io/ggdag/reference/dagify.md)
formulas — calling it directly will result in an error, similar to
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
