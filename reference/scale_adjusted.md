# Common scale adjustments for DAGs

`scale_adjusted()` is a convenience function that implements ways of
visualizing adjustment for a variable. By convention, a square shape is
used to indicate adjustment and a circle when not adjusted. Arrows out
of adjusted variables are often eliminated or de-emphasized, and
`scale_adjusted()` uses a lower `alpha` for these arrows. When adjusting
a collider, a dashed line is sometimes used to demarcate opened
pathways, and `scale_adjusted()` does this whenever
[`geom_dag_collider_edges()`](https://r-causal.github.io/ggdag/reference/geom_dag_collider_edges.md)
is used. `scale_dag()` is deprecated in favor of `scale_adjusted()`.

## Usage

``` r
scale_adjusted(
  include_linetype = TRUE,
  include_shape = TRUE,
  include_color = TRUE,
  include_alpha = FALSE
)

scale_dag(breaks = ggplot2::waiver())
```

## Arguments

- include_linetype:

  Logical. Include linetype scale for dashed lines on collider edges?
  Default is TRUE.

- include_shape:

  Logical. Include shape scale for adjustment status (squares for
  adjusted, circles for unadjusted)? Default is TRUE.

- include_color:

  Logical. Include color scale for adjustment status? Default is TRUE.

- include_alpha:

  Logical. Include alpha scales for de-emphasizing edges from adjusted
  variables? Default is FALSE.

- breaks:

  One of:

  - NULL for no breaks

  - waiver() for the default breaks computed by the transformation
    object

  - A numeric vector of positions

  - A function that takes the limits as input and returns breaks as
    output
