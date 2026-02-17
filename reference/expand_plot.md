# Quickly scale the size of a ggplot

`expand_plot()` is a convenience function that expands the scales of a
ggplot, as the large node sizes in a DAG will often get clipped in
themes that don't have DAGs in mind.

## Usage

``` r
expand_plot(
  expand_x = expansion(c(0.1, 0.1)),
  expand_y = expansion(c(0.1, 0.1))
)
```

## Arguments

- expand_x, expand_y:

  Vector of range expansion constants used to add some padding around
  the data, to ensure that they are placed some distance away from the
  axes. Use the convenience function
  [`ggplot2::expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  to generate the values for the expand argument.
