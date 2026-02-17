# Quickly remove plot axes and grids

`remove_axes()` and `remove_grid()` are convenience functions that
removes the axes and grids from a ggplot, respectively. This is useful
when you want to use an existing theme, e.g. those included in
`ggplot2`, for a DAG.

## Usage

``` r
remove_axes()

remove_grid()
```

## Examples

``` r
library(ggplot2)
ggdag(confounder_triangle()) +
  theme_bw() +
  remove_axes()

```
