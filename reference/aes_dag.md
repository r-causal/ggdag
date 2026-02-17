# Define Aesthetics for Directed Acyclic Graphs (DAGs)

`aes_dag()` is a wrapper around
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) that
specifies `x`, `y`, `xend`, and `yend`, which are required for most DAG
visualizations. It merges any additional aesthetics, e.g. `color` or
`shape`, with the default aesthetic mappings.

## Usage

``` r
aes_dag(...)
```

## Arguments

- ...:

  Additional aesthetic mappings passed as arguments. These can include
  any aesthetic supported by ggplot2 (e.g., color, size, shape).

## Value

A `ggplot2` aesthetic mapping object that includes both the default DAG
aesthetics and any user-specified aesthetics.

## Examples

``` r
library(ggplot2)
confounder_triangle() |>
  dag_adjustment_sets() |>
  ggplot(aes_dag(color = .data$adjusted)) +
  geom_dag() +
  facet_wrap(~set)

```
