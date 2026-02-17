# Simple grey themes for DAGs

Simple grey themes for DAGs

## Usage

``` r
theme_dag_grey(base_size = 12, base_family = "", ...)

theme_dag_gray(base_size = 12, base_family = "", ...)

theme_dag_grey_grid(base_size = 12, base_family = "", ...)

theme_dag_gray_grid(base_size = 12, base_family = "", ...)
```

## Arguments

- base_size:

  base font size, given in pts.

- base_family:

  base font family

- ...:

  additional arguments passed to
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)

## Examples

``` r
ggdag(m_bias()) + theme_dag_grey()

```
