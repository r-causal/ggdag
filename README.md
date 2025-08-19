
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/r-causal/ggdag/workflows/R-CMD-check/badge.svg)](https://github.com/r-causal/ggdag/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggdag)](https://cran.r-project.org/package=ggdag)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Codecov test
coverage](https://codecov.io/gh/malcolmbarrett/ggdag/branch/main/graph/badge.svg)](https://app.codecov.io/gh/malcolmbarrett/ggdag?branch=main)
[![Total CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/ggdag)](https://cran.r-project.org/package=ggdag)
[![R-CMD-check](https://github.com/malcolmbarrett/ggdag/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/malcolmbarrett/ggdag/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# ggdag: An R Package for visualizing and analyzing causal directed acyclic graphs <a href="https://r-causal.github.io/ggdag/"><img src="man/figures/logo.png" align="right" height="138" /></a>

Tidy, analyze, and plot causal directed acyclic graphs (DAGs). `ggdag`
uses the powerful `dagitty` package to create and analyze structural
causal models and plot them using `ggplot2` and `ggraph` in a consistent
and easy manner.

## Installation

You can install `ggdag` with:

``` r
install.packages("ggdag")
```

Or you can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("r-causal/ggdag")
```

## Example

`ggdag` makes it easy to use `dagitty` in the context of the tidyverse.
You can directly tidy `dagitty` objects or use convenience functions to
create DAGs using a more R-like syntax:

``` r
library(ggdag)
library(ggplot2)

#  example from the dagitty package
dag <- dagitty::dagitty("dag {
    y <- x <- z1 <- v -> z2 -> y
    z1 <- w1 <-> w2 -> z2
    x <- w1 -> y
    x <- w2 -> y
    x [exposure]
    y [outcome]
  }")

tidy_dag <- tidy_dagitty(dag)

tidy_dag
#> # DAG:
#> # A `dagitty` DAG with: 7 nodes and 12 edges
#> # Exposure: x
#> # Outcome: y
#> #
#> # Data:
#> # A tibble: 13 × 7
#>    name       x     y direction to      xend   yend
#>    <chr>  <dbl> <dbl> <fct>     <chr>  <dbl>  <dbl>
#>  1 v     -0.180 0.946 ->        z1     1.20   0.608
#>  2 v     -0.180 0.946 ->        z2     0.177  2.32 
#>  3 w1     1.64  1.49  ->        x      2.23   1.70 
#>  4 w1     1.64  1.49  ->        y      1.63   2.71 
#>  5 w1     1.64  1.49  ->        z1     1.20   0.608
#>  6 w1     1.64  1.49  <->       w2     1.16   2.30 
#>  7 w2     1.16  2.30  ->        x      2.23   1.70 
#>  8 w2     1.16  2.30  ->        y      1.63   2.71 
#>  9 w2     1.16  2.30  ->        z2     0.177  2.32 
#> 10 x      2.23  1.70  ->        y      1.63   2.71 
#> 11 y      1.63  2.71  <NA>      <NA>  NA     NA    
#> 12 z1     1.20  0.608 ->        x      2.23   1.70 
#> 13 z2     0.177 2.32  ->        y      1.63   2.71 
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

#  using more R-like syntax to create the same DAG
tidy_ggdag <- dagify(
  y ~ x + z2 + w2 + w1,
  x ~ z1 + w1 + w2,
  z1 ~ w1 + v,
  z2 ~ w2 + v,
  w1 ~ ~w2, # bidirected path
  exposure = "x",
  outcome = "y"
) |>
  tidy_dagitty()

tidy_ggdag
#> # DAG:
#> # A `dagitty` DAG with: 7 nodes and 12 edges
#> # Exposure: x
#> # Outcome: y
#> #
#> # Data:
#> # A tibble: 13 × 7
#>    name          x     y direction to         xend  yend
#>    <chr>     <dbl> <dbl> <fct>     <chr>     <dbl> <dbl>
#>  1 v      0.696     4.02 ->        z1     1.07      2.66
#>  2 v      0.696     4.02 ->        z2    -0.669     3.67
#>  3 w1     0.175     2.21 ->        x     -0.000636  1.61
#>  4 w1     0.175     2.21 ->        y     -1.05      2.22
#>  5 w1     0.175     2.21 ->        z1     1.07      2.66
#>  6 w1     0.175     2.21 <->       w2    -0.613     2.67
#>  7 w2    -0.613     2.67 ->        x     -0.000636  1.61
#>  8 w2    -0.613     2.67 ->        y     -1.05      2.22
#>  9 w2    -0.613     2.67 ->        z2    -0.669     3.67
#> 10 x     -0.000636  1.61 ->        y     -1.05      2.22
#> 11 y     -1.05      2.22 <NA>      <NA>  NA        NA   
#> 12 z1     1.07      2.66 ->        x     -0.000636  1.61
#> 13 z2    -0.669     3.67 ->        y     -1.05      2.22
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame
```

`ggdag` also provides functionality for analyzing DAGs and plotting them
in `ggplot2`:

``` r
ggdag(tidy_ggdag) +
  theme_dag()
```

<img src="man/figures/ggdag-1.png" width="100%" />

``` r
ggdag_adjustment_set(tidy_ggdag, node_size = 14) +
  theme(legend.position = "bottom")
```

<img src="man/figures/ggdag-2.png" width="100%" />

As well as geoms and other functions for plotting them directly in
`ggplot2`:

``` r
dagify(m ~ x + y) |>
  tidy_dagitty() |>
  node_dconnected("x", "y", controlling_for = "m") |>
  ggplot(aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
    shape = adjusted,
    col = d_relationship
  )) +
  geom_dag_edges(end_cap = ggraph::circle(10, "mm")) +
  geom_dag_collider_edges() +
  geom_dag_point() +
  geom_dag_text(col = "white") +
  theme_dag() +
  scale_adjusted(include_color = FALSE) +
  expand_plot(expand_y = expansion(c(0.2, 0.2))) +
  scale_color_viridis_d(
    name = "d-relationship",
    na.value = "grey85",
    begin = .35
  )
```

<img src="man/figures/ggdag_geoms-1.png" width="100%" />

And common structures of bias:

``` r
ggdag_equivalent_dags(confounder_triangle())
```

<img src="man/figures/ggdag_common-1.png" width="100%" />

``` r

ggdag_butterfly_bias(edge_type = "diagonal")
```

<img src="man/figures/ggdag_common-2.png" width="100%" />
