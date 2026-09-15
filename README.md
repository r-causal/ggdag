
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggdag)](https://cran.r-project.org/package=ggdag)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Codecov test
coverage](https://codecov.io/gh/r-causal/ggdag/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-causal/ggdag?branch=main)
[![Total CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/ggdag)](https://cran.r-project.org/package=ggdag)
[![R-CMD-check](https://github.com/r-causal/ggdag/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-causal/ggdag/actions/workflows/R-CMD-check.yaml)
[![R-universe
version](https://r-causal.r-universe.dev/ggdag/badges/version)](https://r-causal.r-universe.dev/ggdag)
<!-- badges: end -->

# ggdag: An R Package for visualizing and analyzing causal directed acyclic graphs <a href="https://r-causal.github.io/ggdag/"><img src="man/figures/logo.png" align="right" height="138" /></a>

Tidy, analyze, and plot causal directed acyclic graphs (DAGs). `ggdag`
uses the powerful `dagitty` package to create and analyze structural
causal models and plot them using `ggplot2` and `ggraph` in a consistent
and easy manner.

## Installation

You can install ggdag from CRAN with:

``` r
install.packages("ggdag")
```

You can install the development version of ggdag from
[r-causal.r-universe.dev](https://r-causal.r-universe.dev/) with:

``` r
install.packages(
  "ggdag",
  repos = c("https://r-causal.r-universe.dev", getOption("repos"))
)
```

You can also install the development version of ggdag from source from
[GitHub](https://github.com/r-causal/ggdag) with:

``` r
# install.packages("pak")
pak::pak("r-causal/ggdag")
```

## Example

`ggdag` makes it easy to use `dagitty` in the context of the tidyverse.
You can directly tidy `dagitty` objects or use convenience functions to
create DAGs using a more R-like syntax. A DAG without coordinates of its
own is laid out in time order, with causes placed before their effects:

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
#>    name      x      y direction to     xend   yend
#>    <chr> <int>  <dbl> <fct>     <chr> <int>  <dbl>
#>  1 v         1 -0.313 ->        z1        2 -0.626
#>  2 v         1 -0.313 ->        z2        3 -0.258
#>  3 w1        1  0.102 ->        x         3  0.142
#>  4 w1        1  0.102 ->        y         4  0.452
#>  5 w1        1  0.102 ->        z1        2 -0.626
#>  6 w1        1  0.102 <->       w2        1  0.502
#>  7 w2        1  0.502 ->        x         3  0.142
#>  8 w2        1  0.502 ->        y         4  0.452
#>  9 w2        1  0.502 ->        z2        3 -0.258
#> 10 x         3  0.142 ->        y         4  0.452
#> 11 y         4  0.452 <NA>      <NA>     NA NA    
#> 12 z1        2 -0.626 ->        x         3  0.142
#> 13 z2        3 -0.258 ->        y         4  0.452
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
  outcome = "y",
  coords = time_ordered_coords()
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
#>    name      x      y direction to     xend   yend
#>    <chr> <int>  <dbl> <fct>     <chr> <int>  <dbl>
#>  1 v         1 -0.313 ->        z1        2 -0.626
#>  2 v         1 -0.313 ->        z2        3 -0.258
#>  3 w1        1  0.102 ->        x         3  0.142
#>  4 w1        1  0.102 ->        y         4  0.452
#>  5 w1        1  0.102 ->        z1        2 -0.626
#>  6 w1        1  0.102 <->       w2        1  0.502
#>  7 w2        1  0.502 ->        x         3  0.142
#>  8 w2        1  0.502 ->        y         4  0.452
#>  9 w2        1  0.502 ->        z2        3 -0.258
#> 10 x         3  0.142 ->        y         4  0.452
#> 11 y         4  0.452 <NA>      <NA>     NA NA    
#> 12 z1        2 -0.626 ->        x         3  0.142
#> 13 z2        3 -0.258 ->        y         4  0.452
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame
```

`ggdag` also provides functionality for analyzing DAGs and plotting them
in `ggplot2`:

``` r
ggdag(tidy_ggdag) +
  theme_dag()
```

<img src="man/figures/ggdag-1.png" alt="" width="100%" />

``` r
ggdag_adjustment_set(tidy_ggdag, node_size = 14) +
  theme(legend.position = "bottom")
```

<img src="man/figures/ggdag-2.png" alt="" width="100%" />

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
    begin = 0.35
  )
```

<img src="man/figures/ggdag_geoms-1.png" alt="" width="100%" />

And common structures of bias:

``` r
ggdag_equivalent_dags(confounder_triangle())
```

<img src="man/figures/ggdag_common-1.png" alt="" width="100%" />

``` r

ggdag_butterfly_bias(edge_type = "diagonal")
```

<img src="man/figures/ggdag_common-2.png" alt="" width="100%" />
