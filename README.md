
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/malcolmbarrett/ggdag.svg?branch=master)](https://travis-ci.org/malcolmbarrett/ggdag)

ggdag: An R Package for visualizing and analyzing directed acyclic graphs
=========================================================================

Built on top of `dagitty`, `ggplot2`, and `ggraph`, with influences from `ggnetwork`. Still in early development.

Installation
------------

You can install ggdag from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("malcolmbarrett/ggdag")
```

Example
-------

`ggdag` makes it easy to use the powerful `dagitty` package in the context of the tidyverse. You can directly tidy `dagitty` objects or use convenience functions to create using in a more R-like formula.

``` r
library(ggdag)

#  example from the dagitty package
dag <- dagitty::dagitty( "dag {
    y <- x <- z1 <- v -> z2 -> y
               z1 <- w1 <-> w2 -> z2
               x <- w1 -> y
               x <- w2 -> y
               x [exposure]
               y [outcome]
               }")

tidy_dag <- tidy_dagitty(dag)

tidy_dag 
#> # A tibble: 13 x 12
#>    name  from      x     y direction type       to     xend  yend
#>    <chr> <chr> <dbl> <dbl> <fct>     <fct>      <chr> <dbl> <dbl>
#>  1 v     v     11.4   5.57 ->        directed   z1    10.5   4.51
#>  2 v     v     11.4   5.57 ->        directed   z2    10.4   6.46
#>  3 w1    w1     8.97  4.87 ->        directed   x      9.55  4.69
#>  4 w1    w1     8.97  4.87 ->        directed   y      9.02  5.95
#>  5 w1    w1     8.97  4.87 ->        directed   z1    10.5   4.51
#>  6 w1    w1     8.97  4.87 <->       bidirected w2     9.64  5.72
#>  7 w2    w2     9.64  5.72 ->        directed   x      9.55  4.69
#>  8 w2    w2     9.64  5.72 ->        directed   y      9.02  5.95
#>  9 w2    w2     9.64  5.72 ->        directed   z2    10.4   6.46
#> 10 x     x      9.55  4.69 ->        directed   y      9.02  5.95
#> 11 z1    z1    10.5   4.51 ->        directed   x      9.55  4.69
#> 12 z2    z2    10.4   6.46 ->        directed   y      9.02  5.95
#> 13 y     y      9.02  5.95 <NA>      <NA>       <NA>  NA    NA   
#> # ... with 3 more variables: .ggraph.orig_index <int>, circular <lgl>,
#> #   .ggraph.index <int>

tidy_ggdag <- dagify(y ~ x + z2 + w2 + w1,
             x ~ z1 + w1,
             z1 ~ w1 + v,
             z2 ~ w2 + v,
             w1 ~~ w2,
             exposure = "x",
             outcome = "y") %>% tidy_dagitty()

tidy_ggdag
#> # A tibble: 12 x 12
#>    name  from      x     y direction type       to     xend  yend
#>    <chr> <chr> <dbl> <dbl> <fct>     <fct>      <chr> <dbl> <dbl>
#>  1 v     v      12.6  14.9 ->        directed   z1     11.4  14.1
#>  2 v     v      12.6  14.9 ->        directed   z2     11.9  16.0
#>  3 w1    w1     10.2  15.0 ->        directed   x      10.3  14.1
#>  4 w1    w1     10.2  15.0 ->        directed   y      10.8  15.5
#>  5 w1    w1     10.2  15.0 ->        directed   z1     11.4  14.1
#>  6 w1    w1     10.2  15.0 <->       bidirected w2     10.7  16.4
#>  7 w2    w2     10.7  16.4 ->        directed   y      10.8  15.5
#>  8 w2    w2     10.7  16.4 ->        directed   z2     11.9  16.0
#>  9 x     x      10.3  14.1 ->        directed   y      10.8  15.5
#> 10 z1    z1     11.4  14.1 ->        directed   x      10.3  14.1
#> 11 z2    z2     11.9  16.0 ->        directed   y      10.8  15.5
#> 12 y     y      10.8  15.5 <NA>      <NA>       <NA>   NA    NA  
#> # ... with 3 more variables: .ggraph.orig_index <int>, circular <lgl>,
#> #   .ggraph.index <int>
```

`ggdag` also provides functionality for analyzing DAGs and plotting them in `ggplot2`.

``` r
ggdag(tidy_ggdag)
```

<img src="man/figures/README-ggdag-1.png" width="100%" />

``` r
ggdag_adjustment_set(tidy_ggdag)
```

<img src="man/figures/README-ggdag-2.png" width="100%" />

As well as geoms and for plotting them directly in `ggplot2`.

``` r
dagify(m ~ x + y) %>% 
  tidy_dagitty() %>% 
  node_dconnected("x", "y", controlling_for = "m") %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
    geom_dag_edges() +
    geom_dag_collider_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() + 
    scale_dag()
```

<img src="man/figures/README-ggdag_geoms-1.png" width="100%" />
