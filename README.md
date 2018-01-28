
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/malcolmbarrett/ggdag.svg?branch=master)](https://travis-ci.org/malcolmbarrett/ggdag) [![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/kd3ed7rj6p2vd36t?svg=true)](https://ci.appveyor.com/project/malcolmbarrett/ggdag)

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

`ggdag` makes it easy to use the powerful `dagitty` package in the context of the tidyverse. You can directly tidy `dagitty` objects or use convenience functions to create DAGs using a more R-like syntax.

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
#>  1 v     v      12.3  6.20 ->        directed   z1     10.9  6.31
#>  2 v     v      12.3  6.20 ->        directed   z2     12.1  4.78
#>  3 w1    w1     10.7  5.29 ->        directed   x      10.1  5.09
#>  4 w1    w1     10.7  5.29 ->        directed   y      11.2  4.48
#>  5 w1    w1     10.7  5.29 ->        directed   z1     10.9  6.31
#>  6 w1    w1     10.7  5.29 <->       bidirected w2     10.7  4.16
#>  7 w2    w2     10.7  4.16 ->        directed   x      10.1  5.09
#>  8 w2    w2     10.7  4.16 ->        directed   y      11.2  4.48
#>  9 w2    w2     10.7  4.16 ->        directed   z2     12.1  4.78
#> 10 x     x      10.1  5.09 ->        directed   y      11.2  4.48
#> 11 z1    z1     10.9  6.31 ->        directed   x      10.1  5.09
#> 12 z2    z2     12.1  4.78 ->        directed   y      11.2  4.48
#> 13 y     y      11.2  4.48 <NA>      <NA>       <NA>   NA   NA   
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
#>  1 v     v      14.2  15.4 ->        directed   z1     13.1  14.6
#>  2 v     v      14.2  15.4 ->        directed   z2     15.0  14.3
#>  3 w1    w1     13.4  13.2 ->        directed   x      12.7  13.7
#>  4 w1    w1     13.4  13.2 ->        directed   y      14.1  13.6
#>  5 w1    w1     13.4  13.2 ->        directed   z1     13.1  14.6
#>  6 w1    w1     13.4  13.2 <->       bidirected w2     14.8  13.1
#>  7 w2    w2     14.8  13.1 ->        directed   y      14.1  13.6
#>  8 w2    w2     14.8  13.1 ->        directed   z2     15.0  14.3
#>  9 x     x      12.7  13.7 ->        directed   y      14.1  13.6
#> 10 z1    z1     13.1  14.6 ->        directed   x      12.7  13.7
#> 11 z2    z2     15.0  14.3 ->        directed   y      14.1  13.6
#> 12 y     y      14.1  13.6 <NA>      <NA>       <NA>   NA    NA  
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
    geom_dag_edges(aes(end_cap = ggraph::circle(10, "mm"))) +
    geom_dag_collider_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() + 
    scale_dag(expand_y = expand_scale(c(0.2, 0.2)))
```

<img src="man/figures/README-ggdag_geoms-1.png" width="100%" />
