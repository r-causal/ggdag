
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

`ggdag` makes it easy to use the powerful `dagitty` package in the context of the tidyverse. You can directly tidy `dagitty` objects or use convenience functions to create DAGs using a more R-like syntax:

``` r
library(ggdag)
#> 
#> Attaching package: 'ggdag'
#> The following object is masked from 'package:ggplot2':
#> 
#>     expand_scale
#> The following object is masked from 'package:stats':
#> 
#>     filter

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
#>  1 v     v     11.8   8.03 ->        directed   z1    10.4   7.77
#>  2 v     v     11.8   8.03 ->        directed   z2    12.1   6.66
#>  3 w1    w1    10.2   6.85 ->        directed   x      9.95  6.28
#>  4 w1    w1    10.2   6.85 ->        directed   y     11.1   6.39
#>  5 w1    w1    10.2   6.85 ->        directed   z1    10.4   7.77
#>  6 w1    w1    10.2   6.85 <->       bidirected w2    10.9   5.75
#>  7 w2    w2    10.9   5.75 ->        directed   x      9.95  6.28
#>  8 w2    w2    10.9   5.75 ->        directed   y     11.1   6.39
#>  9 w2    w2    10.9   5.75 ->        directed   z2    12.1   6.66
#> 10 x     x      9.95  6.28 ->        directed   y     11.1   6.39
#> 11 z1    z1    10.4   7.77 ->        directed   x      9.95  6.28
#> 12 z2    z2    12.1   6.66 ->        directed   y     11.1   6.39
#> 13 y     y     11.1   6.39 <NA>      <NA>       <NA>  NA    NA   
#> # ... with 3 more variables: .ggraph.orig_index <int>, circular <lgl>,
#> #   .ggraph.index <int>

#  using more R-like syntax to create the same DAG
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
#>  1 v     v      7.25  17.2 ->        directed   z1     7.59  18.5
#>  2 v     v      7.25  17.2 ->        directed   z2     8.57  16.8
#>  3 w1    w1     9.03  18.7 ->        directed   x      8.35  19.3
#>  4 w1    w1     9.03  18.7 ->        directed   y      8.93  18.0
#>  5 w1    w1     9.03  18.7 ->        directed   z1     7.59  18.5
#>  6 w1    w1     9.03  18.7 <->       bidirected w2     9.68  17.5
#>  7 w2    w2     9.68  17.5 ->        directed   y      8.93  18.0
#>  8 w2    w2     9.68  17.5 ->        directed   z2     8.57  16.8
#>  9 x     x      8.35  19.3 ->        directed   y      8.93  18.0
#> 10 z1    z1     7.59  18.5 ->        directed   x      8.35  19.3
#> 11 z2    z2     8.57  16.8 ->        directed   y      8.93  18.0
#> 12 y     y      8.93  18.0 <NA>      <NA>       <NA>  NA     NA  
#> # ... with 3 more variables: .ggraph.orig_index <int>, circular <lgl>,
#> #   .ggraph.index <int>
```

`ggdag` also provides functionality for analyzing DAGs and plotting them in `ggplot2`:

``` r
ggdag(tidy_ggdag)
```

<img src="man/figures/README-ggdag-1.png" width="100%" />

``` r
ggdag_adjustment_set(tidy_ggdag)
```

<img src="man/figures/README-ggdag-2.png" width="100%" />

As well as geoms and for plotting them directly in `ggplot2`:

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

And common structures of bias:

``` r
ggdag_equivalent_dags(confounder_triangle())
```

<img src="man/figures/README-ggdag_common-1.png" width="100%" />

``` r

ggdag_butterfly_bias()
```

<img src="man/figures/README-ggdag_common-2.png" width="100%" />
