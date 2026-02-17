# Activate paths opened by stratifying on a collider

Stratifying on colliders can open biasing pathways between variables.
`activate_collider_paths` activates any such pathways given a variable
or set of variables to adjust for and adds them to the `tidy_dagitty`.

## Usage

``` r
activate_collider_paths(.tdy_dag, adjust_for, ...)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- adjust_for:

  a character vector, the variable(s) to adjust for.

- ...:

  additional arguments passed to
  [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)

## Value

a `tidy_dagitty` with additional rows for collider-activated pathways

## See also

[`control_for()`](https://r-causal.github.io/ggdag/reference/control_for.md),
[`ggdag_adjust()`](https://r-causal.github.io/ggdag/reference/control_for.md),
[`geom_dag_collider_edges()`](https://r-causal.github.io/ggdag/reference/geom_dag_collider_edges.md)

## Examples

``` r
dag <- dagify(m ~ x + y, x ~ y)

collided_dag <- activate_collider_paths(dag, adjust_for = "m")
collided_dag
#> # DAG:
#> # A `dagitty` DAG with: 3 nodes and 3 edges
#> # Paths opened by conditioning on a collider: x <-> y, x <-> y
#> #
#> # Data:
#> # A tibble: 6 × 8
#>   name          x      y direction to         xend   yend collider_line
#>   <chr>     <dbl>  <dbl> <fct>     <chr>     <dbl>  <dbl> <lgl>        
#> 1 m      5.01e- 1  0.289 NA        NA    NA        NA     FALSE        
#> 2 x     -5.01e- 1  0.289 ->        m      5.01e- 1  0.289 FALSE        
#> 3 y     -5.55e-10 -0.578 ->        m      5.01e- 1  0.289 FALSE        
#> 4 y     -5.55e-10 -0.578 ->        x     -5.01e- 1  0.289 FALSE        
#> 5 x     -5.01e- 1  0.289 <->       y     -5.55e-10 -0.578 TRUE         
#> 6 x     -5.01e- 1  0.289 <->       y     -5.55e-10 -0.578 TRUE         
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame
```
