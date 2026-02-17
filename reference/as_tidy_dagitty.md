# Convert objects into `tidy_dagitty` objects

An alternative API and specification to
[`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md),
`as_tidy_dagitty()` allows you to create `tidy_dagitty` objects from
data frames and lists. There is also a method for `dagitty` objects,
which is a thin wrapper for
[`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md).
To create a DAG from a list, each element of the list should be a
character vector, and the order of the elements should be the time order
in which they appear in the DAG, e.g. element 1 occurs at time point 1.
To create a DAG from a data frame, it must contain `name` and `to`
columns, representing the nodes and any edges leading from the nodes. If
there are `x`, `y`, `xend`, and `yend` columns, they will be used as
coordinates. Otherwise, `layout` will be used. See
[tidy_dagitty](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)
for more information about layouts. Additionally, you can specify status
(one of `exposure`, `outcome`, or `latent`) by including a `status`
column. Any other columns in the data set will also be joined to the
`tidy_dagitty` data.

## Usage

``` r
as_tidy_dagitty(x, ...)

# S3 method for class 'dagitty'
as_tidy_dagitty(x, seed = NULL, layout = "nicely", ...)

# S3 method for class 'data.frame'
as_tidy_dagitty(
  x,
  exposure = NULL,
  outcome = NULL,
  latent = NULL,
  labels = NULL,
  coords = NULL,
  seed = NULL,
  layout = "nicely",
  saturate = FALSE,
  ...
)

# S3 method for class 'list'
as_tidy_dagitty(
  x,
  exposure = NULL,
  outcome = NULL,
  latent = NULL,
  labels = NULL,
  coords = NULL,
  seed = NULL,
  layout = "time_ordered",
  ...
)
```

## Arguments

- x:

  An object to convert into a `tidy_dagitty`. Currently supports
  `dagitty` and `data.frame` objects.

- ...:

  optional arguments passed to
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)

- seed:

  a numeric seed for reproducible layout generation

- layout:

  a layout available in `ggraph`. See
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)
  for details. Alternatively, `"time_ordered"` will use
  [`time_ordered_coords()`](https://r-causal.github.io/ggdag/reference/time_ordered_coords.md)
  to algorithmically sort the graph by time.

- exposure:

  a character vector for the exposure (must be a variable name in the
  DAG)

- outcome:

  a character vector for the outcome (must be a variable name in the
  DAG)

- latent:

  a character vector for any latent variables (must be a variable name
  in the DAG)

- labels:

  a named character vector, labels for variables in the DAG

- coords:

  coordinates for the DAG nodes. Can be a named list or a `data.frame`
  with columns x, y, and name

- saturate:

  Logical. Saturate the DAG such that there is an edge going from every
  point in the future from a given node? Setting this to `TRUE` will
  potentially lead to more edges than present in `x`.

## Value

a `tidy_dagitty` object

## See also

[`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md),
[`pull_dag()`](https://r-causal.github.io/ggdag/reference/pull_dag.md)

## Examples

``` r
data.frame(name = c("c", "c", "x"), to = c("x", "y", "y")) |>
  as_tidy_dagitty()
#> # DAG:
#> # A `dagitty` DAG with: 3 nodes and 3 edges
#> #
#> # Data:
#> # A tibble: 4 × 7
#>   name          x      y direction to         xend   yend
#>   <chr>     <dbl>  <dbl> <fct>     <chr>     <dbl>  <dbl>
#> 1 c      4.97e- 1  0.287 ->        x     -4.97e- 1  0.287
#> 2 c      4.97e- 1  0.287 ->        y     -2.05e-10 -0.574
#> 3 x     -4.97e- 1  0.287 ->        y     -2.05e-10 -0.574
#> 4 y     -2.05e-10 -0.574 NA        NA    NA        NA    
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

time_points <- list(c("a", "b", "c"), "d", c("e", "f", "g"), "z")

time_points |>
  # create a saturated, time-ordered DAG
  as_tidy_dagitty() |>
  # remove the edge from `c` to `f`
  dag_prune(c("c" = "f"))
#> # DAG:
#> # A `dagitty` DAG with: 8 nodes and 21 edges
#> #
#> # Data:
#> # A tibble: 22 × 7
#>    name      x     y direction to     xend  yend
#>    <chr> <dbl> <int> <fct>     <chr> <dbl> <int>
#>  1 a         1    -1 ->        d         2     0
#>  2 a         1    -1 ->        e         3    -1
#>  3 a         1    -1 ->        f         3     0
#>  4 a         1    -1 ->        g         3     1
#>  5 a         1    -1 ->        z         4     0
#>  6 b         1     0 ->        d         2     0
#>  7 b         1     0 ->        e         3    -1
#>  8 b         1     0 ->        f         3     0
#>  9 b         1     0 ->        g         3     1
#> 10 b         1     0 ->        z         4     0
#> # ℹ 12 more rows
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame
```
