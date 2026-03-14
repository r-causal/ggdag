# Pull components from DAG objects

`pull_dag()` and `pull_dag_data()` are generic methods to pull
components of DAG objects, e.g. `tidy_dagitty`, such as the `dagitty`
object or the data frame associated with it. These methods are
recommended over extracting components manually, e.g. `my_dag$data`,
because the internal structure of these objects may change over time.
Similarly, use `update_dag()` if you want to sync the data back to the
DAG object or override it with another DAG; use `update_dag_data()` to
do update the data frame. This is useful with `pull_dag_data()`.

## Usage

``` r
pull_dag(x, ...)

# S3 method for class 'tidy_dagitty'
pull_dag(x, ...)

# S3 method for class 'dagitty'
pull_dag(x, ...)

pull_dag_data(x, ...)

# S3 method for class 'tidy_dagitty'
pull_dag_data(x, ...)

# S3 method for class 'dagitty'
pull_dag_data(x, ...)

update_dag_data(x) <- value

# S3 method for class 'tidy_dagitty'
update_dag_data(x) <- value

update_dag(x, ...)

update_dag(x) <- value

# S3 method for class 'tidy_dagitty'
update_dag(x, ...)

# S3 method for class 'tidy_dagitty'
update_dag(x) <- value
```

## Arguments

- x:

  a `tidy_dagitty` or `dagitty` object.

- ...:

  For `dagitty` objects, passed to
  [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)
  if needed, otherwise currently unused.

- value:

  a value to set, either a `dagitty` or `data.frame` object, depending
  on the function.

## Value

a DAG object, e.g. `dagitty`, or data frame

## Examples

``` r
tidy_dagitty_obj <- dagify(y ~ x + z, x ~ z) |>
  tidy_dagitty()
dag <- pull_dag(tidy_dagitty_obj)
dag_data <- pull_dag_data(tidy_dagitty_obj)

tidy_dagitty_obj |>
  dplyr::mutate(name = toupper(name)) |>
  # recreate the DAG component
  update_dag()
#> # DAG:
#> # A `dagitty` DAG with: 4 nodes and 3 edges
#> #
#> # Data:
#> # A tibble: 4 × 7
#>   name          x      y direction to      xend   yend
#>   <chr>     <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl>
#> 1 X     -5.00e- 1  0.289 ->        y      0.500  0.289
#> 2 Y      5.00e- 1  0.289 NA        NA    NA     NA    
#> 3 Z     -1.49e-10 -0.578 ->        x     -0.500  0.289
#> 4 Z     -1.49e-10 -0.578 ->        y      0.500  0.289
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

dag_data$label <- paste0(dag_data$name, "(observed)")
update_dag_data(tidy_dagitty_obj) <- dag_data
```
