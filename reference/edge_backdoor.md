# Classify DAG edges as backdoor or direct

`edge_backdoor()` identifies edges as being on backdoor paths or direct
causal paths between an exposure and outcome. This function adds
edge-level information to the tidy DAG object, classifying each edge
based on the types of paths it appears on.

## Usage

``` r
edge_backdoor(
  .dag,
  from = NULL,
  to = NULL,
  adjust_for = NULL,
  open_only = TRUE,
  ...
)
```

## Arguments

- .dag:

  A `tidy_dagitty` or `dagitty` object

- from:

  A character vector with starting node name(s), or `NULL`. If `NULL`,
  checks DAG for exposure variable.

- to:

  A character vector with ending node name(s), or `NULL`. If `NULL`,
  checks DAG for outcome variable.

- adjust_for:

  character vector, a set of variables to control for. Default is
  `NULL`.

- open_only:

  logical. If `TRUE` (default), only considers open paths. If `FALSE`,
  includes information about closed paths as well.

- ...:

  additional arguments passed to
  [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)

## Value

A `tidy_dagitty` object with additional columns:

- `path_type`: "backdoor", "direct", or "both" classification for each
  edge

- `open`: logical indicating if the edge is part of an open path

## Details

Edges are classified by examining the paths between exposure and
outcome:

- Direct edges appear only on directed causal paths

- Backdoor edges appear only on backdoor paths

- Both edges appear on both direct and backdoor paths

When `open_only = TRUE` (default), `path_type` will be NA for edges that
are only part of closed paths.

## Examples

``` r
# Create a DAG with both direct and backdoor paths
dag <- dagify(
  y ~ x + z,
  x ~ z,
  exposure = "x",
  outcome = "y"
)

# Classify edges
edge_backdoor(dag)
#> # DAG:
#> # A `dagitty` DAG with: 3 nodes and 3 edges
#> # Exposure: x
#> # Outcome: y
#> #
#> # Data:
#> # A tibble: 4 × 9
#>   name          x      y direction to      xend   yend path_type open 
#>   <chr>     <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl> <chr>     <lgl>
#> 1 x      5.01e- 1  0.289 ->        y     -0.501  0.289 direct    TRUE 
#> 2 y     -5.01e- 1  0.289 NA        NA    NA     NA     NA        NA   
#> 3 z     -9.60e-11 -0.579 ->        x      0.501  0.289 backdoor  TRUE 
#> 4 z     -9.60e-11 -0.579 ->        y     -0.501  0.289 backdoor  TRUE 
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

# Include closed paths
edge_backdoor(dag, open_only = FALSE)
#> # DAG:
#> # A `dagitty` DAG with: 3 nodes and 3 edges
#> # Exposure: x
#> # Outcome: y
#> #
#> # Data:
#> # A tibble: 4 × 9
#>   name          x      y direction to      xend   yend path_type open 
#>   <chr>     <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl> <chr>     <lgl>
#> 1 x      5.02e- 1  0.290 ->        y     -0.502  0.290 direct    TRUE 
#> 2 y     -5.02e- 1  0.290 NA        NA    NA     NA     NA        NA   
#> 3 z      1.13e-10 -0.580 ->        x      0.502  0.290 backdoor  TRUE 
#> 4 z      1.13e-10 -0.580 ->        y     -0.502  0.290 backdoor  TRUE 
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame
```
