# Convert DAGS to tidygraph

A thin wrapper to convert `tidy_dagitty` and `dagitty` objects to
`tbl_graph`, which can then be used to work in `tidygraph` and `ggraph`
directly. See
[`tidygraph::as_tbl_graph()`](https://tidygraph.data-imaginist.com/reference/tbl_graph.html).

## Usage

``` r
# S3 method for class 'tidy_dagitty'
as_tbl_graph(x, directed = TRUE, ...)

# S3 method for class 'dagitty'
as_tbl_graph(x, directed = TRUE, ...)
```

## Arguments

- x:

  an object of class `tidy_dagitty` or `dagitty`

- directed:

  logical. Should the constructed graph be directed? Default is `TRUE`

- ...:

  other arguments passed to `as_tbl_graph`

## Value

a `tbl_graph`

## Examples

``` r
library(ggraph)
library(tidygraph)
#> 
#> Attaching package: ‘tidygraph’
#> The following object is masked from ‘package:stats’:
#> 
#>     filter
butterfly_bias() |>
  as_tbl_graph() |>
  ggraph() +
  geom_edge_diagonal() +
  geom_node_point()
#> Using "sugiyama" as default layout

```
