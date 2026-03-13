# Find Exogenous Variables

`node_exogenous` tags exogenous variables given an exposure and outcome.
`ggdag_exogenous` plots all exogenous variables. See
[`dagitty::exogenousVariables()`](https://rdrr.io/pkg/dagitty/man/exogenousVariables.html)
for details.

## Usage

``` r
node_exogenous(.dag, ...)

ggdag_exogenous(
  .tdy_dag,
  ...,
  size = 1,
  edge_type = c("link_arc", "link", "arc", "diagonal"),
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option("edge_cap", 8),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  unified_legend = TRUE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
)
```

## Arguments

- .dag, .tdy_dag:

  input graph, an object of class `tidy_dagitty` or `dagitty`

- ...:

  additional arguments passed to
  [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)

- size:

  A numeric value scaling the size of all elements in the DAG. This
  allows you to change the scale of the DAG without changing the
  proportions.

- edge_type:

  The type of edge, one of "link_arc", "link", "arc", "diagonal".

- node_size:

  The size of the nodes.

- text_size:

  The size of the text.

- label_size:

  The size of the labels.

- text_col:

  The color of the text.

- label_col:

  The color of the labels.

- edge_width:

  The width of the edges.

- edge_cap:

  The size of edge caps (the distance between the arrowheads and the
  node borders).

- arrow_length:

  The length of arrows on edges.

- use_edges:

  A logical value. Include a `geom_dag_edges*()` function? If `TRUE`,
  which is determined by `edge_type`.

- use_nodes:

  A logical value. Include
  [`geom_dag_point()`](https://r-causal.github.io/ggdag/reference/node_point.md)?

- use_stylized:

  A logical value. Include
  [`geom_dag_node()`](https://r-causal.github.io/ggdag/reference/node_point.md)?

- use_text:

  A logical value. Include
  [`geom_dag_text()`](https://r-causal.github.io/ggdag/reference/geom_dag_text.md)?

- use_labels:

  A logical value. Include a label geom? The specific geom used is
  controlled by `label_geom`.

- label_geom:

  A geom function to use for drawing labels when `use_labels = TRUE`.
  Default is `geom_dag_label_repel`. Other options include
  `geom_dag_label`, `geom_dag_text_repel`, `geom_dag_label_repel2`, and
  `geom_dag_text_repel2`.

- unified_legend:

  A logical value. When `TRUE` and both `use_edges` and `use_nodes` are
  `TRUE`, creates a unified legend entry showing both nodes and edges in
  a single key, and hides the separate edge legend. This creates
  cleaner, more compact legends. Default is `TRUE`.

- text:

  The bare name of a column to use for
  [`geom_dag_text()`](https://r-causal.github.io/ggdag/reference/geom_dag_text.md).
  If `use_text = TRUE`, the default is to use `name`.

- label:

  The bare name of a column to use for labels. If `use_labels = TRUE`,
  the default is to use `label`.

- node:

  Deprecated.

- stylized:

  Deprecated.

## Value

a `tidy_dagitty` with an `exogenous` column for exogenous variables or a
`ggplot`

## Examples

``` r
dag <- dagify(y ~ x1 + x2 + x3, b ~ x1 + x2)
ggdag_exogenous(dag)

node_exogenous(dag)
#> # DAG:
#> # A `dagitty` DAG with: 5 nodes and 5 edges
#> #
#> # Data:
#> # A tibble: 7 × 8
#>   name       x         y direction to      xend      yend exogenous
#>   <chr>  <dbl>     <dbl> <fct>     <chr>  <dbl>     <dbl> <chr>    
#> 1 b      1.45   1.64e-10 NA        NA    NA     NA        NA       
#> 2 x1     0.521  7.08e- 1 ->        b      1.45   1.64e-10 exogenous
#> 3 x1     0.521  7.08e- 1 ->        y     -0.573 -1.36e-10 exogenous
#> 4 x2     0.521 -7.08e- 1 ->        b      1.45   1.64e-10 exogenous
#> 5 x2     0.521 -7.08e- 1 ->        y     -0.573 -1.36e-10 exogenous
#> 6 x3    -1.92   1.18e-10 ->        y     -0.573 -1.36e-10 exogenous
#> 7 y     -0.573 -1.36e-10 NA        NA    NA     NA        NA       
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame
```
