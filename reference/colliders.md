# Find colliders

Detects any colliders given a DAG. `node_collider` tags colliders and
`ggdag_collider` plots all exogenous variables.

## Usage

``` r
node_collider(.dag, as_factor = TRUE, ...)

ggdag_collider(
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

- .dag:

  A `tidy_dagitty` or `dagitty` object

- as_factor:

  Logical. Should the column be a factor?

- ...:

  additional arguments passed to
  [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

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

a `tidy_dagitty` with a `collider` column for colliders or a `ggplot`

## Examples

``` r
dag <- dagify(m ~ x + y, y ~ x)

node_collider(dag)
#> # DAG:
#> # A `dagitty` DAG with: 3 nodes and 3 edges
#> #
#> # Data:
#> # A tibble: 4 × 8
#>   name          x      y direction to         xend   yend colliders   
#>   <chr>     <dbl>  <dbl> <fct>     <chr>     <dbl>  <dbl> <fct>       
#> 1 m      4.98e- 1 -0.287 NA        NA    NA        NA     Collider    
#> 2 x     -4.98e- 1 -0.287 ->        m      4.98e- 1 -0.287 Non-Collider
#> 3 x     -4.98e- 1 -0.287 ->        y      3.90e-10  0.575 Non-Collider
#> 4 y      3.90e-10  0.575 ->        m      4.98e- 1 -0.287 Non-Collider
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame
ggdag_collider(dag)

```
