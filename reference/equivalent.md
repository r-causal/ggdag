# Generating Equivalent Models

Returns a set of complete partially directed acyclic graphs (CPDAGs)
given an input DAG. CPDAGs are Markov equivalent to the input graph. See
[`dagitty::equivalentDAGs()`](https://rdrr.io/pkg/dagitty/man/EquivalentModels.html)
for details. `node_equivalent_dags()` returns a set of DAGs, while
`node_equivalent_class()` tags reversable edges.
`ggdag_equivalent_dags()` plots all equivalent DAGs, while
`ggdag_equivalent_class()` plots all reversable edges as undirected.

## Usage

``` r
node_equivalent_dags(
  .dag,
  n = 100,
  layout = ggdag_option("layout", "auto"),
  ...
)

ggdag_equivalent_dags(
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
  key_glyph = NULL,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
)

node_equivalent_class(.dag, layout = ggdag_option("layout", "auto"))

ggdag_equivalent_class(
  .tdy_dag,
  ...,
  size = 1,
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
  key_glyph = NULL,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
)
```

## Arguments

- .dag:

  input graph, an object of class `tidy_dagitty` or `dagitty`

- n:

  maximal number of returned graphs.

- layout:

  a layout available in `ggraph`. See
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)
  for details. Alternatively, `"time_ordered"` will use
  [`time_ordered_coords()`](https://r-causal.github.io/ggdag/reference/time_ordered_coords.md)
  to algorithmically sort the graph by time. You can also pass the
  result of
  [`time_ordered_coords()`](https://r-causal.github.io/ggdag/reference/time_ordered_coords.md)
  directly: either the function returned when called with no arguments,
  or the coordinate tibble returned when called with arguments.

- ...:

  optional arguments passed to
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)

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

- key_glyph:

  A function to use for drawing the legend key glyph for nodes. If
  `NULL` (the default), the glyph is chosen automatically based on the
  `unified_legend` setting. When provided, this overrides the automatic
  selection. Common options include `draw_key_dag_point`,
  `draw_key_dag_combined`, and `draw_key_dag_collider`.

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

a `tidy_dagitty` with at least one DAG, including a `dag` column to
identify graph set for equivalent DAGs or a `reversable` column for
equivalent classes, or a `ggplot`

## Examples

``` r
g_ex <- dagify(y ~ x + z, x ~ z)

g_ex |> node_equivalent_class()
#> # DAG:
#> # A `dagitty` DAG with: 3 nodes and 3 edges
#> #
#> # Data:
#> # A tibble: 4 × 8
#>   name      x     y direction to     xend  yend reversable
#>   <chr> <dbl> <dbl> <fct>     <chr> <dbl> <dbl> <lgl>     
#> 1 x       0       2 ->        y       0.5     1 TRUE      
#> 2 y       0.5     1 NA        NA     NA      NA FALSE     
#> 3 z       0.5     3 ->        x       0       2 TRUE      
#> 4 z       0.5     3 ->        y       0.5     1 TRUE      
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

g_ex |> ggdag_equivalent_dags()

```
