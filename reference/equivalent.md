# Generating Equivalent Models

Analyze the Markov equivalence class of an input DAG: the DAGs that
encode the same conditional independencies as the input graph. See
[`dagitty::equivalentDAGs()`](https://rdrr.io/pkg/dagitty/man/EquivalentModels.html)
and
[`dagitty::equivalenceClass()`](https://rdrr.io/pkg/dagitty/man/EquivalentModels.html)
for details. `node_equivalent_dags()` returns a set of DAGs, while
`node_equivalent_class()` tags reversable edges.
`ggdag_equivalent_dags()` plots all equivalent DAGs, while
`ggdag_equivalent_class()` plots all reversable edges as undirected.

## Usage

``` r
node_equivalent_dags(
  .dag,
  n = 100,
  layout = ggdag_option("layout", "nicely"),
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
  edge_engine = ggdag_option("edge_engine", "ggraph"),
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
)

node_equivalent_class(.dag, layout = ggdag_option("layout", "nicely"), ...)

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
  edge_engine = ggdag_option("edge_engine", "ggraph"),
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
  a single key, and hides the separate edge legend. This creates a
  single, more compact legend. Default is `TRUE`.

- key_glyph:

  A function to use for drawing the legend key glyph for nodes. If
  `NULL` (the default), the glyph is chosen automatically based on the
  `unified_legend` setting. When provided, this overrides the automatic
  selection. Common options include `draw_key_dag_point`,
  `draw_key_dag_combined`, and `draw_key_dag_collider`.

- edge_engine:

  The engine used to draw edges. Either `"ggraph"` (default) or
  `"ggarrow"`. When `"ggarrow"`, edges are drawn using
  [ggarrow](https://teunbrand.github.io/ggarrow/reference/ggarrow-package.html)
  geoms, which support additional customization via the `arrow_head`,
  `arrow_fins`, `arrow_mid`, and `curvature` global options (see
  [`ggdag_options_set()`](https://r-causal.github.io/ggdag/reference/ggdag_options.md)).

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

## Details

`node_equivalent_dags()` restores columns that the input `tidy_dagitty`
carries beyond the standard ones, such as `label` or `status`, by
joining them back on node name. Only node-level columns survive: a
column whose value varies across the edges of a node cannot be matched
to the edges of the equivalent DAGs, so the value of its first edge is
used for every row of that node.

## Edge layers of the composite plotters

The plotters that color or fade edges by an analysis column build their
edge layers themselves, and which layers they build is settled from the
DAG they are called with: a DAG with no bidirected edge is given no
bidirected edge layer. Replacing the data of the returned plot
afterwards, with ggplot2's `%+%`, does not bring a layer back, so a plot
built for one DAG is not a template for another.

## Examples

``` r
g_ex <- dagify(y ~ x + z, x ~ z)

g_ex |> node_equivalent_class()
#> # DAG:
#> # A `dagitty` DAG with: 3 nodes and 3 edges
#> #
#> # Data:
#> # A tibble: 4 × 8
#>   name          x      y direction to      xend   yend reversable
#>   <chr>     <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl> <lgl>     
#> 1 x     -4.99e- 1 -0.288 ->        y      0.499 -0.288 TRUE      
#> 2 y      4.99e- 1 -0.288 NA        NA    NA     NA     FALSE     
#> 3 z      9.70e-11  0.576 ->        x     -0.499 -0.288 TRUE      
#> 4 z      9.70e-11  0.576 ->        y      0.499 -0.288 TRUE      
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

g_ex |> ggdag_equivalent_dags()

```
