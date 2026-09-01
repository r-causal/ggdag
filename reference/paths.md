# Find Open Paths Between Variables

`dag_paths` finds open paths between a given exposure and outcome.
`ggdag_paths` and `ggdag_paths_fan` plot all open paths. See
[`dagitty::paths()`](https://rdrr.io/pkg/dagitty/man/paths.html) for
details.

## Usage

``` r
dag_paths(
  .dag,
  from = NULL,
  to = NULL,
  adjust_for = NULL,
  limit = 100,
  directed = FALSE,
  paths_only = FALSE,
  ...
)

ggdag_paths(
  .tdy_dag,
  from = NULL,
  to = NULL,
  adjust_for = NULL,
  limit = 100,
  directed = FALSE,
  shadow = TRUE,
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
  edge_engine = ggdag_option("edge_engine", "ggraph"),
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
)

ggdag_paths_fan(
  .tdy_dag,
  from = NULL,
  to = NULL,
  adjust_for = NULL,
  limit = 100,
  directed = FALSE,
  ...,
  shadow = TRUE,
  spread = 0.7,
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

- limit:

  maximum amount of paths to show. In general, the number of paths grows
  exponentially with the number of variables in the graph, such that
  path inspection is not useful except for the most simple models.

- directed:

  logical. Should only directed paths be shown?

- paths_only:

  logical. Should only open paths be returned? Default is `FALSE`, which
  includes every variable and edge in the DAG regardless if they are
  part of the path.

- ...:

  additional arguments passed to
  [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- shadow:

  logical. Show edges which are not on an open path?

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

- spread:

  the width of the fan spread

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

## Value

a `tidy_dagitty` with a `path` column for path variables, a `set`
grouping column, and a `path_type` column classifying paths as "direct"
(a directed causal path), "backdoor" (a path whose first edge points
into the exposure), or "other" (any other path, such as one through a
collider), or a `ggplot`.

## Edge layers of the composite plotters

The plotters that color or fade edges by an analysis column build their
edge layers themselves, and which layers they build is settled from the
DAG they are called with: a DAG with no bidirected edge is given no
bidirected edge layer. Replacing the data of the returned plot
afterwards, with ggplot2's `%+%`, does not bring a layer back, so a plot
built for one DAG is not a template for another.

## Examples

``` r
confounder_triangle(x_y_associated = TRUE) |>
  dag_paths(from = "x", to = "y")
#> # DAG:
#> # A `dagitty` DAG with: 3 nodes and 3 edges
#> # Exposure: x
#> # Outcome: y
#> # Paths: 2 open paths: {x -> y}, {x <- z -> y}
#> #
#> # Data:
#> # A tibble: 9 × 10
#>   set   name      x     y direction to     xend  yend path      path_type
#>   <chr> <chr> <int> <int> <fct>     <chr> <int> <int> <chr>     <chr>    
#> 1 1     x         0     0 ->        y         2     0 open path direct   
#> 2 1     y         2     0 NA        NA       NA    NA open path direct   
#> 3 1     z         1     1 ->        x         0     0 NA        NA       
#> 4 1     z         1     1 ->        y         2     0 NA        NA       
#> 5 2     x         0     0 ->        y         2     0 NA        NA       
#> 6 2     y         2     0 NA        NA       NA    NA open path backdoor 
#> 7 2     z         1     1 ->        x         0     0 open path backdoor 
#> 8 2     z         1     1 ->        y         2     0 open path backdoor 
#> 9 2     x         0     0 NA        NA       NA    NA open path backdoor 
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

confounder_triangle(x_y_associated = TRUE) |>
  ggdag_paths(from = "x", to = "y")


butterfly_bias(x_y_associated = TRUE) |>
  ggdag_paths_fan(shadow = TRUE)

```
