# D-relationship between variables

D-separation is a key concept in causal structural models. Variables are
d-separated if there are no open paths between them. The `node_d*()`
functions label variables as d-connected or d-separated. The
`ggdag_d*()` functions plot the results. The `*_dconnected()`,
`*_dseparated()`, and `*_drelationship()` functions essentially produce
the same output and are just different ways of thinking about the
relationship. See
[`dagitty::dseparated()`](https://rdrr.io/pkg/dagitty/man/dconnected.html)
for details.

## Usage

``` r
node_dconnected(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  as_factor = TRUE,
  ...
)

node_dseparated(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  as_factor = TRUE
)

node_drelationship(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  as_factor = TRUE
)

ggdag_drelationship(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  ...,
  edge_type = "link_arc",
  size = 1,
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 10,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  unified_legend = TRUE,
  key_glyph = draw_key_dag_point,
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
)

ggdag_dseparated(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  ...,
  edge_type = "link_arc",
  size = 1,
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 10,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  unified_legend = TRUE,
  key_glyph = draw_key_dag_point,
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
)

ggdag_dconnected(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  ...,
  edge_type = "link_arc",
  size = 1,
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 10,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  unified_legend = TRUE,
  key_glyph = draw_key_dag_point,
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- from:

  A character vector with starting node name(s), or `NULL`. If `NULL`,
  checks DAG for exposure variable.

- to:

  A character vector with ending node name(s), or `NULL`. If `NULL`,
  checks DAG for outcome variable.

- controlling_for:

  A set of variables to control for. This can be a character vector of
  variable names, a list of the form `list(c(...))`, or `NULL`. When
  `NULL`, no control is applied. Default is `NULL`.

- as_factor:

  Logical. Should the column be a factor?

- ...:

  additional arguments passed to
  [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)

- edge_type:

  The type of edge, one of "link_arc", "link", "arc", "diagonal".

- size:

  A numeric value scaling the size of all elements in the DAG. This
  allows you to change the scale of the DAG without changing the
  proportions.

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

- label:

  The bare name of a column to use for labels. If `use_labels = TRUE`,
  the default is to use `label`.

- text:

  The bare name of a column to use for
  [`geom_dag_text()`](https://r-causal.github.io/ggdag/reference/geom_dag_text.md).
  If `use_text = TRUE`, the default is to use `name`.

- node:

  Deprecated.

- stylized:

  Deprecated.

- collider_lines:

  Logical. Should paths opened by conditioning on colliders be shown?

## Value

a `tidy_dagitty` with a `d_relationship` column for variable D
relationship or a `ggplot`

## Examples

``` r
library(ggplot2)
dag <- dagify(m ~ x + y)
dag |> ggdag_drelationship("x", "y")

dag |> ggdag_drelationship("x", "y", controlling_for = "m")


dag |>
  node_dseparated("x", "y") |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted,
             col = d_relationship)) +
  geom_dag_edges() +
  geom_dag_collider_edges() +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() +
  scale_adjusted(include_color = FALSE)


dag |>
  node_dconnected("x", "y", controlling_for = "m") |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted,
             col = d_relationship)) +
  geom_dag_edges() +
  geom_dag_collider_edges() +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() +
  scale_adjusted(include_color = FALSE)


dagify(m ~ x + y, m_jr ~ m) |>
  tidy_dagitty(layout = "nicely") |>
  node_dconnected("x", "y", controlling_for = "m_jr") |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted,
             col = d_relationship)) +
  geom_dag_edges() +
  geom_dag_collider_edges() +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() +
  scale_adjusted(include_color = FALSE)
```
