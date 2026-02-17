# Quickly create a DAGs with common structures of bias

base functions create an object of class `dagitty`; `ggdag_* ` functions
are wrappers that also call
[`ggdag()`](https://r-causal.github.io/ggdag/reference/ggdag.md) on the
`dagitty` object.

## Usage

``` r
m_bias(
  x = NULL,
  y = NULL,
  a = NULL,
  b = NULL,
  m = NULL,
  x_y_associated = FALSE
)

butterfly_bias(
  x = NULL,
  y = NULL,
  a = NULL,
  b = NULL,
  m = NULL,
  x_y_associated = FALSE
)

confounder_triangle(x = NULL, y = NULL, z = NULL, x_y_associated = FALSE)

collider_triangle(x = NULL, y = NULL, m = NULL, x_y_associated = FALSE)

mediation_triangle(x = NULL, y = NULL, m = NULL, x_y_associated = FALSE)

quartet_collider(x = NULL, y = NULL, z = NULL, x_y_associated = TRUE)

quartet_confounder(x = NULL, y = NULL, z = NULL, x_y_associated = TRUE)

quartet_mediator(x = NULL, y = NULL, z = NULL, x_y_associated = FALSE)

quartet_m_bias(
  x = NULL,
  y = NULL,
  z = NULL,
  u1 = NULL,
  u2 = NULL,
  x_y_associated = TRUE
)

quartet_time_collider(
  x0 = NULL,
  x1 = NULL,
  x2 = NULL,
  x3 = NULL,
  y1 = NULL,
  y2 = NULL,
  y3 = NULL,
  z1 = NULL,
  z2 = NULL,
  z3 = NULL
)

ggdag_m_bias(
  x = NULL,
  y = NULL,
  a = NULL,
  b = NULL,
  m = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
)

ggdag_butterfly_bias(
  x = NULL,
  y = NULL,
  a = NULL,
  b = NULL,
  m = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
)

ggdag_confounder_triangle(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
)

ggdag_collider_triangle(
  x = NULL,
  y = NULL,
  m = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
)

ggdag_mediation_triangle(
  x = NULL,
  y = NULL,
  m = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
)

ggdag_quartet_collider(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = TRUE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  text = NULL,
  label = NULL
)

ggdag_quartet_confounder(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = TRUE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  text = NULL,
  label = NULL
)

ggdag_quartet_mediator(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  text = NULL,
  label = NULL
)

ggdag_quartet_m_bias(
  x = NULL,
  y = NULL,
  z = NULL,
  u1 = NULL,
  u2 = NULL,
  x_y_associated = TRUE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  text = NULL,
  label = NULL
)

ggdag_quartet_time_collider(
  x0 = NULL,
  x1 = NULL,
  x2 = NULL,
  x3 = NULL,
  y1 = NULL,
  y2 = NULL,
  y3 = NULL,
  z1 = NULL,
  z2 = NULL,
  z3 = NULL,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  text = NULL,
  label = NULL
)
```

## Arguments

- x, y, a, b, m, z:

  Character vector. Optional label. Default is `NULL`

- x_y_associated:

  Logical. Are x and y associated? Default is `FALSE`.

- u1, u2:

  Character vector. Optional label for unmeasured nodes, used in
  `quartet_m_bias()`. Default is `NULL`

- x0, x1, x2, x3, y1, y2, y3, z1, z2, z3:

  Character vector. Optional labels for time-indexed nodes, used in
  `quartet_time_collider()`. Default is `NULL`

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

a DAG of class `dagitty` or a `ggplot`

## Details

The `quartet_*` functions create DAGs that represent the causal quartet,
which are four example datasets with identical statistical properties
but different causal structures. These are inspired by Anscombe's
quartet and demonstrate that statistical summaries alone cannot
determine causal relationships. See [Causal Inference in
R](https://www.r-causal.org/chapters/05-not-just-a-stats-problem)

The four structures represent different relationships between exposure
(x), outcome (y), and a covariate (z):

- Collider: z is caused by both x and y (should not adjust for z)

- Confounder: z causes both x and y (must adjust for z)

- Mediator: z is on the causal path from x to y (adjust for direct
  effect only)

- M-bias: z is a collider with unmeasured confounders u1 and u2 (should
  not adjust for z)

The time-varying collider (`quartet_time_collider()`) demonstrates how
time-ordering can help identify causal relationships when variables are
measured at multiple time points.

## References

D'Agostino McGowan L, Gerke T, Barrett M (2023). "Causal inference is
not just a statistics problem." Journal of Statistics and Data Science
Education, 32(1), 1-4.
[doi:10.1080/26939169.2023.2276446](https://doi.org/10.1080/26939169.2023.2276446)

## Examples

``` r
m_bias() |> ggdag_adjust("m")

ggdag_confounder_triangle()


# Causal Quartets
ggdag_quartet_collider()

ggdag_quartet_confounder()

ggdag_quartet_mediator()

ggdag_quartet_m_bias()


# Time-varying collider
ggdag_quartet_time_collider()

```
