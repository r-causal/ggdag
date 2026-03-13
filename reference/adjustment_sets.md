# Covariate Adjustment Sets

See
[`dagitty::adjustmentSets()`](https://rdrr.io/pkg/dagitty/man/adjustmentSets.html)
for details.

## Usage

``` r
dag_adjustment_sets(.tdy_dag, exposure = NULL, outcome = NULL, ...)

ggdag_adjustment_set(
  .tdy_dag,
  exposure = NULL,
  outcome = NULL,
  ...,
  shadow = TRUE,
  size = 1,
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option_proportional("edge_cap", 8, 10),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated(),
  expand_x = expansion(c(0.25, 0.25)),
  expand_y = expansion(c(0.2, 0.2))
)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- exposure:

  A character vector, the exposure variable. Default is `NULL`, in which
  case it will be determined from the DAG.

- outcome:

  A character vector, the outcome variable. Default is `NULL`, in which
  case it will be determined from the DAG.

- ...:

  additional arguments to `adjustmentSets`

- shadow:

  logical. Show paths blocked by adjustment?

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

- expand_x, expand_y:

  Vector of range expansion constants used to add some padding around
  the data, to ensure that they are placed some distance away from the
  axes. Use the convenience function
  [`ggplot2::expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  to generate the values for the expand argument.

## Value

a `tidy_dagitty` with an `adjusted` column and `set` column, indicating
adjustment status and DAG ID, respectively, for the adjustment sets or a
`ggplot`

## Examples

``` r
dag <- dagify(
  y ~ x + z2 + w2 + w1,
  x ~ z1 + w1,
  z1 ~ w1 + v,
  z2 ~ w2 + v,
  w1 ~ ~w2,
  exposure = "x",
  outcome = "y"
)

tidy_dagitty(dag) |> dag_adjustment_sets()
#> # DAG:
#> # A `dagitty` DAG with: 7 nodes and 11 edges
#> # Exposure: x
#> # Outcome: y
#> # Adjustment sets: 3 sets: {w1, w2, z2}, {v, w1}, {w1, z1}
#> #
#> # Data:
#> # A tibble: 36 × 9
#>    name        x       y direction to       xend    yend adjusted   set         
#>    <chr>   <dbl>   <dbl> <fct>     <chr>   <dbl>   <dbl> <chr>      <chr>       
#>  1 v     -0.631   1.36   ->        z1     0.659   0.796  unadjusted {w1, w2, z2}
#>  2 v     -0.631   1.36   ->        z2    -1.17    0.104  unadjusted {w1, w2, z2}
#>  3 w1     0.633  -0.658  ->        x      1.29   -0.0707 adjusted   {w1, w2, z2}
#>  4 w1     0.633  -0.658  ->        y     -0.0837 -0.441  adjusted   {w1, w2, z2}
#>  5 w1     0.633  -0.658  ->        z1     0.659   0.796  adjusted   {w1, w2, z2}
#>  6 w1     0.633  -0.658  <->       w2    -0.700  -1.09   adjusted   {w1, w2, z2}
#>  7 w2    -0.700  -1.09   ->        y     -0.0837 -0.441  adjusted   {w1, w2, z2}
#>  8 w2    -0.700  -1.09   ->        z2    -1.17    0.104  adjusted   {w1, w2, z2}
#>  9 x      1.29   -0.0707 ->        y     -0.0837 -0.441  unadjusted {w1, w2, z2}
#> 10 y     -0.0837 -0.441  NA        NA    NA      NA      unadjusted {w1, w2, z2}
#> # ℹ 26 more rows
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

ggdag_adjustment_set(dag)


ggdag_adjustment_set(
  dagitty::randomDAG(10, .5),
  exposure = "x3",
  outcome = "x5"
)

```
