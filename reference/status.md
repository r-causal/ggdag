# Find variable status

Detects variable status given a DAG (exposure, outcome, latent). See
[`dagitty::VariableStatus()`](https://rdrr.io/pkg/dagitty/man/VariableStatus.html)
for details.

## Usage

``` r
node_status(.dag, as_factor = TRUE, ...)

ggdag_status(
  .tdy_dag,
  ...,
  size = 1,
  edge_type = c("link_arc", "link", "arc", "diagonal"),
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

- as_factor:

  treat `status` variable as factor

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

a `tidy_dagitty` with a `status` column for variable status or a
`ggplot`

## Details

`node_collider` tags variable status and `ggdag_collider` plots all
variable statuses.

## Examples

``` r
dag <- dagify(
  l ~ x + y,
  y ~ x,
  exposure = "x",
  outcome = "y",
  latent = "l"
)

node_status(dag)
#> # DAG:
#> # A `dagitty` DAG with: 3 nodes and 3 edges
#> # Exposure: x
#> # Outcome: y
#> # Latent Variable: l
#> #
#> # Data:
#> # A tibble: 4 × 8
#>   name          x      y direction to         xend   yend status  
#>   <chr>     <dbl>  <dbl> <fct>     <chr>     <dbl>  <dbl> <fct>   
#> 1 l     -5.00e- 1  0.288 NA        NA    NA        NA     latent  
#> 2 x      5.00e- 1  0.288 ->        l     -5.00e- 1  0.288 exposure
#> 3 x      5.00e- 1  0.288 ->        y      1.28e-10 -0.577 exposure
#> 4 y      1.28e-10 -0.577 ->        l     -5.00e- 1  0.288 outcome 
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame
ggdag_status(dag)

```
