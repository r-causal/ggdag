# Canonicalize a DAG

Takes an input graph with bidirected edges and replaces every bidirected
edge x \<-\> y with a substructure x \<- L -\> y, where L is a latent
variable. See
[`dagitty::canonicalize()`](https://rdrr.io/pkg/dagitty/man/canonicalize.html)
for details. Undirected edges are not currently supported in `ggdag`.

## Usage

``` r
node_canonical(.dag, ...)

ggdag_canonical(
  .tdy_dag,
  ...,
  edge_type = ggdag_option("edge_type", "link_arc"),
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", text_col),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", NULL),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  label = NULL,
  text = NULL,
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

## Value

a `tidy_dagitty` that includes L or a `ggplot`

## Examples

``` r
dag <- dagify(y ~ x + z, x ~ ~z)

ggdag(dag)


node_canonical(dag)
#> # DAG:
#> # A `dagitty` DAG with: 4 nodes and 4 edges
#> # Latent Variable: L1
#> #
#> # Data:
#> # A tibble: 5 × 7
#>   name          x             y direction to         xend          yend
#>   <chr>     <dbl>         <dbl> <fct>     <chr>     <dbl>         <dbl>
#> 1 L1    -0.000782  0.803        ->        x     -0.817     0.0000000178
#> 2 L1    -0.000782  0.803        ->        z      0.818    -0.0000000183
#> 3 x     -0.817     0.0000000178 ->        y     -0.000782 -0.803       
#> 4 y     -0.000782 -0.803        NA        NA    NA        NA           
#> 5 z      0.818    -0.0000000183 ->        y     -0.000782 -0.803       
#> #
#> # ℹ Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame
ggdag_canonical(dag)

```
