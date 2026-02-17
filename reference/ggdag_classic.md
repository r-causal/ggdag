# Quickly plot a DAG in ggplot2

`ggdag_classic()` is a wrapper to quickly plot DAGs in a more
traditional style.

## Usage

``` r
ggdag_classic(
  .tdy_dag,
  ...,
  size = 8,
  label_rect_size = NULL,
  text_label = "name",
  text_col = "black",
  use_edges = TRUE
)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- ...:

  additional arguments passed to
  [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)

- size:

  text size, with a default of 8.

- label_rect_size:

  specify the `fontsize` argument in
  [`ggraph::label_rect`](https://ggraph.data-imaginist.com/reference/geometry.html);
  default is `NULL`, in which case it is scaled relative ti `size`

- text_label:

  text variable, with a default of "name"

- text_col:

  text color, with a default of "black"

- use_edges:

  logical value whether to include edges

## Value

a `ggplot`

## See also

[`ggdag()`](https://r-causal.github.io/ggdag/reference/ggdag.md)

## Examples

``` r
dag <- dagify(
  y ~ x + z2 + w2 + w1,
  x ~ z1 + w1,
  z1 ~ w1 + v,
  z2 ~ w2 + v,
  w1 ~ ~w2
)

ggdag_classic(dag)

ggdag_classic(dag) + theme_dag_blank()


ggdag_classic(dagitty::randomDAG(5, .5))

```
