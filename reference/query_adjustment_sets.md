# Query Adjustment Sets

Find adjustment sets that close backdoor paths between exposure and
outcome. Unlike
[`dag_adjustment_sets()`](https://r-causal.github.io/ggdag/reference/adjustment_sets.md),
this function returns a tibble with the adjustment sets as list columns
rather than a tidy_dagitty object.

## Usage

``` r
query_adjustment_sets(
  .tdy_dag,
  exposure = NULL,
  outcome = NULL,
  type = c("minimal", "canonical", "all"),
  effect = c("total", "direct"),
  max.results = Inf
)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- exposure:

  A character vector of exposure variable names. If NULL, uses the
  exposure defined in the DAG.

- outcome:

  A character vector of outcome variable names. If NULL, uses the
  outcome defined in the DAG.

- type:

  Character string specifying the type of adjustment sets to find.
  Options are "minimal" (default), "canonical", or "all".

- effect:

  Character string specifying the effect type. Options are "total"
  (default) or "direct".

- max.results:

  Maximum number of adjustment sets to return. Default is Inf.

## Value

A tibble with columns:

- `set_id`: Integer identifier for each adjustment set

- `type`: Type of adjustment set (minimal, canonical, or all)

- `effect`: Effect type (total or direct)

- `set`: String representation of the adjustment set (e.g., "{a, b, c}")

- `variables`: List column containing the variables in each set

## Examples

``` r
library(ggdag)
dag <- dagify(
  y ~ x + z,
  x ~ z,
  exposure = "x",
  outcome = "y"
)

query_adjustment_sets(dag)
#> # A tibble: 1 × 5
#>   set_id type    effect set   variables   
#>    <int> <chr>   <chr>  <chr> <named list>
#> 1      1 minimal total  {z}   <chr [1]>   
```
