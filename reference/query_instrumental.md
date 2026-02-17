# Query Instrumental Variables

Identify instrumental variables for a given exposure-outcome pair.

## Usage

``` r
query_instrumental(
  .tdy_dag,
  exposure = NULL,
  outcome = NULL,
  conditioned_on = NULL
)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- exposure:

  Character vector of exposure variable names. If NULL, uses the
  exposure defined in the DAG.

- outcome:

  Character vector of outcome variable names. If NULL, uses the outcome
  defined in the DAG.

- conditioned_on:

  Character vector of variables that must be conditioned on.

## Value

A tibble with columns:

- `instrument`: The instrumental variable

- `exposure`: The exposure variable

- `outcome`: The outcome variable

- `conditioning_set`: String representation of conditioning variables

- `conditioned_on`: List column of required conditioning variables

## Examples

``` r
library(ggdag)
dag <- dagify(
  y ~ x + u,
  x ~ z + u,
  exposure = "x",
  outcome = "y",
  latent = "u"
)

query_instrumental(dag)
#> # A tibble: 1 × 5
#>   instrument exposure outcome conditioning_set conditioned_on
#>   <chr>      <chr>    <chr>   <chr>            <list>        
#> 1 z          x        y       {}               <chr [0]>     
```
