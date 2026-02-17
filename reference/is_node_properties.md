# Test node properties

These functions test various properties of nodes in a DAG:

- `is_exogenous()` tests whether a variable is exogenous (has no
  parents)

- `is_instrumental()` tests whether a variable is instrumental

- `is_exposure()`, `is_outcome()`, `is_latent()` test variable status

## Usage

``` r
is_exogenous(.dag, .var)

is_instrumental(.dag, .var, exposure = NULL, outcome = NULL)

is_exposure(.dag, .var)

is_outcome(.dag, .var)

is_latent(.dag, .var)
```

## Arguments

- .dag:

  A `tidy_dagitty` or `dagitty` object

- .var:

  A character string specifying the variable to test

- exposure:

  A character vector, the exposure variable. Default is `NULL`, in which
  case it will be determined from the DAG.

- outcome:

  A character vector, the outcome variable. Default is `NULL`, in which
  case it will be determined from the DAG.

## Value

A logical value indicating whether the tested property holds

## Examples

``` r
dag <- dagify(
  y ~ x + z,
  x ~ z,
  exposure = "x",
  outcome = "y",
  latent = "z"
)

is_exogenous(dag, "z")
#> [1] TRUE
is_exposure(dag, "x")
#> [1] TRUE
is_outcome(dag, "y")
#> [1] TRUE
is_latent(dag, "z")
#> [1] TRUE
```
