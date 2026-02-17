# Test DAG properties

These functions test various properties of DAGs:

- `is_acyclic()` tests whether a DAG is acyclic

- `is_adjustment_set()` tests whether a set of variables is a valid
  adjustment set

- `is_d_separated()` tests whether two sets of variables are d-separated

- `is_d_connected()` tests whether two sets of variables are d-connected

## Usage

``` r
is_acyclic(.dag)

is_adjustment_set(.dag, Z, exposure = NULL, outcome = NULL)

is_d_separated(.dag, from = NULL, to = NULL, controlling_for = NULL)

is_d_connected(.dag, from = NULL, to = NULL, controlling_for = NULL)
```

## Arguments

- .dag:

  A `tidy_dagitty` or `dagitty` object

- Z:

  A set of variables to test or condition on. This can be a character
  vector of variable names, a list of the form `list(c(...))`, or
  `NULL`.

- exposure:

  A character vector, the exposure variable. Default is `NULL`, in which
  case it will be determined from the DAG.

- outcome:

  A character vector, the outcome variable. Default is `NULL`, in which
  case it will be determined from the DAG.

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

## Value

A logical value indicating whether the tested property holds

## Examples

``` r
dag <- dagify(
  y ~ x + z,
  x ~ z,
  exposure = "x",
  outcome = "y"
)

is_acyclic(dag)
#> [1] TRUE
is_adjustment_set(dag, "z")
#> [1] TRUE
is_d_separated(dag, "x", "y", "z")
#> [1] FALSE
is_d_connected(dag, "x", "y")
#> [1] TRUE
```
