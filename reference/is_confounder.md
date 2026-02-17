# Assess if a variable confounds a relationship

Assess if a variable confounds a relationship

## Usage

``` r
is_confounder(.tdy_dag, z, x, y, direct = FALSE)
```

## Arguments

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- z:

  a character vector, the potential confounder

- x, y:

  a character vector, the variables z may confound.

- direct:

  logical. Only consider direct confounding? Default is `FALSE`

## Value

Logical. Is the variable a confounder?

## Examples

``` r
dag <- dagify(y ~ z, x ~ z)

is_confounder(dag, "z", "x", "y")
#> [1] TRUE
is_confounder(dag, "x", "z", "y")
#> [1] FALSE
```
