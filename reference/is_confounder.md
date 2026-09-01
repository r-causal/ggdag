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

## Details

A confounder is a common cause of `x` and `y`. `z` therefore has to
reach `x` by a directed path that does not run through `y`, and reach
`y` by a directed path that does not run through `x`. Being a descendant
of `z` is not enough: descent is transitive through `x`, so every
upstream cause of the exposure, such as an instrument, would qualify
even though it opens no backdoor path.

## Examples

``` r
dag <- dagify(y ~ z, x ~ z)

is_confounder(dag, "z", "x", "y")
#> [1] TRUE
is_confounder(dag, "x", "z", "y")
#> [1] FALSE
```
