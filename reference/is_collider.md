# Detecting colliders in DAGs

Detecting colliders in DAGs

## Usage

``` r
is_collider(.dag, .var, downstream = TRUE)

is_downstream_collider(.dag, .var)
```

## Arguments

- .dag:

  an input graph, an object of class `tidy_dagitty` or `dagitty`

- .var:

  a character vector of length 1, the potential collider to check

- downstream:

  Logical. Check for downstream colliders? Default is `TRUE`.

## Value

Logical. Is the variable a collider or downstream collider?

## Examples

``` r
dag <- dagify(m ~ x + y, m_jr ~ m)
is_collider(dag, "m")
#> [1] TRUE
is_downstream_collider(dag, "m_jr")
#> [1] TRUE

#  a downstream collider is also treated as a collider
is_collider(dag, "m_jr")
#> [1] TRUE

#  but a direct collider is not treated as a downstream collider
is_downstream_collider(dag, "m")
#> [1] FALSE
```
