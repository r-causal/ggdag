# DAG labels

Label or otherwise retrieve labels from objects of either class
`tidy_dagitty` or `dagitty`

## Usage

``` r
label(x) <- value

# S3 method for class 'dagitty'
label(x) <- value

# S3 method for class 'tidy_dagitty'
label(x) <- value

dag_label(.tdy_dag, labels = NULL)

label(.tdy_dag)

has_labels(.tdy_dag)
```

## Arguments

- x:

  an object of either class `tidy_dagitty` or `dagitty`

- value:

  a named character vector, where the names are nodes in the DAG (a name
  that matches no node is an error), or `NULL` to remove labels

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- labels:

  a named character vector, where the names are node names, or `NULL` to
  reuse the labels already attached to the DAG

## Value

`label` returns the label attribute of x

## Examples

``` r
labelled_dag <- dagify(y ~ z, x ~ z) |>
  tidy_dagitty() |>
  dag_label(labels = c("x" = "exposure", "y" = "outcome", "z" = "confounder"))

has_labels(labelled_dag)
#> [1] TRUE
```
