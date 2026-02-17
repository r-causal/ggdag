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

  a character vector

- .tdy_dag:

  A `tidy_dagitty` or `dagitty` object

- labels:

  a character vector

## Value

`label` returns the label attribute of x

## Examples

``` r
labelled_dag <- dagify(y ~ z, x ~ z) |>
  tidy_dagitty() |>
  dag_label(labels = c("x" = "exposure", "y" = "outcome", "z" = "confounder"))

has_labels(labelled_dag)
#> [1] FALSE
```
