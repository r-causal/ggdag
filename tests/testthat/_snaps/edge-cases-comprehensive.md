# dag_label edge cases

    Code
      expr
    Condition
      Warning in `dag_label()`:
      No labels provided
    Output
      # DAG:
      # A `dagitty` DAG with: 2 nodes and 1 edge
      #
      # Data:
      # A tibble: 2 x 7
        name       x      y direction to      xend   yend
        <chr>  <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl>
      1 x      0.137  0.206 ->        y     -0.579 -0.496
      2 y     -0.579 -0.496 <NA>      <NA>  NA     NA    
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

# pull functions edge cases

    Code
      expr
    Condition <ggdag_type_error>
      Error in `pull_dag()`:
      ! `pull_dag()` requires a <tidy_dagitty> or <dagitty> object.
      x You provided a <data.frame> object.

# tidy_dag additional edge cases

    Code
      expr
    Condition <ggdag_type_error>
      Error in `tidy_dagitty()`:
      ! Input must be a <dagitty> object.
      x You provided a <data.frame> object.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `pull_dag()`:
      ! `pull_dag()` requires a <tidy_dagitty> or <dagitty> object.
      x You provided a <list> object.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `pull_dag_data()`:
      ! `pull_dag_data()` requires a <tidy_dagitty> or <dagitty> object.
      x You provided a <list> object.

