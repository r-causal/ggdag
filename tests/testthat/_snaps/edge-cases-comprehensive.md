# dag_label edge cases

    Code
      expr
    Condition
      Warning in `dag_label()`:
      No labels provided
    Output
      # A DAG with 2 nodes and 1 edges
      #
      # A tibble: 2 x 7
        name       x      y direction to      xend   yend
        <chr>  <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl>
      1 x      0.137  0.206 ->        y     -0.579 -0.496
      2 y     -0.579 -0.496 <NA>      <NA>  NA     NA    

# pull functions edge cases

    Code
      expr
    Condition <simpleError>
      Error in `UseMethod()`:
      ! no applicable method for 'pull_dag' applied to an object of class "data.frame"

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
    Condition <simpleError>
      Error in `UseMethod()`:
      ! no applicable method for 'pull_dag' applied to an object of class "list"

---

    Code
      expr
    Condition <simpleError>
      Error in `UseMethod()`:
      ! no applicable method for 'pull_dag_data' applied to an object of class "list"

