# tidied dags are in good shape

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `tidy_dagitty()`:
      ! `.dagitty` must be of graph type "dag".
      x Graph type is "mag".

# `as_tidy_dagitty()` returns correct objects

    Code
      expr
    Condition <ggdag_columns_error>
      Error in `as_tidy_dagitty()`:
      ! Required columns are missing from the data.
      x Missing columns: name and to
      i Available columns:

# Forbidden layouts error

    Code
      expr
    Condition <ggdag_error>
      Error in `check_verboten_layout()`:
      ! Layout type "dendogram" is not supported in ggdag.
      i See `?ggraph::create_layout()` for available layouts.

# tidy_dagitty warns about cyclic graphs

    Code
      expr
    Condition <ggdag_cyclic_warning>
      Warning in `tidy_dagitty()`:
      Graph contains a cycle and is not a valid DAG.
      ! Cycle detected: A -> B -> A
      i Causal diagram algorithms require acyclic graphs.
      i Consider revising your DAG specification.
    Output
      # DAG:
      # A `dagitty` DAG with: 2 nodes and 2 edges
      #
      # Data:
      # A tibble: 2 x 7
        name       x      y direction to      xend   yend
        <chr>  <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl>
      1 A     -0.363 -0.364 ->        B      0.202  0.197
      2 B      0.202  0.197 ->        A     -0.363 -0.364
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

