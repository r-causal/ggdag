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
        name       x         y direction to      xend      yend
        <chr>  <dbl>     <dbl> <fct>     <chr>  <dbl>     <dbl>
      1 A      0.398  1.37e-19 ->        B     -0.398 -1.37e-19
      2 B     -0.398 -1.37e-19 ->        A      0.398  1.37e-19
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

# as_tidy_dagitty() empty list error message

    Code
      expr
    Condition <ggdag_type_error>
      Error in `as_tidy_dagitty()`:
      ! `x` must contain at least one time point.
      x You supplied an empty list.
      i Each element of `x` is a time point, and edges connect consecutive time points.

# as_tidy_dagitty() empty time point error message

    Code
      expr
    Condition <ggdag_type_error>
      Error in `as_tidy_dagitty()`:
      ! Every time point in `x` must name at least one node.
      x Time point 1 is empty.
      i Each element of `x` is a time point, and edges connect consecutive time points.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `as_tidy_dagitty()`:
      ! Every time point in `x` must name at least one node.
      x Time point 2 is empty.
      i Each element of `x` is a time point, and edges connect consecutive time points.

# as_tidy_dagitty() direction error message

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `as_tidy_dagitty()`:
      ! direction must be one of "->", "<->", and "--".
      x Unsupported values: "<-".
      i To reverse an edge, swap the name and to values.

