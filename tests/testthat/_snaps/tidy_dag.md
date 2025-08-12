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

