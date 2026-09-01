# dag_paths() errors informatively without endpoints

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `dag_paths()`:
      ! Both `from` (exposure) and `to` (outcome) must be set.
      i Use `dag_paths(dag, from = "x", to = "y")` to specify paths.

