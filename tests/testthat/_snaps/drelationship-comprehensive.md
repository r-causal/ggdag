# node_dconnected identifies d-connected nodes correctly

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `node_dconnected()`:
      ! Both `from` and `to` must be set.
      i Set `from` to specify the starting variable.
      i Set `to` to specify the ending variable.

# node_dseparated identifies d-separated nodes correctly

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `node_dseparated()`:
      ! Both `from` and `to` must be set.
      i Set `from` to specify the starting variable.
      i Set `to` to specify the ending variable.

# d-relationship functions handle edge cases

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `dagify()`:
      ! Self-loops are not allowed in DAGs.
      x Variable "x" cannot depend on itself.
      i Remove the self-referencing formula.

