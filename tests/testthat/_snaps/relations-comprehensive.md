# relations functions handle edge cases

    Code
      expr
    Condition <purrr_error_indexed>
      Error in `map()`:
      i In index: 1.
      Caused by error in `dagify()`:
      ! Self-loops are not allowed in DAGs.
      x Variable "x" cannot depend on itself.
      i Remove the self-referencing formula.

---

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `node_children()`:
      ! `.var` not found in DAG.
      x Missing: "z"
      i Available nodes: "x" and "y"

