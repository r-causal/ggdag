# is_d_*() name the offending argument when a node is missing

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `is_d_separated()`:
      ! `from` and `to` not found in DAG.
      x Missing: "nope"
      i Available nodes: "m", "x", and "y"

---

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `is_d_connected()`:
      ! `from` and `to` not found in DAG.
      x Missing: "nope"
      i Available nodes: "m", "x", and "y"

---

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `is_d_separated()`:
      ! `controlling_for` not found in DAG.
      x Missing: "nope"
      i Available nodes: "m", "x", and "y"

