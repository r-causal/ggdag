# node_d*() name the offending argument when a node is missing

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `node_dconnected()`:
      ! `from` and `to` not found in DAG.
      x Missing: "nope"
      i Available nodes: "m", "x", and "y"

---

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `node_dseparated()`:
      ! `from` and `to` not found in DAG.
      x Missing: "nope"
      i Available nodes: "m", "x", and "y"

---

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `node_drelationship()`:
      ! `controlling_for` not found in DAG.
      x Missing: "nope"
      i Available nodes: "m", "x", and "y"

