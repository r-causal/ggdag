# missing edges produce an informative message

    Code
      expr
    Condition <ggdag_missing_edges_error>
      Error in `dag_prune()`:
      ! `edges` must name edges that are in the DAG.
      x Not in the DAG: "y -> x".
      i The name of an element of `edges` is the starting node and its value is the end node.

# unnamed edges produce an informative message

    Code
      expr
    Condition <ggdag_type_error>
      Error in `dag_prune()`:
      ! Every element of `edges` must be named.
      i Use the form `c("from" = "to")`.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `dag_prune()`:
      ! Every element of `edges` must be named.
      i Use the form `c("from" = "to")`.

