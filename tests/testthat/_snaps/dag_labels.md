# duplicated label names produce an informative message

    Code
      expr
    Condition <ggdag_type_error>
      Error in `label<-`:
      ! `labels` must name each node at most once.
      x Duplicated node name: "x".
      i Labels are joined to the DAG data by name, so a repeated name would give a node more than one row.

# unnamed labels produce an informative message

    Code
      expr
    Condition <ggdag_type_error>
      Error in `label<-`:
      ! `labels` must be a named character vector.
      x Each label must be named for the node it belongs to.
      i For example: `c(x = "Exposure", y = "Outcome")`.

# unknown label names produce an informative message

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `label<-`:
      ! `labels` not found in DAG.
      x Missing: "nope"
      i Available nodes: "x" and "y"

