# control_for handles non-existent nodes

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `control_for()`:
      ! `var` not found in DAG.
      x Missing: "non_existent"
      i Available nodes: "x", "y", and "z"

---

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `control_for()`:
      ! `var` not found in DAG.
      x Missing: "non_existent"
      i Available nodes: "x", "y", and "z"

# dag_adjustment_sets handles edge cases

    Code
      expr
    Condition <ggdag_failed_to_close_backdoor_warning>
      Warning in `dag_adjustment_sets()`:
      Failed to close all backdoor paths.
      ! Common reasons include:
      * Graph is not acyclic
      * Backdoor paths are not closeable with given set of variables
      * Necessary variables are unmeasured (latent)

# is_collider and is_downstream_collider work correctly

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `is_collider()`:
      ! `.var` not found in DAG.
      x Missing: "non_existent"
      i Available nodes: "m", "w", "x", "y", and "z"

---

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `is_downstream_collider()`:
      ! `.var` not found in DAG.
      x Missing: "non_existent"
      i Available nodes: "m", "w", "x", "y", and "z"

