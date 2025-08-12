# dag_adjustment_sets handles no valid adjustment sets with warning

    Code
      expr
    Condition
      Warning in `dag_adjustment_sets()`:
      Failed to close all backdoor paths.
      ! Common reasons include:
      * Graph is not acyclic
      * Backdoor paths are not closeable with given set of variables
      * Necessary variables are unmeasured (latent)

# control_for handles var with no matches

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `control_for()`:
      ! `var` not found in DAG.
      x Missing: "z"
      i Available nodes: "x" and "y"

# ggdag_adjust handles node styling

    Code
      expr
    Condition
      Warning:
      The `use_labels` argument of `geom_dag()` must be a logical as of ggdag 0.3.0.
      i Set `use_labels = TRUE` and `label = label`

---

    Code
      expr
    Condition
      Warning:
      The `text` argument of `geom_dag()` no longer accepts logicals as of ggdag 0.3.0.
      i Set `use_text = FALSE`. To use a variable other than node names, set `text = variable_name`

