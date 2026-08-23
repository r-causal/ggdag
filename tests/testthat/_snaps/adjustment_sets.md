# dag_adjustment_sets handles no valid adjustment sets with warning

    Code
      expr
    Condition <ggdag_failed_to_close_backdoor_warning>
      Warning in `dag_adjustment_sets()`:
      Failed to close all backdoor paths.
      ! Common reasons include:
      * Graph is not acyclic
      * Backdoor paths are not closeable with given set of variables
      * Necessary variables are unmeasured (latent)

# adjustment set guards are informative

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `dag_adjustment_sets()`:
      ! Both `exposure` and `outcome` must be set.
      i Set them in `dagify()` or `dagitty::dagitty()`.
      i Or pass `exposure` and `outcome` directly.

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `dag_adjustment_sets()`:
      ! Both `exposure` and `outcome` must be set.
      i Set them in `dagify()` or `dagitty::dagitty()`.
      i Or pass `exposure` and `outcome` directly.

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `ggdag_adjust()`:
      ! An adjusting variable needs to be set.
      i Use `var` or `control_for()` to specify adjusting variables.

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `ggdag_adjust()`:
      ! An adjusting variable needs to be set.
      i Use `var` or `control_for()` to specify adjusting variables.

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
    Condition <lifecycle_warning_deprecated>
      Warning:
      The `use_labels` argument of `geom_dag()` must be a logical as of ggdag 0.3.0.
      i Set `use_labels = TRUE` and `label = label`

---

    Code
      expr
    Condition <lifecycle_warning_deprecated>
      Warning:
      The `text` argument of `geom_dag()` no longer accepts logicals as of ggdag 0.3.0.
      i Set `use_text = FALSE`. To use a variable other than node names, set `text = variable_name`

