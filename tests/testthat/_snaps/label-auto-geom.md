# the auto geoms name the argument they refuse

    Code
      expr
    Condition <ggdag_type_error>
      Error in `geom_dag_label_auto()`:
      ! `wrap` must be a single positive whole number of characters.
      x You provided a number.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `geom_dag_text_auto()`:
      ! `wrap` must be a single positive whole number of characters.
      x You provided a number.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `geom_dag_label_auto()`:
      ! `min.segment.length` must be a single positive number of millimetres, `Inf`, or a `grid::unit()`.
      x You provided a number.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `geom_dag_text_auto()`:
      ! `min.segment.length` must be a single positive number of millimetres, `Inf`, or a `grid::unit()`.
      x You provided a number.

# geom_dag_label_auto() reports a missing x aesthetic

    Code
      expr
    Condition <rlang_error>
      Error in `label_geom()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error:
      ! The automatic label geoms need the DAG aesthetics on the plot.
      x The layer does not set x.
      i Build the plot with `ggplot(dag, aes_dag())`.

# geom_dag_text_auto() reports a missing x aesthetic

    Code
      expr
    Condition <rlang_error>
      Error in `label_geom()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error:
      ! The automatic label geoms need the DAG aesthetics on the plot.
      x The layer does not set x.
      i Build the plot with `ggplot(dag, aes_dag())`.

