# dagify rejects self-loops with helpful error

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
    Condition <purrr_error_indexed>
      Error in `map()`:
      i In index: 1.
      Caused by error in `dagify()`:
      ! Self-loops are not allowed in DAGs.
      x Variable "y" cannot depend on itself.
      i Remove the self-referencing formula.

---

    Code
      expr
    Condition <purrr_error_indexed>
      Error in `map()`:
      i In index: 3.
      Caused by error in `dagify()`:
      ! Self-loops are not allowed in DAGs.
      x Variable "z" cannot depend on itself.
      i Remove the self-referencing formula.

# dagify validates exposure and outcome constraints

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `validate_dag_inputs()`:
      ! A variable cannot be both exposure and outcome.
      x Found: "x"

---

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `validate_dag_inputs()`:
      ! A variable cannot be both exposure and outcome.
      x Found: "y"

# dagify validates latent variable constraints

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `validate_dag_inputs()`:
      ! Latent variables cannot also be exposures.
      x Found: "u"

---

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `validate_dag_inputs()`:
      ! Latent variables cannot also be outcomes.
      x Found: "u"

# dagify validates variables exist in DAG

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `validate_dag_inputs()`:
      ! Exposure variable(s) not found in DAG.
      x Missing: "z"
      i Available variables: "y" and "x"

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `validate_dag_inputs()`:
      ! Outcome variable(s) not found in DAG.
      x Missing: "z"
      i Available variables: "y" and "x"

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `validate_dag_inputs()`:
      ! Latent variable(s) not found in DAG.
      x Missing: "z"
      i Available variables: "y" and "x"

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `validate_dag_inputs()`:
      ! Exposure variable(s) not found in DAG.
      x Missing: "z" and "w"
      i Available variables: "y" and "x"

# dagify() rejects one-sided formulas

    Code
      expr
    Condition <ggdag_type_error>
      Error in `dagify()`:
      ! Each argument to `dagify()` must be a two-sided formula.
      x `~x` has no left-hand side.
      i For example: `dagify(y ~ x + z, x ~ z)`.

# dagify() rejects character input

    Code
      expr
    Condition <ggdag_type_error>
      Error in `dagify()`:
      ! Each argument to `dagify()` must be a two-sided formula.
      x You provided a string.
      i For example: `dagify(y ~ x + z, x ~ z)`.

# curved() errors when called directly

    Code
      expr
    Condition <ggdag_error>
      Error in `curved()`:
      ! `curved()` can only be used inside `dagify()` formulas.
      i Example: `dagify(y ~ x + curved(m, 0.5))`

---

    Code
      expr
    Condition <ggdag_error>
      Error in `curved()`:
      ! `curved()` can only be used inside `dagify()` formulas.
      i Example: `dagify(y ~ x + curved(m, 0.5))`

# curved() non-literal curvature error carries the ggdag classes

    Code
      expr
    Condition <ggdag_type_error>
      Error in `find_curved_calls()`:
      ! `curvature` in `curved()` must be a numeric literal.
      i Example: `curved(x, 0.5)` or `curved(x, -0.3)`

# curve_edge() errors when the edge does not exist

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `curve_edge()`:
      ! 1 edge not found in the DAG.
      x Missing: "y -> m"
      i Did you swap `from` and `to`?

# set_curve_edges() errors when an edge does not exist

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `set_curve_edges()`:
      ! 2 edges not found in the DAG.
      x Missing: "y -> m" and "y -> x"
      i Did you swap `from` and `to`?

# set_curve_edges() rejects one bidirected edge named both ways round

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `set_curve_edges()`:
      ! 1 edge named more than once in `edges`.
      x Repeated: "x <-> y"
      i An edge takes one curvature, and a bidirected edge is the same edge whichever way round it is named.

