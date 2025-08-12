# dagify rejects self-loops with helpful error

    Code
      expr
    Condition <purrr_error_indexed>
      Error in `map()`:
      i In index: 1.
      Caused by error in `.f()`:
      ! Self-loops are not allowed in DAGs.
      x Variable "x" cannot depend on itself.
      i Remove the self-referencing formula.

---

    Code
      expr
    Condition <purrr_error_indexed>
      Error in `map()`:
      i In index: 1.
      Caused by error in `.f()`:
      ! Self-loops are not allowed in DAGs.
      x Variable "y" cannot depend on itself.
      i Remove the self-referencing formula.

---

    Code
      expr
    Condition <purrr_error_indexed>
      Error in `map()`:
      i In index: 3.
      Caused by error in `.f()`:
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

