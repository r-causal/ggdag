# an NA endpoint is treated as unset

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `is_adjustment_set()`:
      ! Both `exposure` and `outcome` must be set.
      x `NA` does not name a variable.
      i Set them in `dagify()` or `dagitty::dagitty()`.
      i Or pass `exposure` and `outcome` directly.

# endpoint guards in the is_*() family are informative

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `is_instrumental()`:
      ! Both `exposure` and `outcome` must be set.
      i Set them in `dagify()` or `dagitty::dagitty()`.
      i Or pass `exposure` and `outcome` directly.

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `is_instrumental()`:
      ! `exposure` and `outcome` must each be a single variable.
      x `exposure` names 2 variables; `outcome` names 1.
      i Instrumental variables are defined for one exposure and one outcome.

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `is_adjustment_set()`:
      ! Both `exposure` and `outcome` must be set.
      i Set them in `dagify()` or `dagitty::dagitty()`.
      i Or pass `exposure` and `outcome` directly.

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

