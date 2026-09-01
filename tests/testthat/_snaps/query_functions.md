# query_* conditions are informative

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `query_adjustment_sets()`:
      ! No exposure variable found in DAG and none provided.
      i Set `exposure` in `dagify()`, or pass it directly.

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `query_adjustment_sets()`:
      ! No outcome variable found in DAG and none provided.
      i Set `outcome` in `dagify()`, or pass it directly.

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `query_paths()`:
      ! No exposure variable found in DAG and no `from` provided.
      i Set `exposure` in `dagify()`, or pass `from` directly.

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `query_paths()`:
      ! No outcome variable found in DAG and no `to` provided.
      i Set `outcome` in `dagify()`, or pass `to` directly.

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `query_instrumental()`:
      ! No exposure variable found in DAG and none provided.
      i Set `exposure` in `dagify()`, or pass it directly.

---

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `query_instrumental()`:
      ! No outcome variable found in DAG and none provided.
      i Set `outcome` in `dagify()`, or pass it directly.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `query_dseparated()`:
      ! `from` must be a character vector.
      x You provided a <numeric> object.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `query_dseparated()`:
      ! `to` must be a character vector.
      x You provided a <numeric> object.

---

    Code
      expr
    Condition <ggdag_defunct_error>
      Error in `query_instrumental()`:
      ! `conditioned_on` is defunct and must be `NULL`.
      x An instrument's conditioning set cannot be chosen: conditioning on further variables can stop an instrument from being one.
      i `dagitty::instrumentalVariables()` works the set out itself and reports it in the conditioning_set and conditioned_on columns.

---

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `query_parents()`:
      ! `.var` not found in DAG.
      x Missing: "yy"
      i Available nodes: "x" and "y"

---

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `query_markov_blanket()`:
      ! `.var` not found in DAG.
      x Missing: "yy"
      i Available nodes: "x" and "y"

---

    Code
      expr
    Condition <ggdag_missing_nodes_error>
      Error in `query_status()`:
      ! `.var` not found in DAG.
      x Missing: "exposre"
      i Available nodes: "x" and "y"

