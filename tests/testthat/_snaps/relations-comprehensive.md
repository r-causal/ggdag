# relations functions handle edge cases

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
    Condition <simpleError>
      Error in `.checkAllNames()`:
      ! z is not a variable in `x`

