# `query_conditional_independence()` returns a tibble of independencies

    Code
      expr
    Condition <ggdag_type_error>
      Error in `query_conditional_independence()`:
      ! `.tdy_dag` must be a <tidy_dagitty> or <dagitty> object.
      i You provided a <character> object.

# `test_conditional_independence()` works

    Code
      expr
    Condition <simpleError>
      Error in `dagitty::localTests()`:
      ! Please provide either data or sample covariance matrix!

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `test_conditional_independence()`:
      ! `.tdy_dag` must be a <tidy_dagitty> or <dagitty> object.
      i You provided a <character> object.

---

    Code
      expr
    Condition <simpleError>
      Error in `cov()`:
      ! supply both 'x' and 'y' or a matrix-like 'x'

# `ggdag_conditional_independence()` works

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `ggdag_conditional_independence()`:
      ! `.test_result` must contain at least one row of test results.

