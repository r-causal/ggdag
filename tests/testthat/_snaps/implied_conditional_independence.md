# `query_conditional_independence()` returns a tibble of independencies

    Code
      expr
    Condition <ggdag_type_error>
      Error in `query_conditional_independence()`:
      ! `.tdy_dag` must be a <tidy_dagitty> or <dagitty> object.
      i You provided a <character> object.

# the missing independence column is reported clearly

    Code
      expr
    Condition <ggdag_columns_error>
      Error in `ggdag_conditional_independence()`:
      ! Required columns are missing from the data.
      x Missing columns: independence
      i Available columns: estimate, p.value, 2.5%, and 97.5%

# `test_conditional_independence()` works

    Code
      expr
    Condition <ggdag_missing_data_error>
      Error in `test_conditional_independence()`:
      ! Either `data` or `sample.cov` must be provided.
      i Use `data` to provide raw data.
      i Use `sample.cov` to provide a covariance matrix.

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

