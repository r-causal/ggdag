# transmute.tidy_dagitty creates new columns and drops others

    Code
      expr
    Condition <ggdag_columns_error>
      Error in `transmute()`:
      ! Required columns are missing from the data.
      x Missing columns: name and to
      i Available columns: doubled_x

# distinct.tidy_dagitty removes duplicate rows

    Code
      expr
    Condition <ggdag_columns_error>
      Error in `distinct()`:
      ! Required columns are missing from the data.
      x Missing columns: name and to
      i Available columns: group

# multiple dplyr operations can be chained

    Code
      expr
    Condition <ggdag_columns_error>
      Error in `select()`:
      ! Required columns are missing from the data.
      x Missing columns: to
      i Available columns: name, x, y, and is_exposure

