# activate_collider_paths() conditions are informative

    Code
      expr
    Condition <ggdag_dots_error>
      Error in `activate_collider_paths()`:
      ! `...` must be empty.
      x Unused arguments: from and to
      i `...` is passed to `tidy_dagitty()`, which is not called for an input that is already a <tidy_dagitty>.

---

    Code
      expr
    Condition <ggdag_path_limit_warning>
      Warning in `adjustment_opens_path()`:
      Only the first 1000 paths between "x1" and "x8" were checked.
      ! A pathway opened by the adjustment may be missing from the plot.
      i Consider a smaller DAG or a sparser set of variables to adjust for.
    Output
      [1] TRUE

