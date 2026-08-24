# compute_time_ordered_layout: pin before a node's ancestors errors

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `longest_path_layers()`:
      ! Pinned time 1 for "c" is too early.
      x "c" has ancestors requiring at least time 3.

# compute_time_ordered_layout: pin before one ancestor errors

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `longest_path_layers()`:
      ! Pinned time 1 for "b" is too early.
      x "b" has ancestors requiring at least time 2.

# compute_time_ordered_layout: pins squeezing a node out name the pins

    Code
      expr
    Condition <ggdag_dag_error>
      Error in `longest_path_layers()`:
      ! Pinned times violate DAG ordering.
      x "c" is pinned to time 4, but "a" pushes it to time 5 at the earliest.
      i Move "c" later, or move "a" earlier.

# compute_time_ordered_layout: non-integer fixed_time errors

    Code
      expr
    Condition <ggdag_error>
      Error in `compute_time_ordered_layout()`:
      ! `fixed_time` values must be whole numbers.
      x "m" is pinned to a fractional time.

