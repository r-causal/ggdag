# time_ordered_coords(): a missing time value errors

    Code
      expr
    Condition <ggdag_missing_error>
      Error in `time_ordered_coords()`:
      ! Every variable needs a time point.
      x Time is missing for: "b".

# time_ordered_coords(): a character time column errors

    Code
      expr
    Condition <ggdag_type_error>
      Error in `time_ordered_coords()`:
      ! The time column of `.vars` must be numeric.
      x It is <character>.
      i Text time labels sort alphabetically rather than in time order; convert them to numeric time points first.

# time_ordered_coords(): time_points with a data frame errors

    Code
      expr
    Condition <ggdag_error>
      Error in `time_ordered_coords()`:
      ! `time_points` cannot be used with a data frame.
      i The second column of `.vars` already gives the time point of each variable.

# time_ordered_coords(): a longer time_points errors with optimize = FALSE

    Code
      expr
    Condition <ggdag_type_error>
      Error in `time_ordered_coords()`:
      ! `time_points` must have one value per time period.
      x `time_points` has 3 values, but `.vars` has 2 time periods.

# time_ordered_coords(): a shorter time_points errors with optimize = FALSE

    Code
      expr
    Condition <ggdag_type_error>
      Error in `time_ordered_coords()`:
      ! `time_points` must have one value per time period.
      x `time_points` has 1 value, but `.vars` has 2 time periods.

