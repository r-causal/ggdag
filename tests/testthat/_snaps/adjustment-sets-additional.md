# control_for handles non-existent nodes

    Code
      expr
    Condition <simpleError>
      Error in `.checkAllNames()`:
      ! non_existent is not a variable in `x`

---

    Code
      expr
    Condition <simpleError>
      Error in `.checkAllNames()`:
      ! non_existent is not a variable in `x`

# dag_adjustment_sets handles edge cases

    Code
      expr
    Condition
      Warning in `dag_adjustment_sets()`:
      Failed to close all backdoor paths.
      ! Common reasons include:
      * Graph is not acyclic
      * Backdoor paths are not closeable with given set of variables
      * Necessary variables are unmeasured (latent)

# is_collider and is_downstream_collider work correctly

    Code
      expr
    Condition <simpleError>
      Error in `.checkAllNames()`:
      ! non_existent is not a variable in `x`

---

    Code
      expr
    Condition <simpleError>
      Error in `.checkAllNames()`:
      ! non_existent is not a variable in `x`

