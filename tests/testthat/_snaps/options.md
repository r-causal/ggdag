# validation rejects non-numeric for numeric params

    Code
      expr
    Condition <ggdag_type_error>
      Error in `ggdag_options_set()`:
      ! `node_size` must be a single positive number.
      x You provided a string.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `ggdag_options_set()`:
      ! `text_size` must be a single positive number.
      x You provided `TRUE`.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `ggdag_options_set()`:
      ! `edge_cap` must be a single positive number.
      x You provided a number.

# validation rejects non-logical for logical params

    Code
      expr
    Condition <ggdag_type_error>
      Error in `ggdag_options_set()`:
      ! `use_edges` must be a single logical value (TRUE or FALSE).
      x You provided a string.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `ggdag_options_set()`:
      ! `use_nodes` must be a single logical value (TRUE or FALSE).
      x You provided a number.

# validation rejects invalid edge_type

    Code
      expr
    Condition <ggdag_type_error>
      Error in `ggdag_options_set()`:
      ! `edge_type` must be one of "link_arc", "link", "arc", and "diagonal".
      x You provided "squiggle".

# validation rejects non-function for label_geom

    Code
      expr
    Condition <ggdag_type_error>
      Error in `ggdag_options_set()`:
      ! `label_geom` must be a function.
      x You provided a string.

# layout option validation rejects invalid types

    Code
      expr
    Condition <ggdag_type_error>
      Error in `ggdag_options_set()`:
      ! `layout` must be a single character string or a function.
      x You provided a number.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `ggdag_options_set()`:
      ! `layout` must be a single character string or a function.
      x You provided `TRUE`.

