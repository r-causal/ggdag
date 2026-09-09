# edge_route_options() rejects a millimetre field that is not one positive number

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `clearance` must be a single positive number of millimetres.
      x You provided a string.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `edge_sep` must be a single positive number of millimetres.
      x You provided a double vector.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `parallel_sep` must be a single positive number of millimetres.
      x You provided a numeric `NA`.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `clearance` must be a single positive number of millimetres.
      x You provided a number.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `edge_sep_min` must be a single positive number of millimetres.
      x You provided a number.

# edge_route_options() rejects a corner radius below the radius the ladder can reach

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `corner_radius` must be a single number of millimetres of at least 0.8, the radius the orthogonal ladder can shrink a corner to.
      x You provided a number.

# edge_route_options() rejects a bow, a price, or an angle out of range

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `max_bow` must be a single number between 0 and 1, a fraction of the chord it caps.
      x You provided a number.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `max_bow` must be a single number between 0 and 1, a fraction of the chord it caps.
      x You provided a number.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `head_penalty` must be a single number of at least 0.
      x You provided a number.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `steep_angle` must be a single number of degrees greater than 0 and at most 90.
      x You provided a number.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `tangent_clamp` must be a single number of degrees greater than 0 and at most 90.
      x You provided a number.

# edge_route_options() rejects an infinite value

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `clearance` must be a single positive number of millimetres.
      x You provided a number.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `head_penalty` must be a single number of at least 0.
      x You provided a number.

# edge_route_options() rejects a value that is not one of its choices

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `corners` must be one of "rounded" and "sharp".
      x You provided a string.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `crossing_saturation` must be a single logical value (TRUE or FALSE).
      x You provided a string.

# edge_route_options() rejects a separation floor above the separation it floors

    Code
      expr
    Condition <ggdag_type_error>
      Error in `edge_route_options()`:
      ! `edge_sep_min` must not be greater than `edge_sep`.
      x You provided 5 and 2.

# printing an edge_route_options object names what was set and what was not

    Code
      edge_route_options(clearance = 4, max_bow = 0.12, corners = "sharp")
    Message
      <ggdag_edge_route_options>
      * clearance: 4 mm
      * corners: "sharp"
      * max_bow: 0.12 of the chord
      i 12 fields left to the router; the size-dependent ones are derived when the
        plot is drawn.

---

    Code
      edge_route_options()
    Message
      <ggdag_edge_route_options>
      i 15 fields left to the router; the size-dependent ones are derived when the
        plot is drawn.

