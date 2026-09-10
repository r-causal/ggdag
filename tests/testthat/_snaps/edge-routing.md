# a routing object that is not one names the function the user called

    Code
      expr
    Condition <ggdag_type_error>
      Error in `geom_dag()`:
      ! `edge_route_options` must be an object from `edge_route_options()`.
      x You provided a string.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `ggdag()`:
      ! `edge_route_options` must be an object from `edge_route_options()`.
      x You provided a list.

---

    Code
      expr
    Condition <ggdag_type_error>
      Error in `geom_dag_routed_arrows()`:
      ! `edge_route_options` must be an object from `edge_route_options()`.
      x You provided a string.

# geom_dag_edges() reports the ignored grid arrows once

    Code
      expr
    Condition <ggdag_edge_arrow_warning>
      Warning in `warn_ignored_edge_arrows()`:
      Arrow specifications from `grid::arrow()` are drawn by the ggraph edge engine only.
      x The "ggarrow" engine is drawing these edges, so `arrow_directed` and `arrow_bidirected` are ignored.
      i Set the ornaments with `ggdag_options_set(arrow_head = , arrow_fins = )`, or draw with `edge_engine = "ggraph"`.

