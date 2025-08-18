# geom_dag_collider_edges creates correct layer

    Code
      expr
    Condition
      Warning in `geom_dag_collider_edges()`:
      `size` is deprecated for lines.
      i Please use `linewidth` instead.

# geom_dag main function works with different options

    Code
      expr
    Condition
      Warning:
      The `node` argument of `geom_dag()` is deprecated as of ggdag 0.3.0.
      i Please use the `use_nodes` argument instead.
    Output
      [[1]]
      NULL
      
      [[2]]
      [[2]][[1]]
      mapping: start_cap = ~ggraph::circle(sizes[["cap"]], "mm"), end_cap = ~ggraph::circle(sizes[["cap"]], "mm") 
      geom_dag_edge_path: arrow = list(angle = 30, length = 5, ends = 2, type = 2), interpolate = FALSE, na.rm = TRUE
      stat_edge_link: na.rm = TRUE
      position_identity 
      
      [[2]][[2]]
      mapping: start_cap = ~ggraph::circle(sizes[["cap"]], "mm"), end_cap = ~ggraph::circle(sizes[["cap"]], "mm") 
      geom_dag_edge_path: arrow = list(angle = 30, length = 5, ends = 3, type = 2), interpolate = FALSE, na.rm = TRUE, lineend = butt, linejoin = round, linemitre = 1, label_colour = black, label_alpha = 1, label_parse = FALSE, check_overlap = FALSE, angle_calc = rot, force_flip = TRUE, label_dodge = NULL, label_push = NULL
      stat_edge_arc: strength = 0.3, fold = FALSE, na.rm = TRUE, n = 100
      position_identity 
      
      
      [[3]]
      mapping: label = ~name 
      geom_dag_text: parse = FALSE, check_overlap = FALSE, na.rm = FALSE
      stat_nodes: na.rm = FALSE
      position_identity 
      
      [[4]]
      NULL
      

---

    Code
      expr
    Condition
      Warning:
      The `stylized` argument of `geom_dag()` is deprecated as of ggdag 0.3.0.
      i Please use the `stylized` argument instead.
    Output
      [[1]]
      : na.rm = FALSE
      stat_nodes: na.rm = FALSE
      position_identity 
      
      [[2]]
      [[2]][[1]]
      mapping: start_cap = ~ggraph::circle(sizes[["cap"]], "mm"), end_cap = ~ggraph::circle(sizes[["cap"]], "mm") 
      geom_dag_edge_path: arrow = list(angle = 30, length = 5, ends = 2, type = 2), interpolate = FALSE, na.rm = TRUE
      stat_edge_link: na.rm = TRUE
      position_identity 
      
      [[2]][[2]]
      mapping: start_cap = ~ggraph::circle(sizes[["cap"]], "mm"), end_cap = ~ggraph::circle(sizes[["cap"]], "mm") 
      geom_dag_edge_path: arrow = list(angle = 30, length = 5, ends = 3, type = 2), interpolate = FALSE, na.rm = TRUE, lineend = butt, linejoin = round, linemitre = 1, label_colour = black, label_alpha = 1, label_parse = FALSE, check_overlap = FALSE, angle_calc = rot, force_flip = TRUE, label_dodge = NULL, label_push = NULL
      stat_edge_arc: strength = 0.3, fold = FALSE, na.rm = TRUE, n = 100
      position_identity 
      
      
      [[3]]
      mapping: label = ~name 
      geom_dag_text: parse = FALSE, check_overlap = FALSE, na.rm = FALSE
      stat_nodes: na.rm = FALSE
      position_identity 
      
      [[4]]
      NULL
      

---

    Code
      expr
    Condition
      Warning:
      The `text` argument of `geom_dag()` no longer accepts logicals as of ggdag 0.3.0.
      i Set `use_text = FALSE`. To use a variable other than node names, set `text = variable_name`

---

    Code
      expr
    Condition
      Warning:
      The `use_labels` argument of `geom_dag()` must be a logical as of ggdag 0.3.0.
      i Set `use_labels = TRUE` and `label = label`

