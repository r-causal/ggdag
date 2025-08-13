test_that("m_bias creates correct DAG structure", {
  # Default m-bias without x-y association
  dag1 <- m_bias()
  expect_s3_class(dag1, "dagitty")

  # Check structure
  edges <- dagitty::edges(dag1)
  expect_equal(nrow(edges), 4)
  expect_true(all(
    c("a -> x", "a -> m", "b -> y", "b -> m") %in% paste(edges$v, "->", edges$w)
  ))

  # Check exposure and outcome
  expect_equal(dagitty::exposures(dag1), "x")
  expect_equal(dagitty::outcomes(dag1), "y")

  # With x-y association
  dag2 <- m_bias(x_y_associated = TRUE)
  edges2 <- dagitty::edges(dag2)
  expect_equal(nrow(edges2), 5)
  expect_true("x -> y" %in% paste(edges2$v, "->", edges2$w))

  # With labels
  dag3 <- m_bias(
    x = "Exposure",
    y = "Outcome",
    a = "A",
    b = "B",
    m = "Mediator"
  )
  labels <- label(dag3)
  expect_equal(labels[["x"]], "Exposure")
  expect_equal(labels[["y"]], "Outcome")
  expect_equal(labels[["a"]], "A")
  expect_equal(labels[["b"]], "B")
  expect_equal(labels[["m"]], "Mediator")

  # Check coordinates are set
  coords <- dagitty::coordinates(dag3)
  expect_equal(length(coords$x), 5)
  expect_equal(coords$x[["x"]], 0)
  expect_equal(coords$x[["y"]], 2)
  expect_equal(coords$x[["m"]], 1)
})

test_that("butterfly_bias creates correct DAG structure", {
  # Default butterfly bias without x-y association
  dag1 <- butterfly_bias()
  expect_s3_class(dag1, "dagitty")

  # Check structure
  edges <- dagitty::edges(dag1)
  expect_equal(nrow(edges), 6)
  expect_true(all(
    c("a -> x", "a -> m", "b -> y", "b -> m", "m -> x", "m -> y") %in%
      paste(edges$v, "->", edges$w)
  ))

  # With x-y association
  dag2 <- butterfly_bias(x_y_associated = TRUE)
  edges2 <- dagitty::edges(dag2)
  expect_equal(nrow(edges2), 7)
  expect_true("x -> y" %in% paste(edges2$v, "->", edges2$w))

  # With labels
  dag3 <- butterfly_bias(x = "X1", y = "Y1", a = "A1", b = "B1", m = "M1")
  labels <- label(dag3)
  expect_equal(labels[["x"]], "X1")
  expect_equal(labels[["y"]], "Y1")
  expect_equal(labels[["m"]], "M1")

  # Check exposure and outcome
  expect_equal(dagitty::exposures(dag1), "x")
  expect_equal(dagitty::outcomes(dag1), "y")
})

test_that("confounder_triangle creates correct DAG structure", {
  # Default confounder without x-y association
  dag1 <- confounder_triangle()
  expect_s3_class(dag1, "dagitty")

  # Check structure
  edges <- dagitty::edges(dag1)
  expect_equal(nrow(edges), 2)
  expect_true(all(c("z -> x", "z -> y") %in% paste(edges$v, "->", edges$w)))

  # With x-y association
  dag2 <- confounder_triangle(x_y_associated = TRUE)
  edges2 <- dagitty::edges(dag2)
  expect_equal(nrow(edges2), 3)
  expect_true("x -> y" %in% paste(edges2$v, "->", edges2$w))

  # With labels
  dag3 <- confounder_triangle(x = "Treatment", y = "Response", z = "Confounder")
  labels <- label(dag3)
  expect_equal(labels[["x"]], "Treatment")
  expect_equal(labels[["y"]], "Response")
  expect_equal(labels[["z"]], "Confounder")

  # Check coordinates
  coords <- dagitty::coordinates(dag1)
  expect_equal(coords$y[["z"]], 1) # z is above x and y
})

test_that("collider_triangle creates correct DAG structure", {
  # Default collider without x-y association
  dag1 <- collider_triangle()
  expect_s3_class(dag1, "dagitty")

  # Check structure
  edges <- dagitty::edges(dag1)
  expect_equal(nrow(edges), 2)
  expect_true(all(c("x -> m", "y -> m") %in% paste(edges$v, "->", edges$w)))

  # With x-y association
  dag2 <- collider_triangle(x_y_associated = TRUE)
  edges2 <- dagitty::edges(dag2)
  expect_equal(nrow(edges2), 3)
  expect_true("x -> y" %in% paste(edges2$v, "->", edges2$w))

  # With labels
  dag3 <- collider_triangle(x = "X", y = "Y", m = "Collider")
  labels <- label(dag3)
  expect_equal(labels[["m"]], "Collider")

  # Check m is collider
  tidy_dag <- tidy_dagitty(dag1)
  colliders <- node_collider(tidy_dag)
  collider_data <- pull_dag_data(colliders)
  m_row <- collider_data[collider_data$name == "m", ]
  expect_equal(as.character(m_row$colliders), "Collider")
})

test_that("mediation_triangle creates correct DAG structure", {
  # Default mediation without x-y association
  dag1 <- mediation_triangle()
  expect_s3_class(dag1, "dagitty")

  # Check structure
  edges <- dagitty::edges(dag1)
  expect_equal(nrow(edges), 2)
  expect_true(all(c("x -> m", "m -> y") %in% paste(edges$v, "->", edges$w)))

  # With x-y association (direct effect)
  dag2 <- mediation_triangle(x_y_associated = TRUE)
  edges2 <- dagitty::edges(dag2)
  expect_equal(nrow(edges2), 3)
  expect_true("x -> y" %in% paste(edges2$v, "->", edges2$w))

  # With labels
  dag3 <- mediation_triangle(x = "Exposure", y = "Outcome", m = "Mediator")
  labels <- label(dag3)
  expect_equal(labels[["m"]], "Mediator")

  # Check m is on path from x to y
  paths <- dagitty::paths(dag1, from = "x", to = "y")
  expect_true(length(paths$paths) > 0)
  expect_true(any(grepl("m", paths$paths)))
})

test_that("ggdag_m_bias creates plots correctly", {
  # Basic plot
  p1 <- ggdag_m_bias()
  expect_s3_class(p1, "gg")
  # ggdag returns a ggplot object, not the data directly
  expect_s3_class(p1, "gg")

  # With labels
  p2 <- ggdag_m_bias(x = "X", y = "Y", m = "M")
  expect_true("label" %in% names(p2$data))

  # With x-y association
  p3 <- ggdag_m_bias(x_y_associated = TRUE)
  # Should have one more edge
  edge_count <- sum(!is.na(p3$data$to))
  expect_equal(edge_count, 5)

  # Test visual parameters
  p4 <- ggdag_m_bias(
    size = 2,
    node_size = 20,
    text_size = 5,
    edge_type = "arc",
    use_stylized = TRUE
  )
  expect_s3_class(p4, "gg")

  # Test with use_nodes = FALSE
  p5 <- ggdag_m_bias(use_nodes = FALSE)
  # Should not have geom_dag_point layer
  has_point_layer <- any(purrr::map_lgl(p5$layers, \(l) {
    inherits(l$geom, "GeomDagPoint")
  }))
  expect_false(has_point_layer)

  # Test with use_text = FALSE
  p6 <- ggdag_m_bias(use_text = FALSE)
  # Should not have text/label layer
  has_text_layer <- any(purrr::map_lgl(p6$layers, \(l) {
    inherits(l$geom, "GeomDagText") || inherits(l$geom, "GeomLabel")
  }))
  expect_false(has_text_layer)
})

test_that("ggdag_butterfly_bias creates plots correctly", {
  # Basic plot
  p1 <- ggdag_butterfly_bias()
  expect_s3_class(p1, "gg")

  # Check it has butterfly structure (6 edges)
  edge_count <- sum(!is.na(p1$data$to))
  expect_equal(edge_count, 6)

  # With association
  p2 <- ggdag_butterfly_bias(x_y_associated = TRUE)
  edge_count2 <- sum(!is.na(p2$data$to))
  expect_equal(edge_count2, 7)

  # Test parameters pass through
  p3 <- ggdag_butterfly_bias(
    edge_width = 1.2,
    arrow_length = 10,
    use_edges = FALSE
  )
  # Should not have edge layers
  has_edge_layer <- any(purrr::map_lgl(p3$layers, \(l) {
    inherits(l$geom, "GeomDagEdges") ||
      inherits(l$geom, "GeomCurve") ||
      inherits(l$geom, "GeomSegment")
  }))
  expect_false(has_edge_layer)
})

test_that("ggdag_confounder_triangle creates plots correctly", {
  # Basic plot
  p1 <- ggdag_confounder_triangle()
  expect_s3_class(p1, "gg")

  # Check triangle structure
  node_names <- unique(p1$data$name)
  expect_setequal(node_names, c("x", "y", "z"))

  # Without association should have 2 edges
  edge_count <- sum(!is.na(p1$data$to))
  expect_equal(edge_count, 2)

  # With labels
  p2 <- ggdag_confounder_triangle(
    x = "Exposure",
    y = "Outcome",
    z = "Confounder"
  )
  expect_true("label" %in% names(p2$data))
  expect_true(any(p2$data$label == "Confounder", na.rm = TRUE))

  # Test use_labels parameter
  p3 <- ggdag_confounder_triangle(
    x = "Exp",
    y = "Out",
    z = "Conf",
    use_labels = TRUE
  )
  # Should have label layer
  has_label_layer <- any(purrr::map_lgl(p3$layers, \(l) {
    inherits(l$geom, "GeomLabel") || inherits(l$geom, "GeomText")
  }))
  expect_true(has_label_layer)
})

test_that("ggdag_collider_triangle creates plots correctly", {
  # Basic plot
  p1 <- ggdag_collider_triangle()
  expect_s3_class(p1, "gg")

  # Check collider structure
  edge_data <- p1$data[!is.na(p1$data$to), ]
  # Both x and y should point to m
  expect_true(all(edge_data$to == "m"))

  # With association
  p2 <- ggdag_collider_triangle(x_y_associated = TRUE)
  edge_count <- sum(!is.na(p2$data$to))
  expect_equal(edge_count, 3)

  # Test text color parameter
  p3 <- ggdag_collider_triangle(text_col = "red", label_col = "blue")
  expect_s3_class(p3, "gg")
})

test_that("ggdag_mediation_triangle creates plots correctly", {
  # Basic plot
  p1 <- ggdag_mediation_triangle()
  expect_s3_class(p1, "gg")

  # Check mediation structure (x -> m -> y)
  edge_data <- p1$data[!is.na(p1$data$to), ]
  # Filter to edge rows that have 'from' column
  from_data <- edge_data[!is.na(edge_data$name), ]
  x_edges <- from_data[from_data$name == "x", ]
  m_edges <- from_data[from_data$name == "m", ]
  # Check that x points to m and m points to y
  expect_true(any(x_edges$to == "m"))
  expect_true(any(m_edges$to == "y"))

  # With direct effect
  p2 <- ggdag_mediation_triangle(x_y_associated = TRUE)
  edge_count <- sum(!is.na(p2$data$to))
  expect_equal(edge_count, 3)
  # Should have direct x -> y edge
  edge_data2 <- p2$data[!is.na(p2$data$to), ]
  x_edges2 <- edge_data2[edge_data2$name == "x", ]
  expect_true("y" %in% x_edges2$to)

  # Test edge_cap parameter
  p3 <- ggdag_mediation_triangle(edge_cap = 20)
  expect_s3_class(p3, "gg")
})

test_that("quick plot functions handle NULL labels correctly", {
  # All functions should work with NULL labels (default)
  expect_s3_class(m_bias(), "dagitty")
  expect_s3_class(butterfly_bias(), "dagitty")
  expect_s3_class(confounder_triangle(), "dagitty")
  expect_s3_class(collider_triangle(), "dagitty")
  expect_s3_class(mediation_triangle(), "dagitty")

  # And with partial labels
  expect_s3_class(m_bias(x = "X"), "dagitty")
  expect_s3_class(butterfly_bias(y = "Y", m = "M"), "dagitty")
  expect_s3_class(confounder_triangle(z = "Z"), "dagitty")
})

test_that("all ggdag quick plots accept standard parameters", {
  # Test that all functions accept the standard ggdag parameters
  funcs <- list(
    ggdag_m_bias,
    ggdag_butterfly_bias,
    ggdag_confounder_triangle,
    ggdag_collider_triangle,
    ggdag_mediation_triangle
  )

  purrr::walk(funcs, \(func) {
    # Should all accept these parameters without error
    p <- func(
      size = 1.5,
      edge_type = "diagonal",
      node_size = 18,
      text_size = 4,
      label_size = 4,
      text_col = "black",
      label_col = "white",
      edge_width = 0.8,
      edge_cap = 10,
      arrow_length = 6,
      use_edges = TRUE,
      use_nodes = TRUE,
      use_stylized = FALSE,
      use_text = TRUE,
      use_labels = FALSE
    )
    expect_s3_class(p, "gg")
  })
})

test_that("quick plot edge cases work correctly", {
  # Empty label vectors
  dag1 <- m_bias(x = character(0), y = character(0))
  expect_s3_class(dag1, "dagitty")

  # Very long labels
  long_label <- paste(rep("long", 20), collapse = "_")
  dag2 <- confounder_triangle(x = long_label)
  expect_equal(label(dag2)[["x"]], long_label)

  # Unicode labels
  dag3 <- collider_triangle(m = "αβγ")
  expect_equal(label(dag3)[["m"]], "αβγ")
})

test_that("quartet_collider creates correct DAG structure", {
  # Default collider with x-y association
  dag1 <- quartet_collider()
  expect_s3_class(dag1, "dagitty")
  
  # Check structure
  edges <- dagitty::edges(dag1)
  expect_equal(nrow(edges), 3) # x->y, x->z, y->z
  expect_true(all(c("x -> z", "y -> z", "x -> y") %in% paste(edges$v, "->", edges$w)))
  
  # Without x-y association
  dag2 <- quartet_collider(x_y_associated = FALSE)
  edges2 <- dagitty::edges(dag2)
  expect_equal(nrow(edges2), 2)
  expect_false("x -> y" %in% paste(edges2$v, "->", edges2$w))
  
  # With labels
  dag3 <- quartet_collider(x = "Exposure", y = "Outcome", z = "Collider")
  labels <- label(dag3)
  expect_equal(labels[["z"]], "Collider")
  
  # Check z is collider
  tidy_dag <- tidy_dagitty(dag1)
  colliders <- node_collider(tidy_dag)
  collider_data <- pull_dag_data(colliders)
  z_row <- collider_data[collider_data$name == "z", ]
  expect_equal(as.character(z_row$colliders), "Collider")
})

test_that("quartet_confounder creates correct DAG structure", {
  # Default confounder
  dag1 <- quartet_confounder()
  expect_s3_class(dag1, "dagitty")
  
  # Check structure
  edges <- dagitty::edges(dag1)
  expect_equal(nrow(edges), 3) # z->x, z->y, x->y
  expect_true(all(c("z -> x", "z -> y", "x -> y") %in% paste(edges$v, "->", edges$w)))
  
  # Without x-y association
  dag2 <- quartet_confounder(x_y_associated = FALSE)
  edges2 <- dagitty::edges(dag2)
  expect_equal(nrow(edges2), 2)
  expect_false("x -> y" %in% paste(edges2$v, "->", edges2$w))
  
  # Check exposure and outcome
  expect_equal(dagitty::exposures(dag1), "x")
  expect_equal(dagitty::outcomes(dag1), "y")
})

test_that("quartet_mediator creates correct DAG structure", {
  # Default mediator without x-y association
  dag1 <- quartet_mediator()
  expect_s3_class(dag1, "dagitty")
  
  # Check structure
  edges <- dagitty::edges(dag1)
  expect_equal(nrow(edges), 2) # x->z, z->y
  expect_true(all(c("x -> z", "z -> y") %in% paste(edges$v, "->", edges$w)))
  
  # With x-y association (direct effect)
  dag2 <- quartet_mediator(x_y_associated = TRUE)
  edges2 <- dagitty::edges(dag2)
  expect_equal(nrow(edges2), 3)
  expect_true("x -> y" %in% paste(edges2$v, "->", edges2$w))
  
  # Check z is on path from x to y
  paths <- dagitty::paths(dag1, from = "x", to = "y")
  expect_true(length(paths$paths) > 0)
  expect_true(any(grepl("z", paths$paths)))
})

test_that("quartet_m_bias creates correct DAG structure", {
  # Default m-bias with x-y association
  dag1 <- quartet_m_bias()
  expect_s3_class(dag1, "dagitty")
  
  # Check structure
  edges <- dagitty::edges(dag1)
  expect_equal(nrow(edges), 5) # u1->z, u2->z, u1->x, u2->y, x->y
  expect_true(all(
    c("u1 -> z", "u2 -> z", "u1 -> x", "u2 -> y", "x -> y") %in% 
    paste(edges$v, "->", edges$w)
  ))
  
  # Without x-y association
  dag2 <- quartet_m_bias(x_y_associated = FALSE)
  edges2 <- dagitty::edges(dag2)
  expect_equal(nrow(edges2), 4)
  expect_false("x -> y" %in% paste(edges2$v, "->", edges2$w))
  
  # With labels including u1 and u2
  dag3 <- quartet_m_bias(
    x = "X", y = "Y", z = "Z", 
    u1 = "Unmeasured1", u2 = "Unmeasured2"
  )
  labels <- label(dag3)
  expect_equal(labels[["u1"]], "Unmeasured1")
  expect_equal(labels[["u2"]], "Unmeasured2")
})

test_that("quartet_time_collider creates correct DAG structure", {
  # Time-varying collider
  dag1 <- quartet_time_collider()
  expect_s3_class(dag1, "dagitty")
  
  # Check structure
  edges <- dagitty::edges(dag1)
  expect_equal(nrow(edges), 8)
  expect_true(all(
    c("x1 -> y2", "x2 -> y3", "x1 -> x2", "x1 -> z2", "x2 -> z3",
      "y2 -> z2", "y3 -> z3", "z2 -> z3") %in% 
    paste(edges$v, "->", edges$w)
  ))
  
  # With labels
  dag2 <- quartet_time_collider(
    x0 = "X0", x1 = "X1", x2 = "X2", x3 = "X3",
    y1 = "Y1", y2 = "Y2", y3 = "Y3",
    z1 = "Z1", z2 = "Z2", z3 = "Z3"
  )
  labels <- label(dag2)
  expect_equal(length(labels), 10)
  expect_equal(labels[["x2"]], "X2")
  
  # Check exposure and outcome
  expect_equal(dagitty::exposures(dag1), "x2")
  expect_equal(dagitty::outcomes(dag1), "y3")
})

test_that("ggdag_quartet functions create plots correctly", {
  # Test all quartet ggdag functions
  p1 <- ggdag_quartet_collider()
  expect_s3_class(p1, "gg")
  
  p2 <- ggdag_quartet_confounder()
  expect_s3_class(p2, "gg")
  
  p3 <- ggdag_quartet_mediator()
  expect_s3_class(p3, "gg")
  
  p4 <- ggdag_quartet_m_bias()
  expect_s3_class(p4, "gg")
  
  p5 <- ggdag_quartet_time_collider()
  expect_s3_class(p5, "gg")
  
  # Test with labels
  p6 <- ggdag_quartet_collider(x = "E", y = "O", z = "C")
  expect_true("label" %in% names(p6$data))
  
  # Test parameter passing
  p7 <- ggdag_quartet_confounder(
    use_nodes = FALSE,
    use_text = FALSE,
    edge_type = "arc"
  )
  expect_s3_class(p7, "gg")
})

test_that("quartet functions have correct default x_y_associated values", {
  # Collider and confounder default to TRUE
  dag1 <- quartet_collider()
  edges1 <- dagitty::edges(dag1)
  expect_true("x -> y" %in% paste(edges1$v, "->", edges1$w))
  
  dag2 <- quartet_confounder()
  edges2 <- dagitty::edges(dag2)
  expect_true("x -> y" %in% paste(edges2$v, "->", edges2$w))
  
  # Mediator defaults to FALSE
  dag3 <- quartet_mediator()
  edges3 <- dagitty::edges(dag3)
  expect_false("x -> y" %in% paste(edges3$v, "->", edges3$w))
  
  # M-bias defaults to TRUE
  dag4 <- quartet_m_bias()
  edges4 <- dagitty::edges(dag4)
  expect_true("x -> y" %in% paste(edges4$v, "->", edges4$w))
})

test_that("all ggdag quartet plots accept standard parameters", {
  # Test that all quartet functions accept the standard ggdag parameters
  funcs <- list(
    ggdag_quartet_collider,
    ggdag_quartet_confounder,
    ggdag_quartet_mediator,
    ggdag_quartet_m_bias,
    ggdag_quartet_time_collider
  )
  
  purrr::walk(funcs, \(func) {
    # Should all accept these parameters without error
    p <- func(
      size = 1.5,
      edge_type = "diagonal",
      node_size = 18,
      text_size = 4,
      label_size = 4,
      text_col = "black",
      label_col = "white",
      edge_width = 0.8,
      edge_cap = 10,
      arrow_length = 6,
      use_edges = TRUE,
      use_nodes = TRUE,
      use_stylized = FALSE,
      use_text = TRUE,
      use_labels = FALSE
    )
    expect_s3_class(p, "gg")
  })
})
