test_that("node_dconnected identifies d-connected nodes correctly", {
  # Basic test with simple DAG
  dag <- dagify(m ~ x + y)

  # Without controlling for anything, x and y are d-separated
  result <- node_dconnected(dag, from = "x", to = "y")
  result_df <- pull_dag_data(result)

  expect_true("d_relationship" %in% names(result_df))
  x_row <- result_df[result_df$name == "x", ]
  y_row <- result_df[result_df$name == "y", ]
  expect_equal(as.character(x_row$d_relationship), "d-separated")
  expect_equal(as.character(y_row$d_relationship), "d-separated")

  # Controlling for collider m opens the path
  result_controlled <- node_dconnected(
    dag,
    from = "x",
    to = "y",
    controlling_for = "m"
  )
  result_controlled_df <- pull_dag_data(result_controlled)
  x_row_controlled <- result_controlled_df[result_controlled_df$name == "x", ]
  y_row_controlled <- result_controlled_df[result_controlled_df$name == "y", ]
  expect_equal(as.character(x_row_controlled$d_relationship[1]), "d-connected")
  expect_equal(as.character(y_row_controlled$d_relationship[1]), "d-connected")

  # Check that controlling variables are marked as adjusted
  m_row_controlled <- result_controlled_df[result_controlled_df$name == "m", ]
  expect_equal(as.character(m_row_controlled$adjusted), "adjusted")

  # Test with tidy_dagitty input
  tidy_dag <- tidy_dagitty(dag)
  result_tidy <- node_dconnected(tidy_dag, from = "x", to = "y")
  expect_s3_class(result_tidy, "tidy_dagitty")

  # Test with NULL from/to (should use exposure/outcome)
  dag_exp <- dagify(y ~ x + z, exposure = "x", outcome = "y")
  result_auto <- node_dconnected(dag_exp)
  expect_true("d_relationship" %in% names(pull_dag_data(result_auto)))

  # Test error when from/to not set and no exposure/outcome
  expect_ggdag_error(
    node_dconnected(dag)
  )

  # Test as_factor parameter
  result_no_factor <- node_dconnected(
    dag,
    from = "x",
    to = "y",
    as_factor = FALSE
  )
  result_no_factor_df <- pull_dag_data(result_no_factor)
  expect_type(result_no_factor_df$d_relationship, "character")

  result_factor <- node_dconnected(dag, from = "x", to = "y", as_factor = TRUE)
  result_factor_df <- pull_dag_data(result_factor)
  expect_s3_class(result_factor_df$d_relationship, "factor")
  expect_equal(
    levels(result_factor_df$d_relationship),
    c("d-connected", "d-separated")
  )
})

test_that("node_dseparated identifies d-separated nodes correctly", {
  # Basic test - should give opposite results from dconnected
  dag <- dagify(m ~ x + y)

  # Without controlling, x and y are d-separated
  result <- node_dseparated(dag, from = "x", to = "y")
  result_df <- pull_dag_data(result)

  x_row <- result_df[result_df$name == "x", ]
  y_row <- result_df[result_df$name == "y", ]
  expect_equal(as.character(x_row$d_relationship), "d-separated")
  expect_equal(as.character(y_row$d_relationship), "d-separated")

  # Controlling for m opens the path (no longer d-separated)
  result_controlled <- node_dseparated(
    dag,
    from = "x",
    to = "y",
    controlling_for = "m"
  )
  result_controlled_df <- pull_dag_data(result_controlled)
  x_row_controlled <- result_controlled_df[result_controlled_df$name == "x", ]
  y_row_controlled <- result_controlled_df[result_controlled_df$name == "y", ]
  expect_equal(as.character(x_row_controlled$d_relationship[1]), "d-connected")
  expect_equal(as.character(y_row_controlled$d_relationship[1]), "d-connected")

  # Test with no controlling_for - should add collider_line and adjusted columns
  result_no_control <- node_dseparated(dag, from = "x", to = "y")
  result_no_control_df <- pull_dag_data(result_no_control)
  expect_true("collider_line" %in% names(result_no_control_df))
  expect_true("adjusted" %in% names(result_no_control_df))
  expect_true(all(result_no_control_df$collider_line == FALSE))
  expect_true(all(result_no_control_df$adjusted == "unadjusted"))

  # Test with exposure/outcome
  dag_exp <- dagify(y ~ x + z, z ~ x, exposure = "x", outcome = "y")
  result_auto <- node_dseparated(dag_exp)
  expect_true("d_relationship" %in% names(pull_dag_data(result_auto)))

  # Test error handling
  expect_ggdag_error(
    node_dseparated(dag)
  )
})

test_that("node_drelationship works as general function", {
  dag <- dagify(m ~ x + y, y ~ x)

  # Basic functionality
  result <- node_drelationship(dag, from = "x", to = "y")
  result_df <- pull_dag_data(result)

  x_row <- result_df[result_df$name == "x", ]
  y_row <- result_df[result_df$name == "y", ]
  # x causes y, so they are d-connected
  expect_equal(as.character(x_row$d_relationship[1]), "d-connected")
  expect_equal(as.character(y_row$d_relationship[1]), "d-connected")

  # Nodes not in from/to should have NA
  m_row <- result_df[result_df$name == "m", ]
  expect_true(is.na(m_row$d_relationship))

  # Test with controlling
  result_controlled <- node_drelationship(
    dag,
    from = "x",
    to = "m",
    controlling_for = "y"
  )
  result_controlled_df <- pull_dag_data(result_controlled)
  # Controlling for collider y opens path from x to m
  x_row_controlled <- result_controlled_df[result_controlled_df$name == "x", ]
  m_row_controlled <- result_controlled_df[result_controlled_df$name == "m", ]
  expect_equal(as.character(x_row_controlled$d_relationship[1]), "d-connected")
  expect_equal(as.character(m_row_controlled$d_relationship[1]), "d-connected")

  # Test with exposure/outcome auto-detection
  dag_exp <- dagify(y ~ x + z, exposure = "x", outcome = "y")
  result_auto <- node_drelationship(dag_exp)
  expect_true("d_relationship" %in% names(pull_dag_data(result_auto)))

  # Test as_factor
  result_char <- node_drelationship(
    dag,
    from = "x",
    to = "y",
    as_factor = FALSE
  )
  expect_type(pull_dag_data(result_char)$d_relationship, "character")
})

test_that("ggdag_drelationship creates correct plots", {
  dag <- dagify(m ~ x + y)

  # Basic plot
  p1 <- ggdag_drelationship(dag, from = "x", to = "y")
  expect_s3_class(p1, "gg")
  expect_true("d_relationship" %in% names(p1$data))

  # Check that d_relationship is in the data
  expect_true("d_relationship" %in% names(p1$data))

  # With controlling_for
  p2 <- ggdag_drelationship(dag, from = "x", to = "y", controlling_for = "m")
  expect_true("adjusted" %in% names(p2$data))

  # Check that collider edges are added when appropriate
  expect_true(any(sapply(p2$layers, function(l) {
    inherits(l$geom, "GeomCurve")
  })))

  # Without collider lines
  p3 <- ggdag_drelationship(
    dag,
    from = "x",
    to = "y",
    controlling_for = "m",
    collider_lines = FALSE
  )
  # Should not have GeomCurve layer
  expect_false(any(sapply(p3$layers, function(l) {
    inherits(l$geom, "GeomCurve")
  })))

  # Test with various parameters
  p4 <- ggdag_drelationship(
    dag,
    from = "x",
    to = "y",
    size = 2,
    node_size = 20,
    text_size = 5,
    edge_type = "arc",
    use_labels = TRUE
  )
  expect_s3_class(p4, "gg")

  # Test that it uses aes mapping for shape when adjusted
  dag_adj <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty() |>
    control_for("z")
  p5 <- ggdag_drelationship(dag_adj, from = "x", to = "y")
  mapping <- p5$mapping
  expect_true("shape" %in% names(mapping))

  # Test without adjusted column
  p6 <- ggdag_drelationship(dag, from = "x", to = "y")
  mapping6 <- p6$mapping
  expect_false("shape" %in% names(mapping6))
  expect_true("colour" %in% names(mapping6))
})

test_that("ggdag_dseparated works correctly", {
  dag <- dagify(m ~ x + y)

  # Should call ggdag_drelationship internally
  p1 <- ggdag_dseparated(dag, from = "x", to = "y")
  expect_s3_class(p1, "gg")
  expect_true("d_relationship" %in% names(p1$data))

  # Test with controlling
  p2 <- ggdag_dseparated(dag, from = "x", to = "y", controlling_for = "m")
  expect_true("adjusted" %in% names(p2$data))

  # Test parameter passing
  p3 <- ggdag_dseparated(
    dag,
    from = "x",
    to = "y",
    size = 1.5,
    text_col = "red",
    use_stylized = TRUE
  )
  expect_s3_class(p3, "gg")
})

test_that("ggdag_dconnected works correctly", {
  dag <- dagify(m ~ x + y)

  # Should call ggdag_drelationship internally
  p1 <- ggdag_dconnected(dag, from = "x", to = "y")
  expect_s3_class(p1, "gg")
  expect_true("d_relationship" %in% names(p1$data))

  # Test with controlling
  p2 <- ggdag_dconnected(dag, from = "x", to = "y", controlling_for = "m")
  expect_true("adjusted" %in% names(p2$data))

  # Test parameter passing
  p3 <- ggdag_dconnected(
    dag,
    from = "x",
    to = "y",
    edge_type = "diagonal",
    use_text = FALSE,
    use_labels = TRUE,
    label = label
  )
  expect_s3_class(p3, "gg")
})

test_that("d-relationship functions handle complex DAGs", {
  # More complex DAG with multiple paths
  complex_dag <- dagify(
    y ~ x + z + w,
    z ~ x + v,
    w ~ v,
    exposure = "x",
    outcome = "y"
  )

  # Test d-connection through different paths
  result1 <- node_dconnected(complex_dag, from = "x", to = "y")
  expect_equal(
    as.character(pull_dag_data(result1)[
      pull_dag_data(result1)$name == "x",
    ]$d_relationship[1]),
    "d-connected"
  )

  # Controlling for z should not d-separate x and y (other paths exist)
  result2 <- node_dconnected(
    complex_dag,
    from = "x",
    to = "y",
    controlling_for = "z"
  )
  expect_equal(
    as.character(pull_dag_data(result2)[
      pull_dag_data(result2)$name == "x",
    ]$d_relationship[1]),
    "d-connected"
  )

  # Test d-separation - x and v are d-separated without controlling
  result3 <- node_dseparated(
    complex_dag,
    from = "x",
    to = "v"
  )
  result3_df <- pull_dag_data(result3)
  x_result3 <- result3_df[result3_df$name == "x", ]
  expect_equal(
    as.character(x_result3$d_relationship[1]),
    "d-separated"
  )
})

test_that("d-relationship functions handle edge cases", {
  # Self-loop should error during dagify
  expect_ggdag_error(
    dagify(x ~ x)
  )

  # Disconnected nodes
  disconnected_dag <- dagify(
    y ~ x,
    w ~ z
  )
  result <- node_dconnected(disconnected_dag, from = "x", to = "z")
  expect_equal(
    as.character(
      pull_dag_data(result)[pull_dag_data(result)$name == "x", ]$d_relationship
    ),
    "d-separated"
  )

  # Empty controlling_for
  dag <- dagify(y ~ x)
  result1 <- node_dconnected(
    dag,
    from = "x",
    to = "y",
    controlling_for = character(0)
  )
  result2 <- node_dconnected(dag, from = "x", to = "y", controlling_for = NULL)
  expect_equal(
    pull_dag_data(result1)$d_relationship,
    pull_dag_data(result2)$d_relationship
  )
})

test_that("deprecated parameters show warnings", {
  dag <- dagify(y ~ x)

  expect_warning(
    ggdag_drelationship(dag, "x", "y", node = "deprecated"),
    "deprecated"
  )

  expect_warning(
    ggdag_drelationship(dag, "x", "y", stylized = "deprecated"),
    "deprecated"
  )
})
