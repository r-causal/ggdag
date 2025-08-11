test_that("adjustment sets drawn correctly", {
  p1 <- ggdag_adjustment_set(test_dag)
  expect_doppelganger("ggdag_adjustment_set() renders", p1)

  # Add edge count test
  n_edges <- count_dag_edges(test_dag)
  n_sets <- count_adjustment_sets(test_dag)
  expect_edge_count(p1, n_edges * n_sets, "ggdag_adjustment_set default")

  p2 <- ggdag_adjustment_set(test_dag, shadow = FALSE)
  expect_doppelganger("ggdag_adjustment_set() renders without shadows", p2)

  # Shadow parameter doesn't affect edge count for adjustment sets
  expect_edge_count(p2, n_edges * n_sets, "ggdag_adjustment_set shadow=FALSE")
})

test_that("adjustment sets drawn correctly with width set low", {
  withr::with_options(
    list(width = 20),
    {
      p <- ggdag_adjustment_set(test_dag)
      expect_doppelganger("ggdag_adjustment_set() renders with low width", p)

      # Add edge count test - width doesn't affect edge count
      n_edges <- count_dag_edges(test_dag)
      n_sets <- count_adjustment_sets(test_dag)
      expect_edge_count(
        p,
        n_edges * n_sets,
        "ggdag_adjustment_set with low width"
      )
    }
  )
})

# Unit tests for dag_adjustment_sets
test_that("dag_adjustment_sets identifies correct adjustment sets", {
  # Simple confounding
  simple_dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  adj_sets <- dag_adjustment_sets(simple_dag)
  dag_data <- pull_dag_data(adj_sets)

  # Check that z is marked as adjusted
  z_adjusted <- dag_data[dag_data$name == "z", "adjusted"]
  expect_true(all(z_adjusted == "adjusted"))

  # Check that x and y are not adjusted
  x_adjusted <- dag_data[dag_data$name == "x", "adjusted"]
  y_adjusted <- dag_data[dag_data$name == "y", "adjusted"]
  expect_true(all(x_adjusted == "unadjusted"))
  expect_true(all(y_adjusted == "unadjusted"))

  # Check set column is created
  expect_true("set" %in% names(dag_data))
})

test_that("ggdag_adjustment_set respects use_edges parameter", {
  # Create a DAG with adjustment set: y ~ x + z, x ~ z
  # This should have 3 edges total: y <- x, y <- z, x <- z
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  # Test with use_edges = TRUE (should have edges)
  p_with_edges <- ggdag_adjustment_set(dag, use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  expect_equal(
    analysis_with$total_edges,
    3,
    info = "Should have exactly 3 edges when use_edges = TRUE"
  )
  expect_gt(
    analysis_with$edge_layers,
    0,
    "Should have at least 1 edge layer when use_edges = TRUE"
  )

  # Test with use_edges = FALSE (should have NO edges)
  p_without_edges <- ggdag_adjustment_set(dag, use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without_edges)

  expect_false(
    analysis_without$has_edge_layers,
    paste(
      "Should have NO edge layers when use_edges = FALSE, but found:",
      analysis_without$edge_layers
    )
  )
  expect_equal(
    analysis_without$total_edges,
    0,
    info = paste(
      "Should have 0 edges when use_edges = FALSE, but found:",
      analysis_without$total_edges
    )
  )
  expect_equal(
    analysis_without$edge_layers,
    0,
    info = "Should have 0 edge layers when use_edges = FALSE"
  )
})

test_that("ggdag_adjust respects use_edges parameter", {
  # Create a DAG and control for z: y ~ x + z, x ~ z
  # This should have 3 edges total: y <- x, y <- z, x <- z
  dag <- dagify(y ~ x + z, x ~ z)

  # Test with use_edges = TRUE (should have edges)
  p_with_edges <- ggdag_adjust(dag, var = "z", use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  expect_equal(
    analysis_with$total_edges,
    3,
    info = "Should have exactly 3 edges when use_edges = TRUE"
  )
  expect_gt(
    analysis_with$edge_layers,
    0,
    "Should have at least 1 edge layer when use_edges = TRUE"
  )

  # Test with use_edges = FALSE (should have NO edges)
  p_without_edges <- ggdag_adjust(dag, var = "z", use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without_edges)

  expect_false(
    analysis_without$has_edge_layers,
    paste(
      "Should have NO edge layers when use_edges = FALSE, but found:",
      analysis_without$edge_layers
    )
  )
  expect_equal(
    analysis_without$total_edges,
    0,
    info = paste(
      "Should have 0 edges when use_edges = FALSE, but found:",
      analysis_without$total_edges
    )
  )
  expect_equal(
    analysis_without$edge_layers,
    0,
    info = "Should have 0 edge layers when use_edges = FALSE"
  )
})

test_that("ggdag_adjustment_set handles complex DAG with bidirected edges", {
  # Complex DAG with bidirected edge from ?ggdag
  dag <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2, # bidirected edge
    exposure = "x",
    outcome = "y"
  )

  # Count edges in base DAG
  dag_edges <- dagitty::edges(dag)
  n_dag_edges <- nrow(dag_edges)
  expect_equal(n_dag_edges, 11, info = "Base DAG should have 11 edges")

  # Get adjustment sets to know how many facets we'll have
  adj_sets <- dag_adjustment_sets(dag)
  dag_data <- pull_dag_data(adj_sets)
  n_sets <- length(unique(dag_data$set))

  # Test with use_edges = TRUE
  # Expected: n_edges * n_sets = 11 * n_sets
  p_with_edges <- ggdag_adjustment_set(dag, use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  expect_equal(
    analysis_with$total_edges,
    n_dag_edges * n_sets,
    info = paste("Should have", n_dag_edges * n_sets, "edge-panel combinations")
  )

  # Test with use_edges = FALSE
  p_without_edges <- ggdag_adjustment_set(dag, use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without_edges)

  expect_false(
    analysis_without$has_edge_layers,
    "Should have NO edge layers when use_edges = FALSE"
  )
  expect_equal(
    analysis_without$total_edges,
    0,
    info = "Should have 0 edges when use_edges = FALSE"
  )
})

test_that("ggdag_adjust handles complex DAG with bidirected edges", {
  # Complex DAG with bidirected edge from ?ggdag
  dag <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2, # bidirected edge
    exposure = "x",
    outcome = "y"
  )

  # Count edges in base DAG
  dag_edges <- dagitty::edges(dag)
  n_dag_edges <- nrow(dag_edges)
  expect_equal(n_dag_edges, 11, info = "Base DAG should have 11 edges")

  # Test adjusting for w1
  p_with_edges <- ggdag_adjust(dag, var = "w1", use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  # ggdag_adjust doesn't create facets, so just n_dag_edges
  expect_equal(
    analysis_with$total_edges,
    n_dag_edges,
    info = "Should have exactly 11 edges when use_edges = TRUE"
  )

  # Test with use_edges = FALSE
  p_without_edges <- ggdag_adjust(dag, var = "w1", use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without_edges)

  expect_false(
    analysis_without$has_edge_layers,
    "Should have NO edge layers when use_edges = FALSE"
  )
  expect_equal(
    analysis_without$total_edges,
    0,
    info = "Should have 0 edges when use_edges = FALSE"
  )
})

test_that("dag_adjustment_sets handles multiple adjustment sets", {
  # DAG with multiple valid adjustment sets
  multi_dag <- dagify(
    y ~ x + a + b,
    x ~ a + b,
    a ~ c,
    b ~ c,
    exposure = "x",
    outcome = "y"
  )

  # Get the dagitty result directly
  dagitty_sets <- dagitty::adjustmentSets(multi_dag)
  expected_n_sets <- length(dagitty_sets)

  adj_sets <- dag_adjustment_sets(multi_dag)
  dag_data <- pull_dag_data(adj_sets)

  # Should have same number of adjustment sets as dagitty
  n_sets <- length(unique(dag_data$set))
  expect_equal(n_sets, expected_n_sets)

  # Each set should be properly formatted
  expect_true(all(grepl("^\\{.*\\}$", unique(dag_data$set))))
})

test_that("dag_adjustment_sets handles no valid adjustment sets with warning", {
  # Create a DAG where backdoor paths cannot be closed
  # Simple confounding through latent variable
  no_adjust_dag <- dagify(
    y ~ x + u,
    x ~ u,
    exposure = "x",
    outcome = "y",
    latent = "u"
  )

  expect_warning(
    adj_sets <- dag_adjustment_sets(no_adjust_dag),
    "Failed to close backdoor paths"
  )

  dag_data <- pull_dag_data(adj_sets)
  # Should have special marker for no adjustment sets
  expect_true(any(grepl("No Way to Block", dag_data$set)))
})

test_that("dag_adjustment_sets handles unconditionally closed paths", {
  # No backdoor paths
  no_backdoor <- dagify(
    y ~ x,
    x ~ z,
    w ~ z,
    exposure = "x",
    outcome = "y"
  )

  adj_sets <- dag_adjustment_sets(no_backdoor)
  dag_data <- pull_dag_data(adj_sets)

  # Should indicate paths are unconditionally closed
  expect_true(any(grepl("Unconditionally Closed", dag_data$set)))
})

test_that("dag_adjustment_sets works without specified exposure/outcome", {
  # Should infer from DAG
  infer_dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  # Remove exposure/outcome from call
  adj_sets <- dag_adjustment_sets(infer_dag)
  expect_s3_class(adj_sets, "tidy_dagitty")

  # Should still find adjustment sets
  dag_data <- pull_dag_data(adj_sets)
  expect_true(any(dag_data$adjusted == "adjusted"))
})

test_that("dag_adjustment_sets passes additional arguments to adjustmentSets", {
  # Test with type argument
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  # Get canonical adjustment sets
  adj_sets <- dag_adjustment_sets(dag, type = "canonical")
  expect_s3_class(adj_sets, "tidy_dagitty")
})

# Unit tests for is_confounder
test_that("is_confounder correctly identifies confounders", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  # z is a confounder
  expect_true(is_confounder(dag, "z", "x", "y"))

  # x is not a confounder of itself and y
  expect_false(is_confounder(dag, "x", "x", "y"))

  # y is not a confounder
  expect_false(is_confounder(dag, "y", "x", "y"))
})

test_that("is_confounder handles direct parameter", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    z ~ w,
    exposure = "x",
    outcome = "y"
  )

  # z is a direct confounder
  expect_true(is_confounder(dag, "z", "x", "y", direct = TRUE))

  # w is not a direct confounder (affects through z)
  expect_false(is_confounder(dag, "w", "x", "y", direct = TRUE))

  # But w is an indirect confounder
  expect_true(is_confounder(dag, "w", "x", "y", direct = FALSE))
})

test_that("is_confounder works with tidy_dagitty objects", {
  dag <- dagify(
    y ~ x + z,
    x ~ z
  ) |>
    tidy_dagitty()

  expect_true(is_confounder(dag, "z", "x", "y"))
})

# Unit tests for control_for
test_that("control_for updates DAG with adjusted variables", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  controlled <- control_for(dag, "z")
  dag_data <- pull_dag_data(controlled)

  # z should be adjusted
  z_data <- dag_data[dag_data$name == "z", ]
  expect_true(all(z_data$adjusted == "adjusted"))

  # Others should be unadjusted
  other_data <- dag_data[dag_data$name != "z", ]
  expect_true(all(other_data$adjusted == "unadjusted"))
})

test_that("control_for handles multiple variables", {
  dag <- dagify(
    y ~ x + a + b,
    x ~ a + b
  )

  controlled <- control_for(dag, c("a", "b"))
  dag_data <- pull_dag_data(controlled)

  adjusted_vars <- dag_data[dag_data$name %in% c("a", "b"), ]
  expect_true(all(adjusted_vars$adjusted == "adjusted"))
})

test_that("control_for handles var with no matches", {
  dag <- dagify(y ~ x)

  # Should error when variable doesn't exist in DAG
  expect_error(
    control_for(dag, "z"),
    "is not a variable"
  )
})

# Unit tests for ggdag_adjust
test_that("ggdag_adjust creates plot with adjusted variables", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  p <- ggdag_adjust(dag, "z")
  expect_s3_class(p, "ggplot")

  # Check that the plot has the expected layers
  expect_true(length(p$layers) > 0)
})

test_that("ggdag_adjust works with different parameters", {
  dag <- dagify(
    y ~ x + z,
    x ~ z
  )

  # Test with collider_lines
  p1 <- ggdag_adjust(dag, "z", collider_lines = FALSE)
  expect_s3_class(p1, "ggplot")

  # Test with different variables to adjust
  p2 <- ggdag_adjust(dag, var = c("z"))
  expect_s3_class(p2, "ggplot")

  # Test with empty adjustment (no variables)
  p3 <- ggdag_adjust(dag, var = character(0))
  expect_s3_class(p3, "ggplot")
})

test_that("ggdag_adjust handles node styling", {
  dag <- dagify(
    y ~ x + z,
    x ~ z
  )

  # Test use_labels with new API
  expect_warning(
    p1 <- ggdag_adjust(dag, "z", use_labels = "label"),
    "use_labels.*must be a logical"
  )
  expect_s3_class(p1, "ggplot")

  # Test with text = FALSE (deprecated)
  expect_warning(
    p2 <- ggdag_adjust(dag, "z", text = FALSE),
    "text.*no longer accepts logicals"
  )
  expect_s3_class(p2, "ggplot")

  # Test with new API
  p3 <- ggdag_adjust(dag, "z", use_labels = TRUE)
  expect_s3_class(p3, "ggplot")
})

# Test extract_sets helper function
test_that("extract_sets processes adjustment sets correctly", {
  # Create mock adjustment sets
  sets <- list(c("a", "b"), c("c"), character(0))
  names(sets) <- NULL

  extracted <- extract_sets(sets)

  expect_length(extracted, 3)
  expect_equal(extracted[[1]], c("a", "b"))
  expect_equal(extracted[[2]], "c")
  expect_equal(extracted[[3]], "(Backdoor Paths Unconditionally Closed)")
})
