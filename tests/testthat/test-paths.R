set.seed(1234)

test_that("DAG paths are identified and drawn correctly", {
  coords_confounder <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    0,
    0,
    "y",
    2,
    0,
    "z",
    1,
    1
  )

  confounder_triangle_dag <- dagify(
    x ~ z,
    y ~ x + z,
    exposure = "x",
    outcome = "y",
    coords = coords_confounder
  )

  coords_butterfly <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    0,
    0,
    "y",
    2,
    0,
    "a",
    0,
    1,
    "b",
    2,
    1,
    "m",
    1,
    .5
  )

  butterfly_bias_dag <- dagify(
    m ~ a + b,
    x ~ a + m,
    y ~ b + x + m,
    exposure = "x",
    outcome = "y",
    coords = coords_butterfly
  )

  p1 <- confounder_triangle_dag %>%
    ggdag_paths(from = "x", to = "y")

  p2 <- butterfly_bias_dag %>%
    ggdag_paths_fan()

  p3 <- confounder_triangle_dag %>%
    ggdag_paths(from = "x", to = "y", shadow = FALSE)

  p4 <- butterfly_bias_dag %>%
    ggdag_paths_fan(shadow = FALSE)

  expect_doppelganger("ggdag_paths() draws 2 open paths", p1)
  expect_doppelganger("ggdag_paths_fan() draws 4 open paths", p2)
  expect_doppelganger("ggdag_paths() draws 2 open paths without shadows", p3)
  expect_doppelganger(
    "ggdag_paths_fan() draws 4 open paths without shadows",
    p4
  )
})

test_that("dag_paths() handles no open paths correctly (issue #180)", {
  # Create a DAG where Treatment and Outcome are d-separated by a collider
  dag <- dagify(
    Censoring ~ Treatment + Age,
    Outcome ~ Age,
    exposure = "Treatment",
    outcome = "Outcome"
  )

  # This should not throw an error
  expect_no_error(result <- dag_paths(dag, from = "Treatment", to = "Outcome"))

  # Check that the result is a valid tidy_dagitty object
  expect_true(is.tidy_dagitty(result))

  # Check that the DAG structure is preserved
  dag_data <- pull_dag_data(result)
  expect_true("Treatment" %in% dag_data$name)
  expect_true("Outcome" %in% dag_data$name)
  expect_true("Censoring" %in% dag_data$name)
  expect_true("Age" %in% dag_data$name)

  # There should be a path column with all NA values
  expect_true("path" %in% names(dag_data))
  expect_true(all(is.na(dag_data$path)))

  # Verify using dagitty that paths are indeed separated
  expect_true(dagitty::dseparated(dag, "Treatment", "Outcome"))

  # Test that ggdag_paths() works with no open paths
  p_shadow <- ggdag_paths(dag, shadow = TRUE)
  p_no_shadow <- ggdag_paths(dag, shadow = FALSE)

  expect_doppelganger("ggdag_paths() with no open paths and shadow", p_shadow)
  expect_doppelganger("ggdag_paths() with no open paths no shadow", p_no_shadow)
})

test_that("ggdag_paths respects use_edges parameter", {
  # Simple DAG for paths testing: y ~ x + z, x ~ z
  # This should have 3 edges total: y <- x, y <- z, x <- z
  # With faceting (2 panels), shadow=TRUE shows 6 edge-panel combinations
  dag <- dagify(y ~ x + z, x ~ z)

  # Test with use_edges = TRUE and shadow = TRUE (default)
  p_with_edges <- ggdag_paths(
    dag,
    from = "x",
    to = "y",
    use_edges = TRUE,
    shadow = TRUE
  )
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  expect_equal(
    analysis_with$total_edges,
    6,
    info = "Should have exactly 6 edge-panel combinations when use_edges = TRUE with shadow"
  )
  expect_gt(
    analysis_with$edge_layers,
    0,
    "Should have at least 1 edge layer when use_edges = TRUE"
  )

  # Test with use_edges = TRUE and shadow = FALSE
  p_with_edges_no_shadow <- ggdag_paths(
    dag,
    from = "x",
    to = "y",
    use_edges = TRUE,
    shadow = FALSE
  )
  analysis_with_no_shadow <- analyze_plot_edges(p_with_edges_no_shadow)

  expect_true(
    analysis_with_no_shadow$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  expect_equal(
    analysis_with_no_shadow$total_edges,
    3,
    info = "Should have exactly 3 edge-panel combinations when use_edges = TRUE without shadow"
  )
  expect_gt(
    analysis_with_no_shadow$edge_layers,
    0,
    "Should have at least 1 edge layer when use_edges = TRUE"
  )

  # Test with use_edges = FALSE (should have NO edges)
  p_without_edges <- ggdag_paths(dag, from = "x", to = "y", use_edges = FALSE)
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

test_that("ggdag_paths handles bidirected edges correctly", {
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

  # Analyze paths to calculate expected edge counts
  paths_result <- dagitty::paths(dag, from = "x", to = "y", limit = 100)
  n_open_paths <- sum(paths_result$open)
  expect_equal(n_open_paths, 8, info = "Should have 8 open paths")

  # Count edges in base DAG
  dag_edges <- dagitty::edges(dag)
  n_dag_edges <- nrow(dag_edges)
  expect_equal(n_dag_edges, 11, info = "Base DAG should have 11 edges")

  # Count edges in each open path
  open_paths_idx <- which(paths_result$open)
  edges_in_open_paths <- 0
  for (idx in open_paths_idx) {
    path_str <- paths_result$paths[idx]
    n_arrows <- lengths(regmatches(path_str, gregexpr("<-|->|<->", path_str)))
    edges_in_open_paths <- edges_in_open_paths + n_arrows
  }
  expect_equal(
    edges_in_open_paths,
    26,
    info = "Sum of edges in open paths should be 26"
  )

  # Test with shadow = TRUE
  # Expected: total edges in DAG × number of open paths = 11 × 8 = 88
  p_shadow <- ggdag_paths(
    dag,
    from = "x",
    to = "y",
    shadow = TRUE,
    use_edges = TRUE
  )
  analysis_shadow <- analyze_plot_edges(p_shadow)

  expect_true(
    analysis_shadow$has_edge_layers,
    "Should have edge layers with shadow = TRUE"
  )
  expect_equal(
    analysis_shadow$total_edges,
    88,
    info = "With shadow: should have 88 edge-panel combinations (11 edges × 8 paths)"
  )

  # Test with shadow = FALSE
  # Expected: sum of edges in open paths = 26
  p_no_shadow <- ggdag_paths(
    dag,
    from = "x",
    to = "y",
    shadow = FALSE,
    use_edges = TRUE
  )
  analysis_no_shadow <- analyze_plot_edges(p_no_shadow)

  expect_true(
    analysis_no_shadow$has_edge_layers,
    "Should have edge layers with shadow = FALSE"
  )
  expect_equal(
    analysis_no_shadow$total_edges,
    26,
    info = "Without shadow: should have 26 edge-panel combinations (sum of edges in paths)"
  )

  # Test with use_edges = FALSE
  p_no_edges <- ggdag_paths(dag, from = "x", to = "y", use_edges = FALSE)
  analysis_no_edges <- analyze_plot_edges(p_no_edges)

  expect_false(
    analysis_no_edges$has_edge_layers,
    "Should have NO edge layers when use_edges = FALSE"
  )
  expect_equal(
    analysis_no_edges$total_edges,
    0,
    info = "Should have 0 edges when use_edges = FALSE"
  )
})

test_that("ggdag_paths_fan respects use_edges parameter", {
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

  # Analyze paths to calculate expected edge counts
  paths_result <- dagitty::paths(dag, from = "x", to = "y", limit = 100)
  n_open_paths <- sum(paths_result$open)
  expect_equal(n_open_paths, 8, info = "Should have 8 open paths")

  # Count edges in base DAG
  dag_edges <- dagitty::edges(dag)
  n_dag_edges <- nrow(dag_edges)
  expect_equal(n_dag_edges, 11, info = "Base DAG should have 11 edges")

  # For ggdag_paths_fan with shadow=TRUE (default), it shows all edges for each path set
  # Unlike ggdag_paths which facets, paths_fan colors by set and fans out the edges
  # So we expect n_dag_edges × n_open_paths when shadow=TRUE

  # Test with use_edges = TRUE and shadow = TRUE (default)
  p_with_edges <- ggdag_paths_fan(
    dag,
    from = "x",
    to = "y",
    use_edges = TRUE,
    shadow = TRUE
  )
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  expect_equal(
    analysis_with$total_edges,
    n_dag_edges * n_open_paths,
    info = paste(
      "Should have",
      n_dag_edges * n_open_paths,
      "edges with shadow=TRUE (11 × 8)"
    )
  )

  # Test with use_edges = TRUE and shadow = FALSE
  # Count edges in each open path (same calculation as before)
  open_paths_idx <- which(paths_result$open)
  edges_in_open_paths <- 0
  for (idx in open_paths_idx) {
    path_str <- paths_result$paths[idx]
    n_arrows <- lengths(regmatches(path_str, gregexpr("<-|->|<->", path_str)))
    edges_in_open_paths <- edges_in_open_paths + n_arrows
  }

  p_with_edges_no_shadow <- ggdag_paths_fan(
    dag,
    from = "x",
    to = "y",
    use_edges = TRUE,
    shadow = FALSE
  )
  analysis_no_shadow <- analyze_plot_edges(p_with_edges_no_shadow)

  expect_true(
    analysis_no_shadow$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  # With shadow=FALSE, only edges in open paths are shown
  # But since paths_fan doesn't deduplicate overlapping edges like paths does,
  # we expect the sum of all edges in all paths
  expect_equal(
    analysis_no_shadow$total_edges,
    edges_in_open_paths,
    info = paste("Should have", edges_in_open_paths, "edges without shadow")
  )

  # Test with use_edges = FALSE
  p_without_edges <- ggdag_paths_fan(
    dag,
    from = "x",
    to = "y",
    use_edges = FALSE
  )
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
