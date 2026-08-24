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
    0.5
  )

  butterfly_bias_dag <- dagify(
    m ~ a + b,
    x ~ a + m,
    y ~ b + x + m,
    exposure = "x",
    outcome = "y",
    coords = coords_butterfly
  )

  p1 <- confounder_triangle_dag |>
    ggdag_paths(from = "x", to = "y")

  p2 <- butterfly_bias_dag |>
    ggdag_paths_fan()

  p3 <- confounder_triangle_dag |>
    ggdag_paths(from = "x", to = "y", shadow = FALSE)

  p4 <- butterfly_bias_dag |>
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
    outcome = "Outcome",
    coords = list(
      x = c(Treatment = 1, Age = 2, Censoring = 1.5, Outcome = 3),
      y = c(Treatment = 0, Age = 1, Censoring = -0.5, Outcome = 0)
    )
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

test_that("dag_paths() includes path_type classification", {
  # Simple confounder triangle: y ~ x + z, x ~ z
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  result <- dag_paths(dag, from = "x", to = "y")
  dag_data <- pull_dag_data(result)

  # Check that path_type column exists
  expect_true("path_type" %in% names(dag_data))

  # There should be 2 paths: x -> y (direct) and x <- z -> y (backdoor)
  # Check path types
  path_types <- unique(dag_data$path_type[!is.na(dag_data$path_type)])
  expect_setequal(path_types, c("direct", "backdoor"))

  # Each path should have consistent path_type within its set
  path_summary <- dag_data |>
    dplyr::filter(!is.na(path_type)) |>
    dplyr::group_by(set) |>
    dplyr::summarise(
      unique_types = length(unique(path_type)),
      path_type = unique(path_type)[1]
    )

  expect_true(
    all(path_summary$unique_types == 1),
    "Each path set should have only one path_type"
  )

  # Verify path classifications are correct
  # Set 1 should be the direct path (x -> y)
  # Set 2 should be the backdoor path (x <- z -> y)
  direct_sets <- path_summary$set[path_summary$path_type == "direct"]
  backdoor_sets <- path_summary$set[path_summary$path_type == "backdoor"]

  expect_equal(length(direct_sets), 1)
  expect_equal(length(backdoor_sets), 1)
})

test_that("dag_paths() handles no paths with path_type column", {
  # Create a DAG with no paths between exposure and outcome
  dag <- dagify(
    y ~ z,
    x ~ w,
    exposure = "x",
    outcome = "y"
  )

  result <- dag_paths(dag, from = "x", to = "y")
  dag_data <- pull_dag_data(result)

  # Check that path_type column exists but is all NA
  expect_true("path_type" %in% names(dag_data))
  expect_true(all(is.na(dag_data$path_type)))
})

test_that("edge_backdoor() classifies edges correctly", {
  # Create a DAG with clear backdoor and direct paths
  dag <- dagify(
    y ~ x + z + m,
    x ~ z,
    m ~ x,
    exposure = "x",
    outcome = "y"
  )

  result <- edge_backdoor(dag, from = "x", to = "y")
  dag_data <- pull_dag_data(result)

  # Check that new columns exist
  expect_true("path_type" %in% names(dag_data))
  expect_true("open" %in% names(dag_data))

  # Check specific edge classifications
  # x -> y should be direct
  x_to_y <- dag_data |>
    dplyr::filter(name == "x", to == "y") |>
    dplyr::pull(path_type)
  expect_equal(x_to_y, "direct")

  # z -> x should be backdoor (part of x <- z -> y)
  z_to_x <- dag_data |>
    dplyr::filter(name == "z", to == "x") |>
    dplyr::pull(path_type)
  expect_equal(z_to_x, "backdoor")

  # z -> y should be backdoor
  z_to_y <- dag_data |>
    dplyr::filter(name == "z", to == "y") |>
    dplyr::pull(path_type)
  expect_equal(z_to_y, "backdoor")

  # x -> m should be direct (part of x -> m -> y)
  x_to_m <- dag_data |>
    dplyr::filter(name == "x", to == "m") |>
    dplyr::pull(path_type)
  expect_equal(x_to_m, "direct")

  # m -> y should be direct
  m_to_y <- dag_data |>
    dplyr::filter(name == "m", to == "y") |>
    dplyr::pull(path_type)
  expect_equal(m_to_y, "direct")
})

test_that("edge_backdoor() handles closed paths correctly", {
  # Create a DAG with a collider that blocks a path
  dag <- dagify(
    y ~ x + c,
    c ~ z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  # Test with open_only = TRUE (default)
  result_open <- edge_backdoor(dag, from = "x", to = "y", adjust_for = "c")
  dag_data_open <- pull_dag_data(result_open)

  # With c adjusted, x <- z -> c <- y path is closed
  # Only x -> y is open
  open_edges <- dag_data_open |>
    dplyr::filter(open)

  expect_true(all(open_edges$path_type == "direct"))

  # Test with open_only = FALSE
  result_all <- edge_backdoor(
    dag,
    from = "x",
    to = "y",
    adjust_for = "c",
    open_only = FALSE
  )
  dag_data_all <- pull_dag_data(result_all)

  # Should have both open and closed path information
  expect_true(any(!dag_data_all$open, na.rm = TRUE))
  # Edges that are on paths between x and y should have path_type
  edges_on_paths <- dag_data_all |>
    dplyr::filter(!is.na(path_type))
  expect_true(nrow(edges_on_paths) > 0)
})

test_that("edge_backdoor() handles no paths gracefully", {
  # Disconnected DAG
  dag <- dagify(
    y ~ z,
    x ~ w,
    exposure = "x",
    outcome = "y"
  )

  result <- edge_backdoor(dag, from = "x", to = "y")
  dag_data <- pull_dag_data(result)

  # Should have columns but all NA
  expect_true("path_type" %in% names(dag_data))
  expect_true("open" %in% names(dag_data))

  expect_true(all(is.na(dag_data$path_type)))
  expect_true(all(is.na(dag_data$open)))
})

test_that("edge_backdoor() requires exposure and outcome", {
  dag <- dagify(y ~ x + z, x ~ z)

  # Should error without exposure and outcome
  expect_error(
    edge_backdoor(dag),
    class = "ggdag_missing_error"
  )

  # Should work with explicit from and to
  expect_no_error(edge_backdoor(dag, from = "x", to = "y"))
})

test_that("edge_backdoor() correctly classifies edges appearing on both paths", {
  # Create DAG where v2 -> v3 appears on both direct and backdoor paths
  dag <- dagify(
    v3 ~ v1 + v2,
    v2 ~ v1 + v4,
    v1 ~ v4,
    exposure = "v1",
    outcome = "v3"
  )

  result <- edge_backdoor(dag)
  dag_data <- pull_dag_data(result)

  # v2 -> v3 should be classified as "both"
  # It appears on direct path: v1 -> v2 -> v3
  # And on backdoor path: v1 <- v4 -> v2 -> v3
  v2_to_v3 <- dag_data |>
    dplyr::filter(name == "v2", to == "v3") |>
    dplyr::pull(path_type)
  expect_equal(v2_to_v3, "both")

  # v1 -> v2 should be "direct" (only on direct path)
  v1_to_v2 <- dag_data |>
    dplyr::filter(name == "v1", to == "v2") |>
    dplyr::pull(path_type)
  expect_equal(v1_to_v2, "direct")

  # v4 -> v1 should be "backdoor" (only on backdoor path)
  v4_to_v1 <- dag_data |>
    dplyr::filter(name == "v4", to == "v1") |>
    dplyr::pull(path_type)
  expect_equal(v4_to_v1, "backdoor")

  # v4 -> v2 should be "backdoor" (only on backdoor path)
  v4_to_v2 <- dag_data |>
    dplyr::filter(name == "v4", to == "v2") |>
    dplyr::pull(path_type)
  expect_equal(v4_to_v2, "backdoor")
})

test_that("dag_paths() marks the exposure once when a path leaves it", {
  # Both open paths (x -> y and x -> m -> y) leave x through one of its two
  # children, so no path needs a synthetic exposure node row.
  dag <- dagify(
    y ~ x + m,
    m ~ x,
    exposure = "x",
    outcome = "y"
  )

  base_rows <- nrow(pull_dag_data(tidy_dagitty(dag)))
  path_data <- pull_dag_data(dag_paths(dag))

  rows_per_set <- dplyr::count(path_data, set)
  expect_equal(nrow(rows_per_set), 2)
  expect_equal(rows_per_set$n, rep(base_rows, 2))

  exposure_rows <- path_data |>
    dplyr::filter(name == "x", path == "open path") |>
    dplyr::count(set)
  expect_equal(exposure_rows$n, rep(1L, 2))
})

test_that("dag_paths() marks the outcome once when it is an edge source", {
  # Conditioning on m opens the collider path x -> m <- y, on which the outcome
  # is the source of an edge, so it already has a marked row.
  dag <- dagify(
    m ~ x + y,
    exposure = "x",
    outcome = "y"
  )

  base_rows <- nrow(pull_dag_data(tidy_dagitty(dag)))
  path_data <- pull_dag_data(dag_paths(dag, adjust_for = "m"))

  expect_equal(dplyr::n_distinct(path_data$set), 1)
  expect_equal(nrow(path_data), base_rows)

  outcome_rows <- path_data |>
    dplyr::filter(name == "y", path == "open path")
  expect_equal(nrow(outcome_rows), 1)
  expect_equal(outcome_rows$to, "m")
})

test_that("dag_paths() keeps parallel directed and bidirected edges distinct", {
  # dagitty enumerates x -> y and x <-> y as two separate open paths
  dag <- dagify(
    y ~ x,
    x ~ ~y,
    exposure = "x",
    outcome = "y"
  )

  path_data <- pull_dag_data(dag_paths(dag))
  marked_edges <- path_data |>
    dplyr::filter(!is.na(path), !is.na(to))

  expect_equal(nrow(marked_edges), 2)
  expect_equal(dplyr::count(marked_edges, set)$n, rep(1L, 2))

  direct_edges <- dplyr::filter(marked_edges, path_type == "direct")
  expect_equal(as.character(direct_edges$direction), "->")

  backdoor_edges <- dplyr::filter(marked_edges, path_type == "backdoor")
  expect_equal(as.character(backdoor_edges$direction), "<->")
})

test_that("edge_backdoor() classifies parallel edges independently", {
  dag <- dagify(
    y ~ x,
    x ~ ~y,
    exposure = "x",
    outcome = "y"
  )

  edge_data <- pull_dag_data(edge_backdoor(dag)) |>
    dplyr::filter(!is.na(to))

  directed_edge <- dplyr::filter(edge_data, direction == "->")
  expect_equal(directed_edge$path_type, "direct")

  bidirected_edge <- dplyr::filter(edge_data, direction == "<->")
  expect_equal(bidirected_edge$path_type, "backdoor")

  expect_false(any(edge_data$path_type == "both", na.rm = TRUE))
})

test_that("dag_paths() errors informatively without endpoints", {
  dag <- dagify(y ~ x + z, x ~ z)

  expect_error(dag_paths(dag), class = "ggdag_missing_error")
  expect_ggdag_error(dag_paths(dag))
})

test_that("dag_paths() respects the directed argument", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  path_data <- pull_dag_data(dag_paths(dag, directed = TRUE))
  marked <- dplyr::filter(path_data, !is.na(path))

  expect_equal(unique(marked$set), "1")
  expect_equal(unique(marked$path_type), "direct")

  built <- ggplot2::ggplot_build(ggdag_paths(dag, directed = TRUE))
  expect_equal(nrow(built$layout$layout), 1)
})

test_that("path functions error on DAGs with several exposures or outcomes", {
  multi_exposure <- dagify(
    y ~ x1 + x2,
    x1 ~ z,
    x2 ~ z,
    exposure = c("x1", "x2"),
    outcome = "y"
  )

  expect_error(dag_paths(multi_exposure), class = "ggdag_error")
  expect_error(ggdag_paths(multi_exposure), class = "ggdag_error")
  expect_error(edge_backdoor(multi_exposure), class = "ggdag_error")

  multi_outcome <- dagify(
    y1 ~ x,
    y2 ~ x,
    exposure = "x",
    outcome = c("y1", "y2")
  )

  expect_error(dag_paths(multi_outcome), class = "ggdag_error")
  expect_error(edge_backdoor(multi_outcome), class = "ggdag_error")
})

test_that("dag_paths() labels collider paths as neither direct nor backdoor", {
  dag <- dagify(
    y ~ x,
    z ~ x + y,
    exposure = "x",
    outcome = "y"
  )

  path_data <- pull_dag_data(dag_paths(dag, adjust_for = "z"))
  path_types <- path_data |>
    dplyr::filter(path == "open path") |>
    dplyr::distinct(set, path_type)

  expect_equal(path_types$path_type[path_types$set == "1"], "direct")
  expect_equal(path_types$path_type[path_types$set == "2"], "other")
  expect_false(any(path_types$path_type == "backdoor"))
})

test_that("edge_backdoor() labels collider path edges as other", {
  dag <- dagify(
    y ~ x,
    z ~ x + y,
    exposure = "x",
    outcome = "y"
  )

  edge_data <- pull_dag_data(edge_backdoor(dag, adjust_for = "z"))

  expect_equal(
    dplyr::filter(edge_data, name == "x", to == "y")$path_type,
    "direct"
  )
  expect_equal(
    dplyr::filter(edge_data, name == "x", to == "z")$path_type,
    "other"
  )
  expect_equal(
    dplyr::filter(edge_data, name == "y", to == "z")$path_type,
    "other"
  )
  expect_false(any(edge_data$path_type == "backdoor", na.rm = TRUE))
})

test_that("edge_backdoor() still labels true backdoor edges as backdoor", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  edge_data <- pull_dag_data(edge_backdoor(dag))

  expect_equal(
    dplyr::filter(edge_data, name == "z", to == "x")$path_type,
    "backdoor"
  )
  expect_equal(
    dplyr::filter(edge_data, name == "z", to == "y")$path_type,
    "backdoor"
  )
  expect_equal(
    dplyr::filter(edge_data, name == "x", to == "y")$path_type,
    "direct"
  )
})

test_that("dag_paths() is idempotent", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  once <- dag_paths(dag)
  twice <- dag_paths(once)

  expect_equal(pull_dag_data(twice), pull_dag_data(once))
})

test_that("ggdag_paths() draws only causal paths when directed = TRUE", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    coords = list(
      x = c(x = 0, y = 2, z = 1),
      y = c(x = 0, y = 0, z = 1)
    )
  )

  expect_doppelganger(
    "ggdag_paths() with directed paths only",
    ggdag_paths(dag, directed = TRUE)
  )
})

test_that("ggdag_paths() draws collider paths with their own path type", {
  dag <- dagify(
    y ~ x,
    z ~ x + y,
    exposure = "x",
    outcome = "y",
    coords = list(
      x = c(x = 0, y = 2, z = 1),
      y = c(x = 0, y = 0, z = 1)
    )
  )

  expect_doppelganger(
    "ggdag_paths() with an open collider path",
    ggdag_paths(dag, adjust_for = "z")
  )
})

test_that("ggdag_paths() draws parallel edges on their own paths", {
  dag <- dagify(
    y ~ x,
    x ~ ~y,
    exposure = "x",
    outcome = "y",
    coords = list(
      x = c(x = 0, y = 2),
      y = c(x = 0, y = 0)
    )
  )

  expect_doppelganger(
    "ggdag_paths() with parallel directed and bidirected edges",
    ggdag_paths(dag)
  )
})

test_that("ggdag_paths() draws one node per variable on causal paths", {
  dag <- dagify(
    y ~ x + m,
    m ~ x,
    exposure = "x",
    outcome = "y",
    coords = list(
      x = c(x = 0, m = 1, y = 2),
      y = c(x = 0, m = 1, y = 0)
    )
  )

  expect_doppelganger("ggdag_paths() with a mediator", ggdag_paths(dag))
})

test_that("ggdag_paths() sizes the edge layers it builds itself", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  p <- ggdag_paths(
    dag,
    from = "x",
    to = "y",
    size = 2,
    edge_cap = 3,
    edge_width = 2,
    arrow_length = 20
  )

  expect_equal(edge_cap_radii(p), 6)
  expect_equal(edge_widths(p), 4)
  expect_equal(edge_arrow_lengths(p), 40)
})

test_that("ggdag_paths() honors edge_type in the edge layers it builds itself", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  stats <- layer_stat_classes(
    ggdag_paths(dag, from = "x", to = "y", edge_type = "arc")
  )

  expect_true("StatEdgeArc" %in% stats)
  expect_false("StatEdgeLink" %in% stats)
})

test_that("ggdag_paths_fan() sizes the edge layer it builds itself", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  p <- ggdag_paths_fan(
    dag,
    from = "x",
    to = "y",
    size = 2,
    edge_cap = 3,
    edge_width = 2,
    arrow_length = 20
  )

  expect_equal(edge_cap_radii(p), 6)
  expect_equal(edge_widths(p), 4)
  expect_equal(edge_arrow_lengths(p), 40)
})

test_that("ggdag_paths_fan() accepts edge_engine and key_glyph", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  expect_s3_class(
    ggdag_paths_fan(dag, from = "x", to = "y", key_glyph = draw_key_dag_point),
    "gg"
  )

  skip_if_not_installed("ggarrow")
  p <- ggdag_paths_fan(dag, from = "x", to = "y", edge_engine = "ggarrow")
  expect_s3_class(p, "gg")
  expect_true(uses_ggarrow_edges(p))
})

test_that("ggdag_paths() draws the edge sizes it is given", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  expect_doppelganger(
    "ggdag_paths() with wide capped edges",
    ggdag_paths(dag, from = "x", to = "y", edge_cap = 16, edge_width = 2)
  )
})

test_that("ggdag_paths() rejects an edge type it cannot draw", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  # `edge_type_switch()` answers an unknown type with `NULL`, which would
  # otherwise surface as "attempt to apply non-function"
  expect_error(
    ggdag_paths(dag, from = "x", to = "y", edge_type = "bogus"),
    "should be one of"
  )
  expect_s3_class(
    ggdag_paths(dag, from = "x", to = "y", edge_type = "diagonal"),
    "gg"
  )
})
