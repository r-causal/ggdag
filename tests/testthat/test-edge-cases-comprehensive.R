test_that("as_tidy_dagitty.list creates time-ordered DAGs", {
  # Create list of time points
  time_points <- list(
    t1 = c("a", "b"),
    t2 = c("c", "d"),
    t3 = c("e")
  )

  # Convert to tidy_dagitty
  result <- as_tidy_dagitty(time_points)
  expect_s3_class(result, "tidy_dagitty")

  # Check that edges go forward in time
  edges_df <- pull_dag_data(result)
  edge_rows <- edges_df[!is.na(edges_df$to), ]

  # t1 nodes should connect to t2 and t3
  a_edges <- edge_rows[edge_rows$name == "a", ]
  expect_true(all(a_edges$to %in% c("c", "d", "e")))

  # t2 nodes should connect to t3
  c_edges <- edge_rows[edge_rows$name == "c", ]
  expect_true(all(c_edges$to %in% c("e")))

  # Test with seed
  result_seed <- as_tidy_dagitty(time_points, seed = 123)
  expect_s3_class(result_seed, "tidy_dagitty")

  # Test with exposure and outcome
  result_exp <- as_tidy_dagitty(
    time_points,
    exposure = "a",
    outcome = "e"
  )
  dag <- pull_dag(result_exp)
  expect_equal(dagitty::exposures(dag), "a")
  expect_equal(dagitty::outcomes(dag), "e")

  # Test with latent variables
  result_latent <- as_tidy_dagitty(
    time_points,
    latent = c("b", "d")
  )
  dag_latent <- pull_dag(result_latent)
  expect_true(all(c("b", "d") %in% dagitty::latents(dag_latent)))
})

test_that("as_tidy_dagitty.data.frame with saturate works", {
  # Create a data frame of edges
  edges_df <- data.frame(
    name = c("x", "y"),
    to = c("y", "z"),
    x = c(0, 1),
    y = c(0, 0),
    xend = c(1, 2),
    yend = c(0, 0)
  )

  # Test with saturate = TRUE
  result <- as_tidy_dagitty(edges_df, saturate = TRUE)
  expect_s3_class(result, "tidy_dagitty")
})

test_that("edge cases in adjustment set functions", {
  # DAG with no adjustment sets needed
  dag <- dagify(y ~ x)

  # ggdag_adjustment_set - test it works
  p1 <- ggdag_adjustment_set(dag, exposure = "x", outcome = "y")
  expect_s3_class(p1, "gg")

  # Test ggdag_adjust deprecated parameters
  expect_warning(
    ggdag_adjust(dag, var = "x", node = "deprecated"),
    class = "lifecycle_warning_deprecated"
  )

  expect_warning(
    ggdag_adjust(dag, var = "x", stylized = "deprecated"),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("collider edge cases", {
  dag <- dagify(m ~ x + y)

  # Test activate_collider_paths
  tidy_dag <- tidy_dagitty(dag)
  result <- activate_collider_paths(
    tidy_dag,
    from = "x",
    to = "y",
    adjust_for = "m"
  )
  expect_s3_class(result, "tidy_dagitty")
})

test_that("dag_label edge cases", {
  # minor differences in old windows versions; not important so skip on windows
  skip_on_os("windows")

  dag <- dagify(y ~ x)
  tidy_dag <- tidy_dagitty(dag)

  # Test with NULL labels (should warn)
  expect_ggdag_warning(
    dag_label(tidy_dag, labels = NULL)
  )

  # Test when labels already exist
  labeled_dag <- dag_label(tidy_dag, labels = c(x = "X", y = "Y"))
  # Adding new labels should replace
  relabeled <- dag_label(labeled_dag, labels = c(x = "X2", y = "Y2"))
  relabeled_data <- pull_dag_data(relabeled)
  expect_equal(relabeled_data$label[relabeled_data$name == "x"], "X2")
})

test_that("dagify edge cases", {
  # Test with empty coords
  dag <- dagify(y ~ x, coords = list())
  expect_s3_class(dag, "dagitty")
})

test_that("dplyr_methods edge case", {
  dag <- dagify(y ~ x)
  tidy_dag <- tidy_dagitty(dag)

  # Just verify a regular dplyr method works
  result <- dplyr::mutate(tidy_dag, test = 1)
  expect_s3_class(result, "tidy_dagitty")
})

test_that("drelationship edge case", {
  dag <- dagify(y ~ x)
  tidy_dag <- tidy_dagitty(dag)

  # Test ggdag_dseparated with additional arguments
  p <- ggdag_dseparated(
    tidy_dag,
    from = "x",
    to = "y",
    controlling_for = character(0)
  )
  expect_s3_class(p, "gg")
})

test_that("equivalence functions work", {
  # Create two equivalent DAGs
  dag1 <- dagify(y ~ x + z, x ~ z)
  dag2 <- dagify(y ~ x + z, x ~ z)

  # Test node_equivalent_dags
  result <- node_equivalent_dags(dag1)
  expect_s3_class(result, "tidy_dagitty")

  # Test ggdag_equivalent_dags
  p1 <- ggdag_equivalent_dags(dag1)
  expect_s3_class(p1, "gg")

  # Test node_equivalent_class
  result2 <- node_equivalent_class(dag1)
  expect_s3_class(result2, "tidy_dagitty")
  # Check for the actual column name
  result2_data <- pull_dag_data(result2)
  expect_true(any(c("reversable", "reversible") %in% names(result2_data)))

  # Test ggdag_equivalent_class
  p2 <- ggdag_equivalent_class(dag1)
  expect_s3_class(p2, "gg")
})

test_that("geom_dag edge cases", {
  p <- ggplot(tidy_dagitty(dagify(y ~ x)))

  # Test geom_dag_edges_link
  p1 <- p + geom_dag_edges_link()
  expect_s3_class(p1, "gg")

  # Test deprecated scale_dag
  expect_warning(
    p + scale_dag(breaks = c("x", "y")),
    class = "deprecatedWarning"
  )

  expect_true(is_empty_or_null(NULL))
  expect_true(is_empty_or_null(character(0)))
  expect_false(is_empty_or_null("a"))
})

test_that("ggdag edge case", {
  dag <- dagify(y ~ x)

  # Test ggdag_classic
  p <- ggdag_classic(dag)
  expect_s3_class(p, "gg")
})

test_that("layout functions work", {
  dag <- dagify(
    y ~ x + z,
    x ~ z
  )

  # Test layout generation through tidy_dagitty
  tidy_dag <- tidy_dagitty(dag, layout = "nicely")
  expect_s3_class(tidy_dag, "tidy_dagitty")

  # Test with specific layout
  tidy_dag2 <- tidy_dagitty(dag, layout = "fr")
  expect_s3_class(tidy_dag2, "tidy_dagitty")
})

test_that("paths functions edge cases", {
  dag <- dagify(
    y ~ x + z,
    x ~ z
  )

  # Test dag_paths with directed = TRUE
  result <- dag_paths(dag, from = "z", to = "y", directed = TRUE)
  expect_s3_class(result, "tidy_dagitty")

  # Test node_ancestors (from paths.R)
  result2 <- node_ancestors(dag, "y")
  expect_s3_class(result2, "tidy_dagitty")

  # Test ggdag_paths_fan
  p <- ggdag_paths_fan(dag, from = "z", to = "y")
  expect_s3_class(p, "gg")
})

test_that("pull functions edge cases", {
  dag <- dagify(y ~ x)
  tidy_dag <- tidy_dagitty(dag)

  # pull_dag_data only works on tidy_dagitty objects
  result <- pull_dag_data(tidy_dag)
  expect_s3_class(result, "data.frame")

  # Test pull_dag.data.frame (should fail)
  df <- data.frame(name = "x", to = "y")
  expect_ggdag_error(pull_dag(df))
})

test_that("tidy_dag additional edge cases", {
  # Test fortify.tidy_dagitty
  dag <- dagify(y ~ x)
  tidy_dag <- tidy_dagitty(dag)
  fortified <- fortify(tidy_dag)
  expect_s3_class(fortified, "data.frame")

  # Test fortify returns data.frame
  fortified2 <- fortify(tidy_dag)
  expect_s3_class(fortified2, "data.frame")

  # Test if_not_tidy_daggity with data.frame
  df <- data.frame(x = 1:3, y = 4:6)
  expect_ggdag_error(if_not_tidy_daggity(df))

  # Test arrange_coords with custom coords
  coords <- list(
    x = c(a = 0, b = 1),
    y = c(a = 0, b = 1)
  )
  dag <- dagify(b ~ a, coords = coords)
  tidy_dag <- tidy_dagitty(dag)
  result_df <- pull_dag_data(tidy_dag)
  expect_equal(unname(result_df$x[result_df$name == "a"]), 0)
  expect_equal(unname(result_df$y[result_df$name == "a"]), 0)

  # Test pull_dag.list
  expect_ggdag_error(pull_dag(list(a = 1, b = 2)))

  # Test pull_dag_data.list
  expect_ggdag_error(pull_dag_data(list(a = 1, b = 2)))

  # Test has_exposure/outcome with no exposure/outcome
  dag_no_exp <- dagify(y ~ x)
  tidy_no_exp <- tidy_dagitty(dag_no_exp)
  expect_false(has_exposure(tidy_no_exp))
  expect_false(has_outcome(tidy_no_exp))

  # Test collider identification through node_collider
  dag_collider <- dagify(m ~ x + y)
  tidy_collider <- tidy_dagitty(dag_collider)
  result <- node_collider(tidy_collider)
  expect_true("colliders" %in% names(pull_dag_data(result)))
})

test_that("utils edge cases", {
  # Test unique_pairs edge cases
  # Empty input
  expect_equal(nrow(unique_pairs(character(0))), 0)

  # Single element
  result <- unique_pairs("a")
  expect_equal(nrow(result), 0)

  # Test with exclude_identical = FALSE
  result2 <- unique_pairs(c("a", "b"), exclude_identical = FALSE)
  expect_equal(nrow(result2), 3) # Should include a-a, a-b, b-b (not b-a since it's same as a-b)
})

test_that("saturate_edges function works", {
  # Test the saturate_edges function used in as_tidy_dagitty.list
  time_points <- list(
    t1 = c("a", "b"),
    t2 = c("c", "d"),
    t3 = c("e")
  )

  # saturate_edges is called for each time point except the last
  result <- saturate_edges(1, time_points)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("name", "to") %in% names(result)))
  # Should connect t1 nodes to all future nodes
  expect_equal(nrow(result), 6) # 2 nodes * 3 future nodes
})

test_that("ggraph_create_layout wrapper works", {
  dag <- dagify(y ~ x)
  ig <- tidygraph::as_tbl_graph(tidy_dagitty(dag))

  # Test the wrapper function
  layout_result <- ggraph_create_layout(ig, layout = "nicely")
  expect_true(
    inherits(layout_result, "layout_tbl_graph") ||
      inherits(layout_result, "data.frame")
  )
})
