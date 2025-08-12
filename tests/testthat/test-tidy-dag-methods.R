test_that("fortify methods work correctly", {
  dag <- dagify(y ~ x + z, x ~ z)
  tidy_dag <- tidy_dagitty(dag)

  # Test fortify.tidy_dagitty
  fortified_tidy <- fortify(tidy_dag)
  expect_s3_class(fortified_tidy, "data.frame")
  expect_true(all(
    c("name", "x", "y", "to", "xend", "yend") %in% names(fortified_tidy)
  ))

  # Test fortify.dagitty
  fortified_dag <- fortify(dag)
  expect_s3_class(fortified_dag, "data.frame")
  expect_true(all(
    c("name", "x", "y", "to", "xend", "yend") %in% names(fortified_dag)
  ))
})

test_that("data frame conversion methods work", {
  dag <- dagify(y ~ x + z, x ~ z)
  tidy_dag <- tidy_dagitty(dag)

  # Test as.data.frame
  df <- as.data.frame(tidy_dag)
  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "tbl_df"))

  # Test with row.names - need to match actual number of rows
  n_rows <- nrow(pull_dag_data(tidy_dag))
  df_rownames <- as.data.frame(tidy_dag, row.names = 1:n_rows)
  expect_equal(rownames(df_rownames), as.character(1:n_rows))
})

test_that("tibble conversion methods work", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  dag <- dagify(y ~ x + z, x ~ z)
  tidy_dag <- tidy_dagitty(dag)

  # Test as_tibble
  tbl <- as_tibble(tidy_dag)
  expect_s3_class(tbl, "tbl_df")
  expect_s3_class(tbl, "data.frame")

  # Test tbl_df method
  tbl2 <- tbl_df(tidy_dag)
  expect_s3_class(tbl2, "tbl_df")

  # Test deprecated as.tbl
  tbl3 <- as.tbl(tidy_dag)
  expect_s3_class(tbl3, "tbl_df")
})

test_that("print method works correctly", {
  # Simple DAG
  dag1 <- dagify(y ~ x)
  tidy_dag1 <- tidy_dagitty(dag1)

  expect_output(print(tidy_dag1), "# A DAG with 2 nodes and 1 edges")

  # DAG with exposure and outcome
  dag2 <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")
  tidy_dag2 <- tidy_dagitty(dag2)

  output2 <- capture.output(print(tidy_dag2))
  expect_true(any(grepl("Exposure: x", output2)))
  expect_true(any(grepl("Outcome: y", output2)))

  # DAG with latent variables
  dag3 <- dagify(y ~ x + u, x ~ u, latent = "u")
  tidy_dag3 <- tidy_dagitty(dag3)

  output3 <- capture.output(print(tidy_dag3))
  expect_true(any(grepl("Latent Variable: u", output3)))

  # DAG with collider paths
  dag4 <- dagify(m ~ x + y, y ~ x)
  tidy_dag4 <- tidy_dagitty(dag4)
  tidy_dag4 <- activate_collider_paths(
    tidy_dag4,
    from = "x",
    to = "y",
    adjust_for = "m"
  )

  output4 <- capture.output(print(tidy_dag4))
  expect_true(any(grepl("Paths opened by conditioning", output4)))
})

test_that("new_tidy_dagitty constructor works", {
  dag <- dagify(y ~ x, labels = c(x = "Exposure", y = "Outcome"))
  dag_data <- dagify(y ~ x) |> tidy_dagitty() |> pull_dag_data()

  # Create new tidy_dagitty manually
  tidy_dag <- new_tidy_dagitty(dag_data, dag)
  expect_s3_class(tidy_dag, "tidy_dagitty")
  expect_equal(names(tidy_dag), c("data", "dag"))

  # Check that the dag component has labels
  expect_equal(label(pull_dag(tidy_dag)), c(x = "Exposure", y = "Outcome"))
})

test_that("tidy_dag_edges_and_coords handles missing direction column", {
  dag <- dagify(y ~ x + z, x ~ z)
  edges <- get_dagitty_edges(dag)

  # Remove direction column to test it gets added
  edges$direction <- NULL

  coords_df <- data.frame(
    name = c("x", "y", "z"),
    x = c(0, 2, 1),
    y = c(0, 0, 1)
  )

  result <- tidy_dag_edges_and_coords(edges, coords_df)
  expect_true("direction" %in% names(result))
  expect_s3_class(result$direction, "factor")
  expect_equal(levels(result$direction), c("->", "<->", "--"))
})

test_that("generate_layout with existing coordinates", {
  # Create edges data frame
  edges_df <- data.frame(
    name = c("x", "z", "z"),
    to = c("y", "x", "y")
  )

  # Provide existing coordinates
  coords <- list(
    x = c(x = 0, y = 2, z = 1),
    y = c(x = 0, y = 0, z = 1)
  )

  result <- generate_layout(edges_df, layout = "nicely", coords = coords)
  expect_equal(unname(result$x[result$name == "x"]), 0)
  expect_equal(unname(result$y[result$name == "x"]), 0)
  expect_equal(unname(result$x[result$name == "z"]), 1)
  expect_equal(unname(result$y[result$name == "z"]), 1)

  # Test with NA coordinates (should use layout algorithm)
  coords_na <- list(
    x = c(x = NA, y = NA, z = NA),
    y = c(x = NA, y = NA, z = NA)
  )

  result_na <- generate_layout(edges_df, layout = "nicely", coords = coords_na)
  expect_false(any(is.na(result_na$x)))
  expect_false(any(is.na(result_na$y)))
})

test_that("as_tidy_dagitty.data.frame with adjusted column", {
  # Create data frame with adjusted column
  dag_df <- data.frame(
    name = c("x", "z", "z", "y"),
    to = c("y", "x", "y", NA),
    adjusted = c("unadjusted", "adjusted", "adjusted", "unadjusted"),
    stringsAsFactors = FALSE
  )

  result <- as_tidy_dagitty(dag_df)
  dag <- pull_dag(result)

  # Check adjusted nodes
  adj_nodes <- dagitty::adjustedNodes(dag)
  expect_equal(adj_nodes, "z")

  # Test empty adjusted case
  dag_df2 <- data.frame(
    name = c("x", "z", "z"),
    to = c("y", "x", "y"),
    adjusted = rep("unadjusted", 3),
    stringsAsFactors = FALSE
  )

  result2 <- as_tidy_dagitty(dag_df2)
  dag2 <- pull_dag(result2)
  expect_length(dagitty::adjustedNodes(dag2), 0)
})

test_that("edge case: DAG with no edges", {
  # Create a DAG with only nodes, no edges
  dag_df <- data.frame(
    name = c("a", "b", "c"),
    to = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  result <- as_tidy_dagitty(dag_df)
  expect_s3_class(result, "tidy_dagitty")
  expect_equal(n_edges(result), 0)
  # Isolated nodes without edges are dropped
  expect_equal(n_nodes(result), 0)
})

test_that("tidy_dagitty with use_existing_coords = FALSE", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    coords = list(
      x = c(x = 0, y = 2, z = 1),
      y = c(x = 0, y = 0, z = 1)
    )
  )

  # Should ignore existing coords and generate new layout
  tidy_dag <- tidy_dagitty(dag, use_existing_coords = FALSE, seed = 123)
  coords_new <- pull_dag(tidy_dag) |> dagitty::coordinates()

  # Coordinates should be different from original
  # (with seed they should be consistent but different from manual coords)
  expect_true(coords_new$x["x"] != 0 || coords_new$y["x"] != 0)
})
