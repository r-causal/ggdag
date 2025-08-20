test_that("tidied dags are in good shape", {
  .mag <- dagitty::dagitty("mag{ x<-> y }")
  expect_ggdag_error(
    tidy_dagitty(.mag)
  )
  tidy_dag <- dagify(y ~ x + z, x ~ z) |> tidy_dagitty()
  expect_true(dagitty::is.dagitty(pull_dag(tidy_dag)))
  expect_true(dplyr::is.tbl(pull_dag_data(tidy_dag)))
  dag_col_names <- names(pull_dag_data(tidy_dag))
  expected_names <- c(
    "x",
    "y",
    "xend",
    "yend",
    "name",
    "direction",
    "to"
  )
  expect_true(all(expected_names %in% dag_col_names))
  # circular should not be present for non-circular layouts (issue #119)
  expect_false("circular" %in% dag_col_names)
  expect_equal(unique(pull_dag_data(tidy_dag)$name), c("x", "y", "z"))
  expect_equal(
    pull_dag_data(tidy_dag)$direction,
    factor(c("->", NA, "->", "->"), levels = c(c("->", "<->", "--")))
  )
  expect_true(is.numeric(pull_dag_data(tidy_dag)$x))
  expect_true(is.numeric(pull_dag_data(tidy_dag)$y))
})

test_that("circular column is present only when needed", {
  # Non-circular layout should not have circular column
  tidy_dag <- dagify(y ~ x + z, x ~ z) |> tidy_dagitty()
  expect_false("circular" %in% names(pull_dag_data(tidy_dag)))

  # Linear circular layout should have circular column
  tidy_dag_circular <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty(layout = "linear", circular = TRUE)
  expect_true("circular" %in% names(pull_dag_data(tidy_dag_circular)))
  expect_true(all(pull_dag_data(tidy_dag_circular)$circular))
})

test_that("nodes without edges are captured correctly", {
  .dagitty <- dagitty::dagitty(
    "dag {
  x -> y
  z
  }"
  )

  x <- tidy_dagitty(.dagitty)
  expect_identical(pull_dag_data(x)$name, c("x", "y", "z"))
})

test_that("`as_tidy_dagitty()` returns correct objects", {
  expect_ggdag_error(
    as_tidy_dagitty(data.frame())
  )

  df_dag <- data.frame(name = c("c", "c", "x"), to = c("x", "y", "y")) |>
    as_tidy_dagitty(seed = 1234, layout = "time_ordered")
  expect_true(is.tidy_dagitty(df_dag))
  expect_true(dagitty::is.dagitty(pull_dag(df_dag)))
  expect_true(dplyr::is.tbl(pull_dag_data(df_dag)))

  # `as_tidy_dagitty()` is the same for `dagitty` objects
  .dag <- dagify(y ~ x + z, x ~ z)
  v1_dag <- tidy_dagitty(.dag, seed = 1234)
  v2_dag <- as_tidy_dagitty(.dag, seed = 1234)
  expect_equal(v1_dag, v2_dag)
})

test_that("`as_tidy_dagitty()` works with other configurations", {
  .df <- data.frame(
    name = c("c", "c", "x"),
    to = c("x", "y", "y"),
    x = 1,
    y = 1,
    xend = 1,
    yend = 1
  )

  df_dag <- .df |>
    as_tidy_dagitty(seed = 1234)

  expect_true(is.tidy_dagitty(df_dag))
  expect_true(dagitty::is.dagitty(pull_dag(df_dag)))
  expect_true(dplyr::is.tbl(pull_dag_data(df_dag)))

  .df <- dplyr::full_join(
    .df,
    data.frame(
      name = c("x", "y", "c"),
      status = c("exposure", "outcome", "latent"),
      adjusted = c("unadjusted", "unadjusted", "adjusted")
    ),
    by = "name"
  ) |>
    dplyr::mutate(x = 1, y = 1, xend = 1, yend = 1)

  status_dag <- as_tidy_dagitty(.df) |> pull_dag()
  expect_identical(dagitty::exposures(status_dag), "x")
  expect_identical(dagitty::outcomes(status_dag), "y")
  expect_identical(dagitty::latents(status_dag), "c")
  expect_identical(dagitty::adjustedNodes(status_dag), "c")
})

test_that("list is correctly converted to a saturated, time-ordered DAG", {
  node_groups <- list(c("x1", "x2"), c("y1", "y2"), "z")
  dag <- as_tidy_dagitty(node_groups)
  expect_s3_class(dag, "tidy_dagitty")
  expect_equal(nrow(pull_dag_data(dag)), 9)
  coords_df <- pull_dag_data(dag) |>
    dplyr::distinct(name, .keep_all = TRUE)
  pull_coords <- function(.n) {
    unname(dplyr::filter(coords_df, name == .n)$x)
  }
  expect_equal(pull_coords("x1"), 1)
  expect_equal(pull_coords("x2"), 1)
  expect_equal(pull_coords("y1"), 2)
  expect_equal(pull_coords("y2"), 2)
  expect_equal(pull_coords("z"), 3)
  p1 <- ggdag(dag)
  expect_doppelganger("List creates saturated, ordered DAG", p1)
})


test_that("Forbidden layouts error", {
  expect_ggdag_error(
    tidy_dagitty(dagify(y ~ x + z, x ~ z), layout = "dendogram")
  )
})

test_that("igraph attribute does not hitchhike onto tidy dag", {
  td <- tidy_dagitty(dagify(y ~ x + z, x ~ z))
  expect_null(attr(pull_dag_data(td), "graph"))
})

test_that("DAGs with no edges are handled correctly (issue #159)", {
  # Create a DAG with no edges
  no_edge_dag <- dagitty::dagitty(
    "dag {
    x1
    x2
    x3
    x4
    x5
  }"
  )

  # This should not throw an error
  expect_no_error(tidy_no_edge_dag <- tidy_dagitty(no_edge_dag))

  # Check that the result is a valid tidy_dagitty object
  expect_true(is.tidy_dagitty(tidy_no_edge_dag))

  # Check that all nodes are present
  dag_data <- pull_dag_data(tidy_no_edge_dag)
  expect_equal(sort(unique(dag_data$name)), c("x1", "x2", "x3", "x4", "x5"))

  # Check that no edges exist (all 'to' values should be NA)
  expect_true(all(is.na(dag_data$to)))

  # Check that coordinates were generated
  expect_true(all(!is.na(dag_data$x)))
  expect_true(all(!is.na(dag_data$y)))
})

test_that("as_tidy_dagitty preserves edge direction from data frame (issue #177)", {
  # Test case from issue #177
  dag <- data.frame(name = c("c", "c", "x"), to = c("x", "y", "y")) |>
    as_tidy_dagitty()

  # Extract the data and dagitty object
  dag_data <- pull_dag_data(dag)
  dag_obj <- pull_dag(dag)

  # The data frame should show edges in the correct direction
  # c -> x, c -> y, x -> y
  edges <- dag_data |>
    dplyr::filter(!is.na(to)) |>
    dplyr::select(name, to, direction)

  expect_equal(edges$name, c("c", "c", "x"))
  expect_equal(edges$to, c("x", "y", "y"))
  expect_equal(as.character(edges$direction), c("->", "->", "->"))

  # The dagitty object should also have the same edges
  # We can check this by converting back to string representation
  dag_string <- as.character(dag_obj)

  # The DAG should contain these edges in the correct direction
  expect_true(grepl("c -> x", dag_string))
  expect_true(grepl("c -> y", dag_string))
  expect_true(grepl("x -> y", dag_string))

  # It should NOT contain reversed edges
  expect_false(grepl("x -> c", dag_string))
  expect_false(grepl("y -> c", dag_string))
  expect_false(grepl("y -> x", dag_string))
})

expect_function_produces_name <- function(tidy_dag, column) {
  .df <- pull_dag_data(tidy_dag)
  expect_true(all(column %in% names(.df)))
}

test_that("node functions produce correct columns", {
  tidy_dag <- dagify(y ~ x + z, x ~ z) |> tidy_dagitty()
  expect_function_produces_name(node_ancestors(tidy_dag, "y"), "ancestor")
  expect_function_produces_name(node_children(tidy_dag, "z"), "children")
  expect_function_produces_name(node_collider(tidy_dag), "colliders")
  expect_function_produces_name(
    node_dconnected(tidy_dag, "x", "y"),
    "d_relationship"
  )
  expect_function_produces_name(
    node_dconnected(tidy_dag, "x", "y", controlling_for = "z"),
    c("adjusted", "d_relationship")
  )
  expect_function_produces_name(node_descendants(tidy_dag, "z"), "descendant")
  expect_function_produces_name(
    node_drelationship(tidy_dag, "x", "y"),
    "d_relationship"
  )
  expect_function_produces_name(
    node_dseparated(tidy_dag, "x", "y"),
    "d_relationship"
  )
  expect_function_produces_name(node_equivalent_class(tidy_dag), "reversable")
  expect_function_produces_name(node_equivalent_dags(tidy_dag), "dag")
  expect_function_produces_name(node_exogenous(tidy_dag), "exogenous")
  expect_function_produces_name(
    node_instrumental(
      tidy_dag,
      exposure = "x",
      outcome = "y"
    ),
    "instrumental"
  )
  expect_function_produces_name(node_parents(tidy_dag, "z"), "parent")
  expect_function_produces_name(node_status(tidy_dag), "status")
})

test_that("`as_tibble()` and friends convert data frames", {
  tidy_dag <- dagify(y ~ x + z, x ~ z) |> tidy_dagitty()
  df_dag1 <- dplyr::as_tibble(tidy_dag)
  expect_true(dplyr::is.tbl(df_dag1))

  # all other friends deprecated!
})

test_that("coordinate conversion functions work forward and backwards", {
  coords <- list(
    x = c(A = 1, B = 2, D = 3, C = 3, F = 3, E = 4, G = 5, H = 5, I = 5),
    y = c(A = 0, B = 0, D = 1, C = 0, F = -1, E = 0, G = 1, H = 0, I = -1)
  )
  coord_df <- coords2df(coords)
  expect_true(is.data.frame(coord_df))
  expect_length(coord_df, 3)
  expect_equal(nrow(coord_df), length(coords$x))
  expect_equal(coords, coords2list(coord_df))
})

test_that("tidy_dagitty warns about cyclic graphs", {
  # Simple 2-node cycle
  cyclic_dag1 <- dagitty::dagitty("dag { A -> B -> A }")
  expect_ggdag_warning(
    tidy_dagitty(cyclic_dag1)
  )

  # Verify the warning message contains the cycle
  expect_warning(
    tidy_dagitty(cyclic_dag1),
    regexp = "A -> B -> A"
  )

  # 3-node cycle
  cyclic_dag2 <- dagitty::dagitty("dag { X -> Y -> Z -> X }")
  expect_warning(
    tidy_dagitty(cyclic_dag2),
    regexp = "X -> Y -> Z -> X"
  )

  # Self-loop
  cyclic_dag3 <- dagitty::dagitty("dag { A -> A }")
  expect_warning(
    tidy_dagitty(cyclic_dag3),
    regexp = "A -> A"
  )

  # Complex graph with cycle
  cyclic_dag4 <- dagitty::dagitty("dag { A -> B -> C -> D -> B E -> F }")
  expect_warning(
    tidy_dagitty(cyclic_dag4),
    regexp = "B -> C -> D -> B"
  )

  # No warning for acyclic graphs
  acyclic_dag <- dagitty::dagitty("dag { A -> B -> C }")
  expect_no_warning(tidy_dagitty(acyclic_dag))

  # The warning should occur only once, not on subsequent operations
  expect_warning(
    tidy_cyclic <- tidy_dagitty(cyclic_dag1),
    class = "ggdag_cyclic_warning"
  )
  # Operations on already tidied dag should not re-warn
  expect_no_warning(pull_dag(tidy_cyclic))
  expect_no_warning(pull_dag_data(tidy_cyclic))

  # Warning should propagate through if_not_tidy_daggity
  expect_warning(
    expect_warning(
      dag_adjustment_sets(cyclic_dag1, exposure = "A", outcome = "B"),
      class = "ggdag_cyclic_warning"
    ),
    class = "ggdag_failed_to_close_backdoor_warning"
  )
})
