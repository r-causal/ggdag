test_that("get_dagitty_edges works correctly", {
  # Simple DAG
  dag <- dagify(y ~ x + z, x ~ z)
  edges <- get_dagitty_edges(dag)

  expect_s3_class(edges, "data.frame")
  expect_true(all(c("name", "to") %in% names(edges)))
  expect_equal(nrow(edges), 3) # 3 edges

  # DAG with bidirected edges
  dag_bidir <- dagify(y ~ x, x ~ ~y)
  edges_bidir <- get_dagitty_edges(dag_bidir)

  # Should have both directed and bidirected edges
  expect_true("direction" %in% names(edges_bidir))
  expect_true("<->" %in% edges_bidir$direction)

  # Empty DAG
  dag_empty <- dagitty::dagitty("dag { }")
  edges_empty <- get_dagitty_edges(dag_empty)
  expect_equal(nrow(edges_empty), 0)
})

test_that("prep_dag_data handles various inputs", {
  # Basic data frame
  df <- data.frame(
    name = c("x", "z", "z", "y"),
    to = c("y", "x", "y", NA)
  )

  result <- prep_dag_data(df)
  expect_true(all(c("name", "to", "x", "y", "xend", "yend") %in% names(result)))

  # Data frame with existing coordinates
  df_coords <- data.frame(
    name = c("x", "y", "z"),
    to = c("y", NA, "x"),
    x = c(0, 2, 1),
    y = c(0, 0, 1),
    xend = c(2, NA, 0),
    yend = c(0, NA, 0)
  )

  result_coords <- prep_dag_data(df_coords)
  expect_equal(result_coords$x, c(0, 2, 1))
  expect_equal(result_coords$y, c(0, 0, 1))
})

test_that("prep_dag_data leaves direction NA on rows with no edge", {
  # terminal node `y` has no outgoing edge
  df <- data.frame(name = c("z", "x", "y"), to = c("x", "y", NA))
  tidy_dag <- as_tidy_dagitty(df, seed = 1)
  dag_data <- pull_dag_data(tidy_dag)

  expect_s3_class(dag_data$direction, "factor")
  expect_equal(levels(dag_data$direction), c("->", "<->", "--"))
  expect_true(is.na(dag_data$direction[dag_data$name == "y"]))
  expect_equal(sum(!is.na(dag_data$direction)), 2)
  expect_equal(n_edges(tidy_dag), 2)
})

test_that("prep_dag_data stamps direction when coordinates are supplied", {
  df <- data.frame(
    name = c("x", "y"),
    to = c("y", NA),
    x = c(0, 1),
    y = c(0, 0),
    xend = c(1, NA),
    yend = c(0, NA)
  )
  tidy_dag <- as_tidy_dagitty(df)
  dag_data <- pull_dag_data(tidy_dag)

  expect_s3_class(dag_data$direction, "factor")
  expect_equal(levels(dag_data$direction), c("->", "<->", "--"))
  expect_true(is.na(dag_data$direction[dag_data$name == "y"]))
  expect_equal(n_edges(tidy_dag), 1)
})

test_that("prep_dag_data regenerates coordinates when only some are missing", {
  # node-level coordinates without edge endpoints
  df <- data.frame(
    name = c("c", "c", "x", "y"),
    to = c("x", "y", "y", NA),
    x = c(0, 0, 1, 2),
    y = c(1, 1, 0, 0)
  )

  result <- prep_dag_data(df)
  expect_true(all(c("x", "y", "xend", "yend") %in% names(result)))
  expect_false(any(c("xendend", "yendend") %in% names(result)))

  node_coords <- dplyr::distinct(result, name, x, y)
  edges <- dplyr::filter(result, !is.na(to))
  expect_equal(edges$xend, node_coords$x[match(edges$to, node_coords$name)])
  expect_equal(edges$yend, node_coords$y[match(edges$to, node_coords$name)])
})

test_that("compile_dag_from_df creates valid dagitty objects", {
  # Simple DAG
  df <- data.frame(
    name = c("x", "z", "z"),
    to = c("y", "x", "y")
  )

  dag <- compile_dag_from_df(df)
  expect_s3_class(dag, "dagitty")
  expect_equal(dagitty::graphType(dag), "dag")

  # DAG with isolated nodes - isolated nodes are kept in the compiled DAG
  df_isolated <- data.frame(
    name = c("x", "y", "isolated"),
    to = c("y", NA, NA)
  )

  dag_isolated <- compile_dag_from_df(df_isolated)
  expect_setequal(names(dag_isolated), c("x", "y", "isolated"))

  # Empty data frame (no edges) causes error
  df_empty <- data.frame(
    name = character(0),
    to = character(0),
    stringsAsFactors = FALSE
  )

  # Empty data frame causes error
  expect_error(compile_dag_from_df(df_empty))
})

test_that("compile_dag_from_df quotes node names", {
  # unquoted dagitty identifiers cannot contain spaces
  spaced <- data.frame(name = c("my var", "y"), to = c("y", NA))
  expect_setequal(names(compile_dag_from_df(spaced)), c("my var", "y"))

  accented <- data.frame(name = c("café", "y"), to = c("y", NA))
  expect_setequal(names(compile_dag_from_df(accented)), c("café", "y"))

  # ordinary names are unaffected
  ordinary <- data.frame(name = c("x", "z", "z"), to = c("y", "x", "y"))
  expect_setequal(names(compile_dag_from_df(ordinary)), c("x", "y", "z"))
})

test_that("return_status extracts correct status values", {
  # Create tidy_dag with status column
  df <- data.frame(
    name = c("x", "y", "z", "u"),
    to = c("y", NA, "x", "x"),
    status = c("exposure", "outcome", NA, "latent")
  )

  # Test exposure extraction
  exposures <- return_status(df, "exposure")
  expect_equal(exposures, "x")

  # Test outcome extraction
  outcomes <- return_status(df, "outcome")
  expect_equal(outcomes, "y")

  # Test latent extraction
  latents <- return_status(df, "latent")
  expect_equal(latents, "u")

  # Test non-existent status
  none <- return_status(df, "non_existent")
  expect_length(none, 0)
})

test_that("empty2list converts empty vectors to empty lists", {
  # Empty character vector
  expect_equal(empty2list(character(0)), list())

  # Empty numeric vector
  expect_equal(empty2list(numeric(0)), list())

  # Non-empty vector (should return unchanged)
  expect_equal(empty2list(c("a", "b")), c("a", "b"))

  # NULL might return list() based on implementation
  result_null <- empty2list(NULL)
  expect_true(
    is.null(result_null) || (is.list(result_null) && length(result_null) == 0)
  )
})


test_that("has_exposure, has_outcome, has_latent work correctly", {
  # DAG with all types
  dag_all <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    latent = "z"
  )
  tidy_all <- tidy_dagitty(dag_all)

  expect_true(has_exposure(tidy_all))
  expect_true(has_outcome(tidy_all))
  expect_true(has_latent(tidy_all))

  # DAG with none
  dag_none <- dagify(y ~ x)
  tidy_none <- tidy_dagitty(dag_none)

  expect_false(has_exposure(tidy_none))
  expect_false(has_outcome(tidy_none))
  expect_false(has_latent(tidy_none))

  # DAG with some
  dag_some <- dagify(y ~ x, exposure = "x")
  tidy_some <- tidy_dagitty(dag_some)

  expect_true(has_exposure(tidy_some))
  expect_false(has_outcome(tidy_some))
  expect_false(has_latent(tidy_some))
})

test_that("ggraph_create_layout wrapper function works", {
  # Create a simple graph
  edges <- data.frame(from = c("a", "b"), to = c("b", "c"))
  ig <- igraph::graph_from_data_frame(edges)

  # Test wrapper function
  layout <- ggraph_create_layout(ig, layout = "nicely")
  expect_true(
    inherits(layout, "data.frame") || inherits(layout, "layout_tbl_graph")
  )
  expect_true(all(c("x", "y") %in% names(layout)))

  # Test with different layout
  layout_fr <- ggraph_create_layout(ig, layout = "fr")
  expect_true(
    inherits(layout_fr, "data.frame") || inherits(layout_fr, "layout_tbl_graph")
  )
})
