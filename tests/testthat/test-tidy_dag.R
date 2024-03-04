test_that("tidied dags are in good shape", {
  .mag <- dagitty::dagitty("mag{ x<-> y }")
  expect_error(
    tidy_dagitty(.mag),
    "`.dagitty` must be of graph type `dag`"
  )
  tidy_dag <- dagify(y ~ x + z, x ~ z) %>% tidy_dagitty()
  expect_true(dagitty::is.dagitty(pull_dag(tidy_dag)))
  expect_true(dplyr::is.tbl(pull_dag_data(tidy_dag)))
  dag_col_names <- names(pull_dag_data(tidy_dag))
  expected_names <- c(
    "x", "y", "xend", "yend", "name", "direction",
    "to", "circular"
  )
  expect_true(all(expected_names %in% dag_col_names))
  expect_equal(unique(pull_dag_data(tidy_dag)$name), c("x", "y", "z"))
  expect_equal(
    pull_dag_data(tidy_dag)$direction,
    factor(c("->", NA, "->", "->"), levels = c(c("->", "<->", "--")))
  )
  expect_true(is.logical(pull_dag_data(tidy_dag)$circular))
  expect_true(is.numeric(pull_dag_data(tidy_dag)$x))
  expect_true(is.numeric(pull_dag_data(tidy_dag)$y))
})

test_that("nodes without edges are captured correctly", {
  .dagitty <- dagitty::dagitty("dag {
  x -> y
  z
  }")

  x <- tidy_dagitty(.dagitty)
  expect_identical(pull_dag_data(x)$name, c("x", "y", "z"))
})

test_that("`as_tidy_dagitty()` returns correct objects", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  expect_error(
    as_tidy_dagitty(data.frame()),
    "Columns `name` and `to` not found"
  )

  df_dag <- data.frame(name = c("c", "c", "x"), to = c("x", "y", "y")) %>%
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
    x = 1, y = 1, xend = 1, yend = 1
  )

  df_dag <- .df %>%
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
  ) %>% dplyr::mutate(x = 1, y = 1, xend = 1, yend = 1)


  status_dag <- as_tidy_dagitty(.df) %>% pull_dag()
  expect_identical(dagitty::exposures(status_dag), "x")
  expect_identical(dagitty::outcomes(status_dag), "y")
  expect_identical(dagitty::latents(status_dag), "c")
  expect_identical(dagitty::adjustedNodes(status_dag), "c")
})

test_that("Forbidden layouts error", {
  expect_error(
    tidy_dagitty(dagify(y ~ x + z, x ~ z), layout = "dendogram"),
    "Layout type `dendogram` not supported in ggdag"
  )
})

expect_function_produces_name <- function(tidy_dag, column) {
  .df <- pull_dag_data(tidy_dag)
  expect_true(all(column %in% names(.df)))
}

test_that("node functions produce correct columns", {
  tidy_dag <- dagify(y ~ x + z, x ~ z) %>% tidy_dagitty()
  expect_function_produces_name(node_ancestors(tidy_dag, "y"), "ancestor")
  expect_function_produces_name(node_children(tidy_dag, "z"), "children")
  expect_function_produces_name(node_collider(tidy_dag), "colliders")
  expect_function_produces_name(
    node_dconnected(tidy_dag, "x", "y"),
    c("adjusted", "d_relationship")
  )
  expect_function_produces_name(node_descendants(tidy_dag, "z"), "descendant")
  expect_function_produces_name(
    node_drelationship(tidy_dag, "x", "y"),
    c("adjusted", "d_relationship")
  )
  expect_function_produces_name(
    node_dseparated(tidy_dag, "x", "y"),
    c("adjusted", "d_relationship")
  )
  expect_function_produces_name(node_equivalent_class(tidy_dag), "reversable")
  expect_function_produces_name(node_equivalent_dags(tidy_dag), "dag")
  expect_function_produces_name(node_exogenous(tidy_dag), "exogenous")
  expect_function_produces_name(
    node_instrumental(tidy_dag,
      exposure = "x",
      outcome = "y"
    ),
    c("adjusted", "instrumental")
  )
  expect_function_produces_name(node_parents(tidy_dag, "z"), "parent")
  expect_function_produces_name(node_status(tidy_dag), "status")
})

test_that("`as_tibble()` and friends convert data frames", {
  tidy_dag <- dagify(y ~ x + z, x ~ z) %>% tidy_dagitty()
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

