test_that("tidied dags are in good shape", {
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
    factor(c("->", NA, "->", "->"), levels = c("<-", "->", "<->"))
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
