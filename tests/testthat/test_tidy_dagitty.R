context("Tidying DAGs")

test_that("tidied dags are in good shape", {
  tidy_dag <- dagify(y ~ x + z, x ~ z) %>% tidy_dagitty()
  expect_true(dagitty::is.dagitty(tidy_dag$dag))
  expect_true(dplyr::is.tbl(tidy_dag$data))
  dag_col_names <- names(tidy_dag$data)
  expected_names <- c("x", "y", "xend", "yend", "name", "direction",
                      "to", "circular")
  expect_true(all(expected_names %in% dag_col_names))
  expect_equal(unique(tidy_dag$data$name), c("x", "z", "y"))
  expect_equal(tidy_dag$data$direction,
               factor(c("->", "->", "->", NA), levels = c("<-", "->", "<->")))
  expect_true(is.logical(tidy_dag$data$circular))
  expect_true(is.numeric(tidy_dag$data$x))
  expect_true(is.numeric(tidy_dag$data$y))
})

expect_function_produces_name <- function(tidy_dag, column) {
  .df <- tidy_dag$data
  expect_true(all(column %in% names(.df)))
}

test_that("node functions produce correct columns", {
  tidy_dag <- dagify(y ~ x + z, x ~ z) %>% tidy_dagitty()
  expect_function_produces_name(node_ancestors(tidy_dag, "y"), "ancestor")
  expect_function_produces_name(node_children(tidy_dag, "z"), "children")
  expect_function_produces_name(node_collider(tidy_dag), "colliders")
  expect_function_produces_name(node_dconnected(tidy_dag, "x", "y"),
                                c("adjusted", "d_relationship"))
  expect_function_produces_name(node_descendants(tidy_dag, "z"), "descendant")
  expect_function_produces_name(node_drelationship(tidy_dag, "x", "y"),
                                c("adjusted", "d_relationship"))
  expect_function_produces_name(node_dseparated(tidy_dag, "x", "y"),
                                c("adjusted", "d_relationship"))
  expect_function_produces_name(node_equivalent_class(tidy_dag), "reversable")
  expect_function_produces_name(node_equivalent_dags(tidy_dag), "dag")
  expect_function_produces_name(node_exogenous(tidy_dag), "exogenous")
  expect_function_produces_name(node_instrumental(tidy_dag,
                                                  exposure = "x",
                                                  outcome = "y"),
                                c("adjusted", "instrumental"))
  expect_function_produces_name(node_parents(tidy_dag, "z"), "parent")
  expect_function_produces_name(node_status(tidy_dag), "status")
})
