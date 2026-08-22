test_that("dag_saturate returns a saturated DAG", {
  withr::local_seed(1234)
  .dag <- tidy_dagitty(dagify(y ~ x, x ~ z))
  .saturated_dag <- dag_saturate(.dag)
  expect_s3_class(.saturated_dag, "tidy_dagitty")
  expect_gt(nrow(pull_dag_data(.saturated_dag)), nrow(pull_dag_data(.dag)))
  expect_equal(nrow(pull_dag_data(.saturated_dag)), 4)
  p1 <- ggdag(.saturated_dag)
  expect_doppelganger("dag_saturate returns a saturated DAG", p1)
})

test_that("dag_saturate() keeps isolated nodes", {
  withr::local_seed(1234)
  .dag <- dagitty::dagitty("dag{x -> y; z}")
  .saturated_dag <- dag_saturate(.dag)

  expect_setequal(
    unique(pull_dag_data(.saturated_dag)$name),
    c("x", "y", "z")
  )
  expect_setequal(names(pull_dag(.saturated_dag)), c("x", "y", "z"))
})

test_that("visual: dag_saturate() keeps isolated nodes", {
  withr::local_seed(1234)
  .saturated_dag <- dag_saturate(dagitty::dagitty("dag{x -> y; z}"))
  # never record a baseline from a saturation that lost the isolated node
  skip_if_not(
    setequal(unique(pull_dag_data(.saturated_dag)$name), c("x", "y", "z"))
  )
  expect_doppelganger(
    "dag_saturate keeps isolated nodes",
    ggdag(.saturated_dag)
  )
})

test_that("use_existing_coords works as expected", {
  .tdy_dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()
  result_with_coords <- dag_saturate(.tdy_dag, use_existing_coords = TRUE)
  expect_equal(
    dagitty::coordinates(pull_dag(result_with_coords)),
    dagitty::coordinates(pull_dag(.tdy_dag))
  )
})

test_that("dag_saturate() carries the DAG's labels through", {
  withr::local_seed(1234)
  labels <- c("x" = "Exposure", "y" = "Outcome", "z" = "Confounder")
  .dag <- dagify(y ~ x, x ~ z, labels = labels)
  .saturated_dag <- dag_saturate(tidy_dagitty(.dag))

  expect_equal(label(pull_dag(.saturated_dag)), labels)

  dag_data <- pull_dag_data(.saturated_dag)
  expect_true("label" %in% names(dag_data))
  expect_equal(unique(dag_data$label[dag_data$name == "z"]), "Confounder")
})

test_that("visual: dag_saturate() keeps labels", {
  withr::local_seed(1234)
  .saturated_dag <- dagify(
    y ~ x,
    x ~ z,
    labels = c("x" = "Exposure", "y" = "Outcome", "z" = "Confounder")
  ) |>
    tidy_dagitty() |>
    dag_saturate()
  # never record a baseline from a saturation that lost the labels
  skip_if_not("label" %in% names(pull_dag_data(.saturated_dag)))
  expect_doppelganger(
    "dag_saturate keeps labels",
    ggdag(.saturated_dag, use_labels = TRUE)
  )
})

test_that("dag_saturate() works on DAGs with a single time point", {
  withr::local_seed(1234)
  single_node <- dag_saturate(dagitty::dagitty("dag{x}"))
  expect_s3_class(single_node, "tidy_dagitty")
  expect_setequal(unique(pull_dag_data(single_node)$name), "x")
  expect_setequal(names(pull_dag(single_node)), "x")
  expect_equal(n_edges(single_node), 0)

  edge_free <- dag_saturate(dagitty::dagitty("dag{x; y}"))
  expect_s3_class(edge_free, "tidy_dagitty")
  expect_setequal(unique(pull_dag_data(edge_free)$name), c("x", "y"))
  expect_setequal(names(pull_dag(edge_free)), c("x", "y"))
  expect_equal(n_edges(edge_free), 0)
})

test_that("edges are correctly pruned from the DAG", {
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))
  expect_equal(nrow(pull_dag_data(.tdy_dag)), 4)
  expect_equal(
    .tdy_dag |>
      pull_dag_data() |>
      dplyr::filter(name == "z", to == "x") |>
      nrow(),
    1
  )
  pruned_dag <- dag_prune(.tdy_dag, c("z" = "x"))
  expect_equal(nrow(pull_dag_data(pruned_dag)), 3)
  expect_equal(
    pruned_dag |>
      pull_dag_data() |>
      dplyr::filter(name == "z", to == "x") |>
      nrow(),
    0
  )
})
