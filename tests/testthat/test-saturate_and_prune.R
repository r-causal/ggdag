test_that("dag_saturate returns a saturated DAG", {
  .dag <- tidy_dagitty(dagify(y ~ x, x ~ z))
  .saturated_dag <- dag_saturate(.dag)
  expect_s3_class(.saturated_dag, "tidy_dagitty")
  expect_gt(nrow(pull_dag_data(.saturated_dag)), nrow(pull_dag_data(.dag)))
  expect_equal(nrow(pull_dag_data(.saturated_dag)), 4)
  p1 <- ggdag(.saturated_dag)
  expect_doppelganger("dag_saturate returns a saturated DAG", p1)
})

test_that("use_existing_coords works as expected", {
  .tdy_dag <- dagify(y ~ x + z, x ~ z) %>%
    tidy_dagitty()
  result_with_coords <- dag_saturate(.tdy_dag, use_existing_coords = TRUE)
  expect_equal(
    dagitty::coordinates(pull_dag(result_with_coords)),
    dagitty::coordinates(pull_dag(.tdy_dag))
  )
})

test_that("edges are correctly pruned from the DAG", {
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))
  expect_equal(nrow(pull_dag_data(.tdy_dag)), 4)
  expect_equal(
    .tdy_dag %>%
      pull_dag_data() %>%
      dplyr::filter(name == "z", to == "x") %>%
      nrow(),
    1
  )
  pruned_dag <- dag_prune(.tdy_dag, c("z" = "x"))
  expect_equal(nrow(pull_dag_data(pruned_dag)), 3)
  expect_equal(
    pruned_dag %>%
      pull_dag_data() %>%
      dplyr::filter(name == "z", to == "x") %>%
      nrow(),
    0
  )
})
