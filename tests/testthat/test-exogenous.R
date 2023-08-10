set.seed(1234)

test_that("dags identify exogenous variables correctly", {
  exo_dag <- dplyr::filter(node_exogenous(test_dag), exogenous == "exogenous")
  exo_vars <- unique(pull_dag_data(exo_dag)$name)
  expect_equal(exo_vars, dagitty::exogenousVariables(pull_dag(test_dag)))
  p <- ggdag_exogenous(test_dag)
  expect_doppelganger("ggdag_exogenous() identifies v, w1, w2 as exogenous", p)
})
