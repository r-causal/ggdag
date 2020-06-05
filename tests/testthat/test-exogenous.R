context("ggdag_exogenous")
set.seed(1234)

test_that("dags identify exogenous variables correctly", {
  exo_vars <- unique(filter(node_exogenous(test_dag), exogenous == "exogenous")$data$name)
  expect_equal(exo_vars, dagitty::exogenousVariables(test_dag$dag))
  p <- ggdag_exogenous(test_dag)
  vdiffr::expect_doppelganger("ggdag_exogenous() identifies v, w1, w2 as exogenous", p)
})
