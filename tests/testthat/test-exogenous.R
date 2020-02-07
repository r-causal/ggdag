context("ggdag_exogenous")
set.seed(1234)

test_that("dags identify exogenous variables correctly", {
  exo_vars <- unique(filter(node_exogenous(dag), exogenous == "exogenous")$data$name)
  expect_equal(exo_vars, dagitty::exogenousVariables(dag$dag))
  p <- ggdag_exogenous(dag)
  vdiffr::expect_doppelganger("ggdag_exogenous() identifies v, w1, w2 as exogenous", p)
})
