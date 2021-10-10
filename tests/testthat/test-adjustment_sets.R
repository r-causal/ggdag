test_that("adjustment sets drawn correctly", {
  p <- ggdag_adjustment_set(test_dag)
  expect_doppelganger("ggdag_adjustment_set() renders", p)
})
