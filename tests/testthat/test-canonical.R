test_that("dags cannonicalize correctly", {
  withr::local_seed(1234)
  test_dag <- dagify(y ~ x + z, x ~ ~z)
  p <- ggdag_canonical(test_dag)
  expect_doppelganger("ggdag_canonical() expands ~~", p)
})
