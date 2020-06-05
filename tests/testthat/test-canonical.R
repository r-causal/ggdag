context("ggdag_canonical")
set.seed(1234)

test_that("dags cannonicalize correctly", {
  test_dag2 <- dagify(y ~ x + z, x ~~ z)
  p <- ggdag_canonical(test_dag2)
  vdiffr::expect_doppelganger("ggdag_canonical() expands ~~", p)
})
