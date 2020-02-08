context("ggdag_canonical")
set.seed(1234)

test_that("dags cannonicalize correctly", {
  dag <- dagify(y ~ x + z, x ~~ z)
  p <- ggdag_canonical(dag)
  vdiffr::expect_doppelganger("ggdag_canonical() expands ~~", p)
})
