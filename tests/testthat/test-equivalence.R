context("ggdag_equivalent_dags")
set.seed(1234)

test_that("dags ............", {
  # non-reversible dag
  p1 <- ggdag_equivalent_dags(dag)
  p2 <- ggdag_equivalent_class(dag)
  vdiffr::expect_doppelganger("ggdag_equivalent_dags() plots no equivalent dags", p1)
  vdiffr::expect_doppelganger("ggdag_equivalent_class() plots no reversible edges", p2)

  # reversible dag
  g_ex <- dagify(y ~ x + z, x ~ z)
  p3 <- ggdag_equivalent_dags(g_ex)
  p4 <- ggdag_equivalent_class(g_ex)
  vdiffr::expect_doppelganger("ggdag_equivalent_dags() plots 6 equivalent dags", p3)
  vdiffr::expect_doppelganger("ggdag_equivalent_class() plots all reversible edges", p4)
})

