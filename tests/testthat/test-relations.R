context("relations")
set.seed(1234)

test_that("dags find relations correctly", {
  vdiffr::expect_doppelganger("ggdag_children() identifies `y`, `x`, and `z1`", ggdag_children(test_dag, "w1"))
  vdiffr::expect_doppelganger("ggdag_parents() identifies `z2`, `x`, `w1`, and `w2`", ggdag_parents(test_dag, "y"))
  vdiffr::expect_doppelganger("ggdag_ancestors() identifies `v`, `w1`, and `z1`", ggdag_ancestors(test_dag, "x"))
  vdiffr::expect_doppelganger("ggdag_descendants() identifies `y`, `x`, and `z1`", ggdag_descendants(test_dag, "w1"))
})
