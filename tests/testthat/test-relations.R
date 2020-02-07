context("relations")
set.seed(1234)

test_that("dags cannonicalize correctly", {
  vdiffr::expect_doppelganger("ggdag_children() identifies `y`, `x`, and `z1`", ggdag_children(dag, "w1"))
  vdiffr::expect_doppelganger("ggdag_parents() identifies `z2`, `x`, `w1`, and `w2`", ggdag_parents(dag, "y"))
  vdiffr::expect_doppelganger("ggdag_ancestors() identifies `v`, `w1`, and `z1`", ggdag_ancestors(dag, "x"))
  vdiffr::expect_doppelganger("ggdag_descendants() identifies `y`, `x`, and `z1`", ggdag_descendants(dag, "w1"))
})
