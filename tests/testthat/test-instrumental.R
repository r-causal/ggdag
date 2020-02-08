context("ggdag_instrumental")
set.seed(1234)

test_that("dags identify IVs correctly", {
  p <- ggdag_instrumental(dagitty::dagitty("dag{ i->x->y; i2->x->y; x<->y }"), "x", "y")
  vdiffr::expect_doppelganger("ggdag_instrumental() identifies `i` and `i2` as instrumental", p)
})
