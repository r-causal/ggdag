context("ggdag_drelationship")
set.seed(1234)

test_that("d relationships correctly identified", {
  dag <- dagify(m ~ x + y) %>%
    tidy_dagitty()
  p1 <- ggdag_drelationship(dag, "x", "y")
  p2 <- ggdag_drelationship(dag, "x", "y", controlling_for = "m")
  p3 <- ggdag_drelationship(dag, "x", "y", controlling_for = "m", collider_lines = FALSE)
  vdiffr::expect_doppelganger("ggdag_drelationship() d-separates x and y", p1)
  vdiffr::expect_doppelganger("ggdag_drelationship() d-connects x and y", p2 + theme_test() + theme(legend.position = "none"))
  vdiffr::expect_doppelganger("ggdag_drelationship() d-connects x and y: no collider line", p3)

  p4 <- ggdag_dseparated(dag, "x", "y")
  p5 <- ggdag_dconnected(dag, "x", "y")
  vdiffr::expect_doppelganger("ggdag_dseparated() d-separates x and y", p4)
  vdiffr::expect_doppelganger("ggdag_dconnected() d-separates x and y", p5)
})
