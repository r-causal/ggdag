set.seed(1234)

test_that("d relationships correctly identified", {
  test_dag <- dagify(m ~ x + y) %>%
    tidy_dagitty()
  p1 <- ggdag_drelationship(test_dag, "x", "y")
  p2 <- ggdag_drelationship(test_dag, "x", "y", controlling_for = "m")
  p3 <- ggdag_drelationship(test_dag, "x", "y", controlling_for = "m", collider_lines = FALSE)
  expect_doppelganger("ggdag_drelationship() d-separates x and y", p1)
  expect_doppelganger("ggdag_drelationship() d-connects x and y", p2 + theme_test() + theme(legend.position = "none"))
  expect_doppelganger("ggdag_drelationship() d-connects xy: no collider", p3)

  p4 <- ggdag_dseparated(test_dag, "x", "y")
  p5 <- ggdag_dconnected(test_dag, "x", "y")
  expect_doppelganger("ggdag_dseparated() d-separates x and y", p4)
  expect_doppelganger("ggdag_dconnected() d-separates x and y", p5)
})
