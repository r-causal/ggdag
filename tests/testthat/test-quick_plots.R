context("quick_plots")
set.seed(1234)

test_that("quick plots render correctly", {
  expect_doppelganger("ggdag_m_bias() is an M", ggdag_m_bias())
  expect_doppelganger("ggdag_butterfly_bias() is a butterfly", ggdag_butterfly_bias())
  expect_doppelganger("ggdag_confounder_triangle() is triangle", ggdag_confounder_triangle())
  expect_doppelganger("ggdag_collider_triangle() is triangle, too", ggdag_collider_triangle())
})
