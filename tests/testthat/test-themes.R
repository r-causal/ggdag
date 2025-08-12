set.seed(1234)

test_that("themes look good", {
  p <- ggdag(test_dag)
  expect_identical(theme_dag, theme_dag_blank)
  expect_identical(theme_dag_gray, theme_dag_grey)
  expect_identical(theme_dag_gray_grid, theme_dag_grey_grid)
  expect_doppelganger("theme_dag()", p + theme_dag())
  expect_doppelganger("theme_dag_grid()", p + theme_dag_grid())
  expect_doppelganger("theme_dag_gray()", p + theme_dag_gray())
  expect_doppelganger("theme_dag_gray_grid()", p + theme_dag_gray_grid())
})

test_that("themes work with facets", {
  p_facet <- ggdag_adjustment_set(test_dag) + theme_dag()
  expect_doppelganger("theme_dag() with facets", p_facet)

  p_facet_grid <- ggdag_adjustment_set(test_dag) + theme_dag_grid()
  expect_doppelganger("theme_dag_grid() with facets", p_facet_grid)

  p_facet_gray <- ggdag_adjustment_set(test_dag) + theme_dag_gray()
  expect_doppelganger("theme_dag_gray() with facets", p_facet_gray)

  p_facet_gray_grid <- ggdag_adjustment_set(test_dag) + theme_dag_gray_grid()
  expect_doppelganger("theme_dag_gray_grid() with facets", p_facet_gray_grid)
})
