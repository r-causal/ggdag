context("themes")
set.seed(1234)

test_that("themes look good", {
  p <- ggdag(test_dag)
  expect_identical(theme_dag, theme_dag_blank)
  expect_identical(theme_dag_gray, theme_dag_grey)
  expect_identical(theme_dag_gray_grid, theme_dag_grey_grid)
  vdiffr::expect_doppelganger("theme_dag()", p + theme_dag())
  vdiffr::expect_doppelganger("theme_dag_grid()", p + theme_dag_grid())
  vdiffr::expect_doppelganger("theme_dag_gray()", p + theme_dag_gray())
  vdiffr::expect_doppelganger("theme_dag_gray_grid()", p + theme_dag_gray_grid())
})

