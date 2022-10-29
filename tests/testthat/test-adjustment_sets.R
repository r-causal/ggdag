test_that("adjustment sets drawn correctly", {
  p <- ggdag_adjustment_set(test_dag)
  expect_doppelganger("ggdag_adjustment_set() renders", p)
})

test_that("adjustment sets drawn correctly with width set low", {
  withr::with_options(
    list(width = 20),
    {
      p <- ggdag_adjustment_set(test_dag)
      expect_doppelganger("ggdag_adjustment_set() renders with low width", p)
    }
  )
})
