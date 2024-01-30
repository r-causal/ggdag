test_that("adjustment sets drawn correctly", {
  p1 <- ggdag_adjustment_set(test_dag)
  expect_doppelganger("ggdag_adjustment_set() renders", p1)

  p2 <- ggdag_adjustment_set(test_dag, shadow = FALSE)
  expect_doppelganger("ggdag_adjustment_set() renders without shadows", p2)
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
