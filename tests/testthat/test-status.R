set.seed(1234)

test_that("dags have correct status", {
  test_dag <- dagify(
    l ~ x + y,
    y ~ x,
    exposure = "x",
    outcome = "y",
    latent = "l"
  )

  p <- ggdag_status(test_dag)

  expect_doppelganger("ggdag_status() `x` as exposure, `y` as outcome, and `l` as latent", p)
})
