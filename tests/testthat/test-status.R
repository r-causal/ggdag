test_that("dags have correct status", {
  test_dag <- dagify(
    l ~ x + y,
    y ~ x,
    exposure = "x",
    outcome = "y",
    latent = "l",
    coords = time_ordered_coords()
  )

  p <- ggdag_status(test_dag)

  expect_doppelganger(
    "ggdag_status() `x` as exposure, `y` as outcome, and `l` as latent",
    p
  )
})

test_that("ggdag_status() renders curved edges via geom_dag()", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ curved(x, -0.5) + m,
    m ~ x,
    exposure = "x",
    outcome = "y",
    coords = list(x = c(x = 1, m = 2, y = 3), y = c(x = 0, m = 0, y = 0))
  )
  withr::local_options(ggdag.edge_engine = "ggarrow")
  p <- ggdag_status(dag)

  expect_doppelganger("ggdag_status with curved edges", p)
})

test_that("ggdag_status() passes ... to tidy_dagitty()", {
  dag <- dagify(
    l ~ x + y,
    y ~ x,
    exposure = "x",
    outcome = "y",
    latent = "l"
  )

  expected <- tidy_node_coords(tidy_dagitty(dag, layout = "circle"))
  actual <- node_coords(ggdag_status(dag, layout = "circle"))

  expect_equal(actual$name, expected$name)
  expect_equal(actual$x, expected$x)
  expect_equal(actual$y, expected$y)
})

test_that("ggdag_status() lays out a circle passed through ...", {
  dag <- dagify(
    l ~ x + y,
    y ~ x,
    exposure = "x",
    outcome = "y",
    latent = "l"
  )

  expect_doppelganger(
    "ggdag_status() circle layout through dots",
    ggdag_status(dag, layout = "circle")
  )
})
