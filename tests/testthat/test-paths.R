set.seed(1234)

test_that("DAG paths are identified and drawn correctly", {
  coords_confounder <- tibble::tribble(
    ~name, ~x, ~y,
    "x", 0, 0,
    "y", 2, 0,
    "z", 1, 1
  )

  confounder_triangle_dag <- dagify(
    x ~ z,
    y ~ x + z,
    exposure = "x",
    outcome = "y",
    coords = coords_confounder
  )


  coords_butterfly <- tibble::tribble(
    ~name, ~x, ~y,
    "x", 0, 0,
    "y", 2, 0,
    "a", 0, 1,
    "b", 2, 1,
    "m", 1, .5
  )

  butterfly_bias_dag <- dagify(
    m ~ a + b,
    x ~ a + m,
    y ~ b + x + m,
    exposure = "x",
    outcome = "y",
    coords = coords_butterfly
  )

  p1 <- confounder_triangle_dag %>%
    ggdag_paths(from = "x", to = "y")

  p2 <- butterfly_bias_dag %>%
    ggdag_paths_fan(shadow = TRUE)

  expect_doppelganger("ggdag_paths() draws 2 open paths", p1)
  expect_doppelganger("ggdag_paths_fan() draws 4 open paths", p2)
})
