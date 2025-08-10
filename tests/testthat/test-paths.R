set.seed(1234)

test_that("DAG paths are identified and drawn correctly", {
  coords_confounder <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    0,
    0,
    "y",
    2,
    0,
    "z",
    1,
    1
  )

  confounder_triangle_dag <- dagify(
    x ~ z,
    y ~ x + z,
    exposure = "x",
    outcome = "y",
    coords = coords_confounder
  )

  coords_butterfly <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    0,
    0,
    "y",
    2,
    0,
    "a",
    0,
    1,
    "b",
    2,
    1,
    "m",
    1,
    .5
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
    ggdag_paths_fan()

  p3 <- confounder_triangle_dag %>%
    ggdag_paths(from = "x", to = "y", shadow = FALSE)

  p4 <- butterfly_bias_dag %>%
    ggdag_paths_fan(shadow = FALSE)

  expect_doppelganger("ggdag_paths() draws 2 open paths", p1)
  expect_doppelganger("ggdag_paths_fan() draws 4 open paths", p2)
  expect_doppelganger("ggdag_paths() draws 2 open paths without shadows", p3)
  expect_doppelganger(
    "ggdag_paths_fan() draws 4 open paths without shadows",
    p4
  )
})

test_that("dag_paths() handles no open paths correctly (issue #180)", {
  # Create a DAG where Treatment and Outcome are d-separated by a collider
  dag <- dagify(
    Censoring ~ Treatment + Age,
    Outcome ~ Age,
    exposure = "Treatment",
    outcome = "Outcome"
  )

  # This should not throw an error
  expect_no_error(result <- dag_paths(dag, from = "Treatment", to = "Outcome"))

  # Check that the result is a valid tidy_dagitty object
  expect_true(is.tidy_dagitty(result))

  # Check that the DAG structure is preserved
  dag_data <- pull_dag_data(result)
  expect_true("Treatment" %in% dag_data$name)
  expect_true("Outcome" %in% dag_data$name)
  expect_true("Censoring" %in% dag_data$name)
  expect_true("Age" %in% dag_data$name)

  # There should be a path column with all NA values
  expect_true("path" %in% names(dag_data))
  expect_true(all(is.na(dag_data$path)))

  # Verify using dagitty that paths are indeed separated
  expect_true(dagitty::dseparated(dag, "Treatment", "Outcome"))

  # Test that ggdag_paths() works with no open paths
  p_shadow <- ggdag_paths(dag, shadow = TRUE)
  p_no_shadow <- ggdag_paths(dag, shadow = FALSE)

  expect_doppelganger("ggdag_paths() with no open paths and shadow", p_shadow)
  expect_doppelganger("ggdag_paths() with no open paths no shadow", p_no_shadow)
})
