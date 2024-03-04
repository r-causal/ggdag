test_that("time ordered layout works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  coords <- time_ordered_coords(list(
    # time point 1
    "a",
    # time point 2
    c("b1", "b2"),
    # time point 3
    c("c1", "c2", "c3"),
    # time point 4
    "d"
  ))

  d1 <- dagify(
    d ~ c1 + c2 + c3,
    c1 ~ b1 + b2,
    c3 ~ a,
    b1 ~ a,
    coords = coords
  )

  p1 <- ggdag(d1)

  auto_coords_coords <- dagify(
    d ~ c1 + c2 + c3,
    c1 ~ b1 + b2,
    c3 ~ a,
    b1 ~ a,
    coords = time_ordered_coords()
  )

  # auto time ordering is the same
  expect_equal(
    coords2list(coords),
    dagitty::coordinates(auto_coords_coords)
  )

  auto_coords_layout <- dagify(
    d ~ c1 + c2 + c3,
    c1 ~ b1 + b2,
    c3 ~ a,
    b1 ~ a
  )

  # specifying in dagify or tidy_dagitty is the same
  expect_equal(
    tidy_dagitty(auto_coords_layout, layout = "time_ordered") %>% pull_dag_data(),
    tidy_dagitty(auto_coords_coords) %>% pull_dag_data()
  )

  # or use a data frame
  x <- data.frame(
    name = c("x1", "x2", "y", "z1", "z2", "z3", "a"),
    time = c(1, 1, 2, 3, 3, 3, 4)
  )

  p2 <- dagify(
    z3 ~ y,
    y ~ x1 + x2,
    a ~ z1 + z2 + z3,
    coords = time_ordered_coords(x)
  ) %>%
    ggdag()

  expect_doppelganger("list time ordered coords", p1)
  expect_doppelganger("df time ordered coords", p2)
})
