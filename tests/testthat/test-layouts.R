test_that("time ordered layout works", {
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

  p1 <- dagify(
    d ~ c1 + c2 + c3,
    c1 ~ b1 + b2,
    c3 ~ a,
    b1 ~ a,
    coords = coords
  ) %>% ggdag()

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
