test_that("time ordered layout works", {
  withr::local_seed(1234)
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

  # auto time ordering produces valid coordinates
  auto_coords <- dagitty::coordinates(auto_coords_coords)
  expect_true(!anyNA(unlist(auto_coords)))

  auto_coords_layout <- dagify(
    d ~ c1 + c2 + c3,
    c1 ~ b1 + b2,
    c3 ~ a,
    b1 ~ a
  )

  # specifying in dagify or tidy_dagitty is the same
  expect_equal(
    tidy_dagitty(auto_coords_layout, layout = "time_ordered") |>
      pull_dag_data(),
    tidy_dagitty(auto_coords_coords) |> pull_dag_data()
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
  ) |>
    ggdag()

  expect_doppelganger("list time ordered coords", p1)
  expect_doppelganger("df time ordered coords", p2)
})

test_that("layout = time_ordered_coords() (function) works in tidy_dagitty", {
  dag <- dagify(
    d ~ c1 + c2 + c3,
    c1 ~ b1 + b2,
    c3 ~ a,
    b1 ~ a
  )

  result_fn <- tidy_dagitty(dag, layout = time_ordered_coords())
  result_str <- tidy_dagitty(dag, layout = "time_ordered")

  expect_equal(
    pull_dag_data(result_fn),
    pull_dag_data(result_str)
  )
})

test_that("layout = time_ordered_coords(list(...)) (tibble) works in tidy_dagitty", {
  coords <- time_ordered_coords(
    list(
      "a",
      c("b1", "b2"),
      c("c1", "c2", "c3"),
      "d"
    ),
    optimize = FALSE
  )

  dag <- dagify(
    d ~ c1 + c2 + c3,
    c1 ~ b1 + b2,
    c3 ~ a,
    b1 ~ a
  )

  result <- tidy_dagitty(dag, layout = coords)
  expect_s3_class(result, "tidy_dagitty")
})

test_that("layout = time_ordered_coords() works through ggdag()", {
  dag <- dagify(
    d ~ c1 + c2 + c3,
    c1 ~ b1 + b2,
    c3 ~ a,
    b1 ~ a
  )

  p <- ggdag(dag, layout = time_ordered_coords())
  expect_s3_class(p, "ggplot")
})

test_that("time_ordered_coords(): a missing time value errors", {
  time_df <- data.frame(name = c("a", "b", "c"), time = c(1, NA, 2))
  expect_error(
    time_ordered_coords(time_df),
    class = "ggdag_missing_error"
  )
  expect_ggdag_error(
    time_ordered_coords(data.frame(name = c("a", "b"), time = c(1, NA)))
  )
})

test_that("time_ordered_coords(): a character time column errors", {
  time_df <- data.frame(
    name = c("a", "b", "c"),
    time = c("baseline", "6 months", "12 months")
  )
  expect_error(
    time_ordered_coords(time_df),
    class = "ggdag_type_error"
  )
  expect_ggdag_error(
    time_ordered_coords(data.frame(name = "a", time = "baseline"))
  )
})

test_that("time_ordered_coords(): a factor time column errors", {
  time_df <- data.frame(
    name = c("a", "b"),
    time = factor(c("baseline", "follow-up"))
  )
  expect_error(
    time_ordered_coords(time_df),
    class = "ggdag_type_error"
  )
})

test_that("time_ordered_coords(): default time points are one per period", {
  vars <- list("a", c("b1", "b2"), c("c1", "c2", "c3"), "d")
  coords <- time_ordered_coords(vars, optimize = FALSE)
  expect_equal(sort(unique(coords$x)), seq_along(vars))
})

test_that("time_ordered_coords(): time_points is one element per period", {
  vars <- list("a", c("b1", "b2"), c("c1", "c2", "c3"), "d")
  coords <- time_ordered_coords(
    vars,
    time_points = c(1, 2, 4, 8),
    optimize = FALSE
  )
  expect_equal(sort(unique(coords$x)), c(1, 2, 4, 8))
})

test_that("time_ordered_coords(): time_points with a data frame errors", {
  time_df <- data.frame(name = c("a", "b", "c"), time = c(1, 2, 3))
  expect_error(
    time_ordered_coords(time_df, time_points = c(1, 2, 3)),
    class = "ggdag_error"
  )
  expect_ggdag_error(time_ordered_coords(time_df, time_points = c(1, 2, 3)))
})

test_that("time_ordered_coords(): a longer time_points errors with optimize = FALSE", {
  expect_error(
    time_ordered_coords(list("a", "b"), time_points = 1:3, optimize = FALSE),
    class = "ggdag_type_error"
  )
  expect_ggdag_error(
    time_ordered_coords(list("a", "b"), time_points = 1:3, optimize = FALSE)
  )
})

test_that("time_ordered_coords(): a shorter time_points errors with optimize = FALSE", {
  expect_error(
    time_ordered_coords(
      list("a", c("b", "c")),
      time_points = 1,
      optimize = FALSE
    ),
    class = "ggdag_type_error"
  )
  expect_ggdag_error(
    time_ordered_coords(
      list("a", c("b", "c")),
      time_points = 1,
      optimize = FALSE
    )
  )
})

test_that("time_ordered_coords(): a duplicated variable errors with optimize = FALSE", {
  expect_error(
    time_ordered_coords(list("a", c("a", "b")), optimize = FALSE),
    class = "ggdag_type_error"
  )
})

test_that("time_ordered_coords(): time_points is checked inside a dagify() call", {
  expect_error(
    dagify(
      b ~ a,
      coords = time_ordered_coords(
        list("a", "b"),
        time_points = 1,
        optimize = FALSE
      )
    ),
    class = "ggdag_type_error"
  )

  expect_error(
    dagify(
      b ~ a,
      coords = time_ordered_coords(list("a", "b"), time_points = 1:3)
    ),
    class = "ggdag_type_error"
  )
})

# The axis the layers run along ------------------------------------------------

# A DAG whose coordinates the user wrote out: they belong to no layout, so
# nothing about them names an axis.
hand_coords_dag <- function() {
  dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
}

test_that("time_ordered_coords(): the coordinates record the direction of time", {
  edges_df <- data.frame(
    name = c("a", "b", "c"),
    to = c("b", "c", NA),
    stringsAsFactors = FALSE
  )

  down <- time_ordered_coords(direction = "y")
  expect_identical(attr(down(edges_df), "layout_direction"), "y")

  across <- time_ordered_coords()
  expect_identical(attr(across(edges_df), "layout_direction"), "x")

  # the unoptimized path hands back the coordinates themselves, and they
  # carry the direction the same way
  spread <- time_ordered_coords(
    list("a", "b", "c"),
    direction = "y",
    optimize = FALSE
  )
  expect_identical(attr(spread, "layout_direction"), "y")
})

test_that("dagify(): a time-ordered layout records its direction on the DAG", {
  down <- dagify(
    y ~ x + m,
    m ~ x,
    coords = time_ordered_coords(direction = "y")
  )
  expect_identical(attr(down, "layout_direction"), "y")

  across <- dagify(y ~ x + m, m ~ x, coords = time_ordered_coords())
  expect_identical(attr(across, "layout_direction"), "x")

  spread <- dagify(
    y ~ x + m,
    m ~ x,
    coords = time_ordered_coords(
      list("x", "m", "y"),
      direction = "y",
      optimize = FALSE
    )
  )
  expect_identical(attr(spread, "layout_direction"), "y")

  expect_null(attr(hand_coords_dag(), "layout_direction"))
})

test_that("tidy_dagitty(): the direction survives tidying and the dplyr verbs", {
  down <- dagify(
    y ~ x + m,
    m ~ x,
    coords = time_ordered_coords(direction = "y")
  )

  tidy_down <- tidy_dagitty(down)
  expect_identical(attr(pull_dag(tidy_down), "layout_direction"), "y")
  expect_identical(
    attr(pull_dag(dplyr::mutate(tidy_down, kept = TRUE)), "layout_direction"),
    "y"
  )

  # a layout named on the call records itself just as one named in dagify()
  named <- tidy_dagitty(
    dagify(y ~ x + m, m ~ x),
    layout = time_ordered_coords(direction = "y")
  )
  expect_identical(attr(pull_dag(named), "layout_direction"), "y")

  # and coordinates from anywhere else leave nothing behind
  moved <- tidy_dagitty(
    down,
    layout = data.frame(
      name = c("x", "m", "y"),
      x = c(0, 1, 2),
      y = c(0, 0, 0)
    )
  )
  expect_null(attr(pull_dag(moved), "layout_direction"))
  expect_null(attr(
    pull_dag(tidy_dagitty(hand_coords_dag())),
    "layout_direction"
  ))
})

test_that("layout_layer_axis(): only a layout down the panel names an axis", {
  down <- tidy_dagitty(dagify(
    y ~ x + m,
    m ~ x,
    coords = time_ordered_coords(direction = "y")
  ))
  across <- tidy_dagitty(dagify(
    y ~ x + m,
    m ~ x,
    coords = time_ordered_coords()
  ))

  # the router infers layers along x already, so a layout across the panel
  # has nothing to add and leaves the inference to it
  expect_identical(layout_layer_axis(down), "y")
  expect_identical(layout_layer_axis(across), "auto")
  expect_identical(layout_layer_axis(tidy_dagitty(hand_coords_dag())), "auto")
  expect_identical(layout_layer_axis(NULL), "auto")

  # the plot's own data carries it to the layers
  expect_identical(
    layout_layer_axis(ggplot2::ggplot(down, aes_dag())$data),
    "y"
  )
})
