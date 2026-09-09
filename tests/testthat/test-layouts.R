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

# A DAG laid out down the panel: three time points along y, in two columns.
# The router's own inference reads the two columns as the layers, so the axis
# the layout ran along is the one that has to reach it.
down_panel_dag <- function() {
  dagify(
    y ~ x + m,
    m ~ x,
    b ~ a,
    c ~ b,
    coords = time_ordered_coords(
      list(c("x", "a"), c("m", "b"), c("y", "c")),
      direction = "y",
      optimize = FALSE
    )
  )
}

# The axis the one routed edge layer of `plot` was given.
routed_axis_of <- function(plot) {
  routed <- vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomDAGRoutedArrow"),
    logical(1)
  )
  expect_length(which(routed), 1)
  plot$layers[[which(routed)]]$geom_params$layer_axis
}

# Draw with the engine that routes, so the layer the axis reaches exists.
local_routing_options <- function(.env = parent.frame()) {
  local_ggdag_option_state(.env = .env)
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")
}

test_that("layout_coordinates(): a marked grid keeps its axis", {
  grid <- time_ordered_coords(
    list("x", "m", "y"),
    direction = "y",
    optimize = FALSE
  )
  edges <- data.frame(
    name = c("x", "m"),
    to = c("m", "y"),
    stringsAsFactors = FALSE
  )

  expect_identical(layout_direction(layout_coordinates(edges, grid)), "y")
})

test_that("update_dag_data(): a rebuilt layout replaces the axis", {
  skip_if_not_installed("ggarrow")
  local_routing_options()

  tidy_down <- tidy_dagitty(down_panel_dag())
  # dropping a coordinate column rebuilds the layout from the layout option,
  # which runs its layers across the panel rather than down it
  dropped <- dplyr::select(
    tidy_down,
    -dplyr::any_of(c("x", "y", "xend", "yend"))
  )

  expect_false(identical(attr(pull_dag(dropped), "layout_direction"), "y"))
  expect_identical(routed_axis_of(ggdag(dropped)), "auto")
})

test_that("update_dag_data(): coordinates from elsewhere drop the axis", {
  tidy_down <- tidy_dagitty(down_panel_dag())

  # the same DAG, turned onto its side by hand: the layout that ran down the
  # panel no longer describes where the nodes sit
  turned <- pull_dag_data(tidy_down)
  turned[c("x", "y", "xend", "yend")] <- turned[c("y", "x", "yend", "xend")]
  update_dag_data(tidy_down) <- turned

  expect_null(attr(pull_dag(tidy_down), "layout_direction"))
})

test_that("as_tidy_dagitty(): the coordinates a data frame is laid out with name the axis", {
  edges_df <- data.frame(
    name = c("x", "m"),
    to = c("m", "y"),
    stringsAsFactors = FALSE
  )

  from_coords <- as_tidy_dagitty(
    edges_df,
    coords = time_ordered_coords(
      list("x", "m", "y"),
      direction = "y",
      optimize = FALSE
    )
  )
  expect_identical(attr(pull_dag(from_coords), "layout_direction"), "y")

  from_layout <- as_tidy_dagitty(
    edges_df,
    layout = time_ordered_coords(direction = "y")
  )
  expect_identical(attr(pull_dag(from_layout), "layout_direction"), "y")

  # coordinates from anywhere else name no axis
  by_hand <- as_tidy_dagitty(
    edges_df,
    coords = data.frame(
      name = c("x", "m", "y"),
      x = c(0, 1, 2),
      y = c(0, 0, 0)
    )
  )
  expect_null(attr(pull_dag(by_hand), "layout_direction"))
})

test_that("dag_saturate(): reused coordinates keep the axis they name", {
  adjusted <- control_for(tidy_dagitty(down_panel_dag()), "m")

  saturated <- dag_saturate(adjusted, use_existing_coords = TRUE)
  expect_identical(attr(pull_dag(saturated), "layout_direction"), "y")

  # the bidirected edges of the input are put back on a rebuilt dagitty
  # object, which keeps the axis as well
  bidirected <- tidy_dagitty(dagify(
    y ~ x + m,
    m ~ x,
    x ~ ~y,
    coords = time_ordered_coords(
      list("x", "m", "y"),
      direction = "y",
      optimize = FALSE
    )
  ))
  expect_identical(
    attr(
      pull_dag(dag_saturate(bidirected, use_existing_coords = TRUE)),
      "layout_direction"
    ),
    "y"
  )

  # a fresh layout replaces the coordinates, and the axis with them
  expect_null(attr(pull_dag(dag_saturate(adjusted)), "layout_direction"))
})

test_that("the axis survives the verbs that rebuild the dagitty object", {
  skip_if_not_installed("ggarrow")
  local_routing_options()

  tidy_down <- tidy_dagitty(down_panel_dag())

  adjusted <- control_for(tidy_down, "m")
  pruned <- dag_prune(tidy_down, c(a = "b"))
  equivalent <- node_equivalent_dags(tidy_down)

  expect_identical(attr(pull_dag(adjusted), "layout_direction"), "y")
  expect_identical(attr(pull_dag(pruned), "layout_direction"), "y")
  expect_identical(attr(pull_dag(equivalent), "layout_direction"), "y")

  expect_identical(routed_axis_of(ggdag(adjusted)), "y")
  expect_identical(routed_axis_of(ggdag(pruned)), "y")
  expect_identical(routed_axis_of(ggdag_adjust(tidy_down, "m")), "y")
  expect_identical(routed_axis_of(ggdag_equivalent_dags(tidy_down)), "y")
})
