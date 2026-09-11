# Tests for the arguments a built-in layout named by string takes. `layout =
# "time_ordered"` resolves to `time_ordered_coords()`, so the arguments that
# function takes have to reach it through `tidy_dagitty()`, while everything
# else in `...` keeps going to `ggraph::create_layout()`.

# Helpers ----------------------------------------------------------------------

# A contemporaneous exposure and outcome feeding one later collider. With
# `adjust_exposure_outcome = TRUE` the outcome is pushed a layer past the
# exposure; with `FALSE` the two share a layer.
collider_dag <- function() {
  dagify(q ~ x + y, exposure = "x", outcome = "y")
}

# One coordinate row per node, in a stable order, from either a `tidy_dagitty`
# or a plot built from one.
node_coords <- function(.x) {
  dag_data <- if (inherits(.x, "ggplot")) .x$data else pull_dag_data(.x)
  rows <- dplyr::arrange(dplyr::distinct(dag_data, name, x, y), name)

  # a fresh tibble, so that the layout attributes the data carries are not
  # part of what these tests compare
  tibble::tibble(
    name = as.character(rows$name),
    x = as.numeric(rows$x),
    y = as.numeric(rows$y)
  )
}

# The coordinate of one node along the time axis.
time_of <- function(coords, node) {
  coords$x[coords$name == node]
}

# Forwarding to the layout ------------------------------------------------------

test_that("tidy_dagitty(): a built-in layout takes its own arguments", {
  coords <- node_coords(tidy_dagitty(
    collider_dag(),
    layout = "time_ordered",
    adjust_exposure_outcome = FALSE
  ))

  # the exposure and the outcome are contemporaneous, and the collider they
  # both feed is the only node in the next layer
  expect_equal(time_of(coords, "x"), time_of(coords, "y"))
  expect_equal(time_of(coords, "q"), time_of(coords, "x") + 1)
})

test_that("tidy_dagitty(): the default layering is unchanged", {
  coords <- node_coords(tidy_dagitty(collider_dag(), layout = "time_ordered"))

  expect_equal(time_of(coords, "x"), 1)
  expect_equal(time_of(coords, "y"), 2)
  expect_equal(time_of(coords, "q"), 3)
})

test_that("tidy_dagitty(): the layout string matches the coordinate function", {
  named <- tidy_dagitty(
    collider_dag(),
    layout = "time_ordered",
    adjust_exposure_outcome = FALSE
  )
  built <- tidy_dagitty(dagify(
    q ~ x + y,
    exposure = "x",
    outcome = "y",
    coords = time_ordered_coords(adjust_exposure_outcome = FALSE)
  ))

  expect_equal(node_coords(named), node_coords(built))
})

test_that("tidy_dagitty(): direction and force_y reach the layout too", {
  dag <- collider_dag()

  down <- tidy_dagitty(dag, layout = "time_ordered", direction = "y")
  down_coords <- node_coords(down)
  # time runs down the panel, so the layers are the y coordinate
  expect_equal(down_coords$y[down_coords$name == "x"], 1)
  expect_equal(down_coords$y[down_coords$name == "y"], 2)
  expect_equal(down_coords$y[down_coords$name == "q"], 3)
  expect_equal(
    node_coords(down),
    node_coords(tidy_dagitty(dagify(
      q ~ x + y,
      exposure = "x",
      outcome = "y",
      coords = time_ordered_coords(direction = "y")
    )))
  )
  # the layout that placed the nodes names the axis its layers run along
  expect_identical(attr(pull_dag(down), "layout_direction"), "y")

  unforced <- tidy_dagitty(dag, layout = "time_ordered", force_y = FALSE)
  expect_equal(
    node_coords(unforced),
    node_coords(tidy_dagitty(dagify(
      q ~ x + y,
      exposure = "x",
      outcome = "y",
      coords = time_ordered_coords(force_y = FALSE)
    )))
  )
  # and skipping the force-directed pass is not a no-op
  expect_false(isTRUE(all.equal(
    node_coords(unforced)$y,
    node_coords(tidy_dagitty(dag, layout = "time_ordered"))$y
  )))
})

test_that("tidy_dagitty(): manual tiers reach the layout by name", {
  tiered <- tidy_dagitty(
    collider_dag(),
    layout = "time_ordered",
    .vars = list(c("x", "y"), "q")
  )

  coords <- node_coords(tiered)
  expect_equal(time_of(coords, "x"), 1)
  expect_equal(time_of(coords, "y"), 1)
  expect_equal(time_of(coords, "q"), 2)
})

# The quick plotters -------------------------------------------------------------

test_that("the quick plotters carry the layout arguments", {
  expected <- node_coords(tidy_dagitty(
    collider_dag(),
    layout = "time_ordered",
    adjust_exposure_outcome = FALSE
  ))

  plotted <- node_coords(ggdag(
    collider_dag(),
    layout = "time_ordered",
    adjust_exposure_outcome = FALSE
  ))
  expect_equal(plotted, expected)

  status <- node_coords(ggdag_status(
    collider_dag(),
    layout = "time_ordered",
    adjust_exposure_outcome = FALSE
  ))
  expect_equal(status, expected)
})

# Everything else still goes to ggraph --------------------------------------------

test_that("a ggraph layout still takes its own arguments", {
  dag <- collider_dag()

  centered <- node_coords(tidy_dagitty(
    dag,
    layout = "star",
    center = "q",
    use_existing_coords = FALSE
  ))

  # `center` is `ggraph::create_layout()`'s argument, and the node it names
  # sits at the middle of the star
  expect_equal(time_of(centered, "q"), 0)
  expect_equal(centered$y[centered$name == "q"], 0)
})

test_that("an argument no layout takes is still an error", {
  expect_error(
    tidy_dagitty(collider_dag(), layout = "time_ordered", zzz_unknown = 1),
    "unused argument"
  )
  expect_error(
    tidy_dagitty(
      collider_dag(),
      layout = "stress",
      zzz_unknown = 1,
      use_existing_coords = FALSE
    ),
    "unused argument"
  )
})

# Visual ---------------------------------------------------------------------------

test_that("a contemporaneous exposure and outcome share a layer", {
  expect_doppelganger(
    "time-ordered without the exposure-outcome shift",
    ggdag(
      collider_dag(),
      layout = "time_ordered",
      adjust_exposure_outcome = FALSE
    )
  )
})
