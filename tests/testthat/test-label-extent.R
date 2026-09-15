# A five-node DAG whose labels are long enough that the panel has to make room
# for them, laid out by the default time-ordered layout.
label_room_dag <- function() {
  dagify(
    exam ~ podcast + prepared + mood,
    podcast ~ mood + humor + prepared,
    exposure = "podcast",
    outcome = "exam",
    labels = c(
      podcast = "Listened to a podcast",
      exam = "Exam score",
      mood = "Mood before the exam",
      humor = "Sense of humor",
      prepared = "Time spent preparing"
    )
  )
}

# The ranges the panel of `plot` ends up with, after every layer has trained the
# position scales and the expansion has been applied.
panel_ranges <- function(plot) {
  params <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]

  list(x = params$x.range, y = params$y.range)
}

# How much of the panel of `plot` lies beyond the outermost node, as a fraction
# of the panel, taking the tighter of the two sides of each axis. This is the
# room a label beside an edge node has to sit in.
node_clearance <- function(plot, dag) {
  nodes <- pull_dag_data(tidy_dagitty(dag))
  ranges <- panel_ranges(plot)

  clearance <- function(range, values) {
    values <- values[is.finite(values)]
    min(min(values) - range[[1]], range[[2]] - max(values)) / diff(range)
  }

  c(
    x = clearance(ranges$x, c(nodes$x, nodes$xend)),
    y = clearance(ranges$y, c(nodes$y, nodes$yend))
  )
}

test_that("a plot with no labels keeps the panel it has always had", {
  dag <- label_room_dag()
  nodes <- pull_dag_data(tidy_dagitty(dag))
  ranges <- panel_ranges(ggdag(dag))

  expected <- function(values) {
    span <- range(values[is.finite(values)])
    span + c(-1, 1) * 0.1 * diff(span)
  }

  expect_equal(ranges$x, expected(c(nodes$x, nodes$xend)))
  expect_equal(ranges$y, expected(c(nodes$y, nodes$yend)))
})

test_that("a labelled plot reserves panel room the unlabelled one does not", {
  dag <- label_room_dag()
  bare <- panel_ranges(ggdag(dag))
  labelled <- panel_ranges(
    ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto)
  )

  expect_gt(diff(labelled$x), diff(bare$x))
  expect_gt(diff(labelled$y), diff(bare$y))
})

test_that("the repel label geoms reserve no room", {
  # ggrepel pushes labels apart until its forces settle and never measures a
  # candidate spot against the panel's edge, so an emptier panel is one it
  # pushes further into: room reserved at the edge carries its labels away from
  # the nodes they name. The room is reserved for the geoms that place labels
  # in the panel they are given.
  dag <- tidy_dagitty(label_room_dag())
  reserved <- c("xmin", "xmax", "ymin", "ymax")

  auto <- ggplot2::layer_data(
    ggplot2::ggplot(dag, aes_dag()) +
      geom_dag_label_auto(ggplot2::aes(label = label)),
    1
  )
  repel <- ggplot2::layer_data(
    ggplot2::ggplot(dag, aes_dag()) +
      geom_dag_label_repel(ggplot2::aes(label = label)),
    1
  )

  expect_true(all(reserved %in% names(auto)))
  expect_false(any(reserved %in% names(repel)))
})

# A DAG whose discs already fill the panel they are drawn in.
crowded_room_dag <- function() {
  names <- paste0("v", 1:12)
  dagify(
    v11 ~ v1 + v2 + v3 + v4 + v5,
    v12 ~ v6 + v7 + v8 + v9 + v10,
    v10 ~ v1 + v2,
    v9 ~ v3 + v4,
    labels = stats::setNames(paste("Variable", 1:12), names)
  )
}

test_that("a crowded DAG reserves no room", {
  # The panel is a fixed size on the page, so a strip given to the margin is a
  # strip taken from the drawing. A DAG that leaves its panel mostly empty can
  # spare it; one whose discs already fill the panel cannot, and the labels it
  # had been fitting between its nodes would be thrown out to the border.
  bare <- panel_ranges(ggdag(crowded_room_dag()))
  labelled <- panel_ranges(
    ggdag(
      crowded_room_dag(),
      use_labels = TRUE,
      label_geom = geom_dag_label_auto
    )
  )

  expect_equal(labelled$x, bare$x)
  expect_equal(labelled$y, bare$y)
})

test_that("bigger nodes leave the panel less to spare", {
  dag <- label_room_dag()
  default <- node_clearance(
    ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto),
    dag
  )
  big <- node_clearance(
    ggdag(
      dag,
      use_labels = TRUE,
      label_geom = geom_dag_label_auto,
      node_size = 30
    ),
    dag
  )

  expect_lt(big[["x"]], default[["x"]])
  expect_lt(big[["y"]], default[["y"]])
})

# The millimetres of panel the drawn node discs of one render leave clear,
# taking the tightest of the four sides. The engine works inside a panel inset
# by half a node radius, so the room the reservation asks for arrives here less
# that inset: a negative figure means a disc sits outside the bounds the engine
# places labels in, with nothing beyond it to put a label in.
node_room_mm <- function(capture) {
  nodes <- capture$inputs$nodes
  bounds <- capture$inputs$bounds

  min(
    nodes$x - nodes$radius - bounds[[1]],
    nodes$y - nodes$radius - bounds[[2]],
    bounds[[3]] - (nodes$x + nodes$radius),
    bounds[[4]] - (nodes$y + nodes$radius)
  )
}

test_that("a labelled DAG drawn small leaves room beyond its outermost nodes", {
  skip_if_not_installed("ragg")

  capture <- perf_dag_capture(label_room_dag(), "spline", size = c(4.5, 3.5))

  # Without the reservation the outermost disc of this scene sits 1.9 mm
  # outside the bounds the engine places labels in, so the panel offers a
  # label beside it nothing at all.
  expect_gt(node_room_mm(capture), 0)
})

test_that("a labelled DAG keeps its labels near the nodes they name", {
  skip_if_not_installed("ragg")
  skip_unless_reference_label_font()

  capture <- perf_dag_capture(label_room_dag(), "spline", size = c(7, 5))
  boxes <- capture$placement$boxes
  labels <- capture$inputs$labels
  distance <- sqrt((boxes$x - labels$x)^2 + (boxes$y - labels$y)^2)

  # Without room reserved at the panel's edge the furthest label of this scene
  # sits 28.2 mm from the node it names, on a leader that crosses the drawing.
  expect_lt(max(distance), 27)
})

test_that("a labelled DAG renders with room around its labels", {
  expect_doppelganger(
    "labelled DAG with reserved room",
    ggdag(label_room_dag(), use_labels = TRUE, label_geom = geom_dag_label_auto)
  )
})
