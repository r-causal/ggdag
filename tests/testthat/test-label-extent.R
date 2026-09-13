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

test_that("the repel label geoms reserve the same room as the automatic ones", {
  dag <- label_room_dag()
  auto <- node_clearance(
    ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto),
    dag
  )
  repel <- node_clearance(ggdag(dag, use_labels = TRUE), dag)
  text_repel <- node_clearance(
    ggdag(dag, use_labels = TRUE, label_geom = geom_dag_text_repel),
    dag
  )

  expect_gte(repel[["x"]], auto[["x"]])
  expect_gte(repel[["y"]], auto[["y"]])
  expect_gte(text_repel[["x"]], auto[["x"]])
  expect_gte(text_repel[["y"]], auto[["y"]])
})

test_that("bigger nodes reserve more label room", {
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

  expect_gt(big[["x"]], default[["x"]])
  expect_gt(big[["y"]], default[["y"]])
})

test_that("a labelled DAG drawn small keeps its labels near their nodes", {
  skip_if_not_installed("ragg")

  capture <- perf_dag_capture(label_room_dag(), "spline", size = c(4.5, 3.5))
  boxes <- capture$placement$boxes
  labels <- capture$inputs$labels
  distance <- sqrt((boxes$x - labels$x)^2 + (boxes$y - labels$y)^2)

  # The same scene at 7 by 5 inches puts its furthest label 28 mm from the node
  # it names. A figure drawn smaller has less room, so its labels sit somewhat
  # further out, but a label the reader has to trace across the panel is the
  # failure this reserves room to avoid.
  expect_lt(max(distance), 35)
})
