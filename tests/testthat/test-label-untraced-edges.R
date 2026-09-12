# The automatic label geoms keep their labels clear of the edges that reach
# them in their own data. A plot whose edges are drawn from somewhere else
# leaves the engine nothing to avoid, and the labels land on the drawing, so
# the plot says so once rather than placing them silently.

# A four-node chain with a skip edge, labelled on every node.
untraced_chain <- function() {
  dagify(
    d ~ a + b + c,
    c ~ b,
    b ~ a,
    labels = c(
      a = "Alpha node",
      b = "Beta node",
      c = "Gamma node",
      d = "Delta node"
    ),
    coords = list(
      x = c(a = 0, b = 1, c = 2, d = 3),
      y = c(a = 0, b = 0, c = 0, d = 0)
    )
  )
}

# The plot's rows with their endpoints blanked, which is a node-only data
# frame that still carries the endpoint columns.
nodes_without_edges <- function(tidy_dag) {
  dplyr::mutate(
    pull_dag_data(tidy_dag),
    xend = NA_real_,
    yend = NA_real_
  )
}

# Every `ggdag_untraced_edges_warning` that building `plot` emits. They are
# muffled as they are collected, so the count is exact and none escapes as
# stray output.
untraced_warnings <- function(plot) {
  collected <- list()
  withCallingHandlers(
    invisible(ggplot2::ggplot_build(plot)),
    warning = function(w) {
      collected[[length(collected) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  purrr::keep(collected, \(w) inherits(w, "ggdag_untraced_edges_warning"))
}

test_that("automatic labels report edges drawn from data they never see", {
  skip_if_not_installed("ggarrow")

  tidy_dag <- tidy_dagitty(untraced_chain())
  edges <- dplyr::filter(pull_dag_data(tidy_dag), !is.na(.data$direction))

  plot <- ggplot(nodes_without_edges(tidy_dag), aes_dag()) +
    geom_dag_routed_arrows(data_directed = edges, route = "spline") +
    geom_dag_point() +
    geom_dag_label_auto()

  warned <- untraced_warnings(plot)
  expect_length(warned, 1)
  expect_s3_class(warned[[1]], "ggdag_untraced_edges_warning")
  expect_match(conditionMessage(warned[[1]]), "edges")
})

test_that("the report is made once for the plot, not once for each label", {
  skip_if_not_installed("ggarrow")

  # four labels, one report
  tidy_dag <- tidy_dagitty(untraced_chain())
  edges <- dplyr::filter(pull_dag_data(tidy_dag), !is.na(.data$direction))

  plot <- ggplot(nodes_without_edges(tidy_dag), aes_dag()) +
    geom_dag_routed_arrows(data_directed = edges, route = "spline") +
    geom_dag_point() +
    geom_dag_text_auto()

  expect_length(untraced_warnings(plot), 1)
})

test_that("a plot with no edge layer places its labels without a report", {
  tidy_dag <- tidy_dagitty(untraced_chain())

  plot <- ggplot(nodes_without_edges(tidy_dag), aes_dag()) +
    geom_dag_point() +
    geom_dag_label_auto()

  expect_length(untraced_warnings(plot), 0)
})

test_that("a plot whose own rows carry its edges places labels without a report", {
  tidy_dag <- tidy_dagitty(untraced_chain())

  straight <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_label_auto()
  expect_length(untraced_warnings(straight), 0)

  skip_if_not_installed("ggarrow")
  routed <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_routed_arrows(route = "spline") +
    geom_dag_point() +
    geom_dag_label_auto()
  expect_length(untraced_warnings(routed), 0)
})

test_that("an edge layer with no edges to draw leaves the labels silent", {
  # a DAG of two isolated nodes draws an empty edge layer, which covers
  # nothing
  isolated <- dagitty::dagitty("dag { x ; y }")
  label(isolated) <- c(x = "Exposure node", y = "Outcome node")

  plot <- ggplot(tidy_dagitty(isolated), aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_label_auto()

  expect_length(untraced_warnings(plot), 0)
})
