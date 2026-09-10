# `StatNodesRepel` overrides `compute_layer()`, so the check ggplot2 makes for
# a stat's required aesthetics never runs on it. A repel label layer missing
# one used to fail inside dplyr, on a bare `label` that resolved to the base
# function. The stat asks for what it reads instead, with the error class the
# automatic label geoms use for the same question.

repel_node_data <- function() {
  pull_dag_data(tidy_dagitty(dagify(
    y ~ m + x,
    m ~ x,
    labels = c(x = "Exposure", m = "Mediator", y = "Outcome"),
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )))
}

# A plot with no `x` on it. The labels are mapped, so the missing aesthetic is
# the DAG one whatever the layer falls back to for its text.
repel_plot_without_x <- function(label_geom) {
  ggplot(
    repel_node_data(),
    ggplot2::aes(y = y, xend = xend, yend = yend, label = label)
  ) +
    label_geom()
}

repel_label_geoms <- list(
  geom_dag_label_repel,
  geom_dag_text_repel,
  geom_dag_label_repel2,
  geom_dag_text_repel2
)

test_that("the repel label geoms report a missing x aesthetic", {
  for (label_geom in repel_label_geoms) {
    plot <- repel_plot_without_x(label_geom)
    expect_error(ggplot2::ggplot_build(plot), class = "ggdag_missing_error")
  }
})

test_that("the repel label geoms name the aesthetic they are missing", {
  expect_ggdag_error(
    ggplot2::ggplot_build(repel_plot_without_x(geom_dag_label_repel))
  )
})

test_that("a repel label layer holding every aesthetic still builds", {
  for (label_geom in repel_label_geoms) {
    plot <- ggplot(repel_node_data(), aes_dag(label = label)) +
      geom_dag_point() +
      label_geom()

    expect_no_error(ggplot2::ggplot_build(plot))
  }
})
