# The repel label geoms take the same `label` fallback chain the automatic
# label geoms take: a mapping the user wrote, then the DAG's labels, then the
# node names, each trumping the next. A bare `geom_dag_label_repel()` used to
# fail for want of a `label` aesthetic, which is a geom whose whole job is to
# place a DAG's labels refusing to place them.
#
# `geom_dag_text()` and `geom_dag_label()` name their nodes rather than label
# them, so the chain must not reach them: they always draw the node names.

repel_labelled_dag <- function() {
  dagify(
    y ~ m + x,
    m ~ x,
    labels = c(x = "Exposure", m = "Mediator", y = "Outcome"),
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )
}

repel_unlabelled_dag <- function() {
  dagify(
    y ~ m + x,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )
}

repel_stat_layer_index <- function(plot) {
  index <- which(vapply(
    plot$layers,
    function(layer) inherits(layer$stat, "StatNodesRepel"),
    logical(1)
  ))
  expect_length(index, 1)
  index
}

# The expression the repel layer maps `label` to, or NULL when it maps none of
# its own and inherits the plot's.
repel_label_expr <- function(plot) {
  mapping <- plot$layers[[repel_stat_layer_index(plot)]]$mapping$label
  if (is.null(mapping)) {
    return(NULL)
  }
  rlang::quo_get_expr(mapping)
}

# The text the repel layer places. `StatNodesRepel` hands ggrepel a skeleton of
# obstacle points alongside the labels, and those rows carry an empty label.
repel_drawn_labels <- function(plot) {
  built <- ggplot2::ggplot_build(plot)$data[[repel_stat_layer_index(plot)]]
  built$label[nzchar(built$label)]
}

repel_label_geoms <- list(
  geom_dag_label_repel,
  geom_dag_text_repel,
  geom_dag_label_repel2,
  geom_dag_text_repel2
)

test_that("the repel label geoms fall back to a labelled DAG's labels", {
  labels <- c("Exposure", "Mediator", "Outcome")

  for (label_geom in repel_label_geoms) {
    plot <- ggplot(repel_labelled_dag(), aes_dag()) +
      geom_dag_point() +
      label_geom()

    expect_identical(repel_label_expr(plot), quote(.data$label))
    expect_setequal(repel_drawn_labels(plot), labels)
  }
})

test_that("the repel label geoms fall back to names on an unlabelled DAG", {
  for (label_geom in repel_label_geoms) {
    plot <- ggplot(repel_unlabelled_dag(), aes_dag()) +
      geom_dag_point() +
      label_geom()

    expect_identical(repel_label_expr(plot), quote(.data$name))
    expect_setequal(repel_drawn_labels(plot), c("x", "m", "y"))
  }
})

test_that("a repel label column of blanks falls back to node names", {
  for (blank in list(NA_character_, "")) {
    node_data <- pull_dag_data(tidy_dagitty(repel_labelled_dag()))
    node_data$label <- blank

    for (label_geom in repel_label_geoms) {
      plot <- ggplot(node_data, aes_dag()) +
        geom_dag_point() +
        label_geom()

      expect_identical(repel_label_expr(plot), quote(.data$name))
      expect_setequal(repel_drawn_labels(plot), c("x", "m", "y"))
    }
  }
})

test_that("a partly labelled DAG falls back to the labels it carries", {
  partly_labelled <- dagify(
    y ~ m + x,
    m ~ x,
    labels = c(x = "Exposure", y = "Outcome"),
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )

  for (label_geom in repel_label_geoms) {
    plot <- ggplot(partly_labelled, aes_dag()) +
      geom_dag_point() +
      label_geom()

    expect_identical(repel_label_expr(plot), quote(.data$label))
    expect_setequal(repel_drawn_labels(plot), c("Exposure", "Outcome"))
  }
})

test_that("a layer-level label mapping wins over the repel default", {
  for (label_geom in repel_label_geoms) {
    plot <- ggplot(repel_labelled_dag(), aes_dag()) +
      geom_dag_point() +
      label_geom(ggplot2::aes(label = name))

    expect_identical(repel_label_expr(plot), quote(name))
    expect_setequal(repel_drawn_labels(plot), c("x", "m", "y"))
  }
})

test_that("a plot-level label mapping wins over the repel default", {
  for (label_geom in repel_label_geoms) {
    plot <- ggplot(repel_labelled_dag(), aes_dag(label = name)) +
      geom_dag_point() +
      label_geom()

    # an inherited plot mapping counts as the layer's, so nothing is injected
    expect_null(repel_label_expr(plot))
    expect_setequal(repel_drawn_labels(plot), c("x", "m", "y"))
  }
})

test_that("a layer-level label mapping beats a plot-level one", {
  for (label_geom in repel_label_geoms) {
    plot <- ggplot(repel_labelled_dag(), aes_dag(label = name)) +
      geom_dag_point() +
      label_geom(ggplot2::aes(label = label))

    expect_identical(repel_label_expr(plot), quote(label))
    expect_setequal(
      repel_drawn_labels(plot),
      c("Exposure", "Mediator", "Outcome")
    )
  }
})

test_that("the chain does not reach the geoms that name their nodes", {
  for (label_geom in list(geom_dag_text, geom_dag_label)) {
    plot <- ggplot(repel_labelled_dag(), aes_dag()) +
      geom_dag_point() +
      label_geom()

    expect_identical(
      rlang::quo_get_expr(plot$layers[[2]]$mapping$label),
      quote(.data$name)
    )
    built <- ggplot2::ggplot_build(plot)$data[[2]]
    expect_setequal(built$label, c("x", "m", "y"))
  }
})

test_that("a repel label layer with no mapping still reports a missing x", {
  node_data <- pull_dag_data(tidy_dagitty(repel_labelled_dag()))

  for (label_geom in repel_label_geoms) {
    plot <- ggplot(
      node_data,
      ggplot2::aes(y = y, xend = xend, yend = yend)
    ) +
      label_geom()

    expect_error(ggplot2::ggplot_build(plot), class = "ggdag_missing_error")
  }
})

# The picture the fallback makes. A bare repel label layer used to draw
# nothing at all, so this is a new baseline rather than a changed one.
test_that("repel visuals: a labelled DAG's own labels by default", {
  p <- ggplot(repel_labelled_dag(), aes_dag()) +
    geom_dag_point() +
    geom_dag_edges() +
    geom_dag_label_repel() +
    theme_dag()
  expect_doppelganger("repel-default-labels", p)
})
