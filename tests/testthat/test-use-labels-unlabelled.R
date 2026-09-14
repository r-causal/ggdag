# `use_labels = TRUE` on a DAG that carries no labels is a silent no-op: the
# plot draws exactly as it would without the argument, and the label layer is
# dropped rather than left to fail on a `label` column that is not there.
#
# `geom_dag()` writes the label mapping itself, so the layer arrives with an
# explicit `label = label` and the fallback chain, which fires only when
# nothing maps `label`, never sees it. Dropping the layer is what keeps the
# no-op a no-op: the chain would otherwise turn `use_labels = TRUE` on an
# unlabelled DAG into a second copy of the node names.

unlabelled_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    coords = list(x = c(z = 0, x = 1, y = 2), y = c(z = 1, x = 0, y = 0))
  )
}

labelled_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder"),
    coords = list(x = c(z = 0, x = 1, y = 2), y = c(z = 1, x = 0, y = 0))
  )
}

partly_labelled_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome"),
    coords = list(x = c(z = 0, x = 1, y = 2), y = c(z = 1, x = 0, y = 0))
  )
}

# The label layers of a plot: the repel geoms and the automatic geoms are the
# layers whose job is to place a DAG's labels.
label_layer_indices <- function(plot) {
  which(vapply(
    plot$layers,
    function(layer) {
      inherits(layer$stat, c("StatNodesRepel", "StatNodesLabelAuto"))
    },
    logical(1)
  ))
}

# The text the plot's one label layer places. `StatNodesRepel` hands ggrepel a
# skeleton of obstacle points alongside the labels, and those rows carry an
# empty label.
drawn_labels <- function(plot) {
  index <- label_layer_indices(plot)
  expect_length(index, 1)
  built <- ggplot2::ggplot_build(plot)$data[[index]]
  built$label[nzchar(built$label) & !is.na(built$label)]
}

repel_label_geoms <- list(
  geom_dag_label_repel = geom_dag_label_repel,
  geom_dag_text_repel = geom_dag_text_repel,
  geom_dag_label_repel2 = geom_dag_label_repel2,
  geom_dag_text_repel2 = geom_dag_text_repel2
)

# Built when a block runs, so a geom missing from the namespace fails the blocks
# that enumerate it rather than every block in the file.
auto_label_geoms <- function() {
  list(
    geom_dag_label_auto = geom_dag_label_auto,
    geom_dag_label_auto2 = geom_dag_label_auto2,
    geom_dag_text_auto = geom_dag_text_auto
  )
}

test_that("ggdag(use_labels = TRUE) is a no-op on the default label geom", {
  plot <- ggdag(unlabelled_dag(), use_labels = TRUE)

  expect_no_condition(ggplot2::ggplot_build(plot))
  expect_length(label_layer_indices(plot), 0)
})

test_that("ggdag_status(use_labels = TRUE) is a no-op by default", {
  plot <- ggdag_status(unlabelled_dag(), use_labels = TRUE)

  expect_no_condition(ggplot2::ggplot_build(plot))
  expect_length(label_layer_indices(plot), 0)
})

test_that("use_labels on an unlabelled DAG is a no-op for every repel geom", {
  for (label_geom in repel_label_geoms) {
    plot <- ggdag(unlabelled_dag(), use_labels = TRUE, label_geom = label_geom)
    expect_no_condition(ggplot2::ggplot_build(plot))
    expect_length(label_layer_indices(plot), 0)

    status <- ggdag_status(
      unlabelled_dag(),
      use_labels = TRUE,
      label_geom = label_geom
    )
    expect_no_condition(ggplot2::ggplot_build(status))
    expect_length(label_layer_indices(status), 0)
  }
})

test_that("use_labels on an unlabelled DAG is a no-op for the auto geoms", {
  for (label_geom in auto_label_geoms()) {
    plot <- ggdag(unlabelled_dag(), use_labels = TRUE, label_geom = label_geom)
    expect_no_condition(ggplot2::ggplot_build(plot))
    expect_length(label_layer_indices(plot), 0)

    status <- ggdag_status(
      unlabelled_dag(),
      use_labels = TRUE,
      label_geom = label_geom
    )
    expect_no_condition(ggplot2::ggplot_build(status))
    expect_length(label_layer_indices(status), 0)
  }
})

test_that("use_labels still draws a labelled DAG's labels", {
  labels <- c("Exposure", "Outcome", "Confounder")

  for (label_geom in c(repel_label_geoms, auto_label_geoms())) {
    plot <- ggdag(labelled_dag(), use_labels = TRUE, label_geom = label_geom)
    expect_setequal(drawn_labels(plot), labels)

    status <- ggdag_status(
      labelled_dag(),
      use_labels = TRUE,
      label_geom = label_geom
    )
    expect_setequal(drawn_labels(status), labels)
  }

  default <- ggdag(labelled_dag(), use_labels = TRUE)
  expect_setequal(drawn_labels(default), labels)
})

test_that("use_labels draws the labels a partly labelled DAG carries", {
  for (label_geom in c(repel_label_geoms, auto_label_geoms())) {
    plot <- ggdag(
      partly_labelled_dag(),
      use_labels = TRUE,
      label_geom = label_geom
    )
    expect_setequal(drawn_labels(plot), c("Exposure", "Outcome"))
  }

  default <- ggdag(partly_labelled_dag(), use_labels = TRUE)
  expect_setequal(drawn_labels(default), c("Exposure", "Outcome"))
})

test_that("the label fallback chain is untouched by the no-op", {
  # nothing maps `label`, so the chain runs: the DAG's labels when it has
  # them, and the node names when it does not
  for (label_geom in repel_label_geoms) {
    labelled <- ggplot(labelled_dag(), aes_dag()) +
      geom_dag_point() +
      label_geom()
    expect_setequal(
      drawn_labels(labelled),
      c("Exposure", "Outcome", "Confounder")
    )

    unlabelled <- ggplot(unlabelled_dag(), aes_dag()) +
      geom_dag_point() +
      label_geom()
    expect_setequal(drawn_labels(unlabelled), c("x", "y", "z"))
  }
})

test_that("use_labels with an explicit label column still draws it", {
  named <- unlabelled_dag() |>
    tidy_dagitty() |>
    dplyr::mutate(note = paste0("node ", name))

  plot <- ggdag(named, use_labels = TRUE, label = note)
  expect_setequal(drawn_labels(plot), paste0("node ", c("x", "y", "z")))

  labelled <- ggdag(labelled_dag(), use_labels = TRUE, label = name)
  expect_setequal(drawn_labels(labelled), c("x", "y", "z"))
})

# The picture the no-op makes: the same DAG the plot draws without
# `use_labels`, nodes, edges, and node names, and no labels anywhere.
test_that("visuals: use_labels on an unlabelled DAG draws no labels", {
  p <- ggdag(unlabelled_dag(), use_labels = TRUE) + theme_dag()
  expect_doppelganger("use-labels-unlabelled-no-op", p)
})

# Any geom can draw the labels, and `label_geom` is documented with the static
# geoms alongside the repelled ones, so the no-op belongs to `use_labels`
# rather than to the geoms whose stat places labels around the nodes.
static_label_geoms <- list(
  geom_dag_label = geom_dag_label,
  geom_dag_text = geom_dag_text,
  geom_label = ggplot2::geom_label,
  geom_text = ggplot2::geom_text
)

# Every piece of text the plot draws, wherever it is drawn from.
all_drawn_text <- function(plot) {
  built <- ggplot2::ggplot_build(plot)$data
  drawn <- unlist(lapply(built, function(layer_data) {
    if ("label" %in% names(layer_data)) as.character(layer_data$label)
  }))
  unique(drawn[!is.na(drawn) & nzchar(drawn)])
}

test_that("use_labels on an unlabelled DAG is a no-op for a static geom", {
  plain <- ggdag(unlabelled_dag())
  plain_status <- ggdag_status(unlabelled_dag())

  for (label_geom in static_label_geoms) {
    plot <- ggdag(unlabelled_dag(), use_labels = TRUE, label_geom = label_geom)
    expect_no_condition(ggplot2::ggplot_build(plot))
    expect_length(plot$layers, length(plain$layers))
    expect_setequal(all_drawn_text(plot), c("x", "y", "z"))

    status <- ggdag_status(
      unlabelled_dag(),
      use_labels = TRUE,
      label_geom = label_geom
    )
    expect_no_condition(ggplot2::ggplot_build(status))
    expect_length(status$layers, length(plain_status$layers))
    expect_setequal(all_drawn_text(status), c("x", "y", "z"))
  }
})

test_that("a static label geom still draws a labelled DAG's labels", {
  labels <- c("Exposure", "Outcome", "Confounder")

  for (label_geom in static_label_geoms) {
    plot <- ggdag(labelled_dag(), use_labels = TRUE, label_geom = label_geom)
    expect_true(all(labels %in% all_drawn_text(plot)))
  }
})

test_that("a static label geom still draws an explicit label column", {
  named <- unlabelled_dag() |>
    tidy_dagitty() |>
    dplyr::mutate(note = paste0("node ", name))

  for (label_geom in static_label_geoms) {
    plot <- ggdag(
      named,
      use_labels = TRUE,
      label = note,
      label_geom = label_geom
    )
    expect_true(all(
      paste0("node ", c("x", "y", "z")) %in% all_drawn_text(plot)
    ))
  }
})
