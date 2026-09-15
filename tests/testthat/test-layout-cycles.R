# Layer assignment on graphs that hold a directed cycle.
#
# A cycle has no topological order, so the layering picks a feedback set of
# edges, reverses it for the layer pass alone, and draws every edge in its
# true direction afterwards. The blocks here pin the layers that fall out of
# that, the determinism of the choice, the invariant that an acyclic graph
# reverses nothing, and the picture the two-node loop draws: one node per
# layer, read left to right.

# Layers are returned in the order the nodes were met, which says nothing
# about the assignment itself, so every comparison sorts by name.
by_name <- function(layers) layers[order(names(layers))]

# Layer assignment -------------------------------------------------------------

test_that("a two-node cycle gets one layer per node", {
  layers <- longest_path_layers(canonical_dag_edges(c("a->b", "b->a")))

  expect_identical(by_name(layers), c(a = 0L, b = 1L))
})

test_that("a three-node cycle gets one layer per node", {
  layers <- longest_path_layers(canonical_dag_edges(c("a->b", "b->c", "c->a")))

  expect_identical(by_name(layers), c(a = 0L, b = 1L, c = 2L))
})

test_that("a tail into and out of a cycle keeps its order", {
  layers <- longest_path_layers(canonical_dag_edges(c(
    "t->a",
    "a->b",
    "b->c",
    "c->a",
    "c->u"
  )))

  expect_identical(by_name(layers), c(a = 1L, b = 2L, c = 3L, t = 0L, u = 4L))
})

test_that("two disjoint cycles are each broken", {
  layers <- longest_path_layers(canonical_dag_edges(c(
    "a->b",
    "b->a",
    "c->d",
    "d->c"
  )))

  expect_identical(by_name(layers), c(a = 0L, b = 1L, c = 0L, d = 1L))
})

test_that("an acyclic component beside a cycle is unaffected", {
  layers <- longest_path_layers(canonical_dag_edges(c(
    "a->b",
    "b->a",
    "x->y",
    "y->z"
  )))

  expect_identical(
    by_name(layers),
    c(a = 0L, b = 1L, x = 0L, y = 1L, z = 2L)
  )
})

test_that("a self-loop leaves the rest of the graph orderable", {
  layers <- longest_path_layers(canonical_dag_edges(c("a->a", "a->b")))

  expect_identical(by_name(layers), c(a = 0L, b = 1L))
})

test_that("a lone self-loop stays on one layer", {
  layers <- longest_path_layers(canonical_dag_edges("a->a"))

  expect_identical(layers, c(a = 0L))
})

test_that("every edge of a broken cycle but the feedback set points forward", {
  edges <- canonical_dag_edges(c("t->a", "a->b", "b->c", "c->a", "c->u"))
  layers <- longest_path_layers(edges)

  directed <- edges[!is.na(edges$to), , drop = FALSE]
  backward <- paste(directed$name, "->", directed$to)[
    layers[directed$name] >= layers[directed$to]
  ]

  expect_identical(backward, "c -> a")
})

# Determinism ------------------------------------------------------------------

test_that("the feedback set does not depend on edge row order", {
  specs <- list(
    two = c("a->b", "b->a"),
    three = c("a->b", "b->c", "c->a"),
    tailed = c("t->a", "a->b", "b->c", "c->a", "c->u"),
    disjoint = c("a->b", "b->a", "c->d", "d->c")
  )

  for (spec in specs) {
    edges <- canonical_dag_edges(spec)
    expected <- by_name(longest_path_layers(edges))

    for (i in seq_len(nrow(edges))) {
      rotated <- edges[c(seq_len(nrow(edges))[-seq_len(i)], seq_len(i)), ]
      expect_identical(by_name(longest_path_layers(rotated)), expected)
    }
  }
})

test_that("a cyclic layout is identical run to run and row order to row order", {
  edges <- canonical_dag_edges(c("ac_use->global_temp", "global_temp->ac_use"))

  first <- compute_time_ordered_layout(edges)
  second <- compute_time_ordered_layout(edges)
  reversed <- compute_time_ordered_layout(edges[rev(seq_len(nrow(edges))), ])

  expect_identical(first, second)
  expect_identical(first[order(first$name), ], reversed[order(reversed$name), ])
})

# The acyclic invariant --------------------------------------------------------

test_that("no canonical DAG has a feedback edge", {
  for (nm in names(canonical_dag_specs)) {
    edges <- canonical_dag_edges(canonical_dag_specs[[nm]])
    directed <- edges[!is.na(edges$to), , drop = FALSE]

    expect_identical(
      feedback_edges(directed$name, directed$to),
      logical(nrow(directed)),
      label = paste0(nm, ": feedback set")
    )
  }
})

test_that("every canonical DAG still points forward in time", {
  for (nm in names(canonical_dag_specs)) {
    edges <- canonical_dag_edges(canonical_dag_specs[[nm]])
    layers <- longest_path_layers(edges)
    directed <- edges[!is.na(edges$to), , drop = FALSE]

    expect_true(
      all(layers[directed$name] < layers[directed$to]),
      label = paste0(nm, ": every edge advances a layer")
    )
  }
})

# The picture the two-node loop draws ------------------------------------------

# The cycle is the figure's subject, so the warning the tidied DAG raises
# about it is muffled by class rather than by silencing whatever else the
# scene says.
without_cycle_warning <- function(expr) {
  withCallingHandlers(
    expr,
    ggdag_cyclic_warning = function(cnd) rlang::cnd_muffle(cnd)
  )
}

# The book's feedback-loop figure with its hand-written layout and edge type
# taken out, which is how the gallery draws it: the defaults place the pair
# and route the two edges between them.
feedback_loop_plot <- function() {
  dag <- without_cycle_warning(dagify(
    ac_use ~ global_temp,
    global_temp ~ ac_use,
    labels = c(ac_use = "A/C use", global_temp = "Global\ntemperature")
  ))
  without_cycle_warning(ggdag(
    dag,
    use_text = FALSE,
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ))
}

# The node centres the labelled plot drew, in millimetres of the panel
# viewport, one row per label. The `dag_labels_auto` gTree carries the node
# it placed each box around in npc of the panel, which is the same reading
# test-label-placement-quality.R takes of a drawn scene.
feedback_node_centres_mm <- function(size, route) {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  withr::local_options(list(
    ggdag.edge_engine = "ggarrow",
    ggdag.edge_route = route
  ))
  plot <- feedback_loop_plot()

  file <- withr::local_tempfile(fileext = ".png")
  open_test_ragg(file, size[[1]], size[[2]], res = 150)
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()
  grid::grid.draw(ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot)))
  grid::grid.force()

  paths <- grid::grid.grep("dag_labels_auto", grep = TRUE, global = TRUE)
  paths <- vapply(paths, as.character, character(1))
  paths <- paths[grepl("^layout::panel", paths)]
  viewport <- strsplit(paths[[1]], "::", fixed = TRUE)[[1]][[2]]

  grid::seekViewport(viewport)
  # the viewport is left before the device is closed: once it is closed,
  # grid would open the default device to leave it, which writes Rplots.pdf
  on.exit(grid::upViewport(0), add = TRUE, after = FALSE)
  panel_width <- grid::convertWidth(grid::unit(1, "npc"), "mm", TRUE)
  panel_height <- grid::convertHeight(grid::unit(1, "npc"), "mm", TRUE)

  tree <- grid::grid.get(paths[[1]])
  data.frame(
    label = tree$labels$label,
    x = tree$labels$x * panel_width,
    y = tree$labels$y * panel_height,
    stringsAsFactors = FALSE
  )
}

test_that("the feedback loop is drawn as a horizontal pair", {
  for (route in c("spline", "orthogonal")) {
    for (size in list(c(4.5, 3.5), c(7, 5))) {
      centres <- feedback_node_centres_mm(size, route)
      where <- paste0(route, " at ", size[[1]], "x", size[[2]])

      expect_identical(nrow(centres), 2L, label = where)
      # One node per layer reads left to right: the centres are a node
      # diameter and more apart on the time axis and level with each other
      # across it.
      expect_gt(abs(diff(centres$x)), 20, label = where)
      expect_lt(abs(diff(centres$y)), 0.5, label = where)
    }
  }
})
