# Tests for the automatic label placement layer machinery to be implemented
# in R/label_auto.R on top of the pure engine place_dag_labels():
#
# * StatNodesLabelAuto emits role-tagged rows in one data frame, with a
#   ggdag_role column taking the values "label" (one row per labelled node),
#   "node" (every node, carrying the discovered node_size), and "edge"
#   (ordered polyline points tracing each drawn edge, keyed by edge_id).
#   Positions stay in data units; conversion to mm happens at draw time.
# * GeomDagLabelAuto partitions by role at draw time, measures label grobs,
#   calls place_dag_labels(), and draws borderless rounded-rect labels, text,
#   and leader segments inside a gTree with a makeContent method.
# * geom_dag_label_auto() / geom_dag_text_auto() are dag_layer()-wrapped
#   constructors that discover node size and edge geometry from the plot and
#   participate in the debug_repel_points overlay.
# * dag_node_aware() tags a label geom constructor with a "dag_node_aware"
#   attribute so geom_dag()'s label branch threads node-aware parameters to
#   any tagged function, replacing the identical() chain over the repel
#   constructors.
#
# Everything here is snapshot-free except the expect_doppelganger() visual
# baselines, which write NEW files under _snaps/label-auto-geom/ the first
# time they pass.

# A mediation triangle on fixed coordinates with a label on every node.
labelled_triangle <- function() {
  dagify(
    y ~ m + x,
    m ~ x,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", m = "Mediator", y = "Outcome"),
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )
}

# The same shape with labels on only two of the three nodes.
partially_labelled_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    labels = c(x = "Exposure", y = "Outcome"),
    coords = list(x = c(x = 0, z = 1, y = 2), y = c(x = 0, z = 1, y = 0))
  )
}

# The layer of `plot` computed by the automatic label stat.
auto_layer_index <- function(plot) {
  which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$stat, "StatNodesLabelAuto")
  }))
}

# The built data of the automatic label layer of `plot`.
auto_stat_data <- function(plot) {
  index <- auto_layer_index(plot)
  expect_length(index, 1)
  ggplot2::layer_data(plot, index)
}

# Every text grob in `grob`, flattened to one data frame row per grob with
# the drawn label and the raw unit values of its position. Unit values are
# only compared between renders at the same device size, so they need no
# conversion.
collect_text_grobs <- function(grob) {
  if (inherits(grob, "text")) {
    return(list(
      data.frame(
        label = as.character(grob$label),
        x = as.numeric(grob$x),
        y = as.numeric(grob$y),
        stringsAsFactors = FALSE
      )
    ))
  }
  children <- grob$children
  if (is.null(children)) {
    return(list())
  }
  unlist(lapply(children, collect_text_grobs), recursive = FALSE)
}

# The text grobs `plot` draws on a fixed-size headless device, sorted so grob
# order cannot decide a comparison.
rendered_text_grobs <- function(plot) {
  file <- tempfile(fileext = ".png")
  grDevices::png(file, width = 700, height = 600)
  on.exit(
    {
      grDevices::dev.off()
      unlink(file)
    },
    add = TRUE
  )

  print(plot)
  grid::grid.force()
  scene <- grid::grid.grab()

  rows <- do.call(rbind, collect_text_grobs(scene))
  rows <- rows[order(rows$label, rows$x, rows$y), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

no_edges <- data.frame(edge_id = character(), x = numeric(), y = numeric())

# Stat contract ---------------------------------------------------------------

test_that("the auto stat emits role-tagged label, node, and edge rows", {
  p <- ggplot(partially_labelled_dag(), aes_dag()) +
    geom_dag_point(size = 24) +
    geom_dag_edges_link() +
    geom_dag_label_auto(aes(label = label))

  stat_data <- auto_stat_data(p)

  expect_contains(names(stat_data), "ggdag_role")
  expect_setequal(unique(stat_data$ggdag_role), c("label", "node", "edge"))

  # Exactly one label row per labelled node: x and y carry labels, z does not.
  label_rows <- stat_data[stat_data$ggdag_role == "label", , drop = FALSE]
  expect_equal(nrow(label_rows), 2)
  expect_setequal(label_rows$label, c("Exposure", "Outcome"))
  expect_equal(anyDuplicated(label_rows[, c("x", "y")]), 0)

  # One node row per node, each carrying the node size discovered from the
  # geom_dag_point() layer.
  node_rows <- stat_data[stat_data$ggdag_role == "node", , drop = FALSE]
  expect_equal(nrow(node_rows), 3)
  coords <- pull_dag_data(tidy_dagitty(partially_labelled_dag())) |>
    dplyr::distinct(name, x, y)
  expect_setequal(
    paste(node_rows$x, node_rows$y),
    paste(coords$x, coords$y)
  )
  expect_contains(names(node_rows), "node_size")
  expect_equal(unique(node_rows$node_size), 24)

  # Edge rows trace each drawn edge: one consistent edge_id per edge, at
  # least two points each.
  edge_rows <- stat_data[stat_data$ggdag_role == "edge", , drop = FALSE]
  expect_contains(names(edge_rows), "edge_id")
  points_per_edge <- table(edge_rows$edge_id)
  expect_length(points_per_edge, 3)
  expect_true(all(points_per_edge >= 2))
})

test_that("auto stat edge rows follow a drawn curved edge", {
  dag <- dagify(
    b ~ a,
    labels = c(a = "Start", b = "End"),
    coords = list(x = c(a = 0, b = 2), y = c(a = 0, b = 0))
  ) |>
    tidy_dagitty() |>
    curve_edge("a", "b", 0.3)

  p <- ggdag(
    dag,
    edge_engine = "ggarrow",
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )

  stat_data <- auto_stat_data(p)
  edge_rows <- stat_data[stat_data$ggdag_role == "edge", , drop = FALSE]

  expect_gte(nrow(edge_rows), 2)
  expect_length(unique(edge_rows$edge_id), 1)

  # The traced points sit on the drawn curve, not on the straight chord:
  # every point lies within 2.5% of the edge length of the modeled curve, and
  # the trace bows below the chord as positive curvature demands.
  curve <- sample_curved_edge(0, 0, 2, 0, curvature = 0.3, n = 400)
  nearest <- vapply(
    seq_len(nrow(edge_rows)),
    function(i) {
      min(sqrt(
        (curve$x - edge_rows$x[i])^2 + (curve$y - edge_rows$y[i])^2
      ))
    },
    numeric(1)
  )
  expect_lt(max(nearest), 0.05)
  expect_lt(min(edge_rows$y), -0.3)
})

test_that("auto stat edge rows carry the spec a routed edge is routed with", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    u ~ ~v,
    labels = c(
      x = "Exposure",
      m = "Mediator",
      y = "Outcome",
      u = "Latent",
      v = "Latent cause"
    ),
    coords = list(
      x = c(x = 0, m = 1, y = 2, u = 0, v = 2),
      y = c(x = 0, m = 0, y = 0, u = 2, v = 2)
    )
  )
  p <- ggdag(
    dag,
    edge_engine = "ggarrow",
    edge_route = "spline",
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )

  stat_data <- auto_stat_data(p)
  edge_rows <- stat_data[stat_data$ggdag_role == "edge", , drop = FALSE]
  expect_contains(
    names(edge_rows),
    c("route_style", "route_clearance", "route_sep", "route_layer_axis")
  )

  style <- if ("route_style" %in% names(edge_rows)) {
    edge_rows$route_style
  } else {
    rep(NA_character_, nrow(edge_rows))
  }

  # A routed edge is drawn along a path decided in millimetres at draw time,
  # so it reaches the stat as its two chord endpoints carrying how it is
  # routed. The geom routes it again from that spec, with the same router the
  # arrows are drawn with.
  routed <- edge_rows[!is.na(style), , drop = FALSE]
  expect_equal(nrow(routed), 6)
  expect_length(unique(routed$edge_id), 3)
  expect_true(all(routed$route_style == "spline"))
  expect_true(all(is.na(routed$route_clearance)))
  expect_true(all(is.na(routed$route_sep)))
  expect_equal(routed$route_layer_axis, rep("auto", nrow(routed)))

  # the bidirected edge is drawn as an arc in data space, so it is traced
  # rather than routed and carries no spec
  arc <- edge_rows[is.na(style), , drop = FALSE]
  expect_length(unique(arc$edge_id), 1)
  expect_gt(nrow(arc), 2)

  # the node rows still carry the size of the discs the router routes around
  node_rows <- stat_data[stat_data$ggdag_role == "node", , drop = FALSE]
  expect_contains(names(node_rows), "node_size")
  expect_equal(unique(node_rows$node_size), 16)
})

test_that("the auto constructors return debug-enabled discovering dag layers", {
  constructors <- list(geom_dag_label_auto, geom_dag_text_auto)
  for (constructor in constructors) {
    wrapped <- constructor(aes(label = name))
    expect_s3_class(wrapped, "dag_layer")
    expect_setequal(wrapped$discover, c("node_size", "edge_geometry"))
    expect_true(wrapped$debug)
    expect_true(inherits(wrapped$stat, "StatNodesLabelAuto"))
  }
})

# Refactor guard --------------------------------------------------------------

test_that("StatNodesRepel output is unchanged through the edge_id refactor", {
  # This pins the exact built data of a repel layer as produced before
  # straight_edge_points() and drawn_edge_points() gained an edge_id column.
  # StatNodesRepel must drop that column before binding, so this test stays
  # green through the refactor.
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    coords = list(x = c(x = 0, z = 1, y = 2), y = c(x = 0, z = 1, y = 0))
  )
  p <- ggplot(dag, aes_dag()) +
    geom_dag_point() +
    geom_dag_label_repel(
      aes(label = name),
      node_size = 16,
      n_edge_points = 3,
      n_node_points = 4,
      seed = 12
    )

  repel_data <- ggplot2::layer_data(p, 2)

  expect_identical(
    colnames(repel_data),
    c(
      "label",
      "x",
      "y",
      "PANEL",
      "group",
      "point.size",
      "colour",
      "fill",
      "size",
      "angle",
      "alpha",
      "family",
      "fontface",
      "lineheight",
      "hjust",
      "vjust",
      "linewidth",
      "linetype",
      "segment.linetype",
      "segment.size",
      "segment.curvature",
      "segment.angle",
      "segment.ncp",
      "segment.shape",
      "segment.square",
      "segment.squareShape",
      "segment.inflect",
      "segment.debug",
      "segment.colour",
      "segment.alpha"
    )
  )
  expect_equal(nrow(repel_data), 87)

  expected_x <- c(
    0,
    2,
    1,
    0.5,
    1,
    1.5,
    0.75,
    0.5,
    0.25,
    1.25,
    1.5,
    1.75,
    0,
    0.0075000000000000015,
    -0.0074999999999999971,
    -0.014999999999999999,
    -0.0075000000000000067,
    0.0074999999999999893,
    0.014999999999999999,
    1.8369701987210296e-18,
    -0.025980762113533153,
    -0.025980762113533163,
    -5.5109105961630888e-18,
    0.025980762113533149,
    0.025980762113533156,
    0.022500000000000003,
    -0.022499999999999992,
    -0.044999999999999998,
    -0.02250000000000002,
    0.022499999999999968,
    0.044999999999999998,
    3.6739403974420592e-18,
    -0.051961524227066305,
    -0.051961524227066326,
    -1.1021821192326178e-17,
    0.051961524227066298,
    0.051961524227066312,
    2,
    2.0074999999999998,
    1.9924999999999999,
    1.9850000000000001,
    1.9924999999999999,
    2.0074999999999998,
    2.0150000000000001,
    2,
    1.9740192378864669,
    1.9740192378864669,
    2,
    2.0259807621135333,
    2.0259807621135333,
    2.0225,
    1.9775,
    1.9550000000000001,
    1.9775,
    2.0225,
    2.0449999999999999,
    2,
    1.9480384757729337,
    1.9480384757729337,
    2,
    2.0519615242270661,
    2.0519615242270661,
    1,
    1.0075000000000001,
    0.99250000000000005,
    0.98499999999999999,
    0.99249999999999994,
    1.0075000000000001,
    1.0149999999999999,
    1,
    0.97401923788646683,
    0.97401923788646683,
    1,
    1.0259807621135331,
    1.0259807621135331,
    1.0225,
    0.97750000000000004,
    0.95499999999999996,
    0.97750000000000004,
    1.0225,
    1.0449999999999999,
    1,
    0.94803847577293365,
    0.94803847577293365,
    1,
    1.0519615242270663,
    1.0519615242270663
  )
  expected_y <- c(
    0,
    0,
    1,
    0,
    0,
    0,
    0.75,
    0.5,
    0.25,
    0.75,
    0.5,
    0.25,
    0,
    0.012990381056766578,
    0.01299038105676658,
    1.8369701987210296e-18,
    -0.012990381056766575,
    -0.012990381056766585,
    -3.6739403974420592e-18,
    0.029999999999999999,
    0.01500000000000001,
    -0.014999999999999993,
    -0.029999999999999999,
    -0.015000000000000013,
    0.014999999999999999,
    0.038971143170299732,
    0.038971143170299739,
    5.5109105961630896e-18,
    -0.038971143170299725,
    -0.038971143170299753,
    -1.1021821192326179e-17,
    0.059999999999999998,
    0.03000000000000002,
    -0.029999999999999985,
    -0.059999999999999998,
    -0.030000000000000027,
    0.029999999999999999,
    0,
    0.012990381056766578,
    0.01299038105676658,
    1.8369701987210296e-18,
    -0.012990381056766575,
    -0.012990381056766585,
    -3.6739403974420592e-18,
    0.029999999999999999,
    0.01500000000000001,
    -0.014999999999999993,
    -0.029999999999999999,
    -0.015000000000000013,
    0.014999999999999999,
    0.038971143170299732,
    0.038971143170299739,
    5.5109105961630896e-18,
    -0.038971143170299725,
    -0.038971143170299753,
    -1.1021821192326179e-17,
    0.059999999999999998,
    0.03000000000000002,
    -0.029999999999999985,
    -0.059999999999999998,
    -0.030000000000000027,
    0.029999999999999999,
    1,
    1.0129903810567666,
    1.0129903810567666,
    1,
    0.98700961894323347,
    0.98700961894323347,
    1,
    1.03,
    1.0149999999999999,
    0.98499999999999999,
    0.96999999999999997,
    0.98499999999999999,
    1.0149999999999999,
    1.0389711431702997,
    1.0389711431702997,
    1,
    0.9610288568297003,
    0.9610288568297003,
    1,
    1.0600000000000001,
    1.03,
    0.96999999999999997,
    0.93999999999999995,
    0.96999999999999997,
    1.03
  )

  expect_equal(repel_data$x, expected_x, tolerance = 1e-12)
  expect_equal(repel_data$y, expected_y, tolerance = 1e-12)
  expect_identical(repel_data$label, c("x", "y", "z", rep("", 84)))
  expect_equal(
    repel_data$point.size,
    c(rep(15.940224159402241, 3), rep(0, 84)),
    tolerance = 1e-12
  )
  expect_identical(repel_data$group, c(rep(-1L, 3), rep(NA_integer_, 84)))
  expect_identical(levels(repel_data$PANEL), "1")
  expect_identical(as.integer(repel_data$PANEL), rep(1L, 87))

  constant_columns <- list(
    colour = "black",
    fill = "white",
    size = 3.88,
    angle = 0,
    alpha = NA,
    family = "",
    fontface = 1,
    lineheight = 1.2,
    hjust = 0.5,
    vjust = 0.5,
    linewidth = 0.25,
    linetype = 1,
    segment.linetype = 1,
    segment.size = 0.5,
    segment.curvature = 0,
    segment.angle = 90,
    segment.ncp = 1,
    segment.shape = 0.5,
    segment.square = TRUE,
    segment.squareShape = 1,
    segment.inflect = FALSE,
    segment.debug = FALSE,
    segment.colour = "grey50",
    segment.alpha = 1
  )
  for (column in names(constant_columns)) {
    expect_equal(
      unique(repel_data[[column]]),
      constant_columns[[column]],
      info = column
    )
  }
})

test_that("StatNodesRepel with an arc edge layer is unchanged through the edge_id refactor", {
  # The sibling of the guard above for the drawn-edge path: the values were
  # produced before drawn_edge_points() gained an edge_id column, so the arc
  # tracing must come out unchanged.
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    coords = list(x = c(x = 0, z = 1, y = 2), y = c(x = 0, z = 1, y = 0))
  )
  p <- ggplot(dag, aes_dag()) +
    geom_dag_point() +
    geom_dag_edges_arc() +
    geom_dag_label_repel(
      aes(label = name),
      node_size = 16,
      n_edge_points = 3,
      n_node_points = 4,
      seed = 12
    )

  repel_data <- ggplot2::layer_data(p, 3)

  expect_equal(nrow(repel_data), 87)
  expect_identical(repel_data$label, c("x", "y", "z", rep("", 84)))

  expected_x <- c(
    0,
    2,
    1,
    0.516384835131935,
    0.990205323144434,
    1.48361516486806,
    0.942013818307175,
    0.770035326475999,
    0.45839865343911,
    1.45839865343911,
    1.76024064962043,
    1.94201381830717,
    0,
    0.0075,
    -0.0075,
    -0.015,
    -0.00750000000000001,
    0.00749999999999999,
    0.015,
    1.83697019872103e-18,
    -0.0259807621135332,
    -0.0259807621135332,
    -5.51091059616309e-18,
    0.0259807621135331,
    0.0259807621135332,
    0.0225,
    -0.0225,
    -0.045,
    -0.0225,
    0.0225,
    0.045,
    3.67394039744206e-18,
    -0.0519615242270663,
    -0.0519615242270663,
    -1.10218211923262e-17,
    0.0519615242270663,
    0.0519615242270663,
    2,
    2.0075,
    1.9925,
    1.985,
    1.9925,
    2.0075,
    2.015,
    2,
    1.97401923788647,
    1.97401923788647,
    2,
    2.02598076211353,
    2.02598076211353,
    2.0225,
    1.9775,
    1.955,
    1.9775,
    2.0225,
    2.045,
    2,
    1.94803847577293,
    1.94803847577293,
    2,
    2.05196152422707,
    2.05196152422707,
    1,
    1.0075,
    0.9925,
    0.985,
    0.9925,
    1.0075,
    1.015,
    1,
    0.974019237886467,
    0.974019237886467,
    1,
    1.02598076211353,
    1.02598076211353,
    1.0225,
    0.9775,
    0.955,
    0.9775,
    1.0225,
    1.045,
    1,
    0.948038475772934,
    0.948038475772934,
    1,
    1.05196152422707,
    1.05196152422707
  )
  expected_y <- c(
    0,
    0,
    1,
    0.400412471746285,
    0.530275976096431,
    0.400412471746285,
    0.54160134656089,
    0.239759350379567,
    0.0579861816928252,
    0.942013818307175,
    0.770035326475999,
    0.45839865343911,
    0,
    0.0129903810567666,
    0.0129903810567666,
    1.83697019872103e-18,
    -0.0129903810567666,
    -0.0129903810567666,
    -3.67394039744206e-18,
    0.03,
    0.015,
    -0.015,
    -0.03,
    -0.015,
    0.015,
    0.0389711431702997,
    0.0389711431702997,
    5.51091059616309e-18,
    -0.0389711431702997,
    -0.0389711431702998,
    -1.10218211923262e-17,
    0.06,
    0.03,
    -0.03,
    -0.06,
    -0.03,
    0.03,
    0,
    0.0129903810567666,
    0.0129903810567666,
    1.83697019872103e-18,
    -0.0129903810567666,
    -0.0129903810567666,
    -3.67394039744206e-18,
    0.03,
    0.015,
    -0.015,
    -0.03,
    -0.015,
    0.015,
    0.0389711431702997,
    0.0389711431702997,
    5.51091059616309e-18,
    -0.0389711431702997,
    -0.0389711431702998,
    -1.10218211923262e-17,
    0.06,
    0.03,
    -0.03,
    -0.06,
    -0.03,
    0.03,
    1,
    1.01299038105677,
    1.01299038105677,
    1,
    0.987009618943233,
    0.987009618943233,
    1,
    1.03,
    1.015,
    0.985,
    0.97,
    0.985,
    1.015,
    1.0389711431703,
    1.0389711431703,
    1,
    0.9610288568297,
    0.9610288568297,
    1,
    1.06,
    1.03,
    0.97,
    0.94,
    0.97,
    1.03
  )

  expect_equal(repel_data$x, expected_x, tolerance = 1e-12)
  expect_equal(repel_data$y, expected_y, tolerance = 1e-12)
  expect_equal(
    repel_data$point.size,
    c(rep(15.940224159402241, 3), rep(0, 84)),
    tolerance = 1e-12
  )
})

# Node-aware registration -----------------------------------------------------

test_that("a custom function tagged with dag_node_aware() gets node-aware params", {
  recorded <- new.env(parent = emptyenv())
  recorder <- function(...) {
    recorded$args <- rlang::list2(...)
    NULL
  }
  aware <- dag_node_aware(recorder)

  geom_dag(
    use_labels = TRUE,
    label_geom = aware,
    node_size = 20,
    n_edge_points = 7,
    n_node_points = 5
  )

  expect_contains(
    names(recorded$args),
    c("mapping", "data", "size", "col", "show.legend")
  )
  expect_equal(recorded$args$node_size, 20)
  expect_equal(recorded$args$n_edge_points, 7)
  expect_equal(recorded$args$n_node_points, 5)
  expect_equal(recorded$args$box.padding, 0.5)
  expect_equal(recorded$args$max.overlaps, Inf)
})

test_that("an untagged custom function still gets only the common params", {
  recorded <- new.env(parent = emptyenv())
  recorder <- function(...) {
    recorded$args <- rlang::list2(...)
    NULL
  }

  geom_dag(use_labels = TRUE, label_geom = recorder, node_size = 20)

  expect_named(
    recorded$args,
    c("mapping", "data", "size", "col", "show.legend"),
    ignore.order = TRUE
  )
})

test_that("every packaged label geom that repels or places is node-aware", {
  geoms <- list(
    geom_dag_label_repel = geom_dag_label_repel,
    geom_dag_label_repel2 = geom_dag_label_repel2,
    geom_dag_text_repel = geom_dag_text_repel,
    geom_dag_text_repel2 = geom_dag_text_repel2,
    geom_dag_label_auto = geom_dag_label_auto,
    geom_dag_text_auto = geom_dag_text_auto
  )
  for (name in names(geoms)) {
    expect_true(
      isTRUE(attr(geoms[[name]], "dag_node_aware")),
      info = name
    )
  }
})

test_that("geom_dag() still threads node_size to the repel label geoms", {
  repel_geoms <- list(
    geom_dag_label_repel = geom_dag_label_repel,
    geom_dag_label_repel2 = geom_dag_label_repel2,
    geom_dag_text_repel = geom_dag_text_repel,
    geom_dag_text_repel2 = geom_dag_text_repel2
  )
  for (name in names(repel_geoms)) {
    layers <- geom_dag(
      use_labels = TRUE,
      label_geom = repel_geoms[[name]],
      node_size = 20
    )
    label_item <- layers[[4]]
    expect_s3_class(label_item, "dag_layer")
    expect_equal(label_item$stat_params$node_size, 20, info = name)
  }
})

test_that("geom_dag() threads edge_cap to the auto label geom", {
  layers <- geom_dag(
    use_labels = TRUE,
    label_geom = geom_dag_label_auto,
    edge_cap = 12
  )
  label_item <- layers[[4]]
  expect_s3_class(label_item, "dag_layer")
  params <- c(label_item$stat_params, label_item$geom_params)
  expect_equal(params[["edge_cap"]], 12)
})

# Validation hardening --------------------------------------------------------

test_that("place_dag_labels() rejects a non-finite or negative gap", {
  labels <- data.frame(id = "a", x = 0, y = 0, width = 10, height = 5)
  nodes <- data.frame(x = 0, y = 0, radius = 3)
  bounds <- c(-30, -30, 30, 30)

  expect_error(
    place_dag_labels(labels, nodes, no_edges, bounds, gap = Inf),
    class = "ggdag_type_error"
  )
  expect_error(
    place_dag_labels(labels, nodes, no_edges, bounds, gap = NaN),
    class = "ggdag_type_error"
  )
  expect_error(
    place_dag_labels(labels, nodes, no_edges, bounds, gap = NA_real_),
    class = "ggdag_type_error"
  )
  expect_error(
    place_dag_labels(labels, nodes, no_edges, bounds, gap = -1),
    class = "ggdag_type_error"
  )

  # Zero clearance is a valid, if tight, request.
  expect_silent(place_dag_labels(labels, nodes, no_edges, bounds, gap = 0))
})

test_that("place_dag_labels() rejects weights = NULL", {
  labels <- data.frame(id = "a", x = 0, y = 0, width = 10, height = 5)
  nodes <- data.frame(x = 0, y = 0, radius = 3)
  bounds <- c(-30, -30, 30, 30)

  expect_error(
    place_dag_labels(labels, nodes, no_edges, bounds, weights = NULL),
    class = "ggdag_type_error"
  )
})

test_that("place_dag_labels() rejects non-finite coordinates", {
  labels <- data.frame(id = "a", x = 0, y = 0, width = 10, height = 5)
  nodes <- data.frame(x = 0, y = 0, radius = 3)
  bounds <- c(-30, -30, 30, 30)

  bad_labels <- labels
  bad_labels$x <- NA_real_
  expect_error(
    place_dag_labels(bad_labels, nodes, no_edges, bounds),
    class = "ggdag_type_error"
  )

  bad_labels <- labels
  bad_labels$y <- Inf
  expect_error(
    place_dag_labels(bad_labels, nodes, no_edges, bounds),
    class = "ggdag_type_error"
  )

  bad_nodes <- nodes
  bad_nodes$x <- Inf
  expect_error(
    place_dag_labels(labels, bad_nodes, no_edges, bounds),
    class = "ggdag_type_error"
  )

  bad_nodes <- nodes
  bad_nodes$y <- NaN
  expect_error(
    place_dag_labels(labels, bad_nodes, no_edges, bounds),
    class = "ggdag_type_error"
  )

  bad_edges <- data.frame(
    edge_id = c("e1", "e1", "e1"),
    x = c(0, 5, 10),
    y = c(0, NaN, 0)
  )
  expect_error(
    place_dag_labels(labels, nodes, bad_edges, bounds),
    class = "ggdag_type_error"
  )
})

# Determinism -----------------------------------------------------------------

test_that("rendering the same labelled DAG twice places labels identically", {
  p <- ggplot(labelled_triangle(), aes_dag()) +
    geom_dag_point() +
    geom_dag_edges_link() +
    geom_dag_label_auto(aes(label = label))

  withr::local_seed(4321)
  seed_before <- get(".Random.seed", envir = globalenv())

  first <- rendered_text_grobs(p)
  second <- rendered_text_grobs(p)

  expect_gt(nrow(first), 0)
  expect_equal(first, second)

  # Building and rendering the automatic labels never touches the RNG.
  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)
})

# Visual baselines ------------------------------------------------------------

test_that("label-auto visuals: mediation triangle", {
  p <- ggdag(
    labelled_triangle(),
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ) +
    theme_dag()
  expect_doppelganger("label-auto-mediation-triangle", p)
})

test_that("label-auto visuals: ten-node time-ordered DAG", {
  dag <- dagify(
    b ~ a,
    c ~ a,
    d ~ b,
    e ~ b + c,
    f ~ c,
    g ~ d + e,
    h ~ e + f,
    x ~ g,
    y ~ g + h + x,
    exposure = "x",
    outcome = "y",
    labels = c(
      a = "Genetics",
      b = "Diet",
      c = "Exercise",
      d = "Weight",
      e = "Blood pressure",
      f = "Cholesterol",
      g = "Medication",
      h = "Stress",
      x = "Treatment",
      y = "Outcome"
    )
  )
  p <- ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto) +
    theme_dag()
  expect_doppelganger("label-auto-time-ordered-ten-nodes", p)
})

test_that("label-auto visuals: labels avoid a drawn curved edge", {
  dag <- dagify(
    y ~ x + z,
    z ~ x,
    labels = c(x = "Exposure", y = "Outcome", z = "Mediator"),
    coords = list(x = c(x = 0, z = 1, y = 2), y = c(x = 0, z = 1, y = 0))
  ) |>
    tidy_dagitty() |>
    curve_edge("x", "y", 0.3)

  p <- ggdag(
    dag,
    edge_engine = "ggarrow",
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ) +
    theme_dag()
  expect_doppelganger("label-auto-curved-edge", p)
})

test_that("label-auto visuals: text variant", {
  p <- ggdag(
    labelled_triangle(),
    use_labels = TRUE,
    label_geom = geom_dag_text_auto
  ) +
    theme_dag()
  expect_doppelganger("label-auto-text-variant", p)
})

test_that("label-auto visuals: dense DAG degrades without overlap chaos", {
  dag <- dagify(
    y ~ a + b + c + x,
    x ~ a + b,
    a ~ c,
    b ~ c,
    exposure = "x",
    outcome = "y",
    labels = c(
      a = "Alcohol consumption",
      b = "Body mass index",
      c = "Socioeconomic status",
      x = "Physical activity",
      y = "Cardiovascular disease"
    ),
    coords = list(
      x = c(c = 0, a = 1, b = 1, x = 2, y = 3),
      y = c(c = 0, a = 0.5, b = -0.5, x = 0, y = 0)
    )
  )
  p <- ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto) +
    theme_dag()
  expect_doppelganger("label-auto-dense-dag", p)
})

test_that("label-auto visuals: debug overlay draws the discovered obstacles", {
  withr::local_options(list(ggdag.debug_repel_points = TRUE))
  p <- ggdag(
    labelled_triangle(),
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ) +
    theme_dag()
  expect_doppelganger("label-auto-debug-overlay", p)
})

test_that("label-auto visuals: faceted paths place labels per panel", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder"),
    coords = list(x = c(x = 0, z = 1, y = 2), y = c(x = 0, z = 1, y = 0))
  )
  p <- ggdag_paths(dag, use_labels = TRUE, label_geom = geom_dag_label_auto)
  expect_doppelganger("label-auto-faceted-paths", p)
})

# Edge cases ------------------------------------------------------------------

test_that("a single labelled node renders", {
  dag <- tidy_dagitty(dagitty::dagitty("dag { x }")) |>
    dag_label(labels = c(x = "Lone node"))

  p <- ggplot(dag, aes_dag()) +
    geom_dag_point() +
    geom_dag_label_auto(aes(label = label))

  grobs <- rendered_text_grobs(p)
  expect_contains(grobs$label, "Lone node")
})

test_that("use_labels with no label column is a silent no-op for the auto geom", {
  dag <- dagify(y ~ x)
  p <- ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto)
  expect_no_condition(ggplot2::ggplot_build(p))
})
