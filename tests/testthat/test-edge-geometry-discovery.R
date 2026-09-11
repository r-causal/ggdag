# Tests for the two edge geometry types the label obstacle machinery discovers
# beyond the ggraph stats and the scalar ggarrow curve:
#
# * "curve": a ggarrow curve layer that maps the `edge_curvature` column, as
#   written by `curve_edge()` and the automatic curving of blocked edges. Each
#   row carries its own strength, and `drawn_edge_points()` traces each edge
#   as the quadratic Bezier arc `sample_curved_edge()` models for that row's
#   curvature. Unlike "ggarrow_curve", these edges are traced for every
#   consumer of `repel_edge_points()`, not only when arrows are asked for. The
#   row also carries the curvature grid bends it at, because the ggarrow curve
#   geom draws it on the device: with `trace_arrows` the edge goes to the
#   automatic label stat as its two chord ends and that curvature instead, to
#   be traced in millimetres at draw time.
# * "routed": a layer drawing with `GeomDAGRoutedArrow`, whose path is
#   decided in millimetres when the plot is drawn. The spec is one wide row
#   per drawn edge carrying how the edge is routed, `route_style`,
#   `route_options`, and `route_layer_axis`, rather than where
#   it goes; the ggrepel obstacle tracers follow its chord, and the automatic
#   label engine routes it again at draw time from that spec.

# Distance from each point to a single segment, clamped at the ends.
point_segment_dist <- function(px, py, x, y, xend, yend) {
  dx <- xend - x
  dy <- yend - y
  len2 <- dx^2 + dy^2
  t <- if (len2 == 0) {
    rep(0, length(px))
  } else {
    pmin(pmax(((px - x) * dx + (py - y) * dy) / len2, 0), 1)
  }
  sqrt((px - (x + t * dx))^2 + (py - (y + t * dy))^2)
}

# Distance from each point to the nearest segment of the polyline through
# (poly_x, poly_y) in order.
polyline_dist <- function(px, py, poly_x, poly_y) {
  seg_dists <- vapply(
    seq_len(length(poly_x) - 1),
    function(s) {
      point_segment_dist(
        px,
        py,
        poly_x[s],
        poly_y[s],
        poly_x[s + 1],
        poly_y[s + 1]
      )
    },
    numeric(length(px))
  )
  apply(matrix(seg_dists, nrow = length(px)), 1, min)
}

# Distance from each point to the modelled quadratic Bezier arc, by dense
# sampling; the sampling spacing bounds the error well below the tolerances
# used here.
arc_dist <- function(px, py, x, y, xend, yend, curvature) {
  arc <- sample_curved_edge(x, y, xend, yend, curvature = curvature, n = 1000)
  vapply(
    seq_along(px),
    function(i) {
      min(sqrt((arc$x - px[i])^2 + (arc$y - py[i])^2))
    },
    numeric(1)
  )
}

# A three-node DAG on fixed coordinates: x -> y along the chord from (0, 0)
# to (2, 0), and x -> m up to (1, 1).
base_dag <- function() {
  dagify(
    y ~ x,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  ) |>
    tidy_dagitty()
}

# The same DAG with per-edge curvature: x -> y curved at 0.3, x -> m straight.
curved_edge_dag <- function() {
  base_dag() |>
    curve_edge("x", "y", 0.3)
}

# One row of discovered geometry for a curve-type edge, in the shape
# discover_edge_geometry() emits: the per-edge curvature is the row's
# `strength`, like the ggraph types, so the drawn path is fully described by
# the shared columns.
curve_geometry <- function(x, y, xend, yend, strength) {
  data.frame(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
    circular = FALSE,
    type = "curve",
    strength = strength,
    n = 100,
    fold = FALSE,
    flipped = FALSE,
    from = NA_character_,
    to = NA_character_,
    curvature = NA_real_,
    stringsAsFactors = FALSE
  )
}

# The index of the layer whose stat inherits from `class`.
stat_layer_index <- function(plot, class) {
  which(vapply(
    plot$layers,
    function(layer) inherits(layer$stat, class),
    logical(1)
  ))
}

# Discovery: type "curve" --------------------------------------------------

test_that("a per-edge-curvature ggarrow layer is discovered as type curve", {
  p <- ggplot(curved_edge_dag(), aes_dag()) +
    geom_dag_arrow_arc(aes(edge_curvature = edge_curvature)) +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  expect_false(is.null(geometry))
  geometry <- geometry[order(geometry$xend), , drop = FALSE]

  expect_equal(geometry$type, c("curve", "curve"))
  # per-row strength: the straight x -> m edge carries zero and the curved
  # x -> y edge carries its own curvature
  expect_equal(geometry$strength, c(0, 0.3))
})

test_that("a scalar-curvature ggarrow layer keeps the ggarrow_curve type", {
  # without a mapped edge_curvature column there is no per-edge geometry, so
  # the layer stays on the arrow path, traced only when arrows are asked for
  p <- ggplot(base_dag(), aes_dag()) +
    geom_dag_arrow_arc(curvature = 0.25) +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  expect_equal(unique(geometry$type), "ggarrow_curve")
  expect_equal(geometry$curvature, rep(0.25, nrow(geometry)))
})

# Discovery: type "routed" -------------------------------------------------

test_that("a routed arrows layer is discovered as a routing spec", {
  skip_if_not_installed("ggarrow")

  tidy_dag <- base_dag()
  p <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  expect_false(is.null(geometry))
  routed <- geometry[geometry$type == "routed", , drop = FALSE]

  # one wide row per drawn edge, the shape every other type is discovered
  # with, plus how the edges are routed
  expect_equal(nrow(routed), 2)
  expect_contains(
    names(routed),
    c("x", "y", "xend", "yend", "route_style", "route_options")
  )
  expect_true(all(routed$route_style == "spline"))
  # the layer carries an object whose every field is the router's own
  expect_true(all(vapply(
    routed$route_options,
    function(options) all(vapply(options, is.null, logical(1))),
    logical(1)
  )))

  edges <- pull_dag_data(tidy_dag)
  edges <- edges[!is.na(edges$to), , drop = FALSE]
  expect_setequal(
    paste(routed$x, routed$y, routed$xend, routed$yend),
    paste(edges$x, edges$y, edges$xend, edges$yend)
  )
})

test_that("routed and bent edge layers are discovered together", {
  skip_if_not_installed("ggarrow")

  p <- ggplot(base_dag(), aes_dag()) +
    geom_dag_edges_arc(curvature = 0.4) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  expect_false(is.null(geometry))

  arc <- geometry[geometry$type == "arc", , drop = FALSE]
  expect_equal(nrow(arc), 2)
  expect_equal(unique(arc$strength), 0.4)

  routed <- geometry[geometry$type == "routed", , drop = FALSE]
  expect_equal(nrow(routed), 2)

  # the arc rows fill the routing columns with NA, so the specs stack
  expect_true(all(is.na(arc$route_style)))
})

test_that("the routing spec carries the parameters the edges are routed with", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")

  mediator <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
  spec <- discover_edge_geometry(ggdag(tidy_dagitty(mediator)))
  routed <- spec[spec$type == "routed", , drop = FALSE]

  expect_equal(nrow(routed), 3)
  expect_contains(
    names(routed),
    c("route_style", "route_options", "route_layer_axis")
  )
  expect_true(all(routed$route_style == "spline"))

  # the clearance and the separation are the router's own defaults unless the
  # geom sets them, and the spec says so rather than guessing a number; the
  # layer axis is inferred unless the layout knows which way its layers run
  expect_true(all(vapply(
    routed$route_options,
    function(options) is.null(options$clearance) && is.null(options$edge_sep),
    logical(1)
  )))
  expect_equal(routed$route_layer_axis, rep("auto", nrow(routed)))

  # what the geom is given reaches the spec as it is, so the label engine
  # routes with the numbers the edges were drawn with. The arc layer is a
  # ggraph layer and says that it cannot route, which is what this plot asks
  # the routed layer beside it for.
  expect_warning(
    p <- ggplot(base_dag(), aes_dag()) +
      geom_dag_edges_arc(curvature = 0.4) +
      geom_dag_routed_arrows(clearance = 4, edge_sep = 2, layer_axis = "y") +
      geom_dag_point(),
    class = "ggdag_edge_route_warning"
  )
  geometry <- discover_edge_geometry(p)
  expect_contains(names(geometry), "route_layer_axis")

  given <- geometry[geometry$type == "routed", , drop = FALSE]
  expect_equal(
    vapply(
      given$route_options,
      function(options) options$clearance,
      numeric(1)
    ),
    c(4, 4)
  )
  expect_equal(
    vapply(given$route_options, function(options) options$edge_sep, numeric(1)),
    c(2, 2)
  )
  expect_equal(given$route_layer_axis, c("y", "y"))

  # a layer of any other type carries no routing spec at all
  arc <- geometry[geometry$type == "arc", , drop = FALSE]
  expect_true(all(is.na(arc$route_style)))
  expect_true(all(vapply(arc$route_options, is.null, logical(1))))
  expect_true(all(is.na(arc$route_layer_axis)))
})

# Tracing: type "curve" ----------------------------------------------------

test_that("drawn_edge_points traces a curve row as its quadratic Bezier arc", {
  geometry <- curve_geometry(0, 0, 2, 0, strength = 0.3)
  n <- 10

  traced <- drawn_edge_points(geometry, 1L, n, include_endpoints = TRUE)
  expected <- sample_curved_edge(0, 0, 2, 0, curvature = 0.3, n = n + 2)

  expect_equal(nrow(traced), n + 2)
  expect_equal(traced$x, expected$x)
  expect_equal(traced$y, expected$y)
  expect_length(unique(traced$edge_id), 1)
  expect_equal(unique(traced$PANEL), 1L)
  # positive curvature bows below a left-to-right edge, to the depth the
  # drawn spline reaches: a shade under a quarter of the chord at 0.3
  expect_lt(min(traced$y), -0.25)
})

test_that("drawn_edge_points excludes curve endpoints unless asked", {
  geometry <- curve_geometry(0, 0, 2, 0, strength = 0.3)
  n <- 10

  traced <- drawn_edge_points(geometry, 1L, n, include_endpoints = FALSE)
  expected <- sample_curved_edge(0, 0, 2, 0, curvature = 0.3, n = n + 2)
  interior <- expected[-c(1, n + 2), , drop = FALSE]

  expect_equal(traced$x, interior$x)
  expect_equal(traced$y, interior$y)
})

test_that("drawn_edge_points reads the strength of each curve row", {
  # a mirrored pair between the same endpoints: the strengths differ row by
  # row, and the shared endpoints force distinct edge ids
  geometry <- rbind(
    curve_geometry(0, 0, 2, 0, strength = 0.3),
    curve_geometry(0, 0, 2, 0, strength = -0.3)
  )

  traced <- drawn_edge_points(geometry, 1L, 8, include_endpoints = TRUE)
  groups <- split(traced, traced$edge_id)
  expect_length(groups, 2)

  bows <- sort(vapply(groups, function(g) g$y[which.max(abs(g$y))], numeric(1)))
  expect_lt(bows[[1]], -0.25)
  expect_gt(bows[[2]], 0.25)

  below <- groups[[which(vapply(groups, function(g) min(g$y), numeric(1)) < 0)]]
  above <- groups[[which(vapply(groups, function(g) max(g$y), numeric(1)) > 0)]]
  expect_lt(max(arc_dist(below$x, below$y, 0, 0, 2, 0, 0.3)), 0.01)
  expect_lt(max(arc_dist(above$x, above$y, 0, 0, 2, 0, -0.3)), 0.01)
})

test_that("a curve row with zero strength traces the straight chord", {
  geometry <- curve_geometry(0, 0, 2, 1, strength = 0)

  traced <- drawn_edge_points(geometry, 1L, 6, include_endpoints = TRUE)
  expected <- sample_curved_edge(0, 0, 2, 1, curvature = 0, n = 8)

  expect_equal(traced$x, expected$x)
  expect_equal(traced$y, expected$y)
})

# Obstacle assembly --------------------------------------------------------

test_that("repel_edge_points traces curve specs for every consumer", {
  # no trace_arrows flag: a curve spec is drawn geometry, so it is followed
  # by the repel skeleton as well as the automatic label stat
  edges <- data.frame(
    x = c(0, 0),
    y = c(0, 0),
    xend = c(2, 1),
    yend = c(0, 1),
    PANEL = c(1L, 1L)
  )
  geometry <- curve_geometry(0, 0, 2, 0, strength = 0.3)

  points <- repel_edge_points(
    edges,
    10,
    geometry,
    NULL,
    include_endpoints = TRUE
  )

  # every traced point sits on the arc of the claimed edge or on the chord of
  # the unclaimed one, and the arc actually bows below its chord
  d_arc <- arc_dist(points$x, points$y, 0, 0, 2, 0, 0.3)
  d_chord <- point_segment_dist(points$x, points$y, 0, 0, 1, 1)
  expect_true(all(pmin(d_arc, d_chord) < 0.01))
  expect_lt(min(points$y), -0.25)
  expect_true(any(d_chord < 1e-8))
})

test_that("a curve row drawn by ggarrow reaches the label stat as its chord", {
  # the ggarrow curve geom bends the edge in device units, so with
  # `trace_arrows` the row travels as the two ends of its chord and the
  # curvature it is drawn at, for the label grob to trace in millimetres
  edges <- data.frame(x = 0, y = 0, xend = 2, yend = 0, PANEL = 1L)
  geometry <- curve_geometry(0, 0, 2, 0, strength = 0.3)
  geometry$curvature <- 0.3

  points <- repel_edge_points(
    edges,
    10,
    geometry,
    NULL,
    include_endpoints = TRUE,
    trace_arrows = TRUE
  )

  expect_equal(nrow(points), 2)
  expect_equal(points$x, c(0, 2))
  expect_equal(points$y, c(0, 0))
  expect_equal(points$curvature, c(0.3, 0.3))
})

test_that("a curve row naming no drawn curvature keeps its data-space arc", {
  # the discrimination is the drawing engine, not the mapping: a spec that
  # names no device curvature has nothing to trace in millimetres, so it is
  # followed as the arc its strength models however it is asked for
  edges <- data.frame(x = 0, y = 0, xend = 2, yend = 0, PANEL = 1L)
  geometry <- curve_geometry(0, 0, 2, 0, strength = 0.3)

  points <- repel_edge_points(
    edges,
    10,
    geometry,
    NULL,
    include_endpoints = TRUE,
    trace_arrows = TRUE
  )

  expected <- sample_curved_edge(0, 0, 2, 0, curvature = 0.3, n = 12)
  expect_equal(points$x, expected$x)
  expect_equal(points$y, expected$y)
})

test_that("repel_edge_points traces a routed spec along its chord", {
  # the drawn detour is decided in millimetres at draw time, so a data-space
  # tracer follows the chord, which is what every consumer saw before
  # routing existed
  edges <- data.frame(x = 0, y = 0, xend = 2, yend = 0, PANEL = 1L)
  geometry <- data.frame(
    x = 0,
    y = 0,
    xend = 2,
    yend = 0,
    circular = FALSE,
    type = "routed",
    strength = NA_real_,
    n = 100,
    fold = FALSE,
    flipped = FALSE,
    from = "x",
    to = "y",
    curvature = NA_real_,
    route_style = "spline",
    stringsAsFactors = FALSE
  )
  geometry$route_options <- list(edge_route_options())

  points <- repel_edge_points(
    edges,
    12,
    geometry,
    NULL,
    include_endpoints = TRUE
  )

  expect_gt(nrow(points), 0)
  expect_equal(max(abs(points$y)), 0)
  expect_lt(max(polyline_dist(points$x, points$y, c(0, 2), c(0, 0))), 1e-8)
})

# End to end ----------------------------------------------------------------

test_that("StatNodesRepel receives arc obstacles from per-edge curvature", {
  p <- ggplot(curved_edge_dag(), aes_dag()) +
    geom_dag_arrow_arc(aes(edge_curvature = edge_curvature)) +
    geom_dag_point() +
    geom_dag_label_repel(
      aes(label = name),
      n_node_points = 0,
      n_edge_points = 20,
      seed = 1234
    )

  index <- stat_layer_index(p, "StatNodesRepel")
  expect_length(index, 1)
  built <- ggplot2::layer_data(p, index)

  # every node is labelled and the node skeleton is off, so the empty-label
  # rows are exactly the edge obstacles
  obstacles <- built[built$label == "", , drop = FALSE]
  expect_gt(nrow(obstacles), 0)

  # the curved x -> y edge feeds arc points that bow below its chord rather
  # than straight-chord points
  expect_lt(min(obstacles$y), -0.25)
  below <- obstacles[obstacles$y < 0, , drop = FALSE]
  if (nrow(below) > 0) {
    expect_lt(max(arc_dist(below$x, below$y, 0, 0, 2, 0, 0.3)), 0.05)
  }
})

test_that("StatNodesLabelAuto receives edge obstacles from a routed layer", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x,
    m ~ x,
    labels = c(x = "Exposure", m = "Mediator", y = "Outcome"),
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )
  p <- ggplot(dag, aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point() +
    geom_dag_label_auto(aes(label = label))

  index <- stat_layer_index(p, "StatNodesLabelAuto")
  expect_length(index, 1)
  built <- ggplot2::layer_data(p, index)

  edge_rows <- built[built$ggdag_role == "edge", , drop = FALSE]
  expect_gt(nrow(edge_rows), 0)
  expect_length(unique(edge_rows$edge_id), 2)

  # the routed layer's edges reach the stat as their chords: the detour is
  # decided in the millimetres of the device when the plot is drawn
  on_chords <- pmin(
    polyline_dist(edge_rows$x, edge_rows$y, c(0, 2), c(0, 0)),
    polyline_dist(edge_rows$x, edge_rows$y, c(0, 1), c(0, 1))
  )
  expect_lt(max(on_chords), 1e-8)
})

# Discovery: plot-level mappings --------------------------------------------

test_that("a plot-level edge_curvature mapping is discovered as type curve", {
  dag <- base_dag() |> curve_edge("x", "y", 0.4)
  p <- ggplot(dag, aes_dag(edge_curvature = edge_curvature)) +
    geom_dag_arrow_arc() +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  expect_false(is.null(geometry))
  geometry <- geometry[order(geometry$xend), , drop = FALSE]

  # the layer inherits the plot's aesthetics, so the mapped column gives each
  # edge its own drawn curvature just as a layer-level mapping does
  expect_equal(geometry$type, c("curve", "curve"))
  expect_equal(geometry$strength, c(0, 0.4))

  # the traced points follow the arc the edge is drawn as rather than its chord
  curved <- geometry[geometry$strength != 0, , drop = FALSE]
  traced <- drawn_edge_points(curved, 1L, 10, include_endpoints = TRUE)
  expected <- sample_curved_edge(0, 0, 2, 0, curvature = 0.4, n = 12)
  expect_equal(traced$x, expected$x)
  expect_equal(traced$y, expected$y)
  expect_lt(min(traced$y), -0.35)
})

test_that("a layer that ignores the plot mapping keeps its scalar curvature", {
  dag <- base_dag() |> curve_edge("x", "y", 0.4)
  p <- ggplot(dag, aes_dag(edge_curvature = edge_curvature)) +
    geom_dag_arrow_arc(
      aes(x = x, y = y, xend = xend, yend = yend),
      data = pull_dag_data(dag),
      inherit.aes = FALSE,
      curvature = 0.25
    ) +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  expect_equal(unique(geometry$type), "ggarrow_curve")
  expect_equal(unique(geometry$curvature), 0.25)
})

test_that("a plot-level mapping under another name reaches the curve spec", {
  dag <- base_dag() |>
    curve_edge("x", "y", 0.4) |>
    dplyr::mutate(bend = edge_curvature)
  p <- ggplot(dag, aes_dag(edge_curvature = bend)) +
    geom_dag_arrow_arc() +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  geometry <- geometry[order(geometry$xend), , drop = FALSE]
  expect_equal(geometry$type, c("curve", "curve"))
  expect_equal(geometry$strength, c(0, 0.4))
})

test_that("a plot-level mapping under another name reaches the routed spec", {
  skip_if_not_installed("ggarrow")

  dag <- base_dag() |>
    curve_edge("x", "y", 0.4) |>
    dplyr::mutate(bend = edge_curvature)
  p <- ggplot(dag, aes_dag(edge_curvature = bend)) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  routed <- discover_edge_geometry(p)
  routed <- routed[order(routed$xend), , drop = FALSE]

  # the layer inherits the mapping, so the curved edge is the one the geom
  # leaves alone and the other is the one it routes
  expect_equal(routed$type, c("routed", "routed"))
  expect_equal(routed$curvature, c(NA, 0.4))
})

test_that("a curvature column the plot maps nowhere leaves the spec NA", {
  skip_if_not_installed("ggarrow")

  dag <- base_dag() |> curve_edge("x", "y", 0.4)
  p <- ggplot(dag, aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  routed <- discover_edge_geometry(p)

  # nothing maps the column, so it never reaches the geom and every edge is
  # routed; a spec reading the column would trace an arc nobody draws
  expect_equal(routed$type, c("routed", "routed"))
  expect_true(all(is.na(routed$curvature)))
})

# Repeated coordinates ------------------------------------------------------

test_that("coordinates repeated across panels give one spec row per edge", {
  p <- ggdag_equivalent_dags(
    dagify(
      y ~ x + z,
      x ~ z,
      coords = list(x = c(x = 1, y = 2, z = 0), y = c(x = 1, y = 0, z = 0))
    ),
    edge_type = "arc"
  )

  geometry <- discover_edge_geometry(p)
  arcs <- geometry[geometry$type == "arc", , drop = FALSE]

  plot_data <- p$data
  if (inherits(plot_data, "tidy_dagitty")) {
    plot_data <- pull_dag_data(plot_data)
  }
  edges <- plot_data[!is.na(plot_data$to), , drop = FALSE]
  drawn <- unique(paste(
    edges$x,
    edges$y,
    edges$xend,
    edges$yend,
    edges$name,
    edges$to,
    edges$direction
  ))

  # every panel draws the same node positions, so the same edge is repeated in
  # the layer's data once per panel that draws it
  expect_lt(nrow(arcs), nrow(edges))
  expect_equal(nrow(arcs), length(drawn))
  expect_equal(
    anyDuplicated(paste(
      arcs$x,
      arcs$y,
      arcs$xend,
      arcs$yend,
      arcs$from,
      arcs$to
    )),
    0
  )
})

test_that("a plain facet keeps one spec row per drawn edge", {
  p <- ggplot(base_dag(), aes_dag()) +
    geom_dag_edges_arc(curvature = 0.4) +
    geom_dag_point() +
    ggplot2::facet_wrap(~name)

  geometry <- discover_edge_geometry(p)
  expect_equal(nrow(geometry), 2)
})

test_that("parallel edges between one pair of nodes keep a spec row each", {
  # a directed and a bidirected edge run between the same two nodes on the
  # same coordinates; a fan spreads them apart only because there are two of
  # them, so both rows have to survive
  dag <- dagify(
    y ~ x,
    x ~ ~y,
    coords = list(x = c(x = 0, y = 2), y = c(x = 0, y = 0))
  ) |>
    tidy_dagitty()

  p <- ggplot(dag, aes_dag()) +
    geom_dag_edges_fan(spread = 0.7) +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  fans <- geometry[geometry$type == "fan", , drop = FALSE]
  expect_equal(nrow(fans), 2)
})

test_that("parallel edges drawn at different curvature keep a spec row each", {
  dag <- dagify(
    y ~ x,
    x ~ ~y,
    coords = list(x = c(x = 0, y = 2), y = c(x = 0, y = 0))
  ) |>
    tidy_dagitty() |>
    dplyr::mutate(
      edge_curvature = ifelse(as.character(direction) == "->", 0.3, -0.3)
    )

  p <- ggplot(dag, aes_dag(edge_curvature = edge_curvature)) +
    geom_dag_arrow_arc() +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  curves <- geometry[geometry$type == "curve", , drop = FALSE]
  expect_equal(nrow(curves), 2)
  expect_setequal(curves$strength, c(0.3, -0.3))
})
