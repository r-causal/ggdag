# Tests for the two edge geometry types the label obstacle machinery discovers
# beyond the ggraph stats and the scalar ggarrow curve:
#
# * "curve": a ggarrow curve layer that maps the `edge_curvature` column, as
#   written by `curve_edge()` and the automatic curving of blocked edges. Each
#   row carries its own strength, and `drawn_edge_points()` traces each edge
#   as the quadratic Bezier arc `sample_curved_edge()` models for that row's
#   curvature. Unlike "ggarrow_curve", these edges are traced for every
#   consumer of `repel_edge_points()`, not only when arrows are asked for.
# * "routed": a layer whose data is waypoint long format, one row per
#   waypoint with `edge_id`, `x`, `y`, and `seq` columns. The edge is traced
#   as the polyline through its waypoints in `seq` order; the first and last
#   waypoints of an edge are its endpoints.

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

# Arc-length position of each point along the polyline, taken at the segment
# the point is closest to; used to check that a trace follows waypoint order.
polyline_position <- function(px, py, poly_x, poly_y) {
  seg_len <- sqrt(diff(poly_x)^2 + diff(poly_y)^2)
  cum_start <- cumsum(c(0, seg_len))
  vapply(
    seq_along(px),
    function(i) {
      best <- Inf
      best_pos <- 0
      for (s in seq_along(seg_len)) {
        dx <- poly_x[s + 1] - poly_x[s]
        dy <- poly_y[s + 1] - poly_y[s]
        len2 <- dx^2 + dy^2
        t <- if (len2 == 0) {
          0
        } else {
          min(
            max(
              ((px[i] - poly_x[s]) * dx + (py[i] - poly_y[s]) * dy) / len2,
              0
            ),
            1
          )
        }
        d <- sqrt(
          (px[i] - (poly_x[s] + t * dx))^2 + (py[i] - (poly_y[s] + t * dy))^2
        )
        if (d < best) {
          best <- d
          best_pos <- cum_start[s] + t * seg_len[s]
        }
      }
      best_pos
    },
    numeric(1)
  )
}

# Distance from each point to the modeled quadratic Bezier arc, by dense
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

# Waypoints in the long format a routing layer carries: one row per waypoint,
# ordered by `seq` within `edge_id`. The x_y edge detours through (1, 0.8);
# the x_m edge runs straight between its endpoints.
routed_waypoints <- function() {
  data.frame(
    edge_id = c("x_y", "x_y", "x_y", "x_m", "x_m"),
    x = c(0, 1, 2, 0, 1),
    y = c(0, 0.8, 0, 0, 1),
    seq = c(1L, 2L, 3L, 1L, 2L),
    stringsAsFactors = FALSE
  )
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

test_that("a waypoint layer is discovered as routed with its waypoints", {
  waypoints <- routed_waypoints()
  p <- ggplot(base_dag(), aes_dag()) +
    ggplot2::geom_path(
      data = waypoints,
      aes(x, y, group = edge_id),
      inherit.aes = FALSE
    ) +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  expect_false(is.null(geometry))
  routed <- geometry[geometry$type == "routed", , drop = FALSE]
  expect_equal(nrow(routed), nrow(waypoints))
  expect_contains(names(routed), c("edge_id", "x", "y", "seq"))

  routed <- routed[order(routed$edge_id, routed$seq), , drop = FALSE]
  waypoints <- waypoints[
    order(waypoints$edge_id, waypoints$seq),
    ,
    drop = FALSE
  ]
  expect_equal(routed$edge_id, waypoints$edge_id)
  expect_equal(routed$seq, waypoints$seq)
  expect_equal(routed$x, waypoints$x)
  expect_equal(routed$y, waypoints$y)
})

test_that("routed and bent edge layers are discovered together", {
  waypoints <- routed_waypoints()
  p <- ggplot(base_dag(), aes_dag()) +
    geom_dag_edges_arc(curvature = 0.4) +
    ggplot2::geom_path(
      data = waypoints,
      aes(x, y, group = edge_id),
      inherit.aes = FALSE
    ) +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  expect_false(is.null(geometry))

  arc <- geometry[geometry$type == "arc", , drop = FALSE]
  expect_equal(nrow(arc), 2)
  expect_equal(unique(arc$strength), 0.4)

  routed <- geometry[geometry$type == "routed", , drop = FALSE]
  expect_equal(nrow(routed), nrow(waypoints))
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
  # positive curvature bows below a left-to-right edge
  expect_lt(min(traced$y), -0.3)
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
  expect_lt(bows[[1]], -0.3)
  expect_gt(bows[[2]], 0.3)

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

# Tracing: type "routed" ---------------------------------------------------

test_that("drawn_edge_points traces routed waypoints in seq order", {
  waypoints <- routed_waypoints()
  # row order deliberately shuffled; `seq` alone carries the drawing order
  geometry <- waypoints[c(3, 1, 5, 2, 4), , drop = FALSE]
  geometry$type <- "routed"

  traced <- drawn_edge_points(geometry, 1L, 24, include_endpoints = TRUE)
  expect_equal(unique(traced$PANEL), 1L)

  groups <- split(traced, traced$edge_id)
  expect_length(groups, 2)

  ends_at <- function(g, x, y) {
    abs(g$x[nrow(g)] - x) < 1e-8 && abs(g$y[nrow(g)] - y) < 1e-8
  }
  xy_group <- groups[[which(vapply(groups, ends_at, logical(1), x = 2, y = 0))]]
  xm_group <- groups[[which(vapply(groups, ends_at, logical(1), x = 1, y = 1))]]

  # each edge starts at its first waypoint and stays on its own polyline
  expect_equal(c(xy_group$x[1], xy_group$y[1]), c(0, 0))
  expect_lt(
    max(polyline_dist(xy_group$x, xy_group$y, c(0, 1, 2), c(0, 0.8, 0))),
    1e-8
  )
  expect_lt(
    max(polyline_dist(xm_group$x, xm_group$y, c(0, 1), c(0, 1))),
    1e-8
  )

  # the trace follows the waypoints in seq order and passes the detour corner
  positions <- polyline_position(
    xy_group$x,
    xy_group$y,
    c(0, 1, 2),
    c(0, 0.8, 0)
  )
  expect_true(all(diff(positions) >= -1e-8))
  expect_lt(min(sqrt((xy_group$x - 1)^2 + (xy_group$y - 0.8)^2)), 0.15)
})

test_that("drawn_edge_points excludes routed endpoints unless asked", {
  geometry <- routed_waypoints()
  geometry$type <- "routed"

  traced <- drawn_edge_points(geometry, 1L, 12, include_endpoints = FALSE)
  terminals <- data.frame(x = c(0, 2, 1), y = c(0, 0, 1))
  for (i in seq_len(nrow(terminals))) {
    expect_gt(
      min(sqrt(
        (traced$x - terminals$x[i])^2 + (traced$y - terminals$y[i])^2
      )),
      1e-6
    )
  }
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
  expect_lt(min(points$y), -0.3)
  expect_true(any(d_chord < 1e-8))
})

test_that("repel_edge_points traces routed specs through their waypoints", {
  # an edge matches a routed spec when its endpoints are the spec's first and
  # last waypoints in seq order
  edges <- data.frame(x = 0, y = 0, xend = 2, yend = 0, PANEL = 1L)
  geometry <- data.frame(
    edge_id = "x_y",
    x = c(0, 1, 2),
    y = c(0, 0.8, 0),
    seq = 1:3,
    type = "routed",
    stringsAsFactors = FALSE
  )

  points <- repel_edge_points(
    edges,
    12,
    geometry,
    NULL,
    include_endpoints = TRUE
  )

  expect_gt(max(points$y), 0.5)
  expect_lt(
    max(polyline_dist(points$x, points$y, c(0, 1, 2), c(0, 0.8, 0))),
    1e-8
  )
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
  expect_lt(min(obstacles$y), -0.3)
  below <- obstacles[obstacles$y < 0, , drop = FALSE]
  if (nrow(below) > 0) {
    expect_lt(max(arc_dist(below$x, below$y, 0, 0, 2, 0, 0.3)), 0.05)
  }
})

test_that("StatNodesLabelAuto receives routed obstacles from a waypoint layer", {
  dag <- dagify(
    y ~ x,
    m ~ x,
    labels = c(x = "Exposure", m = "Mediator", y = "Outcome"),
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )
  waypoints <- data.frame(
    edge_id = "x_y",
    x = c(0, 1, 2),
    y = c(0, 0.8, 0),
    seq = 1:3,
    stringsAsFactors = FALSE
  )
  p <- ggplot(dag, aes_dag()) +
    ggplot2::geom_path(
      data = waypoints,
      aes(x, y, group = edge_id),
      inherit.aes = FALSE
    ) +
    geom_dag_point() +
    geom_dag_label_auto(aes(label = label))

  index <- stat_layer_index(p, "StatNodesLabelAuto")
  expect_length(index, 1)
  built <- ggplot2::layer_data(p, index)

  edge_rows <- built[built$ggdag_role == "edge", , drop = FALSE]
  expect_gt(nrow(edge_rows), 0)
  expect_length(unique(edge_rows$edge_id), 2)

  # points strictly between the x -> y chord and the x -> m chord exist only
  # if the detour polyline was traced, and they sit on it
  detour <- edge_rows[
    edge_rows$y > 0.3 & (edge_rows$x - edge_rows$y) > 0.05,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(detour), 0)
  if (nrow(detour) > 0) {
    expect_lt(
      max(polyline_dist(detour$x, detour$y, c(0, 1, 2), c(0, 0.8, 0))),
      1e-6
    )
  }
})
