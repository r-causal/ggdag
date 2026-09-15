# Tests for the two helpers that carry a drawn arc to the automatic label
# engine, one at each end of the journey.
#
# * `arrow_chord_points()` (R/StatsandGeoms.R) hands the label stat the two
#   ends of a ggarrow curve layer's chord and the curvature the layer draws it
#   at. `grid::curveGrob()` bends the arc in device units, so where the ink
#   lands is not known while the stat runs.
# * `trace_curved_obstacles()` (R/label_auto.R) is the other end of it. Inside
#   `makeContent.dag_labels_auto()`, where the panel's millimetres are known,
#   it replaces every edge that carries a curvature with the points of that
#   edge's arc and keeps the routing spec aligned row for row, so that the
#   routed obstacles built from it next still read each edge's own spec.
#
# The tracer works in whatever units it is handed, so these blocks pass plain
# numbers standing for the millimetres the grob passes.

# The obstacle rows of one edge together with its routing spec, in the shape
# `makeContent.dag_labels_auto()` holds them: the points and the spec are one
# frame there, and the points alone are handed to the tracer beside it.
traced_edge <- function(
  edge_id,
  x,
  y,
  curvature = NA_real_,
  route_style = NA_character_,
  route_fixed = NA
) {
  rows <- data.frame(
    edge_id = edge_id,
    x = x,
    y = y,
    PANEL = 1L,
    route_style = route_style,
    route_layer_axis = NA_character_,
    route_cap = NA_real_,
    route_fixed = route_fixed,
    curvature = curvature,
    stringsAsFactors = FALSE
  )
  rows$route_options <- rep(list(NULL), nrow(rows))
  rows
}

# The tracer's two arguments from one such frame, as the grob passes them.
trace_frames <- function(spec, n) {
  trace_curved_obstacles(spec[, c("edge_id", "x", "y")], spec, n = n)
}

# How far a path reaches to either side of the line between its own two ends.
depth_either_side <- function(path) {
  last <- nrow(path)
  dx <- path$x[[last]] - path$x[[1]]
  dy <- path$y[[last]] - path$y[[1]]
  offsets <- ((dx * (path$y - path$y[[1]]) - (path$x - path$x[[1]]) * dy) /
    sqrt(dx^2 + dy^2))
  c(above = max(offsets), below = min(offsets))
}

# Tracing curved obstacles -----------------------------------------------------

test_that("every curved edge of a panel is traced at its own curvature", {
  spec <- rbind(
    traced_edge("bows_down", c(0, 20), c(0, 0), curvature = 0.5),
    traced_edge("bows_up", c(0, 20), c(10, 10), curvature = -0.5),
    traced_edge("straight", c(0, 20), c(20, 20))
  )

  traced <- trace_frames(spec, n = 12)

  # the spec keeps a row for every point, and the two frames name the same
  # edge in the same row, which is what the routed tracer called next reads
  expect_equal(nrow(traced$edges), nrow(traced$spec))
  expect_identical(traced$edges$edge_id, traced$spec$edge_id)

  # the edge no curve layer draws keeps the two points it arrived as
  kept <- traced$edges[traced$edges$edge_id == "straight", , drop = FALSE]
  expect_equal(kept$x, c(0, 20))
  expect_equal(kept$y, c(20, 20))

  # each arc is the one its own curvature models, and the two bow to
  # opposite sides of their chords
  for (case in list(c("bows_down", 0.5), c("bows_up", -0.5))) {
    arc <- traced$edges[traced$edges$edge_id == case[[1]], , drop = FALSE]
    y <- if (identical(case[[1]], "bows_down")) 0 else 10
    expected <- sample_curved_edge(
      0,
      y,
      20,
      y,
      curvature = as.numeric(case[[2]]),
      n = 12
    )
    expect_equal(arc$x, expected$x)
    expect_equal(arc$y, expected$y)
    expect_equal(
      traced$spec$curvature[traced$spec$edge_id == case[[1]]],
      rep(as.numeric(case[[2]]), 12)
    )
  }
  down <- traced$edges[traced$edges$edge_id == "bows_down", , drop = FALSE]
  up <- traced$edges[traced$edges$edge_id == "bows_up", , drop = FALSE]
  expect_lt(min(depth_either_side(down)), -4)
  expect_gt(max(depth_either_side(up)), 4)

  # a panel of edges none of which is drawn as an arc is left alone
  plain <- traced_edge("straight", c(0, 20), c(20, 20))
  expect_identical(
    trace_frames(plain, n = 12)$edges,
    plain[, c("edge_id", "x", "y")]
  )
})

test_that("a curve layer drawn at zero curvature traces its chord", {
  # a layer that bends nothing, and one that never named a curvature at all,
  # both reach the stat as the two ends of the chord, and a curvature of zero
  # is a curvature: the edge is traced, along the chord itself, rather than
  # left as the two points it arrived as
  geometry <- data.frame(
    x = c(0, 0),
    y = c(0, 10),
    xend = c(20, 20),
    yend = c(0, 10),
    curvature = c(0, NA_real_),
    stringsAsFactors = FALSE
  )

  points <- arrow_chord_points(geometry, 1L)

  expect_equal(nrow(points), 4)
  expect_equal(points$x, c(0, 20, 0, 20))
  expect_equal(points$y, c(0, 0, 10, 10))
  expect_equal(points$curvature, c(0, 0, 0, 0))
  expect_equal(points$PANEL, rep(1L, 4))
  expect_length(unique(points$edge_id), 2)

  spec <- points
  spec$route_style <- NA_character_
  spec$route_layer_axis <- NA_character_
  spec$route_cap <- NA_real_
  spec$route_fixed <- NA
  spec$route_options <- rep(list(NULL), nrow(spec))

  traced <- trace_frames(spec, n = 8)

  expect_equal(nrow(traced$edges), 16)
  for (id in unique(traced$edges$edge_id)) {
    chord <- traced$edges[traced$edges$edge_id == id, , drop = FALSE]
    expect_equal(nrow(chord), 8)
    expect_equal(chord$x, seq(0, 20, length.out = 8))
    expect_equal(chord$y, rep(chord$y[[1]], 8))
  }
})

test_that("a pinned routed edge is retraced from the ends of its chord", {
  # An edge the user pinned with `curve_edge()` is never rerouted, so the
  # stat traces it in data space and the routed layer draws it as the arc it
  # asked for. The bow is settled on the page, so the grob keeps the two ends
  # of the path it arrived as, which are the ends of the chord, and traces
  # the arc again between them in the millimetres it is drawn in. Here the
  # arriving path is stretched three times over across the chord, the way the
  # panel's shape stretches a data-space trace, so a path passed through
  # rather than retraced would be three times as deep as the arc.
  arc <- sample_curved_edge(0, 0, 30, 0, curvature = 0.5, n = 10)
  spec <- traced_edge(
    "pinned",
    arc$x,
    arc$y * 3,
    curvature = 0.5,
    route_style = "orthogonal",
    route_fixed = TRUE
  )

  traced <- trace_frames(spec, n = 16)

  expected <- sample_curved_edge(0, 0, 30, 0, curvature = 0.5, n = 16)
  expect_equal(nrow(traced$edges), 16)
  expect_equal(traced$edges$x, expected$x)
  expect_equal(traced$edges$y, expected$y)

  # the ends are the ends it arrived with, and the stretched interior is gone
  expect_equal(traced$edges$x[c(1, 16)], c(0, 30))
  expect_equal(traced$edges$y[c(1, 16)], c(0, 0))
  expect_lt(max(abs(traced$edges$y)), max(abs(spec$y)) / 2)

  # the spec still says the edge is pinned, so the routed tracer called next
  # leaves it where it is instead of routing it
  expect_equal(nrow(traced$spec), 16)
  expect_true(all(traced$spec$route_fixed))
  expect_equal(traced$spec$route_style, rep("orthogonal", 16))
  expect_equal(traced$spec$curvature, rep(0.5, 16))
})

test_that("a pinned routed edge reaches the grob as a path between its ends", {
  # the premise of the block above: `repel_edge_points()` hands the automatic
  # label stat a routed edge whose curvature the user set as a path in data
  # space, endpoints included, tagged as pinned and carrying the curvature it
  # is drawn at
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
    curvature = 0.5,
    route_style = "orthogonal",
    stringsAsFactors = FALSE
  )
  geometry$route_options <- list(edge_route_options())

  points <- repel_edge_points(
    edges,
    10,
    geometry,
    NULL,
    include_endpoints = TRUE,
    trace_arrows = TRUE
  )

  expect_gt(nrow(points), 2)
  expect_true(all(points$route_fixed))
  expect_equal(points$curvature, rep(0.5, nrow(points)))
  expect_equal(points$x[c(1, nrow(points))], c(0, 2))
  expect_equal(points$y[c(1, nrow(points))], c(0, 0))
  # and it is a bowed path rather than the chord, so the ends the grob reads
  # are the ends of a curve it is about to draw again
  expect_lt(min(points$y), -0.25)
})
