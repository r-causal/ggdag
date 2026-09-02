# Tests for route_edges_mm(), the millimetre-space edge router, and its
# helpers. The router works entirely in millimetres at draw time: nodes are
# discs of known radius, layers are inferred from the layer-axis coordinate,
# a blocked edge gets one waypoint per crossed layer (or a single bow around
# the obstacle), the waypoint chain is reduced to one arch by a convex hull,
# and the result is drawn as a centripetal Catmull-Rom spline with end
# tangents clamped toward the chord. Every expectation is a numeric predicate
# on the returned geometry; there are no snapshots.
#
# Coordinates are in mm throughout. The layer axis is x and "above" (side +1)
# means larger y. The default node size 16 draws a disc of radius r = 6 mm,
# which fixes the constants used below: margin m = 3, so R = 9 is the full
# obstruction radius and R_soft = 7.2 is the floor below which a node is hit
# rather than grazed. Sampled minima carry a 0.1 mm verification tolerance.

r_default <- 6
r_full <- 9
r_soft <- 7.2
verify_tol <- 0.1

# Scene construction ------------------------------------------------------------

mm_nodes <- function(name, x, y, r = r_default) {
  data.frame(name = name, x = x, y = y, r = r, stringsAsFactors = FALSE)
}

mm_edges <- function(from, to, curvature = NA_real_) {
  data.frame(
    from = from,
    to = to,
    direction = "->",
    curvature = curvature,
    stringsAsFactors = FALSE
  )
}

edge_labels <- function(edges) {
  paste0(edges$from, "->", edges$to)
}

pt <- function(x, y) {
  data.frame(x = x, y = y)
}

# Fixtures are specified on a 160 x 110 mm panel. The device-size tests
# rescale node coordinates per axis and keep the 6 mm radius, which is what
# happens when the same plot is drawn on a different device.
panel_scale <- function(panel) {
  c(panel[1] / 160, panel[2] / 110)
}

mediator_scene <- function(m_y = 55, panel = c(160, 110)) {
  s <- panel_scale(panel)
  list(
    nodes = mm_nodes(
      c("x", "m", "y"),
      c(7.3, 80, 152.7) * s[1],
      c(55, m_y, 55) * s[2]
    ),
    edges = mm_edges(c("x", "m", "x"), c("m", "y", "y")),
    bounds = c(0, 0, panel)
  )
}

fan_scene <- function(panel = c(160, 110)) {
  s <- panel_scale(panel)
  list(
    nodes = mm_nodes(
      c("a", "b", "c", "d", "e"),
      c(20, 80, 80, 80, 140) * s[1],
      c(55, 85, 55, 25, 55) * s[2]
    ),
    edges = mm_edges(
      c("a", "a", "a", "b", "c", "a"),
      c("b", "c", "d", "e", "e", "e")
    ),
    bounds = c(0, 0, panel)
  )
}

four_layer_scene <- function(panel = c(160, 110)) {
  s <- panel_scale(panel)
  list(
    nodes = mm_nodes(
      c("p", "q1", "q2", "s1", "s2", "s3", "t"),
      c(20, 60, 60, 100, 100, 100, 140) * s[1],
      c(55, 75, 35, 90, 55, 20, 55) * s[2]
    ),
    edges = mm_edges(
      c("p", "p", "q1", "q1", "q2", "q2", "s1", "s2", "s3", "p"),
      c("q1", "q2", "s1", "s2", "s2", "s3", "t", "t", "t", "t")
    ),
    bounds = c(0, 0, panel)
  )
}

# A long horizontal chord with one node above and one below it; `offset` is
# the perpendicular distance of each node centre from the chord.
weave_scene <- function(offset = 16) {
  list(
    nodes = mm_nodes(
      c("S", "T", "A", "B"),
      c(0, 640, 160, 480),
      c(0, 0, offset, -offset)
    ),
    edges = mm_edges(c("S", "A"), c("T", "B")),
    bounds = c(-10, -60, 650, 60)
  )
}

scale_scene <- function(scene, k) {
  scene$nodes$x <- scene$nodes$x * k
  scene$nodes$y <- scene$nodes$y * k
  scene$nodes$r <- scene$nodes$r * k
  scene$bounds <- scene$bounds * k
  scene
}

route_scene <- function(scene, ...) {
  route_edges_mm(scene$nodes, scene$edges, scene$bounds, ...)
}

node_xy <- function(scene, name) {
  i <- match(name, scene$nodes$name)
  c(scene$nodes$x[i], scene$nodes$y[i])
}

edge_endpoints <- function(scene, i) {
  list(
    from = node_xy(scene, scene$edges$from[i]),
    to = node_xy(scene, scene$edges$to[i])
  )
}

other_nodes <- function(scene, i) {
  keep <- !scene$nodes$name %in% c(scene$edges$from[i], scene$edges$to[i])
  scene$nodes[keep, , drop = FALSE]
}

# Canonical DAG scenes: the layout fixture's data-space coordinates mapped
# into a panel with a fixed margin using independent x and y scaling, which
# reproduces the free aspect ratio of a real ggplot panel.
layout_fixture <- readRDS(test_path("fixtures", "layout-invariance.rds"))

to_mm_range <- function(v, lo, hi) {
  rg <- range(v)
  if (diff(rg) == 0) {
    return(rep((lo + hi) / 2, length(v)))
  }
  lo + (v - rg[1]) / diff(rg) * (hi - lo)
}

canonical_layer_index <- function(name) {
  layers <- layout_fixture[[name]]$ordering$layer_nodes
  idx <- rep(seq_along(layers), lengths(layers))
  names(idx) <- unlist(layers)
  idx
}

canonical_scene <- function(name, panel = c(160, 110), margin = 10) {
  coords <- layout_fixture[[name]]$coords
  edges <- canonical_dag_edges(canonical_dag_specs[[name]])
  edges <- edges[!is.na(edges$to), , drop = FALSE]
  list(
    name = name,
    nodes = mm_nodes(
      coords$name,
      to_mm_range(coords$x, margin, panel[1] - margin),
      to_mm_range(coords$y, margin, panel[2] - margin)
    ),
    edges = mm_edges(edges$name, edges$to),
    bounds = c(0, 0, panel),
    layer = canonical_layer_index(name)
  )
}

canonical_span <- function(scene, i) {
  abs(
    scene$layer[[scene$edges$from[i]]] - scene$layer[[scene$edges$to[i]]]
  )
}

# Geometry predicates -----------------------------------------------------------

# Closest approach of the straight chord of edge i to any non-endpoint node.
chord_min_clearance <- function(scene, i) {
  ends <- edge_endpoints(scene, i)
  others <- other_nodes(scene, i)
  if (nrow(others) == 0) {
    return(Inf)
  }
  min(dist_to_edge(
    others$x,
    others$y,
    ends$from[1],
    ends$from[2],
    ends$to[1],
    ends$to[2]
  ))
}

# Distance from a node centre to a sampled path, measured to the segments so
# that the result does not depend on the sampling density.
path_min_dist <- function(path, centre) {
  n <- nrow(path)
  if (n < 2) {
    return(sqrt((path$x - centre[1])^2 + (path$y - centre[2])^2))
  }
  min(vapply(
    seq_len(n - 1),
    function(i) {
      dist_to_edge(
        centre[1],
        centre[2],
        path$x[i],
        path$y[i],
        path$x[i + 1],
        path$y[i + 1]
      )
    },
    numeric(1)
  ))
}

# Closest approach of a sampled path to any non-endpoint node of edge i.
path_min_clearance <- function(scene, i, path) {
  others <- other_nodes(scene, i)
  if (nrow(others) == 0) {
    return(Inf)
  }
  min(vapply(
    seq_len(nrow(others)),
    function(j) path_min_dist(path, c(others$x[j], others$y[j])),
    numeric(1)
  ))
}

angle_between <- function(a, b) {
  atan2(a[1] * b[2] - a[2] * b[1], a[1] * b[1] + a[2] * b[2]) * 180 / pi
}

# Signed turning angle in degrees between consecutive non-degenerate segments.
turning_angles <- function(path) {
  dx <- diff(path$x)
  dy <- diff(path$y)
  keep <- dx^2 + dy^2 > 0
  dx <- dx[keep]
  dy <- dy[keep]
  n <- length(dx)
  if (n < 2) {
    return(numeric(0))
  }
  a <- seq_len(n - 1)
  b <- a + 1
  atan2(dx[a] * dy[b] - dy[a] * dx[b], dx[a] * dx[b] + dy[a] * dy[b]) *
    180 /
    pi
}

# Sign changes of the turning angle after suppressing near-zero turns.
count_inflections <- function(path, suppress = 0.1) {
  theta <- turning_angles(path)
  s <- sign(theta[abs(theta) >= suppress])
  sum(diff(s) != 0)
}

chord_unit <- function(from, to) {
  (to - from) / sqrt(sum((to - from)^2))
}

# Projection of each path point onto the chord, in mm from `from`.
chord_progress <- function(path, from, to) {
  u <- chord_unit(from, to)
  (path$x - from[1]) * u[1] + (path$y - from[2]) * u[2]
}

# Signed perpendicular offset of each path point from the chord; positive is
# to the left of the from -> to direction, which is "above" for a chord that
# runs left to right.
chord_offset <- function(path, from, to) {
  u <- chord_unit(from, to)
  u[1] * (path$y - from[2]) - u[2] * (path$x - from[1])
}

chord_length <- function(from, to) {
  sqrt(sum((to - from)^2))
}

# Angle between the path's local direction at the point `back` mm of arc
# before the target and the radial direction from that point into the
# target's centre. This is where the visible arrowhead starts after ggarrow
# resects the edge cap, so it should be aimed at the node centre.
arrival_angle <- function(path, to, back = 8) {
  seg <- sqrt(diff(path$x)^2 + diff(path$y)^2)
  from_end <- rev(cumsum(rev(seg)))
  k <- max(which(from_end >= back))
  tangent <- c(path$x[k + 1] - path$x[k], path$y[k + 1] - path$y[k])
  radial <- c(to[1] - path$x[k], to[2] - path$y[k])
  abs(angle_between(tangent, radial))
}

# Proper crossing of two segments; touching at an endpoint does not count.
segments_cross <- function(p1, p2, q1, q2) {
  side <- function(a, b, c) {
    sign((b[1] - a[1]) * (c[2] - a[2]) - (b[2] - a[2]) * (c[1] - a[1]))
  }
  d1 <- side(q1, q2, p1)
  d2 <- side(q1, q2, p2)
  d3 <- side(p1, p2, q1)
  d4 <- side(p1, p2, q2)
  d1 * d2 < 0 && d3 * d4 < 0
}

count_path_crossings <- function(path, a, b) {
  n <- nrow(path)
  sum(vapply(
    seq_len(n - 1),
    function(i) {
      segments_cross(
        c(path$x[i], path$y[i]),
        c(path$x[i + 1], path$y[i + 1]),
        a,
        b
      )
    },
    logical(1)
  ))
}

max_dist_to_polyline <- function(pts, poly) {
  n <- nrow(poly)
  d <- vapply(
    seq_len(n - 1),
    function(i) {
      dist_to_edge(
        pts$x,
        pts$y,
        poly$x[i],
        poly$y[i],
        poly$x[i + 1],
        poly$y[i + 1]
      )
    },
    numeric(nrow(pts))
  )
  d <- matrix(d, nrow = nrow(pts))
  max(apply(d, 1, min))
}

# Symmetric Hausdorff distance between two polylines, measured point to
# segment so that different sample counts do not inflate the distance.
polyline_hausdorff <- function(a, b) {
  max(max_dist_to_polyline(a, b), max_dist_to_polyline(b, a))
}

expect_straight_path <- function(path, from, to) {
  expect_s3_class(path, "data.frame")
  expect_identical(nrow(path), 2L)
  expect_identical(c(path$x[1], path$y[1]), from)
  expect_identical(c(path$x[2], path$y[2]), to)
}

expect_exact_endpoints <- function(path, from, to) {
  n <- nrow(path)
  expect_identical(c(path$x[1], path$y[1]), from)
  expect_identical(c(path$x[n], path$y[n]), to)
}

# Constants ----------------------------------------------------------------------

test_that("route_opts() derives the design constants from the reference radius", {
  opts <- route_opts(6)
  expect_type(opts, "list")
  expect_equal(opts$r_ref, 6)
  # clearance margin m = max(0.5 r, 1.2) and its soft floor
  expect_equal(opts$m, 3)
  expect_equal(opts$m_min, 1.2)
  expect_equal(opts$R, 9)
  expect_equal(opts$R_soft, 7.2)
  # edge-edge separation in a slot and multi-edge translation
  expect_equal(opts$sep_e, 3.6)
  expect_equal(opts$sep_m, 6)
  # layer clustering tolerance is one radius
  expect_equal(opts$tol_layer, 6)
  expect_equal(opts$steep_deg, 60)
  expect_equal(opts$sagitta_max, 0.22)
  expect_equal(opts$t_clamp, c(0.2, 0.8))
  expect_equal(opts$tangent_clamp, 40)
  expect_equal(opts$alpha, 0.5)
  expect_equal(opts$sample_spacing, 0.5)
  expect_equal(opts$sample_min_n, 16)
  expect_equal(opts$crossing_penalty, 16)
  expect_equal(opts$congestion_penalty, 2)
  expect_equal(opts$displacement_weight, 1)
  expect_equal(opts$periphery_span, 3)
  expect_equal(opts$verify_tol, 0.1)
})

test_that("route_opts() applies the millimetre floors at small radii", {
  opts <- route_opts(2)
  expect_equal(opts$m, 1.2)
  expect_equal(opts$R, 3.2)
  expect_equal(opts$R_soft, 3.2)
  expect_equal(opts$sep_e, 1.5)
  expect_equal(opts$sep_m, 2.5)
  expect_equal(opts$tol_layer, 2)
})

test_that("route_opts() scales with the radius above the floors", {
  opts <- route_opts(12)
  expect_equal(opts$m, 6)
  expect_equal(opts$R, 18)
  expect_equal(opts$R_soft, 13.2)
  expect_equal(opts$sep_e, 7.2)
  expect_equal(opts$sep_m, 12)
})

# Output structure ------------------------------------------------------------

test_that("route_edges_mm() returns paths, meta, and waypoints in input edge order", {
  scene <- mediator_scene()
  res <- route_scene(scene)

  expect_type(res, "list")
  expect_named(res, c("paths", "meta", "waypoints"))
  expect_length(res$paths, 3)
  expect_length(res$waypoints, 3)

  expect_s3_class(res$meta, "data.frame")
  expect_identical(nrow(res$meta), 3L)
  expect_named(
    res$meta,
    c(
      "edge",
      "routed",
      "mode",
      "side",
      "n_waypoints",
      "waypoint_layers",
      "clearance_ok",
      "sagitta_ratio",
      "sagitta_capped"
    )
  )
  expect_equal(res$meta$edge, edge_labels(scene$edges))
  expect_type(res$meta$routed, "logical")
  expect_type(res$meta$mode, "character")
  expect_type(res$meta$waypoint_layers, "list")
  expect_type(res$meta$clearance_ok, "logical")
  expect_type(res$meta$sagitta_ratio, "double")
  expect_type(res$meta$sagitta_capped, "logical")

  for (path in res$paths) {
    expect_s3_class(path, "data.frame")
    expect_named(path, c("x", "y"))
  }
  for (wp in res$waypoints) {
    expect_s3_class(wp, "data.frame")
    expect_named(wp, c("x", "y", "layer"))
  }
})

test_that("route_edges_mm() rejects an unknown mode with a typed error naming the choices", {
  err <- expect_error(
    route_scene(mediator_scene(), mode = "bogus"),
    class = "ggdag_type_error"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "spline")
  expect_match(msg, "orthogonal")
  expect_match(msg, "straight")
})

test_that("orthogonal routing is not yet implemented", {
  skip("orthogonal routing is not yet implemented")
})

# Fixture 1: mediator -----------------------------------------------------------

test_that("mediator: unblocked edges are two-row straight paths with bit-exact endpoints", {
  scene <- mediator_scene()
  res <- route_scene(scene)

  for (i in 1:2) {
    ends <- edge_endpoints(scene, i)
    expect_false(res$meta$routed[i])
    expect_equal(res$meta$mode[i], "straight")
    expect_equal(res$meta$n_waypoints[i], 0)
    expect_identical(nrow(res$waypoints[[i]]), 0L)
    expect_straight_path(res$paths[[i]], ends$from, ends$to)
  }
})

test_that("mediator: x->y arches above m through the (80, 64) slot", {
  scene <- mediator_scene()
  res <- route_scene(scene)

  expect_true(res$meta$routed[3])
  expect_equal(res$meta$mode[3], "interior")
  # both sides cost the same (displacement 9, no crossings, no congestion
  # because the far endpoints of x->m and m->y lie on the chord); the tie
  # rule picks above
  expect_equal(res$meta$side[3], 1)
  expect_equal(res$meta$n_waypoints[3], 1)
  expect_equal(res$meta$waypoint_layers[[3]], 2)
  expect_true(res$meta$clearance_ok[3])
  expect_false(res$meta$sagitta_capped[3])
  # sagitta 9 / 145.4
  expect_lt(abs(res$meta$sagitta_ratio[3] - 9 / 145.4), 0.01)

  wp <- res$waypoints[[3]]
  expect_identical(nrow(wp), 1L)
  expect_equal(wp$x, 80)
  expect_equal(wp$y, 64)
  expect_equal(wp$layer, 2)
})

test_that("mediator: the routed path clears m, stays above the chord, and is smooth", {
  scene <- mediator_scene()
  res <- route_scene(scene)
  path <- res$paths[[3]]
  ends <- edge_endpoints(scene, 3)

  expect_gte(nrow(path), 16)
  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(path_min_dist(path, node_xy(scene, "m")), r_full - verify_tol)
  expect_true(all(path$y >= 55 - 1e-9))
  # monotone progress along the chord
  expect_true(all(diff(path$x) >= -0.1))
  # sagitta of the sampled path is bounded like the waypoint's
  offset <- chord_offset(path, ends$from, ends$to)
  expect_lte(max(offset) / chord_length(ends$from, ends$to), 0.22)
  expect_lt(max(abs(turning_angles(path))), 12)
  expect_lte(count_inflections(path), 2)
  expect_lt(arrival_angle(path, ends$to), 15)
})

# Fixture 2: fan ---------------------------------------------------------------

test_that("fan: every edge but a->e is straight", {
  scene <- fan_scene()
  res <- route_scene(scene)

  for (i in 1:5) {
    ends <- edge_endpoints(scene, i)
    expect_false(res$meta$routed[i])
    expect_straight_path(res$paths[[i]], ends$from, ends$to)
  }
})

test_that("fan: a->e bows below c, away from the crowded side of the fan", {
  scene <- fan_scene()
  res <- route_scene(scene)
  path <- res$paths[[6]]
  ends <- edge_endpoints(scene, 6)

  expect_true(res$meta$routed[6])
  expect_equal(res$meta$mode[6], "interior")
  # b sits above the chord at both a and e, d below at a only: congestion
  # 2 above versus 1 below sends the bow down
  expect_equal(res$meta$side[6], -1)
  expect_equal(res$meta$n_waypoints[6], 1)
  expect_equal(res$meta$waypoint_layers[[6]], 2)
  expect_true(res$meta$clearance_ok[6])
  expect_false(res$meta$sagitta_capped[6])

  wp <- res$waypoints[[6]]
  expect_identical(nrow(wp), 1L)
  expect_equal(wp$x, 80)
  expect_equal(wp$y, 46)

  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(path_min_dist(path, node_xy(scene, "c")), r_full - verify_tol)
  expect_gt(path_min_dist(path, node_xy(scene, "d")), r_full)
  expect_gte(path_min_clearance(scene, 6, path), r_full - verify_tol)
  expect_true(all(path$y <= 55 + 1e-9))
  expect_true(all(diff(path$x) >= -0.1))
  expect_lt(max(abs(turning_angles(path))), 12)
  expect_lte(count_inflections(path), 2)
  expect_lt(arrival_angle(path, ends$to), 15)
})

# Fixture 3: four-layer periphery ----------------------------------------------

test_that("four-layer: every short edge is straight", {
  scene <- four_layer_scene()
  res <- route_scene(scene)

  for (i in 1:9) {
    ends <- edge_endpoints(scene, i)
    expect_false(res$meta$routed[i])
    expect_straight_path(res$paths[[i]], ends$from, ends$to)
  }
})

test_that("four-layer: p->t sweeps the periphery above the two middle layers", {
  scene <- four_layer_scene()
  res <- route_scene(scene)
  path <- res$paths[[10]]
  ends <- edge_endpoints(scene, 10)

  expect_true(res$meta$routed[10])
  expect_equal(res$meta$mode[10], "periphery")
  # threading the interior crosses q1->s2 (or q2->s2 below); the periphery
  # arch costs only displacement and ties above versus below, so above wins
  expect_equal(res$meta$side[10], 1)
  expect_equal(res$meta$n_waypoints[10], 2)
  expect_equal(res$meta$waypoint_layers[[10]], c(2, 3))
  expect_true(res$meta$clearance_ok[10])
  # arches around a stack are exempt from the sagitta cap
  expect_false(res$meta$sagitta_capped[10])

  # The slots snap to the outer free interval of each layer: 75 + 9 = 84 at
  # layer 2 and 90 + 9 = 99 at layer 3. Those are lower bounds rather than
  # exact positions because the spline approaches each waypoint at an angle
  # and the verify step may push a waypoint further out along its layer; a
  # repaired waypoint still belongs to its layer's outer slot.
  wp <- res$waypoints[[10]]
  expect_identical(nrow(wp), 2L)
  expect_equal(wp$layer, c(2, 3))
  expect_gte(wp$y[1], 84 - 1e-6)
  expect_lte(wp$y[1], 110 - 0.5)
  expect_gte(wp$y[2], 99 - 1e-6)
  expect_lte(wp$y[2], 110 - 0.5)
  # repair moves a waypoint along its layer only
  expect_equal(wp$x, c(60, 100))

  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(path_min_dist(path, node_xy(scene, "q1")), r_full - verify_tol)
  expect_gte(path_min_dist(path, node_xy(scene, "s1")), r_full - verify_tol)
  expect_gte(path_min_clearance(scene, 10, path), r_full - verify_tol)
  expect_true(all(path$y >= 55 - 1e-9))
  expect_true(all(diff(path$x) >= -0.1))
  expect_lt(max(abs(turning_angles(path))), 12)
  expect_lt(arrival_angle(path, ends$to), 15)
  # runs outside q1->s1 rather than through it
  expect_identical(
    count_path_crossings(path, node_xy(scene, "q1"), node_xy(scene, "s1")),
    0L
  )
})

test_that("layer_free_intervals() lists the gaps between padded nodes with outer flags", {
  bounds <- c(0, 0, 160, 110)

  # layer 2 of the four-layer fixture: nodes at y = 75 and 35, R = 9, and a
  # 0.5 mm pad at the panel edge
  l2 <- layer_free_intervals(mm_nodes(c("q1", "q2"), 60, c(75, 35)), 3, bounds)
  expect_s3_class(l2, "data.frame")
  expect_named(l2, c("lo", "hi", "outer"))
  expect_equal(l2$lo, c(0.5, 44, 84))
  expect_equal(l2$hi, c(26, 66, 109.5))
  expect_equal(l2$outer, c(TRUE, FALSE, TRUE))

  # layer 3: nodes at 90, 55, 20
  l3 <- layer_free_intervals(
    mm_nodes(c("s1", "s2", "s3"), 100, c(90, 55, 20)),
    3,
    bounds
  )
  expect_equal(l3$lo, c(0.5, 29, 64, 99))
  expect_equal(l3$hi, c(11, 46, 81, 109.5))
  expect_equal(l3$outer, c(TRUE, FALSE, FALSE, TRUE))

  # a gap narrower than 2 R yields no interval
  tight <- layer_free_intervals(mm_nodes(c("u", "v"), 60, c(40, 50)), 3, bounds)
  expect_equal(tight$lo, c(0.5, 59))
  expect_equal(tight$hi, c(31, 109.5))
  expect_equal(tight$outer, c(TRUE, TRUE))

  # a single node splits the layer into two outer intervals
  one <- layer_free_intervals(mm_nodes("m", 80, 55), 3, bounds)
  expect_equal(one$lo, c(0.5, 64))
  expect_equal(one$hi, c(46, 109.5))
  expect_equal(one$outer, c(TRUE, TRUE))
})

test_that("nearest_free_y() snaps to the nearest free y on the requested side", {
  ints <- layer_free_intervals(
    mm_nodes(c("s1", "s2", "s3"), 100, c(90, 55, 20)),
    3,
    c(0, 0, 160, 110)
  )

  # interior: the smallest free y >= 55 above, the largest <= 55 below
  expect_equal(nearest_free_y(ints, 55, 1, FALSE), 64)
  expect_equal(nearest_free_y(ints, 55, -1, FALSE), 46)
  # periphery: outer intervals only
  expect_equal(nearest_free_y(ints, 55, 1, TRUE), 99)
  expect_equal(nearest_free_y(ints, 55, -1, TRUE), 11)
  # a chord point that is already free is returned unchanged
  expect_equal(nearest_free_y(ints, 30, 1, FALSE), 30)
  expect_equal(nearest_free_y(ints, 30, -1, FALSE), 30)
  # nothing free on that side of the panel
  expect_true(is.na(nearest_free_y(ints, 200, 1, FALSE)))
  expect_true(is.na(nearest_free_y(ints, -50, -1, FALSE)))
})

# Fixture 4: weave -------------------------------------------------------------

test_that("weave: nodes 16 mm off the chord are not obstacles", {
  scene <- weave_scene(16)
  res <- route_scene(scene)

  for (i in 1:2) {
    ends <- edge_endpoints(scene, i)
    expect_false(res$meta$routed[i])
    expect_equal(res$meta$n_waypoints[i], 0)
    expect_straight_path(res$paths[[i]], ends$from, ends$to)
  }
})

test_that("weave: grazed nodes in the soft band get sub-2 mm nudges", {
  scene <- weave_scene(8.5)
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)

  expect_true(res$meta$routed[1])
  expect_equal(res$meta$mode[1], "soft")
  expect_exact_endpoints(path, ends$from, ends$to)
  # the nudge amplitude is R - d = 0.5 mm per node, well under m - m_min
  expect_lt(max(abs(path$y)), 1.8)
  expect_lte(res$meta$sagitta_ratio[1], 1.8 / 640)
  expect_gte(path_min_dist(path, node_xy(scene, "A")), r_soft - verify_tol)
  expect_gte(path_min_dist(path, node_xy(scene, "B")), r_soft - verify_tol)
  expect_true(all(diff(path$x) >= -0.1))
  expect_lt(max(abs(turning_angles(path))), 12)
  # a sub-2 mm S is allowed here
  expect_lte(count_inflections(path), 3)

  ends2 <- edge_endpoints(scene, 2)
  expect_false(res$meta$routed[2])
  expect_straight_path(res$paths[[2]], ends2$from, ends2$to)
})

test_that("weave: hit nodes get one bow on one side, above by the tie rule", {
  scene <- weave_scene(5)
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)

  expect_true(res$meta$routed[1])
  expect_equal(res$meta$side[1], 1)
  expect_true(res$meta$clearance_ok[1])
  expect_false(res$meta$sagitta_capped[1])
  # above: A needs an offset of 5 + 9 = 14, B only 9 - 5 = 4, and the hull
  # keeps just the higher waypoint, so the route is a single bow
  expect_equal(res$meta$n_waypoints[1], 1)
  wp <- res$waypoints[[1]]
  expect_identical(nrow(wp), 1L)
  expect_equal(wp$x, 160, tolerance = 1e-6)
  expect_equal(wp$y, 14, tolerance = 1e-6)

  expect_exact_endpoints(path, ends$from, ends$to)
  expect_lte(res$meta$sagitta_ratio[1], 0.22)
  expect_gte(path_min_dist(path, node_xy(scene, "A")), r_full - verify_tol)
  expect_gte(path_min_dist(path, node_xy(scene, "B")), r_full - verify_tol)
  # never dips below the chord: no S
  expect_true(all(path$y >= -1e-9))
  expect_true(all(diff(path$x) >= -0.1))
  expect_lt(max(abs(turning_angles(path))), 12)
  expect_lte(count_inflections(path), 2)

  ends2 <- edge_endpoints(scene, 2)
  expect_false(res$meta$routed[2])
  expect_straight_path(res$paths[[2]], ends2$from, ends2$to)
})

# Free-bow tier ------------------------------------------------------------------

# A chord steeper than 60 degrees to the layer axis: slots along y are
# meaningless, so the hit node gets a single bow offset R from its centre.
steep_scene <- function() {
  list(
    nodes = mm_nodes(c("S", "T", "C"), c(0, 20, 10), c(0, 60, 30)),
    edges = mm_edges("S", "T"),
    bounds = c(-20, -10, 40, 70)
  )
}

test_that("free bow: a steep chord through a node gets one bow offset R from the node", {
  scene <- steep_scene()
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)
  centre <- node_xy(scene, "C")

  expect_true(res$meta$routed[1])
  expect_equal(res$meta$mode[1], "bow")
  # no crossings and no congestion: the tie goes to +1
  expect_equal(res$meta$side[1], 1)
  expect_equal(res$meta$n_waypoints[1], 1)
  expect_true(res$meta$clearance_ok[1])
  expect_false(res$meta$sagitta_capped[1])
  # 9 / 63.2
  expect_lte(res$meta$sagitta_ratio[1], 0.22)

  wp <- res$waypoints[[1]]
  expect_identical(nrow(wp), 1L)
  expect_equal(sqrt(sum((c(wp$x, wp$y) - centre)^2)), 9, tolerance = 1e-6)
  # the waypoint sits on the chord normal through C, halfway along the chord
  expect_equal(
    chord_progress(wp, ends$from, ends$to),
    chord_length(ends$from, ends$to) / 2,
    tolerance = 1e-6
  )
  expect_gt(chord_offset(wp, ends$from, ends$to), 0)

  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(path_min_dist(path, centre), r_full - verify_tol)
  expect_lte(
    max(abs(chord_offset(path, ends$from, ends$to))) /
      chord_length(ends$from, ends$to),
    0.22
  )
  expect_true(all(diff(chord_progress(path, ends$from, ends$to)) >= -0.1))
  expect_lt(max(abs(turning_angles(path))), 12)
  expect_lte(count_inflections(path), 2)
})

test_that("free bow: a hit in the source's own layer is routed as a bow", {
  # x -> t spans two layers at 56 degrees; the only hit is u, which shares
  # x's layer and sits 12.5 mm above it, 7 mm from the chord. No crossed
  # slot is blocked, so the spanning tier has nothing to place and the hit
  # is handled by the free-bow tier.
  scene <- list(
    nodes = mm_nodes(
      c("x", "u", "m", "t"),
      c(20, 20, 50, 80),
      c(10, 22.5, 80, 99)
    ),
    edges = mm_edges(c("x", "m"), c("t", "t")),
    bounds = c(0, 0, 160, 110)
  )
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)

  expect_lt(chord_min_clearance(scene, 1), r_soft)
  expect_true(res$meta$routed[1])
  expect_equal(res$meta$mode[1], "bow")
  expect_true(res$meta$clearance_ok[1])
  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(nrow(path), 16)
  expect_gte(path_min_dist(path, node_xy(scene, "u")), r_full - verify_tol)
  expect_gte(path_min_clearance(scene, 1, path), r_full - verify_tol)
  expect_lt(max(abs(turning_angles(path))), 12)
  expect_true(all(diff(chord_progress(path, ends$from, ends$to)) >= -0.1))

  ends2 <- edge_endpoints(scene, 2)
  expect_false(res$meta$routed[2])
  expect_straight_path(res$paths[[2]], ends2$from, ends2$to)
})

test_that("free bow: an oblique hit between layers never returns an unverified chord", {
  # a -> b climbs at 50 degrees across layer 2. Node c sits 10.5 mm above
  # the chord point at its layer, so the slot at the layer coordinate is
  # free, yet its perpendicular distance to the chord is 6.75 mm: a hard
  # hit. The edge must still be routed and verified.
  scene <- list(
    nodes = mm_nodes(c("a", "b", "c"), c(20, 80, 50), c(10, 81.5, 56.25)),
    edges = mm_edges(c("a", "a", "c"), c("b", "c", "b")),
    bounds = c(0, 0, 160, 110)
  )
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)

  expect_lt(chord_min_clearance(scene, 1), r_soft)
  expect_true(res$meta$routed[1])
  expect_equal(res$meta$mode[1], "bow")
  expect_true(res$meta$clearance_ok[1])
  expect_gte(nrow(path), 16)
  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(path_min_dist(path, node_xy(scene, "c")), r_full - verify_tol)
  expect_lt(max(abs(turning_angles(path))), 12)
  expect_true(all(diff(chord_progress(path, ends$from, ends$to)) >= -0.1))

  for (i in 2:3) {
    ends_i <- edge_endpoints(scene, i)
    expect_false(res$meta$routed[i])
    expect_straight_path(res$paths[[i]], ends_i$from, ends_i$to)
  }
})

test_that("a panel with no free slot on either side yields a least-bad bow, not an error", {
  # the panel is a 10 mm band around the chord, so layer 2 has no free
  # interval above or below m
  scene <- mediator_scene()
  scene$bounds <- c(0, 50, 160, 60)
  # the call itself must not error
  res <- route_scene(scene)
  path <- res$paths[[3]]
  ends <- edge_endpoints(scene, 3)

  expect_true(res$meta$routed[3])
  expect_false(res$meta$clearance_ok[3])
  expect_gte(nrow(path), 2)
  expect_exact_endpoints(path, ends$from, ends$to)
  # one side only
  offset <- chord_offset(path, ends$from, ends$to)
  expect_true(all(offset >= -1e-9) || all(offset <= 1e-9))

  for (i in 1:2) {
    ends_i <- edge_endpoints(scene, i)
    expect_false(res$meta$routed[i])
    expect_straight_path(res$paths[[i]], ends_i$from, ends_i$to)
  }
})

# Layer axis and edge direction ------------------------------------------------

test_that("layers along y: the scene is transposed and the result transposed back", {
  ref <- route_scene(mediator_scene())
  rotated <- list(
    nodes = mm_nodes(c("x", "m", "y"), c(55, 55, 55), c(7.3, 80, 152.7)),
    edges = mm_edges(c("x", "m", "x"), c("m", "y", "y")),
    bounds = c(0, 0, 110, 160)
  )
  res <- route_scene(rotated)

  expect_identical(res$meta$routed, ref$meta$routed)
  expect_identical(res$meta$mode, ref$meta$mode)
  # side is reported in the canonical orientation
  expect_identical(res$meta$side, ref$meta$side)
  expect_identical(res$meta$waypoint_layers, ref$meta$waypoint_layers)

  for (i in 1:3) {
    expect_equal(res$paths[[i]]$x, ref$paths[[i]]$y, tolerance = 1e-9)
    expect_equal(res$paths[[i]]$y, ref$paths[[i]]$x, tolerance = 1e-9)
    ends <- edge_endpoints(rotated, i)
    expect_exact_endpoints(res$paths[[i]], ends$from, ends$to)
  }
  wp <- res$waypoints[[3]]
  expect_equal(wp$x, 64)
  expect_equal(wp$y, 80)
  expect_equal(wp$layer, 2)
})

test_that("exact x layers keep x as the layer axis even when y has fewer distinct rows", {
  # four exact x layers, two y rows; the layered axis is x
  scene <- list(
    nodes = mm_nodes(
      c("a", "b", "e", "c", "d"),
      c(20, 60, 60, 100, 140),
      c(40, 70, 40, 40, 70)
    ),
    edges = mm_edges(
      c("a", "a", "e", "b", "c", "a"),
      c("b", "e", "c", "c", "d", "c")
    ),
    bounds = c(0, 0, 160, 110)
  )
  res <- route_scene(scene)

  for (i in 1:5) {
    ends <- edge_endpoints(scene, i)
    expect_false(res$meta$routed[i])
    expect_straight_path(res$paths[[i]], ends$from, ends$to)
  }

  expect_true(res$meta$routed[6])
  expect_equal(res$meta$mode[6], "interior")
  # b and d lie above the chord at a and c, so the bow goes below
  expect_equal(res$meta$side[6], -1)
  expect_equal(res$meta$waypoint_layers[[6]], 2)
  wp <- res$waypoints[[6]]
  expect_identical(nrow(wp), 1L)
  # the waypoint sits at layer 2's x, displaced along y
  expect_equal(wp$x, 60)
  expect_equal(wp$y, 31)
  path <- res$paths[[6]]
  expect_true(all(path$y <= 40 + 1e-9))
  expect_gte(path_min_dist(path, node_xy(scene, "e")), r_full - verify_tol)
})

test_that("a reversed edge returns the reversed polyline of its forward twin", {
  forward <- route_scene(mediator_scene())
  scene <- mediator_scene()
  scene$edges <- mm_edges(c("x", "m", "y"), c("m", "y", "x"))
  res <- route_scene(scene)
  ends <- edge_endpoints(scene, 3)

  expect_true(res$meta$routed[3])
  expect_identical(res$meta$side[3], forward$meta$side[3])
  expect_identical(res$meta$mode[3], forward$meta$mode[3])
  expect_identical(res$waypoints[[3]], forward$waypoints[[3]])
  expect_identical(res$paths[[3]]$x, rev(forward$paths[[3]]$x))
  expect_identical(res$paths[[3]]$y, rev(forward$paths[[3]]$y))
  expect_exact_endpoints(res$paths[[3]], ends$from, ends$to)
})

# Fixed paths and parallel edges ---------------------------------------------------

# The mediator plus a fixed edge p -> q whose pre-sampled arc dips to y = 60
# over m: it crosses the above candidate for x -> y but not the one below.
fixed_arc_scene <- function(fixed = TRUE) {
  arc_x <- seq(40, 120, length.out = 81)
  arc <- pt(arc_x, 60 + 30 * ((arc_x - 80) / 40)^2)
  edges <- mm_edges(c("x", "m", "x", "p"), c("m", "y", "y", "q"))
  if (fixed) {
    edges$curvature[4] <- 0.3
    edges$fixed_path <- vector("list", 4)
    edges$fixed_path[[4]] <- arc
  }
  list(
    nodes = mm_nodes(
      c("x", "m", "y", "p", "q"),
      c(7.3, 80, 152.7, 40, 120),
      c(55, 55, 55, 90, 90)
    ),
    edges = edges,
    bounds = c(0, 0, 160, 110),
    arc = arc
  )
}

test_that("a fixed_path edge is returned verbatim and counts as a placed polyline", {
  scene <- fixed_arc_scene(fixed = TRUE)
  res <- route_scene(scene)

  expect_false(res$meta$routed[4])
  expect_equal(res$meta$mode[4], "fixed")
  expect_equal(res$meta$n_waypoints[4], 0)
  expect_identical(res$paths[[4]]$x, scene$arc$x)
  expect_identical(res$paths[[4]]$y, scene$arc$y)

  # the arc crosses the above candidate, so x -> y bows below
  expect_true(res$meta$routed[3])
  expect_equal(res$meta$side[3], -1)
  expect_equal(res$waypoints[[3]]$y, 46)

  # without the fixed arc the same scene ties and goes above
  control <- route_scene(fixed_arc_scene(fixed = FALSE))
  expect_false(control$meta$routed[4])
  expect_equal(control$meta$side[3], 1)
  expect_equal(control$waypoints[[3]]$y, 64)
})

test_that("parallel routed edges are spread sep_m apart with fixed endpoints", {
  scene <- steep_scene()
  scene$edges <- mm_edges(c("S", "T"), c("T", "S"))
  res <- route_scene(scene)
  from <- node_xy(scene, "S")
  to <- node_xy(scene, "T")

  expect_true(all(res$meta$routed))
  for (i in 1:2) {
    ends <- edge_endpoints(scene, i)
    expect_gte(nrow(res$paths[[i]]), 16)
    expect_exact_endpoints(res$paths[[i]], ends$from, ends$to)
  }
  # both bows sit on the same side; their apexes differ by sep_m = 6 mm
  apex <- vapply(
    res$paths,
    function(path) max(chord_offset(path, from, to)),
    numeric(1)
  )
  expect_equal(abs(diff(apex)), 6, tolerance = 0.05)
  expect_true(all(apex > 0))
})

# Straight mode and user curvature ---------------------------------------------

test_that("mode = 'straight' draws every edge as a chord regardless of obstacles", {
  scene <- mediator_scene()
  res <- route_scene(scene, mode = "straight")

  expect_false(any(res$meta$routed))
  for (i in 1:3) {
    ends <- edge_endpoints(scene, i)
    expect_straight_path(res$paths[[i]], ends$from, ends$to)
    expect_identical(nrow(res$waypoints[[i]]), 0L)
  }
})

test_that("user-set curvature is passed through and never rerouted", {
  scene <- mediator_scene()
  scene$edges$curvature[3] <- 0.4
  res <- route_scene(scene)

  expect_false(res$meta$routed[3])
  expect_equal(res$meta$mode[3], "fixed")
  expect_equal(res$meta$n_waypoints[3], 0)
  expect_identical(nrow(res$waypoints[[3]]), 0L)

  # the other edges are unaffected
  for (i in 1:2) {
    ends <- edge_endpoints(scene, i)
    expect_false(res$meta$routed[i])
    expect_straight_path(res$paths[[i]], ends$from, ends$to)
  }
})

test_that("curvature 0 forces a straight chord even through a node", {
  scene <- mediator_scene()
  scene$edges$curvature[3] <- 0
  res <- route_scene(scene)
  ends <- edge_endpoints(scene, 3)

  expect_false(res$meta$routed[3])
  expect_straight_path(res$paths[[3]], ends$from, ends$to)
})

# Fixture 5: canonical DAGs -----------------------------------------------------

# At 160 x 110 the time-ordered layout leaves every canonical chord at least
# 9.6 mm from any non-endpoint node, so nothing routes. At 100 x 70 (a small
# device) large_epi has four hard-blocked chords and treatment two grazed
# ones, so the sweep exercises both the spanning tier and the soft band.
canonical_panels <- list(c(160, 110), c(100, 70))

canonical_label <- function(scene, i, panel) {
  sprintf(
    "%s %s at %d x %d",
    scene$name,
    edge_labels(scene$edges)[i],
    panel[1],
    panel[2]
  )
}

test_that("canonical DAGs: unblocked chords are straight, hit chords are routed, endpoints exact", {
  for (panel in canonical_panels) {
    for (nm in names(canonical_dag_specs)) {
      scene <- canonical_scene(nm, panel)
      res <- route_scene(scene)
      expect_length(res$paths, nrow(scene$edges))

      for (i in seq_len(nrow(scene$edges))) {
        label <- canonical_label(scene, i, panel)
        ends <- edge_endpoints(scene, i)
        path <- res$paths[[i]]
        d <- chord_min_clearance(scene, i)

        expect_exact_endpoints(path, ends$from, ends$to)
        if (d >= r_full) {
          expect_false(res$meta$routed[i], label = label)
          expect_identical(nrow(path), 2L, label = label)
        }
        if (d < r_soft) {
          expect_true(res$meta$routed[i], label = label)
        }
      }
    }
  }
})

test_that("canonical DAGs: routed edges clear obstacles, turn gently, and progress monotonically", {
  for (panel in canonical_panels) {
    for (nm in names(canonical_dag_specs)) {
      scene <- canonical_scene(nm, panel)
      res <- route_scene(scene)

      for (i in which(res$meta$routed)) {
        label <- canonical_label(scene, i, panel)
        ends <- edge_endpoints(scene, i)
        path <- res$paths[[i]]

        # soft nudges and sagitta-capped bows are verified at the soft radius
        full <- res$meta$clearance_ok[i] &&
          res$meta$mode[i] != "soft" &&
          !res$meta$sagitta_capped[i]
        floor <- if (full) {
          r_full - verify_tol
        } else {
          r_soft - verify_tol
        }
        expect_gte(path_min_clearance(scene, i, path), floor, label = label)
        expect_lt(max(abs(turning_angles(path))), 12, label = label)
        expect_gte(nrow(path), 16, label = label)

        if (canonical_span(scene, i) >= 2) {
          progress <- chord_progress(path, ends$from, ends$to)
          expect_true(all(diff(progress) >= -0.1), label = label)
        }
      }
    }
  }
})

test_that("canonical DAGs: routing is deterministic and invariant to row order", {
  for (panel in canonical_panels) {
    for (nm in names(canonical_dag_specs)) {
      scene <- canonical_scene(nm, panel)
      res <- route_scene(scene)
      expect_identical(route_scene(scene), res)

      # reverse the node rows and rotate the edge rows
      n_edges <- nrow(scene$edges)
      edge_perm <- c(seq_len(n_edges)[-1], 1L)
      shuffled <- scene
      shuffled$nodes <- scene$nodes[rev(seq_len(nrow(scene$nodes))), ]
      shuffled$edges <- scene$edges[edge_perm, ]
      rownames(shuffled$nodes) <- NULL
      rownames(shuffled$edges) <- NULL
      res2 <- route_scene(shuffled)

      keys <- edge_labels(scene$edges)
      keys2 <- edge_labels(shuffled$edges)
      for (i in seq_len(n_edges)) {
        j <- match(keys[i], keys2)
        label <- canonical_label(scene, i, panel)
        expect_identical(res2$paths[[j]], res$paths[[i]], label = label)
        expect_identical(res2$waypoints[[j]], res$waypoints[[i]], label = label)
        expect_identical(res2$meta$side[j], res$meta$side[i], label = label)
        expect_identical(res2$meta$mode[j], res$meta$mode[i], label = label)
      }
    }
  }
})

test_that("canonical DAGs: the sweep exercises routing on at least one DAG", {
  routed_dags <- character(0)
  for (panel in canonical_panels) {
    for (nm in names(canonical_dag_specs)) {
      res <- route_scene(canonical_scene(nm, panel))
      if (any(res$meta$routed)) {
        routed_dags <- c(routed_dags, nm)
      }
    }
  }
  # expected: large_epi and treatment at 100 x 70; none at 160 x 110
  expect_gte(length(unique(routed_dags)), 1)
  expect_true("large_epi" %in% routed_dags)
})

# Fixture 6: scale invariance --------------------------------------------------

test_that("scale invariance: mediator and fan routes scale exactly with k", {
  for (make in list(mediator_scene, fan_scene)) {
    base <- make()
    ref <- route_scene(base, cap = 8)

    for (k in c(0.5, 1, 3)) {
      res <- route_scene(scale_scene(base, k), cap = 8 * k)
      expect_identical(res$meta$routed, ref$meta$routed)
      expect_identical(res$meta$side, ref$meta$side)
      expect_identical(res$meta$mode, ref$meta$mode)

      for (i in seq_along(ref$paths)) {
        expect_equal(
          res$waypoints[[i]]$x / k,
          ref$waypoints[[i]]$x,
          tolerance = 1e-9
        )
        expect_equal(
          res$waypoints[[i]]$y / k,
          ref$waypoints[[i]]$y,
          tolerance = 1e-9
        )
        expect_equal(res$waypoints[[i]]$layer, ref$waypoints[[i]]$layer)
        scaled <- pt(res$paths[[i]]$x / k, res$paths[[i]]$y / k)
        expect_lt(polyline_hausdorff(scaled, ref$paths[[i]]), 0.05)
      }
    }
  }
})

test_that("scale invariance: the four-layer arch keeps its side, mode, and layers under scaling", {
  # The verification tolerance is a fixed 0.1 mm, so a route that the repair
  # step adjusts is not bit-for-bit scale invariant; the discrete decisions
  # are, and the paths agree to within half a millimetre.
  base <- four_layer_scene()
  ref <- route_scene(base, cap = 8)

  for (k in c(0.5, 1, 3)) {
    res <- route_scene(scale_scene(base, k), cap = 8 * k)
    expect_identical(res$meta$routed, ref$meta$routed)
    expect_identical(res$meta$side, ref$meta$side)
    expect_identical(res$meta$mode, ref$meta$mode)
    expect_identical(res$meta$n_waypoints, ref$meta$n_waypoints)
    expect_identical(res$meta$waypoint_layers, ref$meta$waypoint_layers)

    for (i in seq_along(ref$paths)) {
      expect_equal(res$waypoints[[i]]$layer, ref$waypoints[[i]]$layer)
      scaled <- pt(res$paths[[i]]$x / k, res$paths[[i]]$y / k)
      expect_lt(polyline_hausdorff(scaled, ref$paths[[i]]), 0.5)
    }
  }
})

# Fixture 7: device-size stability ---------------------------------------------

test_that("device size: routing decisions do not change with the panel size", {
  panels <- list(c(100, 70), c(160, 110), c(240, 160))

  for (make in list(mediator_scene, fan_scene, four_layer_scene)) {
    ref <- route_scene(make(panel = c(160, 110)))
    for (panel in panels) {
      res <- route_scene(make(panel = panel))
      expect_identical(res$meta$routed, ref$meta$routed)
      expect_identical(res$meta$side, ref$meta$side)
      expect_identical(res$meta$mode, ref$meta$mode)
      expect_identical(res$meta$waypoint_layers, ref$meta$waypoint_layers)
    }
  }
})

# Fixture 8: threshold continuity ------------------------------------------------

test_that("continuity: sliding m through the obstruction threshold never pops the path", {
  # m moves from 9.5 mm above the chord (clear) to 8.5 mm (soft band) in
  # 0.1 mm steps; consecutive x->y paths must stay within 0.6 mm
  offsets <- seq(9.5, 8.5, by = -0.1)
  paths <- lapply(offsets, function(d) {
    route_scene(mediator_scene(m_y = 55 + d))$paths[[3]]
  })

  for (k in seq_len(length(paths) - 1)) {
    expect_lt(polyline_hausdorff(paths[[k]], paths[[k + 1]]), 0.6)
  }
  # clear at 9.5, still straight at exactly 9, nudged inside the band
  expect_identical(nrow(paths[[1]]), 2L)
  expect_identical(nrow(paths[[6]]), 2L)
  expect_gt(nrow(paths[[length(paths)]]), 2L)
})

# Helper: infer_layers -----------------------------------------------------------

test_that("infer_layers() clusters x within one radius", {
  # 5 mm apart is one layer, 40 mm is another
  nodes <- mm_nodes(c("a", "b", "c"), c(0, 5, 40), c(10, 30, 20))
  layers <- infer_layers(nodes, tol = 6)
  expect_type(layers, "list")
  expect_equal(layers$n, 2)
  expect_equal(layers$id, c(1, 1, 2))
  expect_equal(layers$x, c(2.5, 40))
  expect_equal(unname(layers$members), list(c(1L, 2L), 3L))

  # 7 mm apart is two layers
  nodes7 <- mm_nodes(c("a", "b", "c"), c(0, 7, 40), c(10, 30, 20))
  layers7 <- infer_layers(nodes7, tol = 6)
  expect_equal(layers7$n, 3)
  expect_equal(layers7$id, c(1, 2, 3))
  expect_equal(layers7$x, c(0, 7, 40))
})

test_that("infer_layers() chains consecutive gaps and treats a gap of exactly tol as one layer", {
  chain <- infer_layers(mm_nodes(c("a", "b", "c"), c(0, 5, 10), 0), tol = 6)
  expect_equal(chain$n, 1)
  expect_equal(chain$id, c(1, 1, 1))
  expect_equal(chain$x, 5)

  edge <- infer_layers(mm_nodes(c("a", "b"), c(0, 6), 0), tol = 6)
  expect_equal(edge$n, 1)
})

test_that("infer_layers() does not depend on node row order", {
  nodes <- mm_nodes(c("c", "a", "b"), c(40, 0, 5), c(20, 10, 30))
  layers <- infer_layers(nodes, tol = 6)
  expect_equal(layers$id, c(2, 1, 1))
  expect_equal(layers$x, c(2.5, 40))
  expect_equal(unname(layers$members), list(c(2L, 3L), 1L))
})

# Helper: find_blocked_edges ---------------------------------------------------

test_that("find_blocked_edges() classifies hits as hard or soft with signed offsets", {
  nodes <- mm_nodes(
    c("s", "t", "hard", "soft", "clear", "rim"),
    c(0, 100, 30, 60, 80, 50),
    c(0, 0, 5, -8, 12, -9)
  )
  edges <- mm_edges(c("s", "s"), c("t", "clear"))
  hits <- find_blocked_edges(nodes, edges, R_soft = r_soft, R = r_full)

  expect_s3_class(hits, "data.frame")
  expect_named(hits, c("edge", "node", "d", "h", "t", "severity"))

  # edge 1 (s->t along y = 0): `hard` sits 5 above, `soft` 8 below, `rim`
  # exactly 9 below (not a hit), `clear` 12 above; endpoints are excluded.
  # edge 2 (s->clear): only `hard` is within R of the chord.
  chord2 <- c(80, 12)
  len2 <- sqrt(sum(chord2^2))
  h2 <- (chord2[1] * 5 - chord2[2] * 30) / len2
  t2 <- (30 * chord2[1] + 5 * chord2[2]) / len2^2

  expect_identical(nrow(hits), 3L)
  expect_equal(hits$edge, c(1, 1, 2))
  expect_equal(hits$node, c("hard", "soft", "hard"))
  expect_equal(hits$d, c(5, 8, abs(h2)), tolerance = 1e-9)
  expect_equal(hits$h, c(5, -8, h2), tolerance = 1e-9)
  expect_equal(hits$t, c(0.3, 0.6, t2), tolerance = 1e-9)
  expect_equal(hits$severity, c("hard", "soft", "hard"))
})

test_that("find_blocked_edges() returns no rows when nothing is within R", {
  scene <- fan_scene()
  clear_edges <- scene$edges[1:5, ]
  hits <- find_blocked_edges(
    scene$nodes,
    clear_edges,
    R_soft = r_soft,
    R = r_full
  )
  expect_s3_class(hits, "data.frame")
  expect_identical(nrow(hits), 0L)
  expect_named(hits, c("edge", "node", "d", "h", "t", "severity"))
})

# Helper: hull_waypoints -------------------------------------------------------

test_that("hull_waypoints() drops a waypoint that dips toward the chord", {
  from <- pt(0, 0)
  to <- pt(100, 0)
  wp <- data.frame(x = c(25, 50, 75), y = c(20, 10, 20), layer = 1:3)

  above <- hull_waypoints(from, wp, to, side = 1)
  expect_s3_class(above, "data.frame")
  expect_equal(above$x, c(25, 75))
  expect_equal(above$y, c(20, 20))
  expect_equal(above$layer, c(1, 3))

  # every waypoint lies above the chord, so the lower hull is the chord
  below <- hull_waypoints(from, wp, to, side = -1)
  expect_identical(nrow(below), 0L)
})

test_that("hull_waypoints() keeps a monotone arch and removes chord points", {
  from <- pt(0, 0)
  to <- pt(100, 0)

  arch <- data.frame(x = c(25, 50, 75), y = c(20, 30, 20), layer = 1:3)
  expect_equal(hull_waypoints(from, arch, to, side = 1)$y, c(20, 30, 20))

  dip <- data.frame(x = c(25, 50, 75), y = c(-20, -30, -20), layer = 1:3)
  expect_equal(hull_waypoints(from, dip, to, side = -1)$y, c(-20, -30, -20))

  # a waypoint on the chord adds nothing to either hull
  flat <- data.frame(x = c(50, 75), y = c(0, 20), layer = 2:3)
  expect_equal(hull_waypoints(from, flat, to, side = 1)$x, 75)
  on_chord <- data.frame(x = 50, y = 0, layer = 2)
  expect_identical(nrow(hull_waypoints(from, on_chord, to, side = 1)), 0L)
  expect_identical(nrow(hull_waypoints(from, on_chord, to, side = -1)), 0L)

  # a single waypoint is kept
  one <- data.frame(x = 80, y = 9, layer = 2)
  expect_equal(hull_waypoints(from, one, to, side = 1), one, ignore_attr = TRUE)
})

test_that("hull_waypoints() returns waypoints ordered along the chord", {
  from <- pt(0, 0)
  to <- pt(100, 0)
  wp <- data.frame(x = c(75, 25), y = c(20, 20), layer = c(3, 1))
  res <- hull_waypoints(from, wp, to, side = 1)
  expect_equal(res$x, c(25, 75))
  expect_equal(res$layer, c(1, 3))
})

# Helper: catmull_rom_beziers --------------------------------------------------

test_that("catmull_rom_beziers() reproduces the centripetal control points", {
  P <- rbind(c(0, 0), c(10, 10), c(20, 10), c(30, 0))
  deg <- pi / 180
  d_start <- c(cos(40 * deg), sin(40 * deg))
  d_end <- c(cos(40 * deg), -sin(40 * deg))
  B <- catmull_rom_beziers(P, alpha = 0.5, d_start, d_end, arm_min = 14)

  expect_type(B, "list")
  expect_length(B, 3)
  for (seg in B) {
    expect_true(is.matrix(seg))
    expect_identical(dim(seg), c(4L, 2L))
  }
  # each segment starts and ends on the input points
  for (i in 1:3) {
    expect_equal(B[[i]][1, ], P[i, ])
    expect_equal(B[[i]][4, ], P[i + 1, ])
  }

  # Centripetal knots: t_{i+1} - t_i = |P_{i+1} - P_i|^0.5. Duplicating the
  # end points for the padded neighbours adds nothing to the knot vector.
  dt <- sqrt(sqrt(rowSums(diff(P)^2)))
  t <- cumsum(c(0, dt))
  # tangent directions at the interior points (Yuksel et al.)
  D2 <- (P[2, ] - P[1, ]) /
    (t[2] - t[1]) -
    (P[3, ] - P[1, ]) / (t[3] - t[1]) +
    (P[3, ] - P[2, ]) / (t[3] - t[2])
  D3 <- (P[3, ] - P[2, ]) /
    (t[3] - t[2]) -
    (P[4, ] - P[2, ]) / (t[4] - t[2]) +
    (P[4, ] - P[3, ]) / (t[4] - t[3])
  # each segment scales the tangents at both ends by its own knot spacing
  expect_equal(B[[1]][3, ], P[2, ] - dt[1] * D2 / 3, tolerance = 1e-6)
  expect_equal(B[[2]][2, ], P[2, ] + dt[2] * D2 / 3, tolerance = 1e-6)
  expect_equal(B[[2]][3, ], P[3, ] - dt[2] * D3 / 3, tolerance = 1e-6)
  expect_equal(B[[3]][2, ], P[3, ] + dt[3] * D3 / 3, tolerance = 1e-6)

  # End arms: the duplicated end knot gives a vanishing natural tangent, so
  # the arm is min(arm_min, 0.45 * first segment) along the clamped direction.
  arm <- min(14, 0.45 * sqrt(sum((P[2, ] - P[1, ])^2)))
  expect_equal(B[[1]][2, ], P[1, ] + arm * d_start, tolerance = 1e-6)
  expect_equal(B[[3]][3, ], P[4, ] - arm * d_end, tolerance = 1e-6)
  # numerically: (4.8751, 4.0907), (6.3241, 8.4774), (13.0911, 11.2804)
  expect_equal(B[[1]][2, ], c(4.8751, 4.0907), tolerance = 1e-4)
  expect_equal(B[[1]][3, ], c(6.3241, 8.4774), tolerance = 1e-4)
  expect_equal(B[[2]][2, ], c(13.0911, 11.2804), tolerance = 1e-4)
  expect_equal(B[[2]][3, ], c(16.9089, 11.2804), tolerance = 1e-4)
  expect_equal(B[[3]][2, ], c(23.6759, 8.4774), tolerance = 1e-4)
  expect_equal(B[[3]][3, ], c(25.1249, 4.0907), tolerance = 1e-4)
})

test_that("catmull_rom_beziers() end arms reach arm_min on long first and last segments", {
  P <- rbind(c(0, 0), c(40, 10), c(80, 10), c(120, 0))
  d_start <- c(40, 10) / sqrt(1700)
  d_end <- c(40, -10) / sqrt(1700)
  B <- catmull_rom_beziers(P, alpha = 0.5, d_start, d_end, arm_min = 14)

  # 0.45 * 41.2 mm exceeds arm_min, so the arm is exactly 14 mm along d_start
  expect_equal(sqrt(sum((B[[1]][2, ] - P[1, ])^2)), 14, tolerance = 1e-6)
  expect_equal(B[[1]][2, ], P[1, ] + 14 * d_start, tolerance = 1e-6)
  expect_equal(sqrt(sum((P[4, ] - B[[3]][3, ])^2)), 14, tolerance = 1e-6)
  expect_equal(B[[3]][3, ], P[4, ] - 14 * d_end, tolerance = 1e-6)
})

# Helper: clamp_direction -------------------------------------------------------

test_that("clamp_direction() leaves directions within the cone alone and rotates others to its edge", {
  deg <- pi / 180
  ref <- c(1, 0)

  inside <- clamp_direction(2 * c(cos(30 * deg), sin(30 * deg)), ref)
  expect_equal(inside, c(cos(30 * deg), sin(30 * deg)), tolerance = 1e-12)

  above <- clamp_direction(c(cos(60 * deg), sin(60 * deg)), ref)
  expect_equal(above, c(cos(40 * deg), sin(40 * deg)), tolerance = 1e-12)

  below <- clamp_direction(c(cos(60 * deg), -sin(60 * deg)), ref)
  expect_equal(below, c(cos(40 * deg), -sin(40 * deg)), tolerance = 1e-12)

  # explicit cone width
  narrow <- clamp_direction(c(cos(30 * deg), sin(30 * deg)), ref, max_deg = 10)
  expect_equal(narrow, c(cos(10 * deg), sin(10 * deg)), tolerance = 1e-12)

  # the reference need not be axis-aligned
  tilted <- clamp_direction(
    c(cos(150 * deg), sin(150 * deg)),
    c(0, 1)
  )
  expect_equal(tilted, c(cos(130 * deg), sin(130 * deg)), tolerance = 1e-12)

  # output is always a unit vector
  expect_equal(sqrt(sum(inside^2)), 1, tolerance = 1e-12)
  expect_equal(sqrt(sum(above^2)), 1, tolerance = 1e-12)
})

# Helper: sample_beziers -------------------------------------------------------

test_that("sample_beziers() returns at least 16 points per segment and hits the ends exactly", {
  short <- list(rbind(c(0, 0), c(0.5, 0.2), c(1.5, 0.2), c(2, 0)))
  pts <- sample_beziers(short)
  expect_s3_class(pts, "data.frame")
  expect_named(pts, c("x", "y"))
  expect_identical(nrow(pts), 16L)
  expect_identical(pts$x[1], 0)
  expect_identical(pts$y[1], 0)
  expect_identical(pts$x[16], 2)
  expect_identical(pts$y[16], 0)
})

test_that("sample_beziers() samples at 0.5 mm along the control polygon", {
  long <- list(rbind(c(0, 0), c(20, 0), c(40, 0), c(60, 0)))
  pts <- sample_beziers(long, spacing = 0.5, min_n = 16)
  expect_identical(nrow(pts), 120L)
  step <- sqrt(diff(pts$x)^2 + diff(pts$y)^2)
  expect_lt(max(step), 0.51)
  expect_identical(pts$x[nrow(pts)], 60)

  coarse <- sample_beziers(long, spacing = 5, min_n = 4)
  expect_identical(nrow(coarse), 12L)
})

test_that("sample_beziers() drops the duplicated join between segments", {
  B <- list(
    rbind(c(0, 0), c(2, 1), c(4, 1), c(6, 0)),
    rbind(c(6, 0), c(8, -1), c(10, -1), c(12, 0))
  )
  pts <- sample_beziers(B)
  expect_identical(nrow(pts), 31L)
  expect_identical(sum(pts$x == 6 & pts$y == 0), 1L)
  expect_identical(pts$x[16], 6)
  expect_identical(pts$x[31], 12)
  expect_identical(pts$y[31], 0)
})

# Helper: verify_clearance ------------------------------------------------------

test_that("verify_clearance() reports the depth of a sample inside R and nothing otherwise", {
  samples <- pt(0:10, rep(0, 11))
  obstacles <- pt(c(5.2, 5), c(3, 20))

  viol <- verify_clearance(samples, obstacles, R_vec = c(9, 9), tol = 0.1)
  expect_s3_class(viol, "data.frame")
  expect_identical(nrow(viol), 1L)
  expect_equal(viol$obstacle, 1)
  # nearest sample is (5, 0) at distance sqrt(0.04 + 9)
  expect_equal(viol$sample, 6)
  expect_equal(viol$depth, 9 - 0.1 - sqrt(0.2^2 + 3^2), tolerance = 1e-9)

  none <- verify_clearance(samples, pt(5, 20), R_vec = 9, tol = 0.1)
  expect_s3_class(none, "data.frame")
  expect_identical(nrow(none), 0L)
})

test_that("verify_clearance() honours the tolerance and per-obstacle radii", {
  samples <- pt(0:10, rep(0, 11))

  # exactly R - tol away is not a violation
  edge <- verify_clearance(samples, pt(5, 8.9), R_vec = 9, tol = 0.1)
  expect_identical(nrow(edge), 0L)
  inside <- verify_clearance(samples, pt(5, 8.8), R_vec = 9, tol = 0.1)
  expect_identical(nrow(inside), 1L)
  expect_equal(inside$depth, 0.1, tolerance = 1e-9)

  # the same obstacle with the soft radius is clear
  soft <- verify_clearance(samples, pt(5, 8), R_vec = 7.2, tol = 0.1)
  expect_identical(nrow(soft), 0L)

  # tol defaults to 0.1
  default <- verify_clearance(samples, pt(5, 8.95), R_vec = 9)
  expect_identical(nrow(default), 0L)
})

# Purity -------------------------------------------------------------------------

test_that("route_edges_mm() never touches the RNG and repeats identically", {
  withr::local_preserve_seed()
  scene <- fan_scene()

  if (exists(".Random.seed", envir = globalenv())) {
    rm(list = ".Random.seed", envir = globalenv())
  }
  first <- route_scene(scene)
  expect_false(exists(".Random.seed", envir = globalenv()))

  set.seed(20240601)
  seed <- get(".Random.seed", envir = globalenv())
  second <- route_scene(scene)
  expect_identical(get(".Random.seed", envir = globalenv()), seed)

  expect_identical(first, second)
})
