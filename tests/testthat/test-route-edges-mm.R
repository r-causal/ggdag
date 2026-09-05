# Tests for route_edges_mm(), the millimetre-space edge router, and its
# helpers. The router works entirely in millimetres at draw time: nodes are
# discs of known radius, layers are inferred from the layer-axis coordinate,
# a blocked edge gets one waypoint per crossed layer (or a single bow around
# the obstacle), the waypoint chain is reduced to one arch by a convex hull,
# and the result is drawn as a centripetal Catmull-Rom spline with end
# tangents clamped toward the chord. Orthogonal mode instead draws every edge
# as axis-aligned runs: E/W ports with one vertical slot per crossed gap, or
# S/N ports with a channel past a stack, with corners rounded by a quadratic
# Bezier. Every expectation is a numeric predicate on the returned geometry;
# there are no snapshots.
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

# Orthogonal constants at r = 6: the corner radius rc = clamp(0.35 r, 0.8,
# 2.5) = 2.1, the default edge cap of 8 mm, the stub r + cap + rc = 16.1 that
# keeps the resected arrowhead on a straight run, and the slot separation
# sep_e = max(0.6 r, 1.5) = 3.6.
cap_default <- 8
rc_default <- 2.1
stub_default <- r_default + cap_default + rc_default
sep_e_default <- 3.6

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

# Orthogonal geometry ----------------------------------------------------------

# Drop consecutive duplicate points so that zero-length segments do not
# break the run detection below.
dedupe_path <- function(path, tol = 1e-9) {
  keep <- c(TRUE, abs(diff(path$x)) >= tol | abs(diff(path$y)) >= tol)
  path[keep, , drop = FALSE]
}

# Distance from each point of `pts` to the nearest segment of `poly`.
point_polyline_dist <- function(pts, poly) {
  n <- nrow(poly)
  if (n == 1) {
    return(sqrt((pts$x - poly$x)^2 + (pts$y - poly$y)^2))
  }
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
  apply(d, 1, min)
}

# Axis of each segment: "h" horizontal, "v" vertical, "o" oblique.
segment_axes <- function(path, tol = 1e-6) {
  dx <- diff(path$x)
  dy <- diff(path$y)
  ifelse(abs(dy) < tol, "h", ifelse(abs(dx) < tol, "v", "o"))
}

# Maximal runs of consecutive axis-aligned segments along one axis. `from`
# and `to` index the deduplicated path, `length` is the run's arc length,
# `coord` its constant coordinate (y of a horizontal run, x of a vertical
# one), and `lo`, `hi` bound its varying coordinate.
straight_runs <- function(path, tol = 1e-6) {
  path <- dedupe_path(path)
  axis <- segment_axes(path, tol)
  seg_len <- sqrt(diff(path$x)^2 + diff(path$y)^2)
  r <- rle(axis)
  end <- cumsum(r$lengths)
  start <- end - r$lengths + 1L
  keep <- which(r$values != "o")
  runs <- lapply(keep, function(k) {
    idx <- start[k]:(end[k] + 1L)
    horizontal <- r$values[k] == "h"
    varying <- if (horizontal) path$x[idx] else path$y[idx]
    data.frame(
      axis = r$values[k],
      from = start[k],
      to = end[k] + 1L,
      length = sum(seg_len[start[k]:end[k]]),
      coord = if (horizontal) path$y[start[k]] else path$x[start[k]],
      lo = min(varying),
      hi = max(varying),
      stringsAsFactors = FALSE
    )
  })
  if (length(runs) == 0) {
    return(data.frame(
      axis = character(),
      from = integer(),
      to = integer(),
      length = numeric(),
      coord = numeric(),
      lo = numeric(),
      hi = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, runs)
}

# Points of a sampled path that lie inside a rounded corner, that is within
# `radius` of one of the bend points. The quadratic Bezier through P, B, Q
# stays inside the triangle P B Q, so every corner sample is within rr <= rc
# of its bend B.
corner_points <- function(path, bends, radius) {
  if (nrow(bends) == 0) {
    return(rep(FALSE, nrow(path)))
  }
  d <- sqrt(outer(path$x, bends$x, "-")^2 + outer(path$y, bends$y, "-")^2)
  apply(d, 1, min) <= radius
}

# Distinct x values of the vertical runs of a path strictly inside the gap
# (x_left, x_right). A vertical run within sep_e of a layer x is a port
# stub, not a slot: an S/N port sits on the layer x, or sep_e / 2 beside it
# when a node's arrivals and departures share a side, and a slot never comes
# closer than the stub to a layer.
slot_xs <- function(path, gap, tol = 1e-6) {
  runs <- straight_runs(path, tol)
  inside <- runs$axis == "v" &
    runs$coord > gap[1] + sep_e_default &
    runs$coord < gap[2] - sep_e_default
  unique(round(runs$coord[inside], 9))
}

# Points of a path within `radius` of either endpoint centre.
hidden_points <- function(path, ends, radius) {
  d_from <- sqrt((path$x - ends$from[1])^2 + (path$y - ends$from[2])^2)
  d_to <- sqrt((path$x - ends$to[1])^2 + (path$y - ends$to[2])^2)
  pmin(d_from, d_to) <= radius
}

# Every segment whose endpoints both lie outside the rounded corners is
# axis-aligned. When `ends` is given, segments hidden inside an endpoint
# (both ends within the cap of its centre) are exempt as well: the arrow
# layer resects the cap from each end, so the connector from an offset port
# to the node centre is never drawn.
expect_orthogonal_outside_corners <- function(
  path,
  bends,
  rc,
  tol = 1e-6,
  label = NULL,
  ends = NULL
) {
  path <- dedupe_path(path)
  near <- corner_points(path, bends, rc + tol)
  if (!is.null(ends)) {
    near <- near | hidden_points(path, ends, cap_default + tol)
  }
  axis <- segment_axes(path, tol)
  outside <- !near[-length(near)] & !near[-1]
  expect_true(all(axis[outside] != "o"), label = label)
}

rev_path <- function(path) {
  path[rev(seq_len(nrow(path))), , drop = FALSE]
}

# Arc length of the terminal stub at the end of a path: from the endpoint,
# a node centre, back over any segments hidden inside the node (both ends
# within the cap of the centre) and then along the last axis-aligned run up
# to its first corner. After the cap is resected the arrowhead sits on this
# stub, which is why it must be straight for at least cap + rc. Pass the
# reversed path for the stub at the start.
end_stub_length <- function(path, centre, tol = 1e-6) {
  path <- dedupe_path(path)
  n <- nrow(path)
  if (n < 2) {
    return(0)
  }
  inside <- sqrt((path$x - centre[1])^2 + (path$y - centre[2])^2) <=
    cap_default + tol
  axis <- segment_axes(path, tol)
  len <- sqrt(diff(path$x)^2 + diff(path$y)^2)
  total <- 0
  run_axis <- NULL
  run_coord <- NULL
  for (i in rev(seq_len(n - 1))) {
    if (is.null(run_axis) && inside[i] && inside[i + 1]) {
      total <- total + len[i]
      next
    }
    if (axis[i] == "o") {
      break
    }
    coord <- if (axis[i] == "h") path$y[i] else path$x[i]
    if (is.null(run_axis)) {
      run_axis <- axis[i]
      run_coord <- coord
    }
    if (axis[i] != run_axis || abs(coord - run_coord) > tol) {
      break
    }
    total <- total + len[i]
  }
  total
}

# The orthogonal predicates for a whole scene: exact endpoints; straight
# edges only where the endpoints share a y; axis-aligned runs outside the
# corners and outside the endpoint discs; a terminal stub of at least
# cap + rc at each end (always for the hand-checked fixtures, otherwise only
# when clearance is reported, since a gap too narrow for its band falls back
# to midpoint slots); R clearance from every non-endpoint node when clearance
# is reported; and slot x values within a gap that differ by at least sep_e,
# both as a set and for any two segments from different source ports whose
# y extents overlap or meet. Two touching verticals at one x form a
# continuous line through both sources that reads as an edge the DAG does
# not have, so touching counts like overlap; after corner rounding two
# touching runs fall short of each other by two corner cuts, 2 rc.
expect_orthogonal_scene <- function(
  scene,
  res,
  stub_always = TRUE,
  prefix = ""
) {
  layers <- infer_layers(scene$nodes, r_default)
  gaps <- if (layers$n > 1) {
    cbind(layers$x[-layers$n], layers$x[-1])
  } else {
    matrix(numeric(0), 0, 2)
  }
  labels <- edge_labels(scene$edges)
  slots <- list()

  for (i in seq_len(nrow(scene$edges))) {
    label <- paste0(prefix, labels[i])
    ends <- edge_endpoints(scene, i)
    path <- res$paths[[i]]
    expect_exact_endpoints(path, ends$from, ends$to)

    if (res$meta$mode[i] == "straight") {
      expect_false(res$meta$routed[i], label = label)
      expect_identical(nrow(path), 2L, label = label)
      # only a horizontal chord or a vertical chord within one layer stays
      # straight
      expect_true(
        abs(ends$from[2] - ends$to[2]) < 1e-3 ||
          abs(ends$from[1] - ends$to[1]) < 1e-3,
        label = label
      )
      next
    }

    expect_equal(res$meta$mode[i], "orthogonal", label = label)
    expect_true(res$meta$routed[i], label = label)
    bends <- res$waypoints[[i]]
    expect_gte(nrow(bends), 2L, label = label)
    expect_equal(res$meta$n_waypoints[i], nrow(bends), label = label)
    expect_true(all(is.na(bends$layer)), label = label)
    expect_orthogonal_outside_corners(
      path,
      bends,
      rc_default,
      label = label,
      ends = ends
    )

    runs <- straight_runs(path)
    expect_gte(nrow(runs), 2L, label = label)
    if (stub_always || res$meta$clearance_ok[i]) {
      floor <- cap_default + rc_default - 1e-9
      expect_gte(
        end_stub_length(rev_path(path), ends$from),
        floor,
        label = label
      )
      expect_gte(end_stub_length(path, ends$to), floor, label = label)
    }
    if (res$meta$clearance_ok[i]) {
      expect_gte(
        path_min_clearance(scene, i, path),
        r_full - verify_tol,
        label = label
      )
    }

    # the canonical source of a segment is the left endpoint
    left <- if (ends$from[1] <= ends$to[1]) {
      scene$edges$from[i]
    } else {
      scene$edges$to[i]
    }
    # port stubs within sep_e of a layer x are not slots (see slot_xs())
    vertical <- runs[runs$axis == "v", , drop = FALSE]
    for (g in seq_len(nrow(gaps))) {
      inside <- vertical$coord > gaps[g, 1] + sep_e_default &
        vertical$coord < gaps[g, 2] - sep_e_default
      if (any(inside)) {
        slots[[length(slots) + 1]] <- data.frame(
          gap = g,
          left = left,
          x = vertical$coord[inside],
          lo = vertical$lo[inside],
          hi = vertical$hi[inside],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(slots) == 0) {
    return(invisible())
  }
  slots <- do.call(rbind, slots)
  for (g in unique(slots$gap)) {
    s <- slots[slots$gap == g, , drop = FALSE]
    gap_label <- paste0(prefix, "gap ", g, " slots")
    xs <- sort(unique(round(s$x, 9)))
    if (length(xs) > 1) {
      expect_true(all(diff(xs) >= sep_e_default - 1e-9), label = gap_label)
    }
    n <- nrow(s)
    for (a in seq_len(n - 1)) {
      for (b in (a + 1):n) {
        # runs from different sources that overlap or meet in y never
        # share an x; two rounded runs that met before rounding are 2 rc
        # apart
        overlap <- min(s$hi[a], s$hi[b]) - max(s$lo[a], s$lo[b])
        touching <- overlap >= -2 * rc_default - 1e-6
        if (s$left[a] != s$left[b] && touching) {
          expect_gte(
            abs(s$x[a] - s$x[b]),
            sep_e_default - 1e-9,
            label = gap_label
          )
        }
      }
    }
  }
  invisible()
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

test_that("route_opts() derives the corner radius and defaults to rounded corners", {
  opts <- route_opts(6)
  expect_true(all(c("corners", "rc") %in% names(opts)))
  expect_equal(opts$corners, "rounded")
  # rc = clamp(0.35 r, 0.8, 2.5): 0.35 * 6 = 2.1 lies inside the clamp
  expect_equal(opts$rc, 2.1)
  # 0.35 * 2 = 0.7 is lifted to the 0.8 mm floor
  expect_equal(route_opts(2)$rc, 0.8)
  # 0.35 * 10 = 3.5 is cut to the 2.5 mm ceiling
  expect_equal(route_opts(10)$rc, 2.5)
  expect_equal(route_opts(6, corners = "sharp")$corners, "sharp")
  expect_error(route_opts(6, corners = "bevel"))
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

# Steepest |dy / dx| along a sampled path, ignoring vertical steps.
max_slope <- function(path) {
  dx <- diff(path$x)
  dy <- diff(path$y)
  keep <- dx > 1e-6
  max(abs(dy[keep] / dx[keep]))
}

test_that("four-layer: the periphery arch peaks near mid-span and climbs as steeply as it descends", {
  scene <- four_layer_scene()
  res <- route_scene(scene)
  path <- res$paths[[10]]
  ends <- edge_endpoints(scene, 10)

  # The chord is horizontal, so the perpendicular offset is y - 55 and the
  # chord parameter is (x - 20) / 120. The outer slots sit 9 mm above the
  # stack tops, 84 at layer 2 and 99 at layer 3, and the verify step lifts
  # the layer 3 waypoint to about 101. An arch that hugs both slots peaks
  # over layer 3 at t = 0.63; a symmetric arch through (60, 101) and
  # (100, 101) peaks at t = 0.5.
  offset <- chord_offset(path, ends$from, ends$to)
  apex <- which.max(offset)
  t_apex <- chord_progress(path, ends$from, ends$to)[apex] /
    chord_length(ends$from, ends$to)
  expect_gte(t_apex, 0.4)
  expect_lte(t_apex, 0.6)

  # The descent is steep by construction: from the layer 3 slot at
  # (100, >= 99) down to t at (140, 55) the mean slope is at least 44 / 40 =
  # 1.1, so no arch through that slot descends at 45 degrees, and the
  # symmetric arch drops at 1.44. What distinguishes a balanced arch is that
  # it climbs as steeply as it falls, whereas the slot-hugging arch climbs
  # at 0.88 and falls at 1.50.
  climb <- max_slope(pt(path$x[seq_len(apex)], path$y[seq_len(apex)]))
  descent <- max_slope(pt(
    path$x[apex:nrow(path)],
    path$y[apex:nrow(path)]
  ))
  expect_lte(descent, 1.25 * climb)
  expect_lte(descent, 1.5)

  # both ends leave and arrive within the 40 degree tangent clamp
  expect_lt(arrival_angle(path, ends$to), 40)
  expect_lt(arrival_angle(pt(rev(path$x), rev(path$y)), ends$from), 40)
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

test_that("layer_free_intervals() drops interior gaps narrower than sep_e", {
  bounds <- c(0, 0, 160, 110)

  # Four nodes at y = 20, 38.2, 59.2, and 82.2 with padded radius R = 9
  # leave gaps of 0.2 mm ([29, 29.2]), 3.0 mm ([47.2, 50.2]), and 5.0 mm
  # ([68.2, 73.2]). A slot narrower than sep_e = 3.6 cannot hold an edge
  # with its separation, so only the 5 mm gap and the two outer intervals
  # remain.
  nodes <- mm_nodes(c("a", "b", "c", "d"), 80, c(20, 38.2, 59.2, 82.2))
  ints <- layer_free_intervals(nodes, 3, bounds, sep_e = sep_e_default)
  expect_equal(ints$lo, c(0.5, 68.2, 91.2))
  expect_equal(ints$hi, c(11, 73.2, 109.5))
  expect_equal(ints$outer, c(TRUE, FALSE, TRUE))

  # a chord dead on b snaps past both slivers to the surviving intervals
  expect_equal(nearest_free_y(ints, 38.2, 1, FALSE), 68.2)
  expect_equal(nearest_free_y(ints, 38.2, -1, FALSE), 11)

  # an outer interval is kept however narrow: a node at y = 100 leaves
  # [109, 109.5] toward the panel edge
  edge <- layer_free_intervals(
    mm_nodes("z", 80, 100),
    3,
    bounds,
    sep_e = sep_e_default
  )
  expect_equal(edge$lo, c(0.5, 109))
  expect_equal(edge$hi, c(91, 109.5))
  expect_equal(edge$outer, c(TRUE, TRUE))
})

test_that("a chord whose nearest slot is a sliver is routed to the next interval", {
  # S -> T runs dead on b through a layer whose gaps above and below b are
  # 3.0 and 0.2 mm wide. Without the width floor both sides snap 9 mm off
  # the chord into a sliver and the tie goes above, to y = 47.2. With it the
  # candidates are 11 below (displacement 27.2) and 68.2 above (30), so the
  # waypoint sits in the lower outer interval.
  scene <- list(
    nodes = rbind(
      mm_nodes(c("S", "T"), c(20, 140), 38.2),
      mm_nodes(c("a", "b", "c", "d"), 80, c(20, 38.2, 59.2, 82.2))
    ),
    edges = mm_edges("S", "T"),
    bounds = c(0, 0, 160, 110)
  )
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)

  expect_true(res$meta$routed[1])
  expect_true(res$meta$mode[1] %in% c("interior", "periphery"))
  expect_equal(res$meta$side[1], -1)
  expect_true(res$meta$clearance_ok[1])
  wp <- res$waypoints[[1]]
  at_layer <- wp$y[wp$layer == 2]
  expect_length(at_layer, 1)
  expect_true(at_layer <= 11 + 1e-6 || at_layer >= 68.2 - 1e-6)

  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(nrow(path), 16)
  expect_gte(path_min_clearance(scene, 1, path), r_full - verify_tol)
  expect_true(all(diff(path$x) >= -0.1))
  expect_lt(max(abs(turning_angles(path))), 12)
})

# Two horizontal chords at y = 30 and 46 cross a layer whose stack at y =
# 40, 23, and 6 leaves one free interval, [49, 109.5], so both snap to 49
# above. `low` and `high` name the endpoints of the chord-30 and chord-46
# edges; the edges tie on span and length, so the canonical name order
# decides which is routed first.
shared_slot_scene <- function(low, high) {
  list(
    nodes = rbind(
      mm_nodes(c(low, high), c(20, 140, 20, 140), c(30, 30, 46, 46)),
      mm_nodes(c("m1", "m2", "m3"), 80, c(40, 23, 6))
    ),
    edges = mm_edges(c(low[1], high[1]), c(low[2], high[2])),
    bounds = c(0, 0, 160, 110)
  )
}

expect_shared_slot_order <- function(scene) {
  res <- route_scene(scene)
  expect_true(all(res$meta$routed))
  expect_equal(res$meta$mode, c("interior", "interior"))
  expect_equal(res$meta$side, c(1, 1))
  expect_true(all(res$meta$clearance_ok))

  low <- res$waypoints[[1]]
  high <- res$waypoints[[2]]
  expect_identical(nrow(low), 1L)
  expect_identical(nrow(high), 1L)
  expect_equal(c(low$x, high$x), c(80, 80))
  expect_equal(c(low$layer, high$layer), c(2, 2))
  # the lower chord takes the slot boundary, 9 above m1; the higher chord
  # sits sep_e further out
  expect_equal(low$y, 49)
  expect_equal(high$y, 49 + sep_e_default)
  expect_lt(low$y, high$y)
  expect_gte(high$y - low$y, sep_e_default - 1e-6)

  for (i in 1:2) {
    ends <- edge_endpoints(scene, i)
    expect_exact_endpoints(res$paths[[i]], ends$from, ends$to)
    expect_gte(
      path_min_clearance(scene, i, res$paths[[i]]),
      r_full - verify_tol
    )
  }
}

test_that("spread_in_slot(): edges sharing a slot keep the y-order of their chords", {
  # the lower chord is routed first and the higher one is spread outward
  expect_shared_slot_order(shared_slot_scene(c("a1", "b1"), c("a2", "b2")))
  # the higher chord is routed first: the order of the chords at the layer
  # still decides who sits inside, not the order of routing
  expect_shared_slot_order(shared_slot_scene(c("a2", "b2"), c("a1", "b1")))
})

# Two fan-in edges pa -> t and pb -> t span four layers. The stacks at
# layers 2 and 3 (x = 60 and 100) leave no free interval below and one
# above each: [99, 109.5] at layer 2 and [65, 109.5] at layer 3. Both
# chords are blocked at both layers and can only arch above.
occupied_arch_scene <- function() {
  list(
    nodes = rbind(
      mm_nodes(c("pa", "pb", "t"), c(20, 20, 140), c(40, 60, 55)),
      mm_nodes(paste0("q", 1:6), 60, c(5, 22, 39, 56, 73, 90)),
      mm_nodes(paste0("s", 1:4), 100, c(5, 22, 39, 56))
    ),
    edges = mm_edges(c("pa", "pb"), c("t", "t")),
    bounds = c(0, 0, 160, 110)
  )
}

# y of a sampled path where it crosses the vertical line x = x0.
path_y_at <- function(path, x0) {
  n <- nrow(path)
  k <- which(path$x[-n] <= x0 & path$x[-1] >= x0)[1]
  stats::approx(path$x[k + 0:1], path$y[k + 0:1], xout = x0)$y
}

test_that("an arch occupies every layer it crosses, not only the layers of its waypoints", {
  scene <- occupied_arch_scene()
  res <- route_scene(scene)

  expect_true(all(res$meta$routed))
  expect_true(all(res$meta$clearance_ok))
  expect_equal(res$meta$side, c(1, 1))

  # pa -> t is the longer edge (120.9 against 120.1 mm) and routes first.
  # Its slots are 99 at layer 2 and 65 at layer 3; the line from (60, 99)
  # to t passes layer 3 at 77, above 65, so the hull drops the layer 3
  # waypoint and the arch crosses that layer without one. The verify step
  # then lifts the remaining waypoint to about 101.
  expect_identical(res$meta$waypoint_layers[[1]], 2L)

  # pb -> t wants the same slots. Its chord is higher at both layers (58.3
  # against 45 at layer 2, 56.7 against 50 at layer 3), so it sits outside
  # pa -> t: sep_e above the waypoint pa -> t was drawn through at layer 2,
  # and sep_e above where the pa -> t arch actually passes layer 3, rather
  # than at the slot boundary 65 that its own hull would discard. Spreading
  # only against registered waypoints drew the arches 1.6 mm apart at
  # x = 60 and 0.4 mm apart at x = 100.
  for (x0 in c(60, 100)) {
    ya <- path_y_at(res$paths[[1]], x0)
    yb <- path_y_at(res$paths[[2]], x0)
    expect_gt(yb, ya)
    expect_gte(yb - ya, sep_e_default - 0.1)
  }

  for (i in 1:2) {
    ends <- edge_endpoints(scene, i)
    path <- res$paths[[i]]
    expect_exact_endpoints(path, ends$from, ends$to)
    expect_gte(nrow(path), 16)
    expect_gte(path_min_clearance(scene, i, path), r_full - verify_tol)
    expect_true(all(diff(path$x) >= -0.1))
    expect_lt(max(abs(turning_angles(path))), 12)
  }

  # deterministic and invariant to row order
  expect_identical(route_scene(scene), res)
  shuffled <- scene
  shuffled$nodes <- scene$nodes[rev(seq_len(nrow(scene$nodes))), ]
  shuffled$edges <- scene$edges[2:1, ]
  rownames(shuffled$nodes) <- NULL
  rownames(shuffled$edges) <- NULL
  res2 <- route_scene(shuffled)
  expect_identical(res2$paths[[2]], res$paths[[1]])
  expect_identical(res2$paths[[1]], res$paths[[2]])
  expect_identical(res2$waypoints[[2]], res$waypoints[[1]])
  expect_identical(res2$waypoints[[1]], res$waypoints[[2]])
})

# The occupied-arch scene with a fifth node s5 at (100, 80) on top of the
# layer 3 stack. Both chords now snap into the 6 mm gap between s4 and s5,
# which the hull discards because the line from the layer 2 slot to t
# passes above it, so the layer 3 waypoint of each arch is inserted by the
# repair loop at s5 + R = 89 from the node disc alone. Without a re-check
# against the occupancy, pb -> t was drawn 0.29 mm from pa -> t at x = 100.
# The panel is 120 mm high: the outer arch passes layer 2 at 107.6 and
# every drawn curve keeps the clearance margin m = 3 from the bounds.
repaired_arch_scene <- function() {
  scene <- occupied_arch_scene()
  scene$nodes <- rbind(scene$nodes, mm_nodes("s5", 100, 80))
  scene$bounds <- c(0, 0, 160, 120)
  scene
}

test_that("an arch that repairs moved onto another arch is spread again", {
  scene <- repaired_arch_scene()
  res <- route_scene(scene)

  expect_true(all(res$meta$routed))
  expect_true(all(res$meta$clearance_ok))
  expect_equal(res$meta$side, c(1, 1))

  # pa -> t routes first and its repaired arch passes layer 3 near 92;
  # pb -> t has the higher chord at both layers and stays sep_e outside it
  for (x0 in c(60, 100)) {
    ya <- path_y_at(res$paths[[1]], x0)
    yb <- path_y_at(res$paths[[2]], x0)
    expect_gt(yb, ya)
    expect_gte(yb - ya, sep_e_default - 0.1)
  }
  for (i in 1:2) {
    ends <- edge_endpoints(scene, i)
    path <- res$paths[[i]]
    expect_exact_endpoints(path, ends$from, ends$to)
    expect_gte(path_min_clearance(scene, i, path), r_full - verify_tol)
    expect_true(all(diff(path$x) >= -0.1))
    expect_lt(max(abs(turning_angles(path))), 12)
  }

  # deterministic and invariant to row order
  expect_identical(route_scene(scene), res)
  shuffled <- scene
  shuffled$nodes <- scene$nodes[rev(seq_len(nrow(scene$nodes))), ]
  shuffled$edges <- scene$edges[2:1, ]
  rownames(shuffled$nodes) <- NULL
  rownames(shuffled$edges) <- NULL
  res2 <- route_scene(shuffled)
  expect_identical(res2$paths[[2]], res$paths[[1]])
  expect_identical(res2$paths[[1]], res$paths[[2]])
})

test_that("a re-spread arch keeps its parallel-group offset only once", {
  # t -> pb makes pb -> t one of a parallel pair, routed with an extra margin
  # and translated sep_m / 2 = 3 mm inward. The re-spread waypoints already
  # carry that offset; applying it a second time pulled the arch back onto
  # pa -> t (0.60 mm apart at x = 60) and cost it its clearance.
  scene <- repaired_arch_scene()
  scene$edges <- rbind(scene$edges, mm_edges("t", "pb"))
  res <- route_scene(scene)

  expect_true(all(res$meta$routed))
  expect_true(all(res$meta$clearance_ok))
  expect_equal(res$meta$side, c(1, 1, 1))
  for (i in 1:3) {
    expect_gte(
      path_min_clearance(scene, i, res$paths[[i]]),
      r_full - verify_tol
    )
  }

  # the reversed member is drawn target to source
  flip <- function(path) pt(rev(path$x), rev(path$y))
  pa <- res$paths[[1]]
  inner <- res$paths[[2]]
  outer <- flip(res$paths[[3]])
  for (x0 in c(60, 100)) {
    ya <- path_y_at(pa, x0)
    yi <- path_y_at(inner, x0)
    yo <- path_y_at(outer, x0)
    expect_gt(yi, ya)
    expect_gte(yi - ya, sep_e_default - 0.1)
    # the pair sits at least sep_m = 6 apart, the inner member nearer
    # pa -> t; the outer member is also spread sep_e from the inner one
    # before its own translation, so the gap is sep_m + (sep_e - sep_m / 2)
    expect_gt(yo, yi)
    expect_gte(yo - yi, 6 - 0.1)
    expect_lte(yo - yi, 6 + sep_e_default)
  }

  # deterministic and invariant to row order
  expect_identical(route_scene(scene), res)
  shuffled <- scene
  shuffled$nodes <- scene$nodes[rev(seq_len(nrow(scene$nodes))), ]
  shuffled$edges <- scene$edges[3:1, ]
  rownames(shuffled$nodes) <- NULL
  rownames(shuffled$edges) <- NULL
  res2 <- route_scene(shuffled)
  for (i in 1:3) {
    expect_identical(res2$paths[[4 - i]], res$paths[[i]])
  }
})

# Two skip edges of one row, a -> d and b -> e, have chords of exactly 120
# mm each; both are blocked by the nodes between their endpoints. With equal
# spans and lengths the name order decides which routes first, and a
# floating difference in the seventh decimal must not change that.
equal_skip_scene <- function(eps = 0) {
  list(
    nodes = mm_nodes(
      c("a", "b", "c", "d", "e"),
      c(0, 40, 80, 120, 160 + eps),
      50
    ),
    edges = mm_edges(
      c("a", "b", "a", "b", "c", "d"),
      c("d", "e", "b", "c", "d", "e")
    ),
    bounds = c(-10, 0, 170, 100)
  )
}

# proper crossings between two sampled paths
count_paths_crossing <- function(p, q) {
  total <- 0L
  for (i in seq_len(nrow(q) - 1)) {
    total <- total +
      count_path_crossings(
        p,
        c(q$x[i], q$y[i]),
        c(q$x[i + 1], q$y[i + 1])
      )
  }
  total
}

test_that("skip edges of equal chord length route in name order and take opposite sides", {
  ref <- route_scene(equal_skip_scene())
  expect_true(all(ref$meta$routed[1:2]))
  expect_equal(ref$meta$mode[1:2], c("interior", "interior"))
  # a -> d routes first and ties above; b -> e above would cross it twice,
  # so it goes below and the two arches cross nowhere
  expect_equal(ref$meta$side[1:2], c(1, -1))
  expect_identical(count_paths_crossing(ref$paths[[1]], ref$paths[[2]]), 0L)

  # a 1e-9 mm perturbation of e's x in either direction changes b -> e's
  # length in the ninth decimal only, so the ordering, the sides, and the
  # modes are unchanged
  for (eps in c(1e-9, -1e-9)) {
    res <- route_scene(equal_skip_scene(eps))
    expect_identical(res$meta$mode, ref$meta$mode)
    expect_identical(res$meta$side, ref$meta$side)
    expect_identical(res$meta$waypoint_layers, ref$meta$waypoint_layers)
    expect_identical(count_paths_crossing(res$paths[[1]], res$paths[[2]]), 0L)
  }
})

# A short bottom-row chord b -> g at y = 11 blocked by d, whose floor slot
# ends at d - R = 2, inside the clearance margin m = 3. Above d a stack of
# four more nodes leaves slivers only until the gap between n2 and n3, at
# [59, 76], so the other interior slot is 48 mm above the chord and an arch
# through it crosses the placed chord p -> q at y = 40 twice: cost 8 for the
# displacement plus 32 for the crossings. The free bow above d sits 9 mm
# above the chord and crosses nothing, at cost 1.5.
far_slot_scene <- function() {
  list(
    nodes = rbind(
      mm_nodes(c("b", "g", "p", "q"), c(20, 80, 20, 80), c(11, 11, 40, 40)),
      mm_nodes(c("d", "n1", "n2", "n3", "n4"), 50, c(11, 31, 50, 85, 104))
    ),
    edges = mm_edges(c("b", "p"), c("g", "q")),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("a chord whose remaining slot is far away takes the cheaper bow", {
  scene <- far_slot_scene()
  res <- route_scene(scene)
  ends <- edge_endpoints(scene, 1)
  path <- res$paths[[1]]

  # p -> q clears n1 and n2 by exactly R and stays straight
  expect_false(res$meta$routed[2])
  expect_straight_path(res$paths[[2]], node_xy(scene, "p"), node_xy(scene, "q"))

  # the floor slot cannot keep the margin, and the slot at 59 is not taken
  # by default: the bow above d is cheaper and verifies
  expect_true(res$meta$routed[1])
  expect_equal(res$meta$mode[1], "bow")
  expect_equal(res$meta$side[1], 1)
  expect_true(res$meta$clearance_ok[1])
  expect_lte(max(abs(chord_offset(path, ends$from, ends$to))), 2 * r_full)
  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(path_min_clearance(scene, 1, path), r_full - verify_tol)
  expect_gte(min(path$y[-c(1, nrow(path))]), 3 - 1e-6)
  expect_identical(
    count_path_crossings(path, node_xy(scene, "p"), node_xy(scene, "q")),
    0L
  )

  # deterministic and invariant to row order
  expect_identical(route_scene(scene), res)
  shuffled <- scene
  shuffled$nodes <- scene$nodes[rev(seq_len(nrow(scene$nodes))), ]
  shuffled$edges <- scene$edges[2:1, ]
  rownames(shuffled$nodes) <- NULL
  rownames(shuffled$edges) <- NULL
  res2 <- route_scene(shuffled)
  expect_identical(res2$paths[[2]], res$paths[[1]])
  expect_identical(res2$meta$mode[2], "bow")
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

# A and B are `gap` mm apart on y = 50 and C sits above the chord midpoint.
short_chord_scene <- function(gap, c_y) {
  list(
    nodes = mm_nodes(
      c("A", "B", "C"),
      c(50, 50 + gap, 50 + gap / 2),
      c(50, 50, c_y)
    ),
    edges = mm_edges("A", "B"),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("a chord shorter than 2R between overlapping discs stays straight", {
  # A and B are 14 mm apart, so their 6 mm discs are 2 mm from touching and
  # the chord is shorter than 2R = 18. C sits 8 mm above the chord midpoint
  # (a soft hit) or 6 mm above it (a hard hit); either way its disc overlaps
  # both endpoint discs and a bow around it would be steeper than the chord
  # is long, so the chord is drawn as is.
  for (c_y in c(58, 56)) {
    scene <- short_chord_scene(14, c_y)
    res <- route_scene(scene)
    ends <- edge_endpoints(scene, 1)

    expect_lt(chord_min_clearance(scene, 1), r_full)
    expect_false(res$meta$routed[1])
    expect_equal(res$meta$mode[1], "straight")
    expect_equal(res$meta$n_waypoints[1], 0)
    expect_straight_path(res$paths[[1]], ends$from, ends$to)
  }
})

test_that("a chord just longer than 2R is still routed around a hit", {
  # 18.5 mm chord, C 6 mm above its midpoint: a hard hit on an edge long
  # enough to bow, so the rule is a threshold and not an exemption for
  # short edges
  scene <- short_chord_scene(18.5, 56)
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)

  expect_lt(chord_min_clearance(scene, 1), r_soft)
  expect_true(res$meta$routed[1])
  expect_true(res$meta$clearance_ok[1])
  expect_gte(nrow(path), 16)
  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(path_min_dist(path, node_xy(scene, "C")), r_full - verify_tol)
  expect_lt(max(abs(turning_angles(path))), 12)
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
        if (d < r_soft && chord_length(ends$from, ends$to) >= 2 * r_full) {
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

expect_paths_inside <- function(res, bounds, label) {
  for (path in res$paths) {
    expect_gte(min(path$x), bounds[1] - 1e-6, label = label)
    expect_lte(max(path$x), bounds[3] + 1e-6, label = label)
    expect_gte(min(path$y), bounds[2] - 1e-6, label = label)
    expect_lte(max(path$y), bounds[4] + 1e-6, label = label)
  }
}

# every sampled point of every path keeps the clearance margin m = 3 from
# the panel bounds
m_default <- 3

# the endpoints are node centres, which may sit closer to the bounds than
# m on a small device, so only the interior samples are held to the margin
expect_paths_keep_margin <- function(res, bounds, label) {
  for (path in res$paths) {
    n <- nrow(path)
    if (n <= 2) {
      next
    }
    x <- path$x[-c(1, n)]
    y <- path$y[-c(1, n)]
    d <- min(x - bounds[1], bounds[3] - x, y - bounds[2], bounds[4] - y)
    expect_gte(d, m_default - 1e-6, label = label)
  }
}

test_that("device size: routing decisions do not change with the panel size", {
  panels <- list(c(100, 70), c(120, 84), c(160, 110), c(240, 160))

  for (make in list(mediator_scene, fan_scene)) {
    ref <- route_scene(make(panel = c(160, 110)))
    for (panel in panels) {
      scene <- make(panel = panel)
      res <- route_scene(scene)
      expect_identical(res$meta$routed, ref$meta$routed)
      expect_identical(res$meta$side, ref$meta$side)
      expect_identical(res$meta$mode, ref$meta$mode)
      expect_identical(res$meta$waypoint_layers, ref$meta$waypoint_layers)
      label <- paste(panel, collapse = "x")
      expect_paths_inside(res, scene$bounds, label)
      expect_paths_keep_margin(res, scene$bounds, label)
      ortho <- route_scene(scene, mode = "orthogonal")
      expect_paths_keep_margin(ortho, scene$bounds, label)
    }
  }

  # The four-layer periphery needs a slot above s1 that keeps m = 3 from
  # the panel edge: at 160 x 110 and 240 x 160 it has one, and p->t sweeps
  # the periphery with full clearance. At 120 x 84 the slot may reach
  # 84 - 3 = 81 and s1 + R = 77.7 fits, but the levelled arch bulges past
  # 81 and the candidate gives way to the interior; at 100 x 70 the slot
  # ends at 67 while s1 + R reaches 66.3, and the arch again bulges past
  # it. Whatever the size, no sampled point leaves the panel or comes
  # closer than m to its bounds, in either mode.
  ref <- route_scene(four_layer_scene(panel = c(160, 110)))
  for (panel in panels) {
    scene <- four_layer_scene(panel = panel)
    res <- route_scene(scene)
    label <- paste(panel, collapse = "x")
    expect_identical(res$meta$routed, ref$meta$routed)
    expect_identical(res$meta$side, ref$meta$side)
    expect_identical(res$meta$mode[1:9], ref$meta$mode[1:9])
    expect_paths_inside(res, scene$bounds, label)
    expect_paths_keep_margin(res, scene$bounds, label)
    expect_paths_keep_margin(
      route_scene(scene, mode = "orthogonal"),
      scene$bounds,
      label
    )
    if (panel[1] >= 160) {
      expect_identical(res$meta$mode[10], "periphery", label = label)
      expect_identical(res$meta$waypoint_layers, ref$meta$waypoint_layers)
      expect_true(res$meta$clearance_ok[10], label = label)
    } else {
      expect_identical(res$meta$mode[10], "interior", label = label)
    }
  }
  # the interior route verifies at 120 x 84 (8.97 mm from s2) but not at
  # 100 x 70 (8.82 mm), where the panel is too small for any route to
  # clear s2 by R; that value is pinned so a change is noticed
  expect_true(route_scene(four_layer_scene(
    panel = c(120, 84)
  ))$meta$clearance_ok[10])
  expect_false(route_scene(four_layer_scene(
    panel = c(100, 70)
  ))$meta$clearance_ok[10])
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

# Fixture: orthogonal ------------------------------------------------------------

# Orthogonal mode draws every edge orthogonally whether or not its chord is
# blocked. A chord whose endpoints share a y is the straight chord. Every
# other edge leaves through a port and follows axis-aligned runs: an E port
# (S$x + r, S$y) and a W port (T$x - r, T$y) with one vertical run in each
# crossed gap at an assigned slot x, or, for a spanning edge whose endpoints
# are alone in (or the extreme of) their layers, S/N ports and a channel run
# past the crossed stack at extreme_y +/- R. Within a gap the slots are spread
# evenly over the band [x_L + stub, x_R - stub]. The default corners are
# rounded with a quadratic Bezier of radius rc; "sharp" keeps the bends, which
# makes coordinates exact.
#
# Every drawn segment belongs to one edge unless two edges share a port:
# edges leaving one port form a hyperedge trunk and edges entering one port
# merge into their last run. So two segments from different sources never
# share an x in a gap, even when their y-intervals only meet; a channel is
# never shared by edges with different sources, and two same-side channels
# whose x-ranges overlap stack sep_e apart with the shorter span inside; an
# arrival and a departure on one side of a node use ports sep_e apart along
# that side; and a channel whose margin band would cut a disc is infeasible,
# the edge running through a free interval of every crossed layer instead.
# Every candidate, S/N or E/W, is priced as displacement / r + bend_penalty
# per bend + the crossing and congestion terms, ties above.
ortho <- function(scene, corners = NULL, ...) {
  opts <- if (is.null(corners)) {
    route_opts(r_default)
  } else {
    route_opts(r_default, corners = corners)
  }
  route_scene(scene, mode = "orthogonal", opts = opts, ...)
}

edge_index <- function(scene, label) {
  match(label, edge_labels(scene$edges))
}

# The worked trace of the fan. Gap 1 (x 20 to 80) has the band [36.1, 63.9],
# whose single slot sits at 36.1 + 27.8 / 2 = 50; gap 2 (80 to 140) has the
# band [96.1, 123.9] and its single slot at 110. a->e spans both gaps with a
# and e alone in their layers, so it takes S ports and the channel below the
# middle stack at 25 - 9 = 16. The polylines below omit the ports, which are
# collinear with the centres and the first bends.
fan_sharp_polylines <- list(
  "a->b" = pt(c(20, 50, 50, 80), c(55, 55, 85, 85)),
  "a->d" = pt(c(20, 50, 50, 80), c(55, 55, 25, 25)),
  "b->e" = pt(c(80, 110, 110, 140), c(85, 85, 55, 55)),
  "a->e" = pt(c(20, 20, 140, 140), c(55, 16, 16, 55))
)
fan_straight <- c("a->c", "c->e")

test_that("orthogonal fan: sharp corners reproduce the worked trace exactly", {
  scene <- fan_scene()
  res <- ortho(scene, corners = "sharp")

  for (lab in fan_straight) {
    i <- edge_index(scene, lab)
    ends <- edge_endpoints(scene, i)
    expect_false(res$meta$routed[i])
    expect_equal(res$meta$mode[i], "straight")
    expect_equal(res$meta$n_waypoints[i], 0)
    expect_identical(nrow(res$waypoints[[i]]), 0L)
    expect_true(is.na(res$meta$side[i]))
    expect_straight_path(res$paths[[i]], ends$from, ends$to)
  }

  for (lab in names(fan_sharp_polylines)) {
    i <- edge_index(scene, lab)
    ends <- edge_endpoints(scene, i)
    poly <- fan_sharp_polylines[[lab]]
    path <- res$paths[[i]]

    expect_true(res$meta$routed[i], label = lab)
    expect_equal(res$meta$mode[i], "orthogonal", label = lab)
    expect_true(res$meta$clearance_ok[i], label = lab)
    expect_true(is.na(res$meta$sagitta_ratio[i]), label = lab)
    expect_true(is.na(res$meta$sagitta_capped[i]), label = lab)
    expect_true(all(is.na(res$meta$waypoint_layers[[i]])), label = lab)
    expect_exact_endpoints(path, ends$from, ends$to)
    expect_lt(polyline_hausdorff(path, poly), 1e-6, label = lab)
    # a sharp path is axis-aligned everywhere
    expect_true(all(segment_axes(dedupe_path(path)) != "o"), label = lab)

    # the two bends are the interior vertices of the polyline
    bends <- res$waypoints[[i]]
    expect_equal(res$meta$n_waypoints[i], 2, label = lab)
    expect_identical(nrow(bends), 2L, label = lab)
    expect_equal(bends$x, poly$x[2:3], tolerance = 1e-6, label = lab)
    expect_equal(bends$y, poly$y[2:3], tolerance = 1e-6, label = lab)
    expect_true(all(is.na(bends$layer)), label = lab)
  }

  # only the channel edge reports a side: below, away from the crowded fan
  # (b lies above the chord at both a and e, d below at a only)
  expect_equal(res$meta$side[edge_index(scene, "a->e")], -1)
  expect_true(all(is.na(res$meta$side[-edge_index(scene, "a->e")])))
})

test_that("orthogonal fan: rounded corners stay within rc of each bend and turn gently", {
  scene <- fan_scene()
  res <- ortho(scene)
  sharp <- ortho(scene, corners = "sharp")

  for (lab in fan_straight) {
    i <- edge_index(scene, lab)
    ends <- edge_endpoints(scene, i)
    expect_straight_path(res$paths[[i]], ends$from, ends$to)
  }

  for (lab in names(fan_sharp_polylines)) {
    i <- edge_index(scene, lab)
    ends <- edge_endpoints(scene, i)
    path <- res$paths[[i]]
    bends <- res$waypoints[[i]]

    expect_equal(res$meta$mode[i], "orthogonal", label = lab)
    expect_equal(bends$x, sharp$waypoints[[i]]$x, label = lab)
    expect_equal(bends$y, sharp$waypoints[[i]]$y, label = lab)
    expect_exact_endpoints(path, ends$from, ends$to)

    # every sample lies on the sharp polyline or inside a corner span, and
    # each corner span lies within rc of its bend
    d <- point_polyline_dist(path, sharp$paths[[i]])
    near <- corner_points(path, bends, rc_default + 1e-6)
    expect_true(all(d <= 1e-6 | near), label = lab)
    expect_true(any(near), label = lab)
    # the corner is really cut: the path never reaches the bend itself (the
    # quadratic's closest approach to a right-angle bend is 0.354 rc = 0.74)
    for (k in seq_len(nrow(bends))) {
      expect_gt(
        path_min_dist(path, c(bends$x[k], bends$y[k])),
        0.5,
        label = lab
      )
    }
    expect_orthogonal_outside_corners(path, bends, rc_default, label = lab)
    # the corner must turn less than 12 degrees per sample; at uniform t a
    # quadratic Bezier through a right angle needs at least 11 samples for
    # that (8 samples peak at 15.9 degrees), so the sample count is not pinned
    expect_lt(max(abs(turning_angles(path))), 12, label = lab)
  }

  # at the (50, 55) corner of a->b the curve is tangent to the runs at
  # P = (50 - 2.1, 55) and Q = (50, 55 + 2.1)
  ab <- res$paths[[edge_index(scene, "a->b")]]
  expect_lt(point_polyline_dist(pt(47.9, 55), ab), 0.05)
  expect_lt(point_polyline_dist(pt(50, 57.1), ab), 0.05)
})

test_that("orthogonal predicates hold on the fan, four-layer, and mediator fixtures", {
  for (make in list(fan_scene, four_layer_scene, mediator_scene)) {
    scene <- make()
    res <- ortho(scene)
    expect_orthogonal_scene(scene, res, stub_always = TRUE)
  }
})

test_that("orthogonal four-layer: p->t takes the channel above the stacks at y = 99", {
  scene <- four_layer_scene()
  res <- ortho(scene)
  i <- edge_index(scene, "p->t")
  path <- res$paths[[i]]
  ends <- edge_endpoints(scene, i)

  expect_true(res$meta$routed[i])
  expect_equal(res$meta$mode[i], "orthogonal")
  expect_true(res$meta$clearance_ok[i])
  # above: 90 + 9 = 99, below: 20 - 9 = 11, both displace the chord by 44;
  # q1 and s1 lie above the chord and q2 and s3 below, so congestion ties
  # too and the tie goes above
  expect_equal(res$meta$side[i], 1)
  expect_equal(res$meta$n_waypoints[i], 2)
  wp <- res$waypoints[[i]]
  expect_equal(wp$x, c(20, 140), tolerance = 1e-6)
  expect_equal(wp$y, c(99, 99), tolerance = 1e-6)

  expect_exact_endpoints(path, ends$from, ends$to)
  runs <- straight_runs(path)
  channel <- runs[which.max(runs$length), ]
  expect_equal(channel$axis, "h")
  expect_equal(channel$coord, 99, tolerance = 1e-6)
  # up the N stub, across, and down: x never decreases
  expect_true(all(diff(dedupe_path(path)$x) >= -1e-9))
  expect_true(all(path$y >= 55 - 1e-9))
  expect_gte(path_min_dist(path, node_xy(scene, "s1")), r_full - verify_tol)
  expect_gte(path_min_clearance(scene, i, path), r_full - verify_tol)

  # the short edges are all orthogonal, with s2->t the only straight chord
  for (j in setdiff(seq_len(nrow(scene$edges)), i)) {
    lab <- edge_labels(scene$edges)[j]
    if (lab == "s2->t") {
      expect_equal(res$meta$mode[j], "straight", label = lab)
    } else {
      expect_equal(res$meta$mode[j], "orthogonal", label = lab)
      expect_true(is.na(res$meta$side[j]), label = lab)
    }
  }
})

test_that("orthogonal mediator: the collinear scene keeps x->m and m->y straight and channels x->y above", {
  scene <- mediator_scene()
  res <- ortho(scene)

  for (lab in c("x->m", "m->y")) {
    i <- edge_index(scene, lab)
    ends <- edge_endpoints(scene, i)
    expect_false(res$meta$routed[i], label = lab)
    expect_equal(res$meta$mode[i], "straight", label = lab)
    expect_straight_path(res$paths[[i]], ends$from, ends$to)
  }

  i <- edge_index(scene, "x->y")
  path <- res$paths[[i]]
  ends <- edge_endpoints(scene, i)
  expect_equal(res$meta$mode[i], "orthogonal")
  expect_true(res$meta$clearance_ok[i])
  # both channels displace equally and nothing is congested: the tie goes up
  expect_equal(res$meta$side[i], 1)
  expect_equal(res$meta$n_waypoints[i], 2)
  wp <- res$waypoints[[i]]
  expect_equal(wp$x, c(7.3, 152.7), tolerance = 1e-6)
  expect_equal(wp$y[1], wp$y[2], tolerance = 1e-6)
  # the channel clears m by R (y >= 55 + 9 = 64) and, because x and y sit at
  # the same y as m, must reach further than that to leave a straight stub
  # of cap + rc after the corner is rounded
  expect_gte(wp$y[1], 64 - 1e-9)
  runs <- straight_runs(path)
  expect_gte(runs$length[1], cap_default + rc_default - 1e-9)
  expect_gte(runs$length[nrow(runs)], cap_default + rc_default - 1e-9)
  expect_true(all(path$y >= 55 - 1e-9))
  expect_gte(path_min_dist(path, node_xy(scene, "m")), r_full - verify_tol)
})

test_that("orthogonal mediator: a displaced m gets one slot per gap and the channel runs away from it", {
  # m sits 20 mm above the chord, so x->m and m->y climb and descend through
  # their gaps. The horizontal chord x->y clears m by more than R, so it is
  # already axis-aligned and stays straight: only a chord that needs a bend
  # to be axis-aligned is drawn orthogonally.
  scene <- mediator_scene(m_y = 75)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  # gap 1 band [7.3 + 16.1, 80 - 16.1] = [23.4, 63.9] has its single slot at
  # the midpoint 43.65; gap 2 band [96.1, 136.6] at 116.35
  i <- edge_index(scene, "x->m")
  expect_equal(res$meta$mode[i], "orthogonal")
  expect_equal(slot_xs(res$paths[[i]], c(7.3, 80)), 43.65, tolerance = 1e-6)
  expect_equal(res$meta$n_waypoints[i], 2)
  expect_equal(res$waypoints[[i]]$x, c(43.65, 43.65), tolerance = 1e-6)
  expect_equal(res$waypoints[[i]]$y, c(55, 75), tolerance = 1e-6)

  j <- edge_index(scene, "m->y")
  expect_equal(res$meta$mode[j], "orthogonal")
  expect_equal(slot_xs(res$paths[[j]], c(80, 152.7)), 116.35, tolerance = 1e-6)
  expect_equal(res$waypoints[[j]]$x, c(116.35, 116.35), tolerance = 1e-6)
  expect_equal(res$waypoints[[j]]$y, c(75, 55), tolerance = 1e-6)

  k <- edge_index(scene, "x->y")
  ends <- edge_endpoints(scene, k)
  expect_gte(chord_min_clearance(scene, k), r_full)
  expect_equal(res$meta$mode[k], "straight")
  expect_false(res$meta$routed[k])
  expect_true(is.na(res$meta$side[k]))
  expect_equal(res$meta$n_waypoints[k], 0)
  expect_straight_path(res$paths[[k]], ends$from, ends$to)
})

test_that("orthogonal mediator: a horizontal chord that a displaced m blocks takes the channel away from it", {
  # m sits 8 mm above the chord, inside R = 9, so the straight chord is
  # blocked and x->y takes the channel below, away from m
  scene <- mediator_scene(m_y = 63)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  k <- edge_index(scene, "x->y")
  path <- res$paths[[k]]
  expect_lt(chord_min_clearance(scene, k), r_full)
  expect_equal(res$meta$mode[k], "orthogonal")
  expect_true(res$meta$routed[k])
  expect_equal(res$meta$side[k], -1)
  expect_equal(res$meta$n_waypoints[k], 2)
  wp <- res$waypoints[[k]]
  expect_equal(wp$x, c(7.3, 152.7), tolerance = 1e-6)
  expect_equal(wp$y[1], wp$y[2], tolerance = 1e-6)
  # the S ports and the channel lie on the same side of the endpoints, so
  # the path never rises above the chord
  expect_true(all(path$y <= 55 + 1e-9))
  expect_lte(wp$y[1], 63 - r_full + 1e-9)
  expect_gte(path_min_dist(path, node_xy(scene, "m")), r_full - verify_tol)
  runs <- straight_runs(path)
  expect_gte(runs$length[1], cap_default + rc_default - 1e-9)
  expect_gte(runs$length[nrow(runs)], cap_default + rc_default - 1e-9)

  for (lab in c("x->m", "m->y")) {
    i <- edge_index(scene, lab)
    expect_equal(res$meta$mode[i], "orthogonal", label = lab)
    expect_equal(res$meta$n_waypoints[i], 2, label = lab)
  }
})

test_that("orthogonal fan: edges sharing a source port share one vertical segment", {
  scene <- fan_scene()
  res <- ortho(scene)
  # a->b, a->c, a->d leave a's E port together and form one hyperedge
  # segment over the union interval [25, 85]; the single slot of gap 1 is
  # 36.1 + (63.9 - 36.1) / 2 = 50
  ab <- slot_xs(res$paths[[edge_index(scene, "a->b")]], c(20, 80))
  ad <- slot_xs(res$paths[[edge_index(scene, "a->d")]], c(20, 80))
  expect_length(ab, 1)
  expect_length(ad, 1)
  expect_equal(ab, ad)
  expect_equal(ab, 50, tolerance = 1e-6)
})

# The fan plus a node b2 at (80, 70) and an edge a->b2. a->b covers
# [55, 85] and a->b2 [55, 70], a proper overlap on the same side of a, which
# only hyperedge merging can place in one slot. fan_scene() itself stays as
# in the worked trace.
fan_same_side_scene <- function() {
  scene <- fan_scene()
  scene$nodes <- rbind(scene$nodes, mm_nodes("b2", 80, 70))
  scene$edges <- rbind(scene$edges, mm_edges("a", "b2"))
  scene
}

test_that("orthogonal fan: edges leaving one port to the same side merge into one segment", {
  scene <- fan_same_side_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  # Without merging, a->b and a->b2 would need two slots in gap 1 at
  # 36.1 + 27.8 / 3 = 45.367 and 36.1 + 2 * 27.8 / 3 = 54.633. As one
  # segment over the union [25, 85] with a->d they share the single slot
  # 36.1 + 27.8 / 2 = 50.
  labs <- c("a->b", "a->b2", "a->d")
  xs <- vapply(
    labs,
    function(lab) {
      x <- slot_xs(res$paths[[edge_index(scene, lab)]], c(20, 80))
      expect_length(x, 1)
      x
    },
    numeric(1)
  )
  expect_equal(unname(xs), rep(50, 3), tolerance = 1e-6)

  # the vertical runs of the merged edges sit at bit-identical x
  run_x <- function(lab) {
    runs <- straight_runs(res$paths[[edge_index(scene, lab)]])
    runs$coord[runs$axis == "v"]
  }
  expect_identical(run_x("a->b"), run_x("a->b2"))
  expect_identical(run_x("a->b"), run_x("a->d"))

  # the rest of the fan is unchanged by the extra target
  base <- ortho(fan_scene())
  for (lab in edge_labels(fan_scene()$edges)) {
    i <- edge_index(scene, lab)
    j <- edge_index(fan_scene(), lab)
    expect_lt(polyline_hausdorff(res$paths[[i]], base$paths[[j]]), 1e-6)
  }

  # The four-layer fixture has no same-side pair to check in the same way:
  # p->q1 and p->q2, q1->s1 and q1->s2, and q2->s2 and q2->s3 all leave
  # their port to opposite sides and share one trunk because they share the
  # port. Its segments from different sources that meet at a port's y (q1's
  # and q2's in gap 2) must not share a slot; that is pinned below.
})

test_that("orthogonal: a vertical chord within one layer stays a straight chord", {
  # u and v share a layer 40 mm apart in y; w sits in a second layer so the
  # layer axis is inferable
  scene <- list(
    nodes = mm_nodes(c("u", "v", "w"), c(20, 20, 80), c(20, 60, 40)),
    edges = mm_edges(c("u", "w"), c("v", "v")),
    bounds = c(0, 0, 160, 110)
  )
  res <- ortho(scene)
  ends <- edge_endpoints(scene, 1)

  expect_false(res$meta$routed[1])
  expect_equal(res$meta$mode[1], "straight")
  expect_equal(res$meta$n_waypoints[1], 0)
  expect_identical(nrow(res$waypoints[[1]]), 0L)
  expect_straight_path(res$paths[[1]], ends$from, ends$to)

  # the oblique edge into v is drawn orthogonally as usual
  expect_equal(res$meta$mode[2], "orthogonal")
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
})

# Two nodes 3 mm apart in x fall into one layer (tol_layer = 6), so a chord
# between them spans no gap and has nothing to route through: it stays the
# straight chord even though it is oblique. The layer axis is named because
# the y values are exact clusters and the x values are not.
same_layer_scene <- function() {
  list(
    nodes = mm_nodes(
      c("a", "b", "c", "d"),
      c(20, 23, 80, 80),
      c(50, 80, 60, 20)
    ),
    edges = mm_edges(c("a", "a", "b"), c("b", "c", "d")),
    bounds = c(0, 0, 160, 110)
  )
}

# The orthogonal scene predicates on every edge but the same-layer chord,
# which is straight without being horizontal or vertical.
expect_orthogonal_scene_without <- function(scene, res, drop) {
  keep <- setdiff(seq_len(nrow(scene$edges)), drop)
  sub <- scene
  sub$edges <- scene$edges[keep, , drop = FALSE]
  rownames(sub$edges) <- NULL
  res_sub <- list(
    paths = res$paths[keep],
    meta = res$meta[keep, , drop = FALSE],
    waypoints = res$waypoints[keep]
  )
  expect_orthogonal_scene(sub, res_sub, stub_always = TRUE)
}

test_that("orthogonal: an oblique chord between two nodes of one layer stays straight", {
  scene <- same_layer_scene()
  layers <- infer_layers(scene$nodes, r_default)
  expect_equal(layers$id[1:2], c(1, 1))
  # the call itself must not error
  res <- route_scene(
    scene,
    mode = "orthogonal",
    opts = route_opts(r_default, layer_axis = "x")
  )

  i <- edge_index(scene, "a->b")
  ends <- edge_endpoints(scene, i)
  expect_false(res$meta$routed[i])
  expect_equal(res$meta$mode[i], "straight")
  expect_true(is.na(res$meta$side[i]))
  expect_equal(res$meta$n_waypoints[i], 0)
  expect_identical(nrow(res$waypoints[[i]]), 0L)
  expect_straight_path(res$paths[[i]], ends$from, ends$to)

  for (lab in c("a->c", "b->d")) {
    j <- edge_index(scene, lab)
    expect_equal(res$meta$mode[j], "orthogonal", label = lab)
    expect_equal(res$meta$n_waypoints[j], 2, label = lab)
  }
  expect_orthogonal_scene_without(scene, res, i)
})

test_that("orthogonal: a same-layer chord in the last layer stays straight like one in the first", {
  scene <- list(
    nodes = mm_nodes(
      c("c", "d", "a", "b"),
      c(20, 20, 80, 83),
      c(60, 20, 50, 80)
    ),
    edges = mm_edges(c("c", "d", "a"), c("a", "b", "b")),
    bounds = c(0, 0, 160, 110)
  )
  res <- route_scene(
    scene,
    mode = "orthogonal",
    opts = route_opts(r_default, layer_axis = "x")
  )

  i <- edge_index(scene, "a->b")
  ends <- edge_endpoints(scene, i)
  expect_false(res$meta$routed[i])
  expect_equal(res$meta$mode[i], "straight")
  expect_true(is.na(res$meta$side[i]))
  expect_equal(res$meta$n_waypoints[i], 0)
  expect_straight_path(res$paths[[i]], ends$from, ends$to)

  for (lab in c("c->a", "d->b")) {
    j <- edge_index(scene, lab)
    expect_equal(res$meta$mode[j], "orthogonal", label = lab)
    expect_equal(res$meta$n_waypoints[j], 2, label = lab)
  }
  expect_orthogonal_scene_without(scene, res, i)
})

test_that("orthogonal fan: edges entering the same port share their last run", {
  scene <- fan_scene()
  res <- ortho(scene)
  be <- res$paths[[edge_index(scene, "b->e")]]
  ce <- res$paths[[edge_index(scene, "c->e")]]

  # b->e turns down at x = 110 onto y = 55 and runs into e's W port along
  # the chord of c->e, so one arrowhead is visible
  tail <- be[be$x > 110 + rc_default + 1e-6, , drop = FALSE]
  expect_gt(nrow(tail), 0)
  expect_true(all(abs(tail$y - 55) < 1e-6))
  expect_lt(max(point_polyline_dist(tail, ce)), 1e-6)
  runs <- straight_runs(be)
  last <- runs[nrow(runs), ]
  expect_equal(last$axis, "h")
  expect_equal(last$coord, 55, tolerance = 1e-6)
  # from the tangent point at 110 + 2.1 to the centre of e at 140
  expect_gte(last$length, 30 - rc_default - 1e-6)
})

# Two layers 40 mm apart leave a band [20 + 16.1, 60 - 16.1] = [36.1, 43.9]
# of 7.8 mm. Each adjacent pair of the four staircase edges overlaps in y
# (a1/a3 and a2/a4 only touch, a1/a4 are disjoint), which forces the chain
# a4 -> a3 -> a2 -> a1 and four slots: an even spread would put them
# 7.8 / 5 = 1.56 mm apart, closer than sep_e, so the slots fall back to the
# gap midpoint 40 spaced by 3.6.
narrow_band_scene <- function() {
  list(
    nodes = mm_nodes(
      c("a1", "a2", "a3", "a4", "b1", "b2", "b3", "b4"),
      c(20, 20, 20, 20, 60, 60, 60, 60),
      c(20, 35, 50, 65, 50, 65, 80, 95)
    ),
    edges = mm_edges(c("a1", "a2", "a3", "a4"), c("b1", "b2", "b3", "b4")),
    bounds = c(0, 0, 80, 110)
  )
}

test_that("orthogonal: a band too narrow for its slots centres them on the gap midpoint", {
  scene <- narrow_band_scene()
  # the call itself must not error
  res <- ortho(scene)

  expect_true(all(res$meta$routed))
  expect_true(all(res$meta$mode == "orthogonal"))
  expect_false(any(res$meta$clearance_ok))
  for (i in seq_len(nrow(scene$edges))) {
    ends <- edge_endpoints(scene, i)
    expect_exact_endpoints(res$paths[[i]], ends$from, ends$to)
    expect_orthogonal_outside_corners(
      res$paths[[i]],
      res$waypoints[[i]],
      rc_default
    )
  }

  xs <- vapply(
    res$paths,
    function(path) {
      x <- slot_xs(path, c(20, 60))
      expect_length(x, 1)
      x
    },
    numeric(1)
  )
  # 40 + (-1.5, -0.5, 0.5, 1.5) * 3.6
  expect_equal(sort(xs), c(34.6, 38.2, 41.8, 45.4), tolerance = 1e-6)
  # A staircase nests without crossings when the higher edge runs further
  # left: with a1->b1 left of a2->b2, a2's run at y = 35 would cross a1's
  # vertical and a1's run at y = 50 would cross a2's, so a2 goes left of a1,
  # a3 left of a2, and a4 left of a3.
  expect_equal(xs, c(45.4, 41.8, 38.2, 34.6), tolerance = 1e-6)
})

test_that("orthogonal: layers along y are transposed in and the result transposed back", {
  base <- fan_scene()
  ref <- ortho(base)
  rotated <- base
  rotated$nodes <- swap_xy(base$nodes)
  rotated$bounds <- base$bounds[c(2, 1, 4, 3)]
  # both axes of the rotated fan have exact clusters, so the layer axis is
  # named rather than inferred
  res <- route_scene(
    rotated,
    mode = "orthogonal",
    opts = route_opts(r_default, layer_axis = "y")
  )

  expect_identical(res$meta$routed, ref$meta$routed)
  expect_identical(res$meta$mode, ref$meta$mode)
  # side is reported in the canonical orientation
  expect_identical(res$meta$side, ref$meta$side)
  expect_identical(res$meta$n_waypoints, ref$meta$n_waypoints)

  for (i in seq_along(ref$paths)) {
    ends <- edge_endpoints(rotated, i)
    expect_exact_endpoints(res$paths[[i]], ends$from, ends$to)
    expect_lt(polyline_hausdorff(swap_xy(res$paths[[i]]), ref$paths[[i]]), 1e-6)
    expect_equal(res$waypoints[[i]]$x, ref$waypoints[[i]]$y, tolerance = 1e-9)
    expect_equal(res$waypoints[[i]]$y, ref$waypoints[[i]]$x, tolerance = 1e-9)
  }
})

test_that("orthogonal: a reversed edge returns the reversed polyline of its forward twin", {
  ref <- ortho(fan_scene())
  scene <- fan_scene()
  scene$edges <- mm_edges(
    c("a", "a", "a", "b", "c", "e"),
    c("b", "c", "d", "e", "e", "a")
  )
  res <- ortho(scene)
  ends <- edge_endpoints(scene, 6)

  expect_true(res$meta$routed[6])
  expect_equal(res$meta$mode[6], "orthogonal")
  expect_identical(res$meta$side[6], ref$meta$side[6])
  expect_identical(res$meta$n_waypoints[6], ref$meta$n_waypoints[6])
  expect_exact_endpoints(res$paths[[6]], ends$from, ends$to)
  expect_lt(polyline_hausdorff(res$paths[[6]], ref$paths[[6]]), 1e-6)
  expect_equal(sort(res$waypoints[[6]]$x), sort(ref$waypoints[[6]]$x))
  expect_equal(sort(res$waypoints[[6]]$y), sort(ref$waypoints[[6]]$y))

  for (i in 1:5) {
    expect_identical(res$paths[[i]], ref$paths[[i]])
  }
})

expect_orthogonal_order_invariant <- function(scene, prefix) {
  res <- ortho(scene)
  expect_identical(ortho(scene), res)

  # reverse the node rows and rotate the edge rows
  n_edges <- nrow(scene$edges)
  edge_perm <- c(seq_len(n_edges)[-1], 1L)
  shuffled <- scene
  shuffled$nodes <- scene$nodes[rev(seq_len(nrow(scene$nodes))), ]
  shuffled$edges <- scene$edges[edge_perm, ]
  rownames(shuffled$nodes) <- NULL
  rownames(shuffled$edges) <- NULL
  res2 <- ortho(shuffled)

  keys <- edge_labels(scene$edges)
  keys2 <- edge_labels(shuffled$edges)
  for (i in seq_len(n_edges)) {
    j <- match(keys[i], keys2)
    label <- paste(prefix, keys[i])
    expect_identical(res2$paths[[j]], res$paths[[i]], label = label)
    expect_identical(res2$waypoints[[j]], res$waypoints[[i]], label = label)
    expect_identical(res2$meta$side[j], res$meta$side[i], label = label)
    expect_identical(res2$meta$mode[j], res$meta$mode[i], label = label)
    expect_identical(
      res2$meta$clearance_ok[j],
      res$meta$clearance_ok[i],
      label = label
    )
  }
}

test_that("orthogonal: routing is deterministic and invariant to row order", {
  expect_orthogonal_order_invariant(fan_scene(), "fan")
  expect_orthogonal_order_invariant(four_layer_scene(), "four-layer")
})

test_that("orthogonal: bends scale exactly with k while rc stays inside its clamp", {
  # rc = clamp(0.35 r k, 0.8, 2.5) scales with k only for 0.35 * 6 k in
  # [0.8, 2.5], that is k in [0.381, 1.19]; k = 3 would hit the ceiling and
  # is excluded
  for (make in list(fan_scene, four_layer_scene)) {
    base <- make()
    ref <- route_scene(base, cap = 8, mode = "orthogonal")

    for (k in c(0.5, 1)) {
      res <- route_scene(scale_scene(base, k), cap = 8 * k, mode = "orthogonal")
      expect_identical(res$meta$routed, ref$meta$routed)
      expect_identical(res$meta$side, ref$meta$side)
      expect_identical(res$meta$mode, ref$meta$mode)
      expect_identical(res$meta$n_waypoints, ref$meta$n_waypoints)

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
        # sample counts may differ between scales, so the sampled paths are
        # compared at 0.05 mm like the spline scale test
        scaled <- pt(res$paths[[i]]$x / k, res$paths[[i]]$y / k)
        expect_lt(polyline_hausdorff(scaled, ref$paths[[i]]), 0.05)
      }
    }
  }
})

test_that("spline and straight mode ignore the corners option", {
  scene <- fan_scene()
  sharp <- route_opts(r_default, corners = "sharp")
  expect_identical(route_scene(scene, opts = sharp), route_scene(scene))
  expect_identical(
    route_scene(scene, mode = "straight", opts = sharp),
    route_scene(scene, mode = "straight")
  )
})

test_that("orthogonal: parallel duplicate edges are spread sep_m apart with fixed endpoints", {
  scene <- fan_scene()
  scene$edges <- rbind(scene$edges, mm_edges("a", "b"))
  res <- ortho(scene, corners = "sharp")
  from <- node_xy(scene, "a")
  to <- node_xy(scene, "b")
  dup <- c(1L, 7L)

  for (i in dup) {
    ends <- edge_endpoints(scene, i)
    expect_true(res$meta$routed[i])
    expect_equal(res$meta$mode[i], "orthogonal")
    expect_gte(nrow(res$paths[[i]]), 4)
    expect_exact_endpoints(res$paths[[i]], ends$from, ends$to)
  }
  # the upper bend of a->b lies 13.4 mm left of its chord; the two copies
  # are translated sep_m = 6 mm apart and stay on that side
  apex <- vapply(
    res$paths[dup],
    function(path) max(chord_offset(path, from, to)),
    numeric(1)
  )
  expect_equal(abs(diff(apex)), 6, tolerance = 0.05)
  expect_true(all(apex > 0))
  expect_gt(polyline_hausdorff(res$paths[[1]], res$paths[[7]]), 1)
})

test_that("canonical DAGs: orthogonal mode is axis-aligned with exact endpoints and separated slots", {
  n_orthogonal <- 0L
  for (panel in canonical_panels) {
    for (nm in names(canonical_dag_specs)) {
      scene <- canonical_scene(nm, panel)
      # the call itself must not error
      res <- ortho(scene)
      expect_length(res$paths, nrow(scene$edges))
      prefix <- sprintf("%s at %d x %d: ", nm, panel[1], panel[2])
      # gaps narrower than 2 stub fall back to midpoint slots and report
      # clearance_ok = FALSE, so the stub and clearance checks are gated
      expect_orthogonal_scene(scene, res, stub_always = FALSE, prefix = prefix)

      for (i in seq_len(nrow(scene$edges))) {
        ends <- edge_endpoints(scene, i)
        label <- paste0(prefix, edge_labels(scene$edges)[i])
        # every chord that is neither horizontal nor vertical is orthogonal
        oblique <- abs(ends$from[2] - ends$to[2]) >= 1e-3 &&
          abs(ends$from[1] - ends$to[1]) >= 1e-3
        if (oblique) {
          expect_equal(res$meta$mode[i], "orthogonal", label = label)
        }
      }
      n_orthogonal <- n_orthogonal + sum(res$meta$mode == "orthogonal")
    }
  }
  expect_gt(n_orthogonal, 0)
})

# Orthogonal ports, slots, and channels ------------------------------------------

# Total length over which the axis-aligned runs of two paths coincide: runs
# on the same axis at the same constant coordinate contribute the overlap of
# their extents.
shared_run_length <- function(path_a, path_b, tol = 1e-6) {
  ra <- straight_runs(path_a, tol)
  rb <- straight_runs(path_b, tol)
  total <- 0
  for (i in seq_len(nrow(ra))) {
    for (j in seq_len(nrow(rb))) {
      if (ra$axis[i] != rb$axis[j] || abs(ra$coord[i] - rb$coord[j]) > tol) {
        next
      }
      overlap <- min(ra$hi[i], rb$hi[j]) - max(ra$lo[i], rb$lo[j])
      if (overlap > tol) {
        total <- total + overlap
      }
    }
  }
  total
}

# Arc length of the initial stretch of one path that lies on the other, up
# to the last sample still on it, taken from whichever path measures it
# longer so that a branch point that is a vertex of either path is exact.
common_prefix_length <- function(path_a, path_b, tol = 1e-6) {
  prefix_on <- function(pa, pb) {
    pa <- dedupe_path(pa)
    on <- point_polyline_dist(pa, pb) <= tol
    k <- which(!on)[1]
    if (is.na(k)) {
      k <- nrow(pa) + 1L
    }
    if (k <= 2) {
      return(0)
    }
    idx <- seq_len(k - 1)
    sum(sqrt(diff(pa$x[idx])^2 + diff(pa$y[idx])^2))
  }
  max(prefix_on(path_a, path_b), prefix_on(path_b, path_a))
}

common_suffix_length <- function(path_a, path_b, tol = 1e-6) {
  common_prefix_length(rev_path(path_a), rev_path(path_b), tol)
}

# Points of a path whose arc distance from its start (or its end) lies in
# [lo, hi], in path order.
arc_window <- function(path, lo, hi, from_end = FALSE) {
  path <- dedupe_path(path)
  if (from_end) {
    path <- rev_path(path)
  }
  s <- c(0, cumsum(sqrt(diff(path$x)^2 + diff(path$y)^2)))
  path[s >= lo - 1e-9 & s <= hi + 1e-9, , drop = FALSE]
}

# The run-sharing contract over a routed scene: two edges leaving one source
# port share exactly their common trunk, two entering one target port share
# exactly their merged last run, and every other pair shares nothing. The
# equalities need sharp corners, since rounding cuts rc off every run at a
# corner while the sampled prefix keeps the corner arc; with rounded corners
# a shared run can only be shorter than the sharp one.
expect_runs_owned <- function(scene, res, sharp, prefix = "") {
  labels <- edge_labels(scene$edges)
  n <- length(labels)
  ports <- lapply(seq_len(n), function(i) {
    ends <- edge_endpoints(scene, i)
    if (ends$from[1] <= ends$to[1]) {
      c(scene$edges$from[i], scene$edges$to[i])
    } else {
      c(scene$edges$to[i], scene$edges$from[i])
    }
  })
  for (a in seq_len(n - 1)) {
    for (b in (a + 1):n) {
      label <- paste0(prefix, labels[a], " | ", labels[b])
      pa <- res$paths[[a]]
      pb <- res$paths[[b]]
      shared <- shared_run_length(pa, pb)
      expected <- 0
      if (ports[[a]][1] == ports[[b]][1]) {
        expected <- expected + common_prefix_length(pa, pb)
      }
      if (ports[[a]][2] == ports[[b]][2]) {
        expected <- expected + common_suffix_length(pa, pb)
      }
      if (sharp || expected == 0) {
        expect_lt(abs(shared - expected), 1e-6, label = label)
      } else {
        expect_lte(shared, expected + 1e-6, label = label)
      }
    }
  }
}

# A collinear chain with two skip edges: a->c is blocked by b and c->e by d,
# so both channel above at max(55 + 9, 55 + 16.1) = 71.1 (S/N: 16.1 / 6 + 2
# bends = 4.68, against an E/W run at 55 + 9 = 64 with four bends, 9 / 6 + 4
# = 5.5; nothing is congested and the tie between the sides goes above).
# a->c arrives at c's N port and c->e leaves from it.
chain_scene <- function() {
  list(
    nodes = mm_nodes(
      c("a", "b", "c", "d", "e"),
      c(20, 50, 80, 110, 140),
      rep(55, 5)
    ),
    edges = mm_edges(
      c("a", "b", "c", "d", "a", "c"),
      c("b", "c", "d", "e", "c", "e")
    ),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("orthogonal ports: an arrival and a departure on one side of a node use distinct ports", {
  scene <- chain_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  ac <- edge_index(scene, "a->c")
  ce <- edge_index(scene, "c->e")
  expect_equal(res$meta$mode[ac], "orthogonal")
  expect_equal(res$meta$mode[ce], "orthogonal")
  expect_equal(res$meta$side[ac], 1)
  expect_equal(res$meta$side[ce], 1)
  expect_true(res$meta$clearance_ok[ac])
  expect_true(res$meta$clearance_ok[ce])

  runs_ac <- straight_runs(res$paths[[ac]])
  runs_ce <- straight_runs(res$paths[[ce]])
  v_ac <- runs_ac[runs_ac$axis == "v", , drop = FALSE]
  v_ce <- runs_ce[runs_ce$axis == "v", , drop = FALSE]
  expect_gte(nrow(v_ac), 2)
  expect_gte(nrow(v_ce), 2)
  # a's N port carries only a departure and e's only an arrival, so those
  # stubs stay on the centre lines x = 20 and x = 140
  expect_equal(v_ac$coord[1], 20, tolerance = 1e-6)
  expect_equal(v_ce$coord[nrow(v_ce)], 140, tolerance = 1e-6)

  # At c the arrival of a->c and the departure of c->e are parallel
  # verticals beside the centre line x = 80 and at least sep_e = 3.6 apart,
  # so no vertical carries two edges in opposite directions. Both are port
  # stubs, within sep_e of c's layer.
  x_arr <- v_ac$coord[nrow(v_ac)]
  x_dep <- v_ce$coord[1]
  expect_lt(abs(x_arr - 80), sep_e_default)
  expect_lt(abs(x_dep - 80), sep_e_default)
  expect_gte(abs(x_dep - x_arr), sep_e_default - 1e-6)
  expect_equal(shared_run_length(res$paths[[ac]], res$paths[[ce]]), 0)

  # the visible arrowhead zone of a->c, the 8 mm of path after the resected
  # cap of 8 mm, keeps sep_e from the visible part of c->e (everything past
  # its own cap)
  head <- arc_window(
    res$paths[[ac]],
    cap_default,
    cap_default + 8,
    from_end = TRUE
  )
  visible_ce <- arc_window(res$paths[[ce]], cap_default, Inf)
  expect_gt(nrow(head), 0)
  expect_gte(
    min(point_polyline_dist(head, visible_ce)),
    sep_e_default - 1e-6
  )
})

test_that("orthogonal slots: touching segments from different sources never share an x", {
  # In gap 2 (x 60 to 100) q1's hyperedge covers [55, 90] and q2's covers
  # [20, 55]; they meet at s2's y. One slot for both drew a continuous
  # vertical from q2 up to q1 that read as q1->s3 and q2->s1. The band
  # [76.1, 83.9] is 7.8 mm wide, so two slots at least sep_e apart both
  # fit inside it. Gap 3 (100 to 140) is the same picture with s1->t over
  # [55, 90] and s3->t over [20, 55] meeting at t's y.
  scene <- four_layer_scene()
  res <- ortho(scene)
  x_of <- function(lab, gap) {
    x <- slot_xs(res$paths[[edge_index(scene, lab)]], gap)
    expect_length(x, 1)
    x
  }

  gap2 <- c(60, 100)
  band2 <- gap2 + c(stub_default, -stub_default)
  q1 <- c(x_of("q1->s1", gap2), x_of("q1->s2", gap2))
  q2 <- c(x_of("q2->s2", gap2), x_of("q2->s3", gap2))
  # a hyperedge still shares its trunk
  expect_equal(q1[1], q1[2])
  expect_equal(q2[1], q2[2])
  expect_gte(abs(q1[1] - q2[1]), sep_e_default - 1e-6)
  expect_true(all(c(q1, q2) >= band2[1] - 1e-6 & c(q1, q2) <= band2[2] + 1e-6))

  gap3 <- c(100, 140)
  band3 <- gap3 + c(stub_default, -stub_default)
  s1 <- x_of("s1->t", gap3)
  s3 <- x_of("s3->t", gap3)
  expect_gte(abs(s1 - s3), sep_e_default - 1e-6)
  expect_true(all(c(s1, s3) >= band3[1] - 1e-6 & c(s1, s3) <= band3[2] + 1e-6))

  # the scene predicate treats meeting intervals like overlapping ones
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
})

# A five-node row at y = 100 with a second row 16 mm beneath it, so the S
# side is not extreme and the two skip edges d->f (blocked by e) and c->h
# (blocked by d, e, f) both channel above. Their spans nest, d->f inside
# c->h, so stacking them costs no crossing: d->f, placed first as the
# shorter, runs at max(100 + 9, 100 + 16.1) = 116.1 and c->h sep_e outside
# it at 119.7 (three crossed layers displaced 19.7 each: 59.1 / 6 + 2 bends
# = 11.85, against an E/W run under both rows at 84 - 9 = 75, 75 / 6 + 4 =
# 16.5). The rows are 16 mm apart, so no run fits between them: a run
# needs R = 9 from each row and 100 - 9 < 84 + 9.
nested_row_scene <- function() {
  list(
    nodes = mm_nodes(
      c("c", "d", "e", "f", "h", "b1", "b2", "b3", "b4", "b5"),
      c(20, 60, 100, 140, 180, 20, 60, 100, 140, 180),
      c(rep(100, 5), rep(84, 5))
    ),
    edges = mm_edges(
      c("c", "d", "e", "f", "d", "c", "b1", "b2", "b3", "b4"),
      c("d", "e", "f", "h", "f", "h", "b2", "b3", "b4", "b5")
    ),
    bounds = c(0, 0, 200, 130)
  )
}

# The horizontal run of a path that spans the layer at `x_cross`.
channel_run <- function(path, x_cross) {
  runs <- straight_runs(path)
  h <- runs[
    runs$axis == "h" & runs$lo < x_cross - 1e-6 & runs$hi > x_cross + 1e-6,
    ,
    drop = FALSE
  ]
  expect_equal(nrow(h), 1)
  h
}

test_that("orthogonal channels: nested same-side channels stack sep_e apart with the shorter inside", {
  scene <- nested_row_scene()
  res <- ortho(scene, corners = "sharp")
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  df <- edge_index(scene, "d->f")
  ch <- edge_index(scene, "c->h")
  for (i in c(df, ch)) {
    expect_equal(res$meta$mode[i], "orthogonal")
    expect_equal(res$meta$side[i], 1)
    expect_true(res$meta$clearance_ok[i])
  }

  run_df <- channel_run(res$paths[[df]], 100)
  run_ch <- channel_run(res$paths[[ch]], 100)
  expect_equal(run_df$coord, 116.1, tolerance = 1e-6)
  expect_equal(run_ch$coord, 119.7, tolerance = 1e-6)
  expect_gte(run_ch$coord - run_df$coord, sep_e_default - 1e-6)
  # each channel is one run from its own N stub to its own N stub
  expect_equal(c(run_df$lo, run_df$hi), c(60, 140), tolerance = 1e-6)
  expect_equal(c(run_ch$lo, run_ch$hi), c(20, 180), tolerance = 1e-6)
  expect_equal(shared_run_length(res$paths[[df]], res$paths[[ch]]), 0)
})

# The row c, e, f, h at y = 100 with a second row at y = 60 beneath it and a
# panel too short for a channel above (116.1 + 3 > 110). c->f (blocked by e)
# and e->h (blocked by f) are E/W spanning edges; both channel runs went
# under the lower row at 60 - 9 = 51 and their gap-2 slots, at 86.1 + 17.8
# / 3 = 92.03 and 97.97, interleaved so the two runs coincided between the
# slots: the picture read as c->h plus e->f. The rows are 40 mm apart, so a
# run fits between them in [60 + 9, 100 - 9] = [69, 91].
row_scene <- function() {
  list(
    nodes = mm_nodes(
      c("c", "e", "f", "h", "b", "d", "g", "k"),
      c(20, 70, 120, 170, 20, 70, 120, 170),
      c(rep(100, 4), rep(60, 4))
    ),
    edges = mm_edges(
      c("c", "e", "f", "c", "e", "b", "d", "g"),
      c("e", "f", "h", "f", "h", "d", "g", "k")
    ),
    bounds = c(0, 0, 190, 110)
  )
}

test_that("orthogonal channels: two edges never share a channel run", {
  scene <- row_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  cf <- edge_index(scene, "c->f")
  eh <- edge_index(scene, "e->h")
  for (i in c(cf, eh)) {
    expect_equal(res$meta$mode[i], "orthogonal")
    expect_true(res$meta$clearance_ok[i])
  }

  # both channel runs pass under the row and above the margin
  run_cf <- channel_run(res$paths[[cf]], 70)
  run_eh <- channel_run(res$paths[[eh]], 120)
  expect_lt(run_cf$coord, 100 - r_full + 1e-6)
  expect_lt(run_eh$coord, 100 - r_full + 1e-6)
  expect_gt(min(run_cf$coord, run_eh$coord), 3)
  # distinct y at least sep_e apart; the spans tie at 100 mm, so the name
  # order puts c->f inside, nearer the row
  expect_gte(abs(run_cf$coord - run_eh$coord), sep_e_default - 1e-6)
  expect_gt(run_cf$coord, run_eh$coord)
  # no horizontal run of one edge overlaps in x with a run of the other at
  # the same y
  expect_equal(shared_run_length(res$paths[[cf]], res$paths[[eh]]), 0)
})

# The canonical complex_chain layout on a 200 x 110 panel: a (10, 20),
# b (55, 90), c (100, 55), d (145, 90), e (190, 20), with 45 mm gaps so that
# two slots fit in one gap (band 12.8 mm, slots 4.27 mm apart) and both S/N
# channels of a->c lie inside the margin. a->c spans b's layer with a and c
# alone in their layers.
complex_chain_scene <- function() {
  list(
    nodes = mm_nodes(
      c("a", "b", "c", "d", "e"),
      c(10, 55, 100, 145, 190),
      c(20, 90, 55, 90, 20)
    ),
    edges = mm_edges(
      c("a", "b", "c", "d", "a", "b", "c", "a"),
      c("b", "c", "d", "e", "c", "d", "e", "e")
    ),
    bounds = c(0, 0, 200, 110)
  )
}

test_that("orthogonal runs: no segment carries two edges unless they share a port", {
  scenes <- list(
    fan = fan_scene(),
    "four-layer" = four_layer_scene(),
    chain = chain_scene(),
    "nested row" = nested_row_scene(),
    row = row_scene(),
    "complex chain" = complex_chain_scene(),
    # w -> x -> z -> y with w -> z and x -> y: x->y descends to y's row where
    # w->z runs along it toward its own ascent, a four-way junction on a
    # segment that neither edge owns
    overcontrol = canonical_scene("overcontrol")
  )
  for (nm in names(scenes)) {
    scene <- scenes[[nm]]
    expect_runs_owned(
      scene,
      ortho(scene, corners = "sharp"),
      sharp = TRUE,
      prefix = paste0(nm, " sharp: ")
    )
    expect_runs_owned(
      scene,
      ortho(scene),
      sharp = FALSE,
      prefix = paste0(nm, " rounded: ")
    )
  }

  # the fan's trunk and merge, as a check on the measure itself: a->b and
  # a->d share a's port and the 30 mm from a to the slot at 50; b->e and
  # c->e share e's port and the 30 mm from the slot at 110 to e
  scene <- fan_scene()
  res <- ortho(scene, corners = "sharp")
  ab <- res$paths[[edge_index(scene, "a->b")]]
  ad <- res$paths[[edge_index(scene, "a->d")]]
  be <- res$paths[[edge_index(scene, "b->e")]]
  ce <- res$paths[[edge_index(scene, "c->e")]]
  expect_equal(shared_run_length(ab, ad), 30, tolerance = 1e-6)
  expect_equal(common_prefix_length(ab, ad), 30, tolerance = 1e-6)
  expect_equal(shared_run_length(be, ce), 30, tolerance = 1e-6)
  expect_equal(common_suffix_length(be, ce), 30, tolerance = 1e-6)
})

# The collinear mediator with two more nodes in m's layer at the panel's
# extremes, m2 at (80, 10) and m3 at (80, 90), on a 160 x 100 panel. A
# channel above needs y >= 90 + 9 = 99 but the margin allows at most 97; one
# below needs y <= 10 - 9 = 1 against a floor of 3; the E/W runs past the
# stack's extremes are the same two lines. The router clamped the channel to
# 97, through m3's disc (top at 96), and reported no clearance. The layer's
# free intervals, each disc padded by R, are [19, 46] and [64, 81].
stacked_layer_scene <- function() {
  list(
    nodes = mm_nodes(
      c("x", "m", "y", "m2", "m3"),
      c(7.3, 80, 152.7, 80, 80),
      c(55, 55, 55, 10, 90)
    ),
    edges = mm_edges(c("x", "m", "x"), c("m", "y", "y")),
    bounds = c(0, 0, 160, 100)
  )
}

test_that("orthogonal channels: a channel that would cut a disc gives way to a run through a free interval", {
  scene <- stacked_layer_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  for (lab in c("x->m", "m->y")) {
    expect_equal(res$meta$mode[edge_index(scene, lab)], "straight", label = lab)
  }

  k <- edge_index(scene, "x->y")
  path <- res$paths[[k]]
  expect_equal(res$meta$mode[k], "orthogonal")
  expect_true(res$meta$routed[k])
  expect_true(res$meta$clearance_ok[k])
  # E/W through the gaps: one vertical run in each
  expect_length(slot_xs(path, c(7.3, 80)), 1)
  expect_length(slot_xs(path, c(80, 152.7)), 1)
  # the run past the stack sits inside a free interval of the middle layer
  mid <- channel_run(path, 80)
  inside <- (mid$coord >= 19 - 1e-6 && mid$coord <= 46 + 1e-6) ||
    (mid$coord >= 64 - 1e-6 && mid$coord <= 81 + 1e-6)
  expect_true(inside)
  expect_gte(path_min_clearance(scene, k, path), r_full - verify_tol)
  for (nm in c("m", "m2", "m3")) {
    expect_gte(path_min_dist(path, node_xy(scene, nm)), r_full - verify_tol)
  }

  # nothing in the scene is clamped to the margin: every path keeps more
  # than m = 3 from the bounds and reports clearance
  expect_true(all(res$meta$clearance_ok))
  for (p in res$paths) {
    expect_gt(min(p$y), 3 + 1e-6)
    expect_lt(max(p$y), 100 - 3 - 1e-6)
    expect_gt(min(p$x), 3 + 1e-6)
    expect_lt(max(p$x), 160 - 3 - 1e-6)
  }
})

# The collinear mediator with z at (7.3, 75) above x in x's layer. x is not
# the top of its layer, so there is no N channel (one would have to pass
# z's disc from x's N port); x->y takes the S channel at min(55 - 9,
# 55 - 16.1) = 38.9 (S/N below 16.1 / 6 + 2 bends = 4.68, against an E/W
# run at 46 or 64 with four bends, 9 / 6 + 4 = 5.5).
flanked_source_scene <- function() {
  list(
    nodes = mm_nodes(
      c("x", "m", "y", "z"),
      c(7.3, 80, 152.7, 7.3),
      c(55, 55, 55, 75)
    ),
    edges = mm_edges(c("x", "m", "x"), c("m", "y", "y")),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("orthogonal channels: a node beside the source in its layer rules out that side", {
  scene <- flanked_source_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  k <- edge_index(scene, "x->y")
  path <- res$paths[[k]]
  expect_equal(res$meta$mode[k], "orthogonal")
  expect_true(res$meta$clearance_ok[k])
  expect_equal(res$meta$side[k], -1)
  expect_equal(res$meta$n_waypoints[k], 2)
  wp <- res$waypoints[[k]]
  expect_equal(wp$x, c(7.3, 152.7), tolerance = 1e-6)
  expect_equal(wp$y, c(38.9, 38.9), tolerance = 1e-6)
  # the path never rises above the chord, so it never comes near z
  expect_true(all(path$y <= 55 + 1e-9))
  expect_gte(path_min_dist(path, node_xy(scene, "z")), r_full - verify_tol)
  expect_gte(path_min_dist(path, node_xy(scene, "m")), r_full - verify_tol)
})

# Bend-priced candidates -----------------------------------------------------------

test_that("route_opts() prices a bend at one reference radius by default", {
  expect_equal(route_opts(r_default)$bend_penalty, 1)
  expect_equal(route_opts(r_default, bend_penalty = 0)$bend_penalty, 0)
  expect_equal(route_opts(r_default, bend_penalty = 2.5)$bend_penalty, 2.5)
  expect_equal(route_opts(3)$bend_penalty, 1)
})

test_that("orthogonal pricing: the collinear mediator keeps its two-bend channel until bends are free", {
  # S/N above: channel at max(55 + 9, 55 + 16.1) = 71.1, displacing the
  # chord by 16.1: 16.1 / 6 + 2 * 1 = 4.683. E/W above: run at 55 + 9 = 64
  # with four bends: 9 / 6 + 4 * 1 = 5.5. Below mirrors both, nothing is
  # congested, and the tie between the sides goes above.
  scene <- mediator_scene()
  res <- ortho(scene)
  i <- edge_index(scene, "x->y")
  expect_equal(res$meta$mode[i], "orthogonal")
  expect_equal(res$meta$side[i], 1)
  expect_equal(res$meta$n_waypoints[i], 2)
  wp <- res$waypoints[[i]]
  expect_equal(wp$x, c(7.3, 152.7), tolerance = 1e-6)
  expect_equal(wp$y, c(71.1, 71.1), tolerance = 1e-6)

  # With bend_penalty = 0 the E/W run at 64 costs 1.5 against the channel's
  # 2.683 and wins: four bends, the run past m no lower than 64 and below
  # the channel's 71.1, leaving x's E port and entering y's W port along
  # the chord.
  free <- route_scene(
    scene,
    mode = "orthogonal",
    opts = route_opts(r_default, bend_penalty = 0)
  )
  expect_equal(free$meta$mode[i], "orthogonal")
  expect_true(free$meta$clearance_ok[i])
  expect_equal(free$meta$n_waypoints[i], 4)
  path <- free$paths[[i]]
  mid <- channel_run(path, 80)
  expect_gte(mid$coord, 64 - 1e-6)
  expect_lt(mid$coord, 71.1)
  runs <- straight_runs(path)
  expect_equal(runs$axis[1], "h")
  expect_equal(runs$coord[1], 55, tolerance = 1e-6)
  expect_equal(runs$axis[nrow(runs)], "h")
  expect_equal(runs$coord[nrow(runs)], 55, tolerance = 1e-6)
  expect_length(slot_xs(path, c(7.3, 80)), 1)
  expect_length(slot_xs(path, c(80, 152.7)), 1)
  expect_gte(path_min_dist(path, node_xy(scene, "m")), r_full - verify_tol)
  for (lab in c("x->m", "m->y")) {
    expect_equal(
      free$meta$mode[edge_index(scene, lab)],
      "straight",
      label = lab
    )
  }
})

test_that("orthogonal pricing: a skip edge with a free E/W route takes it instead of a channel loop", {
  # a->c's chord passes x = 55 at y = 37.5. The channel below at
  # min(90 - 9, 20 - 16.1, 55 - 16.1) = 3.9 displaces it by 33.6 and has
  # two bends: 33.6 / 6 + 2 + 4 (e lies below the chord for a->e and c->e)
  # = 11.6. The channel above at 90 + 9 = 99 displaces by 61.5: 10.25 + 2 +
  # 6 (b twice and d above) = 18.25. The E/W route at c's own y = 55 has two
  # bends and displaces by 17.5: 17.5 / 6 + 2 + 4 = 8.92, so it wins; the
  # E/W run at 99 with four bends costs 20.25. Without a bend penalty the
  # loop under a scored 9.6 against the E/W route's 6.92, but S/N candidates
  # were tried first and the loop was drawn 0.9 mm inside the margin. c->e
  # mirrors a->c through d's layer.
  scene <- complex_chain_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  ac <- edge_index(scene, "a->c")
  path <- res$paths[[ac]]
  expect_equal(res$meta$mode[ac], "orthogonal")
  expect_true(res$meta$clearance_ok[ac])
  expect_equal(res$meta$n_waypoints[ac], 2)
  runs <- straight_runs(path)
  # out of a's E port along y = 20, up through gap 1, and into c's W port
  # along y = 55
  expect_equal(runs$axis[1], "h")
  expect_equal(runs$coord[1], 20, tolerance = 1e-6)
  expect_equal(runs$axis[nrow(runs)], "h")
  expect_equal(runs$coord[nrow(runs)], 55, tolerance = 1e-6)
  expect_length(slot_xs(path, c(10, 55)), 1)
  expect_gt(min(path$y), 3 + 1e-6)
  expect_lt(max(path$y), 110 - 3 - 1e-6)

  ce <- edge_index(scene, "c->e")
  path <- res$paths[[ce]]
  expect_equal(res$meta$mode[ce], "orthogonal")
  expect_equal(res$meta$n_waypoints[ce], 2)
  runs <- straight_runs(path)
  expect_equal(runs$axis[1], "h")
  expect_equal(runs$coord[1], 55, tolerance = 1e-6)
  expect_equal(runs$axis[nrow(runs)], "h")
  expect_equal(runs$coord[nrow(runs)], 20, tolerance = 1e-6)
  expect_length(slot_xs(path, c(145, 190)), 1)
  expect_gt(min(path$y), 3 + 1e-6)

  # the level skip chords clear the middle layer by more than R and stay
  # straight
  for (lab in c("a->e", "b->d")) {
    expect_equal(res$meta$mode[edge_index(scene, lab)], "straight", label = lab)
  }
})

test_that("orthogonal pricing: the fan's a->e keeps the channel below at 16", {
  # c sits on the chord, so a->e cannot run at its own y. S/N below at
  # 25 - 9 = 16 displaces by 39: 39 / 6 + 2 bends + 2 (d below the chord)
  # = 10.5. S/N above at 85 + 9 = 94: 6.5 + 2 + 4 (b above, for a->b and
  # b->e) = 12.5. An E/W run at 16 has four bends and shares the fan's trunk
  # through gap 1: 6.5 + 4 + 2 = 12.5; at 94 it costs 14.5.
  scene <- fan_scene()
  res <- ortho(scene)
  i <- edge_index(scene, "a->e")
  expect_equal(res$meta$mode[i], "orthogonal")
  expect_equal(res$meta$side[i], -1)
  expect_equal(res$meta$n_waypoints[i], 2)
  wp <- res$waypoints[[i]]
  expect_equal(wp$x, c(20, 140), tolerance = 1e-6)
  expect_equal(wp$y, c(16, 16), tolerance = 1e-6)
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

# TRUE when the collation locale `loc` can be selected on this platform.
has_collate <- function(loc) {
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old))
  identical(suppressWarnings(Sys.setlocale("LC_COLLATE", loc)), loc)
}

test_that("routing does not depend on the collation locale", {
  # S -> T grazes two nodes at the same chord parameter, B 8.5 mm above and
  # a 8.5 mm below, so the soft nudges tie on t and are ordered by node
  # name. "B" sorts before "a" in the C locale and after it in en_US, so a
  # locale-aware sort swaps the two waypoints and changes the curve.
  scene <- list(
    nodes = mm_nodes(
      c("S", "T", "B", "a"),
      c(0, 100, 50, 50),
      c(0, 0, 8.5, -8.5)
    ),
    edges = mm_edges("S", "T"),
    bounds = c(-10, -60, 110, 60)
  )
  in_c <- withr::with_collate("C", route_scene(scene))
  expect_equal(in_c$meta$mode[1], "soft")
  expect_identical(nrow(in_c$waypoints[[1]]), 2L)

  skip_if_not(
    has_collate("en_US.UTF-8"),
    "the en_US.UTF-8 collation is not available"
  )
  in_en <- withr::with_collate("en_US.UTF-8", route_scene(scene))
  expect_identical(in_en$meta, in_c$meta)
  expect_identical(in_en$waypoints, in_c$waypoints)
  expect_identical(in_en$paths, in_c$paths)
})

# Performance -----------------------------------------------------------------------

test_that("route_edges_mm() routes large_epi at a small device size at interactive speed", {
  skip_on_cran()
  skip_on_ci()
  # Opt-in pin; see test-layout-perf.R for the GGDAG_RUN_PERF_TESTS contract.
  skip_if(
    Sys.getenv("GGDAG_RUN_PERF_TESTS") == "",
    "GGDAG_RUN_PERF_TESTS is not set"
  )
  skip_if_not_installed("bench")

  # large_epi at 100 x 70 is the densest canonical scene: four hard-blocked
  # chords through the spanning and free-bow tiers plus the straight rest
  scene <- canonical_scene("large_epi", c(100, 70))

  timing <- bench::mark(
    route_scene(scene, mode = "spline"),
    iterations = 30,
    filter_gc = FALSE
  )
  # The design target is 5 ms and the router measures 4.7 to 5.0 ms on the
  # development machine, a margin that fails intermittently under load;
  # the gate is 6 ms so that only a real regression trips it.
  expect_lt(as.numeric(timing$median), 0.006)
})
