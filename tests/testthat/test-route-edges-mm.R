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
m_default <- 3
m_min_default <- 1.2

# Orthogonal constants at r = 6: the corner radius rc = clamp(0.35 r, 0.8,
# 2.5) = 2.1, the default edge cap of 8 mm, the nominal stub r + cap + rc =
# 16.1, and the slot separation sep_e = max(0.6 r, 1.5) = 3.6. The arrowhead
# is a literal 2 mm, the same length the label engine reserves for it, and
# the true floor a bend vertex needs past a node centre is
# cap + max(head, rc) + rc = 12.2, since the resect is measured from the
# centre. The ladder tightens the spacing no further than
# sep_min = max(0.25 r, 1.5) = 1.5 and the corner radius no further than 0.8.
cap_default <- 8
rc_default <- 2.1
stub_default <- r_default + cap_default + rc_default
sep_e_default <- 3.6
head_default <- 2
sep_min_default <- 1.5
rc_min_default <- 0.8
stub_min_default <- cap_default + max(head_default, rc_default) + rc_default

# Where the ladder's last rung places a slot, the run left between it and
# the target's layer is the run the arrowhead is drawn on: cap + head = 10
# mm of it holds the whole head. A gap holds that floor and the source's
# soft band together from R_soft + cap + head = 17.2 mm on, which is where
# the target-side floor starts to apply.
head_run_default <- cap_default + head_default
head_run_gap_default <- r_soft + head_run_default

# The drawn head is head_w = 1.3 mm wide, so a port row stays within
# h = r - head_w / 2 = 5.35 mm of the centre line and the whole head is drawn
# on the disc. A stack of arrival rows keeps them while they are at least
# max(sep_e / 2, sep_min) = 1.8 mm apart, and merges when it cannot. A ported
# end's resect is cap - r + sqrt(r^2 - offset^2), which puts every tip
# cap - r = 2 mm from the disc face along its own run.
head_w_default <- 1.3
port_row_max <- r_default - head_w_default / 2
row_floor_default <- max(sep_e_default / 2, sep_min_default)
face_tip_default <- cap_default - r_default
port_resect_at <- function(offset, r = r_default, cap = cap_default) {
  cap - r + sqrt(r^2 - offset^2)
}

# ggarrow resects the last cap = 8 mm of every edge, so the drawn arrowhead
# of an edge occupies the arc of its path from 2 cap to cap mm before its
# target: its head zone. The point at cap is where the head ends, its tip.
# Two tips theta degrees apart at a shared target are 2 cap sin(theta / 2)
# mm apart, so keeping arrivals theta_min = 2 asin(sep_e / (2 cap)) = 26.0
# degrees apart keeps the drawn heads sep_e apart.
theta_min_default <- 2 * asin(sep_e_default / (2 * cap_default)) * 180 / pi

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

# A 120 mm skip edge dead on a node of its own row, with a fourth node
# feeding that node from below. c -> f cannot pass above e, whose disc plus
# R reaches 109 against a 107 mm limit, so it bows below; b -> e arrives 14
# degrees off vertical and its drawn arrowhead occupies the window from
# (76.1, 84.5) to (78.1, 92.2), which a shallow bow skims.
skip_over_head_scene <- function() {
  list(
    nodes = mm_nodes(
      c("c", "e", "f", "b"),
      c(20, 80, 140, 60),
      c(100, 100, 100, 20)
    ),
    edges = mm_edges(c("c", "e", "b", "c"), c("e", "f", "e", "f")),
    bounds = c(0, 0, 160, 110)
  )
}

# A 120 mm horizontal chord dead on n1 and 9.5 mm from n2. The two centres
# are 16 mm apart: at least 2 (r + m_min) = 14.4, so a curve can pass
# between them at the soft margin, but short of the 18 mm that two padded
# discs need for a slot at the full margin.
tight_slot_scene <- function() {
  list(
    nodes = mm_nodes(
      c("S", "T", "n1", "n2"),
      c(20, 140, 80, 80),
      c(50, 50, 43.5, 59.5)
    ),
    edges = mm_edges("S", "T"),
    bounds = c(0, 0, 160, 110)
  )
}

# A 140 mm span-4 chord across three crossed layers whose twelve interior
# edges weave over and under it. Every interior route crosses at least five
# of them and the only way to cross fewer is a deep arch under the stacks,
# so the scene separates a linear crossing price from a saturating one.
dense_span_scene <- function() {
  list(
    nodes = rbind(
      mm_nodes(c("s", "t"), c(10, 150), c(50, 50)),
      mm_nodes(
        c("a1", "a2", "b1", "b2", "c1", "c2"),
        c(45, 45, 80, 80, 115, 115),
        c(30, 74, 50, 90, 30, 74)
      ),
      mm_nodes(c("u", "w"), 80, c(108, 4))
    ),
    edges = mm_edges(
      c(
        "s",
        "s",
        "a1",
        "a1",
        "a2",
        "b1",
        "b2",
        "b2",
        "c1",
        "c2",
        "u",
        "u",
        "u",
        "w",
        "w",
        "w",
        "s"
      ),
      c(
        "a1",
        "a2",
        "b1",
        "b2",
        "b2",
        "c1",
        "c1",
        "c2",
        "t",
        "t",
        "a1",
        "c1",
        "b2",
        "a2",
        "c2",
        "b1",
        "t"
      )
    ),
    bounds = c(0, 0, 160, 110)
  )
}

# The interior edges of dense_span_scene(): the twelve that touch neither
# endpoint of s -> t and so are free to be crossed.
dense_span_interior <- c(3:8, 11:16)

# S -> T is dead on s2 and spans two crowded layers. The candidate that
# ranks first, above the chord, stays inside the panel but its drawn curve
# cannot be repaired clear of q2; the candidate below it verifies.
unverified_slot_scene <- function() {
  list(
    nodes = rbind(
      mm_nodes(c("S", "T"), c(20, 140), c(67, 52)),
      mm_nodes(c("q1", "q2"), 60, c(90, 74)),
      mm_nodes(c("s1", "s2"), 100, c(26, 57))
    ),
    edges = mm_edges("S", "T"),
    bounds = c(0, 0, 160, 110)
  )
}

# S -> T runs along a row 100 mm up a 110 mm panel, dead on B. A bow above B
# needs B + R = 109, past the 107 mm the clearance margin leaves, so the
# free bow can only be drawn by pressing it against the border; below B the
# 21 mm gap to u is a slot only at the soft margin.
border_bow_scene <- function() {
  list(
    nodes = rbind(
      mm_nodes(c("S", "B", "T"), c(20, 80, 140), 100),
      mm_nodes(c("u", "v"), c(80, 60), c(79, 74))
    ),
    edges = mm_edges("S", "T"),
    bounds = c(0, 0, 160, 110)
  )
}

# Three edges arrive at y. a -> y and b -> y come in from the corners at
# 27.9 degrees either side of the row y sits on, and x -> y comes along it.
# x is dead on the c -> y chord, so c -> y detours around it and then has to
# land in a squeeze: no direction within the tangent clamp keeps theta_min
# from all three, and the two arrivals it is caught between are 27.9 degrees
# apart.
dense_arrival_scene <- function() {
  list(
    nodes = mm_nodes(
      c("a", "b", "c", "x", "y"),
      c(62.6826, 62.6826, 14.4652, 110.9, 159.1173),
      c(112.5506, 10.2319, 61.3912, 61.3912, 61.3912)
    ),
    edges = mm_edges(
      c("a", "a", "b", "b", "c", "c", "c", "x"),
      c("x", "y", "x", "y", "a", "b", "y", "y")
    ),
    bounds = c(0, 0, 173.5825, 122.7825)
  )
}

# S -> T runs along a row 10 mm below the top of the panel and is dead on B,
# one layer in from T; u sits 14 mm below the row in the middle layer. The
# candidate above the chord ranks first and clears every disc, but its curve
# leaves the panel margin, so the pool opens; the candidates inside the
# margin all graze u; and the free bow above B reaches y = 109, past the 107
# the margin allows, so it can only be drawn pressed against the border.
margin_bow_scene <- function() {
  list(
    nodes = rbind(
      mm_nodes(c("S", "B", "T"), c(20, 100, 140), 100),
      mm_nodes("u", 80, 86)
    ),
    edges = mm_edges("S", "T"),
    bounds = c(0, 0, 160, 110)
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

# Arc length from the far end of a path to each of its points.
arc_to_end <- function(path) {
  seg <- sqrt(diff(path$x)^2 + diff(path$y)^2)
  rev(cumsum(rev(c(seg, 0))))
}

# The point `s` mm of arc before the end of a path.
point_before_end <- function(path, s) {
  d <- arc_to_end(path)
  k <- min(max(which(d >= s)), nrow(path) - 1L)
  f <- (d[k] - s) / (d[k] - d[k + 1L])
  c(
    path$x[k] + f * (path$x[k + 1L] - path$x[k]),
    path$y[k] + f * (path$y[k + 1L] - path$y[k])
  )
}

# Where the drawn arrowhead of an edge ends: the point `cap` mm of arc
# before its target, since the arrow layer resects that much of the path.
tip <- function(path, cap = cap_default) {
  point_before_end(path, cap)
}

# The head zone of an edge as a two-row data frame: the arc of its path from
# 2 cap to cap mm before its target, which the drawn arrowhead occupies.
head_window <- function(path, cap = cap_default) {
  far <- point_before_end(path, 2 * cap)
  near <- point_before_end(path, cap)
  pt(c(far[1], near[1]), c(far[2], near[2]))
}

# The visible ink of a path: what is left once the cap is resected at each
# end. Samples inside a cap are hidden and cannot collide with anything.
visible_body <- function(path, cap = cap_default) {
  d <- arc_to_end(path)
  path[d <= d[1] - cap & d >= cap, , drop = FALSE]
}

# Closest approach between a polyline and the segment from `a` to `b`, taken
# both ways so that neither sampling density decides the answer.
path_to_segment_dist <- function(path, a, b) {
  n <- nrow(path)
  if (n < 2) {
    return(dist_to_edge(path$x, path$y, a[1], a[2], b[1], b[2]))
  }
  min(vapply(
    seq_len(n - 1),
    function(i) {
      p <- c(path$x[i], path$x[i + 1])
      q <- c(path$y[i], path$y[i + 1])
      min(
        dist_to_edge(p, q, a[1], a[2], b[1], b[2]),
        dist_to_edge(a[1], a[2], p[1], q[1], p[2], q[2]),
        dist_to_edge(b[1], b[2], p[1], q[1], p[2], q[2])
      )
    },
    numeric(1)
  ))
}

# Distance from a path to the head zone of another edge's path.
path_to_head_dist <- function(path, other, cap = cap_default) {
  w <- head_window(other, cap)
  path_to_segment_dist(
    visible_body(path, cap),
    c(w$x[1], w$y[1]),
    c(w$x[2], w$y[2])
  )
}

# Distance between the drawn tips of two edges arriving at one target.
tip_distance <- function(path_a, path_b, cap = cap_default) {
  d <- tip(path_a, cap) - tip(path_b, cap)
  sqrt(sum(d^2))
}

# The angle at a shared target between the arrival directions of two edges,
# measured from each drawn tip to the target centre.
arrival_separation <- function(path_a, path_b, to, cap = cap_default) {
  abs(angle_between(to - tip(path_a, cap), to - tip(path_b, cap)))
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

# An orthogonal path ends at its port's axis point: the node's own coordinate
# along the port's axis, offset onto the port's line. A W or E port's row is
# offset along the layer-crossing axis by at most h = r - head_w / 2, so the
# head drawn along the row stays inside the disc silhouette; an N or S port is
# offset along the layer axis by at most sep_e / 2. A centre port is exact.
# The offset run from the disc face to the axis point is hidden under the
# node, and because the path's last point lies on the run itself, the arrow
# layer draws the head along the run instead of angling it at the centre.
expect_axis_point <- function(point, centre, axis = "x", label = NULL) {
  offset <- point - centre
  crossing <- if (axis == "x") 2L else 1L
  along <- if (axis == "x") 1L else 2L
  exact <- offset[[1]] == 0 && offset[[2]] == 0
  row <- abs(offset[[along]]) < 1e-9 &&
    abs(offset[[crossing]]) <= port_row_max + 1e-9
  stub <- abs(offset[[crossing]]) < 1e-9 &&
    abs(offset[[along]]) <= sep_e_default / 2 + 1e-9
  expect_true(exact || row || stub, label = label)
}

expect_port_endpoints <- function(path, from, to, axis = "x", label = NULL) {
  n <- nrow(path)
  expect_axis_point(c(path$x[1], path$y[1]), from, axis, label = label)
  expect_axis_point(c(path$x[n], path$y[n]), to, axis, label = label)
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

# Every segment whose endpoints both lie outside the rounded corners is
# axis-aligned. Nothing is exempt: a ported end lies on its own run, so an
# orthogonal path carries no oblique segment anywhere, drawn or hidden.
expect_orthogonal_outside_corners <- function(
  path,
  bends,
  rc,
  tol = 1e-6,
  label = NULL
) {
  path <- dedupe_path(path)
  near <- corner_points(path, bends, rc + tol)
  axis <- segment_axes(path, tol)
  outside <- !near[-length(near)] & !near[-1]
  expect_true(all(axis[outside] != "o"), label = label)
}

rev_path <- function(path) {
  path[rev(seq_len(nrow(path))), , drop = FALSE]
}

# Length of the terminal stub at the end of a path: the axis-aligned run that
# reaches the path's last point. Every ported end lies on its own run, at the
# axis point of an offset port or at the centre of a centre port, so the run
# the arrowhead is drawn on is the last run and nothing is skipped. A path
# whose last point is reached by an oblique segment has no stub at all, and a
# short vertical stub under a channel measures as the stub, not the channel.
# After the cap is resected the arrowhead sits on that run, which is why it
# must be straight for at least cap + rc. Pass the reversed path for the stub
# at the start.
end_stub_length <- function(path, tol = 1e-6) {
  path <- dedupe_path(path)
  runs <- straight_runs(path, tol)
  n <- nrow(runs)
  if (n == 0) {
    return(0)
  }
  last <- runs[n, ]
  if (last$to != nrow(path)) {
    return(0)
  }
  last$length
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
  # the scene draws every corner at the smallest radius any of its gaps
  # needed, and the terminal stub follows the same radius
  rc_used <- res$ortho$rc %||% rc_default

  for (i in seq_len(nrow(scene$edges))) {
    label <- paste0(prefix, labels[i])
    ends <- edge_endpoints(scene, i)
    path <- res$paths[[i]]
    expect_port_endpoints(path, ends$from, ends$to, label = label)

    if (res$meta$mode[i] == "straight") {
      expect_false(res$meta$routed[i], label = label)
      expect_identical(nrow(path), 2L, label = label)
      # only a chord level with the row it arrives on, one whose source and
      # path end differ in y by at most the corner radius, or a vertical
      # chord within one layer stays straight
      expect_true(
        abs(path$y[1] - path$y[2]) <= rc_used + 1e-9 ||
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
    expect_orthogonal_outside_corners(path, bends, rc_used, label = label)

    runs <- straight_runs(path)
    expect_gte(nrow(runs), 2L, label = label)
    if (stub_always || res$meta$clearance_ok[i]) {
      floor <- cap_default + max(head_default, rc_used) - 1e-9
      expect_gte(
        end_stub_length(rev_path(path)),
        floor,
        label = label
      )
      expect_gte(end_stub_length(path), floor, label = label)
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
  # a gap on the second or third rung of the ladder tightens its spacing
  # below sep_e, and reports the spacing its slots keep
  gap_sep <- function(g) {
    gaps_used <- res$ortho$gaps
    if (is.null(gaps_used)) {
      return(sep_e_default)
    }
    row <- gaps_used[gaps_used$gap == g, , drop = FALSE]
    if (nrow(row) != 1 || !is.finite(row$spacing)) {
      return(sep_e_default)
    }
    min(sep_e_default, row$spacing)
  }
  for (g in unique(slots$gap)) {
    s <- slots[slots$gap == g, , drop = FALSE]
    gap_label <- paste0(prefix, "gap ", g, " slots")
    sep_g <- gap_sep(g)
    xs <- sort(unique(round(s$x, 9)))
    if (length(xs) > 1) {
      expect_true(all(diff(xs) >= sep_g - 1e-9), label = gap_label)
    }
    n <- nrow(s)
    for (a in seq_len(n - 1)) {
      for (b in (a + 1):n) {
        # runs from different sources that overlap or come within sep_e
        # in y never share an x; two rounded runs that met before rounding
        # are 2 rc apart, and stacked ports part two runs that used to meet
        # by as much as a port stack is tall
        overlap <- min(s$hi[a], s$hi[b]) - max(s$lo[a], s$lo[b])
        touching <- overlap >= -sep_e_default - 2 * rc_used - 1e-6
        if (s$left[a] != s$left[b] && touching) {
          expect_gte(
            abs(s$x[a] - s$x[b]),
            sep_g - 1e-9,
            label = gap_label
          )
        }
      }
    }
  }
  invisible()
}

# Constants ----------------------------------------------------------------------

test_that("route_constants() derives the design constants from the reference radius", {
  opts <- route_constants(6)
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

test_that("route_constants() applies the millimetre floors at small radii", {
  opts <- route_constants(2)
  expect_equal(opts$m, 1.2)
  expect_equal(opts$R, 3.2)
  expect_equal(opts$R_soft, 3.2)
  expect_equal(opts$sep_e, 1.5)
  expect_equal(opts$sep_m, 2.5)
  expect_equal(opts$tol_layer, 2)
})

test_that("route_constants() scales with the radius above the floors", {
  opts <- route_constants(12)
  expect_equal(opts$m, 6)
  expect_equal(opts$R, 18)
  expect_equal(opts$R_soft, 13.2)
  expect_equal(opts$sep_e, 7.2)
  expect_equal(opts$sep_m, 12)
})

test_that("route_constants() derives the corner radius and defaults to rounded corners", {
  opts <- route_constants(6)
  expect_true(all(c("corners", "rc") %in% names(opts)))
  expect_equal(opts$corners, "rounded")
  # rc = clamp(0.35 r, 0.8, 2.5): 0.35 * 6 = 2.1 lies inside the clamp
  expect_equal(opts$rc, 2.1)
  # 0.35 * 2 = 0.7 is lifted to the 0.8 mm floor
  expect_equal(route_constants(2)$rc, 0.8)
  # 0.35 * 10 = 3.5 is cut to the 2.5 mm ceiling
  expect_equal(route_constants(10)$rc, 2.5)
  expect_equal(route_constants(6, corners = "sharp")$corners, "sharp")
  expect_error(route_constants(6, corners = "bevel"))
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

test_that("mediator: x->y arrives clear of m->y's arrowhead", {
  # x -> y and m -> y share a target, so their drawn arrowheads sit on the
  # same 8 mm circle around y. The detour must arrive theta_min apart from
  # the straight edge for the two heads to read as two edges; unseparated,
  # the arrivals are 8.4 degrees apart and the tips 1.17 mm.
  scene <- mediator_scene()
  res <- route_scene(scene)
  path <- res$paths[[3]]
  straight <- res$paths[[2]]
  y <- node_xy(scene, "y")

  expect_gte(tip_distance(path, straight), sep_e_default - verify_tol)
  expect_gte(
    arrival_separation(path, straight, y),
    theta_min_default - 1
  )

  # separating the arrival changes only the last arm: the slot, the
  # clearance from m and the smoothness of the curve are unchanged
  expect_equal(res$meta$mode[3], "interior")
  expect_equal(res$meta$side[3], 1)
  wp <- res$waypoints[[3]]
  expect_identical(nrow(wp), 1L)
  expect_equal(c(wp$x, wp$y), c(80, 64))
  expect_true(res$meta$clearance_ok[3])
  expect_gte(path_min_dist(path, node_xy(scene, "m")), r_full - verify_tol)
  expect_lt(max(abs(turning_angles(path))), 12)
  expect_lt(arrival_angle(path, y), 15)
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

test_that("fan: a->e arrives clear of the other two arrowheads at e", {
  # Three edges arrive at e. b -> e comes from above and is far enough off
  # on its own; c -> e is the level chord a -> e detours around, and the two
  # heads sit 1.40 mm apart until the arrival is separated.
  scene <- fan_scene()
  res <- route_scene(scene)
  path <- res$paths[[6]]
  e <- node_xy(scene, "e")

  expect_gte(tip_distance(path, res$paths[[5]]), sep_e_default - verify_tol)
  expect_gte(arrival_separation(path, res$paths[[5]], e), theta_min_default - 1)
  expect_gte(tip_distance(path, res$paths[[4]]), sep_e_default)

  # the separated arrival still comes from the side the bow was drawn on,
  # so the curve never crosses back above the chord
  expect_true(all(path$y <= 55 + 1e-9))
  wp <- res$waypoints[[6]]
  expect_equal(c(wp$x, wp$y), c(80, 46))
  expect_lt(arrival_angle(path, e), 15)
})

# Fixture 2b: a squeezed arrival ------------------------------------------------

test_that("separate_arrival() takes the midpoint when no angle clears them all", {
  # Three edges arrive at a target: two 27.9 degrees either side of the
  # chord and one along it. No direction within the 40 degree clamp keeps
  # theta_min from all three, so the best available is the direction with
  # the largest minimum gap, and that is the midpoint of the pair the
  # arrival is caught between: -13.95 degrees, 13.95 from each. The clamp
  # edge at -40 leaves only 12.05 to the nearest arrival.
  unit <- function(deg) c(cos(deg * pi / 180), sin(deg * pi / 180))
  arrivals <- rbind(unit(-27.9), unit(27.9), unit(0))

  d <- separate_arrival(
    unit(-10.6),
    arrivals,
    theta_min_default,
    c(1, 0),
    40,
    0
  )

  expect_lt(abs(atan2(d[2], d[1]) * 180 / pi + 13.95), 0.5)
})

test_that("separate_arrival() takes the nearest clear angle when one exists", {
  # One other edge arrives along the chord and the current direction is
  # 5 degrees off it, inside theta_min. Rotating to 15 degrees clears the
  # rival by exactly theta_min, so that is the answer: the largest minimum
  # gap is reserved for the squeeze, where no candidate clears every
  # arrival. The clamp edge at 40 degrees also clears the rival, but it
  # turns 35 degrees to buy nothing.
  unit <- function(deg) c(cos(deg * pi / 180), sin(deg * pi / 180))
  arrivals <- rbind(unit(0))

  d <- separate_arrival(unit(5), arrivals, 15, c(1, 0), 40, 0)

  expect_lt(abs(atan2(d[2], d[1]) * 180 / pi - 15), 0.5)
})

test_that("separate_arrival() takes the nearest clear angle on the minus side", {
  # The mirror of the case above, with the detour constraining the arrival
  # to negative angles: -15 clears the rival on the admissible side, and
  # the clamp edge at -40 is a 35 degree turn for the same clearance.
  unit <- function(deg) c(cos(deg * pi / 180), sin(deg * pi / 180))
  arrivals <- rbind(unit(0))

  d <- separate_arrival(unit(-5), arrivals, 15, c(1, 0), 40, -1)

  expect_lt(abs(atan2(d[2], d[1]) * 180 / pi + 15), 0.5)
})

test_that("dense arrival: c->y lands midway between the arrivals crowding it", {
  # c -> y detours above x and arrives between x -> y along the row and
  # a -> y from the corner. Neither gap can reach theta_min, so the two are
  # equalised instead: rotating to the clamp leaves 6.1 degrees to a -> y
  # and the two tips 0.85 mm apart, which reads as one arrowhead.
  scene <- dense_arrival_scene()
  res <- route_scene(scene)
  labels <- edge_labels(scene$edges)
  i <- match("c->y", labels)
  path <- res$paths[[i]]
  rivals <- res$paths[match(c("a->y", "x->y"), labels)]
  y <- node_xy(scene, "y")

  gaps <- vapply(rivals, function(p) arrival_separation(path, p, y), numeric(1))
  expect_gte(min(gaps), theta_min_default / 2)
  expect_lt(abs(diff(gaps)), 2)
  tips <- vapply(rivals, function(p) tip_distance(path, p), numeric(1))
  expect_gt(min(tips), 1.8)

  # the detour itself is unchanged: the same slot above x, on the same side
  expect_true(res$meta$routed[i])
  expect_equal(res$meta$mode[i], "interior")
  expect_equal(res$meta$side[i], 1)
  expect_true(res$meta$clearance_ok[i])
  expect_gte(path_min_dist(path, node_xy(scene, "x")), r_full - verify_tol)
  expect_lt(arrival_angle(path, y), 15)
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
  # Three edges arrive at t. s1 -> t comes down from above, on the side the
  # arch descends from, and s2 -> t along the chord. The arch cannot reach
  # theta_min from both inside the 40 degree tangent clamp, so it takes the
  # admissible angle with the largest minimum gap: 15 degrees from s1 -> t,
  # whose tips are then 2 cap sin(15 / 2 deg) = 2.09 mm apart, and a full
  # theta_min from s2 -> t. Aiming a 26 degree arm at a target 14 mm away
  # bends the curve inside the arm, so the radial arrival reads 16.7 and the
  # sampled tips fall a little short of the nominal separation, 3.41 mm
  # rather than the 3.6 the angle asks for.
  expect_gte(
    tip_distance(path, res$paths[[7]]),
    2 * cap_default * sin(15 / 2 * pi / 180) - verify_tol
  )
  expect_gte(tip_distance(path, res$paths[[8]]), sep_e_default - 0.3)
  expect_lt(arrival_angle(path, ends$to), 20)
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
  # at 0.88 and falls at 1.50. Separating the arrival at t steepens the last
  # few millimetres of the descent without unbalancing the arch, so the
  # absolute bound is 1.8 while the ratio to the climb still holds.
  climb <- max_slope(pt(path$x[seq_len(apex)], path$y[seq_len(apex)]))
  descent <- max_slope(pt(
    path$x[apex:nrow(path)],
    path$y[apex:nrow(path)]
  ))
  expect_lte(descent, 1.25 * climb)
  expect_lte(descent, 1.8)

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
  expect_named(l2, c("lo", "hi", "outer", "tight"))
  expect_equal(l2$lo, c(0.5, 44, 84))
  expect_equal(l2$hi, c(26, 66, 109.5))
  expect_equal(l2$outer, c(TRUE, FALSE, TRUE))
  # a gap wide enough for a slot at the full margin is never a tight slot
  expect_equal(l2$tight, c(FALSE, FALSE, FALSE))

  # layer 3: nodes at 90, 55, 20
  l3 <- layer_free_intervals(
    mm_nodes(c("s1", "s2", "s3"), 100, c(90, 55, 20)),
    3,
    bounds
  )
  expect_equal(l3$lo, c(0.5, 29, 64, 99))
  expect_equal(l3$hi, c(11, 46, 81, 109.5))
  expect_equal(l3$outer, c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(l3$tight, rep(FALSE, 4))

  # centres 10 mm apart are closer than 2 (r + m_min) = 14.4, too close even
  # for a tight slot, so the gap yields no interval at all
  overlapped <- layer_free_intervals(
    mm_nodes(c("u", "v"), 60, c(40, 50)),
    3,
    bounds
  )
  expect_equal(overlapped$lo, c(0.5, 59))
  expect_equal(overlapped$hi, c(31, 109.5))
  expect_equal(overlapped$outer, c(TRUE, TRUE))
  expect_equal(overlapped$tight, c(FALSE, FALSE))

  # a single node splits the layer into two outer intervals
  one <- layer_free_intervals(mm_nodes("m", 80, 55), 3, bounds)
  expect_equal(one$lo, c(0.5, 64))
  expect_equal(one$hi, c(46, 109.5))
  expect_equal(one$outer, c(TRUE, TRUE))
  expect_equal(one$tight, c(FALSE, FALSE))
})

test_that("layer_free_intervals() keeps a narrow gap as a tight slot on its centre line", {
  bounds <- c(0, 0, 160, 110)

  # n1 and n2 are 16 mm apart: their padded discs overlap, so the gap holds
  # no slot at the full margin, but the centres are at least
  # 2 (r + m_min) = 14.4 apart, so a curve can thread the gap at the soft
  # margin. That gap is kept as a zero-width row on its centre line, flagged
  # tight and never flagged outer.
  ints <- layer_free_intervals(
    mm_nodes(c("n1", "n2"), 80, c(43.5, 59.5)),
    3,
    bounds,
    sep_e = sep_e_default
  )
  expect_named(ints, c("lo", "hi", "outer", "tight"))
  expect_equal(ints$lo, c(0.5, 51.5, 68.5))
  expect_equal(ints$hi, c(34.5, 51.5, 109.5))
  expect_equal(ints$outer, c(TRUE, FALSE, TRUE))
  expect_equal(ints$tight, c(FALSE, TRUE, FALSE))

  # a chord dead on the gap snaps to its centre line on either side
  expect_equal(nearest_free_y(ints, 51.5, 1, FALSE), 51.5)
  expect_equal(nearest_free_y(ints, 51.5, -1, FALSE), 51.5)
  # the tight slot is not an outer interval, so a periphery arch skips it
  expect_equal(nearest_free_y(ints, 51.5, 1, TRUE), 68.5)

  # centres 13 mm apart are too close: no tight row
  closer <- layer_free_intervals(
    mm_nodes(c("n1", "n2"), 80, c(43.5, 56.5)),
    3,
    bounds,
    sep_e = sep_e_default
  )
  expect_equal(closer$lo, c(0.5, 65.5))
  expect_equal(closer$hi, c(34.5, 109.5))
  expect_equal(closer$outer, c(TRUE, TRUE))
  expect_equal(closer$tight, c(FALSE, FALSE))
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
  # with its separation at the full margin, so neither of the first two
  # survives as a slot. Their centres are 18.2 and 21 mm apart, both at
  # least 2 (r + m_min) = 14.4, so each is kept as a tight slot on its
  # centre line instead: zero width, flagged tight, priced for the margin
  # the two flanking discs give up.
  nodes <- mm_nodes(c("a", "b", "c", "d"), 80, c(20, 38.2, 59.2, 82.2))
  ints <- layer_free_intervals(nodes, 3, bounds, sep_e = sep_e_default)
  expect_equal(ints$lo, c(0.5, 29.1, 48.7, 68.2, 91.2))
  expect_equal(ints$hi, c(11, 29.1, 48.7, 73.2, 109.5))
  expect_equal(ints$outer, c(TRUE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(ints$tight, c(FALSE, TRUE, TRUE, FALSE, FALSE))

  # a chord dead on b snaps to the centre line of the gap on each side
  expect_equal(nearest_free_y(ints, 38.2, 1, FALSE), 48.7)
  expect_equal(nearest_free_y(ints, 38.2, -1, FALSE), 29.1)
  # a periphery arch takes neither: a tight slot is never an outer interval
  expect_equal(nearest_free_y(ints, 38.2, 1, TRUE), 91.2)
  expect_equal(nearest_free_y(ints, 38.2, -1, TRUE), 11)

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
  expect_equal(edge$tight, c(FALSE, FALSE))
})

test_that("a chord whose nearest slot is a sliver is routed to the next interval", {
  # S -> T runs dead on b through a layer whose four nodes sit 13 mm apart,
  # closer than the 2 (r + m_min) = 14.4 a tight slot needs and far closer
  # than the 18 mm two padded discs need for a slot. Every interior gap is a
  # sliver of negative width, so the candidates are the outer intervals: 11
  # below (displacement 22) and 68 above (35), and the waypoint sits in the
  # lower one.
  scene <- list(
    nodes = rbind(
      mm_nodes(c("S", "T"), c(20, 140), 33),
      mm_nodes(c("a", "b", "c", "d"), 80, c(20, 33, 46, 59))
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
  expect_true(at_layer <= 11 + 1e-6 || at_layer >= 68 - 1e-6)

  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(nrow(path), 16)
  expect_gte(path_min_clearance(scene, 1, path), r_full - verify_tol)
  expect_true(all(diff(path$x) >= -0.1))
  expect_lt(max(abs(turning_angles(path))), 12)
})

test_that("a chord threads a tight slot instead of bowing around the stack", {
  # n1 and n2 leave a 16 mm gap the chord runs through. At the full margin
  # that gap holds nothing, and the route detours 15.5 mm below n1 for a
  # 4.4 mm longer path. Kept as a tight slot it is drawn almost straight,
  # verified at the soft margin R_soft = 7.2 against both discs, and
  # reported sagitta_capped, the flag a route verified at the soft margin
  # already carries.
  scene <- tight_slot_scene()
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)

  expect_true(res$meta$routed[1])
  expect_equal(res$meta$mode[1], "interior")
  expect_equal(res$meta$side[1], 1)
  expect_true(res$meta$clearance_ok[1])
  expect_true(res$meta$sagitta_capped[1])

  wp <- res$waypoints[[1]]
  expect_identical(nrow(wp), 1L)
  expect_equal(c(wp$x, wp$y), c(80, 51.5))

  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(path_min_dist(path, node_xy(scene, "n1")), r_soft - verify_tol)
  expect_gte(path_min_dist(path, node_xy(scene, "n2")), r_soft - verify_tol)
  # threading the gap costs almost nothing: a 0.04 mm longer path at a
  # sagitta the eye cannot read as a bow
  expect_lt(res$meta$sagitta_ratio[1], 0.03)
  excess <- sum(sqrt(diff(path$x)^2 + diff(path$y)^2)) -
    chord_length(ends$from, ends$to)
  expect_lt(excess, 0.5)
  expect_true(all(diff(path$x) >= -0.1))
  expect_lt(max(abs(turning_angles(path))), 12)
})

# Two horizontal chords at y = 30 and 46 cross a layer whose stack at y =
# 40, 27, 14, and 1 leaves one free interval, [49, 109.5], so both snap to
# 49 above. The nodes sit 13 mm apart, closer than the 2 (r + m_min) = 14.4
# a tight slot needs, so no gap in the stack is usable and the lowest node
# reaches past the panel edge. `low` and `high` name the endpoints of the
# chord-30 and chord-46 edges; the edges tie on span and length, so the
# canonical name order decides which is routed first.
shared_slot_scene <- function(low, high) {
  list(
    nodes = rbind(
      mm_nodes(c(low, high), c(20, 140, 20, 140), c(30, 30, 46, 46)),
      mm_nodes(c("m1", "m2", "m3", "m4"), 80, c(40, 27, 14, 1))
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
# layers 2 and 3 (x = 60 and 100) sit 13 mm apart, closer than a tight slot
# needs, and reach past the lower panel edge, so they leave no free interval
# below and one above each: [83, 109.5] at layer 2 and [57, 109.5] at layer
# 3. Both chords are blocked at both layers and can only arch above.
occupied_arch_scene <- function() {
  list(
    nodes = rbind(
      mm_nodes(c("pa", "pb", "t"), c(20, 20, 140), c(40, 60, 55)),
      mm_nodes(paste0("q", 1:6), 60, c(9, 22, 35, 48, 61, 74)),
      mm_nodes(paste0("s", 1:4), 100, c(9, 22, 35, 48))
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
  # Its slots are 83 at layer 2 and 57 at layer 3; the line from (60, 83)
  # to t passes layer 3 at 69, above 57, so the hull drops the layer 3
  # waypoint and the arch crosses that layer without one. The verify step
  # then lifts the remaining waypoint further.
  expect_identical(res$meta$waypoint_layers[[1]], 2L)

  # pb -> t wants the same slots. Its chord is higher at both layers (58.3
  # against 45 at layer 2, 56.7 against 50 at layer 3), so it sits outside
  # pa -> t: sep_e above the waypoint pa -> t was drawn through at layer 2,
  # and sep_e above where the pa -> t arch actually passes layer 3, rather
  # than at the slot boundary 57 that its own hull would discard. Spreading
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

# The occupied-arch scene with a fifth node s5 at (100, 72) on top of the
# layer 3 stack. Both chords now snap into the 6 mm gap between s4 and s5,
# which the hull discards because the line from the layer 2 slot to t
# passes above it, so the layer 3 waypoint of each arch is inserted by the
# repair loop at s5 + R = 81 from the node disc alone. Without a re-check
# against the occupancy, pb -> t was drawn 0.29 mm from pa -> t at x = 100.
# The panel is 120 mm high, and every drawn curve keeps the clearance
# margin m = 3 from the bounds.
repaired_arch_scene <- function() {
  scene <- occupied_arch_scene()
  scene$nodes <- rbind(scene$nodes, mm_nodes("s5", 100, 72))
  scene$bounds <- c(0, 0, 160, 120)
  scene
}

test_that("an arch that repairs moved onto another arch is spread again", {
  scene <- repaired_arch_scene()
  res <- route_scene(scene)

  expect_true(all(res$meta$routed))
  expect_true(all(res$meta$clearance_ok))
  expect_equal(res$meta$side, c(1, 1))

  # pa -> t routes first and its repaired arch passes layer 3 near 83;
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

test_that("a skip edge detours past another edge's arrowhead, not through it", {
  # c -> f bows under e. The shallowest bow that clears e's disc passes
  # 0.24 mm from the arrowhead of b -> e, which arrives at e from below:
  # two edges drawn through each other's ink. The head zone of every other
  # edge is an obstacle for a detour with the same margin a disc has, so
  # the bow drops to 18.7 mm below e and clears the window by m.
  scene <- skip_over_head_scene()
  res <- route_scene(scene)
  path <- res$paths[[4]]
  ends <- edge_endpoints(scene, 4)

  expect_gte(path_to_head_dist(path, res$paths[[3]]), m_default - verify_tol)

  expect_true(res$meta$routed[4])
  expect_equal(res$meta$mode[4], "interior")
  expect_equal(res$meta$side[4], -1)
  expect_true(res$meta$clearance_ok[4])
  wp <- res$waypoints[[4]]
  expect_identical(nrow(wp), 1L)
  expect_equal(wp$x, 80)
  expect_lte(wp$y, 100 - 2 * r_full)

  # the shaft of b -> e is still crossed: only its head is given room
  expect_identical(
    count_path_crossings(path, node_xy(scene, "b"), node_xy(scene, "e")),
    1L
  )
  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(path_min_dist(path, node_xy(scene, "e")), r_full - verify_tol)
  # the detour is still a shallow curve, not an arch
  expect_lte(res$meta$sagitta_ratio[4], 0.2)
  expect_true(all(diff(path$x) >= -0.1))
  expect_lt(max(abs(turning_angles(path))), 12)
  expect_lt(arrival_angle(path, ends$to), 15)
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

test_that("a route through a tangle takes the interior rather than a deep arch", {
  # s -> t crosses three layers whose twelve interior edges weave across it.
  # No route crosses fewer than five of them, so a price of one crossing
  # each buys nothing: the arch under the stacks avoids three crossings at
  # the cost of 28 mm of extra path and a sagitta of 0.27. Charging the
  # first crossing in full and halving each further one leaves the interior
  # slot 9 mm above b1 as the cheaper route, at 1.3 mm of excess.
  scene <- dense_span_scene()
  res <- route_scene(scene)
  i <- match("s->t", edge_labels(scene$edges))
  path <- res$paths[[i]]
  ends <- edge_endpoints(scene, i)

  expect_true(res$meta$routed[i])
  expect_equal(res$meta$mode[i], "interior")
  expect_equal(res$meta$side[i], 1)
  expect_true(res$meta$clearance_ok[i])
  wp <- res$waypoints[[i]]
  expect_identical(nrow(wp), 1L)
  expect_equal(c(wp$x, wp$y), c(80, 59))

  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(path_min_dist(path, node_xy(scene, "b1")), r_full - verify_tol)
  expect_gte(path_min_clearance(scene, i, path), r_full - verify_tol)
  # the drawn curve reads as a straight edge nudged over one node, not an
  # arch: it never leaves a tenth of its own span
  expect_lt(
    max(abs(chord_offset(path, ends$from, ends$to))) /
      chord_length(ends$from, ends$to),
    0.1
  )
  # the tangle is still crossed, which is the point: paying to avoid some
  # of five crossings is not worth a detour
  crossings <- sum(vapply(
    dense_span_interior,
    function(o) {
      other <- res$paths[[o]]
      sum(vapply(
        seq_len(nrow(other) - 1),
        function(k) {
          count_path_crossings(
            path,
            c(other$x[k], other$y[k]),
            c(other$x[k + 1], other$y[k + 1])
          )
        },
        integer(1)
      ))
    },
    integer(1)
  ))
  expect_gte(crossings, 5L)
})

test_that("a rank-1 candidate that fails verification opens the candidate pool", {
  # The candidate above the chord ranks first and stays inside the panel,
  # but no repair gets its curve clear of q2: it is drawn 7.58 mm from that
  # disc, short of R. A rank-1 candidate that is infeasible after routing
  # opens the pool to the rest, and the candidate below verifies with a
  # single waypoint in the gap under s2.
  scene <- unverified_slot_scene()
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)

  expect_true(res$meta$routed[1])
  expect_equal(res$meta$mode[1], "interior")
  expect_equal(res$meta$side[1], -1)
  expect_true(res$meta$clearance_ok[1])
  wp <- res$waypoints[[1]]
  expect_identical(nrow(wp), 1L)
  expect_equal(c(wp$x, wp$y), c(100, 48))

  expect_exact_endpoints(path, ends$from, ends$to)
  expect_gte(path_min_clearance(scene, 1, path), r_full - verify_tol)
  expect_true(all(diff(path$x) >= -0.1))
  expect_lt(max(abs(turning_angles(path))), 12)
})

test_that("a free bow outside the panel margin never replaces a spanning route", {
  # S -> T is dead on B, on a row 10 mm below the top of the panel. The free
  # bow above B needs B + R = 109, past the 107 the clearance margin allows,
  # so it can only be drawn pressed against the border. A bow that leaves
  # the margin is infeasible rather than clamped, and the spanning route
  # below B stands: it threads the 21 mm gap to u as a tight slot.
  scene <- border_bow_scene()
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)

  expect_true(res$meta$routed[1])
  expect_equal(res$meta$mode[1], "interior")
  expect_equal(res$meta$side[1], -1)
  expect_true(res$meta$clearance_ok[1])
  wp <- res$waypoints[[1]]
  expect_identical(nrow(wp), 1L)
  expect_equal(c(wp$x, wp$y), c(80, 89.5))

  expect_exact_endpoints(path, ends$from, ends$to)
  # the drawn curve is nowhere near the border it would have been clamped to
  expect_gt(scene$bounds[4] - max(path$y), m_default + 1)
  expect_gte(path_min_dist(path, node_xy(scene, "B")), r_soft - verify_tol)
  expect_gte(path_min_dist(path, node_xy(scene, "u")), r_soft - verify_tol)
  expect_lt(max(abs(turning_angles(path))), 12)
})

test_that("a free bow outside the margin loses to a spanning route that grazes", {
  # Here nothing inside the margin verifies. The rank-1 candidate above the
  # chord clears every disc but leaves the margin, so the pool opens; the
  # candidates below it stay inside and graze u by 0.11 mm; and the free bow
  # above B clears the discs only at y = 109, outside the margin. A bow that
  # can be drawn only by clamping it to the border is infeasible, so the
  # grazing spanning route wins: it keeps 9 mm from B where the clamped bow
  # would have passed 7 mm from it and run along the border.
  #
  # No route in this scene verifies, so clearance_ok is FALSE either way:
  # a route that keeps the margin and verifies would have been taken by the
  # candidate pool and would never reach the bow.
  scene <- margin_bow_scene()
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)

  expect_true(res$meta$routed[1])
  expect_equal(res$meta$mode[1], "interior")
  expect_equal(res$meta$side[1], -1)
  expect_false(res$meta$clearance_ok[1])
  wp <- res$waypoints[[1]]
  expect_identical(nrow(wp), 2L)
  expect_equal(wp$x, c(80, 100))
  expect_equal(wp$y, c(95, 91))

  expect_exact_endpoints(path, ends$from, ends$to)
  # the curve never rises above the row, let alone to the border
  expect_lte(max(path$y), 100 + 1e-9)
  expect_gt(scene$bounds[4] - max(path$y), m_default + 1)
  expect_gte(path_min_dist(path, node_xy(scene, "B")), r_full - verify_tol)
  expect_gte(path_min_dist(path, node_xy(scene, "u")), r_soft)
  expect_lt(max(abs(turning_angles(path))), 12)
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
  # One blocked edge and one obstacle, so nothing else arrives at either
  # endpoint: reversing the edge reverses the drawn polyline point for
  # point. A scene with other arrivals is a different DAG once the edge is
  # reversed, since the arrival the router separates is at the other end.
  scene <- list(
    nodes = mm_nodes(c("x", "m", "y"), c(7.3, 80, 152.7), c(55, 55, 55)),
    edges = mm_edges("x", "y"),
    bounds = c(0, 0, 160, 110)
  )
  forward <- route_scene(scene)
  scene$edges <- mm_edges("y", "x")
  res <- route_scene(scene)
  ends <- edge_endpoints(scene, 1)

  expect_true(res$meta$routed[1])
  expect_identical(res$meta$side[1], forward$meta$side[1])
  expect_identical(res$meta$mode[1], forward$meta$mode[1])
  expect_identical(res$waypoints[[1]], forward$waypoints[[1]])
  expect_identical(res$paths[[1]]$x, rev(forward$paths[[1]]$x))
  expect_identical(res$paths[[1]]$y, rev(forward$paths[[1]]$y))
  expect_exact_endpoints(res$paths[[1]], ends$from, ends$to)
})

test_that("a reversed edge keeps its twin's slot when other edges share its ends", {
  # In the mediator, reversing x -> y makes m -> y the edge that arrives at
  # y where y -> x now departs, so the two paths differ at that end. The
  # slot the detour is drawn through is the same either way.
  forward <- route_scene(mediator_scene())
  scene <- mediator_scene()
  scene$edges <- mm_edges(c("x", "m", "y"), c("m", "y", "x"))
  res <- route_scene(scene)
  ends <- edge_endpoints(scene, 3)

  expect_true(res$meta$routed[3])
  expect_identical(res$meta$side[3], forward$meta$side[3])
  expect_identical(res$meta$mode[3], forward$meta$mode[3])
  expect_identical(res$waypoints[[3]], forward$waypoints[[3]])
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
# per bend (2 by default, so four bends cost a second detour) + the crossing
# and congestion terms, ties above.
ortho <- function(scene, corners = NULL, ...) {
  opts <- if (is.null(corners)) {
    route_constants(r_default)
  } else {
    route_constants(r_default, corners = corners)
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
#
# c->e runs at e's own y, so it owns e's centre row and b->e stacks one slot
# above it at 55 + 3.6 = 58.6. That row runs on to e's own x, ending at the
# axis point (140, 58.6): the last 4.8 mm are hidden under the disc, and the
# head is drawn along the row rather than angled at the centre.
fan_sharp_polylines <- list(
  "a->b" = pt(c(20, 50, 50, 80), c(55, 55, 85, 85)),
  "a->d" = pt(c(20, 50, 50, 80), c(55, 55, 25, 25)),
  "b->e" = pt(c(80, 110, 110, 140), c(85, 85, 58.6, 58.6)),
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
    expect_port_endpoints(path, ends$from, ends$to, label = lab)
    expect_lt(polyline_hausdorff(path, poly), 1e-6, label = lab)
    # a sharp path is axis-aligned everywhere, drawn or hidden: an offset
    # port's row reaches the axis point without a connector, so no segment
    # of the path is oblique
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
    expect_port_endpoints(path, ends$from, ends$to, label = lab)

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
    # that (8 samples peak at 15.9 degrees), so the sample count is not
    # pinned. Every turn of the path is a rounded corner now, so none of it
    # is excluded.
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

  expect_port_endpoints(path, ends$from, ends$to)
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
  # x->y runs at y's own y, so it owns the centre row and m->y stacks one
  # slot above it at 55 + 3.6 = 58.6
  expect_equal(res$waypoints[[j]]$y, c(75, 58.6), tolerance = 1e-6)

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
    waypoints = res$waypoints[keep],
    ortho = res$ortho
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
    opts = route_constants(r_default, layer_axis = "x")
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
    opts = route_constants(r_default, layer_axis = "x")
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

test_that("orthogonal fan: edges entering the same port take their own rows", {
  scene <- fan_scene()
  res <- ortho(scene)
  be <- res$paths[[edge_index(scene, "b->e")]]
  ce <- res$paths[[edge_index(scene, "c->e")]]

  # b->e turns down at x = 110 onto the port row sep_e above e's centre
  # rather than onto the chord of the level c->e, so both arrowheads show.
  # The run carries on to e's own x, ending at the axis point (140, 58.6);
  # its last 4.8 mm are hidden under the disc.
  tail <- be[
    be$x > 110 + rc_default + 1e-6,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(tail), 0)
  expect_true(all(abs(tail$y - 58.6) < 1e-6))
  expect_gte(min(point_polyline_dist(tail, ce)), sep_e_default - 1e-6)
  expect_equal(
    unname(unlist(be[nrow(be), c("x", "y")])),
    c(140, 58.6),
    tolerance = 1e-6
  )
  runs <- straight_runs(be)
  last <- runs[nrow(runs), ]
  expect_equal(last$axis, "h")
  expect_equal(last$coord, 58.6, tolerance = 1e-6)
  # from the tangent point at 110 + 2.1 to the axis point at x = 140
  expect_gte(last$length, 30 - rc_default - 1e-6)
})

# Two layers `gap` apart, centred on x = 40, with four staircase edges
# between them. Each adjacent pair overlaps in y (a1/a3 and a2/a4 only
# touch, a1/a4 are disjoint), which forces the chain a4 -> a3 -> a2 -> a1
# and four slots. At the default 40 mm the nominal band [36.1, 43.9] is
# 7.8 mm wide, too narrow for four slots at sep_e, and the gap climbs the
# ladder from there; the narrower gaps drive it rung by rung.
narrow_band_scene <- function(gap = 40) {
  half <- gap / 2
  list(
    nodes = mm_nodes(
      c("a1", "a2", "a3", "a4", "b1", "b2", "b3", "b4"),
      c(rep(40 - half, 4), rep(40 + half, 4)),
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
  # the first rung of the ladder buys the band by shortening the stub to
  # (40 - 3 * 3.6) / 2 = 14.6, so these slots keep their clearance
  expect_true(all(res$meta$clearance_ok))
  for (i in seq_len(nrow(scene$edges))) {
    ends <- edge_endpoints(scene, i)
    expect_port_endpoints(res$paths[[i]], ends$from, ends$to)
    expect_orthogonal_outside_corners(
      res$paths[[i]],
      res$waypoints[[i]],
      rc_default
    )
    expect_gte(
      end_stub_length(res$paths[[i]]),
      14.6 - rc_default - 1e-6
    )
    expect_gte(
      end_stub_length(rev_path(res$paths[[i]])),
      14.6 - rc_default - 1e-6
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
    opts = route_constants(r_default, layer_axis = "y")
  )

  expect_identical(res$meta$routed, ref$meta$routed)
  expect_identical(res$meta$mode, ref$meta$mode)
  # side is reported in the canonical orientation
  expect_identical(res$meta$side, ref$meta$side)
  expect_identical(res$meta$n_waypoints, ref$meta$n_waypoints)

  for (i in seq_along(ref$paths)) {
    ends <- edge_endpoints(rotated, i)
    expect_port_endpoints(res$paths[[i]], ends$from, ends$to, axis = "y")
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
  expect_port_endpoints(res$paths[[6]], ends$from, ends$to)
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
  sharp <- route_constants(r_default, corners = "sharp")
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
    expect_port_endpoints(res$paths[[i]], ends$from, ends$to)
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

test_that("canonical DAGs: orthogonal mode is axis-aligned with port endpoints and separated slots", {
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
        path <- res$paths[[i]]
        # every chord that is neither level nor vertical is orthogonal; a
        # chord within the corner radius of the row it arrives on may stay
        # straight, whether that row is the centre or an offset one
        row <- path$y[[nrow(path)]]
        oblique <- abs(ends$from[2] - row) > rc_default &&
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

test_that("end_stub_length() measures the last run that reaches the path end", {
  # a centre port: a 12 mm run down x = 80 after a perpendicular channel
  expect_equal(end_stub_length(pt(c(0, 80, 80), c(67, 67, 55))), 12)
  # an offset N port: the run continues to the centre's own y, so the stub is
  # the whole 12 mm the head is drawn on
  expect_equal(end_stub_length(pt(c(0, 78.2, 78.2), c(67, 67, 55))), 12)
  # an offset W port: the row runs to the centre's own x, 4.8 mm of it under
  # the disc, and the stub is the row
  expect_equal(end_stub_length(pt(c(110, 140), c(58.6, 58.6))), 30)
  # a 7 mm stub followed by a 100 mm channel is 7 mm, neither 107 nor 100,
  # and fails the floor
  expect_equal(end_stub_length(pt(c(-20, 80, 80), c(62, 62, 55))), 7)
  expect_lt(7, cap_default + rc_default)
  expect_equal(end_stub_length(pt(c(-20, 80, 80), c(64, 64, 55))), 9)
  expect_equal(end_stub_length(pt(c(-20, 80, 80), c(58, 58, 55))), 3)
  # the last run is the stub whatever its length, so a 5 mm leg onto the
  # centre line is 5 mm of stub and fails the floor
  expect_equal(
    end_stub_length(pt(c(0, 75, 75, 80), c(67, 67, 55, 55))),
    5
  )
  # a channel at 62 leaves a 7 mm stub whose straight part after rounding is
  # 7 - 2.1 = 4.9, measured as itself and not as the channel run
  P <- rbind(c(7.3, 55), c(7.3, 62), c(80, 62), c(80, 55))
  rounded <- dedupe_points(sample_runs(round_corners(P, rc_default), 0.5))
  expect_equal(
    end_stub_length(pt(rounded[, 1], rounded[, 2])),
    4.9,
    tolerance = 1e-6
  )
  # a short vertical leg under a channel is a stub in its own right: 1.5 mm
  # sharp, and 0.75 mm once rounding cuts half the leg, both under the floor
  expect_equal(
    end_stub_length(pt(c(-20, 80, 80), c(56.5, 56.5, 55))),
    1.5
  )
  P <- rbind(c(7.3, 55), c(7.3, 56.5), c(80, 56.5), c(80, 55))
  rounded <- dedupe_points(sample_runs(round_corners(P, rc_default), 0.5))
  expect_equal(
    end_stub_length(pt(rounded[, 1], rounded[, 2])),
    0.75,
    tolerance = 1e-6
  )
  # an oblique tail has no stub
  expect_equal(
    end_stub_length(pt(c(0, 78, 78, 80), c(67, 67, 61, 55))),
    0
  )
  # the reversed path measures the stub at the start
  expect_equal(
    end_stub_length(rev_path(pt(c(80, 80, 0), c(55, 67, 67)))),
    12
  )
})

# A collinear chain with two skip edges: a->c is blocked by b and c->e by d,
# so both channel above at max(55 + 9, 55 + 16.1) = 71.1 (S/N: 16.1 / 6 + 2
# bends at 2 = 6.68, against an E/W run at 55 + 9 = 64 with four bends,
# 9 / 6 + 8 = 9.5; nothing is congested and the tie between the sides goes
# above).
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

  # the port's own axis is 1.8 mm off the centre line, so its resect is
  # cap - r + sqrt(r^2 - 1.8^2) = 7.724 and both tips sit cap - r = 2 mm
  # from c's disc face along the stub
  expect_equal(res$meta$resect_head[[ac]], 7.724, tolerance = 1e-3)
  expect_equal(res$meta$resect_fins[[ce]], 7.724, tolerance = 1e-3)
  expect_equal(
    res$meta$resect_head[[ac]],
    port_resect_at(sep_e_default / 2),
    tolerance = 1e-9
  )
  expect_equal(res$meta$resect_fins[[ac]], cap_default, tolerance = 1e-6)
  expect_equal(res$meta$resect_head[[ce]], cap_default, tolerance = 1e-6)
})

test_that("orthogonal slots: touching segments from different sources never share an x", {
  # In gap 2 (x 60 to 100) q1's hyperedge covers [55, 90] and q2's covers
  # [20, 55]; they meet at s2's y. One slot for both drew a continuous
  # vertical from q2 up to q1 that read as q1->s3 and q2->s1. The band
  # [76.1, 83.9] is 7.8 mm wide and holds two slots at exactly sep_e = 3.6
  # apart (an even spread would put them 2.6 apart). A band that holds its
  # K slots at sep_e spacing is not narrow; the even-spread criterion applies
  # only when K slots at sep_e do not fit, so these edges keep their
  # clearance. Gap 3 (100 to 140) is the same picture with s1->t over
  # [55, 90] and s3->t over [20, 55] meeting at t's y.
  scene <- four_layer_scene()
  res <- ortho(scene)
  for (lab in c("q1->s1", "q1->s2", "q2->s2", "q2->s3", "s1->t", "s3->t")) {
    expect_true(res$meta$clearance_ok[edge_index(scene, lab)], label = lab)
  }
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

# Two chords whose y-intervals in the gap stop short of each other: s1->t1
# covers [60, 80] and s2->t2 covers [40, 58], a 2 mm break, more than
# touching and less than sep_e.
near_interval_scene <- function() {
  list(
    nodes = mm_nodes(
      c("s1", "s2", "t1", "t2"),
      c(20, 20, 100, 100),
      c(80, 40, 60, 58)
    ),
    edges = mm_edges(c("s1", "s2"), c("t1", "t2")),
    bounds = c(0, 0, 120, 110)
  )
}

test_that("orthogonal slots: segments that stop short of each other by less than sep_e never share an x", {
  # One slot for both would draw a vertical from 40 up to 80 with a 2 mm
  # break in it, which reads as one line through all four discs, and a
  # stacked arrival port closes a break that small anyway. The two segments
  # take distinct ranks and the rung 0 spread of an 80 mm gap puts them
  # (80 - 32.2) / 3 = 15.93 apart.
  scene <- near_interval_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  expect_equal(res$ortho$gaps$ranks, 2)

  slots <- vapply(
    edge_labels(scene$edges),
    function(lab) {
      x <- slot_xs(res$paths[[edge_index(scene, lab)]], c(20, 100))
      expect_length(x, 1)
      x
    },
    numeric(1)
  )
  expect_gte(unname(abs(diff(slots))), sep_e_default - 1e-9)
  expect_equal(
    unname(abs(diff(slots))),
    (80 - 2 * stub_default) / 3,
    tolerance = 1e-6
  )
})

# A five-node row at y = 100 with a second row 16 mm beneath it, so the S
# side is not extreme and the two skip edges d->f (blocked by e) and c->h
# (blocked by d, e, f) both channel above. Their spans nest, d->f inside
# c->h, so stacking them costs no crossing: d->f, placed first as the
# shorter, runs at max(100 + 9, 100 + 16.1) = 116.1 and c->h sep_e outside
# it at 119.7. d->f costs 16.1 / 6 + 2 bends at 2 = 6.68 against an E/W run
# under both rows at 84 - 9 = 75, 25 / 6 + 8 = 12.17; c->h, three crossed
# layers displaced 19.7 each, costs 59.1 / 6 + 4 = 13.85 against 75 / 6 + 8
# = 20.5. The rows are 16 mm apart, so no run fits between them: a run
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
  # No horizontal run of one edge overlaps in x with a run of the other at
  # the same y. This forces one proper crossing: c->f rises into f along
  # y = 100 from its gap-2 slot and e->h leaves e along y = 100 up to its
  # gap-2 slot, so the two runs on the row's line stay apart only when
  # e->h's slot is left of c->f's, and then e->h's descent crosses c->f's
  # channel run. One crossing is accepted here, as in the overcontrol X.
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

  # the fan's trunk, as a check on the measure itself: a->b and a->d share
  # a's port and the 30 mm from a to the slot at 50. b->e and c->e enter
  # e's W side on rows of their own, so they share nothing: a shared suffix
  # belongs to a merged side, not to every pair at one port.
  scene <- fan_scene()
  res <- ortho(scene, corners = "sharp")
  ab <- res$paths[[edge_index(scene, "a->b")]]
  ad <- res$paths[[edge_index(scene, "a->d")]]
  be <- res$paths[[edge_index(scene, "b->e")]]
  ce <- res$paths[[edge_index(scene, "c->e")]]
  expect_equal(shared_run_length(ab, ad), 30, tolerance = 1e-6)
  expect_equal(common_prefix_length(ab, ad), 30, tolerance = 1e-6)
  expect_equal(shared_run_length(be, ce), 0)
  expect_equal(common_suffix_length(be, ce), 0)
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
# 55 - 16.1) = 38.9 (S/N below 16.1 / 6 + 2 bends at 2 = 6.68, against an
# E/W run at 46 or 64 with four bends, 9 / 6 + 8 = 9.5).
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

test_that("route_constants() prices a bend at two reference radii by default", {
  # two bends are the price of one detour, so four bends cost a second one
  expect_equal(route_constants(r_default)$bend_penalty, 2)
  expect_equal(route_constants(r_default, bend_penalty = 0)$bend_penalty, 0)
  expect_equal(route_constants(r_default, bend_penalty = 2.5)$bend_penalty, 2.5)
  expect_equal(route_constants(3)$bend_penalty, 2)
})

test_that("orthogonal pricing: the collinear mediator keeps its two-bend channel until bends are free", {
  # S/N above: channel at max(55 + 9, 55 + 16.1) = 71.1, displacing the
  # chord by 16.1: 16.1 / 6 + 2 * 2 = 6.683. E/W above: run at 55 + 9 = 64
  # with four bends: 9 / 6 + 4 * 2 = 9.5. Below mirrors both, nothing is
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
  # the channel's 71.1, leaving x's E port along the chord. m->y runs at
  # y's own y, so it owns the centre row and x->y enters one slot above it
  # at 55 + 3.6 = 58.6.
  free <- route_scene(
    scene,
    mode = "orthogonal",
    opts = route_constants(r_default, bend_penalty = 0)
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
  expect_equal(runs$coord[nrow(runs)], 58.6, tolerance = 1e-6)
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

test_that("orthogonal pricing: the displaced mediator keeps its two-bend channel under the default penalty", {
  # m sits 8 mm above the chord. S/N below at min(63 - 9, 55 - 16.1) = 38.9
  # displaces the chord by 16.1: 16.1 / 6 + 2 * 2 = 6.683; S/N above at
  # max(63 + 9, 71.1) = 72 displaces by 17: 17 / 6 + 4 = 6.833. The E/W run
  # below at 63 - 9 = 54 jogs the chord by 1 mm with four bends: 1 / 6 + 4 *
  # 2 = 8.167 (at one per bend it would cost 4.167 and win, a visible wobble
  # for nothing); above at 72 it costs 17 / 6 + 8 = 10.83. Nothing is
  # congested, since m's offset of 8 lies inside R.
  scene <- mediator_scene(m_y = 63)
  res <- ortho(scene)
  k <- edge_index(scene, "x->y")
  expect_equal(res$meta$mode[k], "orthogonal")
  expect_true(res$meta$clearance_ok[k])
  expect_equal(res$meta$side[k], -1)
  expect_equal(res$meta$n_waypoints[k], 2)
  wp <- res$waypoints[[k]]
  expect_equal(wp$x, c(7.3, 152.7), tolerance = 1e-6)
  expect_equal(wp$y, c(38.9, 38.9), tolerance = 1e-6)
  path <- res$paths[[k]]
  runs <- straight_runs(path)
  # down the S stub, along the channel, and up: no jog near the chord
  expect_equal(runs$axis[1], "v")
  expect_equal(runs$axis[nrow(runs)], "v")
  expect_equal(channel_run(path, 80)$coord, 38.9, tolerance = 1e-6)
  expect_true(all(path$y <= 55 + 1e-9))
})

test_that("orthogonal pricing: a skip edge with a free E/W route takes it instead of a channel loop", {
  # a->c's chord passes x = 55 at y = 37.5. The channel below at
  # min(90 - 9, 20 - 16.1, 55 - 16.1) = 3.9 displaces it by 33.6 and has
  # two bends at 2: 33.6 / 6 + 4 + 4 (e lies below the chord for a->e and
  # c->e) = 13.6. The channel above at 90 + 9 = 99 displaces by 61.5: 10.25
  # + 4 + 6 (b twice and d above) = 20.25. The E/W route at c's own y = 55
  # has two bends and displaces by 17.5: 17.5 / 6 + 4 + 4 = 10.92, so it
  # wins; the E/W run at 99 with four bends costs 10.25 + 8 + 6 = 24.25.
  # An E/W route at a's own y = 20 would cost the same 10.92; running at
  # c's y follows the ew_lo rule (the endpoint y nearest the stack), not
  # the pricing. Without a bend penalty the loop under a scored 9.6 against
  # the E/W route's 6.92, but S/N candidates were tried first and the loop
  # was drawn 0.9 mm inside the margin. c->e mirrors a->c through d's layer.
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
  # a->e runs at e's own y, so it owns the centre row and the two arrivals
  # from above stack at s = min(3.6, (6 - 0.65) / 2) = 2.675: c->e takes the
  # first slot at 20 + 2.675 = 22.675 and d->e the second at 25.35
  expect_equal(runs$coord[nrow(runs)], 22.675, tolerance = 1e-6)
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
  # 25 - 9 = 16 displaces by 39: 39 / 6 + 2 bends at 2 + 2 (d below the
  # chord) = 12.5. S/N above at 85 + 9 = 94: 6.5 + 4 + 4 (b above, for a->b
  # and b->e) = 14.5. An E/W run at 16 has four bends and shares the fan's
  # trunk through gap 1: 6.5 + 8 + 2 = 16.5; at 94 it costs 18.5.
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


# Level tolerance, the ladder, stacked ports, and per-edge resects ---------------

# The point at arc length `s` from the end of a sampled path, the geometry
# the router's per-edge resect names: the arrowhead is drawn from the end of
# the path inward, so a resect of `s` puts the head's tip here.
arc_from_end <- function(path, s) {
  if (length(s) != 1 || !is.finite(s)) {
    return(c(NA_real_, NA_real_))
  }
  path <- dedupe_path(rev_path(path))
  d <- c(0, cumsum(sqrt(diff(path$x)^2 + diff(path$y)^2)))
  k <- max(which(d <= s + 1e-12))
  if (k == nrow(path)) {
    return(c(path$x[[k]], path$y[[k]]))
  }
  f <- (s - d[[k]]) / (d[[k + 1L]] - d[[k]])
  c(
    path$x[[k]] + f * (path$x[[k + 1L]] - path$x[[k]]),
    path$y[[k]] + f * (path$y[[k + 1L]] - path$y[[k]])
  )
}

# The angle, in degrees, between the arrowhead the layer draws and the run it
# is drawn on. ggarrow cuts the path at arc length `resect` from the end and
# aims the head from that cut point at the path's last point, so the head is
# axis-aligned exactly when those two points share a coordinate. Anything the
# path does before the cut plays no part.
head_tilt_degrees <- function(path, resect) {
  path <- dedupe_path(path)
  n <- nrow(path)
  tip <- arc_from_end(path, resect)
  towards <- c(path$x[[n]] - tip[[1]], path$y[[n]] - tip[[2]])
  if (sqrt(sum(towards^2)) < 1e-9) {
    return(0)
  }
  degrees <- atan2(towards[[2]], towards[[1]]) * 180 / pi
  off <- ((degrees %% 90) + 90) %% 90
  min(off, 90 - off)
}

# The number of arrowheads a scene draws at more than `tol` degrees to their
# own run.
tilted_heads <- function(res, tol = 0.5) {
  sum(vapply(
    seq_along(res$paths),
    function(i) {
      if (res$meta$mode[[i]] != "orthogonal") {
        return(0L)
      }
      as.integer(
        head_tilt_degrees(res$paths[[i]], res$meta$resect_head[[i]]) > tol
      )
    },
    integer(1)
  ))
}

# The last axis-aligned run of a path, the run an arrival is drawn along.
last_run <- function(path) {
  runs <- straight_runs(path)
  runs[nrow(runs), ]
}

# The constant coordinate of the last run of edge `label`: the row an
# arrival enters its target on.
arrival_row <- function(scene, res, label) {
  last_run(res$paths[[edge_index(scene, label)]])$coord
}

# Two arrivals at b, one of them level. a sits 1.5 mm below b, inside the
# 2.1 mm corner radius, so a->b is level and stays a straight chord; c
# arrives from below and takes the port row sep_e beneath b's centre. The
# variant tilts b to 2.7 mm above a, past the corner radius, so a->b bends.
near_level_scene <- function(by = 56.5) {
  list(
    nodes = mm_nodes(c("a", "b", "c"), c(20, 80, 20), c(55, by, 20)),
    edges = mm_edges(c("a", "c"), c("b", "b")),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("orthogonal: a chord within the corner radius of level stays straight", {
  scene <- near_level_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  ab <- edge_index(scene, "a->b")
  cb <- edge_index(scene, "c->b")
  ends <- edge_endpoints(scene, ab)
  # a jog of 1.5 mm cannot show two corners of radius 2.1, so the chord is
  # drawn as it is
  expect_equal(res$meta$mode[ab], "straight")
  expect_false(res$meta$routed[ab])
  expect_equal(res$meta$n_waypoints[ab], 0)
  expect_identical(nrow(res$paths[[ab]]), 2L)
  expect_straight_path(res$paths[[ab]], ends$from, ends$to)

  # the level chord owns b's centre row, so c->b takes the port sep_e below
  # it and the two runs are drawn apart
  expect_equal(res$meta$mode[cb], "orthogonal")
  expect_equal(arrival_row(scene, res, "c->b"), 52.9, tolerance = 1e-6)
  expect_equal(shared_run_length(res$paths[[ab]], res$paths[[cb]]), 0)

  # the arrowhead zone of c->b, the 8 mm of path after its resected cap,
  # keeps half a separation from the chord it used to be drawn under
  head <- arc_window(
    res$paths[[cb]],
    cap_default,
    cap_default + 8,
    from_end = TRUE
  )
  expect_gt(nrow(head), 0)
  expect_gte(
    min(point_polyline_dist(head, res$paths[[ab]])),
    sep_e_default / 2 - 1e-6
  )
})

test_that("orthogonal: a chord tilted past the corner radius bends at one slot", {
  scene <- near_level_scene(by = 57.7)
  res <- ortho(scene, corners = "sharp")
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  ab <- edge_index(scene, "a->b")
  expect_equal(res$meta$mode[ab], "orthogonal")
  expect_true(res$meta$routed[ab])
  wp <- res$waypoints[[ab]]
  expect_identical(nrow(wp), 2L)
  expect_equal(wp$x[[1]], wp$x[[2]], tolerance = 1e-6)
  expect_equal(wp$y, c(55, 59.5), tolerance = 1e-6)

  # the 4.5 mm vertical is drawn, and the two arrivals keep distinct slots
  runs <- straight_runs(res$paths[[ab]])
  vertical <- runs[runs$axis == "v", , drop = FALSE]
  expect_identical(nrow(vertical), 1L)
  expect_equal(vertical$length, 4.5, tolerance = 1e-6)
  x_ab <- slot_xs(res$paths[[ab]], c(20, 80))
  x_cb <- slot_xs(res$paths[[edge_index(scene, "c->b")]], c(20, 80))
  expect_length(x_ab, 1)
  expect_length(x_cb, 1)
  expect_gte(abs(x_ab - x_cb), sep_e_default - 1e-9)

  # neither arrival is level now, so no chord owns b's centre row and the
  # two rows are centred on it: both arrive from below, and the leftmost
  # slot takes the higher row, which is the crossing-free order
  rows <- vapply(
    edge_labels(scene$edges),
    function(lab) arrival_row(scene, res, lab),
    numeric(1)
  )
  expect_equal(sort(unname(rows)), c(55.9, 59.5), tolerance = 1e-6)
  expect_equal(unname(rows[["a->b"]]), 57.7 + sep_e_default / 2)
  expect_equal(unname(rows[["c->b"]]), 57.7 - sep_e_default / 2)
})

test_that("canonical multi_mediator: the near-level x->y is a straight chord", {
  # at the large panel x and y differ by 1.89 mm, inside the corner radius,
  # and the chord clears every crossed disc by 26 mm
  scene <- canonical_scene("multi_mediator", c(249.78, 148.18))
  i <- edge_index(scene, "x->y")
  ends <- edge_endpoints(scene, i)
  expect_lte(abs(ends$from[2] - ends$to[2]), rc_default)
  expect_gt(chord_min_clearance(scene, i), r_full)

  res <- ortho(scene)
  expect_equal(res$meta$mode[i], "straight")
  expect_false(res$meta$routed[i])
  expect_identical(nrow(res$paths[[i]]), 2L)
})

# x spans the layer of a and b to reach m, whose own y clears both discs by
# more than R: the two-bend run at m's y displaces as little as the run at
# x's own y and half as much as the channel above the stack.
staircase_scene <- function() {
  list(
    nodes = mm_nodes(
      c("x", "a", "b", "m"),
      c(20, 60, 60, 100),
      c(55, 12, 98, 75)
    ),
    edges = mm_edges(c("x", "x", "x", "a", "b"), c("a", "b", "m", "m", "m")),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("orthogonal: a spanning chord takes the two-bend run at its target's y", {
  scene <- staircase_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  i <- edge_index(scene, "x->m")
  expect_equal(res$meta$mode[i], "orthogonal")
  expect_true(res$meta$clearance_ok[i])
  # the runs at m's y (75) and at x's own y (55) displace 10 mm each and
  # cost the same; the tie goes above
  expect_equal(res$meta$side[i], 1)
  expect_equal(res$meta$n_waypoints[i], 2)
  wp <- res$waypoints[[i]]
  expect_equal(wp$x, c(40, 40), tolerance = 1e-6)
  expect_equal(wp$y, c(55, 75), tolerance = 1e-6)

  # the path stays between the two endpoint rows and leaves x's E port
  # along its own row
  path <- res$paths[[i]]
  expect_true(all(path$y >= 55 - 1e-6 & path$y <= 75 + 1e-6))
  runs <- straight_runs(path)
  expect_equal(runs$axis[[1]], "h")
  expect_equal(runs$coord[[1]], 55, tolerance = 1e-6)
})

test_that("orthogonal: the four-layer sweep keeps its channel at the large panel", {
  # the endpoint runs of p->t are blocked by s2 in the crossed layer, so its
  # candidate table is unchanged and the periphery sweep still wins
  scene <- four_layer_scene(panel = c(249.78, 148.18))
  res <- ortho(scene)
  i <- edge_index(scene, "p->t")
  expect_equal(res$meta$mode[i], "orthogonal")
  expect_equal(res$meta$side[i], 1)
  expect_equal(res$meta$n_waypoints[i], 2)
  wp <- res$waypoints[[i]]
  expect_equal(wp$y, c(130.24, 130.24), tolerance = 1e-4)
})

# The ladder ---------------------------------------------------------------------

test_that("route_constants() derives the minimum slot separation and takes an override", {
  # sep_min = max(0.25 r, 1.5), the floor the ladder tightens the slot
  # spacing to, never wider than the nominal separation
  expect_equal(route_constants(6)$sep_min, sep_min_default)
  expect_equal(route_constants(2)$sep_min, 1.5)
  expect_equal(route_constants(10)$sep_min, 2.5)
  # the user option fixes the spacing at the separation
  expect_equal(route_constants(6, sep_min = 3.6)$sep_min, 3.6)
  expect_lte(route_constants(6)$sep_min, route_constants(6)$sep_e)
})

# The slot x of each edge of the narrow band, in edge order.
narrow_band_slots <- function(scene, res) {
  layers <- infer_layers(scene$nodes, r_default)
  vapply(
    res$paths,
    function(path) {
      x <- slot_xs(path, layers$x)
      expect_length(x, 1)
      x
    },
    numeric(1)
  )
}

test_that("orthogonal ladder: a 40 mm gap takes the stub slack and keeps its slots", {
  # rung 1: 4 slots at sep_e need 24.4 + 10.8 = 35.2 mm of gap, so the stub
  # gives up 1.5 mm at each end, 16.1 down to (40 - 10.8) / 2 = 14.6, and
  # the slots are the same four x values the centred fallback drew, now
  # with the clearance the fallback could not claim
  scene <- narrow_band_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  expect_true(all(res$meta$clearance_ok))
  expect_equal(
    narrow_band_slots(scene, res),
    c(45.4, 41.8, 38.2, 34.6),
    tolerance = 1e-6
  )

  expect_type(res$ortho, "list")
  expect_equal(res$ortho$rc, rc_default)
  gaps <- res$ortho$gaps
  expect_true(is.data.frame(gaps))
  expect_named(
    gaps,
    c("gap", "width", "ranks", "rung", "stub", "spacing")
  )
  expect_identical(nrow(gaps), 1L)
  expect_equal(gaps$gap, 1)
  expect_equal(gaps$width, 40)
  expect_equal(gaps$ranks, 4)
  expect_equal(gaps$rung, 1)
  expect_equal(gaps$stub, 14.6)
  expect_equal(gaps$spacing, sep_e_default)
})

test_that("orthogonal ladder: a 30 mm gap tightens the spacing", {
  # rung 2: the stub is at its floor of 12.2 and the four slots share the
  # remaining 5.6 mm at (30 - 24.4) / 3 = 1.867, still above sep_min
  scene <- narrow_band_scene(gap = 30)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  expect_true(all(res$meta$clearance_ok))
  expect_equal(
    sort(narrow_band_slots(scene, res)),
    c(37.2, 39.0667, 40.9333, 42.8),
    tolerance = 1e-4
  )

  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 2)
  expect_equal(gaps$width, 30)
  expect_equal(gaps$stub, stub_min_default)
  expect_equal(gaps$spacing, (30 - 2 * stub_min_default) / 3, tolerance = 1e-6)
  expect_equal(res$ortho$rc, rc_default)
  expect_gte(gaps$spacing, sep_min_default)
})

test_that("orthogonal ladder: a 27 mm gap tightens the corner radius", {
  # rung 3: at sep_min spacing the four slots need 4.5 mm, leaving 11.25 mm
  # of stub at each end, so rc drops to 11.25 - cap - head = 1.25 and every
  # corner of the scene is drawn at that radius
  scene <- narrow_band_scene(gap = 27)
  res <- ortho(scene)
  expect_true(all(res$meta$clearance_ok))
  expect_equal(res$ortho$rc, 1.25, tolerance = 1e-6)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 3)
  expect_equal(gaps$width, 27)
  expect_equal(gaps$stub, 11.25, tolerance = 1e-6)
  expect_equal(gaps$spacing, sep_min_default)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  slots <- sort(narrow_band_slots(scene, res))
  expect_equal(diff(slots), rep(sep_min_default, 3), tolerance = 1e-6)
  expect_gte(min(slots), 26.5 + 11.25 - 1e-6)
  expect_lte(max(slots), 53.5 - 11.25 + 1e-6)
})

test_that("orthogonal ladder: a 20 mm gap keeps the target-side slot a head's run clear", {
  # rung 4: no stub fits, and 20 mm holds R_soft + cap + head, so the slot
  # nearest the target sits cap + head from the target's layer and the rest
  # follow it at the gap's spacing. The gap's edges lose their clearance
  # either way; what the floor buys is a straight run for every head.
  scene <- narrow_band_scene(gap = 20)
  res <- ortho(scene)
  expect_true(all(res$meta$routed))
  expect_false(any(res$meta$clearance_ok))
  expect_equal(res$ortho$rc, rc_min_default)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 4)
  expect_equal(gaps$width, 20)

  slots <- sort(narrow_band_slots(scene, res))
  expect_length(unique(round(slots, 9)), 4)
  expect_equal(max(slots), 50 - head_run_default, tolerance = 1e-6)
  expect_true(all(diff(slots) >= sep_min_default - 1e-9))
  # 20 mm is 2.8 mm short of the floor and the two soft bands together, and
  # the source side is the one that gives that up: its slot is inside the
  # source's band, and still inside the gap
  expect_lt(min(slots), 30 + r_soft)
  expect_gte(min(slots), 30)
})

test_that("orthogonal ladder: a fixed sep_min falls back to the centred slots", {
  # edge_sep_min = edge_sep makes rung 2 a no-op and rung 3 unreachable, so
  # a 30 mm gap drops to rung 4 with the slots the fallback drew: sep_e
  # apart, without clearance, and 0.4 mm nearer the source than the gap
  # midpoint, which is what the target-side floor costs here
  scene <- narrow_band_scene(gap = 30)
  res <- route_scene(
    scene,
    mode = "orthogonal",
    opts = route_constants(r_default, sep_min = sep_e_default)
  )
  expect_true(all(res$meta$routed))
  expect_false(any(res$meta$clearance_ok))
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(res$ortho$gaps$spacing, sep_e_default)

  slots <- narrow_band_slots(scene, res)
  expect_equal(sort(slots), c(34.2, 37.8, 41.4, 45.0), tolerance = 1e-6)
  expect_equal(max(slots), 55 - head_run_default, tolerance = 1e-6)
})

# `k` sources into one target across one gap, the saturated scene's worst
# gap in miniature. The sources sit 10 mm apart on the left layer, skipping
# the row beside the target, so every interval contains the target's y and
# the segments overlap pairwise and take `k` ranks. `k` is at least 4, so
# that the skipped row falls among the sources.
k_arrival_scene <- function(gap, k) {
  ys <- setdiff(seq(15, by = 10, length.out = k + 1), 55)
  names <- paste0("s", seq_along(ys))
  list(
    nodes = mm_nodes(
      c(names, "t"),
      c(rep(20, length(ys)), 20 + gap),
      c(ys, 60)
    ),
    edges = mm_edges(names, rep("t", length(ys))),
    bounds = c(0, 0, 40 + gap, 10 * (k + 1) + 20)
  )
}

# The nine-source case, the width the rungs of the ladder are pinned at.
nine_arrival_scene <- function(gap) {
  k_arrival_scene(gap, 9)
}

# The x of every vertical run of a routed scene that lies strictly inside
# the single gap.
gap_slots <- function(scene, res) {
  layers <- infer_layers(scene$nodes, r_default)
  sort(unlist(lapply(res$paths, function(path) slot_xs(path, layers$x))))
}

test_that("orthogonal ladder: nine arrivals in a 41.63 mm gap reach the second rung", {
  # rung 1 would need 24.4 + 3.6 * 8 = 53.2 mm; rung 2 keeps the 12.2 mm
  # stub at both ends and shares the remaining 17.23 mm at 2.154
  scene <- nine_arrival_scene(41.63)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  expect_true(all(res$meta$clearance_ok))

  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 2)
  expect_equal(gaps$ranks, 9)
  expect_equal(gaps$width, 41.63)
  expect_equal(gaps$stub, stub_min_default)
  expect_equal(gaps$spacing, (41.63 - 24.4) / 8, tolerance = 1e-6)

  # the slots are read off the sharp polyline: a jog shorter than twice the
  # corner radius is drawn as one continuous curve, with no straight
  # vertical left to measure, and where the slots sit is not a question
  # about rounding
  slots <- gap_slots(scene, ortho(scene, corners = "sharp"))
  expect_length(slots, 9)
  expect_equal(diff(slots), rep((41.63 - 24.4) / 8, 8), tolerance = 1e-6)
  # every join sits at or beyond the rung's stub from the target, so no
  # vertical enters the disc the arrowhead is drawn in
  expect_lte(max(slots), 61.63 - stub_min_default + 1e-6)
  expect_gte(min(slots), 20 + stub_min_default - 1e-6)
  expect_gte(61.63 - max(slots), r_full)
})

test_that("orthogonal ladder: nine arrivals in a 28.93 mm gap give the target its head run", {
  # 24.4 + 1.5 * 8 = 36.4 and 21.6 + 1.5 * 8 = 33.6 both exceed the gap, so
  # it falls to rung 4, where the nine slots would sit r + m_min from either
  # layer. The gap is wider than R_soft + cap + head, so the nine slide
  # 2.8 mm toward the source to leave the target's arrival its head run.
  scene <- nine_arrival_scene(28.93)
  res <- ortho(scene)
  expect_true(all(res$meta$routed))
  expect_false(any(res$meta$clearance_ok))
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(res$ortho$rc, rc_min_default)

  slots <- gap_slots(scene, res)
  expect_length(unique(round(slots, 9)), 9)
  expect_equal(max(slots), 48.93 - head_run_default, tolerance = 1e-6)
  # the source side is inside its own soft band, and inside the gap
  expect_lt(min(slots), 20 + r_soft)
  expect_gte(min(slots), 20)
})

# The x of the vertical run of every path of a scene with a single gap,
# wherever that run sits. ladder_slots() drops a run that lands on a layer's
# centre line, which is the very position a containment claim has to see.
crossing_slots <- function(res) {
  unlist(lapply(res$paths, function(path) {
    runs <- straight_runs(path)
    runs$coord[runs$axis == "v"]
  }))
}

# The x of the vertical run of every path that is not on a layer line. At
# rung 4 a slot can sit closer to a layer than sep_e, or outside the gap
# altogether, and slot_xs() reports neither.
ladder_slots <- function(scene, res) {
  layers <- infer_layers(scene$nodes, r_default)
  vapply(
    res$paths,
    function(path) {
      runs <- straight_runs(path)
      x <- runs$coord[runs$axis == "v"]
      x[!vapply(x, function(v) any(abs(v - layers$x) < 1e-6), logical(1))]
    },
    numeric(1)
  )
}

# Two arrivals crossing one gap into two targets, their y-intervals
# overlapping, so they take two ranks and each head arrives on its target's
# centre row at the full cap resect. The layers sit `gap` apart.
two_arrival_scene <- function(gap) {
  list(
    nodes = mm_nodes(
      c("a1", "a2", "b1", "b2"),
      c(30, 30, 30 + gap, 30 + gap),
      c(20, 35, 50, 65)
    ),
    edges = mm_edges(c("a1", "a2"), c("b1", "b2")),
    bounds = c(0, 0, 60 + gap, 90)
  )
}

# The same scene drawn the other way: the sources are on the right layer and
# the two targets on the left, so the target side of the gap is the left one.
mirrored_two_arrival_scene <- function(gap) {
  scene <- two_arrival_scene(gap)
  scene$edges <- mm_edges(scene$edges$to, scene$edges$from)
  scene
}

test_that("orthogonal ladder: two rung-4 slots span the band from R_soft to the head run", {
  # 20.8 mm is R_soft + cap + head + sep_e, the width at which two slots a
  # separation apart fit the band exactly: the far one keeps cap + head from
  # the target's layer and the near one R_soft from the source's
  scene <- two_arrival_scene(20.8)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(res$ortho$gaps$ranks, 2)
  expect_equal(res$ortho$gaps$width, 20.8)

  expect_equal(
    sort(ladder_slots(scene, res)),
    c(30 + r_soft, 50.8 - head_run_default),
    tolerance = 1e-6
  )
})

test_that("orthogonal ladder: a rung-4 gap crossed leftwards floors its left slot", {
  # the mirror of the same width: the target's layer is the left one, so the
  # floor is measured from it and the source's band is on the right
  scene <- mirrored_two_arrival_scene(20.8)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(res$ortho$gaps$ranks, 2)

  expect_equal(
    sort(ladder_slots(scene, res)),
    c(30 + head_run_default, 50.8 - r_soft),
    tolerance = 1e-6
  )
})

test_that("orthogonal ladder: a gap under 17.2 mm keeps the slots it had", {
  # 16 mm cannot hold the target's floor and the source's soft band at once,
  # so the rung's centred slots stand: four of them 1.5 mm apart about the
  # midpoint at 40, the arrangement this gap has always drawn
  scene <- narrow_band_scene(gap = 16)
  res <- ortho(scene)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 4)
  expect_lt(gaps$width, head_run_gap_default)
  expect_equal(gaps$spacing, sep_min_default)

  expect_equal(
    sort(narrow_band_slots(scene, res)),
    c(37.75, 39.25, 40.75, 42.25),
    tolerance = 1e-6
  )
})

# Two layers `gap` apart crossed in both directions: `a1 -> b1` rightwards
# and `b2 -> a2` leftwards, their y-intervals overlapping, so the two
# segments take two ranks and neither layer is the gap's target side.
mixed_direction_scene <- function(gap) {
  list(
    nodes = mm_nodes(
      c("a1", "a2", "b1", "b2"),
      c(30, 30, 30 + gap, 30 + gap),
      c(20, 80, 50, 35)
    ),
    edges = mm_edges(c("a1", "b2"), c("b1", "a2")),
    bounds = c(0, 0, 60 + gap, 110)
  )
}

# One port crossed in both directions: `a1 -> b1` and `b2 -> a1` both use
# a1's right side, so they share one segment that points both ways.
mixed_port_scene <- function(gap) {
  list(
    nodes = mm_nodes(
      c("a1", "b1", "b2"),
      c(30, 30 + gap, 30 + gap),
      c(20, 55, 80)
    ),
    edges = mm_edges(c("a1", "b2"), c("b1", "a1")),
    bounds = c(0, 0, 60 + gap, 110)
  )
}

test_that("orthogonal ladder: ten rung-4 arrivals keep every slot inside the gap", {
  # ten ranks at sep_min span 13.5 mm, so the centred slots come within
  # 4.75 mm of the source's layer, less than the 5.25 mm the target-side
  # slot would have to move to earn its head run and less than the 5.8 mm
  # band. The slide stops where the source-side slot reaches the source's
  # layer: a slot past it draws a run on the far side of the layer the edge
  # starts from, outside the gap it is crossing.
  scene <- k_arrival_scene(23, 10)
  res <- ortho(scene, corners = "sharp")
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 4)
  expect_equal(gaps$ranks, 10)
  expect_equal(gaps$width, 23)

  slots <- sort(crossing_slots(res))
  expect_length(slots, 10)
  expect_gte(min(slots), 20)
  expect_lte(max(slots), 43)
})

test_that("orthogonal ladder: the band caps the slide short of the head run", {
  # 18 mm is 0.8 mm past the 17.2 mm gate, so the nine centred slots may
  # move 0.8 mm and no further: the target-side slot stops 3.8 mm from the
  # target's layer rather than at the 10 mm head run, and the source-side
  # slot keeps 2.2 mm from the source's layer
  scene <- nine_arrival_scene(18)
  res <- ortho(scene)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 4)
  expect_equal(gaps$ranks, 9)
  expect_equal(gaps$spacing, sep_min_default)

  slots <- sort(ladder_slots(scene, res))
  expect_equal(slots, 22.2 + sep_min_default * (0:8), tolerance = 1e-6)
  expect_equal(min(slots) - 20, 2.2, tolerance = 1e-6)
  expect_equal(38 - max(slots), 3.8, tolerance = 1e-6)
  expect_lt(38 - max(slots), head_run_default)
})

test_that("orthogonal ladder: a rung-4 gap crossed both ways keeps its centred slots", {
  # `a1 -> b1` with `b2 -> a2` leaves the gap without a target side, so
  # neither layer's head run is the one to leave clear: the two slots stay
  # centred on the midpoint a separation apart
  scene <- mixed_direction_scene(20)
  res <- ortho(scene)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 4)
  expect_equal(gaps$ranks, 2)
  expect_equal(
    sort(ladder_slots(scene, res)),
    40 + c(-1, 1) * sep_e_default / 2,
    tolerance = 1e-6
  )

  # the same geometry crossed rightwards by both edges does slide toward
  # the source, so the centred slots above are the mixed gap's own answer
  rightwards <- scene
  rightwards$edges <- mm_edges(c("a1", "a2"), c("b1", "b2"))
  expect_equal(
    sort(ladder_slots(rightwards, ortho(rightwards))),
    c(36.4, 40),
    tolerance = 1e-6
  )
})

test_that("orthogonal ladder: a mixed hyperedge keeps its slot on the midpoint", {
  # `a1 -> b1` and `b2 -> a1` share one port on a1, so one segment carries
  # both directions: it has no target side either, and its single slot
  # stays on the gap's midpoint
  scene <- mixed_port_scene(20)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(res$ortho$gaps$ranks, 1)
  expect_equal(unique(ladder_slots(scene, res)), 40, tolerance = 1e-6)
})

test_that("orthogonal ladder: rung 4 spreads nine slots at sep_min on both sides of the soft bands", {
  # 2 R_soft = 14.4 is the width at which the two layers' soft bands meet.
  # A spread of band / (K - 1) alone gives 0.00125 mm just above that width
  # against sep_e just below it: a 14.4 mm jump in the drawing, and eight
  # verticals a reader takes for one. The floor holds the spacing at
  # sep_min = 1.5 on both sides of the boundary.
  wide <- nine_arrival_scene(14.41)
  narrow <- nine_arrival_scene(14.39)
  res_wide <- ortho(wide)
  res_narrow <- ortho(narrow)

  for (res in list(res_wide, res_narrow)) {
    expect_equal(res$ortho$gaps$rung, 4)
    expect_equal(res$ortho$gaps$spacing, sep_min_default, tolerance = 1e-9)
    expect_true(is.na(res$ortho$gaps$stub))
    expect_false(any(res$meta$clearance_ok))
  }

  # nine sources, so every pair of slots comes from a different source
  slots_wide <- sort(ladder_slots(wide, res_wide))
  slots_narrow <- sort(ladder_slots(narrow, res_narrow))
  expect_length(slots_wide, 9)
  expect_length(slots_narrow, 9)
  expect_true(all(diff(slots_wide) >= sep_min_default - 1e-9))
  expect_true(all(diff(slots_narrow) >= sep_min_default - 1e-9))
  # the 0.02 mm change in the gap moves no slot by a separation
  expect_lte(max(abs(slots_wide - slots_narrow)), sep_e_default)
})

test_that("orthogonal ladder: the target-side slot moves continuously as the gap widens", {
  # The gap swept from rung 4 through rungs 3 and 2, finely across 17.2 mm,
  # the width from which the target-side floor applies. The slot nearest the
  # target is the one every head in the gap is drawn on, so it is the slot a
  # reader follows: it may not move by more than a separation between
  # neighbouring widths. The slots behind it do step at the 3 / 4 handover,
  # where rung 3 measures from its stub and rung 4 from the floor, which is
  # why the property is stated about the target-side slot alone. Within
  # rung 4, including across 17.2 mm, no slot may step at all.
  previous <- NULL
  previous_rung <- NA_integer_
  worst_target <- 0
  worst_rung4 <- 0
  gaps <- sort(unique(c(seq(13, 45, by = 0.05), seq(16.8, 17.6, by = 0.01))))
  for (gap in gaps) {
    scene <- nine_arrival_scene(gap)
    # on the sharp polyline, so that a jog the corners round away entirely
    # still reports the slot it turns at
    res <- ortho(scene, corners = "sharp")
    slots <- sort(ladder_slots(scene, res))
    rung <- res$ortho$gaps$rung
    if (!is.null(previous)) {
      worst_target <- max(worst_target, abs(max(slots) - max(previous)))
      if (rung == 4 && previous_rung == 4) {
        worst_rung4 <- max(worst_rung4, max(abs(slots - previous)))
      }
    }
    previous <- slots
    previous_rung <- rung
  }
  expect_lte(worst_target, sep_e_default + 1e-9)
  expect_lte(worst_rung4, sep_e_default + 1e-9)
})

test_that("orthogonal channels: an S/N channel retries at the shorter stub", {
  # a 72 mm panel leaves 69 mm of room, so the channel above the mediator
  # at 55 + 16.1 = 71.1 does not fit; at the 12.2 mm floor it sits at 67.2
  # and displaces 12.2 against the 16.1 of the channel below
  scene <- list(
    nodes = mm_nodes(c("x", "m", "y"), c(7.3, 80, 152.7), rep(55, 3)),
    edges = mm_edges(c("x", "m", "x"), c("m", "y", "y")),
    bounds = c(0, 0, 160, 72)
  )
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  i <- edge_index(scene, "x->y")
  expect_equal(res$meta$mode[i], "orthogonal")
  expect_equal(res$meta$side[i], 1)
  expect_true(res$meta$clearance_ok[i])
  wp <- res$waypoints[[i]]
  expect_equal(wp$y, c(67.2, 67.2), tolerance = 1e-6)
  expect_gte(
    end_stub_length(res$paths[[i]]),
    cap_default + max(head_default, rc_default) - 1e-9
  )
})

# Stacked ports ------------------------------------------------------------------

# One target with a level source and `n_above` sources above it, all in one
# layer 80 mm away: the gap is wide enough for rung 0, so the ladder leaves
# the joins alone and only the ports decide how many rows the target shows.
stacked_port_scene <- function(n_above) {
  above <- paste0("b", seq_len(n_above))
  ys <- 55 + 15 * seq_len(n_above)
  list(
    nodes = mm_nodes(
      c("t", "a", above),
      c(100, rep(20, n_above + 1)),
      c(55, 55, ys)
    ),
    edges = mm_edges(c("a", above), rep("t", n_above + 1)),
    bounds = c(0, 0, 140, max(ys) + 20)
  )
}

test_that("orthogonal ports: arrivals on one W side take stacked rows", {
  scene <- fan_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  # c->e is level and keeps e's centre row; b->e arrives from above and
  # takes the row sep_e above it, so the two heads are drawn apart
  expect_equal(res$meta$mode[edge_index(scene, "c->e")], "straight")
  expect_equal(arrival_row(scene, res, "c->e"), 55, tolerance = 1e-6)
  expect_equal(arrival_row(scene, res, "b->e"), 58.6, tolerance = 1e-6)
  expect_equal(
    arrival_row(scene, res, "b->e") - arrival_row(scene, res, "c->e"),
    sep_e_default,
    tolerance = 1e-6
  )
  expect_equal(
    shared_run_length(
      res$paths[[edge_index(scene, "b->e")]],
      res$paths[[edge_index(scene, "c->e")]]
    ),
    0
  )

  # a->e arrives at e's S port up the centre line and is unchanged
  ae <- last_run(res$paths[[edge_index(scene, "a->e")]])
  expect_equal(ae$axis, "v")
  expect_equal(ae$coord, 140, tolerance = 1e-6)
})

test_that("orthogonal ports: the four-layer's three arrivals stack at t", {
  scene <- four_layer_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  # s2->t is the level chord and keeps the centre; s1 arrives from above
  # and s3 from below
  expect_equal(res$meta$mode[edge_index(scene, "s2->t")], "straight")
  expect_equal(arrival_row(scene, res, "s1->t"), 58.6, tolerance = 1e-6)
  expect_equal(arrival_row(scene, res, "s3->t"), 51.4, tolerance = 1e-6)
  expect_equal(arrival_row(scene, res, "s2->t"), 55, tolerance = 1e-6)

  # the port rows part the two segments, and they still take distinct
  # slots: one vertical carrying both would read as an edge through s2
  gap3 <- c(100, 140)
  s1 <- slot_xs(res$paths[[edge_index(scene, "s1->t")]], gap3)
  s3 <- slot_xs(res$paths[[edge_index(scene, "s3->t")]], gap3)
  expect_length(s1, 1)
  expect_length(s3, 1)
  expect_gte(abs(s1 - s3), sep_e_default - 1e-9)

  for (pair in list(
    c("s1->t", "s2->t"),
    c("s1->t", "s3->t"),
    c("s2->t", "s3->t")
  )) {
    expect_equal(
      shared_run_length(
        res$paths[[edge_index(scene, pair[[1]])]],
        res$paths[[edge_index(scene, pair[[2]])]]
      ),
      0,
      label = paste(pair, collapse = " | ")
    )
  }
})

test_that("orthogonal ports: two arrivals above a level chord stack at 2.68", {
  # three rows need the outermost head to stay inside the disc silhouette:
  # s = min(sep_e, (r - 0.65) / 2) = 2.675, so the rows are 55, 57.675 and
  # 60.35 and every head is drawn on the node
  scene <- stacked_port_scene(2)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  rows <- vapply(
    edge_labels(scene$edges),
    function(lab) arrival_row(scene, res, lab),
    numeric(1)
  )
  expect_equal(sort(unname(rows)), c(55, 57.675, 60.35), tolerance = 1e-6)
  expect_equal(unname(rows[["a->t"]]), 55, tolerance = 1e-6)
  expect_lte(max(abs(rows - 55)), port_row_max + 1e-9)

  # each row's resect is the closed form cap - r + sqrt(r^2 - offset^2), so
  # all three tips sit cap - r = 2 mm from the disc face along their own run
  resects <- res$meta$resect_head
  names(resects) <- edge_labels(scene$edges)
  expect_equal(unname(resects[["a->t"]]), cap_default, tolerance = 1e-6)
  expect_equal(unname(resects[["b1->t"]]), 7.371, tolerance = 1e-3)
  expect_equal(unname(resects[["b2->t"]]), 4.716, tolerance = 1e-3)
  for (lab in names(resects)) {
    offset <- unname(rows[[lab]]) - 55
    expect_equal(
      unname(resects[[lab]]),
      port_resect_at(offset),
      tolerance = 1e-9,
      label = lab
    )
    tip <- arc_from_end(res$paths[[edge_index(scene, lab)]], resects[[lab]])
    face <- 100 - sqrt(r_default^2 - offset^2)
    expect_equal(tip[[1]], face - face_tip_default, tolerance = 1e-3)
  }

  for (a in 1:2) {
    for (b in (a + 1):3) {
      expect_equal(
        shared_run_length(res$paths[[a]], res$paths[[b]]),
        0,
        label = paste(edge_labels(scene$edges)[c(a, b)], collapse = " | ")
      )
    }
  }
})

test_that("orthogonal ports: a group of four arrivals merges into one row", {
  # a stack keeps its rows only while they are max(sep_e / 2, sep_min) =
  # 1.8 mm apart; four arrivals over a level chord would need 1.34 mm rows,
  # so they merge onto one row of their own at the next free offset and join
  # it at or beyond the gap's stub
  scene <- stacked_port_scene(4)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  expect_equal(res$ortho$gaps$rung, 0)

  labels <- edge_labels(scene$edges)
  rows <- vapply(labels, function(lab) arrival_row(scene, res, lab), numeric(1))
  expect_equal(unname(rows[["a->t"]]), 55, tolerance = 1e-6)
  expect_equal(sort(unique(round(unname(rows), 6))), c(55, 58.6))

  # the merged arrivals share their last run, and none of them shares it
  # with the level chord
  merged <- setdiff(labels, "a->t")
  for (lab in merged) {
    expect_gt(
      shared_run_length(
        res$paths[[edge_index(scene, lab)]],
        res$paths[[edge_index(scene, merged[[1]])]]
      ),
      0
    )
    expect_equal(
      shared_run_length(
        res$paths[[edge_index(scene, lab)]],
        res$paths[[edge_index(scene, "a->t")]]
      ),
      0,
      label = lab
    )
  }

  # every join onto the merged row sits at or beyond the nominal stub
  slots <- gap_slots(scene, res)
  expect_length(slots, 4)
  expect_lte(max(slots), 100 - stub_default + 1e-6)
})

test_that("orthogonal ports: arrivals that cannot hold their rows merge instead of stacking", {
  # node_size 8 draws r = 3. Two arrivals above the level chord would take
  # rows (3 - 0.65) / 2 = 1.175 mm apart, below the row floor of
  # max(sep_e / 2, sep_min) = max(0.9, 1.5) = 1.5 and below the 1.3 mm head
  # width, so the two heads would be drawn on top of each other. The pair
  # merges onto one row instead, at the sep_e of 1.8 that a single row
  # affords, and the level chord keeps the centre.
  scene <- stacked_port_scene(2)
  scene$nodes$r <- 3
  opts <- route_constants(3)
  expect_equal(opts$sep_e, 1.8)
  expect_equal(opts$sep_min, 1.5)
  expect_equal(max(opts$sep_e / 2, opts$sep_min), 1.5)
  res <- route_scene(scene, mode = "orthogonal", opts = opts)

  labels <- edge_labels(scene$edges)
  rows <- vapply(labels, function(lab) arrival_row(scene, res, lab), numeric(1))
  expect_equal(unname(rows[["a->t"]]), 55, tolerance = 1e-6)
  expect_equal(unname(rows[["b1->t"]]), unname(rows[["b2->t"]]))
  expect_equal(sort(unique(round(unname(rows), 6))), c(55, 56.8))
  # two rows closer than the head width draw one head over the other
  expect_gte(min(diff(sort(unique(round(unname(rows), 6))))), opts$head_w)

  # the merged pair shares its row, and neither shares it with the chord
  b1 <- res$paths[[edge_index(scene, "b1->t")]]
  b2 <- res$paths[[edge_index(scene, "b2->t")]]
  at <- res$paths[[edge_index(scene, "a->t")]]
  expect_gt(shared_run_length(b1, b2), 0)
  expect_equal(shared_run_length(b1, at), 0)
  expect_equal(shared_run_length(b2, at), 0)
})

# One target with `n_above` sources above it and `n_below` below, all in one
# layer 80 mm away and none of them level with it, so no chord owns the
# target's centre row and the arrivals are centred on it.
centred_port_scene <- function(n_above, n_below = 0) {
  above <- if (n_above > 0) paste0("b", seq_len(n_above)) else character(0)
  below <- if (n_below > 0) paste0("d", seq_len(n_below)) else character(0)
  ys_above <- 55 + 15 * seq_len(n_above)
  ys_below <- 55 - 15 * seq_len(n_below)
  list(
    nodes = mm_nodes(
      c("t", above, below),
      c(100, rep(20, n_above + n_below)),
      c(55, ys_above, ys_below)
    ),
    edges = mm_edges(c(above, below), rep("t", n_above + n_below)),
    bounds = c(0, 0, 140, max(c(ys_above, 70)) + 20)
  )
}

# The row each arrival at `node` is drawn on, in edge order.
arrival_rows <- function(scene, res, node) {
  labels <- edge_labels(scene$edges)[scene$edges$to == node]
  vapply(labels, function(lab) arrival_row(scene, res, lab), numeric(1))
}

test_that("orthogonal ports: rows with no level owner are centred on the node's line", {
  # Two arrivals from above and nothing at the target's own y: the pair takes
  # the centred rows +- sep_e / 2 rather than the centre and one row above
  # it, so the node's centre line runs between the two heads instead of
  # under one of them. The leftmost slot takes the lower row, which is the
  # order that keeps the two verticals from crossing.
  scene <- centred_port_scene(2)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  rows <- arrival_rows(scene, res, "t")
  expect_equal(unname(rows[["b1->t"]]), 53.2, tolerance = 1e-6)
  expect_equal(unname(rows[["b2->t"]]), 56.8, tolerance = 1e-6)
  expect_equal(mean(rows), 55, tolerance = 1e-6)
  expect_gte(abs(diff(unname(rows))), row_floor_default - 1e-9)
  expect_equal(shared_run_length(res$paths[[1]], res$paths[[2]]), 0)

  # three arrivals take the centre and one row either side of it
  scene <- centred_port_scene(3)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  rows <- arrival_rows(scene, res, "t")
  expect_equal(unname(rows), c(51.4, 55, 58.6), tolerance = 1e-6)
  for (a in 1:2) {
    for (b in (a + 1):3) {
      expect_equal(
        shared_run_length(res$paths[[a]], res$paths[[b]]),
        0,
        label = paste(edge_labels(scene$edges)[c(a, b)], collapse = " | ")
      )
    }
  }

  # one arrival from each side is centred the same way, the one from above
  # on the upper row
  scene <- centred_port_scene(1, 1)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  rows <- arrival_rows(scene, res, "t")
  expect_equal(unname(rows[["b1->t"]]), 56.8, tolerance = 1e-6)
  expect_equal(unname(rows[["d1->t"]]), 53.2, tolerance = 1e-6)
})

test_that("orthogonal ports: a canonical node's two arrivals straddle its centre line", {
  # The same rule on the layout fixture. cascade e, triple_confound x and
  # double_iv y each take two arrivals with nothing at their own y, and each
  # pair straddles the centre at +- sep_e / 2 instead of sitting on the
  # centre and one row beside it.
  cases <- list(
    list(name = "cascade", panel = c(160, 110), node = "e"),
    list(name = "triple_confound", panel = c(160, 110), node = "x"),
    list(name = "triple_confound", panel = c(100, 70), node = "x"),
    list(name = "double_iv", panel = c(160, 110), node = "y"),
    list(name = "double_iv", panel = c(100, 70), node = "y")
  )
  for (case in cases) {
    scene <- canonical_scene(case$name, case$panel)
    res <- ortho(scene)
    label <- sprintf(
      "%s at %d x %d: %s",
      case$name,
      case$panel[1],
      case$panel[2],
      case$node
    )
    rows <- arrival_rows(scene, res, case$node)
    centre <- node_xy(scene, case$node)[[2]]
    expect_equal(
      sort(unname(rows) - centre),
      c(-sep_e_default / 2, sep_e_default / 2),
      tolerance = 1e-6,
      label = label
    )
  }
})

test_that("orthogonal ports: a stack keeps its rows only while the floor holds", {
  # Four arrivals with no level owner still fit: their rows are
  # 2 h / (n - 1) = 3.567 mm apart, above the floor of 1.8, so the outermost
  # sits at h = 5.35 and the whole comb is drawn on the disc.
  scene <- centred_port_scene(4)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  rows <- arrival_rows(scene, res, "t")
  expect_equal(
    unname(rows),
    55 + c(-5.35, -1.7833333, 1.7833333, 5.35),
    tolerance = 1e-6
  )
  expect_gte(min(diff(sort(unname(rows)))), row_floor_default - 1e-9)
  expect_lte(max(abs(rows - 55)), port_row_max + 1e-9)

  # seven would need 1.783 mm rows, under the floor, so the group merges
  # onto one row: with nothing else on the side, that row is the centre
  scene <- centred_port_scene(7)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  rows <- arrival_rows(scene, res, "t")
  expect_equal(unname(rows), rep(55, 7), tolerance = 1e-6)
  expect_lt(2 * port_row_max / 6, row_floor_default)

  # three arrivals over a level chord are the same arithmetic from the other
  # side: h / 3 = 1.783 is under the floor, so they merge onto the chord's
  # first free row at sep_e
  scene <- stacked_port_scene(3)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  rows <- arrival_rows(scene, res, "t")
  expect_equal(unname(rows[["a->t"]]), 55, tolerance = 1e-6)
  expect_equal(
    unname(rows[c("b1->t", "b2->t", "b3->t")]),
    rep(58.6, 3),
    tolerance = 1e-6
  )
  expect_lt(port_row_max / 3, row_floor_default)
})

# Two arrivals at one target, the second of them from a source that sits
# `off` millimetres from the row it is assigned. With a level chord owning
# the centre, that row is sep_e above it, so a source at 55 + sep_e + off
# reaches its row with a jog of `off`.
row_level_scene <- function(off = 1.5) {
  list(
    nodes = mm_nodes(
      c("t", "a", "b"),
      c(100, 20, 20),
      c(55, 55, 55 + sep_e_default + off)
    ),
    edges = mm_edges(c("a", "b"), c("t", "t")),
    bounds = c(0, 0, 140, 110)
  )
}

test_that("orthogonal: a chord within the corner radius of its row is a straight chord", {
  # A jog of 1.5 mm cannot show two corners of radius 2.1 whether it lands on
  # the centre row or on a port row, so the arrival is drawn as the chord
  # from its source to the axis point of the row, and the slot it would have
  # taken is left free.
  scene <- row_level_scene(1.5)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  i <- edge_index(scene, "b->t")
  ends <- edge_endpoints(scene, i)
  path <- res$paths[[i]]
  expect_equal(res$meta$mode[i], "straight")
  expect_false(res$meta$routed[i])
  expect_equal(res$meta$n_waypoints[i], 0)
  expect_identical(nrow(path), 2L)
  expect_equal(c(path$x[[1]], path$y[[1]]), ends$from, tolerance = 1e-9)
  expect_equal(c(path$x[[2]], path$y[[2]]), c(100, 58.6), tolerance = 1e-9)
  expect_length(slot_xs(path, c(20, 100)), 0)

  # a source 2.5 mm off its row is past the corner radius and bends as before
  scene <- row_level_scene(2.5)
  res <- ortho(scene)
  i <- edge_index(scene, "b->t")
  expect_equal(res$meta$mode[i], "orthogonal")
  expect_equal(res$meta$n_waypoints[i], 2)
  expect_equal(arrival_row(scene, res, "b->t"), 58.6, tolerance = 1e-6)
})

test_that("orthogonal: a canonical sub-corner jog onto a port row is drawn straight", {
  # triple_confound's x->m at the small panel is the fixture's own instance:
  # m's arrivals stack, x->m is assigned the row 3.6 mm above m's centre and
  # x sits 1.749 mm below it, so the chord to the row replaces a jog no
  # reader could see as two corners. The chord is tilted 3.8 degrees, the
  # tilt item 48 accepts for a level chord.
  scene <- canonical_scene("triple_confound", c(100, 70))
  res <- ortho(scene)
  i <- edge_index(scene, "x->m")
  path <- res$paths[[i]]
  ends <- edge_endpoints(scene, i)

  expect_equal(res$meta$mode[i], "straight")
  expect_identical(nrow(path), 2L)
  expect_equal(path$y[[2]], ends$to[[2]] + sep_e_default, tolerance = 1e-6)
  expect_equal(path$x[[2]], ends$to[[1]], tolerance = 1e-9)
  jog <- abs(path$y[[1]] - path$y[[2]])
  expect_lte(jog, rc_default)
  tilt <- atan2(jog, abs(path$x[[2]] - path$x[[1]])) * 180 / pi
  expect_lt(tilt, 5)
})

# Rows out of a floored rung-4 gap -----------------------------------------------

# Two sources one gap to the left of a single target, one above it and one
# below, their y-intervals meeting at the target, so the gap carries two
# ranks and both heads arrive at the same node. The layers sit `gap` apart.
floored_pair_scene <- function(gap) {
  list(
    nodes = mm_nodes(c("s1", "s2", "t"), c(20, 20, 20 + gap), c(40, 70, 55)),
    edges = mm_edges(c("s1", "s2"), c("t", "t")),
    bounds = c(0, 0, 40 + gap, 110)
  )
}

# The same pair with a leftward edge across the same gap, so the gap is
# crossed both ways and neither layer is its target side.
mixed_pair_scene <- function(gap) {
  scene <- floored_pair_scene(gap)
  scene$nodes <- rbind(scene$nodes, mm_nodes("u", 20 + gap, 95))
  scene$edges <- rbind(scene$edges, mm_edges("u", "s2"))
  scene$bounds <- c(0, 0, 40 + gap, 130)
  scene
}

# The offset from `node`'s centre line at which each arrival's head is drawn:
# the constant coordinate of the path's last axis-aligned run, or the path's
# own end for a chord drawn straight, which has no run to read.
arrival_offsets <- function(scene, res, node) {
  centre <- node_xy(scene, node)[[2]]
  labels <- edge_labels(scene$edges)[scene$edges$to == node]
  vapply(
    labels,
    function(lab) {
      path <- res$paths[[edge_index(scene, lab)]]
      runs <- straight_runs(path)
      end <- if (nrow(runs) == 0) {
        path$y[[nrow(path)]]
      } else {
        runs$coord[[nrow(runs)]]
      }
      end - centre
    },
    numeric(1)
  )
}

# The offsets a canonical node's arrivals are drawn at, at the small panel.
canonical_arrival_offsets <- function(name, node, panel = c(100, 70)) {
  scene <- canonical_scene(name, panel)
  arrival_offsets(scene, ortho(scene), node)
}

test_that("orthogonal ports: a floored rung-4 gap gives its arrivals rows", {
  # At 20.8 mm the gap's slots have slid until the one nearest the target
  # sits cap + head from its layer, so the run each head is drawn on holds a
  # row as well as the head. The pair straddles the target's centre line at
  # +- sep_e / 2, the arrangement two arrivals with no level owner take, and
  # each tip lands on the disc face of its own row.
  scene <- floored_pair_scene(20.8)
  res <- ortho(scene)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 4)
  expect_equal(gaps$ranks, 2)
  expect_equal(gaps$width, 20.8)

  # the slots are the ones round four placed: the rule reads them, it does
  # not move them
  slots <- sort(ladder_slots(scene, ortho(scene, corners = "sharp")))
  expect_equal(slots, c(27.2, 30.8), tolerance = 1e-6)
  expect_equal(40.8 - max(slots), head_run_default, tolerance = 1e-6)

  offs <- arrival_offsets(scene, res, "t")
  expect_equal(unname(offs[["s1->t"]]), -sep_e_default / 2, tolerance = 1e-6)
  expect_equal(unname(offs[["s2->t"]]), sep_e_default / 2, tolerance = 1e-6)
  expect_gte(abs(diff(unname(offs))), row_floor_default - 1e-9)
  expect_lte(max(abs(offs)), port_row_max + 1e-9)
  expect_equal(shared_run_length(res$paths[[1]], res$paths[[2]]), 0)

  # the head of a ported arrival stops at its row's face, and it is drawn
  # along the run, not across the corner behind it
  expect_equal(
    res$meta$resect_head,
    rep(port_resect_at(sep_e_default / 2), 2),
    tolerance = 1e-6
  )
  for (i in 1:2) {
    expect_lte(
      head_tilt_degrees(res$paths[[i]], res$meta$resect_head[[i]]),
      0.5
    )
  }
})

test_that("orthogonal ports: a rung-4 gap short of the floor keeps the centre row", {
  # 16 mm cannot hold the target's floor and the source's soft band at once,
  # so the slots do not move and the run left to the target is 7.2 mm, too
  # short to carry a row as well as a head. Both arrivals stay on the centre
  # line, exactly as they are drawn today.
  scene <- floored_pair_scene(16)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(
    sort(ladder_slots(scene, ortho(scene, corners = "sharp"))),
    c(27.2, 28.8),
    tolerance = 1e-6
  )
  expect_equal(unname(arrival_offsets(scene, res, "t")), c(0, 0))

  # 18 mm is past the 17.2 mm gate but the band caps the slide at 0.8 mm, so
  # the slot nearest the target stops 8 mm from its layer, short of the
  # 10 mm floor. A partly floored gap is not a floored one.
  scene <- floored_pair_scene(18)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  slots <- sort(ladder_slots(scene, ortho(scene, corners = "sharp")))
  expect_equal(slots, c(26.4, 30), tolerance = 1e-6)
  expect_lt(38 - max(slots), head_run_default)
  expect_equal(unname(arrival_offsets(scene, res, "t")), c(0, 0))

  # the nine-source gap at the same width is the room-capped case of the
  # same thing: its target-side slot stops 3.8 mm out and all nine arrivals
  # keep the centre row
  scene <- nine_arrival_scene(18)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(unname(arrival_offsets(scene, res, "t")), rep(0, 9))
})

test_that("orthogonal ports: a rung-4 gap with no target side keeps the centre row", {
  # `u -> s2` crosses the same gap leftwards, so neither layer is the gap's
  # target side and the slots stay centred on the midpoint. Without the
  # slide there is no floor to reach, so the two arrivals at `t` keep the
  # centre row at a width that would otherwise give them rows.
  scene <- mixed_pair_scene(20.8)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(res$ortho$gaps$ranks, 2)
  expect_equal(
    sort(unique(round(crossing_slots(ortho(scene, corners = "sharp")), 6))),
    30.4 + c(-1, 1) * sep_e_default / 2,
    tolerance = 1e-6
  )
  expect_equal(unname(arrival_offsets(scene, res, "t")), c(0, 0))
})

test_that("orthogonal ports: rows hold as the floored gap widens", {
  # Swept from just past the width at which the floor is first reached
  # (58 / 3 = 19.33 mm for this pair) up to 30 mm, across the rung 4, 3, 2
  # and 1 handovers. The rows are a property of the target, not of the
  # ladder, so they may not move at all over the sweep, and no slot may step
  # by more than a separation. The switch itself is excluded by design: rows
  # appear at the width the floor is reached, and that is a step.
  rows <- c(-sep_e_default / 2, sep_e_default / 2)
  worst_row <- 0
  worst_slot <- 0
  previous <- NULL
  rungs <- integer(0)
  for (gap in seq(19.35, 30, by = 0.05)) {
    scene <- floored_pair_scene(gap)
    res <- ortho(scene, corners = "sharp")
    rungs <- c(rungs, res$ortho$gaps$rung)
    offs <- sort(unname(arrival_offsets(scene, res, "t")))
    worst_row <- max(worst_row, max(abs(offs - rows)))
    slots <- sort(crossing_slots(res))
    if (!is.null(previous)) {
      worst_slot <- max(worst_slot, max(abs(slots - previous)))
    }
    previous <- slots
  }
  expect_true(4L %in% rungs)
  expect_true(any(rungs < 4L))
  expect_lte(worst_row, 1e-6)
  expect_lte(worst_slot, sep_e_default + 1e-9)
})

test_that("orthogonal ports: napkin's shared target draws its arrivals apart", {
  # At the small panel every gap of the napkin falls to rung 4 and every one
  # of them reaches the floor. `u1 -> a` is level with `a` and keeps its
  # centre row as the level owner; the other two take the first free row on
  # their own side, so the three heads are drawn on three lines instead of
  # one.
  scene <- canonical_scene("napkin", c(100, 70))
  res <- ortho(scene)
  expect_true(all(res$ortho$gaps$rung == 4))
  expect_true(all(res$ortho$gaps$width >= head_run_gap_default))

  offs <- arrival_offsets(scene, res, "a")
  expect_equal(unname(offs[["u1->a"]]), 0, tolerance = 1e-6)
  expect_equal(unname(offs[["u2->a"]]), -sep_e_default, tolerance = 1e-6)
  expect_equal(unname(offs[["z->a"]]), sep_e_default, tolerance = 1e-6)
  expect_equal(res$meta$mode[edge_index(scene, "u1->a")], "straight")

  # the two ported heads no longer land on the same point
  tip_of <- function(lab) {
    i <- edge_index(scene, lab)
    arc_from_end(res$paths[[i]], res$meta$resect_head[[i]])
  }
  expect_equal(
    sqrt(sum((tip_of("u2->a") - tip_of("z->a"))^2)),
    2 * sep_e_default,
    tolerance = 1e-6
  )
  expect_gte(sqrt(sum((tip_of("u2->a") - tip_of("u1->a"))^2)), 3.5)
  expect_gte(sqrt(sum((tip_of("z->a") - tip_of("u1->a"))^2)), 3.5)

  # y's arrival from u2 runs level into it and owns the centre row; the two
  # arrivals above fit at h / 2 = 2.675, the spacing R4 allows without
  # merging
  offs <- arrival_offsets(scene, res, "y")
  expect_equal(unname(offs[["u2->y"]]), 0, tolerance = 1e-6)
  expect_equal(unname(offs[["a->y"]]), port_row_max / 2, tolerance = 1e-6)
  expect_equal(unname(offs[["m->y"]]), port_row_max, tolerance = 1e-6)
})

test_that("orthogonal ports: five floored arrivals are centred on their target", {
  # epidemiology's health takes five arrivals at the small panel, none of
  # them level with it, so R3 centres the five rows on its centre line and
  # R4 sets the spacing at 2 h / (n - 1) = 2.675, above the 1.8 mm floor.
  scene <- canonical_scene("epidemiology", c(100, 70))
  res <- ortho(scene)
  offs <- arrival_offsets(scene, res, "health")
  expect_length(offs, 5)
  expect_equal(
    sort(unname(offs)),
    port_row_max * c(-1, -0.5, 0, 0.5, 1),
    tolerance = 1e-6
  )
  expect_equal(unname(offs[["age->health"]]), -port_row_max, tolerance = 1e-6)
  expect_equal(
    unname(offs[["ses->health"]]),
    -port_row_max / 2,
    tolerance = 1e-6
  )
  expect_equal(unname(offs[["edu->health"]]), 0, tolerance = 1e-6)
  expect_equal(
    unname(offs[["gene->health"]]),
    port_row_max / 2,
    tolerance = 1e-6
  )
  expect_equal(unname(offs[["income->health"]]), port_row_max, tolerance = 1e-6)
  expect_equal(mean(offs), 0, tolerance = 1e-6)
  expect_gte(min(diff(sort(unname(offs)))), row_floor_default - 1e-9)
})

test_that("orthogonal ports: a floored stack merges under the row floor", {
  # multi_mediator at the small panel. y takes a level chord from x and a
  # level run from m1, both on its centre row, and three arrivals from
  # above: h / 3 = 1.783 is under the 1.8 mm floor, so the three merge onto
  # the first free row at sep_e and their heads still coincide, the degrade
  # R4 accepts rather than move the floor. m2 has an owner and one arrival
  # on each side, which fit, and m3 has a pair with no owner.
  offs <- canonical_arrival_offsets("multi_mediator", "y")
  expect_equal(unname(offs[["x->y"]]), 0, tolerance = 1e-6)
  expect_equal(unname(offs[["m1->y"]]), 0, tolerance = 1e-6)
  expect_equal(
    unname(offs[c("m2->y", "m3->y", "u->y")]),
    rep(sep_e_default, 3),
    tolerance = 1e-6
  )
  expect_lt(port_row_max / 3, row_floor_default)

  offs <- canonical_arrival_offsets("multi_mediator", "m2")
  expect_equal(unname(offs[["x->m2"]]), 0, tolerance = 1e-6)
  expect_equal(unname(offs[["m1->m2"]]), -sep_e_default, tolerance = 1e-6)
  expect_equal(unname(offs[["u->m2"]]), sep_e_default, tolerance = 1e-6)

  offs <- canonical_arrival_offsets("multi_mediator", "m3")
  expect_equal(unname(offs[["m2->m3"]]), -sep_e_default / 2, tolerance = 1e-6)
  expect_equal(unname(offs[["x->m3"]]), sep_e_default / 2, tolerance = 1e-6)
})

test_that("orthogonal ports: the other floored shared targets take their rows", {
  # The remaining canonical scenes whose small-panel gaps reach the floor.
  # deep_confound's b is a pair with no owner and straddles the centre; its
  # d is an owner with two arrivals above; its c an owner with one. The
  # complex chain's c and e are the same two shapes again.
  offs <- canonical_arrival_offsets("deep_confound", "b")
  expect_equal(unname(offs[["a->b"]]), -sep_e_default / 2, tolerance = 1e-6)
  expect_equal(unname(offs[["u->b"]]), sep_e_default / 2, tolerance = 1e-6)

  offs <- canonical_arrival_offsets("deep_confound", "d")
  expect_equal(unname(offs[["a->d"]]), 0, tolerance = 1e-6)
  expect_equal(unname(offs[["b->d"]]), port_row_max / 2, tolerance = 1e-6)
  expect_equal(unname(offs[["c->d"]]), port_row_max, tolerance = 1e-6)

  offs <- canonical_arrival_offsets("deep_confound", "c")
  expect_equal(unname(offs[["u->c"]]), 0, tolerance = 1e-6)
  expect_equal(unname(offs[["b->c"]]), -sep_e_default, tolerance = 1e-6)

  offs <- canonical_arrival_offsets("complex_chain", "c")
  expect_equal(unname(offs[["a->c"]]), 0, tolerance = 1e-6)
  expect_equal(unname(offs[["b->c"]]), sep_e_default, tolerance = 1e-6)

  offs <- canonical_arrival_offsets("complex_chain", "e")
  expect_equal(unname(offs[["a->e"]]), 0, tolerance = 1e-6)
  expect_equal(unname(offs[["c->e"]]), port_row_max / 2, tolerance = 1e-6)
  expect_equal(unname(offs[["d->e"]]), port_row_max, tolerance = 1e-6)
})

test_that("orthogonal ladder: a scene with no floored gap is unchanged", {
  # The 16 mm band is too narrow to reach the floor, so nothing about it
  # moves: the four slots are the ones pinned before rows in rung-4 gaps
  # existed, and every arrival is still drawn on its target's centre line.
  scene <- narrow_band_scene(gap = 16)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  expect_lt(res$ortho$gaps$width, head_run_gap_default)
  expect_equal(
    sort(narrow_band_slots(scene, res)),
    c(37.75, 39.25, 40.75, 42.25),
    tolerance = 1e-6
  )
  for (node in c("b1", "b2", "b3", "b4")) {
    expect_equal(unname(arrival_offsets(scene, res, node)), 0, tolerance = 1e-6)
  }

  # two arrivals into separate targets across a floored gap are unchanged as
  # well: a row assignment of one arrival is the centre row
  scene <- two_arrival_scene(20.8)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(unname(arrival_offsets(scene, res, "b1")), 0, tolerance = 1e-6)
  expect_equal(unname(arrival_offsets(scene, res, "b2")), 0, tolerance = 1e-6)
  expect_equal(
    sort(ladder_slots(scene, ortho(scene, corners = "sharp"))),
    c(30 + r_soft, 50.8 - head_run_default),
    tolerance = 1e-6
  )
})

test_that("orthogonal ports: the chain's N ports keep their sep_e / 2 stack", {
  # a's departure and c's arrival share no side, and the two edges at c's N
  # side keep the +- sep_e / 2 offsets the two-member stack reproduces
  scene <- chain_scene()
  res <- ortho(scene)
  runs_ac <- straight_runs(res$paths[[edge_index(scene, "a->c")]])
  runs_ce <- straight_runs(res$paths[[edge_index(scene, "c->e")]])
  v_ac <- runs_ac[runs_ac$axis == "v", , drop = FALSE]
  v_ce <- runs_ce[runs_ce$axis == "v", , drop = FALSE]
  expect_equal(
    v_ac$coord[[nrow(v_ac)]],
    80 - sep_e_default / 2,
    tolerance = 1e-6
  )
  expect_equal(v_ce$coord[[1]], 80 + sep_e_default / 2, tolerance = 1e-6)
  expect_equal(v_ac$coord[[1]], 20, tolerance = 1e-6)
  expect_equal(v_ce$coord[[nrow(v_ce)]], 140, tolerance = 1e-6)
})

# Five layers of paired nodes, every endpoint at the bottom of its layer
# with a blocker above it, so each spanning chord is level, blocked, and
# channels below. a->d has the shortest span and takes a's stub line at
# 40 - 16.1 = 23.9; a->y stacks sep_e beneath it at 20.3 and b->y beneath
# that at 16.7. a's S side therefore carries two departures and y's S side
# two arrivals, each pair from a different channel.
s_port_scene <- function() {
  list(
    nodes = mm_nodes(
      c("b", "a", "m", "d", "y", "b2", "a2", "m2", "d2", "y2"),
      rep(c(20, 60, 100, 140, 180), 2),
      c(rep(40, 5), rep(80, 5))
    ),
    edges = mm_edges(c("a", "b", "a"), c("y", "y", "d")),
    bounds = c(0, 0, 200, 110)
  )
}

# The scene reflected in the panel's horizontal centre line, which turns
# every S side into the N side of the same picture.
mirror_scene <- function(scene) {
  scene$nodes$y <- scene$bounds[[2]] + scene$bounds[[4]] - scene$nodes$y
  scene
}

mirror_path <- function(path, scene) {
  path$y <- scene$bounds[[2]] + scene$bounds[[4]] - path$y
  path
}

# The x of the first and the last vertical run of a path: the stubs an S or
# N channel drops from its source and raises to its target.
stub_xs <- function(path) {
  v <- straight_runs(path)
  v <- v[v$axis == "v", , drop = FALSE]
  c(v$coord[[1]], v$coord[[nrow(v)]])
}

test_that("orthogonal ports: two channel stubs on an S side stack in the mirror of the N order", {
  scene <- s_port_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  expect_true(all(res$meta$side == -1))

  ay <- edge_index(scene, "a->y")
  by <- edge_index(scene, "b->y")
  ad <- edge_index(scene, "a->d")
  channel <- function(i) channel_run(res$paths[[i]], 100)$coord
  expect_equal(channel(ad), 23.9, tolerance = 1e-6)
  expect_equal(channel(ay), 20.3, tolerance = 1e-6)
  expect_equal(channel(by), 16.7, tolerance = 1e-6)

  # neither port crosses its stubs: the inner channel of a pair keeps the
  # port on the side its own run lies on, and the outer channel takes the
  # far one
  expect_identical(count_paths_crossing(res$paths[[ay]], res$paths[[by]]), 0L)
  expect_identical(count_paths_crossing(res$paths[[ay]], res$paths[[ad]]), 0L)
  half <- sep_e_default / 2
  expect_equal(stub_xs(res$paths[[ad]])[[1]], 60 + half, tolerance = 1e-6)
  expect_equal(stub_xs(res$paths[[ay]])[[1]], 60 - half, tolerance = 1e-6)
  expect_equal(stub_xs(res$paths[[ay]])[[2]], 180 - half, tolerance = 1e-6)
  expect_equal(stub_xs(res$paths[[by]])[[2]], 180 + half, tolerance = 1e-6)

  # the N side of the mirrored scene draws the mirror image, crossings and
  # all
  flipped <- mirror_scene(scene)
  res_n <- ortho(flipped)
  expect_true(all(res_n$meta$side == 1))
  expect_identical(
    count_paths_crossing(res_n$paths[[ay]], res_n$paths[[by]]),
    0L
  )
  expect_identical(
    count_paths_crossing(res_n$paths[[ay]], res_n$paths[[ad]]),
    0L
  )
  for (i in seq_len(nrow(scene$edges))) {
    expect_lt(
      polyline_hausdorff(res_n$paths[[i]], mirror_path(res$paths[[i]], scene)),
      1e-9,
      label = edge_labels(scene$edges)[i]
    )
  }
})

# Per-edge resects ---------------------------------------------------------------

test_that("orthogonal resects: the router puts every tip cap - r from the disc face", {
  # a->c arrives at c's N port 1.8 mm off the centre line, where the disc
  # face sits sqrt(r^2 - 1.8^2) = 5.724 mm above the centre: the head is
  # resected by cap - r + 5.724 = 7.724 so that its tip sits cap - r = 2 mm
  # past the face, as a centre port's tip does. c->e leaves the same side
  # and its fins ask for the same.
  scene <- chain_scene()
  res <- ortho(scene)
  n <- nrow(scene$edges)
  expect_length(res$meta$resect_head, n)
  expect_length(res$meta$resect_fins, n)

  ac <- edge_index(scene, "a->c")
  ce <- edge_index(scene, "c->e")
  expect_equal(res$meta$resect_head[[ac]], 7.724, tolerance = 1e-3)
  expect_equal(res$meta$resect_fins[[ce]], 7.724, tolerance = 1e-3)
  expect_equal(
    res$meta$resect_head[-ac],
    rep(cap_default, n - 1),
    tolerance = 1e-3
  )
  expect_equal(
    res$meta$resect_fins[-ce],
    rep(cap_default, n - 1),
    tolerance = 1e-3
  )

  # the tip of the head sits 2 mm past c's disc face on the port's own axis
  face <- 55 + sqrt(r_default^2 - (sep_e_default / 2)^2)
  expect_equal(
    arc_from_end(res$paths[[ac]], res$meta$resect_head[[ac]]),
    c(78.2, face + face_tip_default),
    tolerance = 1e-3
  )
  expect_equal(face + face_tip_default, 62.724, tolerance = 1e-3)
})

test_that("orthogonal resects: an offset W port shortens the head resect", {
  # b->e enters e's W port 3.6 mm above the centre line and runs on to the
  # axis point (140, 58.6). The disc face on that row is 4.8 mm from the
  # centre, so the resect is cap - r + 4.8 = 6.8 and the tip sits 2 mm past
  # the face at x = 133.2, the same 2 mm as the level c->e's tip at x = 132.
  scene <- fan_scene()
  res <- ortho(scene)
  be <- edge_index(scene, "b->e")
  ce <- edge_index(scene, "c->e")
  expect_equal(res$meta$resect_head[[be]], 6.8, tolerance = 1e-3)
  expect_equal(
    res$meta$resect_head[[be]],
    port_resect_at(sep_e_default),
    tolerance = 1e-9
  )
  expect_equal(res$meta$resect_head[[ce]], cap_default, tolerance = 1e-6)
  face <- 140 - sqrt(r_default^2 - sep_e_default^2)
  expect_equal(
    arc_from_end(res$paths[[be]], res$meta$resect_head[[be]]),
    c(face - face_tip_default, 58.6),
    tolerance = 1e-3
  )
  expect_equal(
    arc_from_end(res$paths[[ce]], res$meta$resect_head[[ce]]),
    c(140 - r_default - face_tip_default, 55),
    tolerance = 1e-6
  )
})

test_that("orthogonal ports: an offset port's path ends at its axis point", {
  # A W or E port's run carries on to the node's own x, an N or S port's to
  # its own y, and the path ends there. The last stretch is hidden under the
  # disc, and because the end lies on the run the arrow layer has nothing
  # oblique to aim the head along.
  fan <- fan_scene()
  res <- ortho(fan)
  be <- res$paths[[edge_index(fan, "b->e")]]
  ce <- res$paths[[edge_index(fan, "c->e")]]
  expect_equal(
    unname(unlist(be[nrow(be), c("x", "y")])),
    c(140, 58.6),
    tolerance = 1e-9
  )
  # the level chord keeps the centre, so its own end is exact
  expect_identical(c(ce$x[[nrow(ce)]], ce$y[[nrow(ce)]]), node_xy(fan, "e"))

  chain <- chain_scene()
  res <- ortho(chain)
  ac <- res$paths[[edge_index(chain, "a->c")]]
  ce <- res$paths[[edge_index(chain, "c->e")]]
  expect_equal(
    unname(unlist(ac[nrow(ac), c("x", "y")])),
    c(80 - sep_e_default / 2, 55),
    tolerance = 1e-9
  )
  expect_equal(
    unname(unlist(ce[1, c("x", "y")])),
    c(80 + sep_e_default / 2, 55),
    tolerance = 1e-9
  )

  four <- four_layer_scene()
  res <- ortho(four)
  for (case in list(c("s1->t", "58.6"), c("s3->t", "51.4"))) {
    path <- res$paths[[edge_index(four, case[[1]])]]
    expect_equal(
      unname(unlist(path[nrow(path), c("x", "y")])),
      c(140, as.numeric(case[[2]])),
      tolerance = 1e-9,
      label = case[[1]]
    )
  }

  # every scene's ends are axis points, and every last segment runs along
  # the port's own axis into it
  for (scene in list(fan, chain, four, stacked_port_scene(2), s_port_scene())) {
    res <- ortho(scene)
    for (i in seq_len(nrow(scene$edges))) {
      label <- edge_labels(scene$edges)[i]
      path <- res$paths[[i]]
      ends <- edge_endpoints(scene, i)
      expect_port_endpoints(path, ends$from, ends$to, label = label)
      if (res$meta$mode[i] != "orthogonal") {
        next
      }
      last <- dedupe_path(path)
      last <- last[c(nrow(last) - 1L, nrow(last)), , drop = FALSE]
      expect_true(
        segment_axes(last) != "o",
        label = paste(label, "last segment")
      )
    }
  }
})

# The scenes the head census runs over: the hand fixtures and the canonical
# DAGs at both panels.
head_census_scenes <- function() {
  scenes <- list(
    fan_scene(),
    four_layer_scene(),
    mediator_scene(),
    chain_scene(),
    s_port_scene(),
    stacked_port_scene(1),
    stacked_port_scene(2),
    stacked_port_scene(4),
    centred_port_scene(2),
    centred_port_scene(3),
    centred_port_scene(4),
    near_level_scene(57.7)
  )
  for (panel in canonical_panels) {
    for (nm in names(canonical_dag_specs)) {
      scenes[[length(scenes) + 1]] <- canonical_scene(nm, panel)
    }
  }
  scenes
}

test_that("orthogonal heads: no head is drawn at an angle to the run it sits on", {
  # The arrow layer cuts each path at its own resect and aims the head from
  # the cut point at the path's last point, so a head is straight exactly
  # when those two share a coordinate. Over every hand fixture and every
  # canonical scene whose gaps all hold a stub, no head is drawn more than
  # half a degree off its run. The scenes whose gaps fall to the ladder's
  # last rung are excluded: there the slot nearest the target sits inside
  # the cap and the tip lands on a corner, which is a separate question.
  n_scenes <- 0L
  n_heads <- 0L
  for (scene in head_census_scenes()) {
    res <- ortho(scene)
    gaps <- res$ortho$gaps
    if (!is.null(gaps) && nrow(gaps) > 0 && any(gaps$rung > 3)) {
      next
    }
    n_scenes <- n_scenes + 1L
    n_heads <- n_heads + sum(res$meta$mode == "orthogonal")
    expect_equal(
      tilted_heads(res),
      0L,
      label = paste0(
        scene$name %||% "fixture",
        " tilted heads"
      )
    )
  }
  # the census is worth having only if it covers the pictures: the scenes
  # above draw hundreds of orthogonal heads between them
  expect_gt(n_scenes, 30)
  expect_gt(n_heads, 200)
})

# The index of the gap an arrival comes out of: the gap holding the x where
# the path's last run begins, or `NA` when the arrival takes no slot there,
# as an S/N channel arriving along a layer line does.
arrival_gap <- function(scene, res, i) {
  if (res$meta$mode[[i]] != "orthogonal") {
    return(NA_integer_)
  }
  layers <- infer_layers(scene$nodes, r_default)
  path <- dedupe_path(res$paths[[i]])
  runs <- straight_runs(path)
  last <- runs[nrow(runs), ]
  if (last$axis != "h") {
    return(NA_integer_)
  }
  x <- path$x[[last$from]]
  g <- which(
    layers$x[-layers$n] < x - 1e-6 & layers$x[-1] > x + 1e-6
  )
  if (length(g) != 1) {
    return(NA_integer_)
  }
  g
}

test_that("orthogonal heads: a rung-4 arrival is straight once the gap holds the floor", {
  # The scenes of the census above, this time keeping the ones whose gaps
  # fall to the ladder's last rung. A gap of at least R_soft + cap + head
  # keeps the slot nearest the target a head's run from its layer, so every
  # head that arrives out of one of those gaps is drawn along its own run.
  # The narrower gaps cannot hold the floor and are counted, not pinned:
  # over half of their arrivals are still drawn on a corner, which is a
  # question about gaps too narrow for a head, not about the floor.
  wide <- 0L
  wide_tilted <- 0L
  narrow <- 0L
  for (scene in head_census_scenes()) {
    res <- ortho(scene)
    gaps <- res$ortho$gaps
    if (is.null(gaps) || nrow(gaps) == 0) {
      next
    }
    for (i in seq_len(nrow(scene$edges))) {
      g <- arrival_gap(scene, res, i)
      if (is.na(g)) {
        next
      }
      row <- gaps[gaps$gap == g, , drop = FALSE]
      if (nrow(row) != 1 || row$rung != 4) {
        next
      }
      if (row$width < head_run_gap_default - 1e-9) {
        narrow <- narrow + 1L
        next
      }
      wide <- wide + 1L
      tilt <- head_tilt_degrees(res$paths[[i]], res$meta$resect_head[[i]])
      if (tilt > 0.5) {
        wide_tilted <- wide_tilted + 1L
      }
      expect_lte(
        tilt,
        0.5,
        label = paste0(
          scene$name %||% "fixture",
          " ",
          edge_labels(scene$edges)[[i]],
          " head tilt"
        )
      )
    }
  }
  expect_equal(wide_tilted, 0L)
  # the census is worth having only if the pictures put arrivals in such
  # gaps: forty of the scenes' arrivals come out of one
  expect_gt(wide, 30L)
  expect_gt(narrow, 0L)
})

test_that("orthogonal heads: a row in a floored gap keeps its head on its run", {
  # The head census again, over the arrivals that come out of a rung-4 gap
  # wide enough to hold the floor. Each is drawn along its own row: the last
  # run of the sharp polyline sits on the row, the resect is the face resect
  # of that row, and the head is not tilted. The count of arrivals drawn off
  # the centre line is pinned too, since a rule that gave rows to none of
  # them would satisfy everything else here.
  wide <- 0L
  ported <- 0L
  for (scene in head_census_scenes()) {
    res <- ortho(scene)
    sharp <- ortho(scene, corners = "sharp")
    gaps <- res$ortho$gaps
    if (is.null(gaps) || nrow(gaps) == 0) {
      next
    }
    for (i in seq_len(nrow(scene$edges))) {
      g <- arrival_gap(scene, res, i)
      if (is.na(g)) {
        next
      }
      row <- gaps[gaps$gap == g, , drop = FALSE]
      if (nrow(row) != 1 || row$rung != 4) {
        next
      }
      if (row$width < head_run_gap_default - 1e-9) {
        next
      }
      wide <- wide + 1L
      label <- paste0(
        scene$name %||% "fixture",
        " ",
        edge_labels(scene$edges)[[i]]
      )
      centre <- node_xy(scene, scene$edges$to[[i]])[[2]]
      last <- last_run(res$paths[[i]])
      last_sharp <- last_run(sharp$paths[[i]])
      off <- last$coord - centre
      if (abs(off) > 1e-9) {
        ported <- ported + 1L
      }
      expect_lte(abs(off), port_row_max + 1e-9, label = label)
      expect_equal(last_sharp$axis, "h", label = label)
      expect_equal(
        last_sharp$coord,
        last$coord,
        tolerance = 1e-6,
        label = label
      )
      expect_equal(
        res$meta$resect_head[[i]],
        port_resect_at(off),
        tolerance = 1e-6,
        label = label
      )
      expect_lte(
        head_tilt_degrees(res$paths[[i]], res$meta$resect_head[[i]]),
        0.5,
        label = label
      )
    }
  }
  expect_gt(wide, 30L)
  expect_gt(ported, 20L)
})

# The panel's routed paths as the arrow grob the layer draws them with, so
# that the resect each head is cut at is the one ggarrow is handed. The
# colours and widths play no part in where a head points; they are here
# because `routed_arrow_grob()` reads them off the drawn data.
routed_grob <- function(res) {
  n <- length(res$paths)
  edges <- data.frame(
    linewidth = rep(0.5, n),
    colour = "black",
    stroke_colour = NA_character_,
    alpha = NA_real_,
    linetype = 1,
    stroke_width = 0,
    stringsAsFactors = FALSE
  )
  par <- list(
    resect = list(head = cap_default, fins = 0),
    length = list(head = NULL, fins = NULL, mid = 4),
    arrow = list(head = ggarrow::arrow_head_wings(), fins = NULL, mid = NULL),
    justify = 0,
    force_arrow = FALSE,
    mid_place = 0.5,
    linejoin = "round",
    linemitre = 10,
    lineend = "butt"
  )
  routed_arrow_grob(edges, res$paths, par, res$meta)
}

# The angle of every head the grob draws, in degrees off its own run, read
# from the grob's own millimetres and its own resect.
grob_head_tilts <- function(res) {
  grob <- routed_grob(res)
  x <- as.numeric(grid::convertX(grob$x, "mm"))
  y <- as.numeric(grid::convertY(grob$y, "mm"))
  resect <- rep_len(
    as.numeric(grid::convertUnit(grob$resect$head, "mm")),
    length(res$paths)
  )
  id <- rep(seq_along(res$paths), vapply(res$paths, nrow, integer(1)))
  vapply(
    seq_along(res$paths),
    function(i) {
      head_tilt_degrees(data.frame(x = x[id == i], y = y[id == i]), resect[[i]])
    },
    numeric(1)
  )
}

test_that("routed arrows: a head at the end of a cap + head run is drawn along it", {
  skip_if_not_installed("ggarrow")
  # The 20 mm gap's target-side slot leaves exactly cap + head of run before
  # the disc, the shortest run the floor allows. ggarrow cuts the path at
  # the router's resect and aims the head at the path's last point, so both
  # lie on that run and every head is drawn along it.
  scene <- narrow_band_scene(gap = 20)
  sharp <- ortho(scene, corners = "sharp")
  runs <- vapply(sharp$paths, function(path) last_run(path)$length, numeric(1))
  expect_equal(min(runs), head_run_default, tolerance = 1e-6)

  res <- ortho(scene)
  # each of the four targets takes one arrival, so every head is cut at the
  # full cap
  expect_equal(res$meta$resect_head, rep(cap_default, 4), tolerance = 1e-9)
  expect_true(all(grob_head_tilts(res) < 0.5))
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

# Rename every node of a scene, keeping the row order, so that only the
# names the caller chose differ.
rename_scene <- function(scene, names) {
  old <- scene$nodes$name
  scene$nodes$name <- names
  scene$edges$from <- names[match(scene$edges$from, old)]
  scene$edges$to <- names[match(scene$edges$to, old)]
  scene$layer <- NULL
  scene
}

test_that("routing does not depend on how the caller names the nodes", {
  # Two callers naming one scene differently must draw it identically: the
  # label grob and the drawn layer key their nodes independently, and any
  # tie broken by name lets the two disagree about the same picture. The
  # reversed alphabet inverts the sort order of every name; the
  # numeric-looking strings sort differently again under radix ("n10"
  # before "n6"). Only the edge label column of the meta may change.
  scenes <- list(
    fan = fan_scene(),
    "four-layer" = four_layer_scene(),
    large_epi = canonical_scene("large_epi")
  )
  for (nm in names(scenes)) {
    scene <- scenes[[nm]]
    n <- nrow(scene$nodes)
    renamings <- list(
      reversed = rev(letters)[seq_len(n)],
      numeric = paste0("n", seq(n + 5, by = -1, length.out = n))
    )
    for (mode in c("spline", "orthogonal", "straight")) {
      ref <- route_scene(scene, mode = mode)
      keep <- setdiff(names(ref$meta), "edge")
      for (rn in names(renamings)) {
        res <- route_scene(rename_scene(scene, renamings[[rn]]), mode = mode)
        label <- paste(nm, mode, rn)
        for (i in seq_along(ref$paths)) {
          expect_lt(
            polyline_hausdorff(res$paths[[i]], ref$paths[[i]]),
            1e-9,
            label = paste(label, ref$meta$edge[i])
          )
        }
        expect_identical(res$meta[keep], ref$meta[keep], label = label)
      }
    }
  }
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
  # Head zones and arrival separation cost this scene about twice the curve
  # and verification iterations, at about 1.6 times the work per iteration,
  # so what measured 4.7 to 5.0 ms without them measures 19 ms with them on
  # the development machine. The gate is 25 ms so that only a real
  # regression trips it.
  expect_lt(as.numeric(timing$median), 0.025)
})
