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
# mm of it holds the whole head, and a further head_margin = sep_e / 2 = 1.8
# keeps the base of that head clear of the nearest run in the gap. The last
# rung spreads its slots over the band from the source layer's R_soft to
# cap + head + head_margin = 11.8 mm before the target layer, so a gap holds
# the whole band from R_soft + cap + head + head_margin = 19 mm on, and holds
# the head run with its margin at the tightest spacing the ladder allows
# from 11.8 + sep_min (K - 1) mm on. A gap holds the head run and the
# source's soft band together from R_soft + cap + head = 17.2 mm on.
head_run_default <- cap_default + head_default
head_margin_default <- sep_e_default / 2
head_run_margin_default <- head_run_default + head_margin_default
head_run_gap_default <- r_soft + head_run_default

# The drawn head is an obstacle for a chord that clears every disc too: it is
# a pseudo-disc of radius head / 2 = 1 mm at the head's centre, cap + head / 2
# before the target, and a chord passing within head / 2 + head_margin = 2.8
# mm of that centre is nudged past it, which leaves the curve head_margin
# from the head's own axis.
head_r_default <- head_default / 2
head_clear_default <- head_r_default + head_margin_default

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

# The drawn shaft is 0.45 mm wide, so the ink of a shaft overlaps the ink of
# a head when the shaft comes within head_w / 2 + shaft / 2 = 0.875 mm of
# the head's axis, which the head-crossing census below calls touching.
shaft_default <- 0.45

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

# Distinct x values of the vertical runs of a path inside the gap
# (x_left, x_right), the layer lines included. The last rung of the ladder
# spreads its slots to the source layer's own centre line, so a slot can sit
# anywhere in the gap and no distance from a layer tells a slot from a port
# stub: every vertical inside the gap is counted, a port stub among them.
# Each caller asserts a count it has read off the paths of its own scene.
slot_xs <- function(path, gap, tol = 1e-6) {
  runs <- straight_runs(path, tol)
  inside <- runs$axis == "v" &
    runs$coord >= gap[1] - tol &
    runs$coord <= gap[2] + tol
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
    runs <- straight_runs(path)
    expect_gte(nrow(runs), 2L, label = label)
    # an E/W path turns twice, once out of its source and once into its
    # target, unless its slot sits on the source layer's own centre line:
    # then the first run has no length, the path leaves its node vertically
    # and there is one bend to make
    off_source_line <- runs$axis[[1]] == "v" && runs$axis[[nrow(runs)]] == "h"
    expect_gte(nrow(bends), if (off_source_line) 1L else 2L, label = label)
    expect_equal(res$meta$n_waypoints[i], nrow(bends), label = label)
    expect_true(all(is.na(bends$layer)), label = label)
    expect_orthogonal_outside_corners(path, bends, rc_used, label = label)
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
    # a vertical run within sep_e of a layer x is a port stub rather than a
    # slot: an S/N port sits on the layer x, or sep_e / 2 beside it when a
    # node's arrivals and departures share a side
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

test_that("arrival_deficit() turns away from one rival rather than between two", {
  # The sampled curve runs straight into its target with two other edges
  # arriving there: one 10 degrees off the chord, inside theta_min, and one
  # 30 degrees off it, already clear. Only one of the two is a squeeze, so
  # the rotation is 1.2 times that one's deficit away from it. The midpoint
  # rule, applied without asking whether both rivals are inside theta_min,
  # would turn 10 degrees and put the arrival on top of the near rival.
  pts <- data.frame(x = seq(0, 100, by = 0.5), y = 0)
  fr <- list(S = c(0, 0), E = c(100, 0))
  unit <- function(deg) c(cos(deg * pi / 180), sin(deg * pi / 180))

  turn <- arrival_deficit(
    pts,
    fr,
    cap_default,
    rbind(unit(10), unit(-30)),
    "E",
    theta_min_default
  )

  expect_equal(turn, -1.2 * (theta_min_default - 10), tolerance = 1e-6)
  expect_gt(abs(turn), 15)
})

test_that("arrival_deficit() equalises the two gaps of a true squeeze", {
  # Both rivals are inside theta_min and on opposite sides of the sampled
  # direction, so no rotation clears them both and the arrival aims for the
  # midpoint of the two gaps instead: the rivals sit 10 degrees one way and
  # 12 the other, so the turn is 1 degree towards the wider gap.
  pts <- data.frame(x = seq(0, 100, by = 0.5), y = 0)
  fr <- list(S = c(0, 0), E = c(100, 0))
  unit <- function(deg) c(cos(deg * pi / 180), sin(deg * pi / 180))

  turn <- arrival_deficit(
    pts,
    fr,
    cap_default,
    rbind(unit(10), unit(-12)),
    "E",
    theta_min_default
  )

  expect_equal(turn, -1, tolerance = 1e-6)
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

# Heads as soft obstacles -------------------------------------------------------

# A chord clear of every disc that runs under another edge's arrowhead.
# c -> d is vertical, crossing a -> b at x = 80; its head (8 to 10 mm
# before d) sits at y_d - 10 to y_d - 8, centre y_d - 9.
head_on_chord_scene <- function(y_d, x_c = 80) {
  list(
    nodes = mm_nodes(
      c("a", "b", "c", "d"),
      c(20, 140, x_c, 80),
      c(50, 50, y_d - 45, y_d)
    ),
    edges = mm_edges(c("a", "c"), c("b", "d")),
    bounds = c(0, 0, 160, 110)
  )
}

head_centre <- function(path, cap = cap_default, head = head_default) {
  (tip(path, cap) + point_before_end(path, cap + head)) / 2
}

test_that("a clear chord under another edge's arrowhead is nudged past it", {
  # d is 9.5 mm above the chord, clear of it; the head of c -> d has its
  # centre 0.5 mm above the chord. The chord is nudged 2.3 mm below, away
  # from d, and passes 2.8 mm from the head's centre, 1.8 from its axis.
  scene <- head_on_chord_scene(59.5)
  res <- route_scene(scene)
  path <- res$paths[[1]]
  ends <- edge_endpoints(scene, 1)
  crossed <- res$paths[[2]]

  expect_true(res$meta$routed[1])
  expect_equal(res$meta$mode[1], "soft")
  expect_true(res$meta$clearance_ok[1])
  wp <- res$waypoints[[1]]
  expect_identical(nrow(wp), 1L)
  expect_equal(c(wp$x, wp$y), c(80, 47.7))
  expect_gte(
    path_min_dist(path, head_centre(crossed)),
    head_clear_default - verify_tol
  )
  expect_gte(
    path_to_segment_dist(
      path,
      tip(crossed),
      point_before_end(crossed, cap_default + head_default)
    ),
    head_margin_default - verify_tol
  )
  off <- chord_offset(path, ends$from, ends$to)
  expect_lte(max(off), 1e-9)
  expect_gte(min(off), -(head_clear_default - 0.5) - verify_tol)
  expect_lt(res$meta$sagitta_ratio[1], 0.03)
  expect_lt(max(abs(turning_angles(path))), 12)
  expect_exact_endpoints(path, ends$from, ends$to)
  # the edge whose head it is stays straight
  expect_false(res$meta$routed[2])
  expect_straight_path(crossed, node_xy(scene, "c"), node_xy(scene, "d"))
})

test_that("the head nudge is continuous with straight and clears the head at every step", {
  # d slides from 8.5 mm above the chord (a grazed disc) to 14.5 (clear);
  # the head centre from 0.5 below the chord to 5.5 above. The route moves
  # by at most 0.2 mm per 0.1 mm of slide, keeps exactly one waypoint while
  # routed, keeps the head centre 2.8 mm off, and is straight once the
  # centre is more than 2.8 mm from the chord.
  prev <- NULL
  for (y in seq(58.5, 64.5, by = 0.1)) {
    scene <- head_on_chord_scene(y)
    res <- route_scene(scene)
    path <- res$paths[[1]]
    label <- sprintf("y_d = %.1f", y)
    if (y - 9 - 50 < head_clear_default + 1e-9) {
      expect_true(res$meta$routed[1], label = label)
      expect_equal(res$meta$mode[1], "soft", label = label)
      expect_identical(nrow(res$waypoints[[1]]), 1L, label = label)
      expect_gte(
        path_min_dist(path, c(80, y - 9)),
        head_clear_default - verify_tol,
        label = label
      )
    } else {
      expect_false(res$meta$routed[1], label = label)
    }
    if (!is.null(prev)) {
      expect_lt(polyline_hausdorff(path, prev), 0.2, label = label)
    }
    prev <- path
  }
  # the boundary itself
  expect_true(route_scene(head_on_chord_scene(61.5))$meta$routed[1])
  expect_equal(route_scene(head_on_chord_scene(61.5))$waypoints[[1]]$y, 49.7)
  expect_false(route_scene(head_on_chord_scene(62.0))$meta$routed[1])
})

test_that("a head hit merges with the grazed disc it arrives at", {
  # d is 8 mm above the chord, a soft hit whose own nudge is 1 mm; the head
  # of c -> d has its centre 1 mm below the chord. One waypoint, the
  # head's: 3.8 mm below the chord, away from d.
  scene <- head_on_chord_scene(58.0)
  res <- route_scene(scene)
  wp <- res$waypoints[[1]]
  expect_equal(res$meta$mode[1], "soft")
  expect_identical(nrow(wp), 1L)
  expect_equal(c(wp$x, wp$y), c(80, 46.2))
  expect_gte(
    path_min_dist(res$paths[[1]], c(80, 49)),
    head_clear_default - verify_tol
  )
  expect_gte(
    path_min_dist(res$paths[[1]], node_xy(scene, "d")),
    r_full - verify_tol
  )
  expect_true(res$meta$clearance_ok[1])
})

test_that("a short chord is nudged past a head too", {
  # a 32 mm chord under a head 1 mm above it: one waypoint 1.8 mm below
  scene <- list(
    nodes = mm_nodes(
      c("a", "b", "c", "d"),
      c(20, 52, 36, 36),
      c(50, 50, 15, 60)
    ),
    edges = mm_edges(c("a", "c"), c("b", "d")),
    bounds = c(0, 0, 160, 110)
  )
  res <- route_scene(scene)
  expect_equal(res$meta$mode[1], "soft")
  expect_equal(c(res$waypoints[[1]]$x, res$waypoints[[1]]$y), c(36, 48.2))
  expect_gte(
    path_min_dist(res$paths[[1]], c(36, 51)),
    head_clear_default - verify_tol
  )
})

test_that("heads into the chord's own target or source do not block it", {
  # c -> b arrives at b 12 degrees above the chord; its head centre is
  # 1.89 mm above the chord, inside the head clearance, but the two edges
  # share their target and the arrival rules own their tips: a -> b stays
  # straight.
  scene <- list(
    nodes = mm_nodes(c("a", "b", "c"), c(20, 140, 70), c(50, 50, 65)),
    edges = mm_edges(c("a", "c"), c("b", "b")),
    bounds = c(0, 0, 160, 110)
  )
  res <- route_scene(scene)
  expect_false(res$meta$routed[1])
  expect_lt(abs(head_centre(res$paths[[2]])[2] - 50), head_clear_default)

  # c -> a arrives at a from the upper right; its head centre is 2.59 mm
  # above the chord 8.6 mm from a, where a -> b leaves. Which port an edge
  # leaves its own source on is not the router's question: a -> b stays
  # straight.
  scene <- list(
    nodes = mm_nodes(c("a", "b", "c"), c(20, 140, 70), c(50, 50, 65)),
    edges = mm_edges(c("a", "c"), c("b", "a")),
    bounds = c(0, 0, 160, 110)
  )
  res <- route_scene(scene)
  expect_false(res$meta$routed[1])
  expect_lt(abs(head_centre(res$paths[[2]])[2] - 50), head_clear_default)
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

test_that("canonical DAGs: hit chords are routed, endpoints exact", {
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
        if (d < r_soft && chord_length(ends$from, ends$to) >= 2 * r_full) {
          expect_true(res$meta$routed[i], label = label)
        }
      }
    }
  }
})

test_that("canonical DAGs: a chord clear of every disc is straight or a head nudge", {
  # Clearing every disc no longer settles the drawing: a chord that also
  # clears every arrowhead is straight, and one that does not is nudged
  # past the head by at most head_r + head_margin. The four chords named
  # below are the ones the canonical layouts run under a head, all of them
  # at the small device size where the panel packs the layers closer.
  nudged <- character()
  for (panel in canonical_panels) {
    for (nm in names(canonical_dag_specs)) {
      scene <- canonical_scene(nm, panel)
      res <- route_scene(scene)
      for (i in seq_len(nrow(scene$edges))) {
        if (chord_min_clearance(scene, i) < r_full) {
          next
        }
        label <- canonical_label(scene, i, panel)
        ends <- edge_endpoints(scene, i)
        path <- res$paths[[i]]
        expect_exact_endpoints(path, ends$from, ends$to)
        if (!res$meta$routed[i]) {
          expect_identical(nrow(path), 2L, label = label)
          next
        }
        expect_equal(res$meta$mode[i], "soft", label = label)
        expect_lte(
          max(abs(chord_offset(path, ends$from, ends$to))),
          head_clear_default + verify_tol,
          label = label
        )
        nudged <- c(
          nudged,
          paste(nm, edge_labels(scene$edges)[i], paste(panel, collapse = "x"))
        )
      }
    }
  }
  expect_setequal(
    nudged,
    c(
      "wide_dag x3->y 100x70",
      "large_epi bmi->health 100x70",
      "triple_confound x->y 100x70",
      "multi_mediator x->m3 100x70"
    )
  )
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

# Two spans across one middle layer, arranged so that the free interval
# between the middle layer's discs is only just wide enough for a run and
# the first channel placed sits beside it. m1 (60, 20) and m2 (60, 42) leave
# the interval [29, 33] between them at the full R = 9; s1 -> t1 is placed
# first and takes its own source line 30, which is inside that interval, so
# s2 -> t2's interior candidate at 29 is pushed one separation past it to
# 33.6, where it is 8.4 mm from m2 and cuts the disc. The refusal sends
# s2 -> t2 to the run below the layer at 20 - 9 = 11.
pushed_into_disc_scene <- function() {
  list(
    nodes = mm_nodes(
      c("s1", "s2", "m1", "m2", "t1", "t2"),
      c(20, 20, 60, 60, 100, 100),
      c(30, 12, 20, 42, 34, 36)
    ),
    edges = mm_edges(c("s1", "s2"), c("t1", "t2")),
    bounds = c(0, 0, 120, 60)
  )
}

test_that("orthogonal channels: a candidate pushed onto a disc is refused", {
  # The disc check is applied to the pushed y, not to the y the candidate
  # was priced at: skipping it once a candidate has been stacked would put
  # s2 -> t2's run at 33.6, inside m2's disc.
  scene <- pushed_into_disc_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  a <- edge_index(scene, "s1->t1")
  expect_equal(res$meta$mode[a], "orthogonal")
  expect_equal(res$meta$n_waypoints[a], 2)
  expect_equal(channel_run(res$paths[[a]], 60)$coord, 30, tolerance = 1e-6)

  b <- edge_index(scene, "s2->t2")
  expect_equal(res$meta$mode[b], "orthogonal")
  expect_equal(res$meta$side[b], -1)
  expect_equal(res$meta$n_waypoints[b], 4)
  run <- channel_run(res$paths[[b]], 60)
  expect_equal(run$coord, 11, tolerance = 1e-6)
  for (nm in c("m1", "m2")) {
    expect_gte(abs(run$coord - node_xy(scene, nm)[[2]]), r_full - 1e-6)
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
# 2.1 mm corner radius, so a->b is level and is drawn as the run on b's
# line; c arrives from below and takes the port row sep_e beneath b's
# centre. The variant tilts b to 2.7 mm above a, past the corner radius, so
# a->b bends.
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
  # a jog of 1.5 mm cannot show two corners of radius 2.1, so the edge is
  # drawn as the run on b's line: a leaves through the port 1.5 mm above its
  # own centre and the run arrives at b's centre, with nothing oblique
  # drawn. The tail's resect is the face resect of that offset, so its end
  # still sits cap - r from a's face along the run it is drawn on.
  path_ab <- res$paths[[ab]]
  expect_equal(res$meta$mode[ab], "straight")
  expect_false(res$meta$routed[ab])
  expect_equal(res$meta$n_waypoints[ab], 0)
  expect_identical(nrow(path_ab), 2L)
  expect_equal(
    c(path_ab$x[[1]], path_ab$y[[1]]),
    c(ends$from[[1]], ends$to[[2]]),
    tolerance = 1e-9
  )
  expect_equal(
    c(path_ab$x[[2]], path_ab$y[[2]]),
    ends$to,
    tolerance = 1e-9
  )
  expect_equal(
    res$meta$resect_fins[[ab]],
    port_resect_at(1.5),
    tolerance = 1e-9
  )
  expect_equal(res$meta$resect_head[[ab]], cap_default)

  # the level edge owns b's centre row, so c->b takes the port sep_e below
  # it and the two runs are drawn apart
  expect_equal(res$meta$mode[cb], "orthogonal")
  expect_equal(arrival_row(scene, res, "c->b"), 52.9, tolerance = 1e-6)
  expect_equal(shared_run_length(res$paths[[ab]], res$paths[[cb]]), 0)

  # the arrowhead zone of c->b, the 8 mm of path after its resected cap,
  # keeps half a separation from the run it used to be drawn under
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

# One level pair on its own: s and t a single gap apart, t sitting `d`
# millimetres above s. Every value of d up to the corner radius is level
# and every value past it bends, so the pair is the whole level rule in one
# scene.
level_pair_scene <- function(d) {
  list(
    nodes = mm_nodes(c("s", "t"), c(20, 80), c(55, 55 + d)),
    edges = mm_edges("s", "t"),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("orthogonal: a level chord is the run on its target's line", {
  # An orthogonal drawing shows no oblique run, so a level chord is drawn as
  # the horizontal run on its target's line: the tail leaves s through the
  # port `d` millimetres off its centre, the head arrives at t's centre, and
  # the tail's resect is the face resect of that offset. The shape is the
  # same whether the scene rounds its corners or not, since the run has no
  # corner to round.
  for (d in c(1, rc_default)) {
    scene <- level_pair_scene(d)
    res <- ortho(scene)
    expect_orthogonal_scene(scene, res, stub_always = TRUE)
    path <- res$paths[[1]]
    label <- paste0("d = ", d, ":")

    expect_equal(res$meta$mode[[1]], "straight", label = paste(label, "mode"))
    expect_false(res$meta$routed[[1]], label = paste(label, "routed"))
    expect_equal(
      res$meta$n_waypoints[[1]],
      0,
      label = paste(label, "waypoints")
    )
    expect_identical(nrow(path), 2L)
    expect_equal(
      c(path$x[[1]], path$y[[1]]),
      c(20, 55 + d),
      tolerance = 1e-9,
      label = paste(label, "tail port")
    )
    expect_equal(
      c(path$x[[2]], path$y[[2]]),
      c(80, 55 + d),
      tolerance = 1e-9,
      label = paste(label, "head point")
    )
    expect_equal(
      res$meta$resect_fins[[1]],
      port_resect_at(d),
      tolerance = 1e-9,
      label = paste(label, "tail resect")
    )
    expect_equal(
      res$meta$resect_head[[1]],
      cap_default,
      label = paste(label, "head resect")
    )
    expect_identical(ortho(scene, corners = "sharp")$paths[[1]], path)
  }
})

test_that("orthogonal: a chord level with its target is left as it is", {
  # The control. With no offset at all the run and the chord coincide, so
  # the path is the incoming chord between the centres and both resects are
  # the plain cap.
  scene <- level_pair_scene(0)
  res <- ortho(scene)
  ends <- edge_endpoints(scene, 1)

  expect_equal(res$meta$mode[[1]], "straight")
  expect_straight_path(res$paths[[1]], ends$from, ends$to)
  expect_equal(res$meta$resect_fins[[1]], cap_default)
  expect_equal(res$meta$resect_head[[1]], cap_default)
})

test_that("orthogonal: a chord past the corner radius still bends at its slot", {
  # The other control. At 2.15 mm the pair is past the level threshold,
  # which this round leaves where it is, so the edge takes its slot and
  # draws the two corners, with both resects at the plain cap.
  scene <- level_pair_scene(rc_default + 0.05)
  res <- ortho(scene, corners = "sharp")
  ends <- edge_endpoints(scene, 1)

  expect_equal(res$meta$mode[[1]], "orthogonal")
  expect_true(res$meta$routed[[1]])
  expect_equal(res$meta$n_waypoints[[1]], 2)
  expect_equal(
    c(res$paths[[1]]$x[[1]], res$paths[[1]]$y[[1]]),
    ends$from,
    tolerance = 1e-9
  )
  runs <- straight_runs(res$paths[[1]])
  expect_equal(runs$axis, c("h", "v", "h"))
  expect_equal(runs$coord, c(55, 50, 57.15), tolerance = 1e-6)
  expect_equal(res$meta$resect_fins[[1]], cap_default)
  expect_equal(res$meta$resect_head[[1]], cap_default)
})

test_that("orthogonal: the drawing is continuous across the level threshold", {
  # Crossing the threshold from below moves the tail port from `rc` off the
  # centre back onto it and puts an S of the same height at the slot. Both
  # drawings are axis-aligned and, as the round's design states the bound,
  # nowhere more than the corner radius apart; the head end moves only by
  # the change in d.
  below <- ortho(level_pair_scene(rc_default))$paths[[1]]
  above <- ortho(level_pair_scene(rc_default + 0.05))$paths[[1]]

  expect_lte(polyline_hausdorff(below, above), rc_default + 1e-6)
  n <- nrow(above)
  expect_lte(
    sqrt(
      (below$x[[2]] - above$x[[n]])^2 + (below$y[[2]] - above$y[[n]])^2
    ),
    0.05 + 1e-9
  )
})

# A source with one level departure and one that bends, both leaving its E
# face: s->t is level, s->u drops a gap to a node well below it.
sibling_level_scene <- function(d = 1.5) {
  list(
    nodes = mm_nodes(c("s", "t", "u"), c(20, 80, 80), c(55, 55 + d, 20)),
    edges = mm_edges(c("s", "s"), c("t", "u")),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("orthogonal: a level departure leaves the trunk where it is", {
  # The level run needs no slot, so it leaves the hyperedge its sibling
  # takes and runs `d` above the trunk from s's face to the slot. The
  # sibling keeps the drawing it had before the level rule: out along s's
  # own line to the slot at the gap's centre, down, and in to u.
  scene <- sibling_level_scene()
  res <- ortho(scene, corners = "sharp")
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  st <- edge_index(scene, "s->t")
  su <- edge_index(scene, "s->u")

  level <- straight_runs(res$paths[[st]])
  expect_equal(level$axis, "h")
  expect_equal(level$coord, 56.5, tolerance = 1e-9)
  expect_equal(c(level$lo, level$hi), c(20, 80), tolerance = 1e-9)
  expect_equal(
    res$meta$resect_fins[[st]],
    port_resect_at(1.5),
    tolerance = 1e-9
  )

  trunk <- straight_runs(res$paths[[su]])
  expect_equal(trunk$axis, c("h", "v", "h"))
  expect_equal(trunk$coord, c(55, 50, 20), tolerance = 1e-9)
  expect_equal(res$meta$resect_fins[[su]], cap_default)
  expect_equal(res$meta$resect_head[[su]], cap_default)

  # the two departures are drawn apart the whole way: the level run sits
  # exactly d above the trunk and shares none of it
  expect_equal(shared_run_length(res$paths[[st]], res$paths[[su]]), 0)
  expect_equal(level$coord - trunk$coord[[1]], 1.5, tolerance = 1e-9)
})

# A level pair two gaps apart with one node in the crossed layer. At 65.5 the
# node is 9.5 mm from the chord between the centres and 8.5 mm from the run
# on t's line; at 46.5 it is 9.5 from the chord and 10.5 from the run.
spanning_level_scene <- function(my) {
  list(
    nodes = mm_nodes(c("s", "m", "t"), c(20, 60, 100), c(55, my, 57)),
    edges = mm_edges("s", "t"),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("orthogonal: a spanning level chord is cleared on its run", {
  # The run, not the chord, is what gets drawn, so the run is what the
  # crossed discs have to clear. A disc that the chord clears by more than R
  # but the run does not blocks the edge, which then bends like any other
  # spanning edge.
  scene <- spanning_level_scene(65.5)
  expect_gt(chord_min_clearance(scene, 1), r_full)
  res <- ortho(scene, corners = "sharp")

  expect_equal(res$meta$mode[[1]], "orthogonal")
  expect_true(res$meta$routed[[1]])
  runs <- straight_runs(res$paths[[1]])
  expect_equal(runs$axis, c("h", "v", "h"))
  expect_equal(runs$coord, c(55, 80, 57), tolerance = 1e-9)

  # mirrored, the run clears the disc by 10.5 mm and the edge is the run on
  # t's line, two points and nothing oblique
  scene <- spanning_level_scene(46.5)
  res <- ortho(scene, corners = "sharp")
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  path <- res$paths[[1]]

  expect_equal(res$meta$mode[[1]], "straight")
  expect_false(res$meta$routed[[1]])
  expect_identical(nrow(path), 2L)
  expect_equal(c(path$x[[1]], path$y[[1]]), c(20, 57), tolerance = 1e-9)
  expect_equal(c(path$x[[2]], path$y[[2]]), c(100, 57), tolerance = 1e-9)
  expect_equal(res$meta$resect_fins[[1]], port_resect_at(2), tolerance = 1e-9)
  expect_equal(res$meta$resect_head[[1]], cap_default)
})

# The same level pair drawn the other way round: b is the real source and
# the head arrives at a, so the run lies on a's line and the offset port is
# b's, on its W face.
reversed_level_scene <- function(d = 1.5) {
  list(
    nodes = mm_nodes(c("a", "b"), c(20, 80), c(55, 55 + d)),
    edges = mm_edges("b", "a"),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("orthogonal: a reversed level chord runs on its own target's line", {
  # The rule mirrored: the tail port is on the right node at the target's y
  # and the head keeps a's centre, so the path starts at (80, 55) and ends
  # at (20, 55) with the tail's resect the face resect of b's 1.5 mm offset.
  scene <- reversed_level_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)
  path <- res$paths[[1]]

  expect_equal(res$meta$mode[[1]], "straight")
  expect_false(res$meta$routed[[1]])
  expect_equal(res$meta$n_waypoints[[1]], 0)
  expect_identical(nrow(path), 2L)
  expect_equal(c(path$x[[1]], path$y[[1]]), c(80, 55), tolerance = 1e-9)
  expect_equal(c(path$x[[2]], path$y[[2]]), c(20, 55), tolerance = 1e-9)
  expect_equal(
    res$meta$resect_fins[[1]],
    port_resect_at(1.5),
    tolerance = 1e-9
  )
  expect_equal(res$meta$resect_head[[1]], cap_default)
})

test_that("spline and straight mode draw a level chord between the centres", {
  # The control on the other two modes: the level rule belongs to the
  # orthogonal router, and nothing else in the scene changes shape because
  # of it.
  scene <- level_pair_scene(1.5)
  ends <- edge_endpoints(scene, 1)
  for (mode in c("spline", "straight")) {
    res <- route_scene(scene, mode = mode)
    expect_false(res$meta$routed[[1]], label = mode)
    expect_straight_path(res$paths[[1]], ends$from, ends$to)
  }
})

test_that("canonical multi_mediator: the near-level x->y runs on y's line", {
  # The fixture's own instance of the rule. x sits 1.893 mm above y at the
  # large panel, inside the corner radius, and the run clears every crossed
  # disc, so x->y is the horizontal run on y's line.
  scene <- canonical_scene("multi_mediator", c(249.78, 148.18))
  i <- edge_index(scene, "x->y")
  ends <- edge_endpoints(scene, i)
  off <- ends$from[[2]] - ends$to[[2]]
  expect_lte(abs(off), rc_default)
  expect_gt(chord_min_clearance(scene, i), r_full)

  res <- ortho(scene)
  expect_false(res$meta$routed[i])
  path <- res$paths[[i]]
  expect_equal(res$meta$mode[i], "straight")
  expect_identical(nrow(path), 2L)
  expect_equal(path$y, rep(ends$to[[2]], 2), tolerance = 1e-9)
  expect_equal(path$x, c(ends$from[[1]], ends$to[[1]]), tolerance = 1e-9)
  expect_equal(
    res$meta$resect_fins[[i]],
    port_resect_at(abs(off)),
    tolerance = 1e-9
  )
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

# The x of the vertical run each path of a single-gap scene crosses the gap
# on, in edge order, one value per path where slot_xs() reports the distinct
# x values of a whole scene.
ladder_slots <- function(res) {
  vapply(
    res$paths,
    function(path) {
      runs <- straight_runs(path)
      unique(runs$coord[runs$axis == "v"])
    },
    numeric(1)
  )
}

# Every vertical run of every path of a scene with a single gap, flat rather
# than one value per path, for a scene whose paths do not each cross the gap
# exactly once.
crossing_slots <- function(res) {
  unlist(lapply(res$paths, function(path) {
    runs <- straight_runs(path)
    runs$coord[runs$axis == "v"]
  }))
}

# The distinct slots a scene draws inside the gap between the layer lines
# `lo` and `hi`, for a scene of more than one gap.
slots_in_gap <- function(res, lo, hi) {
  xs <- crossing_slots(res)
  sort(unique(round(xs[xs > lo + 1e-6 & xs < hi - 1e-6], 9)))
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
    ladder_slots(res),
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
    sort(ladder_slots(res)),
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

  slots <- sort(ladder_slots(res))
  expect_equal(diff(slots), rep(sep_min_default, 3), tolerance = 1e-6)
  expect_gte(min(slots), 26.5 + 11.25 - 1e-6)
  expect_lte(max(slots), 53.5 - 11.25 + 1e-6)
})

test_that("orthogonal ladder: rung 3 keeps its stub floor", {
  # The head margin belongs to the last rung alone. Rung 3 buys its band by
  # shrinking the corner radius, and its stub with it, to cap + head + rc:
  # 11.083 mm on the treatment scene and 11.0 on large_epi, both of which
  # leave the whole head and less than the 1.8 mm behind it that the last
  # rung leaves. Carrying the margin here would push these two gaps to the
  # last rung and redraw both scenes, for a crossing no census observes
  # below it, so the stub floor stands where it is.
  scene <- canonical_scene("treatment", c(100, 70))
  res <- ortho(scene)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, c(2, 2, 3))
  expect_equal(gaps$stub[[3]], 11.083333, tolerance = 1e-6)
  expect_equal(res$ortho$rc, 1.083333, tolerance = 1e-6)
  expect_lt(gaps$stub[[3]], head_run_margin_default)
  expect_gt(gaps$stub[[3]], head_run_default)
  slots <- slots_in_gap(ortho(scene, corners = "sharp"), 190 / 3, 90)
  expect_equal(
    slots,
    c(74.416667, 75.916667, 77.416667, 78.916667),
    tolerance = 1e-6
  )
  expect_equal(min(slots) - 190 / 3, gaps$stub[[3]], tolerance = 1e-6)
  expect_equal(90 - max(slots), gaps$stub[[3]], tolerance = 1e-6)

  scene <- canonical_scene("large_epi", c(160, 110))
  res <- ortho(scene)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, c(1, 1, 1, 1, 3))
  expect_equal(gaps$stub[[5]], 11, tolerance = 1e-6)
  expect_equal(res$ortho$rc, 1, tolerance = 1e-6)
  slots <- slots_in_gap(ortho(scene, corners = "sharp"), 122, 150)
  expect_equal(slots, c(133, 134.5, 136, 137.5, 139), tolerance = 1e-6)
  expect_equal(min(slots) - 122, 11, tolerance = 1e-6)
  expect_equal(150 - max(slots), 11, tolerance = 1e-6)
})

test_that("orthogonal ladder: a 20 mm gap keeps the head run and its margin clear", {
  # rung 4: no stub fits, so the four slots are spread over the band from
  # the source's soft margin to the head run and its margin, 11.8 mm before
  # the target's layer. The band is 1 mm wide against the 4.5 mm four slots
  # need at sep_min, so the spread is anchored at the head run and overflows
  # toward the source. The gap's edges lose their clearance either way; what
  # the rule buys is a straight run for every head with nothing drawn across
  # the base of it.
  scene <- narrow_band_scene(gap = 20)
  res <- ortho(scene)
  expect_true(all(res$meta$routed))
  expect_false(any(res$meta$clearance_ok))
  expect_equal(res$ortho$rc, rc_min_default)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 4)
  expect_equal(gaps$width, 20)
  expect_equal(gaps$spacing, sep_min_default)

  slots <- sort(ladder_slots(res))
  expect_length(unique(round(slots, 9)), 4)
  expect_equal(slots, 38.2 - sep_min_default * (3:0), tolerance = 1e-6)
  expect_equal(max(slots), 50 - head_run_margin_default, tolerance = 1e-6)
  expect_true(all(diff(slots) >= sep_min_default - 1e-9))
  # the source's band is the one that gives way: the slot nearest the source
  # is inside it, and still inside the gap
  expect_lt(min(slots), 30 + r_soft)
  expect_gte(min(slots), 30)
})

test_that("orthogonal ladder: a fixed sep_min falls back to the centred slots", {
  # edge_sep_min = edge_sep makes rung 2 a no-op and rung 3 unreachable, so
  # a 30 mm gap drops to rung 4, where four slots a full separation apart
  # span 10.8 mm of the band's 11, and are centred in it: 0.1 mm short of
  # the head run at the target end and 0.1 mm clear of the soft margin at
  # the source's
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

  slots <- ladder_slots(res)
  expect_equal(sort(slots), c(32.3, 35.9, 39.5, 43.1), tolerance = 1e-6)
  expect_equal(
    max(slots),
    55 - head_run_margin_default - 0.1,
    tolerance = 1e-6
  )
  expect_equal(min(slots), 25 + r_soft + 0.1, tolerance = 1e-6)
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

# The x of every vertical run of a routed scene that lies in the single
# gap, its two layer lines included: the last rung's spread can leave a
# slot a hair inside the source layer's own centre line.
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
  # it falls to rung 4. Nine slots at sep_min span 12 mm against the 9.93 mm
  # band from the soft margin to the head run and its margin, so the spread
  # is anchored at the head run and reaches back to 5.13 mm from the source's
  # layer, inside its soft band and clear of its centre line.
  scene <- nine_arrival_scene(28.93)
  res <- ortho(scene)
  expect_true(all(res$meta$routed))
  expect_false(any(res$meta$clearance_ok))
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(res$ortho$rc, rc_min_default)

  slots <- gap_slots(scene, res)
  expect_length(unique(round(slots, 9)), 9)
  expect_equal(diff(slots), rep(sep_min_default, 8), tolerance = 1e-6)
  expect_equal(
    max(slots),
    48.93 - head_run_margin_default,
    tolerance = 1e-6
  )
  # the source side is inside its own soft band, and inside the gap
  expect_lt(min(slots), 20 + r_soft)
  expect_gte(min(slots), 20)
})

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

test_that("orthogonal ladder: two rung-4 slots span the band from R_soft to the head margin", {
  # 20.8 mm is R_soft + cap + head + head_margin + sep_e / 2, the width at
  # which two slots fit the band exactly at half a separation: the far one
  # keeps the head run and its margin from the target's layer and the near
  # one R_soft from the source's
  scene <- two_arrival_scene(20.8)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(res$ortho$gaps$ranks, 2)
  expect_equal(res$ortho$gaps$width, 20.8)
  expect_equal(res$ortho$gaps$spacing, sep_e_default / 2, tolerance = 1e-6)

  expect_equal(
    sort(ladder_slots(res)),
    c(30 + r_soft, 50.8 - head_run_margin_default),
    tolerance = 1e-6
  )
})

test_that("orthogonal ladder: the last rung spreads over the band it is given", {
  # The band runs from R_soft at the source layer to cap + head + head_margin
  # before the target layer, and the slots are placed in it four ways as it
  # narrows. Measured from the source layer in each case.
  band_slots <- function(scene, left) {
    sort(ladder_slots(ortho(scene, corners = "sharp"))) - left
  }

  # centred in the band: at 23 mm it is 4 mm wide and two slots a full
  # separation apart span 3.6 of it, so each sits 0.2 mm inside an end
  scene <- two_arrival_scene(23)
  expect_equal(ortho(scene)$ortho$gaps$spacing, sep_e_default)
  expect_equal(band_slots(scene, 30), c(7.4, 11), tolerance = 1e-6)

  # filling it exactly: four ranks over the 4.6 mm band of a 23.6 mm gap
  # take 4.6 / 3 apiece, where the centred and anchored placements coincide
  scene <- k_arrival_scene(23.6, 4)
  expect_equal(ortho(scene)$ortho$gaps$ranks, 4)
  expect_equal(ortho(scene)$ortho$gaps$spacing, 4.6 / 3, tolerance = 1e-6)
  expect_equal(
    band_slots(scene, 20),
    r_soft + (4.6 / 3) * (0:3),
    tolerance = 1e-6
  )

  # anchored at the head run and overflowing toward the source: seven ranks
  # at sep_min need 9 mm against the 8.1 mm band of a 27.1 mm gap
  scene <- k_arrival_scene(27.1, 7)
  expect_equal(ortho(scene)$ortho$gaps$spacing, sep_min_default)
  expect_equal(
    band_slots(scene, 20),
    6.3 + sep_min_default * (0:6),
    tolerance = 1e-6
  )
  expect_equal(
    27.1 - max(band_slots(scene, 20)),
    head_run_margin_default,
    tolerance = 1e-6
  )

  # stopped by the source layer's centre line: the same seven ranks in the
  # 20.82 mm gap the gallery's largest scene draws, where the overflow has
  # 0.02 mm to spare and the target still keeps its whole run
  scene <- k_arrival_scene(20.82, 7)
  expect_equal(
    band_slots(scene, 20),
    0.02 + sep_min_default * (0:6),
    tolerance = 1e-6
  )
  expect_equal(
    20.82 - max(band_slots(scene, 20)),
    head_run_margin_default,
    tolerance = 1e-6
  )

  # the gap census reads all seven, the leftmost 0.02 mm inside the source
  # layer's own centre line rather than a separation clear of it
  slots <- gap_slots(scene, ortho(scene, corners = "sharp"))
  expect_length(slots, 7)
  expect_equal(min(slots), 20.02, tolerance = 1e-6)
  expect_equal(diff(slots), rep(sep_min_default, 6), tolerance = 1e-6)
})

test_that("orthogonal ladder: a rung-4 gap crossed leftwards floors its left slot", {
  # the mirror of the same width: the target's layer is the left one, so the
  # floor is measured from it and the source's band is on the right
  scene <- mirrored_two_arrival_scene(20.8)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  expect_equal(res$ortho$gaps$ranks, 2)

  expect_equal(
    sort(ladder_slots(res)),
    c(30 + head_run_margin_default, 50.8 - r_soft),
    tolerance = 1e-6
  )

  # and in the order the ranks give them: rank 1 is the crossing-free slot
  # nearest the source layer, which is the right one here, so b1 -> a1 takes
  # 43.6 and b2 -> a2 41.8, the mirror image of the rightward 39.0 and 37.2
  expect_equal(
    ladder_slots(res),
    c(50.8 - r_soft, 30 + head_run_margin_default),
    tolerance = 1e-6
  )
})

# Three arrivals crossing one gap leftwards: the sources sit on the right
# layer and the targets on the left, their y-intervals overlapping in a
# chain, so the three segments take three ranks measured from the right.
mirrored_three_arrival_scene <- function(gap) {
  list(
    nodes = mm_nodes(
      c("a1", "a2", "a3", "b1", "b2", "b3"),
      c(rep(30, 3), rep(30 + gap, 3)),
      c(20, 35, 50, 50, 65, 80)
    ),
    edges = mm_edges(c("b1", "b2", "b3"), c("a1", "a2", "a3")),
    bounds = c(0, 0, 60 + gap, 105)
  )
}

# Crossings between the runs of different paths of a scene: a horizontal run
# of one path meeting a vertical run of another at a point interior to both.
# The runs are compared directly rather than through count_paths_crossing(),
# which reads such a meeting as a touch whenever the sampling puts a vertex
# on it, as an axis-aligned crossing usually does.
count_run_crossings <- function(res, tol = 1e-6) {
  runs <- lapply(res$paths, straight_runs)
  pairs <- expand.grid(i = seq_along(runs), j = seq_along(runs))
  pairs <- pairs[pairs$i != pairs$j, ]
  sum(vapply(
    seq_len(nrow(pairs)),
    function(k) {
      h <- runs[[pairs$i[k]]]
      v <- runs[[pairs$j[k]]]
      h <- h[h$axis == "h", ]
      v <- v[v$axis == "v", ]
      grid <- expand.grid(a = seq_len(nrow(h)), b = seq_len(nrow(v)))
      sum(
        v$coord[grid$b] > h$lo[grid$a] + tol &
          v$coord[grid$b] < h$hi[grid$a] - tol &
          h$coord[grid$a] > v$lo[grid$b] + tol &
          h$coord[grid$a] < v$hi[grid$b] - tol
      )
    },
    integer(1)
  ))
}

test_that("orthogonal ladder: leftward rung-4 slots keep the crossing-free order", {
  # the ranks are geometric, so measuring them from the source layer is what
  # keeps the runs apart: three leftward arrivals draw no crossing at any
  # width the last rung covers, anchored at 16 mm, centred at 23 and 24 mm,
  # against the rung-0 control at 40 mm
  for (gap in c(16, 23, 24)) {
    res <- ortho(mirrored_three_arrival_scene(gap), corners = "sharp")
    expect_equal(res$ortho$gaps$rung, 4)
    expect_equal(res$ortho$gaps$ranks, 3)
    expect_identical(count_run_crossings(res), 0L)
    # the slots descend with the ranks, away from the source layer
    expect_true(all(diff(ladder_slots(res)) < 0))
  }

  res <- ortho(mirrored_three_arrival_scene(40), corners = "sharp")
  expect_equal(res$ortho$gaps$rung, 0)
  expect_identical(count_run_crossings(res), 0L)
})

test_that("orthogonal ladder: a gap too narrow for the band overflows to the source's line", {
  # 16 mm cannot hold four slots at sep_min and the head run with its margin
  # at once: the spread is anchored 11.8 mm from the target, overflows the
  # source's centre line by 0.3 mm, and is shifted back onto that line, so
  # the target keeps 11.5 mm of run instead of 11.8. The head still has its
  # whole run; what the gap cannot afford is the margin behind it.
  scene <- narrow_band_scene(gap = 16)
  res <- ortho(scene)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 4)
  expect_lt(gaps$width, head_run_gap_default)
  expect_equal(gaps$spacing, sep_min_default)

  slots <- sort(ladder_slots(res))
  expect_equal(slots, c(32, 33.5, 35, 36.5), tolerance = 1e-6)
  # the source layer's centre line, the hard stop the overflow reaches
  expect_equal(min(slots), 32, tolerance = 1e-9)
  expect_equal(48 - max(slots), 11.5, tolerance = 1e-6)
  expect_gt(48 - max(slots), head_run_default)
  expect_lt(48 - max(slots), head_run_margin_default)
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

test_that("orthogonal ladder: the source's line caps the spread short of the head run", {
  # nine slots at sep_min need 12 mm and the head run with its margin
  # another 11.8, against an 18 mm gap. The spread is anchored at the head
  # run, overflows the source's centre line, and is shifted back onto it, so
  # the slot nearest the target stops 6 mm out, short of the head's own run,
  # and the gap is not floored at all.
  scene <- nine_arrival_scene(18)
  res <- ortho(scene)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 4)
  expect_equal(gaps$ranks, 9)
  expect_equal(gaps$spacing, sep_min_default)

  slots <- sort(ladder_slots(res))
  expect_equal(slots, 20 + sep_min_default * (0:8), tolerance = 1e-6)
  expect_equal(min(slots), 20, tolerance = 1e-9)
  expect_equal(38 - max(slots), 6, tolerance = 1e-6)
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
    sort(ladder_slots(res)),
    40 + c(-1, 1) * sep_e_default / 2,
    tolerance = 1e-6
  )

  # the same geometry crossed rightwards by both edges does slide toward
  # the source, so the centred slots above are the mixed gap's own answer
  rightwards <- scene
  rightwards$edges <- mm_edges(c("a1", "a2"), c("b1", "b2"))
  expect_equal(
    sort(ladder_slots(ortho(rightwards))),
    c(36.7, 38.2),
    tolerance = 1e-6
  )
  expect_equal(
    50 - max(ladder_slots(ortho(rightwards))),
    head_run_margin_default,
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
  expect_equal(unique(ladder_slots(res)), 40, tolerance = 1e-6)
})

test_that("orthogonal ladder: rung 4 keeps nine slots at sep_min across the soft bands", {
  # 2 R_soft = 14.4 is the width at which the two layers' soft bands meet.
  # A spread of band / (K - 1) alone gives 0.00125 mm just above that width
  # against sep_e just below it: a 14.4 mm jump in the drawing, and eight
  # verticals a reader takes for one. The spacing floor holds the nine slots
  # at sep_min = 1.5 on both sides of the boundary; at either width they
  # overflow the band and sit on the source's centre line, so the two
  # arrangements are the same one.
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
  slots_wide <- sort(ladder_slots(res_wide))
  slots_narrow <- sort(ladder_slots(res_narrow))
  expect_length(slots_wide, 9)
  expect_length(slots_narrow, 9)
  expect_true(all(diff(slots_wide) >= sep_min_default - 1e-9))
  expect_true(all(diff(slots_narrow) >= sep_min_default - 1e-9))
  # the 0.02 mm change in the gap moves no slot by a separation
  expect_lte(max(abs(slots_wide - slots_narrow)), sep_e_default)
})

# One scene's ladder swept over a list of gap widths, on the sharp polyline
# so that a jog the corners round away entirely still reports the slot it
# turns at. The reduction is the largest step any slot takes between
# neighbouring widths within the last rung, the largest step of the slot
# nearest the target overall and within the last rung, the largest step of
# that slot at a change of rung, and the rungs the sweep visited.
ladder_sweep <- function(maker, gaps) {
  previous <- NULL
  previous_rung <- NA_integer_
  out <- list(rung4 = 0, target = 0, target4 = 0, handover = 0)
  rungs <- integer(0)
  for (gap in gaps) {
    res <- ortho(maker(gap), corners = "sharp")
    slots <- sort(ladder_slots(res))
    rung <- res$ortho$gaps$rung
    if (!is.null(previous)) {
      step <- max(abs(slots - previous))
      target <- abs(max(slots) - max(previous))
      out$target <- max(out$target, target)
      if (rung == 4L && previous_rung == 4L) {
        out$rung4 <- max(out$rung4, step)
        out$target4 <- max(out$target4, target)
      } else if (rung != previous_rung) {
        out$handover <- max(out$handover, target)
      }
    }
    previous <- slots
    previous_rung <- rung
    rungs <- c(rungs, rung)
  }
  out$rungs <- unique(rungs)
  out
}

test_that("orthogonal ladder: the target-side slot moves continuously as the gap widens", {
  # The gap swept from rung 4 through rungs 3 and 2. The slot nearest the
  # target is the one every head in the gap is drawn on, so it is the slot a
  # reader follows: it may not move by more than a separation between
  # neighbouring widths, and within the last rung no slot may move faster
  # than the gap widens at all, which the 0.01 mm sweeps below measure at
  # exactly 0.01 mm a step. The handover from rung 3 is the one step the
  # ladder takes: the target-side slot moves from the rung-3 stub out to the
  # head run with its margin, 1.25 mm at two ranks and 1.0 at nine, read to
  # the sweep's own 0.01 mm resolution.
  coarse <- ladder_sweep(
    nine_arrival_scene,
    sort(unique(c(seq(13, 45, by = 0.05), seq(16.8, 17.6, by = 0.01))))
  )
  expect_true(all(c(2L, 3L, 4L) %in% coarse$rungs))
  expect_lte(coarse$target, sep_e_default + 1e-9)
  expect_lte(coarse$rung4, sep_e_default + 1e-9)

  # two ranks and nine, each swept across its own rung 3 / 4 handover
  for (sweep in list(
    ladder_sweep(two_arrival_scene, seq(20, 26, by = 0.01)),
    ladder_sweep(nine_arrival_scene, seq(30, 36, by = 0.01))
  )) {
    expect_lte(sweep$rung4, 0.0101)
    expect_lte(sweep$target4, 0.0101)
    expect_gt(sweep$handover, 0.9)
    expect_lte(sweep$handover, 1.26)
  }
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

# One target with a level source on its own line and one arrival from
# above, at node radius `r`. The row rule reads the radius through the
# half-height a head fits in, h = r - head_w / 2, and through the floor
# max(sep_e / 2, sep_min); h shrinks with the radius faster than the floor
# does, so these three nodes reach every regime of the rule.
small_node_scene <- function(r) {
  list(
    nodes = mm_nodes(c("x", "a", "y"), c(20, 20, 60), c(55, 75, 55), r = r),
    edges = mm_edges(c("x", "a"), c("y", "y")),
    bounds = c(0, 0, 80, 110)
  )
}

# The same target with no level owner: one arrival from above and one from
# below, the pair that straddles the centre line.
small_node_pair_scene <- function(r) {
  list(
    nodes = mm_nodes(c("a", "b", "y"), c(20, 20, 60), c(75, 35, 55), r = r),
    edges = mm_edges(c("a", "b"), c("y", "y")),
    bounds = c(0, 0, 80, 110)
  )
}

# Route a scene of node radius `r` with the constants that radius gives.
ortho_at <- function(scene, r) {
  route_scene(scene, mode = "orthogonal", opts = route_constants(r))
}

test_that("orthogonal ports: a singleton row under the floor takes the centre", {
  # Beside a level owner the one arrival takes the row min(sep_e, h). At the
  # default radius that is sep_e, well above the floor, but h falls to the
  # floor at r = 2.15 and below it after that, and under r = 0.65 it turns
  # negative, which draws the arrival on the far side of the centre from
  # its source. A row the floor cannot hold collapses onto the centre row
  # instead, where the head is drawn on the owner's line at the whole cap.
  # The resect of a row at offset o is cap - r + sqrt(r^2 - o^2).
  cases <- list(
    list(r = 6, row = 58.6, resect = 6.8),
    list(r = 3, row = 56.8, resect = 7.4),
    list(r = 2.15, row = 56.5, resect = 7.390292),
    list(r = 1.5, row = 55, resect = 8),
    list(r = 1, row = 55, resect = 8),
    list(r = 0.5, row = 55, resect = 8)
  )
  for (case in cases) {
    scene <- small_node_scene(case$r)
    res <- ortho_at(scene, case$r)
    label <- sprintf("r = %s", case$r)
    i <- edge_index(scene, "a->y")
    expect_equal(
      res$meta$mode[edge_index(scene, "x->y")],
      "straight",
      label = label
    )
    expect_equal(
      arrival_row(scene, res, "a->y"),
      case$row,
      tolerance = 1e-6,
      label = label
    )
    expect_equal(
      res$meta$resect_head[[i]],
      case$resect,
      tolerance = 1e-6,
      label = label
    )
    # the source is above the target, so its row never crosses the centre
    expect_gte(arrival_row(scene, res, "a->y"), 55)
  }
})

test_that("orthogonal ports: a centred pair under the floor takes the centre", {
  # With no owner the pair straddles the centre at half of min(sep_e, 2 h).
  # That spacing holds the floor down to r = 1.5, where the two rows are
  # exactly sep_min apart. Below it the pair would be drawn a fraction of a
  # millimetre from the centre, and under r = 0.65 the negative h would
  # draw the arrival from above on the lower row. Both rows collapse onto
  # the centre instead.
  cases <- list(
    list(r = 6, rows = c(56.8, 53.2), resect = 7.723635),
    list(r = 1.5, rows = c(55.75, 54.25), resect = 7.799038),
    list(r = 1, rows = c(55, 55), resect = 8),
    list(r = 0.5, rows = c(55, 55), resect = 8)
  )
  for (case in cases) {
    scene <- small_node_pair_scene(case$r)
    res <- ortho_at(scene, case$r)
    label <- sprintf("r = %s", case$r)
    rows <- arrival_rows(scene, res, "y")
    expect_equal(unname(rows), case$rows, tolerance = 1e-6, label = label)
    expect_equal(
      res$meta$resect_head,
      rep(case$resect, 2),
      tolerance = 1e-6,
      label = label
    )
    # the arrival from above is never drawn under the one from below
    expect_gte(unname(rows[["a->y"]]), unname(rows[["b->y"]]))
  }
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
  # the centre row or on a port row, so the arrival is drawn as the run on
  # its row's line, leaving b through the port 1.5 mm below its centre, and
  # the slot it would have taken is left free.
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
  expect_equal(
    c(path$x[[1]], path$y[[1]]),
    c(ends$from[[1]], 58.6),
    tolerance = 1e-9
  )
  expect_equal(c(path$x[[2]], path$y[[2]]), c(100, 58.6), tolerance = 1e-9)
  expect_length(slot_xs(path, c(20, 100)), 0)
  expect_equal(
    res$meta$resect_fins[[i]],
    port_resect_at(1.5),
    tolerance = 1e-9
  )
  expect_equal(
    res$meta$resect_head[[i]],
    port_resect_at(sep_e_default),
    tolerance = 1e-9
  )

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
  # x sits 1.749 mm below it, so the run on the row replaces a jog no reader
  # could see as two corners. x leaves through the port on that row, so the
  # run is level and the tail's resect is the face resect of the 1.749 mm
  # offset.
  scene <- canonical_scene("triple_confound", c(100, 70))
  res <- ortho(scene)
  i <- edge_index(scene, "x->m")
  path <- res$paths[[i]]
  ends <- edge_endpoints(scene, i)
  off <- ends$to[[2]] + sep_e_default - ends$from[[2]]

  expect_equal(res$meta$mode[i], "straight")
  expect_identical(nrow(path), 2L)
  expect_equal(path$y[[2]], ends$to[[2]] + sep_e_default, tolerance = 1e-6)
  expect_equal(path$x[[2]], ends$to[[1]], tolerance = 1e-9)
  jog <- abs(path$y[[1]] - path$y[[2]])
  expect_lte(jog, rc_default)
  tilt <- atan2(jog, abs(path$x[[2]] - path$x[[1]])) * 180 / pi
  expect_lt(tilt, 5)

  expect_equal(jog, 0, tolerance = 1e-9)
  expect_equal(path$x[[1]], ends$from[[1]], tolerance = 1e-9)
  expect_lte(abs(off), rc_default)
  expect_equal(
    res$meta$resect_fins[[i]],
    port_resect_at(abs(off)),
    tolerance = 1e-9
  )
  expect_equal(
    res$meta$resect_head[[i]],
    port_resect_at(sep_e_default),
    tolerance = 1e-9
  )
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
  # At 20.8 mm the slot nearest the target sits cap + head + head_margin
  # from its layer, so the run each head is drawn on holds a row as well as
  # the head. The pair straddles the target's centre line at
  # +- sep_e / 2, the arrangement two arrivals with no level owner take, and
  # each tip lands on the disc face of its own row.
  scene <- floored_pair_scene(20.8)
  res <- ortho(scene)
  gaps <- res$ortho$gaps
  expect_equal(gaps$rung, 4)
  expect_equal(gaps$ranks, 2)
  expect_equal(gaps$width, 20.8)

  # the slots are the ones the ladder placed: the row rule reads them, it
  # does not move them
  slots <- sort(ladder_slots(ortho(scene, corners = "sharp")))
  expect_equal(slots, c(27.2, 29), tolerance = 1e-6)
  expect_equal(
    40.8 - max(slots),
    head_run_margin_default,
    tolerance = 1e-6
  )

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
  # A gap holds the whole head run only from cap + head + sep_min (K - 1) mm
  # on: at 11.4 mm two slots at sep_min sit on the source's centre line and
  # the run left to the target is 9.9 mm, too short to carry a row as well
  # as a head. Both arrivals stay on the centre line, exactly as they are
  # drawn today.
  scene <- floored_pair_scene(11.4)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  slots <- sort(ladder_slots(ortho(scene, corners = "sharp")))
  expect_equal(slots, c(20, 21.5), tolerance = 1e-6)
  expect_equal(31.4 - max(slots), 9.9, tolerance = 1e-6)
  expect_lt(31.4 - max(slots), head_run_default)
  expect_equal(unname(arrival_offsets(scene, res, "t")), c(0, 0))

  # the nine-source gap at 18 mm is the same case with nine slots: they take
  # 12 mm of an 18 mm gap, the target-side slot stops 6 mm out, and all nine
  # arrivals keep the centre row
  scene <- nine_arrival_scene(18)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  expect_lt(38 - max(sort(ladder_slots(res))), head_run_default)
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
  # multi_mediator at the small panel. y takes a level chord from x on its
  # centre row, m1 from below, and three arrivals from above: h / 3 = 1.783
  # is under the 1.8 mm floor, so the three merge onto the first free row
  # at sep_e and their heads still coincide, the degrade R4 accepts rather
  # than move the floor. m2 has an owner and one arrival on each side,
  # which fit, and m3 has a pair with no owner.
  offs <- canonical_arrival_offsets("multi_mediator", "y")
  expect_equal(unname(offs[["x->y"]]), 0, tolerance = 1e-6)
  expect_equal(unname(offs[["m1->y"]]), -sep_e_default, tolerance = 1e-6)
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

test_that("orthogonal ports: a level chord is the only owner of its target's row", {
  # multi_mediator's y takes the level chord from x, drawn on y's own line,
  # and m1's spanning channel, which took that same line: their last runs
  # coincided over 87 mm at the 160 by 110 panel and one head was drawn on
  # top of the other. A target's centre row has one owner, so no spanning
  # candidate may run on the line of a node a level chord arrives at: m1->y
  # keeps its own source's line and drops to the row below y's centre at
  # the slot of the gap it arrives through.
  for (panel in canonical_panels) {
    scene <- canonical_scene("multi_mediator", panel)
    res <- ortho(scene)
    label <- paste("panel", panel[[1]])
    ty <- node_xy(scene, "y")[[2]]
    layers <- infer_layers(scene$nodes, r_default)
    chord <- edge_index(scene, "x->y")
    channel <- edge_index(scene, "m1->y")

    expect_equal(res$meta$mode[[chord]], "straight", label = label)
    expect_equal(
      res$paths[[chord]]$y,
      rep(ty, 2),
      tolerance = 1e-9,
      label = label
    )
    expect_equal(res$meta$resect_head[[chord]], cap_default, label = label)

    expect_equal(res$meta$mode[[channel]], "orthogonal", label = label)
    runs <- straight_runs(dedupe_path(res$paths[[channel]]))
    expect_equal(runs$axis, c("h", "v", "h"), label = label)
    expect_equal(
      runs$coord[[1]],
      node_xy(scene, "m1")[[2]],
      tolerance = 1e-6,
      label = label
    )
    expect_equal(
      runs$coord[[3]],
      ty - sep_e_default,
      tolerance = 1e-6,
      label = label
    )
    expect_equal(
      res$meta$resect_head[[channel]],
      port_resect_at(sep_e_default),
      tolerance = 1e-9,
      label = label
    )
    expect_gt(runs$coord[[2]], layers$x[[layers$n - 1L]])
    expect_lt(runs$coord[[2]], layers$x[[layers$n]])

    # the chord and the channel share no ink at all
    expect_equal(
      shared_run_length(res$paths[[chord]], res$paths[[channel]]),
      0,
      label = label
    )

    # the rows at y: the chord on the centre, m1 below it, and the three
    # arrivals from above merged onto the row above
    offs <- arrival_offsets(scene, res, "y")
    expect_equal(unname(offs[["x->y"]]), 0, tolerance = 1e-9, label = label)
    expect_equal(
      unname(offs[["m1->y"]]),
      -sep_e_default,
      tolerance = 1e-9,
      label = label
    )
    expect_equal(
      unname(offs[c("m2->y", "m3->y", "u->y")]),
      rep(sep_e_default, 3),
      tolerance = 1e-9,
      label = label
    )
  }
})

# Two level chords into one node. Their sources share a layer, which is the
# only way two of them reach one node: a chord from a farther layer passes
# within R of the nearer source, which that source's own level chord holds
# within rc of the target's line, and a blocked run bends.
two_owner_scene <- function() {
  list(
    nodes = mm_nodes(c("x1", "x2", "y"), c(80, 80, 140), c(53.2, 56.8, 55)),
    edges = mm_edges(c("x1", "x2"), c("y", "y")),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("orthogonal ports: two level chords into one node take one row each", {
  # Both chords are level with y inside the corner radius, so both were
  # drawn on its line, 60 mm of shared run and one head over the other. One
  # keeps the row and the other is an ordinary arrival: it takes the row
  # above, and since its source is within rc of that row it is the run on
  # the row's line, its tail leaving x2 at the face resect of the 1.8 mm
  # offset.
  scene <- two_owner_scene()
  res <- ortho(scene)
  keeper <- edge_index(scene, "x1->y")
  moved <- edge_index(scene, "x2->y")

  expect_equal(res$paths[[keeper]]$y, rep(55, 2), tolerance = 1e-9)
  expect_equal(res$meta$resect_head[[keeper]], cap_default)
  expect_equal(
    res$meta$resect_fins[[keeper]],
    port_resect_at(1.8),
    tolerance = 1e-9
  )
  expect_equal(
    res$paths[[moved]]$y,
    rep(55 + sep_e_default, 2),
    tolerance = 1e-9
  )
  expect_equal(
    res$meta$resect_head[[moved]],
    port_resect_at(sep_e_default),
    tolerance = 1e-9
  )
  expect_equal(
    res$meta$resect_fins[[moved]],
    port_resect_at(1.8),
    tolerance = 1e-9
  )

  offs <- arrival_offsets(scene, res, "y")
  expect_equal(unname(offs[["x1->y"]]), 0, tolerance = 1e-9)
  expect_equal(unname(offs[["x2->y"]]), sep_e_default, tolerance = 1e-9)
  expect_equal(shared_run_length(res$paths[[keeper]], res$paths[[moved]]), 0)
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
  # A 14 mm band holds four slots at sep_min and 9.5 mm of run before the
  # target, half a millimetre short of the floor, so no arrival takes a row:
  # every one is still drawn on its target's centre line.
  scene <- narrow_band_scene(gap = 14)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)
  slots <- sort(ladder_slots(res))
  expect_equal(slots, c(33, 34.5, 36, 37.5), tolerance = 1e-6)
  expect_lt(47 - max(slots), head_run_default)
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
    sort(ladder_slots(ortho(scene, corners = "sharp"))),
    c(30 + r_soft, 50.8 - head_run_margin_default),
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

# The scene reflected in the panel's vertical centre line, which reverses
# the layer order and sends every edge from right to left. `mirror_scene()`
# reflects y instead, so it cannot produce this picture. The name carries a
# suffix so a census failure says which copy it read, and the canonical
# layer index is renumbered from the far end to stay the index of the layer
# the node now sits in.
mirror_scene_x <- function(scene) {
  scene$nodes$x <- scene$bounds[[1]] + scene$bounds[[3]] - scene$nodes$x
  if (!is.null(scene$layer)) {
    scene$layer <- max(scene$layer) + 1L - scene$layer
  }
  scene$name <- paste(scene$name %||% "fixture", "mirrored")
  scene
}

# A scene list alongside the mirror image of each of its scenes.
with_mirrored_scenes <- function(scenes) {
  c(scenes, lapply(scenes, mirror_scene_x))
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

# Leftward rows ------------------------------------------------------------------

# Rows belong to the head end of an edge. An edge drawn rightwards arrives
# at its target's W side and one drawn leftwards at its target's E side,
# while the node an edge leaves keeps the centre of the side it departs
# through. Every scene below is one of the scenes above reflected in the
# panel's vertical centre line, so it draws the mirror image of the rows the
# forward scene draws.

test_that("orthogonal ports: a leftward pair straddles its target's centre line", {
  # centred_port_scene(2) drawn right to left. The two arrivals at t take
  # the rows +- sep_e / 2 on t's E side, each tip lands on the face of its
  # own row, and the two heads are drawn on two points rather than one. The
  # sources keep their own centres, since the row belongs to the end the
  # head is at.
  forward <- centred_port_scene(2)
  scene <- mirror_scene_x(forward)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  rows <- arrival_rows(scene, res, "t")
  expect_equal(
    unname(rows[["b1->t"]]),
    55 - sep_e_default / 2,
    tolerance = 1e-6
  )
  expect_equal(
    unname(rows[["b2->t"]]),
    55 + sep_e_default / 2,
    tolerance = 1e-6
  )
  expect_equal(
    unname(rows),
    unname(arrival_rows(forward, ortho(forward), "t")),
    tolerance = 1e-6
  )
  expect_gte(abs(diff(unname(rows))), row_floor_default - 1e-9)

  # each head stops at the face of its own row and is drawn along the run
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
  tips <- lapply(
    1:2,
    function(i) arc_from_end(res$paths[[i]], res$meta$resect_head[[i]])
  )
  expect_gte(abs(tips[[1]][[2]] - tips[[2]][[2]]), row_floor_default - 1e-9)

  # the departures leave b1 and b2 through their own centres, so the first
  # run of each path is at its source's y and the tail keeps the whole cap
  for (i in 1:2) {
    runs <- straight_runs(dedupe_path(res$paths[[i]]))
    expect_equal(
      runs$coord[[1]],
      node_xy(scene, scene$edges$from[[i]])[[2]],
      tolerance = 1e-6,
      label = edge_labels(scene$edges)[[i]]
    )
  }
  expect_equal(res$meta$resect_fins, rep(cap_default, 2))
})

# One source on the right with nine targets on the left, every edge crossing
# the same gap leftwards.
leftward_fan_scene <- function(gap = 30) {
  n <- 9L
  list(
    nodes = mm_nodes(
      c("s", paste0("t", seq_len(n))),
      c(20 + gap, rep(20, n)),
      c(100, 100 + 22.5 * (seq_len(n) - (n + 1) / 2))
    ),
    edges = mm_edges(rep("s", n), paste0("t", seq_len(n))),
    bounds = c(0, 0, 40 + gap, 200)
  )
}

test_that("orthogonal ports: a leftward fan leaves its source through its centre", {
  # Nine targets to the left of one source. A departure holds the centre of
  # the side it leaves through, so every path's first run is at s's own y
  # and every tail keeps the whole cap; each target takes one arrival, and
  # a lone arrival is drawn on its target's centre row.
  scene <- leftward_fan_scene()
  res <- ortho(scene)
  labels <- edge_labels(scene$edges)
  for (i in seq_along(labels)) {
    runs <- straight_runs(dedupe_path(res$paths[[i]]))
    expect_equal(runs$coord[[1]], 100, tolerance = 1e-6, label = labels[[i]])
    expect_equal(
      runs$coord[[nrow(runs)]],
      node_xy(scene, scene$edges$to[[i]])[[2]],
      tolerance = 1e-6,
      label = labels[[i]]
    )
  }
  expect_equal(res$meta$resect_fins, rep(cap_default, length(labels)))
  expect_equal(res$meta$resect_head, rep(cap_default, length(labels)))
})

# One node with an arrival on the side it also departs through: u -> t comes
# in from the left and t -> v leaves to the left, both across t's W side.
mixed_side_scene <- function() {
  list(
    nodes = mm_nodes(c("t", "u", "v"), c(100, 20, 20), c(55, 70, 40)),
    edges = mm_edges(c("u", "t"), c("t", "v")),
    bounds = c(0, 0, 140, 110)
  )
}

test_that("orthogonal ports: an arrival beside a departure takes the row above it", {
  # t -> v leaves t through the centre of its W side and owns that centre
  # row, so u -> t takes the first row above it, s = min(sep_e, h) = 3.6,
  # and its head stops at the face of that row. Reflected, the same two
  # edges draw the same two rows on t's E side.
  for (scene in list(mixed_side_scene(), mirror_scene_x(mixed_side_scene()))) {
    res <- ortho(scene)
    label <- scene$name %||% "forward"
    ut <- edge_index(scene, "u->t")
    tv <- edge_index(scene, "t->v")

    expect_equal(
      arrival_row(scene, res, "u->t"),
      55 + sep_e_default,
      tolerance = 1e-6,
      label = label
    )
    expect_equal(
      res$meta$resect_head[[ut]],
      port_resect_at(sep_e_default),
      tolerance = 1e-9,
      label = label
    )

    # the departure keeps t's centre line and arrives at v's own centre
    runs <- straight_runs(dedupe_path(res$paths[[tv]]))
    expect_equal(runs$coord[[1]], 55, tolerance = 1e-6, label = label)
    expect_equal(
      arrival_row(scene, res, "t->v"),
      40,
      tolerance = 1e-6,
      label = label
    )
    expect_equal(res$meta$resect_head[[tv]], cap_default, label = label)
    expect_equal(res$meta$resect_fins[[tv]], cap_default, label = label)
  }
})

test_that("orthogonal ports: a leftward stack takes the rows of its forward twin", {
  # stacked_port_scene(2) reflected. The level chord from a arrives at t's E
  # side and keeps its centre row, and the two arrivals above it take 2.675
  # and 5.35: the same three rows and the same three resects the forward
  # scene draws, and every head is drawn on the disc.
  forward <- stacked_port_scene(2)
  scene <- mirror_scene_x(forward)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  rows <- arrival_rows(scene, res, "t")
  expect_equal(unname(rows), c(55, 57.675, 60.35), tolerance = 1e-6)
  expect_equal(
    unname(rows),
    unname(arrival_rows(forward, ortho(forward), "t")),
    tolerance = 1e-6
  )
  expect_equal(
    res$meta$resect_head,
    port_resect_at(c(0, 2.675, 5.35)),
    tolerance = 1e-6
  )
  expect_lte(max(abs(rows - 55)), port_row_max + 1e-9)
})

test_that("orthogonal ports: a floored gap crossed leftwards gives its arrivals rows", {
  # floored_pair_scene(20.8) reflected. The gap still reaches the floor, so
  # the run each head is drawn on holds a row as well as the head and the
  # pair straddles t's centre line at +- sep_e / 2 on the E side, the mirror
  # image of the rows the forward scene draws.
  forward <- floored_pair_scene(20.8)
  scene <- mirror_scene_x(forward)
  res <- ortho(scene)
  expect_equal(res$ortho$gaps$rung, 4)

  offs <- arrival_offsets(scene, res, "t")
  expect_equal(unname(offs[["s1->t"]]), -sep_e_default / 2, tolerance = 1e-6)
  expect_equal(unname(offs[["s2->t"]]), sep_e_default / 2, tolerance = 1e-6)
  expect_equal(
    unname(offs),
    unname(arrival_offsets(forward, ortho(forward), "t")),
    tolerance = 1e-6
  )
  expect_equal(
    res$meta$resect_head,
    rep(port_resect_at(sep_e_default / 2), 2),
    tolerance = 1e-6
  )
})

# The offset of every head drawn on a horizontal run from its target's
# centre line, with the resect that head is cut at. A head on a vertical
# stub keeps its own port whichever way the scene runs, so it is left out.
head_row_table <- function(scene) {
  res <- ortho(scene)
  labels <- edge_labels(scene$edges)
  keep <- which(vapply(
    res$paths,
    function(path) identical(last_run(path)$axis, "h"),
    logical(1)
  ))
  data.frame(
    edge = labels[keep],
    offset = vapply(
      keep,
      function(i) {
        last_run(res$paths[[i]])$coord -
          node_xy(scene, scene$edges$to[[i]])[[2]]
      },
      numeric(1)
    ),
    resect = res$meta$resect_head[keep],
    stringsAsFactors = FALSE
  )
}

test_that("orthogonal ports: a reflected hand fixture draws the mirror of its rows", {
  # The four hand fixtures reflected. Their rows and head resects are the
  # same edge by edge whichever way the scene runs, which is what it means
  # for the row to belong to the head end. Whole paths are not compared:
  # the canonical tie-breaks read x, so a reflected scene may pack its
  # channels differently.
  for (scene in list(
    fan_scene(),
    four_layer_scene(),
    mediator_scene(),
    chain_scene()
  )) {
    expect_equal(
      head_row_table(mirror_scene_x(scene)),
      head_row_table(scene),
      tolerance = 1e-6
    )
  }
})

# Source keys and head-end ownership --------------------------------------------

# Three layers. The 14 mm gap between the staircase `a1..a4 -> b1..b4` is
# the rung-4 gap of `narrow_band_scene()`, too narrow for a stub and short
# of the head-run floor, and the 60 mm gap beyond it is wide. `a1 -> t`
# crosses both and `b1 -> t` only the second, so `t` takes two arrivals and
# no owner; `u` keeps `t` off the bottom of its layer so that no S/N
# channel is offered for the spanning edge.
narrow_source_scene <- function() {
  list(
    name = "narrow_source",
    nodes = mm_nodes(
      c("a1", "a2", "a3", "a4", "b1", "b2", "b3", "b4", "t", "u"),
      c(rep(33, 4), rep(47, 4), 107, 107),
      c(20, 35, 50, 65, 50, 65, 80, 95, 30, 10)
    ),
    edges = mm_edges(
      c("a1", "a2", "a3", "a4", "a1", "b1"),
      c("b1", "b2", "b3", "b4", "t", "t")
    ),
    bounds = c(0, 0, 140, 110)
  )
}

test_that("orthogonal ports: a narrow gap beside the source leaves the arrival its row", {
  # An arrival out of a gap too narrow for any stub keeps its target's
  # centre row, since the gap leaves no run to hold a row as well as a
  # head. The gap that decides is the one the head arrives through, which
  # for a leftward edge is its first gap and not its last: reflected,
  # `a1 -> t` arrives through the 60 mm gap and takes its row, while the
  # narrow gap it crosses on the way out of `a1` is beside its source and
  # says nothing about the row. Read at the far gap instead, both arrivals
  # at `t` would be drawn on `t`'s own centre line.
  forward <- narrow_source_scene()
  for (scene in list(forward, mirror_scene_x(forward))) {
    res <- ortho(scene)
    label <- scene$name
    gaps <- res$ortho$gaps
    expect_equal(sort(gaps$width), c(14, 60), tolerance = 1e-9, label = label)
    expect_equal(sort(gaps$rung), c(0, 4), label = label)

    offs <- arrival_offsets(scene, res, "t")
    expect_equal(
      unname(offs[["a1->t"]]),
      -sep_e_default / 2,
      tolerance = 1e-6,
      label = label
    )
    expect_equal(
      unname(offs[["b1->t"]]),
      sep_e_default / 2,
      tolerance = 1e-6,
      label = label
    )
    expect_gte(abs(diff(unname(offs))), row_floor_default - 1e-9)
  }
})

test_that("orthogonal slots: every leftward arrival at a node takes its own slot", {
  # A hyperedge segment is the pieces leaving one source port, so the
  # arrivals at one node through one gap belong to as many segments, and
  # take as many slots, as they have sources, whichever way they run.
  # Reflected, `centred_port_scene()` draws the forward slots reflected in
  # its panel. Keyed on the left-hand node instead, every leftward arrival
  # at `t` is one segment on one slot and the verticals of two and three
  # different sources are drawn on one x, a line the DAG does not have.
  for (n in 2:3) {
    forward <- centred_port_scene(n)
    scene <- mirror_scene_x(forward)
    res <- ortho(scene)
    expect_orthogonal_scene(scene, res, stub_always = TRUE)

    slots <- ladder_slots(res)
    expect_length(unique(round(slots, 9)), n)
    expect_equal(
      slots,
      scene$bounds[[3]] - ladder_slots(ortho(forward)),
      tolerance = 1e-6
    )
    expect_gte(min(diff(sort(slots))), sep_min_default - 1e-9)
  }
})

test_that("orthogonal ports: a leftward stack of two groups keeps the forward rows", {
  # Two arrivals from above and two from below, reflected. The slots are
  # read from the source's side of the gap, so their order is reversed in a
  # scene drawn leftwards and the rows come out edge for edge as the
  # forward scene draws them. Read in the raw order of the slot x, the two
  # arrivals from below swap rows, which is the crossing the order exists
  # to avoid.
  forward <- centred_port_scene(2, 2)
  scene <- mirror_scene_x(forward)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  rows <- arrival_rows(scene, res, "t")
  expect_equal(
    unname(rows),
    unname(arrival_rows(forward, ortho(forward), "t")),
    tolerance = 1e-6
  )
  expect_equal(
    unname(rows),
    55 + c(1.7833333, 5.35, -1.7833333, -5.35),
    tolerance = 1e-6
  )
})

# A level chord and a spanning edge into one node. `c -> y` is one layer
# long and level, so it is drawn as the run on `y`'s own line and owns
# `y`'s centre row. `s -> y` spans three gaps: its own line at 40 is
# blocked by `p1`, the band below the crossed stacks is at 31, and the band
# above them reaches exactly `y`'s line, so 70 is the line it would take
# were the row free. `z` keeps `s` off the bottom of its layer so that no
# S/N channel is offered.
owned_line_scene <- function() {
  list(
    name = "owned_line",
    nodes = mm_nodes(
      c("z", "s", "c", "p1", "p2", "q1", "y"),
      c(20, 20, 20, 60, 60, 100, 140),
      c(15, 40, 70, 40, 55, 61, 70)
    ),
    edges = mm_edges(c("c", "s"), c("y", "y")),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("orthogonal packing: a leftward level chord keeps its own target's line", {
  # A level chord owns the centre row of the node its head arrives at,
  # whichever way it is drawn, and no other run may take that line. Read at
  # the canonical right node instead, a reflected scene protects the
  # chord's source rather than its target, and a scene with no rightward
  # chord at all protects nothing: `s -> y` is then drawn along `c -> y`'s
  # own run, two edges on one line into one centre row.
  forward <- owned_line_scene()
  h_runs <- function(scene) {
    res <- ortho(scene)
    runs <- straight_runs(dedupe_path(res$paths[[edge_index(scene, "s->y")]]))
    runs$coord[runs$axis == "h"]
  }
  # s's own line, the band R below the crossed stacks, and the row sep_e
  # under the centre the chord owns
  lines <- c(40, 40 - r_full, 70 - sep_e_default)
  expect_equal(h_runs(forward), lines, tolerance = 1e-6)
  expect_equal(h_runs(mirror_scene_x(forward)), lines, tolerance = 1e-6)

  for (scene in list(forward, mirror_scene_x(forward))) {
    res <- ortho(scene)
    label <- scene$name
    on_line <- vapply(
      res$paths,
      function(path) {
        runs <- straight_runs(dedupe_path(path))
        any(runs$axis == "h" & abs(runs$coord - 70) < 1e-6)
      },
      logical(1)
    )
    expect_equal(
      edge_labels(scene$edges)[on_line],
      "c->y",
      label = label
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

# The hand fixtures and the canonical DAGs at both panels, every one of them
# ordered left to right as a gallery layout is.
forward_census_scenes <- function() {
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

# The scenes the head census runs over: those, and the mirror image of each.
# Every scene above runs left to right, so without the mirrored copies no
# census reads the router's leftward branches.
head_census_scenes <- function() {
  with_mirrored_scenes(forward_census_scenes())
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

# The oblique-run census -------------------------------------------------------

# The three device sizes the routing gallery renders at, as panel dimensions
# in millimetres: 4 x 3, 7 x 5 and 10 x 6 inches, each less the 4.2175 mm the
# theme leaves outside the panel.
gallery_panels <- list(
  c(97.3824823578, 71.9824823578),
  c(173.5824823578, 122.7824823578),
  c(249.7824823578, 148.1824823578)
)

# The largest scene the gallery draws, in the coordinates the router is
# handed: thirty nodes over eleven layers, each at its layer's share of the
# panel width and its own fraction of the panel height, joined by the
# fifty-six edges of the DAG. Two of its edges are the level chords the
# round was reported on.
very_big_nodes <- data.frame(
  name = c(
    "parental_ses",
    "genetics",
    "education",
    "birth_weight",
    "adversity",
    "occupation",
    "stress",
    "social_support",
    "income",
    "depression",
    "nutrition",
    "phys_act",
    "diet",
    "sleep",
    "bmi",
    "healthcare_access",
    "alcohol",
    "medication",
    "insulin_resistance",
    "air_pollution",
    "smoking",
    "diabetes",
    "bp",
    "inflammation",
    "chol",
    "ckd",
    "cancer",
    "cvd",
    "frailty",
    "mortality"
  ),
  layer = c(
    1L,
    2L,
    2L,
    3L,
    3L,
    3L,
    3L,
    3L,
    4L,
    4L,
    4L,
    5L,
    5L,
    5L,
    6L,
    6L,
    7L,
    7L,
    7L,
    8L,
    8L,
    8L,
    8L,
    9L,
    9L,
    9L,
    10L,
    10L,
    10L,
    11L
  ),
  fraction = c(
    0.443717277487,
    0.247818499127,
    0.335078534031,
    0.45462478185,
    0.592495636998,
    0.702879581152,
    0.794284467714,
    0.916666666667,
    0.409904013962,
    0.497164048866,
    0.753054101222,
    0.694153577661,
    0.781413612565,
    0.868673647469,
    0.2842495637,
    0.371509598604,
    0.299520069808,
    0.602530541012,
    0.689790575916,
    0.0833333333333,
    0.335296684119,
    0.532722513089,
    0.622382198953,
    0.126745200698,
    0.214005235602,
    0.564354275742,
    0.258944153578,
    0.346204188482,
    0.568935427574,
    0.391797556719
  ),
  stringsAsFactors = FALSE
)

very_big_edge_specs <- c(
  "adversity->depression",
  "adversity->smoking",
  "air_pollution->inflammation",
  "alcohol->bp",
  "alcohol->cancer",
  "birth_weight->nutrition",
  "bmi->bp",
  "bmi->inflammation",
  "bmi->insulin_resistance",
  "bp->ckd",
  "bp->cvd",
  "cancer->mortality",
  "chol->cvd",
  "ckd->frailty",
  "ckd->mortality",
  "cvd->mortality",
  "depression->medication",
  "depression->phys_act",
  "diabetes->ckd",
  "diabetes->cvd",
  "diet->bmi",
  "education->healthcare_access",
  "education->income",
  "education->occupation",
  "education->smoking",
  "frailty->mortality",
  "genetics->birth_weight",
  "genetics->bmi",
  "genetics->chol",
  "healthcare_access->medication",
  "income->diet",
  "income->healthcare_access",
  "income->phys_act",
  "inflammation->cancer",
  "inflammation->cvd",
  "insulin_resistance->diabetes",
  "medication->bp",
  "nutrition->diet",
  "occupation->income",
  "parental_ses->adversity",
  "parental_ses->birth_weight",
  "parental_ses->education",
  "parental_ses->nutrition",
  "phys_act->bmi",
  "phys_act->bp",
  "phys_act->cvd",
  "sleep->bmi",
  "smoking->cancer",
  "smoking->chol",
  "smoking->cvd",
  "social_support->depression",
  "social_support->phys_act",
  "stress->alcohol",
  "stress->depression",
  "stress->sleep",
  "stress->smoking"
)

very_big_scene <- function(panel) {
  parts <- strsplit(very_big_edge_specs, "->", fixed = TRUE)
  list(
    name = "very_big",
    nodes = mm_nodes(
      very_big_nodes$name,
      very_big_nodes$layer / 12 * panel[1],
      very_big_nodes$fraction * panel[2]
    ),
    edges = mm_edges(
      vapply(parts, `[[`, character(1), 1L),
      vapply(parts, `[[`, character(1), 2L)
    ),
    bounds = c(0, 0, panel)
  )
}

# The scenes the oblique census runs over: everything the head census sees,
# plus the canonical DAGs and the gallery's largest scene at each of the
# three device sizes, and the mirror image of every one of them.
oblique_census_scenes <- function() {
  scenes <- forward_census_scenes()
  for (panel in gallery_panels) {
    for (nm in names(canonical_dag_specs)) {
      scenes[[length(scenes) + 1L]] <- canonical_scene(nm, panel)
    }
    scenes[[length(scenes) + 1L]] <- very_big_scene(panel)
  }
  with_mirrored_scenes(scenes)
}

test_that("orthogonal: no run in any scene is drawn at an angle", {
  # An orthogonal drawing shows axis-aligned runs and rounded corners and
  # nothing else, so over every census scene at every size no segment of any
  # path across layers is oblique. The reading is the sharp polyline, the one
  # the corner rounding is applied to, since a rounded corner's samples are
  # oblique by construction. The count of level chords drawn on a line is
  # pinned too: a rule that drew none of them would satisfy everything else
  # here.
  oblique <- 0L
  crossing <- 0L
  on_a_line <- 0L
  for (scene in oblique_census_scenes()) {
    res <- ortho(scene, corners = "sharp")
    layers <- infer_layers(scene$nodes, r_default)
    layer_of <- stats::setNames(layers$id, scene$nodes$name)
    for (i in seq_len(nrow(scene$edges))) {
      from <- layer_of[[scene$edges$from[[i]]]]
      to <- layer_of[[scene$edges$to[[i]]]]
      if (from == to) {
        next
      }
      crossing <- crossing + 1L
      path <- dedupe_path(res$paths[[i]])
      axes <- segment_axes(path, 1e-6)
      label <- paste0(
        scene$name %||% "fixture",
        " ",
        edge_labels(scene$edges)[[i]],
        " at ",
        paste(round(scene$bounds[3:4], 2), collapse = " x ")
      )
      expect_equal(sum(axes == "o"), 0L, label = label)
      oblique <- oblique + sum(axes == "o")
      ends <- edge_endpoints(scene, i)
      moved <- nrow(path) == 2L &&
        res$meta$mode[[i]] == "straight" &&
        abs(path$y[[1]] - ends$from[[2]]) > 1e-9
      if (moved) {
        on_a_line <- on_a_line + 1L
      }
    }
  }
  expect_equal(oblique, 0L)
  # the census is worth having only if it covers the pictures, and only if
  # the scenes hold level chords for the rule to move
  expect_gt(crossing, 900L)
  expect_gt(on_a_line, 30L)
})

test_that("orthogonal: the reported very_big level chords are horizontal", {
  # The two edges the round was reported on, at the size they were reported
  # at. Both are span-2 level chords with their endpoints 1.616 mm apart, so
  # each is drawn as the run on its target's line: the path starts at the
  # source's x on the target's y, is exactly level, and carries the face
  # resect of the offset at its tail and the plain cap at its head.
  scene <- very_big_scene(gallery_panels[[3]])
  res <- ortho(scene)

  for (label in c("parental_ses->birth_weight", "smoking->cvd")) {
    i <- edge_index(scene, label)
    path <- res$paths[[i]]
    ends <- edge_endpoints(scene, i)
    off <- ends$from[[2]] - ends$to[[2]]

    expect_equal(
      abs(off),
      1.616,
      tolerance = 1e-3,
      label = paste(label, "offset")
    )
    expect_equal(res$meta$mode[[i]], "straight", label = paste(label, "mode"))
    expect_identical(nrow(path), 2L)
    expect_equal(
      c(path$x[[1]], path$y[[1]]),
      c(ends$from[[1]], ends$to[[2]]),
      tolerance = 1e-9,
      label = paste(label, "tail port")
    )
    expect_equal(
      c(path$x[[2]], path$y[[2]]),
      ends$to,
      tolerance = 1e-9,
      label = paste(label, "head point")
    )
    expect_equal(path$y[[1]], path$y[[2]], label = paste(label, "tilt"))
    expect_equal(
      res$meta$resect_fins[[i]],
      port_resect_at(abs(off)),
      tolerance = 1e-9,
      label = paste(label, "tail resect")
    )
    expect_equal(
      res$meta$resect_fins[[i]],
      port_resect_at(1.616),
      tolerance = 1e-3,
      label = paste(label, "tail resect at the design's offset")
    )
    expect_equal(
      res$meta$resect_head[[i]],
      cap_default,
      label = paste(label, "head resect")
    )
  }
})


# The head-crossing census ------------------------------------------------------

# The nearest point of each segment (x0, y0) - (x1, y1) to the point
# (px, py), vectorised over the segments.
nearest_on_segment <- function(px, py, x0, y0, x1, y1) {
  dx <- x1 - x0
  dy <- y1 - y0
  len2 <- dx^2 + dy^2
  along <- ifelse(len2 > 0, ((px - x0) * dx + (py - y0) * dy) / len2, 0)
  along <- pmin(pmax(along, 0), 1)
  qx <- x0 + along * dx
  qy <- y0 + along * dy
  list(d = sqrt((px - qx)^2 + (py - qy)^2), qx = qx, qy = qy)
}

# Whether the segment a properly crosses each segment b, vectorised over b.
segments_meet <- function(ax0, ay0, ax1, ay1, bx0, by0, bx1, by1) {
  cross <- function(ux, uy, vx, vy) ux * vy - uy * vx
  d1 <- cross(ax1 - ax0, ay1 - ay0, bx0 - ax0, by0 - ay0)
  d2 <- cross(ax1 - ax0, ay1 - ay0, bx1 - ax0, by1 - ay0)
  d3 <- cross(bx1 - bx0, by1 - by0, ax0 - bx0, ay0 - by0)
  d4 <- cross(bx1 - bx0, by1 - by0, ax1 - bx0, ay1 - by0)
  (d1 * d2 < 0) & (d3 * d4 < 0)
}

# The distance from a drawn head's axis, the segment from its tip to its
# base, to each segment of another path, with the nearest point on that
# segment so that a crossing hidden under a node's disc can be dropped.
head_axis_dist <- function(tip, base, x0, y0, x1, y1) {
  crossed <- segments_meet(
    tip[[1]],
    tip[[2]],
    base[[1]],
    base[[2]],
    x0,
    y0,
    x1,
    y1
  )
  a <- nearest_on_segment(tip[[1]], tip[[2]], x0, y0, x1, y1)
  b <- nearest_on_segment(base[[1]], base[[2]], x0, y0, x1, y1)
  c0 <- nearest_on_segment(x0, y0, tip[[1]], tip[[2]], base[[1]], base[[2]])
  c1 <- nearest_on_segment(x1, y1, tip[[1]], tip[[2]], base[[1]], base[[2]])
  d <- pmin(a$d, b$d, c0$d, c1$d)
  d[crossed] <- 0
  best <- max.col(-cbind(a$d, b$d, c0$d, c1$d), ties.method = "first")
  list(
    d = d,
    qx = ifelse(
      best == 1,
      a$qx,
      ifelse(best == 2, b$qx, ifelse(best == 3, x0, x1))
    ),
    qy = ifelse(
      best == 1,
      a$qy,
      ifelse(best == 2, b$qy, ifelse(best == 3, y0, y1))
    )
  )
}

# Every foreign shaft drawn across an arrowhead of a scene. The head of an
# edge is the arc of its path from the tip, `resect_head` from its end, to
# the base a further `head` back, and it is head_w wide; a segment of
# another edge's path is drawn across it when it comes within
# head_w / 2 + tol of that axis, which at the default tolerance of half the
# shaft width is where the two inks overlap. Segments whose nearest point is
# hidden under a node's disc draw nothing, and an edge sharing the head's
# target is not foreign: those are the merged rows and shared runs of a
# stack, which the port rules own. Each hit carries the gap its segment
# crosses when that segment is a vertical inside one, so that a hit in a gap
# wide enough to have avoided it can be told from a hit in one that could
# not.
head_crossings <- function(scene, res, tol = shaft_default / 2, resect = NULL) {
  layers <- infer_layers(scene$nodes, r_default)
  gaps <- res$ortho$gaps
  paths <- lapply(res$paths, dedupe_path)
  drawn <- vapply(paths, nrow, integer(1)) >= 2L
  labels <- edge_labels(scene$edges)
  hits <- list()
  for (i in seq_len(nrow(scene$edges))) {
    if (!drawn[[i]]) {
      next
    }
    resect_i <- resect %||% res$meta$resect_head[[i]]
    tip <- arc_from_end(paths[[i]], resect_i)
    base <- arc_from_end(paths[[i]], resect_i + head_default)
    for (j in seq_len(nrow(scene$edges))) {
      if (j == i || !drawn[[j]] || scene$edges$to[[j]] == scene$edges$to[[i]]) {
        next
      }
      path <- paths[[j]]
      n <- nrow(path)
      d <- head_axis_dist(
        tip,
        base,
        path$x[-n],
        path$y[-n],
        path$x[-1],
        path$y[-1]
      )
      near <- which(d$d < head_w_default / 2 + tol)
      hidden <- vapply(
        near,
        function(k) {
          any(
            sqrt(
              (scene$nodes$x - d$qx[[k]])^2 + (scene$nodes$y - d$qy[[k]])^2
            ) <
              scene$nodes$r - 1e-9
          )
        },
        logical(1)
      )
      near <- near[!hidden]
      if (length(near) == 0) {
        next
      }
      k <- near[[which.min(d$d[near])]]
      vertical <- abs(path$x[[k + 1L]] - path$x[[k]]) < 1e-6 &&
        abs(path$y[[k + 1L]] - path$y[[k]]) > 1e-6
      x <- path$x[[k]]
      inside <- vertical &&
        !any(abs(layers$x - x) < sep_e_default / 2 + 1e-6) &&
        x > layers$x[[1]] &&
        x < layers$x[[layers$n]]
      gap <- if (inside) max(which(layers$x < x)) else NA_integer_
      row <- if (!is.na(gap) && !is.null(gaps)) {
        gaps[gaps$gap == gap, , drop = FALSE]
      } else {
        NULL
      }
      hits[[length(hits) + 1L]] <- data.frame(
        head = labels[[i]],
        cross = labels[[j]],
        dist = d$d[[k]],
        gap = gap,
        width = if (is.null(row) || nrow(row) != 1) NA_real_ else row$width,
        ranks = if (is.null(row) || nrow(row) != 1) NA_real_ else row$ranks,
        rung = if (is.null(row) || nrow(row) != 1) NA_integer_ else row$rung,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(hits) == 0) {
    return(data.frame(
      head = character(),
      cross = character(),
      dist = numeric(),
      gap = integer(),
      width = numeric(),
      ranks = numeric(),
      rung = integer(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, hits)
}

# Whether each hit's crossing segment is a vertical in a rung-4 gap wide
# enough to hold the head run with its margin at the tightest spacing the
# ladder allows, which is the gap the last rung could have kept clear.
in_a_wide_gap <- function(hits) {
  !is.na(hits$rung) &
    hits$rung == 4 &
    hits$width >=
      head_run_margin_default + sep_min_default * (hits$ranks - 1) - 1e-9
}

# Every gap of a scene that is crossed in one direction, with the shortest
# run any vertical drawn in it leaves before the target layer's centre line.
# A gap crossed both ways has no target side and is left out.
gap_head_runs <- function(scene, res) {
  layers <- infer_layers(scene$nodes, r_default)
  gaps <- res$ortho$gaps
  if (is.null(gaps) || nrow(gaps) == 0) {
    return(NULL)
  }
  layer_of <- stats::setNames(layers$id, scene$nodes$name)
  out <- list()
  for (g in gaps$gap) {
    lo <- layers$x[[g]]
    hi <- layers$x[[g + 1L]]
    sides <- integer(0)
    run <- Inf
    for (i in seq_len(nrow(scene$edges))) {
      if (res$meta$mode[[i]] != "orthogonal") {
        next
      }
      from <- layer_of[[scene$edges$from[[i]]]]
      to <- layer_of[[scene$edges$to[[i]]]]
      runs <- straight_runs(dedupe_path(res$paths[[i]]))
      inside <- runs$axis == "v" &
        runs$coord > lo + 1e-6 &
        runs$coord < hi - 1e-6
      if (!any(inside)) {
        next
      }
      if (min(from, to) <= g && max(from, to) > g) {
        sides <- c(sides, sign(to - from))
      }
      target <- if (to > from) hi else lo
      run <- min(run, min(abs(target - runs$coord[inside])))
    }
    if (length(unique(sides)) != 1L || !is.finite(run)) {
      next
    }
    row <- gaps[gaps$gap == g, , drop = FALSE]
    out[[length(out) + 1L]] <- data.frame(
      gap = g,
      width = row$width,
      ranks = row$ranks,
      rung = row$rung,
      run = run,
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0) {
    return(NULL)
  }
  do.call(rbind, out)
}

test_that("orthogonal ladder: a gap that can hold the head margin holds it", {
  # Over every census scene at every size. A rung-4 gap holds the head run
  # and its margin once it is 11.8 + sep_min (K - 1) mm wide, and in every
  # gap that wide no vertical is drawn nearer the target's layer than
  # 11.8 mm, with the nearest sitting exactly there. The gaps too narrow for
  # that are counted, not pinned: their ranks and the head run cannot both
  # be had, and the spread gives up the source's side of the gap first.
  wide <- 0L
  narrow <- 0L
  nearest <- Inf
  for (scene in oblique_census_scenes()) {
    res <- ortho(scene)
    tab <- gap_head_runs(scene, res)
    if (is.null(tab)) {
      next
    }
    tab <- tab[tab$rung == 4, , drop = FALSE]
    for (k in seq_len(nrow(tab))) {
      bound <- head_run_margin_default + sep_min_default * (tab$ranks[[k]] - 1)
      if (tab$width[[k]] < bound - 1e-9) {
        narrow <- narrow + 1L
        next
      }
      wide <- wide + 1L
      nearest <- min(nearest, tab$run[[k]])
      expect_gte(
        tab$run[[k]],
        head_run_margin_default - 1e-9,
        label = paste0(
          scene$name %||% "fixture",
          " gap ",
          tab$gap[[k]],
          " at ",
          paste(round(scene$bounds[3:4], 2), collapse = " x ")
        )
      )
    }
  }
  expect_equal(nearest, head_run_margin_default, tolerance = 1e-9)
  # the census is worth having only if the pictures put arrivals in such
  # gaps, and only if the narrow ones are there to be excluded honestly
  expect_gt(wide, 60L)
  expect_gt(narrow, 0L)
})

test_that("orthogonal heads: no foreign shaft is drawn on a head out of a gap that can spare it", {
  # The same scenes, read as ink rather than as slots: every shaft drawn
  # within half its own width of an arrowhead of another edge. None of them
  # is a vertical in a gap wide enough to have avoided it. The sixty-three
  # that remain are all in the two sizes at which the gallery's largest
  # scene has 8.1 and 14.5 mm gaps to fit eleven layers into, where no
  # arrangement of six or eight ranks leaves a head its run: thirty of them
  # (17 and 13) drawn left to right and thirty-three (26 and 7) in the
  # mirror image, where the arrivals of a gap are counted from the other
  # side and each gap takes a rank count of its own, so the narrow ones are
  # packed differently. At the largest size both copies draw none.
  hits <- list()
  for (scene in oblique_census_scenes()) {
    res <- ortho(scene)
    found <- head_crossings(scene, res)
    if (nrow(found) == 0) {
      next
    }
    hits[[length(hits) + 1L]] <- cbind(
      scene = scene$name %||% "fixture",
      found,
      stringsAsFactors = FALSE
    )
  }
  hits <- do.call(rbind, hits)
  expect_equal(sum(in_a_wide_gap(hits)), 0L)
  expect_equal(nrow(hits), 63L)
  expect_setequal(unique(hits$scene), c("very_big", "very_big mirrored"))
})

test_that("orthogonal heads: very_big draws no foreign shaft on a head at 10 x 6", {
  # The scene the ladder's floor was reported on. At 10 x 6 its eleven
  # layers sit 20.815 mm apart, which every gap can spare the margin out of,
  # and no shaft touches a head. At the two smaller sizes the gaps are 8.1
  # and 14.5 mm and the count stands where the ranks leave it.
  counts <- vapply(
    gallery_panels,
    function(panel) {
      scene <- very_big_scene(panel)
      nrow(head_crossings(scene, ortho(scene)))
    },
    integer(1)
  )
  expect_equal(counts, c(17L, 13L, 0L))

  scene <- very_big_scene(gallery_panels[[3]])
  res <- ortho(scene)
  sharp <- ortho(scene, corners = "sharp")
  layers <- infer_layers(scene$nodes, r_default)
  verticals <- function(label) {
    runs <- straight_runs(dedupe_path(sharp$paths[[edge_index(scene, label)]]))
    runs$coord[runs$axis == "v"]
  }

  # the vertical that ran down the base of both medication heads, and the
  # two heads themselves, which end on medication's layer at their own rows
  expect_equal(
    verticals("bmi->insulin_resistance"),
    133.906448,
    tolerance = 1e-6
  )
  expect_equal(
    layers$x[[7]] - 133.906448,
    head_run_margin_default,
    tolerance = 1e-6
  )
  for (label in c("healthcare_access->medication", "depression->medication")) {
    i <- edge_index(scene, label)
    path <- res$paths[[i]]
    expect_equal(path$x[[nrow(path)]], layers$x[[7]], tolerance = 1e-6)
    expect_equal(
      res$meta$resect_head[[i]],
      port_resect_at(sep_e_default / 2),
      tolerance = 1e-6
    )
  }

  # gap 7's target-side vertical, which ran through the bodies of five heads
  # at smoking, bp and diabetes
  expect_equal(max(verticals("stress->smoking")), 154.721655, tolerance = 1e-6)
  expect_equal(
    layers$x[[8]] - 154.721655,
    head_run_margin_default,
    tolerance = 1e-6
  )
})

test_that("orthogonal ports: gap 7 of very_big at 10 x 6 is floored", {
  # Seven ranks at sep_min take 9 mm of the gap's 20.815 and the head run
  # with its margin another 11.8, so the spread reaches to 0.02 mm of the
  # source layer's centre line and the target keeps its whole run. The gap
  # is floored, so its arrivals take rows: smoking's three heads and bp's
  # four are drawn on their own lines instead of piled on one.
  scene <- very_big_scene(gallery_panels[[3]])
  res <- ortho(scene)
  gaps <- res$ortho$gaps
  row <- gaps[gaps$gap == 7, , drop = FALSE]
  expect_equal(row$ranks, 7)
  expect_equal(row$rung, 4)
  expect_equal(row$spacing, sep_min_default)

  resects <- function(offs) {
    vapply(
      names(offs),
      function(label) res$meta$resect_head[[edge_index(scene, label)]],
      numeric(1)
    )
  }
  offs <- arrival_offsets(scene, res, "smoking")
  expect_equal(
    offs[c("adversity->smoking", "education->smoking", "stress->smoking")],
    c(
      "adversity->smoking" = 0,
      "education->smoking" = -sep_e_default,
      "stress->smoking" = sep_e_default
    ),
    tolerance = 1e-6
  )
  expect_equal(
    unname(resects(offs)),
    c(cap_default, rep(port_resect_at(sep_e_default), 2)),
    tolerance = 1e-6
  )

  offs <- arrival_offsets(scene, res, "bp")
  expect_equal(
    offs[c("alcohol->bp", "bmi->bp", "medication->bp", "phys_act->bp")],
    c(
      "alcohol->bp" = -port_row_max,
      "bmi->bp" = -port_row_max / 3,
      "medication->bp" = port_row_max / 3,
      "phys_act->bp" = port_row_max
    ),
    tolerance = 1e-6
  )
  expect_equal(
    unname(resects(offs)),
    port_resect_at(c(
      port_row_max,
      port_row_max / 3,
      port_row_max / 3,
      port_row_max
    )),
    tolerance = 1e-6
  )
})


# The owners of a target's centre row: the arrivals drawn on the node's own
# line whose last run reaches back across the gap they arrive through. A
# merged stack and the pile a gap too narrow for a stub leaves both jog at a
# slot in that gap, so neither is counted here; an owner runs into the node
# from the neighbouring layer on the target's own line, and two of them draw
# one head on top of the other.
centre_row_owners <- function(scene, res) {
  layers <- infer_layers(scene$nodes, r_default)
  lid <- stats::setNames(layers$id, scene$nodes$name)
  labels <- edge_labels(scene$edges)
  out <- list()
  for (target in unique(scene$edges$to)) {
    ti <- match(target, scene$nodes$name)
    ty <- scene$nodes$y[[ti]]
    tx <- scene$nodes$x[[ti]]
    owners <- character(0)
    for (i in which(scene$edges$to == target)) {
      if (res$meta$mode[[i]] == "fixed") {
        next
      }
      runs <- straight_runs(dedupe_path(res$paths[[i]]))
      if (nrow(runs) == 0) {
        next
      }
      last <- runs[nrow(runs), ]
      if (last$axis != "h" || abs(last$coord - ty) > 1e-6) {
        next
      }
      sx <- scene$nodes$x[[match(scene$edges$from[[i]], scene$nodes$name)]]
      left <- sx < tx
      layer_x <- layers$x[lid[[target]] + if (left) -1L else 1L]
      if (is.na(layer_x)) {
        next
      }
      jog <- if (left) {
        any(
          runs$axis == "v" &
            runs$coord > layer_x + 1e-6 &
            runs$coord < tx - 1e-6
        )
      } else {
        any(
          runs$axis == "v" &
            runs$coord < layer_x - 1e-6 &
            runs$coord > tx + 1e-6
        )
      }
      reaches <- if (left) {
        last$lo < layer_x + 1e-6
      } else {
        last$hi > layer_x - 1e-6
      }
      if (!jog && reaches) {
        owners <- c(owners, labels[[i]])
      }
    }
    if (length(owners) > 0) {
      out[[length(out) + 1L]] <- data.frame(
        scene = scene$name %||% "fixture",
        target = target,
        n = length(owners),
        edges = paste(owners, collapse = " + "),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) > 0) do.call(rbind, out) else NULL
}

test_that("orthogonal ports: no target is drawn two heads on its centre row", {
  # Over every census scene at every size, each target's centre row carries
  # one owner. The four that carried two are multi_mediator's y at four of
  # its five sizes, where the level chord from x and the channel from m1
  # both ran on y's line. A scene and its mirror have the same owners:
  # a segment is keyed on its source, so a leftward arrival takes a slot
  # and a row of its own and leaves the centre to whatever owns it, and the
  # ownership a scene draws is the ownership its reflection draws. The one
  # target that carries two is very_big's cvd at the small panel, where two
  # leftward channels both run on cvd's own line out of the layer beyond
  # the gap they arrive through; nothing in this round moves it.
  rows <- list()
  for (scene in oblique_census_scenes()) {
    found <- centre_row_owners(scene, ortho(scene))
    if (is.null(found)) {
      next
    }
    found$mirrored <- grepl("mirrored", scene$name %||% "")
    rows[[length(rows) + 1L]] <- found
  }
  rows <- do.call(rbind, rows)
  forward <- rows[!rows$mirrored, ]
  mirrored <- rows[rows$mirrored, ]

  # the census is worth having only if the pictures put arrivals on their
  # targets' own lines at all
  expect_equal(nrow(forward), 179L)
  expect_equal(sum(forward$n), 179L)
  expect_equal(sum(forward$n >= 2L), 0L)

  expect_equal(nrow(mirrored), nrow(forward))
  expect_equal(nrow(mirrored), 179L)
  expect_equal(sum(mirrored$n), 180L)
  expect_setequal(
    paste(mirrored$scene[mirrored$n >= 2L], mirrored$target[mirrored$n >= 2L]),
    "very_big mirrored cvd"
  )
})

# S/N head zones ------------------------------------------------------------

# The densest scene the gallery draws, in the coordinates the router is
# handed: the ten nodes of a saturated six-layer DAG, each at its layer's
# share of the panel width and its own fraction of the panel height, joined
# by all forty-one edges. It is the one drawing in which an east-west
# channel run is priced onto the line an arrowhead at the end of a
# north-south stub already occupies.
saturated_nodes <- data.frame(
  name = c("a", "b", "c", "d", "e", "f", "g", "h", "x", "y"),
  layer = c(1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L),
  fraction = c(
    0.401783941558,
    0.12084356444,
    0.611467637823,
    0.0833333333333,
    0.916666666667,
    0.867176624013,
    0.14527495897,
    0.397846631473,
    0.724010538671,
    0.424646889132
  )
)

saturated_edge_specs <- c(
  "a->b",
  "a->c",
  "a->d",
  "a->e",
  "a->f",
  "a->g",
  "a->h",
  "a->x",
  "a->y",
  "b->d",
  "b->e",
  "b->f",
  "b->g",
  "b->h",
  "b->x",
  "b->y",
  "c->d",
  "c->e",
  "c->f",
  "c->g",
  "c->h",
  "c->x",
  "c->y",
  "d->f",
  "d->g",
  "d->h",
  "d->x",
  "d->y",
  "e->f",
  "e->g",
  "e->h",
  "e->x",
  "e->y",
  "f->h",
  "f->x",
  "f->y",
  "g->h",
  "g->x",
  "g->y",
  "h->y",
  "x->y"
)

saturated_scene <- function(panel) {
  parts <- strsplit(saturated_edge_specs, "->", fixed = TRUE)
  list(
    name = "saturated",
    nodes = mm_nodes(
      saturated_nodes$name,
      (2 * saturated_nodes$layer - 1) / 12 * panel[1],
      saturated_nodes$fraction * panel[2]
    ),
    edges = mm_edges(
      vapply(parts, `[[`, character(1), 1L),
      vapply(parts, `[[`, character(1), 2L)
    ),
    bounds = c(0, 0, panel)
  )
}

# The band an arrowhead at the end of a north-south stub occupies, plus the
# margin the router keeps everywhere else: from cap - head_margin to
# cap + head + head_margin past the head node's centre along the stub,
# over the stub's x give or take sep_e / 2. `sn_head_zones()` reads one out
# of the drawing rather than out of the router's state, so a channel that
# reached its run by any route is read the same way.
sn_head_zones <- function(scene, res) {
  zones <- list()
  for (i in seq_along(res$paths)) {
    if (res$meta$mode[[i]] != "orthogonal") {
      next
    }
    runs <- straight_runs(dedupe_path(res$paths[[i]]))
    if (nrow(runs) < 2) {
      next
    }
    if (runs$axis[[1]] != "v" || runs$axis[[nrow(runs)]] != "v") {
      next
    }
    t <- match(scene$edges$to[[i]], scene$nodes$name)
    band <- sort(
      scene$nodes$y[[t]] +
        res$meta$side[[i]] *
          c(
            cap_default - head_margin_default,
            cap_default + head_default + head_margin_default
          )
    )
    zones[[length(zones) + 1L]] <- data.frame(
      e = i,
      x = scene$nodes$x[[t]],
      lo = band[[1]],
      hi = band[[2]],
      to = scene$edges$to[[i]],
      stringsAsFactors = FALSE
    )
  }
  if (length(zones) == 0) {
    return(NULL)
  }
  do.call(rbind, zones)
}

# The horizontal runs of other edges lying strictly inside one of those
# zones. An edge that shares the head's target is not foreign: those are
# the merged rows of a stack, which the port rules own.
zone_intrusions <- function(scene, res) {
  zones <- sn_head_zones(scene, res)
  if (is.null(zones)) {
    return(NULL)
  }
  labels <- edge_labels(scene$edges)
  found <- list()
  for (k in seq_len(nrow(zones))) {
    for (j in seq_along(res$paths)) {
      if (scene$edges$to[[j]] == zones$to[[k]]) {
        next
      }
      runs <- straight_runs(dedupe_path(res$paths[[j]]))
      runs <- runs[runs$axis == "h", , drop = FALSE]
      if (nrow(runs) == 0) {
        next
      }
      inside <- runs$coord > zones$lo[[k]] + 1e-9 &
        runs$coord < zones$hi[[k]] - 1e-9 &
        runs$lo < zones$x[[k]] + sep_e_default / 2 - 1e-9 &
        runs$hi > zones$x[[k]] - sep_e_default / 2 + 1e-9
      if (!any(inside)) {
        next
      }
      found[[length(found) + 1L]] <- data.frame(
        head = labels[[zones$e[[k]]]],
        run = labels[[j]],
        y = runs$coord[inside][[1]],
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(found) == 0) {
    return(NULL)
  }
  do.call(rbind, found)
}

test_that("orthogonal heads: saturated at 10 x 6 draws no horizontal run through an S/N head", {
  # b -> g is an S channel whose stub rises from y = 3.35 to g, so its
  # arrowhead stands 8 to 10 mm below g's centre. d -> h's cheapest run is
  # its own source line at 12.35, which crosses that stub 9.18 mm below g:
  # the middle of the head, and no farther from g than the R = 9 every run
  # beyond a stack keeps from the extreme node of the layer it crosses.
  # The run gives up its line for the head's margin, which costs it 2.62 mm
  # and one step down at its first slot.
  scene <- saturated_scene(gallery_panels[[3]])
  res <- ortho(scene)
  expect_equal(nrow(head_crossings(scene, res)), 0L)

  g_y <- node_xy(scene, "g")[[2]]
  g_x <- node_xy(scene, "g")[[1]]
  dh <- edge_index(scene, "d->h")
  expect_equal(res$meta$n_waypoints[[dh]], 4L)
  expect_equal(res$meta$resect_head[[dh]], 6.8, tolerance = 1e-9)
  expect_equal(
    res$waypoints[[dh]]$x,
    c(128.4912411789, 128.4912411789, 167.478900112217, 167.478900112217),
    tolerance = 1e-9
  )
  expect_equal(
    res$waypoints[[dh]]$y,
    c(
      12.3485401964784,
      9.72720404460215,
      9.72720404460215,
      55.353901449358
    ),
    tolerance = 1e-9
  )

  # the run sits the head and its margin below g, which leaves 1.8 mm of
  # daylight under the arrowhead's base at g's y - 10
  run <- channel_run(res$paths[[dh]], g_x)$coord
  expect_equal(
    run,
    g_y - (cap_default + head_default + head_margin_default),
    tolerance = 1e-9
  )
  expect_equal(
    g_y - head_run_default - run,
    head_margin_default,
    tolerance = 1e-9
  )

  # b -> g keeps its S channel, and b -> h the run its own slot gave it
  bg <- edge_index(scene, "b->g")
  expect_equal(res$meta$n_waypoints[[bg]], 2L)
  expect_equal(res$meta$resect_head[[bg]], 8, tolerance = 1e-9)
  expect_equal(
    res$waypoints[[bg]]$y,
    rep(3.34854019647839, 2),
    tolerance = 1e-9
  )
  bh <- edge_index(scene, "b->h")
  expect_equal(
    res$waypoints[[bh]]$y,
    c(17.906899355684, 44.1322729380839, 44.1322729380839, 55.353901449358),
    tolerance = 1e-9
  )

  # d -> y loses the line beside b -> g's channel, which the push and the
  # stacking rule together take below the panel margin, and crosses the
  # middle band instead; b -> y, which held that line, sits sep_e lower
  expect_equal(
    channel_run(res$paths[[edge_index(scene, "d->y")]], g_x)$coord,
    36.9322729380839,
    tolerance = 1e-9
  )
  expect_equal(
    channel_run(res$paths[[edge_index(scene, "b->y")]], g_x)$coord,
    33.3322729380839,
    tolerance = 1e-9
  )

  # two runs fewer in the third gap drop its ladder a rung
  gaps <- res$ortho$gaps
  expect_equal(gaps$ranks[gaps$gap == 3], 5L)
  expect_equal(gaps$rung[gaps$gap == 3], 1L)

  # the two smaller panels are the drawing they already were: at 4 x 3 the
  # scene is over-constrained and its count stands where the ranks leave
  # it, and at 7 x 5 no shaft touches a head
  counts <- vapply(
    gallery_panels[1:2],
    function(panel) {
      scene <- saturated_scene(panel)
      nrow(head_crossings(scene, ortho(scene)))
    },
    integer(1)
  )
  expect_equal(counts, c(23L, 0L))
})

# An east-west channel whose only affordable run is the line beyond the
# stack of the layer it crosses, which sits R = 9 mm past that layer's top
# node: exactly the centre of the arrowhead of an N stub standing there.
# p -> n takes the N channel and its stub at n carries the head from
# `top` + 8 to `top` + 10; s -> t has both endpoint lines blocked, no
# feasible S/N side, and no interior slot, so the run beyond the stack is
# all it has.
stub_head_scene <- function(top = 80) {
  list(
    name = "stub_head",
    nodes = mm_nodes(
      c("p", "s", "m1", "m6", "n", "m2", "m3", "m4", "m5", "t"),
      c(20, 20, 70, 70, 120, 120, 120, 120, 120, 170),
      c(top, 60, 60, top, top, 60, 44, 28, 11, 60)
    ),
    edges = mm_edges(c("p", "s"), c("n", "t")),
    bounds = c(0, 0, 190, 110)
  )
}

test_that("orthogonal channels: a run beyond a stack is pushed past the head of the stub standing there", {
  scene <- stub_head_scene()
  res <- ortho(scene)
  pn <- edge_index(scene, "p->n")
  st <- edge_index(scene, "s->t")

  # p -> n is placed first and is untouched by the rule
  expect_equal(res$meta$n_waypoints[[pn]], 2L)
  expect_equal(res$meta$resect_head[[pn]], 8, tolerance = 1e-9)
  expect_equal(res$waypoints[[pn]]$y, rep(96.1, 2), tolerance = 1e-9)
  expect_equal(tilted_heads(res), 0L)

  # s -> t's run is pushed from the head's centre at 89 to the far edge of
  # the zone, 11.8 mm past n, and keeps its clearance there
  expect_equal(
    channel_run(res$paths[[st]], 95)$coord,
    80 + cap_default + head_default + head_margin_default,
    tolerance = 1e-9
  )
  expect_equal(res$waypoints[[st]]$x, c(45, 45, 145, 145), tolerance = 1e-9)
  expect_equal(
    res$waypoints[[st]]$y,
    c(60, 91.8, 91.8, 60),
    tolerance = 1e-9
  )
  expect_true(res$meta$clearance_ok[[st]])
  expect_equal(nrow(head_crossings(scene, res)), 0L)
})

test_that("orthogonal channels: the pushed run follows the head", {
  # The push is continuous in the geometry it reads: as the head node rises
  # the pushed run rises with it, millimetre for millimetre, and never
  # jumps. The candidate set's own discontinuity is unchanged: once the gap
  # between the head node and the node below it opens to sep_e the interior
  # slot at 69 becomes feasible and wins, at the same width as it always
  # did.
  tops <- seq(74, 81.5, by = 0.1)
  runs <- vapply(
    tops,
    function(top) {
      scene <- stub_head_scene(top)
      channel_run(ortho(scene)$paths[[edge_index(scene, "s->t")]], 95)$coord
    },
    numeric(1)
  )
  expect_equal(
    runs,
    tops + cap_default + head_default + head_margin_default,
    tolerance = 1e-9
  )
  expect_lt(max(abs(diff(runs))), 0.1 + 1e-9)

  scene <- stub_head_scene(82)
  expect_equal(
    channel_run(ortho(scene)$paths[[edge_index(scene, "s->t")]], 95)$coord,
    69,
    tolerance = 1e-9
  )
})

# Two north-south channels one behind the other. a1 -> t takes an N channel
# whose stub carries its arrowhead from 8 to 10 mm above t, so its zone runs
# from 66.2 to 71.8 over t's x. p -> t2 crosses t's layer and its own N
# channel is priced at the top of that stack, t's centre plus R = 9: the
# middle of the head standing there. The zone is a rule about runs, not
# about the kind of channel the run belongs to, so this one is pushed to the
# zone's far edge like any other.
sn_through_head_scene <- function() {
  list(
    name = "sn_through_head",
    nodes = mm_nodes(
      c("a1", "p", "t", "f1", "q", "t2"),
      c(20, 70, 120, 120, 170, 220),
      c(58, 52, 60, 8, 40, 52)
    ),
    edges = mm_edges(c("a1", "p"), c("t", "t2")),
    bounds = c(0, 0, 240, 110)
  )
}

test_that("orthogonal heads: an S/N run is pushed past the head zone it crosses", {
  scene <- sn_through_head_scene()
  res <- ortho(scene)
  a1t <- edge_index(scene, "a1->t")
  pt2 <- edge_index(scene, "p->t2")
  t_xy <- node_xy(scene, "t")

  # a1 -> t is placed first and keeps its N channel at t's y plus the
  # nominal stub, the highest of the three terms its run is the maximum of
  expect_equal(res$meta$n_waypoints[[a1t]], 2L)
  expect_equal(res$meta$resect_head[[a1t]], 8, tolerance = 1e-9)
  expect_equal(res$waypoints[[a1t]]$x, c(20, 120), tolerance = 1e-9)
  expect_equal(res$waypoints[[a1t]]$y, rep(76.1, 2), tolerance = 1e-9)

  # p -> t2 keeps its own N channel, and its run leaves a1 -> t's head
  # whole: pushed from 69, t's centre plus R, to the far edge of the zone
  expect_equal(res$meta$n_waypoints[[pt2]], 2L)
  expect_equal(res$waypoints[[pt2]]$x, c(70, 220), tolerance = 1e-9)
  run <- channel_run(res$paths[[pt2]], t_xy[[1]])$coord
  expect_equal(
    run,
    t_xy[[2]] + cap_default + head_default + head_margin_default,
    tolerance = 1e-9
  )
  expect_equal(run, 71.8, tolerance = 1e-9)

  # the push leaves the two channels at least sep_e apart, so the restack
  # from the zone's edge moves the run no further
  expect_gte(76.1 - run, sep_e_default - 1e-9)
  expect_true(res$meta$clearance_ok[[pt2]])
  expect_equal(nrow(head_crossings(scene, res)), 0L)
  expect_null(zone_intrusions(scene, res))
})

# A scene whose one affordable line for s -> t is s's own line, which lies
# inside the head zone of p -> n's N stub. The line is fixed: a run on it is
# an endpoint run, which the push never moves, so the only thing that keeps
# s -> t off it is `price()` refusing a run inside a zone outright. Without
# that refusal s -> t slides down onto 89.2 and is drawn through p -> n's
# arrowhead; with it s -> t climbs to its N channel above the stack.
fixed_line_zone_scene <- function() {
  list(
    name = "fixed_line_zone",
    nodes = mm_nodes(
      c("p", "s", "u", "k1", "m1", "m2", "m4", "m5", "m6", "n", "m3", "t", "w"),
      c(-30, 20, 20, 20, 70, 70, 70, 70, 70, 120, 120, 170, 170),
      c(80, 89.2, 84, 8, 78, 60, 42, 24, 6, 80, 8, 91.6, 84)
    ),
    edges = mm_edges(c("p", "u", "s"), c("n", "w", "t")),
    bounds = c(-50, 0, 190, 130)
  )
}

test_that("orthogonal heads: a run priced at a fixed line inside a head zone is refused", {
  scene <- fixed_line_zone_scene()
  res <- ortho(scene)
  pn <- edge_index(scene, "p->n")
  uw <- edge_index(scene, "u->w")
  st <- edge_index(scene, "s->t")
  n_y <- node_xy(scene, "n")[[2]]

  # p -> n is placed first, at s's centre plus R, and its stub at n holds
  # the zone from 86.2 to 91.8
  expect_equal(res$meta$n_waypoints[[pn]], 2L)
  expect_equal(res$meta$resect_head[[pn]], 8, tolerance = 1e-9)
  expect_equal(channel_run(res$paths[[pn]], 95)$coord, 98.2, tolerance = 1e-9)

  # u -> w is an east-west run pushed to the zone's far edge
  expect_equal(res$meta$n_waypoints[[uw]], 4L)
  expect_equal(
    channel_run(res$paths[[uw]], 95)$coord,
    n_y + cap_default + head_default + head_margin_default,
    tolerance = 1e-9
  )

  # s -> t has no line left below, and its own source line at 89.2 sits
  # inside the zone, so it takes its N channel at t's y plus the nominal
  # stub rather than the fixed line the push could not have moved
  expect_equal(res$meta$n_waypoints[[st]], 2L)
  expect_equal(channel_run(res$paths[[st]], 95)$coord, 107.7, tolerance = 1e-9)
  expect_equal(nrow(head_crossings(scene, res)), 0L)
  expect_null(zone_intrusions(scene, res))
})

test_that("orthogonal heads: no horizontal run lies inside an S/N head zone", {
  # The census the rule is a guarantee for. Over every scene the oblique
  # census reads, in both directions, and the saturated scene at the three
  # gallery panels, no drawn horizontal run lies in the band an S/N stub's
  # arrowhead and its margin occupy. The census is worth having only if the
  # drawings put S/N stubs in it at all, so the number of zones is pinned
  # too.
  scenes <- c(oblique_census_scenes(), lapply(gallery_panels, saturated_scene))
  zones <- 0L
  found <- list()
  for (scene in scenes) {
    res <- ortho(scene)
    z <- sn_head_zones(scene, res)
    zones <- zones + if (is.null(z)) 0L else nrow(z)
    hits <- zone_intrusions(scene, res)
    if (!is.null(hits)) {
      found[[length(found) + 1L]] <- cbind(
        scene = scene$name %||% "fixture",
        panel = paste(round(scene$bounds[3:4], 2), collapse = " x "),
        hits,
        stringsAsFactors = FALSE
      )
    }
  }
  expect_equal(zones, 25L)
  intruding <- if (length(found) == 0) {
    character()
  } else {
    rows <- do.call(rbind, found)
    paste(rows$scene, rows$panel, rows$head, rows$run)
  }
  expect_equal(intruding, character())
})

test_that("orthogonal heads: the head zones move the saturated scene at 10 x 6 alone", {
  # The rule reaches a candidate only where a placed S/N stub stands in the
  # layers it crosses, which among the tracked scenes is the saturated
  # drawing at its largest panel and nothing else. Everything else is
  # pinned as the aggregate of the three routing modes: the number of
  # routed edges, their waypoints, their total drawn length and, for the
  # orthogonal channels, their total vertical travel.
  scenes <- c(forward_census_scenes(), lapply(gallery_panels, very_big_scene))
  path_length <- function(path) sum(sqrt(diff(path$x)^2 + diff(path$y)^2))
  aggregate_of <- function(mode, over) {
    routed <- 0L
    waypoints <- 0L
    drawn <- 0
    travel <- 0
    for (scene in over) {
      res <- route_scene(scene, mode = mode, opts = route_constants(r_default))
      routed <- routed + sum(res$meta$mode != "straight")
      waypoints <- waypoints + sum(res$meta$n_waypoints)
      for (i in seq_along(res$paths)) {
        drawn <- drawn + path_length(res$paths[[i]])
        if (res$meta$mode[[i]] != "orthogonal") {
          next
        }
        runs <- straight_runs(dedupe_path(res$paths[[i]]))
        if (nrow(runs) > 0) {
          travel <- travel + sum(runs$length[runs$axis == "v"])
        }
      }
    }
    list(routed = routed, waypoints = waypoints, drawn = drawn, travel = travel)
  }

  a <- aggregate_of("orthogonal", scenes)
  expect_equal(a$routed, 473L)
  expect_equal(a$waypoints, 1051L)
  expect_equal(a$drawn, 39526.869597425, tolerance = 1e-9)
  expect_equal(a$travel, 13395.630720657, tolerance = 1e-9)

  b <- aggregate_of("spline", scenes)
  expect_equal(b$routed, 113L)
  expect_equal(b$waypoints, 211L)
  expect_equal(b$drawn, 32390.876311216, tolerance = 1e-9)
  expect_equal(b$travel, 0)

  d <- aggregate_of("straight", scenes)
  expect_equal(d$routed, 0L)
  expect_equal(d$waypoints, 0L)
  expect_equal(d$drawn, 31105.225680966, tolerance = 1e-9)

  # the saturated scene panel by panel: the two smaller drawings stand, and
  # the largest gains the two bends of the pushed run while its vertical
  # travel falls, because d -> y no longer climbs to y from the bottom of
  # the panel
  sat <- lapply(gallery_panels, function(panel) {
    aggregate_of("orthogonal", list(saturated_scene(panel)))
  })
  expect_equal(
    vapply(sat, `[[`, integer(1), "waypoints"),
    c(114L, 118L, 116L)
  )
  expect_equal(
    vapply(sat, `[[`, numeric(1), "drawn"),
    c(2466.330081929, 4111.887778253, 5523.001089904),
    tolerance = 1e-9
  )
  expect_equal(
    vapply(sat, `[[`, numeric(1), "travel"),
    c(1030.544156900, 1594.796952492, 1839.496924575),
    tolerance = 1e-9
  )
})

# Spline heads, repair waypoints, and the fallback depth --------------------------

test_that("spline heads: very_big draws fewer foreign shafts on heads at 10 x 6", {
  # The head census read in spline mode, where the resect is the cap at
  # every end. Treating a head as a soft obstacle removes the chord and
  # nudge crossings at 10 x 6; the 4 x 3 and 7 x 5 counts stand where the
  # detours leave them, since an edge with a hard disc hit never sees a
  # head.
  counts <- vapply(
    gallery_panels,
    function(panel) {
      scene <- very_big_scene(panel)
      res <- route_scene(
        scene,
        mode = "spline",
        opts = route_constants(r_default)
      )
      nrow(head_crossings(scene, res, resect = cap_default))
    },
    integer(1)
  )
  expect_equal(counts, c(33L, 13L, 11L))
})

# Two soft nudges too close together along the chord cannot both be
# honoured, so merge_head_hits() keeps one of them: a head hit is merged
# with a disc hit within that disc's clearance R = 9 mm of it along the
# chord, and with another head hit within twice its own clearance,
# 2 * 2.8 = 5.6 mm. Where the two nudge to a common side the deeper nudge
# is kept; where they nudge to opposite sides the head hit gives way,
# whichever of the two comes first along the chord: to a disc hit always,
# and of two head hits the earlier to the later.

merge_chord <- function() {
  edge_frame(mm_nodes(c("a", "b"), c(20, 140), c(50, 50)), 1L, 2L, FALSE)
}

# The drawn head of an edge arriving vertically into (x, y): its zone runs
# from 2 cap to cap before the target, and head_hits() reads its centre a
# further head / 2 along, cap + head / 2 = 9 mm before the target.
merge_head <- function(x, y, from = c("above", "below")) {
  uy <- if (match.arg(from) == "above") -1 else 1
  data.frame(
    x = x,
    y = y - 2 * cap_default * uy,
    x2 = x,
    y2 = y - cap_default * uy,
    ux = 0,
    uy = uy
  )
}

# A grazed node at (x, y) as the router reads it into the hit frame.
merge_disc <- function(fr, x, y) {
  d <- c(x, y) - fr$S
  data.frame(
    node = 1L,
    h = sum(d * fr$n),
    t = sum(d * fr$u) / fr$Lc,
    layer = NA_integer_,
    r = r_default,
    hard = FALSE,
    mm = m_default,
    side = NA_real_
  )
}

# The hits of the chord a (20, 50) -> b (140, 50), ordered along it and
# merged, with the chord position of each surviving hit as `x`.
merge_hits <- function(heads, discs = list()) {
  fr <- merge_chord()
  eh <- head_hits(
    fr,
    do.call(rbind, heads),
    cap_default,
    route_constants(r_default)
  )
  for (d in discs) {
    eh <- rbind(merge_disc(fr, d[[1]], d[[2]]), eh)
  }
  eh <- eh[order(eh$t), , drop = FALSE]
  kept <- merge_head_hits(eh, fr$Lc)
  kept$x <- fr$S[[1]] + kept$t * fr$Lc
  kept
}

test_that("merge_head_hits: a head gives way to a disc, and the earlier head to the later", {
  # A grazed node at (80, 58) nudges the chord down, and the head of an
  # edge arriving from above into a node at y = 40.5 has its centre at
  # y = 49.5 and nudges it up. The two nudge to opposite sides, so within
  # the disc's 9 mm along the chord the head is dropped, whether it falls
  # before or after the disc.
  disc <- list(c(80, 58))
  after <- merge_hits(list(merge_head(88, 40.5)), disc)
  before <- merge_hits(list(merge_head(72, 40.5)), disc)

  expect_equal(nrow(after), 1L)
  expect_equal(after$x, 80)
  expect_equal(nrow(before), 1L)
  expect_equal(before$x, 80)

  # 10 mm apart is beyond the disc's reach and both nudges are drawn, the
  # head's up and away from its target, the disc's down
  pair <- merge_hits(list(merge_head(90, 40.5)), disc)
  sides <- ifelse(is.na(pair$side), -sign(pair$h), pair$side)
  expect_equal(nrow(pair), 2L)
  expect_equal(sides[is.na(pair$node)], 1)
  expect_equal(sides[!is.na(pair$node)], -1)

  # Two heads on opposite sides 4 mm apart: the later along the chord is
  # kept, whichever side each nudges to and whichever order they are read
  # in. At 6 mm, beyond twice the head clearance, both are kept.
  up_then_down <- merge_hits(list(
    merge_head(80, 40.5),
    merge_head(84, 59.5, "below")
  ))
  read_in_reverse <- merge_hits(list(
    merge_head(84, 59.5, "below"),
    merge_head(80, 40.5)
  ))
  down_then_up <- merge_hits(list(
    merge_head(80, 59.5, "below"),
    merge_head(84, 40.5)
  ))
  apart <- merge_hits(list(merge_head(80, 40.5), merge_head(86, 59.5, "below")))

  expect_equal(nrow(up_then_down), 1L)
  expect_equal(up_then_down$x, 84)
  expect_equal(nrow(read_in_reverse), 1L)
  expect_equal(read_in_reverse$x, 84)
  expect_equal(nrow(down_then_up), 1L)
  expect_equal(down_then_up$x, 84)
  expect_equal(nrow(apart), 2L)
  expect_equal(apart$x, c(80, 86))
})

# A repair waypoint is inserted at the chord parameter of the violating
# sample, which is not a layer position. Reporting it as layer 0 puts it on
# a layer no scene has. The one obstacle that belongs to no layer is
# another edge's arrowhead capsule, so a repair past one reports no layer
# for the waypoint it inserts, in the spanning tier as in the free tier,
# while the waypoints a spanning route is laid out with keep the layer each
# is drawn on.

test_that("no routed edge reports a waypoint on a layer the scene does not have", {
  scenes <- c(forward_census_scenes(), lapply(gallery_panels, very_big_scene))
  reported <- 0L
  for (scene in scenes) {
    res <- route_scene(
      scene,
      mode = "spline",
      opts = route_constants(r_default)
    )
    labels <- edge_labels(scene$edges)
    size <- paste(round(scene$bounds[3:4], 1), collapse = " x ")
    for (i in which(res$meta$routed)) {
      layer_ids <- res$meta$waypoint_layers[[i]]
      label <- paste(scene$name %||% "fixture", labels[[i]], "at", size)
      expect_false(any(layer_ids == 0, na.rm = TRUE), label = label)
      reported <- reported + sum(!is.na(layer_ids))
    }
  }
  # Every layer a routed edge reports is one the scene has: a waypoint laid
  # out on a crossed layer reports that layer, a repair off the layers
  # reports none, and no waypoint reports the layer 0 no scene has. The
  # census is worth having only if the routes report layers at all: 194 of
  # these waypoints name one.
  expect_gt(reported, 150L)
})

test_that("a spanning route's capsule repair reports no layer", {
  # very_big at 7 x 5: education -> healthcare_access is an interior route
  # whose repair past an arrowhead capsule inserts a waypoint between the
  # layers at 28.930 and 43.396. That waypoint reports no layer; the
  # waypoint the route was laid out with keeps the crossed layer it is
  # drawn on.
  scene <- very_big_scene(gallery_panels[[2]])
  res <- route_scene(scene, mode = "spline", opts = route_constants(r_default))
  i <- edge_index(scene, "education->healthcare_access")
  layers <- infer_layers(scene$nodes, r_default)
  layer_of <- stats::setNames(layers$id, scene$nodes$name)
  ends <- c(layer_of[["education"]], layer_of[["healthcare_access"]])
  crossed <- seq(min(ends) + 1L, max(ends) - 1L)
  layer_ids <- res$meta$waypoint_layers[[i]]
  wp <- res$waypoints[[i]]
  repaired <- which(is.na(layer_ids))
  laid <- which(!is.na(layer_ids))

  expect_equal(res$meta$mode[i], "interior")
  expect_length(repaired, 1L)
  expect_true(all(layer_ids[laid] %in% crossed))
  expect_equal(wp$x[laid], layers$x[layer_ids[laid]], tolerance = 1e-6)
  # the repair sits in the gap between two crossed layers, on neither
  expect_gt(wp$x[[repaired]], layers$x[[2]])
  expect_lt(wp$x[[repaired]], layers$x[[3]])
  expect_gt(min(abs(wp$x[[repaired]] - layers$x)), 1)
})

test_that("a free bow's repair waypoint reports no layer", {
  # very_big at 7 x 5: education -> occupation is a free bow whose repair
  # sits between layers and belongs to neither.
  scene <- very_big_scene(gallery_panels[[2]])
  res <- route_scene(scene, mode = "spline", opts = route_constants(r_default))
  i <- edge_index(scene, "education->occupation")

  expect_equal(res$meta$mode[i], "bow")
  expect_true(anyNA(res$meta$waypoint_layers[[i]]))
})

test_that("an unverified fallback is chosen by its summed disc and capsule depth", {
  # When no attempt clears every obstacle, the curve kept is the shallowest
  # by total violation depth, a disc cut and an arrowhead-zone intrusion
  # counted alike. Ranking the discs alone instead sends these three edges
  # to other curves and leaves far more shafts drawn across other edges'
  # arrowheads. very_big at 4 x 3 is where the two orderings part; all three
  # edges stay unverified either way, and it is which curve they settle on
  # that moves.
  scene <- very_big_scene(gallery_panels[[1]])
  res <- route_scene(scene, mode = "spline", opts = route_constants(r_default))
  reading <- function(label) {
    i <- edge_index(scene, label)
    list(
      mode = res$meta$mode[i],
      side = res$meta$side[i],
      ok = res$meta$clearance_ok[i],
      sagitta = res$meta$sagitta_ratio[i]
    )
  }

  smoking <- reading("adversity->smoking")
  expect_equal(smoking$mode, "bow")
  expect_equal(smoking$side, -1)
  expect_false(smoking$ok)
  expect_equal(smoking$sagitta, 0.4756, tolerance = 1e-4)

  inflammation <- reading("bmi->inflammation")
  expect_equal(inflammation$mode, "interior")
  expect_equal(inflammation$side, 1)
  expect_false(inflammation$ok)
  expect_equal(inflammation$sagitta, 0.5994, tolerance = 1e-4)

  alcohol <- reading("stress->alcohol")
  expect_equal(alcohol$mode, "interior")
  expect_equal(alcohol$side, -1)
  expect_false(alcohol$ok)
  expect_equal(alcohol$sagitta, 0.4295, tolerance = 1e-4)
})


# The gallery's bow-first policy scene: ten nodes on a 12-column grid, two
# rows of five above and below a source and a target on the centre line,
# joined by fourteen edges. Its two long skips over a node of their own row,
# c -> f and e -> h, are the deepest arches the spanning tier draws at the
# small device size.
policy_bow_first_scene <- function(panel) {
  list(
    name = "policy_bow_first",
    nodes = mm_nodes(
      c("a", "b", "c", "d", "e", "f", "g", "h", "x", "y"),
      c(1, 3, 3, 5, 5, 7, 7, 9, 9, 11) / 12 * panel[1],
      c(6, 1, 11, 1, 11, 11, 1, 11, 1, 6) / 12 * panel[2]
    ),
    edges = mm_edges(
      c("a", "a", "b", "b", "c", "c", "d", "e", "e", "f", "g", "g", "h", "x"),
      c("b", "c", "d", "e", "e", "f", "g", "g", "h", "h", "x", "y", "y", "y")
    ),
    bounds = c(0, 0, panel)
  )
}

test_that("policy_bow_first: the two arches at 4 x 3 keep the depth the repair gives them", {
  # Heads count as obstacles only for an edge that clears every disc, so
  # neither arch moves: both cut a disc, both are repaired along the head
  # axis as before, and both stay verified interior routes on the side away
  # from the node they skip.
  scene <- policy_bow_first_scene(gallery_panels[[1]])
  res <- route_scene(scene, mode = "spline", opts = route_constants(r_default))
  routed <- edge_labels(scene$edges)[res$meta$routed]
  expect_setequal(routed, c("c->f", "e->h"))

  for (label in c("c->f", "e->h")) {
    i <- edge_index(scene, label)
    expect_equal(res$meta$mode[i], "interior", label = label)
    expect_equal(res$meta$side[i], -1, label = label)
    expect_true(res$meta$clearance_ok[i], label = label)
    expect_false(res$meta$sagitta_capped[i], label = label)
  }
  expect_equal(
    res$meta$sagitta_ratio[edge_index(scene, "c->f")],
    0.9582,
    tolerance = 1e-4
  )
  expect_equal(
    res$meta$sagitta_ratio[edge_index(scene, "e->h")],
    0.8812,
    tolerance = 1e-4
  )
})


# Channel packing --------------------------------------------------------------

# Four layers at x = 20, 70, 120, 170 on a 190 x 110 mm panel; each 50 mm gap
# holds two ranks on the ladder's first rung. m -> n has both its endpoint
# lines blocked (p on 30 at 8 mm, p2 on 80 at 2 mm), so it runs through layer
# 2's free interval at its chord's median. s -> t has its source line 55
# blocked by q (7 mm) and n3 (5 mm) and its target line 50 clear of every
# disc, so the only thing between it and that line is m -> n's placed run.
# s0 sits above s, m below s, n3 below n and t_top above t, which closes
# every S/N side: neither edge has a channel past a stack to fall back on.
crowded_line_scene <- function() {
  list(
    nodes = mm_nodes(
      c("s", "m", "s0", "p", "p2", "n3", "n", "t", "t_top", "q"),
      c(20, 20, 20, 70, 70, 120, 120, 170, 170, 70),
      c(55, 30, 80, 22, 78, 60, 80, 50, 80, 62)
    ),
    edges = mm_edges(c("m", "s"), c("n", "t")),
    bounds = c(0, 0, 190, 110)
  )
}

with_node <- function(scene, name, x, y) {
  scene$nodes <- rbind(scene$nodes, mm_nodes(name, x, y))
  scene
}

# The longest run of a path along one axis, as one row of `straight_runs()`.
longest_run <- function(path, axis = "h") {
  runs <- straight_runs(dedupe_path(path))
  runs <- runs[runs$axis == axis, , drop = FALSE]
  expect_gt(nrow(runs), 0)
  runs[which.max(runs$length), , drop = FALSE]
}

test_that("orthogonal channels: a placed channel pushes a candidate on the other side", {
  # cascade's a -> f and b -> e have the same chord median, so both want the
  # same line and the second placed is stacked one separation off it. The
  # stacking rule reads every placed channel whose x-range overlaps, on
  # either side, which is what keeps a -> f off the line b -> e holds: were
  # only same-side channels counted, a -> f's candidate below the line would
  # sit on 55, undisplaced and cheaper than the 58.6 above.
  for (panel in list(c(160, 110), gallery_panels[[2]])) {
    scene <- canonical_scene("cascade", panel)
    res <- ortho(scene)
    label <- paste0(round(panel[[1]], 2), " mm panel: ")
    expect_orthogonal_scene(scene, res, stub_always = TRUE, prefix = label)

    af <- edge_index(scene, "a->f")
    be <- edge_index(scene, "b->e")
    run_af <- longest_run(res$paths[[af]])
    run_be <- longest_run(res$paths[[be]])
    # the runs cross the same gaps, so the two channels are in conflict
    expect_gt(min(run_af$hi, run_be$hi) - max(run_af$lo, run_be$lo), 0)
    expect_equal(
      run_af$coord - run_be$coord,
      sep_e_default,
      tolerance = 1e-6,
      label = label
    )
    expect_equal(
      shared_run_length(res$paths[[af]], res$paths[[be]]),
      0,
      label = label
    )
  }

  # the two lines at each size, so that a channel drawn on the other's line
  # is named rather than only measured as a difference
  expect_equal(
    vapply(
      c("a->f", "b->e"),
      function(lab) {
        scene <- canonical_scene("cascade", c(160, 110))
        longest_run(ortho(scene)$paths[[edge_index(scene, lab)]])$coord
      },
      numeric(1)
    ),
    c("a->f" = 58.6, "b->e" = 55),
    tolerance = 1e-6
  )
  expect_equal(
    vapply(
      c("a->f", "b->e"),
      function(lab) {
        scene <- canonical_scene("cascade", gallery_panels[[2]])
        longest_run(ortho(scene)$paths[[edge_index(scene, lab)]])$coord
      },
      numeric(1)
    ),
    c("a->f" = 64.991241, "b->e" = 61.391241),
    tolerance = 1e-6
  )
})

test_that("orthogonal channels: very_big keeps its pushed channels off the discs at 10 x 6", {
  # The two channels of the gallery's largest scene that a skipped disc
  # check moves: both run past the layers they cross at least R = 9 from
  # every disc whose x their run spans. The census is deliberately narrow.
  # Not every channel of the scene keeps R today: measured the same way,
  # alcohol -> bp's run at 86.876 comes within 7.94 mm of diabetes, so a
  # blanket sweep would be false.
  scene <- very_big_scene(gallery_panels[[3]])
  res <- ortho(scene)
  lines <- c("phys_act->cvd" = 114.814880, "education->smoking" = 23.251112)

  for (label in names(lines)) {
    i <- edge_index(scene, label)
    run <- longest_run(res$paths[[i]])
    expect_equal(run$coord, lines[[label]], tolerance = 1e-6, label = label)

    ends <- c(scene$edges$from[[i]], scene$edges$to[[i]])
    crossed <- scene$nodes$x >= run$lo - 1e-9 &
      scene$nodes$x <= run$hi + 1e-9 &
      !scene$nodes$name %in% ends
    expect_gt(sum(crossed), 0L)
    expect_gte(
      min(abs(run$coord - scene$nodes$y[crossed])),
      r_full - 1e-6,
      label = label
    )
  }
})

test_that("orthogonal packing: a crowded target line slides the interior run away", {
  # s -> t is placed second, being the longer span, and finds its target's
  # line 50 clear of every disc, of the panel margin and of the pieces
  # already committed, and crowded only by m -> n's interior run at 53. The
  # run on that line is worth more than the price of moving m -> n out of the
  # way, so m -> n slides one sep_e below the line, to 46.4, and s -> t is
  # drawn as the two-bend run into t's centre. At HEAD nothing moves and the
  # stacking rule pushes s -> t down to 49.4, which costs it a second pair of
  # bends and a 0.6 mm jog into the target.
  scene <- crowded_line_scene()
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  st <- edge_index(scene, "s->t")
  expect_equal(res$meta$mode[st], "orthogonal")
  expect_true(res$meta$clearance_ok[st])
  expect_equal(res$meta$n_waypoints[st], 2)
  wp <- res$waypoints[[st]]
  expect_equal(wp$x, c(47.966667, 47.966667), tolerance = 1e-6)
  expect_equal(wp$y, c(55, 50), tolerance = 1e-6)
  # the whole span from the first slot to t is one run on t's own line, and
  # the head sits on it at the plain cap
  run <- longest_run(res$paths[[st]])
  expect_equal(run$coord, 50, tolerance = 1e-6)
  expect_equal(c(run$lo, run$hi), c(50.066667, 170), tolerance = 1e-6)
  expect_equal(run$length, 119.933333, tolerance = 1e-6)
  expect_equal(res$meta$resect_head[st], cap_default)

  mn <- edge_index(scene, "m->n")
  expect_equal(res$meta$mode[mn], "orthogonal")
  expect_true(res$meta$clearance_ok[mn])
  expect_equal(res$meta$n_waypoints[mn], 4)
  wp <- res$waypoints[[mn]]
  expect_equal(wp$x, c(42.033333, 42.033333, 95, 95), tolerance = 1e-6)
  expect_equal(wp$y, c(30, 46.4, 46.4, 80), tolerance = 1e-6)
  # the slid run sits exactly one separation below the line it made room for
  expect_equal(
    longest_run(res$paths[[mn]])$coord,
    50 - sep_e_default,
    tolerance = 1e-6
  )
})

test_that("orthogonal packing: the slide is refused when the crowding run cannot clear a disc either way", {
  # w at (70, 40) leaves m -> n nowhere to go: 46.4 is 6.4 mm from w and 53.6
  # is 8.4 mm from q, both inside R = 9. With no feasible direction the slide
  # is refused and every path is the one HEAD draws, s -> t pushed to 49.4
  # with four waypoints and m -> n on its median at 53.
  scene <- with_node(crowded_line_scene(), "w", 70, 40)
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  st <- edge_index(scene, "s->t")
  expect_equal(res$meta$n_waypoints[st], 4)
  wp <- res$waypoints[[st]]
  expect_equal(wp$x, c(47.966667, 47.966667, 145, 145), tolerance = 1e-6)
  expect_equal(wp$y, c(55, 49.4, 49.4, 50), tolerance = 1e-6)
  expect_equal(longest_run(res$paths[[st]])$coord, 49.4, tolerance = 1e-6)

  mn <- edge_index(scene, "m->n")
  expect_equal(res$meta$n_waypoints[mn], 4)
  expect_equal(longest_run(res$paths[[mn]])$coord, 53, tolerance = 1e-6)
})

test_that("orthogonal packing: ties go above", {
  # m -> n's chord median is exactly t's line 50, so sliding it up to 53.6 and
  # sliding it down to 46.4 change its displacement by the same amount and
  # neither gains a bend. The tie is broken the way every side tie in the
  # router is: above.
  scene <- list(
    nodes = mm_nodes(
      c("s", "m", "s0", "m0", "p", "q", "p2", "n", "t", "t_top"),
      c(20, 20, 20, 20, 70, 70, 70, 120, 170, 170),
      c(55, 30, 80, 12, 22, 63.5, 78, 70, 50, 80)
    ),
    edges = mm_edges(c("m", "s"), c("n", "t")),
    bounds = c(0, 0, 190, 110)
  )
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  mn <- edge_index(scene, "m->n")
  wp <- res$waypoints[[mn]]
  expect_equal(wp$x, c(42.033333, 42.033333, 95, 95), tolerance = 1e-6)
  expect_equal(wp$y, c(30, 53.6, 53.6, 70), tolerance = 1e-6)
  expect_equal(
    longest_run(res$paths[[mn]])$coord,
    50 + sep_e_default,
    tolerance = 1e-6
  )

  st <- edge_index(scene, "s->t")
  expect_equal(res$meta$n_waypoints[st], 2)
  wp <- res$waypoints[[st]]
  expect_equal(wp$x, c(47.966667, 47.966667), tolerance = 1e-6)
  expect_equal(wp$y, c(55, 50), tolerance = 1e-6)
})

test_that("orthogonal packing: the slide does not depend on floating noise in the crowding run", {
  # The tie scene with every y moved up by 0.1 mm. Nothing has moved relative
  # to anything else, so the picture is the one the unshifted scene draws.
  # The shift does change the arithmetic: m -> n's chord ordinate at layer 2
  # lands 7e-15 mm off t's line rather than on it, which puts m -> n's
  # vertical leg across the line the option would run on. The option is
  # priced with the channels the slide moves out of the crossing count, so
  # that leg is not there to be counted and the noise cannot reach the price.
  shift <- 0.1
  scene <- list(
    nodes = mm_nodes(
      c("s", "m", "s0", "m0", "p", "q", "p2", "n", "t", "t_top"),
      c(20, 20, 20, 20, 70, 70, 70, 120, 170, 170),
      c(55, 30, 80, 12, 22, 63.5, 78, 70, 50, 80) + shift
    ),
    edges = mm_edges(c("m", "s"), c("n", "t")),
    bounds = c(0, 0, 190, 110)
  )
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  mn <- edge_index(scene, "m->n")
  wp <- res$waypoints[[mn]]
  expect_equal(wp$x, c(42.033333, 42.033333, 95, 95), tolerance = 1e-6)
  expect_equal(
    wp$y,
    c(30, 53.6, 53.6, 70) + shift,
    tolerance = 1e-6
  )
  expect_equal(
    longest_run(res$paths[[mn]])$coord,
    50 + shift + sep_e_default,
    tolerance = 1e-6
  )

  st <- edge_index(scene, "s->t")
  expect_equal(res$meta$n_waypoints[st], 2)
  wp <- res$waypoints[[st]]
  expect_equal(wp$x, c(47.966667, 47.966667), tolerance = 1e-6)
  expect_equal(wp$y, c(55, 50) + shift, tolerance = 1e-6)
})

test_that("orthogonal packing: a sibling on the source's line never moves", {
  # s -> t2 is placed first and runs on s's own line 55; s -> v's source line
  # is then crowded by it, and v's line 75 is blocked, so s -> v takes the
  # interior run at 65. Its sibling is both a channel of its own hyperedge
  # and a run on an endpoint line, either of which makes it immovable, so no
  # slide is even priced and the picture is HEAD's.
  scene <- list(
    nodes = mm_nodes(
      c("s", "s0", "p2", "n", "v", "t2"),
      c(20, 20, 70, 120, 170, 170),
      c(55, 80, 78, 80, 75, 52)
    ),
    edges = mm_edges(c("s", "s"), c("v", "t2")),
    bounds = c(0, 0, 190, 110)
  )
  res <- ortho(scene)
  expect_orthogonal_scene(scene, res, stub_always = TRUE)

  t2 <- edge_index(scene, "s->t2")
  wp <- res$waypoints[[t2]]
  expect_equal(wp$x, c(145, 145), tolerance = 1e-6)
  expect_equal(wp$y, c(55, 52), tolerance = 1e-6)

  v <- edge_index(scene, "s->v")
  wp <- res$waypoints[[v]]
  expect_equal(wp$x, c(45, 45, 145, 145), tolerance = 1e-6)
  expect_equal(wp$y, c(55, 65, 65, 75), tolerance = 1e-6)
  expect_equal(longest_run(res$paths[[v]])$coord, 65, tolerance = 1e-6)
})

# One record of the placement loop, in the shape `slide_channels()` reads:
# m -> n of `crowded_line_scene()` as it stands when s -> t is priced, on
# the interior line `y`, spanning layers 1 to 3 with the discs of layer 2
# crossed and its chord's y there at 55.
crowded_record <- function(y = 53, kind = "ew", keys = c("s2", "e2-7")) {
  d <- c(120, 80) - c(20, 30)
  u <- d / sqrt(sum(d^2))
  list(
    e = 1L,
    kind = kind,
    side = -1,
    y = y,
    xr = c(36.1, 103.9),
    la = 1L,
    lb = 3L,
    keys = keys,
    Sy = 30,
    Ty = 80,
    fr = list(
      S = c(20, 30),
      E = c(120, 80),
      u = u,
      n = c(-u[[2]], u[[1]]),
      Lc = sqrt(sum(d^2)),
      a = 2L,
      b = 7L
    ),
    state = list(
      members = c(4L, 5L, 10L),
      yc = 55,
      xr_ew = c(36.1, 103.9),
      y_min = 3,
      y_max = 107,
      extra = 0
    )
  )
}

test_that("slide_channels() refuses an immovable channel", {
  # The three kinds of channel that never move, on the records of the crowded
  # scene: an S/N channel, whose y is tied to its stubs; a channel of the
  # candidate's own hyperedge; and a channel already running on its own
  # source's or target's line, which is the shape this pass exists to create
  # and is never taken from another edge. Each makes the direction
  # infeasible, so the helper returns NULL rather than a cheaper packing.
  has_slide <- exists(
    "slide_channels",
    envir = asNamespace("ggdag"),
    inherits = FALSE
  )
  expect_true(has_slide, label = "slide_channels() is defined in the package")

  if (has_slide) {
    scene <- crowded_line_scene()
    opts <- route_constants(r_default)
    empty <- data.frame(
      key = character(0),
      left = numeric(0),
      right = numeric(0),
      stringsAsFactors = FALSE
    )
    slide <- function(records, dir, cand_key = "s1") {
      slide_channels(
        records = records,
        conflicts = 1L,
        y0 = 50,
        dir = dir,
        sep_e = opts$sep_e,
        nodes = scene$nodes,
        R_node = scene$nodes$r + opts$m,
        pieces_base = list(empty, empty, empty),
        cand_pieces = list(
          list(g = 1L, key = "s1", left = 55, right = 50),
          list(g = 3L, key = "e1-8", left = 50, right = 50)
        ),
        opts = opts,
        cand_key = cand_key,
        gap_mid = c(45, 95, 145)
      )
    }

    expect_null(slide(list(crowded_record(kind = "sn")), -1))
    expect_null(slide(list(crowded_record()), -1, cand_key = "s2"))
    expect_null(slide(list(crowded_record(y = 30)), -1))
    expect_null(slide(list(crowded_record(y = 80)), -1))

    # the crowded scene itself: below the line the one record moves to 46.4
    # for 1.1, and above it q leaves nowhere to go
    down <- slide(list(crowded_record()), -1)
    expect_equal(down$y, 46.4, tolerance = 1e-6)
    expect_equal(down$moved, 1L)
    expect_equal(down$cost, 1.1, tolerance = 1e-6)
    expect_null(slide(list(crowded_record()), 1))
  }
})

test_that("slide_channels() prices the bends a moved channel sheds", {
  # A record whose own target's line is exactly one separation above the
  # crowded line 50: the slide puts its run on that line, where the run and
  # the leg into the target are collinear and two of its four bends are gone.
  # The price is the change in displacement, 2.6 mm over the reference
  # radius, less `bend_penalty` for each bend shed, so the move is worth
  # less than nothing. Without the bend term the same move prices at
  # +0.433 and the shape the pass exists to create is never preferred.
  opts <- route_constants(r_default)
  nodes <- mm_nodes(c("a", "b"), c(70, 70), c(5, 105))
  empty <- data.frame(
    key = character(0),
    left = numeric(0),
    right = numeric(0),
    stringsAsFactors = FALSE
  )
  Ty <- 50 + opts$sep_e
  d <- c(120, Ty) - c(20, 30)
  u <- d / sqrt(sum(d^2))
  # the chord's ordinate at the one crossed layer, midway between its ends
  yc <- 30 + (Ty - 30) / 2
  record <- list(
    e = 1L,
    kind = "ew",
    side = 1,
    y = 51,
    xr = c(36, 104),
    la = 1L,
    lb = 3L,
    keys = c("s2", "e2-7"),
    Sy = 30,
    Ty = Ty,
    fr = list(
      S = c(20, 30),
      E = c(120, Ty),
      u = u,
      n = c(-u[[2]], u[[1]]),
      Lc = sqrt(sum(d^2)),
      a = 2L,
      b = 7L
    ),
    state = list(
      members = c(1L, 2L),
      yc = yc,
      xr_ew = c(36, 104),
      y_min = 3,
      y_max = 107,
      extra = 0
    )
  )

  up <- slide_channels(
    records = list(record),
    conflicts = 1L,
    y0 = 50,
    dir = 1,
    sep_e = opts$sep_e,
    nodes = nodes,
    R_node = nodes$r + opts$m,
    pieces_base = list(empty, empty),
    cand_pieces = list(list(g = 1L, key = "s1", left = 55, right = 50)),
    opts = opts,
    cand_key = "s1",
    gap_mid = c(45, 95)
  )
  expect_equal(up$y, Ty, tolerance = 1e-6)
  expect_equal(up$moved, 1L)
  expect_equal(
    up$cost,
    (abs(Ty - yc) - abs(51 - yc)) / r_default - 2 * opts$bend_penalty,
    tolerance = 1e-6
  )
  expect_equal(up$cost, 2.6 / 6 - 4, tolerance = 1e-6)
})

# A record of the placement loop whose target's centre row a level chord
# owns, on the line `y` and spanning layers 1 to 3 with one crossed layer.
owned_record <- function(y, Ty, owned, keys = c("s2", "e2-7")) {
  d <- c(120, Ty) - c(20, 30)
  u <- d / sqrt(sum(d^2))
  list(
    e = 1L,
    kind = "ew",
    side = 1,
    y = y,
    xr = c(36, 104),
    la = 1L,
    lb = 3L,
    keys = keys,
    Sy = 30,
    Ty = Ty,
    owned = owned,
    fr = list(
      S = c(20, 30),
      E = c(120, Ty),
      u = u,
      n = c(-u[[2]], u[[1]]),
      Lc = sqrt(sum(d^2)),
      a = 2L,
      b = 7L
    ),
    state = list(
      members = c(1L, 2L),
      yc = 30 + (Ty - 30) / 2,
      xr_ew = c(36, 104),
      y_min = 3,
      y_max = 107,
      extra = 0
    )
  )
}

test_that("slide_channels() never moves a channel onto its own owned line", {
  # The lattice a slide moves on is y0 + k sep_e, and a record's own
  # target's line can sit on it: the move that shed two bends in the test
  # above puts the run exactly on the target's line. Where a level chord
  # already arrives on that line the target's centre row has an owner, and
  # no run may take it, so the direction is infeasible instead. The same
  # holds for a channel moved by the cascade rather than by the conflict.
  opts <- route_constants(r_default)
  nodes <- mm_nodes(c("a", "b"), c(70, 70), c(5, 105))
  empty <- data.frame(
    key = character(0),
    left = numeric(0),
    right = numeric(0),
    stringsAsFactors = FALSE
  )
  slide <- function(records) {
    slide_channels(
      records = records,
      conflicts = 1L,
      y0 = 50,
      dir = 1,
      sep_e = opts$sep_e,
      nodes = nodes,
      R_node = nodes$r + opts$m,
      pieces_base = list(empty, empty),
      cand_pieces = list(list(g = 1L, key = "s1", left = 55, right = 50)),
      opts = opts,
      cand_key = "s1",
      gap_mid = c(45, 95)
    )
  }
  own_line <- 50 + opts$sep_e

  # the conflicting channel itself: it takes its target's line when the row
  # is free and the direction is refused when a chord owns it
  free <- slide(list(owned_record(51, own_line, FALSE)))
  expect_equal(free$y, own_line, tolerance = 1e-6)
  expect_equal(free$moved, 1L)
  expect_null(slide(list(owned_record(51, own_line, TRUE))))

  # and a channel the cascade moves: the first record is pushed to 53.6,
  # which crowds the second and pushes it a further separation, onto the
  # line of its own target
  cascaded <- function(owned) {
    list(
      owned_record(51, 80, FALSE),
      owned_record(55.5, 50 + 2 * opts$sep_e, owned, keys = c("s3", "e3-9"))
    )
  }
  both <- slide(cascaded(FALSE))
  expect_equal(both$y, c(own_line, 50 + 2 * opts$sep_e), tolerance = 1e-6)
  expect_equal(both$moved, c(1L, 2L))
  expect_null(slide(cascaded(TRUE)))
})

test_that("orthogonal packing: genetics -> chol runs on chol's line at 10 x 6", {
  # The defect the round was reported on. genetics -> chol is placed last of
  # the twenty-two spanning edges and finds chol's line 31.71 blocked only by
  # bmi -> inflammation's run at 30.45; that run slides up one sep_e and
  # genetics -> chol takes the line, drawn as a 9 mm stub, a 3.4 mm drop and
  # a 136 mm run into chol's centre. At HEAD every candidate is infeasible
  # and the edge is clamped at 19.65, 15.5 mm down and 8.7 mm back up.
  scene <- very_big_scene(gallery_panels[[3]])
  res <- ortho(scene)

  gc <- edge_index(scene, "genetics->chol")
  expect_equal(res$meta$mode[gc], "orthogonal")
  expect_equal(res$meta$n_waypoints[gc], 2)
  wp <- res$waypoints[[gc]]
  expect_equal(wp$x, c(50.645621, 50.645621), tolerance = 1e-6)
  expect_equal(wp$y, c(36.72236, 31.711827), tolerance = 1e-6)
  # the run is on chol's own line, so the edge owns its target's centre row
  # and the head is drawn at the plain cap
  run <- longest_run(res$paths[[gc]])
  expect_equal(run$coord, node_xy(scene, "chol")[[2]], tolerance = 1e-6)
  expect_equal(run$coord, 31.711827, tolerance = 1e-6)
  expect_equal(c(run$lo, run$hi), c(51.445621, 187.336862), tolerance = 1e-6)
  expect_equal(res$meta$resect_head[gc], cap_default)

  bi <- edge_index(scene, "bmi->inflammation")
  wp <- res$waypoints[[bi]]
  expect_equal(
    wp$x,
    c(133.906448, 133.906448, 174.036862, 174.036862),
    tolerance = 1e-6
  )
  expect_equal(
    wp$y,
    c(42.120806, 35.311827, 35.311827, 20.581418),
    tolerance = 1e-6
  )
  # the run it slid to is one separation above the line it made room for
  expect_equal(wp$y[[2]] - run$coord, sep_e_default, tolerance = 1e-6)

  # chol's other arrival takes the row above the new owner of its centre
  expect_equal(
    arrival_row(scene, res, "smoking->chol"),
    35.311827,
    tolerance = 1e-6
  )
  expect_equal(
    res$meta$resect_head[edge_index(scene, "smoking->chol")],
    port_resect_at(sep_e_default),
    tolerance = 1e-9
  )

  # nothing else in the region moves: education -> healthcare_access keeps
  # the 2 mm jog beside its own trunk, on the slot the re-ranking gives it
  eh <- edge_index(scene, "education->healthcare_access")
  wp <- res$waypoints[[eh]]
  expect_equal(
    wp$x,
    c(48.830414, 48.830414, 111.591241, 111.591241),
    tolerance = 1e-6
  )
  expect_equal(
    wp$y,
    c(49.652769, 46.052769, 46.052769, 53.251215),
    tolerance = 1e-6
  )
})

# The excursion census: over every orthogonal E/W channel, the vertical
# travel V of its drawn path against the endpoints' y difference dy. A run on
# either endpoint's line spends dy and at most one row offset, so V / dy near
# 1 is the straight picture, while a run e mm outside the chord's y range
# spends dy + 2 e. A channel is flagged when V > 2 max(dy, sep_e): that is an
# excursion of more than half the difference, with the floor at sep_e so that
# a level chord is not flagged for a jog narrower than one channel.
channel_excursions <- function(scenes) {
  rows <- list()
  for (scene in scenes) {
    res <- ortho(scene)
    layers <- infer_layers(scene$nodes, r_default)
    layer_of <- stats::setNames(layers$id, scene$nodes$name)
    for (i in seq_len(nrow(scene$edges))) {
      if (res$meta$mode[[i]] != "orthogonal") {
        next
      }
      span <- abs(
        layer_of[[scene$edges$to[[i]]]] - layer_of[[scene$edges$from[[i]]]]
      )
      if (span < 2L) {
        next
      }
      runs <- straight_runs(dedupe_path(res$paths[[i]]))
      # an E/W channel leaves through an E port, so its first run is
      # horizontal; an S/N channel leaves vertically and is a different shape
      if (nrow(runs) == 0 || runs$axis[[1]] != "h") {
        next
      }
      ends <- edge_endpoints(scene, i)
      dy <- abs(ends$to[[2]] - ends$from[[2]])
      travel <- sum(runs$length[runs$axis == "v"])
      rows[[length(rows) + 1L]] <- data.frame(
        edge = edge_labels(scene$edges)[[i]],
        travel = travel,
        flagged = travel > 2 * max(dy, sep_e_default),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

test_that("orthogonal packing: the excursion census loses the crowded channel", {
  # Over the head census scenes and the gallery's largest scene at 10 x 6,
  # 117 E/W channels are drawn and three of them are excursions. The slide
  # removed genetics -> chol, whose 24.1 mm of vertical travel for a 5.0 mm
  # difference became the 3.4 mm the two corner arcs leave. Two more went
  # with the last rung's spread: gene -> cancer leaves its source vertically,
  # off a census that counts channels leaving through an E port, and
  # phys_act -> bp's travel falls from 24.1 to 18.8 mm, inside twice its
  # endpoints' difference. The three that stay are forced detours, not
  # crowding: a blocked line, a band with no free y, or a channel over a
  # stack that fills the panel.
  # The counts below are the drawing the gallery makes, so they read the
  # left-to-right scenes alone; the mirrored copies belong to the censuses
  # that assert invariants rather than tallies.
  scenes <- c(
    forward_census_scenes(),
    list(very_big_scene(gallery_panels[[3]]))
  )
  x <- channel_excursions(scenes)

  expect_equal(nrow(x), 117L)
  expect_equal(sum(x$flagged), 3L)
  expect_setequal(
    x$edge[x$flagged],
    c(
      "adversity->smoking",
      "education->smoking",
      "parental_ses->nutrition"
    )
  )
  expect_false(any(x$flagged[x$edge == "genetics->chol"]))
})

# The line each spanning orthogonal channel of a canonical scene runs on,
# named by its edge.
channel_lines <- function(name, panel = c(100, 70)) {
  scene <- canonical_scene(name, panel)
  res <- ortho(scene)
  layers <- infer_layers(scene$nodes, r_default)
  layer_of <- stats::setNames(layers$id, scene$nodes$name)
  out <- numeric(0)
  for (i in seq_len(nrow(scene$edges))) {
    if (res$meta$mode[[i]] != "orthogonal") {
      next
    }
    span <- abs(
      layer_of[[scene$edges$to[[i]]]] - layer_of[[scene$edges$from[[i]]]]
    )
    if (span < 2L) {
      next
    }
    out <- c(
      out,
      stats::setNames(
        longest_run(res$paths[[i]])$coord,
        edge_labels(scene$edges)[[i]]
      )
    )
  }
  out
}

test_that("orthogonal packing: the scenes with no crowded endpoint line are untouched", {
  # The slide fires only where a feasible endpoint line is crowded, which
  # among the tracked scenes is very_big at 10 x 6 alone. Over every other
  # census scene the drawn geometry is pinned as the aggregate of the
  # orthogonal, spline and straight routings: the number of routed edges,
  # their waypoints, their total drawn length and, for the orthogonal
  # channels, their total vertical travel. The last rung's spread moves the
  # orthogonal aggregate, and only it: 27 of these scenes' departures now
  # leave on their source's own centre line, which is one bend fewer each.
  # The aggregates are the drawing the gallery makes, so they read the
  # left-to-right scenes alone; the mirrored copies belong to the censuses
  # that assert invariants rather than tallies.
  scenes <- c(
    forward_census_scenes(),
    list(
      very_big_scene(gallery_panels[[1]]),
      very_big_scene(gallery_panels[[2]])
    )
  )
  path_length <- function(path) sum(sqrt(diff(path$x)^2 + diff(path$y)^2))
  aggregate_of <- function(mode) {
    routed <- 0L
    waypoints <- 0L
    drawn <- 0
    travel <- 0
    for (scene in scenes) {
      res <- route_scene(scene, mode = mode, opts = route_constants(r_default))
      routed <- routed + sum(res$meta$mode != "straight")
      waypoints <- waypoints + sum(res$meta$n_waypoints)
      for (i in seq_along(res$paths)) {
        drawn <- drawn + path_length(res$paths[[i]])
        if (res$meta$mode[[i]] != "orthogonal") {
          next
        }
        runs <- straight_runs(dedupe_path(res$paths[[i]]))
        if (nrow(runs) > 0) {
          travel <- travel + sum(runs$length[runs$axis == "v"])
        }
      }
    }
    list(routed = routed, waypoints = waypoints, drawn = drawn, travel = travel)
  }

  a <- aggregate_of("orthogonal")
  expect_equal(a$routed, 420L)
  expect_equal(a$waypoints, 921L)
  expect_equal(a$drawn, 35379.020552, tolerance = 1e-9)
  expect_equal(a$travel, 11601.554659, tolerance = 1e-9)

  # the slide and the ladder are orthogonal rules, so neither curved mode
  # travels at all; the spline aggregate carries the five census chords
  # that run under an arrowhead and are nudged past it
  b <- aggregate_of("spline")
  expect_equal(b$routed, 83L)
  expect_equal(b$waypoints, 160L)
  expect_equal(b$drawn, 29164.596238, tolerance = 1e-9)
  expect_equal(b$travel, 0)

  d <- aggregate_of("straight")
  expect_equal(d$routed, 0L)
  expect_equal(d$waypoints, 0L)
  expect_equal(d$drawn, 28040.89615, tolerance = 1e-9)

  # the canonical scenes the design names as untouched, channel by channel
  expect_equal(
    channel_lines("epidemiology"),
    c(
      "ses->health" = 27.12531,
      "edu->health" = 30.72531,
      "age->health" = 10,
      "gene->health" = 43.79243
    ),
    tolerance = 1e-6
  )
  expect_equal(
    channel_lines("large_epi"),
    c(
      "age->smoking" = 13.82503,
      "age->bmi" = 26.24295,
      "age->health" = 10,
      "ses->health" = 22.62148,
      "smoking->health" = 35.50176,
      "diet->health" = 62.66901,
      "bmi->health" = 56.86856,
      "gene->cancer" = 62.66901
    ),
    tolerance = 1e-6
  )
  expect_equal(
    channel_lines("triple_confound"),
    c(
      "u->y" = 27.2835,
      "v->m" = 43.28865,
      "w->y" = 20.10208,
      "x->y" = 63.65081
    ),
    tolerance = 1e-6
  )
  expect_equal(
    channel_lines("multi_mediator"),
    c(
      "x->m2" = 37.26801,
      "x->m3" = 46.26801,
      "m1->y" = 10,
      "m2->y" = 37.26801,
      "u->y" = 49.86801
    ),
    tolerance = 1e-6
  )
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

test_that("routed arrows: a head at the end of a head run and margin is drawn along it", {
  skip_if_not_installed("ggarrow")
  # The 20 mm gap's target-side slot leaves exactly cap + head + head_margin
  # of run before the disc, the shortest run the last rung leaves where the
  # gap can afford it. ggarrow cuts the path at the router's resect and aims
  # the head at the path's last point, so both lie on that run and every
  # head is drawn along it.
  scene <- narrow_band_scene(gap = 20)
  sharp <- ortho(scene, corners = "sharp")
  runs <- vapply(sharp$paths, function(path) last_run(path)$length, numeric(1))
  expect_equal(min(runs), head_run_margin_default, tolerance = 1e-6)

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

# The same geometry under two namings: the left node at (20, 50), the right
# at (100, 50) with a node between them, and one edge each way, so the two
# edges are a parallel group of two. The edge from the left node is built
# first, so edge 1 is that copy whatever the two nodes are called.
parallel_pair_scene <- function(left, right) {
  list(
    nodes = mm_nodes(c(left, right, "m"), c(20, 100, 60), c(50, 50, 51)),
    edges = mm_edges(c(left, right), c(right, left)),
    bounds = c(0, 0, 120, 100)
  )
}

test_that("parallel copies are spread in the order of their endpoints", {
  # A group's members are translated sep_m apart in a fixed order, and that
  # order decides which copy is drawn on which side. Read from the node
  # names it makes the picture depend on what the caller calls the nodes:
  # naming the left node "a" and the right one "z" draws the left-to-right
  # copy on one side, and swapping the two names draws it on the other. In
  # the order of the endpoints' positions the copy out of the left node
  # takes the first shift under either naming, in both modes: it bows under
  # the chord in spline mode and runs over it in orthogonal mode.
  for (mode in c("spline", "orthogonal")) {
    over <- if (mode == "spline") -1 else 1
    for (naming in list(c("a", "z"), c("z", "a"))) {
      scene <- parallel_pair_scene(naming[[1]], naming[[2]])
      res <- route_scene(scene, mode = mode)
      expect_equal(
        sign(mean(res$paths[[1]]$y) - 50),
        over,
        label = paste(mode, naming[[1]], naming[[2]])
      )
    }
  }

  # in orthogonal mode the two namings draw one picture, edge for edge: the
  # copy out of the left node runs 13.1 mm over the chord and its partner
  # 13.1 mm under it
  ref <- route_scene(parallel_pair_scene("a", "z"), mode = "orthogonal")
  res <- route_scene(parallel_pair_scene("z", "a"), mode = "orthogonal")
  keep <- setdiff(names(ref$meta), "edge")
  expect_identical(res$meta[keep], ref$meta[keep])
  for (i in seq_along(ref$paths)) {
    expect_lt(
      polyline_hausdorff(res$paths[[i]], ref$paths[[i]]),
      1e-9,
      label = paste("orthogonal", i)
    )
  }
  expect_equal(range(ref$paths[[1]]$y), c(50, 63.1), tolerance = 1e-6)
  expect_equal(range(ref$paths[[2]]$y), c(36.9, 50), tolerance = 1e-6)

  # and in spline mode they draw one picture too. The member order alone
  # does not settle that: the second copy is routed around the first, so
  # which of the two is routed first decides the pair. route_scene_mm()
  # visits the edges in order of span and chord length, and reads the
  # endpoints' positions to break the tie the two copies make, so both
  # namings route the copy out of the left node first and the pair
  # straddles the chord. Read from the node names, one naming straddles it
  # and the other bows both copies under it, 24 mm from the other picture.
  ref <- route_scene(parallel_pair_scene("a", "z"), mode = "spline")
  res <- route_scene(parallel_pair_scene("z", "a"), mode = "spline")
  keep <- setdiff(names(ref$meta), "edge")
  expect_identical(res$meta[keep], ref$meta[keep])
  for (i in seq_along(ref$paths)) {
    expect_lt(
      polyline_hausdorff(res$paths[[i]], ref$paths[[i]]),
      1e-9,
      label = paste("spline", i)
    )
  }
  expect_equal(range(ref$paths[[1]]$y), c(36, 50), tolerance = 1e-6)
  expect_equal(range(ref$paths[[2]]$y), c(50, 66), tolerance = 1e-6)

  # the members of a group of duplicates share both endpoints, so their
  # positions tie and the input order decides, as the name order did
  duplicate <- fan_scene()
  duplicate$edges <- rbind(duplicate$edges, mm_edges("a", "b"))
  names_rev <- rev(letters)[seq_len(nrow(duplicate$nodes))]
  for (mode in c("spline", "orthogonal")) {
    ref <- route_scene(duplicate, mode = mode)
    res <- route_scene(rename_scene(duplicate, names_rev), mode = mode)
    for (i in seq_along(ref$paths)) {
      expect_lt(
        polyline_hausdorff(res$paths[[i]], ref$paths[[i]]),
        1e-9,
        label = paste("duplicate", mode, i)
      )
    }
  }
})

# The drawn geometry of the census scenes and the gallery's largest scene at
# every panel, as a checksum: the number of routed edges, the number of
# sampled points, the total drawn length, the coordinate sums and the two
# resect sums.
census_checksum <- function(mode) {
  scenes <- c(forward_census_scenes(), lapply(gallery_panels, very_big_scene))
  out <- list(
    routed = 0L,
    points = 0L,
    drawn = 0,
    x = 0,
    y = 0,
    head = 0,
    fins = 0
  )
  for (scene in scenes) {
    res <- route_scene(scene, mode = mode, opts = route_constants(r_default))
    out$routed <- out$routed + sum(res$meta$mode != "straight")
    out$head <- out$head + sum(res$meta$resect_head)
    out$fins <- out$fins + sum(res$meta$resect_fins)
    for (path in res$paths) {
      out$points <- out$points + nrow(path)
      out$drawn <- out$drawn +
        sum(sqrt(diff(path$x)^2 + diff(path$y)^2))
      out$x <- out$x + sum(path$x)
      out$y <- out$y + sum(path$y)
    }
  }
  out
}

test_that("the scenes drawn at the default node size are untouched", {
  # The row floor fires under r = 2.15 and the position order of a parallel
  # group's members only where a group has two, and no tracked scene has
  # either: every node is 6 mm and no scene draws two edges between one
  # pair of nodes. The checksum is the pin that says the drawn pictures are
  # the same ones.
  orthogonal <- census_checksum("orthogonal")
  expect_equal(orthogonal$routed, 473L)
  expect_equal(orthogonal$points, 80634L)
  expect_equal(orthogonal$drawn, 39526.869597425, tolerance = 1e-9)
  expect_equal(orthogonal$x, 6089493.467054041, tolerance = 1e-9)
  expect_equal(orthogonal$y, 4253066.697316638, tolerance = 1e-9)
  expect_equal(orthogonal$head, 3960.880730802, tolerance = 1e-9)
  expect_equal(orthogonal$fins, 4212.651832214, tolerance = 1e-9)

  spline <- census_checksum("spline")
  expect_equal(spline$routed, 113L)
  expect_equal(spline$points, 16834L)
  expect_equal(spline$drawn, 32390.876311216, tolerance = 1e-9)
  expect_equal(spline$x, 1385005.051702970, tolerance = 1e-9)
  expect_equal(spline$y, 937782.428045510, tolerance = 1e-9)
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

test_that("route_edges_mm() routes very_big under orthogonal at the large panel at interactive speed", {
  skip_on_cran()
  skip_on_ci()
  # Opt-in pin; see test-layout-perf.R for the GGDAG_RUN_PERF_TESTS contract.
  skip_if(
    Sys.getenv("GGDAG_RUN_PERF_TESTS") == "",
    "GGDAG_RUN_PERF_TESTS is not set"
  )
  skip_if_not_installed("bench")

  # very_big at 10 x 6 is the densest orthogonal scene the gallery draws:
  # thirty nodes over eleven layers, twenty-two spanning channels, every gap
  # on the ladder's last rung. It is also the scene the channel packing pass
  # fires on, so this gate bounds what that pass may cost.
  scene <- very_big_scene(gallery_panels[[3]])

  timing <- bench::mark(
    route_scene(scene, mode = "orthogonal", opts = route_constants(r_default)),
    iterations = 30,
    filter_gc = FALSE
  )
  # The scene measures 28 to 30 ms on the development machine without the
  # packing pass and about 34 with a prototype of it. The gate is 45 ms so
  # that only a real regression trips it.
  expect_lt(as.numeric(timing$median), 0.045)
})
