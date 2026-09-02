# Tests for visibility-graph edge routing. The routing engine in
# R/route_edges.R takes each straight edge that a node blocks, builds a local
# visibility graph over the edge endpoints and the tangent points on expanded
# obstacle circles (radius 1.5 * node_radius, giving the drawn node a
# 0.5 * node_radius clearance margin), finds the shortest path with Dijkstra,
# and smooths the corners with two iterations of Chaikin corner cutting.
# StatDAGRoutedEdge turns edge rows (x, y, xend, yend) into waypoint rows in
# the long format the label obstacle machinery already consumes (edge_id, x,
# y, seq), and geom_dag_routed_arrows() renders each waypoint path as a
# multi-point ggarrow arrow. The auto_route option (default FALSE) swaps the
# routed geom into the packaged ggarrow edge rendering.
#
# Everything here is deterministic: the engine consumes no randomness, and an
# on-chord obstruction breaks the up/down tie by detouring below, mirroring
# the auto_curve tie rule (a node on the chord counts as sitting above it, so
# the edge goes below).

# Helpers ----------------------------------------------------------------------

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

# The tangent point on the circle (cx, cy, radius) touched by the tangent
# line from the external point (px, py), on the requested side of the x axis.
# Closed form: the tangent segment has length sqrt(d^2 - radius^2) where d is
# the distance from the point to the center, and leaves the point at the
# angle toward the center plus or minus asin(radius / d).
tangent_point <- function(px, py, cx, cy, radius, side = c("below", "above")) {
  side <- match.arg(side)
  dx <- cx - px
  dy <- cy - py
  d <- sqrt(dx^2 + dy^2)
  angle <- atan2(dy, dx)
  half <- asin(radius / d)
  reach <- sqrt(d^2 - radius^2)
  candidates <- rbind(
    c(px + reach * cos(angle + half), py + reach * sin(angle + half)),
    c(px + reach * cos(angle - half), py + reach * sin(angle - half))
  )
  pick <- if (side == "below") {
    which.min(candidates[, 2])
  } else {
    which.max(candidates[, 2])
  }
  candidates[pick, ]
}

# Reference implementation of the pinned smoothing law: each interior corner
# is replaced by the points one quarter before and one quarter after it along
# its adjacent segments, and the endpoints are kept.
chaikin_reference <- function(x, y, iterations) {
  pts <- cbind(x, y)
  for (k in seq_len(iterations)) {
    n <- nrow(pts)
    if (n <= 2) {
      break
    }
    rows <- list(pts[1, , drop = FALSE])
    for (i in 2:(n - 1)) {
      rows[[length(rows) + 1]] <- rbind(
        pts[i - 1, ] + 0.75 * (pts[i, ] - pts[i - 1, ]),
        pts[i, ] + 0.25 * (pts[i + 1, ] - pts[i, ])
      )
    }
    rows[[length(rows) + 1]] <- pts[n, , drop = FALSE]
    pts <- do.call(rbind, rows)
  }
  data.frame(x = pts[, 1], y = pts[, 2])
}

# Build a minimal tidy_dagitty data tibble from a node table and an edge
# list, mirroring the fixture builder in test-route_edges.R: one row per edge
# plus one terminal row (to = NA) for each node with no outgoing edge.
make_dag_data <- function(coords, edges) {
  direction <- edges$direction
  if (is.null(direction)) {
    direction <- rep("->", nrow(edges))
  }
  edge_rows <- tibble::tibble(
    name = edges$name,
    x = as.numeric(coords$x[match(edges$name, coords$name)]),
    y = as.numeric(coords$y[match(edges$name, coords$name)]),
    direction = factor(direction, levels = c("->", "<->", "--")),
    to = edges$to,
    xend = as.numeric(coords$x[match(edges$to, coords$name)]),
    yend = as.numeric(coords$y[match(edges$to, coords$name)])
  )
  terminal <- setdiff(coords$name, edges$name)
  terminal_rows <- tibble::tibble(
    name = terminal,
    x = as.numeric(coords$x[match(terminal, coords$name)]),
    y = as.numeric(coords$y[match(terminal, coords$name)]),
    direction = factor(NA, levels = c("->", "<->", "--")),
    to = NA_character_,
    xend = NA_real_,
    yend = NA_real_
  )
  dplyr::bind_rows(edge_rows, terminal_rows)
}

# The mediation triangle with the mediator dead on the x -> y chord.
mediator_dag <- function() {
  dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
}

# The indices of the layers drawn by the routing stat.
routed_layer_index <- function(plot) {
  which(vapply(
    plot$layers,
    function(layer) inherits(layer$stat, "StatDAGRoutedEdge"),
    logical(1)
  ))
}

# The waypoints of the edge whose endpoints are (x, y) and (xend, yend),
# ordered by seq, from a routed layer's built data.
edge_waypoints <- function(layer_df, x, y, xend, yend) {
  for (rows in split(layer_df, layer_df$edge_id)) {
    rows <- rows[order(rows$seq), , drop = FALSE]
    n <- nrow(rows)
    if (
      abs(rows$x[1] - x) < 1e-8 &&
        abs(rows$y[1] - y) < 1e-8 &&
        abs(rows$x[n] - xend) < 1e-8 &&
        abs(rows$y[n] - yend) < 1e-8
    ) {
      return(rows)
    }
  }
  NULL
}

# Distance from each point to the modeled quadratic Bezier arc, by dense
# sampling; the sampling spacing bounds the error well below the tolerances
# used here.
bezier_dist <- function(px, py, x, y, xend, yend, curvature) {
  arc <- sample_curved_edge(x, y, xend, yend, curvature = curvature, n = 1000)
  vapply(
    seq_along(px),
    function(i) min(sqrt((arc$x - px[i])^2 + (arc$y - py[i])^2)),
    numeric(1)
  )
}

# chaikin_smooth ---------------------------------------------------------------

test_that("chaikin_smooth: a straight two-point path is returned unchanged", {
  res <- chaikin_smooth(c(0, 2), c(0, 1), iterations = 2)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("x", "y"))
  expect_equal(res$x, c(0, 2))
  expect_equal(res$y, c(0, 1))

  # there is no corner to cut however often the smoothing runs
  res <- chaikin_smooth(c(0, 2), c(0, 1), iterations = 5)
  expect_equal(res$x, c(0, 2))
  expect_equal(res$y, c(0, 1))
})

test_that("chaikin_smooth: one iteration cuts a corner at the quarter points", {
  # the right-angle corner at (1, 0) is replaced by the point one quarter
  # before it along (0,0) -> (1,0) and the point one quarter after it along
  # (1,0) -> (1,1); the endpoints are kept
  res <- chaikin_smooth(c(0, 1, 1), c(0, 0, 1), iterations = 1)
  expect_equal(res$x, c(0, 0.75, 1, 1))
  expect_equal(res$y, c(0, 0, 0.25, 1))
})

test_that("chaikin_smooth: each iteration turns n points into 2n - 2", {
  # every interior corner becomes two cut points and the endpoints survive,
  # so 3 points become 4, then 6, then 10
  x <- c(0, 1, 2)
  y <- c(0, 1, 0)
  expect_identical(nrow(chaikin_smooth(x, y, iterations = 1)), 4L)
  expect_identical(nrow(chaikin_smooth(x, y, iterations = 2)), 6L)
  expect_identical(nrow(chaikin_smooth(x, y, iterations = 3)), 10L)

  two <- chaikin_smooth(x, y, iterations = 2)
  expect_equal(two$x[c(1, nrow(two))], c(0, 2))
  expect_equal(two$y[c(1, nrow(two))], c(0, 0))
  expect_equal(two, chaikin_reference(x, y, iterations = 2))
})

test_that("chaikin_smooth: the default is two iterations", {
  x <- c(0, 1, 2)
  y <- c(0, 1, 0)
  res <- chaikin_smooth(x, y)
  expect_equal(res, chaikin_smooth(x, y, iterations = 2))
  expect_false(nrow(res) == nrow(chaikin_smooth(x, y, iterations = 1)))
})

# route_edge_waypoints: unblocked edges ----------------------------------------

test_that("route_edge_waypoints: an unblocked edge is the straight two-point path", {
  r <- node_radius_data()

  # no obstacle nodes at all
  res <- route_edge_waypoints(0, 0, 2, 0, numeric(), numeric(), node_radius = r)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("x", "y"))
  expect_identical(nrow(res), 2L)
  expect_equal(res$x, c(0, 2))
  expect_equal(res$y, c(0, 0))

  # a node well away from the corridor changes nothing
  res <- route_edge_waypoints(0, 0, 2, 0, 1, 1, node_radius = r)
  expect_identical(nrow(res), 2L)
  expect_equal(res$x, c(0, 2))
  expect_equal(res$y, c(0, 0))

  # a zero-length edge cannot be routed and must come back as its two
  # stacked endpoints without error
  res <- route_edge_waypoints(0, 0, 0, 0, 1, 0, node_radius = r)
  expect_identical(nrow(res), 2L)
})

test_that("route_edge_waypoints: the corridor is 1.5 node radii wide", {
  r <- node_radius_data()

  # a node just outside the expanded obstacle radius does not block the edge
  res <- route_edge_waypoints(0, 0, 2, 0, 1, 1.5 * r + 1e-9, node_radius = r)
  expect_identical(nrow(res), 2L)

  # a node just inside it does
  res <- route_edge_waypoints(0, 0, 2, 0, 1, 1.5 * r - 0.01, node_radius = r)
  expect_gt(nrow(res), 2L)
})

# route_edge_waypoints: single on-chord obstacle -------------------------------

test_that("route_edge_waypoints: an on-chord obstacle produces the tangent path, smoothed", {
  r <- node_radius_data()
  r_exp <- 1.5 * r

  # Derivation for the edge (0,0) -> (2,0) blocked by a node at (1,0), with
  # the expanded obstacle circle at radius R = 1.5 * r = 0.2167. The chord
  # runs through the circle, so the straight path is blocked:
  expect_lt(dist_to_edge(1, 0, 0, 0, 2, 0), r_exp)

  # The visibility path must visit tangent points from both endpoints. Each
  # endpoint is at distance 1 from the center, so its tangent points sit at
  # (0.9531, +/- 0.2115) and (1.0469, +/- 0.2115). The shortcut straight from
  # (0,0) to the far tangent point passes within 0.198 of the center, inside
  # the expanded circle, so no three-point path exists:
  t_start <- tangent_point(0, 0, 1, 0, r_exp, "below")
  t_end <- tangent_point(2, 0, 1, 0, r_exp, "below")
  expect_lt(dist_to_edge(1, 0, 0, 0, t_end[1], t_end[2]), r_exp)

  # The raw shortest path is therefore start, both lower tangent points, end
  # (the up and down paths tie by symmetry, and the tie rule picks below,
  # mirroring auto_curve). Two Chaikin iterations turn its 4 points into
  # 2 * 4 - 2 = 6 and then 2 * 6 - 2 = 10.
  expected <- chaikin_reference(
    c(0, t_start[1], t_end[1], 2),
    c(0, t_start[2], t_end[2], 0),
    iterations = 2
  )

  res <- route_edge_waypoints(0, 0, 2, 0, 1, 0, node_radius = r)
  expect_identical(nrow(res), 10L)
  expect_equal(res$x, expected$x, tolerance = 1e-6)
  expect_equal(res$y, expected$y, tolerance = 1e-6)
})

test_that("route_edge_waypoints: the smoothed path keeps its clearance", {
  r <- node_radius_data()
  r_exp <- 1.5 * r

  res <- route_edge_waypoints(0, 0, 2, 0, 1, 0, node_radius = r)

  # endpoints are exactly the edge endpoints
  expect_equal(c(res$x[1], res$y[1]), c(0, 0))
  expect_equal(c(res$x[nrow(res)], res$y[nrow(res)]), c(2, 0))

  # The deepest point of the detour is the tangent offset R * sqrt(1 - R^2)
  # (tangent points from an endpoint at distance 1), and corner cutting never
  # pushes the path further out than that.
  y_star <- r_exp * sqrt(1 - r_exp^2)
  expect_equal(max(abs(res$y)), y_star, tolerance = 1e-6)

  # The chords between tangent points and the corner cuts dip slightly inside
  # the expanded circle, but the clearance margin absorbs them: the polyline
  # stays at least 1.4 node radii from the obstacle center (measured 1.457 on
  # this fixture), well clear of the drawn node at 1 radius.
  expect_gt(min(polyline_dist(1, 0, res$x, res$y)), 1.4 * r)
  expect_lt(min(polyline_dist(1, 0, res$x, res$y)), r_exp)
})

# route_edge_waypoints: side selection -----------------------------------------

test_that("route_edge_waypoints: the detour goes around the shorter side", {
  r <- node_radius_data()

  # A node at (1, 0.06) blocks the chord mostly from above: the below detour
  # only has to clear the 0.157 of the expanded circle that protrudes below
  # the chord, while the above detour must clear 0.277. The tangent-path
  # lengths are 2.024 below versus 2.075 above, so Dijkstra goes below.
  res <- route_edge_waypoints(0, 0, 2, 0, 1, 0.06, node_radius = r)
  expect_gt(nrow(res), 2L)
  interior <- res[-c(1, nrow(res)), , drop = FALSE]
  expect_true(all(interior$y < 0))
  expect_gt(min(polyline_dist(1, 0.06, res$x, res$y)), r)

  # the mirrored fixture routes the mirrored path
  mirrored <- route_edge_waypoints(0, 0, 2, 0, 1, -0.06, node_radius = r)
  expect_equal(mirrored$x, res$x, tolerance = 1e-8)
  expect_equal(mirrored$y, -res$y, tolerance = 1e-8)
})

# route_edge_waypoints: obstacles on both sides --------------------------------

test_that("route_edge_waypoints: opposite blockers force an S-shaped weave", {
  r <- node_radius_data()

  # The edge (0,0) -> (4,0) is blocked by (1, 0.1) and (3, -0.1). Weaving
  # below the first and above the second is length 4.027 against 4.066 for a
  # detour around both on either single side (checked against a brute-force
  # shortest path over densely sampled obstacle boundaries), so the shortest
  # route is the S shape.
  res <- route_edge_waypoints(
    0,
    0,
    4,
    0,
    c(1, 3),
    c(0.1, -0.1),
    node_radius = r
  )
  expect_gt(nrow(res), 2L)
  expect_equal(c(res$x[1], res$y[1]), c(0, 0))
  expect_equal(c(res$x[nrow(res)], res$y[nrow(res)]), c(4, 0))

  # the path clears both drawn nodes
  expect_gt(min(polyline_dist(1, 0.1, res$x, res$y)), r)
  expect_gt(min(polyline_dist(3, -0.1, res$x, res$y)), r)

  # and weaves: below the chord near the first blocker, above it near the
  # second. Clearing (1, 0.1) from below puts the path at y < -0.04 there,
  # so 0.02 leaves a comfortable margin.
  sampled <- sample_polyline(res$x, res$y, 400)
  expect_lt(sampled$y[which.min(abs(sampled$x - 1))], -0.02)
  expect_gt(sampled$y[which.min(abs(sampled$x - 3))], 0.02)
})

# route_edge_waypoints: defaults and determinism -------------------------------

test_that("route_edge_waypoints: node_radius defaults to node_radius_data()", {
  res <- route_edge_waypoints(0, 0, 2, 0, 1, 0)
  explicit <- route_edge_waypoints(
    0,
    0,
    2,
    0,
    1,
    0,
    node_radius = node_radius_data()
  )
  expect_identical(res, explicit)
})

test_that("route_edge_waypoints: deterministic and consumes no randomness", {
  invisible(stats::runif(1))
  seed_before <- get(".Random.seed", envir = globalenv())

  first <- route_edge_waypoints(0, 0, 4, 0, c(1, 3), c(0.1, -0.1))
  second <- route_edge_waypoints(0, 0, 4, 0, c(1, 3), c(0.1, -0.1))
  expect_identical(first, second)
  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)
})

# StatDAGRoutedEdge and geom_dag_routed_arrows ---------------------------------

test_that("StatDAGRoutedEdge is a ggplot2 stat", {
  expect_true(inherits(StatDAGRoutedEdge, "ggproto"))
  expect_true(inherits(StatDAGRoutedEdge, "Stat"))
})

test_that("geom_dag_routed_arrows: routed directed edges plus an arc for bidirected ones", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    u ~ ~v,
    coords = list(
      x = c(x = 0, m = 1, y = 2, u = 0, v = 2),
      y = c(x = 0, m = 0, y = 0, u = 2, v = 2)
    )
  )
  p <- ggplot(tidy_dagitty(dag), aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  # one routed layer for the directed edges, one curve layer for the
  # bidirected pair, and both draw with the ggarrow engine
  idx <- routed_layer_index(p)
  expect_length(idx, 1)
  expect_identical(count_geom_layers(p, "GeomDAGArrowCurve"), 1L)
  expect_true(inherits(p$layers[[idx]]$geom, "GeomArrow"))
  expect_true(uses_ggarrow_edges(p))

  # one arrowhead at the path end and none at the start by default
  arrow <- p$layers[[idx]]$geom_params$arrow
  expect_false(is.null(arrow$head))
  expect_null(arrow$fins)

  # the routed layer carries exactly the three directed edges; the
  # bidirected pair stays on the arc layer
  built <- ggplot2::layer_data(p, idx)
  expect_length(unique(built$edge_id), 3)
  expect_null(edge_waypoints(built, 0, 2, 2, 2))
})

test_that("StatDAGRoutedEdge emits waypoint long format matching the engine", {
  skip_if_not_installed("ggarrow")
  r <- node_radius_data()

  p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()
  idx <- routed_layer_index(p)
  expect_length(idx, 1)
  built <- ggplot2::layer_data(p, idx)

  expect_contains(names(built), c("edge_id", "x", "y", "seq"))
  groups <- split(built, built$edge_id)
  expect_length(groups, 3)

  # the blocked x -> y edge carries the full routed path the engine computes
  blocked <- edge_waypoints(built, 0, 0, 2, 0)
  expect_false(is.null(blocked))
  expect_identical(nrow(blocked), 10L)
  expect_equal(blocked$seq, seq_len(10))
  engine <- route_edge_waypoints(0, 0, 2, 0, 1, 0, node_radius = r)
  expect_equal(blocked$x, engine$x, tolerance = 1e-8)
  expect_equal(blocked$y, engine$y, tolerance = 1e-8)
  expect_gt(min(polyline_dist(1, 0, blocked$x, blocked$y)), r)

  # the unblocked edges are two-point straight paths
  for (ends in list(c(0, 0, 1, 0), c(1, 0, 2, 0))) {
    straight <- edge_waypoints(built, ends[1], ends[2], ends[3], ends[4])
    expect_false(is.null(straight))
    expect_identical(nrow(straight), 2L)
    expect_equal(straight$seq, c(1, 2))
  }

  # each edge is one drawing group of its own, so the paths render separately
  group_of <- vapply(groups, function(g) g$group[[1]], numeric(1))
  expect_length(unique(group_of), 3)
  for (g in groups) {
    expect_length(unique(g$group), 1)
  }
})

test_that("geom_dag_routed_arrows: node_radius widens the corridor", {
  skip_if_not_installed("ggarrow")

  # m sits 0.3 from the chord, outside the default corridor of
  # 1.5 * 0.144 = 0.217, so nothing routes by default
  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0.3, y = 0))
  )
  td <- tidy_dagitty(dag)

  p <- ggplot(td, aes_dag()) + geom_dag_routed_arrows() + geom_dag_point()
  built <- ggplot2::layer_data(p, routed_layer_index(p))
  expect_true(all(table(built$edge_id) == 2))

  # widening the radius pulls m into the corridor and routes around it with
  # the wider clearance
  p <- ggplot(td, aes_dag()) +
    geom_dag_routed_arrows(node_radius = 0.4) +
    geom_dag_point()
  built <- ggplot2::layer_data(p, routed_layer_index(p))
  blocked <- edge_waypoints(built, 0, 0, 2, 0)
  expect_false(is.null(blocked))
  expect_gt(nrow(blocked), 2L)
  expect_gt(min(polyline_dist(1, 0.3, blocked$x, blocked$y)), 0.4)
})

test_that("geom_dag_routed_arrows: explicit edge curvature is never rerouted", {
  skip_if_not_installed("ggarrow")

  # three stacked mediation triangles, each with its mediator dead on the
  # x -> y chord; only the first leaves its curvature unset
  coords <- data.frame(
    name = c("x1", "m1", "y1", "x2", "m2", "y2", "x3", "m3", "y3"),
    x = rep(c(0, 1, 2), 3),
    y = rep(c(0, 5, 10), each = 3)
  )
  edges <- data.frame(
    name = c("x1", "m1", "x1", "x2", "m2", "x2", "x3", "m3", "x3"),
    to = c("m1", "y1", "y1", "m2", "y2", "y2", "m3", "y3", "y3")
  )
  data <- make_dag_data(coords, edges)
  data$edge_curvature <- NA_real_
  data$edge_curvature[data$name == "x2" & data$to == "y2"] <- 0.4
  data$edge_curvature[data$name == "x2" & data$to == "m2"] <- 0
  data$edge_curvature[data$name == "m2"] <- 0
  data$edge_curvature[data$name == "x3" | data$name == "m3"] <- 0

  p <- ggplot(data, aes_dag(edge_curvature = edge_curvature)) +
    geom_dag_routed_arrows() +
    geom_dag_point()
  built <- ggplot2::layer_data(p, routed_layer_index(p))

  # every directed edge is drawn; none is dropped for carrying a curvature
  expect_length(unique(built$edge_id), 9)

  # unset curvature: the blocked edge routes around its mediator
  routed <- edge_waypoints(built, 0, 0, 2, 0)
  expect_gt(nrow(routed), 2L)
  expect_gt(
    min(polyline_dist(1, 0, routed$x, routed$y)),
    node_radius_data()
  )

  # a numeric curvature wins: the edge follows the user's arc, not a detour
  curved <- edge_waypoints(built, 0, 5, 2, 5)
  expect_gt(nrow(curved), 2L)
  expect_lt(max(bezier_dist(curved$x, curved$y, 0, 5, 2, 5, 0.4)), 0.01)

  # an explicit zero wins too: the edge stays straight through its mediator
  straight <- edge_waypoints(built, 0, 10, 2, 10)
  expect_identical(nrow(straight), 2L)
})

test_that("geom_dag_routed_arrows: curve_edge() curvature survives routing", {
  skip_if_not_installed("ggarrow")

  dag <- curve_edge(mediator_dag(), "x", "y", 0.45)
  p <- ggplot(tidy_dagitty(dag), aes_dag(edge_curvature = edge_curvature)) +
    geom_dag_routed_arrows() +
    geom_dag_point()
  built <- ggplot2::layer_data(p, routed_layer_index(p))

  # the curved edge follows the user's arc
  curved <- edge_waypoints(built, 0, 0, 2, 0)
  expect_gt(nrow(curved), 2L)
  expect_lt(max(bezier_dist(curved$x, curved$y, 0, 0, 2, 0, 0.45)), 0.01)

  # curve_edge() pins the other edges to zero, which counts as set: they are
  # drawn as straight two-point paths
  expect_identical(nrow(edge_waypoints(built, 0, 0, 1, 0)), 2L)
  expect_identical(nrow(edge_waypoints(built, 1, 0, 2, 0)), 2L)
})

test_that("StatDAGRoutedEdge: deterministic and consumes no randomness", {
  skip_if_not_installed("ggarrow")

  build_waypoints <- function() {
    p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
      geom_dag_routed_arrows() +
      geom_dag_point()
    ggplot2::layer_data(p, routed_layer_index(p))
  }

  invisible(stats::runif(1))
  seed_before <- get(".Random.seed", envir = globalenv())

  expect_identical(build_waypoints(), build_waypoints())
  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)
})

test_that("geom_dag_routed_arrows: resects to the node size like the other arrow geoms", {
  skip_if_not_installed("ggarrow")

  p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
    geom_dag_point() +
    geom_dag_routed_arrows()

  idx <- routed_layer_index(p)
  resect <- p$layers[[idx]]$geom_params$resect
  expect_equal(resect$head, node_size_to_cap(16))
  expect_equal(resect$fins, node_size_to_cap(16))
})

# Edge geometry discovery ------------------------------------------------------

test_that("a routed arrows layer is discovered as routed waypoints", {
  skip_if_not_installed("ggarrow")
  r <- node_radius_data()

  p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  expect_false(is.null(geometry))
  routed <- geometry[geometry$type == "routed", , drop = FALSE]
  expect_contains(names(routed), c("edge_id", "x", "y", "seq"))

  # the discovered waypoints are the ones the stat draws: 10 for the routed
  # x -> y edge and 2 for each straight edge
  built <- ggplot2::layer_data(p, routed_layer_index(p))
  expect_identical(nrow(routed), nrow(built))
  expect_length(unique(routed$edge_id), 3)

  counts <- sort(as.integer(table(routed$edge_id)))
  expect_identical(counts, c(2L, 2L, 10L))

  blocked_id <- names(which(table(routed$edge_id) == 10))
  blocked <- routed[routed$edge_id == blocked_id, , drop = FALSE]
  blocked <- blocked[order(blocked$seq), , drop = FALSE]
  expect_gt(min(polyline_dist(1, 0, blocked$x, blocked$y)), r)
})

test_that("automatic labels treat the routed path as the edge obstacle", {
  skip_if_not_installed("ggarrow")
  r <- node_radius_data()

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    labels = c(x = "Exposure", m = "Mediator", y = "Outcome"),
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
  p <- ggplot(tidy_dagitty(dag), aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point() +
    geom_dag_label_auto(aes(label = label))

  waypoints <- ggplot2::layer_data(p, routed_layer_index(p))
  blocked <- edge_waypoints(waypoints, 0, 0, 2, 0)
  expect_gt(nrow(blocked), 2L)

  label_idx <- which(vapply(
    p$layers,
    function(layer) inherits(layer$stat, "StatNodesLabelAuto"),
    logical(1)
  ))
  expect_length(label_idx, 1)
  built <- ggplot2::layer_data(p, label_idx)
  edge_rows <- built[built$ggdag_role == "edge", , drop = FALSE]
  expect_gt(nrow(edge_rows), 0)

  # every straight edge here lies on the y = 0 line, so any off-chord
  # obstacle point can only come from tracing the routed detour, and it must
  # sit on the drawn polyline, clear of the mediator
  detour <- edge_rows[abs(edge_rows$y) > 1e-6, , drop = FALSE]
  expect_gt(nrow(detour), 0)
  expect_gt(max(abs(detour$y)), 0.15)
  expect_lt(
    max(polyline_dist(detour$x, detour$y, blocked$x, blocked$y)),
    1e-6
  )
  expect_gt(min(sqrt((detour$x - 1)^2 + detour$y^2)), r)
})

# auto_route option ------------------------------------------------------------

test_that("auto_route option is registered, defaults to FALSE, and round-trips", {
  local_ggdag_option_state()

  expect_true("auto_route" %in% names(ggdag_defaults))
  expect_identical(ggdag_defaults$auto_route, FALSE)
  expect_false(ggdag_option("auto_route", FALSE))

  ggdag_options_set(auto_route = TRUE)
  expect_true(ggdag_option("auto_route", FALSE))
})

test_that("auto_route option rejects non-logical values with a typed error", {
  local_ggdag_option_state()
  expect_true("auto_route" %in% names(ggdag_defaults))

  expect_error(
    ggdag_options_set(auto_route = "yes"),
    class = "ggdag_type_error"
  )
  expect_error(ggdag_options_set(auto_route = 1), class = "ggdag_type_error")
  expect_error(ggdag_options_set(auto_route = NA), class = "ggdag_type_error")
  expect_error(
    ggdag_options_set(auto_route = c(TRUE, FALSE)),
    class = "ggdag_type_error"
  )
})

test_that("auto_route option validation errors are informative", {
  local_ggdag_option_state()
  # the registration must exist before any snapshot is recorded: without it,
  # the unknown-option error would be captured in place of the validation
  # message
  stopifnot("auto_route" %in% names(ggdag_defaults))

  expect_ggdag_error(ggdag_options_set(auto_route = "yes"))
  expect_ggdag_error(ggdag_options_set(auto_route = 1))
  expect_ggdag_error(ggdag_options_set(auto_route = NA))
})

# auto_route in the packaged edge rendering ------------------------------------

test_that("auto_route off by default: the packaged ggarrow edges stay straight", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  expect_true("auto_route" %in% names(ggdag_defaults))
  ggdag_options_set(edge_engine = "ggarrow")

  p <- ggdag(tidy_dagitty(mediator_dag()))
  expect_length(routed_layer_index(p), 0)

  # the blocked edge is drawn as the straight chord by the curve layer
  curve_idx <- which(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomDAGArrowCurve"),
    logical(1)
  ))
  drawn <- dplyr::bind_rows(
    lapply(curve_idx, function(i) ggplot2::layer_data(p, i))
  )
  expect_identical(sum(drawn$x == 0 & drawn$xend == 2), 1L)
})

test_that("auto_route on: ggdag() swaps in the routed geom for directed edges", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", auto_route = TRUE)
  r <- node_radius_data()

  p <- ggdag(tidy_dagitty(mediator_dag()))

  idx <- routed_layer_index(p)
  expect_length(idx, 1)
  built <- ggplot2::layer_data(p, idx)
  blocked <- edge_waypoints(built, 0, 0, 2, 0)
  expect_false(is.null(blocked))
  expect_gt(nrow(blocked), 2L)
  expect_gt(min(polyline_dist(1, 0, blocked$x, blocked$y)), r)

  # no curve layer draws the directed edges as well: this DAG has no
  # bidirected edges, so every remaining ggarrow curve layer builds empty
  curve_idx <- which(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomDAGArrowCurve"),
    logical(1)
  ))
  curve_rows <- vapply(
    curve_idx,
    function(i) nrow(ggplot2::layer_data(p, i)),
    integer(1)
  )
  expect_identical(sum(curve_rows), 0L)
})

# Visual baselines -------------------------------------------------------------

test_that("vdiffr: routed arrows detour around a mediator", {
  skip_if_not_installed("ggarrow")
  r <- node_radius_data()

  p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  # the routed layer must exist and clear the mediator before a baseline is
  # recorded: a snapshot of the unrouted plot would pin the wrong picture
  idx <- routed_layer_index(p)
  stopifnot(length(idx) == 1)
  blocked <- edge_waypoints(ggplot2::layer_data(p, idx), 0, 0, 2, 0)
  stopifnot(
    !is.null(blocked),
    nrow(blocked) > 2,
    min(polyline_dist(1, 0, blocked$x, blocked$y)) > r
  )

  expect_doppelganger("routed arrows detour around a mediator", p)
})

test_that("vdiffr: routed arrows weave between opposite blockers", {
  skip_if_not_installed("ggarrow")
  r <- node_radius_data()

  coords <- data.frame(
    name = c("x", "a", "b", "y"),
    x = c(0, 1, 3, 4),
    y = c(0, 0.1, -0.1, 0)
  )
  edges <- data.frame(name = c("x", "a"), to = c("y", "b"))
  data <- make_dag_data(coords, edges)

  p <- ggplot(data, aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point() +
    theme_dag()

  # the weave must clear both blockers before a baseline is recorded
  path <- route_edge_waypoints(0, 0, 4, 0, c(1, 3), c(0.1, -0.1))
  stopifnot(
    nrow(path) > 2,
    min(polyline_dist(1, 0.1, path$x, path$y)) > r,
    min(polyline_dist(3, -0.1, path$x, path$y)) > r
  )

  expect_doppelganger("routed arrows weave between opposite blockers", p)
})

test_that("vdiffr: auto route reroutes the packaged edges", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", auto_route = TRUE)
  r <- node_radius_data()

  p <- ggdag(tidy_dagitty(mediator_dag()))

  idx <- routed_layer_index(p)
  stopifnot(length(idx) == 1)
  blocked <- edge_waypoints(ggplot2::layer_data(p, idx), 0, 0, 2, 0)
  stopifnot(
    !is.null(blocked),
    nrow(blocked) > 2,
    min(polyline_dist(1, 0, blocked$x, blocked$y)) > r
  )

  expect_doppelganger("auto route reroutes the packaged edges", p)
})

test_that("vdiffr: auto route off draws the straight edge", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  # the option must exist before this baseline is recorded, so that the
  # comparison picture is the settled default rather than an accident of the
  # option being unimplemented
  stopifnot(
    "auto_route" %in% names(ggdag_defaults),
    identical(ggdag_defaults$auto_route, FALSE)
  )
  ggdag_options_set(edge_engine = "ggarrow")

  p <- ggdag(tidy_dagitty(mediator_dag()))
  stopifnot(length(routed_layer_index(p)) == 0)

  expect_doppelganger("auto route off draws the straight edge", p)
})
