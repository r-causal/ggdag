# Helpers ----------------------------------------------------------------------

# Signed perpendicular offset of a point from the line through (x, y)-(xend,
# yend), using the same cross-product sign convention as
# ctrl_point_to_curvature(): positive offsets lie below a left-to-right edge.
signed_offset <- function(px, py, x, y, xend, yend) {
  dx <- xend - x
  dy <- yend - y
  len <- sqrt(dx^2 + dy^2)
  mx <- (x + xend) / 2
  my <- (y + yend) / 2
  ((px - mx) * dy - (py - my) * dx) / len
}

# Place a control point at the given signed perpendicular offset from the edge
# midpoint and recover the curvature that ctrl_point_to_curvature() assigns it.
recovered_curvature <- function(offset, x, y, xend, yend) {
  dx <- xend - x
  dy <- yend - y
  len <- sqrt(dx^2 + dy^2)
  data <- data.frame(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
    edge_ctrl_x = (x + xend) / 2 + offset * dy / len,
    edge_ctrl_y = (y + yend) / 2 - offset * dx / len
  )
  ctrl_point_to_curvature(data)
}

# dist_to_edge -----------------------------------------------------------------

test_that("dist_to_edge: perpendicular distance beside the segment interior", {
  expect_equal(dist_to_edge(2, 3, 0, 0, 4, 0), 3)
  expect_equal(dist_to_edge(2, -3, 0, 0, 4, 0), 3)
  # diagonal segment: distance from (0, 2) to the line y = x is sqrt(2)
  expect_equal(dist_to_edge(0, 2, 0, 0, 4, 4), sqrt(2))
})

test_that("dist_to_edge: points beyond an endpoint clamp to endpoint distance", {
  # beyond the far endpoint of (0,0)-(4,0)
  expect_equal(dist_to_edge(6, 0, 0, 0, 4, 0), 2)
  # beyond the near endpoint, off axis: distance to (0, 0) is 5
  expect_equal(dist_to_edge(-3, 4, 0, 0, 4, 0), 5)
})

test_that("dist_to_edge: points on the segment have distance zero", {
  expect_equal(dist_to_edge(1, 0, 0, 0, 4, 0), 0)
  expect_equal(dist_to_edge(0, 0, 0, 0, 4, 0), 0)
  expect_equal(dist_to_edge(4, 0, 0, 0, 4, 0), 0)
  expect_equal(dist_to_edge(2, 2, 0, 0, 4, 4), 0)
})

test_that("dist_to_edge: vertical and horizontal segments", {
  # vertical segment (0,0)-(0,4)
  expect_equal(dist_to_edge(3, 2, 0, 0, 0, 4), 3)
  expect_equal(dist_to_edge(0, 6, 0, 0, 0, 4), 2)
  # horizontal segment (1,1)-(5,1)
  expect_equal(dist_to_edge(3, 4, 1, 1, 5, 1), 3)
})

test_that("dist_to_edge: vectorized over points", {
  px <- c(2, 6, -3, 1)
  py <- c(3, 0, 4, 0)
  expect_equal(dist_to_edge(px, py, 0, 0, 4, 0), c(3, 2, 5, 0))
})

# sample_curved_edge -----------------------------------------------------------

test_that("sample_curved_edge: curvature 0 lies on the straight segment", {
  pts <- sample_curved_edge(0, 0, 3, 1.5, curvature = 0)
  # every sampled point satisfies the line equation y = x / 2
  expect_equal(pts$y, pts$x / 2)
  expect_true(all(pts$x >= 0 & pts$x <= 3))
})

test_that("sample_curved_edge: positive curvature bows below a left-to-right edge", {
  pts <- sample_curved_edge(0, 0, 2, 0, curvature = 0.5)
  # grid::curveGrob convention: positive curvature curves below the edge
  expect_true(min(pts$y) < 0)
  expect_true(all(pts$y <= 1e-8))
})

test_that("sample_curved_edge: negative curvature bows above a left-to-right edge", {
  pts <- sample_curved_edge(0, 0, 2, 0, curvature = -0.5)
  expect_true(max(pts$y) > 0)
  expect_true(all(pts$y >= -1e-8))
})

test_that("sample_curved_edge: offset sign matches ctrl_point_to_curvature on a diagonal edge", {
  pts_pos <- sample_curved_edge(0, 0, 2, 2, curvature = 0.4)
  offsets_pos <- signed_offset(pts_pos$x, pts_pos$y, 0, 0, 2, 2)
  expect_true(max(offsets_pos) > 0)
  expect_true(all(offsets_pos >= -1e-8))

  pts_neg <- sample_curved_edge(0, 0, 2, 2, curvature = -0.4)
  offsets_neg <- signed_offset(pts_neg$x, pts_neg$y, 0, 0, 2, 2)
  expect_true(min(offsets_neg) < 0)
  expect_true(all(offsets_neg <= 1e-8))
})

test_that("sample_curved_edge: endpoints are exact", {
  pts <- sample_curved_edge(1, 2, 4, 6, curvature = 0.3)
  expect_equal(pts$x[1], 1)
  expect_equal(pts$y[1], 2)
  expect_equal(pts$x[nrow(pts)], 4)
  expect_equal(pts$y[nrow(pts)], 6)

  pts_straight <- sample_curved_edge(1, 2, 4, 6, curvature = 0)
  expect_equal(pts_straight$x[1], 1)
  expect_equal(pts_straight$y[1], 2)
  expect_equal(pts_straight$x[nrow(pts_straight)], 4)
  expect_equal(pts_straight$y[nrow(pts_straight)], 6)
})

test_that("sample_curved_edge: n controls the number of sampled points", {
  expect_equal(nrow(sample_curved_edge(0, 0, 1, 1, curvature = 0.3)), 24)
  expect_equal(
    nrow(sample_curved_edge(0, 0, 1, 1, curvature = 0.3, n = 10)),
    10
  )
  expect_equal(
    nrow(sample_curved_edge(0, 0, 1, 1, curvature = 0.3, n = 50)),
    50
  )
})

# curvature_to_ctrl_offset -----------------------------------------------------

test_that("curvature_to_ctrl_offset: zero curvature gives zero offset", {
  expect_equal(curvature_to_ctrl_offset(0, 0, 0, 4, 0), 0)
  expect_equal(curvature_to_ctrl_offset(0, 1, 2, 4, 6), 0)
})

test_that("curvature_to_ctrl_offset: exact round trip through ctrl_point_to_curvature", {
  curvatures <- c(-0.9, -0.5, -0.1, 0.1, 0.3, 0.5, 0.9)
  edges <- list(
    c(0, 0, 1, 0), # short horizontal
    c(0, 0, 5, 0), # long horizontal
    c(0, 0, 0, 3), # vertical
    c(1, 2, 4, 6) # diagonal, length 5
  )

  for (edge in edges) {
    for (curvature in curvatures) {
      offset <- curvature_to_ctrl_offset(
        curvature,
        edge[1],
        edge[2],
        edge[3],
        edge[4]
      )
      expect_equal(
        recovered_curvature(offset, edge[1], edge[2], edge[3], edge[4]),
        curvature,
        tolerance = 1e-8
      )
    }
  }
})

test_that("curvature_to_ctrl_offset: matches the inverse of the atan formula", {
  # ctrl_point_to_curvature() maps offset d to atan(2 * d / len) * 2 / pi, so
  # the inverse is d = (len / 2) * tan(curvature * pi / 2)
  len <- 5
  curvature <- 0.6
  expect_equal(
    curvature_to_ctrl_offset(curvature, 0, 0, len, 0),
    (len / 2) * tan(curvature * pi / 2),
    tolerance = 1e-8
  )
})

# node_radius_data -------------------------------------------------------------

test_that("node_radius_data: default is 26 / 180", {
  expect_equal(node_radius_data(), 26 / 180)
  expect_equal(node_radius_data(16), 26 / 180)
})

test_that("node_radius_data: scales linearly with node_size", {
  expect_equal(node_radius_data(32), 2 * 26 / 180)
  expect_equal(node_radius_data(8), 26 / 180 / 2)
  expect_equal(node_radius_data(0), 0)
})

# count_edge_crossings ---------------------------------------------------------

test_that("count_edge_crossings: two edges crossing in an X count once", {
  coords <- data.frame(
    name = c("a", "b", "c", "d"),
    x = c(0, 1, 0, 1),
    y = c(0, 1, 1, 0)
  )
  edges <- data.frame(name = c("a", "c"), to = c("b", "d"))
  expect_identical(count_edge_crossings(coords, edges), 1L)
})

test_that("count_edge_crossings: edges sharing an endpoint do not count", {
  coords <- data.frame(
    name = c("a", "b", "c", "d"),
    x = c(0, 1, 1, 1),
    y = c(0, 1, 0, -1)
  )
  edges <- data.frame(name = c("a", "a", "a"), to = c("b", "c", "d"))
  expect_identical(count_edge_crossings(coords, edges), 0L)
})

test_that("count_edge_crossings: parallel edges do not cross", {
  coords <- data.frame(
    name = c("a", "b", "c", "d"),
    x = c(0, 1, 0, 1),
    y = c(0, 0, 1, 1)
  )
  edges <- data.frame(name = c("a", "c"), to = c("b", "d"))
  expect_identical(count_edge_crossings(coords, edges), 0L)
})

test_that("count_edge_crossings: square with both diagonals has exactly one crossing", {
  coords <- data.frame(
    name = c("a", "b", "c", "d"),
    x = c(0, 1, 1, 0),
    y = c(0, 0, 1, 1)
  )
  edges <- data.frame(
    name = c("a", "b", "c", "d", "a", "b"),
    to = c("b", "c", "d", "a", "c", "d")
  )
  # the perimeter edges share endpoints with each other and with the
  # diagonals; only the two diagonals cross
  expect_identical(count_edge_crossings(coords, edges), 1L)
})

# count_node_edge_overlaps -----------------------------------------------------

test_that("count_node_edge_overlaps: collinear mediator counts one overlap", {
  coords <- data.frame(
    name = c("a", "b", "c"),
    x = c(0, 1, 2),
    y = c(0, 0, 0)
  )
  edges <- data.frame(name = c("a", "b", "a"), to = c("b", "c", "c"))
  # a -> c passes straight through b; b is an endpoint of the other two edges
  expect_identical(
    count_node_edge_overlaps(coords, edges, node_radius_data()),
    1L
  )
})

test_that("count_node_edge_overlaps: sufficient curvature clears the mediator", {
  coords <- data.frame(
    name = c("a", "b", "c"),
    x = c(0, 1, 2),
    y = c(0, 0, 0)
  )
  edges <- data.frame(name = c("a", "b", "a"), to = c("b", "c", "c"))

  # zero curvature everywhere matches the straight-segment count
  expect_identical(
    count_node_edge_overlaps(
      coords,
      edges,
      node_radius_data(),
      curvature = c(0, 0, 0)
    ),
    1L
  )

  # curving a -> c away from b removes the overlap
  expect_identical(
    count_node_edge_overlaps(
      coords,
      edges,
      node_radius_data(),
      curvature = c(0, 0, 0.6)
    ),
    0L
  )
})

test_that("count_node_edge_overlaps: isolated node far from all edges counts zero", {
  coords <- data.frame(
    name = c("a", "b", "z"),
    x = c(0, 2, 10),
    y = c(0, 0, 10)
  )
  edges <- data.frame(name = "a", to = "b")
  expect_identical(
    count_node_edge_overlaps(coords, edges, node_radius_data()),
    0L
  )
})

# score_layout -----------------------------------------------------------------

test_that("score_layout: returns named components that are all non-negative", {
  coords <- data.frame(
    name = c("u1", "u2", "a", "m", "y"),
    x = c(0, 4, 0, 2, 4),
    y = c(2, 2, 0, 1, 0)
  )
  edges <- data.frame(
    name = c("u1", "u1", "u2", "u2", "a"),
    to = c("a", "m", "m", "y", "y")
  )

  score <- score_layout(coords, edges)
  expect_named(
    score,
    c("total", "crossings", "node_edge", "angular", "stress")
  )
  expect_true(all(vapply(score, \(x) x >= 0, logical(1))))
})

test_that("score_layout: a perfect unit path scores zero on every component", {
  coords <- data.frame(name = c("a", "b", "c"), x = c(0, 1, 2), y = c(0, 0, 0))
  edges <- data.frame(name = c("a", "b"), to = c("b", "c"))

  score <- score_layout(coords, edges)
  expect_equal(score$crossings, 0)
  expect_equal(score$node_edge, 0)
  expect_equal(score$angular, 0)
  expect_equal(score$stress, 0, tolerance = 1e-8)
})

test_that("score_layout: stretched edges produce positive stress", {
  coords <- data.frame(name = c("a", "b", "c"), x = c(0, 1, 5), y = c(0, 0, 0))
  edges <- data.frame(name = c("a", "b"), to = c("b", "c"))
  expect_gt(score_layout(coords, edges)$stress, 0)
})

test_that("score_layout: nearly parallel incident edges produce an angular penalty", {
  edges <- data.frame(name = c("o", "o"), to = c("p", "q"))

  # p and q are about 6 degrees apart as seen from o
  narrow <- data.frame(
    name = c("o", "p", "q"),
    x = c(0, 10, 10),
    y = c(0, 0, 1)
  )
  expect_gt(score_layout(narrow, edges)$angular, 0)

  # at 90 degrees there is no penalty
  wide <- data.frame(
    name = c("o", "p", "q"),
    x = c(0, 10, 0),
    y = c(0, 0, 10)
  )
  expect_equal(score_layout(wide, edges)$angular, 0)
})

test_that("score_layout: crossings dominate the total with default weights", {
  edges <- data.frame(name = c("a", "b", "c"), to = c("b", "c", "d"))

  crossed <- data.frame(
    name = c("a", "b", "c", "d"),
    x = c(0, 1, 1, 0),
    y = c(0, 1, 0, 1)
  )
  score <- score_layout(crossed, edges)
  expect_gt(score$crossings, 0)
  # one crossing at weight 100 puts the total at 100 or more
  expect_gte(score$total, 100)
})

test_that("score_layout: a layout with a crossing scores strictly worse", {
  edges <- data.frame(name = c("a", "b", "c"), to = c("b", "c", "d"))

  crossed <- data.frame(
    name = c("a", "b", "c", "d"),
    x = c(0, 1, 1, 0),
    y = c(0, 1, 0, 1)
  )
  straight <- data.frame(
    name = c("a", "b", "c", "d"),
    x = c(0, 1, 2, 3),
    y = c(0, 0, 0, 0)
  )

  expect_gt(
    score_layout(crossed, edges)$total,
    score_layout(straight, edges)$total
  )
})

test_that("score_layout: deterministic across repeated calls", {
  coords <- data.frame(
    name = c("u1", "u2", "a", "m", "y"),
    x = c(0, 4, 0, 2, 4),
    y = c(2, 2, 0, 1, 0)
  )
  edges <- data.frame(
    name = c("u1", "u1", "u2", "u2", "a"),
    to = c("a", "m", "m", "y", "y")
  )
  expect_identical(score_layout(coords, edges), score_layout(coords, edges))
})

# Canonical DAG spot checks ----------------------------------------------------

test_that("canonical mediation: collinear layout has one overlap, triangle has none", {
  edges <- data.frame(name = c("x", "m", "x"), to = c("m", "y", "y"))

  collinear <- data.frame(
    name = c("x", "m", "y"),
    x = c(0, 1, 2),
    y = c(0, 0, 0)
  )
  expect_identical(count_edge_crossings(collinear, edges), 0L)
  expect_identical(
    count_node_edge_overlaps(collinear, edges, node_radius_data()),
    1L
  )

  triangle <- data.frame(
    name = c("x", "m", "y"),
    x = c(0, 1, 2),
    y = c(0, 1, 0)
  )
  expect_identical(count_edge_crossings(triangle, edges), 0L)
  expect_identical(
    count_node_edge_overlaps(triangle, edges, node_radius_data()),
    0L
  )
})

test_that("canonical m-bias: standard layout is crossing- and overlap-free", {
  edges <- data.frame(
    name = c("u1", "u1", "u2", "u2", "a"),
    to = c("a", "m", "m", "y", "y")
  )
  coords <- data.frame(
    name = c("u1", "u2", "a", "m", "y"),
    x = c(0, 4, 0, 2, 4),
    y = c(2, 2, 0, 1, 0)
  )
  expect_identical(count_edge_crossings(coords, edges), 0L)
  expect_identical(
    count_node_edge_overlaps(coords, edges, node_radius_data()),
    0L
  )
})

test_that("canonical m-bias: swapping a and y creates exactly one crossing", {
  edges <- data.frame(
    name = c("u1", "u1", "u2", "u2", "a"),
    to = c("a", "m", "m", "y", "y")
  )
  coords <- data.frame(
    name = c("u1", "u2", "a", "m", "y"),
    x = c(0, 4, 4, 2, 0),
    y = c(2, 2, 0, 1.5, 0)
  )
  # u1 -> a and u2 -> y cross at (2, 1); no node sits within a radius of a
  # non-incident edge
  expect_identical(count_edge_crossings(coords, edges), 1L)
  expect_identical(
    count_node_edge_overlaps(coords, edges, node_radius_data()),
    0L
  )
})

test_that("canonical butterfly: standard layout is crossing- and overlap-free", {
  edges <- data.frame(
    name = c("x1", "x2", "m", "m", "x1", "x2"),
    to = c("m", "m", "y1", "y2", "y1", "y2")
  )
  coords <- data.frame(
    name = c("x1", "x2", "m", "y1", "y2"),
    x = c(0, 0, 1, 2, 2),
    y = c(2, 0, 1, 2, 0)
  )
  expect_identical(count_edge_crossings(coords, edges), 0L)
  expect_identical(
    count_node_edge_overlaps(coords, edges, node_radius_data()),
    0L
  )
})
