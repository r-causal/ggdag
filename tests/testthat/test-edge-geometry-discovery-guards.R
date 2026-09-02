# Negative controls for the routed waypoint signature: a layer is discovered
# as routed only when its data carries all of `edge_id`, `x`, `y`, and `seq`
# and none of the wide-format endpoint columns. Also guards the obstacle
# assembly path for mirrored curve pairs, which reach the tracer through the
# grouping in `repel_edge_points()` rather than in one direct call.

test_that("mirrored curve rows keep distinct edge ids through repel_edge_points", {
  # a mirrored pair between the same endpoints: both rows share the chord
  # key, so only the tracer's row index can tell their ids apart, and the
  # grouping must not split the pair into separate calls that each restart
  # the index at 1
  geometry <- data.frame(
    x = c(0, 0),
    y = c(0, 0),
    xend = c(2, 2),
    yend = c(0, 0),
    circular = FALSE,
    type = "curve",
    strength = c(0.3, -0.3),
    n = 100,
    fold = FALSE,
    flipped = FALSE,
    from = NA_character_,
    to = NA_character_,
    curvature = NA_real_,
    stringsAsFactors = FALSE
  )
  edges <- data.frame(x = 0, y = 0, xend = 2, yend = 0, PANEL = 1L)

  points <- repel_edge_points(
    edges,
    10,
    geometry,
    NULL,
    include_endpoints = TRUE
  )

  groups <- split(points, points$edge_id)
  expect_length(groups, 2)

  # each id carries one arc only, bowing to its own side of the chord
  min_y <- vapply(groups, function(g) min(g$y), numeric(1))
  max_y <- vapply(groups, function(g) max(g$y), numeric(1))
  below <- groups[[which(min_y < -0.3)]]
  above <- groups[[which(max_y > 0.3)]]
  expect_lte(max(below$y), 0)
  expect_gte(min(above$y), 0)
})

test_that("a waypoint layer without edge_id is not discovered as routed", {
  dag <- dagify(
    y ~ x,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )
  partial <- data.frame(
    x = c(0, 1, 2),
    y = c(0, 0.8, 0),
    seq = 1:3
  )
  p <- ggplot(dag, aes_dag()) +
    ggplot2::geom_path(data = partial, aes(x, y), inherit.aes = FALSE) +
    geom_dag_point()

  expect_null(discover_edge_geometry(p))
})

test_that("a layer with endpoint columns alongside waypoints is not routed", {
  dag <- dagify(
    y ~ x,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )
  wide <- data.frame(
    edge_id = c("x_y", "x_y", "x_y"),
    x = c(0, 1, 2),
    y = c(0, 0.8, 0),
    seq = 1:3,
    xend = c(1, 2, NA),
    yend = c(0.8, 0, NA),
    stringsAsFactors = FALSE
  )
  p <- ggplot(dag, aes_dag()) +
    ggplot2::geom_path(
      data = wide,
      aes(x, y, group = edge_id),
      inherit.aes = FALSE
    ) +
    geom_dag_point()

  expect_null(discover_edge_geometry(p))
})
