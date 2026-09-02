# Negative controls for the routed layer signature: a layer is discovered as
# routed only when it draws with `GeomDAGRoutedArrow`, whatever columns its
# data happens to carry. Also guards the obstacle assembly path for mirrored
# curve pairs, which reach the tracer through the grouping in
# `repel_edge_points()` rather than in one direct call.

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

test_that("a plain path layer is not discovered as routed", {
  dag <- dagify(
    y ~ x,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )
  detour <- data.frame(
    x = c(0, 1, 2),
    y = c(0, 0.8, 0),
    seq = 1:3
  )
  p <- ggplot(dag, aes_dag()) +
    ggplot2::geom_path(data = detour, aes(x, y), inherit.aes = FALSE) +
    geom_dag_point()

  expect_null(discover_edge_geometry(p))
})

test_that("a routed arrows layer is discovered even though it carries xend", {
  skip_if_not_installed("ggarrow")

  # the routed layer's data are the plot rows, endpoint columns and all: the
  # geom is what says the edges are routed, not the shape of the data
  dag <- dagify(
    y ~ x,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )
  p <- ggplot(dag, aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  expect_false(is.null(geometry))
  routed <- geometry[geometry$type == "routed", , drop = FALSE]
  expect_identical(nrow(routed), 2L)
  expect_contains(names(routed), c("xend", "yend", "route_style"))
})
