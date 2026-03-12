expect_ggproto <- function(x) {
  testthat::expect_s3_class(x, "ggproto")
}

test_that("Geom and Stat ggprotos are in fact ggprotos", {
  expect_ggproto(StatNodes)
  expect_ggproto(StatNodesRepel)
  expect_ggproto(GeomDagPoint)
  expect_ggproto(GeomDagNode)
  expect_ggproto(GeomDagText)
  expect_ggproto(StatEdgeLink)
  expect_ggproto(StatEdgeArc)
  expect_ggproto(StatEdgeDiagonal)
  expect_ggproto(StatEdgeFan)
  expect_ggproto(GeomDAGEdgePath)
})

test_that("StatNodesRepel handles duplicate labels at different positions correctly", {
  # Create test data with duplicate labels at different positions
  test_data <- data.frame(
    x = c(1, 2, 3, 4),
    y = c(1, 1, 2, 2),
    label = c("unmeasured", "unmeasured", "actual", "posted"),
    PANEL = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  # Test the compute_layer function
  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_node_points = 0),
    NULL
  )

  # Should keep all 4 rows since they have different positions
  # even though "unmeasured" appears twice
  expect_equal(nrow(result), 4)
  expect_equal(result$label, c("unmeasured", "unmeasured", "actual", "posted"))
  expect_equal(result$x, c(1, 2, 3, 4))
  expect_equal(result$y, c(1, 1, 2, 2))
})

test_that("StatNodesRepel removes true duplicates (same position and label)", {
  # Create test data with true duplicates (same x, y, label)
  test_data <- data.frame(
    x = c(1, 1, 2, 3),
    y = c(1, 1, 1, 2),
    label = c("unmeasured", "unmeasured", "actual", "posted"),
    PANEL = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  # Test the compute_layer function
  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_node_points = 0),
    NULL
  )

  # Should remove the true duplicate but keep others
  expect_equal(nrow(result), 3)
  # Should keep first occurrence of duplicate
  expect_true("unmeasured" %in% result$label)
  expect_true("actual" %in% result$label)
  expect_true("posted" %in% result$label)
})

test_that("StatNodesRepel adds point.size from node_size param", {
  test_data <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 1, 2),
    xend = c(2, 3, 1),
    yend = c(1, 2, 1),
    label = c("a", "b", "c"),
    PANEL = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  # With explicit node_size param

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(node_size = 20, n_edge_points = 0, n_node_points = 0),
    NULL
  )
  expect_true("point.size" %in% names(result))
  expect_equal(result[["point.size"]], rep(20 * ggplot2::.pt / 14.4, 3))

  # With default (NULL) params — should use node_size = 16

  result_default <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0, n_node_points = 0),
    NULL
  )
  expect_true("point.size" %in% names(result_default))
  expect_equal(result_default[["point.size"]], rep(16 * ggplot2::.pt / 14.4, 3))
})

test_that("StatNodesRepel does not overwrite mapped point.size", {
  test_data <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    xend = c(2, 1),
    yend = c(2, 1),
    label = c("a", "b"),
    point.size = c(5, 10),
    PANEL = c(1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(node_size = 16, n_edge_points = 0, n_node_points = 0),
    NULL
  )
  # Should preserve user-mapped values

  expect_equal(result[["point.size"]], c(5, 10))
})

test_that("StatNodesRepel generates fake points along edges", {
  test_data <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 1, 2),
    xend = c(2, 3, 1),
    yend = c(1, 2, 1),
    label = c("a", "b", "c"),
    PANEL = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 5, n_node_points = 0),
    NULL
  )

  # 3 nodes + 3 edges * 5 points = 18 rows
  expect_equal(nrow(result), 18)

  # Fake points have label = ""
  fake_rows <- result[result$label == "", ]
  expect_equal(nrow(fake_rows), 15)

  # Fake points have point.size = 0

  expect_true(all(fake_rows[["point.size"]] == 0))
})

test_that("StatNodesRepel n_edge_points controls fake point count", {
  test_data <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    xend = c(2, 1),
    yend = c(2, 1),
    label = c("a", "b"),
    PANEL = c(1, 1),
    stringsAsFactors = FALSE
  )

  result_3 <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 3, n_node_points = 0),
    NULL
  )
  # 2 nodes + 2 edges * 3 points = 8
  expect_equal(nrow(result_3), 8)

  result_10 <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 10, n_node_points = 0),
    NULL
  )
  # 2 nodes + 2 edges * 10 points = 22
  expect_equal(nrow(result_10), 22)
})

test_that("StatNodesRepel n_edge_points = 0 disables fake points", {
  test_data <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    xend = c(2, 1),
    yend = c(2, 1),
    label = c("a", "b"),
    PANEL = c(1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0, n_node_points = 0),
    NULL
  )

  # Only 2 node rows, no fake points
  expect_equal(nrow(result), 2)
  expect_true(all(result$label != ""))
})

test_that("StatNodesRepel generates no fake points without xend/yend", {
  test_data <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 1, 2),
    label = c("a", "b", "c"),
    PANEL = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 10, n_node_points = 0),
    NULL
  )

  # Only 3 node rows
  expect_equal(nrow(result), 3)
  expect_true(all(result$label != ""))
})

test_that("StatNodesRepel fake points don't affect node point.size", {
  test_data <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    xend = c(2, 1),
    yend = c(2, 1),
    label = c("a", "b"),
    PANEL = c(1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(node_size = 20, n_edge_points = 5, n_node_points = 0),
    NULL
  )

  node_rows <- result[result$label != "", ]
  fake_rows <- result[result$label == "", ]

  # Nodes get node_size-based point.size
  expect_equal(node_rows[["point.size"]], rep(20 * ggplot2::.pt / 14.4, 2))
  # Fake points get point.size = 0
  expect_true(all(fake_rows[["point.size"]] == 0))
})

test_that("StatNodesRepel fake points preserve PANEL", {
  test_data <- data.frame(
    x = c(1, 2, 3, 4),
    y = c(1, 2, 1, 2),
    xend = c(2, 1, 4, 3),
    yend = c(2, 1, 2, 1),
    label = c("a", "b", "c", "d"),
    PANEL = c(1, 1, 2, 2),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 3, n_node_points = 0),
    NULL
  )

  fake_rows <- result[result$label == "", ]
  # Each panel has 2 edges * 3 points = 6 fake points
  expect_equal(sum(fake_rows$PANEL == 1), 6)
  expect_equal(sum(fake_rows$PANEL == 2), 6)
})

test_that("StatNodesRepel default n_edge_points is 50", {
  test_data <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    xend = c(2, 1),
    yend = c(2, 1),
    label = c("a", "b"),
    PANEL = c(1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_node_points = 0),
    NULL
  )

  fake_rows <- result[result$label == "", ]
  # 2 edges * 50 points = 100
  expect_equal(nrow(fake_rows), 100)
})

test_that("StatNodesRepel generates node skeleton points", {
  test_data <- data.frame(
    x = c(1, 3, 5),
    y = c(1, 3, 1),
    label = c("a", "b", "c"),
    PANEL = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0, n_node_points = 8),
    NULL
  )

  # 3 nodes + 3 nodes * 8 skeleton points = 27
  expect_equal(nrow(result), 27)

  skeleton_rows <- result[result$label == "", ]
  expect_equal(nrow(skeleton_rows), 24)
})

test_that("StatNodesRepel skeleton points have label='' and point.size=0", {
  test_data <- data.frame(
    x = c(1, 3),
    y = c(1, 3),
    label = c("a", "b"),
    PANEL = c(1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0, n_node_points = 6),
    NULL
  )

  skeleton_rows <- result[result$label == "", ]
  expect_equal(nrow(skeleton_rows), 12)
  expect_true(all(skeleton_rows$label == ""))
  expect_true(all(skeleton_rows[["point.size"]] == 0))
})

test_that("StatNodesRepel n_node_points = 0 disables skeleton", {
  test_data <- data.frame(
    x = c(1, 3),
    y = c(1, 3),
    label = c("a", "b"),
    PANEL = c(1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0, n_node_points = 0),
    NULL
  )

  # Only 2 node rows, no skeleton
  expect_equal(nrow(result), 2)
  expect_true(all(result$label != ""))
})

test_that("StatNodesRepel skeleton preserves PANEL", {
  test_data <- data.frame(
    x = c(1, 3, 5, 7),
    y = c(1, 3, 1, 3),
    label = c("a", "b", "c", "d"),
    PANEL = c(1, 1, 2, 2),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0, n_node_points = 4),
    NULL
  )

  skeleton_rows <- result[result$label == "", ]
  # Each panel: 2 nodes * 4 points = 8 skeleton points
  expect_equal(sum(skeleton_rows$PANEL == 1), 8)
  expect_equal(sum(skeleton_rows$PANEL == 2), 8)
})

test_that("StatNodesRepel skeleton radius scales with node_size", {
  test_data <- data.frame(
    x = c(0, 2),
    y = c(0, 2),
    label = c("a", "b"),
    PANEL = c(1, 1),
    stringsAsFactors = FALSE
  )

  result_small <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0, n_node_points = 4, node_size = 8),
    NULL
  )
  result_large <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0, n_node_points = 4, node_size = 32),
    NULL
  )

  # Extract skeleton x values for first node (at x=0)
  skel_small <- result_small[result_small$label == "", ]
  skel_large <- result_large[result_large$label == "", ]

  # Larger node_size should produce skeleton at greater distance from center
  max_dist_small <- max(sqrt(skel_small$x^2 + skel_small$y^2))
  max_dist_large <- max(sqrt(skel_large$x^2 + skel_large$y^2))
  expect_gt(max_dist_large, max_dist_small)
})

test_that("StatNodesRepel skeleton + edge fake points coexist", {
  test_data <- data.frame(
    x = c(1, 3),
    y = c(1, 3),
    xend = c(3, 1),
    yend = c(3, 1),
    label = c("a", "b"),
    PANEL = c(1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 5, n_node_points = 4),
    NULL
  )

  fake_rows <- result[result$label == "", ]
  # 2 edges * 5 edge points + 2 nodes * 4 skeleton points = 18
  expect_equal(nrow(fake_rows), 18)

  # Total: 2 real nodes + 18 fake = 20
  expect_equal(nrow(result), 20)
})

test_that("StatNodesRepel skeleton skipped for single node", {
  test_data <- data.frame(
    x = 1,
    y = 1,
    label = "a",
    PANEL = 1,
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0, n_node_points = 12),
    NULL
  )

  # Only 1 node, no skeleton (can't estimate radius with single node)
  expect_equal(nrow(result), 1)
})

test_that("StatNodesRepel default n_node_points is 12", {
  test_data <- data.frame(
    x = c(1, 3),
    y = c(1, 3),
    label = c("a", "b"),
    PANEL = c(1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0),
    NULL
  )

  skeleton_rows <- result[result$label == "", ]
  # 2 nodes * 12 points = 24
  expect_equal(nrow(skeleton_rows), 24)
})

test_that("dag_layer() creates correct S3 class", {
  # Create a minimal layer
  layer <- ggplot2::layer(
    data = NULL,
    mapping = ggplot2::aes(),
    stat = "identity",
    geom = ggrepel::GeomTextRepel,
    position = "identity",
    params = list(na.rm = FALSE)
  )

  result <- dag_layer(layer, discover = "node_size")
  expect_s3_class(result, "dag_layer")
  expect_identical(result$layer, layer)
  expect_equal(result$discover, "node_size")
})

test_that("ggplot_add.dag_layer discovers node_size from GeomDagNode", {
  p <- ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    geom_dag_node(size = 25)

  layer <- ggplot2::layer(
    data = NULL,
    mapping = ggplot2::aes(),
    stat = StatNodesRepel,
    geom = ggrepel::GeomTextRepel,
    position = "identity",
    params = list(na.rm = FALSE, node_size = NULL)
  )

  wrapped <- dag_layer(layer, discover = "node_size")
  result <- ggplot_add.dag_layer(wrapped, p)

  # The repel layer should now have node_size = 25
  repel_layer <- result@layers[[2]]
  expect_equal(repel_layer$stat_params$node_size, 25)
})

test_that("ggplot_add.dag_layer discovers node_size from GeomDagPoint", {
  p <- ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    geom_dag_point(size = 30)

  layer <- ggplot2::layer(
    data = NULL,
    mapping = ggplot2::aes(),
    stat = StatNodesRepel,
    geom = ggrepel::GeomTextRepel,
    position = "identity",
    params = list(na.rm = FALSE, node_size = NULL)
  )

  wrapped <- dag_layer(layer, discover = "node_size")
  result <- ggplot_add.dag_layer(wrapped, p)

  repel_layer <- result@layers[[2]]
  expect_equal(repel_layer$stat_params$node_size, 30)
})

test_that("ggplot_add.dag_layer respects explicit node_size", {
  p <- ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    geom_dag_node(size = 25)

  layer <- ggplot2::layer(
    data = NULL,
    mapping = ggplot2::aes(),
    stat = StatNodesRepel,
    geom = ggrepel::GeomTextRepel,
    position = "identity",
    params = list(na.rm = FALSE, node_size = 50)
  )

  wrapped <- dag_layer(layer, discover = "node_size")
  result <- ggplot_add.dag_layer(wrapped, p)

  # Explicit node_size = 50 should NOT be overwritten by discovered 25
  repel_layer <- result@layers[[2]]
  expect_equal(repel_layer$stat_params$node_size, 50)
})

test_that("ggplot_add.dag_layer falls back when no node layer exists", {
  p <- ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y))

  layer <- ggplot2::layer(
    data = NULL,
    mapping = ggplot2::aes(),
    stat = StatNodesRepel,
    geom = ggrepel::GeomTextRepel,
    position = "identity",
    params = list(na.rm = FALSE, node_size = NULL)
  )

  wrapped <- dag_layer(layer, discover = "node_size")
  result <- ggplot_add.dag_layer(wrapped, p)

  # node_size should remain NULL; StatNodesRepel falls back to 16
  repel_layer <- result@layers[[1]]
  expect_null(repel_layer$stat_params$node_size)
})

test_that("We do not need to update `silent_add()`.", {
  # This is a sentinel test to see if upstream ggplot2 has made changes to
  # the ggplot2:::Scales$add() method.
  # If this test fails, the add method has likely changed and `silent_add()`
  # may need to be updated in StatsandGeoms.R.
  body <- body(environment(ggplot()$scales$add)$f)
  expect_snapshot(body)
})
