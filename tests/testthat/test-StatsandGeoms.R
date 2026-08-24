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

  point_size_for <- function(...) {
    result <- StatNodesRepel$compute_layer(
      test_data,
      list(n_edge_points = 0, n_node_points = 0, ...),
      NULL
    )
    expect_true("point.size" %in% names(result))
    result[["point.size"]]
  }

  # every node row carries the same point.size, and it grows with node_size
  size_20 <- point_size_for(node_size = 20)
  expect_length(unique(size_20), 1)
  expect_length(size_20, 3)
  expect_gt(size_20[1], point_size_for(node_size = 16)[1])

  # with default (NULL) params, node_size = 16 is used
  expect_equal(point_size_for(), point_size_for(node_size = 16))
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

  without_fake_points <- StatNodesRepel$compute_layer(
    test_data,
    list(node_size = 20, n_edge_points = 0, n_node_points = 0),
    NULL
  )

  node_rows <- result[result$label != "", ]
  fake_rows <- result[result$label == "", ]

  # Nodes get the same node_size-based point.size either way
  expect_equal(
    node_rows[["point.size"]],
    without_fake_points[["point.size"]]
  )
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

  # 3 nodes + 3 nodes * 25 disc points = 78
  expect_equal(nrow(result), 78)

  skeleton_rows <- result[result$label == "", ]
  expect_equal(nrow(skeleton_rows), 75)
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
  expect_equal(nrow(skeleton_rows), 50)
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
  # Each panel: 2 nodes * 25 disc points = 50 skeleton points
  expect_equal(sum(skeleton_rows$PANEL == 1), 50)
  expect_equal(sum(skeleton_rows$PANEL == 2), 50)
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
  # 2 edges * 5 edge points + 2 nodes * 25 disc points = 60
  expect_equal(nrow(fake_rows), 60)

  # Total: 2 real nodes + 60 fake = 62
  expect_equal(nrow(result), 62)
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
  # 2 nodes * 25 disc points = 50
  expect_equal(nrow(skeleton_rows), 50)
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
  repel_layer <- result$layers[[2]]
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

  repel_layer <- result$layers[[2]]
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
  repel_layer <- result$layers[[2]]
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
  repel_layer <- result$layers[[1]]
  expect_null(repel_layer$stat_params$node_size)
})

test_that("one stored repel layer reads each plot it joins", {
  # what a layer learns from the plot it is added to -- the node size and the
  # curve each edge is drawn along -- must not follow the object to the next
  # plot.
  arced_dag <- function(m_x, m_y) {
    tidy_dagitty(dagify(
      y ~ x,
      m ~ ~x,
      coords = list(
        x = c(x = 0, y = 2 * m_x, m = m_x),
        y = c(x = 0, y = 0, m = m_y)
      )
    ))
  }

  repel_layer <- geom_dag_label_repel(aes(label = name), seed = 1234)
  plot_with <- function(tidy_dag, node_size) {
    ggplot(tidy_dag, aes_dag()) +
      geom_dag_edges() +
      geom_dag_point(size = node_size) +
      repel_layer
  }

  first <- plot_with(arced_dag(1, 1.5), 16)
  second <- plot_with(arced_dag(3, 3), 24)

  repel_stat_params <- function(p) {
    stats <- vapply(p$layers, function(l) class(l$stat)[1], character(1))
    p$layers[[which(stats == "StatNodesRepel")[1]]]$stat_params
  }

  first_geometry <- repel_stat_params(first)$edge_geometry
  second_geometry <- repel_stat_params(second)$edge_geometry

  expect_equal(first_geometry$x, 1)
  expect_equal(first_geometry$y, 1.5)
  expect_equal(second_geometry$x, 3)
  expect_equal(second_geometry$y, 3)

  expect_equal(repel_stat_params(first)$node_size, 16)
  expect_equal(repel_stat_params(second)$node_size, 24)

  # the stored layer is still the blank one that was created
  expect_null(repel_layer$layer$stat_params$edge_geometry)
  expect_null(repel_layer$layer$stat_params$node_size)
})

test_that("StatNodesRepel protects nodes whose label is missing", {
  # `m` sits at (1, 1) with no label. It is still drawn as a node, so the
  # repulsion geometry has to cover it: a disc of skeleton points and a row
  # carrying the node's point.size.
  test_data <- data.frame(
    x = c(0, 1, 2),
    y = c(0, 1, 0),
    xend = c(1, 2, NA),
    yend = c(1, 0, NA),
    label = c("a", NA, "b"),
    PANEL = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0, n_node_points = 12),
    NULL
  )

  at_node <- function(res, x, y) {
    res[abs(res$x - x) < 1e-9 & abs(res$y - y) < 1e-9, , drop = FALSE]
  }

  expect_gt(nrow(at_node(result, 1, 1)), 0)
  expect_true(any(at_node(result, 1, 1)[["point.size"]] > 0))

  # the labelled nodes keep the geometry they already had
  expect_gt(nrow(at_node(result, 0, 0)), 0)
  expect_gt(nrow(at_node(result, 2, 0)), 0)

  # ggrepel still draws only the two real labels
  expect_setequal(setdiff(result$label, ""), c("a", "b"))
})

test_that("StatNodesRepel keeps the skeleton when one node is labelled", {
  # Only `a` is labelled, but the layer still draws three nodes, so the disc
  # skeleton must be built from all of them rather than from the single row
  # that survives label filtering.
  test_data <- data.frame(
    x = c(0, 1, 2),
    y = c(0, 1, 0),
    xend = c(1, 2, NA),
    yend = c(1, 0, NA),
    label = c("a", NA, NA),
    PANEL = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_edge_points = 0, n_node_points = 12),
    NULL
  )

  skeleton <- result[result$label == "", , drop = FALSE]
  expect_gt(nrow(skeleton), 0)
  # the labelled node is surrounded, not just marked
  near_a <- skeleton[
    sqrt(skeleton$x^2 + skeleton$y^2) < 0.5,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(near_a), 1)
})

test_that("partially labelled DAGs keep repulsion geometry at every node", {
  g <- dagify(
    m ~ x + y,
    y ~ x,
    labels = c(x = "Exposure", y = "Outcome"),
    coords = list(x = c(x = 0, y = 2, m = 1), y = c(x = 0, y = 0, m = 1))
  )
  tidy_dag <- tidy_dagitty(g)

  p <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_label_repel(aes(label = label), seed = 1234)

  stats <- vapply(p$layers, function(l) class(l$stat)[1], character(1))
  repel_data <- ggplot2::layer_data(p, which(stats == "StatNodesRepel")[1])

  nodes <- unique(pull_dag_data(tidy_dag)[, c("name", "x", "y")])
  at_centre <- vapply(
    seq_len(nrow(nodes)),
    function(i) {
      sum(
        abs(repel_data$x - nodes$x[i]) < 1e-9 &
          abs(repel_data$y - nodes$y[i]) < 1e-9
      )
    },
    integer(1)
  )

  expect_true(all(at_centre > 0))
})

# Largest distance from a point on a drawn edge to the nearest repulsion
# point. Edge ends are excluded so the node discs cannot mask the gap.
edge_protection_gap <- function(plot, edge_stat, from, to, end_buffer = 0.4) {
  stats <- vapply(plot$layers, function(l) class(l$stat)[1], character(1))
  drawn <- ggplot2::layer_data(plot, which(stats == edge_stat)[1])
  repel_data <- ggplot2::layer_data(
    plot,
    which(stats == "StatNodesRepel")[1]
  )
  fake <- repel_data[repel_data$label == "", , drop = FALSE]

  from_ends <- pmin(
    sqrt((drawn$x - from[1])^2 + (drawn$y - from[2])^2),
    sqrt((drawn$x - to[1])^2 + (drawn$y - to[2])^2)
  )
  interior <- drawn[from_ends > end_buffer, , drop = FALSE]

  max(vapply(
    seq_len(nrow(interior)),
    function(i) {
      min(sqrt((fake$x - interior$x[i])^2 + (fake$y - interior$y[i])^2))
    },
    numeric(1)
  ))
}

# Spacing between consecutive fake points along an edge of this length.
fake_point_spacing <- function(from, to, n_edge_points = 50) {
  sqrt((to[1] - from[1])^2 + (to[2] - from[2])^2) / (n_edge_points + 1)
}

test_that("repel fake points follow a drawn bidirected arc", {
  # geom_dag_edges() draws every bidirected edge as an arc, so interpolating
  # along the straight chord leaves the drawn curve unprotected.
  g <- dagify(
    y ~ x,
    m ~ ~x,
    coords = list(x = c(x = 0, y = 2, m = 1), y = c(x = 0, y = 0, m = 1.5))
  )

  p <- ggplot(tidy_dagitty(g), aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_label_repel(aes(label = name), seed = 1234)

  from <- c(0, 0)
  to <- c(1, 1.5)
  expect_lt(
    edge_protection_gap(p, "StatEdgeArc", from, to),
    1.5 * fake_point_spacing(from, to)
  )
})

test_that("repel fake points follow drawn arc edges", {
  g <- dagify(
    y ~ x,
    coords = list(x = c(x = 0, y = 2), y = c(x = 0, y = 0))
  )

  p <- ggplot(tidy_dagitty(g), aes_dag()) +
    geom_dag_edges_arc() +
    geom_dag_point() +
    geom_dag_label_repel(aes(label = name), seed = 1234)

  from <- c(0, 0)
  to <- c(2, 0)
  expect_lt(
    edge_protection_gap(p, "StatEdgeArc", from, to),
    1.5 * fake_point_spacing(from, to)
  )
})

test_that("StatNodesRepel declares xend and yend as optional aesthetics", {
  g <- dagify(
    m ~ x + y,
    y ~ x,
    coords = list(x = c(x = 0, y = 2, m = 1), y = c(x = 0, y = 0, m = 1))
  )
  tidy_dag <- tidy_dagitty(g)

  # xend and yend are used by the stat, so mapping them explicitly must not
  # be reported as unknown
  expect_no_warning(
    explicit <- geom_dag_label_repel(
      mapping = aes_dag(label = name),
      inherit.aes = FALSE,
      seed = 1234
    )
  )

  p_explicit <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    explicit
  p_inherited <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_label_repel(aes(label = name), seed = 1234)

  repel_rows <- function(p) {
    stats <- vapply(p$layers, function(l) class(l$stat)[1], character(1))
    nrow(ggplot2::layer_data(p, which(stats == "StatNodesRepel")[1]))
  }

  expect_equal(repel_rows(p_explicit), repel_rows(p_inherited))
})

test_that("StatNodesRepel point.size covers the drawn node", {
  # ggrepel turns point.size into the segment-endpoint radius as
  # `point.size * .pt / .stroke / 20` centimetres, while a pch-19 node of
  # size `s` draws with a radius of `0.375 * s` millimetres. Labels are
  # buried under large nodes because the emitted point.size describes a
  # much smaller circle than the one on the page.
  test_data <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 1, 2),
    xend = c(2, 3, 1),
    yend = c(1, 2, 1),
    label = c("a", "b", "c"),
    PANEL = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  segment_radius_cm <- function(point_size) {
    point_size * ggplot2::.pt / ggplot2::.stroke / 20
  }
  node_radius_cm <- function(node_size) 0.375 * node_size / 10

  for (node_size in c(25, 40)) {
    result <- StatNodesRepel$compute_layer(
      test_data,
      list(node_size = node_size, n_edge_points = 0, n_node_points = 0),
      NULL
    )
    radius <- segment_radius_cm(result[["point.size"]][1])
    expect_gte(radius, node_radius_cm(node_size))
    # and not so large that labels are flung off the panel
    expect_lt(radius, 2 * node_radius_cm(node_size))
  }
})

test_that("StatEdgeFan fans only edges that share a node pair", {
  # a -> z and b -> w have no node in common, so neither belongs to a fan and
  # both must be drawn straight. Ranking `from` and `to` independently gives
  # the two edges the same pair id, and ggraph then curves them apart.
  dag <- dagify(
    z ~ a,
    w ~ b,
    coords = list(
      x = c(a = 0, b = 0, z = 1, w = 1),
      y = c(a = 0, b = 1, z = 0, w = 1)
    )
  )

  edge_data <- ggplot2::layer_data(
    ggplot(dag, aes_dag()) + geom_dag_edges_fan(),
    1
  )

  # both edges are horizontal, at y = 0 and y = 1
  expect_lt(max(abs(edge_data$y - round(edge_data$y))), 1e-6)
})

test_that("StatEdgeFan still fans genuine parallel edges", {
  dag <- dagify(
    y ~ x,
    y ~ ~x,
    coords = list(x = c(x = 0, y = 1), y = c(x = 0, y = 0))
  )

  edge_data <- ggplot2::layer_data(
    ggplot(dag, aes_dag()) + geom_dag_edges_fan(),
    1
  )

  # the two edges bow away from the straight line between x and y
  expect_gt(max(abs(edge_data$y)), 0.01)
  expect_equal(length(unique(edge_data$group)), 2)
})

test_that("handle_missing_circular_column handles zero-row data", {
  empty <- data.frame(x = numeric(), y = numeric())
  result <- handle_missing_circular_column(empty)
  expect_true("circular" %in% names(result))
  expect_equal(nrow(result), 0)

  # non-empty behaviour is unchanged
  filled <- data.frame(x = 1:2, circular = c(NA, TRUE))
  expect_equal(handle_missing_circular_column(filled)$circular, c(FALSE, TRUE))
})

test_that("edge stats draw an empty layer when the DAG has no edges", {
  dag <- tidy_dagitty(dagitty::dagitty("dag { x }"))

  edge_geoms <- list(
    link = geom_dag_edges_link,
    arc = geom_dag_edges_arc,
    diagonal = geom_dag_edges_diagonal,
    fan = geom_dag_edges_fan
  )

  for (edge_geom in edge_geoms) {
    p <- ggplot(dag, aes_dag()) + edge_geom()
    expect_no_error(ggplot2::ggplot_build(p))
    edge_rows <- tryCatch(
      nrow(ggplot2::ggplot_build(p)$data[[1]]),
      error = function(e) NA_integer_
    )
    expect_equal(edge_rows, 0)
  }
})

test_that("edge stats still draw edges for a DAG that has them", {
  dag <- tidy_dagitty(dagify(
    y ~ x,
    m ~ ~x,
    coords = list(x = c(x = 0, y = 2, m = 1), y = c(x = 0, y = 0, m = 1))
  ))

  edge_geoms <- list(
    link = geom_dag_edges_link,
    arc = geom_dag_edges_arc,
    diagonal = geom_dag_edges_diagonal,
    fan = geom_dag_edges_fan
  )

  for (edge_geom in edge_geoms) {
    p <- ggplot(dag, aes_dag()) + edge_geom()
    built <- ggplot2::ggplot_build(p)
    expect_equal(length(unique(built$data[[1]]$group)), 2)
  }
})

test_that("We do not need to update `silent_add()`.", {
  # This is a sentinel test to see if upstream ggplot2 has made changes to
  # the ggplot2:::Scales$add() method.
  # If this test fails, the add method has likely changed and `silent_add()`
  # may need to be updated in StatsandGeoms.R.
  body <- body(environment(ggplot()$scales$add)$f)
  expect_snapshot(body)
})

test_that("StatNodes keeps one row per node, preferring the marked one", {
  node_data <- data.frame(
    x = c(0, 0, 1, 1),
    y = c(0, 0, 1, 1),
    xend = c(1, NA, NA, NA),
    yend = c(1, NA, NA, NA),
    PANEL = factor(1),
    group = c(1L, 2L, 1L, 2L),
    colour = c("direct", NA, NA, "direct"),
    stringsAsFactors = FALSE
  )

  result <- StatNodes$compute_layer(node_data, NULL, list())

  expect_equal(nrow(result), 2)
  # the row carrying the analysis value is the one drawn
  expect_equal(result$colour, c("direct", "direct"))
  expect_equal(result$x, c(0, 1))
})

test_that("StatNodes keeps a node in each panel it appears in", {
  node_data <- data.frame(
    x = c(0, 0, 0),
    y = c(0, 0, 0),
    PANEL = factor(c(1, 1, 2)),
    group = 1L,
    colour = c("direct", NA, NA),
    stringsAsFactors = FALSE
  )

  result <- StatNodes$compute_layer(node_data, NULL, list())

  expect_equal(nrow(result), 2)
  expect_equal(as.character(result$PANEL), c("1", "2"))
  expect_equal(result$colour, c("direct", NA))
})

test_that("StatNodes keeps two nodes that share coordinates", {
  # a coordinate typo puts two nodes in one place; both are still drawn, so the
  # typo is visible rather than one node quietly going missing
  dag <- dagify(
    c ~ a,
    c ~ b,
    coords = list(
      x = c(a = 0, b = 0, c = 1),
      y = c(a = 0, b = 0, c = 1)
    )
  )

  p <- ggplot2::ggplot(dag, aes_dag()) +
    geom_dag_point() +
    geom_dag_text()

  expect_setequal(built_text_labels(p), c("a", "b", "c"))
})

test_that("StatNodes tells two labelled nodes at one position apart", {
  node_data <- data.frame(
    x = c(0, 0, 0),
    y = c(0, 0, 0),
    PANEL = factor(1),
    group = c(1L, 2L, 3L),
    label = c("a", "a", "b"),
    colour = c("direct", NA, NA),
    stringsAsFactors = FALSE
  )

  result <- StatNodes$compute_layer(node_data, NULL, list())

  expect_equal(nrow(result), 2)
  expect_equal(result$label, c("a", "b"))
  # the duplicate rows of one node still collapse to the marked one
  expect_equal(result$colour, c("direct", NA))
})

test_that("StatNodesRepel draws one label per node when alpha is mapped", {
  # rows of one node can differ in an edge-level aesthetic, so they survive
  # `unique()` and would each repel a label of their own
  test_data <- data.frame(
    x = c(0, 0, 1),
    y = c(0, 0, 1),
    xend = c(1, 1, NA),
    yend = c(1, 1, NA),
    label = c("x", "x", "y"),
    alpha = c(1, 1, 1),
    group = c(1L, 2L, 1L),
    PANEL = factor(1),
    stringsAsFactors = FALSE
  )

  result <- StatNodesRepel$compute_layer(
    test_data,
    list(n_node_points = 0, n_edge_points = 0),
    NULL
  )

  labelled <- result[result$label != "", ]
  expect_equal(nrow(labelled), 2)
  expect_setequal(labelled$label, c("x", "y"))
})
