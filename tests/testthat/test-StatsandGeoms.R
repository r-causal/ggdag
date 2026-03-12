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
  result <- StatNodesRepel$compute_layer(test_data, list(), NULL)

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
  result <- StatNodesRepel$compute_layer(test_data, list(), NULL)

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
    list(node_size = 20),
    NULL
  )
  expect_true("point.size" %in% names(result))
  expect_equal(result[["point.size"]], rep(20 * ggplot2::.pt / 14.4, 3))

  # With default (NULL) params — should use node_size = 16

  result_default <- StatNodesRepel$compute_layer(test_data, list(), NULL)
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
    list(node_size = 16),
    NULL
  )
  # Should preserve user-mapped values

  expect_equal(result[["point.size"]], c(5, 10))
})

test_that("We do not need to update `silent_add()`.", {
  # This is a sentinel test to see if upstream ggplot2 has made changes to
  # the ggplot2:::Scales$add() method.
  # If this test fails, the add method has likely changed and `silent_add()`
  # may need to be updated in StatsandGeoms.R.
  body <- body(environment(ggplot()$scales$add)$f)
  expect_snapshot(body)
})
