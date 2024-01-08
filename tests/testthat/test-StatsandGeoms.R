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

test_that("We do not need to update `silent_add()`.", {
  # This is a sentinel test to see if upstream ggplot2 has made changes to
  # the ggplot2:::Scales$add() method.
  # If this test fails, the add method has likely changed and `silent_add()`
  # may need to be updated in StatsandGeoms.R.
  body <- body(environment(ggplot()$scales$add)$f)
  expect_snapshot(body)
})
