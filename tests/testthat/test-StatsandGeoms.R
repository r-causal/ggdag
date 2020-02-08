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
  expect_ggproto(ScalesListQuiet)
})
