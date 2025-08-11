set.seed(1234)

test_that("dags cannonicalize correctly", {
  # Get edge count for all tests
  n_edges <- count_dag_edges(test_dag)

  p1 <- ggdag_children(test_dag, "w1")
  expect_doppelganger(
    "ggdag_children() identifies `y`, `x`, and `z1`",
    p1
  )
  expect_edge_count(p1, n_edges, "ggdag_children")

  p2 <- ggdag_parents(test_dag, "y")
  expect_doppelganger(
    "ggdag_parents() identifies `z2`, `x`, `w1`, and `w2`",
    p2
  )
  expect_edge_count(p2, n_edges, "ggdag_parents")

  p3 <- ggdag_ancestors(test_dag, "x")
  expect_doppelganger(
    "ggdag_ancestors() identifies `v`, `w1`, and `z1`",
    p3
  )
  expect_edge_count(p3, n_edges, "ggdag_ancestors")

  p4 <- ggdag_descendants(test_dag, "w1")
  expect_doppelganger(
    "ggdag_descendants() identifies `y`, `x`, and `z1`",
    p4
  )
  expect_edge_count(p4, n_edges, "ggdag_descendants")
})
