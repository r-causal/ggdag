set.seed(1234)

test_that("dags identify IVs correctly", {
  dag <- dagitty::dagitty("dag{ i->x->y; i2->x->y; x<->y }")
  p <- ggdag_instrumental(dag, "x", "y")
  expect_doppelganger(
    "ggdag_instrumental() identifies `i` and `i2` as instrumental",
    p
  )

  # Add edge count test - instrumental plots are faceted by IV
  n_edges <- count_dag_edges(dag)
  # There are 2 IVs, so 2 panels
  expect_edge_count(p, n_edges * 2, "ggdag_instrumental with 2 IVs")
})

test_that("dags without IVs are shown correctly", {
  no_iv <- dagify(
    y ~ t + x1 + x2 + x4,
    t ~ x1 + x3,
    x2 ~ x3 + x4,
    exposure = "t",
    outcome = "y",
    latent = c("x1", "x4")
  )

  p <- ggdag_instrumental(no_iv)
  expect_doppelganger(
    "ggdag_instrumental() identifies nothing as instrumental",
    p
  )

  # Add edge count test
  n_edges <- count_dag_edges(no_iv)
  expect_edge_count(p, n_edges, "ggdag_instrumental with no IVs")
})

test_that("dags with colliders + IVs are shown correctly", {
  iv_collider <- dagify(
    y ~ t + x1 + x2 + x4,
    t ~ x1 + x3,
    x2 ~ x3 + x4,
    exposure = "t",
    outcome = "y",
    latent = c("x1")
  )

  p <- ggdag_instrumental(iv_collider)
  expect_doppelganger("ggdag_instrumental() instrumental plus collider", p)

  # Add edge count test
  n_edges <- count_dag_edges(iv_collider)
  expect_edge_count(p, n_edges, "ggdag_instrumental with collider")
})
