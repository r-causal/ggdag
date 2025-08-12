# Comprehensive edge counting tests for all ggdag visualization functions
# This ensures exact edge counts for all plot types

test_that("ggdag_adjustment_set edge counts are correct", {
  # Simple DAG with one adjustment set
  simple_dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  n_edges <- count_dag_edges(simple_dag)
  n_sets <- count_adjustment_sets(simple_dag)

  # Test default behavior
  p1 <- ggdag_adjustment_set(simple_dag)
  expect_edge_count(p1, n_edges * n_sets, "simple DAG adjustment set")

  # Test with use_edges = FALSE
  p2 <- ggdag_adjustment_set(simple_dag, use_edges = FALSE)
  expect_edge_count(p2, 0, "adjustment set with use_edges=FALSE")

  # Complex DAG with multiple adjustment sets
  complex_dag <- dagify(
    y ~ x + a + b,
    x ~ a + b,
    a ~ c,
    b ~ c,
    exposure = "x",
    outcome = "y"
  )

  n_edges_complex <- count_dag_edges(complex_dag)
  n_sets_complex <- count_adjustment_sets(complex_dag)

  p3 <- ggdag_adjustment_set(complex_dag)
  expect_edge_count(
    p3,
    n_edges_complex * n_sets_complex,
    "complex DAG adjustment sets"
  )
})

test_that("ggdag_paths edge counts respect shadow parameter", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  n_edges <- count_dag_edges(dag)
  n_open_paths <- count_open_paths(dag, "x", "y")
  n_edges_in_paths <- count_edges_in_open_paths(dag, "x", "y")

  # Test with shadow = TRUE (default)
  p1 <- ggdag_paths(dag, from = "x", to = "y", shadow = TRUE)
  expect_edge_count(p1, n_edges * n_open_paths, "paths with shadow=TRUE")

  # Test with shadow = FALSE
  p2 <- ggdag_paths(dag, from = "x", to = "y", shadow = FALSE)
  expect_edge_count(p2, n_edges_in_paths, "paths with shadow=FALSE")

  # Test with use_edges = FALSE
  p3 <- ggdag_paths(dag, from = "x", to = "y", use_edges = FALSE)
  expect_edge_count(p3, 0, "paths with use_edges=FALSE")
})

test_that("ggdag_paths_fan edge counts are correct", {
  dag <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2,
    exposure = "x",
    outcome = "y"
  )

  n_edges <- count_dag_edges(dag)
  n_open_paths <- count_open_paths(dag, "x", "y")
  n_edges_in_paths <- count_edges_in_open_paths(dag, "x", "y")

  # Test with shadow = TRUE
  p1 <- ggdag_paths_fan(dag, from = "x", to = "y", shadow = TRUE)
  expect_edge_count(p1, n_edges * n_open_paths, "paths_fan with shadow=TRUE")

  # Test with shadow = FALSE
  p2 <- ggdag_paths_fan(dag, from = "x", to = "y", shadow = FALSE)
  expect_edge_count(p2, n_edges_in_paths, "paths_fan with shadow=FALSE")

  # Test with use_edges = FALSE
  p3 <- ggdag_paths_fan(dag, from = "x", to = "y", use_edges = FALSE)
  expect_edge_count(p3, 0, "paths_fan with use_edges=FALSE")
})

test_that("ggdag_collider edge counts are correct", {
  # DAG with colliders
  collider_dag <- dagify(
    m ~ x + y,
    x ~ z,
    y ~ z
  )

  n_edges <- count_dag_edges(collider_dag)
  n_colliders <- count_colliders(collider_dag)

  # Test default behavior
  p1 <- ggdag_collider(collider_dag)
  expect_edge_count(p1, n_edges, "collider default")

  # Test with use_edges = FALSE
  p2 <- ggdag_collider(collider_dag, use_edges = FALSE)
  expect_edge_count(p2, 0, "collider with use_edges=FALSE")
})

test_that("ggdag_equivalent_class edge counts are correct", {
  # DAG with reversible edges
  dag <- dagify(
    y ~ x + z,
    x ~ z
  )

  n_edges <- count_dag_edges(dag)

  # Test with use_edges = TRUE
  # ggdag_equivalent_class shows the base DAG with reversible edges marked
  p1 <- ggdag_equivalent_class(dag, use_edges = TRUE)
  expect_edge_count(p1, n_edges, "equivalent class with use_edges=TRUE")

  # Test with use_edges = FALSE
  p2 <- ggdag_equivalent_class(dag, use_edges = FALSE)
  expect_edge_count(p2, 0, "equivalent class with use_edges=FALSE")
})

test_that("ggdag single panel functions have correct edge counts", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    z ~ w,
    exposure = "x",
    outcome = "y"
  )

  n_edges <- count_dag_edges(dag)

  # Test ggdag()
  p_ggdag <- ggdag(dag)
  expect_edge_count(p_ggdag, n_edges, "ggdag()")

  # Test ggdag_classic()
  p_classic <- ggdag_classic(dag)
  expect_edge_count(p_classic, n_edges, "ggdag_classic()")

  # Test ggdag_canonical() - requires a DAG with bidirected edges
  dag_bidirected <- dagify(
    y ~ x + z,
    x ~ ~z # bidirected edge
  )
  # Canonical transformation converts bidirected edges to latent nodes
  # Original: 3 edges (including 1 bidirected)
  # Canonical: 4 edges (bidirected x<->z becomes x<-L1->z)
  p_canonical <- ggdag_canonical(dag_bidirected, seed = 1234)
  expect_edge_count(p_canonical, 4, "ggdag_canonical()")

  # Test ggdag_status()
  p_status <- ggdag_status(dag)
  expect_edge_count(p_status, n_edges, "ggdag_status()")

  # Test ggdag_exogenous()
  p_exo <- ggdag_exogenous(dag)
  expect_edge_count(p_exo, n_edges, "ggdag_exogenous()")
})

test_that("ggdag_dconnected/dseparated edge counts are correct", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    m ~ x + y
  )

  n_edges <- count_dag_edges(dag)

  # Test d-connected
  p1 <- ggdag_dconnected(dag, "x", "y")
  expect_edge_count(p1, n_edges, "ggdag_dconnected()")

  # Test d-separated
  p2 <- ggdag_dseparated(dag, "x", "y", controlling_for = "z")
  expect_edge_count(p2, n_edges, "ggdag_dseparated()")

  # Test drelationship
  p3 <- ggdag_drelationship(dag, "x", "y", controlling_for = "z")
  expect_edge_count(p3, n_edges, "ggdag_drelationship()")
})

test_that("edge counts are zero when DAG has no edges", {
  # Create a truly empty DAG using dagitty directly
  empty_dag <- dagitty::dagitty("dag { x ; y }")
  n_edges <- count_dag_edges(empty_dag)
  expect_equal(n_edges, 0, info = "Empty DAG should have 0 edges")

  p1 <- ggdag(empty_dag)
  expect_edge_count(p1, 0, "empty DAG with unconnected nodes")

  # Single isolated node
  single_node <- dagitty::dagitty("dag { x }")
  n_edges_single <- count_dag_edges(single_node)
  expect_equal(n_edges_single, 0, info = "Single node should have 0 edges")

  p2 <- ggdag(single_node)
  expect_edge_count(p2, 0, "single isolated node")
})

test_that("edge counts handle bidirected edges correctly", {
  # DAG with bidirected edge
  dag <- dagify(
    y ~ x,
    x ~ ~y # bidirected edge
  )

  n_edges <- count_dag_edges(dag)
  expect_equal(n_edges, 2, info = "Should count bidirected edge")

  p <- ggdag(dag)
  expect_edge_count(p, n_edges, "DAG with bidirected edge")
})

test_that("edge counts are consistent across equivalent function calls", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  # These should all have the same edge count
  p1 <- ggdag(dag)
  p2 <- dag |> tidy_dagitty() |> ggdag()
  p3 <- ggdag(dag, use_edges = TRUE)

  analysis1 <- analyze_plot_edges(p1)
  analysis2 <- analyze_plot_edges(p2)
  analysis3 <- analyze_plot_edges(p3)

  expect_equal(analysis1$total_edges, analysis2$total_edges)
  expect_equal(analysis1$total_edges, analysis3$total_edges)
})
