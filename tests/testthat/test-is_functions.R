test_that("is_acyclic() correctly identifies acyclic graphs", {
  # Acyclic DAG
  dag1 <- dagify(y ~ x + z, x ~ z)
  expect_true(is_acyclic(dag1))

  # Cyclic graph
  dag2 <- dagitty::dagitty("dag { x -> y -> z -> x }")
  expect_false(is_acyclic(dag2))

  # Works with tidy_dagitty
  tidy_dag <- tidy_dagitty(dag1)
  expect_true(is_acyclic(tidy_dag))
})

test_that("is_adjustment_set() correctly identifies valid adjustment sets", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  # Valid adjustment set
  expect_true(is_adjustment_set(dag, "z"))

  # Empty set is not valid in this case
  expect_false(is_adjustment_set(dag, character(0)))

  # Can specify exposure and outcome explicitly
  expect_true(is_adjustment_set(dag, "z", exposure = "x", outcome = "y"))
})

test_that("is_d_separated() and is_d_connected() work correctly", {
  # Create a simple collider structure for clear d-separation
  dag <- dagify(
    m ~ x,
    m ~ y,
    exposure = "x",
    outcome = "y"
  )

  # x and y are d-separated (no conditioning)
  expect_true(is_d_separated(dag, "x", "y", character(0)))
  expect_false(is_d_connected(dag, "x", "y", character(0)))

  # x and y are d-connected when conditioning on collider m
  expect_false(is_d_separated(dag, "x", "y", "m"))
  expect_true(is_d_connected(dag, "x", "y", "m"))

  # Test with NULL from/to (should use exposure/outcome)
  expect_true(is_d_separated(dag, controlling_for = character(0)))

  # Test error when no exposure/outcome
  dag_no_exp <- dagify(m ~ x, m ~ y)
  expect_error(
    is_d_separated(dag_no_exp, controlling_for = "z"),
    class = "ggdag_error"
  )
})

test_that("is_exogenous() correctly identifies exogenous variables", {
  dag <- dagify(y ~ x + z, x ~ z)

  # z has no parents, so it's exogenous
  expect_true(is_exogenous(dag, "z"))

  # x and y have parents, so they're not exogenous
  expect_false(is_exogenous(dag, "x"))
  expect_false(is_exogenous(dag, "y"))

  # Test with invalid node
  expect_error(is_exogenous(dag, "invalid"))
})

test_that("is_instrumental() correctly identifies instrumental variables", {
  # Classic IV example
  dag <- dagitty::dagitty("dag { i -> x -> y ; x <-> y }")
  dagitty::exposures(dag) <- "x"
  dagitty::outcomes(dag) <- "y"

  expect_true(is_instrumental(dag, "i"))
  expect_false(is_instrumental(dag, "x"))
  expect_false(is_instrumental(dag, "y"))
})

test_that("variable status functions work correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    latent = "z"
  )

  # Test is_exposure()
  expect_true(is_exposure(dag, "x"))
  expect_false(is_exposure(dag, "y"))
  expect_false(is_exposure(dag, "z"))

  # Test is_outcome()
  expect_false(is_outcome(dag, "x"))
  expect_true(is_outcome(dag, "y"))
  expect_false(is_outcome(dag, "z"))

  # Test is_latent()
  expect_false(is_latent(dag, "x"))
  expect_false(is_latent(dag, "y"))
  expect_true(is_latent(dag, "z"))
})

test_that("node relationship functions work correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    m ~ x
  )

  # Test is_parent()
  expect_true(is_parent(dag, "z", "x"))
  expect_true(is_parent(dag, "x", "y"))
  expect_false(is_parent(dag, "y", "x"))

  # Test is_child()
  expect_true(is_child(dag, "x", "z"))
  expect_true(is_child(dag, "y", "x"))
  expect_false(is_child(dag, "x", "y"))

  # Test is_ancestor()
  expect_true(is_ancestor(dag, "z", "y")) # z -> x -> y
  expect_true(is_ancestor(dag, "z", "m")) # z -> x -> m
  expect_false(is_ancestor(dag, "y", "z"))

  # Test is_descendant()
  expect_true(is_descendant(dag, "y", "z"))
  expect_true(is_descendant(dag, "m", "z"))
  expect_false(is_descendant(dag, "z", "y"))

  # Test is_adjacent()
  expect_true(is_adjacent(dag, "x", "y"))
  expect_true(is_adjacent(dag, "y", "x")) # adjacency is symmetric
  # Note: dagitty considers nodes adjacent if they're connected through any path
  expect_true(is_adjacent(dag, "z", "y"))
})

test_that("functions handle tidy_dagitty objects", {
  dag <- dagify(
    m ~ x,
    m ~ y,
    exposure = "x",
    outcome = "y"
  )
  tidy_dag <- tidy_dagitty(dag)

  # All functions should work with tidy_dagitty
  expect_true(is_acyclic(tidy_dag))
  expect_true(is_d_separated(tidy_dag, "x", "y", character(0)))
  expect_true(is_exposure(tidy_dag, "x"))
  expect_true(is_outcome(tidy_dag, "y"))
  expect_false(is_parent(tidy_dag, "x", "y"))
  expect_false(is_ancestor(tidy_dag, "x", "y"))
  expect_true(is_adjacent(tidy_dag, "x", "m"))
})

test_that("controlling_for accepts different formats", {
  # Simple collider structure
  dag <- dagify(m ~ x, m ~ y, exposure = "x", outcome = "y")

  # Character vector
  expect_false(is_d_separated(dag, controlling_for = "m"))
  expect_false(is_d_separated(dag, controlling_for = c("m")))

  # List format
  expect_false(is_d_separated(dag, controlling_for = list("m")))

  # NULL/empty (should be d-separated without conditioning)
  expect_true(is_d_separated(dag, controlling_for = NULL))
  expect_true(is_d_separated(dag, controlling_for = character(0)))
})
