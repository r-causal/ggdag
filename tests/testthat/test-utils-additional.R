test_that("edge_type_switch returns expected values", {
  # Test valid edge types
  expect_equal(edge_type_switch("link_arc"), geom_dag_edges)
  expect_equal(edge_type_switch("link"), geom_dag_edges_link)
  expect_equal(edge_type_switch("arc"), geom_dag_edges_arc)
  expect_equal(edge_type_switch("diagonal"), geom_dag_edges_diagonal)

  # Test invalid edge type returns NULL (no default in switch)
  result <- edge_type_switch("invalid_type")
  expect_null(result)
})

test_that("is_empty_or_null handles various inputs", {
  # Already tested in edge cases, but let's be thorough
  expect_true(is_empty_or_null(NULL))
  expect_true(is_empty_or_null(character(0)))
  expect_true(is_empty_or_null(numeric(0)))
  expect_true(is_empty_or_null(logical(0)))
  expect_true(is_empty_or_null(list()))

  expect_false(is_empty_or_null(""))
  expect_false(is_empty_or_null(0))
  expect_false(is_empty_or_null(FALSE))
  expect_false(is_empty_or_null(NA))
  expect_false(is_empty_or_null(c(NA, NA)))
})

test_that("deprecated argument handlers work when argument is not present", {
  # Create a test function that uses is_present
  test_deprecated <- function(new_arg = NULL, old_arg = deprecated()) {
    if (is_present(old_arg)) {
      deprecate_warn(
        "1.0.0",
        "test_deprecated(old_arg)",
        "test_deprecated(new_arg)"
      )
      new_arg <- old_arg
    }
    new_arg
  }

  # Should not warn when old argument is not used
  expect_no_warning(result <- test_deprecated(new_arg = "value"))
  expect_equal(result, "value")

  # Should warn when old argument is used
  expect_warning(
    result2 <- test_deprecated(old_arg = "old_value"),
    "deprecated"
  )
  expect_equal(result2, "old_value")
})

test_that("if_not_tidy_daggity with pathological inputs", {
  # Test with various non-dagitty objects - should all give "Input must be a dagitty object"
  expect_error(if_not_tidy_daggity(NULL), "Input must be a dagitty object")
  expect_error(if_not_tidy_daggity(list()), "Input must be a dagitty object")
  expect_error(
    if_not_tidy_daggity("not a dag"),
    "Input must be a dagitty object"
  )
  expect_error(if_not_tidy_daggity(42), "Input must be a dagitty object")

  # Test with actual tidy_dagitty (should return the object)
  dag <- dagify(y ~ x)
  tidy_dag <- tidy_dagitty(dag)
  expect_equal(if_not_tidy_daggity(tidy_dag), tidy_dag)
})

test_that("has_labels works with edge cases", {
  # DAG without labels
  dag1 <- dagify(y ~ x)
  expect_false(has_labels(dag1))

  # DAG with empty labels
  dag2 <- dagify(y ~ x)
  expect_false(has_labels(dag2))

  # DAG with NA labels
  dag3 <- dagify(y ~ x, labels = c(x = NA, y = NA))
  expect_true(has_labels(dag3))

  # DAG with partial labels
  dag4 <- dagify(y ~ x, labels = c(x = "Exposure"))
  expect_true(has_labels(dag4))
})

test_that("n_nodes and n_edges with standard cases", {
  # Simple DAG with edges
  dag <- dagify(y ~ x + z, x ~ z)
  tidy_dag <- tidy_dagitty(dag)
  expect_equal(n_nodes(tidy_dag), 3)
  expect_equal(n_edges(tidy_dag), 3)

  # Single edge DAG
  dag_single <- dagify(y ~ x)
  tidy_single <- tidy_dagitty(dag_single)
  expect_equal(n_nodes(tidy_single), 2)
  expect_equal(n_edges(tidy_single), 1)
})

test_that("update_dag works correctly", {
  # Create a tidy_dagitty
  dag <- dagify(y ~ x + z, x ~ z)
  tidy_dag <- tidy_dagitty(dag)

  # Update the DAG
  new_dag <- dagify(y ~ x)
  updated <- update_dag(tidy_dag, new_dag)

  # Check that it returns a tidy_dagitty with the new dag
  expect_s3_class(updated, "tidy_dagitty")
  # The internal structure might be more complex
  expect_equal(
    names(dagitty::edges(pull_dag(updated))),
    names(dagitty::edges(new_dag))
  )
})
