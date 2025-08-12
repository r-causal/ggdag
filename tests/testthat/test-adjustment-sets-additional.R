test_that("control_for handles non-existent nodes", {
  dag <- dagify(y ~ x + z, x ~ z)
  tidy_dag <- tidy_dagitty(dag)

  # Try to control for non-existent node
  expect_error(
    control_for(tidy_dag, "non_existent"),
    "not a variable"
  )

  # Try to control for multiple nodes, some non-existent
  expect_error(
    control_for(tidy_dag, c("z", "non_existent")),
    "not a variable"
  )
})

test_that("ggdag_adjust with no adjustment variables", {
  # DAG with no adjusted nodes
  dag <- dagify(y ~ x)

  # Should work without adjustment variables
  p1 <- ggdag_adjust(dag)
  expect_s3_class(p1, "gg")

  # Test with var = NULL (no variables to highlight)
  p2 <- ggdag_adjust(dag, var = NULL)
  expect_s3_class(p2, "gg")

  # Test with empty character vector
  p3 <- ggdag_adjust(dag, var = character(0))
  expect_s3_class(p3, "gg")
})

test_that("dag_adjustment_sets handles edge cases", {
  # Simple DAG with no confounders
  dag_simple <- dagify(y ~ x)
  result_simple <- dag_adjustment_sets(
    dag_simple,
    exposure = "x",
    outcome = "y"
  )

  # Should have empty adjustment set
  adj_data <- pull_dag_data(result_simple)
  adj_sets <- dplyr::filter(adj_data, adjusted == "adjusted")
  expect_equal(nrow(adj_sets), 0)

  # DAG where adjustment is impossible (collider bias)
  dag_collider <- dagify(
    m ~ x + y,
    y ~ u,
    x ~ u,
    latent = "u"
  )
  expect_warning(
    result_collider <- dag_adjustment_sets(
      dag_collider,
      exposure = "x",
      outcome = "y"
    ),
    "Failed to close backdoor paths"
  )
  expect_s3_class(result_collider, "tidy_dagitty")
})

test_that("ggdag_adjustment_set with effect parameter", {
  dag <- dagify(
    y ~ x + z,
    x ~ z
  )

  # Test with effect = "direct"
  p_direct <- ggdag_adjustment_set(
    dag,
    exposure = "x",
    outcome = "y",
    effect = "direct"
  )
  expect_s3_class(p_direct, "gg")

  # The plot should be created even if direct effect is not identifiable
  dag_no_direct <- dagify(
    y ~ x + m,
    m ~ x
  )
  p_no_direct <- ggdag_adjustment_set(
    dag_no_direct,
    exposure = "x",
    outcome = "y",
    effect = "direct"
  )
  expect_s3_class(p_no_direct, "gg")
})

test_that("dag_adjustment_sets with different adjustment types", {
  dag <- dagify(
    y ~ x + z + w,
    x ~ z,
    w ~ z
  )

  # Test type = "all"
  all_sets <- dag_adjustment_sets(
    dag,
    exposure = "x",
    outcome = "y",
    type = "all"
  )
  expect_s3_class(all_sets, "tidy_dagitty")

  # Should have multiple adjustment sets
  sets_data <- pull_dag_data(all_sets)
  unique_sets <- unique(sets_data$set[sets_data$adjusted == "adjusted"])
  expect_true(length(unique_sets) >= 1)

  # Test type = "canonical"
  canonical_sets <- dag_adjustment_sets(
    dag,
    exposure = "x",
    outcome = "y",
    type = "canonical"
  )
  expect_s3_class(canonical_sets, "tidy_dagitty")
})

test_that("adjustment set functions preserve additional columns", {
  # Create DAG with additional data
  dag_df <- data.frame(
    name = c("x", "z", "z", "y"),
    to = c("y", "x", "y", NA),
    custom_col = c("a", "b", "b", "c"),
    value = c(1, 2, 2, 3),
    stringsAsFactors = FALSE
  )

  tidy_dag <- as_tidy_dagitty(dag_df)

  # Apply adjustment set function
  result <- dag_adjustment_sets(tidy_dag, exposure = "x", outcome = "y")
  result_data <- pull_dag_data(result)

  # Check that custom columns are preserved
  expect_true("custom_col" %in% names(result_data))
  expect_true("value" %in% names(result_data))
})

test_that("is_collider and is_downstream_collider work correctly", {
  dag <- dagify(
    m ~ x + y,
    y ~ x + z,
    w ~ m
  )
  tidy_dag <- tidy_dagitty(dag)

  # m is a collider
  expect_true(is_collider(tidy_dag, "m"))

  # y is also a collider (x -> y <- z)
  expect_true(is_collider(tidy_dag, "y"))

  # x is not a collider
  expect_false(is_collider(tidy_dag, "x"))

  # w is downstream from collider m
  expect_true(is_downstream_collider(tidy_dag, "w"))

  # Test with non-existent node - should error
  expect_error(is_collider(tidy_dag, "non_existent"), "not a variable")
  expect_error(
    is_downstream_collider(tidy_dag, "non_existent"),
    "not a variable"
  )
})

test_that("control_for works with tidyselect helpers", {
  dag <- dagify(
    y ~ x + z1 + z2 + w,
    x ~ z1 + z2,
    exposure = "x",
    outcome = "y"
  )
  tidy_dag <- tidy_dagitty(dag)

  # Control for variables starting with 'z'
  z_vars <- c("z1", "z2")
  result <- control_for(tidy_dag, z_vars)
  dag_controlled <- pull_dag(result)
  adjusted <- dagitty::adjustedNodes(dag_controlled)

  expect_true(all(c("z1", "z2") %in% adjusted))
  expect_false("w" %in% adjusted)
  expect_false("x" %in% adjusted)
  expect_false("y" %in% adjusted)
})
