test_that("if_not_tidy_daggity converts non-tidy dagitty objects", {
  # Test with regular dagitty object
  dag <- dagitty::dagitty("dag{x->y}")
  result <- if_not_tidy_daggity(dag)
  expect_s3_class(result, "tidy_dagitty")

  # Test with already tidy dagitty object
  tidy_dag <- tidy_dagitty(dag)
  result2 <- if_not_tidy_daggity(tidy_dag)
  expect_identical(result2, tidy_dag)

  # Test that extra arguments are passed through
  result3 <- if_not_tidy_daggity(dag, seed = 123)
  expect_s3_class(result3, "tidy_dagitty")
})

test_that("unique_pairs generates correct pairs", {
  # Basic test
  pairs <- unique_pairs(c("a", "b", "c"))
  expect_equal(nrow(pairs), 3) # (a,b), (a,c), (b,c) - excludes identical and duplicates
  expect_true(all(pairs$Var1 != pairs$Var2))

  # Test with exclude_identical = FALSE
  pairs_with_identical <- unique_pairs(c("a", "b"), exclude_identical = FALSE)
  expect_equal(nrow(pairs_with_identical), 3) # (a,a), (a,b OR b,a), (b,b)

  # Test that pairs are unique regardless of order
  pairs_ab <- pairs[pairs$Var1 == "a" & pairs$Var2 == "b", ]
  pairs_ba <- pairs[pairs$Var1 == "b" & pairs$Var2 == "a", ]
  expect_equal(nrow(pairs_ab) + nrow(pairs_ba), 1)

  # Single element with exclude_identical = TRUE
  single <- unique_pairs("a")
  expect_equal(nrow(single), 0)

  # Single element with exclude_identical = FALSE
  single_with_identical <- unique_pairs("a", exclude_identical = FALSE)
  expect_equal(nrow(single_with_identical), 1) # Just (a,a)

  # Empty vector
  empty <- unique_pairs(character(0))
  expect_equal(nrow(empty), 0)
})

test_that("formula2char converts formulas to character correctly", {
  # Simple formula
  expect_equal(formula2char(y ~ x), "y <- {x}")

  # Multiple predictors
  expect_equal(formula2char(y ~ x + z), "y <- {x z}")

  # Bidirectional relationship
  expect_equal(formula2char(y ~ x + ~z), "y <-> {x z}")

  # Only bidirectional
  expect_equal(formula2char(y ~ ~x), "y <-> {x}")
})

test_that("edge_type_switch returns correct geom functions", {
  expect_identical(edge_type_switch("link_arc"), geom_dag_edges)
  expect_identical(edge_type_switch("link"), geom_dag_edges_link)
  expect_identical(edge_type_switch("arc"), geom_dag_edges_arc)
  expect_identical(edge_type_switch("diagonal"), geom_dag_edges_diagonal)

  # Test invalid edge type returns NULL
  expect_null(edge_type_switch("invalid"))
})

test_that("is_empty_or_null correctly identifies empty/null values", {
  expect_true(is_empty_or_null(NULL))
  expect_true(is_empty_or_null(character(0)))
  expect_true(is_empty_or_null(list()))
  expect_true(is_empty_or_null(numeric(0)))

  expect_false(is_empty_or_null("a"))
  expect_false(is_empty_or_null(1))
  expect_false(is_empty_or_null(list(1)))
  expect_false(is_empty_or_null(NA))
  expect_false(is_empty_or_null(c(NA, NA)))
})

test_that("is_false correctly identifies FALSE values", {
  expect_true(is_false(FALSE))

  expect_false(is_false(TRUE))
  expect_false(is_false(NA))
  expect_false(is_false(0))
  expect_false(is_false("FALSE"))
  expect_false(is_false(c(FALSE, FALSE)))
  expect_false(is_false(NULL))
})

test_that("has_exposure correctly identifies DAGs with exposures", {
  # DAG with exposure
  dag_with_exp <- dagify(y ~ x + z, x ~ z, exposure = "x")
  expect_true(has_exposure(dag_with_exp))
  expect_true(has_exposure(tidy_dagitty(dag_with_exp)))

  # DAG without exposure
  dag_no_exp <- dagify(y ~ x + z, x ~ z)
  expect_false(has_exposure(dag_no_exp))
  expect_false(has_exposure(tidy_dagitty(dag_no_exp)))
})

test_that("has_outcome correctly identifies DAGs with outcomes", {
  # DAG with outcome
  dag_with_out <- dagify(y ~ x + z, x ~ z, outcome = "y")
  expect_true(has_outcome(dag_with_out))
  expect_true(has_outcome(tidy_dagitty(dag_with_out)))

  # DAG without outcome
  dag_no_out <- dagify(y ~ x + z, x ~ z)
  expect_false(has_outcome(dag_no_out))
  expect_false(has_outcome(tidy_dagitty(dag_no_out)))
})

test_that("has_latent correctly identifies DAGs with latent variables", {
  # DAG with latent
  dag_with_latent <- dagify(y ~ x + u, x ~ u, latent = "u")
  expect_true(has_latent(dag_with_latent))
  expect_true(has_latent(tidy_dagitty(dag_with_latent)))

  # DAG without latent
  dag_no_latent <- dagify(y ~ x + z, x ~ z)
  expect_false(has_latent(dag_no_latent))
  expect_false(has_latent(tidy_dagitty(dag_no_latent)))
})

test_that("has_collider_path correctly identifies collider paths", {
  # Create a DAG with collider paths
  dag_with_collider <- dagify(
    y ~ x,
    y ~ z,
    x ~ a,
    z ~ a
  )

  tidy_dag <- dag_with_collider |>
    tidy_dagitty() |>
    activate_collider_paths(adjust_for = "y") # y is a collider

  expect_true(has_collider_path(tidy_dag))

  # DAG without activated collider paths
  dag_no_collider <- dagify(y ~ x, x ~ z)
  expect_false(has_collider_path(tidy_dagitty(dag_no_collider)))
})

test_that("n_nodes counts nodes correctly", {
  # Simple DAG
  dag1 <- dagify(y ~ x)
  expect_equal(n_nodes(tidy_dagitty(dag1)), 2)

  # More complex DAG
  dag2 <- dagify(y ~ x + z, x ~ z, w ~ z)
  expect_equal(n_nodes(tidy_dagitty(dag2)), 4)

  # Single isolated node (no edges)
  dag3 <- dagify(x ~ w, y ~ z) # Two disconnected components
  expect_equal(n_nodes(tidy_dagitty(dag3)), 4)
})

test_that("n_edges counts edges correctly", {
  # Simple DAG
  dag1 <- dagify(y ~ x)
  expect_equal(n_edges(tidy_dagitty(dag1)), 1)

  # More complex DAG
  dag2 <- dagify(y ~ x + z, x ~ z)
  expect_equal(n_edges(tidy_dagitty(dag2)), 3)
})


test_that("n_collder_paths counts collider paths correctly", {
  # DAG without collider paths
  dag_no_collider <- dagify(y ~ x, x ~ z)
  expect_equal(n_collder_paths(tidy_dagitty(dag_no_collider)), 0)

  # DAG with collider paths
  dag_with_collider <- dagify(
    y ~ x,
    y ~ z,
    x ~ a,
    z ~ a
  )

  tidy_dag <- dag_with_collider |>
    tidy_dagitty() |>
    activate_collider_paths(adjust_for = "y")

  expect_gt(n_collder_paths(tidy_dag), 0)
})

test_that("collider_paths returns correct paths", {
  # DAG without collider paths
  dag_no_collider <- dagify(y ~ x, x ~ z)
  expect_length(collider_paths(tidy_dagitty(dag_no_collider)), 0)

  # DAG with collider paths
  dag_with_collider <- dagify(
    y ~ x,
    y ~ z,
    x ~ a,
    z ~ a
  )

  tidy_dag <- dag_with_collider |>
    tidy_dagitty() |>
    activate_collider_paths(adjust_for = "y")

  paths <- collider_paths(tidy_dag)
  expect_true(length(paths) > 0)
  expect_true(all(grepl("<->", paths)))
})

test_that("expansion returns correct expansion function", {
  # Should return a scale expansion
  exp <- expansion(mult = 0.1)
  expect_true(inherits(exp, "numeric") || inherits(exp, "expansion"))

  # Test with different arguments
  exp2 <- expansion(mult = c(0.1, 0.2))
  expect_true(inherits(exp2, "numeric") || inherits(exp2, "expansion"))
})

test_that("ggplot2_version returns a package version", {
  version <- ggplot2_version()
  expect_s3_class(version, "package_version")
  # Check it has major.minor.patch structure
  expect_true(length(version) >= 1)
  expect_true(is.integer(unclass(version)[[1]]))
})

test_that("dplyr_version returns a package version", {
  version <- dplyr_version()
  expect_s3_class(version, "package_version")
  # Check it has major.minor.patch structure
  expect_true(length(version) >= 1)
  expect_true(is.integer(unclass(version)[[1]]))
})

test_that("ggname adds name to grob", {
  # Create a simple grob
  grob <- grid::circleGrob()
  named_grob <- ggname("test", grob)

  # Check that the name was set (it will have a unique suffix)
  expect_true(grepl("^test", named_grob$name))
  expect_true(inherits(named_grob, "grob"))
})

test_that("ggdag_left_join performs left join correctly", {
  # Create test data
  df1 <- data.frame(id = 1:3, value = c("a", "b", "c"))
  df2 <- data.frame(id = c(1, 1, 2, 2, 3), score = 1:5)

  result <- ggdag_left_join(df1, df2, by = "id")
  expect_equal(nrow(result), 5)
  expect_true(all(c("id", "value", "score") %in% names(result)))
})

test_that("%nin% operator works correctly", {
  expect_true("a" %nin% c("b", "c"))
  expect_false("a" %nin% c("a", "b", "c"))
  expect_true(1 %nin% c(2, 3, 4))
  expect_false(1 %nin% c(1, 2, 3))

  # Test with NA
  expect_true(NA %nin% c(1, 2, 3))
  expect_true(1 %nin% c(NA, 2, 3))
})

test_that("check_arg_node handles deprecated node argument", {
  skip_if_not_installed("lifecycle")

  # Need to use the is_present check from lifecycle
  # When node is explicitly provided
  expect_warning(
    result <- check_arg_node(
      node = "x",
      use_nodes = "y",
      what = "test_function"
    ),
    "deprecated"
  )
  expect_equal(result, "x")
})

test_that("check_arg_stylized handles deprecated stylized argument", {
  skip_if_not_installed("lifecycle")

  # When stylized is explicitly provided
  expect_warning(
    result <- check_arg_stylized(
      stylized = TRUE,
      use_stylized = FALSE,
      what = "test_function"
    ),
    "deprecated"
  )
  expect_true(result)
})

test_that("ggraph_create_layout creates layout without graph attribute", {
  # Create a simple DAG
  dag <- dagify(y ~ x, x ~ z)
  tidy_dag <- tidy_dagitty(dag)

  # Create layout
  layout <- ggraph_create_layout(tidy_dag, layout = "nicely")

  # Check that layout is created but graph attribute is NULL
  expect_s3_class(layout, "data.frame")
  expect_null(attr(layout, "graph"))
  expect_true("x" %in% names(layout))
  expect_true("y" %in% names(layout))
})

# Edge case tests
test_that("utility functions handle edge cases gracefully", {
  # Test functions with NULL inputs where applicable
  expect_true(is_empty_or_null(NULL))
  expect_false(is_false(NULL))
})
