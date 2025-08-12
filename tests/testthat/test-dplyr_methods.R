test_that("dplyr methods work on tidy dags: `summarize()`", {
  library(dplyr, warn.conflicts = FALSE)
  x <- tidy_dagitty(m_bias()) %>%
    group_by(name) %>%
    summarize(n = n())

  expect_s3_class(x, "tbl")
  expect_length(x, 2)
  expect_equal(nrow(x), 5)
  expect_named(x, c("name", "n"))
})

test_that("arrange.tidy_dagitty sorts DAG data correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    coords = list(
      x = c(x = 0, y = 2, z = 1),
      y = c(x = 0, y = 0, z = 1)
    )
  ) |>
    tidy_dagitty()

  # Arrange by name
  arranged <- arrange(dag, name)
  dag_data <- pull_dag_data(arranged)
  # First three sorted names should be x, y, z
  unique_names <- unique(dag_data$name)
  expect_equal(unique_names, c("x", "y", "z"))

  # Arrange by x coordinate
  arranged_x <- arrange(dag, x)
  dag_data_x <- pull_dag_data(arranged_x)
  # Check first three x values are sorted
  expect_equal(dag_data_x$x[1:3], sort(dag_data_x$x[1:3]))

  # Arrange descending
  arranged_desc <- arrange(dag, desc(name))
  dag_data_desc <- pull_dag_data(arranged_desc)
  unique_names_desc <- unique(dag_data_desc$name)
  expect_equal(unique_names_desc, c("z", "y", "x"))

  # Original DAG object should be preserved
  expect_s3_class(arranged, "tidy_dagitty")
  expect_equal(pull_dag(arranged), pull_dag(dag))
})

test_that("ungroup.tidy_dagitty removes grouping", {
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()

  # Group then ungroup
  grouped <- group_by(dag, name)
  expect_true(dplyr::is_grouped_df(pull_dag_data(grouped)))

  ungrouped <- ungroup(grouped)
  expect_false(dplyr::is_grouped_df(pull_dag_data(ungrouped)))

  # Should preserve tidy_dagitty class
  expect_s3_class(ungrouped, "tidy_dagitty")

  # Should be able to ungroup an already ungrouped object
  ungrouped2 <- ungroup(dag)
  expect_false(dplyr::is_grouped_df(pull_dag_data(ungrouped2)))
})

test_that("transmute.tidy_dagitty creates new columns and drops others", {
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()

  # transmute with explicitly listed columns plus new one
  transmuted <- transmute(
    dag,
    name,
    to,
    x,
    y,
    direction,
    xend,
    yend,
    doubled_x = x * 2
  )
  dag_data <- pull_dag_data(transmuted)

  # Should have the new column
  expect_true("doubled_x" %in% names(dag_data))

  # Check the calculation
  expect_equal(dag_data$doubled_x, dag_data$x * 2)

  # Should preserve tidy_dagitty class when all columns kept
  expect_s3_class(transmuted, "tidy_dagitty")

  # Test transmute without essential columns - should error
  expect_ggdag_error(
    transmute(dag, doubled_x = x * 2)
  )
})

test_that("distinct.tidy_dagitty removes duplicate rows", {
  # Create a DAG with potential duplicates
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    coords = list(
      x = c(x = 0, y = 2, z = 1),
      y = c(x = 0, y = 0, z = 1)
    )
  ) |>
    tidy_dagitty()

  # Add a duplicate column for testing
  dag_with_dup <- mutate(dag, group = ifelse(name == "z", "conf", "other"))

  # Get distinct by group with .keep_all
  distinct_dag <- distinct(dag_with_dup, group, .keep_all = TRUE)
  dag_data <- pull_dag_data(distinct_dag)

  # Should have only 2 rows (one for each unique group)
  expect_equal(length(unique(dag_data$group)), 2)
  expect_equal(nrow(dag_data), 2)

  # Test without .keep_all - this may not include required columns
  # so it might fail prep_dag_data validation
  expect_ggdag_error(
    distinct(dag_with_dup, group)
  )

  # Should preserve tidy_dagitty class with .keep_all = TRUE
  expect_s3_class(distinct_dag, "tidy_dagitty")
})

test_that("full_join.tidy_dagitty joins data correctly", {
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()

  # Create additional data to join
  extra_data <- data.frame(
    name = c("x", "y", "w"),
    value = c(1, 2, 3)
  )

  # Full join
  joined <- full_join(dag, extra_data, by = "name")
  dag_data <- pull_dag_data(joined)

  # Should have value column
  expect_true("value" %in% names(dag_data))

  # Check joined values
  x_rows <- dag_data[dag_data$name == "x", ]
  expect_true(all(x_rows$value == 1))

  # Should have NA for z (not in extra_data)
  z_rows <- dag_data[dag_data$name == "z", ]
  expect_true(all(is.na(z_rows$value)))

  # Should include w from extra_data
  expect_true("w" %in% dag_data$name)

  # Should preserve tidy_dagitty class
  expect_s3_class(joined, "tidy_dagitty")
})

test_that("inner_join.tidy_dagitty joins data correctly", {
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()

  # Create additional data to join
  extra_data <- data.frame(
    name = c("x", "y"),
    value = c(1, 2)
  )

  # Inner join
  joined <- inner_join(dag, extra_data, by = "name")
  dag_data <- pull_dag_data(joined)

  # Should only have rows for x and y
  expect_false("z" %in% dag_data$name)
  expect_true(all(dag_data$name %in% c("x", "y")))

  # Should have value column
  expect_true("value" %in% names(dag_data))

  # Should preserve tidy_dagitty class
  expect_s3_class(joined, "tidy_dagitty")
})

test_that("right_join.tidy_dagitty joins data correctly", {
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()

  # Create additional data to join
  extra_data <- data.frame(
    name = c("x", "w"),
    value = c(1, 3)
  )

  # Right join
  joined <- right_join(dag, extra_data, by = "name")
  dag_data <- pull_dag_data(joined)

  # Should have only x and w
  expect_equal(sort(unique(dag_data$name)), c("w", "x"))

  # Should have value column
  expect_true("value" %in% names(dag_data))

  # Should preserve tidy_dagitty class
  expect_s3_class(joined, "tidy_dagitty")
})

test_that("semi_join.tidy_dagitty filters correctly", {
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()

  # Create filter data
  filter_data <- data.frame(
    name = c("x", "y")
  )

  # Semi join
  joined <- semi_join(dag, filter_data, by = "name")
  dag_data <- pull_dag_data(joined)

  # Should only have rows where name is x or y
  expect_false("z" %in% dag_data$name)
  expect_true(all(dag_data$name %in% c("x", "y")))

  # Should NOT have columns from filter_data
  expect_equal(names(dag_data), names(pull_dag_data(dag)))

  # Should preserve tidy_dagitty class
  expect_s3_class(joined, "tidy_dagitty")
})

test_that("slice.tidy_dagitty selects rows correctly", {
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()

  # Slice first 3 rows
  sliced <- slice(dag, 1:3)
  dag_data <- pull_dag_data(sliced)
  expect_equal(nrow(dag_data), 3)

  # Slice with negative indices
  original_n <- nrow(pull_dag_data(dag))
  sliced_neg <- slice(dag, -(1:2))
  expect_equal(nrow(pull_dag_data(sliced_neg)), original_n - 2)

  # Should preserve tidy_dagitty class
  expect_s3_class(sliced, "tidy_dagitty")
})

test_that("join operations handle suffix correctly", {
  dag <- dagify(y ~ x, x ~ z) |>
    tidy_dagitty() |>
    mutate(value = 1)

  extra_data <- data.frame(
    name = c("x", "y"),
    value = c(2, 3)
  )

  # Test suffix parameter
  joined <- full_join(
    dag,
    extra_data,
    by = "name",
    suffix = c("_dag", "_extra")
  )
  dag_data <- pull_dag_data(joined)

  expect_true("value_dag" %in% names(dag_data))
  expect_true("value_extra" %in% names(dag_data))
})

test_that("deprecated underscore methods still work with warnings", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()

  # Test select_ - preserves tidy_dagitty with all columns
  selected <- select_(dag, ~ dplyr::everything())
  expect_s3_class(selected, "tidy_dagitty")
  expect_equal(names(pull_dag_data(selected)), names(pull_dag_data(dag)))

  # Test filter_
  filtered <- filter_(dag, ~ name == "x")
  expect_true(all(pull_dag_data(filtered)$name == "x"))

  # Test mutate_
  mutated <- mutate_(dag, new_col = ~ x + 1)
  expect_true("new_col" %in% names(pull_dag_data(mutated)))

  # Test arrange_
  arranged <- arrange_(dag, ~ desc(name))
  expect_equal(unique(pull_dag_data(arranged)$name)[1], "z")

  # Test slice_
  sliced <- slice_(dag, ~ 1:2)
  expect_equal(nrow(pull_dag_data(sliced)), 2)

  # Test summarise_ - errors because it tries to preserve tidy_dagitty
  expect_ggdag_error(
    summarise_(dag, n = ~ n())
  )
})

test_that("multiple dplyr operations can be chained", {
  dag <- dagify(
    y ~ x + z + w,
    x ~ z,
    z ~ w,
    coords = list(
      x = c(x = 0, y = 3, z = 1, w = 2),
      y = c(x = 0, y = 0, z = 1, w = 2)
    )
  ) |>
    tidy_dagitty()

  # Chain multiple operations that preserve tidy_dagitty
  result <- dag |>
    mutate(is_exposure = name == "x") |>
    arrange(desc(is_exposure), name) |>
    filter(x < 2) # Filter x < 2 to exclude w (x = 2)

  dag_data <- pull_dag_data(result)

  # Check the chain worked correctly
  expect_true("is_exposure" %in% names(dag_data))
  # Check that x rows appear first (is_exposure = TRUE)
  x_rows <- which(dag_data$name == "x")
  if (length(x_rows) > 0 && any(dag_data$name != "x")) {
    expect_true(all(x_rows < which(dag_data$name != "x")[1]))
  }
  expect_false("w" %in% dag_data$name) # w should be filtered out (x = 2)

  # Should still be a tidy_dagitty
  expect_s3_class(result, "tidy_dagitty")

  # select with all columns preserves tidy_dagitty
  selected <- result |> select(dplyr::everything())
  expect_s3_class(selected, "tidy_dagitty")
  expect_true("is_exposure" %in% names(pull_dag_data(selected)))

  # select without essential columns returns error
  expect_ggdag_error(
    result |> select(name, x, y, is_exposure)
  )
})

test_that("distinct.tidy_dagitty handles no duplicates gracefully", {
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()

  original_rows <- nrow(pull_dag_data(dag))

  # Distinct on name with .keep_all = TRUE preserves tidy_dagitty
  distinct_dag <- distinct(dag, name, .keep_all = TRUE)

  # Should have 3 rows (one for each unique name: x, y, z)
  expect_equal(nrow(pull_dag_data(distinct_dag)), 3)

  # Should have all unique names
  expect_equal(sort(unique(pull_dag_data(distinct_dag)$name)), c("x", "y", "z"))

  # Should preserve tidy_dagitty class
  expect_s3_class(distinct_dag, "tidy_dagitty")
})

test_that("join operations preserve DAG structure", {
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()

  extra_data <- data.frame(
    name = c("x", "y", "z"),
    label = c("Exposure", "Outcome", "Confounder")
  )

  # Various joins should preserve the DAG
  for (join_fn in list(full_join, left_join, inner_join)) {
    joined <- join_fn(dag, extra_data, by = "name")

    # Check DAG is preserved
    expect_equal(pull_dag(joined), pull_dag(dag))

    # Check class is preserved
    expect_s3_class(joined, "tidy_dagitty")
  }
})

test_that("anti_join.tidy_dagitty filters correctly", {
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()

  # Create filter data
  filter_data <- data.frame(
    name = c("x", "y")
  )

  # Anti join - should keep only z
  joined <- anti_join(dag, filter_data, by = "name")
  dag_data <- pull_dag_data(joined)

  # Should only have z
  expect_true(all(dag_data$name == "z"))
  expect_false("x" %in% dag_data$name)
  expect_false("y" %in% dag_data$name)

  # Should preserve tidy_dagitty class
  expect_s3_class(joined, "tidy_dagitty")
})
