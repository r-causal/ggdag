test_that("`query_conditional_independence()` returns a tibble of independencies", {
  result <- query_conditional_independence(test_dag)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 15)
  expect_named(result, c("set", "a", "b", "conditioning_set", "conditioned_on"))
  expect_type(result$conditioned_on, "list")
  expect_type(result$conditioning_set, "character")

  result <- query_conditional_independence(test_dag, type = "all.pairs")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 57)
  expect_named(result, c("set", "a", "b", "conditioning_set", "conditioned_on"))
  expect_type(result$conditioned_on, "list")
  expect_type(result$conditioning_set, "character")

  expect_ggdag_error(
    query_conditional_independence(letters)
  )
})

test_that("`query_conditional_independence()` handles empty results correctly", {
  # from the documentation of `dagitty::impliedConditionalIndependencies()`
  g <- dagitty::dagitty("dag{ x -> m -> y }")
  result <- query_conditional_independence(g)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)

  dagitty::latents(g) <- "m"
  result <- query_conditional_independence(g)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)

  # the empty result keeps the documented schema
  expected_types <- c(
    set = "integer",
    a = "character",
    b = "character",
    conditioning_set = "character",
    conditioned_on = "list"
  )
  expect_named(result, names(expected_types))
  expect_equal(purrr::map_chr(result, typeof), expected_types)

  # a saturated DAG implies no conditional independencies either
  saturated <- query_conditional_independence(tidy_dagitty(dagify(y ~ x)))
  expect_equal(nrow(saturated), 0)
  expect_named(saturated, names(expected_types))
  expect_equal(purrr::map_chr(saturated, typeof), expected_types)

  # a non-empty result has the same types, for either query type. `dagitty`
  # names the independencies of a "missing.edge" query, which would otherwise
  # make `set` a character vector of those names
  populated <- query_conditional_independence(tidy_dagitty(dagify(
    y ~ x,
    x ~ z
  )))
  expect_gt(nrow(populated), 0)
  expect_equal(purrr::map_chr(populated, typeof), expected_types)
  expect_equal(populated$set, seq_len(nrow(populated)))

  all_pairs <- query_conditional_independence(test_dag, type = "all.pairs")
  expect_equal(purrr::map_chr(all_pairs, typeof), expected_types)
  expect_equal(all_pairs$set, seq_len(nrow(all_pairs)))

  # so an empty and a non-empty result row-bind
  expect_equal(
    nrow(dplyr::bind_rows(saturated, populated)),
    nrow(populated)
  )
})

test_that("`query_conditional_independence()` renders an empty conditioning set", {
  # `x _||_ y` holds unconditionally, so its conditioning set is empty
  result <- query_conditional_independence(tidy_dagitty(dagify(y ~ z, x ~ w)))
  unconditional <- result[result$conditioning_set == "{}", ]

  expect_gt(nrow(unconditional), 0)
  expect_false(anyNA(result$conditioning_set))
  expect_equal(unconditional$conditioned_on[[1]], character())
  # the list column and the set string agree in every row
  expect_equal(
    lengths(result$conditioned_on) == 0,
    result$conditioning_set == "{}"
  )
})

test_that("`ggdag_conditional_independence()` requires an independence column", {
  # the shape of raw `dagitty::localTests()` output: the independence
  # statements live in the row names, not in a column
  local_tests_output <- data.frame(
    estimate = c(0.05, -0.12),
    p.value = c(0.6, 0.3),
    `2.5%` = c(-0.15, -0.30),
    `97.5%` = c(0.25, 0.06),
    row.names = c("w _||_ y | x", "x _||_ w | z"),
    check.names = FALSE
  )

  expect_error(
    ggdag_conditional_independence(local_tests_output),
    class = "ggdag_columns_error"
  )

  # the wrapped workflow still plots
  wrapped <- tibble::as_tibble(local_tests_output, rownames = "independence")
  expect_s3_class(ggdag_conditional_independence(wrapped), "gg")
})

test_that("the missing independence column is reported clearly", {
  local_tests_output <- data.frame(
    estimate = c(0.05, -0.12),
    p.value = c(0.6, 0.3),
    `2.5%` = c(-0.15, -0.30),
    `97.5%` = c(0.25, 0.06),
    row.names = c("w _||_ y | x", "x _||_ w | z"),
    check.names = FALSE
  )

  expect_ggdag_error(ggdag_conditional_independence(local_tests_output))
})

test_that("`test_conditional_independence()` works", {
  data <- simulate_data(test_dag)
  result <- test_conditional_independence(test_dag, data)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(query_conditional_independence(test_dag)))
  expect_length(result, 5)

  # live output plots, but its values are not cross-platform reproducible
  # (`MASS::mvrnorm()`), so the visual check uses a fixed fixture instead
  expect_s3_class(ggdag_conditional_independence(result), "gg")

  expect_ggdag_error(test_conditional_independence(test_dag))

  expect_ggdag_error(
    test_conditional_independence(letters, letters)
  )

  expect_ggdag_error(test_conditional_independence(test_dag, letters))
})


test_that("`ggdag_conditional_independence()` works", {
  test_result <- data.frame(
    independence = c("x _||_ y", "y _||_ z"),
    estimate = c(0.1, 0.2),
    lower = c(-0.1, 0.1),
    upper = c(0.3, 0.4)
  )

  p1 <- ggdag_conditional_independence(test_result)
  expect_doppelganger("fake tests flexibly plot", p1)

  # a fixed copy of `test_conditional_independence(test_dag, ...)` output:
  # the real pipeline draws through `MASS::mvrnorm()`, whose values depend
  # on the BLAS/LAPACK build, so live estimates are not reproducible across
  # platforms or R versions. The shape matters here — `independence` plus
  # positional estimate/CI columns as `dagitty::localTests()` names them.
  test_result <- tibble::tibble(
    independence = c(
      "v _||_ w1",
      "v _||_ w2",
      "v _||_ x | w1, z1",
      "v _||_ y | w1, w2, x, z2",
      "v _||_ y | w1, w2, z1, z2",
      "w1 _||_ z2 | w2",
      "w2 _||_ x | w1",
      "w2 _||_ z1 | w1",
      "x _||_ z2 | v, w2",
      "x _||_ z2 | v, w1",
      "x _||_ z2 | w1, z1",
      "y _||_ z1 | v, w1, x",
      "y _||_ z1 | w1, w2, x, z2",
      "z1 _||_ z2 | v, w2",
      "z1 _||_ z2 | v, w1"
    ),
    estimate = c(
      -0.02,
      0.05,
      -0.11,
      0.08,
      0.13,
      -0.07,
      0.02,
      0.16,
      -0.14,
      0.04,
      0.09,
      -0.05,
      0.11,
      -0.09,
      0.06
    ),
    p.value = c(
      0.81,
      0.55,
      0.21,
      0.34,
      0.12,
      0.42,
      0.79,
      0.06,
      0.09,
      0.62,
      0.28,
      0.56,
      0.19,
      0.29,
      0.48
    ),
    `2.5%` = c(
      -0.18,
      -0.11,
      -0.27,
      -0.08,
      -0.03,
      -0.23,
      -0.14,
      0.00,
      -0.30,
      -0.12,
      -0.07,
      -0.21,
      -0.05,
      -0.25,
      -0.10
    ),
    `97.5%` = c(
      0.14,
      0.21,
      0.05,
      0.24,
      0.29,
      0.09,
      0.18,
      0.32,
      0.02,
      0.20,
      0.25,
      0.11,
      0.27,
      0.07,
      0.22
    )
  )
  p2 <- ggdag_conditional_independence(
    test_result,
    vline_linewidth = 1,
    vline_color = "purple",
    point_size = 3
  )

  expect_doppelganger("real tests plot", p2)

  test_result <- data.frame(
    independence = character(),
    estimate = numeric(),
    lower = numeric(),
    upper = numeric()
  )
  expect_ggdag_error(ggdag_conditional_independence(test_result))
})

test_that("deprecated `pointrange_fatten` still maps to `point_size`", {
  test_result <- data.frame(
    independence = c("x _||_ y", "y _||_ z"),
    estimate = c(0.1, 0.2),
    lower = c(-0.1, 0.1),
    upper = c(0.3, 0.4)
  )

  p_new <- ggdag_conditional_independence(test_result, point_size = 5)
  p_old <- withr::with_options(
    list(lifecycle_verbosity = "quiet"),
    ggdag_conditional_independence(test_result, pointrange_fatten = 5)
  )

  build_new <- ggplot2::ggplot_build(p_new)
  build_old <- ggplot2::ggplot_build(p_old)
  expect_equal(build_new$data[[2]]$size, build_old$data[[2]]$size)

  expect_warning(
    ggdag_conditional_independence(test_result, pointrange_fatten = 5),
    "pointrange_fatten"
  )
})

test_that("`ggdag_conditional_independence()` sorting works correctly", {
  test_result <- data.frame(
    independence = c("x _||_ y", "y _||_ z", "a _||_ b"),
    estimate = c(0.3, 0.1, 0.2),
    lower = c(0.2, -0.1, 0.0),
    upper = c(0.4, 0.3, 0.4)
  )

  # Test with default sort = TRUE
  p1 <- ggdag_conditional_independence(test_result)
  expect_doppelganger("sorted plot default", p1)

  # Verify the order is sorted by estimate
  plot_build <- ggplot2::ggplot_build(p1)
  y_order <- levels(plot_build$plot$data$independence)
  expect_equal(y_order, c("y & z", "a & b", "x & y"))

  # Test with sort = FALSE
  p2 <- ggdag_conditional_independence(test_result, sort = FALSE)
  expect_doppelganger("unsorted plot", p2)

  # Verify the order is preserved when sort = FALSE
  plot_build2 <- ggplot2::ggplot_build(p2)
  y_order2 <- plot_build2$plot$data$independence
  expect_equal(as.character(y_order2), c("x & y", "y & z", "a & b"))
})
