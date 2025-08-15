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
})

test_that("`test_conditional_independence()` works", {
  data <- simulate_data(test_dag)
  result <- test_conditional_independence(test_dag, data)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(query_conditional_independence(test_dag)))
  expect_length(result, 5)

  expect_ggdag_error(test_conditional_independence(test_dag))

  expect_ggdag_error(
    test_conditional_independence(letters, letters)
  )

  expect_ggdag_error(test_conditional_independence(test_dag, letters))
})


test_that("`ggdag_conditional_independence()` works", {
  set.seed(1)
  test_result <- data.frame(
    independence = c("x _||_ y", "y _||_ z"),
    estimate = c(0.1, 0.2),
    lower = c(-0.1, 0.1),
    upper = c(0.3, 0.4)
  )

  p1 <- ggdag_conditional_independence(test_result)
  expect_doppelganger("fake tests flexibly plot", p1)

  data <- simulate_data(test_dag)
  test_result <- test_conditional_independence(test_dag, data)
  p2 <- ggdag_conditional_independence(
    test_result,
    vline_linewidth = 1,
    vline_color = "purple",
    pointrange_fatten = 4
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
