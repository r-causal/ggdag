test_that("expand_plot returns correct scale objects", {
  # Default expansion
  scales <- expand_plot()
  expect_length(scales, 2)
  expect_s3_class(scales[[1]], "ScaleContinuousPosition")
  expect_s3_class(scales[[2]], "ScaleContinuousPosition")
  expect_true("x" %in% scales[[1]]$aesthetics)
  expect_true("y" %in% scales[[2]]$aesthetics)

  # Custom expansion
  scales_custom <- expand_plot(
    expand_x = expansion(c(0.05, 0.05)),
    expand_y = expansion(c(0.2, 0.2))
  )
  expect_equal(scales_custom[[1]]$expand, expansion(c(0.05, 0.05)))
  expect_equal(scales_custom[[2]]$expand, expansion(c(0.2, 0.2)))
})

test_that("theme_dag_blank creates correct theme", {
  theme <- theme_dag_blank()
  expect_s3_class(theme, "theme")
  expect_s3_class(theme, "gg")

  # Check specific theme elements
  expect_equal(theme$axis.text, ggplot2::element_blank())
  expect_equal(theme$axis.title, ggplot2::element_blank())
  expect_equal(theme$panel.grid, ggplot2::element_blank())
  expect_s3_class(theme$strip.text, "element_text")
  expect_equal(theme$strip.text$face, "bold")

  # With custom parameters
  theme_custom <- theme_dag_blank(base_size = 16, base_family = "serif")
  expect_equal(theme_custom$text$size, 16)
  expect_equal(theme_custom$text$family, "serif")

  # With additional arguments
  theme_extra <- theme_dag_blank(plot.title = ggplot2::element_text(size = 20))
  expect_equal(theme_extra$plot.title$size, 20)
})

test_that("theme_dag is an alias for theme_dag_blank", {
  expect_identical(theme_dag, theme_dag_blank)
})

test_that("theme_dag_grid creates correct theme", {
  theme <- theme_dag_grid()
  expect_s3_class(theme, "theme")
  expect_s3_class(theme, "gg")

  # Check specific theme elements - should have grid but no axes
  expect_equal(theme$axis.text, ggplot2::element_blank())
  expect_equal(theme$axis.title, ggplot2::element_blank())
  # Grid should be present (not blank)
  expect_true(
    is.null(theme$panel.grid) || !inherits(theme$panel.grid, "element_blank")
  )

  # With custom parameters
  theme_custom <- theme_dag_grid(base_size = 14, base_family = "mono")
  expect_equal(theme_custom$text$size, 14)
  expect_equal(theme_custom$text$family, "mono")

  # With additional arguments
  theme_extra <- theme_dag_grid(
    panel.background = ggplot2::element_rect(fill = "lightblue")
  )
  expect_equal(theme_extra$panel.background$fill, "lightblue")
})

test_that("theme_dag_grey creates correct theme", {
  theme <- theme_dag_grey()
  expect_s3_class(theme, "theme")
  expect_s3_class(theme, "gg")

  # Check specific theme elements
  expect_equal(theme$axis.text, ggplot2::element_blank())
  expect_equal(theme$axis.title, ggplot2::element_blank())
  expect_equal(theme$axis.ticks, ggplot2::element_blank())
  expect_s3_class(theme$panel.grid.major, "element_line")
  expect_s3_class(theme$panel.grid.minor, "element_line")
  expect_equal(theme$panel.grid.major$colour, "grey92")
  expect_equal(theme$panel.grid.minor$colour, "grey92")

  # With custom parameters
  theme_custom <- theme_dag_grey(base_size = 18, base_family = "Times")
  expect_equal(theme_custom$text$size, 18)
  expect_equal(theme_custom$text$family, "Times")

  # With additional arguments
  theme_extra <- theme_dag_grey(legend.position = "none")
  expect_equal(theme_extra$legend.position, "none")
})

test_that("theme_dag_gray is an alias for theme_dag_grey", {
  expect_identical(theme_dag_gray, theme_dag_grey)
})

test_that("theme_dag_grey_grid creates correct theme", {
  theme <- theme_dag_grey_grid()
  expect_s3_class(theme, "theme")
  expect_s3_class(theme, "gg")

  # Check specific theme elements
  expect_equal(theme$axis.text, ggplot2::element_blank())
  expect_equal(theme$axis.title, ggplot2::element_blank())
  expect_equal(theme$axis.ticks, ggplot2::element_blank())
  # Should have default grey theme grid (not modified)

  # With custom parameters
  theme_custom <- theme_dag_grey_grid(base_size = 10, base_family = "Helvetica")
  expect_equal(theme_custom$text$size, 10)
  expect_equal(theme_custom$text$family, "Helvetica")

  # With additional arguments
  theme_extra <- theme_dag_grey_grid(
    strip.background = ggplot2::element_rect(fill = "white")
  )
  expect_equal(theme_extra$strip.background$fill, "white")
})

test_that("theme_dag_gray_grid is an alias for theme_dag_grey_grid", {
  expect_identical(theme_dag_gray_grid, theme_dag_grey_grid)
})

test_that("scale_adjusted creates correct scales", {
  # Without alpha
  scales <- scale_adjusted(include_alpha = FALSE)
  expect_type(scales, "list")
  expect_length(scales, 5) # 3 scales + 2 NULLs for alpha scales

  # Check linetype scale
  expect_s3_class(scales[[1]], "ScaleDiscrete")
  expect_equal(scales[[1]]$aesthetics, "linetype")
  expect_equal(scales[[1]]$palette(1), "dashed")

  # Check shape scale
  expect_s3_class(scales[[2]], "ScaleDiscrete")
  expect_equal(scales[[2]]$aesthetics, "shape")
  expect_equal(scales[[2]]$limits, c("adjusted", "unadjusted"))
  # Get the values from the scale
  shape_values <- scales[[2]]$palette(2)
  names(shape_values) <- c("adjusted", "unadjusted")
  expect_equal(shape_values, c("adjusted" = 15, "unadjusted" = 19))

  # Check color scale
  expect_s3_class(scales[[3]], "ScaleDiscrete")
  expect_equal(scales[[3]]$aesthetics, "colour")
  expect_equal(scales[[3]]$limits, c("adjusted", "unadjusted"))

  # Without alpha, positions 4 and 5 should be NULL
  expect_null(scales[[4]])
  expect_null(scales[[5]])

  # With alpha
  scales_alpha <- scale_adjusted(include_alpha = TRUE)
  expect_length(scales_alpha, 5)

  # Check alpha scale
  expect_s3_class(scales_alpha[[4]], "ScaleDiscrete")
  expect_equal(scales_alpha[[4]]$aesthetics, "alpha")
  expect_equal(scales_alpha[[4]]$limits, c("adjusted", "unadjusted"))

  # Check edge alpha scale
  expect_s3_class(scales_alpha[[5]], "ScaleDiscrete")
  expect_equal(scales_alpha[[5]]$aesthetics, "edge_alpha")
  expect_equal(scales_alpha[[5]]$limits, c("adjusted", "unadjusted"))
})

test_that("scale_dag is deprecated", {
  expect_warning(
    scales <- scale_dag(),
    "deprecated"
  )

  # Should still work despite deprecation
  expect_type(scales, "list")
  expect_length(scales, 2)

  # With custom breaks
  expect_warning(
    scales_custom <- scale_dag(breaks = c("a", "b")),
    "deprecated"
  )
  expect_type(scales_custom, "list")
})

test_that("breaks helper function creates correct scales", {
  # Default breaks
  scale_list <- breaks()
  expect_length(scale_list, 2)
  expect_s3_class(scale_list[[1]], "ScaleDiscrete")
  expect_s3_class(scale_list[[2]], "ScaleDiscrete")
  expect_equal(scale_list[[1]]$aesthetics, "colour")
  expect_equal(scale_list[[2]]$aesthetics, "fill")

  # Custom breaks
  scale_list_custom <- breaks(
    breaks = c("one", "two"),
    name = "Categories",
    drop = FALSE
  )
  expect_equal(scale_list_custom[[1]]$name, "Categories")
  expect_equal(scale_list_custom[[2]]$name, "Categories")
  expect_equal(scale_list_custom[[1]]$breaks, c("one", "two"))
  expect_equal(scale_list_custom[[2]]$breaks, c("one", "two"))
  expect_false(scale_list_custom[[1]]$drop)
  expect_false(scale_list_custom[[2]]$drop)
})

test_that("remove_axes creates correct theme modification", {
  theme_mod <- remove_axes()
  expect_s3_class(theme_mod, "theme")
  expect_equal(theme_mod$axis.text, ggplot2::element_blank())
  expect_equal(theme_mod$axis.title, ggplot2::element_blank())
  expect_equal(theme_mod$axis.ticks, ggplot2::element_blank())

  # Should only have these three elements
  expect_equal(length(theme_mod), 3)
})

test_that("remove_grid creates correct theme modification", {
  theme_mod <- remove_grid()
  expect_s3_class(theme_mod, "theme")
  expect_equal(theme_mod$panel.grid, ggplot2::element_blank())

  # Should only have this one element
  expect_equal(length(theme_mod), 1)
})

test_that("theme functions work with ggplot", {
  library(ggplot2, exclude = "expansion")
  p <- ggdag(dagify(y ~ x))

  # Test that themes can be added to plots
  expect_s3_class(p + theme_dag_blank(), "gg")
  expect_s3_class(p + theme_dag_grid(), "gg")
  expect_s3_class(p + theme_dag_grey(), "gg")
  expect_s3_class(p + theme_dag_grey_grid(), "gg")

  # Test that helper functions can be added
  expect_s3_class(p + remove_axes(), "gg")
  expect_s3_class(p + remove_grid(), "gg")
  expect_s3_class(p + expand_plot(), "gg")
})

test_that("scale_adjusted works with adjustment data", {
  dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty() |>
    adjust_for("z")

  p <- dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges(aes(edge_alpha = adjusted)) +
    geom_dag_point(aes(shape = adjusted))

  # Test that scale_adjusted can be added
  expect_s3_class(p + scale_adjusted(include_alpha = TRUE), "gg")
})
