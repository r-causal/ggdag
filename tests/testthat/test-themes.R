test_that("themes look good", {
  p <- ggdag(test_dag)
  expect_identical(theme_dag, theme_dag_blank)
  expect_identical(theme_dag_gray, theme_dag_grey)
  expect_identical(theme_dag_gray_grid, theme_dag_grey_grid)
  expect_doppelganger("theme_dag()", p + theme_dag())
  expect_doppelganger("theme_dag_grid()", p + theme_dag_grid())
  expect_doppelganger("theme_dag_gray()", p + theme_dag_gray())
  expect_doppelganger("theme_dag_gray_grid()", p + theme_dag_gray_grid())
})

test_that("themes work with facets", {
  p_facet <- ggdag_adjustment_set(test_dag) + theme_dag()
  expect_doppelganger("theme_dag() with facets", p_facet)

  p_facet_grid <- ggdag_adjustment_set(test_dag) + theme_dag_grid()
  expect_doppelganger("theme_dag_grid() with facets", p_facet_grid)

  p_facet_gray <- ggdag_adjustment_set(test_dag) + theme_dag_gray()
  expect_doppelganger("theme_dag_gray() with facets", p_facet_gray)

  p_facet_gray_grid <- ggdag_adjustment_set(test_dag) + theme_dag_gray_grid()
  expect_doppelganger("theme_dag_gray_grid() with facets", p_facet_gray_grid)
})

test_that("theme dots override elements the theme sets", {
  # the roxygen promises that ... is passed to theme(), so a user value has to
  # replace the theme's own setting for that element rather than clash with it.
  # Each variant is built separately so one clash does not hide the others.
  overridden <- function(theme_fn, ...) {
    tryCatch(theme_fn(...), error = function(e) NULL)
  }

  expect_equal(
    overridden(
      theme_dag,
      axis.text = ggplot2::element_text(size = 5)
    )$axis.text$size,
    5
  )
  expect_equal(
    overridden(
      theme_dag,
      strip.text = ggplot2::element_text(size = 5)
    )$strip.text$size,
    5
  )
  expect_s3_class(
    overridden(theme_dag, panel.grid = ggplot2::element_line())$panel.grid,
    "element_line"
  )
  expect_equal(
    overridden(
      theme_dag_grid,
      axis.title = ggplot2::element_text(size = 5)
    )$axis.title$size,
    5
  )
  expect_s3_class(
    overridden(
      theme_dag_grey,
      panel.grid.major = ggplot2::element_blank()
    )$panel.grid.major,
    "element_blank"
  )
  expect_s3_class(
    overridden(theme_dag_grey, axis.ticks = ggplot2::element_line())$axis.ticks,
    "element_line"
  )
  expect_s3_class(
    overridden(
      theme_dag_grey_grid,
      axis.ticks = ggplot2::element_line()
    )$axis.ticks,
    "element_line"
  )
})

test_that("theme dots still reach elements the theme does not set", {
  expect_equal(theme_dag(legend.position = "none")$legend.position, "none")
})

test_that("themes keep their presets when no dots are supplied", {
  expect_s3_class(theme_dag()$axis.text, "element_blank")
  expect_s3_class(theme_dag()$axis.title, "element_blank")
  expect_s3_class(theme_dag()$panel.grid, "element_blank")
  expect_true(attr(theme_dag(), "complete"))

  expect_s3_class(theme_dag_grid()$axis.text, "element_blank")
  expect_true(attr(theme_dag_grid(), "complete"))

  expect_s3_class(theme_dag_grey()$axis.ticks, "element_blank")
  expect_s3_class(theme_dag_grey()$panel.grid.major, "element_line")
  expect_true(attr(theme_dag_grey(), "complete"))

  expect_s3_class(theme_dag_grey_grid()$axis.text, "element_blank")
  expect_true(attr(theme_dag_grey_grid(), "complete"))
})
