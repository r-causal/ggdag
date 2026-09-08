# A DAG whose nodes all share one coordinate trains that axis to a zero-width
# range. A multiplicative expansion of a zero range adds nothing, so the scale
# falls back to a zero-width placeholder centred on the single value, and under
# `coord_fixed()` the panel is only millimetres tall and the node discs are
# clipped flat. Every `expand_plot()` call site instead gives a zero-range axis
# an additive expansion of one eighth of the other axis's raw span on each side.
# The five-node chain below spans 4 units on its non-degenerate axis, so the
# degenerate axis expands to c(-0.5, 0.5). Non-degenerate axes keep the
# multiplicative expansion they already had.

quarter_span <- c(-0.5, 0.5)

test_that("ggdag() expands a zero-range y axis", {
  # The placeholder without the additive expansion is c(-0.1, 0.1).
  ranges <- built_ranges(ggdag(flat_chain_dag()))

  expect_equal(ranges$y, quarter_span, tolerance = 1e-8)
  expect_equal(ranges$x, c(-0.4, 4.4), tolerance = 1e-8)
})

test_that("geom_dag() expands a zero-range y axis", {
  # The placeholder without the additive expansion is c(-0.1, 0.1).
  p <- ggplot2::ggplot(tidy_dagitty(flat_chain_dag()), aes_dag()) + geom_dag()
  ranges <- built_ranges(p)

  expect_equal(ranges$y, quarter_span, tolerance = 1e-8)
  expect_equal(ranges$x, c(-0.4, 4.4), tolerance = 1e-8)
})

test_that("ggdag_adjustment_set() expands a zero-range y axis", {
  # The placeholder without the additive expansion is c(-0.2, 0.2).
  p <- ggdag_adjustment_set(flat_chain_dag(), exposure = "a", outcome = "e")
  ranges <- built_ranges(p)

  expect_equal(ranges$y, quarter_span, tolerance = 1e-8)
  expect_equal(ranges$x, c(-1, 5), tolerance = 1e-8)
})

test_that("ggdag_adjust() expands a zero-range y axis", {
  # The placeholder without the additive expansion is c(-0.2, 0.2).
  ranges <- built_ranges(ggdag_adjust(flat_chain_dag(), var = "c"))

  expect_equal(ranges$y, quarter_span, tolerance = 1e-8)
  expect_equal(ranges$x, c(-0.4, 4.4), tolerance = 1e-8)
})

test_that("ggdag_drelationship() expands a zero-range y axis", {
  # The placeholder without the additive expansion is c(-0.2, 0.2).
  p <- ggdag_drelationship(flat_chain_dag(), from = "a", to = "e")
  ranges <- built_ranges(p)

  expect_equal(ranges$y, quarter_span, tolerance = 1e-8)
  expect_equal(ranges$x, c(-0.4, 4.4), tolerance = 1e-8)
})

test_that("ggdag_equivalent_dags() expands a zero-range y axis", {
  # The placeholder without the additive expansion is c(-0.25, 0.25).
  ranges <- built_ranges(ggdag_equivalent_dags(flat_chain_dag()))

  expect_equal(ranges$y, quarter_span, tolerance = 1e-8)
  expect_equal(ranges$x, c(-1, 5), tolerance = 1e-8)
})

test_that("ggdag_paths() expands a zero-range y axis", {
  # The placeholder without the additive expansion is c(-0.1, 0.1).
  ranges <- built_ranges(ggdag_paths(flat_chain_dag(), from = "a", to = "e"))

  expect_equal(ranges$y, quarter_span, tolerance = 1e-8)
  expect_equal(ranges$x, c(-1, 5), tolerance = 1e-8)
})

test_that("ggdag_paths_fan() expands a zero-range y axis", {
  # The placeholder without the additive expansion is c(-0.1, 0.1).
  p <- ggdag_paths_fan(flat_chain_dag(), from = "a", to = "e")
  ranges <- built_ranges(p)

  expect_equal(ranges$y, quarter_span, tolerance = 1e-8)
  expect_equal(ranges$x, c(-1, 5), tolerance = 1e-8)
})

test_that("a zero-range x axis is expanded the same way", {
  # The placeholder without the additive expansion is c(-0.1, 0.1).
  ranges <- built_ranges(ggdag(upright_chain_dag()))

  expect_equal(ranges$x, quarter_span, tolerance = 1e-8)
  expect_equal(ranges$y, c(-0.4, 4.4), tolerance = 1e-8)
})

test_that("a zero-range x axis is expanded in a faceted plot", {
  # The placeholder without the additive expansion is c(-0.25, 0.25).
  ranges <- built_ranges(ggdag_paths(upright_chain_dag(), from = "a", to = "e"))

  expect_equal(ranges$x, quarter_span, tolerance = 1e-8)
  expect_equal(ranges$y, c(-0.4, 4.4), tolerance = 1e-8)
})

test_that("plots with two non-degenerate axes keep their expansion", {
  ranges <- built_ranges(ggdag(test_dag))

  expect_equal(
    ranges$x,
    c(0.625099210480681, 4.30680916268357),
    tolerance = 1e-8
  )
  expect_equal(ranges$y, c(-0.6426, 0.6366), tolerance = 1e-8)

  path_ranges <- built_ranges(ggdag_paths(test_dag, from = "x", to = "y"))

  expect_equal(
    path_ranges$x,
    c(0.164885466455319, 4.76702290670894),
    tolerance = 1e-8
  )
  expect_equal(path_ranges$y, c(-0.6426, 0.6366), tolerance = 1e-8)
})

test_that("a plot with two zero-range axes is left alone", {
  ranges <- built_ranges(ggdag(lone_node_dag()))

  expect_equal(ranges$x, c(-0.1, 0.1), tolerance = 1e-8)
  expect_equal(ranges$y, c(-0.1, 0.1), tolerance = 1e-8)
})

test_that("a flat chain under coord_fixed() draws round nodes", {
  p <- ggdag(flat_chain_dag()) + ggplot2::coord_fixed()
  ranges <- built_ranges(p)

  # `coord_fixed()` maps both axes to the same number of millimetres per data
  # unit, so the panel is only as tall as the y range is wide relative to x.
  # A fifth of the width leaves room for the 12 mm node discs at 7 x 5 inches.
  stopifnot(diff(ranges$y) >= diff(ranges$x) / 5)

  expect_doppelganger("flat chain under coord_fixed", p)
})
