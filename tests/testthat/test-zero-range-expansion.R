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
  # An axis that spans anything at all keeps the multiplicative expansion of
  # its `expand_plot()` call site, a tenth of the drawn span on each side for
  # `ggdag()` and a quarter on the x axis of the faceted path plots. The
  # expectation is derived from what the layers of the built plot draw rather
  # than written out, so this block follows the layout instead of pinning the
  # coordinates one layout gives today.
  expanded <- function(limits, fraction) {
    limits + c(-1, 1) * fraction * diff(limits)
  }

  ranges <- built_ranges(ggdag(test_dag))
  extent <- drawn_extent(ggdag(test_dag))

  expect_equal(ranges$x, expanded(extent$x, 0.1), tolerance = 1e-8)
  expect_equal(ranges$y, expanded(extent$y, 0.1), tolerance = 1e-8)

  paths <- ggdag_paths(test_dag, from = "x", to = "y")
  path_ranges <- built_ranges(paths)
  path_extent <- drawn_extent(paths)

  expect_equal(path_ranges$x, expanded(path_extent$x, 0.25), tolerance = 1e-8)
  expect_equal(path_ranges$y, expanded(path_extent$y, 0.1), tolerance = 1e-8)
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

# An axis can be flat in the picture without being flat in the arithmetic. A
# circle layout places a node at `sin(pi)`, which is 1.224647e-16 rather than
# 0, so a pair of nodes the reader sees side by side trains an axis to a span
# of a fraction of the last bit of a double. Such a span is not zero and, when
# the smaller endpoint is exactly 0, no relative test can call it one either,
# so the blocks below pin both the rule and the picture it produces.

test_that("is_zero_range() reads equal and relatively equal endpoints", {
  expect_true(is_zero_range(c(0, 0)))
  expect_true(is_zero_range(c(2.5, 2.5)))
  # The two endpoints are one and the same double.
  expect_true(is_zero_range(c(1, 1 + 1e-16)))
  expect_true(is_zero_range(c(1e6, 1e6 + 1e-9)))

  expect_false(is_zero_range(c(1e6, 1e6 + 1)))
  expect_false(is_zero_range(NULL))
  expect_false(is_zero_range(c(NA_real_, 1)))
  expect_false(is_zero_range(c(NA_real_, NA_real_)))
})

test_that("is_zero_range() reads a span of floating-point noise as flat", {
  expect_true(is_zero_range(c(0, 1.224647e-16)))
  expect_true(is_zero_range(range(sin(c(0, pi)))))
  expect_true(is_zero_range(c(-1.2e-16, 0)))
})

test_that("is_zero_range() leaves a real span alone at any scale", {
  expect_false(is_zero_range(c(0, 1e-6)))
  expect_false(is_zero_range(c(0, 0.001)))
  expect_false(is_zero_range(c(0, 1)))
  expect_false(is_zero_range(c(0, 1e6)))
  expect_false(is_zero_range(c(-1e6, 1e6)))
})

test_that("a pair level to within noise takes the additive expansion", {
  ranges <- built_ranges(ggdag(circle_pair_dag(), layout = "circle"))

  # The nodes sit at x -1 and 1, so the y axis borrows a quarter of that span.
  expect_equal(ranges$x, c(-1.2, 1.2), tolerance = 1e-8)
  expect_equal(ranges$y, c(-0.25, 0.25), tolerance = 1e-8)
})

test_that("a pair level to within noise is drawn level at every size", {
  skip_if_not_installed("ragg")

  p <- ggdag(circle_pair_dag(), layout = "circle")
  sizes <- list(c(4.5, 3.5), c(6, 6), c(9, 3.5))
  names(sizes) <- vapply(
    sizes,
    function(size) paste0(size[[1]], "x", size[[2]]),
    character(1)
  )

  # Half a millimetre is a tenth of the drawn node radius, well inside what
  # the reader could see, and the two centres differ by a thousandth of that
  # once the axis is read as flat.
  drop <- vapply(
    sizes,
    function(size) diff(range(node_centres_mm(p, size)$y)),
    numeric(1)
  )

  expect_equal(names(drop)[drop > 0.5], character(0))
})

test_that("an axis with a small but real span keeps its expansion", {
  ranges <- built_ranges(ggdag(shallow_pair_dag()))

  expect_equal(ranges$x, c(-0.1, 1.1), tolerance = 1e-8)
  expect_equal(ranges$y, c(-1e-4, 1.1e-3), tolerance = 1e-8)
})
