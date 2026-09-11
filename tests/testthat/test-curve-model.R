# The model of a drawn curved edge, measured against the curve the drawing
# path actually builds.
#
# `sample_curved_edge()` is what the package believes a curved edge looks
# like: the automatic label engine treats its points as the ink an arc lays
# down, and the layout scorer prices crossings against them. What a curved
# ggarrow edge draws is `grid::curveGrob()`'s X-spline, so the model is only
# as good as its agreement with that spline.
#
# Nothing here hard-codes a deflection. Every expectation measures the spline
# inside the test: `ggarrow::grob_arrow_curve()` hands `grid::curveGrob()` the
# parameters `GeomDAGArrowCurve` draws with, takes the forced curve's
# `grid::xsplinePoints()` as the shaft, and widens that into the drawn
# polygon, so building the same grob and reading its realised points gives the
# path the arc runs along. `R/edge_extent.R` carries a constant for the same
# spline, but `curve_deflection_ratio` there is deliberately a generous bound
# (half the chord per unit of curvature, against the 0.486 to 0.488 the spline
# measures) because over-reserving panel room only costs white space, so it is
# not the number a model of the ink may use.

# Measuring the drawn spline ---------------------------------------------------

# Unit conversion and spline realisation both need an open device.
local_curve_device <- function(env = parent.frame()) {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off(), envir = env)
}

# The points grid realises for the curve a ggarrow arc is drawn along, in
# millimetres. `angle`, `ncp`, `shape`, `square`, and `open` are the values
# `ggarrow::GeomArrowCurve$draw_panel()` passes on and `GeomDAGArrowCurve`
# keeps.
drawn_curve_points <- function(x, y, xend, yend, curvature, ncp = 5L) {
  grob <- grid::curveGrob(
    x,
    y,
    xend,
    yend,
    default.units = "mm",
    curvature = curvature,
    angle = 90,
    ncp = ncp,
    shape = 0.5,
    square = FALSE,
    squareShape = 1L,
    inflect = FALSE,
    open = TRUE
  )
  realised <- grid::makeContent(grob)$children[[1]]
  points <- grid::xsplinePoints(realised)
  if (all(c("x", "y") %in% names(points))) {
    points <- list(points)
  }
  data.frame(
    x = unlist(lapply(points, function(p) grid::convertX(p$x, "mm", TRUE))),
    y = unlist(lapply(points, function(p) grid::convertY(p$y, "mm", TRUE)))
  )
}

# The signed perpendicular offset of each point from the chord, positive to
# the left of the chord read from `(x, y)` to `(xend, yend)`.
chord_offsets <- function(points, x, y, xend, yend) {
  length <- sqrt((xend - x)^2 + (yend - y)^2)
  ((xend - x) * (points$y - y) - (points$x - x) * (yend - y)) / length
}

# The deepest of those offsets, keeping its sign.
deepest_offset <- function(points, x, y, xend, yend) {
  offsets <- chord_offsets(points, x, y, xend, yend)
  offsets[[which.max(abs(offsets))]]
}

# The distance from each point to the nearest place on `path`.
distance_to_path <- function(points, path) {
  last <- nrow(path)
  vapply(
    seq_len(nrow(points)),
    function(i) {
      min(dist_to_edge(
        points$x[[i]],
        points$y[[i]],
        path$x[-last],
        path$y[-last],
        path$x[-1],
        path$y[-1]
      ))
    },
    numeric(1)
  )
}

# Chords of several lengths and directions, including one traced backwards, so
# that a model tied to a particular orientation is caught.
curve_model_chords <- list(
  horizontal_short = c(0, 0, 40, 0),
  horizontal_long = c(0, 0, 100, 0),
  vertical = c(10, 5, 10, 70),
  oblique = c(5, 5, 65, 45),
  oblique_reversed = c(65, 45, 5, 5)
)

curve_model_curvatures <- c(
  -0.9,
  -0.8,
  -0.5,
  -0.3,
  -0.1,
  0.1,
  0.3,
  0.5,
  0.8,
  0.9
)

curve_model_cases <- function(curvatures = curve_model_curvatures) {
  cases <- expand.grid(
    chord = names(curve_model_chords),
    curvature = curvatures,
    stringsAsFactors = FALSE
  )
  cases$case <- paste(cases$chord, cases$curvature, sep = " @ ")
  cases
}

# Depth ------------------------------------------------------------------------

test_that("the modelled arc is as deep as the arc grid draws", {
  local_curve_device()

  cases <- curve_model_cases()
  measure <- function(trace) {
    vapply(
      seq_len(nrow(cases)),
      function(i) {
        chord <- curve_model_chords[[cases$chord[[i]]]]
        points <- trace(
          chord[[1]],
          chord[[2]],
          chord[[3]],
          chord[[4]],
          cases$curvature[[i]]
        )
        deepest_offset(points, chord[[1]], chord[[2]], chord[[3]], chord[[4]])
      },
      numeric(1)
    )
  }

  modelled <- measure(function(x, y, xend, yend, curvature) {
    sample_curved_edge(x, y, xend, yend, curvature = curvature, n = 401)
  })
  drawn <- measure(drawn_curve_points)
  names(modelled) <- cases$case
  names(drawn) <- cases$case

  # The spline's depth per unit of curvature drifts from 0.4881 of the chord
  # at curvature 0.1 to 0.4857 at 0.9, so a model carrying one constant is
  # within 0.3 percent of it everywhere on this grid; 2 percent leaves room
  # for that without admitting a different formula.
  expect_equal(modelled, drawn, tolerance = 0.02)
  expect_lt(max(abs(modelled - drawn) / abs(drawn)), 0.02)
})

test_that("the modelled arc follows the drawn one along its whole length", {
  local_curve_device()

  # Depth alone does not make an obstacle: a model as deep as the spline but
  # differently shaped still reports ink where there is none. Past half a unit
  # of curvature the spline is deep enough that the shape a model of it can
  # reasonably take diverges (a depth-matched quadratic Bezier is 7 percent of
  # the chord off the spline at curvature 0.9, a circular arc 3 percent), so
  # the shape is pinned over the range the package draws at by default, where
  # both of those sit within 1.2 percent of the chord.
  cases <- curve_model_cases(c(-0.5, -0.3, -0.1, 0.1, 0.3, 0.5))
  deviation <- vapply(
    seq_len(nrow(cases)),
    function(i) {
      chord <- curve_model_chords[[cases$chord[[i]]]]
      length <- sqrt((chord[[3]] - chord[[1]])^2 + (chord[[4]] - chord[[2]])^2)
      modelled <- sample_curved_edge(
        chord[[1]],
        chord[[2]],
        chord[[3]],
        chord[[4]],
        curvature = cases$curvature[[i]],
        n = 201
      )
      drawn <- drawn_curve_points(
        chord[[1]],
        chord[[2]],
        chord[[3]],
        chord[[4]],
        cases$curvature[[i]]
      )
      max(distance_to_path(modelled, drawn)) / length
    },
    numeric(1)
  )
  names(deviation) <- cases$case

  expect_lt(max(deviation), 0.03)
})

# Similarity -------------------------------------------------------------------

test_that("scaling and rotating the endpoints scales and rotates the arc", {
  local_curve_device()

  curvature <- 0.4
  scale <- 2.5
  angle <- 37 * pi / 180
  shift <- c(12, -7)
  move <- function(points) {
    data.frame(
      x = shift[[1]] +
        scale * (cos(angle) * points$x - sin(angle) * points$y),
      y = shift[[2]] + scale * (sin(angle) * points$x + cos(angle) * points$y)
    )
  }

  plain <- sample_curved_edge(0, 0, 60, 0, curvature = curvature, n = 201)
  ends <- move(data.frame(x = c(0, 60), y = c(0, 0)))
  moved <- sample_curved_edge(
    ends$x[[1]],
    ends$y[[1]],
    ends$x[[2]],
    ends$y[[2]],
    curvature = curvature,
    n = 201
  )

  expect_equal(moved, move(plain), tolerance = 1e-9)

  # The spline is similarity-invariant, so the depth the model carries as a
  # fraction of its chord has to be the spline's fraction, at either size.
  drawn <- drawn_curve_points(0, 0, 60, 0, curvature)
  drawn_ratio <- abs(deepest_offset(drawn, 0, 0, 60, 0)) / 60
  expect_equal(
    abs(deepest_offset(plain, 0, 0, 60, 0)) / 60,
    drawn_ratio,
    tolerance = 0.02
  )
  expect_equal(
    abs(deepest_offset(
      moved,
      ends$x[[1]],
      ends$y[[1]],
      ends$x[[2]],
      ends$y[[2]]
    )) /
      (scale * 60),
    drawn_ratio,
    tolerance = 0.02
  )
})

# Sign -------------------------------------------------------------------------

test_that("positive curvature bows to the side grid bows to", {
  local_curve_device()

  # The side is a contract, not a detail: `R/layout_time_ordered.R` chooses the
  # sign it hands the model from the side it wants the edge to bow to, and
  # `curved()` documents the same convention for the user. Positive curvature
  # bows below a left-to-right edge, which is to the right of the chord read
  # from start to end, so the signed offset is negative.
  cases <- curve_model_cases(c(-0.6, 0.6))
  sides <- function(trace) {
    vapply(
      seq_len(nrow(cases)),
      function(i) {
        chord <- curve_model_chords[[cases$chord[[i]]]]
        points <- trace(
          chord[[1]],
          chord[[2]],
          chord[[3]],
          chord[[4]],
          cases$curvature[[i]]
        )
        sign(deepest_offset(
          points,
          chord[[1]],
          chord[[2]],
          chord[[3]],
          chord[[4]]
        ))
      },
      numeric(1)
    )
  }

  modelled <- sides(function(x, y, xend, yend, curvature) {
    sample_curved_edge(x, y, xend, yend, curvature = curvature, n = 401)
  })
  names(modelled) <- cases$case
  drawn <- sides(drawn_curve_points)
  names(drawn) <- cases$case

  expected <- ifelse(cases$curvature > 0, -1, 1)
  names(expected) <- cases$case
  expect_identical(modelled, drawn)
  expect_identical(modelled, expected)

  # Spelled out once, away from the measurement, so the convention is readable.
  below <- sample_curved_edge(0, 0, 2, 0, curvature = 0.5, n = 51)
  expect_true(all(below$y[-c(1, 51)] < 0))
  above <- sample_curved_edge(0, 0, 2, 0, curvature = -0.5, n = 51)
  expect_true(all(above$y[-c(1, 51)] > 0))
})

# Endpoints and degenerate edges -----------------------------------------------

test_that("the traced arc starts and ends exactly at the endpoints", {
  for (curvature in c(-0.8, -0.3, 0, 0.3, 0.8)) {
    points <- sample_curved_edge(
      1.5,
      -2,
      4,
      6.25,
      curvature = curvature,
      n = 37
    )
    expect_equal(points$x[[1]], 1.5, tolerance = 1e-12)
    expect_equal(points$y[[1]], -2, tolerance = 1e-12)
    expect_equal(points$x[[37]], 4, tolerance = 1e-12)
    expect_equal(points$y[[37]], 6.25, tolerance = 1e-12)
  }
})

test_that("a zero curvature traces the chord", {
  points <- sample_curved_edge(1, 2, 4, 6, curvature = 0, n = 25)
  expect_equal(
    max(abs(chord_offsets(points, 1, 2, 4, 6))),
    0,
    tolerance = 1e-12
  )
})

test_that("a zero-length edge traces its own position without erroring", {
  expect_no_error(sample_curved_edge(3, 4, 3, 4, curvature = 0.5, n = 12))

  points <- sample_curved_edge(3, 4, 3, 4, curvature = 0.5, n = 12)
  expect_equal(points$x, rep(3, 12), tolerance = 1e-12)
  expect_equal(points$y, rep(4, 12), tolerance = 1e-12)
})
