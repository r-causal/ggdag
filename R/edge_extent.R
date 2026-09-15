# How much room a curved ggarrow edge asks of the panel it is drawn in.
#
# A ggarrow arc is bent by `grid::curveGrob()`, which works in inches: how far
# the bow reaches away from the chord is a property of the drawn page and not
# of the data, and it is settled long after the scales have trained. A panel
# that never saw the bow draws it outside itself, where it is cut off, taking
# whole edges and the nodes beside them off the figure. The ggraph arcs are
# real data rows, so the scales already train on them; these helpers give the
# ggarrow arcs the same standing by estimating where the ink will land.
#
# The estimate can only be an approximation, since the device is unknown while
# the scales train, so it is deliberately a generous one: reserving a little
# too much room costs some white space, while reserving too little cuts the
# picture.

# The deepest a drawn curve reaches away from its chord, as a fraction of the
# chord's length. `grid::curveGrob()` draws an X-spline through control points
# a `curvature` apart, and the deflection grows with `ncp` toward half the
# chord: at the default of five control points it is 0.4876 of the chord per
# unit of curvature, and it stays under 0.5 for every `ncp`, for every `angle`,
# and for curvature past the (-1, 1) ggdag draws in. Half therefore bounds the
# ink for any of them.
curve_deflection_ratio <- 0.5

# The shape of the panel assumed while the scales train, as a ratio of width
# to height. The bow of an arc across the panel grows with the panel's width
# and the bow along it with the panel's height, so a single figure bounds both
# when it is read as the widest panel to reserve room across and, inverted, as
# the tallest panel to reserve room along. Panels between 4:7 and 7:4 are
# covered, which spans the shapes a figure is ordinarily drawn at.
nominal_panel_aspect <- 1.75

# How finely the modelled curve is sampled when its extent is measured.
curve_extent_n <- 25

# How many times the reserved room is fed back into the estimate, and the
# relative change at which the feedback stops.
curve_extent_iterations <- 20
curve_extent_tolerance <- 1e-4

# The most the reservation may stretch an axis, as a multiple of the span the
# rest of the data trains. An arc that bows further than the panel is tall
# cannot be fitted into any panel, so the feedback below would not settle;
# such an edge is given what room this allows and drawn past it.
curve_extent_max_growth <- 8

#' The shape of a drawn curve, as a fraction of its deepest deflection
#'
#' `grid::curveGrob()` draws close to a circular arc through the two ends of
#' the chord, so the deflection along the chord is read off the circle that
#' passes through both ends and reaches `deflection` at the middle. An arc
#' deeper than half its chord is more than a semicircle, which no circle
#' through the ends describes; such an arc takes the semicircle the formula
#' gives at exactly half, which is the fullest shape it can return.
#'
#' @param deflection Numeric vector of deflections, one per edge, as a
#'   fraction of the chord's length.
#' @param t Numeric vector of positions along the chord, from 0 to 1.
#' @return A matrix of one row per edge and one column per position, holding
#'   the deflection there as a fraction of the deepest deflection.
#' @noRd
curve_shape <- function(deflection, t) {
  depth <- pmin(abs(deflection), 0.5)
  radius <- (0.25 + depth^2) / (2 * depth)

  along <- (t - 0.5)^2
  shape <- sqrt(pmax(outer(radius^2, along, `-`), 0)) - (radius - depth)
  shape <- shape / depth

  # A chord is a curve of no depth, and reaches nowhere away from itself.
  shape[depth == 0, ] <- 0
  shape
}

#' Bounding box of the curve a ggarrow edge draws
#'
#' Models the drawn edge as a chord deflected along the shape
#' `curve_shape()` gives, reaching half the chord's length per unit of
#' curvature at its deepest and bowing below a left-to-right edge for a
#' positive curvature, as `grid::curveGrob()` draws it. The curve is bent on
#' the page, so the model is mapped back into data units through the spans of
#' the two axes and the assumed shape of the panel.
#'
#' @param x,y,xend,yend Numeric vectors of edge endpoint coordinates.
#' @param curvature Numeric vector of signed curvatures, one per edge.
#' @param x_span,y_span The spans of the two axes, in data units.
#' @param aspect The assumed ratio of panel width to panel height.
#' @return A data frame of `xmin`, `xmax`, `ymin`, and `ymax`, one row per
#'   edge, each the extent of the modelled curve including its endpoints.
#' @noRd
curved_edge_bounds <- function(
  x,
  y,
  xend,
  yend,
  curvature,
  x_span,
  y_span,
  aspect = nominal_panel_aspect
) {
  t <- seq(0, 1, length.out = curve_extent_n)
  deflection <- curve_deflection_ratio * curvature
  shape <- curve_shape(deflection, t)

  dx <- xend - x
  dy <- yend - y

  # The bow is perpendicular to the chord on the page, so the room it asks of
  # one axis is set by the edge's span along the other, scaled by the shape of
  # the panel the chord is drawn in.
  x_bow <- deflection * (dy / y_span) * x_span * aspect
  y_bow <- -deflection * (dx / x_span) * y_span * aspect

  curve_x <- outer(x, rep(1, curve_extent_n)) + outer(dx, t) + x_bow * shape
  curve_y <- outer(y, rep(1, curve_extent_n)) + outer(dy, t) + y_bow * shape

  data.frame(
    xmin = apply(curve_x, 1, min),
    xmax = apply(curve_x, 1, max),
    ymin = apply(curve_y, 1, min),
    ymax = apply(curve_y, 1, max)
  )
}

#' Per-edge curvature of a ggarrow arc layer
#'
#' Resolves the curvature each edge is drawn at the way
#' `GeomDAGArrowCurve$draw_panel()` does: the layer's own curvature where no
#' edge carries one, and otherwise the `edge_curvature` aesthetic, with an
#' edge that carries none drawn as a chord or at the layer's curvature
#' according to `unset`.
#'
#' @param data The stat's data.
#' @param curvature The layer's curvature.
#' @param unset What an edge with an unset `edge_curvature` is drawn as.
#' @return A numeric vector of curvatures, one per row, or `NULL` where the
#'   `edge_curvature` aesthetic holds something the layer cannot draw.
#' @noRd
resolve_edge_curvature <- function(data, curvature, unset) {
  has_edge_curvature <- "edge_curvature" %in%
    names(data) &&
    !all(is.na(data$edge_curvature))

  if (!has_edge_curvature) {
    return(rep(curvature, nrow(data)))
  }

  if (!is.numeric(data$edge_curvature)) {
    # The geom reports this when it draws; the panel says nothing about it.
    return(NULL)
  }

  resolved <- data$edge_curvature
  resolved[is.na(resolved)] <- if (identical(unset, "curvature")) {
    curvature
  } else {
    0
  }
  resolved
}

#' Train the position scales on the arcs a ggarrow edge layer draws
#'
#' Adds the extent of each drawn curve to the layer's data as `xmin`, `xmax`,
#' `ymin`, and `ymax`, which the position scales train on, so the panel makes
#' room for the ink.
#'
#' A reservation widens the panel, and a wider panel holds a deeper bow, since
#' the curve keeps its size on the page while the axis it is measured against
#' grows. The estimate is therefore fed its own result until it settles.
#'
#' @param data The stat's data, already filtered to edge rows.
#' @param scales The panel's scales.
#' @param curvature The layer's curvature.
#' @param unset What an edge with an unset `edge_curvature` is drawn as.
#' @return `data`, with the four extent columns added where the layer draws a
#'   curve, and unchanged otherwise.
#' @noRd
reserve_curved_edge_room <- function(data, scales, curvature, unset) {
  if (nrow(data) == 0) {
    return(data)
  }

  resolved <- resolve_edge_curvature(data, curvature, unset)
  if (is.null(resolved) || all(!is.finite(resolved) | resolved == 0)) {
    return(data)
  }
  resolved[!is.finite(resolved)] <- 0

  ranges <- trained_position_ranges(scales)
  if (is.null(ranges)) {
    return(data)
  }

  x_range <- ranges$x
  y_range <- ranges$y
  x_limit <- curve_extent_max_growth * diff(x_range)
  y_limit <- curve_extent_max_growth * diff(y_range)

  for (iteration in seq_len(curve_extent_iterations)) {
    bounds <- curved_edge_bounds(
      data$x,
      data$y,
      data$xend,
      data$yend,
      resolved,
      x_span = diff(x_range),
      y_span = diff(y_range)
    )

    # An edge with a missing endpoint models no curve, and says nothing
    # about the room the panel needs.
    next_x <- range(x_range, bounds$xmin, bounds$xmax, na.rm = TRUE)
    next_y <- range(y_range, bounds$ymin, bounds$ymax, na.rm = TRUE)
    settled <- diff(next_x) - diff(x_range) <
      curve_extent_tolerance *
        diff(x_range) &&
      diff(next_y) - diff(y_range) < curve_extent_tolerance * diff(y_range)

    x_range <- next_x
    y_range <- next_y
    if (settled || diff(x_range) > x_limit || diff(y_range) > y_limit) {
      break
    }
  }

  # Limits the user set are the panel, whatever the edges would like of it.
  if (!is.null(scales$x$limits)) {
    bounds$xmin <- clamp_to(bounds$xmin, ranges$x)
    bounds$xmax <- clamp_to(bounds$xmax, ranges$x)
  }
  if (!is.null(scales$y$limits)) {
    bounds$ymin <- clamp_to(bounds$ymin, ranges$y)
    bounds$ymax <- clamp_to(bounds$ymax, ranges$y)
  }

  data$xmin <- bounds$xmin
  data$xmax <- bounds$xmax
  data$ymin <- bounds$ymin
  data$ymax <- bounds$ymax
  data
}

#' The data ranges the position scales have trained so far
#'
#' @param scales The panel's scales.
#' @return A list of the `x` and `y` ranges, or `NULL` where either axis has no
#'   finite continuous range to measure a curve against.
#' @noRd
trained_position_ranges <- function(scales) {
  if (is.null(scales) || is.null(scales$x) || is.null(scales$y)) {
    return(NULL)
  }
  if (scales$x$is_discrete() || scales$y$is_discrete()) {
    return(NULL)
  }

  x_range <- scales$x$dimension()
  y_range <- scales$y$dimension()
  if (!all(is.finite(c(x_range, y_range)))) {
    return(NULL)
  }
  # A panel with no width or no height has no shape for a curve to be bent in.
  if (diff(x_range) <= 0 || diff(y_range) <= 0) {
    return(NULL)
  }

  list(x = x_range, y = y_range)
}

clamp_to <- function(value, range) {
  pmin(pmax(value, range[[1]]), range[[2]])
}
