# Geometry and scoring helpers for evaluating DAG layouts. All functions are
# pure: they take plain coordinate and edge data frames and return values
# without touching tidy_dagitty objects.

#' Distance from points to a line segment
#'
#' Computes the true Euclidean distance from each point to the segment from
#' `(x, y)` to `(xend, yend)`, clamped to the endpoints. A zero-length segment
#' is treated as a point, so the plain point-to-point distance is returned.
#'
#' @param px,py Numeric vectors of point coordinates.
#' @param x,y,xend,yend Scalar segment endpoint coordinates.
#' @return Numeric vector of distances, one per point.
#' @noRd
dist_to_edge <- function(px, py, x, y, xend, yend) {
  dx <- xend - x
  dy <- yend - y
  len2 <- dx^2 + dy^2

  # Projection parameter along the segment, clamped so points beyond an
  # endpoint measure their distance to that endpoint. A zero-length segment
  # makes the projection 0 / 0 = NaN; setting t = 0 there collapses the
  # formula below to the plain point-to-point distance without branching on
  # `len2`, which stays safe if it ever has length greater than one.
  t <- ((px - x) * dx + (py - y) * dy) / len2
  t[is.nan(t)] <- 0
  t <- pmin(pmax(t, 0), 1)

  sqrt((px - (x + t * dx))^2 + (py - (y + t * dy))^2)
}

#' Sample points along a drawn curved edge
#'
#' Models the drawn curve as a quadratic Bezier that passes through the
#' control point implied by inverting `ctrl_point_to_curvature()`: the point
#' at signed perpendicular offset `curvature_to_ctrl_offset()` from the edge
#' midpoint. Positive curvature bows below a left-to-right edge, matching the
#' `grid::curveGrob()` convention used throughout ggdag.
#'
#' @param x,y,xend,yend Scalar edge endpoint coordinates.
#' @param curvature Scalar curvature in `(-1, 1)`; 0 gives a straight edge.
#' @param n Number of points to sample along the curve.
#' @return A data frame with columns `x` and `y`; the first and last rows are
#'   exactly the edge endpoints.
#' @noRd
sample_curved_edge <- function(x, y, xend, yend, curvature, n = 24) {
  t <- seq(0, 1, length.out = n)

  dx <- xend - x
  dy <- yend - y
  len <- sqrt(dx^2 + dy^2)

  if (len == 0) {
    return(data.frame(x = rep(x, n), y = rep(y, n)))
  }

  # Point the curve passes through at its midpoint, at the signed
  # perpendicular offset that ctrl_point_to_curvature() maps back to
  # `curvature`.
  offset <- curvature_to_ctrl_offset(curvature, x, y, xend, yend)
  mx <- (x + xend) / 2
  my <- (y + yend) / 2
  through_x <- mx + offset * dy / len
  through_y <- my - offset * dx / len

  # Bezier control point placed so the curve passes through (through_x,
  # through_y) at t = 0.5.
  ctrl_x <- 2 * through_x - mx
  ctrl_y <- 2 * through_y - my

  data.frame(
    x = (1 - t)^2 * x + 2 * t * (1 - t) * ctrl_x + t^2 * xend,
    y = (1 - t)^2 * y + 2 * t * (1 - t) * ctrl_y + t^2 * yend
  )
}

#' Convert curvature to a signed control-point offset
#'
#' Exact inverse of the mapping in `ctrl_point_to_curvature()`, which
#' compresses the signed perpendicular offset `d` of a control point at the
#' edge midpoint to `atan(2 * d / len) * 2 / pi`.
#'
#' @param curvature Scalar curvature in `(-1, 1)`.
#' @param x,y,xend,yend Scalar edge endpoint coordinates.
#' @return The signed perpendicular offset `d = (len / 2) * tan(curvature *
#'   pi / 2)`.
#' @noRd
curvature_to_ctrl_offset <- function(curvature, x, y, xend, yend) {
  len <- sqrt((xend - x)^2 + (yend - y)^2)
  (len / 2) * tan(curvature * pi / 2)
}

#' Drawn node radius in data units
#'
#' The time-ordered layout engine works on a grid where nodes sit 180 units
#' apart per layer and a default node (size 16) is drawn with radius 26, so
#' the drawn radius in data units is `26 / 180` scaled linearly by
#' `node_size`.
#'
#' @param node_size Node point size, as passed to `geom_dag_node()`.
#' @return Scalar node radius in data units.
#' @noRd
node_radius_data <- function(node_size = 16) {
  (26 / 180) * node_size / 16
}

#' Count geometric crossings between edges
#'
#' Counts pairs of edges whose straight segments properly cross. Edges that
#' share an endpoint never count as crossing.
#'
#' @param coords Data frame with columns `name`, `x`, and `y`.
#' @param edges_df Data frame of directed edges with columns `name` and `to`.
#'   Rows with `to = NA` represent terminal or isolated nodes and are ignored.
#' @return Integer count of crossing pairs.
#' @noRd
count_edge_crossings <- function(coords, edges_df) {
  edges_df <- edges_df[!is.na(edges_df$to), , drop = FALSE]
  n <- nrow(edges_df)
  if (n < 2) {
    return(0L)
  }

  x <- coords$x[match(edges_df$name, coords$name)]
  y <- coords$y[match(edges_df$name, coords$name)]
  xend <- coords$x[match(edges_df$to, coords$name)]
  yend <- coords$y[match(edges_df$to, coords$name)]

  pairs <- which(upper.tri(diag(n)), arr.ind = TRUE)
  i <- pairs[, 1]
  j <- pairs[, 2]

  shared <- edges_df$name[i] == edges_df$name[j] |
    edges_df$name[i] == edges_df$to[j] |
    edges_df$to[i] == edges_df$name[j] |
    edges_df$to[i] == edges_df$to[j]

  # Cross product of (b - a) with (p - a): the sign says which side of the
  # directed line a-b the point p falls on.
  side <- function(ax, ay, bx, by, px, py) {
    (bx - ax) * (py - ay) - (by - ay) * (px - ax)
  }

  # Segments properly cross when each one's endpoints straddle the line
  # through the other.
  d1 <- side(x[i], y[i], xend[i], yend[i], x[j], y[j])
  d2 <- side(x[i], y[i], xend[i], yend[i], xend[j], yend[j])
  d3 <- side(x[j], y[j], xend[j], yend[j], x[i], y[i])
  d4 <- side(x[j], y[j], xend[j], yend[j], xend[i], yend[i])

  sum(!shared & d1 * d2 < 0 & d3 * d4 < 0)
}

#' Count nodes that edges pass through
#'
#' Counts (edge, node) pairs where a node that is not an endpoint of the edge
#' sits closer to the drawn edge than `node_radius`. Straight edges use the
#' exact segment distance; curved edges use the minimum distance to points
#' sampled along the drawn curve.
#'
#' @param coords Data frame with columns `name`, `x`, and `y`.
#' @param edges_df Data frame of directed edges with columns `name` and `to`.
#'   Rows with `to = NA` represent terminal or isolated nodes and are ignored.
#' @param node_radius Clearance radius in data units.
#' @param curvature Optional numeric vector of per-edge curvatures aligned to
#'   the rows of `edges_df`, including any rows with `to = NA` (0 means
#'   straight). `NULL` treats every edge as straight.
#' @return Integer count of overlapping (edge, node) pairs.
#' @noRd
count_node_edge_overlaps <- function(
  coords,
  edges_df,
  node_radius,
  curvature = NULL
) {
  if (is.null(curvature)) {
    curvature <- rep(0, nrow(edges_df))
  } else if (length(curvature) != nrow(edges_df)) {
    abort(
      c(
        "{.arg curvature} must have one value per row of {.arg edges_df}.",
        "x" = "{.arg curvature} has length {length(curvature)}, but
               {.arg edges_df} has {nrow(edges_df)} row{?s}."
      ),
      error_class = "ggdag_type_error"
    )
  }

  directed <- !is.na(edges_df$to)
  edges_df <- edges_df[directed, , drop = FALSE]
  curvature <- curvature[directed]

  x <- coords$x[match(edges_df$name, coords$name)]
  y <- coords$y[match(edges_df$name, coords$name)]
  xend <- coords$x[match(edges_df$to, coords$name)]
  yend <- coords$y[match(edges_df$to, coords$name)]

  overlaps <- 0L
  for (i in seq_len(nrow(edges_df))) {
    other <- coords$name != edges_df$name[i] & coords$name != edges_df$to[i]
    if (!any(other)) {
      next
    }
    px <- coords$x[other]
    py <- coords$y[other]

    if (curvature[i] == 0) {
      clearance <- dist_to_edge(px, py, x[i], y[i], xend[i], yend[i])
    } else {
      pts <- sample_curved_edge(x[i], y[i], xend[i], yend[i], curvature[i])
      dists <- sqrt(
        outer(px, pts$x, `-`)^2 + outer(py, pts$y, `-`)^2
      )
      clearance <- apply(dists, 1, min)
    }

    overlaps <- overlaps + sum(clearance < node_radius)
  }

  as.integer(overlaps)
}

#' Count pairs of incident edges meeting at a narrow angle
#'
#' For each node, considers the rays of its incident edges pointing away from
#' the node and counts pairs of rays separated by strictly less than
#' `min_angle` degrees. Rays meeting at 180 degrees are never penalized.
#'
#' @param coords Data frame with columns `name`, `x`, and `y`.
#' @param edges_df Data frame of directed edges with columns `name` and `to`.
#'   Rows with `to = NA` represent terminal or isolated nodes and are ignored.
#' @param min_angle Angular threshold in degrees.
#' @return Integer count of narrow-angle ray pairs.
#' @noRd
count_narrow_angles <- function(coords, edges_df, min_angle = 15) {
  edges_df <- edges_df[!is.na(edges_df$to), , drop = FALSE]

  x <- coords$x[match(edges_df$name, coords$name)]
  y <- coords$y[match(edges_df$name, coords$name)]
  xend <- coords$x[match(edges_df$to, coords$name)]
  yend <- coords$y[match(edges_df$to, coords$name)]

  # Each edge contributes a ray at both endpoints, pointing away from the
  # node toward the other end.
  ray_node <- c(edges_df$name, edges_df$to)
  ray_angle <- atan2(c(yend - y, y - yend), c(xend - x, x - xend)) * 180 / pi
  zero_length <- c(x == xend & y == yend, x == xend & y == yend)
  ray_node <- ray_node[!zero_length]
  ray_angle <- ray_angle[!zero_length]

  narrow <- 0L
  for (angles in split(ray_angle, ray_node)) {
    if (length(angles) < 2) {
      next
    }
    gap <- abs(outer(angles, angles, `-`)) %% 360
    gap <- pmin(gap, 360 - gap)
    narrow <- narrow + sum(gap[upper.tri(gap)] < min_angle)
  }

  as.integer(narrow)
}

#' Stress of a layout against graph distances
#'
#' Sums `(euclidean - graph_distance)^2 / graph_distance^2` over all connected
#' node pairs, where graph distance is the unweighted shortest-path distance
#' on the undirected skeleton of the DAG. A layout that places every
#' connected pair exactly its graph distance apart scores 0. The value is
#' scale-dependent because euclidean distances are measured in data units
#' while graph distances are unit hop counts, so candidate layouts must be
#' compared in a common coordinate scale.
#'
#' @param coords Data frame with columns `name`, `x`, and `y`.
#' @param edges_df Data frame of directed edges with columns `name` and `to`.
#'   Rows with `to = NA` represent terminal or isolated nodes and are ignored.
#' @return Scalar non-negative stress value.
#' @noRd
layout_stress <- function(coords, edges_df) {
  edges_df <- edges_df[!is.na(edges_df$to), , drop = FALSE]

  graph <- igraph::graph_from_data_frame(
    edges_df,
    directed = FALSE,
    vertices = coords$name
  )
  graph_dist <- igraph::distances(graph)[coords$name, coords$name]

  euclidean <- sqrt(
    outer(coords$x, coords$x, `-`)^2 + outer(coords$y, coords$y, `-`)^2
  )

  connected <- upper.tri(graph_dist) & is.finite(graph_dist) & graph_dist > 0
  sum(
    (euclidean[connected] - graph_dist[connected])^2 /
      graph_dist[connected]^2
  )
}

#' Score a DAG layout
#'
#' Combines four aesthetic criteria into a single weighted score. Each
#' component is reported raw (unweighted); `total` is the weighted sum. Lower
#' is better.
#'
#' @param coords Data frame with columns `name`, `x`, and `y`.
#' @param edges_df Data frame of directed edges with columns `name` and `to`.
#'   Rows with `to = NA` represent terminal or isolated nodes and are ignored.
#' @param node_radius Clearance radius in data units for node-edge overlaps.
#' @param weights Named numeric vector weighting each component in `total`.
#' @return A list with elements `total`, `crossings`, `node_edge`, `angular`,
#'   and `stress`.
#' @noRd
score_layout <- function(
  coords,
  edges_df,
  node_radius = node_radius_data(),
  weights = c(crossings = 100, node_edge = 30, angular = 5, stress = 1)
) {
  crossings <- count_edge_crossings(coords, edges_df)
  node_edge <- count_node_edge_overlaps(coords, edges_df, node_radius)
  angular <- count_narrow_angles(coords, edges_df)
  stress <- layout_stress(coords, edges_df)

  total <- weights[["crossings"]] *
    crossings +
    weights[["node_edge"]] * node_edge +
    weights[["angular"]] * angular +
    weights[["stress"]] * stress

  list(
    total = total,
    crossings = crossings,
    node_edge = node_edge,
    angular = angular,
    stress = stress
  )
}
