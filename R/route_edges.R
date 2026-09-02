# Automatic edge routing: find the nodes that sit on straight edges and write
# a deterministic per-edge curvature that steers each blocked edge around
# them. `tidy_dagitty()` applies the safety net when the `auto_curve` option
# is `TRUE`; curvature the user set through `curved()`, `curve_edge()`, or
# dagitty control points is never overridden.

#' Find nodes that obstruct straight edges
#'
#' For each edge in `edges_df`, reports the nodes that are not endpoints of
#' the edge but sit strictly within `node_radius` of its straight segment,
#' measured with `dist_to_edge()`. Rows with `to = NA` and zero-length edges
#' report no obstructions but keep their place in the row numbering, so
#' `edge` always indexes `edges_df` as given.
#'
#' @param coords Data frame with columns `name`, `x`, and `y`.
#' @param edges_df Data frame of edges with columns `name` and `to`. Rows with
#'   `to = NA` represent terminal or isolated nodes and are ignored.
#' @param node_radius Clearance radius in data units.
#' @return A data frame with columns `edge` (integer row index into
#'   `edges_df`), `node` (character), and `distance` (numeric), ordered by
#'   `edge` and then by the row order of `coords`.
#' @noRd
find_obstructing_nodes <- function(coords, edges_df, node_radius) {
  found <- vector("list", nrow(edges_df))
  for (i in seq_len(nrow(edges_df))) {
    if (is.na(edges_df$to[i])) {
      next
    }
    x <- coords$x[match(edges_df$name[i], coords$name)]
    y <- coords$y[match(edges_df$name[i], coords$name)]
    xend <- coords$x[match(edges_df$to[i], coords$name)]
    yend <- coords$y[match(edges_df$to[i], coords$name)]
    if (x == xend && y == yend) {
      next
    }

    other <- which(
      coords$name != edges_df$name[i] & coords$name != edges_df$to[i]
    )
    if (length(other) == 0) {
      next
    }
    distance <- dist_to_edge(
      coords$x[other],
      coords$y[other],
      x,
      y,
      xend,
      yend
    )
    hit <- distance < node_radius
    if (!any(hit)) {
      next
    }
    found[[i]] <- data.frame(
      edge = i,
      node = coords$name[other][hit],
      distance = distance[hit]
    )
  }

  found <- found[!vapply(found, is.null, logical(1))]
  if (length(found) == 0) {
    return(
      data.frame(edge = integer(), node = character(), distance = numeric())
    )
  }
  obstructions <- do.call(rbind, found)
  rownames(obstructions) <- NULL
  obstructions
}

#' Minimum clearance between a drawn arc and a set of points
#'
#' The smallest distance from any of the points to the curve
#' `sample_curved_edge()` models for the given curvature.
#'
#' @param curvature Scalar curvature; 0 samples the straight edge.
#' @param x,y,xend,yend Scalar edge endpoint coordinates.
#' @param px,py Numeric vectors of point coordinates.
#' @return Scalar minimum distance.
#' @noRd
arc_clearance <- function(curvature, x, y, xend, yend, px, py) {
  pts <- sample_curved_edge(x, y, xend, yend, curvature)
  min(sqrt(outer(px, pts$x, `-`)^2 + outer(py, pts$y, `-`)^2))
}

#' Route one edge around the nodes near it
#'
#' Starting from the candidate curvature, finds a curvature whose sampled arc
#' clears every point in `(px, py)` by at least `node_radius`. The candidate
#' is tried first, then its mirror image on the other side of the chord at
#' the same magnitude, and then the magnitude grows in steps of 0.05, trying
#' the preferred side and then the flipped side at each step, up to a
#' magnitude of 0.95. If nothing clears by then, the tried curvature with the
#' greatest clearance is returned.
#'
#' @param candidate Scalar signed candidate curvature.
#' @param x,y,xend,yend Scalar edge endpoint coordinates.
#' @param px,py Coordinates of the nodes the arc must clear.
#' @param node_radius Clearance radius in data units.
#' @return Scalar signed curvature.
#' @noRd
route_edge_around <- function(
  candidate,
  x,
  y,
  xend,
  yend,
  px,
  py,
  node_radius
) {
  side <- sign(candidate)
  magnitude <- abs(candidate)
  magnitudes <- magnitude
  if (magnitude + 0.05 <= 0.95) {
    magnitudes <- c(magnitudes, seq(magnitude + 0.05, 0.95, by = 0.05))
  }

  best <- candidate
  best_clearance <- -Inf
  for (m in magnitudes) {
    for (signed in c(side * m, -side * m)) {
      clearance <- arc_clearance(signed, x, y, xend, yend, px, py)
      if (clearance >= node_radius) {
        return(signed)
      }
      if (clearance > best_clearance) {
        best_clearance <- clearance
        best <- signed
      }
    }
  }
  best
}

#' Automatically curve edges around obstructing nodes
#'
#' A deterministic safety net for layouts where a node sits on or near the
#' straight path of an edge. Every directed, non-bidirected edge whose
#' `edge_curvature` is unset (`NA`, or the column is absent) is checked
#' against the other nodes of the DAG with `find_obstructing_nodes()`. When
#' no such edge is blocked, the input is returned untouched. Otherwise each
#' blocked edge is given a curvature as follows:
#'
#' - The arc bows away from the side of the chord where the majority of the
#'   obstructing nodes sit, following the `sample_curved_edge()` sign
#'   convention: a node above a left-to-right edge produces positive
#'   curvature, which bows the edge below. Tie rule: a node exactly on the
#'   chord counts as sitting above it, and an equal count on both sides bows
#'   the edge below (positive curvature).
#' - The magnitude is `0.1 + 1.5 * worst_intrusion / edge_length`, clamped to
#'   `[0.15, 0.6]`, where `worst_intrusion` is `node_radius` minus the
#'   smallest obstructing distance.
#' - The candidate arc is re-tested against every non-endpoint node with
#'   `route_edge_around()`, which flips the side and then escalates the
#'   magnitude until the sampled arc clears them all.
#'
#' When any curvature is written, the remaining unset directed non-bidirected
#' edges are set to 0 so the scalar curvature fallback cannot curve them,
#' while bidirected edges and terminal rows keep `NA`: a bidirected edge is
#' drawn as an arc by its edge layer, not as a straight chord, so it is never
#' a candidate. Preset curvature values, including an explicit 0, always
#' survive. The routine is deterministic and consumes no randomness.
#'
#' @param tidy_dag_data The data of a `tidy_dagitty`.
#' @param node_radius Clearance radius in data units.
#' @return `tidy_dag_data`, with an `edge_curvature` column filled in where
#'   edges were routed, or identical to the input when nothing is blocked.
#' @noRd
auto_curve_edges <- function(tidy_dag_data, node_radius = node_radius_data()) {
  curvature <- if ("edge_curvature" %in% names(tidy_dag_data)) {
    tidy_dag_data$edge_curvature
  } else {
    rep(NA_real_, nrow(tidy_dag_data))
  }
  unset <- !is.na(tidy_dag_data$to) &
    !is_bidirected_edge(tidy_dag_data) &
    is.na(curvature)
  if (!any(unset)) {
    return(tidy_dag_data)
  }

  coords <- dplyr::distinct(
    data.frame(
      name = tidy_dag_data$name,
      x = tidy_dag_data$x,
      y = tidy_dag_data$y
    )
  )
  obstructions <- find_obstructing_nodes(coords, tidy_dag_data, node_radius)
  obstructions <- obstructions[unset[obstructions$edge], , drop = FALSE]
  if (nrow(obstructions) == 0) {
    return(tidy_dag_data)
  }

  for (i in unique(obstructions$edge)) {
    blockers <- obstructions[obstructions$edge == i, , drop = FALSE]
    x <- tidy_dag_data$x[i]
    y <- tidy_dag_data$y[i]
    xend <- tidy_dag_data$xend[i]
    yend <- tidy_dag_data$yend[i]
    edge_length <- sqrt((xend - x)^2 + (yend - y)^2)

    # Which side of the chord each blocker sits on: the cross product of the
    # edge vector with the vector to the blocker is positive on the side that
    # positive curvature bows away from.
    bx <- coords$x[match(blockers$node, coords$name)]
    by <- coords$y[match(blockers$node, coords$name)]
    cross <- (xend - x) * (by - y) - (yend - y) * (bx - x)
    side <- if (sum(cross >= 0) >= sum(cross < 0)) 1 else -1

    worst_intrusion <- node_radius - min(blockers$distance)
    magnitude <- pmin(
      pmax(0.1 + 1.5 * worst_intrusion / edge_length, 0.15),
      0.6
    )

    clear_of <- coords$name != tidy_dag_data$name[i] &
      coords$name != tidy_dag_data$to[i]
    curvature[i] <- route_edge_around(
      side * magnitude,
      x,
      y,
      xend,
      yend,
      coords$x[clear_of],
      coords$y[clear_of],
      node_radius
    )
  }

  # Pin the remaining unset directed edges straight so the scalar curvature
  # fallback cannot curve them; bidirected and terminal rows stay NA.
  still_unset <- !is.na(tidy_dag_data$to) &
    !is_bidirected_edge(tidy_dag_data) &
    is.na(curvature)
  curvature[still_unset] <- 0
  tidy_dag_data$edge_curvature <- curvature
  tidy_dag_data
}
