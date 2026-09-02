# Automatic edge routing: find the nodes that sit on straight edges and steer
# each blocked edge around them, deterministically. Two engines live here.
# `auto_curve_edges()` writes a per-edge curvature that `tidy_dagitty()`
# applies when the `auto_curve` option is `TRUE`. `route_edge_waypoints()`
# routes one edge as a polyline: it builds a visibility graph over the
# tangent points of the expanded obstacle circles, takes the shortest path
# with Dijkstra, and smooths the corners with Chaikin corner cutting; the
# routed edge geom draws the result. In both engines, curvature the user set
# through `curved()`, `curve_edge()`, or dagitty control points is never
# overridden.

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

#' Cut the corners of a polyline with Chaikin smoothing
#'
#' Each iteration replaces every interior corner with two points, one a
#' quarter of the way back along the incoming segment and one a quarter of
#' the way forward along the outgoing segment, and keeps the endpoints, so a
#' path of `n` points becomes one of `2 * n - 2`. A path of two points has no
#' corner to cut and is returned unchanged however often the smoothing runs.
#'
#' @param x,y Numeric vectors of waypoint coordinates, in path order.
#' @param iterations Number of smoothing passes.
#' @return A data frame with columns `x` and `y`; the first and last rows are
#'   exactly the input endpoints.
#' @noRd
chaikin_smooth <- function(x, y, iterations = 2) {
  for (pass in seq_len(iterations)) {
    n <- length(x)
    if (n <= 2) {
      break
    }
    i <- seq(2, n - 1)
    cut_x <- rbind(
      x[i - 1] + 0.75 * (x[i] - x[i - 1]),
      x[i] + 0.25 * (x[i + 1] - x[i])
    )
    cut_y <- rbind(
      y[i - 1] + 0.75 * (y[i] - y[i - 1]),
      y[i] + 0.25 * (y[i + 1] - y[i])
    )
    x <- c(x[[1]], as.vector(cut_x), x[[n]])
    y <- c(y[[1]], as.vector(cut_y), y[[n]])
  }
  data.frame(x = x, y = y)
}

#' Tangent points on a circle seen from an external point
#'
#' The two points where the tangent lines from `(px, py)` touch the circle at
#' `(cx, cy)` with the given radius, or `NULL` when the point is on or inside
#' the circle and no tangent exists.
#'
#' @param px,py Scalar coordinates of the external point.
#' @param cx,cy,radius Scalar circle center and radius.
#' @return A two-row matrix with columns `x` and `y`, or `NULL`.
#' @noRd
point_circle_tangents <- function(px, py, cx, cy, radius) {
  dx <- cx - px
  dy <- cy - py
  d <- sqrt(dx^2 + dy^2)
  if (d <= radius) {
    return(NULL)
  }
  angle <- atan2(dy, dx)
  half <- asin(radius / d)
  reach <- sqrt(d^2 - radius^2)
  cbind(
    x = c(px + reach * cos(angle + half), px + reach * cos(angle - half)),
    y = c(py + reach * sin(angle + half), py + reach * sin(angle - half))
  )
}

#' Common tangent segments between two circles of equal radius
#'
#' The two external tangents, which run parallel to the center line when the
#' radii are equal, plus the two internal tangents, which cross between the
#' circles and exist only while the circles are disjoint. Coincident centers
#' have no common tangent.
#'
#' @param c1x,c1y,c2x,c2y Scalar circle centers.
#' @param radius Scalar radius shared by both circles.
#' @return A matrix with columns `x1`, `y1`, `x2`, `y2`, one tangent segment
#'   per row touching the first circle at `(x1, y1)` and the second at
#'   `(x2, y2)`, or `NULL`.
#' @noRd
circle_circle_tangents <- function(c1x, c1y, c2x, c2y, radius) {
  d <- sqrt((c2x - c1x)^2 + (c2y - c1y)^2)
  if (d == 0) {
    return(NULL)
  }
  theta <- atan2(c2y - c1y, c2x - c1x)

  segments <- list()
  for (side in c(1, -1)) {
    a <- theta + side * (pi / 2)
    segments[[length(segments) + 1]] <- c(
      c1x + radius * cos(a),
      c1y + radius * sin(a),
      c2x + radius * cos(a),
      c2y + radius * sin(a)
    )
  }
  if (d > 2 * radius) {
    alpha <- acos(2 * radius / d)
    for (side in c(1, -1)) {
      a <- theta + side * alpha
      segments[[length(segments) + 1]] <- c(
        c1x + radius * cos(a),
        c1y + radius * sin(a),
        c2x + radius * cos(a + pi),
        c2y + radius * sin(a + pi)
      )
    }
  }

  tangents <- do.call(rbind, segments)
  colnames(tangents) <- c("x1", "y1", "x2", "y2")
  tangents
}

#' Is a segment clear of every expanded obstacle circle?
#'
#' A segment is clear when it comes no closer than `radius` to any circle
#' center, up to a small tolerance that keeps a tangent segment, which
#' touches its own circle at exactly `radius`, from blocking itself through
#' floating point noise. The circles in `skip` are not checked, which is how
#' a tangent segment skips the circles it touches and a same-circle chord
#' skips its own circle.
#'
#' @param ax,ay,bx,by Scalar segment endpoint coordinates.
#' @param cx,cy Numeric vectors of circle centers.
#' @param radius Scalar expanded circle radius.
#' @param skip Integer indices into `cx`/`cy` to leave unchecked.
#' @return `TRUE` when no checked circle blocks the segment.
#' @noRd
segment_is_clear <- function(ax, ay, bx, by, cx, cy, radius, skip = integer()) {
  check <- setdiff(seq_along(cx), skip)
  if (length(check) == 0) {
    return(TRUE)
  }
  all(dist_to_edge(cx[check], cy[check], ax, ay, bx, by) >= radius - 1e-9)
}

#' Shortest path through the tangent visibility graph
#'
#' The classical visibility-graph construction over circular obstacles.
#' Vertices are the two edge endpoints plus the tangent points on the
#' expanded obstacle circles (radius `1.5 * node_radius`). Graph edges are
#' the tangent segments that no circle blocks: the direct endpoint chord,
#' each endpoint's tangents to each circle, and the common tangents between
#' circle pairs, plus the chords between tangent points on the same circle,
#' which let a path arrive at a circle on one tangency and leave on another.
#' A same-circle chord cuts inside the expanded circle by construction, so it
#' is admitted only while it stays at least `node_radius` from its own
#' center, clear of the drawn node.
#'
#' Dijkstra finds the shortest path by Euclidean length. Vertices are ordered
#' with the side below the chord first and settled smallest-first, and a
#' relaxation only replaces a predecessor when it is strictly shorter, so an
#' exact tie between the two sides detours below: a vertex on the chord
#' counts as sitting above it, mirroring the `auto_curve` tie rule.
#'
#' @param x,y,xend,yend Scalar edge endpoint coordinates.
#' @param px,py Numeric vectors of obstacle circle centers, none within the
#'   expanded radius of an endpoint.
#' @param node_radius Drawn node radius in data units.
#' @return A data frame with columns `x` and `y`, the path vertices in order
#'   from `(x, y)` to `(xend, yend)`, or `NULL` when the graph holds no path.
#' @noRd
tangent_graph_path <- function(x, y, xend, yend, px, py, node_radius) {
  radius <- 1.5 * node_radius

  vert_x <- c(x, xend)
  vert_y <- c(y, yend)
  vert_circle <- c(0L, 0L)
  add_vertex <- function(vx, vy, circle) {
    vert_x[[length(vert_x) + 1]] <<- vx
    vert_y[[length(vert_y) + 1]] <<- vy
    vert_circle[[length(vert_circle) + 1]] <<- circle
    length(vert_x)
  }

  # candidate tangent segments, each a row of from, to, and the circles the
  # segment touches (0 for none)
  candidates <- list(c(1, 2, 0, 0))
  for (j in seq_along(px)) {
    for (terminal in 1:2) {
      tangents <- point_circle_tangents(
        vert_x[[terminal]],
        vert_y[[terminal]],
        px[[j]],
        py[[j]],
        radius
      )
      if (is.null(tangents)) {
        next
      }
      for (row in 1:2) {
        vertex <- add_vertex(tangents[row, "x"], tangents[row, "y"], j)
        candidates[[length(candidates) + 1]] <- c(terminal, vertex, j, 0)
      }
    }
  }
  if (length(px) > 1) {
    for (j in seq_len(length(px) - 1)) {
      for (k in seq(j + 1, length(px))) {
        tangents <- circle_circle_tangents(
          px[[j]],
          py[[j]],
          px[[k]],
          py[[k]],
          radius
        )
        if (is.null(tangents)) {
          next
        }
        for (row in seq_len(nrow(tangents))) {
          first <- add_vertex(tangents[row, "x1"], tangents[row, "y1"], j)
          second <- add_vertex(tangents[row, "x2"], tangents[row, "y2"], k)
          candidates[[length(candidates) + 1]] <- c(first, second, j, k)
        }
      }
    }
  }

  # chords between tangent points on the same circle, admitted only while
  # they keep the drawn node's clearance from their own center
  for (j in seq_along(px)) {
    on_circle <- which(vert_circle == j)
    if (length(on_circle) < 2) {
      next
    }
    for (a in seq_len(length(on_circle) - 1)) {
      for (b in seq(a + 1, length(on_circle))) {
        first <- on_circle[[a]]
        second <- on_circle[[b]]
        clearance <- dist_to_edge(
          px[[j]],
          py[[j]],
          vert_x[[first]],
          vert_y[[first]],
          vert_x[[second]],
          vert_y[[second]]
        )
        if (clearance >= node_radius) {
          candidates[[length(candidates) + 1]] <- c(first, second, j, 0)
        }
      }
    }
  }

  # Order the vertices with the below side of the chord first: Dijkstra
  # settles equal distances smallest-index first and keeps the first
  # predecessor it finds, so the below path wins an exact tie. A vertex on
  # the chord has cross 0 and sorts with the above side.
  cross <- (xend - x) * (vert_y - y) - (yend - y) * (vert_x - x)
  tangent_verts <- seq_along(vert_x)[-c(1, 2)]
  ordering <- c(1L, tangent_verts[order(cross[tangent_verts])], 2L)
  new_index <- integer(length(ordering))
  new_index[ordering] <- seq_along(ordering)
  vert_x <- vert_x[ordering]
  vert_y <- vert_y[ordering]

  n <- length(vert_x)
  weights <- matrix(Inf, nrow = n, ncol = n)
  for (candidate in candidates) {
    from <- new_index[[candidate[[1]]]]
    to <- new_index[[candidate[[2]]]]
    clear <- segment_is_clear(
      vert_x[[from]],
      vert_y[[from]],
      vert_x[[to]],
      vert_y[[to]],
      px,
      py,
      radius,
      skip = candidate[3:4]
    )
    if (!clear) {
      next
    }
    len <- sqrt(
      (vert_x[[to]] - vert_x[[from]])^2 + (vert_y[[to]] - vert_y[[from]])^2
    )
    weights[from, to] <- min(weights[from, to], len)
    weights[to, from] <- weights[from, to]
  }

  start <- new_index[[1]]
  end <- new_index[[2]]
  distance <- rep(Inf, n)
  distance[[start]] <- 0
  predecessor <- rep(NA_integer_, n)
  visited <- rep(FALSE, n)
  repeat {
    current <- 0L
    best <- Inf
    for (vertex in seq_len(n)) {
      if (!visited[[vertex]] && distance[[vertex]] < best) {
        best <- distance[[vertex]]
        current <- vertex
      }
    }
    if (current == 0L || current == end) {
      break
    }
    visited[[current]] <- TRUE
    for (vertex in seq_len(n)) {
      if (visited[[vertex]] || is.infinite(weights[current, vertex])) {
        next
      }
      relaxed <- distance[[current]] + weights[current, vertex]
      if (relaxed < distance[[vertex]]) {
        distance[[vertex]] <- relaxed
        predecessor[[vertex]] <- current
      }
    }
  }

  if (is.infinite(distance[[end]])) {
    return(NULL)
  }
  path <- end
  while (path[[1]] != start) {
    path <- c(predecessor[[path[[1]]]], path)
  }
  data.frame(x = vert_x[path], y = vert_y[path])
}

#' Route one edge around the nodes that block it
#'
#' Routes the edge from `(x, y)` to `(xend, yend)` around the obstacle points
#' in `(px, py)`, each an obstacle circle of radius `1.5 * node_radius`
#' around a drawn node of radius `node_radius`, leaving a clearance margin of
#' half a node radius. An edge that no circle blocks, strictly within the
#' expanded radius as in `find_obstructing_nodes()`, comes back as its
#' straight two-point path, and so does a zero-length edge. A blocked edge
#' takes the shortest path through the tangent visibility graph of
#' `tangent_graph_path()`, smoothed with two iterations of
#' `chaikin_smooth()`. The first and last waypoints are exactly the edge
#' endpoints, and the routine is deterministic and consumes no randomness.
#'
#' @param x,y,xend,yend Scalar edge endpoint coordinates.
#' @param px,py Numeric vectors of obstacle point coordinates; the edge's own
#'   endpoints must not be among them.
#' @param node_radius Drawn node radius in data units.
#' @return A data frame with columns `x` and `y`, one waypoint per row in
#'   path order.
#' @noRd
route_edge_waypoints <- function(
  x,
  y,
  xend,
  yend,
  px,
  py,
  node_radius = node_radius_data()
) {
  straight <- data.frame(x = c(x, xend), y = c(y, yend))
  if (x == xend && y == yend) {
    return(straight)
  }

  radius <- 1.5 * node_radius
  if (length(px) > 0) {
    # a circle swallowing an endpoint has no tangents from that endpoint and
    # would block every segment leaving it, so it cannot be routed around
    clear_of_ends <- sqrt((px - x)^2 + (py - y)^2) > radius &
      sqrt((px - xend)^2 + (py - yend)^2) > radius
    px <- px[clear_of_ends]
    py <- py[clear_of_ends]
  }
  if (
    length(px) == 0 ||
      !any(dist_to_edge(px, py, x, y, xend, yend) < radius)
  ) {
    return(straight)
  }

  path <- tangent_graph_path(x, y, xend, yend, px, py, node_radius)
  if (is.null(path)) {
    return(straight)
  }
  chaikin_smooth(path$x, path$y, iterations = 2)
}

#' The distinct node positions of a DAG's data
#'
#' One row per drawn node, used as the obstacle set for routing. Data without
#' position columns has no obstacles to offer.
#'
#' @param dag_data The data of a `tidy_dagitty`, or any data frame with `x`
#'   and `y` columns and optionally `name`.
#' @return A data frame with columns `x` and `y`, plus `name` when the input
#'   carries it.
#' @noRd
node_obstacle_coords <- function(dag_data) {
  if (!all(c("x", "y") %in% names(dag_data))) {
    return(data.frame(x = numeric(), y = numeric()))
  }
  if ("name" %in% names(dag_data)) {
    return(dplyr::distinct(
      data.frame(
        name = as.character(dag_data$name),
        x = dag_data$x,
        y = dag_data$y,
        stringsAsFactors = FALSE
      )
    ))
  }
  dplyr::distinct(data.frame(x = dag_data$x, y = dag_data$y))
}

#' The waypoints one edge is drawn through
#'
#' Dispatches on the edge's curvature: an unset curvature (`NA`) routes the
#' edge around the obstacle points with `route_edge_waypoints()`, an explicit
#' 0 keeps the straight two-point chord, and any other number samples the
#' quadratic Bezier arc the curvature draws, so curvature the user set is
#' never rerouted.
#'
#' @param x,y,xend,yend Scalar edge endpoint coordinates.
#' @param curvature Scalar curvature, `NA` when unset.
#' @param px,py Obstacle point coordinates.
#' @param node_radius Drawn node radius in data units.
#' @return A data frame with columns `x` and `y`; the first and last rows are
#'   exactly the edge endpoints.
#' @noRd
routed_edge_path <- function(x, y, xend, yend, curvature, px, py, node_radius) {
  if (is.na(curvature)) {
    return(route_edge_waypoints(x, y, xend, yend, px, py, node_radius))
  }
  if (curvature == 0) {
    return(data.frame(x = c(x, xend), y = c(y, yend)))
  }
  sample_curved_edge(x, y, xend, yend, curvature = curvature)
}

#' Waypoint rows for a routed edge layer
#'
#' Turns edge rows into the long waypoint format the routed edge geom draws
#' and the label obstacle machinery consumes: one row per waypoint with
#' `edge_id`, `x`, `y`, and `seq` columns, each edge's waypoints in `seq`
#' order and its terminal waypoints exactly the node positions. The obstacle
#' set of each edge is every distinct node of `dag_data` other than the
#' edge's own endpoints, matched by name when the edges carry `name` and `to`
#' columns and by exact position otherwise. An `edge_curvature` column on the
#' edges is honored per `routed_edge_path()`.
#'
#' @param edges Data frame of edge rows with `x`, `y`, `xend`, and `yend`
#'   columns; rows with a missing endpoint are dropped.
#' @param dag_data Data frame the obstacle node positions are read from.
#' @param node_radius Drawn node radius in data units.
#' @return A data frame with columns `edge_id`, `x`, `y`, and `seq`.
#' @noRd
route_dag_edges <- function(edges, dag_data, node_radius = node_radius_data()) {
  empty <- data.frame(
    edge_id = character(),
    x = numeric(),
    y = numeric(),
    seq = integer(),
    stringsAsFactors = FALSE
  )
  if (!all(c("x", "y", "xend", "yend") %in% names(edges))) {
    return(empty)
  }
  edges <- edges[!is.na(edges$xend) & !is.na(edges$yend), , drop = FALSE]
  if (nrow(edges) == 0) {
    return(empty)
  }

  coords <- node_obstacle_coords(dag_data)
  curvature <- if ("edge_curvature" %in% names(edges)) {
    edges$edge_curvature
  } else {
    rep(NA_real_, nrow(edges))
  }
  by_name <- all(c("name", "to") %in% names(edges)) &&
    "name" %in% names(coords)

  # The key alone does not identify an edge: two edges can run between the
  # same pair of nodes, so the row index tells them apart.
  key <- edge_key(edges$x, edges$y, edges$xend, edges$yend)
  waypoints <- lapply(seq_len(nrow(edges)), function(i) {
    others <- if (by_name) {
      coords$name != as.character(edges$name[[i]]) &
        coords$name != as.character(edges$to[[i]])
    } else {
      !(coords$x == edges$x[[i]] & coords$y == edges$y[[i]]) &
        !(coords$x == edges$xend[[i]] & coords$y == edges$yend[[i]])
    }
    path <- routed_edge_path(
      edges$x[[i]],
      edges$y[[i]],
      edges$xend[[i]],
      edges$yend[[i]],
      curvature[[i]],
      coords$x[others],
      coords$y[others],
      node_radius
    )
    data.frame(
      edge_id = paste(key[[i]], i, sep = "\r"),
      x = path$x,
      y = path$y,
      seq = seq_len(nrow(path)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, waypoints)
}
