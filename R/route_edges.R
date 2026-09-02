# Edge routing in millimetres. The router takes node discs and edge chords in
# the units they are drawn in, infers layers from the layer-axis coordinate,
# and routes every blocked edge as a centripetal Catmull-Rom spline through a
# few waypoints: one per crossed layer, snapped into the free slots of that
# layer, or a single bow around the obstacle for short and steep chords. The
# side of a detour is chosen by a cost that weighs edge crossings,
# displacement, and the crowding of the endpoints' fans. Every routed curve
# is sampled, verified against the node discs, and repaired by pushing its
# waypoints outward. The engine is pure: it consumes no randomness, does not
# depend on row order, and returns identical output on repeated calls.
#
# The layer axis is called x and the within-layer axis y. A scene whose
# layers run along device y is transposed on entry and back on exit. Every
# edge is routed with its source to the left of its target; "above" (side +1)
# means larger y in that orientation, and the polyline of an edge that runs
# right to left is reversed on output.

#' Constants of the millimetre router
#'
#' Derives every constant the router uses from a reference node radius. The
#' clearance margin, the edge separations, and the layer tolerance scale with
#' the radius above millimetre floors; the remaining values are dimensionless
#' or fixed lengths. `m` and `sep_e` override the two the caller is allowed to
#' set; `layer_axis` overrides the axis the layers are inferred from.
#'
#' @param r_ref Reference node radius in mm, typically the median radius.
#' @param m Clearance margin in mm, or `NULL` for `max(0.5 * r_ref, 1.2)`.
#' @param sep_e Edge separation within a slot in mm, or `NULL` for
#'   `max(0.6 * r_ref, 1.5)`.
#' @param layer_axis The axis the layers run along: `"auto"` infers it,
#'   `"x"` and `"y"` name it.
#' @return A named list of constants.
#' @noRd
route_opts <- function(
  r_ref,
  m = NULL,
  sep_e = NULL,
  layer_axis = c("auto", "x", "y")
) {
  layer_axis <- match.arg(layer_axis)
  m <- m %||% max(0.5 * r_ref, 1.2)
  m_min <- min(1.2, m)
  list(
    r_ref = r_ref,
    m = m,
    m_min = m_min,
    layer_axis = layer_axis,
    R = r_ref + m,
    R_soft = r_ref + m_min,
    sep_e = sep_e %||% max(0.6 * r_ref, 1.5),
    sep_m = max(1.0 * r_ref, 2.5),
    tol_layer = r_ref,
    steep_deg = 60,
    sagitta_max = 0.22,
    t_clamp = c(0.2, 0.8),
    tangent_clamp = 40,
    arm_fraction = 0.45,
    alpha = 0.5,
    sample_spacing = 0.5,
    sample_min_n = 16,
    verify_tol = 0.1,
    repair_relax = 1.3,
    repair_slack = 0.15,
    repair_iter = 4,
    repair_window = 0.35,
    crossing_penalty = 16,
    congestion_penalty = 2,
    displacement_weight = 1,
    periphery_span = 3,
    pad = 0.5,
    cost_digits = 9
  )
}

#' Route the edges of a DAG in millimetres
#'
#' The entry point of the router. Nodes are discs of radius `r`; edges are
#' chords between node centres. Edges with a non-missing `curvature` are
#' user-curved and pass through untouched, drawn from their pre-sampled
#' `fixed_path` when one is supplied and as their chord otherwise. Every
#' other edge whose chord comes closer to a non-endpoint node than that
#' node's radius plus the clearance margin is routed; the rest stay straight.
#'
#' @param nodes Data frame with columns `name`, `x`, `y`, and `r` (mm).
#' @param edges Data frame with columns `from`, `to`, and optionally
#'   `curvature` (`NA` to route) and a `fixed_path` list column of
#'   pre-sampled `data.frame(x, y)` paths for user-curved edges.
#' @param bounds Panel bounds `c(xmin, ymin, xmax, ymax)` in mm.
#' @param cap Edge cap in mm, the length the arrow layer resects at each end.
#' @param mode `"spline"` routes blocked edges as curves, `"straight"` draws
#'   every edge as its chord. `"orthogonal"` is not yet implemented.
#' @param opts Constants from `route_opts()`.
#' @return A list with `paths` (one `data.frame(x, y)` per edge, in input
#'   order), `meta` (one row per edge: `edge`, `routed`, `mode`, `side`,
#'   `n_waypoints`, `waypoint_layers`, `clearance_ok`, `sagitta_ratio`,
#'   `sagitta_capped`), and `waypoints` (one `data.frame(x, y, layer)` per
#'   edge).
#' @noRd
route_edges_mm <- function(
  nodes,
  edges,
  bounds,
  cap = 8,
  mode = c("spline", "orthogonal", "straight"),
  opts = route_opts(stats::median(nodes$r))
) {
  mode <- check_route_mode(mode)
  if (mode == "orthogonal") {
    abort(
      c(
        "Orthogonal edge routing is not yet implemented.",
        "i" = "Use {.code mode = \"spline\"} or {.code mode = \"straight\"}."
      )
    )
  }
  scene <- canonicalize_scene(
    nodes,
    edges,
    bounds,
    opts$tol_layer,
    opts$layer_axis %||% "auto"
  )
  res <- route_scene_mm(
    scene$nodes,
    scene$edges,
    scene$bounds,
    cap,
    mode,
    opts
  )
  decanonicalize(res, scene$transposed)
}

#' Validate the routing mode
#' @noRd
check_route_mode <- function(mode) {
  choices <- c("spline", "orthogonal", "straight")
  if (identical(mode, choices)) {
    return(choices[[1]])
  }
  if (!is.character(mode) || length(mode) != 1 || !mode %in% choices) {
    abort(
      c(
        "{.arg mode} must be one of {.val {choices}}.",
        "x" = "You've supplied {.val {mode}}."
      ),
      error_class = "ggdag_type_error"
    )
  }
  mode
}

# Scene orientation -------------------------------------------------------------

#' Orient a scene so that its layers run along x
#'
#' The layer axis defaults to x. Under `layer_axis = "auto"` the scene is
#' transposed only when x has fewer than two clusters, or when the y clusters
#' are exact (zero spread within every cluster) while the x clusters are not;
#' `"x"` and `"y"` name the axis outright.
#'
#' @return A list with `nodes`, `edges`, `bounds`, and `transposed`.
#' @noRd
canonicalize_scene <- function(nodes, edges, bounds, tol, layer_axis = "auto") {
  nodes <- data.frame(
    name = as.character(nodes$name),
    x = as.numeric(nodes$x),
    y = as.numeric(nodes$y),
    r = as.numeric(nodes$r),
    stringsAsFactors = FALSE
  )
  edges <- as.data.frame(edges, stringsAsFactors = FALSE)
  bounds <- as.numeric(bounds)

  transposed <- identical(layer_axis, "y")
  if (identical(layer_axis, "auto") && nrow(nodes) > 0) {
    lx <- infer_layers(nodes, tol)
    ly <- infer_layers(data.frame(x = nodes$y), tol)
    transposed <- lx$n < 2 ||
      (ly$n >= 2 && layers_exact(ly, nodes$y) && !layers_exact(lx, nodes$x))
  }

  if (transposed) {
    nodes <- swap_xy(nodes)
    bounds <- bounds[c(2, 1, 4, 3)]
    if ("fixed_path" %in% names(edges)) {
      edges$fixed_path <- lapply(edges$fixed_path, function(p) {
        if (is.data.frame(p)) swap_xy(p) else p
      })
    }
  }
  list(nodes = nodes, edges = edges, bounds = bounds, transposed = transposed)
}

#' Swap the paths and waypoints back after a transposed routing
#' @noRd
decanonicalize <- function(res, transposed) {
  if (!transposed) {
    return(res)
  }
  res$paths <- lapply(res$paths, swap_xy)
  res$waypoints <- lapply(res$waypoints, swap_xy)
  res
}

swap_xy <- function(df) {
  x <- df$x
  df$x <- df$y
  df$y <- x
  df
}

layers_exact <- function(layers, coord) {
  all(vapply(
    layers$members,
    function(m) diff(range(coord[m])) == 0,
    logical(1)
  ))
}

# Layers and edge spans -----------------------------------------------------------

#' Infer layers from the layer-axis coordinate
#'
#' Sorts the distinct x values and starts a new layer wherever consecutive
#' values are more than `tol` apart, so nodes that overlap horizontally are
#' one layer. Singleton layers are legitimate.
#'
#' @return A list with `id` (layer per node row), `x` (layer coordinate, the
#'   mean of the distinct x values in the cluster), `n`, and `members` (node
#'   row indices per layer).
#' @noRd
infer_layers <- function(nodes, tol) {
  xs <- sort(unique(nodes$x))
  if (length(xs) == 0) {
    return(list(id = integer(0), x = numeric(0), n = 0L, members = list()))
  }
  cluster <- cumsum(c(1L, as.integer(diff(xs) > tol)))
  id <- cluster[match(nodes$x, xs)]
  n <- max(cluster)
  list(
    id = id,
    x = unname(vapply(split(xs, cluster), mean, numeric(1))),
    n = n,
    members = split(seq_len(nrow(nodes)), factor(id, levels = seq_len(n)))
  )
}

#' Per-edge layer span, orientation, and chord geometry
#'
#' An edge is routed with its source to the left of its target. `reversed`
#' marks edges whose target has the smaller x (or, on a vertical chord, the
#' smaller y); their polylines are reversed on output.
#'
#' @noRd
edge_span_info <- function(nodes, from, to, layers) {
  xs <- nodes$x[from]
  xt <- nodes$x[to]
  reversed <- xt < xs | (xt == xs & nodes$y[to] < nodes$y[from])
  la <- layers$id[from]
  lb <- layers$id[to]
  dx <- xt - xs
  dy <- nodes$y[to] - nodes$y[from]
  data.frame(
    la = pmin(la, lb),
    lb = pmax(la, lb),
    span = abs(lb - la),
    reversed = reversed,
    Lc = sqrt(dx^2 + dy^2),
    angle = atan2(abs(dy), abs(dx)) * 180 / pi
  )
}

#' The canonical frame of one edge
#'
#' Source `S`, target `E`, chord unit vector `u`, unit normal `n` (side +1),
#' and chord length `Lc`, in the source-left-of-target orientation.
#'
#' @noRd
edge_frame <- function(nodes, from, to, reversed) {
  a <- if (reversed) to else from
  b <- if (reversed) from else to
  S <- c(nodes$x[[a]], nodes$y[[a]])
  E <- c(nodes$x[[b]], nodes$y[[b]])
  d <- E - S
  Lc <- sqrt(sum(d^2))
  u <- d / Lc
  list(S = S, E = E, u = u, n = c(-u[[2]], u[[1]]), Lc = Lc, a = a, b = b)
}

# Obstruction -----------------------------------------------------------------------

#' Find the nodes that block straight chords
#'
#' Vectorised over every (edge, non-endpoint node) pair. A node is a hit when
#' its centre is closer than `R` to the chord segment; it is a hard hit below
#' `R_soft` and a soft hit (grazed) between the two.
#'
#' @param nodes Data frame with `name`, `x`, `y`.
#' @param edges Data frame with `from`, `to`.
#' @param R_soft,R Obstruction radii, scalars or one value per node row.
#' @return A data frame with `edge` (row index into `edges`), `node` (name),
#'   `d` (segment distance), `h` (signed perpendicular offset, positive to
#'   the left of the from-to direction), `t` (projection parameter), and
#'   `severity`, ordered by edge and then node row order.
#' @noRd
find_blocked_edges <- function(nodes, edges, R_soft, R) {
  n_e <- nrow(edges)
  n_n <- nrow(nodes)
  empty <- data.frame(
    edge = integer(),
    node = character(),
    d = numeric(),
    h = numeric(),
    t = numeric(),
    severity = character(),
    stringsAsFactors = FALSE
  )
  if (n_e == 0 || n_n == 0) {
    return(empty)
  }
  from <- match(as.character(edges$from), nodes$name)
  to <- match(as.character(edges$to), nodes$name)
  R <- rep_len(R, n_n)
  R_soft <- rep_len(R_soft, n_n)

  ei <- rep(seq_len(n_e), each = n_n)
  ni <- rep.int(seq_len(n_n), n_e)
  sx <- nodes$x[from[ei]]
  sy <- nodes$y[from[ei]]
  tx <- nodes$x[to[ei]]
  ty <- nodes$y[to[ei]]
  cx <- nodes$x[ni]
  cy <- nodes$y[ni]
  dx <- tx - sx
  dy <- ty - sy
  len2 <- dx^2 + dy^2
  Lc <- sqrt(len2)

  d <- dist_to_edge(cx, cy, sx, sy, tx, ty)
  t <- ((cx - sx) * dx + (cy - sy) * dy) / len2
  h <- (dx * (cy - sy) - dy * (cx - sx)) / Lc

  keep <- ni != from[ei] & ni != to[ei] & Lc > 0 & d < R[ni]
  keep[is.na(keep)] <- FALSE
  if (!any(keep)) {
    return(empty)
  }
  data.frame(
    edge = ei[keep],
    node = nodes$name[ni[keep]],
    d = d[keep],
    h = h[keep],
    t = t[keep],
    severity = ifelse(d[keep] < R_soft[ni[keep]], "hard", "soft"),
    stringsAsFactors = FALSE
  )
}

# Free slots within a layer ---------------------------------------------------------

#' Free intervals of y in one layer
#'
#' The gaps between the padded discs of a layer's nodes, plus the space below
#' the lowest and above the highest node down to the panel edge less `pad`.
#' Intervals narrower than zero are dropped; the first and last surviving
#' intervals are flagged `outer`.
#'
#' @noRd
layer_free_intervals <- function(layer_nodes, margin, bounds, pad = 0.5) {
  ord <- order(layer_nodes$y)
  ys <- layer_nodes$y[ord]
  Rk <- layer_nodes$r[ord] + margin
  lo <- c(bounds[[2]] + pad, ys + Rk)
  hi <- c(ys - Rk, bounds[[4]] - pad)
  keep <- hi >= lo
  lo <- lo[keep]
  hi <- hi[keep]
  n <- length(lo)
  df_cols(lo = lo, hi = hi, outer = seq_len(n) %in% c(1L, n))
}

#' Snap a y to the nearest free value on one side
#'
#' Side +1 returns the smallest free y at or above `y`, side -1 the largest
#' free y at or below it; a `y` that is already free is returned unchanged.
#' `NA` when nothing is free on that side.
#'
#' @noRd
nearest_free_y <- function(intervals, y, side, outer_only) {
  lo <- intervals$lo
  hi <- intervals$hi
  if (outer_only) {
    lo <- lo[intervals$outer]
    hi <- hi[intervals$outer]
  }
  if (length(lo) == 0) {
    return(NA_real_)
  }
  if (side > 0) {
    ok <- which(hi >= y)
    if (length(ok) == 0) {
      return(NA_real_)
    }
    k <- ok[[which.min(lo[ok])]]
    return(max(lo[[k]], y))
  }
  ok <- which(lo <= y)
  if (length(ok) == 0) {
    return(NA_real_)
  }
  k <- ok[[which.max(hi[ok])]]
  min(hi[[k]], y)
}

#' Spread waypoints that share a slot with already routed edges
#'
#' When another edge occupies the same (layer, interval) within `sep_e`, the
#' new waypoint moves `sep_e` beyond the farthest occupant, away from the
#' chord side. If that leaves the interval, the next free interval outward
#' is used; if none exists the waypoint stops at the interval edge.
#'
#' @return A list with the updated `wp` and the `slot` index per row.
#' @noRd
spread_in_slot <- function(wp, ints, occ, side, sep_e) {
  slot <- integer(nrow(wp))
  for (i in seq_len(nrow(wp))) {
    iv <- ints[[i]]
    y <- wp$y[[i]]
    s <- which(iv$lo <= y & y <= iv$hi)
    if (length(s) == 0) {
      slot[[i]] <- NA_integer_
      next
    }
    s <- s[[1]]
    taken <- occ$y[occ$layer == wp$layer[[i]] & occ$slot == s]
    if (length(taken) > 0 && any(abs(taken - y) < sep_e)) {
      y_new <- if (side > 0) max(taken) + sep_e else min(taken) - sep_e
      inside <- y_new >= iv$lo[[s]] && y_new <= iv$hi[[s]]
      if (!inside) {
        alt <- nearest_free_y(iv, y_new, side, FALSE)
        if (is.na(alt)) {
          y_new <- if (side > 0) iv$hi[[s]] else iv$lo[[s]]
        } else {
          y_new <- alt
          s <- which(iv$lo <= alt & alt <= iv$hi)[[1]]
        }
      }
      wp$y[[i]] <- y_new
    }
    slot[[i]] <- s
  }
  list(wp = wp, slot = slot)
}

# Waypoint chains ----------------------------------------------------------------------

#' Reduce a waypoint chain to one arch
#'
#' Andrew's monotone chain over the source, the waypoints, and the target in
#' the chord frame: the upper hull for side +1, the lower hull for side -1.
#' Waypoints on the wrong side of the chord, on the chord, or dipping toward
#' it between two others are dropped. Endpoints are never returned.
#'
#' @param S,E Endpoints, as `c(x, y)` or one-row data frames.
#' @param wp Data frame with `x` and `y` (and any other columns).
#' @return The kept rows of `wp`, ordered along the chord.
#' @noRd
hull_waypoints <- function(S, wp, E, side) {
  if (nrow(wp) == 0) {
    return(wp)
  }
  S <- xy(S)
  E <- xy(E)
  d <- E - S
  Lc <- sqrt(sum(d^2))
  u <- d / Lc
  n <- c(-u[[2]], u[[1]])
  t <- (wp$x - S[[1]]) * u[[1]] + (wp$y - S[[2]]) * u[[2]]
  o <- (wp$x - S[[1]]) * n[[1]] + (wp$y - S[[2]]) * n[[2]]
  ord <- order(t, side * o)

  px <- c(0, t[ord], Lc)
  py <- c(0, o[ord], 0)
  idx <- c(0L, ord, 0L)
  hull <- integer(0)
  for (k in seq_along(px)) {
    while (length(hull) >= 2) {
      a <- hull[[length(hull) - 1]]
      b <- hull[[length(hull)]]
      cr <- (px[[b]] - px[[a]]) *
        (py[[k]] - py[[a]]) -
        (py[[b]] - py[[a]]) * (px[[k]] - px[[a]])
      if (side * cr >= 0) {
        hull <- hull[-length(hull)]
      } else {
        break
      }
    }
    hull <- c(hull, k)
  }
  keep <- idx[hull]
  df_rows(wp, keep[keep > 0])
}

xy <- function(p) {
  c(as.numeric(p[[1]])[[1]], as.numeric(p[[2]])[[1]])
}

#' Rotate a direction into a cone around a reference direction
#'
#' Returns `unit(v)` when it lies within `max_deg` of `unit(ref)`, and the
#' cone edge on `v`'s side otherwise. A zero-length `v` returns `unit(ref)`.
#'
#' @noRd
clamp_direction <- function(v, ref, max_deg = 40) {
  ref <- ref / sqrt(sum(ref^2))
  lv <- sqrt(sum(v^2))
  if (lv == 0) {
    return(ref)
  }
  v <- v / lv
  ang <- atan2(
    ref[[1]] * v[[2]] - ref[[2]] * v[[1]],
    ref[[1]] * v[[1]] + ref[[2]] * v[[2]]
  )
  max_rad <- max_deg * pi / 180
  if (abs(ang) <= max_rad) {
    return(v)
  }
  th <- sign(ang) * max_rad
  c(
    ref[[1]] * cos(th) - ref[[2]] * sin(th),
    ref[[1]] * sin(th) + ref[[2]] * cos(th)
  )
}

# Curves ---------------------------------------------------------------------------------

#' Centripetal Catmull-Rom spline as cubic Bezier segments
#'
#' Knots advance by `|P[i + 1] - P[i]|^alpha`; the tangent at an interior
#' point follows Yuksel, Schaefer and Keyser (2011), and each segment scales
#' the tangents at its ends by its own knot spacing. The end points are
#' duplicated as their own neighbours, which makes the natural end tangents
#' vanish, so the first and last control arms are set explicitly: they run
#' along `d_start` and `d_end` for `min(arm_min, 0.45 * segment length)`.
#'
#' @param P Matrix of points, one per row, at least three rows.
#' @param d_start,d_end Unit directions of departure and arrival.
#' @param arm_min Minimum end arm length in mm; one value or one per end.
#' @param arm_fraction Cap on the end arm as a fraction of the end segment.
#' @return A list of 4 x 2 control point matrices.
#' @noRd
catmull_rom_beziers <- function(
  P,
  alpha = 0.5,
  d_start,
  d_end,
  arm_min,
  arm_fraction = 0.45
) {
  n <- nrow(P)
  arm_min <- rep_len(arm_min, 2)
  seg <- sqrt(rowSums((P[-1, , drop = FALSE] - P[-n, , drop = FALSE])^2))
  dt <- pmax(seg^alpha, 1e-9)
  t <- cumsum(c(0, dt))

  D <- matrix(0, n, 2)
  if (n > 2) {
    i <- 2:(n - 1)
    D[i, ] <- (P[i, , drop = FALSE] - P[i - 1, , drop = FALSE]) /
      (t[i] - t[i - 1]) -
      (P[i + 1, , drop = FALSE] - P[i - 1, , drop = FALSE]) /
        (t[i + 1] - t[i - 1]) +
      (P[i + 1, , drop = FALSE] - P[i, , drop = FALSE]) / (t[i + 1] - t[i])
  }

  B <- vector("list", n - 1)
  for (i in seq_len(n - 1)) {
    B[[i]] <- rbind(
      P[i, ],
      P[i, ] + dt[[i]] * D[i, ] / 3,
      P[i + 1, ] - dt[[i]] * D[i + 1, ] / 3,
      P[i + 1, ]
    )
  }

  a_s <- max(
    sqrt(sum((dt[[1]] * D[1, ] / 3)^2)),
    min(arm_min[[1]], arm_fraction * seg[[1]])
  )
  a_e <- max(
    sqrt(sum((dt[[n - 1]] * D[n, ] / 3)^2)),
    min(arm_min[[2]], arm_fraction * seg[[n - 1]])
  )
  B[[1]][2, ] <- P[1, ] + a_s * d_start
  B[[n - 1]][3, ] <- P[n, ] - a_e * d_end
  B
}

#' Sample cubic Bezier segments into one polyline
#'
#' Each segment is evaluated at `max(min_n, ceiling(L / spacing))` points,
#' where `L` is the length of its control polygon, and the duplicated join
#' between consecutive segments is dropped. The first and last rows are the
#' first and last control points exactly.
#'
#' @noRd
sample_beziers <- function(B, spacing = 0.5, min_n = 16) {
  out <- vector("list", length(B))
  for (k in seq_along(B)) {
    seg <- B[[k]]
    len <- sum(sqrt(rowSums((seg[-1, ] - seg[-4, ])^2)))
    n <- max(min_n, ceiling(len / spacing))
    t <- (0:(n - 1)) / (n - 1)
    mt <- 1 - t
    b0 <- mt^3
    b1 <- 3 * mt^2 * t
    b2 <- 3 * mt * t^2
    b3 <- t^3
    pts <- cbind(
      b0 * seg[1, 1] + b1 * seg[2, 1] + b2 * seg[3, 1] + b3 * seg[4, 1],
      b0 * seg[1, 2] + b1 * seg[2, 2] + b2 * seg[3, 2] + b3 * seg[4, 2]
    )
    pts[1, ] <- seg[1, ]
    pts[n, ] <- seg[4, ]
    out[[k]] <- if (k > 1) pts[-1, , drop = FALSE] else pts
  }
  pts <- do.call(rbind, out)
  df_cols(x = pts[, 1], y = pts[, 2])
}

#' Find samples that come too close to an obstacle
#'
#' @param samples Data frame with `x`, `y`.
#' @param obstacles Data frame with `x`, `y`.
#' @param R_vec Clearance radius per obstacle.
#' @return A data frame with `obstacle` (row in `obstacles`), `sample` (the
#'   nearest sample) and `depth` (how far inside `R - tol` it lies), one row
#'   per violated obstacle.
#' @noRd
verify_clearance <- function(samples, obstacles, R_vec, tol = 0.1) {
  if (nrow(obstacles) == 0 || nrow(samples) == 0) {
    return(df_cols(obstacle = integer(), sample = integer(), depth = numeric()))
  }
  R_vec <- rep_len(R_vec, nrow(obstacles))
  # only obstacles whose disc reaches the path's bounding box can be violated
  cand <- which(
    obstacles$x + R_vec >= min(samples$x) &
      obstacles$x - R_vec <= max(samples$x) &
      obstacles$y + R_vec >= min(samples$y) &
      obstacles$y - R_vec <= max(samples$y)
  )
  if (length(cand) == 0) {
    return(df_cols(obstacle = integer(), sample = integer(), depth = numeric()))
  }
  d <- sqrt(
    outer(obstacles$x[cand], samples$x, "-")^2 +
      outer(obstacles$y[cand], samples$y, "-")^2
  )
  j <- max.col(-d, ties.method = "first")
  min_d <- d[cbind(seq_along(cand), j)]
  depth <- R_vec[cand] - tol - min_d
  keep <- depth > 0
  df_cols(obstacle = cand[keep], sample = j[keep], depth = depth[keep])
}

#' Push waypoints away from the obstacles a sampled curve violates
#'
#' Deepest violation first. In the spanning tier a violating node that sits
#' in a crossed layer without a waypoint gets one inserted at that layer,
#' `R` from the node on the side of the sample; otherwise every interior
#' waypoint within `repair_window` of the sample's chord parameter moves
#' along y by `relax * depth + slack * R`, so the local leg translates and
#' each waypoint stays at its layer's x. In the free tier the nearest
#' waypoint within the window moves straight away from the node, and a new
#' waypoint is inserted at clearance `R` when none is near.
#'
#' @noRd
repair_waypoints <- function(
  wp,
  viol,
  pts,
  fr,
  obstacles,
  R_vec,
  tier,
  layers,
  la,
  lb,
  opts
) {
  viol <- df_rows(viol, order(-viol$depth, obstacles$name[viol$obstacle]))
  for (k in seq_len(nrow(viol))) {
    ob <- viol$obstacle[[k]]
    C <- c(obstacles$x[[ob]], obstacles$y[[ob]])
    R <- R_vec[[ob]]
    p <- c(pts$x[[viol$sample[[k]]]], pts$y[[viol$sample[[k]]]])
    nv <- p - C
    ln <- sqrt(sum(nv^2))
    nv <- if (ln > 0) nv / ln else fr$n
    push <- opts$repair_relax * viol$depth[[k]] + opts$repair_slack * R
    t_p <- sum((p - fr$S) * fr$u) / fr$Lc
    t_wp <- ((wp$x - fr$S[[1]]) * fr$u[[1]] + (wp$y - fr$S[[2]]) * fr$u[[2]]) /
      fr$Lc
    layer_c <- obstacles$layer[[ob]]
    crossed <- tier == "spanning" && layer_c > la && layer_c < lb
    s_chord <- sign(sum(nv * fr$n))
    if (s_chord == 0) {
      s_chord <- 1
    }

    if (crossed && !layer_c %in% wp$layer) {
      new <- df_cols(
        x = layers$x[[layer_c]],
        y = C[[2]] + s_chord * R,
        layer = layer_c
      )
      wp <- sort_waypoints(df_bind(wp, new), fr)
      next
    }

    near <- which(abs(t_wp - t_p) < opts$repair_window)
    if (length(near) == 0 && nrow(wp) > 0 && tier == "spanning") {
      near <- which(wp$layer == layer_c)
    }
    if (length(near) > 0) {
      if (tier == "spanning") {
        at_layer <- which(wp$layer == layer_c)
        s <- if (length(at_layer) > 0) {
          sign(wp$y[[at_layer[[1]]]] - C[[2]])
        } else {
          sign(nv[[2]])
        }
        if (s == 0) {
          s <- 1
        }
        wp$y[near] <- wp$y[near] + s * push
      } else {
        j <- near[[which.min(abs(t_wp[near] - t_p))]]
        wp$x[[j]] <- wp$x[[j]] + nv[[1]] * push
        wp$y[[j]] <- wp$y[[j]] + nv[[2]] * push
      }
    } else {
      h_c <- sum((C - fr$S) * fr$n)
      t_c <- sum((C - fr$S) * fr$u) / fr$Lc
      tc <- min(max(t_c, opts$t_clamp[[1]]), opts$t_clamp[[2]])
      o <- h_c + s_chord * R
      new <- df_cols(
        x = fr$S[[1]] + tc * fr$Lc * fr$u[[1]] + o * fr$n[[1]],
        y = fr$S[[2]] + tc * fr$Lc * fr$u[[2]] + o * fr$n[[2]],
        layer = layer_c
      )
      wp <- sort_waypoints(df_bind(wp, new), fr)
    }
  }
  wp
}

sort_waypoints <- function(wp, fr) {
  t <- (wp$x - fr$S[[1]]) * fr$u[[1]] + (wp$y - fr$S[[2]]) * fr$u[[2]]
  df_rows(wp, order(t))
}

#' Build, sample, verify, and repair the curve of one edge
#'
#' @param fr Edge frame from `edge_frame()`.
#' @param wp Waypoints with `x`, `y`, `layer`.
#' @param obstacles Non-endpoint nodes with `name`, `x`, `y`, `layer`.
#' @param R_vec Clearance radius per obstacle.
#' @param arm_min End arm lengths, one per end.
#' @param tier `"spanning"` or `"free"`; decides how repairs move waypoints.
#' @param bounds Panel bounds; a repair that pushes a waypoint outside them
#'   ends the loop, since the route can no longer be drawn inside the panel.
#' @param repair Whether to run the repair loop.
#' @return A list with `path`, `wp`, `clearance_ok`, and `depth` (the total
#'   violation depth of the returned curve, 0 when it verifies).
#' @noRd
route_spline_edge <- function(
  fr,
  wp,
  obstacles,
  R_vec,
  arm_min,
  opts,
  tier,
  layers,
  la,
  lb,
  bounds,
  repair = TRUE
) {
  best <- NULL
  best_depth <- Inf
  for (iter in 0:opts$repair_iter) {
    P <- rbind(fr$S, cbind(wp$x, wp$y), fr$E)
    n <- nrow(P)
    d_s <- clamp_direction(P[2, ] - P[1, ], fr$E - fr$S, opts$tangent_clamp)
    d_e <- clamp_direction(P[n, ] - P[n - 1, ], fr$E - fr$S, opts$tangent_clamp)
    B <- catmull_rom_beziers(
      P,
      opts$alpha,
      d_s,
      d_e,
      arm_min,
      opts$arm_fraction
    )
    pts <- sample_beziers(B, opts$sample_spacing, opts$sample_min_n)
    viol <- verify_clearance(pts, obstacles, R_vec, opts$verify_tol)
    total <- sum(viol$depth)
    if (nrow(viol) == 0) {
      return(list(path = pts, wp = wp, clearance_ok = TRUE, depth = 0))
    }
    if (!repair) {
      return(list(path = pts, wp = wp, clearance_ok = FALSE, depth = total))
    }
    if (total < best_depth) {
      best <- list(path = pts, wp = wp)
      best_depth <- total
    }
    if (iter == opts$repair_iter) {
      break
    }
    wp <- repair_waypoints(
      wp,
      viol,
      pts,
      fr,
      obstacles,
      R_vec,
      tier,
      layers,
      la,
      lb,
      opts
    )
    if (!inside_bounds(wp, bounds, opts$pad)) {
      break
    }
  }
  list(path = best$path, wp = best$wp, clearance_ok = FALSE, depth = best_depth)
}

# Costs ------------------------------------------------------------------------------------

#' A placed polyline with the bounding boxes of its segments
#' @noRd
placed_polyline <- function(x, y) {
  n <- length(x)
  cx <- x[-n]
  cy <- y[-n]
  dx <- x[-1]
  dy <- y[-1]
  list(
    cx = cx,
    cy = cy,
    dx = dx,
    dy = dy,
    xmin = pmin(cx, dx),
    xmax = pmax(cx, dx),
    ymin = pmin(cy, dy),
    ymax = pmax(cy, dy),
    n = n - 1L
  )
}

#' Count proper crossings between a waypoint chain and a placed polyline
#'
#' Segment pairs whose endpoints straddle each other's line; touching at a
#' shared endpoint does not count. Each segment of the chain is tested only
#' against the segments of the polyline whose bounding boxes overlap it.
#'
#' @param pa Two-column matrix of chain points.
#' @param pb A polyline from `placed_polyline()`.
#' @noRd
count_polyline_crossings <- function(pa, pb) {
  na <- nrow(pa) - 1L
  if (na < 1 || pb$n < 1) {
    return(0L)
  }
  if (
    max(pa[, 1]) < min(pb$xmin) ||
      min(pa[, 1]) > max(pb$xmax) ||
      max(pa[, 2]) < min(pb$ymin) ||
      min(pa[, 2]) > max(pb$ymax)
  ) {
    return(0L)
  }
  total <- 0L
  for (i in seq_len(na)) {
    ax <- pa[i, 1]
    ay <- pa[i, 2]
    bx <- pa[i + 1, 1]
    by <- pa[i + 1, 2]
    near <- pb$xmax >= min(ax, bx) &
      pb$xmin <= max(ax, bx) &
      pb$ymax >= min(ay, by) &
      pb$ymin <= max(ay, by)
    if (!any(near)) {
      next
    }
    cx <- pb$cx[near]
    cy <- pb$cy[near]
    dx <- pb$dx[near]
    dy <- pb$dy[near]
    d1 <- (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    d2 <- (bx - ax) * (dy - ay) - (by - ay) * (dx - ax)
    d3 <- (dx - cx) * (ay - cy) - (dy - cy) * (ax - cx)
    d4 <- (dx - cx) * (by - cy) - (dy - cy) * (bx - cx)
    total <- total + sum(d1 * d2 < 0 & d3 * d4 < 0)
  }
  total
}

#' Cost of routing an edge through waypoints on one side
#'
#' `16` per crossing of another edge's placed polyline or chord (edges that
#' share an endpoint are never counted), one per reference radius of
#' displacement, and `2` per edge incident to either endpoint whose far
#' endpoint lies strictly on `side`, more than `R` from the chord line.
#'
#' @param fr Edge frame.
#' @param wp Waypoints with `x`, `y`.
#' @param ectx Per-edge context from `edge_cost_context()`.
#' @noRd
side_cost <- function(fr, wp, side, displacement, ectx, placed, opts) {
  poly <- rbind(fr$S, cbind(wp$x, wp$y), fr$E)
  crossings <- 0L
  for (o in ectx$others) {
    crossings <- crossings + count_polyline_crossings(poly, placed[[o]])
  }
  congestion <- sum(side * ectx$h_far > opts$R)
  opts$crossing_penalty *
    crossings +
    opts$displacement_weight * displacement / opts$r_ref +
    opts$congestion_penalty * congestion
}

#' The other edges an edge is priced against
#'
#' `others` are the edges that share no endpoint with edge `e` (their
#' polylines are tested for crossings); `h_far` holds the signed offsets from
#' the chord line of the far endpoints of the edges incident to `e`.
#'
#' @noRd
edge_cost_context <- function(fr, e, ctx) {
  ends <- c(ctx$from[[e]], ctx$to[[e]])
  from_in <- ctx$from %in% ends
  to_in <- ctx$to %in% ends
  shares <- from_in | to_in
  incident <- which(shares)
  incident <- incident[incident != e]
  far <- ifelse(from_in[incident], ctx$to[incident], ctx$from[incident])
  far <- far[!far %in% ends]
  list(
    others = which(!shares & ctx$Lc > 0),
    h_far = (ctx$nodes$x[far] - fr$S[[1]]) *
      fr$n[[1]] +
      (ctx$nodes$y[far] - fr$S[[2]]) * fr$n[[2]]
  )
}

# Waypoint tiers ----------------------------------------------------------------------------

#' Waypoints of the spanning tier
#'
#' One waypoint per crossed layer, snapped into the free slots of that layer
#' on each side. Interior candidates use every free interval; periphery
#' candidates (span at least `periphery_span`) use only the outer ones.
#' Candidates that share a slot with an already routed edge are spread by
#' `sep_e`, reduced to one arch by the hull, and priced by `side_cost()`.
#'
#' @return `NULL` when every candidate has a slot with no free y; otherwise
#'   a list with `wp`, `side`, `scope`, and `occ` (the slots to register).
#' @noRd
assign_spanning_waypoints <- function(
  fr,
  la,
  lb,
  layers,
  ints,
  occ,
  ectx,
  placed,
  opts
) {
  crossed <- (la + 1L):(lb - 1L)
  yc <- fr$S[[2]] +
    (layers$x[crossed] - fr$S[[1]]) /
      (fr$E[[1]] - fr$S[[1]]) *
      (fr$E[[2]] - fr$S[[2]])
  scopes <- if (lb - la >= opts$periphery_span) {
    c("interior", "periphery")
  } else {
    "interior"
  }

  cands <- list()
  for (side in c(1, -1)) {
    for (scope in scopes) {
      yk <- vapply(
        seq_along(crossed),
        function(i) {
          nearest_free_y(ints[[i]], yc[[i]], side, scope == "periphery")
        },
        numeric(1)
      )
      if (anyNA(yk)) {
        next
      }
      wp <- df_cols(x = layers$x[crossed], y = yk, layer = crossed)
      sp <- spread_in_slot(wp, ints, occ, side, opts$sep_e)
      wp <- hull_waypoints(fr$S, sp$wp, fr$E, side)
      at <- match(wp$layer, crossed)
      displacement <- sum(abs(wp$y - yc[at]))
      cost <- side_cost(fr, wp, side, displacement, ectx, placed, opts)
      cands[[length(cands) + 1]] <- list(
        side = side,
        scope = scope,
        wp = wp,
        cost = round(cost, opts$cost_digits),
        occ = df_cols(layer = wp$layer, slot = sp$slot[at], y = wp$y)
      )
    }
  }
  if (length(cands) == 0) {
    return(NULL)
  }
  cost <- vapply(cands, function(c) c$cost, numeric(1))
  interior <- vapply(cands, function(c) c$scope == "interior", logical(1))
  above <- vapply(cands, function(c) c$side > 0, logical(1))
  cands[[order(cost, !interior, !above)[[1]]]]
}

#' Waypoints of the free-bow tier
#'
#' Each hit gets a waypoint at its chord parameter (clamped to `t_clamp`)
#' offset `h + side * R` from the chord, so it sits exactly `R` from the
#' node on the chosen side; the hull merges same-side obstacles into one
#' bow. A bow that would exceed the sagitta cap is recomputed at the soft
#' margin and flagged. Both single sides are priced; when neither is
#' feasible without the cap, an S through opposite-side waypoints is tried,
#' then the cheaper capped side, and finally a least-bad bow whose waypoints
#' are clamped inside the panel and reported with `clearance_ok = FALSE`.
#'
#' @param hits Hits in the edge frame with `h`, `t`, `r`, `layer`.
#' @param extra Additional margin for members of a parallel-edge group.
#' @noRd
free_bow_waypoints <- function(hits, fr, extra, bounds, ectx, placed, opts) {
  R_i <- hits$r + opts$m + extra
  R_soft_i <- hits$r + opts$m_min + extra
  limit <- opts$sagitta_max * fr$Lc

  cands <- list()
  for (side in c(1, -1)) {
    o <- hits$h + side * R_i
    capped <- FALSE
    if (max(abs(o)) + extra > limit) {
      o <- hits$h + side * R_soft_i
      capped <- TRUE
    }
    wp <- bow_points(hits, fr, o, opts)
    wp$o <- o
    wp <- hull_waypoints(fr$S, wp, fr$E, side)
    feasible <- nrow(wp) > 0 && inside_bounds(wp, bounds, opts$pad)
    cost <- if (feasible) {
      side_cost(fr, wp, side, sum(abs(wp$o)), ectx, placed, opts)
    } else {
      Inf
    }
    cands[[length(cands) + 1]] <- list(
      side = side,
      wp = df_cols(x = wp$x, y = wp$y, layer = wp$layer),
      cost = round(cost, opts$cost_digits),
      capped = capped
    )
  }
  cost <- vapply(cands, function(c) c$cost, numeric(1))
  capped <- vapply(cands, function(c) c$capped, logical(1))
  pick <- function(ok) {
    k <- which(ok)
    k <- k[order(cost[k], -vapply(cands[k], function(c) c$side, numeric(1)))]
    c(cands[[k[[1]]]], list(mode = "bow", least_bad = FALSE))
  }

  ok <- is.finite(cost) & !capped
  if (any(ok)) {
    return(pick(ok))
  }

  # S-route through opposite-side waypoints when both single sides fail
  s <- -sign(hits$h)
  s[s == 0] <- 1
  if (length(unique(s)) > 1) {
    o <- hits$h + s * R_i
    ord <- order(hits$t)
    wp <- df_rows(bow_points(hits, fr, o, opts), ord)
    gap <- diff(hits$t[ord]) * fr$Lc
    flips <- diff(s[ord]) != 0
    if (
      all(gap[flips] >= 2 * max(R_i)) && inside_bounds(wp, bounds, opts$pad)
    ) {
      return(list(
        side = NA_real_,
        wp = wp,
        cost = NA_real_,
        capped = FALSE,
        mode = "bow",
        least_bad = FALSE
      ))
    }
  }

  if (any(is.finite(cost))) {
    return(pick(is.finite(cost)))
  }

  # least-bad: the single side whose waypoints need the least clamping
  clamped <- lapply(cands, function(c) {
    clamp_into_bounds(c$wp, bounds, opts$pad)
  })
  moved <- vapply(
    seq_along(cands),
    function(k) {
      sum(abs(clamped[[k]]$x - cands[[k]]$wp$x)) +
        sum(abs(clamped[[k]]$y - cands[[k]]$wp$y))
    },
    numeric(1)
  )
  k <- order(moved, -vapply(cands, function(c) c$side, numeric(1)))[[1]]
  list(
    side = cands[[k]]$side,
    wp = clamped[[k]],
    cost = Inf,
    capped = cands[[k]]$capped,
    mode = "bow",
    least_bad = TRUE
  )
}

#' Sub-2 mm nudges for grazed nodes
#'
#' Every hit is in the soft band, so the curve only needs to move out to `R`
#' on the node's far side: an offset of `h - sign(h) * R`, at most
#' `m - m_min` in magnitude. Opposite-side nudges form an S too shallow to
#' see.
#'
#' @noRd
soft_nudge_waypoints <- function(hits, fr, extra, opts) {
  s <- -sign(hits$h)
  s[s == 0] <- 1
  o <- hits$h + s * (hits$r + opts$m + extra)
  df_rows(bow_points(hits, fr, o, opts), order(hits$t))
}

#' Waypoints offset from the chord at each hit's clamped parameter
#' @noRd
bow_points <- function(hits, fr, offsets, opts) {
  tc <- pmin(pmax(hits$t, opts$t_clamp[[1]]), opts$t_clamp[[2]])
  df_cols(
    x = fr$S[[1]] + tc * fr$Lc * fr$u[[1]] + offsets * fr$n[[1]],
    y = fr$S[[2]] + tc * fr$Lc * fr$u[[2]] + offsets * fr$n[[2]],
    layer = hits$layer
  )
}

inside_bounds <- function(wp, bounds, pad) {
  all(
    wp$x >= bounds[[1]] + pad &
      wp$x <= bounds[[3]] - pad &
      wp$y >= bounds[[2]] + pad &
      wp$y <= bounds[[4]] - pad
  )
}

clamp_into_bounds <- function(wp, bounds, pad) {
  wp$x <- pmin(pmax(wp$x, bounds[[1]] + pad), bounds[[3]] - pad)
  wp$y <- pmin(pmax(wp$y, bounds[[2]] + pad), bounds[[4]] - pad)
  wp
}

#' Translate the waypoints of a parallel-edge group member
#'
#' Members of a group of `k` edges between the same two nodes are routed
#' with an extra margin of `sep_m * (k - 1) / 2` and then translated along
#' the chord normal by `sep_m * (i - (k + 1) / 2)`, so the innermost member
#' sits at the ordinary clearance and the group spreads outward. Spanning
#' waypoints move along y only, by the amount that gives the same
#' perpendicular displacement.
#'
#' @noRd
offset_parallel_edges <- function(wp, shift, fr, tier) {
  if (shift == 0 || nrow(wp) == 0) {
    return(wp)
  }
  if (tier == "spanning") {
    wp$y <- wp$y + shift / fr$u[[1]]
  } else {
    wp$x <- wp$x + shift * fr$n[[1]]
    wp$y <- wp$y + shift * fr$n[[2]]
  }
  wp
}

#' A data frame from equal-length columns without the cost of data.frame()
#' @noRd
df_cols <- function(...) {
  new_df(list(...))
}

new_df <- function(cols) {
  attr(cols, "row.names") <- .set_row_names(length(cols[[1]]))
  class(cols) <- "data.frame"
  cols
}

#' Row subset and row concatenation for the light frames above
#' @noRd
df_rows <- function(df, idx) {
  new_df(lapply(unclass(df), function(col) col[idx]))
}

df_bind <- function(a, b) {
  cols <- lapply(names(a), function(nm) c(a[[nm]], b[[nm]]))
  names(cols) <- names(a)
  new_df(cols)
}

empty_waypoints <- function() {
  df_cols(x = numeric(0), y = numeric(0), layer = integer(0))
}

#' Route one candidate waypoint set of an edge
#'
#' Applies the parallel-group offset, then builds, verifies, and repairs the
#' curve. `job` carries the edge frame, obstacles, radii, and constants.
#'
#' @noRd
route_candidate <- function(
  job,
  wp,
  side,
  mode,
  tier,
  placed,
  soft = FALSE,
  capped = FALSE,
  least_bad = FALSE
) {
  wp <- offset_parallel_edges(wp, job$shift, job$fr, tier)
  res <- route_spline_edge(
    job$fr,
    wp,
    job$obstacles,
    if (soft || capped) job$R_soft else job$R_full,
    job$arm_min,
    job$opts,
    tier,
    job$layers,
    job$la,
    job$lb,
    job$bounds,
    repair = !least_bad
  )
  res$side <- side
  res$mode <- mode
  res$tier <- tier
  res$capped <- capped
  res$least_bad <- least_bad
  res$clearance_ok <- res$clearance_ok && !least_bad
  res$inside <- inside_bounds(res$wp, job$bounds, job$opts$pad)
  res
}

#' Route an edge through the free-bow tier
#' @noRd
route_free_bow <- function(job, placed) {
  fb <- free_bow_waypoints(
    job$hits,
    job$fr,
    job$extra,
    job$bounds,
    job$ectx,
    placed,
    job$opts
  )
  route_candidate(
    job,
    fb$wp,
    fb$side,
    fb$mode,
    "free",
    placed,
    capped = fb$capped,
    least_bad = fb$least_bad
  )
}

# Engine -------------------------------------------------------------------------------------

#' Route a canonically oriented scene
#' @noRd
route_scene_mm <- function(nodes, edges, bounds, cap, mode, opts) {
  n_edges <- nrow(edges)
  from_name <- as.character(edges$from)
  to_name <- as.character(edges$to)
  from <- match(from_name, nodes$name)
  to <- match(to_name, nodes$name)
  if (anyNA(from) || anyNA(to)) {
    unknown <- unique(c(from_name[is.na(from)], to_name[is.na(to)]))
    abort(
      c(
        "Every edge endpoint must be a node of the scene.",
        "x" = "Unknown node{?s}: {.val {unknown}}."
      ),
      error_class = "ggdag_missing_nodes_error"
    )
  }

  curvature <- if ("curvature" %in% names(edges)) {
    as.numeric(edges$curvature)
  } else {
    rep(NA_real_, n_edges)
  }
  fixed_paths <- if ("fixed_path" %in% names(edges)) {
    edges$fixed_path
  } else {
    vector("list", n_edges)
  }
  is_fixed <- !is.na(curvature)

  paths <- vector("list", n_edges)
  waypoints <- rep(list(empty_waypoints()), n_edges)
  routed <- logical(n_edges)
  mode_out <- rep("straight", n_edges)
  side_out <- rep(NA_real_, n_edges)
  n_wp <- integer(n_edges)
  wp_layers <- rep(list(integer(0)), n_edges)
  clearance_ok <- rep(TRUE, n_edges)
  sagitta <- numeric(n_edges)
  capped <- logical(n_edges)

  for (e in seq_len(n_edges)) {
    paths[[e]] <- df_cols(
      x = c(nodes$x[[from[[e]]]], nodes$x[[to[[e]]]]),
      y = c(nodes$y[[from[[e]]]], nodes$y[[to[[e]]]])
    )
  }
  for (e in which(is_fixed)) {
    mode_out[[e]] <- "fixed"
    fp <- fixed_paths[[e]]
    if (is.data.frame(fp) && nrow(fp) >= 2) {
      paths[[e]] <- df_cols(x = as.numeric(fp$x), y = as.numeric(fp$y))
    }
  }

  assemble <- function() {
    meta <- data.frame(
      edge = paste0(from_name, "->", to_name),
      routed = routed,
      mode = mode_out,
      side = side_out,
      n_waypoints = n_wp,
      stringsAsFactors = FALSE
    )
    meta$waypoint_layers <- wp_layers
    meta$clearance_ok <- clearance_ok
    meta$sagitta_ratio <- sagitta
    meta$sagitta_capped <- capped
    list(paths = paths, meta = meta, waypoints = waypoints)
  }
  if (mode == "straight" || n_edges == 0 || nrow(nodes) == 0) {
    return(assemble())
  }

  layers <- infer_layers(nodes, opts$tol_layer)
  info <- edge_span_info(nodes, from, to, layers)
  routable <- !is_fixed & info$Lc > 0 & from != to
  R_full <- nodes$r + opts$m
  R_soft <- nodes$r + opts$m_min

  hits <- find_blocked_edges(
    nodes,
    edges[routable, , drop = FALSE],
    R_soft,
    R_full
  )
  hits$edge <- which(routable)[hits$edge]

  # parallel groups: members share an unordered node pair
  extra <- numeric(n_edges)
  shift <- numeric(n_edges)
  key <- paste(pmin(from, to), pmax(from, to))
  for (k in unique(key[routable])) {
    members <- which(routable & key == k)
    if (length(members) < 2) {
      next
    }
    members <- members[order(
      from_name[members],
      to_name[members],
      method = "radix"
    )]
    size <- length(members)
    extra[members] <- opts$sep_m * (size - 1) / 2
    shift[members] <- opts$sep_m * (seq_len(size) - (size + 1) / 2)
  }

  to_route <- routable & (seq_len(n_edges) %in% hits$edge | shift != 0)
  order_e <- which(to_route)
  order_e <- order_e[order(
    -info$span[order_e],
    -info$Lc[order_e],
    from_name[order_e],
    to_name[order_e],
    method = "radix"
  )]

  placed <- lapply(paths, function(p) placed_polyline(p$x, p$y))
  occ <- df_cols(layer = integer(0), slot = integer(0), y = numeric(0))
  ctx <- list(nodes = nodes, from = from, to = to, Lc = info$Lc)
  base_intervals <- lapply(seq_len(layers$n), function(k) {
    layer_free_intervals(
      nodes[layers$members[[k]], , drop = FALSE],
      opts$m,
      bounds,
      opts$pad
    )
  })

  for (e in order_e) {
    fr <- edge_frame(nodes, from[[e]], to[[e]], info$reversed[[e]])
    la <- info$la[[e]]
    lb <- info$lb[[e]]
    ectx <- edge_cost_context(fr, e, ctx)

    he <- df_rows(hits, which(hits$edge == e))
    idx <- match(he$node, nodes$name)
    dx <- nodes$x[idx] - fr$S[[1]]
    dy <- nodes$y[idx] - fr$S[[2]]
    eh <- df_cols(
      node = idx,
      h = dx * fr$n[[1]] + dy * fr$n[[2]],
      t = (dx * fr$u[[1]] + dy * fr$u[[2]]) / fr$Lc,
      layer = layers$id[idx],
      r = nodes$r[idx],
      hard = he$severity == "hard"
    )
    eh <- df_rows(eh, order(eh$t, nodes$name[idx]))

    others <- setdiff(seq_len(nrow(nodes)), c(fr$a, fr$b))
    job <- list(
      fr = fr,
      la = la,
      lb = lb,
      layers = layers,
      shift = shift[[e]],
      extra = extra[[e]],
      bounds = bounds,
      arm_min = c(nodes$r[[fr$a]], nodes$r[[fr$b]]) + cap,
      obstacles = df_cols(
        name = nodes$name[others],
        x = nodes$x[others],
        y = nodes$y[others],
        layer = layers$id[others]
      ),
      R_full = R_full[others],
      R_soft = R_soft[others],
      hits = eh,
      ectx = ectx,
      opts = opts
    )

    res <- NULL
    cand <- NULL
    if (nrow(eh) == 0) {
      # a parallel-group member whose chord is clear: one midpoint waypoint
      wp <- df_cols(
        x = fr$S[[1]] + 0.5 * fr$Lc * fr$u[[1]],
        y = fr$S[[2]] + 0.5 * fr$Lc * fr$u[[2]],
        layer = NA_integer_
      )
      res <- route_candidate(job, wp, sign(shift[[e]]), "bow", "free", placed)
    } else if (!any(eh$hard)) {
      wp <- soft_nudge_waypoints(eh, fr, extra[[e]], opts)
      res <- route_candidate(
        job,
        wp,
        NA_real_,
        "soft",
        "free",
        placed,
        soft = TRUE
      )
    } else {
      spanning <- lb - la >= 2 && info$angle[[e]] <= opts$steep_deg
      if (spanning) {
        crossed <- (la + 1L):(lb - 1L)
        ints <- if (extra[[e]] == 0) {
          base_intervals[crossed]
        } else {
          lapply(crossed, function(k) {
            layer_free_intervals(
              nodes[layers$members[[k]], , drop = FALSE],
              opts$m + extra[[e]],
              bounds,
              opts$pad
            )
          })
        }
        cand <- assign_spanning_waypoints(
          fr,
          la,
          lb,
          layers,
          ints,
          occ,
          ectx,
          placed,
          opts
        )
        if (!is.null(cand) && nrow(cand$wp) > 0) {
          wp <- cand$wp
          outside <- df_rows(eh, which(eh$layer <= la | eh$layer >= lb))
          if (nrow(outside) > 0) {
            o <- outside$h + cand$side * (outside$r + opts$m + extra[[e]])
            wp <- hull_waypoints(
              fr$S,
              df_bind(wp, bow_points(outside, fr, o, opts)),
              fr$E,
              cand$side
            )
          }
          res <- route_candidate(
            job,
            wp,
            cand$side,
            cand$scope,
            "spanning",
            placed
          )
          # a spanning route that cannot be verified inside the panel falls
          # through to the free-bow tier, which is kept when it does better
          if (!res$clearance_ok || !res$inside) {
            alt <- route_free_bow(job, placed)
            if (
              alt$clearance_ok ||
                (!res$clearance_ok && alt$depth < res$depth) ||
                (!res$inside && alt$inside)
            ) {
              res <- alt
            }
          }
        }
      }
      if (is.null(res)) {
        res <- route_free_bow(job, placed)
      }
    }

    if (res$tier == "spanning" && !is.null(cand)) {
      occ <- df_bind(occ, cand$occ)
    }

    path <- res$path
    placed[[e]] <- placed_polyline(path$x, path$y)
    if (info$reversed[[e]]) {
      path <- df_cols(x = rev(path$x), y = rev(path$y))
    }
    offset <- (path$x - fr$S[[1]]) *
      fr$n[[1]] +
      (path$y - fr$S[[2]]) * fr$n[[2]]

    paths[[e]] <- path
    waypoints[[e]] <- df_cols(
      x = res$wp$x,
      y = res$wp$y,
      layer = as.integer(res$wp$layer)
    )
    routed[[e]] <- TRUE
    mode_out[[e]] <- res$mode
    side_out[[e]] <- res$side
    n_wp[[e]] <- nrow(res$wp)
    wp_layers[[e]] <- as.integer(res$wp$layer)
    clearance_ok[[e]] <- res$clearance_ok
    sagitta[[e]] <- max(abs(offset)) / fr$Lc
    capped[[e]] <- res$capped
  }

  assemble()
}
