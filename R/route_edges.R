# Edge routing in millimetres. The router takes node discs and edge chords in
# the units they are drawn in, infers layers from the layer-axis coordinate,
# and routes every blocked edge as a centripetal Catmull-Rom spline through a
# few waypoints: one per crossed layer, snapped into the free slots of that
# layer, or a single bow around the obstacle for short and steep chords. A
# chord shorter than 2R stays straight, since no bow fits between endpoint
# discs that close. The slots of a layer are the gaps between its padded
# discs; a gap narrower than the edge separation is a sliver and is not a
# slot, and a periphery slot keeps the clearance margin m from the panel
# bounds, as every drawn curve does. The side of a detour is chosen by a cost that
# weighs edge crossings, displacement, and the crowding of the endpoints'
# fans. Every routed curve is sampled, verified against the node discs, and
# repaired by pushing its waypoints outward; a periphery arch is levelled
# first, every waypoint raised to the outermost one, so its apex sits at
# mid-span. The drawn arch is then registered at every layer it crosses,
# and later edges through the same slot are spread from it by the edge
# separation in the order of their chords, a repaired arch being spread
# again if the repair moved it onto a neighbour. When a slot came out in
# routing order rather than chord order, the scene is routed a second time
# with positions reserved for the inner chords. Every drawn curve keeps m
# from the panel bounds: a route whose curve comes closer yields to the free
# bow, and failing that is clamped to that margin and reported without
# clearance. The engine is
# pure: it consumes no randomness, does not depend on row order, and returns
# identical output on repeated calls.
#
# The layer axis is called x and the within-layer axis y. A scene whose
# layers run along device y is transposed on entry and back on exit. Every
# edge is routed with its source to the left of its target; "above" (side +1)
# means larger y in that orientation, and the polyline of an edge that runs
# right to left is reversed on output.
#
# Orthogonal mode shares the orientation, the layers, the side cost, and the
# parallel-edge spreading, but draws every edge as axis-aligned runs whether
# or not a node blocks its chord. Chords that are already axis-aligned stay
# straight: vertical chords, level chords (tilted by no more than the corner
# radius) between adjacent layers, level spanning chords that no crossed
# disc blocks, and chords between two nodes of one layer. A spanning edge
# chooses the cheapest of its candidate channels, each priced by
# displacement, bends, crossings, and congestion: an S or N channel past
# the crossed stacks when its endpoints are the extreme nodes of their
# layers, an E/W run at either endpoint's line or beyond the stacks, and,
# when no S/N channel fits, a run through a free interval of every crossed
# layer. A channel that would cut a disc or run inside the panel margin is
# infeasible; channels that would share a y over overlapping x-ranges are
# stacked sep_e apart, the shorter span inside. Every other edge leaves
# through the E port and enters through the W port, with one vertical run
# per crossed gap. Within a gap the vertical runs are hyperedge segments (a
# fan leaving one port shares one); segments from different sources never
# share a slot, even when they only meet or come within sep_e. They are
# ordered by a dependency graph weighted by the crossings each order would
# cause, an order whose horizontal pieces would coincide being forbidden,
# numbered by longest path, and placed on the first rung of a ladder that
# holds them: the nominal stub and an even spread, then a shorter stub, a
# tighter spacing, a smaller corner radius, and finally slots spread between
# the layers' soft bands without clearance. The arrivals on a node's W side
# take stacked rows beside its centre line, the level chord keeping the
# centre and a group of three or more merging onto one row, and two channel
# stubs on one N or S side sit sep_e / 2 either side of the centre line, so
# no stub carries two edges in opposite directions and every arrowhead is
# drawn on a row of its own. Bends are then rounded with a quadratic
# Bezier, the runs are sampled, the resect each end needs to put its head
# tip on the cap line is measured, and the result is verified against the
# node discs like a spline.

#' Constants of the millimetre router
#'
#' Derives every constant the router uses from a reference node radius. The
#' clearance margin, the edge separations, and the layer tolerance scale with
#' the radius above millimetre floors; the remaining values are dimensionless
#' or fixed lengths. `m` and `sep_e` override the two the caller is allowed to
#' set; `layer_axis` overrides the axis the layers are inferred from;
#' `corners` chooses how orthogonal mode draws its bends and `bend_penalty`
#' what it charges for them.
#'
#' @param r_ref Reference node radius in mm, typically the median radius.
#' @param m Clearance margin in mm, or `NULL` for `max(0.5 * r_ref, 1.2)`.
#' @param sep_e Edge separation within a slot in mm, or `NULL` for
#'   `max(0.6 * r_ref, 1.5)`.
#' @param sep_min The separation the orthogonal ladder may tighten the slot
#'   spacing of a narrow gap to, in mm, or `NULL` for `max(0.25 * r_ref,
#'   1.5)`. Setting it equal to `sep_e` fixes the spacing.
#' @param layer_axis The axis the layers run along: `"auto"` infers it,
#'   `"x"` and `"y"` name it.
#' @param corners In orthogonal mode, `"rounded"` replaces every bend with a
#'   quadratic Bezier of radius `rc`, and `"sharp"` keeps the bends. Spline
#'   and straight mode ignore it.
#' @param bend_penalty In orthogonal mode, the price of one bend in the
#'   units of the other cost terms (one reference radius of displacement is
#'   1). Two bends are the price of one detour, so at the default of 2 a
#'   four-bend run must save a second detour's displacement to win.
#' @return A named list of constants.
#' @noRd
route_opts <- function(
  r_ref,
  m = NULL,
  sep_e = NULL,
  sep_min = NULL,
  layer_axis = c("auto", "x", "y"),
  corners = c("rounded", "sharp"),
  bend_penalty = 2
) {
  layer_axis <- if (identical(layer_axis, c("auto", "x", "y"))) {
    "auto"
  } else {
    match.arg(layer_axis)
  }
  corners <- if (identical(corners, c("rounded", "sharp"))) {
    "rounded"
  } else {
    match.arg(corners)
  }
  m <- m %||% max(0.5 * r_ref, 1.2)
  m_min <- min(1.2, m)
  sep_e <- sep_e %||% max(0.6 * r_ref, 1.5)
  list(
    r_ref = r_ref,
    m = m,
    m_min = m_min,
    layer_axis = layer_axis,
    corners = corners,
    rc = min(max(0.35 * r_ref, 0.8), 2.5),
    # the floor of the corner radius, the drawn arrowhead length the stub
    # must hold past the cap, and the head width that bounds a port stack
    rc_min = 0.8,
    head = 2,
    head_w = 1.3,
    R = r_ref + m,
    R_soft = r_ref + m_min,
    sep_e = sep_e,
    sep_min = min(sep_min %||% max(0.25 * r_ref, 1.5), sep_e),
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
    bend_penalty = bend_penalty,
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
#' @param mode `"spline"` routes blocked edges as curves and `"straight"`
#'   draws every edge as its chord. `"orthogonal"` draws every edge as
#'   axis-aligned runs whether or not its chord is blocked: E and W ports
#'   with one vertical run per crossed gap at an assigned slot, or, for a
#'   spanning edge between the extreme nodes of their layers, S and N ports
#'   with a channel run past the crossed stacks. Corners are rounded unless
#'   `opts$corners` is `"sharp"`. A chord that is already axis-aligned stays
#'   straight: a vertical chord, a horizontal chord between adjacent layers,
#'   or a horizontal spanning chord that no crossed disc blocks; so does a
#'   chord between two nodes of one layer.
#' @param opts Constants from `route_opts()`.
#' @return A list with `paths` (one `data.frame(x, y)` per edge, in input
#'   order), `meta` (one row per edge: `edge`, `routed`, `mode`, `side`,
#'   `n_waypoints`, `waypoint_layers`, `clearance_ok`, `sagitta_ratio`,
#'   `sagitta_capped`), and `waypoints` (one `data.frame(x, y, layer)` per
#'   edge). `clearance_ok` is `FALSE` when the drawn curve could not be kept
#'   `R` from every node disc, when its arch had to stop on another edge's
#'   arch in a shared slot, or when it left the panel and was clamped to it.
#'   In orthogonal mode `meta` also carries `resect_head` and `resect_fins`,
#'   the arc length in mm from each end of the path to the cap line on that
#'   end's port axis (exactly `cap` at a centre port, more at an offset
#'   port, whose hidden connector the resect must pass), and the result
#'   carries `ortho`: `rc`, the corner radius the scene was drawn with, and
#'   `gaps`, one row per gap that holds a slot with `gap`, `width`,
#'   `ranks`, `rung`, `stub`, and `spacing` (see `ortho_slot_positions()`;
#'   `stub` is `NA` on the last rung, where no stub fits).
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
    opts,
    scene$layers
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
#' @return A list with `nodes`, `edges`, `bounds`, `transposed`, and the
#'   `layers` of the oriented scene when they were inferred (`NULL` when
#'   the axis was named).
#' @noRd
canonicalize_scene <- function(nodes, edges, bounds, tol, layer_axis = "auto") {
  nodes <- df_cols(
    name = as.character(nodes$name),
    x = as.numeric(nodes$x),
    y = as.numeric(nodes$y),
    r = as.numeric(nodes$r)
  )
  if (!is.data.frame(edges)) {
    edges <- as.data.frame(edges, stringsAsFactors = FALSE)
  }
  bounds <- as.numeric(bounds)

  transposed <- identical(layer_axis, "y")
  layers <- NULL
  if (identical(layer_axis, "auto") && nrow(nodes) > 0) {
    lx <- infer_layers(nodes, tol)
    layers <- lx
    if (lx$n < 2) {
      transposed <- TRUE
      layers <- infer_layers(df_cols(x = nodes$y), tol)
    } else if (!layers_exact(lx, nodes$x)) {
      ly <- infer_layers(df_cols(x = nodes$y), tol)
      if (ly$n >= 2 && layers_exact(ly, nodes$y)) {
        transposed <- TRUE
        layers <- ly
      }
    }
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
  list(
    nodes = nodes,
    edges = edges,
    bounds = bounds,
    transposed = transposed,
    layers = layers
  )
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
  n <- cluster[[length(cluster)]]
  list(
    id = id,
    x = vapply(
      seq_len(n),
      function(k) {
        v <- xs[cluster == k]
        if (length(v) == 1L) v else mean(v)
      },
      numeric(1)
    ),
    n = n,
    members = lapply(seq_len(n), function(k) which(id == k))
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
  df_cols(
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
  empty <- df_cols(
    edge = integer(),
    node = character(),
    d = numeric(),
    h = numeric(),
    t = numeric(),
    severity = character()
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
  df_cols(
    edge = ei[keep],
    node = nodes$name[ni[keep]],
    d = d[keep],
    h = h[keep],
    t = t[keep],
    severity = ifelse(d[keep] < R_soft[ni[keep]], "hard", "soft")
  )
}

# Free slots within a layer ---------------------------------------------------------

#' Free intervals of y in one layer
#'
#' The gaps between the padded discs of a layer's nodes, plus the space below
#' the lowest and above the highest node down to the panel edge less `pad`.
#' Intervals of zero or negative width are dropped, and so is a gap between
#' two discs narrower than `sep_e`, since a slot that cannot hold an edge
#' with its separation is a sliver; the two intervals toward the panel edges
#' are kept however narrow. The first and last surviving intervals are
#' flagged `outer`.
#'
#' @noRd
layer_free_intervals <- function(
  layer_nodes,
  margin,
  bounds,
  pad = 0.5,
  sep_e = 0
) {
  ord <- order(layer_nodes$y)
  ys <- layer_nodes$y[ord]
  Rk <- layer_nodes$r[ord] + margin
  lo <- c(bounds[[2]] + pad, ys + Rk)
  hi <- c(ys - Rk, bounds[[4]] - pad)
  n <- length(lo)
  width <- hi - lo
  toward_edge <- seq_len(n) %in% c(1L, n)
  keep <- width > 0 & (toward_edge | width >= sep_e)
  lo <- lo[keep]
  hi <- hi[keep]
  n <- length(lo)
  df_cols(lo = lo, hi = hi, outer = seq_len(n) %in% c(1L, n))
}

#' The free intervals a periphery arch may use
#'
#' A periphery waypoint keeps at least the clearance margin `m` from the
#' panel bounds, so the outer interval on each side is clipped by `margin`
#' rather than by the pad. An
#' outer interval that vanishes is kept as a row of `NA` so that slot
#' indices still match the layer's full interval list, and it is no longer
#' flagged `outer`; a side whose outer interval is gone has no periphery
#' slot, and the periphery candidate on that side is infeasible.
#'
#' @param ints Free intervals of one layer from `layer_free_intervals()`.
#' @noRd
periphery_intervals <- function(ints, bounds, margin) {
  n <- nrow(ints)
  if (n == 0) {
    return(ints)
  }
  lo <- ints$lo
  hi <- ints$hi
  outer <- ints$outer
  if (outer[[1]]) {
    lo[[1]] <- max(lo[[1]], bounds[[2]] + margin)
  }
  if (outer[[n]]) {
    hi[[n]] <- min(hi[[n]], bounds[[4]] - margin)
  }
  gone <- which(lo > hi)
  lo[gone] <- NA_real_
  hi[gone] <- NA_real_
  outer[gone] <- FALSE
  df_cols(lo = lo, hi = hi, outer = outer)
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
#' The occupants of a slot are the arches already drawn through it, each
#' registered with the y it passes the layer at and the y of its chord
#' there. A new waypoint keeps the order of the chords when the slot can
#' hold it: it moves `sep_e` beyond every occupant whose chord lies inside
#' its own (nearer the side the route detours to) and stays `sep_e` inside
#' every occupant whose chord lies outside it; when both cannot hold, it
#' moves `sep_e` beyond the outermost occupant. A reserved position (see
#' `slot_reservations()`) holds the waypoint out for edges routed later
#' whose chords lie inside. If that leaves the interval, the next free
#' interval outward is used. When no interval outward can hold the chord
#' order, the waypoint keeps its separation only: it starts from its
#' snapped y and steps `sep_e` beyond any occupant within `sep_e` of it,
#' and if even that leaves every interval it stops at the interval edge and
#' the overlap is reported.
#'
#' @param wp Waypoints with `x`, `y`, `layer`, one per crossed layer.
#' @param ints Free intervals, one data frame per row of `wp`, the ones the
#'   waypoints snap into (a parallel-group member's carry its extra margin).
#' @param occ Occupancy with `layer`, `slot`, `y`, `chord`, whose slots are
#'   indices into the layer's base intervals.
#' @param yc Chord y per row of `wp`.
#' @param reserved Reservations for this edge with `layer`, `slot`, `y`, or
#'   `NULL`.
#' @param base The layer's base free intervals, one per row of `wp`, that
#'   occupancy and reservations are indexed by; `ints` when `NULL`.
#' @return A list with the updated `wp`; `slot`, the index per row of the
#'   interval in `ints` (the member's own list) each waypoint sits in,
#'   whereas `occ$slot` and `reserved$slot` index the layer's base list;
#'   `disordered`, whether any waypoint had to give up the chord order and
#'   keep its separation only; and `overlap`, whether any waypoint had to
#'   stop on an occupant.
#' @noRd
spread_in_slot <- function(
  wp,
  ints,
  occ,
  side,
  sep_e,
  yc,
  reserved = NULL,
  base = NULL
) {
  slot <- integer(nrow(wp))
  base <- base %||% ints
  if (nrow(occ) == 0 && is.null(reserved)) {
    for (i in seq_len(nrow(wp))) {
      s <- which(ints[[i]]$lo <= wp$y[[i]] & wp$y[[i]] <= ints[[i]]$hi)
      slot[[i]] <- if (length(s) == 0) NA_integer_ else s[[1]]
    }
    return(list(wp = wp, slot = slot, disordered = FALSE, overlap = FALSE))
  }
  overlap <- FALSE
  disordered <- FALSE
  outward <- if (side > 0) max else min
  inward <- if (side > 0) min else max
  for (i in seq_len(nrow(wp))) {
    iv <- ints[[i]]
    y0 <- wp$y[[i]]
    s <- which(iv$lo <= y0 & y0 <= iv$hi)
    if (length(s) == 0) {
      slot[[i]] <- NA_integer_
      next
    }
    s <- s[[1]]
    layer <- wp$layer[[i]]
    b <- slot_of(base[[i]], y0, sep_e)
    if (!is.null(reserved)) {
      held <- reserved$y[reserved$layer == layer & reserved$slot == b]
      if (length(held) > 0) {
        y0 <- outward(y0, held[[1]])
      }
    }
    here <- which(occ$layer == layer & occ$slot == b)
    y <- y0
    if (length(here) > 0) {
      oy <- occ$y[here]
      inside <- side * (occ$chord[here] - yc[[i]]) <= 0
      if (any(inside)) {
        y <- outward(y, outward(oy[inside]) + side * sep_e)
      }
      if (!all(inside)) {
        room <- inward(oy[!inside]) - side * sep_e
        if (side * (y - room) > 1e-9) {
          y <- outward(oy) + side * sep_e
        }
      }
    }
    placed <- slot_position(y, iv, s, side)
    if (is.na(placed$slot) && length(here) > 0) {
      disordered <- TRUE
      y <- y0
      repeat {
        near <- abs(oy - y) < sep_e - 1e-9
        if (!any(near)) {
          break
        }
        y <- outward(oy[near]) + side * sep_e
      }
      placed <- slot_position(y, iv, s, side)
    }
    if (is.na(placed$slot)) {
      overlap <- length(here) > 0
      placed <- list(y = if (side > 0) iv$hi[[s]] else iv$lo[[s]], slot = s)
    }
    wp$y[[i]] <- placed$y
    slot[[i]] <- placed$slot
  }
  list(wp = wp, slot = slot, disordered = disordered, overlap = overlap)
}

#' The interval a spread waypoint falls in
#'
#' Its own slot when `y` is still inside it, else the nearest free interval
#' outward; `slot` is `NA` when nothing outward is free.
#'
#' @noRd
slot_position <- function(y, iv, s, side) {
  if (y >= iv$lo[[s]] && y <= iv$hi[[s]]) {
    return(list(y = y, slot = s))
  }
  alt <- nearest_free_y(iv, y, side, FALSE)
  if (is.na(alt)) {
    return(list(y = y, slot = NA_integer_))
  }
  list(y = alt, slot = which(iv$lo <= alt & alt <= iv$hi)[[1]])
}

#' Where a drawn arch passes each crossed layer
#'
#' Registers, for every layer strictly between the endpoints' layers, the y
#' at which the sampled path crosses the layer's x, the slot that y falls
#' in, the y of the chord there, and the innermost y the edge could have
#' taken in that slot on its side. A path that passes a layer just outside
#' a slot, within `sep_e` of it (a repaired curve may sit a fraction of a
#' millimetre inside the clearance), is registered with that slot; layers
#' crossed further from every free interval are not registered.
#'
#' @param path The final sampled path, source to target.
#' @param ints Free intervals, one data frame per crossed layer.
#' @noRd
arch_occupancy <- function(e, path, fr, crossed, layers, ints, side, sep_e) {
  x0 <- layers$x[crossed]
  y <- polyline_y_at(path$x, path$y, x0)
  yc <- fr$S[[2]] +
    (x0 - fr$S[[1]]) / (fr$E[[1]] - fr$S[[1]]) * (fr$E[[2]] - fr$S[[2]])
  slot <- integer(length(crossed))
  base <- numeric(length(crossed))
  lo <- numeric(length(crossed))
  hi <- numeric(length(crossed))
  for (i in seq_along(crossed)) {
    iv <- ints[[i]]
    s <- slot_of(iv, y[[i]], sep_e)
    if (is.na(s)) {
      slot[[i]] <- NA_integer_
      next
    }
    slot[[i]] <- s
    lo[[i]] <- iv$lo[[s]]
    hi[[i]] <- iv$hi[[s]]
    base[[i]] <- if (side > 0) max(lo[[i]], yc[[i]]) else min(hi[[i]], yc[[i]])
  }
  keep <- which(!is.na(slot))
  df_cols(
    edge = rep(e, length(keep)),
    layer = crossed[keep],
    slot = slot[keep],
    y = y[keep],
    chord = yc[keep],
    side = rep(side, length(keep)),
    base = base[keep],
    lo = lo[keep],
    hi = hi[keep]
  )
}

#' The slot a y belongs to
#'
#' The interval containing `y`, else the nearest interval when `y` lies
#' within `sep_e` of it; `NA` otherwise.
#'
#' @noRd
slot_of <- function(iv, y, sep_e) {
  if (is.na(y) || nrow(iv) == 0) {
    return(NA_integer_)
  }
  s <- which(iv$lo <= y & y <= iv$hi)
  if (length(s) > 0) {
    return(s[[1]])
  }
  gap <- pmin(abs(iv$lo - y), abs(iv$hi - y))
  s <- which.min(gap)
  if (length(s) == 0 || gap[[s]] >= sep_e) {
    return(NA_integer_)
  }
  s
}

#' Re-spread a drawn arch that repairs moved onto another edge
#'
#' Repairs and hull insertions place waypoints from the node discs alone,
#' so a repaired arch can end up within `sep_e` of an arch already drawn
#' through the same slot, or out of chord order with it. Every crossed
#' layer of the drawn path is spread again against the occupancy as if it
#' were a fresh waypoint; a layer whose spread position differs from where
#' the path passes gets its waypoint moved there (inserted when the hull
#' had dropped it).
#'
#' @param base The layers' base free intervals the occupancy is indexed by.
#' @return The adjusted waypoints, or `NULL` when the drawn arch already
#'   respects the occupancy.
#' @noRd
respread_arch <- function(res, fr, crossed, layers, ints, occ, opts, base) {
  x0 <- layers$x[crossed]
  y <- polyline_y_at(res$path$x, res$path$y, x0)
  yc <- fr$S[[2]] +
    (x0 - fr$S[[1]]) / (fr$E[[1]] - fr$S[[1]]) * (fr$E[[2]] - fr$S[[2]])
  wp <- res$wp
  moved <- FALSE
  for (i in seq_along(crossed)) {
    s <- slot_of(ints[[i]], y[[i]], opts$sep_e)
    if (is.na(s)) {
      next
    }
    iv <- ints[[i]]
    y_in <- min(max(y[[i]], iv$lo[[s]]), iv$hi[[s]])
    row <- df_cols(x = x0[[i]], y = y_in, layer = crossed[[i]])
    sp <- spread_in_slot(
      row,
      ints[i],
      occ,
      res$side,
      opts$sep_e,
      yc[[i]],
      NULL,
      base[i]
    )
    if (abs(sp$wp$y[[1]] - y_in) < 1e-6) {
      next
    }
    moved <- TRUE
    at <- which(wp$layer == crossed[[i]])
    if (length(at) > 0) {
      wp$y[at] <- sp$wp$y[[1]]
    } else {
      wp <- sort_waypoints(df_bind(wp, sp$wp), fr)
    }
  }
  if (moved) wp else NULL
}

#' y of a polyline where it first crosses each vertical line
#'
#' Linear interpolation on the first segment whose x-range contains `x0`;
#' `NA` when no segment does.
#'
#' @noRd
polyline_y_at <- function(x, y, x0) {
  n <- length(x)
  xa <- x[-n]
  xb <- x[-1]
  vapply(
    x0,
    function(v) {
      k <- which((xa - v) * (xb - v) <= 0)
      if (length(k) == 0) {
        return(NA_real_)
      }
      k <- k[[1]]
      dx <- xb[[k]] - xa[[k]]
      if (abs(dx) < 1e-12) {
        return(y[[k]])
      }
      y[[k]] + (v - xa[[k]]) / dx * (y[[k + 1L]] - y[[k]])
    },
    numeric(1)
  )
}

#' Positions that keep shared slots in chord order
#'
#' Edges whose arches share a slot on the same side should sit in the order
#' of their chords at that layer, the edge with the innermost chord nearest
#' the stack. Routing order alone cannot guarantee this: an edge routed
#' first takes the slot boundary and pushes a later edge with an inner chord
#' outside it. This looks at the occupancy of a finished pass and, for every
#' slot whose occupants are out of chord order, assigns positions inner to
#' outer, each `sep_e` beyond the last. Edges that sit further out than the
#' position they would take on their own get a reservation, and the scene is
#' routed again with it. Only slots whose occupants all sit at a spread
#' position (their own innermost y, or another occupant's y plus `sep_e`)
#' are considered: an arch that passes a layer elsewhere was shaped by a
#' plateau, the hull, or a repair, and a reservation cannot move it. A slot
#' too narrow to hold its occupants in order gets no reservation either.
#'
#' @return `NULL` when every shared slot is already in chord order;
#'   otherwise a data frame with `edge`, `layer`, `slot`, `y`.
#' @noRd
slot_reservations <- function(occ, sep_e) {
  if (nrow(occ) == 0) {
    return(NULL)
  }
  key <- paste(occ$layer, occ$slot, occ$side)
  out <- list()
  for (k in unique(key)) {
    rows <- which(key == k)
    if (length(rows) < 2) {
      next
    }
    side <- occ$side[[rows[[1]]]]
    rows <- rows[order(side * occ$chord[rows], occ$edge[rows])]
    y <- occ$y[rows]
    if (all(side * diff(y) >= 0)) {
      next
    }
    base <- occ$base[rows]
    spread <- vapply(
      seq_along(rows),
      function(i) {
        abs(y[[i]] - base[[i]]) < 1e-6 ||
          any(abs(y[[i]] - (y[-i] + side * sep_e)) < 1e-6)
      },
      logical(1)
    )
    if (!all(spread)) {
      next
    }
    pos <- base
    for (i in seq_along(rows)[-1]) {
      pos[[i]] <- if (side > 0) {
        max(pos[[i]], pos[[i - 1]] + sep_e)
      } else {
        min(pos[[i]], pos[[i - 1]] - sep_e)
      }
    }
    fits <- all(pos >= occ$lo[rows] - 1e-9 & pos <= occ$hi[rows] + 1e-9)
    held <- which(side * (pos - occ$base[rows]) > 1e-9)
    if (fits && length(held) > 0) {
      out[[length(out) + 1]] <- df_cols(
        edge = occ$edge[rows[held]],
        layer = occ$layer[rows[held]],
        slot = occ$slot[rows[held]],
        y = pos[held]
      )
    }
  }
  if (length(out) == 0) {
    return(NULL)
  }
  Reduce(df_bind, out)
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
  if (length(t) == 1L) {
    # a lone waypoint is the arch when it lies strictly on its side
    return(if (side * o[[1]] * Lc > 0) wp else df_rows(wp, integer(0)))
  }
  if (length(t) == 2L) {
    return(hull_two(wp, t, o, Lc, side))
  }
  ord <- order(t, side * o)

  px <- c(0, t[ord], Lc)
  py <- c(0, o[ord], 0)
  idx <- c(0L, ord, 0L)
  m <- length(px)
  hull <- integer(m)
  h <- 0L
  for (k in seq_len(m)) {
    while (h >= 2L) {
      a <- hull[[h - 1L]]
      b <- hull[[h]]
      cr <- (px[[b]] - px[[a]]) *
        (py[[k]] - py[[a]]) -
        (py[[b]] - py[[a]]) * (px[[k]] - px[[a]])
      if (side * cr >= 0) {
        h <- h - 1L
      } else {
        break
      }
    }
    h <- h + 1L
    hull[[h]] <- k
  }
  keep <- idx[hull[seq_len(h)]]
  df_rows(wp, keep[keep > 0])
}

#' The monotone chain of two waypoints, unrolled
#'
#' The same pops as the loop in `hull_waypoints()` over the four points
#' source, first, second, target: the first goes when the second does not
#' turn outward from it, the second when the target does not, and after
#' that the first is checked against the target directly.
#'
#' @noRd
hull_two <- function(wp, t, o, Lc, side) {
  ord <- order(t, side * o)
  a <- ord[[1]]
  b <- ord[[2]]
  turn <- function(px0, py0, px1, py1, px2, py2) {
    side * ((px1 - px0) * (py2 - py0) - (py1 - py0) * (px2 - px0)) >= 0
  }
  keep_a <- !turn(0, 0, t[[a]], o[[a]], t[[b]], o[[b]])
  if (keep_a) {
    keep_b <- !turn(t[[a]], o[[a]], t[[b]], o[[b]], Lc, 0)
    if (!keep_b) {
      keep_a <- !turn(0, 0, t[[a]], o[[a]], Lc, 0)
    }
  } else {
    keep_b <- !turn(0, 0, t[[b]], o[[b]], Lc, 0)
  }
  df_rows(wp, c(a, b)[c(keep_a, keep_b)])
}

xy <- function(p) {
  if (is.numeric(p)) {
    return(p)
  }
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
  px <- P[, 1]
  py <- P[, 2]
  seg <- sqrt((px[-1] - px[-n])^2 + (py[-1] - py[-n])^2)
  dt <- pmax(seg^alpha, 1e-9)
  t <- cumsum(c(0, dt))

  Dx <- numeric(n)
  Dy <- numeric(n)
  if (n > 2) {
    i <- 2:(n - 1)
    Dx[i] <- (px[i] - px[i - 1]) /
      (t[i] - t[i - 1]) -
      (px[i + 1] - px[i - 1]) / (t[i + 1] - t[i - 1]) +
      (px[i + 1] - px[i]) / (t[i + 1] - t[i])
    Dy[i] <- (py[i] - py[i - 1]) /
      (t[i] - t[i - 1]) -
      (py[i + 1] - py[i - 1]) / (t[i + 1] - t[i - 1]) +
      (py[i + 1] - py[i]) / (t[i + 1] - t[i])
  }

  B <- vector("list", n - 1)
  for (i in seq_len(n - 1)) {
    B[[i]] <- matrix(
      c(
        px[[i]],
        px[[i]] + dt[[i]] * Dx[[i]] / 3,
        px[[i + 1]] - dt[[i]] * Dx[[i + 1]] / 3,
        px[[i + 1]],
        py[[i]],
        py[[i]] + dt[[i]] * Dy[[i]] / 3,
        py[[i + 1]] - dt[[i]] * Dy[[i + 1]] / 3,
        py[[i + 1]]
      ),
      4,
      2
    )
  }

  a_s <- max(
    sqrt((dt[[1]] * Dx[[1]] / 3)^2 + (dt[[1]] * Dy[[1]] / 3)^2),
    min(arm_min[[1]], arm_fraction * seg[[1]])
  )
  a_e <- max(
    sqrt((dt[[n - 1]] * Dx[[n]] / 3)^2 + (dt[[n - 1]] * Dy[[n]] / 3)^2),
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
  K <- length(B)
  cp <- matrix(unlist(B, use.names = FALSE), nrow = 8L)
  len <- sqrt((cp[2, ] - cp[1, ])^2 + (cp[6, ] - cp[5, ])^2) +
    sqrt((cp[3, ] - cp[2, ])^2 + (cp[7, ] - cp[6, ])^2) +
    sqrt((cp[4, ] - cp[3, ])^2 + (cp[8, ] - cp[7, ])^2)
  n <- pmax(min_n, ceiling(len / spacing))
  # every segment is sampled from its start, and the shared join is kept
  # once: segments after the first skip their first sample
  first <- c(0L, rep(1L, K - 1L))
  count <- n - first
  seg <- rep.int(seq_len(K), count)
  t <- (sequence(count, from = first + 1L) - 1L) / (n[seg] - 1L)
  mt <- 1 - t
  b0 <- mt^3
  b1 <- 3 * mt^2 * t
  b2 <- 3 * mt * t^2
  b3 <- t^3
  x <- b0 * cp[1, seg] + b1 * cp[2, seg] + b2 * cp[3, seg] + b3 * cp[4, seg]
  y <- b0 * cp[5, seg] + b1 * cp[6, seg] + b2 * cp[7, seg] + b3 * cp[8, seg]
  ends <- cumsum(count)
  x[ends] <- cp[4, ]
  y[ends] <- cp[8, ]
  x[[1]] <- cp[[1, 1]]
  y[[1]] <- cp[[5, 1]]
  df_cols(x = x, y = y)
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
  sx <- samples$x
  sy <- samples$y
  j <- integer(length(cand))
  min_d <- numeric(length(cand))
  for (k in seq_along(cand)) {
    d2 <- (sx - obstacles$x[[cand[[k]]]])^2 + (sy - obstacles$y[[cand[[k]]]])^2
    j[[k]] <- which.min(d2)
    min_d[[k]] <- sqrt(d2[[j[[k]]]])
  }
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
#' each waypoint stays at its layer's x. Waypoints that share a y with a
#' moved one form a level plateau and move with it, so a levelled arch is
#' lifted whole rather than tilted. In the free tier the nearest waypoint
#' within the window moves straight away from the node, and a new waypoint
#' is inserted at clearance `R` when none is near.
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
  # deepest first; equal depths are ordered by the obstacle's position and
  # then its row, never by its name
  if (nrow(viol) > 1) {
    viol <- df_rows(
      viol,
      order(
        -viol$depth,
        obstacles$x[viol$obstacle],
        obstacles$y[viol$obstacle],
        viol$obstacle,
        method = "radix"
      )
    )
  }
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
        level <- logical(nrow(wp))
        for (j in near) {
          level <- level | abs(wp$y - wp$y[[j]]) < 1e-9
        }
        near <- which(level)
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
    P <- cbind(
      c(fr$S[[1]], wp$x, fr$E[[1]]),
      c(fr$S[[2]], wp$y, fr$E[[2]])
    )
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

#' The placed set: every edge as drawn so far
#'
#' Straight edges are kept as one chord each in parallel vectors, so a
#' candidate chain is tested against all of them at once; an edge drawn as
#' a polyline (a routed edge, or a user-curved one) is kept as a
#' `placed_polyline()` instead and tested on its own.
#'
#' @param paths The current paths, one `data.frame(x, y)` per edge.
#' @noRd
placed_set <- function(paths) {
  xs <- lapply(paths, `[[`, "x")
  ys <- lapply(paths, `[[`, "y")
  chord <- lengths(xs) == 2L
  last <- function(v) v[[length(v)]]
  poly <- vector("list", length(paths))
  for (e in which(!chord)) {
    poly[[e]] <- placed_polyline(xs[[e]], ys[[e]])
  }
  list(
    chord = chord,
    ax = vapply(xs, `[[`, numeric(1), 1L),
    ay = vapply(ys, `[[`, numeric(1), 1L),
    bx = vapply(xs, last, numeric(1)),
    by = vapply(ys, last, numeric(1)),
    poly = poly
  )
}

#' Replace one edge of the placed set with its drawn polyline
#' @noRd
place_edge <- function(placed, e, x, y) {
  placed$chord[[e]] <- FALSE
  placed$poly[[e]] <- placed_polyline(x, y)
  placed
}

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
    box = c(min(x), max(x), min(y), max(y)),
    n = n - 1L
  )
}

#' Count proper crossings between a waypoint chain and a set of segments
#'
#' Segment pairs whose endpoints straddle each other's line; touching at a
#' shared endpoint does not count.
#'
#' @param pa Two-column matrix of chain points.
#' @param cx,cy,dx,dy Endpoints of the segments.
#' @noRd
count_segment_crossings <- function(pa, cx, cy, dx, dy) {
  na <- nrow(pa) - 1L
  m <- length(cx)
  if (na < 1L || m == 0L) {
    return(0L)
  }
  # every (chain segment, segment) pair at once
  i <- seq_len(na)
  ax <- rep(pa[i, 1L], each = m)
  ay <- rep(pa[i, 2L], each = m)
  bx <- rep(pa[i + 1L, 1L], each = m)
  by <- rep(pa[i + 1L, 2L], each = m)
  cx <- rep.int(cx, na)
  cy <- rep.int(cy, na)
  dx <- rep.int(dx, na)
  dy <- rep.int(dy, na)
  d1 <- (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
  d2 <- (bx - ax) * (dy - ay) - (by - ay) * (dx - ax)
  d3 <- (dx - cx) * (ay - cy) - (dy - cy) * (ax - cx)
  d4 <- (dx - cx) * (by - cy) - (dy - cy) * (bx - cx)
  sum(d1 * d2 < 0 & d3 * d4 < 0)
}

#' Count proper crossings between a waypoint chain and a placed polyline
#'
#' Each segment of the chain is tested only against the segments of the
#' polyline whose bounding boxes overlap it; a chain whose bounding box
#' misses the polyline's is not tested at all.
#'
#' @param pa Two-column matrix of chain points.
#' @param pb A polyline from `placed_polyline()`.
#' @noRd
count_polyline_crossings <- function(pa, pb) {
  na <- nrow(pa) - 1L
  if (na < 1 || pb$n < 1) {
    return(0L)
  }
  box <- pb$box
  if (
    max(pa[, 1]) < box[[1]] ||
      min(pa[, 1]) > box[[2]] ||
      max(pa[, 2]) < box[[3]] ||
      min(pa[, 2]) > box[[4]]
  ) {
    return(0L)
  }
  total <- 0L
  for (i in seq_len(na)) {
    ax <- pa[[i, 1]]
    ay <- pa[[i, 2]]
    bx <- pa[[i + 1L, 1]]
    by <- pa[[i + 1L, 2]]
    x_lo <- min(ax, bx)
    x_hi <- max(ax, bx)
    y_lo <- min(ay, by)
    y_hi <- max(ay, by)
    if (
      x_hi < box[[1]] || x_lo > box[[2]] || y_hi < box[[3]] || y_lo > box[[4]]
    ) {
      next
    }
    near <- which(
      pb$xmax >= x_lo & pb$xmin <= x_hi & pb$ymax >= y_lo & pb$ymin <= y_hi
    )
    if (length(near) == 0) {
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
  others <- ectx$others
  chords <- others[placed$chord[others]]
  crossings <- count_segment_crossings(
    poly,
    placed$ax[chords],
    placed$ay[chords],
    placed$bx[chords],
    placed$by[chords]
  )
  for (o in others[!placed$chord[others]]) {
    crossings <- crossings + count_polyline_crossings(poly, placed$poly[[o]])
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
#' A periphery slot also keeps the clearance margin `m` from the panel
#' bounds (`periphery_intervals()`), so a periphery arch keeps `m` from the
#' border; when no outer slot on a side satisfies that, the periphery
#' candidate on that side is infeasible. Candidates that share a slot with
#' an already routed edge are spread by `sep_e` in chord order and reduced
#' to one arch by the hull; a candidate that had to stop on an occupant
#' ranks below every other, and one that had to give up the chord order is
#' priced as if it crossed one edge. A periphery
#' arch is then levelled: every surviving waypoint rises to the outermost
#' one, so the apex sits mid-span rather than over the tallest stack and the
#' arch climbs as steeply as it descends; the levelled chain is spread and
#' hulled again. Candidates are priced by `side_cost()` on the displacement
#' the stacks require, measured before levelling, and on the crossings of
#' the chain as drawn.
#'
#' @param bounds Panel bounds.
#' @param reserved Reservations for this edge from `slot_reservations()`,
#'   or `NULL`.
#' @param base The layers' base free intervals that the occupancy is
#'   indexed by; `ints` unless the edge carries an extra margin.
#' @return `NULL` when every candidate has a slot with no free y; otherwise
#'   the candidates ranked best first, each a list with `wp`, `side`,
#'   `scope`, `disordered`, `overlap`, and `ints`, the free intervals the
#'   candidate was placed in (clipped for a periphery candidate).
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
  opts,
  bounds,
  reserved = NULL,
  base = ints
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

  pints <- if ("periphery" %in% scopes) {
    lapply(ints, periphery_intervals, bounds = bounds, margin = opts$m)
  }

  cands <- list()
  for (side in c(1, -1)) {
    for (scope in scopes) {
      periphery <- scope == "periphery"
      use <- if (periphery) pints else ints
      yk <- vapply(
        seq_along(crossed),
        function(i) nearest_free_y(use[[i]], yc[[i]], side, periphery),
        numeric(1)
      )
      if (anyNA(yk)) {
        next
      }
      wp <- df_cols(x = layers$x[crossed], y = yk, layer = crossed)
      sp <- spread_in_slot(wp, use, occ, side, opts$sep_e, yc, reserved, base)
      overlap <- sp$overlap
      disordered <- sp$disordered
      wp <- hull_waypoints(fr$S, sp$wp, fr$E, side)
      # the displacement priced is what the stacks require; levelling a
      # periphery arch shapes it without changing the slots it needs
      at <- match(wp$layer, crossed)
      displacement <- sum(abs(wp$y - yc[at]))
      if (periphery && nrow(wp) > 1) {
        wp$y <- if (side > 0) max(wp$y) else min(wp$y)
        sp <- spread_in_slot(
          wp,
          use[at],
          occ,
          side,
          opts$sep_e,
          yc[at],
          reserved,
          base[at]
        )
        overlap <- overlap || sp$overlap
        disordered <- disordered || sp$disordered
        wp <- hull_waypoints(fr$S, sp$wp, fr$E, side)
      }
      cost <- side_cost(fr, wp, side, displacement, ectx, placed, opts)
      if (disordered) {
        cost <- cost + opts$crossing_penalty
      }
      cands[[length(cands) + 1]] <- list(
        side = side,
        scope = scope,
        wp = wp,
        cost = round(cost, opts$cost_digits),
        disordered = disordered,
        overlap = overlap,
        ints = use
      )
    }
  }
  if (length(cands) == 0) {
    return(NULL)
  }
  cost <- vapply(cands, function(c) c$cost, numeric(1))
  overlap <- vapply(cands, function(c) c$overlap, logical(1))
  interior <- vapply(cands, function(c) c$scope == "interior", logical(1))
  above <- vapply(cands, function(c) c$side > 0, logical(1))
  cands[order(overlap, cost, !interior, !above)]
}

#' Whether every waypoint at a crossed layer lies in one of its intervals
#'
#' A repair or a re-spread can move a periphery waypoint back into the
#' margin its slot was clipped away from; such a candidate is infeasible.
#'
#' @noRd
waypoints_in_slots <- function(wp, crossed, ints) {
  for (i in seq_along(crossed)) {
    at <- which(wp$layer == crossed[[i]])
    if (length(at) == 0) {
      next
    }
    iv <- ints[[i]]
    for (y in wp$y[at]) {
      if (!any(iv$lo <= y & y <= iv$hi, na.rm = TRUE)) {
        return(FALSE)
      }
    }
  }
  TRUE
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
  tc <- hits$t
  tc[tc < opts$t_clamp[[1]]] <- opts$t_clamp[[1]]
  tc[tc > opts$t_clamp[[2]]] <- opts$t_clamp[[2]]
  df_cols(
    x = fr$S[[1]] + tc * fr$Lc * fr$u[[1]] + offsets * fr$n[[1]],
    y = fr$S[[2]] + tc * fr$Lc * fr$u[[2]] + offsets * fr$n[[2]],
    layer = hits$layer
  )
}

#' Whether a sampled path keeps `margin` from every panel bound
#'
#' The endpoints are node centres and are not the path's to move, so only
#' the interior samples are checked.
#'
#' @noRd
path_inside_bounds <- function(path, bounds, margin) {
  n <- length(path$x)
  if (n <= 2) {
    return(TRUE)
  }
  x <- path$x[-c(1L, n)]
  y <- path$y[-c(1L, n)]
  min(x) >= bounds[[1]] + margin &&
    max(x) <= bounds[[3]] - margin &&
    min(y) >= bounds[[2]] + margin &&
    max(y) <= bounds[[4]] - margin
}

#' Clamp the interior samples of a path to `margin` inside the bounds
#' @noRd
clamp_path <- function(path, bounds, margin) {
  n <- length(path$x)
  if (n <= 2) {
    return(path)
  }
  i <- 2:(n - 1L)
  path$x[i] <- pmin(pmax(path$x[i], bounds[[1]] + margin), bounds[[3]] - margin)
  path$y[i] <- pmin(pmax(path$y[i], bounds[[2]] + margin), bounds[[4]] - margin)
  path
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
  attr(cols, "row.names") <- c(NA_integer_, -length(cols[[1]]))
  class(cols) <- "data.frame"
  cols
}

#' Row subset and row concatenation for the light frames above
#' @noRd
df_rows <- function(df, idx) {
  cols <- unclass(df)
  for (j in seq_along(cols)) {
    cols[[j]] <- cols[[j]][idx]
  }
  new_df(cols)
}

df_bind <- function(a, b) {
  cols <- unclass(a)
  for (nm in names(cols)) {
    cols[[nm]] <- c(cols[[nm]], b[[nm]])
  }
  new_df(cols)
}

empty_waypoints <- function() {
  df_cols(x = numeric(0), y = numeric(0), layer = integer(0))
}

empty_occupancy <- function() {
  df_cols(
    edge = integer(0),
    layer = integer(0),
    slot = integer(0),
    y = numeric(0),
    chord = numeric(0),
    side = numeric(0),
    base = numeric(0),
    lo = numeric(0),
    hi = numeric(0)
  )
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
  # the waypoints stay inside the panel and the drawn curve keeps the
  # clearance margin from its bounds: a levelled plateau bulges a few
  # millimetres beyond its waypoints
  res$inside <- inside_bounds(res$wp, job$bounds, job$opts$pad) &&
    path_inside_bounds(res$path, job$bounds, job$opts$m)
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
  res <- route_candidate(
    job,
    fb$wp,
    fb$side,
    fb$mode,
    "free",
    placed,
    capped = fb$capped,
    least_bad = fb$least_bad
  )
  res$cost <- fb$cost
  res
}

#' Route one ranked spanning candidate of an edge
#'
#' Merges the bows around endpoint-layer hits into the candidate's arch,
#' builds and verifies the curve, spreads a repaired arch again against the
#' occupancy (twice at most, reporting a persisting conflict as lost
#' clearance), and marks a periphery waypoint that repairs moved back into
#' the margin clipped from its slot as infeasible. The re-spread waypoints
#' carry the parallel-group offset already, so it is taken off before the
#' candidate applies it again.
#'
#' @param cand One candidate from `assign_spanning_waypoints()`.
#' @param eh The edge's hits in its frame.
#' @param crossed The crossed layers.
#' @param base The layers' base free intervals, one per crossed layer, that
#'   the occupancy is indexed by.
#' @noRd
route_spanning_candidate <- function(
  job,
  cand,
  eh,
  crossed,
  layers,
  base,
  occ,
  placed,
  shift,
  opts
) {
  fr <- job$fr
  wp <- cand$wp
  outside <- df_rows(eh, which(eh$layer <= job$la | eh$layer >= job$lb))
  if (nrow(outside) > 0) {
    o <- outside$h + cand$side * (outside$r + opts$m + job$extra)
    wp <- hull_waypoints(
      fr$S,
      df_bind(wp, bow_points(outside, fr, o, opts)),
      fr$E,
      cand$side
    )
  }
  res <- route_candidate(job, wp, cand$side, cand$scope, "spanning", placed)
  # a waypoint that stopped on an occupant is drawn over another edge,
  # which no verification against the discs can see
  res$clearance_ok <- res$clearance_ok && !cand$overlap
  use <- cand$ints
  for (round in 1:2) {
    wp2 <- respread_arch(res, fr, crossed, layers, use, occ, opts, base)
    if (is.null(wp2)) {
      break
    }
    res <- route_candidate(
      job,
      offset_parallel_edges(wp2, -shift, fr, "spanning"),
      cand$side,
      cand$scope,
      "spanning",
      placed
    )
    res$clearance_ok <- res$clearance_ok && !cand$overlap
  }
  if (
    res$clearance_ok &&
      !is.null(respread_arch(res, fr, crossed, layers, use, occ, opts, base))
  ) {
    res$clearance_ok <- FALSE
  }
  if (cand$scope == "periphery" && !waypoints_in_slots(res$wp, crossed, use)) {
    res$inside <- FALSE
  }
  res
}

# Parallel groups ------------------------------------------------------------------------------

#' Spread the members of parallel-edge groups
#'
#' Members share an unordered node pair. Each member of a group of `k` is
#' routed with an extra clearance of `sep_m * (k - 1) / 2` and translated by
#' `sep_m * (i - (k + 1) / 2)`, in the canonical order of its endpoint names.
#'
#' @return A list with `extra` and `shift`, one value per edge.
#' @noRd
parallel_groups <- function(from, to, from_name, to_name, routable, sep_m) {
  n_edges <- length(from)
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
    extra[members] <- sep_m * (size - 1) / 2
    shift[members] <- sep_m * (seq_len(size) - (size + 1) / 2)
  }
  list(extra = extra, shift = shift)
}

# Orthogonal mode ------------------------------------------------------------------------------

#' Route a canonically oriented scene with axis-aligned runs
#'
#' Every routable edge that needs a bend to be axis-aligned is drawn
#' orthogonally whether or not a node blocks its chord. A chord that is
#' already axis-aligned stays a two-row straight path: a vertical chord, a
#' horizontal chord between adjacent layers, and a horizontal spanning chord
#' that no non-endpoint disc comes within `R` of. A chord between two
#' nodes of one layer stays straight as well, since the layer has no gap to
#' route it through.
#'
#' A spanning edge chooses among the channels `ortho_channel()` prices: an
#' S or N channel `R` beyond the crossed stacks when both endpoints are the
#' extreme node of their layer on that side (two bends), an E/W run at an
#' endpoint's y or `R` beyond the stacks (two or four bends), and, when no
#' S/N channel fits, an E/W run through a free interval of every crossed
#' layer. Every other edge leaves through the E port and enters through the
#' W port with one vertical run per crossed gap at a slot assigned by
#' `ortho_slot_ranks()`. Channels are placed shortest span first, and one
#' that would run within `sep_e` of a placed channel over an overlapping
#' x-range is pushed outward past it, so longer edges nest outside shorter
#' ones and no channel is shared.
#'
#' Every drawn segment belongs to one edge unless two edges share a port.
#' Edges leaving one port form a hyperedge trunk and edges entering one port
#' merge into their last run; two segments from different sources never
#' share a slot, even when their y-intervals only meet; a node whose N (or
#' S) side carries both an arrival and a departure gives them ports
#' `sep_e / 2` on either side of its centre line, the departure toward the
#' target, joined to the centre by a connector hidden inside the disc; and a
#' pair of horizontal pieces in one gap that would coincide in one slot
#' order forces the other order.
#'
#' Duplicates of a straight chord take a rectangular detour so the parallel
#' spread has something to translate.
#'
#' @return The per-edge state of `route_scene_mm()` as a list.
#' @noRd
route_orthogonal_scene <- function(
  nodes,
  from,
  to,
  from_name,
  to_name,
  is_fixed,
  paths,
  bounds,
  cap,
  opts,
  layers
) {
  n_edges <- length(from)
  waypoints <- rep(list(empty_waypoints()), n_edges)
  routed <- logical(n_edges)
  mode_out <- rep("straight", n_edges)
  mode_out[is_fixed] <- "fixed"
  side_out <- rep(NA_real_, n_edges)
  n_wp <- integer(n_edges)
  wp_layers <- rep(list(integer(0)), n_edges)
  clearance_ok <- rep(TRUE, n_edges)
  sagitta <- numeric(n_edges)
  capped <- logical(n_edges)

  info <- edge_span_info(nodes, from, to, layers)
  routable <- !is_fixed & info$Lc > 0 & from != to
  R_node <- nodes$r + opts$m
  rc <- opts$rc
  stub <- opts$r_ref + cap + rc
  # the resect is measured from the node centre, so a bend vertex needs only
  # the cap, the arrowhead or the corner arc, and the corner's tangent length
  # past the centre; the nominal stub carries the radius as slack
  stub_min <- cap + max(opts$head, rc) + rc
  tol <- 1e-3

  # canonical endpoints: the source is the left (or lower) node
  a <- ifelse(info$reversed, to, from)
  b <- ifelse(info$reversed, from, to)
  Sx <- nodes$x[a]
  Sy <- nodes$y[a]
  Tx <- nodes$x[b]
  Ty <- nodes$y[b]
  span <- info$span
  vertical <- abs(Tx - Sx) < tol
  # a chord tilted by no more than the corner radius is level: a jog shorter
  # than rc cannot show two proper corners and reads as a wobble, while the
  # tilt over a gap of at least a stub is a few degrees at most
  horizontal <- abs(Ty - Sy) <= rc + 1e-9

  # a node is extreme on a side when nothing in its layer lies beyond it
  layer_hi <- vapply(layers$members, function(m) max(nodes$y[m]), numeric(1))
  layer_lo <- vapply(layers$members, function(m) min(nodes$y[m]), numeric(1))
  at_top <- nodes$y >= layer_hi[layers$id] - 1e-9
  at_bottom <- nodes$y <= layer_lo[layers$id] + 1e-9

  grp <- parallel_groups(from, to, from_name, to_name, routable, opts$sep_m)
  shift <- grp$shift
  extra <- grp$extra

  # a chord needs a bend when it is oblique and leaves its layer, or when it
  # is horizontal, spans a layer, and a disc in a crossed layer blocks it; a
  # horizontal chord that hits nothing is already axis-aligned
  bent <- routable & span >= 1 & !vertical & !(horizontal & span <= 1)
  level <- which(bent & horizontal)
  if (length(level) > 0) {
    hits <- find_blocked_edges(
      nodes,
      df_cols(from = nodes$name[a[level]], to = nodes$name[b[level]]),
      nodes$r + opts$m_min,
      R_node
    )
    bent[level[!seq_along(level) %in% hits$edge]] <- FALSE
  }
  kind <- rep("straight", n_edges)
  kind[bent] <- "ew"
  kind[
    routable & kind == "straight" & (horizontal | vertical) & shift != 0
  ] <- "detour"

  # the horizontal pieces committed in every gap so far: an edge with a
  # vertical run in a gap enters it at one y and leaves at another; pieces
  # leaving one source port belong to one hyperedge segment
  n_gaps <- max(layers$n - 1L, 0L)
  pieces <- rep(list(empty_pieces()), n_gaps)
  seg_key <- function(e, first) {
    if (first) paste0("s", a[[e]]) else paste0("e", a[[e]], "-", b[[e]])
  }
  add_piece <- function(pieces, g, key, left, right) {
    if (abs(left - right) < tol) {
      return(pieces)
    }
    pieces[[g]] <- df_bind(
      pieces[[g]],
      df_cols(key = key, left = left, right = right)
    )
    pieces
  }
  for (e in which(kind == "ew" & span == 1L)) {
    pieces <- add_piece(
      pieces,
      info$la[[e]],
      seg_key(e, TRUE),
      Sy[[e]],
      Ty[[e]]
    )
  }
  intervals <- lapply(seq_len(layers$n), function(k) {
    layer_free_intervals(
      df_rows(nodes, layers$members[[k]]),
      opts$m,
      bounds,
      opts$pad,
      opts$sep_e
    )
  })

  # channels of spanning edges, shortest first so that a longer edge nests
  # outside the channels already placed, priced against the chords of the
  # edges not yet placed and the channels of those already placed; ties on
  # span and length are broken by the endpoint positions, never by name
  side <- rep(NA_real_, n_edges)
  y_ch <- rep(NA_real_, n_edges)
  clamped <- logical(n_edges)
  placed <- placed_set(paths)
  channels <- df_cols(
    side = numeric(0),
    y = numeric(0),
    lo = numeric(0),
    hi = numeric(0)
  )
  ctx <- list(nodes = nodes, from = from, to = to, Lc = info$Lc)
  gap_mid <- (layers$x[-layers$n] + layers$x[-1]) / 2

  spanning <- which(kind == "ew" & span >= 2)
  spanning <- spanning[order(
    span[spanning],
    info$Lc[spanning],
    Sx[spanning],
    Sy[spanning],
    Tx[spanning],
    Ty[spanning],
    spanning,
    method = "radix"
  )]
  for (e in spanning) {
    fr <- edge_frame(nodes, from[[e]], to[[e]], info$reversed[[e]])
    ch <- ortho_channel(
      fr,
      edge_cost_context(fr, e, ctx),
      info$la[[e]],
      info$lb[[e]],
      layers,
      nodes,
      R_node,
      extra[[e]],
      c(stub, stub_min),
      c(
        at_top[[a[[e]]]] && at_top[[b[[e]]]],
        at_bottom[[a[[e]]]] && at_bottom[[b[[e]]]]
      ),
      gap_mid,
      bounds,
      placed,
      channels,
      intervals,
      pieces,
      c(seg_key(e, TRUE), seg_key(e, FALSE)),
      opts
    )
    kind[[e]] <- ch$kind
    side[[e]] <- ch$side
    y_ch[[e]] <- ch$y
    clamped[[e]] <- ch$clamped
    channels <- df_bind(channels, ch$channel)
    placed <- place_edge(
      placed,
      e,
      c(fr$S[[1]], ch$wp$x, fr$E[[1]]),
      c(fr$S[[2]], ch$wp$y, fr$E[[2]])
    )
    if (ch$kind == "ew") {
      pieces <- add_piece(pieces, info$la[[e]], seg_key(e, TRUE), Sy[[e]], ch$y)
      pieces <- add_piece(
        pieces,
        info$lb[[e]] - 1L,
        seg_key(e, FALSE),
        ch$y,
        Ty[[e]]
      )
    }
  }

  # N and S ports: two channel stubs on one side of a node sit sep_e / 2
  # either side of the centre line, an arrival on the left of a departure,
  # two arrivals in ascending channel y and two departures in descending
  # channel y, which is the crossing-free order. More than two share: the
  # arrivals take the left stub and the departures the right
  port_s <- numeric(n_edges)
  port_t <- numeric(n_edges)
  sn <- which(kind == "sn")
  for (n in unique(c(a[sn], b[sn]))) {
    for (s in c(1, -1)) {
      dep <- sn[a[sn] == n & side[sn] == s]
      arr <- sn[b[sn] == n & side[sn] == s]
      if (length(dep) + length(arr) < 2) {
        next
      }
      half <- opts$sep_e / 2
      if (length(dep) + length(arr) == 2) {
        dep <- dep[order(-y_ch[dep], Sy[dep], b[dep], method = "radix")]
        arr <- arr[order(y_ch[arr], Sy[arr], a[arr], method = "radix")]
        stack <- c(-half, half)
        port_t[arr] <- stack[seq_along(arr)]
        port_s[dep] <- stack[length(arr) + seq_along(dep)]
      } else if (length(dep) > 0 && length(arr) > 0) {
        port_s[dep] <- half
        port_t[arr] <- -half
      }
    }
  }

  # one slot per hyperedge segment in every gap, on the rung of the ladder
  # the gap reaches; the scene draws every corner at the smallest radius
  # any gap needed
  slot_first <- rep(NA_real_, n_edges)
  slot_last <- rep(NA_real_, n_edges)
  narrow <- logical(n_edges)
  ladder <- list()
  rc_used <- rc
  ew <- which(kind == "ew")
  for (g in seq_len(n_gaps)) {
    first <- ew[info$la[ew] == g]
    last <- ew[span[ew] >= 2 & info$lb[ew] - 1L == g]
    if (length(first) + length(last) == 0) {
      next
    }
    segs <- ortho_gap_segments(
      first,
      last,
      a,
      b,
      Sy,
      Ty,
      y_ch,
      span,
      nodes,
      tol
    )
    pos <- ortho_slot_positions(
      segs,
      c(layers$x[[g]], layers$x[[g + 1L]]),
      opts,
      cap
    )
    for (k in seq_along(segs$members)) {
      m <- segs$members[[k]]
      is_first <- segs$member_first[[k]]
      slot_first[m[is_first]] <- pos$x[[k]]
      slot_last[m[!is_first]] <- pos$x[[k]]
      if (pos$narrow) {
        narrow[m] <- TRUE
      }
    }
    if (!is.null(pos$ladder)) {
      ladder[[length(ladder) + 1L]] <- new_df(c(
        list(gap = g),
        unclass(pos$ladder)
      ))
      rc_used <- min(rc_used, pos$rc)
    }
  }
  ladder <- Reduce(
    df_bind,
    ladder,
    df_cols(
      gap = integer(0),
      width = numeric(0),
      ranks = integer(0),
      rung = integer(0),
      stub = numeric(0),
      spacing = numeric(0)
    )
  )

  # W ports: the level chords into a node, straight or running at the
  # node's own line, own its centre row. The other arrivals take rows above
  # and below it in the order of their slots, the leftmost slot nearest the
  # centre, which is the crossing-free order; without a level chord the
  # larger group's first member takes the centre. A group stacks only while
  # two rows hold it; a larger group merges onto one row of its own at its
  # first offset, and the ladder keeps its joins clear of the stub. The
  # copies of a parallel bundle are spread sep_m apart already, and an
  # arrival through a gap too narrow for any stub has no room for a row,
  # so both keep the centre row
  port_y <- numeric(n_edges)
  via_last <- span >= 2
  arrival_slot <- ifelse(via_last, slot_last, slot_first)
  arrival_entry <- ifelse(via_last, y_ch, Sy)
  arrival <- kind == "ew" & !is.na(arrival_slot) & shift == 0 & !narrow
  owner <- (kind == "straight" & routable & horizontal & !vertical) |
    (kind == "ew" & via_last & is.na(slot_last))
  for (t in unique(b[arrival])) {
    idx <- which(arrival & b == t)
    idx <- idx[order(arrival_slot[idx], Sy[idx], a[idx], method = "radix")]
    above <- idx[arrival_entry[idx] > Ty[idx]]
    below <- idx[arrival_entry[idx] <= Ty[idx]]
    has0 <- any(owner & b == t)
    ka <- length(above)
    kb <- length(below)
    a0 <- !has0 && ka > 0 && ka >= kb
    b0 <- !has0 && kb > ka
    mult_a <- seq_len(ka) - a0
    mult_b <- seq_len(kb) - b0
    if (ka > 0 && max(mult_a) > 2) {
      mult_a <- rep(min(mult_a), ka)
    }
    if (kb > 0 && max(mult_b) > 2) {
      mult_b <- rep(min(mult_b), kb)
    }
    k <- max(mult_a, mult_b, 0)
    if (k == 0) {
      next
    }
    s <- min(opts$sep_e, (nodes$r[[t]] - opts$head_w / 2) / k)
    port_y[above] <- mult_a * s
    port_y[below] <- -mult_b * s
  }

  # polylines: centre, port, bends, port, centre; then corners and sampling
  resect_head <- rep(cap, n_edges)
  resect_fins <- rep(cap, n_edges)
  for (e in which(kind != "straight" & !is_fixed)) {
    fr <- edge_frame(nodes, from[[e]], to[[e]], info$reversed[[e]])
    geom <- ortho_bends(
      kind[[e]],
      fr$S,
      fr$E,
      nodes$r[[a[[e]]]],
      nodes$r[[b[[e]]]],
      side[[e]],
      y_ch[[e]],
      slot_first[[e]],
      slot_last[[e]],
      span[[e]],
      shift[[e]],
      fr$u,
      stub,
      port_s[[e]],
      port_t[[e]],
      port_y[[e]],
      tol
    )
    if (is.null(geom$bends)) {
      next
    }
    # the bends are the turns between the ports; the connector from an
    # offset port to the centre is a corner hidden inside the disc, drawn
    # and rounded but not reported as a bend
    core <- drop_collinear(dedupe_points(rbind(
      geom$port_s %||% fr$S,
      geom$bends,
      geom$port_t %||% fr$E
    )))
    bends <- core[-c(1L, nrow(core)), , drop = FALSE]
    if (nrow(bends) == 0) {
      next
    }
    poly <- drop_collinear(dedupe_points(rbind(fr$S, core, fr$E)))
    # the foot of a port row on the disc boundary is a sharp corner: the
    # row runs to the boundary and the connector inside the disc is never
    # drawn, so the arc the resect measures is the row plus the radius
    foot <- if (geom$axis_t == 1L && geom$off_t != 0 && !is.null(geom$port_t)) {
      nrow(poly) - 1L
    }
    pts <- if (opts$corners == "rounded") {
      round_corners(poly, rc_used, sharp = foot %||% integer(0))
    } else {
      poly
    }
    pts <- dedupe_points(sample_runs(pts, opts$sample_spacing))

    # the resect at an offset port: the arc from the centre to the cap line
    # on the port's axis, through the hidden connector and its corner
    at_s <- port_resect(
      pts[rev(seq_len(nrow(pts))), , drop = FALSE],
      fr$S,
      geom$axis_s,
      cap,
      geom$limit_s + rc_used,
      geom$off_s
    )
    at_e <- port_resect(
      pts,
      fr$E,
      geom$axis_t,
      cap,
      geom$limit_t + rc_used,
      geom$off_t
    )
    resect_head[[e]] <- if (info$reversed[[e]]) at_s else at_e
    resect_fins[[e]] <- if (info$reversed[[e]]) at_e else at_s

    others <- setdiff(seq_len(nrow(nodes)), c(a[[e]], b[[e]]))
    ok <- !narrow[[e]] && !clamped[[e]]
    if (ok && length(others) > 0) {
      d <- polyline_min_dist(pts, nodes$x[others], nodes$y[others])
      ok <- all(d >= R_node[others] - opts$verify_tol)
    }

    path <- df_cols(x = pts[, 1], y = pts[, 2])
    # the drawn runs keep the clearance margin from the panel bounds
    if (!path_inside_bounds(path, bounds, opts$m)) {
      path <- clamp_path(path, bounds, opts$m)
      ok <- FALSE
    }
    if (info$reversed[[e]]) {
      path <- df_cols(x = rev(path$x), y = rev(path$y))
    }
    paths[[e]] <- path
    waypoints[[e]] <- df_cols(
      x = bends[, 1],
      y = bends[, 2],
      layer = rep(NA_integer_, nrow(bends))
    )
    routed[[e]] <- TRUE
    mode_out[[e]] <- "orthogonal"
    side_out[[e]] <- geom$side
    n_wp[[e]] <- nrow(bends)
    wp_layers[[e]] <- rep(NA_integer_, nrow(bends))
    clearance_ok[[e]] <- ok
    sagitta[[e]] <- NA_real_
    capped[[e]] <- NA
  }

  list(
    paths = paths,
    waypoints = waypoints,
    routed = routed,
    mode = mode_out,
    side = side_out,
    n_waypoints = n_wp,
    waypoint_layers = wp_layers,
    clearance_ok = clearance_ok,
    sagitta_ratio = sagitta,
    sagitta_capped = capped,
    resect_head = resect_head,
    resect_fins = resect_fins,
    ortho = list(rc = rc_used, gaps = ladder)
  )
}

empty_pieces <- function() {
  df_cols(key = character(0), left = numeric(0), right = numeric(0))
}

#' Choose the channel of a spanning edge
#'
#' Every candidate is priced by `side_cost()` with the displacement summed
#' over the crossed layers, plus `bend_penalty` per bend; the cheapest wins
#' and ties go above. S/N candidates exist for each side on which both
#' endpoints are extreme; their channel sits at `max(extreme_y + R, S_y +
#' stub, T_y + stub)` above (mirrored below), so the stubs always hold the
#' arrowhead, and it has two bends. A channel that leaves the panel margin
#' at the nominal stub is tried again at the stub floor. E/W candidates run
#' at the source's own line and at the target's own line (two bends each, on
#' the side of the chord that line lies on) and at `extreme_y + R` beyond
#' the crossed stacks (mirrored below, four bends). When no S/N channel is
#' feasible the edge also gets an interior E/W candidate on each side: the
#' run nearest the chord that lies in a free interval of every crossed
#' layer, the orthogonal analogue of the spline's interior slot. A candidate
#' that would run within `sep_e` of an already placed channel over an
#' overlapping x-range is pushed outward past it.
#'
#' A candidate is infeasible when its run comes closer than the clearance
#' margin `m` to the panel bounds, when its margin band would cut a disc of
#' a layer it passes, or when its horizontal pieces in a gap would coincide
#' with a committed piece from another source in either slot order. When
#' nothing is feasible the least displaced candidate other than an endpoint
#' run is clamped to the margin and reported without clearance.
#'
#' @param stub The nominal stub and the stub floor, in that order.
#' @param sn_sides Logical pair: are S/N ports available above and below.
#' @param channels The channels placed so far: `side`, `y`, `lo`, `hi`.
#' @param intervals Free intervals per layer from `layer_free_intervals()`.
#' @param pieces Committed horizontal pieces per gap: `key`, `left`,
#'   `right`.
#' @param keys The edge's segment keys in its first and last gap.
#' @return A list with `kind` (`"sn"` or `"ew"`), `side`, `y`, `wp` (the
#'   bends used for pricing), `clamped`, and `channel` (the row to register).
#' @noRd
ortho_channel <- function(
  fr,
  ectx,
  la,
  lb,
  layers,
  nodes,
  R_node,
  extra,
  stub,
  sn_sides,
  gap_mid,
  bounds,
  placed,
  channels,
  intervals,
  pieces,
  keys,
  opts
) {
  crossed <- (la + 1L):(lb - 1L)
  members <- unlist(layers$members[crossed], use.names = FALSE)
  ext_hi <- max(nodes$y[members] + R_node[members]) + extra
  ext_lo <- min(nodes$y[members] - R_node[members]) - extra
  Sx <- fr$S[[1]]
  Sy <- fr$S[[2]]
  Tx <- fr$E[[1]]
  Ty <- fr$E[[2]]
  yc <- Sy + (layers$x[crossed] - Sx) / (Tx - Sx) * (Ty - Sy)
  x_a <- gap_mid[[la]]
  x_b <- gap_mid[[lb - 1L]]
  y_min <- bounds[[2]] + opts$m
  y_max <- bounds[[4]] - opts$m

  # the discs a run must keep clear of: those of the crossed layers, and
  # for a channel from stub to stub the other discs of the endpoint layers
  sn_nodes <- setdiff(
    unlist(layers$members[la:lb], use.names = FALSE),
    c(fr$a, fr$b)
  )
  clear_of <- function(y, idx) {
    all(abs(y - nodes$y[idx]) >= R_node[idx] + extra - 1e-9)
  }

  bends_of <- function(kind, y) {
    if (kind == "sn") {
      df_cols(x = c(Sx, Tx), y = c(y, y))
    } else {
      df_cols(x = c(x_a, x_a, x_b, x_b), y = c(Sy, y, y, Ty))
    }
  }
  extent_of <- function(kind) {
    if (kind == "sn") {
      c(Sx, Tx)
    } else {
      c(layers$x[[la]] + stub[[1]], layers$x[[lb]] - stub[[1]])
    }
  }
  bends_count <- function(kind, y) {
    if (kind == "sn") {
      return(2L)
    }
    P <- drop_collinear(dedupe_points(rbind(
      fr$S,
      c(x_a, Sy),
      c(x_a, y),
      c(x_b, y),
      c(x_b, Ty),
      fr$E
    )))
    nrow(P) - 2L
  }
  coincides <- function(kind, y) {
    kind == "ew" &&
      (pieces_coincide(pieces[[la]], keys[[1]], Sy, y) ||
        pieces_coincide(pieces[[lb - 1L]], keys[[2]], y, Ty))
  }
  candidate <- function(kind, side, y) {
    y <- stack_channel(y, side, extent_of(kind), channels, opts$sep_e)
    wp <- bends_of(kind, y)
    displacement <- sum(abs(y - yc))
    feasible <- y >= y_min &&
      y <= y_max &&
      clear_of(y, if (kind == "sn") sn_nodes else members) &&
      !coincides(kind, y)
    cost <- if (feasible) {
      side_cost(fr, wp, side, displacement, ectx, placed, opts) +
        opts$bend_penalty * bends_count(kind, y)
    } else {
      Inf
    }
    list(
      kind = kind,
      side = side,
      y = y,
      wp = wp,
      displacement = displacement,
      cost = round(cost, opts$cost_digits)
    )
  }
  costs <- function(cands) vapply(cands, function(c) c$cost, numeric(1))

  # an S/N channel keeps the nominal stub when its run fits inside the panel
  # margin, and otherwise retries at the stub floor before it is given up
  sn_candidate <- function(s) {
    y_at <- function(st) {
      if (s > 0) {
        max(ext_hi, Sy + st, Ty + st)
      } else {
        min(ext_lo, Sy - st, Ty - st)
      }
    }
    cand <- candidate("sn", s, y_at(stub[[1]]))
    if (!is.finite(cand$cost) && (cand$y < y_min || cand$y > y_max)) {
      cand <- candidate("sn", s, y_at(stub[[2]]))
    }
    cand
  }
  cands <- list()
  if (sn_sides[[1]]) {
    cands <- c(cands, list(sn_candidate(1)))
  }
  if (sn_sides[[2]]) {
    cands <- c(cands, list(sn_candidate(-1)))
  }
  sn_feasible <- any(is.finite(costs(cands)))
  # E/W runs at each endpoint's own line, two bends each. Such a run passes
  # the crossed stacks on one side, and that is the side it is priced on;
  # when the stacks lie on both sides of it, the side of the chord its line
  # lies on. Then the runs beyond the crossed stacks
  yc_mid <- stats::median(yc)
  at_end <- function(y) {
    s <- if (all(nodes$y[members] > y)) {
      -1
    } else if (all(nodes$y[members] < y)) {
      1
    } else if (y >= yc_mid) {
      1
    } else {
      -1
    }
    cand <- candidate("ew", s, y)
    cand$endpoint <- TRUE
    cand
  }
  cands <- c(
    cands,
    list(
      at_end(Sy),
      at_end(Ty),
      candidate("ew", 1, ext_hi),
      candidate("ew", -1, ext_lo)
    )
  )
  if (!sn_feasible) {
    for (s in c(1, -1)) {
      y <- common_free_y(intervals[crossed], yc_mid, s)
      if (!is.na(y)) {
        cands <- c(cands, list(candidate("ew", s, y)))
      }
    }
  }

  cost <- costs(cands)
  sides <- vapply(cands, function(c) c$side, numeric(1))
  ys <- vapply(cands, function(c) c$y, numeric(1))
  clamped <- FALSE
  if (any(is.finite(cost))) {
    # ties go above: to the upper side, and between two runs priced on one
    # side to the higher run
    best <- cands[[order(cost, -sides, -ys)[[1]]]]
  } else {
    # an endpoint run is never short of the margin, only blocked by a disc,
    # so it is not a candidate for clamping
    pool <- !vapply(cands, function(c) isTRUE(c$endpoint), logical(1))
    cands <- cands[pool]
    sides <- sides[pool]
    displacement <- vapply(cands, function(c) c$displacement, numeric(1))
    best <- cands[[order(displacement, -sides)[[1]]]]
    best$y <- min(max(best$y, y_min), y_max)
    best$wp <- bends_of(best$kind, best$y)
    clamped <- TRUE
  }
  xr <- extent_of(best$kind)
  list(
    kind = best$kind,
    side = best$side,
    y = best$y,
    wp = best$wp,
    clamped = clamped,
    channel = df_cols(side = best$side, y = best$y, lo = xr[[1]], hi = xr[[2]])
  )
}

#' Whether a piece would coincide with a committed piece in both orders
#'
#' In a gap every piece entering a slot from the left runs from the left
#' layer to the slot, and every piece leaving to the right runs from the
#' slot to the right layer, so a piece leaving segment `s1` at a y where a
#' piece enters `s2` shares a run with it whenever `s1` is left of `s2`.
#' One such pair is resolved by ordering the slots the other way; a pair
#' that coincides in both orders cannot be, and a candidate that would
#' create one is infeasible. Pieces under the segment's own key are the
#' segment's hyperedge and are not tested.
#'
#' @noRd
pieces_coincide <- function(pieces, key, left, right) {
  if (abs(left - right) < 1e-3 || nrow(pieces) == 0) {
    return(FALSE)
  }
  for (k in unique(pieces$key[pieces$key != key])) {
    at <- pieces$key == k
    es <- any(abs(right - pieces$left[at]) < 1e-6)
    se <- any(abs(pieces$right[at] - left) < 1e-6)
    if (es && se) {
      return(TRUE)
    }
  }
  FALSE
}

#' The y nearest `y0` on one side that is free in every crossed layer
#'
#' Snaps `y0` to the nearest free y of each layer in turn, on `side`, until
#' a value is free in all of them; `NA` when some layer has nothing free
#' beyond it.
#'
#' @noRd
common_free_y <- function(ints, y0, side) {
  y <- y0
  repeat {
    moved <- FALSE
    for (iv in ints) {
      y2 <- nearest_free_y(iv, y, side, FALSE)
      if (is.na(y2)) {
        return(NA_real_)
      }
      if (abs(y2 - y) > 1e-9) {
        y <- y2
        moved <- TRUE
      }
    }
    if (!moved) {
      return(y)
    }
  }
}

#' Push a channel outward past the placed channels it would run beside
#'
#' Channels whose x-ranges properly overlap this one and whose y lies
#' within `sep_e` of the candidate are conflicts, whichever side they were
#' placed on; the candidate moves `sep_e` beyond the outermost conflict in
#' its own direction and is checked again until it is clear. An E/W channel
#' is registered over the bands of the gaps it crosses, so two channels
#' that share a gap conflict while two S/N channels that meet at a node do
#' not: the node's ports keep those apart.
#'
#' @noRd
stack_channel <- function(y, side, xr, channels, sep_e) {
  same <- channels$hi > min(xr) + 1e-9 & channels$lo < max(xr) - 1e-9
  ys <- channels$y[same]
  repeat {
    near <- abs(ys - y) < sep_e - 1e-9
    if (!any(near)) {
      return(y)
    }
    y <- if (side > 0) max(ys[near]) + sep_e else min(ys[near]) - sep_e
  }
}

#' The hyperedge segments of one gap
#'
#' Every E/W edge whose first gap this is contributes a piece entering at
#' its source's y and leaving at its target's y (its channel y when it
#' spans); every spanning E/W edge whose last gap this is contributes a
#' piece entering at its channel y and leaving at its target's y. Pieces
#' leaving one source port are one segment, as are the pieces of duplicate
#' edges; the y-interval of a segment is the range of its pieces. Segments
#' are returned in canonical order: source segments by the position of
#' their source, then last-gap segments by the positions of both endpoints,
#' with node row indices as the final tie-break, so that the order never
#' depends on node names.
#'
#' @return A list of parallel vectors and lists: `members` (edge indices),
#'   `member_first` (whether each member enters from its source), `lefts`
#'   and `rights` (the y values of the horizontal pieces on each side), `lo`,
#'   `hi`, and `degenerate`.
#' @noRd
ortho_gap_segments <- function(
  first,
  last,
  a,
  b,
  Sy,
  Ty,
  y_ch,
  span,
  nodes,
  tol
) {
  e <- c(first, last)
  is_first <- c(rep(TRUE, length(first)), rep(FALSE, length(last)))
  left_y <- ifelse(is_first, Sy[e], y_ch[e])
  right_y <- ifelse(is_first & span[e] >= 2, y_ch[e], Ty[e])
  key <- ifelse(is_first, paste0("s", a[e]), paste0("e", a[e], "-", b[e]))
  ord <- order(
    as.integer(!is_first),
    nodes$y[a[e]],
    nodes$x[a[e]],
    ifelse(is_first, -Inf, nodes$y[b[e]]),
    ifelse(is_first, -Inf, nodes$x[b[e]]),
    a[e],
    ifelse(is_first, 0L, b[e]),
    method = "radix"
  )
  idx <- lapply(unique(key[ord]), function(k) which(key == k))
  lo <- vapply(idx, function(i) min(left_y[i], right_y[i]), numeric(1))
  hi <- vapply(idx, function(i) max(left_y[i], right_y[i]), numeric(1))
  list(
    members = lapply(idx, function(i) e[i]),
    member_first = lapply(idx, function(i) is_first[i]),
    lefts = lapply(idx, function(i) unique(left_y[i])),
    rights = lapply(idx, function(i) unique(right_y[i])),
    lo = lo,
    hi = hi,
    degenerate = hi - lo < tol
  )
}

#' Arc length from a path end to the cap line on its port's axis
#'
#' The arrow layer resects each end of a path by the cap, measured along the
#' path from the node centre. A centre port runs straight out, so the head
#' tip sits on the cap line `|coord - centre| = cap` on the port's axis
#' exactly `cap` along the path; an offset port reaches that line later,
#' through the hidden connector and its corner, and the resect is the arc
#' length to the crossing. The crossing is looked for within
#' `limit` of arc length, the sharp length of the connector and the terminal
#' run, so that a run too short to reach the cap line falls back to the cap.
#'
#' @param P The sampled path as a matrix, the port's end last.
#' @param axis The column of `P` the port's axis runs along.
#' @noRd
port_resect <- function(P, centre, axis, cap, limit, offset) {
  if (offset == 0 || nrow(P) < 2) {
    return(cap)
  }
  Q <- P[rev(seq_len(nrow(P))), , drop = FALSE]
  s <- c(0, cumsum(sqrt(diff(Q[, 1])^2 + diff(Q[, 2])^2)))
  off <- abs(Q[, axis] - centre[[axis]])
  k <- which(off >= cap - 1e-9 & s <= limit + 1e-9)
  if (length(k) == 0) {
    return(cap)
  }
  k <- k[[1]]
  if (k == 1L) {
    return(cap)
  }
  f <- (cap - off[[k - 1L]]) / (off[[k]] - off[[k - 1L]])
  s[[k - 1L]] + f * (s[[k]] - s[[k - 1L]])
}

#' Slot x of every segment in a gap
#'
#' Degenerate segments (equal entry and exit y) take no slot and stay
#' straight. The others are ranked by `ortho_slot_ranks()` and placed on the
#' first rung of a ladder that holds them, each rung giving up something the
#' one before kept:
#'
#' * Rung 0: the nominal stub `r_ref + cap + rc` at both ends; the ranks are
#'   spread evenly over the band, `x = lo + rank * (hi - lo) / (K + 1)`,
#'   when that keeps neighbouring ranks `sep_e` apart, and otherwise centred
#'   on the band midpoint `sep_e` apart when the band holds them all.
#' * Rung 1: the stub shrinks to `max(stub_min, (G - sep_e (K - 1)) / 2)`,
#'   where `stub_min = cap + max(head, rc) + rc` is the floor a bend vertex
#'   needs past a node centre, and the slots stay `sep_e` apart about the
#'   midpoint.
#' * Rung 2: the stub is at its floor and the spacing tightens to
#'   `(G - 2 stub_min) / (K - 1)`, no closer than `sep_min`.
#' * Rung 3: the spacing is `sep_min` and the corner radius shrinks, with
#'   the stub floor following it, no further than `rc_min`.
#' * Rung 4: no stub fits. The slots are centred on the gap midpoint at the
#'   spacing that keeps them between the layers' soft bands, `r_ref + m_min`
#'   from either layer, and no wider than `sep_e`; the gap is flagged
#'   `narrow` and its edges lose their clearance.
#'
#' @return A list with `x` (the slot of every segment, `NA` for a degenerate
#'   one), `narrow`, `rc` (the corner radius the gap needs), and `ladder`, a
#'   one-row data frame with `width`, `ranks`, `rung`, `stub`, and
#'   `spacing`, `NULL` when no segment took a slot.
#' @noRd
ortho_slot_positions <- function(segs, gap, opts, cap) {
  x <- rep(NA_real_, length(segs$lo))
  live <- which(!segs$degenerate)
  if (length(live) == 0) {
    return(list(x = x, narrow = FALSE, rc = opts$rc, ladder = NULL))
  }
  ranks <- ortho_slot_ranks(
    segs$lo[live],
    segs$hi[live],
    segs$lefts[live],
    segs$rights[live],
    opts$crossing_penalty,
    opts$sep_e
  )
  K <- max(ranks)
  eps <- 1e-9
  G <- gap[[2]] - gap[[1]]
  mid <- mean(gap)
  sep_e <- opts$sep_e
  sep_min <- opts$sep_min
  rc <- opts$rc
  stub_of <- function(rc) cap + max(opts$head, rc) + rc
  stub_min <- stub_of(rc)
  centred <- function(spacing) mid + (ranks - (K + 1) / 2) * spacing
  from_left <- function(stub, spacing) gap[[1]] + stub + (ranks - 1) * spacing

  stub0 <- opts$r_ref + cap + rc
  width0 <- G - 2 * stub0
  even <- width0 >= 0 && (K < 2 || width0 / (K + 1) >= sep_e - eps)
  narrow <- FALSE
  rc_g <- rc
  if (even) {
    rung <- 0L
    stub <- stub0
    spacing <- width0 / (K + 1)
    pos <- gap[[1]] + stub0 + ranks * spacing
  } else if (width0 >= (K - 1) * sep_e - eps) {
    rung <- 0L
    stub <- stub0
    spacing <- sep_e
    pos <- centred(sep_e)
  } else if ((G - sep_e * (K - 1)) / 2 >= stub_min - eps) {
    rung <- 1L
    stub <- (G - sep_e * (K - 1)) / 2
    spacing <- sep_e
    pos <- centred(sep_e)
  } else if (K >= 2 && (G - 2 * stub_min) / (K - 1) >= sep_min - eps) {
    rung <- 2L
    stub <- stub_min
    spacing <- (G - 2 * stub_min) / (K - 1)
    pos <- from_left(stub, spacing)
  } else {
    # the stub the gap can afford at sep_min spacing, and the corner radius
    # whose floor fits inside it
    afford <- (G - sep_min * (K - 1)) / 2
    rc3 <- if ((afford - cap) / 2 > opts$head) {
      (afford - cap) / 2
    } else {
      afford - cap - opts$head
    }
    rc3 <- min(rc3, rc)
    if (rc3 >= opts$rc_min - eps) {
      rung <- 3L
      rc_g <- rc3
      stub <- stub_of(rc3)
      spacing <- sep_min
      pos <- from_left(stub, spacing)
    } else {
      rung <- 4L
      rc_g <- opts$rc_min
      narrow <- TRUE
      stub <- NA_real_
      width4 <- G - 2 * opts$R_soft
      spacing <- if (K >= 2 && width4 > 0) {
        min(sep_e, width4 / (K - 1))
      } else {
        sep_e
      }
      pos <- centred(spacing)
    }
  }
  x[live] <- pos
  list(
    x = x,
    narrow = narrow,
    rc = rc_g,
    ladder = df_cols(
      width = G,
      ranks = K,
      rung = rung,
      stub = stub,
      spacing = spacing
    )
  )
}

#' Order the vertical segments of a gap left to right
#'
#' Every pair whose y-intervals overlap, meet, or come within `sep_e` of
#' each other must take distinct slots: two segments from different sources
#' at one x would draw a continuous line through both, and two that stop
#' short of each other by less than the separation read the same way once
#' stacked ports part the intervals that used to meet. For each such pair
#' the crossings of each order are
#' counted from the horizontal pieces: with `s1` left of `s2`, a piece
#' leaving `s1` crosses `s2` when its y lies strictly inside `s2`'s
#' interval, and a piece entering `s2` crosses `s1` likewise. A piece
#' leaving `s1` at the very y where a piece enters `s2` would share a run
#' with it, so an order that coincides is forbidden when the other does not
#' (the dependency carries infinite weight). Otherwise a dependency toward
#' the cheaper order carries the weight `16 * |c12 - c21| + 1`; equal
#' counts depend in canonical order. Cycles are broken by dropping the
#' lightest dependency on each, ranks follow the longest path from the
#' sources, and any two overlapping segments left on one rank are pushed
#' apart in canonical order until every overlapping pair differs. Ranks are
#' returned compacted to `1:max`.
#'
#' @noRd
ortho_slot_ranks <- function(lo, hi, lefts, rights, crossing_penalty, sep_e) {
  n <- length(lo)
  eps <- 1e-9
  overlap <- matrix(FALSE, n, n)
  d_from <- integer(0)
  d_to <- integer(0)
  d_w <- numeric(0)
  inside <- function(y, l, h) {
    sum(y > l + eps & y < h - eps)
  }
  meets <- function(y, ys) {
    any(abs(outer(y, ys, "-")) < 1e-6)
  }
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      if (min(hi[[i]], hi[[j]]) - max(lo[[i]], lo[[j]]) < -sep_e - eps) {
        next
      }
      overlap[i, j] <- TRUE
      overlap[j, i] <- TRUE
      co_ij <- meets(rights[[i]], lefts[[j]])
      co_ji <- meets(rights[[j]], lefts[[i]])
      if (co_ij != co_ji) {
        d_from <- c(d_from, if (co_ij) j else i)
        d_to <- c(d_to, if (co_ij) i else j)
        d_w <- c(d_w, Inf)
        next
      }
      c_ij <- inside(rights[[i]], lo[[j]], hi[[j]]) +
        inside(lefts[[j]], lo[[i]], hi[[i]])
      c_ji <- inside(rights[[j]], lo[[i]], hi[[i]]) +
        inside(lefts[[i]], lo[[j]], hi[[j]])
      if (c_ji < c_ij) {
        d_from <- c(d_from, j)
        d_to <- c(d_to, i)
      } else {
        d_from <- c(d_from, i)
        d_to <- c(d_to, j)
      }
      d_w <- c(d_w, crossing_penalty * abs(c_ij - c_ji) + 1)
    }
  }

  repeat {
    cycle <- dependency_cycle(n, d_from, d_to)
    if (is.null(cycle)) {
      break
    }
    drop <- cycle[[which.min(d_w[cycle])]]
    d_from <- d_from[-drop]
    d_to <- d_to[-drop]
    d_w <- d_w[-drop]
  }

  repeat {
    ranks <- longest_path_ranks(n, d_from, d_to)
    tied <- which(
      overlap & outer(ranks, ranks, "==") & upper.tri(overlap),
      arr.ind = TRUE
    )
    if (nrow(tied) == 0) {
      break
    }
    tied <- tied[order(tied[, 1], tied[, 2]), , drop = FALSE]
    d_from <- c(d_from, tied[[1, 1]])
    d_to <- c(d_to, tied[[1, 2]])
  }
  match(ranks, sort(unique(ranks)))
}

#' One cycle of a dependency graph, as dependency indices, or `NULL`
#'
#' Sources are peeled off until none remain; if anything is left, walking
#' backwards through remaining predecessors must revisit a node, and the
#' dependencies between the two visits form a cycle.
#'
#' @noRd
dependency_cycle <- function(n, from, to) {
  if (length(from) == 0) {
    return(NULL)
  }
  remaining <- rep(TRUE, n)
  repeat {
    has_pred <- seq_len(n) %in% to[remaining[from]]
    removable <- remaining & !has_pred
    if (!any(removable)) {
      break
    }
    remaining[removable] <- FALSE
  }
  if (!any(remaining)) {
    return(NULL)
  }
  nodes <- which(remaining)[[1]]
  deps <- integer(0)
  repeat {
    current <- nodes[[length(nodes)]]
    d <- which(to == current & remaining[from])[[1]]
    u <- from[[d]]
    k <- match(u, nodes)
    if (!is.na(k)) {
      return(c(d, deps[seq.int(k, length.out = length(deps) - k + 1L)]))
    }
    nodes <- c(nodes, u)
    deps <- c(deps, d)
  }
}

#' Longest-path ranks of an acyclic dependency graph, sources at 1
#' @noRd
longest_path_ranks <- function(n, from, to) {
  rank <- rep(1L, n)
  indeg <- tabulate(to, n)
  queue <- which(indeg == 0)
  while (length(queue) > 0) {
    v <- queue[[1]]
    queue <- queue[-1]
    for (d in which(from == v)) {
      u <- to[[d]]
      rank[[u]] <- max(rank[[u]], rank[[v]] + 1L)
      indeg[[u]] <- indeg[[u]] - 1L
      if (indeg[[u]] == 0) {
        queue <- c(queue, u)
      }
    }
  }
  rank
}

#' Ports and bends of one orthogonal edge
#'
#' The parallel shift is applied along the layer axis for E/W slots and
#' along the within-layer axis for channels, by the amount that gives the
#' requested perpendicular displacement, so the runs stay axis-aligned. An
#' S/N port offset `dx_s` or `dx_t` moves the stub beside the centre line;
#' the port returned is then the foot of the stub on that line, the corner
#' the connector from the centre turns at. A W port offset `dy_t` moves the
#' arrival's last run onto a row beside the centre line; the port returned
#' is then the foot of that row on the disc boundary, from which a hidden
#' connector runs to the centre. An E/W port is dropped when its bend falls
#' short of it, which happens only when a narrow gap has pushed a slot
#' inside the node disc.
#'
#' @return A list with `bends` (a matrix, or `NULL` when the edge has no
#'   bend and stays straight), `port_s`, `port_t`, `side`, and for each end
#'   the port's axis (`axis_s`, `axis_t`, as a column of the path), its
#'   offset from the centre line (`off_s`, `off_t`), and the sharp length of
#'   its connector and terminal run (`limit_s`, `limit_t`), which the resect
#'   is measured within.
#' @noRd
ortho_bends <- function(
  kind,
  S,
  E,
  r_s,
  r_t,
  side,
  y_ch,
  x_first,
  x_last,
  span,
  shift,
  u,
  stub,
  dx_s,
  dx_t,
  dy_t,
  tol
) {
  if (kind == "sn") {
    y <- y_ch + shift / u[[1]]
    xs <- S[[1]] + dx_s
    xt <- E[[1]] + dx_t
    return(list(
      bends = rbind(c(xs, y), c(xt, y)),
      port_s = c(xs, S[[2]]),
      port_t = c(xt, E[[2]]),
      side = side,
      axis_s = 2L,
      axis_t = 2L,
      off_s = dx_s,
      off_t = dx_t,
      limit_s = abs(dx_s) + abs(y - S[[2]]),
      limit_t = abs(dx_t) + abs(y - E[[2]])
    ))
  }
  centre_ports <- list(
    axis_s = 1L,
    axis_t = 1L,
    off_s = 0,
    off_t = 0,
    limit_s = 0,
    limit_t = 0
  )
  if (kind == "detour") {
    if (abs(E[[2]] - S[[2]]) < tol) {
      xa <- S[[1]] + stub
      xb <- E[[1]] - stub
      if (xa > xb) {
        xa <- (S[[1]] + E[[1]]) / 2
        xb <- xa
      }
      y <- S[[2]] + shift
      return(c(
        list(
          bends = rbind(c(xa, S[[2]]), c(xa, y), c(xb, y), c(xb, E[[2]])),
          port_s = c(S[[1]] + r_s, S[[2]]),
          port_t = c(E[[1]] - r_t, E[[2]]),
          side = sign(shift)
        ),
        centre_ports
      ))
    }
    ya <- S[[2]] + stub
    yb <- E[[2]] - stub
    if (ya > yb) {
      ya <- (S[[2]] + E[[2]]) / 2
      yb <- ya
    }
    x <- S[[1]] - shift
    return(c(
      list(
        bends = rbind(c(S[[1]], ya), c(x, ya), c(x, yb), c(E[[1]], yb)),
        port_s = c(S[[1]], S[[2]] + r_s),
        port_t = c(E[[1]], E[[2]] - r_t),
        side = sign(shift)
      ),
      centre_ports
    ))
  }

  dx <- 0
  dy <- 0
  if (shift != 0) {
    if (abs(u[[2]]) > 1e-9) {
      dx <- -shift / u[[2]]
    } else {
      dy <- shift
    }
  }
  # a spanning edge whose run lies on the target's own line has no last
  # vertical to move onto a row
  if (span >= 2 && is.na(x_last)) {
    dy_t <- 0
  }
  yt <- E[[2]] + dy_t
  bends <- NULL
  if (span == 1) {
    if (!is.na(x_first)) {
      bends <- rbind(c(x_first, S[[2]]), c(x_first, yt))
    }
  } else {
    y <- y_ch + dy
    if (!is.na(x_first)) {
      bends <- rbind(bends, c(x_first, S[[2]]), c(x_first, y))
    }
    if (!is.na(x_last)) {
      bends <- rbind(bends, c(x_last, y), c(x_last, yt))
    }
  }
  if (is.null(bends)) {
    return(list(bends = NULL))
  }
  bends[, 1] <- bends[, 1] + dx
  # the foot of the last run: the W port on the centre line, or the point
  # where the port row meets the disc boundary
  foot_x <- E[[1]] - sqrt(max(r_t^2 - dy_t^2, 0))
  x_end <- bends[[nrow(bends), 1]]
  ports <- centre_ports
  ports$off_t <- dy_t
  ports$limit_t <- r_t + max(foot_x - x_end, 0)
  c(
    list(
      bends = bends,
      port_s = if (bends[[1, 1]] > S[[1]] + r_s) c(S[[1]] + r_s, S[[2]]),
      port_t = if (x_end < foot_x) c(foot_x, yt),
      side = if (span >= 2) side else NA_real_
    ),
    ports
  )
}

#' Drop consecutive duplicate points of a polyline matrix
#' @noRd
dedupe_points <- function(P, tol = 1e-9) {
  if (nrow(P) < 2) {
    return(P)
  }
  keep <- c(TRUE, abs(diff(P[, 1])) >= tol | abs(diff(P[, 2])) >= tol)
  P[keep, , drop = FALSE]
}

#' Drop interior vertices a polyline passes straight through
#'
#' A vertex is kept when the path turns at it or reverses on it; only
#' vertices collinear with their neighbours in the same direction go.
#'
#' @noRd
drop_collinear <- function(P) {
  n <- nrow(P)
  if (n < 3) {
    return(P)
  }
  keep <- rep(TRUE, n)
  last <- 1L
  for (i in 2:(n - 1L)) {
    ab <- P[i, ] - P[last, ]
    bc <- P[i + 1L, ] - P[i, ]
    cross <- ab[[1]] * bc[[2]] - ab[[2]] * bc[[1]]
    if (abs(cross) < 1e-9 && sum(ab * bc) > 0) {
      keep[[i]] <- FALSE
    } else {
      last <- i
    }
  }
  P[keep, , drop = FALSE]
}

#' Round the corners of a polyline with quadratic Beziers
#'
#' Each interior vertex `B` with neighbours `A` and `C` is replaced by `n`
#' uniformly spaced samples of the quadratic Bezier from `P` through `B` to
#' `Q`, where `P` and `Q` lie `rr = min(rc, |AB| / 2, |BC| / 2)` along the
#' two runs, so neighbouring corners never overlap. Twelve samples keep the
#' turning angle of a right angle under 12 degrees per step. Vertices the
#' path runs straight through or reverses on are kept as they are, and so
#' are the vertices listed in `sharp`.
#'
#' @noRd
round_corners <- function(P, rc, n = 12L, sharp = integer(0)) {
  k <- nrow(P)
  if (k < 3) {
    return(P)
  }
  t <- seq(0, 1, length.out = n)
  w_p <- (1 - t)^2
  w_b <- 2 * t * (1 - t)
  w_q <- t^2
  out <- vector("list", k)
  out[[1]] <- P[1, , drop = FALSE]
  for (i in 2:(k - 1L)) {
    A <- P[i - 1L, ]
    B <- P[i, ]
    C <- P[i + 1L, ]
    ab <- A - B
    cb <- C - B
    lab <- sqrt(sum(ab^2))
    lcb <- sqrt(sum(cb^2))
    cross <- ab[[1]] * cb[[2]] - ab[[2]] * cb[[1]]
    if (i %in% sharp || abs(cross) < 1e-9 || lab == 0 || lcb == 0) {
      out[[i]] <- P[i, , drop = FALSE]
      next
    }
    rr <- min(rc, lab / 2, lcb / 2)
    Pp <- B + rr * ab / lab
    Qq <- B + rr * cb / lcb
    pts <- cbind(
      w_p * Pp[[1]] + w_b * B[[1]] + w_q * Qq[[1]],
      w_p * Pp[[2]] + w_b * B[[2]] + w_q * Qq[[2]]
    )
    pts[1, ] <- Pp
    pts[n, ] <- Qq
    out[[i]] <- pts
  }
  out[[k]] <- P[k, , drop = FALSE]
  do.call(rbind, out)
}

#' Subdivide the segments of a polyline to a maximum spacing
#'
#' Interior points interpolate each segment, so an axis-aligned run keeps
#' its constant coordinate exactly; the original vertices are kept.
#'
#' @noRd
sample_runs <- function(P, spacing) {
  n <- nrow(P)
  if (n < 2) {
    return(P)
  }
  dx <- diff(P[, 1])
  dy <- diff(P[, 2])
  pieces <- pmax(1L, as.integer(ceiling(sqrt(dx^2 + dy^2) / spacing - 1e-9)))
  if (all(pieces == 1L)) {
    return(P)
  }
  out <- vector("list", n)
  for (i in seq_len(n - 1L)) {
    if (pieces[[i]] == 1L) {
      out[[i]] <- P[i, , drop = FALSE]
      next
    }
    f <- seq_len(pieces[[i]] - 1L) / pieces[[i]]
    out[[i]] <- rbind(
      P[i, ],
      cbind(P[[i, 1]] + f * dx[[i]], P[[i, 2]] + f * dy[[i]])
    )
  }
  out[[n]] <- P[n, , drop = FALSE]
  do.call(rbind, out)
}

#' Distance from each obstacle centre to the nearest segment of a polyline
#' @noRd
polyline_min_dist <- function(P, ox, oy) {
  n <- nrow(P)
  x0 <- P[-n, 1]
  y0 <- P[-n, 2]
  x1 <- P[-1, 1]
  y1 <- P[-1, 2]
  vapply(
    seq_along(ox),
    function(k) min(dist_to_edge(ox[[k]], oy[[k]], x0, y0, x1, y1)),
    numeric(1)
  )
}

# Engine -------------------------------------------------------------------------------------

#' Route a canonically oriented scene
#'
#' @param layers The scene's layers from `infer_layers()`, or `NULL` to
#'   infer them here.
#' @noRd
route_scene_mm <- function(
  nodes,
  edges,
  bounds,
  cap,
  mode,
  opts,
  layers = NULL
) {
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

  resect_head <- NULL
  resect_fins <- NULL
  ortho <- NULL
  assemble <- function() {
    meta <- df_cols(
      edge = paste0(from_name, "->", to_name),
      routed = routed,
      mode = mode_out,
      side = side_out,
      n_waypoints = n_wp,
      waypoint_layers = wp_layers,
      clearance_ok = clearance_ok,
      sagitta_ratio = sagitta,
      sagitta_capped = capped
    )
    # orthogonal mode reports the resect each end needs and the ladder
    if (!is.null(resect_head)) {
      meta$resect_head <- resect_head
      meta$resect_fins <- resect_fins
    }
    out <- list(paths = paths, meta = meta, waypoints = waypoints)
    if (!is.null(ortho)) {
      out$ortho <- ortho
    }
    out
  }
  if (mode == "straight" || n_edges == 0 || nrow(nodes) == 0) {
    return(assemble())
  }

  layers <- layers %||% infer_layers(nodes, opts$tol_layer)

  if (mode == "orthogonal") {
    st <- route_orthogonal_scene(
      nodes,
      from,
      to,
      from_name,
      to_name,
      is_fixed,
      paths,
      bounds,
      cap,
      opts,
      layers
    )
    paths <- st$paths
    waypoints <- st$waypoints
    routed <- st$routed
    mode_out <- st$mode
    side_out <- st$side
    n_wp <- st$n_waypoints
    wp_layers <- st$waypoint_layers
    clearance_ok <- st$clearance_ok
    sagitta <- st$sagitta_ratio
    capped <- st$sagitta_capped
    resect_head <- st$resect_head
    resect_fins <- st$resect_fins
    ortho <- st$ortho
    return(assemble())
  }

  info <- edge_span_info(nodes, from, to, layers)
  routable <- !is_fixed & info$Lc > 0 & from != to
  R_full <- nodes$r + opts$m
  R_soft <- nodes$r + opts$m_min

  hits <- find_blocked_edges(
    nodes,
    df_cols(from = from_name[routable], to = to_name[routable]),
    R_soft,
    R_full
  )
  hits$edge <- which(routable)[hits$edge]
  # a chord shorter than 2R cannot bow around a disc that overlaps both of
  # its endpoint discs, so it is drawn as it is
  hits <- df_rows(hits, which(info$Lc[hits$edge] >= 2 * opts$R))

  grp <- parallel_groups(from, to, from_name, to_name, routable, opts$sep_m)
  extra <- grp$extra
  shift <- grp$shift

  to_route <- routable & (seq_len(n_edges) %in% hits$edge | shift != 0)
  order_e <- which(to_route)
  # chord lengths are rounded to a micrometre so that two skip edges of one
  # row tie on length and the name order decides, not a floating difference
  order_e <- order_e[order(
    -info$span[order_e],
    -round(info$Lc[order_e], 6),
    from_name[order_e],
    to_name[order_e],
    method = "radix"
  )]

  ctx <- list(nodes = nodes, from = from, to = to, Lc = info$Lc)
  base_intervals <- lapply(seq_len(layers$n), function(k) {
    layer_free_intervals(
      df_rows(nodes, layers$members[[k]]),
      opts$m,
      bounds,
      opts$pad,
      opts$sep_e
    )
  })

  # one edge: the candidate tiers in order, verified and repaired
  route_one <- function(e, placed, occ, reserved) {
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
    # along the chord; hits at one parameter are ordered by position and
    # then by row, never by name
    eh <- df_rows(
      eh,
      order(eh$t, nodes$x[idx], nodes$y[idx], idx, method = "radix")
    )

    others <- seq_len(nrow(nodes))[-c(fr$a, fr$b)]
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
    ints <- NULL
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
              df_rows(nodes, layers$members[[k]]),
              opts$m + extra[[e]],
              bounds,
              opts$pad,
              opts$sep_e
            )
          })
        }
        cands <- assign_spanning_waypoints(
          fr,
          la,
          lb,
          layers,
          ints,
          occ,
          ectx,
          placed,
          opts,
          bounds,
          reserved,
          base_intervals[crossed]
        )
        cands <- Filter(function(c) nrow(c$wp) > 0, cands)
        bow <- NULL
        if (length(cands) > 0) {
          try_cand <- function(cand) {
            route_spanning_candidate(
              job,
              cand,
              eh,
              crossed,
              layers,
              base_intervals[crossed],
              occ,
              placed,
              shift[[e]],
              opts
            )
          }
          # the best-ranked candidate wins when its curve keeps the margin
          # from the panel bounds
          first <- try_cand(cands[[1]])
          if (first$inside) {
            res <- first
          } else {
            # otherwise the free bow is priced with the remaining candidates
            # and the cheapest verified one inside the margin wins; a
            # lower-ranked slot never wins by default
            bow <- route_free_bow(job, placed)
            rest <- cands[-1]
            pool_cost <- c(
              vapply(rest, function(c) c$cost, numeric(1)),
              bow$cost
            )
            pool_overlap <- c(
              vapply(rest, function(c) c$overlap, logical(1)),
              FALSE
            )
            fallback <- NULL
            for (k in order(pool_overlap, pool_cost)) {
              tried <- if (k > length(rest)) bow else try_cand(rest[[k]])
              if (tried$inside && tried$clearance_ok) {
                res <- tried
                break
              }
              # when nothing verifies, the cheapest attempt inside the margin
              # is kept, and the free bow may still replace it below
              if (tried$inside && is.null(fallback)) {
                fallback <- tried
              }
            }
            res <- res %||% fallback %||% first
          }
          # a spanning route that cannot be verified inside the panel falls
          # through to the free-bow tier, which is kept when it does better
          if (!res$clearance_ok || !res$inside) {
            alt <- bow %||% route_free_bow(job, placed)
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

    # every drawn curve keeps the clearance margin from the panel bounds: a
    # route that no tier could keep there is clamped to it and loses its
    # clearance
    if (!res$inside) {
      res$path <- clamp_path(res$path, bounds, opts$m)
      res$clearance_ok <- FALSE
    }

    # the drawn arch of a spanning route occupies every layer it crosses
    res$occ <- if (res$tier == "spanning") {
      arch_occupancy(
        e,
        res$path,
        fr,
        (la + 1L):(lb - 1L),
        layers,
        base_intervals[(la + 1L):(lb - 1L)],
        res$side,
        opts$sep_e
      )
    }
    res$fr <- fr
    res
  }

  # every routed edge in order; a second pass with slot reservations when a
  # shared slot came out in routing order rather than chord order
  route_all <- function(reserved) {
    placed <- placed_set(paths)
    occ <- empty_occupancy()
    out <- vector("list", n_edges)
    for (e in order_e) {
      held <- if (!is.null(reserved)) {
        df_rows(reserved, which(reserved$edge == e))
      }
      res <- route_one(e, placed, occ, held)
      placed <- place_edge(placed, e, res$path$x, res$path$y)
      if (!is.null(res$occ)) {
        occ <- df_bind(occ, res$occ)
      }
      out[[e]] <- res
    }
    list(results = out, occ = occ)
  }

  pass <- route_all(NULL)
  reserved <- slot_reservations(pass$occ, opts$sep_e)
  if (!is.null(reserved)) {
    pass <- route_all(reserved)
  }

  for (e in order_e) {
    res <- pass$results[[e]]
    fr <- res$fr
    path <- res$path
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
