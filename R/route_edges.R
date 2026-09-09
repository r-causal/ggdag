# Edge routing in millimetres. The router takes node discs and edge chords in
# the units they are drawn in, infers layers from the layer-axis coordinate,
# and routes every blocked edge as a centripetal Catmull-Rom spline through a
# few waypoints: one per crossed layer, snapped into the free slots of that
# layer, or a single bow around the obstacle for short and steep chords. A
# chord shorter than 2R stays straight, since no bow fits between endpoint
# discs that close. A chord that clears every disc but passes within
# head / 2 + head_margin of another edge's drawn arrowhead is nudged past
# it as a grazed disc is, away from that head's target; a chord clear of
# the heads too stays straight. The slots of a layer are the gaps between
# its padded discs; a gap narrower than the edge separation is a sliver and
# is not a slot, and a periphery slot keeps the clearance margin m from the
# panel bounds, as every drawn curve does. The side of a detour is chosen by a cost that
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
# or not a node blocks its chord. Vertical chords and chords between two
# nodes of one layer stay straight. A level chord (tilted by no more than
# the corner radius) between adjacent layers, or a level spanning chord
# whose run no crossed disc blocks, is drawn as the horizontal run on its
# target's line: the head keeps the target's centre and the tail leaves its
# node through the port on that line, so no run is oblique. A spanning edge
# chooses the cheapest of its candidate channels, each priced by
# displacement, bends, crossings, and congestion: an S or N channel past
# the crossed stacks when its endpoints are the extreme nodes of their
# layers, an E/W run at either endpoint's line or beyond the stacks, and,
# when no S/N channel fits, a run through a free interval of every crossed
# layer. A channel that would cut a disc or run inside the panel margin is
# infeasible; channels that would share a y over overlapping x-ranges are
# stacked sep_e apart, the shorter span inside. A channel run that would
# cross the arrowhead at the end of a placed S/N stub, whichever direction
# it runs, is pushed past the head and its margin first. A spanning edge
# whose run on its own source's or target's line is clear of the discs, the
# margin, and the committed pieces, and is crowded only by the channels
# placed before it, slides those channels sep_e away from the line (each
# pushing the next in turn) and takes the line when the price of their
# moves, their displacement and any bend they gain, is less than the price
# of the pushed alternatives; an S/N channel, a channel of the edge's own
# hyperedge, and a channel already on its own endpoint line never move, and
# a slide that would need one is refused. Every other edge leaves through
# the E port and enters through the W port, with one vertical run per
# crossed gap. Within a gap the vertical runs are hyperedge segments (a
# fan leaving one port shares one); segments from different sources never
# share a slot, even when they only meet or come within sep_e. They are
# ordered by a dependency graph weighted by the crossings each order would
# cause, an order whose horizontal pieces would coincide being forbidden,
# numbered by longest path, and placed on the first rung of a ladder that
# holds them: the nominal stub and an even spread, then a shorter stub, a
# tighter spacing, a smaller corner radius, and finally slots spread without
# clearance between the source layer's soft band and a straight run before
# the target layer that holds an arrowhead and half a separation behind its
# base, overflowing toward the source when the gap cannot hold them all.
# The arrivals on a node's W side
# take stacked rows beside its centre line, the level chord, which runs on
# that line, keeping the centre, the rows centred on the node when no chord
# is level with it, and
# a group whose rows would sit closer than half the edge separation merging
# onto one row; two channel stubs on one N or S side sit sep_e / 2 either
# side of the centre line. So no stub carries two edges in opposite
# directions and every arrowhead is drawn on a row of its own, along the
# run it arrives on: a path into an offset port ends on the port's own
# line, never at the centre, and its resect puts the tip the same distance
# past the disc face as a centre port's. Bends are then rounded with a
# quadratic Bezier, the runs are sampled, and the result is verified
# against the node discs like a spline.

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
#' @param head_penalty In spline mode, the price of a candidate chain that
#'   passes within the edge separation of another edge's arrowhead zone
#'   (the last `cap` of its drawn ink), per zone. A quarter of a crossing
#'   at the default of 4: the chain is coarse, and the term is there to
#'   steer a tie or a near tie toward the side that keeps clear.
#' @param tight_penalty In spline mode, the price of a route through a tight
#'   slot, a gap between two discs too narrow for a slot at the full
#'   clearance margin that a curve can still thread at the soft margin, or
#'   `NULL` for `2 (m - m_min) / r_ref`: the two flanking discs each give
#'   up `m - m_min` of margin, priced as that much displacement.
#' @param crossing_saturation In spline mode, whether the crossing price
#'   saturates: the first crossing of a candidate costs `crossing_penalty`
#'   in full and each further one half of the last (16, 24, 28, 30, ...),
#'   so that in a tangle no route crosses fewer than several edges the
#'   displacement decides. `FALSE` prices every crossing in full.
#'   Orthogonal mode always prices crossings linearly.
#' @param sagitta_max In spline mode, how deep a routed edge may bow off its
#'   chord, as a fraction of the chord, or `NULL`. Two constants are derived
#'   from it. `sagitta_max` is the free-bow tier's cap, which keeps its own
#'   default of `0.22`; `sagitta_max_spanning` is the spanning tier's, which
#'   is `Inf` unless a value was written here, so that a caller who names no
#'   cap leaves the tier's choice of slot unconstrained.
#' @return A named list of constants.
#' @noRd
route_constants <- function(
  r_ref,
  m = NULL,
  sep_e = NULL,
  sep_min = NULL,
  layer_axis = c("auto", "x", "y"),
  corners = c("rounded", "sharp"),
  bend_penalty = 2,
  head_penalty = 4,
  tight_penalty = NULL,
  crossing_saturation = TRUE,
  sagitta_max = NULL
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
    # must hold past the cap, the head width that bounds a port stack, and
    # the run the ladder's last rung keeps behind a head base so that no
    # other edge's slot is drawn across it
    rc_min = 0.8,
    head = 2,
    head_w = 1.3,
    head_margin = sep_e / 2,
    R = r_ref + m,
    R_soft = r_ref + m_min,
    sep_e = sep_e,
    sep_min = min(sep_min %||% max(0.25 * r_ref, 1.5), sep_e),
    sep_m = max(1.0 * r_ref, 2.5),
    tol_layer = r_ref,
    steep_deg = 60,
    # the free-bow tier's cap keeps its own default; the spanning tier's is
    # the value the caller wrote and nothing when they wrote none, since a
    # cap there selects among slots rather than shrinking a bow
    sagitta_max = sagitta_max %||% 0.22,
    sagitta_max_spanning = sagitta_max %||% Inf,
    t_clamp = c(0.2, 0.8),
    # the bound on a departure or arrival tangent off the chord, the wider
    # window an arrival bearing may be chosen in when the narrow one leaves
    # the nearest rival closer than `squeeze_floor` mm of drawn tip, and the
    # bound on the end tangent the arrival feedback loop turns to reach the
    # bearing it was given
    tangent_clamp = 40,
    arrival_clamp = 60,
    head_clamp = 85,
    squeeze_floor = 2.5,
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
    crossing_saturation = isTRUE(crossing_saturation),
    congestion_penalty = 2,
    bend_penalty = bend_penalty,
    head_penalty = head_penalty,
    tight_penalty = tight_penalty %||% (2 * (m - m_min) / r_ref),
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
#' node's radius plus the clearance margin, or crosses another edge's drawn
#' arrowhead, which nudges it as a grazed disc does, is routed; the rest
#' stay straight.
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
#'   with a channel run past the crossed stacks. A channel run of either
#'   direction keeps the head margin past the arrowhead of any S/N stub
#'   it crosses. Corners are rounded unless `opts$corners` is `"sharp"`.
#'   A vertical chord stays straight, and so does a chord between two
#'   nodes of one layer. A level chord, tilted by no more than the corner
#'   radius, is drawn as the horizontal run on its target's line when it
#'   joins adjacent layers or when no crossed disc blocks that run: the
#'   head keeps the target's centre and the tail leaves its node through
#'   the port on that line. Such an edge keeps the mode `"straight"`,
#'   which means it has no bend, not that it joins the two centres.
#' @param opts Constants from `route_constants()`.
#' @return A list with `paths` (one `data.frame(x, y)` per edge, in input
#'   order), `meta` (one row per edge: `edge`, `routed`, `mode`, `side`,
#'   `n_waypoints`, `waypoint_layers`, `clearance_ok`, `sagitta_ratio`,
#'   `sagitta_capped`), and `waypoints` (one `data.frame(x, y, layer)` per
#'   edge). `mode` is `"soft"` for a chord nudged past a grazed disc or
#'   another edge's drawn arrowhead, `"bow"` for a free bow, and
#'   `"interior"` or `"periphery"` for a spanning route through the
#'   layers' slots. `clearance_ok` is `FALSE` when the drawn curve could
#'   not be kept `R` from every node disc, when its arch had to stop on
#'   another edge's arch in a shared slot, or when it left the panel and
#'   was clamped to it. `sagitta_capped` says that the sagitta cap acted on
#'   the route rather than that the route met it: a free bow over the cap
#'   was rebuilt at the soft margin, or, where `sagitta_max_spanning` is set
#'   and nothing the spanning tier could offer met it, the shallowest
#'   attempt that verified was drawn in place of the best-ranked one.
#'   In orthogonal mode `meta` also carries `resect_head` and `resect_fins`,
#'   the arc length in mm the arrow layer cuts from each end of the path:
#'   `cap - r + sqrt(r^2 - o^2)` for a port offset `o` from the centre line
#'   of a node of radius `r`, exactly `cap` at a centre port, so that every
#'   head tip sits `cap - r` past the disc face on its own run. The tail of a
#'   level chord leaves through the port on its target's line, so its
#'   `resect_fins` is that value at the offset between the two centres, and
#'   so is the tail of an arrival drawn as the run on its row; and the result
#'   carries `ortho`: `rc`, the corner radius the scene was drawn with, and
#'   `gaps`, one row per gap that holds a slot with `gap`, `width`,
#'   `ranks`, `rung`, `stub`, and `spacing` (see `ortho_slot_positions()`;
#'   `stub` is `NA` on the last rung, where no stub fits and the slots are
#'   spread between the source's soft band and a head run plus half a
#'   separation before the target, overflowing toward the source when the
#'   band cannot hold them; once the run before the target is a whole head
#'   run the gap's arrivals take rows at their targets as arrivals out of
#'   wider gaps do).
#' @noRd
route_edges_mm <- function(
  nodes,
  edges,
  bounds,
  cap = 8,
  mode = c("spline", "orthogonal", "straight"),
  opts = route_constants(stats::median(nodes$r))
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
#' A dropped gap whose two disc centres are at least `2 (r + m_min)` apart
#' can still be threaded by a curve at the soft margin, and is kept as a
#' tight slot: a zero-width row on the gap's centre line, flagged `tight`
#' and never `outer`. A tight slot holds one edge and is verified at the
#' soft margin. Whether a gap is tight is judged at `tight_margin`, the
#' base clearance margin, even when `margin` carries the extra clearance
#' of a parallel-edge group member: the group's members are offset from
#' one route, so a slot is tight for all of them or for none. `m_min = NULL`
#' finds no tight slots.
#'
#' @noRd
layer_free_intervals <- function(
  layer_nodes,
  margin,
  bounds,
  pad = 0.5,
  sep_e = 0,
  m_min = min(1.2, margin),
  tight_margin = margin
) {
  ord <- order(layer_nodes$y)
  ys <- layer_nodes$y[ord]
  rs <- layer_nodes$r[ord]
  Rk <- rs + margin
  lo <- c(bounds[[2]] + pad, ys + Rk)
  hi <- c(ys - Rk, bounds[[4]] - pad)
  n <- length(lo)
  width <- hi - lo
  toward_edge <- seq_len(n) %in% c(1L, n)
  keep <- width > 0 & (toward_edge | width >= sep_e)
  tight <- logical(n)
  if (!is.null(m_min) && n > 2L) {
    i <- 2:(n - 1L)
    # the gap between neighbouring discs at the base margin, and the
    # least distance between their centres a curve can thread at the
    # soft margin
    gap_base <- (ys[i] - (rs[i] + tight_margin)) -
      (ys[i - 1L] + (rs[i - 1L] + tight_margin))
    centres <- ys[i] - ys[i - 1L]
    threadable <- rs[i] + rs[i - 1L] + 2 * m_min
    tight[i] <- !keep[i] & gap_base < sep_e & centres >= threadable - 1e-9
    at <- which(tight)
    mid <- (ys[at] + ys[at - 1L]) / 2
    lo[at] <- mid
    hi[at] <- mid
    keep <- keep | tight
  }
  lo <- lo[keep]
  hi <- hi[keep]
  tight <- tight[keep]
  n <- length(lo)
  outer <- seq_len(n) %in% c(1L, n) & !tight
  df_cols(lo = lo, hi = hi, outer = outer, tight = tight)
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
  df_cols(lo = lo, hi = hi, outer = outer, tight = ints$tight)
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
#' @return A list of 4 x 2 control point matrices, with the two end arm
#'   lengths as the attribute `arms`.
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
  attr(B, "arms") <- c(a_s, a_e)
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
  n <- control_sample_counts(cp, spacing, min_n)
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

#' The number of samples `sample_beziers()` takes on each segment
#' @noRd
bezier_sample_counts <- function(B, spacing, min_n) {
  control_sample_counts(
    matrix(unlist(B, use.names = FALSE), nrow = 8L),
    spacing,
    min_n
  )
}

control_sample_counts <- function(cp, spacing, min_n) {
  len <- sqrt((cp[2, ] - cp[1, ])^2 + (cp[6, ] - cp[5, ])^2) +
    sqrt((cp[3, ] - cp[2, ])^2 + (cp[7, ] - cp[6, ])^2) +
    sqrt((cp[4, ] - cp[3, ])^2 + (cp[8, ] - cp[7, ])^2)
  pmax(min_n, ceiling(len / spacing))
}

#' Resample one end segment of a sampled curve after its end tangent moved
#'
#' Rotating an end tangent changes one control point of the first or last
#' Bezier segment and nothing else, so only that segment is sampled again
#' and spliced into `pts` in place of its previous samples. The samples are
#' the ones a full `sample_beziers()` would produce.
#'
#' @param pts The current samples.
#' @param B The segments, with the changed end control point in place.
#' @param n_old The sample count of the end segment before the change.
#' @param end `"E"` for the last segment, `"S"` for the first.
#' @noRd
resample_end <- function(pts, B, n_old, end, spacing, min_n) {
  K <- length(B)
  N <- nrow(pts)
  if (end == "E") {
    seg <- sample_beziers(B[K], spacing, min_n)
    if (K == 1L) {
      return(seg)
    }
    # a later segment skips its first sample, the join with the segment
    # before it
    keep <- seq_len(N - (n_old - 1L))
    df_cols(x = c(pts$x[keep], seg$x[-1L]), y = c(pts$y[keep], seg$y[-1L]))
  } else {
    seg <- sample_beziers(B[1L], spacing, min_n)
    if (K == 1L) {
      return(seg)
    }
    keep <- seq.int(n_old + 1L, N)
    df_cols(x = c(seg$x, pts$x[keep]), y = c(seg$y, pts$y[keep]))
  }
}

#' Bounding boxes of the control points of each Bezier segment
#'
#' A Bezier curve lies inside the convex hull of its control points, so an
#' obstacle farther than its clearance from every box cannot be violated.
#'
#' @return A matrix with one row per segment: `xmin`, `xmax`, `ymin`, `ymax`.
#' @noRd
bezier_boxes <- function(B) {
  cp <- matrix(unlist(B, use.names = FALSE), nrow = 8L)
  cbind(
    pmin(cp[1, ], cp[2, ], cp[3, ], cp[4, ]),
    pmax(cp[1, ], cp[2, ], cp[3, ], cp[4, ]),
    pmin(cp[5, ], cp[6, ], cp[7, ], cp[8, ]),
    pmax(cp[5, ], cp[6, ], cp[7, ], cp[8, ])
  )
}

#' Find samples that come too close to an obstacle
#'
#' An obstacle is a disc at `x, y`, or, when the table carries `x2, y2`
#' and a `capsule` flag, the capsule around the segment from `x, y` to
#' `x2, y2`: the arrowhead zone of another edge. Samples within arc length
#' `cut` of either end of the path are hidden by the arrow layer's resect
#' and are not tested against capsules.
#'
#' @param samples Data frame with `x`, `y`.
#' @param obstacles Data frame with `x`, `y`, and optionally `x2`, `y2`,
#'   `capsule`.
#' @param R_vec Clearance radius per obstacle.
#' @param cut Arc length at each end of the path exempt from capsules.
#' @return A data frame with `obstacle` (row in `obstacles`), `sample` (the
#'   nearest sample) and `depth` (how far inside `R - tol` it lies), one row
#'   per violated obstacle.
#' @noRd
verify_clearance <- function(
  samples,
  obstacles,
  R_vec,
  tol = 0.1,
  cut = 0,
  boxes = NULL
) {
  empty <- df_cols(obstacle = integer(), sample = integer(), depth = numeric())
  n_ob <- nrow(obstacles)
  sx <- samples$x
  sy <- samples$y
  if (n_ob == 0 || length(sx) == 0) {
    return(empty)
  }
  R_vec <- rep_len(R_vec, n_ob)
  capsule <- obstacles$capsule %||% logical(n_ob)
  ox <- obstacles$x
  oy <- obstacles$y
  x2 <- obstacles$x2 %||% ox
  y2 <- obstacles$y2 %||% oy
  # only an obstacle whose clearance reaches a box the curve lies in can be
  # violated: the boxes of the curve's segments, else the samples' box
  if (is.null(boxes)) {
    boxes <- matrix(c(min(sx), max(sx), min(sy), max(sy)), 1L)
  }
  nb <- nrow(boxes)
  i <- rep(seq_len(n_ob), each = nb)
  b <- rep.int(seq_len(nb), n_ob)
  lo_x <- pmin(ox, x2)
  hi_x <- pmax(ox, x2)
  lo_y <- pmin(oy, y2)
  hi_y <- pmax(oy, y2)
  gx <- pmax(0, boxes[b, 1L] - hi_x[i], lo_x[i] - boxes[b, 2L])
  gy <- pmax(0, boxes[b, 3L] - hi_y[i], lo_y[i] - boxes[b, 4L])
  near <- gx^2 + gy^2 < R_vec[i]^2
  cand <- which(tabulate(i[near], nbins = n_ob) > 0L)
  if (length(cand) == 0) {
    return(empty)
  }
  hidden <- NULL
  if (any(capsule[cand])) {
    seg <- sqrt(diff(sx)^2 + diff(sy)^2)
    from_start <- c(0, cumsum(seg))
    from_end <- rev(c(0, cumsum(rev(seg))))
    hidden <- from_start < cut | from_end < cut
  }
  j <- integer(length(cand))
  min_d <- numeric(length(cand))
  for (k in seq_along(cand)) {
    o <- cand[[k]]
    if (capsule[[o]]) {
      # a sample outside the capsule's box grown by the clearance is more
      # than the clearance away, so only the samples inside it are measured
      R <- R_vec[[o]]
      inside <- which(
        !hidden &
          sx >= lo_x[[o]] - R &
          sx <= hi_x[[o]] + R &
          sy >= lo_y[[o]] - R &
          sy <= hi_y[[o]] + R
      )
      if (length(inside) == 0) {
        j[[k]] <- 1L
        min_d[[k]] <- Inf
        next
      }
      d <- dist_to_edge(
        sx[inside],
        sy[inside],
        ox[[o]],
        oy[[o]],
        x2[[o]],
        y2[[o]]
      )
      at <- which.min(d)
      j[[k]] <- inside[[at]]
      min_d[[k]] <- d[[at]]
    } else {
      d2 <- (sx - ox[[o]])^2 + (sy - oy[[o]])^2
      j[[k]] <- which.min(d2)
      min_d[[k]] <- sqrt(d2[[j[[k]]]])
    }
  }
  depth <- R_vec[cand] - tol - min_d
  keep <- depth > 0
  df_cols(obstacle = cand[keep], sample = j[keep], depth = depth[keep])
}

#' How far a drawn arm reaches into the arrowhead zones of other edges
#'
#' The zones are the capsules `verify_clearance()` measures, the segment
#' from `zx, zy` to `zx2, zy2` grown by its clearance, and `lim` is that
#' clearance less the tolerance. The depth is summed over the zones the arm
#' is inside of, as it is there. The arrival separation moves only the end
#' arm, so reading the depth over that arm alone is what tells a hooked
#' arrival from one drawn across a neighbour's head.
#'
#' @param px,py The samples of the arm, already trimmed to what is drawn.
#' @noRd
head_zone_depth <- function(px, py, zx, zy, zx2, zy2, lim) {
  if (length(px) == 0 || length(lim) == 0) {
    return(0)
  }
  # a zone whose box, grown by its clearance, misses the arm's box cannot be
  # reached, so only the rest are measured
  near <- which(
    max(px) >= pmin(zx, zx2) - lim &
      min(px) <= pmax(zx, zx2) + lim &
      max(py) >= pmin(zy, zy2) - lim &
      min(py) <= pmax(zy, zy2) + lim
  )
  if (length(near) == 0) {
    return(0)
  }
  # every (sample, zone) pair at once, as `dist_to_edge()` measures one:
  # the distance to the axis, with the projection clamped to its ends
  n <- length(px)
  i <- rep(near, each = n)
  qx <- rep.int(px, length(near))
  qy <- rep.int(py, length(near))
  ax <- zx[i]
  ay <- zy[i]
  ux <- zx2[i] - ax
  uy <- zy2[i] - ay
  t <- ((qx - ax) * ux + (qy - ay) * uy) / (ux^2 + uy^2)
  t[!is.finite(t)] <- 0
  t <- pmin(pmax(t, 0), 1)
  d <- matrix(sqrt((qx - ax - t * ux)^2 + (qy - ay - t * uy)^2), nrow = n)
  least <- vapply(seq_along(near), function(k) min(d[, k]), numeric(1))
  sum(pmax(0, lim[near] - least))
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
#' A curve inside an arrowhead capsule is moved along the capsule's axis
#' past its far end, not merely off the axis: in the spanning tier the
#' waypoint moves along y by the y distance to that end plus the margin,
#' in the free tier along the axis itself, either bounded by the capsule
#' length plus `R`. The perpendicular escape from a near-vertical head zone
#' is sideways, which a spanning waypoint cannot make, and the loop would
#' oscillate. Disc violations are repaired before capsule violations. A
#' capsule belongs to no layer, so a waypoint its repair inserts sits
#' between the layers and reports none.
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
  capsule <- obstacles$capsule %||% logical(nrow(obstacles))
  ox <- obstacles$x
  oy <- obstacles$y
  o_layer <- obstacles$layer
  v_ob <- viol$obstacle
  v_sample <- viol$sample
  v_depth <- viol$depth
  # discs first, then deepest first; equal depths are ordered by the
  # obstacle's position and then its row, never by its name
  if (length(v_ob) > 1) {
    ord <- order(
      capsule[v_ob],
      -v_depth,
      ox[v_ob],
      oy[v_ob],
      v_ob,
      method = "radix"
    )
    v_ob <- v_ob[ord]
    v_sample <- v_sample[ord]
    v_depth <- v_depth[ord]
  }
  for (k in seq_along(v_ob)) {
    ob <- v_ob[[k]]
    C <- c(ox[[ob]], oy[[ob]])
    R <- R_vec[[ob]]
    p <- c(pts$x[[v_sample[[k]]]], pts$y[[v_sample[[k]]]])
    if (capsule[[ob]]) {
      far <- C
      near <- c(obstacles$x2[[ob]], obstacles$y2[[ob]])
      C <- nearest_on_segment(p, far, near)
    }
    nv <- p - C
    ln <- sqrt(sum(nv^2))
    nv <- if (ln > 1e-9) nv / ln else fr$n
    slack <- opts$repair_slack * (if (capsule[[ob]]) max(R, opts$R) else R)
    push <- opts$repair_relax * v_depth[[k]] + slack
    if (capsule[[ob]]) {
      axis <- far - near
      cap_len <- sqrt(sum(axis^2))
      axis <- axis / cap_len
      if (tier == "spanning") {
        if (abs(axis[[2]]) > 1e-6) {
          nv <- c(0, sign(axis[[2]]))
          push <- max(
            push,
            min(abs(far[[2]] - p[[2]]) + R + opts$verify_tol, cap_len + R)
          )
        }
      } else {
        nv <- axis
        push <- max(
          push,
          min(sum((far - p) * axis) + R + opts$verify_tol, cap_len + R)
        )
      }
    }
    t_p <- sum((p - fr$S) * fr$u) / fr$Lc
    t_wp <- ((wp$x - fr$S[[1]]) * fr$u[[1]] + (wp$y - fr$S[[2]]) * fr$u[[2]]) /
      fr$Lc
    # a repair waypoint sits at the violating sample's chord parameter,
    # which is no layer's position. An arrowhead capsule is the obstacle
    # that carries no layer, so its repair reports none for the waypoint it
    # inserts, in either tier, and matches the waypoints earlier repairs
    # left off the layers rather than any laid out on them
    layer_c <- o_layer[[ob]]
    at_layer <- if (is.na(layer_c)) {
      which(is.na(wp$layer))
    } else {
      which(!is.na(wp$layer) & wp$layer == layer_c)
    }
    crossed <- tier == "spanning" &&
      !is.na(layer_c) &&
      layer_c > la &&
      layer_c < lb
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
      # with no waypoint near the sample, the repair moves the one the
      # route carries for the obstacle's layer
      near <- at_layer
    }
    if (length(near) > 0) {
      if (tier == "spanning") {
        level <- logical(nrow(wp))
        for (j in near) {
          level <- level | abs(wp$y - wp$y[[j]]) < 1e-9
        }
        near <- which(level)
        # the plateau moves to the side the waypoint of the obstacle's
        # own layer is on, and away from the obstacle when it has none
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

#' The point of the segment from `a` to `b` nearest to `p`
#' @noRd
nearest_on_segment <- function(p, a, b) {
  d <- b - a
  l2 <- sum(d^2)
  if (l2 == 0) {
    return(a)
  }
  t <- min(1, max(0, sum((p - a) * d) / l2))
  a + t * d
}

#' Build, sample, verify, and repair the curve of one edge
#'
#' The obstacles are the node discs and, for a detour, the arrowhead
#' capsules of the other edges. `clearance_ok` is a statement about the
#' discs: the repair loop runs on capsule violations within the same
#' iteration budget once the discs are clear, keeps the disc-clear
#' iteration with the least remaining capsule depth, and reports that
#' curve as verified even when a head zone is still touched, since head
#' zones sit where the geometry often forces a curve to pass.
#'
#' When other edges arrive at the edge's true target, a bearing clear of
#' theirs is chosen by `separate_arrival()`; the sampled curve bends inside
#' the end arm when the last waypoint is close, so the arrival is measured
#' again `cap` from the target after sampling and the end tangent turned
#' toward the chosen bearing, up to four passes, keeping the pass whose
#' arrival is widest of the other arrivals. That correction runs only where
#' `separate_arrival()` moved the tangent: an arrival it left on its
#' original bearing is drawn as it was sampled, so a curve whose tangent is
#' already clear of the other arrivals but whose sampled bearing is not is
#' left alone. Separating an arrival is not a
#' licence to draw the curve through a neighbour's arrowhead, so a bearing
#' from the wider window stands only while it reaches no further into the
#' head zones than the narrow window's bearing does, and no pass may reach
#' further into them than the bearing it started from. A separated arrival
#' that cannot be verified against the discs is routed again without the
#' separation, and the verified result wins; when the separation never
#' moved a tangent the two routes are the same curve and the second is not
#' built.
#'
#' @param fr Edge frame from `edge_frame()`.
#' @param wp Waypoints with `x`, `y`, `layer`.
#' @param obstacles Non-endpoint nodes with `name`, `x`, `y`, `layer`, and
#'   capsules with `x2`, `y2`, `capsule`.
#' @param R_vec Clearance radius per obstacle.
#' @param arm_min End arm lengths, one per end.
#' @param tier `"spanning"` or `"free"`; decides how repairs move waypoints.
#' @param bounds Panel bounds; a repair that pushes a waypoint outside them
#'   ends the loop, since the route can no longer be drawn inside the panel.
#' @param repair Whether to run the repair loop.
#' @param cap The edge cap; samples within it of either end are hidden.
#' @param arrivals Two-column matrix of unit directions into the true
#'   target of the other edges arriving there, or `NULL`.
#' @param head_end `"E"` when the true target is the frame's `E`, `"S"`
#'   when the edge runs right to left.
#' @param theta_min Least angle between arrival directions, in degrees.
#' @param side The detour's side, or `NA` when either side is allowed.
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
  repair = TRUE,
  cap = 0,
  arrivals = NULL,
  head_end = "E",
  theta_min = 0,
  side = NA_real_
) {
  res <- route_spline_curve(
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
    repair,
    cap,
    arrivals,
    head_end,
    theta_min,
    side
  )
  if (!res$clearance_ok && isTRUE(res$separated) && repair) {
    alt <- route_spline_curve(
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
      repair,
      cap,
      NULL,
      head_end,
      theta_min,
      side
    )
    if (alt$clearance_ok || alt$depth < res$depth) {
      return(alt)
    }
  }
  res
}

#' The verify and repair loop of `route_spline_edge()`
#' @noRd
route_spline_curve <- function(
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
  repair,
  cap,
  arrivals,
  head_end,
  theta_min,
  side
) {
  capsule <- obstacles$capsule %||% logical(nrow(obstacles))
  separate <- !is.null(arrivals) && nrow(arrivals) > 0
  separated <- FALSE
  # the wide window opens only where the narrow one leaves the nearest
  # rival closer than `squeeze_floor` mm of drawn tip, which on the cap
  # circle the tips sit on is this angle
  gap_floor <- 2 * asin(min(1, opts$squeeze_floor / (2 * cap))) * 180 / pi
  arrival_window <- c(opts$tangent_clamp, opts$arrival_clamp, gap_floor)
  phi_star <- 0
  # the arrowhead zones of the other edges, as the capsule ends and the
  # clearance `head_zone_depth()` reads them at
  zone <- if (separate) which(capsule) else integer()
  zone_x <- obstacles$x[zone]
  zone_y <- obstacles$y[zone]
  zone_x2 <- (obstacles$x2 %||% obstacles$x)[zone]
  zone_y2 <- (obstacles$y2 %||% obstacles$y)[zone]
  zone_lim <- rep_len(R_vec, nrow(obstacles))[zone] - opts$verify_tol
  best <- NULL
  best_depth <- Inf
  best_ok <- NULL
  best_ok_depth <- Inf
  for (iter in 0:opts$repair_iter) {
    P <- cbind(
      c(fr$S[[1]], wp$x, fr$E[[1]]),
      c(fr$S[[2]], wp$y, fr$E[[2]])
    )
    n <- nrow(P)
    d_s <- clamp_direction(P[2, ] - P[1, ], fr$E - fr$S, opts$tangent_clamp)
    d_e <- clamp_direction(P[n, ] - P[n - 1, ], fr$E - fr$S, opts$tangent_clamp)
    moved <- FALSE
    if (separate) {
      # the arrival direction is the tangent into the true target, which is
      # the end tangent when the edge runs to `E` and the reverse of the
      # departure tangent when it runs to `S`
      chord_in <- if (head_end == "E") fr$u else -fr$u
      d_raw <- if (head_end == "E") d_e else -d_s
      prefer <- if (is.na(side)) {
        sign(signed_angle(chord_in, d_raw))
      } else if (head_end == "E") {
        -side
      } else {
        side
      }
      d_sep <- separate_arrival(
        d_raw,
        arrivals,
        theta_min,
        chord_in,
        arrival_window,
        prefer
      )
      phi_star <- signed_angle(chord_in, d_sep)
      moved <- any(d_sep != d_raw)
      # a bearing past the tangent clamp is one only the wide window could
      # have given, and the narrow window's answer is worked out again to
      # compare the two curves; a wide window that changed the answer
      # without leaving the clamp draws no hook and is left alone
      widened <- abs(phi_star) > opts$tangent_clamp + 1e-9
      if (widened) {
        d_narrow <- separate_arrival(
          d_raw,
          arrivals,
          theta_min,
          chord_in,
          opts$tangent_clamp,
          prefer
        )
      }
      if (head_end == "E") {
        d_e <- d_sep
      } else {
        d_s <- -d_sep
      }
    }
    B <- catmull_rom_beziers(
      P,
      opts$alpha,
      d_s,
      d_e,
      arm_min,
      opts$arm_fraction
    )
    pts <- sample_beziers(B, opts$sample_spacing, opts$sample_min_n)
    if (separate && moved) {
      arms <- attr(B, "arms")
      # only the control point on the arrival tangent moves, so a pass is
      # remembered as the direction that puts it there and the samples it
      # draws, and only the segment it belongs to is sampled again
      ctrl_seg <- if (head_end == "E") length(B) else 1L
      ctrl_row <- if (head_end == "E") 3L else 2L
      end_pt <- if (head_end == "E") P[n, ] else P[1, ]
      arm <- if (head_end == "E") arms[[2]] else arms[[1]]
      arrival_ctrl <- function(d) end_pt - arm * d
      end_samples <- function(B) {
        bezier_sample_counts(
          B[ctrl_seg],
          opts$sample_spacing,
          opts$sample_min_n
        )
      }
      draw <- function(pts, B, n_old) {
        resample_end(
          pts,
          B,
          n_old,
          head_end,
          opts$sample_spacing,
          opts$sample_min_n
        )
      }
      state <- function(pts) {
        arrival_state(pts, fr, cap, arrivals, phi_star, theta_min, head_end)
      }
      # the drawn part of the arrival arm: its `n_arm` samples in order of
      # approach, less the last `cap` the arrow layer resects and draws
      # nothing of
      drawn_arm <- function(pts, n_arm) {
        N <- length(pts$x)
        rows <- if (head_end == "E") {
          seq.int(N - n_arm + 1L, N)
        } else {
          rev(seq_len(n_arm))
        }
        x <- pts$x[rows]
        y <- pts$y[rows]
        to_target <- rev(cumsum(rev(c(sqrt(diff(x)^2 + diff(y)^2), 0))))
        drawn <- to_target >= cap
        list(x = x[drawn], y = y[drawn])
      }
      arm_depth <- function(arm) {
        if (length(zone) == 0) {
          return(0)
        }
        head_zone_depth(
          arm$x,
          arm$y,
          zone_x,
          zone_y,
          zone_x2,
          zone_y2,
          zone_lim
        )
      }
      # `resample_end()` replaces the arm's samples, so what the new arm
      # holds is what the path gained plus what it replaced
      arm_count <- function(pts, n_before, n_old) {
        length(pts$x) - n_before + n_old
      }

      # The wider window is a window on the bearing, not a licence to draw
      # the curve through a neighbour's arrowhead: a squeeze deep enough to
      # open it keeps the bearing the narrow window gave whenever the wider
      # one reaches further into the head zones.
      d_arr <- d_sep
      # the count the samples carry for the arm, read before the control
      # point moves, is what says which of them to replace
      n_arm <- end_samples(B)
      if (widened) {
        wide_depth <- arm_depth(drawn_arm(pts, n_arm))
        wide_pts <- pts
        wide_ctrl <- B[[ctrl_seg]][ctrl_row, ]
        n_before <- length(pts$x)
        B[[ctrl_seg]][ctrl_row, ] <- arrival_ctrl(d_narrow)
        pts <- draw(pts, B, n_arm)
        n_narrow <- arm_count(pts, n_before, n_arm)
        if (
          wide_depth <= arm_depth(drawn_arm(pts, n_narrow)) + opts$verify_tol
        ) {
          B[[ctrl_seg]][ctrl_row, ] <- wide_ctrl
          pts <- wide_pts
        } else {
          d_arr <- d_narrow
          n_arm <- n_narrow
          phi_star <- signed_angle(chord_in, d_narrow)
        }
      }

      # the arrival is never left worse separated than the pass before it,
      # so a loop that overshoots cannot cost the picture
      st <- state(pts)
      start <- list(gap = st$gap, pts = pts, dir = d_arr, n = n_arm)
      keep <- start
      loop_prefer <- if (is.na(side)) {
        0
      } else if (head_end == "E") {
        -side
      } else {
        side
      }
      for (pass in 1:4) {
        if (st$turn == 0) {
          break
        }
        d_new <- keep_side(
          clamp_direction(rotate(d_arr, st$turn), chord_in, opts$head_clamp),
          chord_in,
          loop_prefer
        )
        # a rotation the clamp and the side constraint absorb entirely
        # leaves nothing to sample again
        if (all(d_new == d_arr)) {
          break
        }
        d_arr <- d_new
        n_before <- length(pts$x)
        B[[ctrl_seg]][ctrl_row, ] <- arrival_ctrl(d_arr)
        pts <- draw(pts, B, n_arm)
        n_arm <- arm_count(pts, n_before, n_arm)
        st <- state(pts)
        if (st$gap > keep$gap + 1e-9) {
          keep <- list(gap = st$gap, pts = pts, dir = d_arr, n = n_arm)
        }
      }
      # A pass that turns the arrival tangent past the ordinary clamp draws
      # a hook, and separating an arrival is no licence to draw one across
      # a neighbour's arrowhead: the loop's result stands only while it
      # reaches no further into the head zones than the bearing it was
      # given does. A result inside the clamp is no more hooked than an
      # unseparated arrival may be, and the zones are not read for it.
      if (
        !identical(keep$dir, start$dir) &&
          abs(signed_angle(chord_in, keep$dir)) > opts$tangent_clamp + 1e-9 &&
          arm_depth(drawn_arm(keep$pts, keep$n)) >
            arm_depth(drawn_arm(start$pts, start$n)) + opts$verify_tol
      ) {
        keep <- start
      }
      pts <- keep$pts
      B[[ctrl_seg]][ctrl_row, ] <- arrival_ctrl(keep$dir)
      if (head_end == "E") {
        d_e <- keep$dir
      } else {
        d_s <- -keep$dir
      }
      separated <- separated || any(keep$dir != d_raw)
    }
    viol <- verify_clearance(
      pts,
      obstacles,
      R_vec,
      opts$verify_tol,
      cap,
      bezier_boxes(B)
    )
    total <- sum(viol$depth)
    disc_viol <- df_rows(viol, which(!capsule[viol$obstacle]))
    if (nrow(disc_viol) == 0 && nrow(viol) > 0) {
      # the discs are clear: repair the head zones while the budget lasts
      # and keep the curve that leaves them the shallowest
      if (total < best_ok_depth) {
        best_ok <- list(path = pts, wp = wp)
        best_ok_depth <- total
      }
      if (!repair || iter == opts$repair_iter) {
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
      next
    }
    viol <- disc_viol
    if (nrow(viol) == 0) {
      return(list(
        path = pts,
        wp = wp,
        clearance_ok = TRUE,
        depth = 0,
        separated = separated
      ))
    }
    if (!repair) {
      return(list(
        path = pts,
        wp = wp,
        clearance_ok = FALSE,
        depth = total,
        separated = separated
      ))
    }
    # a curve that cuts a disc is ranked by its whole violation, the discs
    # and the arrowhead zones together
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
  if (!is.null(best_ok)) {
    return(list(
      path = best_ok$path,
      wp = best_ok$wp,
      clearance_ok = TRUE,
      depth = 0,
      separated = separated
    ))
  }
  list(
    path = best$path,
    wp = best$wp,
    clearance_ok = FALSE,
    depth = best_depth,
    separated = separated
  )
}

#' Signed angle in degrees from direction `a` to direction `b`
#' @noRd
signed_angle <- function(a, b) {
  atan2(a[[1]] * b[[2]] - a[[2]] * b[[1]], sum(a * b)) * 180 / pi
}

#' Rotate a direction by `deg` degrees
#' @noRd
rotate <- function(v, deg) {
  th <- deg * pi / 180
  c(v[[1]] * cos(th) - v[[2]] * sin(th), v[[1]] * sin(th) + v[[2]] * cos(th))
}

#' Snap a direction that crossed to the wrong side of a reference onto it
#'
#' `prefer` is the sign of the admissible angles from `ref`, or 0 when
#' either side is allowed.
#'
#' @noRd
keep_side <- function(d, ref, prefer) {
  phi <- signed_angle(ref, d)
  if (prefer != 0 && sign(phi) == -prefer) ref else d
}

#' Choose an arrival direction clear of the other arrivals at a target
#'
#' Directions into the target are parametrised by their signed angle from
#' the chord direction into it. Admissible angles lie within the window and
#' on the detour's side of the chord (`prefer`, the sign of the admissible
#' angles, or 0 for either side). The candidates are the current direction,
#' the window edges, the angles `theta_min` either side of each arrival,
#' and the midpoint of each pair of arrivals adjacent in angle. The current
#' direction stands when it already keeps `theta_min` from every arrival.
#' Otherwise, among the admissible candidates that keep it from every
#' arrival, the one nearest the current direction wins, ties going to the
#' preferred side. When no candidate keeps `theta_min` from every arrival,
#' the one with the largest minimum gap wins, which in a squeeze is the
#' midpoint of the pair the arrival is caught between, ties going to the
#' smallest rotation from the current direction and then to the preferred
#' side.
#'
#' The window has two steps. The narrow one is used whenever some candidate
#' in it keeps `gap_floor` degrees from every arrival; only a squeeze deeper
#' than that opens the wide one, and the ranking there is the same. A
#' scalar `clamp` is one window, which is what the ranking's own unit pins
#' are written against.
#'
#' @param d_in Current unit direction into the target.
#' @param arrivals Two-column matrix of unit directions into the target.
#' @param chord_in Unit chord direction into the target.
#' @param clamp The window, `c(narrow, wide, gap_floor)` in degrees, or a
#'   scalar for a single window with `gap_floor` at `theta_min`.
#' @noRd
separate_arrival <- function(
  d_in,
  arrivals,
  theta_min,
  chord_in,
  clamp,
  prefer
) {
  narrow <- clamp[[1L]]
  wide <- if (length(clamp) >= 2L) clamp[[2L]] else narrow
  gap_floor <- if (length(clamp) >= 3L) clamp[[3L]] else theta_min
  cur <- signed_angle(chord_in, d_in)
  arr <- atan2(
    chord_in[[1]] * arrivals[, 2L] - chord_in[[2]] * arrivals[, 1L],
    chord_in[[1]] * arrivals[, 1L] + chord_in[[2]] * arrivals[, 2L]
  ) *
    180 /
    pi
  min_gap <- function(phi) min(abs(((arr - phi + 180) %% 360) - 180))
  if (min_gap(cur) >= theta_min - 1e-9) {
    return(d_in)
  }
  sorted <- sort(arr)
  mids <- if (length(sorted) > 1L) {
    (sorted[-1L] + sorted[-length(sorted)]) / 2
  } else {
    numeric()
  }
  window <- function(limit) {
    lo <- if (prefer > 0) 0 else -limit
    hi <- if (prefer < 0) 0 else limit
    cands <- c(cur, lo, hi, arr + theta_min, arr - theta_min, mids)
    cands <- cands[cands >= lo - 1e-9 & cands <= hi + 1e-9]
    list(cands = cands, gaps = vapply(cands, min_gap, numeric(1)))
  }
  set <- window(narrow)
  if (wide > narrow && max(set$gaps) < gap_floor - 1e-9) {
    set <- window(wide)
  }
  cands <- set$cands
  gaps <- set$gaps
  ok <- which(gaps >= theta_min - 1e-9)
  turn <- round(abs(cands - cur), 9)
  off_side <- sign(cands) != prefer
  best <- if (length(ok) > 0) {
    ok[order(turn[ok], off_side[ok])][[1L]]
  } else {
    order(-round(gaps, 9), turn, off_side)[[1L]]
  }
  phi <- cands[[best]]
  rotate(chord_in, phi)
}

#' The sampled arrival, measured once per pass
#'
#' Reads the direction of the sampled curve into its true target at `cap`
#' before it, and reports both the least angle to any arrival (`gap`) and
#' the rotation that would put that direction on `phi_star`, the bearing
#' `separate_arrival()` chose for it (`turn`). The loop drives the sampled
#' arrival to the bearing it was given rather than to a target re-derived
#' from whichever rival is nearest on the pass, so the objective is fixed
#' and the iteration cannot ping-pong between two rivals.
#'
#' `turn` is zero when the sampled arrival already keeps `theta_min` from
#' every rival, so an arrival that needs no separation is left where the
#' unrotated curve puts it, and zero again when the step is under half a
#' degree and would not earn a resample.
#'
#' @param phi_star The chosen arrival bearing, signed from the chord.
#' @noRd
arrival_state <- function(
  pts,
  fr,
  cap,
  arrivals,
  phi_star,
  theta_min,
  head_end
) {
  if (head_end == "E") {
    q <- arc_point_before_end(pts$x, pts$y, cap)
    target <- fr$E
    chord_in <- fr$u
  } else {
    q <- arc_point_before_end(rev(pts$x), rev(pts$y), cap)
    target <- fr$S
    chord_in <- -fr$u
  }
  own <- target - q
  l <- sqrt(sum(own^2))
  if (l == 0) {
    return(list(gap = Inf, turn = 0))
  }
  own <- own / l
  gap <- atan2(
    arrivals[, 1L] * own[[2]] - arrivals[, 2L] * own[[1]],
    arrivals[, 1L] * own[[1]] + arrivals[, 2L] * own[[2]]
  ) *
    180 /
    pi
  least <- min(abs(gap))
  if (least >= theta_min - 0.5) {
    return(list(gap = least, turn = 0))
  }
  turn <- ((phi_star - signed_angle(chord_in, own) + 180) %% 360) - 180
  list(gap = least, turn = if (abs(turn) <= 0.5) 0 else turn)
}

#' The point `d` mm of arc before the end of a polyline
#'
#' The first point when the polyline is shorter than `d`.
#'
#' @noRd
arc_point_before_end <- function(x, y, d) {
  n <- length(x)
  seg <- sqrt(diff(x)^2 + diff(y)^2)
  a <- rev(cumsum(rev(c(seg, 0))))
  if (a[[1]] <= d) {
    return(c(x[[1]], y[[1]]))
  }
  k <- max(which(a >= d))
  if (k >= n) {
    return(c(x[[n]], y[[n]]))
  }
  f <- (a[[k]] - d) / (a[[k]] - a[[k + 1L]])
  c(x[[k]] + f * (x[[k + 1L]] - x[[k]]), y[[k]] + f * (y[[k + 1L]] - y[[k]]))
}

# Arrowhead zones ------------------------------------------------------------------

#' The arrowhead zone of every edge as currently drawn
#'
#' The arrow layer resects the last `cap` of each path, so the drawn head of
#' an edge occupies the arc from `2 cap` to `cap` before its true target.
#' One entry per edge in parallel vectors: the far end of that arc (`x`,
#' `y`), its near end (`x2`, `y2`), the unit direction of travel into the
#' target over the last `cap` (`ux`, `uy`), and whether the edge has a head
#' at all (`valid`, false for a chord of zero length). Paths are given in
#' true orientation, source to target.
#'
#' @param paths The current paths, one `data.frame(x, y)` per edge.
#' @noRd
head_registry <- function(paths, cap) {
  n <- length(paths)
  reg <- list(
    x = numeric(n),
    y = numeric(n),
    x2 = numeric(n),
    y2 = numeric(n),
    ux = numeric(n),
    uy = numeric(n),
    valid = logical(n)
  )
  for (e in seq_len(n)) {
    reg <- register_head(reg, e, paths[[e]]$x, paths[[e]]$y, cap)
  }
  reg
}

#' Record the arrowhead zone of one edge from its path in true orientation
#' @noRd
register_head <- function(reg, e, x, y, cap) {
  n <- length(x)
  far <- arc_point_before_end(x, y, 2 * cap)
  near <- arc_point_before_end(x, y, cap)
  u <- c(x[[n]], y[[n]]) - near
  l <- sqrt(sum(u^2))
  reg$x[[e]] <- far[[1]]
  reg$y[[e]] <- far[[2]]
  reg$x2[[e]] <- near[[1]]
  reg$y2[[e]] <- near[[2]]
  reg$valid[[e]] <- n >= 2 && l > 0
  if (reg$valid[[e]]) {
    reg$ux[[e]] <- u[[1]] / l
    reg$uy[[e]] <- u[[2]] / l
  }
  reg
}

#' The head zones and co-arrivals that constrain one edge's route
#'
#' The head zones of the other edges are capsule obstacles for a detour and,
#' through `head_hits()`, their drawn heads are soft obstacles for a chord,
#' except those of edges into the same target (their arrivals are
#' separated instead), of edges into the edge's own source (they meet at
#' the port it leaves through), and of edges between the same two nodes.
#' The arrivals are the directions into the true target of the other edges
#' that end there.
#'
#' @return A list with `heads`, a data frame of capsules with `x`, `y`,
#'   `x2`, `y2` and the unit direction `ux`, `uy` into the target, and
#'   `arrivals`, a two-column matrix of unit directions.
#' @noRd
head_constraints <- function(reg, e, from, to) {
  source <- from[[e]]
  target <- to[[e]]
  twin <- (from == source & to == target) | (from == target & to == source)
  other <- reg$valid & !twin
  zone <- which(other & to != target & to != source)
  arriving <- which(other & to == target)
  list(
    heads = df_cols(
      x = reg$x[zone],
      y = reg$y[zone],
      x2 = reg$x2[zone],
      y2 = reg$y2[zone],
      ux = reg$ux[zone],
      uy = reg$uy[zone]
    ),
    arrivals = cbind(reg$ux[arriving], reg$uy[arriving])
  )
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
#' endpoint lies strictly on `side`, more than `R` from the chord line. In
#' spline mode the crossing price saturates when `opts$crossing_saturation`
#' is set, `16 (2 - 2^(1 - c))` for `c` crossings, so the first crossing
#' costs 16 and each further one half of the last, and the chain pays
#' `head_penalty` for every arrowhead zone of another edge it passes
#' within `sep_e` of (`chain_head_intrusions()`). Orthogonal mode prices
#' crossings linearly and has no head term.
#'
#' @param fr Edge frame.
#' @param wp Waypoints with `x`, `y`.
#' @param ectx Per-edge context from `edge_cost_context()`.
#' @param spline Whether the spline terms apply.
#' @noRd
side_cost <- function(
  fr,
  wp,
  side,
  displacement,
  ectx,
  placed,
  opts,
  spline = FALSE
) {
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
  cross_cost <- if (spline && isTRUE(opts$crossing_saturation)) {
    opts$crossing_penalty * (2 - 2^(1 - crossings))
  } else {
    opts$crossing_penalty * crossings
  }
  heads <- if (spline && !is.null(ectx$heads) && nrow(ectx$heads) > 0) {
    opts$head_penalty *
      chain_head_intrusions(poly, ectx$heads, ectx$cap, opts$sep_e)
  } else {
    0
  }
  cross_cost +
    opts$displacement_weight * displacement / opts$r_ref +
    opts$congestion_penalty * congestion +
    heads
}

#' Count the arrowhead zones a candidate chain passes through
#'
#' The first and last legs of the chain are trimmed by `cap`, the length
#' the arrow layer resects, since ink inside a cap is never drawn. Every
#' remaining leg is then tested against every head zone at once: the
#' distance between a leg and a zone is the least of the four point to
#' segment distances between their endpoints, and zero when the two
#' segments cross. A zone closer than `sep_e` to any leg counts once.
#'
#' @param poly Two-column matrix of chain points, source to target.
#' @param heads Head zones with `x`, `y` (the far end) and `x2`, `y2` (the
#'   near end, `cap` before the target).
#' @noRd
chain_head_intrusions <- function(poly, heads, cap, sep_e) {
  n <- nrow(poly)
  trim <- function(a, b) {
    d <- b - a
    l <- sqrt(sum(d^2))
    if (l <= cap) b else a + d / l * cap
  }
  poly[1, ] <- trim(poly[1, ], poly[2, ])
  poly[n, ] <- trim(poly[n, ], poly[n - 1, ])
  legs <- n - 1L
  m <- nrow(heads)
  i <- rep(seq_len(legs), each = m)
  k <- rep.int(seq_len(m), legs)
  ax <- poly[i, 1L]
  ay <- poly[i, 2L]
  bx <- poly[i + 1L, 1L]
  by <- poly[i + 1L, 2L]
  cx <- heads$x[k]
  cy <- heads$y[k]
  dx <- heads$x2[k]
  dy <- heads$y2[k]
  d <- pmin(
    dist_to_edge(cx, cy, ax, ay, bx, by),
    dist_to_edge(dx, dy, ax, ay, bx, by),
    dist_to_edge(ax, ay, cx, cy, dx, dy),
    dist_to_edge(bx, by, cx, cy, dx, dy)
  )
  d1 <- (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
  d2 <- (bx - ax) * (dy - ay) - (by - ay) * (dx - ax)
  d3 <- (dx - cx) * (ay - cy) - (dy - cy) * (ax - cx)
  d4 <- (dx - cx) * (by - cy) - (dy - cy) * (bx - cx)
  d[d1 * d2 < 0 & d3 * d4 < 0] <- 0
  # the closest approach of each zone over the legs
  nearest <- d[seq_len(m)]
  for (leg in seq_len(legs)[-1L]) {
    nearest <- pmin(nearest, d[(leg - 1L) * m + seq_len(m)])
  }
  sum(nearest < sep_e)
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
#' priced as if it crossed one edge. A waypoint in a tight slot is kept
#' even when it lies on the chord and the hull would drop it, since it pins
#' the curve through the gap; a candidate through a tight slot is flagged
#' `tight`, priced `tight_penalty`, and routed at the soft margin. A
#' periphery arch is then levelled: every surviving waypoint rises to the
#' outermost one, so the apex sits mid-span rather than over the tallest
#' stack and the arch climbs as steeply as it descends; the levelled chain
#' is spread and hulled again. Candidates are priced by `side_cost()` on
#' the displacement the stacks require, measured before levelling, and on
#' the crossings of the chain as drawn. A candidate whose waypoints sit
#' deeper off the chord than `sagitta_max_spanning` allows ranks below
#' every candidate that meets the cap, whatever it costs; under an unset
#' cap no candidate is over it and the ranking is the cost order alone.
#'
#' @param bounds Panel bounds.
#' @param reserved Reservations for this edge from `slot_reservations()`,
#'   or `NULL`.
#' @param base The layers' base free intervals that the occupancy is
#'   indexed by; `ints` unless the edge carries an extra margin.
#' @return `NULL` when every candidate has a slot with no free y; otherwise
#'   the candidates ranked best first, each a list with `wp`, `side`,
#'   `scope`, `disordered`, `overlap`, `over_cap`, and `ints`, the free
#'   intervals the candidate was placed in (clipped for a periphery
#'   candidate).
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

  # the cap as a perpendicular distance off this chord, and the offset of a
  # waypoint chain in the same frame the drawn sagitta is measured in
  cap_mm <- (opts$sagitta_max_spanning %||% Inf) * fr$Lc
  chain_depth <- function(wp) {
    offset <- (wp$x - fr$S[[1]]) * fr$n[[1]] + (wp$y - fr$S[[2]]) * fr$n[[2]]
    max(abs(offset))
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
      tight_rows <- vapply(
        seq_along(crossed),
        function(i) in_tight_slot(use[[i]], sp$wp$y[[i]]),
        logical(1)
      )
      wp <- hull_waypoints(fr$S, sp$wp, fr$E, side)
      dropped <- which(tight_rows & !sp$wp$layer %in% wp$layer)
      if (length(dropped) > 0) {
        wp <- sort_waypoints(df_bind(wp, df_rows(sp$wp, dropped)), fr)
      }
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
      cost <- side_cost(
        fr,
        wp,
        side,
        displacement,
        ectx,
        placed,
        opts,
        spline = TRUE
      )
      if (disordered) {
        cost <- cost + opts$crossing_penalty
      }
      tight <- any(tight_rows)
      if (tight) {
        cost <- cost + opts$tight_penalty
      }
      cands[[length(cands) + 1]] <- list(
        side = side,
        scope = scope,
        wp = wp,
        cost = round(cost, opts$cost_digits),
        disordered = disordered,
        overlap = overlap,
        tight = tight,
        ints = use,
        # measured after the hull and after a periphery arch is levelled,
        # since that is the chain the curve is drawn through
        over_cap = nrow(wp) > 0 && chain_depth(wp) > cap_mm
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
  over_cap <- vapply(cands, function(c) c$over_cap, logical(1))
  cands[order(over_cap, overlap, cost, !interior, !above)]
}

#' Whether a y is the centre line of a tight slot of a layer
#' @noRd
in_tight_slot <- function(iv, y) {
  any(iv$tight & abs(iv$lo - y) < 1e-9, na.rm = TRUE)
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
      side_cost(
        fr,
        wp,
        side,
        sum(abs(wp$o)),
        ectx,
        placed,
        opts,
        spline = TRUE
      )
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

#' Sub-2 mm nudges for grazed nodes and arrowheads
#'
#' Every hit is in the soft band, so the curve only needs to move out to
#' the hit's clearance on its far side: an offset of `h - sign(h) * R`, at
#' most `m - m_min` in magnitude for a disc. Each hit carries its own
#' clearance margin in `mm` (the disc margin `m` when the column is absent),
#' and a hit that names its `side` is nudged to that side rather than to
#' the far side of its centre: a head hit is nudged away from the head's
#' target. Opposite-side nudges form an S too shallow to see.
#'
#' @noRd
soft_nudge_waypoints <- function(hits, fr, extra, opts) {
  s <- -sign(hits$h)
  s[s == 0] <- 1
  if (!is.null(hits$side)) {
    named <- !is.na(hits$side)
    s[named] <- hits$side[named]
  }
  mm <- hits$mm %||% rep(opts$m, nrow(hits))
  o <- hits$h + s * (hits$r + mm + extra)
  df_rows(bow_points(hits, fr, o, opts), order(hits$t))
}

#' The drawn arrowheads a chord passes too close to
#'
#' The drawn head of another edge is a pseudo-disc of radius `head / 2`
#' centred `cap + head / 2` before that edge's target along its current
#' path. The chord is trimmed by `cap` at both ends, since the ink inside a
#' cap is never drawn, and a head whose centre lies within `head / 2 +
#' head_margin` of the trimmed chord is a soft hit at the centre's chord
#' parameter, to be nudged away from the head's target: the side is
#' `-sign(h_target)`, `+1` when the target sits on the chord. A head is
#' never a hard hit, and a chord too short to have drawn ink between its
#' caps has no head hits.
#'
#' @param fr The edge frame.
#' @param heads The head zones from `head_constraints()`.
#' @return Hits in the frame with the columns of the disc hits (`node` is
#'   `NA`, `layer` is `NA`, `r` is `head / 2`, `hard` is `FALSE`) plus the
#'   clearance margin `mm` and the nudge `side`.
#' @noRd
head_hits <- function(fr, heads, cap, opts) {
  r_h <- opts$head / 2
  clear <- r_h + opts$head_margin
  empty <- df_cols(
    node = integer(),
    h = numeric(),
    t = numeric(),
    layer = integer(),
    r = numeric(),
    hard = logical(),
    mm = numeric(),
    side = numeric()
  )
  if (nrow(heads) == 0 || fr$Lc <= 2 * cap) {
    return(empty)
  }
  cx <- heads$x2 - r_h * heads$ux
  cy <- heads$y2 - r_h * heads$uy
  S2 <- fr$S + cap * fr$u
  E2 <- fr$E - cap * fr$u
  d <- dist_to_edge(cx, cy, S2[[1]], S2[[2]], E2[[1]], E2[[2]])
  keep <- which(d < clear)
  if (length(keep) == 0) {
    return(empty)
  }
  dx <- cx[keep] - fr$S[[1]]
  dy <- cy[keep] - fr$S[[2]]
  tx <- heads$x2[keep] + cap * heads$ux[keep] - fr$S[[1]]
  ty <- heads$y2[keep] + cap * heads$uy[keep] - fr$S[[2]]
  side <- -sign(tx * fr$n[[1]] + ty * fr$n[[2]])
  side[side == 0] <- 1
  n <- length(keep)
  df_cols(
    node = rep(NA_integer_, n),
    h = dx * fr$n[[1]] + dy * fr$n[[2]],
    t = (dx * fr$u[[1]] + dy * fr$u[[2]]) / fr$Lc,
    layer = rep(NA_integer_, n),
    r = rep(r_h, n),
    hard = rep(FALSE, n),
    mm = rep(opts$head_margin, n),
    side = side
  )
}

#' Merge head hits with the hits within reach of them along the chord
#'
#' Two soft nudges within reach of each other along the chord cannot both
#' be honoured by one curve: a head hit within a disc hit's clearance `R`
#' of it along the chord, or within twice its own clearance of another
#' head hit, is merged with that hit. The hit whose nudge takes the
#' curve further from the chord on their common side is kept; when the two
#' nudge to opposite sides the head hit gives way, whichever of the two
#' comes first along the chord: to the disc hit, and of two head hits the
#' earlier to the later. Disc hits are never merged with one another.
#'
#' @param eh The hits of one edge, ordered along the chord, with `mm` and
#'   `side`.
#' @param Lc The chord length.
#' @noRd
merge_head_hits <- function(eh, Lc) {
  n <- nrow(eh)
  if (n < 2) {
    return(eh)
  }
  is_head <- is.na(eh$node)
  s <- -sign(eh$h)
  s[s == 0] <- 1
  named <- !is.na(eh$side)
  s[named] <- eh$side[named]
  o <- eh$h + s * (eh$r + eh$mm)
  drop <- logical(n)
  for (k in which(is_head)) {
    for (j in seq_len(n)) {
      if (j == k || drop[[j]] || drop[[k]]) {
        next
      }
      reach <- if (is_head[[j]]) {
        2 * (eh$r[[k]] + eh$mm[[k]])
      } else {
        eh$r[[j]] + eh$mm[[j]]
      }
      if (abs(eh$t[[j]] - eh$t[[k]]) * Lc >= reach) {
        next
      }
      if (sign(o[[k]]) == sign(o[[j]]) && abs(o[[k]]) > abs(o[[j]])) {
        drop[[j]] <- TRUE
      } else {
        drop[[k]] <- TRUE
      }
    }
  }
  df_rows(eh, which(!drop))
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
#' curve. `job` carries the edge frame, obstacles, radii, and constants. A
#' detour is verified against the arrowhead capsules of the other edges as
#' well, at the margin of a disc (`m`, or `m_min` for a capped route), and
#' its arrival is separated from the other arrivals at its target; a soft
#' nudge is visually straight and gets neither, and is kept off the drawn
#' heads by the blocking test instead (`head_hits()`).
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
  obstacles <- job$obstacles
  R_vec <- if (soft || capped) job$R_soft else job$R_full
  arrivals <- NULL
  heads <- job$ectx$heads
  if (!soft && nrow(heads) > 0) {
    obstacles <- df_bind(
      obstacles,
      df_cols(
        name = rep(NA_character_, nrow(heads)),
        x = heads$x,
        y = heads$y,
        layer = rep(NA_integer_, nrow(heads)),
        x2 = heads$x2,
        y2 = heads$y2,
        capsule = rep(TRUE, nrow(heads))
      )
    )
    R_vec <- c(
      R_vec,
      rep(if (capped) job$opts$m_min else job$opts$m, nrow(heads))
    )
  }
  if (!soft && nrow(job$arrivals) > 0) {
    arrivals <- job$arrivals
  }
  res <- route_spline_edge(
    job$fr,
    wp,
    obstacles,
    R_vec,
    job$arm_min,
    job$opts,
    tier,
    job$layers,
    job$la,
    job$lb,
    job$bounds,
    repair = !least_bad,
    cap = job$cap,
    arrivals = arrivals,
    head_end = job$head_end,
    theta_min = job$theta_min,
    side = side
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
#'
#' A bow whose curve leaves the panel margin is infeasible rather than
#' verified: it is reported without clearance and at infinite depth, so it
#' never replaces a spanning route drawn inside the margin.
#'
#' @param fb The bow's waypoints from `free_bow_waypoints()` when the
#'   caller has them already.
#' @noRd
route_free_bow <- function(job, placed, fb = NULL) {
  fb <- fb %||%
    free_bow_waypoints(
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
  if (!res$inside) {
    res$clearance_ok <- FALSE
    res$depth <- Inf
  }
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
#' candidate applies it again. A candidate through a tight slot is verified
#' at the soft margin and reported capped, as a bow drawn at that margin
#' is.
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
  capped <- isTRUE(cand$tight)
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
  res <- route_candidate(
    job,
    wp,
    cand$side,
    cand$scope,
    "spanning",
    placed,
    capped = capped
  )
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
      placed,
      capped = capped
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
#' `sep_m * (i - (k + 1) / 2)`, in the order of its endpoints' positions:
#' the source's x, then its y, then the target's x and y, and the input
#' order for members whose endpoints coincide, which is every group of
#' duplicates. Ordering by node name instead would let two callers who name
#' one picture differently draw the copies on opposite sides.
#'
#' @return A list with `extra` and `shift`, one value per edge.
#' @noRd
parallel_groups <- function(from, to, nodes, routable, sep_m) {
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
      nodes$x[from[members]],
      nodes$y[from[members]],
      nodes$x[to[members]],
      nodes$y[to[members]],
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
#' orthogonally whether or not a node blocks its chord. A vertical chord
#' stays a two-row straight path, and so does a chord between two nodes of
#' one layer, since the layer has no gap to route it through. A level chord,
#' one tilted by no more than the corner radius `rc`, is drawn as the
#' two-row horizontal run on its target's line when it joins adjacent layers
#' or when no non-endpoint disc comes within `R` of that run: the head end
#' keeps the target's centre, the tail leaves its node through the port on
#' the target's line, and the tail's resect is the face resect of the port's
#' offset. Its mode stays `"straight"`, which means no bend rather than
#' centre to centre. Such a chord owns the centre row of the node its head
#' arrives at, on the side it arrives from: while it does, no spanning
#' candidate may run on that node's line, and where two of them reach one
#' node the chord from the farthest layer keeps the row while the others
#' bend at their slots like ordinary arrivals, so that no node is drawn two
#' heads on one row. The row pass claims the departure side as well, the E
#' centre row of the node the chord leaves, whose corner radius its tail
#' port lies within: an arrival row on that side can come within the row
#' floor of the departure when the port's offset exceeds it. A reversed
#' level chord takes the mirror shape, the tail port on the right node at
#' its target's y; its own run lies on the left node's line, whose E centre
#' row it owns, and leaves the right node's line free, although the row
#' pass claims that node's W centre row for it in the same way.
#'
#' A spanning edge chooses among the channels `ortho_channel()` prices: an
#' S or N channel `R` beyond the crossed stacks when both endpoints are the
#' extreme node of their layer on that side (two bends), an E/W run at an
#' endpoint's y or `R` beyond the stacks (two or four bends), and, when no
#' S/N channel fits, an E/W run through a free interval of every crossed
#' layer. The run at the target's own line is no candidate when a level
#' chord owns that node's row. Every other edge leaves through the E port
#' and enters through the W port with one vertical run per crossed gap at a
#' slot assigned by `ortho_slot_ranks()`. Channels are placed shortest span first, and one
#' that would run within `sep_e` of a placed channel over an overlapping
#' x-range is pushed outward past it, so longer edges nest outside shorter
#' ones and no channel is shared. A run on an endpoint line that only the
#' placed interior runs crowd slides them `sep_e` away, each pushing the
#' next in turn, when the price of their moves is less than that of the
#' pushed alternatives (`slide_channels()`); an S/N channel, a channel of
#' the edge's own hyperedge, and a channel on its own endpoint line never
#' move, so a slide that would need one is refused.
#'
#' Every drawn segment belongs to one edge unless two edges share a port.
#' Edges leaving one port form a hyperedge trunk and edges entering one port
#' merge into their last run; two segments from different sources never
#' share a slot, even when their y-intervals only meet; a node whose N (or
#' S) side carries both an arrival and a departure gives them ports
#' `sep_e / 2` on either side of its centre line, the departure toward the
#' target; the arrivals on a node's W side take rows beside its centre line;
#' and a pair of horizontal pieces in one gap that would coincide in one
#' slot order forces the other order. A path starts and ends at its ports'
#' axis points, the node's own coordinate along the port's axis carried
#' onto the port's line, so the run into an offset port is hidden under the
#' disc past the face and the head is drawn along it, never angled at the
#' centre.
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
  # the real ends of every edge. An edge drawn leftwards leaves from the
  # right-hand node and arrives at the left-hand one, so a decision that
  # belongs to a source or to a head reads these rather than `a` and `b`
  rev_e <- info$reversed
  src_node <- ifelse(rev_e, b, a)
  head_node <- ifelse(rev_e, a, b)
  src_x <- ifelse(rev_e, Tx, Sx)
  src_y <- ifelse(rev_e, Ty, Sy)
  head_y <- ifelse(rev_e, Sy, Ty)
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

  grp <- parallel_groups(from, to, nodes, routable, opts$sep_m)
  shift <- grp$shift
  extra <- grp$extra

  # a chord needs a bend when it is oblique and leaves its layer, or when it
  # is horizontal, spans a layer, and a disc in a crossed layer blocks it. A
  # level chord is drawn as the run on its target's line, so that run, not
  # the chord between the centres, is what the crossed discs must clear; a
  # run that hits nothing is already axis-aligned
  bent <- routable & span >= 1 & !vertical & !(horizontal & span <= 1)
  level <- which(bent & horizontal)
  if (length(level) > 0) {
    blocked <- level_run_blocked(
      nodes,
      a[level],
      b[level],
      head_y[level],
      R_node
    )
    bent[level[!blocked]] <- FALSE
  }
  kind <- rep("straight", n_edges)
  kind[bent] <- "ew"
  kind[
    routable & kind == "straight" & (horizontal | vertical) & shift != 0
  ] <- "detour"

  # one owner per centre row. A level chord is drawn on its head end's
  # line and its head sits on that node's centre, so nothing else may
  # arrive there: where two of them reach one node one keeps the row and
  # the others bend at their slots like ordinary arrivals. Which way a
  # chord is drawn does not change the node its head reaches, so the row
  # belongs to the head end whichever direction the chord runs. Two level
  # chords reach one node from one layer, since a chord from a farther
  # layer passes within R of the nearer source and a blocked run bends, so
  # what decides between them is the source's position. The chords that
  # remain own their head ends' lines, and no spanning candidate may take
  # one
  chords <- which(
    kind == "straight" & routable & horizontal & !vertical & span >= 1L
  )
  for (t in unique(head_node[chords])) {
    idx <- chords[head_node[chords] == t]
    if (length(idx) < 2L) {
      next
    }
    order_e <- order(
      -span[idx],
      src_x[idx],
      src_y[idx],
      idx,
      method = "radix"
    )
    kind[idx[order_e][-1L]] <- "ew"
  }
  level_owned <- unique(head_node[
    kind == "straight" & routable & horizontal & !vertical & span >= 1L
  ])

  # the horizontal pieces committed in every gap so far: an edge with a
  # vertical run in a gap enters it at one y and leaves at another; pieces
  # leaving one source port belong to one hyperedge segment. The gap beside
  # the source is the edge's first gap when it is drawn rightwards and its
  # last when it is drawn leftwards; an arrival at a node's side is not a
  # piece leaving that node's port, so it takes the edge's own key. As a
  # piece key the choice can only split a group or merge two, which
  # `pieces_coincide()` cannot see, since it asks for a mutual match
  # inside one group; what it decides is which channels
  # `slide_channels()` reads as one hyperedge
  n_gaps <- max(layers$n - 1L, 0L)
  pieces <- rep(list(empty_pieces()), n_gaps)
  seg_key <- function(e, first) {
    if (span[[e]] == 1L || xor(first, rev_e[[e]])) {
      paste0("s", src_node[[e]])
    } else {
      paste0("e", a[[e]], "-", b[[e]])
    }
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
  # the pieces of the span-1 edges alone; a slide rebuilds the rest on them
  pieces_base <- pieces
  intervals <- lapply(seq_len(layers$n), function(k) {
    layer_free_intervals(
      df_rows(nodes, layers$members[[k]]),
      opts$m,
      bounds,
      opts$pad,
      opts$sep_e,
      m_min = NULL
    )
  })

  # channels of spanning edges, shortest first so that a longer edge nests
  # outside the channels already placed, priced against the chords of the
  # edges not yet placed and the channels of those already placed; ties on
  # span and length are broken by the endpoint positions, never by name.
  # Every placed channel keeps a record of what `slide_channels()` needs
  # to move it later: an edge whose own endpoint line is clear of
  # everything but the placed channels has them slide out of its way when
  # that costs less than the pushed alternatives
  side <- rep(NA_real_, n_edges)
  y_ch <- rep(NA_real_, n_edges)
  clamped <- logical(n_edges)
  placed <- placed_set(paths)
  channels <- df_cols(
    e = integer(0),
    side = numeric(0),
    y = numeric(0),
    lo = numeric(0),
    hi = numeric(0),
    key = character(0),
    fixed = logical(0)
  )
  records <- list()
  # the head zones of the S/N channels placed so far: the band each stub's
  # arrowhead occupies, plus its margin, which no later channel run enters
  zones <- empty_head_zones()
  ctx <- list(nodes = nodes, from = from, to = to, Lc = info$Lc)
  gap_mid <- (layers$x[-layers$n] + layers$x[-1]) / 2

  # the cheapest slide among the crowded endpoint lines `ortho_channel()`
  # priced for edge `e`: an option's total is its own price plus that of
  # the cheaper feasible direction, ties above, and the best option is
  # taken when its total is below the cost of the best pushed candidate,
  # which is infinite when that candidate had to be clamped
  best_slide <- function(ch, e, keys, src_key) {
    best <- NULL
    best_total <- ch$cost
    for (p in ch$slide) {
      cand_pieces <- list(
        list(g = info$la[[e]], key = keys[[1]], left = Sy[[e]], right = p$y),
        list(
          g = info$lb[[e]] - 1L,
          key = keys[[2]],
          left = p$y,
          right = Ty[[e]]
        )
      )
      sl <- NULL
      for (dir in c(1, -1)) {
        s <- slide_channels(
          records,
          p$conflicts,
          p$y,
          dir,
          opts$sep_e,
          nodes,
          R_node,
          pieces_base,
          cand_pieces,
          opts,
          src_key,
          gap_mid
        )
        if (!is.null(s) && (is.null(sl) || s$cost < sl$cost)) {
          sl <- s
        }
      }
      if (is.null(sl)) {
        next
      }
      total <- round(p$cost + sl$cost, opts$cost_digits)
      if (total < best_total) {
        best_total <- total
        best <- c(sl, list(option = p))
      }
    }
    best
  }

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
    keys <- c(seg_key(e, TRUE), seg_key(e, FALSE))
    # the key of the piece this edge draws in the gap beside its source,
    # which is its first gap drawn rightwards and its last drawn leftwards
    src_key <- seg_key(e, !rev_e[[e]])
    ch <- ortho_channel(
      fr,
      edge_cost_context(fr, e, ctx),
      e,
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
      keys,
      head_node[[e]] %in% level_owned,
      head_y[[e]],
      opts,
      zones
    )
    sl <- if (length(ch$slide) > 0) best_slide(ch, e, keys, src_key) else NULL
    if (is.null(sl)) {
      if (ch$kind == "ew") {
        pieces <- add_piece(pieces, info$la[[e]], keys[[1]], Sy[[e]], ch$y)
        pieces <- add_piece(pieces, info$lb[[e]] - 1L, keys[[2]], ch$y, Ty[[e]])
      }
    } else {
      # the moved channels take their new lines wherever they are
      # registered, and the edge takes its own line, unclamped
      for (k in sl$moved) {
        r <- records[[k]]
        records[[k]]$y <- sl$y[[k]]
        channels$y[[k]] <- sl$y[[k]]
        y_ch[[r$e]] <- sl$y[[k]]
        placed <- place_edge(
          placed,
          r$e,
          c(
            r$fr$S[[1]],
            gap_mid[c(r$la, r$la, r$lb - 1L, r$lb - 1L)],
            r$fr$E[[1]]
          ),
          c(r$fr$S[[2]], r$Sy, sl$y[[k]], sl$y[[k]], r$Ty, r$fr$E[[2]])
        )
      }
      pieces <- sl$pieces
      fields <- c("kind", "side", "y", "wp", "clamped", "channel")
      ch[fields] <- sl$option[fields]
    }
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
    records[[length(records) + 1L]] <- list(
      e = e,
      kind = ch$kind,
      side = ch$side,
      y = ch$y,
      xr = c(ch$channel$lo, ch$channel$hi),
      la = info$la[[e]],
      lb = info$lb[[e]],
      keys = keys,
      src_key = src_key,
      Sy = Sy[[e]],
      Ty = Ty[[e]],
      owned = head_node[[e]] %in% level_owned,
      owned_y = head_y[[e]],
      fr = fr,
      state = ch$state
    )
    if (ch$kind == "sn") {
      zones <- df_bind(
        zones,
        head_zone(nodes$x[[to[[e]]]], nodes$y[[to[[e]]]], ch$side, cap, opts)
      )
    }
  }

  # N and S ports: two channel stubs on one side of a node sit sep_e / 2
  # either side of the centre line, an arrival on the left of a departure,
  # two arrivals in ascending and two departures in descending `s * y_ch`.
  # That is the crossing-free order on either side, and the mirror of the
  # one on the other: the inner channel of a pair keeps the stub on the
  # side its own run lies on and the outer channel takes the far stub, so
  # the outer never has to cross the inner's run to reach its port. More
  # than two share: the arrivals take the left stub and the departures the
  # right
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
        dep <- dep[order(-s * y_ch[dep], Sy[dep], b[dep], method = "radix")]
        arr <- arr[order(s * y_ch[arr], Sy[arr], a[arr], method = "radix")]
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
  # an edge arrives through its last gap, or its only one; the row it may
  # take at its target depends on that gap alone
  arrival_narrow <- logical(n_edges)
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
      tol,
      rev_e
    )
    direction <- vapply(
      segs$members,
      function(m) ortho_target_side(ifelse(info$reversed[m], -1L, 1L)),
      integer(1)
    )
    pos <- ortho_slot_positions(
      segs,
      c(layers$x[[g]], layers$x[[g + 1L]]),
      opts,
      cap,
      direction
    )
    for (k in seq_along(segs$members)) {
      m <- segs$members[[k]]
      is_first <- segs$member_first[[k]]
      slot_first[m[is_first]] <- pos$x[[k]]
      slot_last[m[!is_first]] <- pos$x[[k]]
      if (pos$narrow) {
        narrow[m] <- TRUE
        if (!pos$floored) {
          # an edge arrives through the gap beside its head: its first gap
          # when it runs leftwards or stays within one gap, its last one
          # otherwise
          head_first <- info$reversed[m] | span[m] == 1L
          arrival_narrow[m[is_first == head_first]] <- TRUE
        }
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

  # W and E ports: a row belongs to the head end of an edge, so the W side
  # of a node holds the heads of the edges that reach it from the left and
  # its E side those of the edges that reach it from the right, and each
  # side runs the rule over its own arrivals. The level chords into a node,
  # straight or running at the node's own line, own the centre row of the
  # side their head is on, and so does a departure through a side, which
  # leaves from the node's centre: an edge drawn rightwards departs its
  # source's E side and one drawn leftwards its source's W side. Beside an
  # owner the other arrivals take rows above and below it in the order of
  # their slots, the slot farthest from the target nearest the centre,
  # which is the crossing-free order. Without an owner the rows are centred
  # on the node's centre line, the arrivals from
  # above on the upper rows and those from below on the lower ones, each
  # group in that same order, so a lone arrival takes the centre and a pair
  # straddles it. A stack keeps its rows while they are at least
  # max(sep_e / 2, sep_min) apart within the height the head fits in;
  # otherwise each group merges onto one row, at its first offset beside an
  # owner and at +- sep_e / 2 (or the centre, for one group alone) without
  # one, and the ladder keeps the joins clear of the stub. A merged row the
  # floor still cannot hold collapses onto the centre row: the height a head
  # fits in, h = r - head_w / 2, shrinks with the node radius faster than the
  # floor does and turns negative under r = head_w / 2, so h is floored at 0
  # and a stack that cannot be spread is drawn on the node's own line rather
  # than a fraction of a millimetre from it or on the wrong side of it. The
  # copies of a parallel bundle are spread sep_m apart already, so they keep
  # the centre row. So does an arrival out of a gap too narrow for any stub,
  # unless the gap is floored: the slot nearest the target leaves a whole
  # head run before the target's layer, and that run holds a row as well as
  # a head. Only the arrival gap counts, so an edge that crosses a narrow
  # gap early and arrives through a wide or floored one takes a row like any
  # other
  port_y <- numeric(n_edges)
  row_floor <- max(opts$sep_e / 2, opts$sep_min)
  via_last <- span >= 2
  arrival_slot <- ifelse(via_last & !rev_e, slot_last, slot_first)
  arrival_entry <- ifelse(via_last, y_ch, ifelse(rev_e, Ty, Sy))
  # the slots are read from the source's side of the arrival's own gap and
  # the ties from the source's position, so a scene drawn leftwards takes
  # the mirror image of the rows the same scene drawn rightwards takes
  slot_order <- ifelse(rev_e, -1, 1) * arrival_slot
  arrival <- kind == "ew" &
    !is.na(arrival_slot) &
    shift == 0 &
    !arrival_narrow
  level_chord <- kind == "straight" & routable & horizontal & !vertical
  own_line <- kind == "ew" &
    via_last &
    is.na(ifelse(rev_e, slot_first, slot_last))
  owner_w <- level_chord | (kind == "ew" & rev_e) | (own_line & !rev_e)
  owner_e <- level_chord | (kind == "ew" & !rev_e) | (own_line & rev_e)
  for (east in c(FALSE, TRUE)) {
    side_arrival <- arrival & rev_e == east
    side_node <- if (east) a else b
    owner <- if (east) owner_e else owner_w
    for (t in unique(side_node[side_arrival])) {
      idx <- which(side_arrival & side_node == t)
      idx <- idx[order(
        slot_order[idx],
        src_y[idx],
        src_node[idx],
        method = "radix"
      )]
      above <- idx[arrival_entry[idx] > head_y[idx]]
      below <- idx[arrival_entry[idx] <= head_y[idx]]
      ka <- length(above)
      kb <- length(below)
      h <- max(nodes$r[[t]] - opts$head_w / 2, 0)
      if (any(owner & side_node == t)) {
        mult_a <- seq_len(ka)
        mult_b <- seq_len(kb)
        s <- min(opts$sep_e, h / max(ka, kb))
        if (s < row_floor) {
          mult_a <- rep(1, ka)
          mult_b <- rep(1, kb)
          s <- min(opts$sep_e, h)
        }
        if (s < row_floor) {
          s <- 0
        }
        port_y[above] <- mult_a * s
        port_y[below] <- -mult_b * s
        next
      }
      n <- ka + kb
      s <- if (n >= 2) min(opts$sep_e, 2 * h / (n - 1)) else 0
      rows_a <- rev(seq_len(ka))
      rows_b <- ka + seq_len(kb)
      if (n >= 2 && s < row_floor) {
        n <- (ka > 0) + (kb > 0)
        s <- if (n == 2) min(opts$sep_e, 2 * h) else 0
        if (s < row_floor) {
          s <- 0
        }
        rows_a <- rep(1, ka)
        rows_b <- rep(n, kb)
      }
      row_at <- function(j) ((n + 1) / 2 - j) * s
      port_y[above] <- row_at(rows_a)
      port_y[below] <- row_at(rows_b)
    }
  }

  # polylines: port, bends, port; then corners and sampling. A path starts
  # and ends at its ports' axis points, so the run into an offset port lies
  # on the port's own line, hidden under the disc past the face, and the
  # head the arrow layer aims at the path's end is drawn along that run.
  # The resect of a ported end puts its tip cap - r past the disc face on
  # that run: exactly cap at a centre port, less as the offset grows
  resect_head <- rep(cap, n_edges)
  resect_fins <- rep(cap, n_edges)
  face_resect <- function(r, off) cap - r + sqrt(max(r^2 - off^2, 0))

  # a level chord is drawn as the run on its target's line: the head end
  # keeps the target's centre and the tail leaves its node through the port
  # on that line, so the run is exactly horizontal and the tail's resect is
  # the face resect of the port's offset. A chord with no offset at all is
  # that run already and keeps the chord it came in with
  level <- which(
    kind == "straight" & routable & horizontal & !vertical & span >= 1L
  )
  for (e in level) {
    off <- Ty[[e]] - Sy[[e]]
    if (abs(off) < 1e-12) {
      next
    }
    y_run <- head_y[[e]]
    path <- df_cols(x = c(Sx[[e]], Tx[[e]]), y = c(y_run, y_run))
    if (info$reversed[[e]]) {
      path <- df_cols(x = rev(path$x), y = rev(path$y))
    }
    paths[[e]] <- path
    tail <- src_node[[e]]
    resect_fins[[e]] <- face_resect(nodes$r[[tail]], off)
  }

  for (e in which(kind != "straight" & !is_fixed)) {
    fr <- edge_frame(nodes, from[[e]], to[[e]], info$reversed[[e]])
    geom <- ortho_bends(
      kind[[e]],
      fr$S,
      fr$E,
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
      if (info$reversed[[e]]) port_y[[e]] else 0,
      if (info$reversed[[e]]) 0 else port_y[[e]],
      tol
    )
    if (is.null(geom$bends)) {
      next
    }
    at_s <- face_resect(nodes$r[[a[[e]]]], geom$off_s)
    at_e <- face_resect(nodes$r[[b[[e]]]], geom$off_t)
    resect_head[[e]] <- if (info$reversed[[e]]) at_s else at_e
    resect_fins[[e]] <- if (info$reversed[[e]]) at_e else at_s

    # a span-1 arrival whose source lies within rc of its row cannot show
    # two proper corners, so it is the run on the row's line from the tail's
    # port on that line to the row's axis point, as a level chord is the run
    # on its target's line, and the slot it was assigned goes unused. The
    # row is at the head's end of the frame, which is its right end when the
    # edge runs rightwards and its left end when it runs leftwards, and the
    # tail's resect is the face resect of its port's offset from that line
    row_end <- if (info$reversed[[e]]) geom$port_s else geom$port_t
    y_src <- if (info$reversed[[e]]) fr$E[[2]] else fr$S[[2]]
    if (
      kind[[e]] == "ew" &&
        span[[e]] == 1L &&
        abs(y_src - row_end[[2]]) <= rc_used + 1e-9
    ) {
      y_run <- row_end[[2]]
      tail <- src_node[[e]]
      tail_off <- y_run - y_src
      path <- df_cols(
        x = c(fr$S[[1]], geom$port_t[[1]]),
        y = c(y_run, y_run)
      )
      if (info$reversed[[e]]) {
        path <- df_cols(x = rev(path$x), y = rev(path$y))
      }
      paths[[e]] <- path
      resect_fins[[e]] <- face_resect(nodes$r[[tail]], tail_off)
      next
    }

    # the bends are the turns between the ports
    poly <- drop_collinear(dedupe_points(rbind(
      geom$port_s,
      geom$bends,
      geom$port_t
    )))
    bends <- poly[-c(1L, nrow(poly)), , drop = FALSE]
    if (nrow(bends) == 0) {
      next
    }
    pts <- if (opts$corners == "rounded") round_corners(poly, rc_used) else poly
    pts <- dedupe_points(sample_runs(pts, opts$sample_spacing))

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

#' The head zone of a placed S/N channel
#'
#' The stub of an S/N channel rises from its head node's centre to the
#' channel's run, and the drawn arrowhead stands on it from `cap` to
#' `cap + head` past the centre. Its zone is that band with `head_margin`
#' at each end, the room the router leaves behind a head base everywhere
#' else, over the stub's x give or take `sep_e / 2`: the space a channel run
#' has to keep out of to leave the head whole.
#'
#' @param x,y The head node's centre.
#' @param s The channel's side, `1` for a stub above the node and `-1` for
#'   one below.
#' @param cap Edge cap in mm.
#' @param opts Routing constants from `route_constants()`.
#' @return A one-row frame with `x`, `lo`, `hi`, and `s`.
#' @noRd
head_zone <- function(x, y, s, cap, opts) {
  band <- sort(
    y + s * c(cap - opts$head_margin, cap + opts$head + opts$head_margin)
  )
  df_cols(x = x, lo = band[[1]], hi = band[[2]], s = s)
}

#' No head zones
#' @noRd
empty_head_zones <- function() {
  df_cols(x = numeric(0), lo = numeric(0), hi = numeric(0), s = numeric(0))
}

#' Find the level chords whose run a disc blocks
#'
#' A level chord is drawn as the horizontal run on its target's line, so
#' that run, not the chord between the centres, is what the discs of the
#' crossed layers must clear. Vectorised over every (run, non-endpoint node)
#' pair: a node blocks a run when its centre is closer than `R` to the
#' segment between the endpoints' x at the run's y.
#'
#' @param nodes Data frame with `x`, `y`.
#' @param a,b Row indices of each run's endpoints, `a` the left one.
#' @param y The line each run lies on.
#' @param R Obstruction radius, one value per node row.
#' @return A logical vector, one value per run.
#' @noRd
level_run_blocked <- function(nodes, a, b, y, R) {
  n_r <- length(a)
  n_n <- nrow(nodes)
  blocked <- logical(n_r)
  if (n_r == 0 || n_n == 0) {
    return(blocked)
  }
  ri <- rep(seq_len(n_r), each = n_n)
  ni <- rep.int(seq_len(n_n), n_r)
  dx <- pmax(nodes$x[a[ri]] - nodes$x[ni], nodes$x[ni] - nodes$x[b[ri]], 0)
  dy <- nodes$y[ni] - y[ri]
  hit <- ni != a[ri] & ni != b[ri] & sqrt(dx^2 + dy^2) < R[ni]
  blocked[unique(ri[hit])] <- TRUE
  blocked
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
#' A channel run that would lie in the head zone of a placed S/N stub, the
#' band its arrowhead and margin occupy, is pushed outward along the zone's
#' side to the zone's far edge and stacked again from there, until it lies
#' in no zone; a run that zones of both sides hold has no way out and is
#' infeasible. So is a run priced at a fixed line inside a zone, which a
#' slide option is and a push cannot move. Only the channel run is tested:
#' the departure and arrival legs stay within the endpoint layers' gaps,
#' short of the x of any crossed layer a stub could stand in, except in the
#' room-capped last rung, where a slot driven back to its gap's source
#' layer's centre line can put a leg at a crossed layer's x; and the stubs
#' of the endpoint layers themselves rise beyond the extreme nodes, on the
#' far side of the endpoints' own lines. An S/N run is tested like any
#' other, though a zone of one of its own endpoint layers can never hold
#' it: the run sits a stub past both endpoint lines, and the stub floor
#' `stub_min` reaches past a zone's `cap + head + head_margin` (12.2 mm
#' against 11.8 at the default node size and cap). Since `clear_of()` already
#' refuses a run within `R` of the head node, only the outer part of the
#' band is ever live and the push is at most `cap + head + head_margin -
#' R`.
#'
#' A candidate is infeasible when its run comes closer than the clearance
#' margin `m` to the panel bounds, when its margin band would cut a disc of
#' a layer it passes, when its horizontal pieces in a gap would coincide
#' with a committed piece from another source in either slot order, or when
#' it is an E/W run at `owned_y` and `owned` says a level chord already
#' arrives on that line. When nothing is feasible the least displaced
#' candidate other than an endpoint run is clamped to the margin and
#' reported without clearance.
#'
#' An endpoint line that the placed channels crowd, but that the run could
#' take with them out of the way, is returned as a slide option: the run
#' priced as if the band were empty, with the channels in its way. Their
#' polylines are left out of the crossing count for that price, since the
#' slide moves them off the line before the run is drawn; counting them
#' would price a crowding channel's own vertical leg, which can meet the
#' line to within rounding, as a crossing. The caller decides through
#' `slide_channels()` whether moving them costs less than the pushed
#' alternatives. A line crowded by a channel that never moves, an S/N
#' channel, a channel on its own endpoint line, or one of the edge's own
#' hyperedge (its trunk or a sibling), is not offered.
#'
#' @param e The edge being placed, the index the channel row records.
#' @param stub The nominal stub and the stub floor, in that order.
#' @param sn_sides Logical pair: are S/N ports available above and below.
#' @param channels The channels placed so far: `e`, the edge each belongs
#'   to, `side`, `y`, `lo`, `hi`, `key`, the segment key of the channel's
#'   first gap, and `fixed`, whether `channel_fixed()` holds it.
#' @param intervals Free intervals per layer from `layer_free_intervals()`.
#' @param pieces Committed horizontal pieces per gap: `key`, `left`,
#'   `right`.
#' @param keys The edge's segment keys in its first and last gap.
#' @param owned Whether a level chord arrives at this edge's head end on
#'   that node's own line, which makes the line no candidate for this edge.
#' @param owned_y The line `owned` speaks for: the ordinate of the node
#'   this edge's head arrives at, which is its target's whichever way it
#'   is drawn.
#' @param zones The head zones of the S/N channels placed so far: `x`, the
#'   stub's x, `lo` and `hi`, the band it holds, and `s`, the side of the
#'   head node the stub stands on.
#' @return A list with `kind` (`"sn"` or `"ew"`), `side`, `y`, `wp` (the
#'   bends used for pricing), `cost`, `clamped`, `channel` (the row to
#'   register), `slide` (the options above, each in the same shape plus
#'   `conflicts`, the rows of `channels` in its way), and `state`, what a
#'   later slide needs to re-check this edge at another line: the discs it
#'   crosses (`members`), its chord's ordinates there (`yc`), its E/W
#'   extent (`xr_ew`), the panel margins (`y_min`, `y_max`), and its
#'   parallel bundle's `extra` margin.
#' @noRd
ortho_channel <- function(
  fr,
  ectx,
  e,
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
  owned,
  owned_y,
  opts,
  zones
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
    if (kind == "sn") 2L else ew_bend_count(fr, x_a, x_b, y)
  }
  coincides <- function(kind, y) {
    kind == "ew" &&
      (pieces_coincide(pieces[[la]], keys[[1]], Sy, y) ||
        pieces_coincide(pieces[[lb - 1L]], keys[[2]], y, Ty))
  }
  # the head zones a run at y over the extent xr enters, in one comparison
  # over the zone table: the run is strictly inside the band and its extent
  # reaches the stub's x
  zone_w <- opts$sep_e / 2
  zones_at <- function(y, xr) {
    which(
      zones$lo + 1e-9 < y &
        y < zones$hi - 1e-9 &
        zones$x - zone_w < xr[[2]] - 1e-9 &
        zones$x + zone_w > xr[[1]] + 1e-9
    )
  }
  in_zone <- function(kind, y) {
    length(zones_at(y, extent_of(kind))) > 0
  }
  # the run moved to the far edge of the zones holding it and stacked past
  # the channels it then crowds, until it is clear of both; `NA` when zones
  # of opposite sides hold it at once, since neither direction leads out.
  # The stacking follows the zone's side rather than the candidate's, so a
  # run pushed out of a zone is never pushed back into that zone, and while
  # the passes keep to one side each moves the run outward by the band or a
  # whole separation. A push that lands the run in a zone of the other side
  # can reverse that, and the 20-pass bound is what terminates such a case,
  # the candidate reported infeasible
  push_past_zones <- function(y, xr) {
    for (i in seq_len(20)) {
      hit <- zones_at(y, xr)
      if (length(hit) == 0) {
        return(y)
      }
      s <- unique(zones$s[hit])
      if (length(s) != 1) {
        return(NA_real_)
      }
      y <- stack_channel(
        if (s > 0) max(zones$hi[hit]) else min(zones$lo[hit]),
        s,
        xr,
        channels,
        opts$sep_e
      )
    }
    NA_real_
  }
  # the feasibility and price of a run at y, the placed channels aside; the
  # edges in `drop` are left out of the crossing count, the channels a slide
  # option would move off the line before the run is drawn
  price <- function(kind, side, y, drop = integer(0)) {
    wp <- bends_of(kind, y)
    displacement <- sum(abs(y - yc))
    feasible <- !(owned && kind == "ew" && abs(y - owned_y) < 1e-6) &&
      y >= y_min &&
      y <= y_max &&
      clear_of(y, if (kind == "sn") sn_nodes else members) &&
      !coincides(kind, y) &&
      !in_zone(kind, y)
    cost <- if (feasible) {
      ec <- ectx
      if (length(drop) > 0) {
        ec$others <- setdiff(ec$others, drop)
      }
      side_cost(fr, wp, side, displacement, ec, placed, opts) +
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
  # a candidate is the run pushed past the placed channels it would crowd
  # and past the head zones it would enter; a run with no way out of the
  # zones is priced where the stacking left it and refused
  candidate <- function(kind, side, y) {
    xr <- extent_of(kind)
    y_run <- stack_channel(y, side, xr, channels, opts$sep_e)
    y_out <- push_past_zones(y_run, xr)
    if (is.na(y_out)) {
      cand <- price(kind, side, y_run)
      cand$cost <- Inf
      return(cand)
    }
    price(kind, side, y_out)
  }
  costs <- function(cands) vapply(cands, function(c) c$cost, numeric(1))
  result_of <- function(cand, clamped) {
    xr <- extent_of(cand$kind)
    list(
      kind = cand$kind,
      side = cand$side,
      y = cand$y,
      wp = cand$wp,
      cost = cand$cost,
      clamped = clamped,
      channel = df_cols(
        e = e,
        side = cand$side,
        y = cand$y,
        lo = xr[[1]],
        hi = xr[[2]],
        key = keys[[1]],
        fixed = channel_fixed(cand$kind, cand$y, Sy, Ty)
      )
    )
  }

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
  side_at <- function(y) {
    if (all(nodes$y[members] > y)) {
      -1
    } else if (all(nodes$y[members] < y)) {
      1
    } else if (y >= yc_mid) {
      1
    } else {
      -1
    }
  }
  at_end <- function(y) {
    cand <- candidate("ew", side_at(y), y)
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

  # the slide options: each endpoint line the placed channels crowd that
  # the run could take with the band empty, priced so, the crowding
  # channels' own polylines left out of the count; the caller decides
  # whether moving those channels is worth it. A line crowded by a channel
  # that never moves, one fixed by its kind or line or one of this edge's
  # own hyperedge, is no option and is not priced (`slide_channels()`
  # applies the same rule to the channels a cascade reaches)
  xr_ew <- extent_of("ew")
  slide <- list()
  for (y0 in unique(c(Sy, Ty))) {
    conflicts <- channel_conflicts(y0, xr_ew, channels, opts$sep_e)
    if (
      length(conflicts) == 0 ||
        any(channels$fixed[conflicts] | channels$key[conflicts] == keys[[1]])
    ) {
      next
    }
    p <- price("ew", side_at(y0), y0, channels$e[conflicts])
    if (is.finite(p$cost)) {
      slide <- c(
        slide,
        list(c(result_of(p, FALSE), list(conflicts = conflicts)))
      )
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
  c(
    result_of(best, clamped),
    list(
      slide = slide,
      state = list(
        members = members,
        yc = yc,
        xr_ew = xr_ew,
        y_min = y_min,
        y_max = y_max,
        extra = extra
      )
    )
  )
}

#' The bends of an E/W channel at `y`
#'
#' The turns of the polyline from the source through the first gap's slot,
#' along the channel, and through the last gap's slot to the target, with
#' the vertices a run on an endpoint line makes collinear dropped: two for
#' such a run, four otherwise.
#'
#' @param fr The edge frame, `S` its source and `E` its target.
#' @param x_a,x_b The x of the first and last gap's slot.
#' @noRd
ew_bend_count <- function(fr, x_a, x_b, y) {
  P <- drop_collinear(dedupe_points(rbind(
    fr$S,
    c(x_a, fr$S[[2]]),
    c(x_a, y),
    c(x_b, y),
    c(x_b, fr$E[[2]]),
    fr$E
  )))
  nrow(P) - 2L
}

#' Commit a horizontal piece in a gap
#'
#' A piece shorter than the coincidence tolerance is no piece: the edge
#' enters and leaves its slot on one line.
#'
#' @noRd
add_piece <- function(pieces, g, key, left, right) {
  if (abs(left - right) < 1e-3) {
    return(pieces)
  }
  pieces[[g]] <- df_bind(
    pieces[[g]],
    df_cols(key = key, left = left, right = right)
  )
  pieces
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
  repeat {
    near <- channel_conflicts(y, xr, channels, sep_e)
    if (length(near) == 0) {
      return(y)
    }
    ys <- channels$y[near]
    y <- if (side > 0) max(ys) + sep_e else min(ys) - sep_e
  }
}

#' The placed channels a run would sit within `sep_e` of
#'
#' A channel conflicts with a run at `y` over the x-range `xr` when its own
#' x-range properly overlaps `xr` and its y lies within `sep_e` of `y`,
#' whichever side it was placed on. The stacking rule and the channel slide
#' share this one test.
#'
#' @param channels Channels with `y`, `lo`, and `hi`.
#' @return The row indices of the conflicting channels.
#' @noRd
channel_conflicts <- function(y, xr, channels, sep_e) {
  same <- channels$hi > min(xr) + 1e-9 & channels$lo < max(xr) - 1e-9
  which(same & abs(channels$y - y) < sep_e - 1e-9)
}

#' Whether a channel never moves to make room for another edge's run
#'
#' An S/N channel's y is tied to its stubs, and a channel running on its
#' own source's or target's line has the shape the slide exists to create,
#' which is never taken from another edge. Vectorised over channels.
#'
#' @noRd
channel_fixed <- function(kind, y, Sy, Ty) {
  kind != "ew" | abs(y - Sy) < 1e-6 | abs(y - Ty) < 1e-6
}

#' Slide the placed channels crowding a line away from it
#'
#' `records` are the spanning edges placed so far, in placement order, each
#' with its `kind`, `side`, `y`, extent `xr`, gaps `la` and `lb`, segment
#' `keys` and the `src_key` of the pieces leaving its source port, endpoint
#' ordinates `Sy` and `Ty`, whether a level chord `owned` the centre row of
#' the node this edge's head arrives at and `owned_y`, the line that row
#' lies on, frame `fr`, and the `state` `ortho_channel()` priced it with. The channels in `conflicts` move to
#' `sep_e` beyond `y0` on the side `dir`, and every placed channel then
#' within `sep_e` of a moved one over an overlapping extent moves `sep_e`
#' beyond it in turn, the queue in placement order, until nothing is within
#' `sep_e`. Three kinds of channel never move, and the direction is
#' infeasible when the cascade reaches one: an S/N channel, whose y is tied
#' to its stubs; a channel of the candidate's own hyperedge, its trunk or a
#' sibling on the source's line, which is a channel whose own `src_key` is
#' the candidate's `cand_key`, since which gap a piece leaving the source
#' is drawn in depends on the way the edge runs; and a channel running on
#' its own source's or target's line, the shape this pass exists to create,
#' which is never taken from another edge. A moved channel must stay inside
#' the panel margin, clear its crossed discs at the margin it was placed
#' with, and keep off `owned_y` when it is owned, a line the lattice the
#' cascade moves on can otherwise land on: that centre row has an owner
#' already, as it has for the candidate `ortho_channel()` prices. It must
#' keep its pieces free of coincidence too, tested against the pieces
#' rebuilt from `pieces_base` (those of the span-1 edges) with every
#' channel at its new line and the candidate's own `cand_pieces` committed.
#'
#' The price is the moved channels' change in displacement, the sum of
#' `|y - yc|` over their crossed layers weighted as `side_cost()` weighs it,
#' plus `bend_penalty` per bend gained, rounded to `cost_digits` like every
#' other price so that the two directions are compared on a difference the
#' constants can see rather than on rounding noise. Their crossings are not
#' repriced: a moved channel keeps its side, and a chord between its old and
#' new lines within its extent is rare enough to leave out until a scene
#' shows the gap, which is why the price is kept in this one place.
#'
#' @param cand_pieces The candidate's own pieces, each a list with `g`,
#'   `key`, `left`, and `right`.
#' @param gap_mid The x of every gap's midpoint, where the bends are priced.
#' @return `NULL` when the direction is infeasible; otherwise a list with
#'   `y`, the line of every record after the slide, `moved`, the indices of
#'   the records that moved, the `cost`, and `pieces`, the committed pieces
#'   with the moved channels and the candidate in place.
#' @noRd
slide_channels <- function(
  records,
  conflicts,
  y0,
  dir,
  sep_e,
  nodes,
  R_node,
  pieces_base,
  cand_pieces,
  opts,
  cand_key,
  gap_mid
) {
  n <- length(records)
  field <- function(f, type) vapply(records, `[[`, type, f)
  kind <- field("kind", "")
  xr <- field("xr", numeric(2))
  keys <- field("keys", character(2))
  lines <- df_cols(y = field("y", 1), lo = xr[1, ], hi = xr[2, ])
  fixed <- channel_fixed(kind, lines$y, field("Sy", 1), field("Ty", 1)) |
    field("src_key", "") == cand_key

  # the cascade: every moved channel sits on the lattice y0 + k sep_e, and
  # a move only ever raises a channel's level on it, so no channel returns
  # to a level it left and the queue drains
  moved <- logical(n)
  target <- rep(NA_real_, n)
  target[conflicts] <- y0 + dir * sep_e
  queue <- conflicts
  while (length(queue) > 0) {
    k <- queue[[1]]
    queue <- queue[-1]
    if (fixed[[k]]) {
      return(NULL)
    }
    want <- target[[k]]
    if (dir * (lines$y[[k]] - want) >= -1e-9) {
      next
    }
    lines$y[[k]] <- want
    moved[[k]] <- TRUE
    near <- channel_conflicts(want, records[[k]]$xr, lines, sep_e)
    near <- near[near != k]
    if (any(fixed[near])) {
      return(NULL)
    }
    for (j in near) {
      need <- want + dir * sep_e
      if (is.na(target[[j]]) || dir * (need - target[[j]]) > 1e-9) {
        target[[j]] <- need
        queue <- c(queue, j)
      }
    }
  }

  # every moved channel inside the panel margin and clear of its crossed
  # discs at the margin it was placed with
  moved <- which(moved)
  for (k in moved) {
    st <- records[[k]]$state
    y <- lines$y[[k]]
    clear <- y >= st$y_min &&
      y <= st$y_max &&
      !(isTRUE(records[[k]]$owned) &&
        abs(y - records[[k]]$owned_y) < 1e-6) &&
      all(
        abs(y - nodes$y[st$members]) >= R_node[st$members] + st$extra - 1e-9
      )
    if (!clear) {
      return(NULL)
    }
  }

  # the committed pieces with every channel at its line and the candidate's
  # own pieces in place, rebuilt on the span-1 pieces one gap at a time; a
  # channel's first-gap piece precedes its last-gap piece as when they were
  # committed one by one
  ew <- kind == "ew"
  y_ew <- lines$y[ew]
  cand <- function(f, type) vapply(cand_pieces, `[[`, type, f)
  g <- c(
    as.vector(rbind(field("la", 1)[ew], field("lb", 1)[ew] - 1)),
    cand("g", 1)
  )
  key <- c(as.vector(keys[, ew]), cand("key", ""))
  left <- c(as.vector(rbind(field("Sy", 1)[ew], y_ew)), cand("left", 1))
  right <- c(as.vector(rbind(y_ew, field("Ty", 1)[ew])), cand("right", 1))
  keep <- abs(left - right) >= 1e-3
  pieces <- pieces_base
  for (gap in unique(g[keep])) {
    at <- keep & g == gap
    pieces[[gap]] <- df_bind(
      pieces[[gap]],
      df_cols(key = key[at], left = left[at], right = right[at])
    )
  }

  cost <- 0
  for (k in moved) {
    r <- records[[k]]
    y <- lines$y[[k]]
    if (
      pieces_coincide(pieces[[r$la]], r$keys[[1]], r$Sy, y) ||
        pieces_coincide(pieces[[r$lb - 1L]], r$keys[[2]], y, r$Ty)
    ) {
      return(NULL)
    }
    x_a <- gap_mid[[r$la]]
    x_b <- gap_mid[[r$lb - 1L]]
    cost <- cost +
      opts$displacement_weight *
        (sum(abs(y - r$state$yc)) - sum(abs(r$y - r$state$yc))) /
        opts$r_ref +
      opts$bend_penalty *
        (ew_bend_count(r$fr, x_a, x_b, y) - ew_bend_count(r$fr, x_a, x_b, r$y))
  }
  list(
    y = lines$y,
    moved = moved,
    cost = round(cost, opts$cost_digits),
    pieces = pieces
  )
}

#' The hyperedge segments of one gap
#'
#' Every E/W edge whose first gap this is contributes a piece entering at
#' the left-hand end's y and leaving at the right-hand end's (its channel y
#' when it spans); every spanning E/W edge whose last gap this is
#' contributes a piece entering at its channel y and leaving at the
#' right-hand end's y. A piece drawn in the gap beside its edge's source
#' leaves that source's port, and the pieces leaving one port are one
#' segment, as are the pieces of duplicate edges; a piece in any other gap
#' is the edge's alone. The gap beside the source is the first gap of an
#' edge drawn rightwards and the last gap of one drawn leftwards, and the
#' only gap of a span-1 edge, which lies beside that edge's source
#' whichever way it is drawn. So a first-gap piece enters at its source's y
#' only when the edge runs rightwards, and every arrival at a node's side
#' takes a segment of its own however the edge runs. The y-interval of a
#' segment is the range of its pieces. Segments are returned in canonical order: source segments by
#' the position of their source, then the rest by the positions of both
#' ends, with node row indices as the final tie-break, so that the order
#' never depends on node names.
#'
#' @param reversed Whether each edge is drawn leftwards, indexed like `a`.
#' @return A list of parallel vectors and lists: `members` (edge indices),
#'   `member_first` (whether each member enters through its first gap),
#'   `lefts` and `rights` (the y values of the horizontal pieces on each
#'   side), `lo`, `hi`, and `degenerate`.
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
  tol,
  reversed
) {
  e <- c(first, last)
  is_first <- c(rep(TRUE, length(first)), rep(FALSE, length(last)))
  rev_e <- reversed[e]
  src_gap <- span[e] == 1L | xor(is_first, rev_e)
  src <- ifelse(rev_e, b[e], a[e])
  tgt <- ifelse(rev_e, a[e], b[e])
  left_y <- ifelse(is_first, Sy[e], y_ch[e])
  right_y <- ifelse(is_first & span[e] >= 2, y_ch[e], Ty[e])
  key <- ifelse(src_gap, paste0("s", src), paste0("e", a[e], "-", b[e]))
  ord <- order(
    as.integer(!src_gap),
    nodes$y[src],
    nodes$x[src],
    ifelse(src_gap, -Inf, nodes$y[tgt]),
    ifelse(src_gap, -Inf, nodes$x[tgt]),
    src,
    ifelse(src_gap, 0L, tgt),
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
#' * Rung 4: no stub fits. The gap is flagged `narrow` and its edges lose
#'   their clearance. The slot nearest the target is the run every
#'   arrowhead out of the gap is drawn on, and the base of each head sits
#'   `cap + head` before the target layer's centre line, so a slot there
#'   would be drawn across the base of every other head arriving at that
#'   layer. The slots are spread over the band from the source layer's soft
#'   band, `R_soft`, to the head run and its margin, `cap + head +
#'   head_margin`, before the target layer, at the spacing that fits them
#'   in the band, no wider than `sep_e` and no narrower than `sep_min`:
#'   centred in the band when they fit, and otherwise anchored at the head
#'   run so that the overflow goes toward the source, entering the
#'   source's soft band and stopping at the source layer's centre line,
#'   where the whole spread shifts back and the slot nearest the target
#'   gives up its run instead. So the source's soft band gives way first,
#'   the margin behind the head run next, once the spread has reached the
#'   source layer's centre line, and the head's own run last of all; the
#'   `sep_min` spacing never gives way, and the source layer's centre line
#'   is the hard stop. The band is measured from the source layer whichever
#'   side of the gap it is, while the ranks run left to right, so they are
#'   reversed in a gap crossed leftwards and rank 1 keeps the leftmost slot
#'   either way. The spacing floor keeps the slots of different sources
#'   from collapsing onto one x, which would draw a line the DAG does not
#'   have; every term is continuous in the gap width, so no slot moves
#'   faster than the gap widens within the rung. A gap crossed in both directions has no target side and keeps
#'   the slots centred between the two soft bands. A gap whose slot nearest
#'   the target reaches `cap + head` from the target layer's centre line is
#'   `floored`: the head run out of it holds a row as well as a head, so
#'   its arrivals take rows at their targets like arrivals out of a wider
#'   gap, while those out of a narrow gap short of the floor, or one with
#'   no target side, keep the centre row.
#'
#' @param direction One value per segment: `1` when its edges all point to
#'   the right layer, `-1` when they all point to the left one, and `0` when
#'   the segment carries both, from `ortho_target_side()`.
#' @return A list with `x` (the slot of every segment, `NA` for a degenerate
#'   one), `narrow`, `floored`, `rc` (the corner radius the gap needs), and
#'   `ladder`, a one-row data frame with `width`, `ranks`, `rung`, `stub`,
#'   and `spacing`, `NULL` when no segment took a slot.
#' @noRd
ortho_slot_positions <- function(segs, gap, opts, cap, direction) {
  x <- rep(NA_real_, length(segs$lo))
  live <- which(!segs$degenerate)
  if (length(live) == 0) {
    return(list(
      x = x,
      narrow = FALSE,
      floored = FALSE,
      rc = opts$rc,
      ladder = NULL
    ))
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
  floored <- FALSE
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
      target <- ortho_target_side(direction[live])
      head_run <- cap + opts$head
      if (target == 0L) {
        # crossed both ways, the gap has no target side to keep a head run
        # for, so the slots are centred between the two soft bands
        width4 <- G - 2 * opts$R_soft
        spacing <- if (K >= 2) {
          max(min(sep_e, width4 / (K - 1)), sep_min)
        } else {
          sep_e
        }
        pos <- centred(spacing)
      } else {
        # the band runs from the source's soft band to the head run and
        # its margin before the target, measured from the source layer.
        # The slots are centred in it when they fit; otherwise they are
        # anchored at the head run and overflow toward the source, no
        # further than the source layer's centre line
        band_lo <- opts$R_soft
        band_hi <- G - (head_run + opts$head_margin)
        band_w <- band_hi - band_lo
        spacing <- if (K >= 2) {
          max(min(sep_e, band_w / (K - 1)), sep_min)
        } else {
          sep_e
        }
        # the ranks run left to right, while the offsets are measured from
        # the source layer, so they are reversed when the source is the
        # right layer: rank 1 keeps the leftmost slot either way, which is
        # the order `ortho_slot_ranks()` chose to avoid crossings
        rk <- if (target > 0) ranks else K + 1L - ranks
        off <- if ((K - 1) * spacing <= band_w + eps) {
          (band_lo + band_hi) / 2 + (rk - (K + 1) / 2) * spacing
        } else {
          anchored <- band_hi - (K - rk) * spacing
          anchored - min(min(anchored), 0)
        }
        pos <- if (target > 0) gap[[1]] + off else gap[[2]] - off
        # the gap is floored once the slot nearest the target leaves the
        # whole head run before the target's layer
        to_target <- if (target > 0) {
          gap[[2]] - max(pos)
        } else {
          min(pos) - gap[[1]]
        }
        floored <- to_target >= head_run - eps
      }
    }
  }
  x[live] <- pos
  list(
    x = x,
    narrow = narrow,
    floored = floored,
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

#' The side of a gap its arrowheads point to
#'
#' `direction` holds `1` for an edge or segment pointing to the right layer
#' of a gap and `-1` for one pointing to the left, with `0` for a segment
#' already known to carry both. The result is `1` or `-1` when every entry
#' agrees, and `0` when the gap is crossed in both directions, in which case
#' neither layer is the target side.
#'
#' @noRd
ortho_target_side <- function(direction) {
  if (all(direction == 1L)) {
    1L
  } else if (all(direction == -1L)) {
    -1L
  } else {
    0L
  }
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
#' requested perpendicular displacement, so the runs stay axis-aligned. The
#' ports are the points the path starts and ends at: the node centre, or
#' for an offset port its axis point, the node's own coordinate along the
#' port's axis carried onto the port's line. An S/N port offset `dx_s` or
#' `dx_t` moves the stub beside the centre line, and the path ends where
#' the stub crosses the centre's own y; a W or E port offset `dy_s` or
#' `dy_t` moves the run at that end onto a row beside the centre line, and
#' the path ends where the row crosses the centre's own x. The row is at
#' the head's end of the frame, so `dy_t` carries it for an edge drawn
#' rightwards and `dy_s` for one drawn leftwards. The run from the disc
#' face to the axis point is hidden under the disc, and because the path
#' ends on the run itself the arrow layer draws the head along it.
#'
#' @return A list with `bends` (a matrix, or `NULL` when the edge has no
#'   bend and stays straight), `port_s`, `port_t`, `side`, and each port's
#'   offset from the centre line (`off_s`, `off_t`), which sets its resect.
#' @noRd
ortho_bends <- function(
  kind,
  S,
  E,
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
  dy_s,
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
      off_s = dx_s,
      off_t = dx_t
    ))
  }
  if (kind == "detour") {
    if (abs(E[[2]] - S[[2]]) < tol) {
      xa <- S[[1]] + stub
      xb <- E[[1]] - stub
      if (xa > xb) {
        xa <- (S[[1]] + E[[1]]) / 2
        xb <- xa
      }
      y <- S[[2]] + shift
      return(list(
        bends = rbind(c(xa, S[[2]]), c(xa, y), c(xb, y), c(xb, E[[2]])),
        port_s = S,
        port_t = E,
        side = sign(shift),
        off_s = 0,
        off_t = 0
      ))
    }
    ya <- S[[2]] + stub
    yb <- E[[2]] - stub
    if (ya > yb) {
      ya <- (S[[2]] + E[[2]]) / 2
      yb <- ya
    }
    x <- S[[1]] - shift
    return(list(
      bends = rbind(c(S[[1]], ya), c(x, ya), c(x, yb), c(E[[1]], yb)),
      port_s = S,
      port_t = E,
      side = sign(shift),
      off_s = 0,
      off_t = 0
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
  # a spanning edge whose run lies on an endpoint's own line has no
  # vertical there to move onto a row
  if (span >= 2 && is.na(x_last)) {
    dy_t <- 0
  }
  if (span >= 2 && is.na(x_first)) {
    dy_s <- 0
  }
  ys <- S[[2]] + dy_s
  yt <- E[[2]] + dy_t
  bends <- NULL
  if (span == 1) {
    if (!is.na(x_first)) {
      bends <- rbind(c(x_first, ys), c(x_first, yt))
    }
  } else {
    y <- y_ch + dy
    if (!is.na(x_first)) {
      bends <- rbind(bends, c(x_first, ys), c(x_first, y))
    }
    if (!is.na(x_last)) {
      bends <- rbind(bends, c(x_last, y), c(x_last, yt))
    }
  }
  if (is.null(bends)) {
    return(list(bends = NULL))
  }
  bends[, 1] <- bends[, 1] + dx
  list(
    bends = bends,
    port_s = c(S[[1]], ys),
    port_t = c(E[[1]], yt),
    side = if (span >= 2) side else NA_real_,
    off_s = dy_s,
    off_t = dy_t
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
#' path runs straight through or reverses on are kept as they are.
#'
#' @noRd
round_corners <- function(P, rc, n = 12L) {
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
    if (abs(cross) < 1e-9 || lab == 0 || lcb == 0) {
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

  grp <- parallel_groups(from, to, nodes, routable, opts$sep_m)
  extra <- grp$extra
  shift <- grp$shift

  # every chord long enough to bow is visited, since a chord clear of the
  # discs may still run under another edge's drawn arrowhead; one that
  # clears the heads too comes back as its chord
  to_route <- routable &
    (seq_len(n_edges) %in% hits$edge | shift != 0 | info$Lc >= 2 * opts$R)
  order_e <- which(to_route)
  # Chord lengths are rounded to a micrometre so that two skip edges of one
  # row tie on length rather than on a floating difference. What settles the
  # tie is where the endpoints are, the source's x then its y and then the
  # target's, followed by the input order, as a parallel group's members are
  # ordered. An edge is routed around the ones placed before it, so ordering
  # by node name would let two callers who name one picture differently draw
  # a tied pair in opposite orders.
  order_e <- order_e[order(
    -info$span[order_e],
    -round(info$Lc[order_e], 6),
    nodes$x[from[order_e]],
    nodes$y[from[order_e]],
    nodes$x[to[order_e]],
    nodes$y[to[order_e]],
    order_e,
    method = "radix"
  )]

  ctx <- list(nodes = nodes, from = from, to = to, Lc = info$Lc)
  base_intervals <- lapply(seq_len(layers$n), function(k) {
    layer_free_intervals(
      df_rows(nodes, layers$members[[k]]),
      opts$m,
      bounds,
      opts$pad,
      opts$sep_e,
      opts$m_min
    )
  })
  # the spanning tier's cap on how deep a curve may be drawn off its chord,
  # `Inf` unless the caller wrote one
  cap_ratio <- opts$sagitta_max_spanning %||% Inf
  # two drawn tips theta degrees apart at one target are 2 cap sin(theta / 2)
  # apart, so arrivals this far apart keep their arrowheads sep_e apart
  theta_min <- 2 * asin(min(1, opts$sep_e / (2 * cap))) * 180 / pi

  # one edge: the candidate tiers in order, verified and repaired
  route_one <- function(e, placed, occ, reserved, heads) {
    fr <- edge_frame(nodes, from[[e]], to[[e]], info$reversed[[e]])
    la <- info$la[[e]]
    lb <- info$lb[[e]]
    ectx <- edge_cost_context(fr, e, ctx)
    constraints <- head_constraints(heads, e, from, to)
    ectx$heads <- constraints$heads
    ectx$cap <- cap

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
    eh$mm <- rep(opts$m, nrow(eh))
    eh$side <- rep(NA_real_, nrow(eh))
    # a chord with no hard hit is nudged past the drawn arrowheads of the
    # other edges as it is past a grazed disc; an edge with a hard hit
    # detours, and the head zones govern its detour instead
    if (!any(eh$hard)) {
      hh <- head_hits(fr, constraints$heads, cap, opts)
      if (nrow(hh) > 0) {
        eh <- df_bind(eh, hh)
        eh <- df_rows(eh, order(eh$t, method = "radix"))
        eh <- merge_head_hits(eh, fr$Lc)
      }
    }
    if (nrow(eh) == 0 && shift[[e]] == 0) {
      return(list(straight = TRUE))
    }

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
        layer = layers$id[others],
        x2 = nodes$x[others],
        y2 = nodes$y[others],
        capsule = logical(length(others))
      ),
      R_full = R_full[others],
      R_soft = R_soft[others],
      hits = eh,
      ectx = ectx,
      cap = cap,
      arrivals = constraints$arrivals,
      head_end = if (info$reversed[[e]]) "S" else "E",
      theta_min = theta_min,
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
              opts$sep_e,
              opts$m_min,
              tight_margin = opts$m
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
          # the depth a candidate is actually drawn at, which is not the
          # depth of its waypoints: the curve interpolated through a
          # levelled chain bulges several millimetres past it, so a cap
          # read off the waypoints would pass a curve that misses it
          drawn_sagitta <- function(r) {
            offset <- (r$path$x - fr$S[[1]]) *
              fr$n[[1]] +
              (r$path$y - fr$S[[2]]) * fr$n[[2]]
            round(max(abs(offset)) / fr$Lc, opts$cost_digits)
          }
          meets_cap <- function(r) drawn_sagitta(r) <= cap_ratio
          # the verified attempt with the shallowest drawn curve, kept for
          # when no admissible slot and no bow meets the cap
          shallowest <- NULL
          keep_shallow <- function(r) {
            if (!r$inside || !r$clearance_ok) {
              return(invisible(NULL))
            }
            # two attempts within the verification tolerance of each other
            # are equally shallow and the pool rank decides between them, so
            # a floating difference between two mirror images cannot
            if (
              is.null(shallowest) ||
                drawn_sagitta(r) <
                  drawn_sagitta(shallowest) - opts$verify_tol / fr$Lc
            ) {
              shallowest <<- r
            }
            invisible(NULL)
          }

          # the best-ranked candidate wins when its curve keeps the margin
          # from the panel bounds, verifies against the discs, and is drawn
          # no deeper than the cap
          first <- try_cand(cands[[1]])
          if (first$inside && first$clearance_ok && meets_cap(first)) {
            res <- first
          } else {
            keep_shallow(first)
            # otherwise the free bow is priced with the remaining candidates
            # and the cheapest verified one inside the margin wins; a
            # lower-ranked slot never wins by default. The bow is priced
            # now and drawn only when its turn comes
            fb <- free_bow_waypoints(
              job$hits,
              job$fr,
              job$extra,
              job$bounds,
              job$ectx,
              placed,
              job$opts
            )
            rest <- cands[-1]
            pool_cost <- c(
              vapply(rest, function(c) c$cost, numeric(1)),
              fb$cost
            )
            pool_overlap <- c(
              vapply(rest, function(c) c$overlap, logical(1)),
              FALSE
            )
            fallback <- NULL
            for (k in order(pool_overlap, pool_cost)) {
              if (k > length(rest)) {
                bow <- route_free_bow(job, placed, fb)
                tried <- bow
              } else {
                tried <- try_cand(rest[[k]])
              }
              if (tried$inside && tried$clearance_ok && meets_cap(tried)) {
                res <- tried
                break
              }
              keep_shallow(tried)
              # when nothing verifies, the attempt inside the margin with
              # the least violation is kept, and the free bow may still
              # replace it below; attempts within the verification
              # tolerance of each other are equally bad and the pool rank
              # decides, so a floating difference between two mirror
              # images cannot
              if (
                tried$inside &&
                  (is.null(fallback) ||
                    tried$depth < fallback$depth - opts$verify_tol)
              ) {
                fallback <- tried
              }
            }
            if (
              is.null(res) &&
                !is.null(fallback) &&
                first$inside &&
                fallback$depth > first$depth - opts$verify_tol
            ) {
              fallback <- first
            }
            # a cap nothing met is a preference rather than a bound: the
            # shallowest route that did verify is drawn, and the flag says
            # the cap acted on the route rather than that it was met
            if (is.null(res) && !is.null(shallowest)) {
              shallowest$capped <- TRUE
              res <- shallowest
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
    heads <- head_registry(paths, cap)
    occ <- empty_occupancy()
    out <- vector("list", n_edges)
    for (e in order_e) {
      held <- if (!is.null(reserved)) {
        df_rows(reserved, which(reserved$edge == e))
      }
      res <- route_one(e, placed, occ, held, heads)
      if (isTRUE(res$straight)) {
        # a chord clear of the discs and the heads stays as it is placed
        out[[e]] <- res
        next
      }
      placed <- place_edge(placed, e, res$path$x, res$path$y)
      # the registry keeps every path source to target
      heads <- if (info$reversed[[e]]) {
        register_head(heads, e, rev(res$path$x), rev(res$path$y), cap)
      } else {
        register_head(heads, e, res$path$x, res$path$y, cap)
      }
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
    if (isTRUE(res$straight)) {
      next
    }
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
