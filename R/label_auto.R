# Deterministic placement engine for DAG node labels. All functions are pure:
# they take plain coordinate data frames in mm and return values without
# touching tidy_dagitty objects, graphics devices, or the RNG.

# Clearance margins in mm between a candidate label box and each obstacle
# class. A box closer than the margin to an obstacle counts as violating even
# when it does not overlap it, so labels keep a visible gap from drawn ink.
# The node margin stays below the default `gap` of place_dag_labels() so a
# ring 1 candidate, whose clearance from its own node disc is exactly `gap`,
# is never penalized against that disc. The edge margin stays small so labels
# can settle beside edges in tight layouts; the arrow margin is larger
# because arrowheads are drawn wider than the edge stroke.
label_node_clearance <- 0.5
label_edge_clearance <- 1
label_arrow_clearance <- 2

# Soft comfort zone in mm around node discs other than a label's own node.
# A box inside this zone is not violating, but its penetration depth is
# penalized (per the `soft` weight) so labels drift away from foreign discs
# when an equally near spot is free of them.
label_soft_margin <- 8

#' Place DAG node labels deterministically
#'
#' Chooses a position for each label so that label boxes avoid node discs,
#' edge polylines, arrowhead zones, other labels, and the panel boundary.
#' Placement is fully deterministic: no random numbers are used and equal
#' inputs always produce equal outputs.
#'
#' Candidate positions per label are anchors in the fixed preference order
#' NE, NW, SE, SW, N, S, E, W (45, 135, 315, 225, 90, 270, 0, and 180
#' degrees) at `n_rings` distances from the node. `n_angles` keeps the first
#' `n_angles` anchors of that preference order rather than spacing
#' `n_angles` rays evenly around the node, so small values favor the diagonal
#' anchors: `n_angles = 4` gives the four diagonals, not the four cardinal
#' directions. At ring 1 the box's nearest point to the node center sits at
#' distance `radius + gap` along the anchor ray: the facing box corner for a
#' diagonal anchor, the facing side midpoint for a cardinal anchor. Each
#' further ring steps that distance outward by half the box diagonal.
#' Candidates are ordered ring-major (every anchor at ring 1 precedes any
#' anchor at ring 2), and the `prefer` weight breaks score ties in that
#' order. The radius used for a label's own rings is the radius of the node
#' disc nearest to the label's node center. A candidate whose box spills the
#' panel additionally spawns a slid variant translated by the minimal offset
#' that brings it fully inside `bounds` (per axis, and only when the box fits
#' along that axis), at preference rank + 0.5 and with a `*` suffix on its
#' anchor, so a barely spilling spot can slide inside instead of losing to a
#' farther anchor.
#'
#' Each candidate box is scored as a weighted sum of penalties: node disc
#' penetration depth (including the clearance margin), the count of sampled
#' edge points inside or within the edge margin of the box, the count of
#' final edge segments (the arrowhead zone of each `edge_id`) within the
#' arrow margin of the box, the overlap area with other placed labels, the
#' box area outside `bounds`, the distance in mm from the box center to the
#' label's own node center (the `dist` proximity pull), the total
#' penetration depth in mm into the soft zone extending `label_soft_margin`
#' beyond every other node's disc (the `soft` term), and the candidate's
#' preference rank as a pure tiebreak. The proximity pull makes near anchors
#' (the cardinals at ring 1) beat farther ones unless an obstacle penalty
#' separates them. Labels are assigned most constrained first (fewest
#' violation-free candidates, ties by input order) with a greedy argmin,
#' then refined by two local-improvement sweeps that move a label only when
#' its score strictly improves. When every candidate violates something the
#' least-bad candidate is still returned, so there is always one row per
#' label.
#'
#' @param labels Data frame with columns `id` (unique character), `x`, `y`
#'   (node centers in mm), and `width`, `height` (label box extent in mm,
#'   positive and finite).
#' @param nodes Data frame with columns `x`, `y`, and `radius` (positive and
#'   finite), one row per drawn node disc.
#' @param edges Data frame with columns `edge_id`, `x`, and `y`: ordered
#'   sampled points along each drawn edge, at least two per `edge_id`. The
#'   final segment of each `edge_id` is treated as its arrowhead zone. Zero
#'   rows means no edges.
#' @param bounds Numeric of length 4, `c(xmin, ymin, xmax, ymax)`, the panel
#'   extent in mm; must be strictly ordered in each dimension.
#' @param gap Clearance in mm between the node disc edge and the label box
#'   edge at ring 1.
#' @param n_angles Number of anchors to use, taken from the front of the
#'   preference order above.
#' @param n_rings Number of rings of candidates per anchor.
#' @param weights Named numeric vector weighting the score terms `node`,
#'   `edge`, `arrow`, `label`, and `bounds`, plus the `prefer` tiebreak, the
#'   `dist` proximity pull, and the `soft` clearance-zone term. `dist` and
#'   `soft` default to 0 when absent, so a weights vector from before those
#'   terms existed still works.
#' @return A data frame with one row per label in input order and columns
#'   `id`, `x`, `y` (box centers), `anchor` (a `*` suffix marks a slid
#'   variant), and `score`.
#' @noRd
place_dag_labels <- function(
  labels,
  nodes,
  edges,
  bounds,
  gap = 1.5,
  n_angles = 8L,
  n_rings = 3L,
  weights = c(
    node = 100,
    edge = 12,
    arrow = 40,
    label = 30,
    bounds = 60,
    prefer = 0.01,
    dist = 0.2,
    soft = 1
  )
) {
  validate_label_placement_inputs(
    labels,
    nodes,
    edges,
    bounds,
    gap,
    n_angles,
    n_rings,
    weights
  )

  arrow_segments <- final_edge_segments(edges)
  radius <- nearest_node_radius(labels$x, labels$y, nodes)

  n <- nrow(labels)
  candidates <- vector("list", n)
  static_scores <- vector("list", n)
  clean_counts <- integer(n)
  for (i in seq_len(n)) {
    cand <- label_candidates(
      labels$x[i],
      labels$y[i],
      radius[i],
      gap,
      labels$width[i],
      labels$height[i],
      n_angles,
      n_rings,
      bounds
    )
    scored <- score_label_candidates(
      cand,
      nodes,
      edges,
      arrow_segments,
      bounds,
      weights,
      own_xy = c(labels$x[i], labels$y[i])
    )
    candidates[[i]] <- cand
    static_scores[[i]] <- scored$score
    clean_counts[i] <- scored$n_clean
  }

  # Most constrained labels claim their spots first; ties fall back to input
  # order, keeping the order deterministic.
  placement_order <- order(clean_counts, seq_len(n))
  chosen <- integer(n)

  # Candidate box limits and chosen box limits per label, kept as plain
  # vectors so the assignment loops avoid data frame subsetting.
  cand_xmin <- lapply(candidates, `[[`, "xmin")
  cand_ymin <- lapply(candidates, `[[`, "ymin")
  cand_xmax <- lapply(candidates, `[[`, "xmax")
  cand_ymax <- lapply(candidates, `[[`, "ymax")
  box_xmin <- box_ymin <- box_xmax <- box_ymax <- rep(NA_real_, n)
  record_box <- function(i) {
    box_xmin[i] <<- cand_xmin[[i]][chosen[i]]
    box_ymin[i] <<- cand_ymin[[i]][chosen[i]]
    box_xmax[i] <<- cand_xmax[[i]][chosen[i]]
    box_ymax[i] <<- cand_ymax[[i]][chosen[i]]
  }

  totals_for <- function(i) {
    placed <- which(chosen > 0L)
    placed <- placed[placed != i]
    if (length(placed) == 0) {
      return(static_scores[[i]])
    }
    n_cand <- length(cand_xmin[[i]])
    ci <- rep(seq_len(n_cand), times = length(placed))
    pj <- rep(placed, each = n_cand)
    pair_overlap <- rect_overlap_area(
      cand_xmin[[i]][ci],
      cand_ymin[[i]][ci],
      cand_xmax[[i]][ci],
      cand_ymax[[i]][ci],
      box_xmin[pj],
      box_ymin[pj],
      box_xmax[pj],
      box_ymax[pj]
    )
    overlap <- rowSums(matrix(pair_overlap, nrow = n_cand))
    static_scores[[i]] + weights[["label"]] * overlap
  }

  for (i in placement_order) {
    chosen[i] <- which.min(totals_for(i))
    record_box(i)
  }

  # Two local-improvement sweeps: re-evaluate each label with the others
  # fixed and move only on strict improvement, so the refinement is
  # deterministic and cannot oscillate.
  for (pass in 1:2) {
    for (i in placement_order) {
      totals <- totals_for(i)
      best <- which.min(totals)
      if (totals[best] < totals[chosen[i]]) {
        chosen[i] <- best
        record_box(i)
      }
    }
  }

  pick <- function(column) {
    vapply(
      seq_len(n),
      function(i) candidates[[i]][[column]][chosen[i]],
      numeric(1)
    )
  }
  score <- vapply(
    seq_len(n),
    function(i) totals_for(i)[chosen[i]],
    numeric(1)
  )

  data.frame(
    id = labels$id,
    x = pick("x"),
    y = pick("y"),
    anchor = vapply(
      seq_len(n),
      function(i) candidates[[i]]$anchor[chosen[i]],
      character(1)
    ),
    score = score
  )
}

#' Validate place_dag_labels() inputs
#'
#' Errors with class `ggdag_type_error` on duplicate label ids, non-finite
#' coordinates, non-positive or non-finite label dimensions or node radii, an
#' edge polyline with fewer than two points, unordered bounds, or malformed
#' engine parameters.
#'
#' @inheritParams place_dag_labels
#' @param call Environment reported as the source of the error.
#' @return `NULL`, invisibly.
#' @noRd
validate_label_placement_inputs <- function(
  labels,
  nodes,
  edges,
  bounds,
  gap,
  n_angles,
  n_rings,
  weights,
  call = rlang::caller_env()
) {
  duplicated_ids <- unique(labels$id[duplicated(labels$id)])
  if (length(duplicated_ids) > 0) {
    abort(
      c(
        "Every {.field id} in {.arg labels} must be unique.",
        "x" = "Duplicated id{?s}: {.val {duplicated_ids}}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  bad_label_coords <- !is.finite(labels$x) | !is.finite(labels$y)
  if (any(bad_label_coords)) {
    abort(
      c(
        "{.field x} and {.field y} in {.arg labels} must be finite.",
        "x" = "Non-finite coordinates for label{?s}
               {.val {labels$id[bad_label_coords]}}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  bad_dims <- !is.finite(labels$width) |
    labels$width <= 0 |
    !is.finite(labels$height) |
    labels$height <= 0
  if (any(bad_dims)) {
    abort(
      c(
        "{.field width} and {.field height} in {.arg labels} must be
         positive and finite.",
        "x" = "Bad dimensions for label{?s} {.val {labels$id[bad_dims]}}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  bad_node_coords <- !is.finite(nodes$x) | !is.finite(nodes$y)
  if (any(bad_node_coords)) {
    abort(
      c(
        "{.field x} and {.field y} in {.arg nodes} must be finite.",
        "x" = "Non-finite coordinates in row{?s}
               {.val {which(bad_node_coords)}} of {.arg nodes}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  bad_radii <- !is.finite(nodes$radius) | nodes$radius <= 0
  if (any(bad_radii)) {
    abort(
      c(
        "{.field radius} in {.arg nodes} must be positive and finite.",
        "x" = "Bad radius in row{?s} {.val {which(bad_radii)}} of
               {.arg nodes}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  if (nrow(edges) > 0) {
    bad_edge_coords <- !is.finite(edges$x) | !is.finite(edges$y)
    if (any(bad_edge_coords)) {
      abort(
        c(
          "{.field x} and {.field y} in {.arg edges} must be finite.",
          "x" = "Non-finite coordinates on edge{?s}
                 {.val {unique(edges$edge_id[bad_edge_coords])}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }

    point_counts <- table(edges$edge_id)
    short_edges <- names(point_counts)[point_counts < 2]
    if (length(short_edges) > 0) {
      abort(
        c(
          "Every {.field edge_id} in {.arg edges} needs at least two
           points.",
          "x" = "Edge{?s} {.val {short_edges}} {?has/have} fewer than two."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  }

  bounds_ordered <- length(bounds) == 4 &&
    all(is.finite(bounds)) &&
    bounds[[1]] < bounds[[3]] &&
    bounds[[2]] < bounds[[4]]
  if (!bounds_ordered) {
    abort(
      c(
        "{.arg bounds} must be {.code c(xmin, ymin, xmax, ymax)} with
         strictly ordered, finite limits.",
        "x" = "{.code xmin < xmax} and {.code ymin < ymax} must both hold."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  gap_valid <- length(gap) == 1 &&
    is.numeric(gap) &&
    is.finite(gap) &&
    gap >= 0
  if (!gap_valid) {
    abort(
      "{.arg gap} must be a single finite, non-negative number.",
      error_class = "ggdag_type_error",
      call = call
    )
  }

  n_angles_valid <- length(n_angles) == 1 &&
    is.numeric(n_angles) &&
    is.finite(n_angles) &&
    n_angles == as.integer(n_angles) &&
    n_angles >= 1 &&
    n_angles <= 8
  if (!n_angles_valid) {
    abort(
      c(
        "{.arg n_angles} must be a whole number between 1 and 8.",
        "i" = "It selects the first {.arg n_angles} anchors of the
               preference order."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  n_rings_valid <- length(n_rings) == 1 &&
    is.numeric(n_rings) &&
    is.finite(n_rings) &&
    n_rings == as.integer(n_rings) &&
    n_rings >= 1
  if (!n_rings_valid) {
    abort(
      "{.arg n_rings} must be a whole number of at least 1.",
      error_class = "ggdag_type_error",
      call = call
    )
  }

  weight_names <- c("node", "edge", "arrow", "label", "bounds", "prefer")
  if (!is.numeric(weights)) {
    abort(
      c(
        "{.arg weights} must be a named numeric vector.",
        "x" = "It is {.cls {class(weights)[[1]]}}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }
  bad_weights <- weight_names[!is.finite(weights[weight_names])]
  if (length(bad_weights) > 0) {
    abort(
      c(
        "{.arg weights} must contain finite values named
         {.val {weight_names}}.",
        "x" = "Missing or non-finite weight{?s}: {.val {bad_weights}}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  invisible(NULL)
}

#' Candidate label boxes around one node
#'
#' Builds the ring-major candidate grid for a single label: the first
#' `n_angles` anchors of the preference order at each of `n_rings` rings,
#' with 0-based preference `rank` in evaluation order. When `bounds` is
#' given, every candidate whose box spills the panel also gains a slid
#' variant translated by the minimal offset that brings it inside, at
#' rank + 0.5 and with `*` appended to its anchor; an axis the box cannot
#' fit along is left untranslated.
#'
#' @param x,y Node center in mm.
#' @param radius Node disc radius in mm.
#' @param gap Ring 1 clearance between the disc edge and the box edge in mm.
#' @param width,height Label box extent in mm.
#' @param n_angles,n_rings Candidate grid size.
#' @param bounds Panel extent `c(xmin, ymin, xmax, ymax)` in mm, or `NULL`
#'   to build no slid variants.
#' @return A list of parallel vectors `anchor`, `rank`, `x`, `y` (box
#'   centers), and `xmin`, `ymin`, `xmax`, `ymax`, one element per
#'   candidate.
#' @noRd
label_candidates <- function(
  x,
  y,
  radius,
  gap,
  width,
  height,
  n_angles,
  n_rings,
  bounds = NULL
) {
  anchors <- c("ne", "nw", "se", "sw", "n", "s", "e", "w")[seq_len(n_angles)]
  sign_x <- c(1, -1, 1, -1, 0, 0, 1, -1)[seq_len(n_angles)]
  sign_y <- c(1, 1, -1, -1, 1, -1, 0, 0)[seq_len(n_angles)]
  diagonal <- sign_x != 0 & sign_y != 0

  ring <- rep(seq_len(n_rings), each = n_angles)
  anchor_index <- rep(seq_len(n_angles), times = n_rings)

  # Distance from the node center to the box's nearest point: radius + gap
  # at ring 1, stepping out by half the box diagonal per further ring. Along
  # a diagonal ray the nearest point is the facing corner, so each
  # coordinate offset is that distance over sqrt(2); along a cardinal ray it
  # is the facing side midpoint, at the full distance.
  distance <- radius + gap + (ring - 1) * sqrt(width^2 + height^2) / 2
  reach <- ifelse(diagonal[anchor_index], distance / sqrt(2), distance)

  center_x <- x + sign_x[anchor_index] * (reach + width / 2)
  center_y <- y + sign_y[anchor_index] * (reach + height / 2)

  cand <- list(
    anchor = anchors[anchor_index],
    rank = seq_along(center_x) - 1,
    x = center_x,
    y = center_y,
    xmin = center_x - width / 2,
    ymin = center_y - height / 2,
    xmax = center_x + width / 2,
    ymax = center_y + height / 2
  )
  if (is.null(bounds)) {
    return(cand)
  }

  # Minimal translation that brings a spilling box inside the panel; a box
  # can spill at most one side per axis, so the two shifts never both apply.
  dx <- pmax(0, bounds[[1]] - cand$xmin) - pmax(0, cand$xmax - bounds[[3]])
  dy <- pmax(0, bounds[[2]] - cand$ymin) - pmax(0, cand$ymax - bounds[[4]])
  if (width > bounds[[3]] - bounds[[1]]) {
    dx[] <- 0
  }
  if (height > bounds[[4]] - bounds[[2]]) {
    dy[] <- 0
  }
  spill <- dx != 0 | dy != 0
  if (!any(spill)) {
    return(cand)
  }

  idx <- which(spill)
  list(
    anchor = c(cand$anchor, paste0(cand$anchor[idx], "*")),
    rank = c(cand$rank, cand$rank[idx] + 0.5),
    x = c(cand$x, cand$x[idx] + dx[idx]),
    y = c(cand$y, cand$y[idx] + dy[idx]),
    xmin = c(cand$xmin, cand$xmin[idx] + dx[idx]),
    ymin = c(cand$ymin, cand$ymin[idx] + dy[idx]),
    xmax = c(cand$xmax, cand$xmax[idx] + dx[idx]),
    ymax = c(cand$ymax, cand$ymax[idx] + dy[idx])
  )
}

#' Score candidate boxes against the static obstacles
#'
#' Computes the placement score of every candidate against the obstacles
#' that do not depend on other labels: node discs (both the hard penetration
#' depth and the soft comfort zone around discs other than the label's own),
#' sampled edge points, arrowhead segments, and the panel bounds, plus the
#' proximity pull toward the label's own node and the preference-rank
#' tiebreak. The label overlap term is added later, during assignment.
#'
#' @param cand Candidate list from `label_candidates()`.
#' @param nodes,edges,bounds,weights As in `place_dag_labels()`.
#' @param arrow_segments Data frame from `final_edge_segments()`.
#' @param own_xy Length-2 numeric, the label's own node center; the nearest
#'   disc to it is exempt from the soft term and the proximity pull measures
#'   from it. `NULL` disables both terms.
#' @return A list with `score` (numeric per candidate) and `n_clean` (count
#'   of candidates with no violations at all; soft-zone penetration and
#'   distance are not violations).
#' @noRd
score_label_candidates <- function(
  cand,
  nodes,
  edges,
  arrow_segments,
  bounds,
  weights,
  own_xy = NULL
) {
  n_cand <- length(cand$x)

  # Node discs: total penetration depth past each disc's required clearance,
  # plus the depth into the soft zone beyond every disc except the label's
  # own node's.
  node_penalty <- numeric(n_cand)
  soft_penalty <- numeric(n_cand)
  if (nrow(nodes) > 0) {
    own <- if (is.null(own_xy)) {
      0L
    } else {
      which.min((nodes$x - own_xy[[1]])^2 + (nodes$y - own_xy[[2]])^2)
    }
    i <- rep(seq_len(n_cand), times = nrow(nodes))
    j <- rep(seq_len(nrow(nodes)), each = n_cand)
    dist <- rect_point_dist(
      cand$xmin[i],
      cand$ymin[i],
      cand$xmax[i],
      cand$ymax[i],
      nodes$x[j],
      nodes$y[j]
    )
    depth <- pmax(0, nodes$radius[j] + label_node_clearance - dist)
    node_penalty <- rowSums(matrix(depth, nrow = n_cand))
    soft <- pmax(0, label_soft_margin - (dist - nodes$radius[j]))
    soft[j == own] <- 0
    soft_penalty <- rowSums(matrix(soft, nrow = n_cand))
  }

  # Edges: count of sampled polyline points inside or too near the box.
  # Only points near the candidate region can violate, so the rest are
  # dropped before building the candidate-point grid.
  edge_violations <- numeric(n_cand)
  if (nrow(edges) > 0) {
    px <- edges$x
    py <- edges$y
    near <- px >= min(cand$xmin) - label_edge_clearance &
      px <= max(cand$xmax) + label_edge_clearance &
      py >= min(cand$ymin) - label_edge_clearance &
      py <= max(cand$ymax) + label_edge_clearance
    px <- px[near]
    py <- py[near]
    if (length(px) > 0) {
      i <- rep(seq_len(n_cand), times = length(px))
      j <- rep(seq_along(px), each = n_cand)
      dist <- rect_point_dist(
        cand$xmin[i],
        cand$ymin[i],
        cand$xmax[i],
        cand$ymax[i],
        px[j],
        py[j]
      )
      edge_violations <- rowSums(
        matrix(dist < label_edge_clearance, nrow = n_cand)
      )
    }
  }

  # Arrowheads: count of final edge segments too near the box, using the
  # true segment distance so a segment crossing the box between its sampled
  # endpoints still counts.
  arrow_violations <- numeric(n_cand)
  if (nrow(arrow_segments) > 0) {
    i <- rep(seq_len(n_cand), times = nrow(arrow_segments))
    j <- rep(seq_len(nrow(arrow_segments)), each = n_cand)
    dist <- rect_segment_dist(
      cand$xmin[i],
      cand$ymin[i],
      cand$xmax[i],
      cand$ymax[i],
      arrow_segments$x1[j],
      arrow_segments$y1[j],
      arrow_segments$x2[j],
      arrow_segments$y2[j]
    )
    arrow_violations <- rowSums(
      matrix(dist < label_arrow_clearance, nrow = n_cand)
    )
  }

  # Bounds: box area outside the panel.
  inside <- rect_overlap_area(
    cand$xmin,
    cand$ymin,
    cand$xmax,
    cand$ymax,
    bounds[[1]],
    bounds[[2]],
    bounds[[3]],
    bounds[[4]]
  )
  outside_area <- pmax(
    (cand$xmax - cand$xmin) * (cand$ymax - cand$ymin) - inside,
    0
  )

  # Distance from the box center to the label's own node center, for the
  # proximity pull. `dist` and `soft` default to 0 when absent so weights
  # vectors from before those terms existed keep working.
  center_dist <- if (is.null(own_xy)) {
    0
  } else {
    sqrt((cand$x - own_xy[[1]])^2 + (cand$y - own_xy[[2]])^2)
  }
  dist_weight <- if ("dist" %in% names(weights)) weights[["dist"]] else 0
  soft_weight <- if ("soft" %in% names(weights)) weights[["soft"]] else 0

  score <- weights[["node"]] *
    node_penalty +
    weights[["edge"]] * edge_violations +
    weights[["arrow"]] * arrow_violations +
    weights[["bounds"]] * outside_area +
    weights[["prefer"]] * cand$rank +
    dist_weight * center_dist +
    soft_weight * soft_penalty

  clean <- node_penalty == 0 &
    edge_violations == 0 &
    arrow_violations == 0 &
    outside_area == 0

  list(score = score, n_clean = sum(clean))
}

#' Final segment of each edge polyline
#'
#' Extracts the last segment of every `edge_id`, in order of first
#' appearance, as the arrowhead zone of that edge.
#'
#' @param edges Data frame with columns `edge_id`, `x`, and `y`.
#' @return A data frame with columns `x1`, `y1`, `x2`, `y2`, one row per
#'   `edge_id`; `(x2, y2)` is the polyline's last point.
#' @noRd
final_edge_segments <- function(edges) {
  if (nrow(edges) == 0) {
    return(
      data.frame(x1 = numeric(), y1 = numeric(), x2 = numeric(), y2 = numeric())
    )
  }

  rows <- split(
    seq_len(nrow(edges)),
    factor(edges$edge_id, levels = unique(edges$edge_id))
  )
  from <- vapply(rows, function(r) r[length(r) - 1L], integer(1))
  to <- vapply(rows, function(r) r[length(r)], integer(1))

  data.frame(
    x1 = edges$x[from],
    y1 = edges$y[from],
    x2 = edges$x[to],
    y2 = edges$y[to]
  )
}

#' Radius of the node disc nearest each label's node center
#'
#' @param x,y Numeric vectors of label node centers.
#' @param nodes Data frame with columns `x`, `y`, and `radius`.
#' @return Numeric vector of radii; 0 when `nodes` has no rows.
#' @noRd
nearest_node_radius <- function(x, y, nodes) {
  if (nrow(nodes) == 0) {
    return(rep(0, length(x)))
  }
  vapply(
    seq_along(x),
    function(i) {
      nodes$radius[which.min((nodes$x - x[i])^2 + (nodes$y - y[i])^2)]
    },
    numeric(1)
  )
}

#' Distance from points to axis-aligned rectangles
#'
#' Euclidean distance from each point to the nearest point of the matching
#' rectangle; 0 when the point lies inside. All arguments recycle.
#'
#' @param xmin,ymin,xmax,ymax Rectangle limits.
#' @param px,py Point coordinates.
#' @return Numeric vector of distances.
#' @noRd
rect_point_dist <- function(xmin, ymin, xmax, ymax, px, py) {
  dx <- pmax.int(xmin - px, px - xmax, 0)
  dy <- pmax.int(ymin - py, py - ymax, 0)
  sqrt(dx^2 + dy^2)
}

#' Overlap area of axis-aligned rectangle pairs
#'
#' @param axmin,aymin,axmax,aymax First rectangle limits.
#' @param bxmin,bymin,bxmax,bymax Second rectangle limits. All arguments
#'   recycle.
#' @return Numeric vector of intersection areas; 0 for disjoint pairs.
#' @noRd
rect_overlap_area <- function(
  axmin,
  aymin,
  axmax,
  aymax,
  bxmin,
  bymin,
  bxmax,
  bymax
) {
  overlap_w <- pmax.int(0, pmin.int(axmax, bxmax) - pmax.int(axmin, bxmin))
  overlap_h <- pmax.int(0, pmin.int(aymax, bymax) - pmax.int(aymin, bymin))
  overlap_w * overlap_h
}

#' Distance from segments to axis-aligned rectangles
#'
#' Exact minimum Euclidean distance between each segment from `(x1, y1)` to
#' `(x2, y2)` and the matching rectangle: 0 when they intersect (tested by
#' Liang-Barsky clipping), otherwise the minimum over the segment endpoints
#' against the rectangle and the rectangle corners against the segment,
#' where the distance between disjoint convex shapes is always attained.
#'
#' @param xmin,ymin,xmax,ymax Rectangle limit vectors.
#' @param x1,y1,x2,y2 Segment endpoint vectors, the same length as the
#'   rectangle limits.
#' @return Numeric vector of distances, one per rectangle-segment pair.
#' @noRd
rect_segment_dist <- function(xmin, ymin, xmax, ymax, x1, y1, x2, y2) {
  n <- length(xmin)

  p <- list(x1 - x2, x2 - x1, y1 - y2, y2 - y1)
  q <- list(x1 - xmin, xmax - x1, y1 - ymin, ymax - y1)
  enter <- numeric(n)
  exit <- rep(1, n)
  outside <- logical(n)
  for (k in 1:4) {
    zero <- p[[k]] == 0
    # A segment running parallel to this rectangle side lies entirely
    # beyond it when q < 0.
    outside <- outside | (zero & q[[k]] < 0)
    t_hit <- q[[k]] / p[[k]]
    entering <- !zero & p[[k]] < 0
    leaving <- !zero & p[[k]] > 0
    enter[entering] <- pmax.int(enter[entering], t_hit[entering])
    exit[leaving] <- pmin.int(exit[leaving], t_hit[leaving])
  }
  intersects <- !outside & enter <= exit

  dist <- pmin.int(
    rect_point_dist(xmin, ymin, xmax, ymax, x1, y1),
    rect_point_dist(xmin, ymin, xmax, ymax, x2, y2),
    dist_to_edge(xmin, ymin, x1, y1, x2, y2),
    dist_to_edge(xmin, ymax, x1, y1, x2, y2),
    dist_to_edge(xmax, ymin, x1, y1, x2, y2),
    dist_to_edge(xmax, ymax, x1, y1, x2, y2)
  )
  dist[intersects] <- 0
  dist
}

# Layer machinery for the automatic label geoms. The stat gathers everything
# the placement engine needs, in data units, as role-tagged rows of one data
# frame; the geoms defer measurement and placement to draw time, when the
# panel's size in millimetres is known, so the same plot places its labels
# the same way at every device size.

StatNodesLabelAuto <- ggplot2::ggproto(
  "StatNodesLabelAuto",
  ggplot2::Stat,
  required_aes = c("x", "y", "label"),
  optional_aes = c("xend", "yend"),
  extra_params = c(
    "na.rm",
    "node_size",
    "n_edge_points",
    "n_node_points",
    "edge_geometry"
  ),
  compute_layer = function(data, params, layout) {
    node_size <- params$node_size %||% ggdag_option("node_size", 16)
    n_edge_points <- params$n_edge_points %||% 20
    has_edges <- all(c("xend", "yend") %in% names(data))

    # Every node in the layer is an obstacle, whether or not it carries a
    # label, and a node an edge only arrives at is one too. The routed edge
    # grob collects its obstacles from the same helper, so the router is
    # given one node set however the panel is drawn.
    panels <- unique(data$PANEL)
    all_nodes <- do.call(
      rbind,
      lapply(seq_along(panels), function(i) {
        centers <- panel_node_centers(
          data[data$PANEL == panels[i], , drop = FALSE]
        )
        centers$PANEL <- panels[i]
        centers
      })
    )

    edge_rows <- NULL
    if (has_edges) {
      edges <- unique(data[
        !is.na(data$xend),
        c("x", "y", "xend", "yend", "PANEL")
      ])
      edge_points <- repel_edge_points(
        edges,
        n_edge_points,
        params$edge_geometry,
        layout,
        include_endpoints = TRUE,
        trace_arrows = TRUE
      )
      if (!is.null(edge_points)) {
        # A routed edge arrives as the two endpoints of its chord and the
        # spec it is routed with; the geom rebuilds its path at draw time,
        # where the millimetres the router works in are known.
        edge_rows <- data.frame(
          ggdag_role = "edge",
          label = "",
          x = edge_points$x,
          y = edge_points$y,
          edge_id = edge_points$edge_id,
          PANEL = edge_points$PANEL,
          stringsAsFactors = FALSE
        )
        for (name in route_spec_columns) {
          edge_rows[[name]] <- spec_column(
            edge_points,
            name,
            route_spec_blanks[[name]]
          )
        }
      }
    }

    label_rows <- data
    if (has_edges) {
      label_rows <- dplyr::select(label_rows, -"xend", -"yend")
    }
    label_rows <- label_rows[!is.na(label_rows$label), , drop = FALSE]
    label_rows <- one_row_per_node(label_rows)
    if (nrow(label_rows) > 0) {
      label_rows$ggdag_role <- "label"
    }

    node_rows <- data.frame(
      ggdag_role = "node",
      label = "",
      x = all_nodes$x,
      y = all_nodes$y,
      node_size = node_size,
      PANEL = all_nodes$PANEL,
      stringsAsFactors = FALSE
    )

    out <- dplyr::bind_rows(label_rows, node_rows, edge_rows)
    out$group <- -1L
    out
  }
)

GeomDagLabelAuto <- ggplot2::ggproto(
  "GeomDagLabelAuto",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = "black",
    fill = "white",
    size = 3.88,
    alpha = NA,
    family = "",
    fontface = 1,
    lineheight = 1.2
  ),
  draw_key = ggplot2::draw_key_label,
  boxed = TRUE,
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    gap = 2,
    edge_cap = NULL,
    n_edge_points = NULL,
    label.padding = grid::unit(0.25, "lines"),
    label.r = grid::unit(0.15, "lines"),
    label.size = NA,
    min.segment.length = 5,
    segment.colour = "grey50",
    segment.size = 0.5,
    na.rm = FALSE
  ) {
    coords <- coord$transform(data, panel_params)
    if (!"ggdag_role" %in% names(coords)) {
      coords$ggdag_role <- "label"
    }

    labels <- coords[
      coords$ggdag_role == "label" &
        !is.na(coords$label) &
        coords$label != "",
      ,
      drop = FALSE
    ]
    if (nrow(labels) == 0) {
      return(ggplot2::zeroGrob())
    }

    nodes <- coords[coords$ggdag_role == "node", , drop = FALSE]
    # the router routes around the discs the plot draws, so an unknown node
    # size falls back to the option the node layers are drawn at, which is
    # the fallback the routed edge grob takes as well
    default_size <- ggdag_option("node_size", 16)
    if (!"node_size" %in% names(nodes)) {
      nodes$node_size <- default_size
    }
    nodes$node_size[is.na(nodes$node_size)] <- default_size
    node_size <- if (nrow(nodes) > 0) nodes$node_size[[1]] else default_size

    edges <- coords[coords$ggdag_role == "edge", , drop = FALSE]
    if (!"edge_id" %in% names(edges)) {
      edges$edge_id <- character(nrow(edges))
    }
    # A routed edge arrives as its chord and the spec it is routed with, so
    # the spec travels to draw time with it.
    for (name in route_spec_columns) {
      if (!name %in% names(edges)) {
        edges[[name]] <- rep(route_spec_blanks[[name]], nrow(edges))
      }
    }

    # Positions stay in the panel's native units here; makeContent() converts
    # them with the panel viewport in place, so the millimetres it measures
    # are the millimetres of the device the plot is drawn on.
    grid::gTree(
      labels = labels,
      nodes = nodes[, c("x", "y", "node_size"), drop = FALSE],
      edges = edges[,
        c("edge_id", "x", "y", route_spec_columns),
        drop = FALSE
      ],
      params = list(
        boxed = isTRUE(self$boxed),
        gap = gap,
        edge_cap = edge_cap %||% ggdag_option("edge_cap", 8),
        node_size = node_size,
        n_edge_points = n_edge_points %||% 20,
        label.padding = label.padding,
        label.r = label.r,
        label.size = label.size,
        min.segment.length = min.segment.length,
        segment.colour = segment.colour,
        segment.size = segment.size
      ),
      cl = "dag_labels_auto"
    )
  }
)

GeomDagTextAuto <- ggplot2::ggproto(
  "GeomDagTextAuto",
  GeomDagLabelAuto,
  default_aes = ggplot2::aes(
    colour = "black",
    size = 3.88,
    alpha = NA,
    family = "",
    fontface = 1,
    lineheight = 1.2
  ),
  draw_key = ggplot2::draw_key_text,
  boxed = FALSE
)

#' Compute and draw automatically placed labels
#'
#' Runs at draw time, inside the panel viewport, where positions in native
#' units convert to true millimetres: it measures every label's text, traces
#' the drawn edges up to the arrowhead, calls `place_dag_labels()`, and
#' emits the leader lines, boxes, and text of the final placement.
#'
#' @param x A `dag_labels_auto` gTree built by `GeomDagLabelAuto$draw_panel()`.
#' @return `x`, with children set to the drawn grobs.
#' @exportS3Method grid::makeContent
#' @noRd
makeContent.dag_labels_auto <- function(x) {
  labels <- x$labels
  par <- x$params

  mm_x <- function(value) {
    if (length(value) == 0) {
      return(numeric())
    }
    grid::convertX(grid::unit(value, "npc"), "mm", valueOnly = TRUE)
  }
  mm_y <- function(value) {
    if (length(value) == 0) {
      return(numeric())
    }
    grid::convertY(grid::unit(value, "npc"), "mm", valueOnly = TRUE)
  }
  as_unit <- function(value, default_units) {
    if (grid::is.unit(value)) value else grid::unit(value, default_units)
  }

  panel_width <- grid::convertWidth(
    grid::unit(1, "npc"),
    "mm",
    valueOnly = TRUE
  )
  panel_height <- grid::convertHeight(
    grid::unit(1, "npc"),
    "mm",
    valueOnly = TRUE
  )

  n <- nrow(labels)
  label_gp <- function(i) {
    grid::gpar(
      col = ggplot2::alpha(labels$colour[i], labels$alpha[i]),
      fontsize = labels$size[i] * ggplot2::.pt,
      fontfamily = labels$family[i],
      fontface = labels$fontface[i],
      lineheight = labels$lineheight[i]
    )
  }
  text_grobs <- lapply(seq_len(n), function(i) {
    grid::textGrob(labels$label[i], gp = label_gp(i))
  })

  padding <- as_unit(par$label.padding, "lines")
  pad_w <- grid::convertWidth(padding, "mm", valueOnly = TRUE)
  pad_h <- grid::convertHeight(padding, "mm", valueOnly = TRUE)
  widths <- vapply(
    text_grobs,
    function(tg) {
      grid::convertWidth(grid::grobWidth(tg), "mm", valueOnly = TRUE)
    },
    numeric(1)
  ) +
    2 * pad_w
  heights <- vapply(
    text_grobs,
    function(tg) {
      grid::convertHeight(grid::grobHeight(tg), "mm", valueOnly = TRUE)
    },
    numeric(1)
  ) +
    2 * pad_h

  label_input <- data.frame(
    id = as.character(seq_len(n)),
    x = mm_x(labels$x),
    y = mm_y(labels$y),
    width = widths,
    height = heights
  )
  node_input <- data.frame(
    x = mm_x(x$nodes$x),
    y = mm_y(x$nodes$y),
    radius = node_radius_mm(x$nodes$node_size)
  )
  edges_mm <- data.frame(
    edge_id = x$edges$edge_id,
    x = mm_x(x$edges$x),
    y = mm_y(x$edges$y),
    stringsAsFactors = FALSE
  )
  edges_mm <- route_label_obstacles(
    edges_mm,
    x$edges,
    node_input,
    par,
    c(0, 0, panel_width, panel_height)
  )
  edge_input <- shorten_edge_tails(edges_mm, par$edge_cap)

  placed <- place_dag_labels(
    label_input,
    node_input,
    edge_input,
    bounds = c(0, 0, panel_width, panel_height),
    gap = par$gap
  )

  radius <- nearest_node_radius(label_input$x, label_input$y, node_input)

  leaders <- list()
  boxes <- list()
  texts <- list()
  for (i in seq_len(n)) {
    center_x <- placed$x[i]
    center_y <- placed$y[i]
    box <- c(
      xmin = center_x - widths[i] / 2,
      ymin = center_y - heights[i] / 2,
      xmax = center_x + widths[i] / 2,
      ymax = center_y + heights[i] / 2
    )

    leader <- label_leader_grob(
      box,
      label_input$x[i],
      label_input$y[i],
      radius[i],
      par$min.segment.length,
      par$segment.colour,
      par$segment.size
    )
    if (!is.null(leader)) {
      leaders[[length(leaders) + 1]] <- leader
    }

    if (par$boxed) {
      border <- if (is.na(par$label.size)) {
        list(col = NA, lwd = 0)
      } else {
        list(
          col = ggplot2::alpha(labels$colour[i], labels$alpha[i]),
          lwd = par$label.size * ggplot2::.pt
        )
      }
      boxes[[length(boxes) + 1]] <- grid::roundrectGrob(
        x = grid::unit(center_x, "mm"),
        y = grid::unit(center_y, "mm"),
        width = grid::unit(widths[i], "mm"),
        height = grid::unit(heights[i], "mm"),
        r = as_unit(par$label.r, "lines"),
        gp = grid::gpar(
          fill = ggplot2::fill_alpha(labels$fill[i], labels$alpha[i]),
          col = border$col,
          lwd = border$lwd
        )
      )
    }

    texts[[length(texts) + 1]] <- grid::textGrob(
      labels$label[i],
      x = grid::unit(center_x, "mm"),
      y = grid::unit(center_y, "mm"),
      gp = label_gp(i)
    )
  }

  grid::setChildren(x, do.call(grid::gList, c(leaders, boxes, texts)))
}

#' Rebuild the drawn path of every routed edge, in millimetres
#'
#' A routed edge reaches the label grob as the two endpoints of its chord and
#' the spec the layer routes it with, because where it goes is decided in
#' millimetres at draw time. This calls the same pure router the arrows are
#' drawn with, on the same node discs, panel bounds, cap, and options, and
#' thins each path to the resolution the other edges are traced at. Edges no
#' routed layer draws are returned untouched.
#'
#' @param edges Traced obstacle points in millimetres: `edge_id`, `x`, `y`.
#' @param spec The routing columns of the same rows, as the stat carried them.
#' @param nodes Node centres in millimetres with their `radius`.
#' @param par The gTree parameters, carrying `node_size`, `n_edge_points`, and
#'   `edge_cap`.
#' @param bounds The panel in millimetres, `c(xmin, ymin, xmax, ymax)`.
#' @return `edges`, with each routed edge's two rows replaced by its path.
#' @noRd
route_label_obstacles <- function(edges, spec, nodes, par, bounds) {
  tagged <- !is.na(spec$route_style)
  if (!any(tagged)) {
    return(edges)
  }
  fixed <- !is.na(spec$route_fixed) & spec$route_fixed

  # One row per edge, from the first and last of the points it was traced
  # with. A routed edge is traced as its chord, so those two points are the
  # node centres; an edge the user pinned is traced as the path it is drawn
  # along, whose ends are the same centres.
  chord_rows <- function(mask) {
    if (!any(mask)) {
      return(NULL)
    }
    ids <- unique(edges$edge_id[mask])
    first <- match(ids, edges$edge_id)
    last <- length(edges$edge_id) - match(ids, rev(edges$edge_id)) + 1L
    data.frame(
      edge_id = ids,
      x = edges$x[first],
      y = edges$y[first],
      xend = edges$x[last],
      yend = edges$y[last],
      style = spec$route_style[first],
      clearance = spec$route_clearance[first],
      sep = spec$route_sep[first],
      layer_axis = spec$route_layer_axis[first],
      cap = spec$route_cap[first],
      curvature = spec$curvature[first],
      stringsAsFactors = FALSE
    )
  }

  chords <- chord_rows(tagged)
  pinned <- chord_rows(fixed)

  # The router names its nodes by position, so an endpoint identifies the
  # node it belongs to whichever layer measured it.
  router_nodes <- data.frame(
    name = routed_position_keys(nodes$x, nodes$y),
    x = nodes$x,
    y = nodes$y,
    r = nodes$radius,
    stringsAsFactors = FALSE
  )
  nearest <- function(px, py) {
    vapply(
      seq_along(px),
      function(i) {
        distance <- (router_nodes$x - px[[i]])^2 + (router_nodes$y - py[[i]])^2
        router_nodes$name[[which.min(distance)]]
      },
      character(1)
    )
  }
  name_ends <- function(edge) {
    if (is.null(edge)) {
      return(edge)
    }
    edge$from <- nearest(edge$x, edge$y)
    edge$to <- nearest(edge$xend, edge$yend)
    edge
  }
  chords <- name_ends(chords)
  pinned <- name_ends(pinned)

  # An edge the user curved is drawn as that arc, sampled in millimetres by
  # the drawn grob, and an explicit zero is drawn as the chord. Neither is
  # rerouted, but the router prices every detour against them, so it is
  # shown them exactly as the drawn grob shows them.
  pinned_input <- NULL
  if (!is.null(pinned)) {
    pinned_input <- data.frame(
      from = pinned$from,
      to = pinned$to,
      curvature = pinned$curvature,
      stringsAsFactors = FALSE
    )
    pinned_input$fixed_path <- lapply(seq_len(nrow(pinned)), function(i) {
      if (pinned$curvature[[i]] == 0) {
        return(NULL)
      }
      sample_curved_edge(
        pinned$x[[i]],
        pinned$y[[i]],
        pinned$xend[[i]],
        pinned$yend[[i]],
        curvature = pinned$curvature[[i]],
        n = routed_fixed_path_n
      )
    })
  }

  radius <- node_radius_mm(par$node_size)
  n_points <- (par$n_edge_points %||% 20) + 2

  paths <- vector("list", nrow(chords))
  groups <- paste(
    chords$style,
    chords$clearance,
    chords$sep,
    chords$layer_axis,
    chords$cap,
    sep = "\r"
  )
  for (rows in split(seq_len(nrow(chords)), groups)) {
    settings <- chords[rows[[1]], , drop = FALSE]
    edge_input <- data.frame(
      from = chords$from[rows],
      to = chords$to[rows],
      curvature = NA_real_,
      stringsAsFactors = FALSE
    )
    if (!is.null(pinned_input)) {
      edge_input$fixed_path <- vector("list", nrow(edge_input))
      edge_input <- rbind(edge_input, pinned_input)
    }
    routed <- route_edges_mm(
      nodes = router_nodes,
      edges = edge_input,
      bounds = bounds,
      cap = if (is.na(settings$cap)) par$edge_cap else settings$cap,
      mode = settings$style,
      opts = route_opts(
        r_ref = radius,
        m = if (is.na(settings$clearance)) NULL else settings$clearance,
        sep_e = if (is.na(settings$sep)) NULL else settings$sep,
        layer_axis = if (is.na(settings$layer_axis)) {
          "auto"
        } else {
          settings$layer_axis
        }
      )
    )
    paths[rows] <- routed$paths[seq_along(rows)]
  }

  # The router samples at half a millimetre, so its own vertices carry no
  # information a thinning would lose: an obstacle every `n_points` along
  # the path is what every other traced edge contributes.
  routed_rows <- do.call(
    rbind,
    lapply(seq_len(nrow(chords)), function(i) {
      sampled <- sample_polyline(
        paths[[i]]$x,
        paths[[i]]$y,
        n_points,
        keep_vertices = FALSE
      )
      data.frame(
        edge_id = chords$edge_id[[i]],
        x = sampled$x,
        y = sampled$y,
        stringsAsFactors = FALSE
      )
    })
  )

  rbind(
    edges[!tagged, c("edge_id", "x", "y"), drop = FALSE],
    routed_rows
  )
}

#' Leader line from a node disc to its label box
#'
#' A label pushed past `min_segment_length` millimetres from its node's disc
#' gets a line from the disc edge to the nearest point of the box, so the
#' reader can tell whose label it is. A closer label, or one overlapping its
#' node, gets none.
#'
#' @param box Named numeric with `xmin`, `ymin`, `xmax`, `ymax`, in mm.
#' @param node_x,node_y Node center in mm.
#' @param radius Node disc radius in mm.
#' @param min_segment_length Distance from the disc past which a leader is
#'   drawn, in mm.
#' @param colour,size Leader line colour and linewidth.
#' @return A segments grob, or `NULL` when no leader is needed.
#' @noRd
label_leader_grob <- function(
  box,
  node_x,
  node_y,
  radius,
  min_segment_length,
  colour,
  size
) {
  clearance <- rect_point_dist(
    box[["xmin"]],
    box[["ymin"]],
    box[["xmax"]],
    box[["ymax"]],
    node_x,
    node_y
  ) -
    radius
  if (clearance <= min_segment_length) {
    return(NULL)
  }

  near_x <- min(max(node_x, box[["xmin"]]), box[["xmax"]])
  near_y <- min(max(node_y, box[["ymin"]]), box[["ymax"]])
  dx <- near_x - node_x
  dy <- near_y - node_y
  length <- sqrt(dx^2 + dy^2)
  if (length <= radius) {
    return(NULL)
  }

  grid::segmentsGrob(
    x0 = grid::unit(node_x + radius * dx / length, "mm"),
    y0 = grid::unit(node_y + radius * dy / length, "mm"),
    x1 = grid::unit(near_x, "mm"),
    y1 = grid::unit(near_y, "mm"),
    gp = grid::gpar(col = colour, lwd = size * ggplot2::.pt)
  )
}

#' Shorten each edge polyline at its arrowhead end
#'
#' Cuts `cut` millimetres off the end of every `edge_id`, where the drawn
#' edge is resected to make room for the node and its arrowhead, so the final
#' segment of what remains is the arrowhead actually on the page. An edge
#' shorter than the cut disappears entirely.
#'
#' @param edges Data frame with columns `edge_id`, `x`, and `y`, in mm.
#' @param cut Length to remove in mm.
#' @return The shortened edges, in the same shape.
#' @noRd
shorten_edge_tails <- function(edges, cut) {
  if (nrow(edges) == 0 || cut <= 0) {
    return(edges)
  }

  pieces <- lapply(
    split(
      seq_len(nrow(edges)),
      factor(edges$edge_id, levels = unique(edges$edge_id))
    ),
    function(rows) {
      trimmed <- shorten_polyline_tail(edges$x[rows], edges$y[rows], cut)
      if (is.null(trimmed)) {
        return(NULL)
      }
      data.frame(
        edge_id = edges$edge_id[rows[[1]]],
        x = trimmed$x,
        y = trimmed$y,
        stringsAsFactors = FALSE
      )
    }
  )
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0) {
    return(edges[0, , drop = FALSE])
  }

  do.call(rbind, pieces)
}

#' Cut a length off the end of one polyline
#'
#' @param px,py Ordered polyline coordinates.
#' @param cut Length to remove from the end, along the path.
#' @return A list with the shortened `x` and `y`, ending exactly `cut` from
#'   the old end, or `NULL` when the whole polyline is shorter than the cut.
#' @noRd
shorten_polyline_tail <- function(px, py, cut) {
  segments <- sqrt(diff(px)^2 + diff(py)^2)
  # Distance from each vertex to the last one, along the path.
  from_end <- c(rev(cumsum(rev(segments))), 0)
  if (from_end[[1]] <= cut) {
    return(NULL)
  }

  last_kept <- max(which(from_end > cut))
  t <- (from_end[[last_kept]] - cut) /
    (from_end[[last_kept]] - from_end[[last_kept + 1]])
  keep <- seq_len(last_kept)
  list(
    x = c(
      px[keep],
      px[[last_kept]] + t * (px[[last_kept + 1]] - px[[last_kept]])
    ),
    y = c(
      py[keep],
      py[[last_kept]] + t * (py[[last_kept + 1]] - py[[last_kept]])
    )
  )
}

#' Automatically placed node labels
#'
#' `geom_dag_label_auto()` and `geom_dag_text_auto()` label the nodes of a
#' DAG with deterministic, draw-time placement: each label is measured on the
#' device the plot is drawn on and then placed so that label boxes avoid node
#' discs, drawn edges, arrowheads, one another, and the panel edge. Among
#' positions that avoid those obstacles, the placement prefers spots close to
#' the label's own node and clear of other nodes' discs, so each label reads
#' as belonging to its node. Unlike
#' [geom_dag_label_repel()], no simulation and no random numbers are
#' involved, so the same plot always places its labels the same way.
#' `geom_dag_label_auto()` draws each label in a borderless rounded box;
#' `geom_dag_text_auto()` draws the text alone.
#'
#' Like the repel geoms, these layers read the rest of the plot they are
#' added to: the node size comes from the plot's [geom_dag_point()] or
#' [geom_dag_node()] layer, and edges are traced along the paths the plot's
#' edge layers draw, including the arc of [geom_dag_edges_arc()] and the
#' per-edge curvature drawn by the ggarrow engine (see [curve_edge()]). A
#' label placed further from its node than `min.segment.length` gets a leader
#' line from the node disc to the label box.
#'
#' @inheritParams geom_dag_arrow
#' @param node_size The size of the plot's nodes, as given to
#'   [geom_dag_point()]. `NULL`, the default, discovers it from the plot.
#' @param n_edge_points Number of points traced along each drawn edge as
#'   obstacles. `NULL`, the default, uses 20.
#' @param n_node_points Accepted for compatibility with the repel label
#'   geoms; the automatic placement describes each node by its drawn disc, so
#'   this is ignored.
#' @param edge_cap The distance in millimetres that drawn edges stop short of
#'   the node, as in [geom_dag()]; the traced edges are shortened by the same
#'   amount so the arrowhead zone each label avoids ends where the drawn
#'   arrowhead does. `NULL`, the default, uses the `ggdag.edge_cap` option
#'   (8 mm).
#' @param gap Clearance in millimetres between a node disc and its label box.
#' @param label.padding Padding around the label text, as a [grid::unit()].
#' @param label.r Radius of the label box corners, as a [grid::unit()].
#' @param label.size Width of the label box border in millimetres. The
#'   default, `NA`, draws no border.
#' @param min.segment.length Distance in millimetres from the node disc past
#'   which a label gets a leader line back to its node.
#' @param segment.colour,segment.size Colour and linewidth of the leader
#'   lines.
#' @param box.padding,max.overlaps Accepted for compatibility with the repel
#'   label geoms and ignored.
#'
#' @return A layer that can be added to a ggplot.
#'
#' @examples
#' library(ggplot2)
#'
#' dag <- dagify(
#'   y ~ m + x,
#'   m ~ x,
#'   exposure = "x",
#'   outcome = "y",
#'   labels = c(x = "Exposure", m = "Mediator", y = "Outcome")
#' )
#'
#' ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto) +
#'   theme_dag()
#'
#' ggplot(dag, aes_dag()) +
#'   geom_dag_point() +
#'   geom_dag_edges_link() +
#'   geom_dag_text_auto(aes(label = label)) +
#'   theme_dag()
#'
#' @export
#' @rdname label_auto
geom_dag_label_auto <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  node_size = NULL,
  n_edge_points = NULL,
  n_node_points = NULL,
  edge_cap = NULL,
  gap = 2,
  label.padding = grid::unit(0.25, "lines"),
  label.r = grid::unit(0.15, "lines"),
  label.size = NA,
  min.segment.length = 5,
  segment.colour = "grey50",
  segment.size = 0.5,
  box.padding = NULL,
  max.overlaps = NULL,
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodesLabelAuto,
    geom = GeomDagLabelAuto,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      node_size = node_size,
      n_edge_points = n_edge_points,
      n_node_points = n_node_points,
      edge_cap = edge_cap,
      gap = gap,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      min.segment.length = min.segment.length,
      segment.colour = segment.colour,
      segment.size = segment.size,
      ...
    )
  )

  dag_layer(layer, discover = c("node_size", "edge_geometry"), debug = TRUE)
}

geom_dag_label_auto <- dag_node_aware(geom_dag_label_auto, extra = "edge_cap")

#' @export
#' @rdname label_auto
geom_dag_text_auto <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  node_size = NULL,
  n_edge_points = NULL,
  n_node_points = NULL,
  edge_cap = NULL,
  gap = 2,
  label.padding = grid::unit(0.25, "lines"),
  min.segment.length = 5,
  segment.colour = "grey50",
  segment.size = 0.5,
  box.padding = NULL,
  max.overlaps = NULL,
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodesLabelAuto,
    geom = GeomDagTextAuto,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      node_size = node_size,
      n_edge_points = n_edge_points,
      n_node_points = n_node_points,
      edge_cap = edge_cap,
      gap = gap,
      label.padding = label.padding,
      min.segment.length = min.segment.length,
      segment.colour = segment.colour,
      segment.size = segment.size,
      ...
    )
  )

  dag_layer(layer, discover = c("node_size", "edge_geometry"), debug = TRUE)
}

geom_dag_text_auto <- dag_node_aware(geom_dag_text_auto, extra = "edge_cap")
