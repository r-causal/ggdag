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
#' disc nearest to the label's node center.
#'
#' Each candidate box is scored as a weighted sum of penalties: node disc
#' penetration depth (including the clearance margin), the count of sampled
#' edge points inside or within the edge margin of the box, the count of
#' final edge segments (the arrowhead zone of each `edge_id`) within the
#' arrow margin of the box, the overlap area with other placed labels, the
#' box area outside `bounds`, and the candidate's preference rank as a pure
#' tiebreak. Labels are assigned most constrained first (fewest
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
#'   `edge`, `arrow`, `label`, and `bounds`, plus the `prefer` tiebreak.
#' @return A data frame with one row per label in input order and columns
#'   `id`, `x`, `y` (box centers), `anchor`, and `score`.
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
    prefer = 0.01
  )
) {
  validate_label_placement_inputs(
    labels,
    nodes,
    edges,
    bounds,
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
      n_rings
    )
    scored <- score_label_candidates(
      cand,
      nodes,
      edges,
      arrow_segments,
      bounds,
      weights
    )
    candidates[[i]] <- cand
    static_scores[[i]] <- scored$score
    clean_counts[i] <- scored$n_clean
  }

  # Most constrained labels claim their spots first; ties fall back to input
  # order, keeping the assignment independent of input row permutations.
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
#' Errors with class `ggdag_type_error` on duplicate label ids, non-positive
#' or non-finite label dimensions or node radii, an edge polyline with fewer
#' than two points, unordered bounds, or malformed engine parameters.
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
#' with 0-based preference `rank` in evaluation order.
#'
#' @param x,y Node center in mm.
#' @param radius Node disc radius in mm.
#' @param gap Ring 1 clearance between the disc edge and the box edge in mm.
#' @param width,height Label box extent in mm.
#' @param n_angles,n_rings Candidate grid size.
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
  n_rings
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

  list(
    anchor = anchors[anchor_index],
    rank = seq_along(center_x) - 1,
    x = center_x,
    y = center_y,
    xmin = center_x - width / 2,
    ymin = center_y - height / 2,
    xmax = center_x + width / 2,
    ymax = center_y + height / 2
  )
}

#' Score candidate boxes against the static obstacles
#'
#' Computes the placement score of every candidate against the obstacles
#' that do not depend on other labels: node discs, sampled edge points,
#' arrowhead segments, and the panel bounds, plus the preference-rank
#' tiebreak. The label overlap term is added later, during assignment.
#'
#' @param cand Candidate list from `label_candidates()`.
#' @param nodes,edges,bounds,weights As in `place_dag_labels()`.
#' @param arrow_segments Data frame from `final_edge_segments()`.
#' @return A list with `score` (numeric per candidate) and `n_clean` (count
#'   of candidates with no violations at all).
#' @noRd
score_label_candidates <- function(
  cand,
  nodes,
  edges,
  arrow_segments,
  bounds,
  weights
) {
  n_cand <- length(cand$x)

  # Node discs: total penetration depth past each disc's required clearance.
  node_penalty <- numeric(n_cand)
  if (nrow(nodes) > 0) {
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

  score <- weights[["node"]] *
    node_penalty +
    weights[["edge"]] * edge_violations +
    weights[["arrow"]] * arrow_violations +
    weights[["bounds"]] * outside_area +
    weights[["prefer"]] * cand$rank

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
