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

# Length in mm of the arrowhead zone at the end of every edge polyline. The
# points of a path within this distance of its last point are measured with
# the arrow margin rather than the edge margin, so a box keeps further from
# the drawn head than from the stroke leading up to it. This is a millimetre
# calibration of where the wider margin applies; the drawn head itself is
# shorter.
label_arrow_zone <- 5

# Soft comfort zone in mm around node discs other than a label's own node.
# A box inside this zone is not violating, but its penetration depth is
# penalized (per the `soft` weight) so labels drift away from foreign discs
# when an equally near spot is free of them.
label_soft_margin <- 8

# Off-grid candidates fill the space between the disc and each reach
# threshold at this many clearance steps per threshold, on evenly spaced rays.
# Beyond the reach itself, a few sparser levels at these multiples of the
# reach give a label somewhere to go when nothing nearer is admissible.
label_reach_steps <- 8L
label_far_reach <- c(1.5, 2, 3)

# A leader is priced at its own length, from the disc edge to the box, and a
# leader that crosses drawn ink, a node disc, or another label's box is
# priced as though it were longer still: each ink point within the edge
# margin of the leader adds this many mm to its length, each disc it crosses
# adds this multiple of that disc's radius, and each box it crosses adds this
# many mm. The ink points are sampled every 0.5 mm, so an edge crossing puts
# about four of them within the margin and costs about 16 mm of leader. At
# 2 mm per point a short leader over an edge still beat a longer one over
# open space often enough to leave the very big scene with a third more
# crossings and leaders a fifth longer on average.
label_leader_ink_cost <- 4
label_leader_disc_cost <- 4
label_leader_box_cost <- 20

# A box slid back inside the panel stops this far, in mm, inside the border
# rather than exactly on it. A slide computes the box limit as
# xmin + (bound - xmin), which floating-point addition need not return as
# exactly bound, and the box is later rebuilt from its centre and width, so
# a box slid exactly to the border can read as a hair outside it. The nudge
# is invisible on the page and far larger than that rounding.
label_slide_nudge <- 1e-9

# The local-improvement sweeps stop as soon as one changes nothing; this caps
# them so placement always terminates in bounded time. The repair rounds
# that follow are capped the same way, as is the number of blocked spots one
# label tries to take over from another.
label_max_sweeps <- 10L
label_max_repairs <- 5L
label_max_ejections <- 50L

# Spacing in mm of the fallback candidate grid over the panel, built only
# for a label whose rays all end on obstacles or other labels.
label_grid_spacing <- 2

# The local grid built around the node of a label with no admissible
# candidate within reach: box centers every `label_local_grid_spacing` mm,
# phased about the node center, out to `label_local_reach` times the reach.
# Beyond the reach the rays offer only a few sparse levels, so a pocket
# between two of them is found by no ray, and the panel grid is a repair
# device for a label left on the ink. A 2 mm spacing found the same spots
# on the scenes measured and cost a fifth more time and allocation on the
# very big spline scene, which brought it within a few percent of that
# scene's performance pins; 3 mm keeps the same pictures with room to spare.
label_local_grid_spacing <- 3
label_local_reach <- 3

# Side in mm of the square cells the sampled edge points are bucketed into,
# so a candidate box is compared only with the points in the cells around it.
# The cell size only sets how large that superset of points is, never which
# of them pass the distance tests, so it is free to tune: 5 mm measured best
# on small scenes, where cells smaller than that cost more bookkeeping over
# the cell rectangle than they save in distances.
label_ink_cell <- 5

# Anchors in preference order, with the sign of the offset each one takes
# along x and y.
label_anchor_names <- c("ne", "nw", "se", "sw", "n", "s", "e", "w")
label_anchor_sign_x <- c(1, -1, 1, -1, 0, 0, 1, -1)
label_anchor_sign_y <- c(1, 1, -1, -1, 1, -1, 0, 0)

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
#' A finite `reach` adds off-grid candidates that fill the space between the
#' disc and the reach: on `n_rays` evenly spaced rays around the node, at
#' `label_reach_steps` clearances from `gap` up to `reach` (and up to
#' `leader` when that is finite), a box is placed both with its facing
#' corner on the ray and with its center on the ray, and kept when its
#' clearance from the disc is at least `gap`. A few sparser levels beyond the
#' reach, at `label_far_reach` times `reach`, give a label somewhere to go
#' when nothing nearer is admissible. Off-grid candidates are named after
#' the anchor whose 45 degree sector their ray falls in, and only the sectors
#' of the anchors `n_angles` keeps are used; they share the preference rank
#' range just past the last anchor, so the `prefer` term still favors an
#' anchor over an off-grid spot of equal score.
#'
#' A label none of whose admissible candidates lies within `reach` (or that
#' has none at all) gains, before any label is assigned, a grid of
#' candidates every `label_local_grid_spacing` mm around its own node,
#' phased about the node center and kept where the box clears the disc by at
#' least `gap` and by at most `label_local_reach` times `reach`, so it can
#' settle in a pocket the sparse levels beyond the reach step over. Grid
#' candidates are named after the sector their center lies in and share the
#' rank range just past the last anchor. Like the off-grid candidates, this
#' grid is built only when `reach` is finite.
#'
#' A candidate whose box spills the panel additionally spawns a slid variant
#' translated by the minimal offset that brings it fully inside `bounds`
#' (per axis, and only when the box fits along that axis), at preference
#' rank + 0.5 and with a `*` suffix on its anchor, so a barely spilling spot
#' can slide inside instead of losing to a farther anchor.
#'
#' Scoring has two tiers. A candidate violates a hard constraint when its box
#' penetrates a node disc's clearance margin, comes within the edge margin of
#' a sampled edge point, comes within the arrow margin of a point in an
#' arrowhead zone (the last `label_arrow_zone` mm of each `edge_id`),
#' overlaps another placed label, or lies partly outside `bounds`. Every
#' violating candidate scores above every admissible one, whatever the
#' weights; among violating candidates the weighted sum of node penetration
#' depth, edge point count, arrowhead point count, overlap area, and outside
#' area decides, so that when nothing is admissible the least-bad candidate
#' is still returned and there is always one row per label. Among admissible
#' candidates the proximity bands come first: a candidate whose clearance
#' from the disc exceeds `leader` needs a leader line and loses to any
#' admissible candidate that does not, and one whose clearance exceeds
#' `reach` loses to any admissible candidate within reach. Within a band the
#' score is the distance in mm from the box center to the label's own node
#' center (the `dist` proximity pull), plus for a candidate past `leader`
#' the length of its leader and the extra length it is priced at for the
#' ink and discs it crosses (so a longer leader over open space beats a
#' shorter one over ink), the total penetration depth in mm into the soft
#' zone extending `label_soft_margin` beyond every other node's disc (the
#' `soft` term), the ownership term, and the preference rank as a pure
#' tiebreak.
#'
#' The ownership term keeps a label from reading as another node's. With
#' `d_own` a candidate's clearance from its own disc and `d_other` its
#' clearance from the nearest other disc, the term is
#' `max(0, min(d_own, reach) - d_other)` at the `own` weight: a box nearer a
#' foreign disc than its own pays the difference, a foreign disc farther
#' away than `reach` claims nothing, and with `reach` infinite the term is
#' off. A candidate that pays it and would be drawn without a leader
#' (clearance no more than `leader`) is also raised one band, so a
#' leaderless label beside the wrong node loses to any owned spot, with a
#' leader if need be. An `own` weight of 0 switches both parts off.
#'
#' Labels are assigned in order of the lowest band any of their admissible
#' candidates falls in, so a label that can sit beside its node claims its
#' spot before a label that has to go far; ties go to the most constrained
#' label (fewest violation-free candidates), then to input order. Each label
#' takes its best candidate given the labels placed so far, and
#' local-improvement sweeps then re-evaluate every label with the others
#' fixed, moving it only when its score strictly improves, until a sweep
#' moves nothing (at most `label_max_sweeps`). A repair stage follows for
#' any label still on a violating candidate, when `reach` is finite: the
#' label first gains a grid of candidates every `label_grid_spacing` mm over
#' the whole panel and takes the best admissible one if any exists; failing
#' that, it tries its statically admissible spots that only one other
#' label's box blocks, best first and at most `label_max_ejections` of
#' them, and takes the first one whose blocker can itself move to an
#' admissible candidate. Each repair round ends with another set of sweeps,
#' and repair stops after a round that changes nothing or after
#' `label_max_repairs` rounds. A label no repair can clear keeps its
#' least-bad candidate.
#'
#' @param labels Data frame with columns `id` (unique character), `x`, `y`
#'   (node centers in mm), and `width`, `height` (label box extent in mm,
#'   positive and finite).
#' @param nodes Data frame with columns `x`, `y`, and `radius` (positive and
#'   finite), one row per drawn node disc.
#' @param edges Data frame with columns `edge_id`, `x`, and `y`: ordered
#'   sampled points along each drawn edge, at least two per `edge_id`. The
#'   points within `label_arrow_zone` mm of the last point of each `edge_id`
#'   are its arrowhead zone. Zero rows means no edges.
#' @param bounds Numeric of length 4, `c(xmin, ymin, xmax, ymax)`, the panel
#'   extent in mm; must be strictly ordered in each dimension.
#' @param gap Clearance in mm between the node disc edge and the label box
#'   edge at ring 1.
#' @param n_angles Number of anchors to use, taken from the front of the
#'   preference order above.
#' @param n_rings Number of rings of candidates per anchor.
#' @param weights Named numeric vector weighting the score terms `node`,
#'   `edge`, `arrow`, `label`, and `bounds`, plus the `prefer` tiebreak, the
#'   `dist` proximity pull, the `soft` clearance-zone term, and the `own`
#'   ownership term. `dist`, `soft`, and `own` default to 0 when absent, so
#'   a weights vector from before those terms existed still works.
#' @param reach Clearance in mm from the disc within which a label should
#'   sit when it can: off-grid candidates fill the space up to it, and an
#'   admissible candidate within it always beats one beyond it. `Inf`, the
#'   default, adds no off-grid candidates and no such preference.
#' @param leader Clearance in mm from the disc past which a label is drawn
#'   with a leader line: an admissible candidate within it always beats one
#'   beyond it. `Inf`, the default, adds no such preference.
#' @param n_rays Number of evenly spaced rays the off-grid candidates are
#'   placed on when `reach` is finite.
#' @return A data frame with one row per label in input order and columns
#'   `id`, `x`, `y` (box centers), `anchor` (a `*` suffix marks a slid
#'   variant), and `score`, the total the chosen candidate was chosen by.
#'   For an admissible candidate past `leader` the band multiplier in that
#'   total is formed from the candidates whose leaders were priced, which
#'   orders the label's candidates the same way but is not the multiplier
#'   pricing every leader would give.
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
    soft = 1,
    own = 1
  ),
  reach = Inf,
  leader = Inf,
  n_rays = 36L
) {
  validate_label_placement_inputs(
    labels,
    nodes,
    edges,
    bounds,
    gap,
    n_angles,
    n_rings,
    weights,
    reach,
    leader,
    n_rays
  )

  ink <- label_ink_points(edges)
  radius <- nearest_node_radius(labels$x, labels$y, nodes)
  n <- nrow(labels)

  # Per-label candidate state, kept as plain vectors so the assignment loops
  # avoid data frame subsetting: the candidates themselves, their static
  # scores, and whether each one violates a static hard constraint. The
  # proximity pull, the soft penalty, and the ownership penalty are kept
  # apart from `within_static` so a leader priced later folds into the
  # score with the same arithmetic as one priced up front.
  candidates <- vector("list", n)
  hard_static <- vector("list", n)
  violating_static <- vector("list", n)
  band_static <- vector("list", n)
  within_static <- vector("list", n)
  leader_static <- vector("list", n)
  leader_bbox_static <- vector("list", n)
  center_static <- vector("list", n)
  soft_static <- vector("list", n)
  own_static <- vector("list", n)
  unpriced_static <- vector("list", n)
  unpriced_counts <- integer(n)
  clean_counts <- integer(n)
  best_bands <- numeric(n)

  # Pricing a leader against the ink is the most expensive score term, and
  # the price of a candidate that violates a static hard constraint only
  # ever matters when no admissible candidate is left. Those leaders are
  # priced on demand, in `ensure_priced()`; `evaluate()` says when the
  # deferral is safe. It relies on every score term being non-negative, so a
  # negative weight prices everything up front.
  dist_weight <- if ("dist" %in% names(weights)) weights[["dist"]] else 0
  price_lazily <- isTRUE(all(weights >= 0))
  # A leader's own length and the ownership term are computed for every
  # candidate up front, so full pricing adds only the crossings, and no
  # leader can cross more than every ink point and every disc: this bounds
  # how far full pricing could raise any `within` score.
  price_bound <- dist_weight *
    (label_leader_ink_cost *
      length(ink$x) +
      label_leader_disc_cost * sum(nodes$radius))

  # The chosen candidate of each label and its box, once placed.
  chosen <- integer(n)
  box_xmin <- box_ymin <- box_xmax <- box_ymax <- rep(NA_real_, n)

  # The terms of each label's candidates against the other labels' placed
  # boxes, kept per placed box so that moving one box replaces one column
  # rather than rebuilding every pair: `overlap_cols[[i]][[j]]` is the
  # overlap area of each candidate of label `i` with the box of label `j`,
  # `crossing_cols[[i]][[j]]` whether each candidate's leader crosses that
  # box, and `crossing_counts[[i]]` the running total of the latter over the
  # placed boxes. A column exists only for a placed label, and never for the
  # label itself. Integer counts are exact under any order of addition, so
  # the running total equals a fresh count; the overlap areas are summed
  # afresh from the stored columns whenever they are read, in the same
  # order a full rebuild would sum them, so that sum is bit-identical too.
  #
  # Columns are refreshed when a label is scored, not when a box moves:
  # every move advances the version of its box, and a column whose version
  # is behind is recomputed before it is read. A box moved and moved back,
  # as a failed ejection does, therefore costs nothing for the labels not
  # scored in between, and a column recomputed for a restored box holds the
  # values it held before.
  overlap_cols <- vector("list", n)
  crossing_cols <- vector("list", n)
  crossing_counts <- vector("list", n)
  box_version <- integer(n)
  col_version <- vector("list", n)

  own_node <- function(i) {
    list(x = labels$x[i], y = labels$y[i], radius = radius[i])
  }

  # The bounding box of each leader segment, grown by the edge margin, as
  # `box_crossings()` tests it: computed once per candidate rather than once
  # per placed box.
  leader_bbox <- function(segment) {
    list(
      xmin = pmin(segment$x0, segment$x1) - label_edge_clearance,
      xmax = pmax(segment$x0, segment$x1) + label_edge_clearance,
      ymin = pmin(segment$y0, segment$y1) - label_edge_clearance,
      ymax = pmax(segment$y0, segment$y1) + label_edge_clearance
    )
  }

  # The overlap area of the candidates in `rows` of label `i` with the placed
  # box of label `j`, and whether the leader of each of those candidates
  # crosses that box.
  box_overlap <- function(i, rows, j) {
    cand <- candidates[[i]]
    rect_overlap_area(
      cand$xmin[rows],
      cand$ymin[rows],
      cand$xmax[rows],
      cand$ymax[rows],
      box_xmin[j],
      box_ymin[j],
      box_xmax[j],
      box_ymax[j]
    )
  }
  box_crossings <- function(i, rows, j) {
    segment <- leader_static[[i]]
    bbox <- leader_bbox_static[[i]]
    crossed <- integer(length(rows))
    idx <- which(!is.na(segment$x0[rows]))
    if (length(idx) == 0) {
      return(crossed)
    }
    ci <- rows[idx]
    # Only a leader whose bounding box, grown by the margin, reaches the box
    # can come within the margin of it, so the exact distance is computed
    # for those leaders alone.
    near <- bbox$xmin[ci] <= box_xmax[j] &
      bbox$xmax[ci] >= box_xmin[j] &
      bbox$ymin[ci] <= box_ymax[j] &
      bbox$ymax[ci] >= box_ymin[j]
    ci <- ci[near]
    idx <- idx[near]
    if (length(ci) == 0) {
      return(crossed)
    }
    m <- length(ci)
    dist <- rect_segment_dist(
      rep(box_xmin[j], m),
      rep(box_ymin[j], m),
      rep(box_xmax[j], m),
      rep(box_ymax[j], m),
      segment$x0[ci],
      segment$y0[ci],
      segment$x1[ci],
      segment$y1[ci]
    )
    crossed[idx[dist < label_edge_clearance]] <- 1L
    crossed
  }

  add_candidates <- function(i, cand) {
    scored <- score_label_candidates(
      cand,
      nodes,
      ink,
      bounds,
      weights,
      own = own_node(i),
      reach = reach,
      leader = leader,
      price_violating = !price_lazily
    )
    n_new <- length(cand$x)
    if (is.null(candidates[[i]])) {
      candidates[[i]] <<- cand
      hard_static[[i]] <<- scored$hard
      violating_static[[i]] <<- scored$violating
      band_static[[i]] <<- scored$band
      within_static[[i]] <<- scored$within
      leader_static[[i]] <<- scored$leader
      leader_bbox_static[[i]] <<- leader_bbox(scored$leader)
      center_static[[i]] <<- scored$center_dist
      soft_static[[i]] <<- scored$soft_penalty
      own_static[[i]] <<- scored$own_penalty
      unpriced_static[[i]] <<- scored$unpriced
      overlap_cols[[i]] <<- vector("list", n)
      crossing_cols[[i]] <<- vector("list", n)
      crossing_counts[[i]] <<- integer(n_new)
      col_version[[i]] <<- integer(n)
      new_rows <- seq_len(n_new)
    } else {
      new_rows <- length(candidates[[i]]$x) + seq_len(n_new)
      candidates[[i]] <<- Map(c, candidates[[i]], cand)
      hard_static[[i]] <<- c(hard_static[[i]], scored$hard)
      violating_static[[i]] <<- c(violating_static[[i]], scored$violating)
      band_static[[i]] <<- c(band_static[[i]], scored$band)
      within_static[[i]] <<- c(within_static[[i]], scored$within)
      leader_static[[i]] <<- Map(c, leader_static[[i]], scored$leader)
      leader_bbox_static[[i]] <<- Map(
        c,
        leader_bbox_static[[i]],
        leader_bbox(scored$leader)
      )
      center_static[[i]] <<- c(center_static[[i]], scored$center_dist)
      soft_static[[i]] <<- c(soft_static[[i]], scored$soft_penalty)
      own_static[[i]] <<- c(own_static[[i]], scored$own_penalty)
      unpriced_static[[i]] <<- c(unpriced_static[[i]], scored$unpriced)
      crossing_counts[[i]] <<- c(crossing_counts[[i]], integer(n_new))
    }
    unpriced_counts[i] <<- sum(unpriced_static[[i]])
    clean_counts[i] <<- sum(!violating_static[[i]])
    best_bands[i] <<- if (clean_counts[i] > 0) {
      min(band_static[[i]][!violating_static[[i]]])
    } else {
      Inf
    }
    # The new candidates are appended to every column that is up to date;
    # a column behind its box is dropped, to be recomputed in full when the
    # label is next scored.
    for (j in which(col_version[[i]] > 0L)) {
      if (col_version[[i]][j] == box_version[j]) {
        overlap_cols[[i]][[j]] <<- c(
          overlap_cols[[i]][[j]],
          box_overlap(i, new_rows, j)
        )
        crossed <- box_crossings(i, new_rows, j)
        crossing_cols[[i]][[j]] <<- c(crossing_cols[[i]][[j]], crossed)
        crossing_counts[[i]][new_rows] <<- crossing_counts[[i]][new_rows] +
          crossed
      } else {
        crossing_counts[[i]] <<- crossing_counts[[i]] -
          c(crossing_cols[[i]][[j]], integer(n_new))
        overlap_cols[[i]][j] <<- list(NULL)
        crossing_cols[[i]][j] <<- list(NULL)
        col_version[[i]][j] <<- 0L
      }
    }
  }

  # Price the leaders of label `i` that were deferred, with exactly the
  # arithmetic up-front pricing uses: the extra length is computed per
  # candidate, and `within_score()` combines it per candidate, so the
  # result does not depend on which other candidates were priced alongside.
  # Any cached evaluation of the label is stale afterwards.
  ensure_priced <- function(i) {
    if (unpriced_counts[i] == 0L) {
      return(invisible())
    }
    cand <- candidates[[i]]
    idx <- which(unpriced_static[[i]])
    extra <- leader_crossing_length(
      cand,
      unpriced_static[[i]],
      own_node(i),
      nodes,
      ink
    )$extra
    within_static[[i]][idx] <<- within_score(
      center_static[[i]][idx],
      extra[idx],
      soft_static[[i]][idx],
      own_static[[i]][idx],
      cand$rank[idx],
      weights
    )
    unpriced_static[[i]][idx] <<- FALSE
    unpriced_counts[i] <<- 0L
    evaluate_cache[[i]] <<- new.env(parent = emptyenv())
    invisible()
  }

  for (i in seq_len(n)) {
    add_candidates(
      i,
      label_candidates(
        labels$x[i],
        labels$y[i],
        radius[i],
        gap,
        labels$width[i],
        labels$height[i],
        n_angles,
        n_rings,
        bounds,
        reach = reach,
        leader = leader,
        n_rays = n_rays
      )
    )
  }

  # A label with no admissible candidate within reach has only the sparse
  # far levels of its rays to go to, which step straight over a pocket
  # between two rays, so it gains a grid of candidates around its own node
  # before anything is assigned. Like the ray candidates, the grid fills the
  # space around the anchors, so an infinite `reach` builds none.
  if (is.finite(reach)) {
    for (i in which(best_bands >= 2)) {
      local <- label_local_grid_candidates(
        labels$x[i],
        labels$y[i],
        radius[i],
        gap,
        labels$width[i],
        labels$height[i],
        bounds,
        reach,
        rank_from = n_angles * n_rings + 1
      )
      if (length(local$x) > 0) {
        add_candidates(i, local)
      }
    }
  }

  # Labels that can sit beside their node claim their spots before labels
  # that have to go far, so a far label never takes the spot beside another
  # label's node; within a band the most constrained labels go first, and
  # ties fall back to input order, keeping the order deterministic.
  placement_order <- order(best_bands, clean_counts, seq_len(n))

  # Record the box label `j` now sits on.
  record_box <- function(j) {
    cand <- candidates[[j]]
    box_xmin[j] <<- cand$xmin[chosen[j]]
    box_ymin[j] <<- cand$ymin[chosen[j]]
    box_xmax[j] <<- cand$xmax[chosen[j]]
    box_ymax[j] <<- cand$ymax[chosen[j]]
    box_version[j] <<- box_version[j] + 1L
  }

  # Recompute the column of label `j`'s box in label `i`'s placed-box terms.
  set_column <- function(i, j) {
    rows <- seq_along(candidates[[i]]$x)
    overlap_cols[[i]][[j]] <<- box_overlap(i, rows, j)
    crossed <- box_crossings(i, rows, j)
    previous <- crossing_cols[[i]][[j]]
    if (!is.null(previous)) {
      crossing_counts[[i]] <<- crossing_counts[[i]] - previous
    }
    crossing_counts[[i]] <<- crossing_counts[[i]] + crossed
    crossing_cols[[i]][[j]] <<- crossed
    col_version[[i]][j] <<- box_version[j]
  }
  # Bring every column of label `i` up to date with the placed boxes.
  refresh_columns <- function(i) {
    for (j in which(chosen > 0L)) {
      if (j != i && col_version[[i]][j] != box_version[j]) {
        set_column(i, j)
      }
    }
  }

  # The overlap area of each candidate of label `i` with the placed boxes of
  # the other labels. The stored columns are in label order, so this sums
  # them in the order a full rebuild would.
  placed_overlap <- function(i) {
    n_cand <- length(candidates[[i]]$x)
    cols <- overlap_cols[[i]]
    cols <- cols[lengths(cols) > 0]
    if (length(cols) == 0) {
      return(numeric(n_cand))
    }
    rowSums(matrix(unlist(cols, use.names = FALSE), nrow = n_cand))
  }

  # The number of other labels' placed boxes the leader of each candidate of
  # label `i` would cross, priced like the ink and discs it crosses.
  leader_box_crossings <- function(i) {
    crossing_counts[[i]]
  }

  # Every candidate of label `i` scored against the current placement of the
  # others: the total, and whether it violates a hard constraint.
  score_against_placed <- function(i) {
    refresh_columns(i)
    overlap <- placed_overlap(i)
    within <- within_static[[i]] +
      dist_weight * label_leader_box_cost * leader_box_crossings(i)
    soft <- band_static[[i]] * (max(within) + 1) + within
    violating <- violating_static[[i]] | overlap > 0
    hard <- hard_static[[i]] + weights[["label"]] * overlap
    list(
      total = tiered_score(soft, hard, violating),
      violating = violating,
      within = within
    )
  }

  # Whether deferred pricing could change which candidate of label `i` wins
  # when `best`, an admissible candidate, has the least total under it.
  #
  # Admissible candidates are fully priced, so their `within` scores are the
  # ones full pricing gives; only the band multiplier `M = max(within) + 1`
  # differs, and it is at most `price_bound` larger under full pricing. A
  # candidate of a higher band than `best` adds at least one more `M` to a
  # non-negative `within`, and `M` exceeds every `within` by at least 1, so
  # it stays behind `best` under either multiplier, exactly and after
  # rounding. A candidate of the same band differs from `best` only in
  # `within`, and rounding `band * M + within` can only reorder two such
  # candidates, or tie them, when their `within` scores differ by less than
  # the rounding at that magnitude. Under any multiplier the totals are
  # below `3 * m_upper`, so a difference of more than a few units in the
  # last place at that magnitude survives rounding under both.
  deferred_pricing_unsafe <- function(i, best, scored) {
    band <- band_static[[i]]
    if (band[best] == 0) {
      return(FALSE)
    }
    within <- scored$within
    m_upper <- max(within) + price_bound + 1
    tolerance <- 4 * .Machine$double.eps * 3 * m_upper
    same_band <- !scored$violating & band == band[best]
    delta <- abs(within[same_band] - within[best])
    any(delta > 0 & delta <= tolerance)
  }

  # `score_against_placed()` with two economies that return exactly what it
  # would.
  #
  # The first is deferred pricing. While some leaders of label `i` are not
  # priced, its `within` scores are too low on those candidates, and every
  # one of them violates a static constraint. The band multiplier
  # `max(within) + 1` and the lift `max(soft) + 1` are then not the ones
  # full pricing would give, so the totals are trusted only when the winner
  # is admissible, where the identity of the winner does not depend on
  # either. Every score term is non-negative (this is what `price_lazily`
  # checks), so a violating candidate, which adds the lift, exceeding every
  # `soft` by at least 1, plus a non-negative `hard`, scores above every
  # admissible candidate under any lift formed this way, in exact arithmetic
  # and after rounding (the rounding of a sum of non-negative terms never
  # falls below either term, and a 1 is far beyond the rounding of any
  # score). Among the admissible candidates, all fully priced, the winner is
  # decided by band and then by `within`, and `deferred_pricing_unsafe()`
  # rules out the one way the multiplier could still matter. So `which.min`
  # (and the strict comparison in `sweep()`) picks the same candidate full
  # pricing would. When the winner is violating, when the multiplier could
  # matter, or when the totals of the violating candidates are read (in
  # `eject()` and for the returned score of a violating label), the label
  # is priced in full first and scored again.
  #
  # The second is caching. The refinement asks for the same label in the
  # same surroundings again and again: a sweep pass that moves nothing
  # repeats the pass before it, the stuck check follows such a pass, and
  # the final score repeats the last evaluation. The result depends on the
  # label, on its own candidate set, and on where every other label
  # currently sits, and on nothing else: the static tables are indexed by
  # label and candidate, and candidate sets only ever grow by appending, so
  # their length identifies them. Caching per label on that pair therefore
  # returns exactly what a recomputation would, and pricing a label in full
  # discards its cache. `chosen` also carries the ejection state, so
  # backtracking an ejection restores the key along with the placement it
  # names.
  evaluate_cache <- lapply(seq_len(n), function(i) {
    new.env(parent = emptyenv())
  })
  evaluate <- function(i) {
    key <- paste0(c(length(candidates[[i]]$x), chosen[-i]), collapse = ",")
    cache <- evaluate_cache[[i]]
    cached <- cache[[key]]
    if (!is.null(cached)) {
      return(cached)
    }
    scored <- score_against_placed(i)
    if (unpriced_counts[i] > 0L) {
      best <- which.min(scored$total)
      if (scored$violating[best] || deferred_pricing_unsafe(i, best, scored)) {
        ensure_priced(i)
        cache <- evaluate_cache[[i]]
        scored <- score_against_placed(i)
      }
    }
    cache[[key]] <- scored
    scored
  }

  # Whether the candidate label `i` currently sits on violates a hard
  # constraint. This is `evaluate(i)$violating[chosen[i]]` for that one
  # candidate: its static violations, or any overlap with a placed box. The
  # overlap areas are never negative, so their sum is positive exactly when
  # one of them is.
  chosen_violates <- function(i) {
    if (violating_static[[i]][chosen[i]]) {
      return(TRUE)
    }
    placed <- which(chosen > 0L)
    placed <- placed[placed != i]
    if (length(placed) == 0) {
      return(FALSE)
    }
    any(
      rect_overlap_area(
        box_xmin[i],
        box_ymin[i],
        box_xmax[i],
        box_ymax[i],
        box_xmin[placed],
        box_ymin[placed],
        box_xmax[placed],
        box_ymax[placed]
      ) >
        0
    )
  }

  for (i in placement_order) {
    chosen[i] <- which.min(evaluate(i)$total)
    record_box(i)
  }

  # Local-improvement sweeps: re-evaluate each label with the others fixed
  # and move only on strict improvement, so the refinement is deterministic
  # and cannot oscillate. A sweep that moves nothing ends the refinement,
  # leaving every label at its best candidate given the others.
  sweep <- function() {
    for (pass in seq_len(label_max_sweeps)) {
      moved <- FALSE
      for (i in placement_order) {
        totals <- evaluate(i)$total
        best <- which.min(totals)
        if (totals[best] < totals[chosen[i]]) {
          chosen[i] <<- best
          record_box(i)
          moved <- TRUE
        }
      }
      if (!moved) {
        break
      }
    }
  }
  sweep()

  # Repair: a label left on a violating candidate first gains a grid of
  # candidates over the whole panel, in case its free spot is somewhere no
  # ray reaches, and failing that tries to take an admissible spot one other
  # label is sitting on, when that label can move to an admissible spot of
  # its own. Like the ray candidates, the grid is part of filling the space
  # around the anchors, so an infinite `reach` builds none.
  grid_added <- !is.finite(reach) | logical(n)
  add_grid <- function(i) {
    if (grid_added[i]) {
      return(FALSE)
    }
    grid_added[i] <<- TRUE
    grid <- label_grid_candidates(
      labels$x[i],
      labels$y[i],
      labels$width[i],
      labels$height[i],
      bounds,
      rank_from = n_angles * n_rings + 1
    )
    if (length(grid$x) == 0) {
      return(FALSE)
    }
    add_candidates(i, grid)
    TRUE
  }
  settle <- function(i) {
    scored <- evaluate(i)
    best <- which.min(scored$total)
    if (scored$violating[best]) {
      return(FALSE)
    }
    chosen[i] <<- best
    record_box(i)
    TRUE
  }
  eject <- function(i) {
    cand <- candidates[[i]]
    # The blocked spots are ordered by their totals, which are exact only
    # under full pricing.
    ensure_priced(i)
    scored <- evaluate(i)
    # the admissible spots of `i` that only other boxes block, best first
    blocked <- which(!violating_static[[i]] & scored$violating)
    blocked <- blocked[order(scored$total[blocked])]
    blocked <- blocked[seq_len(min(length(blocked), label_max_ejections))]
    placed <- which(chosen > 0L)
    placed <- placed[placed != i]
    for (k in blocked) {
      overlap <- rect_overlap_area(
        cand$xmin[k],
        cand$ymin[k],
        cand$xmax[k],
        cand$ymax[k],
        box_xmin[placed],
        box_ymin[placed],
        box_xmax[placed],
        box_ymax[placed]
      )
      blockers <- placed[overlap > 0]
      if (length(blockers) != 1) {
        next
      }
      j <- blockers
      previous <- chosen[i]
      chosen[i] <<- k
      record_box(i)
      add_grid(j)
      before <- chosen[j]
      if (settle(j)) {
        return(TRUE)
      }
      chosen[j] <<- before
      chosen[i] <<- previous
      record_box(i)
    }
    FALSE
  }
  for (round in seq_len(label_max_repairs)) {
    stuck <- placement_order[vapply(
      placement_order,
      chosen_violates,
      logical(1)
    )]
    if (length(stuck) == 0) {
      break
    }
    changed <- FALSE
    for (i in stuck) {
      if (add_grid(i) && settle(i)) {
        changed <- TRUE
        next
      }
      if (eject(i)) {
        changed <- TRUE
      }
    }
    if (!changed) {
      break
    }
    sweep()
  }

  pick <- function(column) {
    vapply(
      seq_len(n),
      function(i) candidates[[i]][[column]][chosen[i]],
      numeric(1)
    )
  }
  # The returned score of a violating label is its total under full pricing,
  # which the deferred totals do not give (see `evaluate()`). An admissible
  # label keeps the total it was chosen by.
  score <- vapply(
    seq_len(n),
    function(i) {
      if (unpriced_counts[i] > 0L && chosen_violates(i)) {
        ensure_priced(i)
      }
      evaluate(i)$total[chosen[i]]
    },
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

#' Combine the two score tiers of one label's candidates
#'
#' Every violating candidate is lifted past the largest admissible score, so
#' an admissible candidate always wins when one exists, and the weighted
#' violations order the violating candidates among themselves. A violation
#' counts whatever its weight, so a zero weight reorders the violating
#' candidates without admitting one.
#'
#' @param soft Numeric, the admissible-tier score per candidate.
#' @param hard Numeric, the weighted hard violations per candidate.
#' @param violating Logical, whether each candidate violates a hard
#'   constraint.
#' @return Numeric total per candidate.
#' @noRd
tiered_score <- function(soft, hard, violating) {
  lift <- max(soft) + 1
  soft + ifelse(violating, lift + hard, 0)
}

#' Candidate boxes on a grid over the whole panel
#'
#' Box centers every `label_grid_spacing` mm across the part of `bounds` a
#' box of this size fits in, named after the anchor whose sector the center
#' lies in as seen from the node. These are the fallback for a label whose
#' rays all end on obstacles or other labels.
#'
#' @param x,y Node center in mm.
#' @param width,height Label box extent in mm.
#' @param bounds Panel extent `c(xmin, ymin, xmax, ymax)` in mm.
#' @param rank_from Preference rank of the first grid candidate; the grid
#'   shares the unit range starting there.
#' @return A candidate list as `label_candidates()` returns, possibly empty.
#' @noRd
label_grid_candidates <- function(x, y, width, height, bounds, rank_from) {
  if (
    width > bounds[[3]] - bounds[[1]] ||
      height > bounds[[4]] - bounds[[2]]
  ) {
    return(empty_label_candidates())
  }
  center_x <- seq(
    bounds[[1]] + width / 2,
    bounds[[3]] - width / 2,
    by = label_grid_spacing
  )
  center_y <- seq(
    bounds[[2]] + height / 2,
    bounds[[4]] - height / 2,
    by = label_grid_spacing
  )
  grid <- expand.grid(x = center_x, y = center_y)
  grid_label_candidates(grid$x, grid$y, x, y, width, height, rank_from)
}

#' Candidate boxes on a grid around one node
#'
#' Box centers every `label_local_grid_spacing` mm on a grid phased about
#' the node center, so the candidates sit at whole multiples of the spacing
#' from the node on each axis whatever the panel's origin, kept where the
#' box lies inside `bounds` and clears the node disc by at least `gap` and
#' at most `label_local_reach` times `reach`. These are built for a label
#' with no admissible candidate within reach, whose rays offer only a few
#' sparse levels beyond it, so that a pocket between two rays is found.
#'
#' @param x,y Node center in mm.
#' @param radius Node disc radius in mm.
#' @param gap Minimum clearance between the disc edge and the box edge.
#' @param width,height Label box extent in mm.
#' @param bounds Panel extent `c(xmin, ymin, xmax, ymax)` in mm.
#' @param reach As in `place_dag_labels()`, finite.
#' @param rank_from Preference rank of the first grid candidate; the grid
#'   shares the unit range starting there.
#' @return A candidate list as `label_candidates()` returns, possibly empty.
#' @noRd
label_local_grid_candidates <- function(
  x,
  y,
  radius,
  gap,
  width,
  height,
  bounds,
  reach,
  rank_from
) {
  far <- label_local_reach * reach
  # The farthest a box center can sit while its nearest point is within the
  # far clearance: along a diagonal the nearest point is a corner, half the
  # box diagonal from the center.
  extent <- radius + far + sqrt(width^2 + height^2) / 2
  steps <- ceiling(extent / label_local_grid_spacing)
  offsets <- seq(-steps, steps) * label_local_grid_spacing
  center_x <- x + offsets
  center_y <- y + offsets
  center_x <- center_x[
    center_x - width / 2 >= bounds[[1]] & center_x + width / 2 <= bounds[[3]]
  ]
  center_y <- center_y[
    center_y - height / 2 >= bounds[[2]] &
      center_y + height / 2 <= bounds[[4]]
  ]
  if (length(center_x) == 0 || length(center_y) == 0) {
    return(empty_label_candidates())
  }
  grid <- expand.grid(x = center_x, y = center_y)
  clearance <- rect_point_dist(
    grid$x - width / 2,
    grid$y - height / 2,
    grid$x + width / 2,
    grid$y + height / 2,
    x,
    y
  ) -
    radius
  keep <- clearance >= gap - 1e-9 & clearance <= far
  if (!any(keep)) {
    return(empty_label_candidates())
  }
  grid_label_candidates(
    grid$x[keep],
    grid$y[keep],
    x,
    y,
    width,
    height,
    rank_from
  )
}

#' Name and rank grid candidates
#'
#' Names each box center after the anchor whose 45 degree sector it lies in
#' as seen from the node, and spreads the ranks over the unit range from
#' `rank_from` in the order given.
#'
#' @param center_x,center_y Box centers in mm.
#' @param x,y Node center in mm.
#' @param width,height Label box extent in mm.
#' @param rank_from Preference rank of the first candidate.
#' @return A candidate list as `label_candidates()` returns.
#' @noRd
grid_label_candidates <- function(
  center_x,
  center_y,
  x,
  y,
  width,
  height,
  rank_from
) {
  n <- length(center_x)
  compass <- c("e", "ne", "n", "nw", "w", "sw", "s", "se")
  angle <- atan2(center_y - y, center_x - x)
  list(
    anchor = compass[round(angle / (pi / 4)) %% 8 + 1],
    rank = rank_from + (seq_len(n) - 1) / n,
    x = center_x,
    y = center_y,
    xmin = center_x - width / 2,
    ymin = center_y - height / 2,
    xmax = center_x + width / 2,
    ymax = center_y + height / 2
  )
}

#' An empty candidate list
#'
#' @return A candidate list as `label_candidates()` returns, with no rows.
#' @noRd
empty_label_candidates <- function() {
  list(
    anchor = character(),
    rank = numeric(),
    x = numeric(),
    y = numeric(),
    xmin = numeric(),
    ymin = numeric(),
    xmax = numeric(),
    ymax = numeric()
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
  reach = Inf,
  leader = Inf,
  n_rays = 36L,
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

  whole_positive <- function(value) {
    length(value) == 1 &&
      is.numeric(value) &&
      is.finite(value) &&
      value == as.integer(value) &&
      value >= 1
  }
  if (!whole_positive(n_rings)) {
    abort(
      "{.arg n_rings} must be a whole number of at least 1.",
      error_class = "ggdag_type_error",
      call = call
    )
  }
  if (!whole_positive(n_rays)) {
    abort(
      "{.arg n_rays} must be a whole number of at least 1.",
      error_class = "ggdag_type_error",
      call = call
    )
  }

  # A threshold is a positive distance in mm, or Inf to switch the
  # preference off.
  positive_or_inf <- function(value) {
    length(value) == 1 &&
      is.numeric(value) &&
      !is.na(value) &&
      value > 0
  }
  if (!positive_or_inf(reach)) {
    abort(
      "{.arg reach} must be a single positive number or {.code Inf}.",
      error_class = "ggdag_type_error",
      call = call
    )
  }
  if (!positive_or_inf(leader)) {
    abort(
      "{.arg leader} must be a single positive number or {.code Inf}.",
      error_class = "ggdag_type_error",
      call = call
    )
  }

  required_weights <- c("node", "edge", "arrow", "label", "bounds", "prefer")
  optional_weights <- c("dist", "soft", "own")
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
  present_optional <- optional_weights[optional_weights %in% names(weights)]
  checked <- c(required_weights, present_optional)
  bad_weights <- checked[!is.finite(weights[checked])]
  if (length(bad_weights) > 0) {
    abort(
      c(
        "{.arg weights} must contain finite values named
         {.val {required_weights}}, and finite {.val {optional_weights}}
         when present.",
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
#' Builds the candidate set for a single label: the ring-major anchor grid
#' (the first `n_angles` anchors of the preference order at each of
#' `n_rings` rings, with 0-based preference `rank` in evaluation order),
#' then, when `reach` is finite, the off-grid candidates of
#' `label_ray_candidates()`, which share the rank range just past the last
#' anchor. Candidates whose boxes coincide are kept once, at their first
#' rank. When `bounds` is given, every candidate whose box spills the panel
#' also gains a slid variant translated by the minimal offset that brings it
#' inside, at rank + 0.5 and with `*` appended to its anchor; an axis the box
#' cannot fit along is left untranslated.
#'
#' @param x,y Node center in mm.
#' @param radius Node disc radius in mm.
#' @param gap Ring 1 clearance between the disc edge and the box edge in mm.
#' @param width,height Label box extent in mm.
#' @param n_angles,n_rings Candidate grid size.
#' @param bounds Panel extent `c(xmin, ymin, xmax, ymax)` in mm, or `NULL`
#'   to build no slid variants.
#' @param reach,leader,n_rays Off-grid candidate settings, as in
#'   `place_dag_labels()`.
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
  bounds = NULL,
  reach = Inf,
  leader = Inf,
  n_rays = 36L
) {
  anchors <- label_anchor_names[seq_len(n_angles)]
  sign_x <- label_anchor_sign_x[seq_len(n_angles)]
  sign_y <- label_anchor_sign_y[seq_len(n_angles)]
  diagonal <- sign_x != 0 & sign_y != 0

  ring <- rep(seq_len(n_rings), each = n_angles)
  anchor_index <- rep(seq_len(n_angles), times = n_rings)

  # Distance from the node center to the box's nearest point: radius + gap
  # at ring 1, stepping out by half the box diagonal per further ring. Along
  # a diagonal ray the nearest point is the facing corner, so each
  # coordinate offset is that distance over sqrt(2); along a cardinal ray it
  # is the facing side midpoint, at the full distance.
  distance <- radius + gap + (ring - 1) * sqrt(width^2 + height^2) / 2
  reach_along <- ifelse(diagonal[anchor_index], distance / sqrt(2), distance)

  center_x <- x + sign_x[anchor_index] * (reach_along + width / 2)
  center_y <- y + sign_y[anchor_index] * (reach_along + height / 2)

  cand <- list(
    anchor = anchors[anchor_index],
    rank = seq_along(center_x) - 1,
    x = center_x,
    y = center_y
  )

  if (is.finite(reach)) {
    off <- label_ray_candidates(
      x,
      y,
      radius,
      gap,
      width,
      height,
      anchors,
      reach,
      leader,
      n_rays
    )
    n_off <- length(off$x)
    if (n_off > 0) {
      cand <- list(
        anchor = c(cand$anchor, off$anchor),
        rank = c(cand$rank, length(cand$rank) + (seq_len(n_off) - 1) / n_off),
        x = c(cand$x, off$x),
        y = c(cand$y, off$y)
      )
    }
  }

  # A box reached by two constructions is one candidate, kept at its first
  # (most preferred) rank. Matching each rounded coordinate against itself
  # replaces it with the index of the first candidate holding that value, so
  # two candidates share a rounded position exactly when they share both
  # indices; those indices are in `1:n_cand`, so combining them base `n_cand`
  # gives a whole number below `n_cand^2` that is exact in double precision
  # for any candidate set this engine builds, and one number per candidate
  # identifies the position with no strings to allocate.
  round_x <- round(cand$x, 6)
  round_y <- round(cand$y, 6)
  n_cand <- length(round_x)
  keep <- !duplicated(
    (match(round_x, round_x) - 1) * n_cand + match(round_y, round_y)
  )
  cand <- lapply(cand, `[`, keep)

  cand$xmin <- cand$x - width / 2
  cand$ymin <- cand$y - height / 2
  cand$xmax <- cand$x + width / 2
  cand$ymax <- cand$y + height / 2
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

  # A slid box stops a hair inside the border rather than exactly on it, so
  # that it still reads as inside once its limits have been rebuilt from its
  # centre and width in floating point.
  dx <- dx + sign(dx) * label_slide_nudge
  dy <- dy + sign(dy) * label_slide_nudge

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

#' Off-grid candidate boxes within reach of one node
#'
#' On `n_rays` evenly spaced rays from the node center, at
#' `label_reach_steps` clearances from `gap` to each finite threshold
#' (`reach`, and `leader` when finite) and at the sparser far levels
#' `label_far_reach * reach`, a box is placed two ways: with the corner
#' facing the node on the ray, so its nearest point sits exactly at that
#' clearance, and with its center offset along the ray, so it straddles the
#' ray as a label beside a node often does. A placement whose box comes
#' nearer the disc than `gap` is dropped. Only rays in the 45 degree sectors
#' of `anchors` are used, and each candidate is named after its sector's
#' anchor. Candidates are ordered by clearance level, then by the preference
#' rank of their sector, then by angle, corner placement first.
#'
#' @param x,y Node center in mm.
#' @param radius Node disc radius in mm.
#' @param gap Minimum clearance between the disc edge and the box edge.
#' @param width,height Label box extent in mm.
#' @param anchors Character, the anchors whose sectors are used.
#' @param reach,leader,n_rays As in `place_dag_labels()`; `reach` is finite.
#' @return A list of parallel vectors `anchor`, `x`, `y` (box centers).
#' @noRd
label_ray_candidates <- function(
  x,
  y,
  radius,
  gap,
  width,
  height,
  anchors,
  reach,
  leader,
  n_rays
) {
  empty <- list(anchor = character(), x = numeric(), y = numeric())

  thresholds <- unique(c(reach, leader[is.finite(leader)]))
  levels <- unlist(lapply(thresholds, function(threshold) {
    seq(gap, threshold, length.out = label_reach_steps)
  }))
  levels <- sort(unique(c(levels, label_far_reach * reach)))
  levels <- levels[levels >= gap]
  if (length(levels) == 0) {
    return(empty)
  }

  # Each ray belongs to the anchor whose direction it is nearest, counting
  # sectors counterclockwise from east.
  angles <- seq(0, 2 * pi, length.out = n_rays + 1)[-(n_rays + 1)]
  compass <- c("e", "ne", "n", "nw", "w", "sw", "s", "se")
  sector <- compass[round(angles / (pi / 4)) %% 8 + 1]
  keep <- sector %in% anchors
  if (!any(keep)) {
    return(empty)
  }
  angles <- angles[keep]
  sector <- sector[keep]
  ray_order <- order(match(sector, anchors), angles)
  angles <- angles[ray_order]
  sector <- sector[ray_order]

  ux <- cos(angles)
  uy <- sin(angles)
  sign_x <- sign(round(ux, 8))
  sign_y <- sign(round(uy, 8))

  n_levels <- length(levels)
  n_rays_kept <- length(angles)
  level_index <- rep(seq_len(n_levels), each = 2 * n_rays_kept)
  ray_index <- rep(rep(seq_len(n_rays_kept), each = 2), times = n_levels)
  corner <- rep(c(TRUE, FALSE), times = n_levels * n_rays_kept)

  distance <- radius + levels[level_index]
  ray_x <- x + ux[ray_index] * distance
  ray_y <- y + uy[ray_index] * distance
  center_x <- ray_x +
    ifelse(corner, sign_x[ray_index], ux[ray_index]) * width / 2
  center_y <- ray_y +
    ifelse(corner, sign_y[ray_index], uy[ray_index]) * height / 2

  clearance <- rect_point_dist(
    center_x - width / 2,
    center_y - height / 2,
    center_x + width / 2,
    center_y + height / 2,
    x,
    y
  ) -
    radius
  keep <- clearance >= gap - 1e-9

  list(
    anchor = sector[ray_index][keep],
    x = center_x[keep],
    y = center_y[keep]
  )
}

#' Sampled edge points prepared for box tests
#'
#' Flags the points of each `edge_id` within `label_arrow_zone` mm of its
#' last point as arrowhead-zone points and buckets the points into square
#' cells of `label_ink_cell` mm, sorted by cell, so that `ink_cell_pairs()`
#' can pick out the points near a set of boxes without comparing every box
#' with every point.
#'
#' @param edges Data frame with columns `edge_id`, `x`, and `y`.
#' @return A list with `x`, `y`, `head` (logical), and `id` (the integer code
#'   of the point's `edge_id`) in input order, plus the cell index: `order`
#'   (the permutation sorting the points by cell), `sorted_cell` (their cell
#'   ids in that order), and the grid's origin `col0`, `row0` and height in
#'   cells `n_rows`.
#' @noRd
label_ink_points <- function(edges) {
  if (nrow(edges) == 0) {
    return(list(
      x = numeric(),
      y = numeric(),
      head = logical(),
      id = integer(),
      order = integer(),
      sorted_cell = numeric(),
      col0 = 0,
      row0 = 0,
      n_cols = 0,
      n_rows = 0
    ))
  }
  id <- factor(edges$edge_id, levels = unique(edges$edge_id))
  rows <- split(seq_len(nrow(edges)), id)
  last <- vapply(rows, function(r) r[[length(r)]], integer(1))
  end_x <- edges$x[last][as.integer(id)]
  end_y <- edges$y[last][as.integer(id)]
  head <- sqrt((edges$x - end_x)^2 + (edges$y - end_y)^2) <= label_arrow_zone

  col <- floor(edges$x / label_ink_cell)
  row <- floor(edges$y / label_ink_cell)
  col0 <- min(col)
  row0 <- min(row)
  n_rows <- max(row) - row0 + 1
  cell <- (col - col0) * n_rows + (row - row0)
  ord <- order(cell)
  list(
    x = edges$x,
    y = edges$y,
    head = head,
    id = as.integer(id),
    order = ord,
    sorted_cell = cell[ord],
    col0 = col0,
    row0 = row0,
    n_cols = max(col) - col0 + 1,
    n_rows = n_rows
  )
}

#' Pair boxes with the ink points in the cells around them
#'
#' For each box, the points in every cell the box grown by `margin` touches;
#' no point elsewhere can come within `margin` of the box. The pairs are
#' built with vectorised `sequence()` and `findInterval()` calls over the
#' sorted cell ids, so the work is proportional to the points that actually
#' need a distance.
#'
#' @param xmin,ymin,xmax,ymax Box limits.
#' @param ink Prepared points from `label_ink_points()`.
#' @param margin The widest margin any test on the pairs will use.
#' @return A list with parallel integer vectors `box` and `point` indexing
#'   the boxes and `ink`.
#' @noRd
ink_cell_pairs <- function(xmin, ymin, xmax, ymax, ink, margin) {
  cell <- label_ink_cell
  col_from <- pmax(floor((xmin - margin) / cell) - ink$col0, 0)
  col_to <- pmin(floor((xmax + margin) / cell) - ink$col0, ink$n_cols - 1)
  row_from <- pmax(floor((ymin - margin) / cell) - ink$row0, 0)
  row_to <- pmin(floor((ymax + margin) / cell) - ink$row0, ink$n_rows - 1)
  n_col <- pmax(0, col_to - col_from + 1)
  n_row <- pmax(0, row_to - row_from + 1)
  n_cell <- n_col * n_row

  # Every (box, cell) pair, walking each box's cell rectangle column-major.
  box <- rep(seq_along(xmin), n_cell)
  k <- sequence(n_cell) - 1
  id <- (col_from[box] + k %/% n_row[box]) *
    ink$n_rows +
    row_from[box] +
    k %% n_row[box]

  first <- findInterval(id, ink$sorted_cell, left.open = TRUE) + 1L
  last <- findInterval(id, ink$sorted_cell)
  count <- pmax(0L, last - first + 1L)
  list(
    box = rep(box, count),
    point = ink$order[sequence(count, from = first)]
  )
}

#' Count the ink points within the margins of each box
#'
#' @param xmin,ymin,xmax,ymax Box limits.
#' @param ink Prepared points from `label_ink_points()`.
#' @return A list with `edge`, the number of points within
#'   `label_edge_clearance` of each box, and `arrow`, the number of
#'   arrowhead-zone points within `label_arrow_clearance` of it.
#' @noRd
ink_box_hits <- function(xmin, ymin, xmax, ymax, ink) {
  n <- length(xmin)
  if (length(ink$x) == 0 || n == 0) {
    return(list(edge = integer(n), arrow = integer(n)))
  }
  pairs <- ink_cell_pairs(xmin, ymin, xmax, ymax, ink, label_arrow_clearance)
  ci <- pairs$box
  pj <- pairs$point
  dist <- rect_point_dist(
    xmin[ci],
    ymin[ci],
    xmax[ci],
    ymax[ci],
    ink$x[pj],
    ink$y[pj]
  )
  list(
    edge = tabulate(ci[dist < label_edge_clearance], nbins = n),
    arrow = tabulate(
      ci[ink$head[pj] & dist < label_arrow_clearance],
      nbins = n
    )
  )
}

#' Count the distinct drawn edges each box hits
#'
#' An edge counts once for a box, however many of its sampled points come
#' within the margins, so a box lying along one long edge is one hit rather
#' than one per sample. The margins are those of `ink_box_hits()`: any point
#' within `label_edge_clearance`, or an arrowhead-zone point within
#' `label_arrow_clearance`.
#'
#' @param xmin,ymin,xmax,ymax Box limits.
#' @param ink Prepared points from `label_ink_points()`.
#' @return Integer, the number of distinct edges hit by each box.
#' @noRd
ink_box_edge_hits <- function(xmin, ymin, xmax, ymax, ink) {
  n <- length(xmin)
  if (length(ink$x) == 0 || n == 0) {
    return(integer(n))
  }
  pairs <- ink_cell_pairs(xmin, ymin, xmax, ymax, ink, label_arrow_clearance)
  ci <- pairs$box
  pj <- pairs$point
  dist <- rect_point_dist(
    xmin[ci],
    ymin[ci],
    xmax[ci],
    ymax[ci],
    ink$x[pj],
    ink$y[pj]
  )
  hit <- dist < label_edge_clearance |
    (ink$head[pj] & dist < label_arrow_clearance)
  if (!any(hit)) {
    return(integer(n))
  }
  box <- ci[hit]
  edge <- ink$id[pj][hit]
  # one key per (box, edge) pair, so duplicates of the same pair drop out
  key <- box * (max(ink$id) + 1L) + edge
  tabulate(box[!duplicated(key)], nbins = n)
}

#' Score candidate boxes against the static obstacles
#'
#' Computes both score tiers of every candidate against the obstacles that
#' do not depend on other labels: node discs (both the hard penetration
#' depth and the soft comfort zone around discs other than the label's own),
#' sampled edge points with their arrowhead zones, and the panel bounds, plus
#' the proximity bands, the proximity pull toward the label's own node, the
#' leader pricing, and the preference-rank tiebreak. The label overlap term
#' is added later, during assignment.
#'
#' @param cand Candidate list from `label_candidates()`.
#' @param nodes,bounds,weights,reach,leader As in `place_dag_labels()`.
#' @param ink Prepared points from `label_ink_points()`.
#' @param own A list with the label's own node center `x`, `y` and disc
#'   `radius`; the nearest disc to it is exempt from the soft term, the
#'   proximity pull measures from it, and clearance is measured from its
#'   disc. `NULL` disables the proximity terms.
#' @param price_violating Whether to price the leaders of candidates that
#'   violate a hard constraint. When `FALSE`, those candidates' `within`
#'   omits the leader term and `unpriced` marks them, so the caller can
#'   price them later with `leader_crossing_length()` and `within_score()`.
#' @return A list with `hard` (weighted violations per candidate),
#'   `violating` (whether each candidate violates a hard constraint), `band`
#'   (the proximity band of each candidate: 0 within `leader`, 1 within
#'   `reach`, 2 beyond), `within` (the score that orders admissible
#'   candidates of one band), `leader` (the leader segment of each
#'   candidate as `x0`, `y0`, `x1`, `y1`, `NA` where none is drawn; `NULL`
#'   without `own`), the `center_dist`, `soft_penalty`, and `own_penalty`
#'   terms of `within`, and `unpriced` (whether each candidate's leader is
#'   still to be priced).
#' @noRd
score_label_candidates <- function(
  cand,
  nodes,
  ink,
  bounds,
  weights,
  own = NULL,
  reach = Inf,
  leader = Inf,
  price_violating = TRUE
) {
  n_cand <- length(cand$x)
  dist_weight <- if ("dist" %in% names(weights)) weights[["dist"]] else 0
  own_weight <- if ("own" %in% names(weights)) weights[["own"]] else 0

  # Node discs: total penetration depth past each disc's required clearance,
  # plus the depth into the soft zone beyond every disc except the label's
  # own node's, and the clearance from the nearest of those other discs,
  # which the ownership term below reads.
  node_penalty <- numeric(n_cand)
  soft_penalty <- numeric(n_cand)
  other_clearance <- rep(Inf, n_cand)
  if (nrow(nodes) > 0) {
    own_index <- if (is.null(own)) {
      0L
    } else {
      which.min((nodes$x - own$x)^2 + (nodes$y - own$y)^2)
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
    soft[j == own_index] <- 0
    soft_penalty <- rowSums(matrix(soft, nrow = n_cand))
    if (own_index > 0L && nrow(nodes) > 1) {
      # `dist` runs candidate-fastest, so the block for node `k` is one
      # contiguous slice; a running elementwise minimum over the other
      # nodes' slices costs one pass per node and no reshaping.
      clearance_all <- dist - nodes$radius[j]
      for (k in setdiff(seq_len(nrow(nodes)), own_index)) {
        slice <- (k - 1) * n_cand + seq_len(n_cand)
        other_clearance <- pmin(other_clearance, clearance_all[slice])
      }
    }
  }

  # Ink: sampled points within the edge margin of the box, and arrowhead-zone
  # points within the arrow margin.
  hits <- ink_box_hits(cand$xmin, cand$ymin, cand$xmax, cand$ymax, ink)

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

  hard <- weights[["node"]] *
    node_penalty +
    weights[["edge"]] * hits$edge +
    weights[["arrow"]] * hits$arrow +
    weights[["bounds"]] * outside_area
  violating <- node_penalty > 0 |
    hits$edge > 0 |
    hits$arrow > 0 |
    outside_area > 0

  # Proximity: the pull toward the label's own node center, the band a
  # candidate's clearance from its own disc puts it in, and the pricing of
  # the leader a candidate past `leader` would be drawn with. `dist`,
  # `soft`, and `own` default to 0 when absent so weights vectors from
  # before those terms existed keep working.
  center_dist <- numeric(n_cand)
  band <- numeric(n_cand)
  leader_extra <- numeric(n_cand)
  own_penalty <- numeric(n_cand)
  leaders <- NULL
  unpriced <- logical(n_cand)
  if (!is.null(own)) {
    center_dist <- sqrt((cand$x - own$x)^2 + (cand$y - own$y)^2)
    clearance <- rect_point_dist(
      cand$xmin,
      cand$ymin,
      cand$xmax,
      cand$ymax,
      own$x,
      own$y
    ) -
      own$radius
    with_leader <- clearance > leader
    band <- with_leader + (clearance > reach)
    # Ownership: a box nearer another node's disc than its own reads as that
    # node's label. The term is how much nearer, counting the box's own
    # clearance only up to `reach`, so a foreign disc beyond the distance a
    # label reads as beside a node claims nothing and a far label in the
    # open with a leader pays nothing. A box that would be drawn without a
    # leader has no cue to correct the reader, so it is raised a band as
    # well and loses to any owned spot, with a leader if need be. Both
    # parts are off at an `own` weight of 0 and with `reach` infinite.
    if (own_weight > 0 && is.finite(reach)) {
      own_penalty <- pmax(0, pmin(clearance, reach) - other_clearance)
      band <- band + (own_penalty > 0 & !with_leader)
    }
    # The price of a leader reaches the score only through the `dist`
    # weight, so under a zero weight no leader needs pricing, then or later.
    price <- with_leader & dist_weight != 0 & (price_violating | !violating)
    unpriced <- with_leader & dist_weight != 0 & !price
    leaders <- leader_crossing_length(
      cand,
      with_leader,
      own,
      nodes,
      ink,
      price = price
    )
    leader_extra <- leaders$extra
  }

  within <- within_score(
    center_dist,
    leader_extra,
    soft_penalty,
    own_penalty,
    cand$rank,
    weights
  )

  list(
    hard = hard,
    violating = violating,
    band = band,
    within = within,
    leader = leaders[c("x0", "y0", "x1", "y1")],
    center_dist = center_dist,
    soft_penalty = soft_penalty,
    own_penalty = own_penalty,
    unpriced = unpriced
  )
}

#' The score that orders admissible candidates of one band
#'
#' The proximity pull toward the label's own node, extended by the priced
#' leader, plus the soft-zone penalty, the ownership penalty, and the
#' preference-rank tiebreak. Every operation is elementwise, so the score of
#' a candidate is the same whether it is computed alongside the label's
#' other candidates or on its own. `dist`, `soft`, and `own` default to 0
#' when absent so weights vectors from before those terms existed keep
#' working.
#'
#' @param center_dist Distance from each box center to the node center.
#' @param leader_extra Length each leader is priced at.
#' @param soft_penalty Penetration depth into the soft zones.
#' @param own_penalty How much nearer each box sits to a foreign disc than to
#'   its own, as `score_label_candidates()` computes it.
#' @param rank Preference rank of each candidate.
#' @param weights As in `place_dag_labels()`.
#' @return Numeric, one score per candidate.
#' @noRd
within_score <- function(
  center_dist,
  leader_extra,
  soft_penalty,
  own_penalty,
  rank,
  weights
) {
  dist_weight <- if ("dist" %in% names(weights)) weights[["dist"]] else 0
  soft_weight <- if ("soft" %in% names(weights)) weights[["soft"]] else 0
  own_weight <- if ("own" %in% names(weights)) weights[["own"]] else 0
  dist_weight *
    (center_dist + leader_extra) +
    soft_weight * soft_penalty +
    own_weight * own_penalty +
    weights[["prefer"]] * rank
}

#' Length a leader is priced at, for itself and for what it crosses
#'
#' The leader of a candidate runs from the edge of the label's node disc to
#' the nearest point of the box. Its price starts at its own length, so that
#' among candidates that need a leader the engine still pulls toward the
#' node. Each ink point within the edge margin of the segment then adds
#' `label_leader_ink_cost` mm, and each disc other than the label's own that
#' the segment crosses adds `label_leader_disc_cost` times its radius, so a
#' longer leader over open space beats a shorter one over ink.
#'
#' @param cand Candidate list from `label_candidates()`.
#' @param with_leader Logical per candidate, whether a leader would be drawn.
#' @param own The label's own node, as in `score_label_candidates()`.
#' @param nodes Node discs.
#' @param ink Prepared points from `label_ink_points()`.
#' @param price Logical per candidate, which of the leaders to price for
#'   what they cross; the others get a segment and their own length but no
#'   crossing price. Every leader by default. The price of a leader is
#'   computed per candidate, so pricing a subset gives each of its
#'   candidates the value pricing every leader would.
#' @return A list with `extra`, the priced length per candidate (0 for
#'   candidates without a leader), and the leader segments `x0`, `y0`, `x1`,
#'   `y1` (`NA` for candidates without one).
#' @noRd
leader_crossing_length <- function(
  cand,
  with_leader,
  own,
  nodes,
  ink,
  price = with_leader
) {
  n_cand <- length(cand$x)
  extra <- numeric(n_cand)
  segment <- list(
    x0 = rep(NA_real_, n_cand),
    y0 = rep(NA_real_, n_cand),
    x1 = rep(NA_real_, n_cand),
    y1 = rep(NA_real_, n_cand)
  )
  idx <- which(with_leader)
  if (length(idx) == 0) {
    return(c(list(extra = extra), segment))
  }

  # The leader runs from the disc edge to the nearest point of the box, as it
  # is drawn; a candidate past the leader threshold is clear of its own
  # disc, so the direction is well defined.
  near_x <- pmin(pmax(own$x, cand$xmin[idx]), cand$xmax[idx])
  near_y <- pmin(pmax(own$y, cand$ymin[idx]), cand$ymax[idx])
  length <- sqrt((near_x - own$x)^2 + (near_y - own$y)^2)
  start_x <- own$x + own$radius * (near_x - own$x) / length
  start_y <- own$y + own$radius * (near_y - own$y) / length
  segment$x0[idx] <- start_x
  segment$y0[idx] <- start_y
  segment$x1[idx] <- near_x
  segment$y1[idx] <- near_y

  # Every leader is priced at its own length, from the disc edge to the box.
  # This part of the price needs no ink, so it is never deferred.
  extra[idx] <- length - own$radius

  # From here on only the leaders to be priced for their crossings take part.
  priced <- which(price[idx])
  if (length(priced) == 0) {
    return(c(list(extra = extra), segment))
  }
  idx <- idx[priced]
  near_x <- near_x[priced]
  near_y <- near_y[priced]
  start_x <- start_x[priced]
  start_y <- start_y[priced]

  if (length(ink$x) > 0) {
    pairs <- ink_cell_pairs(
      pmin(start_x, near_x),
      pmin(start_y, near_y),
      pmax(start_x, near_x),
      pmax(start_y, near_y),
      ink,
      label_edge_clearance
    )
    ci <- pairs$box
    pj <- pairs$point
    dist <- dist_to_edge(
      ink$x[pj],
      ink$y[pj],
      start_x[ci],
      start_y[ci],
      near_x[ci],
      near_y[ci]
    )
    crossed <- tabulate(ci[dist < label_edge_clearance], nbins = length(idx))
    extra[idx] <- extra[idx] + label_leader_ink_cost * crossed
  }

  if (nrow(nodes) > 0) {
    own_index <- which.min((nodes$x - own$x)^2 + (nodes$y - own$y)^2)
    others <- setdiff(seq_len(nrow(nodes)), own_index)
    if (length(others) > 0) {
      ci <- rep(seq_along(idx), times = length(others))
      nj <- rep(others, each = length(idx))
      dist <- dist_to_edge(
        nodes$x[nj],
        nodes$y[nj],
        start_x[ci],
        start_y[ci],
        near_x[ci],
        near_y[ci]
      )
      crossed <- (dist < nodes$radius[nj]) * nodes$radius[nj]
      extra[idx] <- extra[idx] +
        label_leader_disc_cost * rowSums(matrix(crossed, nrow = length(idx)))
    }
  }

  c(list(extra = extra), segment)
}

#' Count what each placed label box still hits
#'
#' The count a label is judged against by `max.overlaps`: the node discs its
#' box penetrates, the distinct drawn edges within the engine's margins, the
#' other boxes it overlaps, and one more when it spills `bounds`. The count
#' is zero exactly when the box violates no hard constraint, so a label the
#' engine placed cleanly is never dropped.
#'
#' @param boxes Data frame with columns `xmin`, `ymin`, `xmax`, `ymax`.
#' @param nodes,edges,bounds As in `place_dag_labels()`.
#' @return Integer, one element per box.
#' @noRd
label_box_overlap_counts <- function(boxes, nodes, edges, bounds) {
  n <- nrow(boxes)
  if (n == 0) {
    return(integer())
  }

  edge_hits <- ink_box_edge_hits(
    boxes$xmin,
    boxes$ymin,
    boxes$xmax,
    boxes$ymax,
    label_ink_points(edges)
  )

  node_hits <- integer(n)
  if (nrow(nodes) > 0) {
    i <- rep(seq_len(n), times = nrow(nodes))
    j <- rep(seq_len(nrow(nodes)), each = n)
    dist <- rect_point_dist(
      boxes$xmin[i],
      boxes$ymin[i],
      boxes$xmax[i],
      boxes$ymax[i],
      nodes$x[j],
      nodes$y[j]
    )
    node_hits <- rowSums(
      matrix(dist < nodes$radius[j] + label_node_clearance, nrow = n)
    )
  }

  box_hits <- integer(n)
  if (n > 1) {
    i <- rep(seq_len(n), times = n)
    j <- rep(seq_len(n), each = n)
    area <- rect_overlap_area(
      boxes$xmin[i],
      boxes$ymin[i],
      boxes$xmax[i],
      boxes$ymax[i],
      boxes$xmin[j],
      boxes$ymin[j],
      boxes$xmax[j],
      boxes$ymax[j]
    )
    area[i == j] <- 0
    box_hits <- rowSums(matrix(area > 0, nrow = n))
  }

  outside <- boxes$xmin < bounds[[1]] |
    boxes$ymin < bounds[[2]] |
    boxes$xmax > bounds[[3]] |
    boxes$ymax > bounds[[4]]

  as.integer(edge_hits + node_hits + box_hits + outside)
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
  # Every panel of one drawn layer shares a tally, so the labels no panel
  # could place are reported once for the whole picture rather than once per
  # panel. See `new_label_draw()` for how the tally decides when to warn.
  draw_layer = function(self, data, params, layout, coord) {
    grobs <- ggplot2::ggproto_parent(ggplot2::Geom, self)$draw_layer(
      data,
      params,
      layout,
      coord
    )
    labelled <- vapply(grobs, inherits, logical(1), "dag_labels_auto")
    if (!any(labelled)) {
      return(grobs)
    }
    draw <- new_label_draw(sum(labelled))
    grobs[labelled] <- lapply(grobs[labelled], function(grob) {
      grob[["draw"]] <- draw
      grob
    })
    grobs
  },
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
    max.overlaps = Inf,
    wrap = NULL,
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
        segment.size = segment.size,
        max.overlaps = max.overlaps %||% Inf,
        wrap = wrap
      ),
      draw = NULL,
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

#' Wrap label text to a width in characters
#'
#' @param label Character vector of label text.
#' @param width Width in characters, or `NULL`/`NA` for no wrapping.
#' @return `label`, with each element's lines joined by newlines.
#' @noRd
wrap_label_text <- function(label, width) {
  if (is.null(width) || length(width) == 0 || is.na(width[[1]])) {
    return(label)
  }
  vapply(
    label,
    function(text) {
      if (is.na(text)) {
        return(text)
      }
      # `strwrap()` never breaks a word, so a label with no space in it, or
      # one whose words are longer than the width, is left as it is
      paste(strwrap(text, width = width[[1]]), collapse = "\n")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' A tally of the labels one drawn layer could not place
#'
#' One warning belongs to one draw, not to one panel and not to one call of
#' `makeContent()`. The tally is an environment `draw_layer()` attaches to
#' every panel's gTree of a layer: each panel records what it could not place
#' under its own grob name, and the panel that completes the tally signals
#' the warning for the union. Because the environment belongs to the grobs,
#' drawing those grobs again (`grid::grid.force()` after a `print()`, or a
#' redraw on resize) finds the warning already signalled and stays quiet,
#' while a fresh draw builds fresh grobs and a fresh tally. Nothing here
#' depends on the text of the warning, so identical pictures drawn one after
#' another each still warn.
#'
#' @param n_panels Number of panels the layer drew label trees for.
#' @return An environment.
#' @noRd
new_label_draw <- function(n_panels) {
  draw <- new.env(parent = emptyenv())
  draw$n_panels <- n_panels
  draw$panels <- list()
  draw$warned <- FALSE
  draw
}

#' Record one panel's unplaced labels and warn once the draw is complete
#'
#' @param x A drawn `dag_labels_auto` gTree, carrying `unresolved`, `dropped`,
#'   and the `draw` tally its layer attached.
#' @return `NULL`, invisibly.
#' @noRd
report_unresolved_labels <- function(x) {
  draw <- x$draw
  if (is.null(draw)) {
    return(invisible(NULL))
  }
  draw$panels[[x$name]] <- list(
    unresolved = x$unresolved,
    dropped = x$dropped
  )
  if (draw$warned || length(draw$panels) < draw$n_panels) {
    return(invisible(NULL))
  }
  draw$warned <- TRUE
  collect <- function(field) {
    as.character(unique(unlist(
      lapply(draw$panels, `[[`, field),
      use.names = FALSE
    )))
  }
  warn_unresolved_labels(collect("unresolved"), collect("dropped"))
}

#' Warn about the labels a draw could not place
#'
#' @param unresolved Character vector of labels the engine could not place
#'   clear of the drawing.
#' @param dropped Character vector of those that were left out of the picture
#'   for exceeding `max.overlaps`.
#' @return `NULL`, invisibly.
#' @noRd
warn_unresolved_labels <- function(unresolved, dropped) {
  if (length(unresolved) == 0 && length(dropped) == 0) {
    return(invisible(NULL))
  }
  if (length(dropped) > 0) {
    warn(
      c(
        "{length(dropped)} label{?s} could not be placed clear of the drawing and {?was/were} dropped: {.val {dropped}}.",
        "i" = "Draw the plot on a larger device, or shorten the labels or wrap them with {.arg wrap}, to make room for {cli::qty(length(dropped))}{?it/them}."
      ),
      warning_class = "ggdag_label_unresolved_warning"
    )
    return(invisible(NULL))
  }
  warn(
    c(
      "{length(unresolved)} label{?s} could not be placed clear of the drawing: {.val {unresolved}}.",
      "i" = "Draw the plot on a larger device, shorten the labels or wrap them with {.arg wrap}, or leave {cli::qty(length(unresolved))}{?it/them} out with {.code max.overlaps = 0}."
    ),
    warning_class = "ggdag_label_unresolved_warning"
  )
}

#' Compute and draw automatically placed labels
#'
#' Runs at draw time, inside the panel viewport, where positions in native
#' units convert to true millimetres: it measures every label's text, models
#' the drawn edges as the ink the reader sees, calls `place_dag_labels()`,
#' and emits the leader lines, boxes, and text of the final placement.
#'
#' The engine is given the panel inset by half a node radius on every side,
#' so no box is set down on the panel border, a reach of one and a half node
#' radii, within which a label sits whenever an admissible spot exists there,
#' and the leader threshold `min.segment.length`, so a leader is drawn only
#' when no admissible spot within that distance exists. After placement every
#' box is tested against the same hard constraints, and the labels whose box
#' still violates one are recorded on the tree as `unresolved`. A label that
#' hits more than `max.overlaps` things is left out of the picture entirely
#' and recorded as `dropped`; the labels that stay are not placed again.
#'
#' @param x A `dag_labels_auto` gTree built by `GeomDagLabelAuto$draw_panel()`.
#' @return `x`, with children set to the drawn grobs, the field `unresolved`
#'   set to the character vector of label texts whose box violates a hard
#'   constraint (`character(0)` when every box is clear), and the field
#'   `dropped` set to those of them that were not drawn.
#' @exportS3Method grid::makeContent
#' @noRd
makeContent.dag_labels_auto <- function(x) {
  labels <- x$labels
  par <- x$params

  # The text is wrapped before it is measured, so the boxes the engine places
  # are the boxes the reader sees. The tree keeps the labels as the stat
  # carried them, so `unresolved` and the warning name a label the way the
  # user wrote it.
  drawn_text <- wrap_label_text(labels$label, par$wrap)

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
    grid::textGrob(drawn_text[i], gp = label_gp(i))
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
  # The router names its nodes by position and breaks ties between equally
  # priced routes by name, so the names must be the ones the routed layer
  # used: keys of the npc positions, taken before the millimetre conversion.
  node_input <- data.frame(
    name = routed_position_keys(x$nodes$x, x$nodes$y),
    x = mm_x(x$nodes$x),
    y = mm_y(x$nodes$y),
    radius = node_radius_mm(x$nodes$node_size),
    stringsAsFactors = FALSE
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
  edge_input <- label_ink(edges_mm, par$edge_cap)

  # The engine works inside the panel inset by half a node radius, so a box
  # that slides back from the border stops short of it; a panel too small
  # for that inset keeps at least half of its extent.
  node_radius <- node_radius_mm(par$node_size)
  inset <- min(
    label_inset_radii * node_radius,
    0.25 * min(panel_width, panel_height)
  )
  bounds <- c(inset, inset, panel_width - inset, panel_height - inset)

  placed <- place_dag_labels(
    label_input,
    node_input,
    edge_input,
    bounds = bounds,
    gap = par$gap,
    reach = label_reach_radii * node_radius,
    leader = par$min.segment.length
  )

  overlaps <- label_box_overlap_counts(
    data.frame(
      xmin = placed$x - widths / 2,
      ymin = placed$y - heights / 2,
      xmax = placed$x + widths / 2,
      ymax = placed$y + heights / 2
    ),
    node_input,
    edge_input,
    bounds
  )
  # `unresolved` is the engine's own result, whatever is drawn afterwards.
  # Dropping is post hoc: the labels over the allowance are left out of the
  # picture, and the engine is not run again without them, so the labels that
  # stay are where they were.
  x$unresolved <- as.character(labels$label[overlaps > 0])
  drop <- overlaps > (par$max.overlaps %||% Inf)
  x$dropped <- as.character(labels$label[drop])
  report_unresolved_labels(x)

  radius <- nearest_node_radius(label_input$x, label_input$y, node_input)

  leaders <- list()
  boxes <- list()
  texts <- list()
  for (i in seq_len(n)) {
    if (drop[i]) {
      next
    }
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
      drawn_text[i],
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
#' keeps each path at the router's own sampling, so a long axis-aligned run
#' stays an obstacle along its whole length. Edges no routed layer draws are
#' returned untouched.
#'
#' @param edges Traced obstacle points in millimetres: `edge_id`, `x`, `y`.
#' @param spec The routing columns of the same rows, as the stat carried them.
#' @param nodes Node centres in millimetres with their `radius`, and the
#'   `name` each node is routed under by the routed layer (the position key
#'   of its npc coordinates); without a `name` column the nodes are named by
#'   their millimetre position, which routes the same paths except where the
#'   router breaks a tie by name.
#' @param par The gTree parameters, carrying `node_size` and `edge_cap`.
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
      sep_min = spec$route_sep_min[first],
      layer_axis = spec$route_layer_axis[first],
      cap = spec$route_cap[first],
      curvature = spec$curvature[first],
      stringsAsFactors = FALSE
    )
  }

  chords <- chord_rows(tagged)
  pinned <- chord_rows(fixed)

  # The router breaks ties between equally priced routes by node name, so
  # the nodes carry the names the routed layer routed with; an endpoint is
  # matched to its node by position, whichever layer measured it.
  router_nodes <- data.frame(
    name = nodes$name %||% routed_position_keys(nodes$x, nodes$y),
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
        sep_min = if (is.na(settings$sep_min)) NULL else settings$sep_min,
        layer_axis = if (is.na(settings$layer_axis)) {
          "auto"
        } else {
          settings$layer_axis
        }
      )
    )
    paths[rows] <- routed$paths[seq_along(rows)]
  }

  # The router's path is the drawn path, point for point; `label_ink()`
  # resamples it with every other edge before placement.
  routed_rows <- do.call(
    rbind,
    lapply(seq_len(nrow(chords)), function(i) {
      data.frame(
        edge_id = chords$edge_id[[i]],
        x = paths[[i]]$x,
        y = paths[[i]]$y,
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

# The ink model the labels are placed against, in mm. The panel is inset by
# this many node radii on every side, a label sits within this many radii of
# its disc whenever it can, and every drawn path is resampled at this spacing
# once the cap has been cut from both of its ends.
label_inset_radii <- 0.5
label_reach_radii <- 1.5
label_ink_spacing <- 0.5

#' The drawn edges as the ink the reader sees
#'
#' Turns the traced edge polylines into the obstacle points the engine is
#' scored against. A traced polyline with no bend is collapsed to its chord,
#' which is the path the straight engine draws; every path is then resampled
#' every `spacing` mm, segment by segment, and the points within `cap` mm of
#' either end are dropped, because that is where the arrow layer resects the
#' path to make room for the node and its arrowhead. What remains is the
#' part of each edge actually on the page, with its last `label_arrow_zone`
#' mm carrying the drawn head. An edge shorter than twice the cap disappears
#' entirely.
#'
#' @param edges Data frame with columns `edge_id`, `x`, and `y`, in mm.
#' @param cap Length in mm cut from each end.
#' @param spacing Resampling spacing in mm.
#' @return A data frame with columns `edge_id`, `x`, and `y`.
#' @noRd
label_ink <- function(edges, cap, spacing = label_ink_spacing) {
  empty <- edges[0, c("edge_id", "x", "y"), drop = FALSE]
  if (nrow(edges) == 0) {
    return(empty)
  }

  pieces <- lapply(
    split(
      seq_len(nrow(edges)),
      factor(edges$edge_id, levels = unique(edges$edge_id))
    ),
    function(rows) {
      path <- polyline_chord(edges$x[rows], edges$y[rows])
      dense <- densify_polyline(path$x, path$y, spacing)
      n <- length(dense$x)
      to_ends <- pmin(
        sqrt((dense$x - dense$x[[1]])^2 + (dense$y - dense$y[[1]])^2),
        sqrt((dense$x - dense$x[[n]])^2 + (dense$y - dense$y[[n]])^2)
      )
      keep <- to_ends > cap
      if (sum(keep) < 2) {
        return(NULL)
      }
      data.frame(
        edge_id = edges$edge_id[rows[[1]]],
        x = dense$x[keep],
        y = dense$y[keep],
        stringsAsFactors = FALSE
      )
    }
  )
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0) {
    return(empty)
  }

  do.call(rbind, pieces)
}

#' Collapse a straight polyline to its two endpoints
#'
#' A traced straight edge arrives as many points along one line; the drawn
#' chord is the same line, and resampling the two gives the same points only
#' when they share their vertices.
#'
#' @param px,py Ordered polyline coordinates.
#' @return A list with `x` and `y`: the endpoints when every point lies on
#'   the chord between them, the polyline itself otherwise.
#' @noRd
polyline_chord <- function(px, py) {
  n <- length(px)
  if (n <= 2) {
    return(list(x = px, y = py))
  }
  off_chord <- dist_to_edge(px, py, px[[1]], py[[1]], px[[n]], py[[n]])
  if (max(off_chord) <= 1e-6) {
    return(list(x = px[c(1L, n)], y = py[c(1L, n)]))
  }
  list(x = px, y = py)
}

#' Resample a polyline every `spacing` along each segment
#'
#' Each segment is split into as many equal steps as it takes to keep the
#' spacing at or below `spacing`, so a path a box sits beside is caught by a
#' point beside it. The first point and every vertex are kept.
#'
#' @param px,py Ordered polyline coordinates.
#' @param spacing Maximum spacing between consecutive points.
#' @return A list with the resampled `x` and `y`.
#' @noRd
densify_polyline <- function(px, py, spacing) {
  dx <- diff(px)
  dy <- diff(py)
  steps <- pmax(1, ceiling(sqrt(dx^2 + dy^2) / spacing))
  segment <- rep(seq_along(steps), steps)
  fraction <- sequence(steps) / steps[segment]
  list(
    x = c(px[[1]], px[segment] + fraction * dx[segment]),
    y = c(py[[1]], py[segment] + fraction * dy[segment])
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
#' per-edge curvature drawn by the ggarrow engine (see [curve_edge()]).
#'
#' @section Placement rules:
#' The placement works in the millimetres of the device, so the same plot
#' places its labels the same way at every size that has the same room. A
#' label box is admissible when it clears every node disc, comes no closer
#' than 1 mm to a drawn edge and 2 mm to the last 5 mm of one (where the
#' arrowhead is), overlaps no other label box, and stays inside the panel by
#' half a node radius, so no box is set down on the panel border. Among
#' admissible boxes, a label sits within one and a half node radii of its
#' own disc whenever such a spot exists, preferring the nearer and the
#' anchor order NE, NW, SE, SW, N, S, E, W, and drifting away from other
#' nodes' discs. A label also stays its own node's: a box nearer another
#' node's disc than its own pays for the difference, and a box that would
#' be drawn without a leader beside the wrong node gives way to a spot
#' beside its own node even when that spot needs a leader. A leader line is
#' a fallback: a label is placed further than `min.segment.length` from its
#' disc, and drawn with a leader from the disc to the box, only when no
#' admissible spot within that distance exists. A label with no admissible
#' spot within one and a half node radii searches a fine grid around its
#' own node before it goes farther, and a leader is priced by its length
#' and by every edge or disc it crosses, so a longer leader over open space
#' beats a shorter one across the ink. When
#' no admissible box exists at all, the label is drawn at the least-bad
#' position and its text is recorded in the `unresolved` field of the drawn
#' `dag_labels_auto` grob tree (`character(0)` when every box is clear), so
#' a plot too crowded for its labels can be detected after drawing. Such a
#' draw also warns once, naming the labels; `max.overlaps` leaves them out of
#' the picture instead of drawing them on the ink, and `wrap` gives a long
#' label a smaller box to find room for.
#'
#' @inheritParams geom_dag_arrow
#' @param node_size The size of the plot's nodes, as given to
#'   [geom_dag_point()]. `NULL`, the default, discovers it from the plot.
#' @param n_edge_points Number of points traced along each drawn curved edge
#'   before the trace is resampled every 0.5 mm at draw time; a
#'   straight edge is traced as its chord whatever the value. `NULL`, the
#'   default, uses 20.
#' @param n_node_points Accepted for compatibility with the repel label
#'   geoms; the automatic placement describes each node by its drawn disc, so
#'   this is ignored.
#' @param edge_cap The distance in millimetres that drawn edges stop short of
#'   the node, as in [geom_dag()]; the traced edges are cut by the same
#'   amount at both ends so the ink each label avoids is the ink on the page
#'   and its arrowhead zone ends where the drawn arrowhead does. `NULL`, the
#'   default, uses the `ggdag.edge_cap` option (8 mm).
#' @param gap Clearance in millimetres between a node disc and its label box.
#' @param label.padding Padding around the label text, as a [grid::unit()].
#' @param label.r Radius of the label box corners, as a [grid::unit()].
#' @param label.size Width of the label box border in millimetres. The
#'   default, `NA`, draws no border.
#' @param min.segment.length Distance in millimetres from the node disc past
#'   which a label gets a leader line back to its node. A label is placed
#'   past it only when no admissible spot within it exists.
#' @param segment.colour,segment.size Colour and linewidth of the leader
#'   lines.
#' @param max.overlaps Maximum number of things a label's final box may still
#'   hit and be drawn anyway: the node discs it penetrates, the drawn edges
#'   within the placement margins, the other label boxes it overlaps, and the
#'   panel bounds it spills. The default, `Inf`, draws every label. A finite
#'   value leaves out, after placement and without running the engine again,
#'   every label whose box hits more than that many things, so
#'   `max.overlaps = 0` leaves out exactly the labels reported as
#'   `unresolved`. The labels that stay do not move.
#' @param wrap Width in characters to wrap the label text to, through
#'   [base::strwrap()]. The wrapped text is what is measured, placed, and
#'   drawn, and a word longer than the width is left whole. `NULL`, the
#'   default, and `NA` wrap nothing.
#' @param box.padding Accepted for compatibility with the repel label geoms
#'   and ignored.
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
  max.overlaps = Inf,
  wrap = NULL,
  box.padding = NULL,
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
      max.overlaps = max.overlaps,
      wrap = wrap,
      ...
    )
  )

  dag_layer(layer, discover = c("node_size", "edge_geometry"), debug = TRUE)
}

geom_dag_label_auto <- dag_node_aware(
  geom_dag_label_auto,
  extra = c("edge_cap", "wrap")
)

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
  max.overlaps = Inf,
  wrap = NULL,
  box.padding = NULL,
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
      max.overlaps = max.overlaps,
      wrap = wrap,
      ...
    )
  )

  dag_layer(layer, discover = c("node_size", "edge_geometry"), debug = TRUE)
}

geom_dag_text_auto <- dag_node_aware(
  geom_dag_text_auto,
  extra = c("edge_cap", "wrap")
)
