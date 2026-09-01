# Specification tests for place_dag_labels(), the deterministic label
# placement engine to be implemented in R/label_auto.R. The engine does not
# exist yet, so every test in this file fails until it does; together these
# tests pin its observable behavior.
#
# All coordinates are in mm. place_dag_labels() takes:
#   labels  data frame: id (chr), x, y (node centers), width, height (box)
#   nodes   data frame: x, y, radius, one row per node disc
#   edges   data frame: edge_id, x, y, ordered polyline points per edge_id;
#           the final segment of each edge_id is the arrowhead zone
#   bounds  numeric length 4: xmin, ymin, xmax, ymax (panel extent)
#   gap     clearance in mm between the node disc edge and the label box edge
# and returns one row per label, in input order: id, x, y (box centers),
# anchor, score.
#
# Candidate geometry these tests pin:
# * Anchors in fixed preference order ne, nw, se, sw, n, s, e, w (45, 135,
#   315, 225, 90, 270, 0, 180 degrees); n_angles keeps the first n_angles
#   anchors of that order.
# * Ring 1 places the box so its nearest point to the node center sits at
#   distance radius + gap along the anchor ray: for a diagonal anchor that
#   nearest point is the box corner facing the node, for a cardinal anchor
#   the midpoint of the facing side.
# * Each further ring steps that distance outward by half the box diagonal.
# * Candidates are ordered ring-major: every anchor at ring 1 precedes any
#   anchor at ring 2, and the prefer term breaks ties in that order.

anchor_vocabulary <- c("ne", "nw", "se", "sw", "n", "s", "e", "w")

# Center of the candidate box for `anchor` at `ring`, per the geometry above.
candidate_center <- function(
  x,
  y,
  radius,
  gap,
  width,
  height,
  anchor,
  ring = 1
) {
  d <- radius + gap + (ring - 1) * sqrt(width^2 + height^2) / 2
  dd <- d / sqrt(2)
  switch(
    anchor,
    ne = c(x + dd + width / 2, y + dd + height / 2),
    nw = c(x - dd - width / 2, y + dd + height / 2),
    se = c(x + dd + width / 2, y - dd - height / 2),
    sw = c(x - dd - width / 2, y - dd - height / 2),
    n = c(x, y + d + height / 2),
    s = c(x, y - d - height / 2),
    e = c(x + d + width / 2, y),
    w = c(x - d - width / 2, y)
  )
}

box_around <- function(cx, cy, width, height) {
  c(
    xmin = cx - width / 2,
    ymin = cy - height / 2,
    xmax = cx + width / 2,
    ymax = cy + height / 2
  )
}

placed_box <- function(result, labels, id) {
  i <- match(id, result$id)
  j <- match(id, labels$id)
  box_around(result$x[i], result$y[i], labels$width[j], labels$height[j])
}

box_dist_to_point <- function(box, px, py) {
  dx <- max(box[["xmin"]] - px, px - box[["xmax"]], 0)
  dy <- max(box[["ymin"]] - py, py - box[["ymax"]], 0)
  sqrt(dx^2 + dy^2)
}

box_overlap_area <- function(a, b) {
  overlap_w <- min(a[["xmax"]], b[["xmax"]]) - max(a[["xmin"]], b[["xmin"]])
  overlap_h <- min(a[["ymax"]], b[["ymax"]]) - max(a[["ymin"]], b[["ymin"]])
  max(overlap_w, 0) * max(overlap_h, 0)
}

n_points_in_box <- function(box, px, py) {
  sum(
    px >= box[["xmin"]] &
      px <= box[["xmax"]] &
      py >= box[["ymin"]] &
      py <= box[["ymax"]]
  )
}

straight_edge <- function(edge_id, x, y, xend, yend, n = 20) {
  t <- seq(0, 1, length.out = n)
  data.frame(edge_id = edge_id, x = x + t * (xend - x), y = y + t * (yend - y))
}

no_edges <- data.frame(edge_id = character(), x = numeric(), y = numeric())

test_that("a lone node's label sits at the NE anchor at ring 1", {
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-40, -40, 40, 40)

  res <- place_dag_labels(labels, nodes, no_edges, bounds)

  # Nearest box point (its SW corner) sits at radius + gap = 5.5 mm along the
  # 45 degree ray: corner (5.5 / sqrt(2), 5.5 / sqrt(2)) = (3.8890873,
  # 3.8890873), so the center is offset by half the box from there:
  # (12.8890873, 7.8890873).
  expected <- candidate_center(0, 0, 4, 1.5, 18, 8, "ne")
  expect_equal(res$anchor, "ne")
  expect_equal(res$x, expected[[1]], tolerance = 1e-6)
  expect_equal(res$y, expected[[2]], tolerance = 1e-6)

  # The box clears the node disc by exactly `gap`, so it cannot intersect the
  # disc.
  box <- placed_box(res, labels, "a")
  expect_equal(box_dist_to_point(box, 0, 0) - 4, 1.5, tolerance = 1e-6)
})

test_that("gap sets the clearance between the node disc and the label box", {
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-40, -40, 40, 40)

  res <- place_dag_labels(labels, nodes, no_edges, bounds, gap = 3)

  # Same geometry as the default, with the nearest-point distance now
  # 4 + 3 = 7 mm: corner (7 / sqrt(2), 7 / sqrt(2)) = (4.9497475, 4.9497475).
  expected <- candidate_center(0, 0, 4, 3, 18, 8, "ne")
  expect_equal(res$anchor, "ne")
  expect_equal(res$x, expected[[1]], tolerance = 1e-6)
  expect_equal(res$y, expected[[2]], tolerance = 1e-6)

  box <- placed_box(res, labels, "a")
  expect_equal(box_dist_to_point(box, 0, 0) - 4, 3, tolerance = 1e-6)
})

test_that("an obstacle disc over the NE candidates forces the NW anchor", {
  # An obstacle disc of radius 12 centered on the NE ring 2 candidate center
  # (19.8532814, 14.8532814). Distances from that center to the NE candidate
  # boxes are 2.96 mm (ring 1), 0 (ring 2, center inside), and 2.96 mm
  # (ring 3), so every NE candidate overlaps the disc. The NW ring 1 box
  # (x in [-21.889, -3.889], y in [3.889, 11.889]) keeps a Euclidean
  # clearance of 23.93 - 12 = 11.93 mm from the disc, making NW the next
  # candidate by preference.
  obstacle <- candidate_center(0, 0, 4, 1.5, 18, 8, "ne", ring = 2)
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(
    x = c(0, obstacle[[1]]),
    y = c(0, obstacle[[2]]),
    radius = c(4, 12)
  )
  bounds <- c(-60, -60, 60, 60)

  res <- place_dag_labels(labels, nodes, no_edges, bounds)

  expected <- candidate_center(0, 0, 4, 1.5, 18, 8, "nw")
  expect_equal(res$anchor, "nw")
  expect_equal(res$x, expected[[1]], tolerance = 1e-6)
  expect_equal(res$y, expected[[2]], tolerance = 1e-6)

  # The placed box stays clear of the obstacle disc.
  box <- placed_box(res, labels, "a")
  expect_gte(box_dist_to_point(box, obstacle[[1]], obstacle[[2]]), 12)
})

test_that("edges through the northern candidates push the label to SE", {
  # Three horizontal polylines at y = 8, 15, and 20 (x from -40 to 40) pass
  # through every candidate box that would otherwise beat SE ring 1:
  #   NE/NW ring 1 boxes span y [3.889, 11.889] (crossed by y = 8)
  #   NE/NW ring 2 boxes span y [10.853, 18.853] (crossed by y = 15)
  #   NE/NW ring 3 boxes span y [17.817, 25.817] (crossed by y = 20)
  #   N ring 1 spans y [5.5, 13.5] (y = 8); N ring 2 spans y [15.349, 23.349]
  #   (y = 15 and y = 20)
  # The SE ring 1 box (y in [-11.889, -3.889]) is untouched and is the most
  # preferred clean candidate.
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  edges <- rbind(
    straight_edge("e1", -40, 8, 40, 8),
    straight_edge("e2", -40, 15, 40, 15),
    straight_edge("e3", -40, 20, 40, 20)
  )
  bounds <- c(-50, -50, 50, 50)

  res <- place_dag_labels(labels, nodes, edges, bounds)

  expected <- candidate_center(0, 0, 4, 1.5, 18, 8, "se")
  expect_equal(res$anchor, "se")
  expect_equal(res$x, expected[[1]], tolerance = 1e-6)
  expect_equal(res$y, expected[[2]], tolerance = 1e-6)
})

test_that("an arrowhead-zone overlap loses to an equal non-final overlap", {
  # n_angles = 2 keeps only the NE and NW anchors and n_rings = 1 keeps
  # ring 1, so exactly two candidates compete. Two vertical polylines mirror
  # each other through x = 0, so each crosses its candidate box with
  # identical geometry: three of the 20 sampled points fall inside each box.
  # The right polyline ends at the NE box center, so its final (arrowhead)
  # segment lies inside the NE box; the mirrored left polyline starts at the
  # NW box center, so only non-final segments lie inside the NW box. With
  # equal edge violations, the arrow penalty must push the label to NW even
  # though NE is preferred.
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-60, -60, 60, 60)

  ne <- candidate_center(0, 0, 4, 1.5, 18, 8, "ne")
  right_y <- seq(-30, ne[[2]], length.out = 20)
  edges <- rbind(
    data.frame(edge_id = "r", x = ne[[1]], y = right_y),
    data.frame(edge_id = "l", x = -ne[[1]], y = rev(right_y))
  )

  res <- place_dag_labels(
    labels,
    nodes,
    edges,
    bounds,
    n_angles = 2L,
    n_rings = 1L
  )
  expect_equal(res$anchor, "nw")

  # Reversing both polylines moves the arrowhead zones, flipping the winner.
  edges_flipped <- rbind(
    data.frame(edge_id = "r", x = ne[[1]], y = rev(right_y)),
    data.frame(edge_id = "l", x = -ne[[1]], y = right_y)
  )
  res_flipped <- place_dag_labels(
    labels,
    nodes,
    edges_flipped,
    bounds,
    n_angles = 2L,
    n_rings = 1L
  )
  expect_equal(res_flipped$anchor, "ne")
})

test_that("weights rescale the score terms", {
  # Same fixture as the arrowhead-zone test above, where the default weights
  # push the label to NW because the right polyline's final segment lies
  # inside the NE box. Zeroing the arrow weight removes that penalty, the two
  # candidates tie on every remaining term, and the prefer term restores the
  # preferred NE anchor.
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-60, -60, 60, 60)

  ne <- candidate_center(0, 0, 4, 1.5, 18, 8, "ne")
  right_y <- seq(-30, ne[[2]], length.out = 20)
  edges <- rbind(
    data.frame(edge_id = "r", x = ne[[1]], y = right_y),
    data.frame(edge_id = "l", x = -ne[[1]], y = rev(right_y))
  )

  res <- place_dag_labels(
    labels,
    nodes,
    edges,
    bounds,
    n_angles = 2L,
    n_rings = 1L,
    weights = c(
      node = 100,
      edge = 12,
      arrow = 0,
      label = 30,
      bounds = 60,
      prefer = 0.01
    )
  )
  expect_equal(res$anchor, "ne")

  expect_true("weights" %in% names(formals(place_dag_labels)))
})

test_that("two labels on the same node do not overlap", {
  labels <- data.frame(
    id = c("a", "b"),
    x = 0,
    y = 0,
    width = 18,
    height = 8
  )
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-60, -60, 60, 60)

  res <- place_dag_labels(labels, nodes, no_edges, bounds)

  expect_identical(res$id, c("a", "b"))
  expect_true(all(res$anchor %in% anchor_vocabulary))

  box_a <- placed_box(res, labels, "a")
  box_b <- placed_box(res, labels, "b")
  expect_equal(box_overlap_area(box_a, box_b), 0)

  # Both boxes still clear the shared node disc.
  expect_gte(box_dist_to_point(box_a, 0, 0), 4)
  expect_gte(box_dist_to_point(box_b, 0, 0), 4)
})

test_that("a node near the panel corner gets its label pulled inside", {
  # The panel's top-right corner sits at (8, 8). NE, SE, N, S, and E ring 1
  # boxes all cross the right or top panel edge; the NW ring 1 box crosses
  # the top edge (it spans y up to 11.889 > 8). The first candidate in
  # preference order that lies fully inside is SW ring 1 (x in [-21.889,
  # -3.889], y in [-11.889, -3.889]); W ring 1 is also fully inside but ranks
  # after SW, so the prefer term settles the tie for SW.
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-60, -60, 8, 8)

  res <- place_dag_labels(labels, nodes, no_edges, bounds)

  expected <- candidate_center(0, 0, 4, 1.5, 18, 8, "sw")
  expect_equal(res$anchor, "sw")
  expect_equal(res$x, expected[[1]], tolerance = 1e-6)
  expect_equal(res$y, expected[[2]], tolerance = 1e-6)

  box <- placed_box(res, labels, "a")
  expect_gte(box[["xmin"]], -60)
  expect_gte(box[["ymin"]], -60)
  expect_lte(box[["xmax"]], 8)
  expect_lte(box[["ymax"]], 8)
})

test_that("every candidate outside bounds still yields a least-bad row", {
  # The panel is a 6 x 6 mm square around the node, far too small for any
  # candidate box, so every candidate is fully outside bounds. The engine
  # must still return the least-bad placement rather than dropping the label
  # or erroring.
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-3, -3, 3, 3)

  res <- place_dag_labels(labels, nodes, no_edges, bounds)

  expect_identical(nrow(res), 1L)
  expect_true(is.finite(res$x))
  expect_true(is.finite(res$y))
  expect_true(res$anchor %in% anchor_vocabulary)
  expect_true(is.finite(res$score))
  expect_gt(res$score, 0)

  # Least-bad selection is still deterministic.
  res_again <- place_dag_labels(labels, nodes, no_edges, bounds)
  expect_identical(res, res_again)
})

test_that("rings step outward by half the box diagonal", {
  # Eight obstacle discs of radius 2 sit at distance radius + gap + 2 =
  # 7.5 mm along each anchor ray, placing each disc center inside its ring 1
  # candidate box (diagonal rays: (5.303, 5.303) and reflections; cardinal
  # rays: (0, 7.5), (7.5, 0) and reflections), so all eight ring 1 candidates
  # are dirty. Every ring 2 box clears every disc by more than the gap: the
  # tightest pair is the NE ring 2 box corner at (10.853, 10.853), which is
  # sqrt(5.55^2 + 5.55^2) - 2 = 5.85 mm from the NE ray disc. The winner is
  # therefore NE at ring 2: nearest-point distance 5.5 + sqrt(18^2 + 8^2) / 2
  # = 15.3488578 mm, corner (10.8532814, 10.8532814), center (19.8532814,
  # 14.8532814).
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  angles <- pi / 180 * c(45, 135, 315, 225, 90, 270, 0, 180)
  nodes <- data.frame(
    x = c(0, 7.5 * cos(angles)),
    y = c(0, 7.5 * sin(angles)),
    radius = c(4, rep(2, 8))
  )
  bounds <- c(-60, -60, 60, 60)

  res <- place_dag_labels(labels, nodes, no_edges, bounds)

  expected <- candidate_center(0, 0, 4, 1.5, 18, 8, "ne", ring = 2)
  expect_equal(res$anchor, "ne")
  expect_equal(res$x, expected[[1]], tolerance = 1e-6)
  expect_equal(res$y, expected[[2]], tolerance = 1e-6)
})

test_that("constrained labels are placed before flexible ones", {
  # Corridor bounds c(-60, -6.5, 45, 6.5) leave room only for the E and W
  # anchors, whose boxes span y in [-4, 4]; every other anchor's box leaves
  # the corridor vertically at every ring. Node a at (0, 0) has four clean
  # candidates: E ring 1 (x in [5.5, 23.5], clearing node b's disc x in
  # [26, 34] by 2.5 mm) and W rings 1 to 3 (down to x = -43.2 > -60). Its E
  # rings 2 and 3 (x from 15.35 and 25.20) overlap node b's disc. Node b at
  # (30, 0) has exactly one clean candidate, W ring 1 (x in [6.5, 24.5],
  # clearing node a's disc x in [-4, 4] by 2.5 mm): its E candidates leave
  # the right edge of the corridor and its W rings 2 and 3 (x from -3.35 and
  # -13.20) overlap node a's disc.
  #
  # Placing in input order would give a its preferred E ring 1, which
  # overlaps b's only clean candidate. Constrainedness-first placement gives
  # b its W ring 1 centered at (15.5, 0), after which a's E ring 1 overlaps
  # b's label and a takes its own W ring 1 centered at (-14.5, 0).
  labels <- data.frame(
    id = c("a", "b"),
    x = c(0, 30),
    y = 0,
    width = 18,
    height = 8
  )
  nodes <- data.frame(x = c(0, 30), y = 0, radius = 4)
  bounds <- c(-60, -6.5, 45, 6.5)

  res <- place_dag_labels(labels, nodes, no_edges, bounds)

  expect_identical(res$id, c("a", "b"))
  expect_equal(res$anchor, c("w", "w"))

  expected_a <- candidate_center(0, 0, 4, 1.5, 18, 8, "w")
  expected_b <- candidate_center(30, 0, 4, 1.5, 18, 8, "w")
  expect_equal(res$x, c(expected_a[[1]], expected_b[[1]]), tolerance = 1e-6)
  expect_equal(res$y, c(expected_a[[2]], expected_b[[2]]), tolerance = 1e-6)

  box_a <- placed_box(res, labels, "a")
  box_b <- placed_box(res, labels, "b")
  expect_equal(box_overlap_area(box_a, box_b), 0)
})

test_that("input row order does not affect placements", {
  # Same corridor fixture as above, with the label rows permuted. Placements
  # must match by id; only the output row order follows the input.
  labels_ab <- data.frame(
    id = c("a", "b"),
    x = c(0, 30),
    y = 0,
    width = 18,
    height = 8
  )
  labels_ba <- labels_ab[c(2, 1), ]
  nodes <- data.frame(x = c(0, 30), y = 0, radius = 4)
  bounds <- c(-60, -6.5, 45, 6.5)

  res_ab <- place_dag_labels(labels_ab, nodes, no_edges, bounds)
  res_ba <- place_dag_labels(labels_ba, nodes, no_edges, bounds)

  expect_identical(res_ab$id, c("a", "b"))
  expect_identical(res_ba$id, c("b", "a"))

  matched <- match(res_ab$id, res_ba$id)
  expect_equal(res_ba$x[matched], res_ab$x, tolerance = 1e-6)
  expect_equal(res_ba$y[matched], res_ab$y, tolerance = 1e-6)
  expect_identical(res_ba$anchor[matched], res_ab$anchor)
})

test_that("mediation triangle labels avoid the edges geometrically", {
  # Nodes x (0, 0), m (40, 20), y (80, 0), radius 4; labels 18 x 8 mm.
  # Straight edges x -> m, m -> y, and x -> y, each sampled at 20 points
  # including both endpoints.
  #
  # Hand-verified winners:
  # * x: the x -> m edge (the line y = x / 2) crosses the NE ring 1 box
  #   between x = 7.78 and x = 21.89, crosses the NE ring 2 box between
  #   x = 21.71 and x = 28.85, and has the sampled point (35.789, 17.895)
  #   inside the NE ring 3 box, so every NE candidate is dirty. Every edge
  #   point stays at least 5.5 mm from the NW ring 1 box (the nearest edge
  #   point is the endpoint (0, 0), 5.5 mm from the box corner at (-3.889,
  #   3.889)), so NW ring 1 is the first clean candidate: anchor "nw".
  # * m: both incident edges leave m heading down toward x and y, and the
  #   nearest approach of any edge to the NE ring 1 box is 5.22 mm (the
  #   m -> y line x + 2y = 80 passes 11.667 / sqrt(5) = 5.218 mm from the box
  #   corner at (43.889, 23.889)), so NE ring 1 is clean: anchor "ne".
  # * y: every edge point has x <= 80 while the NE ring 1 box starts at
  #   x = 83.889; the nearest edge point (80, 0) is 5.5 mm from the box
  #   corner, so NE ring 1 is clean: anchor "ne".
  labels <- data.frame(
    id = c("x", "m", "y"),
    x = c(0, 40, 80),
    y = c(0, 20, 0),
    width = 18,
    height = 8
  )
  nodes <- data.frame(x = c(0, 40, 80), y = c(0, 20, 0), radius = 4)
  edges <- rbind(
    straight_edge("xm", 0, 0, 40, 20),
    straight_edge("my", 40, 20, 80, 0),
    straight_edge("xy", 0, 0, 80, 0)
  )
  bounds <- c(-40, -40, 120, 60)

  res <- place_dag_labels(labels, nodes, edges, bounds)

  expect_identical(res$id, c("x", "m", "y"))
  expect_equal(res$anchor, c("nw", "ne", "ne"))

  expected_x <- candidate_center(0, 0, 4, 1.5, 18, 8, "nw")
  expected_m <- candidate_center(40, 20, 4, 1.5, 18, 8, "ne")
  expected_y <- candidate_center(80, 0, 4, 1.5, 18, 8, "ne")
  expect_equal(
    res$x,
    c(expected_x[[1]], expected_m[[1]], expected_y[[1]]),
    tolerance = 1e-6
  )
  expect_equal(
    res$y,
    c(expected_x[[2]], expected_m[[2]], expected_y[[2]]),
    tolerance = 1e-6
  )

  # Structural invariants: no placed box contains a sampled edge point,
  # overlaps a node disc, or overlaps another label.
  boxes <- lapply(labels$id, placed_box, result = res, labels = labels)
  for (box in boxes) {
    expect_identical(n_points_in_box(box, edges$x, edges$y), 0L)
    for (k in seq_len(nrow(nodes))) {
      expect_gte(
        box_dist_to_point(box, nodes$x[k], nodes$y[k]),
        nodes$radius[k]
      )
    }
  }
  expect_equal(box_overlap_area(boxes[[1]], boxes[[2]]), 0)
  expect_equal(box_overlap_area(boxes[[1]], boxes[[3]]), 0)
  expect_equal(box_overlap_area(boxes[[2]], boxes[[3]]), 0)
})

test_that("curved edge polylines act as obstacles", {
  # Five nodes: a (0, 0), b (40, 0), c (80, 0), d (0, 30), e (80, 30), all
  # radius 4, labels 18 x 8 mm. Straight edges a -> b, b -> c, d -> b, and
  # e -> b, plus a curved a -> c edge (curvature 0.15) sampled with
  # sample_curved_edge(). Positive curvature bows the edge below the line
  # y = 0, dipping to y = -9.6 at x = 40.
  #
  # Around b every ring 1 candidate box contains sampled edge points: the
  # d -> b and e -> b edges (lines y = 30 -/+ 0.75 x shifted) cross the
  # northern boxes, the straight a -> b and b -> c edges run through the E
  # and W boxes, and the sampled curve points (x = 4 k, y = 2 t (1 - t) *
  # -19.206 with t = x / 80) fall inside the S, SE, and SW boxes (for
  # example (40, -9.603) in S, (44, -9.507) in SE, (36, -9.507) in SW). So
  # b's label must move farther out; the first fully clear candidate in
  # preference order is SE ring 2 (x in [51.853, 69.853], y in [-18.853,
  # -10.853]), which every sampled edge point misses (the curve stays above
  # y = -8.8 over that x range and the other edges stay at or above y = 0).
  # Nodes d, e, and c have unobstructed NE quadrants (every nearby edge
  # point stays at least 5.5 mm away), so their labels take NE ring 1.
  curve <- sample_curved_edge(0, 0, 80, 0, curvature = 0.15, n = 21)
  edges <- rbind(
    straight_edge("ab", 0, 0, 40, 0),
    straight_edge("bc", 40, 0, 80, 0),
    straight_edge("db", 0, 30, 40, 0),
    straight_edge("eb", 80, 30, 40, 0),
    data.frame(edge_id = "ac", x = curve$x, y = curve$y)
  )
  labels <- data.frame(
    id = c("a", "b", "c", "d", "e"),
    x = c(0, 40, 80, 0, 80),
    y = c(0, 0, 0, 30, 30),
    width = 18,
    height = 8
  )
  nodes <- data.frame(
    x = c(0, 40, 80, 0, 80),
    y = c(0, 0, 0, 30, 30),
    radius = 4
  )
  bounds <- c(-40, -40, 120, 70)

  res <- place_dag_labels(labels, nodes, edges, bounds)

  expect_identical(res$id, labels$id)
  expect_equal(res$anchor[match(c("c", "d", "e"), res$id)], c("ne", "ne", "ne"))

  # Structural invariants for every label, including b's unpinned placement:
  # no sampled edge point inside a placed box, no node disc overlap, and no
  # label-label overlap.
  boxes <- lapply(labels$id, placed_box, result = res, labels = labels)
  for (box in boxes) {
    expect_identical(n_points_in_box(box, edges$x, edges$y), 0L)
    for (k in seq_len(nrow(nodes))) {
      expect_gte(
        box_dist_to_point(box, nodes$x[k], nodes$y[k]),
        nodes$radius[k]
      )
    }
  }
  pairs <- utils::combn(length(boxes), 2)
  for (p in seq_len(ncol(pairs))) {
    expect_equal(
      box_overlap_area(boxes[[pairs[1, p]]], boxes[[pairs[2, p]]]),
      0
    )
  }
})

test_that("the result has one row per label in input order", {
  labels <- data.frame(
    id = c("x", "m", "y"),
    x = c(0, 40, 80),
    y = c(0, 20, 0),
    width = 18,
    height = 8
  )
  nodes <- data.frame(x = c(0, 40, 80), y = c(0, 20, 0), radius = 4)
  edges <- rbind(
    straight_edge("xm", 0, 0, 40, 20),
    straight_edge("my", 40, 20, 80, 0),
    straight_edge("xy", 0, 0, 80, 0)
  )
  bounds <- c(-40, -40, 120, 60)

  res <- place_dag_labels(labels, nodes, edges, bounds)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("id", "x", "y", "anchor", "score"))
  expect_identical(nrow(res), 3L)
  expect_identical(res$id, labels$id)
  expect_type(res$id, "character")
  expect_type(res$x, "double")
  expect_type(res$y, "double")
  expect_type(res$anchor, "character")
  expect_type(res$score, "double")
  expect_true(all(res$anchor %in% anchor_vocabulary))
  expect_true(all(is.finite(res$score)))
  expect_true(all(res$score >= 0))
})

test_that("placement is deterministic and uses no RNG", {
  labels <- data.frame(
    id = c("x", "m", "y"),
    x = c(0, 40, 80),
    y = c(0, 20, 0),
    width = 18,
    height = 8
  )
  nodes <- data.frame(x = c(0, 40, 80), y = c(0, 20, 0), radius = 4)
  edges <- rbind(
    straight_edge("xm", 0, 0, 40, 20),
    straight_edge("my", 40, 20, 80, 0),
    straight_edge("xy", 0, 0, 80, 0)
  )
  bounds <- c(-40, -40, 120, 60)

  withr::local_seed(1234)
  seed_before <- get(".Random.seed", envir = globalenv())

  res1 <- place_dag_labels(labels, nodes, edges, bounds)
  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)

  res2 <- place_dag_labels(labels, nodes, edges, bounds)
  expect_identical(res1, res2)

  expect_false("seed" %in% names(formals(place_dag_labels)))
})

test_that("a dense fixture degrades to least-bad placements without error", {
  # Four nodes on a 10 mm square, each with a 30 x 20 mm label, inside a
  # 34 x 34 mm panel: the four boxes cannot fit anywhere without violations
  # (total label area 2400 mm^2 against a 1156 mm^2 panel). Every label must
  # still get a finite, deterministic, least-bad row.
  labels <- data.frame(
    id = c("a", "b", "c", "d"),
    x = c(0, 10, 0, 10),
    y = c(0, 0, 10, 10),
    width = 30,
    height = 20
  )
  nodes <- data.frame(
    x = c(0, 10, 0, 10),
    y = c(0, 0, 10, 10),
    radius = 4
  )
  bounds <- c(-12, -12, 22, 22)

  res <- expect_silent(place_dag_labels(labels, nodes, no_edges, bounds))

  expect_identical(nrow(res), 4L)
  expect_identical(res$id, labels$id)
  expect_true(all(is.finite(res$x)))
  expect_true(all(is.finite(res$y)))
  expect_true(all(res$anchor %in% anchor_vocabulary))
  expect_true(all(is.finite(res$score)))
  expect_true(all(res$score > 0))

  res_again <- place_dag_labels(labels, nodes, no_edges, bounds)
  expect_identical(res, res_again)
})

test_that("duplicate label ids are a type error", {
  labels <- data.frame(
    id = c("a", "a"),
    x = c(0, 30),
    y = 0,
    width = 18,
    height = 8
  )
  nodes <- data.frame(x = c(0, 30), y = 0, radius = 4)
  bounds <- c(-40, -40, 70, 40)

  expect_error(
    place_dag_labels(labels, nodes, no_edges, bounds),
    class = "ggdag_type_error"
  )
})

test_that("label dimensions must be positive and finite", {
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-40, -40, 40, 40)
  base <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)

  bad_width_zero <- transform(base, width = 0)
  bad_width_na <- transform(base, width = NA_real_)
  bad_height_negative <- transform(base, height = -1)
  bad_height_infinite <- transform(base, height = Inf)

  expect_error(
    place_dag_labels(bad_width_zero, nodes, no_edges, bounds),
    class = "ggdag_type_error"
  )
  expect_error(
    place_dag_labels(bad_width_na, nodes, no_edges, bounds),
    class = "ggdag_type_error"
  )
  expect_error(
    place_dag_labels(bad_height_negative, nodes, no_edges, bounds),
    class = "ggdag_type_error"
  )
  expect_error(
    place_dag_labels(bad_height_infinite, nodes, no_edges, bounds),
    class = "ggdag_type_error"
  )
})

test_that("node radii must be positive and finite", {
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  bounds <- c(-40, -40, 40, 40)

  expect_error(
    place_dag_labels(
      labels,
      data.frame(x = 0, y = 0, radius = 0),
      no_edges,
      bounds
    ),
    class = "ggdag_type_error"
  )
  expect_error(
    place_dag_labels(
      labels,
      data.frame(x = c(0, 10), y = 0, radius = c(4, -2)),
      no_edges,
      bounds
    ),
    class = "ggdag_type_error"
  )
})

test_that("each edge polyline needs at least two points", {
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-40, -40, 40, 40)

  edges <- rbind(
    straight_edge("ok", -30, -30, -10, -30),
    data.frame(edge_id = "short", x = 20, y = 20)
  )

  expect_error(
    place_dag_labels(labels, nodes, edges, bounds),
    class = "ggdag_type_error"
  )
})

test_that("bounds must be ordered", {
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 0, radius = 4)

  expect_error(
    place_dag_labels(labels, nodes, no_edges, c(40, -40, -40, 40)),
    class = "ggdag_type_error"
  )
  expect_error(
    place_dag_labels(labels, nodes, no_edges, c(-40, 10, 40, 10)),
    class = "ggdag_type_error"
  )
})
