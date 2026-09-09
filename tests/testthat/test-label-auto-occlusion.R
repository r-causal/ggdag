# The occlusion tier of place_dag_labels(): a label box on the drawn edges.
#
# A box whose only fault is that it covers the mid-run of drawn edges, with no
# node disc penetrated, no arrowhead zone touched, no other label box
# overlapped, and nothing outside the panel, is admissible. It sits in a tier
# below every clear spot within `label_occlusion_reach` times `reach` of its
# own disc and above every clear spot beyond that, so a label whose near field
# is full of ink stays beside its node instead of flying to the panel border,
# while a clear spot a reader would still read as beside the node keeps
# winning. Among such boxes the price is the leader's own price for the same
# ink, `label_leader_ink_cost` millimetres per sampled ink point within the
# box's margin, so a box crossing one edge beats a box lying along it and one
# edge beats two. The rule is off when `reach` is infinite.
#
# Three kinds of test live here, all in millimetres:
#
# * candidate scoring, on `score_label_candidates()` directly, where the tier,
#   its bands, and its price are decided;
# * whole placements of synthetic scenes, built like `far_field_scene()` in
#   test-label-auto-ownership.R: a node fenced by edge polylines so that the
#   engine's choice is forced by geometry the block writes out;
# * drawn placements of the 30-node scene of helper-label-perf.R, which is
#   where the rule was calibrated.
#
# No snapshot is written here. The scenes that must not move are pinned
# elsewhere: the ten-node, dense, and saturated placements at 7 x 5 against
# fixtures/label-placements-7x5.rds in test-label-auto-ownership.R and
# test-label-auto-perf.R, and the engine's own fixture scenes at infinite
# reach in test-label-auto-ownership.R.

# Helpers ----------------------------------------------------------------------

# These mirror the helpers of test-label-auto-ownership.R, which each label
# test file keeps for itself.

# The box one placed label sits on.
occlusion_box <- function(result, labels, id = labels$id[[1]]) {
  i <- match(id, result$id)
  j <- match(id, labels$id)
  c(
    xmin = result$x[i] - labels$width[j] / 2,
    ymin = result$y[i] - labels$height[j] / 2,
    xmax = result$x[i] + labels$width[j] / 2,
    ymax = result$y[i] + labels$height[j] / 2
  )
}

# Clearance in mm from a box to the disc of node row `k`.
occlusion_clearance <- function(box, nodes, k = 1L) {
  rect_point_dist(
    box[["xmin"]],
    box[["ymin"]],
    box[["xmax"]],
    box[["ymax"]],
    nodes$x[k],
    nodes$y[k]
  ) -
    nodes$radius[k]
}

# The distance from a box to the nearest sampled edge point. A box on ink is
# nearer than `label_edge_clearance`; a clear box is at least that far off.
occlusion_ink_distance <- function(box, edges) {
  if (nrow(edges) == 0) {
    return(Inf)
  }
  min(rect_point_dist(
    box[["xmin"]],
    box[["ymin"]],
    box[["xmax"]],
    box[["ymax"]],
    edges$x,
    edges$y
  ))
}

# The engine defaults with `overrides` applied.
occlusion_weights <- function(...) {
  weights <- eval(formals(place_dag_labels)$weights)
  overrides <- c(...)
  weights[names(overrides)] <- overrides
  weights
}

# Candidate boxes of one size at the centers given, all at the same
# preference rank so that nothing but the obstacles separates their scores.
occlusion_candidates <- function(x, y, width = 8, height = 4) {
  list(
    anchor = rep("n", length(x)),
    rank = rep(0, length(x)),
    x = x,
    y = y,
    xmin = x - width / 2,
    ymin = y - height / 2,
    xmax = x + width / 2,
    ymax = y + height / 2
  )
}

# A polyline of sampled points, `near` over the span the candidate boxes sit
# on and `far` continuing well past them, so that the run's arrowhead zone,
# its last `label_arrow_zone` millimetres, is nowhere near any of them. An
# edge whose whole length is beside a box would put that box in an arrowhead
# zone, which is a different rule from the one under test.
occlusion_run <- function(id, near, far, at, vertical = FALSE) {
  points <- c(near, far)
  if (vertical) {
    data.frame(edge_id = id, x = at, y = points, stringsAsFactors = FALSE)
  } else {
    data.frame(edge_id = id, x = points, y = at, stringsAsFactors = FALSE)
  }
}

# `score_label_candidates()` with the engine defaults, the label's own node
# taken as the first row of `nodes`.
occlusion_scores <- function(
  cand,
  nodes,
  edges,
  bounds,
  reach = 9,
  leader = 5
) {
  score_label_candidates(
    cand,
    nodes,
    label_ink_points(edges),
    bounds,
    occlusion_weights(),
    own = list(x = nodes$x[[1]], y = nodes$y[[1]], radius = nodes$radius[[1]]),
    reach = reach,
    leader = leader
  )
}

# Scenes -----------------------------------------------------------------------

# A corridor 30 mm wide, hatched by horizontal edge runs 4 mm apart from
# y = -52 to y = 52 and sampled every 2 mm from x = -30 to x = 30, so that
# each run's arrowhead zone lies well outside the panel. No 8 x 4 box fits
# clear of the hatch anywhere inside it, and the box height is the hatch
# spacing, so every box position in the hatched band covers ink. The panel
# runs from y = -60 to y = 66, which leaves clear room at either end: the
# nearest clear box center is at y = 56, whose clearance from the disc at the
# origin is 50 mm, past the 45 mm the tier gate sits at for a reach of 9.
#
# `drop` removes whole hatch rows, opening a clear pocket across the
# corridor.
occlusion_hatch <- function(drop = numeric()) {
  rows <- setdiff(seq(-52, 52, by = 4), drop)
  x <- seq(-30, 30, by = 2)
  data.frame(
    edge_id = rep(paste0("hatch", seq_along(rows)), each = length(x)),
    x = rep(x, times = length(rows)),
    y = rep(rows, each = length(x)),
    stringsAsFactors = FALSE
  )
}

occlusion_corridor <- function(edges = occlusion_hatch()) {
  list(
    labels = data.frame(id = "a", x = 0, y = 0, width = 8, height = 4),
    nodes = data.frame(x = 0, y = 0, radius = 4),
    edges = edges,
    bounds = c(-15, -60, 15, 66)
  )
}

place_occlusion_corridor <- function(scene, reach = 9, leader = 5) {
  place_dag_labels(
    scene$labels,
    scene$nodes,
    scene$edges,
    scene$bounds,
    gap = 2,
    n_rings = 1L,
    reach = reach,
    leader = leader
  )
}

# The Occupation cascade in miniature. Two nodes in a corridor hatched the
# same way, with one hatch-free window big enough for a single box: rows 8
# and 12 are broken over `|x| <= 8`, so a clear 8 x 6 box needs its center
# within `|x| < 3.5` and `8 < y < 12`, and two clear boxes cannot both fit.
#
# Node `a` at the origin owns the window: a box there clears its disc by 3 mm
# and needs no leader. Node `b`, 30 mm north, is 13 mm from the window and
# has nothing else clear within the 27 mm its rays and local grid reach, so
# at assignment its choice is between a box on ink and a box on top of `a`'s.
# The label weight is lowered to 5 so that the overlap is the cheaper of the
# two, which is the position Child nutrition was in on the gallery scene.
# The engine then repairs `a` first, in placement order, and sends it to the
# panel border with `b` still on its window: a 78 mm leader for a label that
# had a 3 mm spot.
occlusion_cascade_scene <- function() {
  rows <- seq(-80, 80, by = 4)
  x <- seq(-30, 30, by = 0.5)
  edges <- do.call(
    rbind,
    lapply(seq_along(rows), function(i) {
      keep <- if (rows[i] %in% c(8, 12)) abs(x) > 8 else rep(TRUE, length(x))
      data.frame(
        edge_id = paste0("hatch", i),
        x = x[keep],
        y = rows[i],
        stringsAsFactors = FALSE
      )
    })
  )
  list(
    labels = data.frame(
      id = c("a", "b"),
      x = c(0, 0),
      y = c(0, 30),
      width = c(8, 8),
      height = c(6, 6),
      stringsAsFactors = FALSE
    ),
    nodes = data.frame(x = c(0, 0), y = c(0, 30), radius = c(4, 4)),
    edges = edges,
    bounds = c(-15, -90, 15, 96)
  )
}

# The 30-node life-course scene of helper-label-perf.R with its multi-word
# labels wrapped onto two lines, which is how the gallery draws it and how
# the tier was calibrated. Wrapping changes every box shape, so this is a
# different placement problem from the unwrapped scene the placement fixture
# carries, and the two are measured separately below.
occlusion_very_big_dag <- function() {
  dag <- perf_very_big_dag()
  label(dag) <- sub(" ", "\n", label(dag), fixed = TRUE)
  dag
}

# One row per label of a captured render: the box the engine placed, its
# clearance from its own node's disc, and the leader that clearance is drawn
# as, 0 where the box is inside `min.segment.length`. `perf_placement()`
# emits boxes in label order, which is the order the engine's inputs carry.
occlusion_leaders <- function(capture) {
  labels <- capture$inputs$labels
  boxes <- capture$placement$boxes
  radius <- nearest_node_radius(labels$x, labels$y, capture$inputs$nodes)
  clearance <- rect_point_dist(
    boxes$x - labels$width / 2,
    boxes$y - labels$height / 2,
    boxes$x + labels$width / 2,
    boxes$y + labels$height / 2,
    labels$x,
    labels$y
  ) -
    radius
  data.frame(
    label = gsub("\n", " ", boxes$label, fixed = TRUE),
    clearance = clearance,
    leader = ifelse(clearance > capture$inputs$leader, clearance, 0),
    stringsAsFactors = FALSE
  )
}

# The scenes below are rendered once each and shared by the blocks that read
# them: the 30-node scene costs a couple of seconds a render.
occlusion_capture_cache <- new.env(parent = emptyenv())

occlusion_capture <- function(name, dag, route, size) {
  key <- paste(name, route, size[[1]], size[[2]])
  if (is.null(occlusion_capture_cache[[key]])) {
    occlusion_capture_cache[[key]] <- without_occlusion_warning(
      perf_dag_capture(dag(), route, size)
    )
  }
  occlusion_capture_cache[[key]]
}

# Measuring a placement is not pinning the warning, so the blocks that
# measure one drop the condition on the floor.
without_occlusion_warning <- function(expr) {
  withCallingHandlers(
    expr,
    ggdag_label_unresolved_warning = function(cnd) rlang::cnd_muffle(cnd)
  )
}

# The `ggdag_label_unresolved_warning` conditions signalled while `expr` runs,
# muffled so that R does not collapse repeats of one message into one report.
occlusion_warnings <- function(expr) {
  seen <- list()
  withCallingHandlers(
    expr,
    ggdag_label_unresolved_warning = function(cnd) {
      seen[[length(seen) + 1]] <<- cnd
      rlang::cnd_muffle(cnd)
    }
  )
  seen
}

# The tier ---------------------------------------------------------------------

test_that("a box on the mid-run of an edge beats a flight past five reaches", {
  scene <- occlusion_corridor()
  res <- place_occlusion_corridor(scene)
  box <- occlusion_box(res, scene$labels)

  # The hatch leaves nothing clear inside 45 mm, so the only clear spots are
  # the corridor's ends, 50 mm out and more. The label stays beside its node
  # instead, on a box that covers the mid-run of one hatch row, and a box
  # this close to its disc is inside `min.segment.length`, so it is drawn
  # with no leader at all.
  clearance <- occlusion_clearance(box, scene$nodes)
  expect_lt(clearance, 5)
  expect_gte(clearance, 2)
  expect_lt(occlusion_ink_distance(box, scene$edges), label_edge_clearance)

  expect_equal(res$x, -1, tolerance = 1e-9)
  expect_equal(res$y, -8, tolerance = 1e-9)
})

test_that("a clear spot within five reaches still beats a box on an edge", {
  # Three hatch rows removed open a clear pocket 13 mm north of the disc,
  # well inside the 45 mm gate. The tier is below every clear spot there, so
  # the pocket wins and the box comes down off the ink.
  scene <- occlusion_corridor(occlusion_hatch(drop = c(20, 24, 28)))
  res <- place_occlusion_corridor(scene)
  box <- occlusion_box(res, scene$labels)

  clearance <- occlusion_clearance(box, scene$nodes)
  expect_gt(clearance, 5)
  expect_lt(clearance, 45)
  expect_gte(occlusion_ink_distance(box, scene$edges), label_edge_clearance)

  expect_equal(res$x, 3.733435819839, tolerance = 1e-9)
  expect_equal(res$y, 19.2037511837381, tolerance = 1e-9)
})

test_that("the occlusion tier is off at an infinite reach", {
  # With no finite reach there is no band to gate on and no far field to
  # search, so the engine keeps the placement it has always given: the least
  # bad of eight anchors, all of them on the hatch. This is the guard for the
  # engine's own fixture scenes, which run at infinite reach.
  scene <- occlusion_corridor()
  res <- place_occlusion_corridor(scene, reach = Inf, leader = Inf)
  box <- occlusion_box(res, scene$labels)

  expect_identical(res$anchor, "n")
  expect_identical(res$x, 0)
  expect_identical(res$y, 8)
  expect_lt(occlusion_ink_distance(box, scene$edges), label_edge_clearance)
})

# Hard exclusions --------------------------------------------------------------

test_that("only the mid-run of an edge is forgiven a candidate box", {
  # One node at the origin with a 4 mm disc, a foreign 2 mm disc 24 mm east,
  # and three runs of ink: one across the near field at y = 9, one whose
  # arrowhead zone lies at (-10, -9), and one 60 mm south. Six candidate
  # boxes, one per fault:
  #
  #   (  0,  11)  on the mid-run of one edge, 5 mm clear of its own disc
  #   ( 24,   0)  over the foreign disc
  #   (-10, -11)  in an arrowhead zone
  #   ( 78,  20)  spilling the panel
  #   (  0,  60)  clear, 54 mm out, past five reaches
  #   (  0, -62)  on the mid-run of an edge, 56 mm out, past five reaches
  nodes <- data.frame(x = c(0, 24), y = c(0, 0), radius = c(4, 2))
  bounds <- c(-80, -80, 80, 80)
  edges <- rbind(
    occlusion_run("mid", seq(-6, 6, by = 0.5), seq(12, 24, by = 0.5), at = 9),
    occlusion_run("head", seq(-16, -12, by = 0.5), numeric(), at = -9),
    occlusion_run("far", seq(-6, 6, by = 0.5), seq(12, 24, by = 0.5), at = -60)
  )
  cand <- occlusion_candidates(
    x = c(0, 24, -10, 78, 0, 0),
    y = c(11, 0, -11, 20, 60, -62)
  )
  scored <- occlusion_scores(cand, nodes, edges, bounds)

  # A disc, an arrowhead zone, and the panel border stay hard whatever else
  # the near field holds; only edge points are forgiven.
  expect_identical(
    scored$violating,
    c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  # Band 3 is the tier: below the clear bands 0, 1, and 2 and above the
  # clear band 4 that a spot past five reaches is raised to. An occluding
  # spot past five reaches is raised the same two bands, to 5.
  expect_equal(scored$band[[1]], 3)
  expect_equal(scored$band[[5]], 4)
  expect_equal(scored$band[[6]], 5)
})

# The price --------------------------------------------------------------------

test_that("an occluding candidate is priced per ink point, not per edge", {
  # Four candidate boxes at one preference rank, each on the mid-run of ink
  # and none of them near a disc, an arrowhead, or the border:
  #
  #   (  0,  11)  one run sampled every 0.5 mm  19 points, 1 edge
  #   (  0, -11)  one run sampled every 2 mm     5 points, 1 edge
  #   ( 12,   0)  one run                        9 points, 1 edge
  #   (-12,   0)  two runs                       9 points, 2 edges
  #
  # The first pair sits at one distance from the node and the second at
  # another, so within each pair nothing but the ink separates the scores.
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-80, -80, 80, 80)
  edges <- rbind(
    occlusion_run("dense", seq(-6, 6, by = 0.5), seq(12, 24, by = 0.5), at = 9),
    occlusion_run("sparse", seq(-4, 4, by = 2), seq(12, 24, by = 0.5), at = -9),
    occlusion_run(
      "east",
      seq(-2, 2, by = 0.5),
      seq(10, 22, by = 0.5),
      at = 12,
      vertical = TRUE
    ),
    occlusion_run(
      "west1",
      seq(-2, 2, by = 1),
      seq(10, 22, by = 0.5),
      at = -10,
      vertical = TRUE
    ),
    occlusion_run(
      "west2",
      seq(-1.5, 1.5, by = 1),
      seq(10, 22, by = 0.5),
      at = -14,
      vertical = TRUE
    )
  )
  cand <- occlusion_candidates(x = c(0, 0, 12, -12), y = c(11, -11, 0, 0))
  scored <- occlusion_scores(cand, nodes, edges, bounds)
  hits <- ink_box_hits(
    cand$xmin,
    cand$ymin,
    cand$xmax,
    cand$ymax,
    label_ink_points(edges)
  )

  # All four are in the tier, so their scores are comparable.
  expect_false(any(scored$violating))
  expect_equal(scored$band, rep(3, 4))

  # The scene is the one the block describes.
  expect_identical(hits$edge, c(19L, 5L, 9L, 9L))
  expect_identical(hits$arrow, integer(4))

  # A box lying along a densely sampled edge pays for every point under it,
  # so a box crossing an edge beats it, by the leader's own price for that
  # ink through the `dist` weight.
  expect_lt(scored$within[[2]], scored$within[[1]])
  expect_equal(
    scored$within[[1]] - scored$within[[2]],
    occlusion_weights()[["dist"]] *
      label_leader_ink_cost *
      (hits$edge[[1]] - hits$edge[[2]])
  )

  # The price counts points, not edges: two runs under one box cost what one
  # run of the same sampling costs.
  expect_equal(scored$within[[3]], scored$within[[4]])
})

# Repair -----------------------------------------------------------------------

test_that("a label on a clear spot is not ejected by a later label", {
  scene <- occlusion_cascade_scene()
  res <- place_dag_labels(
    scene$labels,
    scene$nodes,
    scene$edges,
    scene$bounds,
    gap = 2,
    n_rings = 1L,
    weights = occlusion_weights(label = 5),
    reach = 9,
    leader = 5
  )

  # `a` keeps the window: a box clear of the ink, beside its own node, with
  # no leader at all, rather than the panel border 78 mm away.
  a <- occlusion_box(res, scene$labels, "a")
  a_clearance <- occlusion_clearance(a, scene$nodes, 1L)
  expect_lt(a_clearance, 5)
  expect_gte(occlusion_ink_distance(a, scene$edges), label_edge_clearance)

  # `b` takes the tier beside its own node rather than the one spot `a` is
  # on, so nothing is evicted and nothing flies.
  b <- occlusion_box(res, scene$labels, "b")
  b_clearance <- occlusion_clearance(b, scene$nodes, 2L)
  expect_lt(b_clearance, 9)
  expect_lt(occlusion_ink_distance(b, scene$edges), label_edge_clearance)

  # A box in the tier overlaps no other box, here or anywhere.
  expect_equal(
    rect_overlap_area(
      a[["xmin"]],
      a[["ymin"]],
      a[["xmax"]],
      a[["ymax"]],
      b[["xmin"]],
      b[["ymin"]],
      b[["xmax"]],
      b[["ymax"]]
    ),
    0
  )

  expect_equal(res$x, c(0, 0), tolerance = 1e-9)
  expect_equal(res$y, c(9, 40), tolerance = 1e-9)
})

# The 30-node scene ------------------------------------------------------------

test_that("the wrapped 30-node scene keeps its labels near their nodes", {
  skip_on_cran()
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  skip_on_os(c("windows", "linux", "solaris"))

  measured <- occlusion_leaders(occlusion_capture(
    "very_big_wrapped",
    occlusion_very_big_dag,
    "spline",
    c(7, 5)
  ))
  leaders <- measured$leader[measured$leader > 0]

  # The near field of this scene at 7 x 5 is genuinely full: 30 labels on a
  # 171 x 120 mm panel, with the discs 14.5 mm apart across and 12.5 mm apart
  # down. What the tier buys is that no label crosses the picture to find
  # room. The measured placement is a 35.7 mm longest leader and a 19.1 mm
  # mean; the pins leave room around both.
  expect_lt(max(leaders), 40)
  expect_lt(abs(mean(leaders) - 19.1), 3)

  # Occupation is the label the cascade sent 73 mm to the bottom-left border
  # while a 9 mm spot beside its own node stood empty.
  expect_lt(measured$leader[measured$label == "Occupation"], 12)
})

test_that("the wrapped 30-node scene keeps every box off the ink at 10 x 6", {
  skip_on_cran()
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  skip_on_os(c("windows", "linux", "solaris"))

  # The tier gate is five reaches, not three, so that this scene keeps the
  # clear placement it already had wherever the panel has room for one. A
  # label reaches the tier only when its near field leaves it nothing else.
  capture <- occlusion_capture(
    "very_big_wrapped",
    occlusion_very_big_dag,
    "spline",
    c(10, 6)
  )
  expect_identical(capture$placement$unresolved, character(0))

  measured <- occlusion_leaders(capture)
  expect_lt(max(measured$leader), 30)
})

test_that("the fixture 30-node scene places no label far from its node", {
  skip_on_cran()
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  skip_on_os(c("windows", "linux", "solaris"))

  # The unwrapped scene the placement fixture carries. Its `very_big`
  # entries are the ones the tier moves, and these are the properties the
  # regenerated fixture has to satisfy: test-label-auto-perf.R pins the
  # fixture against a fresh render of this same scene to 1e-9, so a fixture
  # that failed them would fail there too.
  #
  # The rule's own bound on a leader is five reaches plus half the box
  # diagonal, about 57 mm here; measured, the longest leader is 44.8 mm under
  # spline and 44.5 mm under straight, against 73.8 and 56.5 before.
  for (route in c("spline", "straight")) {
    capture <- occlusion_capture(
      "very_big",
      perf_very_big_dag,
      route,
      c(7, 5)
    )
    measured <- occlusion_leaders(capture)
    expect_lt(
      max(measured$leader),
      50,
      label = paste(route, "longest leader")
    )
    # One label of this scene comes to rest on the ink under either route,
    # and it is reported as unresolved the way any other box on ink is.
    expect_identical(
      capture$placement$unresolved,
      "Heart disease",
      label = paste(route, "unresolved labels")
    )
  }
})

test_that("a draw names the labels the engine placed on the ink", {
  skip_on_cran()
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  skip_on_os(c("windows", "linux", "solaris"))

  # A box in the tier is still a box on the drawing, so it is still
  # unresolved and still named: one warning per draw, of the same class the
  # rest of the overlap policy uses.
  warnings <- occlusion_warnings(perf_dag_capture(
    occlusion_very_big_dag(),
    "spline",
    c(7, 5)
  ))
  expect_length(warnings, 1)
  expect_s3_class(warnings[[1]], "ggdag_label_unresolved_warning")

  message <- conditionMessage(warnings[[1]])
  for (name in c("Adversity", "Alcohol", "Depression")) {
    expect_true(grepl(name, message, fixed = TRUE), label = name)
  }
})

# Placements that must not move -------------------------------------------------

# The ten-node, dense, and saturated scenes are byte-identical under the tier
# at 7 x 5 under both routes, and test-label-auto-ownership.R pins all three
# against fixtures/label-placements-7x5.rds. The block below is the same
# guard at the size that fixture does not carry. The leader lengths follow the
# drawn edges, so a change to the routing moves them; what must not move them
# is the label engine.

test_that("the saturated scene is unchanged at 10 x 6", {
  skip_on_cran()
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  skip_on_os(c("windows", "linux", "solaris"))

  expected <- list(
    spline = list(leaders = 3L, longest = 20.699102873913),
    straight = list(leaders = 1L, longest = 6.0474966524655)
  )

  for (route in names(expected)) {
    capture <- occlusion_capture(
      "saturated",
      perf_saturated_dag,
      route,
      c(10, 6)
    )
    measured <- occlusion_leaders(capture)
    leaders <- measured$leader[measured$leader > 0]

    expect_identical(
      capture$placement$unresolved,
      character(0),
      label = paste(route, "unresolved labels")
    )
    expect_length(leaders, expected[[route]]$leaders)
    expect_equal(
      max(leaders),
      expected[[route]]$longest,
      tolerance = 1e-9,
      label = paste(route, "longest leader")
    )
  }
})
