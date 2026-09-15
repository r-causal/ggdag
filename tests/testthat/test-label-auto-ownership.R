# Ownership, the local far field, and leader pricing in place_dag_labels().
#
# Three engine rules are pinned here, each on a scene built so that exactly one
# of them decides the placement. All coordinates are in millimetres.
#
# * Ownership. A label box beside the wrong node disc reads as that node's
#   label. Every candidate pays `max(0, min(d_own, reach) - d_other)` at the
#   `own` weight, where `d_own` is the clearance from the label's own disc and
#   `d_other` the clearance from the nearest other disc, and a candidate that
#   would be drawn without a leader (`d_own` no greater than `leader`) and sits
#   nearer a foreign disc than its own is raised one proximity band, so it can
#   lose to an owned spot that needs a leader. Both parts are off when `own` is
#   0 and when `reach` is infinite.
# * The local far field. A label with no admissible candidate within `reach`
#   gains candidates on a 3 mm grid around its own node out to 3 times `reach`,
#   so it can settle in a pocket the sparse ray levels beyond the reach step
#   straight over. The panel-wide repair grid stays a repair device.
# * Leader pricing. The extra length a leader is priced at now includes the
#   leader's own length, and an ink point within its margin costs 4 mm rather
#   than 2, so a long leader over open space can beat a short one over ink.
#
# The scenes below use `n_angles` and `n_rays` to hold the candidate set to a
# handful of spots whose geometry is written out in each block: with `n_rays`
# at 2 the two rays point east and west, so a scene whose anchors are the
# diagonals gets no off-grid candidates at all and the anchor rings are the
# whole vocabulary.

# Helpers ----------------------------------------------------------------------

no_edges <- data.frame(edge_id = character(), x = numeric(), y = numeric())

# The engine defaults with `overrides` applied, so a scene that varies one
# weight still runs under every other default.
engine_weights <- function(...) {
  weights <- eval(formals(place_dag_labels)$weights)
  overrides <- c(...)
  weights[names(overrides)] <- overrides
  weights
}

# The box one placed label sits on.
placed_box <- function(result, labels, id = labels$id[[1]]) {
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
box_clearance <- function(box, nodes, k) {
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

# How far `value` sits from the nearest multiple of `spacing`.
off_grid <- function(value, spacing) {
  remainder <- value %% spacing
  min(remainder, spacing - remainder)
}

# The distance from a box to the nearest sampled edge point.
box_ink_distance <- function(box, edges) {
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

# Ownership --------------------------------------------------------------------

# Node `a` at the origin with a 4 mm disc, its label an 8 x 4 box, and a 1 mm
# disc `b` off to the east at (16, 3.5). With two anchors, two rings and no
# off-grid candidates the label has exactly four spots:
#
#   ne ring 1  ( 10.01,  8.01)  clearance 4.50 from a, 2.20 from b
#   nw ring 1  (-10.01,  8.01)  blocked by the fence west of the node
#   ne ring 2  ( 13.17, 11.17)  clearance 8.97 from a, 4.67 from b
#   nw ring 2  (-13.17, 11.17)  clearance 8.97 from a, 24.80 from b
#
# The ne ring 1 box is the one the engine takes today: it is the only
# admissible spot inside `leader`, and a nearer band wins whatever else the
# score says. It is also 2.3 mm nearer b's disc than a's, which is what
# ownership demotes; once it is demoted the three spots are all in the same
# band and nw ring 2, owned and clear, wins on the score.
ownership_scene <- function() {
  list(
    labels = data.frame(id = "a", x = 0, y = 0, width = 8, height = 4),
    nodes = data.frame(x = c(0, 16), y = c(0, 3.5), radius = c(4, 1)),
    # A short fence 5.2 mm west of the node, close enough to the nw ring 1 box
    # to block it and far enough from the nw ring 2 box and from its leader to
    # leave both alone.
    edges = data.frame(edge_id = "fence", x = -5.2, y = seq(7, 12, by = 0.5)),
    bounds = c(-40, -30, 40, 30)
  )
}

place_ownership_scene <- function(scene, weights = engine_weights()) {
  place_dag_labels(
    scene$labels,
    scene$nodes,
    scene$edges,
    scene$bounds,
    gap = 4.5,
    n_angles = 2L,
    n_rings = 2L,
    n_rays = 2L,
    weights = weights,
    reach = 20,
    leader = 5
  )
}

test_that("a leaderless candidate nearer a foreign disc is demoted a band", {
  scene <- ownership_scene()
  res <- place_ownership_scene(scene)

  expect_identical(res$anchor, "nw")
  expect_equal(res$x, -13.172685300254, tolerance = 1e-9)
  expect_equal(res$y, 11.172685300254, tolerance = 1e-9)

  # The spot the label moves to is owned and takes a leader, which is the
  # trade the demotion is there to make.
  box <- placed_box(res, scene$labels)
  own <- box_clearance(box, scene$nodes, 1)
  expect_gt(own, 5)
  expect_gt(box_clearance(box, scene$nodes, 2), own)
})

test_that("the ownership rule is off at an `own` weight of zero", {
  scene <- ownership_scene()
  res <- place_ownership_scene(scene, engine_weights(own = 0))

  # The undemoted placement: ne ring 1, the only admissible spot inside
  # `leader`, 2.2 mm from b's disc and 4.5 mm from its own.
  expect_identical(res$anchor, "ne")
  expect_equal(res$x, 10.0104076400857, tolerance = 1e-9)
  expect_equal(res$y, 8.01040764008565, tolerance = 1e-9)
})

# Node `a` at the origin again, with three 1 mm discs placed so that the two
# candidate boxes penetrate the same total depth into the 8 mm soft zones but
# stand at different distances from their nearest foreign disc:
#
#   ne  (11.78, 9.78)  one disc 4 mm away        soft 4, own penalty 3
#   nw (-11.78, 9.78)  two discs 6 mm away each  soft 4, own penalty 1
#
# Both spots are the same distance from a, both take a leader of the same
# length, and neither has any ink to cross, so today the two scores differ only
# by the preference rank and the engine takes ne. The continuous ownership term
# reads the nearest foreign disc rather than the sum, so it separates them.
soft_zone_scene <- function() {
  list(
    labels = data.frame(id = "a", x = 0, y = 0, width = 8, height = 4),
    nodes = data.frame(
      x = c(0, 20.778, -22.778, -11.778),
      y = c(0, 9.778, 9.778, 18.778),
      radius = c(4, 1, 1, 1)
    ),
    bounds = c(-40, -30, 40, 30)
  )
}

test_that("the ownership term reads the nearest foreign disc, not the sum", {
  scene <- soft_zone_scene()
  res <- place_dag_labels(
    scene$labels,
    scene$nodes,
    no_edges,
    scene$bounds,
    gap = 7,
    n_angles = 2L,
    n_rings = 1L,
    n_rays = 2L,
    reach = 9,
    leader = 5
  )

  expect_identical(res$anchor, "nw")
  expect_equal(res$x, -11.7781745930520, tolerance = 1e-9)
  expect_equal(res$y, 9.77817459305202, tolerance = 1e-9)
})

# The local far field ----------------------------------------------------------

# Node `a` at the origin with a 4 mm disc and an 8 x 5 label, fenced by an arc
# of ink at radius 12.3 mm that runs all the way round except for a 20 degree
# window due north. The arc is placed so that no box clearing the node by the
# 2 mm `gap` fits inside it and every box outside it clears the node by at
# least 9.3 mm, so every candidate within the 9 mm `reach` violates and the
# label's best admissible band is the far one.
#
# Beyond the reach the rays only offer levels at 1.5, 2 and 3 times it, so the
# nearest spot the engine can name today is 13.5 mm out. The north window
# leaves room for a box 11.5 mm out, which only a grid around the node finds.
far_field_scene <- function(fenced = TRUE) {
  angle <- seq(100, 440, by = 2) * pi / 180
  list(
    labels = data.frame(id = "a", x = 0, y = 0, width = 8, height = 5),
    nodes = data.frame(x = 0, y = 0, radius = 4),
    edges = if (fenced) {
      data.frame(
        edge_id = "arc",
        x = 12.3 * cos(angle),
        y = 12.3 * sin(angle)
      )
    } else {
      no_edges
    },
    bounds = c(-60, -60, 60, 60)
  )
}

place_far_field_scene <- function(scene) {
  place_dag_labels(
    scene$labels,
    scene$nodes,
    scene$edges,
    scene$bounds,
    gap = 2,
    n_rings = 1L,
    reach = 9,
    leader = 5
  )
}

test_that("a label with nothing within reach gains a local grid", {
  scene <- far_field_scene()
  res <- place_far_field_scene(scene)
  box <- placed_box(res, scene$labels)

  # Inside 3 times the reach and nearer than the 13.5 mm ray level, so the
  # leader is short enough to read.
  clearance <- box_clearance(box, scene$nodes, 1)
  expect_gte(clearance, 2)
  expect_lt(clearance, 12)
  expect_lt(clearance, 3 * 9)

  # On the 3 mm grid the local candidates are built on, which is phased from
  # the near edge of the square the search covers rather than from the node:
  # `seq(x - half, x + half, by = 3)` on each axis, where `half` is the
  # farthest a box center can sit and still clear the node by no more than 3
  # times the reach.
  half <- scene$nodes$radius[[1]] +
    3 * 9 +
    sqrt(scene$labels$width[[1]]^2 + scene$labels$height[[1]]^2) / 2
  expect_lt(off_grid(res$x - (scene$nodes$x[[1]] - half), 3), 1e-9)
  expect_lt(off_grid(res$y - (scene$nodes$y[[1]] - half), 3), 1e-9)

  # Beside the node, not out on the panel border where the repair grid puts a
  # label it cannot otherwise place, and clear of the fence.
  bounds <- scene$bounds
  expect_gt(box[["xmin"]] - bounds[[1]], 10)
  expect_gt(box[["ymin"]] - bounds[[2]], 10)
  expect_gt(bounds[[3]] - box[["xmax"]], 10)
  expect_gt(bounds[[4]] - box[["ymax"]], 10)
  expect_gte(box_ink_distance(box, scene$edges), 1)
})

test_that("a label with a spot within reach keeps its anchor", {
  scene <- far_field_scene(fenced = FALSE)
  res <- place_far_field_scene(scene)

  # Unfenced, the label sits at the N anchor of ring 1, and no local grid is
  # built for it.
  expect_identical(res$anchor, "n")
  expect_identical(res$x, 0)
  expect_identical(res$y, 8.5)
})

# Leader pricing ---------------------------------------------------------------

test_that("a leader is priced at its own length plus 4 mm per ink point", {
  own <- list(x = 0, y = 0, radius = 4)
  nodes <- data.frame(x = 0, y = 0, radius = 4)

  # One box due north, its nearest point 14 mm from the node center, so the
  # leader runs 10 mm from the disc edge at (0, 4) to (0, 14).
  cand <- list(
    anchor = "n",
    rank = 0,
    x = 0,
    y = 16.5,
    xmin = -4,
    ymin = 14,
    xmax = 4,
    ymax = 19
  )

  plain <- leader_crossing_length(
    cand,
    TRUE,
    own,
    nodes,
    label_ink_points(no_edges)
  )
  expect_equal(plain$extra, 10)

  # Three sampled points on the leader, each well clear of the box itself.
  crossed <- data.frame(edge_id = "stem", x = 0, y = c(6, 7, 8))
  priced <- leader_crossing_length(
    cand,
    TRUE,
    own,
    nodes,
    label_ink_points(crossed)
  )
  expect_equal(priced$extra, 10 + 3 * 4)
})

# Node `a` at the origin with a 4 mm disc and a wide 20 x 4 label. Five anchors
# at one ring, all 6 mm clear of the disc and so all drawn with a leader:
#
#   ne ( 17.07,  9.07)  center 19.33 mm out, leader crosses nothing
#   nw (-17.07,  9.07)  blocked by the fence west of the node
#   se ( 17.07, -9.07)  blocked by the fence south of the node
#   sw (-17.07, -9.07)  blocked by the fence south of the node
#   n  (  0.00, 12.00)  center 12.00 mm out, leader crosses three ink points
#
# Today the N box wins: 12 mm of center distance and three crossings at 2 mm
# each price it at 3.64 against 3.87 for the NE box. Pricing the leader's own
# 6 mm and each crossing at 4 mm puts N at 6.04 and NE at 5.07.
leader_pricing_scene <- function() {
  list(
    labels = data.frame(id = "a", x = 0, y = 0, width = 20, height = 4),
    nodes = data.frame(x = 0, y = 0, radius = 4),
    edges = rbind(
      data.frame(edge_id = "south", x = seq(-35, 35, by = 1), y = -6.5),
      data.frame(edge_id = "west", x = -12.5, y = seq(6, 13, by = 0.5)),
      data.frame(edge_id = "stem", x = 0, y = c(5, 6, 7))
    ),
    bounds = c(-60, -40, 60, 40)
  )
}

test_that("a long leader over open space beats a short one over ink", {
  scene <- leader_pricing_scene()
  res <- place_dag_labels(
    scene$labels,
    scene$nodes,
    scene$edges,
    scene$bounds,
    gap = 6,
    n_angles = 5L,
    n_rings = 1L,
    n_rays = 2L,
    reach = 20,
    leader = 5
  )

  expect_identical(res$anchor, "ne")
  expect_equal(res$x, 17.0710678118655, tolerance = 1e-9)
  expect_equal(res$y, 9.07106781186548, tolerance = 1e-9)
})

# A scene where the leader's own length is the only thing left to choose on.
# The node sits at the origin with a radius of 4 and a 20 x 4 label. Ink fills
# every candidate the grid offers except two: the E ray from a clearance of 4
# and the N ray from a clearance of 9. Both are admissible, both are in the
# same band, one node means no ownership or soft term, and neither leader
# passes within a millimetre of any ink, so the two differ only in how far the
# box sits from the node and how long a leader reaches it. The E box at
# clearance 4 is 18 mm out on a 4 mm leader and the N box at clearance 9 is
# 15 mm out on a 9 mm leader: 0.2 * (18 + 4) = 4.4 against 0.2 * (15 + 9) =
# 4.8 with the leader's own length priced, and 3.6 against 3.0 without it, so
# the picture flips if that term ever stops being paid.
leader_length_scene <- function() {
  list(
    labels = data.frame(id = "a", x = 0, y = 0, width = 20, height = 4),
    nodes = data.frame(x = 0, y = 0, radius = 4),
    edges = rbind(
      # inside every N box, and within the arrowhead zone of the last one
      data.frame(edge_id = "north", x = 9, y = seq(6, 10.5, by = 0.5)),
      # the same the whole way down the S ray
      data.frame(edge_id = "south", x = 9, y = seq(-35, -6, by = 0.5)),
      # inside the two E boxes nearest the node
      data.frame(edge_id = "east", x = c(5.9, 5.9), y = c(1.9, -1.9)),
      # a stub in each of the eight diagonal ring boxes
      data.frame(edge_id = "ne1", x = c(14, 14.5), y = c(6, 6)),
      data.frame(edge_id = "ne2", x = c(20, 20.5), y = c(13, 13)),
      data.frame(edge_id = "nw1", x = c(-14, -14.5), y = c(6, 6)),
      data.frame(edge_id = "nw2", x = c(-20, -20.5), y = c(13, 13)),
      data.frame(edge_id = "se1", x = c(14, 14.5), y = c(-6, -6)),
      data.frame(edge_id = "se2", x = c(20, 20.5), y = c(-13, -13)),
      data.frame(edge_id = "sw1", x = c(-14, -14.5), y = c(-6, -6)),
      data.frame(edge_id = "sw2", x = c(-20, -20.5), y = c(-13, -13))
    ),
    bounds = c(-60, -60, 60, 60)
  )
}

test_that("a shorter leader wins when nothing else separates two spots", {
  scene <- leader_length_scene()
  res <- place_dag_labels(
    scene$labels,
    scene$nodes,
    scene$edges,
    scene$bounds,
    gap = 2,
    n_angles = 7L,
    n_rings = 2L,
    n_rays = 4L,
    reach = 9,
    leader = 1
  )

  expect_identical(res$anchor, "e")
  expect_equal(res$x, 18, tolerance = 1e-9)
  expect_equal(res$y, 0, tolerance = 1e-9)
})

# Placements that must not move -------------------------------------------------

placement_fixture <- function() {
  readRDS(test_path("fixtures", "label-placement-inputs.rds"))
}

# The engine's own scene fixture is captured with the defaults, where `reach`
# and `leader` are both infinite. Every rule pinned above is off there: no
# candidate needs a leader, so none is priced; the ownership term is bounded by
# `reach` and switches off when it is infinite; and the local grid is built
# only for a finite reach. These are the placements the engine gives today, and
# they must survive the round unchanged.
#
# The pins are exact millimetre geometry at a tolerance of 1e-9, as the 7 x 5
# placement fixture is: R's source parser does not round-trip every double, so
# a decimal literal in a test file cannot always name the bit pattern the
# engine returns.
infinite_reach_placements <- list(
  mediation_triangle = list(
    anchor = c("n", "s", "s"),
    x = c(124.891241178912, 20.8152068631521, 228.967275494673),
    y = c(187.040264029176, 5.28279550756143, 5.28279550756143)
  ),
  ten_node = list(
    anchor = c("n", "n", "n", "s", "n", "n", "n", "ne", "s", "ne*"),
    x = c(
      20.8152068631521,
      62.4456205894562,
      62.4456205894562,
      104.07603431576,
      104.07603431576,
      145.706448042064,
      145.706448042064,
      200.336293105361,
      187.336861768369,
      239.819379410825
    ),
    y = c(
      137.560208957843,
      105.535217045785,
      163.237905175619,
      43.6550831139011,
      138.764018601728,
      187.040264029176,
      81.7089162581516,
      138.679224495125,
      5.28279550756143,
      96.2179690487581
    )
  ),
  curved_edge = list(
    anchor = c("e", "w", "n"),
    x = c(39.1368810276521, 211.004172548673, 124.891241178912),
    y = c(70.0428912994398, 70.0428912994398, 187.040264029176)
  ),
  text_variant = list(
    anchor = c("n", "s", "s"),
    x = c(124.891241178912, 20.8152068631521, 228.967275494673),
    y = c(187.040264029176, 5.28279550756143, 5.28279550756143)
  ),
  dense = list(
    anchor = c("n", "s", "n*", "ne", "ne*"),
    x = c(
      90.1992297403256,
      90.1992297403256,
      22.110827525,
      181.631787715992,
      226.243795966325
    ),
    y = c(
      187.040264029176,
      5.28279550756143,
      129.186116430816,
      104.562510138361,
      121.319597969418
    )
  ),
  faceted_paths_panel1 = list(
    anchor = c("s", "s", "n"),
    x = c(17.0502271386352, 85.251135693176, 51.1506814159056),
    y = c(4.12189555872374, 4.12189555872374, 174.270364591961)
  ),
  faceted_paths_panel2 = list(
    anchor = c("s", "s", "n"),
    x = c(17.0502271386352, 85.251135693176, 51.1506814159056),
    y = c(4.12189555872374, 4.12189555872374, 174.270364591961)
  ),
  debug_overlay = list(
    anchor = c("n", "s", "s"),
    x = c(124.891241178912, 25.9560311746961, 223.826451183129),
    y = c(178.454413157, 13.8686463797372, 13.8686463797372)
  )
)

test_that("the engine at infinite reach places the fixture scenes as before", {
  fixture <- placement_fixture()
  expect_named(fixture, names(infinite_reach_placements))

  for (name in names(infinite_reach_placements)) {
    scene <- fixture[[name]]
    res <- place_dag_labels(
      scene$labels,
      scene$nodes,
      scene$edges,
      scene$bounds,
      gap = scene$gap,
      reach = Inf
    )
    expected <- infinite_reach_placements[[name]]

    expect_identical(
      res$anchor,
      expected$anchor,
      label = paste(name, "anchors")
    )
    expect_equal(
      res$x,
      expected$x,
      tolerance = 1e-9,
      label = paste(name, "box centers x")
    )
    expect_equal(
      res$y,
      expected$y,
      tolerance = 1e-9,
      label = paste(name, "box centers y")
    )
  }
})

test_that("the ten-node fixture scene keeps Medication out of Weight's corridor", {
  scene <- placement_fixture()$ten_node
  res <- place_dag_labels(
    scene$labels,
    scene$nodes,
    scene$edges,
    scene$bounds,
    gap = scene$gap
  )

  # The engine fixture is where a stronger distance pull would drag
  # Medication's label out of the N anchor and down toward Weight. Leader
  # pricing must not reach it: nothing in this scene needs a leader.
  i <- match("Medication", scene$text)
  expect_identical(res$anchor[i], "n")
  expect_equal(res$x[i], 145.706448042064, tolerance = 1e-9)
  expect_equal(res$y[i], 81.7089162581516, tolerance = 1e-9)
})

test_that("the smaller scenes match the pinned 7 x 5 placements", {
  skip_on_cran()
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  # Exact millimetre geometry carries the same platform exposure as the vdiffr
  # baselines: text metrics and the last ulp of the layout arithmetic both
  # differ off macOS. See helper-vdiffr.R.
  skip_on_os(c("windows", "linux", "solaris"))

  # The three scenes of the fixture whose placements no rule of this file, or
  # of test-label-auto-occlusion.R, is allowed to move. The fixture's fourth
  # scene, the 30-node one, is the scene those rules are calibrated on and is
  # pinned in test-label-auto-perf.R alone.
  fixture <- readRDS(test_path("fixtures", "label-placements-7x5.rds"))
  keys <- as.vector(outer(
    c("ten_node", "dense", "saturated"),
    c("straight", "spline"),
    paste,
    sep = "|"
  ))

  for (key in keys) {
    parts <- strsplit(key, "|", fixed = TRUE)[[1]]
    placed <- perf_scene_placement(parts[[1]], parts[[2]])
    expected <- fixture[[key]]

    expect_equal(
      placed$boxes,
      expected$boxes,
      tolerance = 1e-9,
      label = paste(key, "label boxes"),
      expected.label = paste(key, "pinned label boxes")
    )
    expect_equal(
      placed$leaders,
      expected$leaders,
      tolerance = 1e-9,
      label = paste(key, "leader segments"),
      expected.label = paste(key, "pinned leader segments")
    )
    expect_identical(
      placed$unresolved,
      expected$unresolved,
      label = paste(key, "unresolved labels"),
      expected.label = paste(key, "pinned unresolved labels")
    )
  }
})
