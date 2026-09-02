# Placement-quality contracts for place_dag_labels() on real captured scenes.
#
# The fixture at fixtures/label-placement-inputs.rds holds the exact mm-space
# inputs the automatic label geoms hand to the engine for the label-auto
# visual baseline plots, captured on the vdiffr device (svglite, 10 x 8
# inches) with fixtures/make-label-placement-fixtures.R. Calling the engine
# on these inputs exercises it on full scenes deterministically, with no
# graphics device open at test time.
#
# Several tests here pin proximity behavior the engine does not yet have:
# a per-label pull toward its own node, a soft clearance zone around other
# nodes' discs, and slide-into-bounds candidate variants for boxes that
# spill the panel. Those tests fail until the engine gains these forces; the
# remaining tests pin behavior that already holds and must survive them.
#
# Placements are pinned as predicates in mm (inside a region, within a
# distance of the own node, clear of another disc) rather than as exact
# coordinates, so the engine keeps tuning freedom within each contract.

placement_fixture <- function() {
  readRDS(test_path("fixtures", "label-placement-inputs.rds"))
}

fixture_scenes <- c(
  "mediation_triangle",
  "ten_node",
  "curved_edge",
  "text_variant",
  "dense",
  "faceted_paths_panel1",
  "faceted_paths_panel2",
  "debug_overlay"
)

place_scene <- function(scene) {
  place_dag_labels(
    scene$labels,
    scene$nodes,
    scene$edges,
    scene$bounds,
    gap = scene$gap
  )
}

# The node center of the label drawn with `text`, from the captured scene.
scene_node <- function(scene, text) {
  i <- match(text, scene$text)
  c(scene$labels$x[i], scene$labels$y[i])
}

# The placed box center of the label drawn with `text`.
placed_center <- function(result, scene, text) {
  i <- match(text, scene$text)
  c(result$x[i], result$y[i])
}

# The placed box limits of the label drawn with `text`.
placed_box <- function(result, scene, text) {
  i <- match(text, scene$text)
  c(
    xmin = result$x[i] - scene$labels$width[i] / 2,
    ymin = result$y[i] - scene$labels$height[i] / 2,
    xmax = result$x[i] + scene$labels$width[i] / 2,
    ymax = result$y[i] + scene$labels$height[i] / 2
  )
}

box_dist_to_point <- function(box, px, py) {
  dx <- pmax(box[["xmin"]] - px, px - box[["xmax"]], 0)
  dy <- pmax(box[["ymin"]] - py, py - box[["ymax"]], 0)
  sqrt(dx^2 + dy^2)
}

box_overlap_area <- function(a, b) {
  overlap_w <- min(a[["xmax"]], b[["xmax"]]) - max(a[["xmin"]], b[["xmin"]])
  overlap_h <- min(a[["ymax"]], b[["ymax"]]) - max(a[["ymin"]], b[["ymin"]])
  max(overlap_w, 0) * max(overlap_h, 0)
}

dist_between <- function(p, q) {
  sqrt(sum((p - q)^2))
}

# Signed length of the projection of (point - from) onto the unit vector
# pointing from `from` to `toward`: how far `point` has drifted toward
# `toward`, in mm.
drift_toward <- function(point, from, toward) {
  u <- (toward - from) / dist_between(toward, from)
  sum((point - from) * u)
}

# Even-odd test for a point inside the polygon given as a two-column matrix
# of vertices.
point_in_polygon <- function(point, vertices) {
  n <- nrow(vertices)
  j <- n
  inside <- FALSE
  for (i in seq_len(n)) {
    crosses <- (vertices[i, 2] > point[2]) != (vertices[j, 2] > point[2]) &&
      point[1] <
        (vertices[j, 1] - vertices[i, 1]) *
          (point[2] - vertices[i, 2]) /
          (vertices[j, 2] - vertices[i, 2]) +
          vertices[i, 1]
    if (crosses) {
      inside <- !inside
    }
    j <- i
  }
  inside
}

# Hard-overlap checks for one placed label: the box must clear every node
# disc, contain no sampled edge point, stay clear of every arrowhead zone
# (the final segment of each edge polyline), and lie inside the panel.
expect_label_free_of_hard_overlaps <- function(result, scene, text) {
  box <- placed_box(result, scene, text)

  for (k in seq_len(nrow(scene$nodes))) {
    expect_gte(
      box_dist_to_point(box, scene$nodes$x[k], scene$nodes$y[k]),
      scene$nodes$radius[k]
    )
  }

  if (nrow(scene$edges) > 0) {
    inside <- scene$edges$x >= box[["xmin"]] &
      scene$edges$x <= box[["xmax"]] &
      scene$edges$y >= box[["ymin"]] &
      scene$edges$y <= box[["ymax"]]
    expect_identical(sum(inside), 0L)

    arrows <- final_edge_segments(scene$edges)
    arrow_dist <- rect_segment_dist(
      rep(box[["xmin"]], nrow(arrows)),
      rep(box[["ymin"]], nrow(arrows)),
      rep(box[["xmax"]], nrow(arrows)),
      rep(box[["ymax"]], nrow(arrows)),
      arrows$x1,
      arrows$y1,
      arrows$x2,
      arrows$y2
    )
    expect_gt(min(arrow_dist), 0)
  }

  expect_gte(box[["xmin"]], scene$bounds[[1]] - 1e-6)
  expect_gte(box[["ymin"]], scene$bounds[[2]] - 1e-6)
  expect_lte(box[["xmax"]], scene$bounds[[3]] + 1e-6)
  expect_lte(box[["ymax"]], scene$bounds[[4]] + 1e-6)
}

no_edges <- data.frame(edge_id = character(), x = numeric(), y = numeric())

# Proximity, soft clearance, and slide forces ----------------------------------

test_that("a lone node's label stays near its node", {
  # An 18 x 8 mm box at a cardinal ring 1 anchor centers 9.5 mm from the
  # node (radius + gap + height / 2); at a diagonal ring 1 anchor it centers
  # 15.1 mm away. With nothing else in the scene the engine must prefer a
  # near clean spot, whichever anchor that is.
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-40, -40, 40, 40)

  res <- place_dag_labels(labels, nodes, no_edges, bounds)

  expect_lte(dist_between(c(res$x, res$y), c(0, 0)), 10)

  # The box still clears the disc by the default gap.
  box <- c(
    xmin = res$x - 9,
    ymin = res$y - 4,
    xmax = res$x + 9,
    ymax = res$y + 4
  )
  expect_equal(box_dist_to_point(box, 0, 0) - 4, 1.5, tolerance = 1e-6)
})

test_that("zeroing the proximity weight restores the anchor preference", {
  # The same lone node with the proximity pull turned off: nothing but the
  # preference rank separates the clean candidates, so the label returns to
  # the NE ring 1 spot the preference order starts with.
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 0, radius = 4)
  bounds <- c(-40, -40, 40, 40)

  res <- place_dag_labels(
    labels,
    nodes,
    no_edges,
    bounds,
    weights = c(
      node = 100,
      edge = 12,
      arrow = 40,
      label = 30,
      bounds = 60,
      prefer = 0.01,
      dist = 0,
      soft = 1
    )
  )

  expect_equal(res$anchor, "ne")
})

test_that("a label keeps a soft clearance from another node's disc", {
  # A second disc (radius 4) at (0, 20) sits 2.5 mm above the N ring 1 box
  # and 5 mm from the NE ring 1 box: close enough to crowd either, with no
  # hard overlap. The label must take an equally near spot on the far side
  # of its node instead, at least 8 mm clear of the other disc, without
  # giving up its own-node proximity.
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = c(0, 0), y = c(0, 20), radius = 4)
  bounds <- c(-40, -40, 40, 40)

  res <- place_dag_labels(labels, nodes, no_edges, bounds)

  box <- c(
    xmin = res$x - 9,
    ymin = res$y - 4,
    xmax = res$x + 9,
    ymax = res$y + 4
  )
  expect_gte(box_dist_to_point(box, 0, 20) - 4, 8)
  expect_lte(dist_between(c(res$x, res$y), c(0, 0)), 10)
})

test_that("the soft clearance zone ends 8 mm past the disc", {
  # The same scene with the other disc raised to (0, 25.6), exactly 8.1 mm
  # beyond the N ring 1 box: outside the soft zone, so the near northern
  # spot is unpenalized and the label stays close on that side.
  labels <- data.frame(id = "a", x = 0, y = 0, width = 18, height = 8)
  nodes <- data.frame(x = c(0, 0), y = c(0, 25.6), radius = 4)
  bounds <- c(-40, -40, 40, 40)

  res <- place_dag_labels(labels, nodes, no_edges, bounds)

  expect_lte(dist_between(c(res$x, res$y), c(0, 0)), 10)

  box <- c(
    xmin = res$x - 9,
    ymin = res$y - 4,
    xmax = res$x + 9,
    ymax = res$y + 4
  )
  expect_gte(box_dist_to_point(box, 0, 25.6) - 4, 8)
})

test_that("a candidate spilling the panel slides inside instead of spilling", {
  # A node at (0, 9) in a corridor panel 12 mm tall: every candidate box
  # either overlaps the disc or leaves the panel, but the E and W ring 1
  # boxes (y from 5 to 13) spill by only 1 mm, so a minimal vertical slide
  # puts one fully inside while it still clears the disc. The engine must
  # return an in-panel box near the node, not the least-bad spilling one.
  labels <- data.frame(id = "a", x = 0, y = 9, width = 18, height = 8)
  nodes <- data.frame(x = 0, y = 9, radius = 4)
  bounds <- c(-40, 0, 40, 12)

  res <- place_dag_labels(labels, nodes, no_edges, bounds)

  box <- c(
    xmin = res$x - 9,
    ymin = res$y - 4,
    xmax = res$x + 9,
    ymax = res$y + 4
  )
  expect_gte(box[["xmin"]], -40 - 1e-6)
  expect_gte(box[["ymin"]], 0 - 1e-6)
  expect_lte(box[["xmax"]], 40 + 1e-6)
  expect_lte(box[["ymax"]], 12 + 1e-6)

  expect_gte(box_dist_to_point(box, 0, 9) - 4, 1)
  expect_lte(dist_between(c(res$x, res$y), c(0, 9)), 20)
})

# Weights contract -------------------------------------------------------------

test_that("the default weights include the proximity and soft-zone terms", {
  weights <- eval(formals(place_dag_labels)$weights)

  expect_setequal(
    names(weights),
    c("node", "edge", "arrow", "label", "bounds", "prefer", "dist", "soft")
  )
  expect_equal(
    weights[c(
      "node",
      "edge",
      "arrow",
      "label",
      "bounds",
      "prefer",
      "dist",
      "soft"
    )],
    c(
      node = 100,
      edge = 12,
      arrow = 40,
      label = 30,
      bounds = 60,
      prefer = 0.01,
      dist = 0.2,
      soft = 1
    )
  )
})

# Ten-node scene contracts -----------------------------------------------------

test_that("ten-node scene: Blood pressure's label sits inside its diamond", {
  scene <- placement_fixture()$ten_node
  res <- place_scene(scene)

  # Blood pressure's node forms a diamond with Stress, Cholesterol, and
  # Exercise, and the enclosed region is the label's only breathing room in
  # this layout: the box center must land inside it while keeping well clear
  # of the Stress disc at the diamond's far corner.
  diamond <- rbind(
    scene_node(scene, "Blood pressure"),
    scene_node(scene, "Stress"),
    scene_node(scene, "Cholesterol"),
    scene_node(scene, "Exercise")
  )
  center <- placed_center(res, scene, "Blood pressure")
  expect_true(point_in_polygon(center, diamond))

  stress <- scene_node(scene, "Stress")
  stress_radius <- scene$nodes$radius[which.min(
    (scene$nodes$x - stress[[1]])^2 + (scene$nodes$y - stress[[2]])^2
  )]
  box <- placed_box(res, scene, "Blood pressure")
  expect_gte(
    box_dist_to_point(box, stress[[1]], stress[[2]]) - stress_radius,
    8
  )
})

test_that("ten-node scene: Weight's label stays out of the corridor to Medication", {
  scene <- placement_fixture()$ten_node
  res <- place_scene(scene)

  d <- scene_node(scene, "Weight")
  g <- scene_node(scene, "Medication")
  center <- placed_center(res, scene, "Weight")

  # The label must read as Weight's, not Medication's: it stays closer to d
  # than to g and does not drift down the d-to-g corridor by more than 8 mm.
  expect_lt(dist_between(center, d), dist_between(center, g))
  expect_lte(drift_toward(center, d, g), 8)
  expect_label_free_of_hard_overlaps(res, scene, "Weight")
})

test_that("ten-node scene: Medication's label stays out of the corridor to Weight", {
  scene <- placement_fixture()$ten_node
  res <- place_scene(scene)

  d <- scene_node(scene, "Weight")
  g <- scene_node(scene, "Medication")
  center <- placed_center(res, scene, "Medication")

  expect_lte(drift_toward(center, g, d), 8)
  expect_lte(dist_between(center, g), 33)
  expect_label_free_of_hard_overlaps(res, scene, "Medication")
})

test_that("ten-node scene: Outcome's label stays adjacent to its node", {
  scene <- placement_fixture()$ten_node
  res <- place_scene(scene)

  y <- scene_node(scene, "Outcome")
  center <- placed_center(res, scene, "Outcome")

  expect_lte(dist_between(center, y), 25)
  expect_label_free_of_hard_overlaps(res, scene, "Outcome")
})

test_that("ten-node scene: no label overlaps a node, an edge, or a label", {
  scene <- placement_fixture()$ten_node
  res <- place_scene(scene)

  for (text in scene$text) {
    expect_label_free_of_hard_overlaps(res, scene, text)
  }
  boxes <- lapply(scene$text, placed_box, result = res, scene = scene)
  pairs <- utils::combn(length(boxes), 2)
  for (p in seq_len(ncol(pairs))) {
    expect_equal(
      box_overlap_area(boxes[[pairs[1, p]]], boxes[[pairs[2, p]]]),
      0
    )
  }
})

# Dense scene contracts --------------------------------------------------------

test_that("dense scene: Cardiovascular disease's label stays adjacent to y", {
  scene <- placement_fixture()$dense
  res <- place_scene(scene)

  y <- scene_node(scene, "Cardiovascular disease")
  center <- placed_center(res, scene, "Cardiovascular disease")

  expect_lte(dist_between(center, y), 36)
  expect_label_free_of_hard_overlaps(res, scene, "Cardiovascular disease")
})

test_that("dense scene: Socioeconomic status's label stays near c", {
  scene <- placement_fixture()$dense
  res <- place_scene(scene)

  c_node <- scene_node(scene, "Socioeconomic status")
  center <- placed_center(res, scene, "Socioeconomic status")

  expect_lte(dist_between(center, c_node), 36)
  expect_label_free_of_hard_overlaps(res, scene, "Socioeconomic status")
})

test_that("dense scene: Physical activity's label stays clean and close", {
  scene <- placement_fixture()$dense
  res <- place_scene(scene)

  x <- scene_node(scene, "Physical activity")
  center <- placed_center(res, scene, "Physical activity")

  expect_lte(dist_between(center, x), 30)
  expect_label_free_of_hard_overlaps(res, scene, "Physical activity")
})

test_that("dense scene: labels never overlap each other or the panel edge", {
  scene <- placement_fixture()$dense
  res <- place_scene(scene)

  boxes <- lapply(scene$text, placed_box, result = res, scene = scene)
  pairs <- utils::combn(length(boxes), 2)
  for (p in seq_len(ncol(pairs))) {
    expect_equal(
      box_overlap_area(boxes[[pairs[1, p]]], boxes[[pairs[2, p]]]),
      0
    )
  }
  for (box in boxes) {
    expect_gte(box[["xmin"]], scene$bounds[[1]] - 1e-6)
    expect_gte(box[["ymin"]], scene$bounds[[2]] - 1e-6)
    expect_lte(box[["xmax"]], scene$bounds[[3]] + 1e-6)
    expect_lte(box[["ymax"]], scene$bounds[[4]] + 1e-6)
  }
})

# Non-regression across the remaining baseline scenes --------------------------

test_that("the remaining baseline scenes place every label without hard overlaps", {
  fixture <- placement_fixture()
  scenes <- c(
    "mediation_triangle",
    "curved_edge",
    "text_variant",
    "faceted_paths_panel1",
    "faceted_paths_panel2",
    "debug_overlay"
  )

  for (name in scenes) {
    scene <- fixture[[name]]
    res <- place_scene(scene)

    for (text in scene$text) {
      expect_label_free_of_hard_overlaps(res, scene, text)
    }
    boxes <- lapply(scene$text, placed_box, result = res, scene = scene)
    pairs <- utils::combn(length(boxes), 2)
    for (p in seq_len(ncol(pairs))) {
      expect_equal(
        box_overlap_area(boxes[[pairs[1, p]]], boxes[[pairs[2, p]]]),
        0,
        info = name
      )
    }
  }
})

test_that("placement on every captured scene is deterministic", {
  fixture <- placement_fixture()
  expect_setequal(names(fixture), fixture_scenes)

  for (name in fixture_scenes) {
    scene <- fixture[[name]]
    first <- place_scene(scene)
    second <- place_scene(scene)
    expect_identical(
      first,
      second,
      label = paste0(name, ": first run"),
      expected.label = paste0(name, ": second run")
    )
  }
})
