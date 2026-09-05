# Quality of the automatic label placement, measured on drawn pictures.
#
# Every test here renders a labelled plot with the ggarrow engine on an
# off-screen ragg device at 150 dpi, forces the grob tree so that every
# `makeContent()` method has run, and reads the result back in millimetres:
# the label boxes the `dag_labels_auto` gTree drew, the node discs it placed
# them around, and the paths the arrow grobs actually drew. The same plot is
# measured at three device sizes, 4 x 3, 7 x 5, and 10 x 6 inches, because
# the engine works in millimetres and a placement that reads well at one size
# can fail at another.
#
# The quantities measured are:
#
# * clearance: the distance from a box's nearest edge to its own node's disc
#   edge, `rect_point_dist(box, node) - radius`. A ring 1 candidate has
#   clearance `gap` (2 mm here); a leader is drawn once it exceeds
#   `min.segment.length` (5 mm).
# * a hit: drawn ink closer to a box than the engine's own margins, 1 mm for
#   an edge stroke and 2 mm along the last 5 mm of a path where the arrowhead
#   is drawn. Ink is the drawn path resected by the edge cap at both ends,
#   which is the part of it on the page.
# * an admissible spot: a box position for one label, the others where the
#   engine put them, that lies inside the panel by half a node radius, clears
#   every node disc by the node margin, overlaps no other box, and has no hit.
#   Spots are searched on 36 rays around the node at eight clearances between
#   the node margin and the bound under test, anchored both corner-on-ray
#   and centre-on-ray.
#
# Signal pinned for labels the engine cannot place cleanly: after
# `makeContent()`, the `dag_labels_auto` gTree carries a field `unresolved`,
# a character vector of the label texts whose chosen box still violates a
# hard constraint (a node disc, drawn ink within the margins above, another
# label box, or the panel bounds), and `character(0)` when every box is
# clear. `grid.force()` keeps the field on the forced tree, so a test reads
# `tree$unresolved`. A box named there is still drawn at the least-bad
# candidate; a box not named there has no hit.
#
# No snapshots are written here: every expectation is a numeric predicate on
# millimetre geometry, and each block lists the labels that violate it so a
# failure names them.

# Scenes -----------------------------------------------------------------------

# The ten-node epidemiology DAG with a label on every node, laid out by the
# time-ordered layout the suite sets as its default.
ten_node_labelled_dag <- function(labels = ten_node_labels) {
  dagify(
    b ~ a,
    c ~ a,
    d ~ b,
    e ~ b + c,
    f ~ c,
    g ~ d + e,
    h ~ e + f,
    x ~ g,
    y ~ g + h + x,
    exposure = "x",
    outcome = "y",
    labels = labels
  )
}

ten_node_labels <- c(
  a = "Genetics",
  b = "Diet",
  c = "Exercise",
  d = "Weight",
  e = "Blood pressure",
  f = "Cholesterol",
  g = "Medication",
  h = "Stress",
  x = "Treatment",
  y = "Outcome"
)

# A chain at y = 0 with two skip edges over interior nodes, labelled.
labelled_skip_chain_dag <- function() {
  dagify(
    b ~ a,
    c ~ b,
    d ~ c + b,
    e ~ d + a,
    labels = c(
      a = "Baseline",
      b = "Adherence",
      c = "Dose",
      d = "Response",
      e = "Outcome"
    ),
    coords = list(
      x = c(a = 0, b = 1, c = 2, d = 3, e = 4),
      y = c(a = 0, b = 0, c = 0, d = 0, e = 0)
    )
  )
}

# The confounding triangle, for the faceted paths plot.
small_paths_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder"),
    coords = list(x = c(x = 0, z = 1, y = 2), y = c(x = 0, z = 1, y = 0))
  )
}

quality_labelled_plot <- function(dag) {
  ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto) +
    theme_dag()
}

quality_plots <- list(
  ten_node = function() quality_labelled_plot(ten_node_labelled_dag()),
  skip_chain = function() quality_labelled_plot(labelled_skip_chain_dag()),
  saturated = function() {
    quality_labelled_plot(dag_saturate(ten_node_labelled_dag()))
  },
  paths = function() {
    ggdag_paths(
      small_paths_dag(),
      shadow = TRUE,
      use_labels = TRUE,
      label_geom = geom_dag_label_auto
    )
  }
)

quality_sizes <- list(c(4, 3), c(7, 5), c(10, 6))

size_key <- function(size) {
  paste0(size[[1]], "x", size[[2]])
}

# Measuring a drawn plot -------------------------------------------------------

# Build `plot` under the ggarrow engine and the routing mode `route`, draw it
# at `size` inches, force the grob tree, and measure every panel. The plot is
# both built and drawn under the options because `ggdag()` reads the engine
# when it is called and the routed layer reads the mode when it is added.
quality_scenes <- function(build, size, route = "spline") {
  withr::local_options(list(
    ggdag.edge_engine = "ggarrow",
    ggdag.edge_route = route
  ))
  plot <- build()

  file <- tempfile(fileext = ".png")
  ragg::agg_png(
    file,
    width = size[[1]],
    height = size[[2]],
    units = "in",
    res = 150
  )
  on.exit(
    {
      grDevices::dev.off()
      unlink(file)
    },
    add = TRUE
  )

  gtable <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  grid::grid.newpage()
  grid::grid.draw(gtable)
  grid::grid.force()

  # The label gTree and every edge grob of every panel; a legend key draws
  # arrows too, so only grobs under a panel viewport count.
  pattern <- "dag_labels_auto|arrow_path|curve_arrow"
  paths <- grid::grid.grep(pattern, grep = TRUE, global = TRUE)
  paths <- vapply(paths, as.character, character(1))
  paths <- paths[grepl("^layout::panel", paths)]
  paths <- paths[grepl(pattern, sub(".*::", "", paths))]
  viewports <- vapply(
    strsplit(paths, "::", fixed = TRUE),
    `[[`,
    character(1),
    2L
  )
  panels <- split(paths, factor(viewports, levels = unique(viewports)))

  unname(lapply(panels, function(group) {
    viewport <- strsplit(group[[1]], "::", fixed = TRUE)[[1]][[2]]
    grid::seekViewport(viewport)
    on.exit(grid::upViewport(0), add = TRUE)
    panel_width <- grid::convertWidth(grid::unit(1, "npc"), "mm", TRUE)
    panel_height <- grid::convertHeight(grid::unit(1, "npc"), "mm", TRUE)

    tree <- grid::grid.get(group[grepl("dag_labels_auto", group)][[1]])
    edge_grobs <- lapply(
      group[!grepl("dag_labels_auto", group)],
      grid::grid.get
    )
    drawn <- unlist(lapply(edge_grobs, drawn_paths_mm), recursive = FALSE)

    list(
      tree = tree,
      labels = forced_label_table(tree, panel_width, panel_height),
      nodes = data.frame(
        x = tree$nodes$x * panel_width,
        y = tree$nodes$y * panel_height,
        radius = node_radius_mm(tree$nodes$node_size)
      ),
      drawn = drawn,
      cap = tree$params$edge_cap,
      width = panel_width,
      height = panel_height
    )
  }))
}

# Scenes are built once per plot, size, and route, and shared by the blocks
# that measure them.
quality_scene_cache <- new.env(parent = emptyenv())

cached_scenes <- function(name, size, route = "spline") {
  key <- paste(name, size_key(size), route)
  if (is.null(quality_scene_cache[[key]])) {
    quality_scene_cache[[key]] <- quality_scenes(
      quality_plots[[name]],
      size,
      route
    )
  }
  quality_scene_cache[[key]]
}

# The drawn paths of one edge grob, in millimetres of the current viewport,
# one data frame per edge. A routed layer draws an `arrow_path` whose points
# are the router's polyline; the straight engine draws a `curve_arrow` whose
# `curve` holds the chord ends and a curvature, zero for a directed edge.
drawn_paths_mm <- function(grob) {
  if (inherits(grob, "arrow_path")) {
    ids <- grob$id_rle
    ids <- if (inherits(ids, "rle")) {
      inverse.rle(ids)
    } else {
      fields <- unclass(ids)
      rep(fields$group, fields$length)
    }
    points <- data.frame(
      x = grid::convertX(grob$x, "mm", TRUE),
      y = grid::convertY(grob$y, "mm", TRUE)
    )
    return(unname(split(points, factor(ids, levels = unique(ids)))))
  }

  curve <- grob$curve
  if (is.null(curve) || length(curve$x1) == 0) {
    return(list())
  }
  x1 <- grid::convertX(curve$x1, "mm", TRUE)
  y1 <- grid::convertY(curve$y1, "mm", TRUE)
  x2 <- grid::convertX(curve$x2, "mm", TRUE)
  y2 <- grid::convertY(curve$y2, "mm", TRUE)
  lapply(seq_along(x1), function(i) {
    if (curve$curvature == 0) {
      return(data.frame(x = c(x1[i], x2[i]), y = c(y1[i], y2[i])))
    }
    sample_curved_edge(x1[i], y1[i], x2[i], y2[i], curve$curvature, n = 50)
  })
}

# One row per label of a forced `dag_labels_auto` gTree: the text, the box in
# millimetres, and the node centre the label belongs to. Forcing turns each
# box into a polygon whose geometry moves to the viewport
# `makeContext.roundrect()` attaches, in the absolute millimetres the engine
# placed it at. Boxes and texts are emitted in label order, so the i-th box
# belongs to the i-th text.
forced_label_table <- function(tree, panel_width, panel_height) {
  children <- tree$children
  names <- vapply(children, function(child) child$name %||% "", character(1))
  boxes <- unname(children[grepl("roundrect", names)])
  texts <- unname(children[grepl("text", names)])
  stopifnot(
    length(boxes) == length(texts),
    length(texts) == nrow(tree$labels)
  )

  rows <- lapply(seq_along(boxes), function(i) {
    box <- boxes[[i]]
    center_x <- grid::convertX(box$vp$x, "mm", TRUE)
    center_y <- grid::convertY(box$vp$y, "mm", TRUE)
    width <- grid::convertWidth(box$vp$width, "mm", TRUE)
    height <- grid::convertHeight(box$vp$height, "mm", TRUE)
    data.frame(
      label = as.character(texts[[i]]$label),
      xmin = center_x - width / 2,
      xmax = center_x + width / 2,
      ymin = center_y - height / 2,
      ymax = center_y + height / 2,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)

  index <- match(out$label, tree$labels$label)
  out$node_x <- tree$labels$x[index] * panel_width
  out$node_y <- tree$labels$y[index] * panel_height
  out$n_leaders <- sum(grepl("segments", names))
  out
}

# The polyline resampled every `spacing` millimetres, so that a segment
# passing a box is caught by a point beside it.
densify_mm_polyline <- function(path, spacing = 0.5) {
  x <- path$x
  y <- path$y
  dense_x <- x[[1]]
  dense_y <- y[[1]]
  for (i in seq_len(length(x) - 1)) {
    dx <- x[[i + 1]] - x[[i]]
    dy <- y[[i + 1]] - y[[i]]
    steps <- max(1, ceiling(sqrt(dx^2 + dy^2) / spacing))
    fraction <- seq_len(steps) / steps
    dense_x <- c(dense_x, x[[i]] + fraction * dx)
    dense_y <- c(dense_y, y[[i]] + fraction * dy)
  }
  data.frame(x = dense_x, y = dense_y)
}

# The polyline with `cap` millimetres dropped at each end, which is where the
# arrow layer resects it, so only ink the reader sees remains.
trim_by_cap <- function(path, cap) {
  last <- nrow(path)
  to_ends <- pmin(
    sqrt((path$x - path$x[[1]])^2 + (path$y - path$y[[1]])^2),
    sqrt((path$x - path$x[[last]])^2 + (path$y - path$y[[last]])^2)
  )
  path[to_ends > cap, , drop = FALSE]
}

# Every drawn path as visible ink: resected by the cap, resampled every half
# millimetre, with the points under the arrowhead flagged.
drawn_ink <- function(scene) {
  head_length <- ggdag_option("arrow_length", 5)
  lapply(scene$drawn, function(path) {
    dense <- trim_by_cap(densify_mm_polyline(path), scene$cap)
    last <- nrow(dense)
    if (last == 0) {
      dense$head <- logical()
      return(dense)
    }
    to_end <- sqrt(
      (dense$x - dense$x[[last]])^2 + (dense$y - dense$y[[last]])^2
    )
    dense$head <- to_end <= head_length
    dense
  })
}

# The radius of each label's own node disc.
label_radius <- function(scene) {
  nearest_node_radius(scene$labels$node_x, scene$labels$node_y, scene$nodes)
}

# Clearance in millimetres from each box to its own node's disc edge.
box_clearance <- function(scene) {
  labels <- scene$labels
  rect_point_dist(
    labels$xmin,
    labels$ymin,
    labels$xmax,
    labels$ymax,
    labels$node_x,
    labels$node_y
  ) -
    label_radius(scene)
}

# Does drawn ink come within the engine's margins of a box?
ink_hits_box <- function(ink, xmin, ymin, xmax, ymax) {
  for (path in ink) {
    if (nrow(path) == 0) {
      next
    }
    distance <- rect_point_dist(xmin, ymin, xmax, ymax, path$x, path$y)
    margin <- ifelse(path$head, label_arrow_clearance, label_edge_clearance)
    if (any(distance < margin)) {
      return(TRUE)
    }
  }
  FALSE
}

# The labels whose boxes drawn ink hits.
hit_labels <- function(scene) {
  ink <- drawn_ink(scene)
  labels <- scene$labels
  hit <- vapply(
    seq_len(nrow(labels)),
    function(i) {
      ink_hits_box(
        ink,
        labels$xmin[i],
        labels$ymin[i],
        labels$xmax[i],
        labels$ymax[i]
      )
    },
    logical(1)
  )
  labels$label[hit]
}

# The labels whose boxes come closer than half a node radius to the panel
# border.
border_labels <- function(scene) {
  labels <- scene$labels
  inset <- label_radius(scene) / 2
  outside <- labels$xmin < inset |
    labels$ymin < inset |
    labels$xmax > scene$width - inset |
    labels$ymax > scene$height - inset
  labels$label[outside]
}

# Is a box at these limits admissible for the label in row `i`?
box_admissible <- function(xmin, ymin, xmax, ymax, scene, ink, i, inset) {
  if (
    xmin < inset ||
      ymin < inset ||
      xmax > scene$width - inset ||
      ymax > scene$height - inset
  ) {
    return(FALSE)
  }
  to_nodes <- rect_point_dist(
    xmin,
    ymin,
    xmax,
    ymax,
    scene$nodes$x,
    scene$nodes$y
  )
  if (any(to_nodes < scene$nodes$radius + label_node_clearance)) {
    return(FALSE)
  }
  others <- scene$labels[-i, , drop = FALSE]
  overlap <- rect_overlap_area(
    xmin,
    ymin,
    xmax,
    ymax,
    others$xmin,
    others$ymin,
    others$xmax,
    others$ymax
  )
  if (any(overlap > 0)) {
    return(FALSE)
  }
  !ink_hits_box(ink, xmin, ymin, xmax, ymax)
}

# Does label `i` have an admissible box with clearance at most `bound`
# millimetres from its node disc, the other labels left where they are?
admissible_within <- function(scene, i, bound) {
  labels <- scene$labels
  ink <- drawn_ink(scene)
  radius <- label_radius(scene)[[i]]
  inset <- radius / 2
  node_x <- labels$node_x[i]
  node_y <- labels$node_y[i]
  width <- labels$xmax[i] - labels$xmin[i]
  height <- labels$ymax[i] - labels$ymin[i]

  angles <- seq(0, 2 * pi, length.out = 37)[-37]
  gaps <- seq(label_node_clearance, bound, length.out = 8)
  for (gap in gaps) {
    for (theta in angles) {
      ux <- cos(theta)
      uy <- sin(theta)
      ray_x <- node_x + ux * (radius + gap)
      ray_y <- node_y + uy * (radius + gap)
      # The facing corner on the ray puts the box's nearest point exactly at
      # `gap` from the disc; the centre on the ray lets a box straddle the
      # ray, as a label beside a node often does.
      centers <- list(
        c(
          ray_x + sign(round(ux, 8)) * width / 2,
          ray_y + sign(round(uy, 8)) * height / 2
        ),
        c(ray_x + ux * width / 2, ray_y + uy * height / 2)
      )
      for (center in centers) {
        xmin <- center[[1]] - width / 2
        xmax <- center[[1]] + width / 2
        ymin <- center[[2]] - height / 2
        ymax <- center[[2]] + height / 2
        clearance <- rect_point_dist(xmin, ymin, xmax, ymax, node_x, node_y) -
          radius
        if (clearance < label_node_clearance || clearance > bound) {
          next
        }
        if (box_admissible(xmin, ymin, xmax, ymax, scene, ink, i, inset)) {
          return(TRUE)
        }
      }
    }
  }
  FALSE
}

# The labels placed further than `bound` from their node although an
# admissible spot within `bound` exists.
far_labels <- function(scene, bound) {
  clearance <- box_clearance(scene)
  far <- which(clearance > bound)
  avoidable <- far[vapply(
    far,
    function(i) admissible_within(scene, i, bound[[i]]),
    logical(1)
  )]
  scene$labels$label[avoidable]
}

# The box of every label as a named matrix, rows in label order.
label_boxes <- function(scene) {
  labels <- scene$labels
  boxes <- as.matrix(labels[, c("xmin", "xmax", "ymin", "ymax")])
  rownames(boxes) <- labels$label
  boxes[order(rownames(boxes)), , drop = FALSE]
}

# Proximity ----------------------------------------------------------------------

test_that("a label stays within one and a half radii of its node when it can", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # Clearance from the disc edge, in millimetres, of every box the engine
  # places (node radius 6 mm, so the bound is 9 mm), and whether an
  # admissible spot within the bound exists for a box beyond it:
  #
  #   ten-node 4x3: Genetics 13.96 (none), Diet 13.59 (spot), Exercise 2.00,
  #     Weight 10.38 (spot), Blood pressure 23.65 (none), Cholesterol 8.45,
  #     Medication 13.96 (none), Stress 2.00, Treatment 2.00,
  #     Outcome 22.69 (none)
  #   ten-node 7x5: Blood pressure 33.84 (spot), Medication 13.96 (spot),
  #     Outcome 6.97, Cholesterol and Treatment 1.32, the rest 2.00
  #   ten-node 10x6: Blood pressure 17.92 (spot), Medication 13.96 (spot),
  #     Outcome 7.00, Cholesterol and Treatment 0.76, the rest 2.00
  #   skip chain 4x3: Baseline 6.65, Adherence 13.96 (spot), Dose 8.86,
  #     Response 13.26 (spot), Outcome 6.97
  #   skip chain 7x5 and 10x6: Dose 8.86, the rest 2.00
  #
  # At 7 x 5 and 10 x 6 the two boxes a reader sees pushed away from their
  # nodes, Blood pressure and Medication, are the two with a free spot
  # beside the node. A box with no admissible spot within the bound, such as
  # Blood pressure at 4 x 3, is allowed to be far.
  for (name in c("ten_node", "skip_chain")) {
    for (size in quality_sizes) {
      scene <- cached_scenes(name, size)[[1]]
      bound <- 1.5 * label_radius(scene)
      expect_identical(
        far_labels(scene, bound),
        character(0),
        label = paste(name, size_key(size), "labels beyond 1.5 radii")
      )
    }
  }
})

# Panel border -------------------------------------------------------------------

test_that("no box slides onto the panel border", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # A candidate that spills the panel is slid back exactly to the border, so
  # a slid box touches it: at 10 x 6 the Weight box sits at
  # [96.2, 112.0] x [0.0, 5.6] mm and the Medication box at
  # [134.1, 157.3] x [0.0, 5.6] mm on a 249.8 x 148.2 mm panel. Every box
  # must stay inside the panel by half a node radius, 3 mm here. Boxes on
  # the border before the fix, per size:
  #
  #   saturated 4x3: Genetics, Exercise, Blood pressure, Cholesterol,
  #     Stress, Treatment, Outcome
  #   saturated 7x5: Genetics, Weight, Blood pressure, Cholesterol,
  #     Medication, Outcome
  #   saturated 10x6: Genetics, Weight, Blood pressure, Cholesterol,
  #     Medication, Outcome
  for (size in quality_sizes) {
    scene <- cached_scenes("saturated", size)[[1]]
    expect_identical(
      border_labels(scene),
      character(0),
      label = paste("saturated", size_key(size), "boxes on the border")
    )
  }
})

test_that("no box slides onto the panel border on the sparser scenes", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # The same inset on the two scenes with room to spare. Before the fix:
  #
  #   ten-node 4x3: Genetics, Diet, Exercise, Blood pressure, Cholesterol,
  #     Medication, Stress, Outcome
  #   ten-node 7x5 and 10x6: Cholesterol, Treatment, Outcome
  #   skip chain 4x3: Baseline, Outcome
  #   skip chain 7x5 and 10x6: none
  for (name in c("ten_node", "skip_chain")) {
    for (size in quality_sizes) {
      scene <- cached_scenes(name, size)[[1]]
      expect_identical(
        border_labels(scene),
        character(0),
        label = paste(name, size_key(size), "boxes on the border")
      )
    }
  }
})

# Boxes on drawn ink -------------------------------------------------------------

# The scenes with room for every box: no drawn ink may reach a label box, and
# the engine must report nothing unresolved. The saturated DAG at 4 x 3 has no
# clear spot for most boxes and is covered by the block after these.
clear_scenes <- list(
  list(name = "ten_node", route = "spline", sizes = quality_sizes),
  list(name = "skip_chain", route = "spline", sizes = quality_sizes),
  list(name = "saturated", route = "spline", sizes = quality_sizes[2:3]),
  list(name = "saturated", route = "straight", sizes = quality_sizes[2:3])
)

test_that("boxes keep off the drawn paths where the picture has room", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # Labels whose box drawn ink reaches, before the fix, under the routed
  # engine (spline) and the straight one:
  #
  #   ten-node spline 4x3 and 7x5: none; 10x6: Weight (the b -> d arrowhead)
  #   skip chain spline, every size: none
  #   saturated spline 7x5: Outcome; 10x6: none
  #   saturated straight 7x5: Treatment, Outcome; 10x6: none
  for (case in clear_scenes) {
    for (size in case$sizes) {
      scene <- cached_scenes(case$name, size, case$route)[[1]]
      expect_identical(
        hit_labels(scene),
        character(0),
        label = paste(case$name, case$route, size_key(size), "boxes on ink")
      )
    }
  }
})

test_that("the paths plot keeps the Outcome box off the shadow edge", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # Two panels, each a 100.3 x 133.4 mm picture of the confounding triangle
  # with the Outcome node 16.7 mm from the right border. Before the fix the
  # Outcome box is placed above the node at [80.4, 100.3] x [24.1, 29.7] mm,
  # 0.69 mm from the z -> y edge, so the grey shadow arrow reads as running
  # into it in both panels. An admissible spot exists between the two
  # arrows at the node, at [58.1, 78.0] x [14.4, 20.0] mm.
  scenes <- cached_scenes("paths", c(10, 6))
  expect_length(scenes, 2)
  for (k in seq_along(scenes)) {
    expect_identical(
      hit_labels(scenes[[k]]),
      character(0),
      label = paste("paths panel", k, "boxes on ink")
    )
  }
})

test_that("a scene with room for every box reports nothing unresolved", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # The signal has to be present, and empty, on every clear scene: a tree
  # without the field cannot tell a clear placement from an unreported one.
  # Before the fix the field does not exist.
  for (case in clear_scenes) {
    for (size in case$sizes) {
      scene <- cached_scenes(case$name, size, case$route)[[1]]
      expect_identical(
        scene$tree$unresolved,
        character(0),
        label = paste(case$name, case$route, size_key(size), "unresolved")
      )
    }
  }
  for (scene in cached_scenes("paths", c(10, 6))) {
    expect_identical(
      scene$tree$unresolved,
      character(0),
      label = "paths panel unresolved"
    )
  }
})

test_that("a box the engine cannot clear is reported as unresolved", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # The saturated DAG at 4 x 3 inches: 41 edges on a 97.4 x 72.0 mm panel,
  # where several boxes cannot avoid the ink. Before the fix the routed
  # picture has seven boxes hit (Genetics, Exercise, Blood pressure,
  # Cholesterol, Medication, Treatment, Outcome), the straight one two
  # (Cholesterol, Medication), and nothing reports it. Every hit box must be
  # named in `unresolved`; a box not named there must be clear.
  for (route in c("spline", "straight")) {
    scene <- cached_scenes("saturated", c(4, 3), route)[[1]]
    unresolved <- scene$tree$unresolved
    hits <- hit_labels(scene)

    expect_type(unresolved, "character")
    expect_true(
      all(unresolved %in% scene$labels$label),
      label = paste(route, "unresolved names only labels of the scene")
    )
    expect_identical(
      setdiff(hits, unresolved),
      character(0),
      label = paste(route, "hit boxes not reported as unresolved")
    )
  }
})
# Leaders --------------------------------------------------------------------------

test_that("a leader is drawn only when no leaderless spot exists", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # A leader is drawn once a box is more than `min.segment.length` (5 mm)
  # from its disc. It is a fallback: a label only gets one when no admissible
  # spot within 5 mm exists. Boxes with a leader and such a spot, before the
  # fix:
  #
  #   ten-node 4x3: Diet, Weight, Cholesterol
  #   ten-node 7x5 and 10x6: Blood pressure, Medication, Outcome
  #   skip chain 4x3: Baseline, Adherence, Dose, Response, Outcome
  #   skip chain 7x5 and 10x6: Dose
  #   saturated 4x3: none (no spot within 5 mm for any far box)
  #   saturated 7x5: Treatment, Outcome
  #   saturated 10x6: Genetics, Outcome
  min_segment_length <- 5
  for (name in c("ten_node", "skip_chain", "saturated")) {
    for (size in quality_sizes) {
      scene <- cached_scenes(name, size)[[1]]
      clearance <- box_clearance(scene)

      # the leaders drawn are exactly the boxes past the threshold
      expect_equal(
        scene$labels$n_leaders[[1]],
        sum(clearance > min_segment_length),
        label = paste(name, size_key(size), "leader count")
      )

      bound <- rep(min_segment_length, nrow(scene$labels))
      expect_identical(
        far_labels(scene, bound),
        character(0),
        label = paste(name, size_key(size), "leaders with a nearer spot")
      )
    }
  }
})

# Determinism ----------------------------------------------------------------------

test_that("drawing the same plot twice places every box identically", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  first <- quality_scenes(quality_plots$ten_node, c(7, 5))[[1]]
  second <- quality_scenes(quality_plots$ten_node, c(7, 5))[[1]]

  expect_equal(label_boxes(first), label_boxes(second), tolerance = 1e-9)
  expect_identical(hit_labels(first), hit_labels(second))
  expect_identical(border_labels(first), border_labels(second))
  expect_identical(first$tree$unresolved, second$tree$unresolved)
})

test_that("the order of the labels vector does not move a box", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  forward <- cached_scenes("ten_node", c(7, 5))[[1]]
  reversed <- quality_scenes(
    function() {
      quality_labelled_plot(ten_node_labelled_dag(rev(ten_node_labels)))
    },
    c(7, 5)
  )[[1]]

  expect_identical(
    rownames(label_boxes(forward)),
    rownames(label_boxes(reversed))
  )
  expect_lt(max(abs(label_boxes(forward) - label_boxes(reversed))), 0.01)
  expect_identical(sort(hit_labels(forward)), sort(hit_labels(reversed)))
  expect_identical(sort(border_labels(forward)), sort(border_labels(reversed)))
})

test_that("the row order of the DAG data does not move a box", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # The same DAG, at the same coordinates, with its rows in reverse order:
  # the labels reach the engine in a different order and the nodes are
  # collected in a different order, and neither may change where a box goes.
  forward <- cached_scenes("ten_node", c(7, 5))[[1]]
  tidy <- tidy_dagitty(ten_node_labelled_dag())
  reordered <- quality_scenes(
    function() {
      quality_labelled_plot(dplyr::arrange(
        tidy,
        dplyr::desc(name),
        dplyr::desc(to)
      ))
    },
    c(7, 5)
  )[[1]]

  expect_identical(
    rownames(label_boxes(forward)),
    rownames(label_boxes(reordered))
  )
  expect_lt(max(abs(label_boxes(forward) - label_boxes(reordered))), 0.01)
  expect_identical(sort(hit_labels(forward)), sort(hit_labels(reordered)))
  expect_identical(sort(border_labels(forward)), sort(border_labels(reordered)))
})
