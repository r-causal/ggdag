# Tests for the shared geometry contract between the routed edge layer and the
# automatic label engine.
#
# Directed edges are routed at draw time: `GeomDAGRoutedArrow` converts its
# panel to millimetres inside `makeContent()` and calls the pure router
# `route_edges_mm()`. The automatic label geoms place their labels the same
# way, in millimetres at draw time, using the drawn edges as obstacles. The
# rule that keeps the two pictures consistent is that both grobs call the same
# pure router on the same inputs, so discovery communicates a routing spec
# rather than waypoints:
#
# * `panel_node_centers()` is the one helper both layers take their obstacle
#   node centres from, so the two node sets are equal by construction.
# * `repel_edge_points()` hands the automatic stat the chord endpoints of a
#   routed edge tagged with how it is routed, and hands the ggrepel stats the
#   plain chord, which is what data-space repulsion has always been given.
# * `GeomDagLabelAuto$draw_panel()` passes that spec, the node size, and the
#   edge sampling resolution to its gTree, and `makeContent.dag_labels_auto()`
#   calls `route_edges_mm()` with them and samples the result with
#   `sample_polyline()`.
#
# The parity between the two grobs is therefore asserted on a real drawing:
# the plot is rendered to an off-screen device, the grob tree is forced, and
# the label grob's own inputs are handed to the router by hand. The path that
# comes back must be the path the arrow grob drew.

# Helpers ----------------------------------------------------------------------

# The collinear mediation triangle: the mediator sits dead on the x -> y
# chord, so x -> y is routed and every other edge stays straight.
collinear_mediator_dag <- function() {
  dagify(
    y ~ x + m,
    m ~ x,
    labels = c(x = "Exposure", m = "Mediator", y = "Outcome"),
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
}

# One row of discovered geometry for a routed edge, in the shape
# `discover_edge_geometry()` emits: how the edge is routed rather than where
# it goes.
routed_geometry <- function(
  x,
  y,
  xend,
  yend,
  curvature = NA_real_,
  route_style = "spline",
  route_options = edge_route_options(),
  route_layer_axis = "auto"
) {
  geometry <- data.frame(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
    circular = FALSE,
    type = "routed",
    strength = NA_real_,
    n = 100,
    fold = FALSE,
    flipped = FALSE,
    from = "from",
    to = "to",
    curvature = curvature,
    route_style = route_style,
    route_layer_axis = route_layer_axis,
    stringsAsFactors = FALSE
  )
  geometry$route_options <- list(route_options)
  geometry
}

# A column of `data`, or a column of NA when the contract's column is not
# there yet, so that a missing column fails an expectation rather than
# erroring out of the test.
column_or_na <- function(data, name, missing = NA) {
  if (name %in% names(data)) data[[name]] else rep(missing, nrow(data))
}

# The edge index of every point of an `arrow_path` grob. ggarrow stores the
# `id` vector run-length encoded.
arrow_grob_ids <- function(grob) {
  ids <- grob$id_rle
  if (inherits(ids, "rle")) {
    return(inverse.rle(ids))
  }
  fields <- unclass(ids)
  rep(fields$group, fields$length)
}

# The drawn paths of an `arrow_path` grob, one data frame of millimetres per
# edge. The routed grob builds its paths in millimetres, so the numbers are
# already the ones the router returned.
arrow_grob_paths <- function(grob) {
  ids <- arrow_grob_ids(grob)
  points <- data.frame(x = as.numeric(grob$x), y = as.numeric(grob$y))
  unname(split(points, factor(ids, levels = unique(ids))))
}

# Open an off-screen device of `width` by `height` inches and return the file
# it writes to. `"ragg"` is the raster device the contract blocks measure
# millimetres on; `"svglite"` is the device vdiffr renders a baseline on, so a
# block that guards a baseline measures the picture that baseline holds
# rather than a second rendering of the same plot somewhere else.
open_panel_device <- function(device, width, height) {
  if (identical(device, "svglite")) {
    file <- tempfile(fileext = ".svg")
    svg_device <- utils::getFromNamespace("svglite", "vdiffr")
    svg_device(file, width = width, height = height)
    return(file)
  }
  file <- tempfile(fileext = ".png")
  open_test_ragg(file, width, height)
  file
}

# Render `plot` on an off-screen device, force the grob tree so that every
# `makeContent()` method has run, and return the gTrees whose own name
# matches `pattern` together with the panel's size in millimetres. The
# measurement is taken inside the panel viewport, which is the viewport the
# two grobs measured themselves in, so `npc * width` is the millimetre a
# position was converted to at draw time.
forced_panel_scene <- function(
  plot,
  pattern,
  width = 7,
  height = 5,
  device = "ragg"
) {
  file <- open_panel_device(device, width, height)
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

  viewports <- unique(
    grid::grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)$name
  )
  panel <- viewports[grepl("^panel\\.", viewports)][[1]]
  grid::seekViewport(panel)
  panel_width <- grid::convertWidth(grid::unit(1, "npc"), "mm", TRUE)
  panel_height <- grid::convertHeight(grid::unit(1, "npc"), "mm", TRUE)
  grid::upViewport(0)

  paths <- grid::grid.grep(pattern, grep = TRUE, global = TRUE)
  paths <- vapply(paths, as.character, character(1))
  matched <- paths[grepl(pattern, sub(".*::", "", paths))]
  grobs <- lapply(matched, grid::grid.get)
  names(grobs) <- matched

  list(grobs = grobs, width = panel_width, height = panel_height)
}

# The same scene, one entry per panel of a faceted plot. Each panel measures
# its own viewport, so a grob's positions convert to the millimetres that
# panel drew them in, and the label boxes are read while the device that drew
# them is still open, because a closed device leaves nothing to measure in.
forced_panel_scenes <- function(
  plot,
  pattern,
  width = 7,
  height = 5,
  device = "ragg"
) {
  file <- open_panel_device(device, width, height)
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

  paths <- grid::grid.grep(pattern, grep = TRUE, global = TRUE)
  paths <- vapply(paths, as.character, character(1))
  matched <- paths[grepl(pattern, sub(".*::", "", paths))]
  viewports <- vapply(
    strsplit(matched, "::", fixed = TRUE),
    `[[`,
    character(1),
    2L
  )

  panels <- split(matched, factor(viewports, levels = unique(viewports)))
  lapply(panels, function(group) {
    viewport <- strsplit(group[[1]], "::", fixed = TRUE)[[1]][[2]]
    grid::seekViewport(viewport)
    panel_width <- grid::convertWidth(grid::unit(1, "npc"), "mm", TRUE)
    panel_height <- grid::convertHeight(grid::unit(1, "npc"), "mm", TRUE)
    grid::upViewport(0)

    grobs <- lapply(group, grid::grid.get)
    names(grobs) <- group
    list(
      grobs = grobs,
      boxes = do.call(c, lapply(grobs, forced_label_boxes)),
      width = panel_width,
      height = panel_height
    )
  })
}

# The label boxes of a forced `dag_labels_auto` gTree, in millimetres. Forcing
# turns each box into a polygon whose geometry moves to the viewport
# `makeContext.roundrect()` attaches; the engine placed the box in absolute
# millimetres, so that is what the viewport holds.
forced_label_boxes <- function(grob) {
  children <- grob$children
  if (!inherits(grob, "dag_labels_auto") || length(children) == 0) {
    return(list())
  }
  names <- vapply(children, function(child) child$name %||% "", character(1))
  lapply(unname(children[grepl("roundrect", names)]), function(box) {
    center_x <- grid::convertX(box$vp$x, "mm", TRUE)
    center_y <- grid::convertY(box$vp$y, "mm", TRUE)
    width <- grid::convertWidth(box$vp$width, "mm", TRUE)
    height <- grid::convertHeight(box$vp$height, "mm", TRUE)
    c(
      xmin = center_x - width / 2,
      xmax = center_x + width / 2,
      ymin = center_y - height / 2,
      ymax = center_y + height / 2
    )
  })
}

# The obstacle polylines the label grob places against, recovered by handing
# its own inputs to the routine it calls at draw time. The realised polyline
# is not kept on the gTree, so this is the same call, from the same grob.
label_routed_obstacles <- function(tree, scene) {
  edges_mm <- data.frame(
    edge_id = tree$edges$edge_id,
    x = tree$edges$x * scene$width,
    y = tree$edges$y * scene$height,
    stringsAsFactors = FALSE
  )
  # the grob names its nodes by their npc position, as the routed layer does,
  # before converting them to millimetres
  nodes_mm <- data.frame(
    name = routed_position_keys(tree$nodes$x, tree$nodes$y),
    x = tree$nodes$x * scene$width,
    y = tree$nodes$y * scene$height,
    radius = node_radius_mm(tree$nodes$node_size),
    stringsAsFactors = FALSE
  )
  routed <- route_label_obstacles(
    edges_mm,
    tree$edges,
    nodes_mm,
    tree$params,
    c(0, 0, scene$width, scene$height)
  )
  unname(split(
    routed,
    factor(routed$edge_id, levels = unique(routed$edge_id))
  ))
}

# The straight-line length between a polyline's two ends.
span_mm <- function(path) {
  last <- nrow(path)
  sqrt((path$x[[last]] - path$x[[1]])^2 + (path$y[[last]] - path$y[[1]])^2)
}

# The polyline resampled at `spacing` millimetres, so a box a segment passes
# through is caught by a point inside it.
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

# Does `path` come closer to `box` than the engine's own clearances allow?
# The engine keeps `label_edge_clearance` from a drawn edge and the wider
# `label_arrow_clearance` over the last `label_arrow_zone` of it, where the
# arrowhead is drawn, which is what `ink_hits_box()` measures in
# test-label-placement-quality.R. A path point inside the box is one case of
# this, so a guard on the margins is the stricter reading of the same rule.
path_meets_box <- function(path, box) {
  segments <- sqrt(diff(path$x)^2 + diff(path$y)^2)
  to_head <- c(rev(cumsum(rev(segments))), 0)
  margin <- ifelse(
    to_head <= label_arrow_zone,
    label_arrow_clearance,
    label_edge_clearance
  )
  distance <- rect_point_dist(
    box[["xmin"]],
    box[["ymin"]],
    box[["xmax"]],
    box[["ymax"]],
    path$x,
    path$y
  )
  any(distance < margin)
}

# The one forced gTree of `scene` whose name says it was drawn by `cl`.
scene_gtree <- function(scene, cl) {
  trees <- scene$grobs[grepl(cl, names(scene$grobs))]
  testthat::expect_length(trees, 1)
  trees[[1]]
}

# The edge obstacles the label grob was given, in millimetres, one data frame
# per edge with the routing spec the grob carries for it.
label_edge_input <- function(tree, scene) {
  edges <- tree$edges
  edges$x <- edges$x * scene$width
  edges$y <- edges$y * scene$height
  edges$route_style <- column_or_na(edges, "route_style", NA_character_)
  edges$route_layer_axis <- column_or_na(
    edges,
    "route_layer_axis",
    NA_character_
  )
  edges$route_cap <- column_or_na(edges, "route_cap", NA_real_)
  unname(split(edges, factor(edges$edge_id, levels = unique(edges$edge_id))))
}

# The routing spec the label grob carries for an edge. Either the params or
# the per-edge columns may hold it; what the contract fixes is that the
# values reach the grob and match the routed layer's.
label_route_spec <- function(tree, edge) {
  params <- tree$params
  pick <- function(names, from_edge) {
    found <- names[names %in% base::names(params)]
    value <- if (length(found) > 0) params[[found[[1]]]] else from_edge[[1]]
    if (length(value) == 0) NA else value
  }
  axis_or_auto <- function(value) {
    if (length(value) == 0 || is.na(value)) "auto" else value
  }
  list(
    style = pick(c("route", "route_style"), edge$route_style),
    cap = pick(c("route_cap"), edge$route_cap),
    layer_axis = axis_or_auto(pick(
      c("layer_axis", "route_layer_axis"),
      edge$route_layer_axis
    ))
  )
}

# Distance from each point to the nearest segment of the polyline through
# (poly_x, poly_y) in order.
polyline_dist <- function(px, py, poly_x, poly_y) {
  segments <- seq_len(length(poly_x) - 1)
  distances <- vapply(
    segments,
    function(i) {
      x <- poly_x[i]
      y <- poly_y[i]
      dx <- poly_x[i + 1] - x
      dy <- poly_y[i + 1] - y
      len_sq <- dx^2 + dy^2
      t <- if (len_sq == 0) {
        rep(0, length(px))
      } else {
        pmin(1, pmax(0, ((px - x) * dx + (py - y) * dy) / len_sq))
      }
      sqrt((px - (x + t * dx))^2 + (py - (y + t * dy))^2)
    },
    numeric(length(px))
  )
  if (is.null(dim(distances))) {
    return(min(distances))
  }
  apply(distances, 1, min)
}

# The symmetric Hausdorff distance between two polylines, measured point to
# segment in both directions so that two samplings of one curve compare as
# equal.
hausdorff_mm <- function(a, b) {
  max(
    max(polyline_dist(a$x, a$y, b$x, b$y)),
    max(polyline_dist(b$x, b$y, a$x, a$y))
  )
}

# The polyline with the millimetres the arrow layer resects at each end
# dropped, so that two paths are compared over the part of them the reader
# sees. A straight chord is two points, both of them ends, so it is trimmed
# to the points the cap from each end instead.
trim_by_cap <- function(path, cap) {
  last <- nrow(path)
  if (last == 2) {
    length <- sqrt(diff(path$x)^2 + diff(path$y)^2)
    f <- c(cap, length - cap) / length
    return(data.frame(
      x = path$x[[1]] + f * diff(path$x),
      y = path$y[[1]] + f * diff(path$y)
    ))
  }
  to_ends <- pmin(
    sqrt((path$x - path$x[[1]])^2 + (path$y - path$y[[1]])^2),
    sqrt((path$x - path$x[[last]])^2 + (path$y - path$y[[last]])^2)
  )
  path[to_ends > cap, , drop = FALSE]
}

# How far the polyline runs from its own chord at the middle of that chord.
mid_chord_deviation <- function(path) {
  last <- nrow(path)
  mid_x <- (path$x[[1]] + path$x[[last]]) / 2
  mid_y <- (path$y[[1]] + path$y[[last]]) / 2
  min(sqrt((path$x - mid_x)^2 + (path$y - mid_y)^2))
}

# The router's own path for one edge of a label grob's input, routed with the
# node centres, node radius, cap, and spec that grob carries. The label
# engine routes at draw time, so its realised obstacle polyline is not on the
# gTree; this is the same call it has to make, from the same inputs.
route_label_edge <- function(tree, scene, edge, route_options = NULL) {
  spec <- label_route_spec(tree, edge)
  if (is.na(spec$style)) {
    return(NULL)
  }
  node_size <- tree$params$node_size %||% tree$nodes$node_size
  radius <- node_radius_mm(node_size[[1]])
  nodes <- data.frame(
    name = paste0("n", seq_len(nrow(tree$nodes))),
    x = tree$nodes$x * scene$width,
    y = tree$nodes$y * scene$height,
    r = radius,
    stringsAsFactors = FALSE
  )
  nearest <- function(px, py) {
    nodes$name[[which.min((nodes$x - px)^2 + (nodes$y - py)^2)]]
  }
  # the engine routes every directed edge of the scene together, as the
  # arrow layer does, since a route depends on the other edges' chords and
  # arrivals; the requested edge's path is picked out afterwards. Edges
  # carrying different options objects are different calls to the router, so
  # only the requested edge's own set is routed with it.
  options <- route_options %||% label_route_options(edge)
  all_edges <- label_edge_input(tree, scene)
  all_edges <- all_edges[vapply(
    all_edges,
    function(e) {
      !is.na(label_route_spec(tree, e)$style) &&
        identical(label_route_options(e), label_route_options(edge))
    },
    logical(1)
  )]
  ends <- function(e) {
    last <- nrow(e)
    c(nearest(e$x[[1]], e$y[[1]]), nearest(e$x[[last]], e$y[[last]]))
  }
  chords <- t(vapply(all_edges, ends, character(2)))
  want <- ends(edge)
  at <- which(chords[, 1] == want[[1]] & chords[, 2] == want[[2]])[[1]]
  routed <- route_edges_mm(
    nodes = nodes,
    edges = data.frame(
      from = chords[, 1],
      to = chords[, 2],
      curvature = NA_real_,
      stringsAsFactors = FALSE
    ),
    bounds = c(0, 0, scene$width, scene$height),
    cap = if (is.na(spec$cap)) tree$params$edge_cap %||% 8 else spec$cap,
    mode = spec$style,
    opts = route_opts_from(options, radius, layer_axis = spec$layer_axis)
  )
  routed$paths[[at]]
}

# The routing options object the label grob carries for an edge. The spec
# travels as one list column, so the whole object arrives or none of it does.
label_route_options <- function(edge) {
  if (!"route_options" %in% names(edge)) {
    return(NULL)
  }
  edge$route_options[[1]]
}

# The drawn path whose endpoints are those of `edge`, in millimetres.
drawn_path_for <- function(tree, edge) {
  arrows <- find_arrow_paths(tree)
  if (length(arrows) == 0) {
    return(NULL)
  }
  paths <- arrow_grob_paths(arrows[[1]])
  last <- nrow(edge)
  distance <- vapply(
    paths,
    function(path) {
      end <- nrow(path)
      sqrt((path$x[[1]] - edge$x[[1]])^2 + (path$y[[1]] - edge$y[[1]])^2) +
        sqrt(
          (path$x[[end]] - edge$x[[last]])^2 +
            (path$y[[end]] - edge$y[[last]])^2
        )
    },
    numeric(1)
  )
  paths[[which.min(distance)]]
}

# The `arrow_path` children of a forced gTree.
find_arrow_paths <- function(tree) {
  children <- tree$children
  if (length(children) == 0) {
    return(list())
  }
  unname(children[vapply(children, inherits, logical(1), what = "arrow_path")])
}

# The 22 canonical DAGs as dagitty objects.
canonical_dagitty <- function(spec) {
  dagitty::dagitty(paste0(
    "dag {",
    paste(gsub("->", " -> ", spec, fixed = TRUE), collapse = "; "),
    "}"
  ))
}

# Shared node centres ----------------------------------------------------------

test_that("panel_node_centers() collects each node centre once", {
  # the mediation triangle as a layer sees it: three edge rows whose starts
  # and ends between them name every node of the panel
  data <- data.frame(
    x = c(0, 0, 1),
    y = c(0, 0, 0),
    xend = c(1, 2, 2),
    yend = c(0, 0, 0)
  )

  centers <- panel_node_centers(data)

  expect_named(centers, c("x", "y"))
  expect_equal(nrow(centers), 3)

  # the order is the one the drawn scene is built in, starts before ends and
  # each centre at its first appearance, so both layers name their nodes in
  # the same order without sorting
  expect_equal(centers$x, c(0, 1, 2))
  expect_equal(centers$y, c(0, 0, 0))
})

test_that("panel_node_centers() keeps nodes no edge leaves and bidirected ones", {
  # a node with only an incoming edge, an isolated node, and a pair joined by
  # a bidirected edge alone are all obstacles, and none of them starts a
  # directed row
  data <- data.frame(
    x = c(0, 3, 4),
    y = c(0, 3, 0),
    xend = c(2, NA, 5),
    yend = c(0, NA, 0)
  )

  centers <- panel_node_centers(data)

  expect_named(centers, c("x", "y"))
  expect_equal(nrow(centers), 5)
  expect_setequal(
    paste(centers$x, centers$y),
    c("0 0", "3 3", "4 0", "2 0", "5 0")
  )
})

test_that("panel_node_centers() finds every node of every canonical DAG", {
  # the node set the label engine treats as obstacles and the node set the
  # router routes around are the same set, on every DAG the layout tests use
  agrees <- vapply(
    canonical_dag_specs,
    function(spec) {
      dag <- canonical_dagitty(spec)
      data <- pull_dag_data(tidy_dagitty(dag, layout = "time_ordered"))
      centers <- panel_node_centers(data)
      nodes <- unique(as.data.frame(data)[c("name", "x", "y")])
      nrow(centers) == nrow(nodes) &&
        setequal(
          paste(centers$x, centers$y),
          paste(nodes$x, nodes$y)
        )
    },
    logical(1)
  )

  expected <- rep(TRUE, length(canonical_dag_specs))
  names(expected) <- names(canonical_dag_specs)
  expect_equal(agrees, expected)
})

# Sampling a routed path -------------------------------------------------------

test_that("sample_polyline() spaces its points by arc length", {
  # a right angle four millimetres up and three across: the arc length is
  # seven, so eight points fall one millimetre apart and the corner is one of
  # them
  sampled <- sample_polyline(c(0, 0, 3), c(0, 4, 4), 8)

  expect_equal(sampled$x, c(0, 0, 0, 0, 0, 1, 2, 3))
  expect_equal(sampled$y, c(0, 1, 2, 3, 4, 4, 4, 4))

  # the endpoints are the polyline's own, exactly
  expect_identical(sampled$x[[1]], 0)
  expect_identical(sampled$y[[1]], 0)
  expect_identical(sampled$x[[nrow(sampled)]], 3)
  expect_identical(sampled$y[[nrow(sampled)]], 4)
})

test_that("sample_polyline() keeps the polyline's own vertices", {
  # five points along the same right angle are 1.75 mm apart, so none of them
  # is the corner; the corner is kept anyway, because a label placed around a
  # detour has to see where the detour turns
  sampled <- sample_polyline(c(0, 0, 3), c(0, 4, 4), 5)

  expect_equal(nrow(sampled), 6)
  expect_equal(sampled$x, c(0, 0, 0, 0, 1.25, 3))
  expect_equal(sampled$y, c(0, 1.75, 3.5, 4, 4, 4))
})

# Tracing a routed edge for the label engine -----------------------------------

test_that("the automatic tracer tags a routed edge with its routing spec", {
  # one routed edge and one edge no layer claims, so the tagged rows can be
  # told from the untagged ones
  edges <- data.frame(
    x = c(0, 0),
    y = c(0, 0),
    xend = c(2, 1),
    yend = c(0, 1),
    PANEL = c(1L, 1L)
  )
  geometry <- routed_geometry(
    0,
    0,
    2,
    0,
    route_options = edge_route_options(clearance = 4, edge_sep = 2),
    route_layer_axis = "x"
  )

  points <- repel_edge_points(
    edges,
    20,
    geometry,
    NULL,
    include_endpoints = TRUE,
    trace_arrows = TRUE
  )

  expect_contains(
    names(points),
    c(
      "route_style",
      "route_options",
      "route_layer_axis",
      "curvature"
    )
  )

  tagged <- !is.na(column_or_na(points, "route_style", NA_character_))
  routed <- points[tagged, , drop = FALSE]

  # a routed edge reaches the label stat as its two chord endpoints: where it
  # goes is decided in millimetres at draw time, so the stat carries how it is
  # routed instead
  expect_equal(nrow(routed), 2)
  expect_length(unique(routed$edge_id), 1)
  expect_equal(routed$x, c(0, 2))
  expect_equal(routed$y, c(0, 0))
  expect_equal(routed$route_style, c("spline", "spline"))
  # the whole object travels with each of the two rows
  expect_equal(
    vapply(routed$route_options, function(o) o$clearance, numeric(1)),
    c(4, 4)
  )
  expect_equal(
    vapply(routed$route_options, function(o) o$edge_sep, numeric(1)),
    c(2, 2)
  )
  expect_equal(routed$route_layer_axis, c("x", "x"))
  expect_true(all(is.na(routed$curvature)))

  # the unclaimed edge is traced as a chord and carries no routing spec
  chord <- points[!tagged, , drop = FALSE]
  expect_equal(nrow(chord), 22)
  expect_true(all(is.na(column_or_na(chord, "route_style", NA_character_))))
})

test_that("a routed edge the user curved is traced as its arc, untagged", {
  edges <- data.frame(x = 0, y = 0, xend = 2, yend = 0, PANEL = 1L)

  arced <- repel_edge_points(
    edges,
    20,
    routed_geometry(0, 0, 2, 0, curvature = 0.3),
    NULL,
    include_endpoints = TRUE,
    trace_arrows = TRUE
  )

  # curvature the user set is never rerouted, so the edge is drawn as that arc
  # and traced as that arc, exactly as a curve layer's edges are
  curve <- sample_curved_edge(0, 0, 2, 0, curvature = 0.3, n = 22)
  expect_equal(nrow(arced), 22)
  expect_equal(arced$x, curve$x)
  expect_equal(arced$y, curve$y)
  expect_true(all(is.na(column_or_na(arced, "route_style", NA_character_))))

  straight <- repel_edge_points(
    edges,
    20,
    routed_geometry(0, 0, 2, 0, curvature = 0),
    NULL,
    include_endpoints = TRUE,
    trace_arrows = TRUE
  )

  # a zero the user set means the edge goes straight through whatever sits on
  # the chord, so the chord is the obstacle and there is nothing to route
  expect_equal(nrow(straight), 22)
  expect_lt(max(polyline_dist(straight$x, straight$y, c(0, 2), c(0, 0))), 1e-8)
  expect_true(all(is.na(column_or_na(straight, "route_style", NA_character_))))
})

test_that("the repel tracers see a routed edge as a chord", {
  # A deliberately pinned invariant: ggrepel repels in data space with no
  # draw-time hook, so it cannot follow a path decided in millimetres. The
  # routing spec must not reach it, whatever the automatic stat is given.
  edges <- data.frame(x = 0, y = 0, xend = 2, yend = 0, PANEL = 1L)

  points <- repel_edge_points(
    edges,
    12,
    routed_geometry(
      0,
      0,
      2,
      0,
      route_options = edge_route_options(clearance = 4, edge_sep = 2)
    ),
    NULL
  )

  expect_equal(nrow(points), 12)
  expect_lt(max(polyline_dist(points$x, points$y, c(0, 2), c(0, 0))), 1e-8)
  expect_true(all(is.na(column_or_na(points, "route_style", NA_character_))))
})

# The label grob's routing input -----------------------------------------------

test_that("the label grob routes the edge the arrow grob drew", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  p <- ggdag(
    tidy_dagitty(collinear_mediator_dag()),
    edge_engine = "ggarrow",
    edge_route = "spline",
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )

  scene <- forced_panel_scene(p, "dag_routed_edges|dag_labels_auto")
  routed_tree <- scene_gtree(scene, "dag_routed_edges")
  label_tree <- scene_gtree(scene, "dag_labels_auto")

  edges <- label_edge_input(label_tree, scene)
  spec <- vapply(
    edges,
    function(edge) !is.na(edge$route_style[[1]]),
    logical(1)
  )

  # every directed edge of the scene arrives as a routing spec, and the two
  # chord endpoints are all the geometry the stat has to carry
  expect_equal(sum(spec), 3L)
  expect_true(all(vapply(edges[spec], nrow, integer(1)) == 2L))

  # the longest chord is x -> y, the one the mediator blocks
  lengths <- vapply(
    edges,
    function(edge) {
      last <- nrow(edge)
      sqrt(
        (edge$x[[last]] - edge$x[[1]])^2 + (edge$y[[last]] - edge$y[[1]])^2
      )
    },
    numeric(1)
  )
  blocked <- edges[[which.max(lengths)]]

  cap <- label_tree$params$edge_cap %||% 8
  label_path <- route_label_edge(label_tree, scene, blocked)
  drawn_path <- drawn_path_for(routed_tree, blocked)

  # the sentinels stand in for a path the grob could not produce, so a
  # missing routing spec reads as a failed expectation rather than an error
  deviation <- if (is.null(label_path)) {
    -Inf
  } else {
    mid_chord_deviation(label_path)
  }
  parity <- if (is.null(label_path) || is.null(drawn_path)) {
    Inf
  } else {
    hausdorff_mm(trim_by_cap(label_path, cap), trim_by_cap(drawn_path, cap))
  }

  # the label engine's obstacle is the detour, not the chord it was traced
  # from: the mediator sits on the chord, so the path leaves it by more than a
  # node radius at mid-chord
  expect_gte(deviation, 6)

  # and it is the same detour, to a tenth of the drawn line's width: both
  # grobs called one pure router on one set of inputs
  expect_lt(parity, 0.5)
})

test_that("the router's obstacles have to name the nodes they route past", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  p <- ggdag(
    tidy_dagitty(collinear_mediator_dag()),
    edge_engine = "ggarrow",
    edge_route = "spline",
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )

  scene <- forced_panel_scene(p, "dag_labels_auto")
  label_tree <- scene_gtree(scene, "dag_labels_auto")
  edges_mm <- data.frame(
    edge_id = label_tree$edges$edge_id,
    x = label_tree$edges$x * scene$width,
    y = label_tree$edges$y * scene$height,
    stringsAsFactors = FALSE
  )
  nodes_mm <- data.frame(
    x = label_tree$nodes$x * scene$width,
    y = label_tree$nodes$y * scene$height,
    radius = node_radius_mm(label_tree$nodes$node_size),
    stringsAsFactors = FALSE
  )
  bounds <- c(0, 0, scene$width, scene$height)

  # The router breaks ties between equally priced routes by node name, so a
  # caller with no names for its nodes cannot be routing the picture the
  # arrows were drawn from, whatever names were invented for it here.
  expect_error(
    route_label_obstacles(
      edges_mm,
      label_tree$edges,
      nodes_mm,
      label_tree$params,
      bounds
    ),
    class = "ggdag_type_error"
  )

  # the same inputs, named the way the routed layer names them, route
  nodes_mm$name <- routed_position_keys(
    label_tree$nodes$x,
    label_tree$nodes$y
  )
  routed <- route_label_obstacles(
    edges_mm,
    label_tree$edges,
    nodes_mm,
    label_tree$params,
    bounds
  )

  expect_true(all(c("cap_head", "cap_fins") %in% names(routed)))
  expect_gt(nrow(routed), nrow(edges_mm))
})

test_that("the label grob routes every orthogonal channel the arrow grob drew", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # Orthogonal routing assigns slots and channels by sorting, and breaks ties
  # between equally priced routes by node name. The two grobs therefore have
  # to name their nodes identically as well as place them identically: the
  # names are keys of the npc positions, taken before either grob converts
  # to millimetres. The saturated ten-node DAG at 10 x 6 inches draws 41
  # channels, enough ties for a naming difference to move a third of them by
  # several millimetres.
  dag <- dag_saturate(dagify(
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
    labels = c(
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
  ))
  p <- ggdag(
    dag,
    edge_engine = "ggarrow",
    edge_route = "orthogonal",
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )

  scene <- forced_panel_scene(
    p,
    "dag_routed_edges|dag_labels_auto",
    width = 10,
    height = 6
  )
  routed_tree <- scene_gtree(scene, "dag_routed_edges")
  label_tree <- scene_gtree(scene, "dag_labels_auto")
  cap <- label_tree$params$edge_cap %||% 8

  obstacles <- label_routed_obstacles(label_tree, scene)
  expect_length(obstacles, 41)

  # every path the label grob places against is a channel the arrow grob
  # drew, point for point over the part of it on the page
  parity <- vapply(
    obstacles,
    function(path) {
      drawn <- drawn_path_for(routed_tree, path)
      if (is.null(drawn)) {
        return(Inf)
      }
      hausdorff_mm(trim_by_cap(path, cap), trim_by_cap(drawn, cap))
    },
    numeric(1)
  )
  expect_lt(max(parity), 0.5)
})

test_that("the label grob carries the routed layer's routing parameters", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  p <- ggdag(
    tidy_dagitty(collinear_mediator_dag()),
    edge_engine = "ggarrow",
    edge_route = "spline",
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )

  routed_layer <- p$layers[[which(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomDAGRoutedArrow"),
    logical(1)
  ))]]

  scene <- forced_panel_scene(p, "dag_routed_edges|dag_labels_auto")
  label_tree <- scene_gtree(scene, "dag_labels_auto")

  # the router's node discs and its sampling resolution both have to travel to
  # draw time, where the millimetres are known
  expect_equal(label_tree$params$node_size, routed_layer$geom_params$node_size)
  expect_equal(label_tree$params$n_edge_points, 20)

  edges <- label_edge_input(label_tree, scene)
  routed <- edges[vapply(
    edges,
    function(edge) !is.na(edge$route_style[[1]]),
    logical(1)
  )]
  expect_length(routed, 3)

  # a parameter the geom leaves to the router is unset in the object rather
  # than a guessed number, so the two calls agree on the default as well
  for (edge in routed) {
    spec <- label_route_spec(label_tree, edge)
    expect_equal(as.character(spec$style), routed_layer$geom_params$route)
    expect_identical(
      label_route_options(edge),
      routed_layer$geom_params$edge_route_options
    )
    expect_null(label_route_options(edge)$clearance)
    expect_null(label_route_options(edge)$edge_sep)
    expect_equal(
      as.character(spec$layer_axis),
      routed_layer$geom_params$layer_axis
    )
  }
})

test_that("the label grob routes with the options object the arrow grob drew with", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # A default object would prove nothing: the two grobs already agree on the
  # router's own constants. This scene is drawn under a clearance and a bow
  # cap neither of them derives.
  route_options <- edge_route_options(clearance = 5, max_bow = 0.12)
  p <- ggdag(
    tidy_dagitty(collinear_mediator_dag()),
    edge_engine = "ggarrow",
    edge_route = "spline",
    edge_route_options = route_options,
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )

  routed_layer <- p$layers[[which(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomDAGRoutedArrow"),
    logical(1)
  ))]]
  merged <- routed_layer$geom_params$edge_route_options

  scene <- forced_panel_scene(p, "dag_routed_edges|dag_labels_auto")
  routed_tree <- scene_gtree(scene, "dag_routed_edges")
  label_tree <- scene_gtree(scene, "dag_labels_auto")

  edges <- label_edge_input(label_tree, scene)
  routed <- edges[vapply(
    edges,
    function(edge) !is.na(edge$route_style[[1]]),
    logical(1)
  )]
  expect_length(routed, 3)

  # the whole object travels as one column, so a field added to the
  # constructor cannot be dropped on the way to the label engine
  for (edge in routed) {
    expect_identical(label_route_options(edge), merged)
  }

  # and the two grobs still draw one line: the label engine's obstacle is the
  # path the arrow layer drew, under options that are not the defaults
  lengths <- vapply(
    routed,
    function(edge) {
      last <- nrow(edge)
      sqrt(
        (edge$x[[last]] - edge$x[[1]])^2 + (edge$y[[last]] - edge$y[[1]])^2
      )
    },
    numeric(1)
  )
  blocked <- routed[[which.max(lengths)]]

  cap <- label_tree$params$edge_cap %||% 8
  label_path <- route_label_edge(
    label_tree,
    scene,
    blocked,
    route_options = merged
  )
  drawn_path <- drawn_path_for(routed_tree, blocked)

  # the clearance the object asks for is wider than the router's own, so the
  # detour leaves the mediator by more than the 9 mm the default would keep
  expect_gte(mid_chord_deviation(label_path), 10)
  expect_lt(
    hausdorff_mm(trim_by_cap(label_path, cap), trim_by_cap(drawn_path, cap)),
    0.5
  )
})

test_that("without a routed layer the label grob's edges stay chords", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # A deliberately pinned invariant: the straight engine draws chords, so the
  # label engine must be given chords and no routing spec to call the router
  # with.
  p <- ggdag(
    tidy_dagitty(collinear_mediator_dag()),
    edge_engine = "ggarrow",
    edge_route = "straight",
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )

  scene <- forced_panel_scene(p, "dag_labels_auto")
  label_tree <- scene_gtree(scene, "dag_labels_auto")

  edges <- label_edge_input(label_tree, scene)
  expect_length(edges, 3)

  for (edge in edges) {
    expect_true(all(is.na(edge$route_style)))
    last <- nrow(edge)
    chord_x <- c(edge$x[[1]], edge$x[[last]])
    chord_y <- c(edge$y[[1]], edge$y[[last]])
    expect_lt(max(polyline_dist(edge$x, edge$y, chord_x, chord_y)), 1e-8)
  }
})

test_that("the cap the layer draws with is the cap the label grob routes with", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # One cap, read from one place. This layer resects its arrowheads by 12 mm
  # rather than by the node cap, and the router keeps that many millimetres of
  # straight arm at each end so the head still arrives radially. A label grob
  # routing with any other number would model a line the reader never sees.
  p <- ggplot(tidy_dagitty(collinear_mediator_dag()), aes_dag()) +
    geom_dag_routed_arrows(resect = 12) +
    geom_dag_point() +
    geom_dag_label_auto(aes(label = label))

  scene <- forced_panel_scene(p, "dag_routed_edges|dag_labels_auto")
  routed_tree <- scene_gtree(scene, "dag_routed_edges")
  label_tree <- scene_gtree(scene, "dag_labels_auto")

  edges <- label_edge_input(label_tree, scene)
  routed <- edges[vapply(
    edges,
    function(edge) !is.na(edge$route_style[[1]]),
    logical(1)
  )]
  expect_length(routed, 3)

  # the resect the layer draws with, not the node cap the label engine trims
  # its obstacles by, is what the spec carries
  for (edge in routed) {
    expect_equal(as.numeric(label_route_spec(label_tree, edge)$cap), 12)
  }

  lengths <- vapply(
    routed,
    function(edge) {
      last <- nrow(edge)
      sqrt(
        (edge$x[[last]] - edge$x[[1]])^2 + (edge$y[[last]] - edge$y[[1]])^2
      )
    },
    numeric(1)
  )
  blocked <- routed[[which.max(lengths)]]

  label_path <- route_label_edge(label_tree, scene, blocked)
  drawn_path <- drawn_path_for(routed_tree, blocked)

  parity <- if (is.null(label_path) || is.null(drawn_path)) {
    Inf
  } else {
    hausdorff_mm(trim_by_cap(label_path, 12), trim_by_cap(drawn_path, 12))
  }

  expect_lt(parity, 0.5)
})

# The longest of a set of edge inputs, which on the collinear mediator is the
# blocked one.
longest_edge <- function(edges) {
  lengths <- vapply(
    edges,
    function(edge) {
      last <- nrow(edge)
      sqrt(
        (edge$x[[last]] - edge$x[[1]])^2 + (edge$y[[last]] - edge$y[[1]])^2
      )
    },
    numeric(1)
  )
  edges[[which.max(lengths)]]
}

# The edges of a label grob's input that a routed layer draws.
routed_label_edges <- function(tree, scene) {
  edges <- label_edge_input(tree, scene)
  edges[vapply(
    edges,
    function(edge) !is.na(edge$route_style[[1]]),
    logical(1)
  )]
}

test_that("the cap is the finished plot's however the layers were ordered", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # The label layer is written first and the node layer last, so when the
  # label layer is added neither the routed layer nor the node size that
  # settles its resection is on the plot. The cap belongs to the finished
  # plot, not to the part of it a layer happened to be added to, so both
  # grobs read the same 11 mm from a node size of 24, 2 mm beyond its 9 mm
  # radius, and draw one line.
  p <- ggplot(tidy_dagitty(collinear_mediator_dag()), aes_dag()) +
    geom_dag_label_auto(aes(label = label)) +
    geom_dag_routed_arrows() +
    geom_dag_point(size = 24)

  scene <- forced_panel_scene(p, "dag_routed_edges|dag_labels_auto")
  routed_tree <- scene_gtree(scene, "dag_routed_edges")
  label_tree <- scene_gtree(scene, "dag_labels_auto")

  expect_equal(as.numeric(routed_tree$params$resect$head), 11)

  routed <- routed_label_edges(label_tree, scene)
  expect_length(routed, 3)
  for (edge in routed) {
    expect_equal(as.numeric(label_route_spec(label_tree, edge)$cap), 11)
  }

  blocked <- longest_edge(routed)
  label_path <- route_label_edge(label_tree, scene, blocked)
  drawn_path <- drawn_path_for(routed_tree, blocked)

  parity <- if (is.null(label_path) || is.null(drawn_path)) {
    Inf
  } else {
    hausdorff_mm(trim_by_cap(label_path, 11), trim_by_cap(drawn_path, 11))
  }

  # a cap read from the option instead of the plot leaves the arms of the
  # rebuilt path 0.11 mm off the drawn ones, so the threshold is tighter
  # than the half millimetre the other parity blocks allow
  expect_lt(parity, 0.05)
})

test_that("two routed layers each draw with the object they were given", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # One plot, two routed layers, one options object each: x -> y detours
  # around the mediator under a 6 mm clearance and the two short edges are
  # drawn under 2 mm. Each grob routes with its own layer's object, and the
  # label grob is handed both, one per edge, so its obstacles are the two
  # pictures the reader sees rather than one object applied to all three.
  wide <- edge_route_options(clearance = 6)
  narrow <- edge_route_options(clearance = 2)
  blocked_rows <- function(data) {
    data[!is.na(data$to) & data$name == "x" & data$to == "y", , drop = FALSE]
  }
  other_rows <- function(data) {
    data[!is.na(data$to) & !(data$name == "x" & data$to == "y"), , drop = FALSE]
  }

  p <- ggplot(tidy_dagitty(collinear_mediator_dag()), aes_dag()) +
    geom_dag_routed_arrows(
      data_directed = blocked_rows,
      edge_route_options = wide
    ) +
    geom_dag_routed_arrows(
      data_directed = other_rows,
      edge_route_options = narrow
    ) +
    geom_dag_point() +
    geom_dag_label_auto(aes(label = label))

  scene <- forced_panel_scene(p, "dag_routed_edges|dag_labels_auto")
  routed_trees <- scene$grobs[grepl("dag_routed_edges", names(scene$grobs))]
  expect_length(routed_trees, 2)
  label_tree <- scene_gtree(scene, "dag_labels_auto")

  drawn <- vapply(
    routed_trees,
    function(tree) tree$params$edge_route_options$clearance,
    numeric(1)
  )
  expect_setequal(drawn, c(2, 6))

  routed <- routed_label_edges(label_tree, scene)
  expect_length(routed, 3)

  # the whole object identifies the routing, so the two kinds are told apart
  # by the key the engine groups its router calls with
  keys <- vapply(
    routed,
    function(edge) route_options_keys(edge[1, , drop = FALSE]),
    character(1)
  )
  expect_length(unique(keys), 2L)
  clearances <- vapply(
    routed,
    function(edge) label_route_options(edge)$clearance,
    numeric(1)
  )
  expect_equal(sort(clearances), c(2, 2, 6))

  cap <- label_tree$params$edge_cap %||% 8
  for (tree in routed_trees) {
    options <- tree$params$edge_route_options
    mine <- routed[vapply(
      routed,
      function(edge) identical(label_route_options(edge), options),
      logical(1)
    )]
    for (edge in mine) {
      label_path <- route_label_edge(label_tree, scene, edge)
      drawn_path <- drawn_path_for(tree, edge)
      expect_lt(
        hausdorff_mm(
          trim_by_cap(label_path, cap),
          trim_by_cap(drawn_path, cap)
        ),
        0.5
      )
    }
  }

  # and the wider clearance is the one the blocked edge was drawn with: it
  # leaves the mediator's centre by the disc plus the 6 mm asked for
  blocked <- longest_edge(routed)
  expect_equal(label_route_options(blocked)$clearance, 6)
  expect_gte(
    mid_chord_deviation(route_label_edge(label_tree, scene, blocked)),
    node_radius_mm(16) + 6 - 0.1
  )
})

# The fan of the router's own fixtures, in millimetres, as the label grob
# hands it to `route_label_obstacles()`: one row per endpoint of each edge,
# every edge tagged as orthogonal. b->e arrives at e's W port on the row
# sep_e above its centre; c->e is level and keeps the centre.
label_fan_inputs <- function() {
  nodes <- data.frame(
    name = c("a", "b", "c", "d", "e"),
    x = c(20, 80, 80, 80, 140),
    y = c(55, 85, 55, 25, 55),
    radius = 6,
    stringsAsFactors = FALSE
  )
  from <- c("a", "a", "a", "b", "c", "a")
  to <- c("b", "c", "d", "e", "e", "e")
  at <- function(names, column) nodes[[column]][match(names, nodes$name)]
  edges <- data.frame(
    edge_id = rep(paste0(from, "->", to), each = 2),
    x = as.numeric(rbind(at(from, "x"), at(to, "x"))),
    y = as.numeric(rbind(at(from, "y"), at(to, "y"))),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    route_style = rep("orthogonal", nrow(edges)),
    route_fixed = FALSE,
    route_layer_axis = NA_character_,
    route_cap = 8,
    curvature = NA_real_,
    stringsAsFactors = FALSE
  )
  spec$route_options <- rep(list(edge_route_options()), nrow(spec))
  list(
    nodes = nodes,
    edges = edges,
    spec = spec,
    par = list(node_size = 16, edge_cap = 8),
    bounds = c(0, 0, 160, 110)
  )
}

test_that("the label engine hides exactly the head each routed edge draws", {
  # `label_ink()` drops the part of every path the arrow layer resects, so
  # what is left is the ink the reader sees. The resect is per edge: an
  # arrival on an offset row is cut by cap - r + sqrt(r^2 - offset^2), which
  # is 6.8 mm on the fan's 3.6 mm row against the 8 mm cap, so trimming by
  # the cap would hide 1.2 mm of drawn head and let a box sit on it. The
  # engine takes each edge's resect from the router instead.
  input <- label_fan_inputs()
  routed <- route_label_obstacles(
    input$edges,
    input$spec,
    input$nodes,
    input$par,
    input$bounds
  )

  cap <- input$par$edge_cap
  radius <- node_radius_mm(input$par$node_size)
  offset_resect <- cap - radius + sqrt(radius^2 - 3.6^2)
  expect_equal(offset_resect, 6.8, tolerance = 1e-9)

  # the router's per-edge resects travel out with the paths, one value per
  # edge
  expect_true(all(c("cap_head", "cap_fins") %in% names(routed)))
  resect_of <- function(column, id) {
    values <- routed[[column]][routed$edge_id == id]
    if (length(values) == 0) {
      return(NA_real_)
    }
    expect_length(unique(values), 1)
    values[[1]]
  }
  expect_equal(resect_of("cap_head", "b->e"), offset_resect)
  expect_equal(resect_of("cap_fins", "b->e"), cap)
  expect_equal(resect_of("cap_head", "c->e"), cap)
  expect_equal(resect_of("cap_fins", "c->e"), cap)

  # the ink of the offset arrival reaches to its own resect and no further:
  # the head it hides is the head the arrow layer draws
  ink <- label_ink(routed, cap)
  reach <- function(id) {
    path <- routed[routed$edge_id == id, , drop = FALSE]
    points <- ink[ink$edge_id == id, , drop = FALSE]
    expect_gt(nrow(points), 0)
    last <- nrow(path)
    min(sqrt(
      (points$x - path$x[[last]])^2 + (points$y - path$y[[last]])^2
    ))
  }
  expect_gt(reach("b->e"), offset_resect)
  expect_lte(reach("b->e"), offset_resect + label_ink_spacing)
  # a centre port is unchanged: its head ends on the cap line
  expect_gt(reach("c->e"), cap)
  expect_lte(reach("c->e"), cap + label_ink_spacing)
})

# Panels and pinned edges ------------------------------------------------------

test_that("each panel routes the edges that panel draws, once each", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")

  # The routing spec is discovered from the layer's data, which holds every
  # panel at once, so a chord several panels share is matched by each of them
  # once per panel that draws it. The router reads a repeated chord as a
  # bundle of parallel edges and spreads the copies apart, so the label
  # engine's obstacle would be a fan of detours where one line is drawn.
  p <- ggdag_equivalent_dags(
    collinear_mediator_dag(),
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )

  scenes <- forced_panel_scenes(
    p,
    "dag_routed_edges|dag_labels_auto",
    width = 10,
    height = 6
  )
  expect_length(scenes, 6)

  for (scene in scenes) {
    routed_tree <- scene_gtree(scene, "dag_routed_edges")
    label_tree <- scene_gtree(scene, "dag_labels_auto")

    # the panel draws three edges, so the stat carries three routed edges and
    # the two chord endpoints of each
    spec <- label_tree$edges[!is.na(label_tree$edges$route_style), ]
    expect_equal(nrow(spec), 6)
    expect_length(unique(spec$edge_id), 3)

    obstacles <- label_routed_obstacles(label_tree, scene)
    expect_length(obstacles, 3)

    spans <- vapply(obstacles, span_mm, numeric(1))
    by_span <- order(spans)

    # the mediator blocks only the long chord, so the two short edges come
    # back as the chords they are drawn as
    for (edge in obstacles[by_span[1:2]]) {
      last <- nrow(edge)
      chord_x <- c(edge$x[[1]], edge$x[[last]])
      chord_y <- c(edge$y[[1]], edge$y[[last]])
      expect_lt(
        max(polyline_dist(edge$x, edge$y, chord_x, chord_y)),
        1e-6
      )
    }

    # and the blocked one is the detour this panel's arrow grob drew, not a
    # member of a bundle the repetition invented
    blocked <- obstacles[[by_span[[3]]]]
    drawn <- drawn_path_for(routed_tree, blocked)
    expect_gte(mid_chord_deviation(blocked), 6)
    expect_lt(hausdorff_mm(blocked, drawn), 0.5)
  }
})

test_that("an edge the user pinned is shown to the router as it is drawn", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # The router prices a detour by what it would cross and how crowded each
  # side already is, so an edge it never reroutes still decides which way the
  # edges it does reroute go. The label engine has to hand it the same set of
  # edges the drawn grob does: an arc the user set at the curvature it is
  # drawn at, and an edge pinned straight by an explicit zero as a chord.
  coords <- list(
    x = c(x = 0, m = 1, y = 2, z = 1),
    y = c(x = 0, m = 0, y = 0, z = 2)
  )

  for (curvature in c(-0.3, 0.3, 0)) {
    dag <- dagify(
      y ~ x + m,
      m ~ x,
      z ~ x,
      labels = c(x = "Exposure", m = "Mediator", y = "Outcome", z = "Other"),
      coords = coords
    )
    p <- ggdag(
      curve_edge(tidy_dagitty(dag), "x", "z", curvature),
      edge_engine = "ggarrow",
      edge_route = "spline",
      use_labels = TRUE,
      label_geom = geom_dag_label_auto
    )

    scene <- forced_panel_scenes(p, "dag_routed_edges|dag_labels_auto")[[1]]
    routed_tree <- scene_gtree(scene, "dag_routed_edges")
    label_tree <- scene_gtree(scene, "dag_labels_auto")

    # the pinned edge reaches the grob as the path it is drawn along, tagged
    # with the curvature it is drawn at rather than with a routing spec
    pinned <- label_tree$edges[
      !is.na(label_tree$edges$route_fixed) & label_tree$edges$route_fixed,
    ]
    expect_length(unique(pinned$edge_id), 1)
    expect_true(all(is.na(pinned$route_style)))
    expect_equal(unique(pinned$curvature), curvature)

    obstacles <- label_routed_obstacles(label_tree, scene)
    blocked <- obstacles[[which.max(vapply(obstacles, span_mm, numeric(1)))]]
    drawn <- drawn_path_for(routed_tree, blocked)

    expect_gte(mid_chord_deviation(blocked), 6)
    expect_lt(hausdorff_mm(blocked, drawn), 0.5)
  }
})

# Visual -----------------------------------------------------------------------

test_that("label-auto visuals: labels keep off the routed detours", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("vdiffr")

  dag <- dagify(
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
  p <- ggdag(
    dag,
    edge_engine = "ggarrow",
    edge_route = "spline",
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ) +
    theme_dag()

  # A baseline is only worth keeping if the picture is right, so the placement
  # is measured before it is drawn: no label box may sit on a path the router
  # drew, over the part of that path the label engine treats as an obstacle.
  # The measurement is taken on the device and at the size vdiffr renders the
  # baseline with, so the geometry guarded here is the geometry drawn below.
  scene <- forced_panel_scenes(
    p,
    "dag_routed_edges|dag_labels_auto",
    width = 10,
    height = 8,
    device = "svglite"
  )[[1]]
  routed_tree <- scene_gtree(scene, "dag_routed_edges")
  drawn <- unlist(
    lapply(find_arrow_paths(routed_tree), arrow_grob_paths),
    recursive = FALSE
  )
  cap <- scene_gtree(scene, "dag_labels_auto")$params$edge_cap
  stopifnot(length(scene$boxes) == 5, length(drawn) == 6)
  for (box in scene$boxes) {
    for (path in drawn) {
      stopifnot(
        !path_meets_box(
          trim_by_cap(densify_mm_polyline(path), cap),
          box
        )
      )
    }
  }

  expect_doppelganger("label-auto-routed-skip-chain", p)
})

test_that("an unset node size falls back to the option in both grobs", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  local_ggdag_option_state()
  ggdag_options_set(node_size = 24)

  # There is no node layer to discover a size from, so both grobs fall back,
  # and they have to fall back to the same number: the router clears the
  # discs the plot would draw, and a label engine routing around a smaller
  # disc than the arrows did would model a line that is not there.
  p <- ggplot(tidy_dagitty(collinear_mediator_dag()), aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_label_auto(aes(label = label))

  scene <- forced_panel_scenes(p, "dag_routed_edges|dag_labels_auto")[[1]]
  label_tree <- scene_gtree(scene, "dag_labels_auto")
  routed_tree <- scene_gtree(scene, "dag_routed_edges")

  expect_equal(label_tree$params$node_size, 24)
  expect_equal(routed_tree$params$node_size, 24)

  obstacles <- label_routed_obstacles(label_tree, scene)
  blocked <- obstacles[[which.max(vapply(obstacles, span_mm, numeric(1)))]]
  expect_lt(
    hausdorff_mm(blocked, drawn_path_for(routed_tree, blocked)),
    0.5
  )
})
