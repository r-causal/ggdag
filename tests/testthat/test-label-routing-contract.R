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
  route_clearance = NA_real_,
  route_sep = NA_real_,
  route_layer_axis = "auto"
) {
  data.frame(
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
    route_clearance = route_clearance,
    route_sep = route_sep,
    route_layer_axis = route_layer_axis,
    stringsAsFactors = FALSE
  )
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

# Render `plot` on an off-screen raster device, force the grob tree so that
# every `makeContent()` method has run, and return the gTrees whose own name
# matches `pattern` together with the panel's size in millimetres. The
# measurement is taken inside the panel viewport, which is the viewport the
# two grobs measured themselves in, so `npc * width` is the millimetre a
# position was converted to at draw time.
forced_panel_scene <- function(plot, pattern, width = 7, height = 5) {
  file <- tempfile(fileext = ".png")
  ragg::agg_png(file, width = width, height = height, units = "in", res = 96)
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
  edges$route_clearance <- column_or_na(edges, "route_clearance", NA_real_)
  edges$route_sep <- column_or_na(edges, "route_sep", NA_real_)
  edges$route_layer_axis <- column_or_na(
    edges,
    "route_layer_axis",
    NA_character_
  )
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
  list(
    style = pick(c("route", "route_style"), edge$route_style),
    clearance = pick(c("clearance", "route_clearance"), edge$route_clearance),
    sep = pick(c("edge_sep", "route_sep"), edge$route_sep),
    layer_axis = pick(
      c("layer_axis", "route_layer_axis"),
      edge$route_layer_axis
    )
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
# sees.
trim_by_cap <- function(path, cap) {
  last <- nrow(path)
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
route_label_edge <- function(tree, scene, edge) {
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
  last <- nrow(edge)
  routed <- route_edges_mm(
    nodes = nodes,
    edges = data.frame(
      from = nearest(edge$x[[1]], edge$y[[1]]),
      to = nearest(edge$x[[last]], edge$y[[last]]),
      curvature = NA_real_,
      stringsAsFactors = FALSE
    ),
    bounds = c(0, 0, scene$width, scene$height),
    cap = tree$params$edge_cap %||% 8,
    mode = spec$style,
    opts = route_opts(
      r_ref = radius,
      m = if (is.na(spec$clearance)) NULL else spec$clearance,
      sep_e = if (is.na(spec$sep)) NULL else spec$sep,
      layer_axis = spec$layer_axis
    )
  )
  routed$paths[[1]]
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
    route_clearance = 4,
    route_sep = 2,
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
      "route_clearance",
      "route_sep",
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
  expect_equal(routed$route_clearance, c(4, 4))
  expect_equal(routed$route_sep, c(2, 2))
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
    routed_geometry(0, 0, 2, 0, route_clearance = 4, route_sep = 2),
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

  # the sentinels keep a missing routing spec a failed expectation rather
  # than an error, so the reason a red test is red stays legible
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

  # a parameter the geom leaves to the router is NA in the spec rather than a
  # guessed number, so the two calls agree on the default as well
  for (edge in routed) {
    spec <- label_route_spec(label_tree, edge)
    expect_equal(as.character(spec$style), routed_layer$geom_params$route)
    expect_equal(
      as.numeric(spec$clearance),
      as.numeric(routed_layer$geom_params$clearance %||% NA_real_)
    )
    expect_equal(
      as.numeric(spec$sep),
      as.numeric(routed_layer$geom_params$edge_sep %||% NA_real_)
    )
    expect_equal(
      as.character(spec$layer_axis),
      routed_layer$geom_params$layer_axis
    )
  }
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
