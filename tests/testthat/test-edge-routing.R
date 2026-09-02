# Tests for the draw-time routed edge layer and the `edge_route` option.
#
# `GeomDAGRoutedArrow` is a plain ggplot2 geom. Its `draw_panel()` transforms
# the panel's node centres and edge endpoints to npc and returns a
# `dag_routed_edges` gTree; the gTree's `makeContent()` method converts those
# to millimetres, calls the pure router `route_edges_mm()`, and builds the
# arrows with `ggarrow::grob_arrow()` in millimetres. No geometry is decided
# in data space, so the panel aspect ratio never shears a detour and the
# picture re-routes when the plot is resized.
#
# The layer's data are the plain plot rows carrying a logical `.ggdag_draw`
# column that marks the rows the layer draws, mapped to the `draw` aesthetic,
# so scales and legends see exactly what every other DAG layer sees. The
# `edge_route` option ("straight", "spline", or "orthogonal") swaps the routed
# layer into `geom_dag()` and the quick plots under the ggarrow edge engine;
# the ggraph engine cannot route and says so.
#
# Geometry is therefore asserted on the drawn picture: the plot is rendered to
# an off-screen device, the grob tree is forced, and the realised arrow paths
# are read back in millimetres. The constants follow the router's own: the
# default node size 16 draws a disc of radius 6 mm and the default clearance
# margin is 3 mm, so a routed path clears an obstacle by 9 mm less the 0.1 mm
# verification tolerance.
#
# The `edge_route` validation test lives here rather than in test-options.R so
# that `_snaps/options.md` stays as it is; it asserts on the condition class
# and message directly rather than recording a snapshot.

r_node <- node_radius_mm(16)
r_full <- r_node + 3
verify_tol <- 0.1

# Helpers ----------------------------------------------------------------------

# Distance from each point to a single segment, clamped at the ends.
point_segment_dist <- function(px, py, x, y, xend, yend) {
  dx <- xend - x
  dy <- yend - y
  len_sq <- dx^2 + dy^2
  t <- if (len_sq == 0) {
    rep(0, length(px))
  } else {
    pmin(1, pmax(0, ((px - x) * dx + (py - y) * dy) / len_sq))
  }
  sqrt((px - (x + t * dx))^2 + (py - (y + t * dy))^2)
}

# Distance from each point to the nearest segment of the polyline through
# (poly_x, poly_y) in order.
polyline_dist <- function(px, py, poly_x, poly_y) {
  segments <- seq_len(length(poly_x) - 1)
  distances <- vapply(
    segments,
    function(i) {
      point_segment_dist(
        px,
        py,
        poly_x[i],
        poly_y[i],
        poly_x[i + 1],
        poly_y[i + 1]
      )
    },
    numeric(length(px))
  )
  if (is.null(dim(distances))) {
    return(min(distances))
  }
  apply(distances, 1, min)
}

# The mediation triangle with the mediator dead on the x -> y chord. Every
# node sits at y = 0, so the panel's y range is degenerate: this is the scene
# where a data-space router bows an edge entirely out of the panel.
mediator_dag <- function() {
  dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
}

# The same triangle with a latent common cause drawn as a bidirected arc.
latent_mediator_dag <- function() {
  dagify(
    y ~ x + m,
    m ~ x,
    u ~ ~v,
    coords = list(
      x = c(x = 0, m = 1, y = 2, u = 0, v = 2),
      y = c(x = 0, m = 0, y = 0, u = 2, v = 2)
    )
  )
}

# Three mediation triangles stacked well apart, so that each row of the scene
# can carry a curvature rule of its own.
stacked_triangles_dag <- function() {
  dagify(
    y1 ~ x1 + m1,
    m1 ~ x1,
    y2 ~ x2 + m2,
    m2 ~ x2,
    y3 ~ x3 + m3,
    m3 ~ x3,
    coords = list(
      x = c(
        x1 = 0,
        m1 = 1,
        y1 = 2,
        x2 = 0,
        m2 = 1,
        y2 = 2,
        x3 = 0,
        m3 = 1,
        y3 = 2
      ),
      y = c(
        x1 = 0,
        m1 = 0,
        y1 = 0,
        x2 = 5,
        m2 = 5,
        y2 = 5,
        x3 = 10,
        m3 = 10,
        y3 = 10
      )
    )
  )
}

# The positions of the layers `plot` draws routed edges with.
routed_layer_index <- function(plot) {
  which(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomDAGRoutedArrow"),
    logical(1)
  ))
}

# The one layer `plot` draws routed edges with, or NULL when it has none.
routed_layer_of <- function(plot) {
  idx <- routed_layer_index(plot)
  if (length(idx) != 1) {
    return(NULL)
  }
  plot$layers[[idx]]
}

# Evaluate `code` with an off-screen device open, so that grid unit
# conversions have a device to measure against and no file is left behind.
with_offscreen_device <- function(code, width = 7, height = 5) {
  file <- tempfile(fileext = ".png")
  ragg::agg_png(file, width = width, height = height, units = "in", res = 96)
  on.exit(
    {
      grDevices::dev.off()
      unlink(file)
    },
    add = TRUE
  )
  force(code)
}

# Draw `plot` to `file` on an off-screen raster device, so that
# `makeContent()` runs with the panel viewport in place. The device is closed
# on the way out whether or not the drawing succeeds, so the file is complete
# when this returns and no device is left behind when it is not.
render_offscreen <- function(plot, file, width = 7, height = 5) {
  ragg::agg_png(file, width = width, height = height, units = "in", res = 96)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  invisible(NULL)
}

# Draw `plot` off screen, discarding the picture.
draw_offscreen <- function(plot, width = 7, height = 5) {
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  render_offscreen(plot, file, width = width, height = height)
  invisible(NULL)
}

# The bytes of `plot` rendered to a PNG.
render_bytes <- function(plot, width = 7, height = 5) {
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  render_offscreen(plot, file, width = width, height = height)
  readBin(file, "raw", file.size(file))
}

# The panel a forced grob was drawn in, read from its grob path. The gtable
# names each panel's grob tree `panel-<i>`, where `i` is the panel index.
grob_panel <- function(path) {
  matched <- regmatches(path, regexpr("panel-[0-9]+\\.", path))
  if (length(matched) == 0) {
    return(NA_integer_)
  }
  as.integer(sub("panel-([0-9]+)\\.", "\\1", matched))
}

# Render `plot` off screen, force the grob tree so that every `makeContent()`
# method has run and its children are on the display list, and return the
# forced grobs whose own name matches `pattern`, ordered by panel and named by
# their full grob path.
force_panel_grobs <- function(plot, pattern, width = 7, height = 5) {
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

  paths <- grid::grid.grep(pattern, grep = TRUE, global = TRUE)
  paths <- vapply(paths, as.character, character(1))
  own_name <- sub(".*::", "", paths)
  matched <- paths[grepl(pattern, own_name)]

  grobs <- lapply(matched, grid::grid.get)
  names(grobs) <- matched
  grobs[order(vapply(matched, grob_panel, integer(1)))]
}

# The children of a forced gTree that draw with class `cl`.
find_grob_class <- function(gtree, cl) {
  children <- gtree$children
  if (length(children) == 0) {
    return(list())
  }
  unname(children[vapply(children, inherits, logical(1), what = cl)])
}

# The single `dag_routed_edges` gTree of a one-panel plot.
routed_gtree <- function(plot, ...) {
  gtrees <- force_panel_grobs(plot, "dag_routed_edges", ...)
  testthat::expect_length(gtrees, 1)
  gtrees[[1]]
}

# The drawn detour of the collinear mediator scene, read back from a forced
# `dag_routed_edges` gTree: the routed x -> y path and the mediator's centre,
# both in millimetres.
mediator_detour <- function(gtree) {
  arrows <- find_grob_class(gtree, "arrow_path")
  testthat::expect_length(arrows, 1)
  paths <- arrow_grob_paths(arrows[[1]])
  keys <- path_grid_keys(paths)
  short <- paths[[which(keys == "c1r1->c2r1")]]
  list(
    routed = paths[[which(keys == "c1r1->c3r1")]],
    centre = c(x = short$x[[2]], y = short$y[[2]])
  )
}

# A grid unit read back in millimetres. The routed grob is built in
# millimetres, so the conversion is the identity and does not depend on the
# device it is measured on.
unit_mm <- function(value, axis = c("x", "y")) {
  axis <- match.arg(axis)
  if (!grid::is.unit(value)) {
    return(as.numeric(value))
  }
  convert <- if (axis == "x") grid::convertX else grid::convertY
  with_offscreen_device(convert(value, "mm", valueOnly = TRUE))
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
# edge, in the order the grob draws them.
arrow_grob_paths <- function(grob) {
  ids <- arrow_grob_ids(grob)
  points <- data.frame(x = unit_mm(grob$x, "x"), y = unit_mm(grob$y, "y"))
  unname(split(points, factor(ids, levels = unique(ids))))
}

# Name each drawn path by the grid position of its endpoints, columns
# numbered left to right and rows bottom to top: "c1r1->c3r1" is the edge from
# the leftmost to the rightmost node of the bottom row. The fixtures here
# place their nodes on such a grid, so this identifies an edge without
# depending on the order the layer happens to draw in.
path_grid_keys <- function(paths) {
  ends <- function(column, at_end) {
    values <- vapply(
      paths,
      function(path) {
        path[[column]][if (at_end) nrow(path) else 1]
      },
      numeric(1)
    )
    round(values, 6)
  }
  x_from <- ends("x", FALSE)
  y_from <- ends("y", FALSE)
  x_to <- ends("x", TRUE)
  y_to <- ends("y", TRUE)

  columns <- sort(unique(c(x_from, x_to)))
  rows <- sort(unique(c(y_from, y_to)))
  paste0(
    "c",
    match(x_from, columns),
    "r",
    match(y_from, rows),
    "->c",
    match(x_to, columns),
    "r",
    match(y_to, rows)
  )
}

# The curvature a `curve_arrow` grob was built with.
curve_grob_curvature <- function(grob) {
  grob$curve$curvature %||% grob$curvature
}

# The label boxes of a forced `dag_labels_auto` gTree, in millimetres. Forcing
# the tree runs `makeContent.roundrect()` on every box, which returns a
# polygon, so the boxes are found by name rather than by class. Their geometry
# is in the viewport `makeContext.roundrect()` attaches: the label engine
# passes no viewport of its own, so the box is centred on the viewport and its
# extent is the viewport's, in the millimetres the engine placed it in.
label_boxes_mm <- function(gtree) {
  children <- gtree$children
  if (length(children) == 0) {
    return(list())
  }
  names <- vapply(children, function(child) child$name %||% "", character(1))
  lapply(unname(children[grepl("roundrect", names)]), function(box) {
    center_x <- unit_mm(box$vp$x, "x")
    center_y <- unit_mm(box$vp$y, "y")
    width <- unit_mm(box$vp$width, "x")
    height <- unit_mm(box$vp$height, "y")
    c(
      xmin = center_x - width / 2,
      xmax = center_x + width / 2,
      ymin = center_y - height / 2,
      ymax = center_y + height / 2
    )
  })
}

# The polyline resampled at `spacing` millimetres, so that a box crossed by a
# segment is caught by a point inside it.
densify_polyline <- function(x, y, spacing = 0.5) {
  dense_x <- x[1]
  dense_y <- y[1]
  for (i in seq_len(length(x) - 1)) {
    dx <- x[i + 1] - x[i]
    dy <- y[i + 1] - y[i]
    steps <- max(1, ceiling(sqrt(dx^2 + dy^2) / spacing))
    fraction <- seq_len(steps) / steps
    dense_x <- c(dense_x, x[i] + fraction * dx)
    dense_y <- c(dense_y, y[i] + fraction * dy)
  }
  data.frame(x = dense_x, y = dense_y)
}

# Do any of `points` fall inside `box`?
points_in_box <- function(points, box) {
  any(
    points$x >= box[["xmin"]] &
      points$x <= box[["xmax"]] &
      points$y >= box[["ymin"]] &
      points$y <= box[["ymax"]]
  )
}

# How many edges each panel of `plot` draws routed arrows for.
routed_ids_per_panel <- function(plot, ...) {
  gtrees <- force_panel_grobs(plot, "dag_routed_edges", ...)
  vapply(
    gtrees,
    function(gtree) {
      arrows <- find_grob_class(gtree, "arrow_path")
      if (length(arrows) == 0) {
        return(0L)
      }
      length(unique(arrow_grob_ids(arrows[[1]])))
    },
    integer(1)
  )
}

# The layer anatomy --------------------------------------------------------------

test_that("geom_dag_routed_arrows(): the layer draws the plot rows and marks the directed ones", {
  skip_if_not_installed("ggarrow")

  tidy_dag <- tidy_dagitty(latent_mediator_dag())
  p <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  layer <- routed_layer_of(p)
  expect_false(is.null(layer))
  expect_identical(count_geom_layers(p, "GeomDAGArrowCurve"), 1L)
  expect_true(uses_ggarrow_edges(p))

  # every decision is made at draw time, so the layer computes nothing before
  # then and inherits the plot's DAG mapping like any other layer
  expect_true(inherits(layer$stat, "StatIdentity"))
  expect_true(layer$inherit.aes)
  expect_false(is.null(layer$mapping$draw))

  # the layer data are the plain plot rows: the router needs every node of the
  # panel as an obstacle, and the scales must see the rows they always saw
  expect_true(is.function(layer$data))
  plot_data <- pull_dag_data(tidy_dag)
  resolved <- layer$data(plot_data)
  expect_identical(nrow(resolved), nrow(plot_data))
  expect_contains(
    names(resolved),
    c("name", "x", "y", "xend", "yend", ".ggdag_draw")
  )
  expect_true(is.logical(resolved$.ggdag_draw))

  # and `.ggdag_draw` marks exactly the directed edges: node rows and the
  # bidirected pair, which the arc layer draws, are carried but not drawn
  directed <- !is.na(plot_data$to) &
    as.character(plot_data$direction) == "->"
  expect_identical(resolved$.ggdag_draw, directed)
  expect_identical(sum(resolved$.ggdag_draw), 3L)

  # one arrowhead at the end of the path and none at the start, by default
  arrow <- layer$geom_params$arrow
  expect_false(is.null(arrow$head))
  expect_null(arrow$fins)
})

test_that("geom_dag_routed_arrows(): the signature routes in device units", {
  skip_if_not_installed("ggarrow")

  arg_names <- names(formals(geom_dag_routed_arrows))
  expect_contains(arg_names, c("route", "clearance", "edge_sep", "layer_axis"))

  # the routing radius is the drawn node size in millimetres now, so there is
  # no data-space radius left to set
  expect_false("node_radius" %in% arg_names)

  # `route` and `layer_axis` may be written as a single default or as the full
  # set of choices for `match.arg()`; either way the first value is the default
  route <- eval(formals(geom_dag_routed_arrows)$route)
  expect_identical(route[[1]], "spline")
  expect_contains(route, "orthogonal")
  expect_identical(
    eval(formals(geom_dag_routed_arrows)$layer_axis)[[1]],
    "auto"
  )

  # clearance and edge separation are millimetre overrides of the router's own
  # defaults, so they are unset unless the user says otherwise
  expect_null(eval(formals(geom_dag_routed_arrows)$clearance))
  expect_null(eval(formals(geom_dag_routed_arrows)$edge_sep))
})

test_that("geom_dag_routed_arrows(): the routing mode reaches the layer", {
  skip_if_not_installed("ggarrow")

  # only the parameter is pinned here: drawing an orthogonal route is the
  # router's own round, and until then the mode errors when it is drawn
  p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
    geom_dag_routed_arrows(route = "orthogonal") +
    geom_dag_point()

  expect_identical(routed_layer_of(p)$geom_params$route, "orthogonal")
})

test_that("geom_dag_routed_arrows(): resects to the node size and routes around it", {
  skip_if_not_installed("ggarrow")

  p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
    geom_dag_point() +
    geom_dag_routed_arrows()

  layer <- routed_layer_of(p)
  expect_equal(layer$geom_params$resect$head, node_size_to_cap(16))
  expect_equal(layer$geom_params$resect$fins, node_size_to_cap(16))

  # the same discovery gives the router the radius of the discs it must clear
  expect_equal(layer$geom_params$node_size, 16)

  # a bigger node moves both
  bigger <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
    geom_dag_point(size = 24) +
    geom_dag_routed_arrows()

  bigger_layer <- routed_layer_of(bigger)
  expect_equal(bigger_layer$geom_params$resect$head, node_size_to_cap(24))
  expect_equal(bigger_layer$geom_params$resect$fins, node_size_to_cap(24))
  expect_equal(bigger_layer$geom_params$node_size, 24)
})

# The drawn grob -----------------------------------------------------------------

test_that("the routed layer draws one arrow path grob in millimetres", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  gtree <- routed_gtree(p)
  arrows <- find_grob_class(gtree, "arrow_path")
  expect_length(arrows, 1)
  expect_length(find_grob_class(gtree, "curve_arrow"), 0)

  # the drawn geometry is in millimetres, the units the router decides in
  expect_true(all(grid::unitType(arrows[[1]]$x) == "mm"))
  expect_true(all(grid::unitType(arrows[[1]]$y) == "mm"))

  # one id per directed edge, and the ids are the edges of the DAG
  paths <- arrow_grob_paths(arrows[[1]])
  expect_length(paths, 3)
  keys <- path_grid_keys(paths)
  expect_setequal(keys, c("c1r1->c2r1", "c2r1->c3r1", "c1r1->c3r1"))

  # the two unblocked edges are drawn as two-point chords
  expect_identical(nrow(paths[[which(keys == "c1r1->c2r1")]]), 2L)
  expect_identical(nrow(paths[[which(keys == "c2r1->c3r1")]]), 2L)

  # the x -> y edge has the mediator on its chord, so it is routed
  routed <- paths[[which(keys == "c1r1->c3r1")]]
  expect_gt(nrow(routed), 2)

  # paths run centre to centre and are unclipped: the arrow grob resects them
  short <- paths[[which(keys == "c1r1->c2r1")]]
  expect_equal(routed$x[[1]], short$x[[1]])
  expect_equal(routed$y[[1]], short$y[[1]])

  # and the detour clears the mediator disc by the full clearance margin
  mediator_x <- short$x[[2]]
  mediator_y <- short$y[[2]]
  expect_gte(
    min(polyline_dist(mediator_x, mediator_y, routed$x, routed$y)),
    r_full - verify_tol
  )
})

test_that("curvature the user set is drawn as an arrow curve and never rerouted", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # three mediation triangles, each with its mediator dead on the x -> y
  # chord: the bottom row leaves its curvature unset, the middle row asks for
  # an arc of 0.4, and the top row pins its long edge straight
  tidy_dag <- tidy_dagitty(stacked_triangles_dag()) |>
    dplyr::mutate(
      edge_curvature = dplyr::case_when(
        name == "x2" & to == "y2" ~ 0.4,
        name == "x3" & to == "y3" ~ 0,
        .default = NA_real_
      )
    )

  p <- ggplot(tidy_dag, aes_dag(edge_curvature = edge_curvature)) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  gtree <- routed_gtree(p)
  arrows <- find_grob_class(gtree, "arrow_path")
  expect_length(arrows, 1)

  # the arc is drawn by a curve grob of its own, so it is pixel for pixel the
  # arc the un-routed layer would draw; every other edge is an arrow path
  paths <- arrow_grob_paths(arrows[[1]])
  keys <- path_grid_keys(paths)
  expect_setequal(
    keys,
    c(
      "c1r1->c2r1",
      "c2r1->c3r1",
      "c1r1->c3r1",
      "c1r2->c2r2",
      "c2r2->c3r2",
      "c1r3->c2r3",
      "c2r3->c3r3",
      "c1r3->c3r3"
    )
  )

  # unset curvature routes around the mediator
  expect_gt(nrow(paths[[which(keys == "c1r1->c3r1")]]), 2)

  # an explicit zero wins: the edge stays straight through its mediator
  expect_identical(nrow(paths[[which(keys == "c1r3->c3r3")]]), 2L)

  # and the short edges, which nothing blocks, are chords
  short_keys <- grepl("c1r[0-9]->c2r[0-9]|c2r[0-9]->c3r[0-9]", keys)
  expect_true(all(vapply(paths[short_keys], nrow, integer(1)) == 2L))

  curves <- find_grob_class(gtree, "curve_arrow")
  expect_length(curves, 1)
  expect_equal(curve_grob_curvature(curves[[1]]), 0.4)

  # the arc starts where its own edge starts, not where a detour would
  middle_row <- paths[[which(keys == "c1r2->c2r2")]]
  expect_equal(unit_mm(curves[[1]]$curve$x1, "x")[[1]], middle_row$x[[1]])
  expect_equal(unit_mm(curves[[1]]$curve$y1, "y")[[1]], middle_row$y[[1]])
})

test_that("clearance widens the corridor the drawn path keeps", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
    geom_dag_routed_arrows(clearance = 4) +
    geom_dag_point()

  expect_equal(routed_layer_of(p)$geom_params$clearance, 4)

  drawn <- mediator_detour(routed_gtree(p))

  # the clearance is millimetres of daylight beyond the disc, so the drawn
  # path stays at least the node radius plus the clearance from the centre
  expect_gte(
    min(polyline_dist(
      drawn$centre[["x"]],
      drawn$centre[["y"]],
      drawn$routed$x,
      drawn$routed$y
    )),
    r_node + 4 - verify_tol
  )
})

test_that("routing is deterministic, redone at each size, and uses no randomness", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  withr::local_preserve_seed()
  p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  invisible(stats::runif(1))
  seed_before <- get(".Random.seed", envir = globalenv())

  expect_identical(render_bytes(p), render_bytes(p))
  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)

  # the routing is redone every time the plot is drawn, so a device of another
  # shape gets a detour of its own that clears the mediator just the same
  for (size in list(c(4, 3), c(10, 6))) {
    drawn <- mediator_detour(
      routed_gtree(p, width = size[[1]], height = size[[2]])
    )
    expect_gt(nrow(drawn$routed), 2)
    expect_gte(
      min(polyline_dist(
        drawn$centre[["x"]],
        drawn$centre[["y"]],
        drawn$routed$x,
        drawn$routed$y
      )),
      r_full - verify_tol
    )
  }
})

test_that("routed edges warn under a non-linear coordinate system", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # the router works in the millimetres of a linear panel; a coordinate system
  # that bends the panel cannot be routed, and the picture says so rather than
  # drawing a detour that means nothing
  p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point() +
    coord_polar()

  expect_warning(draw_offscreen(p), class = "ggdag_routed_coord_warning")
})

# Edge geometry discovery --------------------------------------------------------

test_that("a routed arrows layer is discovered as a routing spec", {
  skip_if_not_installed("ggarrow")

  tidy_dag <- tidy_dagitty(mediator_dag())
  p <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  geometry <- discover_edge_geometry(p)
  expect_false(is.null(geometry))
  routed <- geometry[geometry$type == "routed", , drop = FALSE]

  # the spec is one wide row per drawn edge, the same shape the arc and link
  # types are discovered with, plus how the edge is routed. Waypoints are not
  # communicated: both grobs call the same pure router on the same inputs.
  expect_identical(nrow(routed), 3L)
  expect_contains(
    names(routed),
    c("x", "y", "xend", "yend", "route_style", "route_clearance", "route_sep")
  )
  expect_true(all(routed$route_style == "spline"))

  # clearance and separation are the router's defaults unless the geom sets
  # them, and the spec says so rather than guessing a number
  expect_true(all(is.na(routed$route_clearance)))
  expect_true(all(is.na(routed$route_sep)))

  edges <- pull_dag_data(tidy_dag)
  edges <- edges[!is.na(edges$to), , drop = FALSE]
  expect_setequal(
    paste(routed$x, routed$y, routed$xend, routed$yend),
    paste(edges$x, edges$y, edges$xend, edges$yend)
  )
})

test_that("automatic labels keep clear of the routed path", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    labels = c(x = "Exposure", m = "Mediator", y = "Outcome"),
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
  p <- ggplot(tidy_dagitty(dag), aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point() +
    geom_dag_label_auto(aes(label = label))

  # both grobs are read from the same drawing, so the label engine's obstacles
  # are the millimetres the arrows were actually drawn in
  grobs <- force_panel_grobs(p, "dag_routed_edges|dag_labels_auto")
  routed_trees <- grobs[grepl("dag_routed_edges", names(grobs))]
  label_trees <- grobs[grepl("dag_labels_auto", names(grobs))]
  expect_length(routed_trees, 1)
  expect_length(label_trees, 1)

  drawn <- mediator_detour(routed_trees[[1]])
  expect_gt(nrow(drawn$routed), 2)

  boxes <- label_boxes_mm(label_trees[[1]])
  expect_length(boxes, 3)

  # the label engine shortens every edge by the node cap before it treats the
  # edge as an obstacle, so the millimetres the arrowhead is resected out of
  # are not part of the drawn edge either
  cap <- node_size_to_cap(16)
  points <- densify_polyline(drawn$routed$x, drawn$routed$y)
  last <- nrow(drawn$routed)
  to_ends <- pmin(
    sqrt(
      (points$x - drawn$routed$x[[1]])^2 + (points$y - drawn$routed$y[[1]])^2
    ),
    sqrt(
      (points$x - drawn$routed$x[[last]])^2 +
        (points$y - drawn$routed$y[[last]])^2
    )
  )
  points <- points[to_ends > cap, , drop = FALSE]
  expect_gt(nrow(points), 0)

  for (box in boxes) {
    expect_false(points_in_box(points, box))
  }
})

# The edge_route option ----------------------------------------------------------

test_that("edge_route option is registered, defaults to straight, and round-trips", {
  local_ggdag_option_state()

  expect_true("edge_route" %in% names(ggdag_defaults))
  expect_identical(ggdag_defaults$edge_route, "straight")
  expect_identical(ggdag_option("edge_route", "straight"), "straight")

  ggdag_options_set(edge_route = "spline")
  expect_identical(ggdag_option("edge_route", "straight"), "spline")

  ggdag_options_set(edge_route = "orthogonal")
  expect_identical(ggdag_option("edge_route", "straight"), "orthogonal")
})

test_that("edge_route option rejects anything but its three modes", {
  local_ggdag_option_state()
  # the registration has to exist before the message is asserted on, or the
  # unknown-option error would stand in for the validation message
  expect_true("edge_route" %in% names(ggdag_defaults))

  expect_error(
    ggdag_options_set(edge_route = "bogus"),
    class = "ggdag_type_error",
    regexp = '"straight", "spline", and "orthogonal"'
  )
  expect_error(
    ggdag_options_set(edge_route = NA),
    class = "ggdag_type_error"
  )
  expect_error(
    ggdag_options_set(edge_route = TRUE),
    class = "ggdag_type_error"
  )
  expect_error(
    ggdag_options_set(edge_route = c("spline", "straight")),
    class = "ggdag_type_error"
  )
})

# edge_route in the packaged edge rendering --------------------------------------

test_that("edge_route is straight by default: the packaged ggarrow edges stay chords", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  expect_identical(ggdag_defaults$edge_route, "straight")
  ggdag_options_set(edge_engine = "ggarrow")

  p <- ggdag(tidy_dagitty(mediator_dag()))
  expect_null(routed_layer_of(p))

  # the blocked edge is drawn as the straight chord by the curve layer
  curve_idx <- which(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomDAGArrowCurve"),
    logical(1)
  ))
  drawn <- dplyr::bind_rows(
    lapply(curve_idx, function(i) ggplot2::layer_data(p, i))
  )
  expect_identical(sum(drawn$x == 0 & drawn$xend == 2), 1L)
})

test_that("edge_route = 'spline': the routed geom draws the link and link_arc types", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")

  tidy_dag <- tidy_dagitty(mediator_dag())

  link_arc <- ggdag(tidy_dag)
  expect_false(is.null(routed_layer_of(link_arc)))

  # `link` draws every edge with one layer, and that layer routes too
  link <- ggdag(tidy_dag, edge_type = "link")
  expect_false(is.null(routed_layer_of(link)))

  # `geom_dag()` builds the same layer as the quick plotter
  assembled <- ggplot(tidy_dag, aes_dag()) + geom_dag()
  routed <- routed_layer_of(assembled)
  expect_false(is.null(routed))

  # and it draws the three directed edges of this DAG
  built <- ggplot2::layer_data(assembled, routed_layer_index(assembled))
  expect_identical(sum(built$draw & !is.na(built$xend)), 3L)
})

test_that("edge_route is a silent no-op for the arc and diagonal edge types", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")

  # these two bend every edge already, so there is nothing for the router to
  # do and nothing worth telling the user about
  for (edge_type in c("arc", "diagonal")) {
    expect_no_warning({
      p <- ggdag(tidy_dagitty(mediator_dag()), edge_type = edge_type)
    })
    expect_null(routed_layer_of(p))
  }
})

test_that("the ggraph engine says that edge routing is ignored", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggraph", edge_route = "spline")

  tidy_dag <- tidy_dagitty(mediator_dag())

  expect_warning(ggdag(tidy_dag), class = "ggdag_edge_route_warning")
  expect_warning(
    ggplot(tidy_dag, aes_dag()) + geom_dag(),
    class = "ggdag_edge_route_warning"
  )

  # once per plot, however many layers the plot builds
  warned <- testthat::capture_warnings(ggdag(tidy_dag))
  expect_length(warned, 1)
  expect_match(warned, "ggarrow")
})

test_that("the routed layer trains the scales on the nodes, as the straight one does", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow")

  panel_ranges <- function(route) {
    ggdag_options_set(edge_route = route)
    built <- ggplot2::ggplot_build(ggdag(tidy_dagitty(mediator_dag())))
    built$layout$panel_params[[1]][c("x.range", "y.range")]
  }

  # every node of this DAG sits at y = 0, so a layer that trained the scales
  # on routed geometry rather than on the nodes would blow the y range open
  expect_identical(panel_ranges("spline"), panel_ranges("straight"))
})

test_that("the routed layer adds no legend keys", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow")

  key_labels <- function(route) {
    ggdag_options_set(edge_route = route)
    p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag(colour = name)) +
      geom_dag()
    ggplot2::get_guide_data(p, "colour")$.label
  }

  # the layer carries the plot rows and no others, so a discrete scale gains
  # no key for rows that exist only to be routed around
  straight <- key_labels("straight")
  spline <- key_labels("spline")
  expect_identical(spline, straight)
  expect_false(anyNA(spline))
  expect_setequal(spline, c("m", "x", "y"))
})

# Faceting -----------------------------------------------------------------------

test_that("a faceted plot routes each panel's own edges", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # two panels over the same node positions: one is the mediation triangle
  # with its blocked x -> y chord, the other is the chain without it, so the
  # panels have different edge counts and a layer that leaks edges across
  # panels cannot match both
  coords <- list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  triangle <- pull_dag_data(tidy_dagitty(mediator_dag()))
  triangle$panel <- "mediation"
  chain <- pull_dag_data(tidy_dagitty(dagify(y ~ m, m ~ x, coords = coords)))
  chain$panel <- "chain"
  data <- dplyr::bind_rows(chain, triangle)

  p <- ggplot(data, aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point() +
    facet_wrap(~panel)

  built <- ggplot2::ggplot_build(p)$data[[routed_layer_index(p)]]
  drawn <- built[built$draw & !is.na(built$xend), , drop = FALSE]
  expect_identical(as.integer(table(drawn$PANEL)), c(2L, 3L))

  # the node rows travel with the panel as obstacles without being drawn
  expect_false(all(built$draw))

  # and each panel draws exactly its own edges
  expect_identical(unname(routed_ids_per_panel(p)), c(2L, 3L))
})

test_that("edge_route = 'spline': ggdag_equivalent_dags() routes each panel", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")

  p <- ggdag_equivalent_dags(mediator_dag())
  expect_false(is.null(routed_layer_of(p)))

  built <- ggplot2::ggplot_build(p)
  expect_identical(nrow(built$layout$layout), 6L)

  layer_df <- built$data[[routed_layer_index(p)]]
  drawn <- layer_df[layer_df$draw & !is.na(layer_df$xend), , drop = FALSE]
  expect_identical(as.integer(table(drawn$PANEL)), rep(3L, 6))

  # every equivalent DAG has the same three edges, drawn in its own panel
  expect_identical(unname(routed_ids_per_panel(p)), rep(3L, 6))
})
