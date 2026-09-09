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

# A DAG laid out down the panel rather than across it: three time points
# along y, in two columns, with `m` sitting on the x -> y chord. Both axes
# hold exact clusters, so the router's own inference reads the two columns as
# the layers and sends the detour the wrong way; the layout knows better.
y_direction_dag <- function() {
  dagify(
    y ~ x + m,
    m ~ x,
    b ~ a,
    c ~ b,
    coords = time_ordered_coords(
      list(c("x", "a"), c("m", "b"), c("y", "c")),
      direction = "y",
      optimize = FALSE
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
# depending on the order the layer happens to draw in. Coordinates within a
# node radius of each other are one column or row: an orthogonal path ends at
# its port's axis point, up to r - head_w / 2 off its node's own line, and
# that is still the same node.
path_grid_keys <- function(paths, tol = r_node) {
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

  index_in <- function(values, all) {
    levels <- sort(unique(all))
    group <- cumsum(c(1L, as.integer(diff(levels) > tol)))
    group[match(values, levels)]
  }
  paste0(
    "c",
    index_in(x_from, c(x_from, x_to)),
    "r",
    index_in(y_from, c(y_from, y_to)),
    "->c",
    index_in(x_to, c(x_from, x_to)),
    "r",
    index_in(y_to, c(y_from, y_to))
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

# A collinear chain of five nodes with two skip edges. a->c and c->e each
# span a node, so both take S/N channels and meet at c's N side, where the
# arrival and the departure take ports sep_e / 2 either side of the centre
# line. The offset is what an offset port costs the arrowhead.
chain_skip_dag <- function() {
  dagify(
    b ~ a,
    c ~ b + a,
    d ~ c,
    e ~ d + c,
    coords = list(
      x = c(a = 0, b = 1, c = 2, d = 3, e = 4),
      y = c(a = 0, b = 0, c = 0, d = 0, e = 0)
    )
  )
}

# The point at arc length `s` from the end of a drawn path, where the
# arrowhead's tip lands once the layer resects `s` from that end.
path_arc_point <- function(path, s) {
  keep <- c(TRUE, abs(diff(path$x)) >= 1e-9 | abs(diff(path$y)) >= 1e-9)
  path <- path[keep, , drop = FALSE]
  path <- path[rev(seq_len(nrow(path))), , drop = FALSE]
  d <- c(0, cumsum(sqrt(diff(path$x)^2 + diff(path$y)^2)))
  k <- max(which(d <= s + 1e-12))
  f <- (s - d[[k]]) / (d[[k + 1L]] - d[[k]])
  c(
    path$x[[k]] + f * (path$x[[k + 1L]] - path$x[[k]]),
    path$y[[k]] + f * (path$y[[k + 1L]] - path$y[[k]])
  )
}

test_that("the routed layer resects each edge by the router's own arc length", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  p <- ggplot(tidy_dagitty(chain_skip_dag()), aes_dag()) +
    geom_dag_routed_arrows(route = "orthogonal") +
    geom_dag_point()

  gtree <- routed_gtree(p)
  arrows <- find_grob_class(gtree, "arrow_path")
  expect_length(arrows, 1)
  paths <- arrow_grob_paths(arrows[[1]])
  keys <- path_grid_keys(paths)

  # the resection reaches ggarrow per edge, in the millimetres the router
  # measured it in
  head <- arrows[[1]]$resect$head
  fins <- arrows[[1]]$resect$fins
  expect_true(all(grid::unitType(head) == "mm"))
  expect_true(all(grid::unitType(fins) == "mm"))
  expect_length(as.numeric(head), length(paths))
  expect_length(as.numeric(fins), length(paths))

  # a->c arrives at c's N port 1.8 mm off the centre line and c->e leaves
  # the other side of it; the disc face on that axis is sqrt(r^2 - 1.8^2) =
  # 5.724 mm from the centre, so each of them is resected by
  # cap - r + 5.724 = 7.724
  skip_edge <- which(keys == "c1r1->c3r1")
  back_edge <- which(keys == "c3r1->c5r1")
  expect_length(skip_edge, 1)
  expect_length(back_edge, 1)
  expect_equal(as.numeric(head)[[skip_edge]], 7.724, tolerance = 1e-3)
  expect_equal(as.numeric(fins)[[back_edge]], 7.724, tolerance = 1e-3)
  expect_equal(
    as.numeric(head)[-skip_edge],
    rep(node_size_to_cap(16), length(paths) - 1),
    tolerance = 1e-3
  )
  expect_equal(
    as.numeric(fins)[-back_edge],
    rep(node_size_to_cap(16), length(paths) - 1),
    tolerance = 1e-3
  )

  # the path ends at the port's axis point, 1.8 mm off c's centre line, and
  # the tip sits cap - r = 2 mm past the disc face on that axis: on the run
  # the head is drawn along, not angled at the centre
  skip_path <- paths[[skip_edge]]
  axis_point <- c(
    skip_path$x[[nrow(skip_path)]],
    skip_path$y[[nrow(skip_path)]]
  )
  tip <- path_arc_point(skip_path, as.numeric(head)[[skip_edge]])
  expect_equal(tip[[1]], axis_point[[1]], tolerance = 1e-3)
  face <- sqrt(r_node^2 - 1.8^2)
  expect_equal(
    abs(tip[[2]] - axis_point[[2]]),
    face + node_size_to_cap(16) - r_node,
    tolerance = 1e-3
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

  # the layer folds the millimetre argument into the object it carries
  expect_equal(
    routed_layer_of(p)$geom_params$edge_route_options$clearance,
    4
  )

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
    c("x", "y", "xend", "yend", "route_style", "route_options")
  )
  expect_true(all(routed$route_style == "spline"))

  # clearance and separation are the router's defaults unless the geom sets
  # them, and the object says so rather than guessing a number
  expect_true(all(vapply(
    routed$route_options,
    function(options) is.null(options$clearance) && is.null(options$edge_sep),
    logical(1)
  )))

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

# The routing options object -----------------------------------------------------

# `edge_route_options()` carries the constants the router draws with. A field
# left unset is derived when the plot is drawn, from the node size the plot
# uses, so the object cannot be resolved until the panel is measured; one
# function, `route_opts_from()`, turns it and a reference radius into the
# constants the router reads, and both the arrow grob and the label engine
# call that one function.
#
# The behaviour tests below hand scenes to `route_edges_mm()` directly, in the
# millimetres the router works in, so each option is measured against the
# geometry it moves rather than against a picture a layout may or may not
# produce. The entry-point tests above them fix the path from the user's call
# to those constants.

# A routing scene in millimetres: node discs of the drawn radius, chords
# between them, and the panel they sit in.
mm_scene <- function(names, x, y, from, to, bounds) {
  list(
    nodes = data.frame(
      name = names,
      x = x,
      y = y,
      r = r_node,
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = from,
      to = to,
      direction = "->",
      curvature = NA_real_,
      stringsAsFactors = FALSE
    ),
    bounds = bounds
  )
}

# Route `scene` with the options object a user would write, through the same
# translation the drawn grob makes.
route_mm <- function(scene, options = edge_route_options(), mode = "spline") {
  route_edges_mm(
    scene$nodes,
    scene$edges,
    scene$bounds,
    mode = mode,
    opts = route_opts_from(options, r_node)
  )
}

# The index of the edge named "from->to".
mm_edge <- function(scene, label) {
  match(label, paste0(scene$edges$from, "->", scene$edges$to))
}

# The closest the routed path of edge `i` comes to a node centre that is not
# one of its own endpoints.
mm_clearance <- function(scene, res, i) {
  others <- !scene$nodes$name %in% c(scene$edges$from[i], scene$edges$to[i])
  if (!any(others)) {
    return(Inf)
  }
  path <- res$paths[[i]]
  min(polyline_dist(
    scene$nodes$x[others],
    scene$nodes$y[others],
    path$x,
    path$y
  ))
}

# The smallest clearance any routed path of the scene keeps.
mm_min_clearance <- function(scene, res) {
  min(vapply(
    seq_len(nrow(scene$edges)),
    function(i) mm_clearance(scene, res, i),
    numeric(1)
  ))
}

# The y a path holds where it crosses `x0`.
mm_y_at <- function(path, x0) {
  path$y[[which.min(abs(path$x - x0))]]
}

# The turn in degrees at every interior vertex of a path, with the repeated
# points a sampled curve carries dropped.
mm_turning_angles <- function(path) {
  keep <- c(TRUE, abs(diff(path$x)) > 1e-9 | abs(diff(path$y)) > 1e-9)
  points <- path[keep, , drop = FALSE]
  dx <- diff(points$x)
  dy <- diff(points$y)
  if (length(dx) < 2) {
    return(numeric(0))
  }
  vapply(
    seq_len(length(dx) - 1),
    function(i) {
      abs(atan2(
        dx[i] * dy[i + 1] - dy[i] * dx[i + 1],
        dx[i] * dx[i + 1] + dy[i] * dy[i + 1]
      )) *
        180 /
        pi
    },
    numeric(1)
  )
}

# The angle in degrees between the chord of a path and the first `forward`
# millimetres of the path itself: how far off the chord the route departs.
mm_departure_angle <- function(path, forward = 8) {
  travelled <- cumsum(c(0, sqrt(diff(path$x)^2 + diff(path$y)^2)))
  at <- which(travelled >= forward)[[1]]
  last <- nrow(path)
  step <- c(path$x[[at]] - path$x[[1]], path$y[[at]] - path$y[[1]])
  chord <- c(path$x[[last]] - path$x[[1]], path$y[[last]] - path$y[[1]])
  abs(atan2(
    step[[1]] * chord[[2]] - step[[2]] * chord[[1]],
    step[[1]] * chord[[1]] + step[[2]] * chord[[2]]
  )) *
    180 /
    pi
}

# Scenes ------------------------------------------------------------------------

# The mediation triangle with the mediator dead on the x -> y chord.
mediator_mm <- function() {
  mm_scene(
    c("x", "m", "y"),
    c(7.3, 80, 152.7),
    55,
    c("x", "m", "x"),
    c("m", "y", "y"),
    c(0, 0, 160, 110)
  )
}

# A four-node chain on one row with all three of its skips drawn. The panel
# floor sits just under the row, so both of the skips over q share the slot
# above it rather than taking a side each.
skip_chain_mm <- function() {
  mm_scene(
    c("p", "q", "r", "s"),
    c(20, 60, 100, 140),
    55,
    c("p", "q", "r", "p", "q", "p"),
    c("q", "r", "s", "r", "s", "s"),
    c(0, 47, 160, 110)
  )
}

# One node pair with two edges between it, the parallel bundle.
parallel_pair_mm <- function() {
  mm_scene(
    c("a", "b"),
    c(30, 130),
    55,
    c("a", "a"),
    c("b", "b"),
    c(0, 0, 160, 110)
  )
}

# Four arrivals across one gap of the given width: the band the orthogonal
# ladder tightens rung by rung.
narrow_band_mm <- function(gap = 40) {
  half <- gap / 2
  mm_scene(
    c("a1", "a2", "a3", "a4", "b1", "b2", "b3", "b4"),
    c(rep(40 - half, 4), rep(40 + half, 4)),
    c(20, 35, 50, 65, 50, 65, 80, 95),
    c("a1", "a2", "a3", "a4"),
    c("b1", "b2", "b3", "b4"),
    c(0, 0, 80, 110)
  )
}

# A skip whose chord runs 62 degrees to the layer axis, past the boundary of
# the free-bow tier, and short enough for the sagitta cap to bind on it.
steep_skip_mm <- function() {
  mm_scene(
    c("a", "b", "c"),
    c(30, 55, 80),
    c(8, 55, 102),
    c("a", "b", "a"),
    c("b", "c", "c"),
    c(0, 0, 110, 110)
  )
}

# The same shape with the chord at 34 degrees, inside the layered tier.
shallow_skip_mm <- function() {
  mm_scene(
    c("a", "b", "c"),
    c(20, 80, 140),
    c(20, 60, 100),
    c("a", "b", "a"),
    c("b", "c", "c"),
    c(0, 0, 160, 120)
  )
}

# A 36 mm chord dead on one node: short enough that a detour around it wants
# to leave its own chord steeply.
short_skip_mm <- function() {
  mm_scene(
    c("s", "n", "t"),
    c(62, 80, 98),
    55,
    "s",
    "t",
    c(0, 0, 160, 110)
  )
}

# a -> b is blocked by m, with both sides of m equally far. u -> m arrives at
# m from above, so its drawn arrowhead occupies the side the tie would
# otherwise fall to.
head_zone_mm <- function() {
  mm_scene(
    c("a", "m", "b", "u"),
    c(20, 80, 140, 80),
    c(55, 55, 55, 95),
    c("u", "a"),
    c("m", "b"),
    c(0, 0, 160, 110)
  )
}

# One source fanning into three nodes of one layer and on to a fourth: a -> e
# is blocked, and both sides of the fan are equally far.
fan_mm <- function() {
  mm_scene(
    c("a", "b", "c", "d", "e"),
    c(20, 80, 80, 80, 140),
    c(55, 85, 55, 25, 55),
    c("a", "a", "a", "b", "c", "a"),
    c("b", "c", "d", "e", "e", "e"),
    c(0, 0, 160, 110)
  )
}

# A 120 mm chord dead on n1 and 9.5 mm from n2. The two centres are 16 mm
# apart: a curve can thread them at the soft margin but not at the full one.
tight_slot_mm <- function() {
  mm_scene(
    c("S", "T", "n1", "n2"),
    c(20, 140, 80, 80),
    c(50, 50, 43.5, 59.5),
    "S",
    "T",
    c(0, 0, 160, 110)
  )
}

# A span-4 chord across three crossed layers whose twelve interior edges weave
# over and under it: the tangle a saturating crossing price is tuned for.
tangle_mm <- function() {
  mm_scene(
    c(
      "s",
      "t",
      "a1",
      "a2",
      "b1",
      "b2",
      "c1",
      "c2",
      "u",
      "w"
    ),
    c(10, 150, 45, 45, 80, 80, 115, 115, 80, 80),
    c(50, 50, 30, 74, 50, 90, 30, 74, 108, 4),
    c(
      "s",
      "s",
      "a1",
      "a1",
      "a2",
      "b1",
      "b2",
      "b2",
      "c1",
      "c2",
      "u",
      "u",
      "u",
      "w",
      "w",
      "w",
      "s"
    ),
    c(
      "a1",
      "a2",
      "b1",
      "b2",
      "b2",
      "c1",
      "c1",
      "c2",
      "t",
      "t",
      "a1",
      "c1",
      "b2",
      "a2",
      "c2",
      "b1",
      "t"
    ),
    c(0, 0, 160, 110)
  )
}

# A five-step chain with one skip whose chord is steep enough for the free-bow
# tier and short enough for the sagitta cap to bind on it. The other five
# edges run straight, so the picture is one bow against a plain chain.
bow_skip_dag <- function() {
  dagify(
    b ~ a,
    c ~ b,
    d ~ c + b,
    e ~ d,
    f ~ e,
    coords = list(
      x = c(a = 0, b = 1, c = 2, d = 3, e = 4, f = 5),
      y = c(a = 3, b = 3, c = 1.5, d = 0, e = 0, f = 0)
    )
  )
}

# The scene the routed layer handed the router, read back off its forced gTree
# in millimetres: the node discs, the chords, the panel, and the cap the arrow
# layer resects. A predicate on this is a predicate on the picture a baseline
# would record.
routed_scene_mm <- function(plot, width = 10, height = 8) {
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

  paths <- grid::grid.grep("dag_routed_edges", grep = TRUE, global = TRUE)
  tree <- grid::grid.get(vapply(paths, as.character, character(1))[[1]])
  radius <- node_radius_mm(tree$params$node_size)

  list(
    nodes = data.frame(
      name = routed_position_keys(tree$nodes$x, tree$nodes$y),
      x = tree$nodes$x * panel_width,
      y = tree$nodes$y * panel_height,
      r = radius,
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = tree$edges$.ggdag_from,
      to = tree$edges$.ggdag_to,
      direction = "->",
      curvature = NA_real_,
      stringsAsFactors = FALSE
    ),
    bounds = c(0, 0, panel_width, panel_height),
    cap = routed_cap_mm(tree$edges, tree$params$resect),
    radius = radius
  )
}

# The router's own paths for a drawn scene under one options object.
route_drawn_scene <- function(scene, options, mode, layer_axis = "auto") {
  route_edges_mm(
    scene$nodes,
    scene$edges,
    scene$bounds,
    cap = scene$cap,
    mode = mode,
    opts = route_opts_from(options, scene$radius, layer_axis = layer_axis)
  )
}

# The axis-aligned runs a routed path is drawn as: one row per run, with the
# axis it holds, the coordinate it holds it at, and its length. The short
# segments a rounded corner is sampled as fall under `min_length` and are
# dropped, so a route reads the same here whether its corners are rounded or
# sharp.
mm_axis_runs <- function(path, min_length = 1, tol = 1e-6) {
  keep <- c(TRUE, abs(diff(path$x)) > tol | abs(diff(path$y)) > tol)
  points <- path[keep, , drop = FALSE]
  n <- nrow(points)
  if (n < 2) {
    return(data.frame(axis = character(0), coord = numeric(0), length = 0)[0, ])
  }
  dx <- diff(points$x)
  dy <- diff(points$y)
  axis <- ifelse(abs(dy) < tol, "h", ifelse(abs(dx) < tol, "v", "o"))
  coord <- ifelse(axis == "h", points$y[-n], points$x[-n])
  id <- cumsum(c(
    TRUE,
    axis[-1] != axis[-(n - 1)] | abs(diff(coord)) > tol
  ))
  first <- !duplicated(id)
  runs <- data.frame(
    axis = axis[first],
    coord = coord[first],
    length = as.numeric(tapply(sqrt(dx^2 + dy^2), id, sum)),
    stringsAsFactors = FALSE
  )
  runs[runs$axis != "o" & runs$length >= min_length, , drop = FALSE]
}

# A drawn scene names its nodes by position, so a scene laid out with a
# `coords` list is read back by ordering: layer first, then height. `names`
# is that order, and the result maps each name to the key the scene carries.
mm_scene_keys <- function(scene, names) {
  ordered <- scene$nodes$name[order(scene$nodes$x, scene$nodes$y)]
  stats::setNames(ordered, names)
}

# The height of a named node of a drawn scene, in millimetres.
mm_node_y <- function(scene, keys, name) {
  scene$nodes$y[[match(keys[[name]], scene$nodes$name)]]
}

# The index of the edge running between two named nodes of a drawn scene.
mm_named_edge <- function(scene, keys, from, to) {
  which(scene$edges$from == keys[[from]] & scene$edges$to == keys[[to]])
}

# The entry points ---------------------------------------------------------------

test_that("edge_route_options is spelled the same at every entry point", {
  skip_if_not_installed("ggarrow")

  arg_names <- names(formals(geom_dag_routed_arrows))
  expect_contains(arg_names, "edge_route_options")
  expect_null(eval(formals(geom_dag_routed_arrows)$edge_route_options))

  # the three millimetre formals stay where they are: they are per-call
  # overrides of the object's fields, not a second spelling of the object
  expect_null(eval(formals(geom_dag_routed_arrows)$clearance))
  expect_null(eval(formals(geom_dag_routed_arrows)$edge_sep))
  expect_null(eval(formals(geom_dag_routed_arrows)$edge_sep_min))

  # `geom_dag()` and `ggdag()` take the object beside `edge_route`, and both
  # default through the global option, as `edge_route` itself does
  for (fn in list(geom_dag, ggdag)) {
    expect_contains(names(formals(fn)), "edge_route_options")
    expect_identical(
      formals(fn)$edge_route_options,
      quote(ggdag_option("edge_route_options", NULL))
    )
  }

  # the quick plotters take it through the global option only
  expect_false("edge_route_options" %in% names(formals(ggdag_paths)))
})

test_that("a routed layer's own clearance overrides the object's field", {
  skip_if_not_installed("ggarrow")

  layer_options <- function(...) {
    p <- ggplot(tidy_dagitty(mediator_dag()), aes_dag()) +
      geom_dag_routed_arrows(...) +
      geom_dag_point()
    routed_layer_of(p)$geom_params$edge_route_options
  }

  # the object alone reaches the layer as it was written
  alone <- layer_options(
    edge_route_options = edge_route_options(clearance = 2, max_bow = 0.12)
  )
  expect_equal(alone$clearance, 2)
  expect_equal(alone$max_bow, 0.12)

  # an explicit formal wins the field of the same name and leaves the rest
  both <- layer_options(
    clearance = 4,
    edge_route_options = edge_route_options(clearance = 2, max_bow = 0.12)
  )
  expect_equal(both$clearance, 4)
  expect_equal(both$max_bow, 0.12)

  # and the formals alone still reach the router, through the same object
  formals_only <- layer_options(clearance = 4, edge_sep = 5, edge_sep_min = 2)
  expect_equal(formals_only$clearance, 4)
  expect_equal(formals_only$edge_sep, 5)
  expect_equal(formals_only$edge_sep_min, 2)
})

test_that("a layer's object beats the global option, which beats the router", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")

  tidy_dag <- tidy_dagitty(mediator_dag())

  # nothing set anywhere leaves every field to the router
  bare <- ggplot(tidy_dag, aes_dag()) + geom_dag()
  bare_options <- routed_layer_of(bare)$geom_params$edge_route_options
  expect_true(is.null(bare_options) || is.null(bare_options$max_bow))

  ggdag_options_set(edge_route_options = edge_route_options(max_bow = 0.12))

  global <- ggplot(tidy_dag, aes_dag()) + geom_dag()
  expect_equal(
    routed_layer_of(global)$geom_params$edge_route_options$max_bow,
    0.12
  )

  local <- ggplot(tidy_dag, aes_dag()) +
    geom_dag(edge_route_options = edge_route_options(max_bow = 0.08))
  expect_equal(
    routed_layer_of(local)$geom_params$edge_route_options$max_bow,
    0.08
  )
})

test_that("the object reaches the routed layer from every entry point", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")

  options <- edge_route_options(clearance = 4, max_bow = 0.12)
  tidy_dag <- tidy_dagitty(mediator_dag())
  reached <- function(plot) {
    routed_layer_of(plot)$geom_params$edge_route_options
  }

  direct <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_routed_arrows(edge_route_options = options) +
    geom_dag_point()
  expect_equal(reached(direct)$max_bow, 0.12)

  assembled <- ggplot(tidy_dag, aes_dag()) +
    geom_dag(edge_route_options = options)
  expect_equal(reached(assembled)$max_bow, 0.12)

  quick <- ggdag(tidy_dag, edge_route_options = options)
  expect_equal(reached(quick)$max_bow, 0.12)

  # the quick plotters carry no formal, so the global option is their route
  ggdag_options_set(edge_route_options = options)
  paths <- ggdag_paths(dagify(
    y ~ x + m,
    m ~ x,
    exposure = "x",
    outcome = "y"
  ))
  expect_equal(reached(paths)$max_bow, 0.12)
})

test_that("the object reaches route_edges_mm() from every entry point", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")

  options <- edge_route_options(clearance = 4, max_bow = 0.12)
  tidy_dag <- tidy_dagitty(mediator_dag())

  # the router is spied on rather than replaced: the picture is still drawn,
  # so the constants captured are the ones a real drawing worked from
  captured_opts <- function(plot) {
    captured <- list()
    original <- route_edges_mm
    local_mocked_bindings(
      route_edges_mm = function(...) {
        args <- list(...)
        captured[[length(captured) + 1L]] <<- args$opts
        do.call(original, args)
      }
    )
    draw_offscreen(plot)
    captured
  }

  plots <- list(
    geom_dag_routed_arrows = ggplot(tidy_dag, aes_dag()) +
      geom_dag_routed_arrows(edge_route_options = options) +
      geom_dag_point(),
    geom_dag = ggplot(tidy_dag, aes_dag()) +
      geom_dag(edge_route_options = options),
    ggdag = ggdag(tidy_dag, edge_route_options = options)
  )

  for (name in names(plots)) {
    seen <- captured_opts(plots[[name]])
    expect_gte(length(seen), 1)
    for (opts in seen) {
      expect_equal(opts$sagitta_max, 0.12, label = name)
      expect_equal(opts$m, 4, label = name)
    }
  }

  # and the same through the global option, which is all the quick plots read
  ggdag_options_set(edge_route_options = options)
  seen <- captured_opts(ggdag_paths(dagify(
    y ~ x + m,
    m ~ x,
    exposure = "x",
    outcome = "y"
  )))
  expect_gte(length(seen), 1)
  for (opts in seen) {
    expect_equal(opts$sagitta_max, 0.12)
    expect_equal(opts$m, 4)
  }
})

# What each option moves ---------------------------------------------------------

test_that("edge_route_options(): clearance widens the corridor the route keeps", {
  scene <- mediator_mm()
  i <- mm_edge(scene, "x->y")

  default <- route_mm(scene)
  wider <- route_mm(scene, edge_route_options(clearance = 6))

  expect_gte(mm_clearance(scene, default, i), r_node + 3 - verify_tol)
  expect_gte(mm_clearance(scene, wider, i), r_node + 6 - verify_tol)
  expect_gt(mm_clearance(scene, wider, i), mm_clearance(scene, default, i))
})

test_that("edge_route_options(): edge_sep is the gap between two paths in one slot", {
  scene <- skip_chain_mm()
  first <- mm_edge(scene, "p->r")
  second <- mm_edge(scene, "p->s")
  slot_gap <- function(res) {
    abs(mm_y_at(res$paths[[first]], 60) - mm_y_at(res$paths[[second]], 60))
  }

  # both skips arch over q on the same side, so the separation between them
  # is the separation the option names
  expect_equal(slot_gap(route_mm(scene)), 3.6, tolerance = 0.01)
  expect_equal(
    slot_gap(route_mm(scene, edge_route_options(edge_sep = 6))),
    6,
    tolerance = 0.01
  )
  expect_equal(
    slot_gap(route_mm(scene, edge_route_options(edge_sep = 10))),
    10,
    tolerance = 0.01
  )
})

test_that("edge_route_options(): edge_sep_min equal to edge_sep fixes the orthogonal spacing", {
  scene <- narrow_band_mm(30)

  # a 30 mm gap is too narrow for four slots at the full separation, so the
  # ladder tightens them
  default <- route_mm(scene, mode = "orthogonal")
  expect_lt(default$ortho$gaps$spacing, 3.6)

  # with the floor at the separation there is nothing left to tighten
  fixed <- route_mm(
    scene,
    edge_route_options(edge_sep = 3.6, edge_sep_min = 3.6),
    mode = "orthogonal"
  )
  expect_equal(fixed$ortho$gaps$spacing, 3.6)
})

test_that("edge_route_options(): corners = 'sharp' leaves every bend a right angle", {
  scene <- narrow_band_mm(40)

  rounded <- route_mm(scene, mode = "orthogonal")
  sharp <- route_mm(
    scene,
    edge_route_options(corners = "sharp"),
    mode = "orthogonal"
  )

  sharp_turns <- unlist(lapply(sharp$paths, mm_turning_angles))
  rounded_turns <- unlist(lapply(rounded$paths, mm_turning_angles))

  # the bends are kept, not cut: each of the eight is an exact quarter turn
  expect_identical(sum(abs(sharp_turns - 90) < 1e-6), 8L)
  expect_lte(max(sharp_turns), 90 + 1e-6)

  # and the default draws none of them, because it rounds every one
  expect_lt(max(rounded_turns), 90 - 1e-6)
})

test_that("edge_route_options(): corner_radius sets the radius the scene draws at", {
  scene <- narrow_band_mm(40)

  default <- route_mm(scene, mode = "orthogonal")
  bigger <- route_mm(
    scene,
    edge_route_options(corner_radius = 4),
    mode = "orthogonal"
  )

  expect_equal(default$ortho$rc, 2.1)
  expect_equal(bigger$ortho$rc, 4)

  # a wider corner reserves a longer stub past the cap, which is what pushes
  # a crowded gap down the ladder
  expect_gt(bigger$ortho$gaps$stub, default$ortho$gaps$stub)
})

test_that("edge_route_options(): a shallower max_bow caps the free bow", {
  scene <- steep_skip_mm()
  i <- mm_edge(scene, "a->c")

  default <- route_mm(scene)
  shallow <- route_mm(scene, edge_route_options(max_bow = 0.06))

  expect_identical(default$meta$mode[[i]], "bow")
  expect_false(default$meta$sagitta_capped[[i]])

  expect_true(shallow$meta$sagitta_capped[[i]])
  expect_lt(
    shallow$meta$sagitta_ratio[[i]],
    default$meta$sagitta_ratio[[i]]
  )
})

test_that("edge_route_options(): bend_penalty = 0 buys the four-bend run", {
  scene <- mediator_mm()
  i <- mm_edge(scene, "x->y")

  default <- route_mm(scene, mode = "orthogonal")
  free <- route_mm(
    scene,
    edge_route_options(bend_penalty = 0),
    mode = "orthogonal"
  )

  # two bends at the default price, four once bends cost nothing
  expect_equal(default$meta$n_waypoints[[i]], 2)
  expect_equal(free$meta$n_waypoints[[i]], 4)
})

test_that("edge_route_options(): a raised crossing_penalty buys the detour instead", {
  scene <- tangle_mm()
  i <- mm_edge(scene, "s->t")

  default <- route_mm(scene)
  dear <- route_mm(scene, edge_route_options(crossing_penalty = 200))

  expect_identical(default$meta$mode[[i]], "interior")
  expect_identical(dear$meta$mode[[i]], "periphery")
  expect_gt(dear$meta$sagitta_ratio[[i]], default$meta$sagitta_ratio[[i]])
})

test_that("edge_route_options(): crossing_saturation = FALSE restores the deep arch", {
  scene <- tangle_mm()
  i <- mm_edge(scene, "s->t")

  default <- route_mm(scene)
  linear <- route_mm(scene, edge_route_options(crossing_saturation = FALSE))

  # priced in full, every crossing the interior route makes adds up until the
  # arch under the stacks is worth its displacement
  expect_identical(default$meta$mode[[i]], "interior")
  expect_identical(linear$meta$mode[[i]], "periphery")
  expect_gt(linear$meta$sagitta_ratio[[i]], default$meta$sagitta_ratio[[i]])
})

test_that("edge_route_options(): head_penalty = 0 passes an arrowhead the default avoids", {
  scene <- head_zone_mm()
  i <- mm_edge(scene, "a->b")

  default <- route_mm(scene)
  free <- route_mm(scene, edge_route_options(head_penalty = 0))

  # the tie between the two sides of m falls away from u's arrowhead while
  # the head zone is priced, and back to the default side once it is free
  expect_equal(default$meta$side[[i]], -1)
  expect_equal(free$meta$side[[i]], 1)
})

test_that("edge_route_options(): tight_penalty prices the tight slot out of the route", {
  scene <- tight_slot_mm()

  default <- route_mm(scene)
  free <- route_mm(scene, edge_route_options(tight_penalty = 0))
  dear <- route_mm(scene, edge_route_options(tight_penalty = 20))

  # the gap between n1 and n2 is threadable only at the soft margin, and at
  # the default price it is still the cheapest way through
  expect_equal(default$meta$side[[1]], 1)
  expect_equal(free$meta$side[[1]], 1)

  # priced high enough, the route goes around the pair instead
  expect_equal(dear$meta$side[[1]], -1)
  expect_gt(dear$meta$sagitta_ratio[[1]], default$meta$sagitta_ratio[[1]])
})

test_that("edge_route_options(): congestion_penalty = 0 sends the fan tie to the other side", {
  scene <- fan_mm()
  i <- mm_edge(scene, "a->e")

  expect_equal(route_mm(scene)$meta$side[[i]], -1)
  expect_equal(
    route_mm(scene, edge_route_options(congestion_penalty = 0))$meta$side[[i]],
    1
  )
})

test_that("edge_route_options(): parallel_sep widens the lens between parallel edges", {
  scene <- parallel_pair_mm()
  lens <- function(res) {
    abs(mm_y_at(res$paths[[1]], 80) - mm_y_at(res$paths[[2]], 80))
  }

  default <- lens(route_mm(scene))
  wider <- lens(route_mm(scene, edge_route_options(parallel_sep = 14)))

  expect_equal(default, 6, tolerance = 0.01)
  expect_equal(wider, 14, tolerance = 0.01)
  expect_gt(wider, default)
})

test_that("edge_route_options(): steep_angle moves the boundary of the free-bow tier", {
  scene <- shallow_skip_mm()
  i <- mm_edge(scene, "a->c")

  # the chord runs 34 degrees to the layer axis, so it takes the layered tier
  # until the boundary drops below it
  expect_identical(route_mm(scene)$meta$mode[[i]], "interior")
  expect_identical(
    route_mm(scene, edge_route_options(steep_angle = 25))$meta$mode[[i]],
    "bow"
  )
})

test_that("edge_route_options(): tangent_clamp bounds the departure tangent", {
  scene <- short_skip_mm()

  default <- route_mm(scene)
  tight <- route_mm(scene, edge_route_options(tangent_clamp = 10))

  expect_lt(
    mm_departure_angle(tight$paths[[1]]),
    mm_departure_angle(default$paths[[1]])
  )
})

# The layer axis ---------------------------------------------------------------

test_that("a routed layer takes the layer axis from the layout", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")

  tidy_down <- tidy_dagitty(y_direction_dag())
  axis_of <- function(plot) routed_layer_of(plot)$geom_params$layer_axis

  direct <- ggplot(tidy_down, aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()
  expect_identical(axis_of(direct), "y")

  assembled <- ggplot(tidy_down, aes_dag()) + geom_dag()
  expect_identical(axis_of(assembled), "y")

  expect_identical(axis_of(ggdag(tidy_down)), "y")
})

test_that("an axis named at the call beats the layout's own", {
  skip_if_not_installed("ggarrow")

  named <- ggplot(tidy_dagitty(y_direction_dag()), aes_dag()) +
    geom_dag_routed_arrows(layer_axis = "x") +
    geom_dag_point()

  expect_identical(routed_layer_of(named)$geom_params$layer_axis, "x")
})

test_that("a layout across the panel leaves the axis to the router", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "spline")

  axis_of <- function(dag) {
    plot <- ggplot(tidy_dagitty(dag), aes_dag()) + geom_dag()
    routed_layer_of(plot)$geom_params$layer_axis
  }

  # the router already infers layers along x, so a layout across the panel
  # names nothing and neither do coordinates the user wrote out
  across <- dagify(y ~ x + m, m ~ x, coords = time_ordered_coords())
  expect_identical(axis_of(across), "auto")
  expect_identical(axis_of(mediator_dag()), "auto")
})

test_that("the layer axis reaches route_edges_mm()", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # the router is spied on rather than replaced: the picture is still drawn,
  # so the constants captured are the ones a real drawing worked from
  captured_axes <- function(plot) {
    captured <- character()
    original <- route_edges_mm
    local_mocked_bindings(
      route_edges_mm = function(...) {
        args <- list(...)
        captured[[length(captured) + 1L]] <<- args$opts$layer_axis
        do.call(original, args)
      }
    )
    draw_offscreen(plot)
    captured
  }

  plot <- ggplot(tidy_dagitty(y_direction_dag()), aes_dag()) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  seen <- captured_axes(plot)
  expect_gte(length(seen), 1)
  expect_true(all(seen == "y"))
})

# The pictures -------------------------------------------------------------------

test_that("edge_route_options visuals: a spline scene under a shallower bow", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  shallow <- edge_route_options(max_bow = 0.04)
  p <- ggdag(
    bow_skip_dag(),
    edge_engine = "ggarrow",
    edge_route = "spline",
    edge_route_options = shallow
  ) +
    theme_dag()

  # A baseline is worth keeping only when the picture is known to be the one
  # the options asked for, so the scene the layer routes is measured before
  # it is drawn.
  scene <- routed_scene_mm(p)
  routed <- route_drawn_scene(scene, shallow, "spline")
  default <- route_drawn_scene(scene, edge_route_options(), "spline")
  bowed <- which(default$meta$mode == "bow")
  stopifnot(
    length(bowed) == 1,
    # the cap is what changed the picture, and it is a cap: the bow is
    # shallower than the one the default scene drew
    routed$meta$sagitta_capped[bowed],
    !default$meta$sagitta_capped[bowed],
    routed$meta$sagitta_ratio[bowed] < default$meta$sagitta_ratio[bowed],
    # and a capped bow still clears every disc it is not an endpoint of, at
    # the soft margin the cap trades the full clearance for
    mm_min_clearance(scene, routed) >= scene$radius + 1.2 - verify_tol
  )

  expect_doppelganger("edge-route-options-spline-shallow-bow", p)
})

test_that("edge_route_options visuals: an orthogonal scene with sharp corners", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  sharp <- edge_route_options(corners = "sharp")
  p <- ggdag(
    bow_skip_dag(),
    edge_engine = "ggarrow",
    edge_route = "orthogonal",
    edge_route_options = sharp
  ) +
    theme_dag()

  scene <- routed_scene_mm(p)
  routed <- route_drawn_scene(scene, sharp, "orthogonal")
  rounded <- route_drawn_scene(scene, edge_route_options(), "orthogonal")
  turns <- unlist(lapply(routed$paths, mm_turning_angles))
  stopifnot(
    # no corner is cut: the bends the picture draws are right angles, apart
    # from the connectors hidden inside the node discs
    sum(abs(turns - 90) < 1e-6) >= 8,
    max(turns) <= 90 + 1e-6,
    # and the scene these replace rounds every one of them
    max(unlist(lapply(rounded$paths, mm_turning_angles))) < 90 - 1e-6,
    mm_min_clearance(scene, routed) >= scene$radius + 3 - verify_tol
  )

  expect_doppelganger("edge-route-options-orthogonal-sharp", p)
})

test_that("orthogonal visuals: a level chord runs on its target's line", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # Two departures from one source, both leaving its east face. `t` sits
  # 0.009 data units above `s`, which the 10 by 8 inch device the vdiffr
  # writer opens draws as 1.5 mm, inside the 2.1 mm corner radius, so s->t
  # is level; `u` sits a full unit above, so s->u is not.
  p <- ggdag(
    dagify(
      t ~ s,
      u ~ s,
      coords = list(
        x = c(s = 0, t = 1, u = 1),
        y = c(s = 0, t = 0.009, u = 1)
      )
    ),
    edge_engine = "ggarrow",
    edge_route = "orthogonal"
  ) +
    theme_dag()

  # A baseline is worth keeping only when the picture is known to be the one
  # the router drew, so the scene the layer routes is measured before it is
  # drawn.
  scene <- routed_scene_mm(p)
  routed <- route_drawn_scene(scene, edge_route_options(), "orthogonal")
  keys <- mm_scene_keys(scene, c("s", "t", "u"))
  st <- mm_named_edge(scene, keys, "s", "t")
  su <- mm_named_edge(scene, keys, "s", "u")
  level <- routed$paths[[st]]
  level_runs <- mm_axis_runs(level)
  trunk_runs <- mm_axis_runs(routed$paths[[su]])
  s_y <- mm_node_y(scene, keys, "s")
  t_y <- mm_node_y(scene, keys, "t")
  u_y <- mm_node_y(scene, keys, "u")
  stopifnot(
    # the scene is the one the level rule is about: one target off level by
    # less than the corner radius and by more than a rounding, and one far
    # enough above to need a trunk
    abs(t_y - s_y) > 1,
    abs(t_y - s_y) < 2.1,
    abs(u_y - s_y) > 100,
    # the level chord is drawn as the single horizontal run on t's line,
    # from the port on that line to t's centre
    routed$meta$mode[[st]] == "straight",
    identical(nrow(level), 2L),
    abs(level$y[[1]] - level$y[[2]]) < 1e-6,
    abs(level$y[[1]] - t_y) < 1e-6,
    identical(nrow(level_runs), 1L),
    identical(level_runs$axis, "h"),
    # and the sibling keeps the trunk it drew before: out along s's own
    # line, one vertical at the slot, and in to u
    identical(nrow(trunk_runs), 3L),
    identical(trunk_runs$axis, c("h", "v", "h")),
    abs(trunk_runs$coord[[1]] - s_y) < 1e-6,
    abs(trunk_runs$coord[[3]] - u_y) < 1e-6
  )

  expect_doppelganger(
    "orthogonal level chord leaves its source at the target's height",
    p
  )
})

test_that("orthogonal visuals: rows in a narrow gap keep the heads apart", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # The napkin, laid out so that the gap the three arrivals at `a` cross is
  # 0.319 data units wide, which the 10 by 8 inch device the vdiffr writer
  # opens draws as 20 mm: too narrow for a stub, so the gap is floored and
  # the arrivals take rows instead of one shared line. Every other gap is a
  # full unit, three times as wide.
  gap <- 0.319
  p <- ggdag(
    dagify(
      z ~ u1,
      a ~ u1 + u2 + z,
      y ~ u2 + a + m,
      m ~ a,
      coords = list(
        x = c(
          u1 = 0,
          u2 = 1,
          z = 1,
          a = 1 + gap,
          m = 2 + gap,
          y = 3 + gap
        ),
        y = c(
          u1 = 0.030,
          u2 = -0.390,
          z = 0.387,
          a = 0.025,
          m = 0.239,
          y = -0.285
        )
      )
    ),
    edge_engine = "ggarrow",
    edge_route = "orthogonal"
  ) +
    theme_dag()

  scene <- routed_scene_mm(p)
  routed <- route_drawn_scene(scene, edge_route_options(), "orthogonal")
  keys <- mm_scene_keys(scene, c("u1", "u2", "z", "a", "m", "y"))
  gaps <- routed$ortho$gaps
  narrow <- gaps[which.min(gaps$width), , drop = FALSE]
  arrivals <- lapply(
    c("u1", "u2", "z"),
    function(from) routed$paths[[mm_named_edge(scene, keys, from, "a")]]
  )
  rows <- vapply(arrivals, function(path) path$y[[nrow(path)]], numeric(1))
  last_run_rise <- vapply(
    arrivals,
    function(path) abs(diff(path$y[nrow(path) - 1:0])),
    numeric(1)
  )
  stopifnot(
    # the gap before `a` is the narrow one, and it is floored: rung 4, with
    # no room left for a stub
    abs(narrow$width - 20) < 0.5,
    narrow$rung == 4,
    is.na(narrow$stub),
    min(gaps$width[-which.min(gaps$width)]) > 60,
    # the three heads arrive on three rows a separation apart, the level
    # one owning the centre row
    length(unique(round(rows, 6))) == 3L,
    min(diff(sort(rows))) > 3.6 - 1e-6,
    abs(rows[[1]] - mm_node_y(scene, keys, "a")) < 1e-6,
    # and each of them is drawn along its own row, not across the corner
    # behind it
    all(last_run_rise < 1e-6)
  )

  expect_doppelganger("orthogonal rows in a narrow gap keep the heads apart", p)
})

test_that("spline visuals: a layout down the panel routes along its own axis", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  p <- ggdag(
    y_direction_dag(),
    edge_engine = "ggarrow",
    edge_route = "spline"
  ) +
    theme_dag()

  # A baseline is worth keeping only when the picture is known to be the one
  # the layout asked for, so the scene the layer routes is measured before it
  # is drawn.
  stopifnot(identical(routed_layer_of(p)$geom_params$layer_axis, "y"))

  scene <- routed_scene_mm(p)
  keys <- mm_scene_keys(scene, c("x", "m", "y", "a", "b", "c"))
  skip <- mm_named_edge(scene, keys, "x", "y")
  column <- scene$nodes$x[[match(keys[["m"]], scene$nodes$name)]]
  m_y <- mm_node_y(scene, keys, "m")

  routed <- route_drawn_scene(scene, edge_route_options(), "spline", "y")
  inferred <- route_drawn_scene(scene, edge_route_options(), "spline")
  waypoint <- routed$waypoints[[skip]]

  stopifnot(
    # the scene is the one the rule is about: left to itself the router reads
    # the two columns as the layers and bows x -> y out towards the panel
    # edge, away from the layer m sits in
    inferred$meta$mode[[skip]] == "bow",
    inferred$waypoints[[skip]]$x < column,
    # told which axis its layers run along, it threads the same edge between
    # the columns instead, past m at the height of m's own layer
    routed$meta$mode[[skip]] == "interior",
    identical(nrow(waypoint), 1L),
    waypoint$x > column,
    abs(waypoint$y - m_y) < 1e-6,
    mm_min_clearance(scene, routed) >= scene$radius + 3 - verify_tol
  )

  expect_doppelganger("spline layout down the panel routes along y", p)
})
