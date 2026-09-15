# Under the ggarrow edge engine an edge is resected so that it stops 2 mm,
# times the plot's `size`, outside the outline of the node at each of its
# ends, as the ggraph engine caps it, unless the caller fixes the resection
# with `edge_cap`, the `ggdag.edge_cap` option, or a resection of the layer's
# own. The outline of a circle node is its radius, and the tip lies that far
# plus the gap from the centre; the outline of a square node is its half side,
# and the tip lies on the square that far plus the gap out from the centre.
#
# ggarrow resects an end by a straight-line distance from the end of the path.
# At a circle node that distance is the whole cap, but at a square node it
# depends on the angle the edge meets the square at, which is known only once
# the plot is drawn in millimetres. So every check here reads the arrow grobs
# of a plot drawn on a device of a fixed size, after their content is forced:
# the resection each grob hands ggarrow, whether the layer settled it when the
# plot was built or when it was drawn, and the tip ggarrow draws at that
# resection. Circle ends are checked by both, square ends by the tip alone.
# The helpers live in helper-node-edge-ends.R.

# Scenes ------------------------------------------------------------------------

# The plotters that take an `edge_engine` argument, each called on a DAG it can
# draw at `node_size` with `...` passed on. `geom_dag()` is called on a plot of
# its own.
engine_plotter_calls <- function(node_size, ...) {
  list(
    ggdag = ggdag(cap_dag(), node_size = node_size, ...),
    geom_dag = ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
      geom_dag(node_size = node_size, ...),
    ggdag_adjust = ggdag_adjust(
      controlled_dag(),
      var = "z",
      node_size = node_size,
      ...
    ),
    ggdag_adjustment_set = ggdag_adjustment_set(
      controlled_dag(),
      node_size = node_size,
      ...
    ),
    ggdag_paths = ggdag_paths(cap_dag(), node_size = node_size, ...),
    ggdag_paths_fan = ggdag_paths_fan(cap_dag(), node_size = node_size, ...),
    ggdag_equivalent_dags = ggdag_equivalent_dags(
      dagify(y ~ x, x ~ z),
      node_size = node_size,
      ...
    ),
    ggdag_equivalent_class = ggdag_equivalent_class(
      dagify(y ~ x, x ~ z, y ~ z),
      node_size = node_size,
      ...
    ),
    ggdag_m_bias = ggdag_m_bias(node_size = node_size, ...),
    ggdag_butterfly_bias = ggdag_butterfly_bias(node_size = node_size, ...),
    ggdag_confounder_triangle = ggdag_confounder_triangle(
      node_size = node_size,
      ...
    ),
    ggdag_collider_triangle = ggdag_collider_triangle(
      node_size = node_size,
      ...
    ),
    ggdag_mediation_triangle = ggdag_mediation_triangle(
      node_size = node_size,
      ...
    ),
    ggdag_quartet_collider = ggdag_quartet_collider(
      node_size = node_size,
      ...
    ),
    ggdag_quartet_confounder = ggdag_quartet_confounder(
      node_size = node_size,
      ...
    ),
    ggdag_quartet_mediator = ggdag_quartet_mediator(
      node_size = node_size,
      ...
    ),
    ggdag_quartet_m_bias = ggdag_quartet_m_bias(node_size = node_size, ...),
    ggdag_quartet_time_collider = ggdag_quartet_time_collider(
      node_size = node_size,
      ...
    )
  )
}

# The plotters that draw their edges with whatever engine the `edge_engine`
# option names, each called on a DAG it can draw at `node_size`.
option_plotter_calls <- function(node_size) {
  list(
    ggdag_status = ggdag_status(cap_dag(), node_size = node_size),
    ggdag_collider = ggdag_collider(cap_dag(), node_size = node_size),
    ggdag_canonical = ggdag_canonical(cap_dag(), node_size = node_size),
    ggdag_exogenous = ggdag_exogenous(cap_dag(), node_size = node_size),
    ggdag_children = ggdag_children(cap_dag(), "z", node_size = node_size),
    ggdag_parents = ggdag_parents(cap_dag(), "y", node_size = node_size),
    ggdag_ancestors = ggdag_ancestors(cap_dag(), "y", node_size = node_size),
    ggdag_descendants = ggdag_descendants(
      cap_dag(),
      "z",
      node_size = node_size
    ),
    ggdag_markov_blanket = ggdag_markov_blanket(
      cap_dag(),
      "x",
      node_size = node_size
    ),
    ggdag_adjacent = ggdag_adjacent(cap_dag(), "x", node_size = node_size),
    ggdag_instrumental = ggdag_instrumental(
      conditional_iv_dag(),
      node_size = node_size
    ),
    ggdag_drelationship = ggdag_drelationship(
      controlled_dag(),
      from = "x",
      to = "y",
      controlling_for = "z",
      node_size = node_size
    ),
    ggdag_dseparated = ggdag_dseparated(
      controlled_dag(),
      from = "x",
      to = "y",
      controlling_for = "z",
      node_size = node_size
    ),
    ggdag_dconnected = ggdag_dconnected(
      controlled_dag(),
      from = "x",
      to = "y",
      controlling_for = "z",
      node_size = node_size
    )
  )
}

# The plotters among these that draw controlled nodes as squares.
square_plotters <- c(
  "ggdag_adjust",
  "ggdag_adjustment_set",
  "ggdag_instrumental",
  "ggdag_drelationship",
  "ggdag_dseparated",
  "ggdag_dconnected"
)

# Does some edge in `ends` meet a square node at an angle, off both of its
# axes, where the square outline and a circle through its face part ways?
meets_square_at_angle <- function(ends) {
  any(
    is_square_shape(ends$shape) &
      abs(ends$tip_dx) > 2 &
      abs(ends$tip_dy) > 2,
    na.rm = TRUE
  )
}

# A DAG laid out on a grid, with the node `m` drawn as a square and the rest as
# circles.
square_m_dag <- function(dag) {
  tidy_dagitty(dag) |>
    dplyr::mutate(shape = ifelse(name == "m", 15, 19))
}

# `tidy_dag` from `square_m_dag()` drawn with its nodes at `size`, 30 unless
# given, and the ggarrow edge layers `edges`.
square_m_plot <- function(tidy_dag, edges, size = 30) {
  ggplot(tidy_dag, aes_dag()) +
    geom_dag_point(aes(shape = shape), size = size) +
    scale_shape_identity() +
    edges +
    theme_dag()
}

# Two edges meet the square `m` at an angle and one leaves it along its axis.
straight_square_scene <- function() {
  square_m_dag(dagify(
    m ~ x + a,
    y ~ m,
    coords = list(
      x = c(x = 0, a = 0, m = 1, y = 2),
      y = c(x = 0, a = 1, m = 0.5, y = 0.5)
    )
  ))
}

# A directed edge into the square `m` and a bidirected arc from it, both at an
# angle.
arc_square_scene <- function() {
  square_m_dag(dagify(
    m ~ x,
    m ~ ~w,
    coords = list(
      x = c(x = 0, m = 1, w = 2),
      y = c(x = 0, m = 0.5, w = 1.2)
    )
  ))
}

# The square `m` sits on the chord of `x -> y`, which the router detours
# around, and `a -> m` meets it at an angle.
spline_square_scene <- function() {
  square_m_dag(dagify(
    m ~ x + a,
    y ~ x + m,
    coords = list(
      x = c(x = 0, a = 0, m = 1, y = 2),
      y = c(x = 0, a = 1, m = 0, y = 0)
    )
  ))
}

# A collinear chain with a skip edge: the orthogonal router runs `x -> m` and
# `m -> y` along the row and takes `x -> y` round the square `m` through a
# channel, and every node side carries one edge, so every path ends on its
# node's centre.
orthogonal_square_scene <- function() {
  square_m_dag(dagify(
    m ~ x,
    y ~ x + m,
    coords = list(
      x = c(x = 0, m = 1, y = 2),
      y = c(x = 0, m = 0, y = 0)
    )
  ))
}

# Three edges arrive at the square `m` from the left, one level with it and
# two from above, so the orthogonal router stacks two arrivals on rows above
# the centre line of m's west side: offset ports at a square.
offset_port_square_scene <- function() {
  square_m_dag(dagify(
    m ~ x + a + b,
    y ~ m,
    c ~ m,
    coords = list(
      x = c(x = 0, a = 0, b = 0, m = 1, y = 2, c = 2),
      y = c(x = 0, a = 1, b = 2, m = 0, y = 0.5, c = -0.5)
    )
  ))
}

# The square `m` sits beside the chord of `x -> y` so that the chord passes
# through its corner. A router that cleared the square around its half side
# plus the clearance the layer is given would take the detour round that
# corner inside the square, since the corner reaches `sqrt(2)` half sides
# from the centre.
square_corner_scene <- function() {
  square_m_dag(dagify(
    y ~ x + m,
    m ~ a,
    coords = list(
      x = c(x = 0, a = 0, m = 1.14, y = 2),
      y = c(x = 0, a = 2, m = 0.86, y = 2)
    )
  ))
}

# The dense DAG with its outcome drawn at size 30 and every other node at the
# default size, so the router is handed two node sizes.
two_size_dag <- function() {
  tidy_dagitty(dagify(
    y ~ a + b + c + x,
    x ~ a + b,
    a ~ c,
    b ~ c,
    exposure = "x",
    outcome = "y",
    coords = list(
      x = c(c = 0, a = 1, b = 1, x = 2, y = 3),
      y = c(c = 0, a = 0.5, b = -0.5, x = 0, y = 0)
    )
  )) |>
    dplyr::mutate(node_size = ifelse(name == "y", 30, 16))
}

labelled_controlled_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    z ~ a,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder", a = "Cause")
  )
}

# The node frames `plot` hands the router when it is drawn, one per call of
# `route_edges_mm()`: the routed edge layer's, then the automatic label
# layer's when the plot has one.
router_node_frames <- function(plot) {
  route_edges_mm <- get("route_edges_mm", envir = asNamespace("ggdag"))
  frames <- list()
  record <- function(nodes, ...) {
    frames[[length(frames) + 1L]] <<- nodes
    route_edges_mm(nodes, ...)
  }
  testthat::with_mocked_bindings(
    with_forced_plot(plot, \(built) NULL),
    route_edges_mm = record,
    .package = "ggdag"
  )
  frames
}

# Every call `plot` makes to the router when it is drawn: which grob made it
# (`"edges"` for the routed edge layer, `"labels"` for the automatic label
# layer), the node frame it was handed, and the path it returned for each
# edge, named by the edge's two ends.
router_routes <- function(plot) {
  route_edges_mm <- get("route_edges_mm", envir = asNamespace("ggdag"))
  routes <- list()
  record <- function(nodes, edges, ...) {
    caller <- paste(deparse(sys.call(-1)[[1]]), collapse = "")
    routed <- route_edges_mm(nodes, edges, ...)
    routes[[length(routes) + 1L]] <<- list(
      grob = if (grepl("route_label_obstacles", caller)) "labels" else "edges",
      nodes = nodes,
      paths = stats::setNames(routed$paths, paste(edges$from, edges$to))
    )
    routed
  }
  testthat::with_mocked_bindings(
    with_forced_plot(plot, \(built) NULL),
    route_edges_mm = record,
    .package = "ggdag"
  )
  routes
}

# The row of `nodes`, a router node frame in millimetres, drawn at the
# position of `name` in `tidy_dag`, by its rank among the nodes' positions.
router_node_row <- function(nodes, tidy_dag, name) {
  data <- pull_dag_data(tidy_dag)
  data <- data[!duplicated(data$name), ]
  order <- order(data$x, data$y)
  ranks <- order(nodes$x, nodes$y)
  ranks[[which(data$name[order] == name)]]
}

# The plotters ------------------------------------------------------------------

test_that("every plotter with an edge_engine argument resects ggarrow edges beyond its nodes", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  for (node_size in c(8, 30)) {
    plots <- engine_plotter_calls(node_size, edge_engine = "ggarrow")
    expect_length(plots, 18)
    for (plotter in names(plots)) {
      label <- paste0(plotter, "(node_size = ", node_size, ")")
      ends <- drawn_arrow_ends(plots[[plotter]])
      expect_gt(nrow(ends), 0, label = label)
      if (plotter %in% square_plotters) {
        expect_true(any(is_square_shape(ends$shape)), label = label)
      }

      expect_equal(
        node_size_mismatches(ends, node_size),
        character(),
        label = paste("the nodes the ggarrow edges of", label, "meet")
      )
      expect_equal(
        circle_resect_mismatches(ends),
        character(),
        label = paste("the circle resections of", label)
      )
      expect_equal(
        tip_gap_mismatches(ends),
        character(),
        label = paste("the drawn ggarrow tips of", label)
      )
    }
  }
})

test_that("the plotters that follow the engine option resect ggarrow edges beyond their nodes", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_engine = "ggarrow"
  )

  plots <- option_plotter_calls(30)
  for (plotter in names(plots)) {
    label <- paste0(plotter, "(node_size = 30) under the engine option")
    ends <- drawn_arrow_ends(plots[[plotter]])
    expect_gt(nrow(ends), 0, label = label)
    if (plotter %in% square_plotters) {
      expect_true(any(is_square_shape(ends$shape)), label = label)
    }

    expect_equal(node_size_mismatches(ends, 30), character(), label = label)
    expect_equal(circle_resect_mismatches(ends), character(), label = label)
    expect_equal(tip_gap_mismatches(ends), character(), label = label)
  }
})

test_that("the adjustment set plotters draw ggarrow tips outside square nodes at an angle", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  # an edge along a square's axis stops where a circle through its face would
  # stop it, so the scene has to meet a square off its axes to tell them apart
  for (node_size in c(8, 30)) {
    plots <- list(
      ggdag_adjust = ggdag_adjust(
        controlled_dag(),
        var = "z",
        node_size = node_size,
        edge_engine = "ggarrow"
      ),
      ggdag_adjustment_set = ggdag_adjustment_set(
        readme_dag(),
        node_size = node_size,
        edge_engine = "ggarrow"
      )
    )
    for (plotter in names(plots)) {
      label <- paste0(plotter, "(node_size = ", node_size, ")")
      ends <- drawn_arrow_ends(plots[[plotter]])
      stopifnot(meets_square_at_angle(ends))

      square <- ends[is_square_shape(ends$shape), , drop = FALSE]
      expect_equal(
        tip_gap_mismatches(square),
        character(),
        label = paste("the square ends of", label)
      )
    }
  }
})

test_that("the ggarrow resection scales its gap with the plot's size", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  # the node is drawn at size 30, 11.25 mm in radius, and the 2 mm gap is
  # scaled to 3 mm with it
  ends <- drawn_arrow_ends(
    ggdag(cap_dag(), node_size = 20, size = 1.5, edge_engine = "ggarrow")
  )

  expect_equal(node_size_mismatches(ends, 30), character())
  expect_equal(circle_resect_mismatches(ends, gap = 3), character())
  expect_equal(tip_gap_mismatches(ends, gap = 3), character())
  expect_equal(unique(round(ends$resect, 6)), 14.25)
})

test_that("an explicit edge_cap fixes every ggarrow resection", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  plots <- engine_plotter_calls(30, edge_engine = "ggarrow", edge_cap = 5)
  for (plotter in names(plots)) {
    ends <- drawn_arrow_ends(plots[[plotter]])
    expect_equal(
      fixed_resect_mismatches(ends, 5),
      character(),
      label = paste0(plotter, "(edge_cap = 5, node_size = 30)")
    )
  }
})

# The drawn tips ------------------------------------------------------------------

test_that("a straight ggarrow edge is drawn 2 mm outside a square node", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- square_m_plot(straight_square_scene(), geom_dag_arrow())
  ends <- drawn_arrow_ends(p)
  stopifnot(
    nrow(ends) == 6,
    meets_square_at_angle(ends),
    any(is_square_shape(ends$shape) & ends$end == "fins")
  )

  expect_equal(circle_resect_mismatches(ends), character())
  expect_equal(tip_gap_mismatches(ends), character())
})

test_that("a ggarrow arc is drawn 2 mm outside a square node", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- square_m_plot(arc_square_scene(), geom_dag_arrows())
  drawings <- arrow_grob_drawings(p)
  ends <- drawn_arrow_ends(p)
  # the directed edge is a straight arrow path and the bidirected edge an arc,
  # whose path bends away from its chord
  arc_paths <- purrr::keep(drawings, \(drawn) {
    any(purrr::map_int(drawn$paths, \(path) length(path$x)) > 2)
  })
  stopifnot(
    length(arc_paths) == 1,
    nrow(ends) == 4,
    meets_square_at_angle(ends)
  )

  expect_equal(circle_resect_mismatches(ends), character())
  expect_equal(tip_gap_mismatches(ends), character())
})

test_that("a spline-routed ggarrow edge is drawn 2 mm outside a square node", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- square_m_plot(
    spline_square_scene(),
    geom_dag_routed_arrows(route = "spline")
  )
  drawings <- arrow_grob_drawings(p)
  ends <- drawn_arrow_ends(p)
  routed <- purrr::map_int(
    purrr::list_flatten(purrr::map(drawings, "paths")),
    \(path) length(path$x)
  )
  stopifnot(
    any(routed > 2),
    nrow(ends) == 8,
    meets_square_at_angle(ends)
  )

  expect_equal(tip_gap_mismatches(ends), character())
})

test_that("an orthogonal-routed ggarrow edge is drawn 2 mm outside a square node", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- square_m_plot(
    orthogonal_square_scene(),
    geom_dag_routed_arrows(route = "orthogonal")
  )
  ends <- drawn_arrow_ends(p)
  path_x <- ifelse(ends$end == "fins", ends$from_x, ends$to_x)
  path_y <- ifelse(ends$end == "fins", ends$from_y, ends$to_y)
  # a path that ends on a port off its node's centre line puts its tip past
  # the outline on the port's own run, which this scene keeps out of the check
  stopifnot(
    nrow(ends) == 6,
    any(is_square_shape(ends$shape) & ends$end == "fins"),
    any(is_square_shape(ends$shape) & ends$end == "head"),
    all(abs(path_x - ends$centre_x) < 1e-6),
    all(abs(path_y - ends$centre_y) < 1e-6)
  )

  expect_equal(tip_gap_mismatches(ends), character())
})

test_that("an orthogonal-routed ggarrow edge into an offset port stops 2 mm outside a square node", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  # the router stacks arrivals on rows beside a node's centre line, and a row
  # at a square must lie within its half side so that the run meets the face
  p <- square_m_plot(
    offset_port_square_scene(),
    geom_dag_routed_arrows(route = "orthogonal")
  )
  ends <- drawn_arrow_ends(p)
  expect_equal(node_size_mismatches(ends, 30), character())
  square_heads <- ends[is_square_shape(ends$shape) & ends$end == "head", ]
  offset <- abs(square_heads$to_y - square_heads$centre_y)
  expect_gt(sum(offset > 1), 1)
  expect_lt(max(offset), 0.375 * 30)
  expect_equal(port_gap_mismatches(ends), character())
  expect_equal(tip_gap_mismatches(ends), character())

  # every port of the README adjustment set at the default size lies within
  # the face of its square, as far in from the side as a head is wide. Its
  # gaps are too narrow at this size for every arrival to run straight for a
  # whole cap before its node, and a head drawn across such a corner is not
  # on the square, so the tips are not checked here. Every square port the
  # router gives this scene is a centre port, so this bound guards the
  # README figure rather than the port placement: the offset-port scene
  # above is the one whose ports at a square lie off its centre line
  adjustment <- withr::with_options(
    list(ggdag.edge_route = "orthogonal"),
    ggdag_adjustment_set(readme_dag(), edge_engine = "ggarrow")
  )
  ends <- drawn_arrow_ends(adjustment)
  expect_equal(node_size_mismatches(ends, 16), character())
  square <- ends[is_square_shape(ends$shape), ]
  expect_gt(nrow(square), 0)
  end_x <- ifelse(square$end == "fins", square$from_x, square$to_x)
  end_y <- ifelse(square$end == "fins", square$from_y, square$to_y)
  offset <- pmax(abs(end_x - square$centre_x), abs(end_y - square$centre_y))
  expect_lte(max(offset), 0.375 * 16 - 1.3 / 2 + 1e-6)
})

test_that("a spline detour clears the corner of a square node", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  half <- 0.375 * 30

  p <- square_m_plot(
    square_corner_scene(),
    geom_dag_routed_arrows(route = "spline", clearance = 3)
  )
  drawings <- arrow_grob_drawings(p)
  nodes <- drawings[[1]]$nodes
  m <- nodes[is_square_shape(nodes$shape), ]
  paths <- purrr::list_flatten(purrr::map(drawings, "paths"))
  # the chord of x -> y is the one path between the two circles at the far
  # left and the far right
  x <- nodes[nodes$x == min(nodes$x) & nodes$y == min(nodes$y), ]
  y <- nodes[nodes$x == max(nodes$x), ]
  chord <- purrr::keep(paths, \(path) {
    n <- length(path$x)
    abs(path$x[[1]] - x$x) < 1e-6 &&
      abs(path$y[[1]] - x$y) < 1e-6 &&
      abs(path$x[[n]] - y$x) < 1e-6 &&
      abs(path$y[[n]] - y$y) < 1e-6
  })
  stopifnot(nrow(m) == 1, length(chord) == 1)
  chord <- chord[[1]]
  # the straight chord would pass through the square's corner
  straight <- list(
    x = seq(chord$x[[1]], chord$x[[length(chord$x)]], length.out = 500),
    y = seq(chord$y[[1]], chord$y[[length(chord$y)]], length.out = 500)
  )
  stopifnot(min(pmax(abs(straight$x - m$x), abs(straight$y - m$y))) < half)

  expect_gt(length(chord$x), 2)
  expect_gte(min(pmax(abs(chord$x - m$x), abs(chord$y - m$y))), half + 1)
})

test_that("routed arrows hand the router the outline and cap of each node", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  tidy_dag <- two_size_dag()
  circle_cap <- function(size) 0.375 * size + 2

  # a scene with two node sizes: the outcome's cap follows its own size, and
  # the label layer routes with the same nodes as the edge layer
  follows <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_point(aes(size = node_size)) +
    scale_size_identity() +
    geom_dag_routed_arrows(route = "spline") +
    geom_dag_label_auto(aes(label = name)) +
    theme_dag()
  # each layer routes once when the plot is drawn and once more when the
  # grob tree is forced, the edge layer before the label layer
  frames <- router_node_frames(follows)
  expect_length(frames, 4)
  nodes <- frames[[1]]
  at_y <- router_node_row(nodes, tidy_dag, "y")
  expect_equal(nodes$r[[at_y]], 0.375 * 30)
  expect_equal(nodes$cap[[at_y]], circle_cap(30))
  expect_equal(unique(nodes$r[-at_y]), 0.375 * 16)
  expect_equal(unique(nodes$cap[-at_y]), circle_cap(16))
  expect_false(any(nodes$square))
  for (frame in frames[-1]) {
    expect_equal(
      frame[c("r", "cap", "square")],
      nodes[c("r", "cap", "square")]
    )
  }

  # a square node is cleared around its half diagonal and capped at its side
  squares <- ggplot(
    dplyr::mutate(tidy_dag, shape = ifelse(name == "y", 15, 19)),
    aes_dag()
  ) +
    geom_dag_point(aes(size = node_size, shape = shape)) +
    scale_size_identity() +
    scale_shape_identity() +
    geom_dag_routed_arrows(route = "spline") +
    theme_dag()
  nodes <- router_node_frames(squares)[[1]]
  at_y <- router_node_row(nodes, tidy_dag, "y")
  expect_equal(nodes$r[[at_y]], 0.375 * 30 * sqrt(2))
  expect_equal(nodes$cap[[at_y]], 0.375 * 30 + 2)
  expect_true(nodes$square[[at_y]])
  expect_equal(unique(nodes$cap[-at_y]), circle_cap(16))

  # an explicit resection is the cap of every node, whatever its size and
  # shape, and the nodes keep their own outlines and shapes, in the router
  # the label layer calls as much as in the edge layer's
  fixed <- ggplot(
    dplyr::mutate(tidy_dag, shape = ifelse(name == "y", 15, 19)),
    aes_dag()
  ) +
    geom_dag_point(aes(size = node_size, shape = shape)) +
    scale_size_identity() +
    scale_shape_identity() +
    geom_dag_routed_arrows(route = "spline", resect = 5) +
    geom_dag_label_auto(aes(label = name)) +
    theme_dag()
  frames <- router_node_frames(fixed)
  expect_length(frames, 4)
  nodes <- frames[[1]]
  at_y <- router_node_row(nodes, tidy_dag, "y")
  expect_equal(nodes$r[[at_y]], 0.375 * 30 * sqrt(2))
  expect_equal(nodes$face[[at_y]], 0.375 * 30)
  expect_true(nodes$square[[at_y]])
  expect_equal(unique(nodes$r[-at_y]), 0.375 * 16)
  expect_equal(unique(nodes$cap), 5)
  for (frame in frames[-1]) {
    expect_equal(
      frame[c("r", "face", "cap", "square")],
      nodes[c("r", "face", "cap", "square")]
    )
  }
})

test_that("the label layer routes each routed layer with the caps that layer draws", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  tidy_dag <- tidy_dagitty(dagify(
    y ~ x + m,
    m ~ x,
    x ~ ~y,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0.1, y = 0))
  ))
  routed <- \(...) geom_dag_routed_arrows(route = "spline", ...)[[1]]
  arcs <- \(...) geom_dag_arrow_arc(data = filter_direction("<->"), ...)
  scene <- function(routed_layer, arc_layer) {
    ggplot(tidy_dag, aes_dag()) +
      geom_dag_point(size = 30) +
      routed_layer +
      arc_layer +
      geom_dag_label_auto(aes(label = name)) +
      theme_dag()
  }

  # the routed layer fixes its resection and the arc layer beside it follows
  # the nodes, so the router is handed the fixed cap for every node, and the
  # other way round the cap of each node
  scenes <- list(
    fixed = list(plot = scene(routed(resect = 5), arcs()), cap = 5),
    follows = list(
      plot = scene(routed(), arcs(resect = 5)),
      cap = 0.375 * 30 + 2
    )
  )
  for (name in names(scenes)) {
    routes <- router_routes(scenes[[name]]$plot)
    grobs <- purrr::map_chr(routes, "grob")
    stopifnot(
      sum(grobs == "edges") > 0,
      sum(grobs == "labels") > 0
    )
    frame <- c("r", "face", "cap", "square")
    for (route in routes) {
      label <- paste("the nodes the", route$grob, "grob routes", name, "with")
      expect_equal(unique(route$nodes$cap), scenes[[name]]$cap, label = label)
      expect_equal(
        route$nodes[frame],
        routes[[1]]$nodes[frame],
        label = label
      )
    }
  }

  # and the label layer cuts each edge back by what the layer that draws it
  # resects it by, the arc beside the routed edges included, with two routed
  # layers as well, one following the nodes into `y` and one fixing its
  # resection. ggarrow draws a head straight from its cut, so on a curve the
  # tip leaves the traced path by the sagitta of that chord (see
  # `drawn_tip_point()`)
  into_y <- \(x) dplyr::filter(filter_direction("->")(x), to == "y")
  not_into_y <- \(x) dplyr::filter(filter_direction("->")(x), to != "y")
  scenes$two_routed <- list(
    plot = scene(
      list(
        routed(data_directed = into_y),
        routed(data_directed = not_into_y, resect = 5)
      ),
      arcs()
    )
  )
  for (name in names(scenes)) {
    p <- scenes[[name]]$plot
    ends <- drawn_and_traced_ends(p)
    drawn <- ends$drawn
    traced <- ends$traced
    stopifnot(any(drawn$arc), nrow(traced) == nrow(drawn) / 2)
    expect_equal(
      label_cap_mismatches(traced, drawn),
      character(),
      label = paste("the caps traced in", name)
    )
    expect_equal(
      label_tip_mismatches(traced, drawn, tolerance = 0.35),
      character(),
      label = paste("the tips traced in", name)
    )
  }
})

# An explicit cap fixes every end, so the router is handed that cap for every
# node and its head zones, arrival arms, and bows agree with the ink whatever
# size and shape the nodes are drawn at. The routes are pinned in
# fixtures/ggarrow-explicit-cap-routes.rds, regenerated only on purpose with
# tests/testthat/fixtures/make-resect-fixtures.R. The epidemiology scenes,
# whose nodes are all circles, draw exactly as they did before the caps
# followed the nodes; the adjustment set differs from then only in that its
# square nodes are cleared around their half diagonals. A scene whose panel
# is sized by text drawn outside it, as `explicit_cap_scene_font_sized`
# records, is compared only where the default font measures like the
# Helvetica the fixture was drawn with.
expect_explicit_cap_routes <- function(scenes) {
  fixture <- readRDS(test_path("fixtures", "ggarrow-explicit-cap-routes.rds"))
  for (scene in scenes) {
    current <- arrow_drawing_record(explicit_cap_scenes[[scene]]())
    expect_equal(
      current$edges,
      fixture[[scene]]$edges,
      tolerance = 1e-10,
      label = paste(scene, "edges and resections")
    )
    expect_equal(
      current$paths,
      fixture[[scene]]$paths,
      tolerance = 1e-10,
      label = paste(scene, "paths")
    )
  }
}

test_that("an explicit edge_cap routes exactly as before the caps followed the nodes", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  fixture <- readRDS(test_path("fixtures", "ggarrow-explicit-cap-routes.rds"))
  expect_named(fixture, names(explicit_cap_scenes))
  expect_named(explicit_cap_scene_font_sized, names(explicit_cap_scenes))

  scenes <- names(which(!explicit_cap_scene_font_sized))
  stopifnot(length(scenes) > 0)
  expect_explicit_cap_routes(scenes)
})

test_that("an explicit edge_cap routes the scenes sized by their legends and strips as before under the reference font", {
  skip_if_not_installed("ragg")
  skip_unless_reference_label_font()
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  scenes <- names(which(explicit_cap_scene_font_sized))
  stopifnot(length(scenes) > 0)
  expect_explicit_cap_routes(scenes)
})

# The default size ----------------------------------------------------------------

test_that("at the default node size the ggarrow edges draw exactly as before", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # A circle node of the default size 16 is 6 mm in radius, and an edge that
  # stops 2 mm outside it is resected by the 8 mm the ggarrow engine has
  # always drawn with, so these scenes, whose nodes are all circles, draw
  # every path and every resection as they did before the resection followed
  # the nodes. The baseline is pinned in fixtures/ggarrow-default-resects.rds,
  # regenerated only on purpose with
  # tests/testthat/fixtures/make-resect-fixtures.R. Doubles must match to
  # within floating-point noise (1e-10).
  fixture <- readRDS(test_path("fixtures", "ggarrow-default-resects.rds"))
  expect_named(fixture, default_resect_scenes)

  current <- default_resect_drawings()
  for (scene in default_resect_scenes) {
    for (route in default_resect_routes) {
      label <- paste(scene, route)
      expect_equal(
        current[[scene]][[route]]$edges,
        fixture[[scene]][[route]]$edges,
        tolerance = 1e-10,
        label = paste(label, "edges and resections")
      )
      expect_equal(
        current[[scene]][[route]]$paths,
        fixture[[scene]][[route]]$paths,
        tolerance = 1e-10,
        label = paste(label, "paths")
      )
    }
  }
})

# Plots assembled by hand ----------------------------------------------------------

hand_built_arrow_layers <- list(
  geom_dag_arrow = \() geom_dag_arrow(),
  geom_dag_arrow_arc = \() geom_dag_arrow_arc(),
  geom_dag_arrows = \() geom_dag_arrows(),
  geom_dag_routed_arrows = \() geom_dag_routed_arrows(),
  geom_dag_edges = \() geom_dag_edges(edge_engine = "ggarrow")
)

test_that("hand-built ggarrow layers resect beyond the nodes in either layer order", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  tidy_dag <- tidy_dagitty(cap_dag())

  scenes <- list(
    circle = \() geom_dag_point(size = 30),
    square = \() geom_dag_point(size = 30, shape = 15),
    stylized = \() geom_dag_node(size = 30)
  )

  for (edges in names(hand_built_arrow_layers)) {
    edge_layer <- hand_built_arrow_layers[[edges]]
    for (scene in names(scenes)) {
      nodes_first <- ggplot(tidy_dag, aes_dag()) +
        scenes[[scene]]() +
        edge_layer()
      edges_first <- ggplot(tidy_dag, aes_dag()) +
        edge_layer() +
        scenes[[scene]]()

      for (order in c("nodes first", "edges first")) {
        p <- if (order == "nodes first") nodes_first else edges_first
        label <- sprintf("%s() with %s nodes, %s", edges, scene, order)
        ends <- drawn_arrow_ends(p)
        expect_gt(nrow(ends), 0, label = label)

        expect_equal(node_size_mismatches(ends, 30), character(), label = label)
        expect_equal(circle_resect_mismatches(ends), character(), label = label)
        expect_equal(tip_gap_mismatches(ends), character(), label = label)
      }
    }
  }
})

test_that("a hand-built ggarrow layer with no node layer resects by the edge_cap option", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.node_size = NULL)
  tidy_dag <- tidy_dagitty(cap_dag())

  layers <- list(
    geom_dag_arrow = \() geom_dag_arrow(),
    geom_dag_routed_arrows = \() geom_dag_routed_arrows(),
    geom_dag_edges = \() geom_dag_edges(edge_engine = "ggarrow")
  )
  # the drawn tip's distance from the end of the path, which is the node
  # centre, is what the reader sees
  tip_from_end <- function(ends) {
    end_x <- ifelse(ends$end == "fins", ends$from_x, ends$to_x)
    end_y <- ifelse(ends$end == "fins", ends$from_y, ends$to_y)
    sqrt((ends$tip_x - end_x)^2 + (ends$tip_y - end_y)^2)
  }

  for (edges in names(layers)) {
    p <- ggplot(tidy_dag, aes_dag()) + layers[[edges]]()

    withr::with_options(list(ggdag.edge_cap = 6), {
      ends <- drawn_arrow_ends(p)
      expect_gt(nrow(ends), 0, label = edges)
      expect_true(all(is.na(ends$size)), label = edges)
      expect_equal(fixed_resect_mismatches(ends, 6), character(), label = edges)
      expect_equal(
        unique(round(tip_from_end(ends), 4)),
        6,
        label = paste(edges, "under the option")
      )
    })

    withr::with_options(list(ggdag.edge_cap = NULL), {
      ends <- drawn_arrow_ends(p)
      expect_equal(fixed_resect_mismatches(ends, 8), character(), label = edges)
      expect_equal(
        unique(round(tip_from_end(ends), 4)),
        8,
        label = paste(edges, "with the option unset")
      )
    })
  }
})

test_that("hand-built ggarrow layers follow a node size mapped to the data", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
    geom_dag_point(aes(size = x)) +
    geom_dag_arrows() +
    scale_size(range = c(8, 30))

  ends <- drawn_arrow_ends(p)
  expect_gt(length(unique(ends$size)), 1)
  expect_false(anyNA(ends$size))
  expect_equal(circle_resect_mismatches(ends), character())
  expect_equal(tip_gap_mismatches(ends), character())
})

test_that("a resection the user gives a hand-built ggarrow layer wins at its end", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  tidy_dag <- tidy_dagitty(cap_dag())
  circle_resect <- 0.375 * 30 + 2

  set <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_point(size = 30) +
    geom_dag_arrows(resect_head = 5)
  ends <- drawn_arrow_ends(set)
  expect_equal(unique(round(ends$resect[ends$end == "head"], 6)), 5)
  expect_equal(unique(round(ends$resect[ends$end == "fins"], 6)), circle_resect)

  # a resection mapped from the data is read for each edge
  mapped <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_point(size = 30) +
    geom_dag_arrow(aes(resect_head = ifelse(name == "z", 3, 5)))
  ends <- drawn_arrow_ends(mapped)
  fins <- ends[ends$end == "fins", ]
  heads <- ends[ends$end == "head", ]
  edge_rows <- pull_dag_data(tidy_dag)
  edges_from_z <- sum(edge_rows$name == "z" & !is.na(edge_rows$to))
  stopifnot(edges_from_z > 0, edges_from_z < nrow(heads))

  head_3 <- abs(heads$resect - 3) < 1e-6
  expect_equal(unique(round(fins$resect, 6)), circle_resect)
  expect_setequal(unique(round(heads$resect, 6)), c(3, 5))
  expect_equal(sum(head_3), edges_from_z)
  # and the edges resected by 3 mm at their heads all start at one node
  expect_length(unique(round(heads$from_x[head_3], 6)), 1)
})

test_that("geom_dag_edges() under the ggarrow engine takes the resection the user sets", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  tidy_dag <- tidy_dagitty(cap_dag())
  circle_resect <- 0.375 * 30 + 2
  resects <- function(edges) {
    ends <- drawn_arrow_ends(
      ggplot(tidy_dag, aes_dag()) + geom_dag_point(size = 30) + edges
    )
    stopifnot(nrow(ends) > 0)
    list(
      fins = unique(round(ends$resect[ends$end == "fins"], 6)),
      head = unique(round(ends$resect[ends$end == "head"], 6))
    )
  }

  for (route in c("straight", "spline")) {
    edges <- \(...) {
      geom_dag_edges(edge_engine = "ggarrow", edge_route = route, ...)
    }

    expect_no_condition(both <- edges(resect = 4))
    expect_equal(resects(both), list(fins = 4, head = 4), label = route)

    expect_no_condition(head <- edges(resect_head = 5))
    expect_equal(
      resects(head),
      list(fins = circle_resect, head = 5),
      label = route
    )

    # a ggraph circle cap in absolute units is the resection of its end
    expect_no_condition(
      caps <- edges(
        start_cap = ggraph::circle(3, "mm"),
        end_cap = ggraph::circle(0.5, "cm")
      )
    )
    expect_equal(resects(caps), list(fins = 3, head = 5), label = route)

    # and so is a circle cap mapped to the data, for each edge
    expect_no_condition(
      mapped <- edges(
        aes(
          start_cap = ggraph::circle(3, "mm"),
          end_cap = ggraph::circle(ifelse(name == "z", 4, 5), "mm")
        )
      )
    )
    expect_equal(
      lapply(resects(mapped), sort),
      list(fins = 3, head = c(4, 5)),
      label = route
    )

    # a resection set for an end wins over a cap at that end, and a cap over
    # a resection set for both ends
    expect_no_condition(
      set_head <- edges(aes(end_cap = ggraph::circle(5, "mm")), resect_head = 6)
    )
    expect_equal(resects(set_head)$head, 6, label = route)
    expect_no_condition(
      set_both <- edges(aes(end_cap = ggraph::circle(5, "mm")), resect = 4)
    )
    expect_equal(resects(set_both), list(fins = 4, head = 5), label = route)
  }

  # the automatic labels cut the edges back where a mapped cap resects them
  labelled <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_point(size = 30) +
    geom_dag_edges(
      aes(end_cap = ggraph::circle(ifelse(name == "z", 4, 5), "mm")),
      edge_engine = "ggarrow",
      edge_route = "spline"
    ) +
    geom_dag_label_auto(aes(label = name))
  ends <- drawn_and_traced_ends(labelled)
  drawn <- ends$drawn
  traced <- ends$traced
  expect_equal(label_cap_mismatches(traced, drawn), character())

  # ggarrow stops an end a distance from the end of the path, so a cap it
  # cannot draw is refused rather than dropped
  expect_error(
    geom_dag_edges(edge_engine = "ggarrow", end_cap = ggraph::square(5, "mm")),
    class = "ggdag_type_error"
  )
  # an ellipse is a circle geometry of different width and height, and has
  # no single distance from its centre to stop at
  expect_error(
    geom_dag_edges(
      edge_engine = "ggarrow",
      end_cap = ggraph::ellipsis(5, 3, "mm")
    ),
    class = "ggdag_type_error"
  )
  expect_error(
    ggplot2::ggplot_build(
      ggplot(tidy_dag, aes_dag()) +
        geom_dag_edges(
          aes(end_cap = ggraph::ellipsis(5, 3, "mm")),
          edge_engine = "ggarrow"
        )
    ),
    class = "ggdag_type_error"
  )
  expect_error(
    ggplot2::ggplot_build(
      ggplot(tidy_dag, aes_dag()) +
        geom_dag_edges(
          aes(start_cap = ggraph::square(5, "mm")),
          edge_engine = "ggarrow"
        )
    ),
    class = "ggdag_type_error"
  )
  expect_error(
    geom_dag_edges(
      edge_engine = "ggarrow",
      start_cap = ggraph::circle(0.1, "npc")
    ),
    class = "ggdag_type_error"
  )
  expect_error(
    geom_dag_edges(edge_engine = "ggarrow", end_cap = 5),
    class = "ggdag_type_error"
  )
})

# The automatic labels ------------------------------------------------------------

test_that("the automatic labels cut ggarrow edges where the drawn edges stop", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  circle_cap <- 0.375 * 30 + 2

  circles <- ggdag(
    labelled_controlled_dag(),
    node_size = 30,
    use_labels = TRUE,
    edge_engine = "ggarrow"
  )
  ends <- drawn_and_traced_ends(circles)
  drawn <- ends$drawn
  traced <- ends$traced
  expect_equal(tip_gap_mismatches(drawn), character())
  expect_equal(unique(c(traced$cap_fins, traced$cap_head)), circle_cap)
  expect_equal(label_tip_mismatches(traced, drawn), character())

  squares <- ggdag_adjust(
    labelled_controlled_dag(),
    var = "z",
    node_size = 30,
    use_labels = TRUE,
    edge_engine = "ggarrow"
  )
  ends <- drawn_and_traced_ends(squares)
  drawn <- ends$drawn
  stopifnot(meets_square_at_angle(drawn))
  traced <- ends$traced
  expect_equal(tip_gap_mismatches(drawn), character())
  expect_equal(label_tip_mismatches(traced, drawn), character())
})

test_that("a resection the plot maps reaches the ggarrow edges and the labels", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  tidy_dag <- tidy_dagitty(controlled_dag())
  circle_cap <- 0.375 * 30 + 2

  # a layer inherits the plot's mapping, so the head resection the plot maps
  # is the one drawn at each head whether or not the layer sets its own, and
  # the labels cut the edges where they are drawn
  for (layer_resect in list(NULL, 5)) {
    label <- paste("a layer resection of", format(layer_resect))
    p <- ggplot(tidy_dag, aes_dag(resect_head = ifelse(name == "z", 3, 9))) +
      geom_dag_point(size = 30) +
      geom_dag_arrow(resect_head = layer_resect) +
      geom_dag_label_auto(aes(label = name))
    ends <- drawn_and_traced_ends(p)
    drawn <- ends$drawn
    expect_setequal(drawn$resect[drawn$end == "head"], c(3, 9))
    expect_equal(unique(drawn$resect[drawn$end == "fins"]), circle_cap)
    expect_equal(
      label_cap_mismatches(ends$traced, drawn),
      character(),
      label = label
    )
  }

  # and so is a cap the plot maps for `geom_dag_edges()` under ggarrow
  capped <- ggplot(tidy_dag, aes_dag(end_cap = ggraph::circle(4, "mm"))) +
    geom_dag_point(size = 30) +
    geom_dag_edges(edge_engine = "ggarrow") +
    geom_dag_label_auto(aes(label = name))
  ends <- drawn_and_traced_ends(capped)
  drawn <- ends$drawn
  expect_equal(unique(drawn$resect[drawn$end == "head"]), 4)
  expect_equal(unique(drawn$resect[drawn$end == "fins"]), circle_cap)
  expect_equal(label_cap_mismatches(ends$traced, drawn), character())
})

test_that("the automatic labels read a resection mapped for each panel in that panel", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # the same DAG in two panels, whose edges a layer resects by 3 mm in one
  # and by 9 mm in the other: the labels cut the edges of each panel by the
  # resection drawn there, and trace the routes of each panel with the cap
  # that panel's routes are drawn with
  data <- pull_dag_data(tidy_dagitty(controlled_dag()))
  data <- dplyr::bind_rows(
    dplyr::mutate(data, panel = "A"),
    dplyr::mutate(data, panel = "B")
  )
  mapped <- ggplot2::aes(resect_head = ifelse(panel == "A", 3, 9))
  faceted <- function(edges) {
    ggplot(data, aes_dag()) +
      geom_dag_point(size = 16) +
      edges +
      ggplot2::facet_wrap(~panel) +
      geom_dag_text_auto(aes(label = name), colour = "black", size = 3) +
      theme_dag()
  }
  plots <- list(
    straight = faceted(geom_dag_arrow(mapped)),
    orthogonal = faceted(geom_dag_routed_arrows(mapped, route = "orthogonal")),
    spline = faceted(geom_dag_routed_arrows(mapped, route = "spline"))
  )
  for (name in names(plots)) {
    p <- plots[[name]]
    for (panel in 1:2) {
      caps <- label_edge_caps(p, panel)
      expect_equal(
        unique(caps$cap_end),
        c(3, 9)[[panel]],
        label = paste("the heads traced in panel", panel, "of", name)
      )
    }
    if (name == "straight") {
      next
    }
    inputs <- drawn_and_traced_router_inputs(p)
    stopifnot(length(inputs$drawn) == 4)
    expect_setequal(
      vapply(inputs$drawn, `[[`, numeric(1), "cap"),
      c(3, 9)
    )
    expect_equal(
      traced_router_input_mismatches(inputs),
      character(),
      label = name
    )
    found <- label_route_deviations(p)
    stopifnot(length(unique(found$panel)) == 2)
    expect_equal(
      found$deviation,
      rep(0, nrow(found)),
      tolerance = 1e-10,
      label = paste("the routes traced for", name)
    )
  }
})

test_that("the automatic labels trace routed edges past square nodes under an explicit resection", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  labelled <- \(p) p + geom_dag_label_auto(aes(label = name))

  # the spline detour round the corner of the square is routed in the label
  # layer exactly as it is drawn
  corner <- labelled(square_m_plot(
    square_corner_scene(),
    geom_dag_routed_arrows(route = "spline", clearance = 3, resect = 5)
  ))
  routes <- router_routes(corner)
  drawn_paths <- purrr::keep(routes, \(route) route$grob == "edges")
  traced_paths <- purrr::keep(routes, \(route) route$grob == "labels")
  stopifnot(length(drawn_paths) > 0, length(traced_paths) > 0)
  for (traced in traced_paths) {
    for (edge in names(drawn_paths[[1]]$paths)) {
      expect_equal(
        traced$paths[[edge]],
        drawn_paths[[1]]$paths[[edge]],
        label = paste("the traced route of", edge)
      )
    }
  }
  # ggarrow draws a head straight from its cut, so on a route that still
  # bends into its node the tip leaves the traced path by the sagitta of that
  # chord (see `drawn_tip_point()`)
  ends <- drawn_and_traced_ends(corner)
  expect_equal(
    label_tip_mismatches(ends$traced, ends$drawn, tolerance = 0.35),
    character()
  )

  # the orthogonal arrivals on the offset ports of the square are cut where
  # their heads are drawn
  for (size in c(16, 30)) {
    offset <- labelled(square_m_plot(
      offset_port_square_scene(),
      geom_dag_routed_arrows(route = "orthogonal", resect = 5),
      size = size
    ))
    ends <- drawn_and_traced_ends(offset)
    drawn <- ends$drawn
    square_heads <- drawn[is_square_shape(drawn$shape) & drawn$end == "head", ]
    stopifnot(sum(abs(square_heads$to_y - square_heads$centre_y) > 1) > 0)
    expect_equal(
      label_tip_mismatches(ends$traced, drawn),
      character(),
      label = paste("the offset ports at size", size)
    )
  }
})

test_that("the automatic labels trace the adjustment set plot's two routed layers as drawn", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # The adjustment set plot draws its adjusted and its unadjusted edges in
  # two routed layers, with a square node where it adjusts. The label layer
  # routes each as the layer that draws it, so every traced route is the
  # drawn one, whether the heads follow the nodes or a cap fixes them. The
  # edge from x to y was once traced to another port than the one it is drawn
  # to, 6.75 mm away.
  for (route in c("spline", "orthogonal")) {
    for (size in c(16, 30)) {
      for (cap in list(NULL, 5)) {
        p <- withr::with_options(
          list(ggdag.edge_route = route),
          ggdag_adjustment_set(
            labelled_controlled_dag(),
            node_size = size,
            edge_cap = cap,
            use_labels = TRUE,
            edge_engine = "ggarrow"
          )
        )
        for (device in list(c(7, 5), c(10, 6))) {
          found <- label_route_deviations(p, device[[1]], device[[2]])
          label <- sprintf(
            "the %s routes at node size %s with %s on a %s by %s inch device",
            route,
            size,
            if (is.null(cap)) "the heads following" else "a 5 mm cap",
            device[[1]],
            device[[2]]
          )
          stopifnot(nrow(found) == 4)
          expect_equal(
            found$deviation,
            rep(0, nrow(found)),
            tolerance = 1e-10,
            label = label
          )
        }
      }
    }

    # the README DAG's three panels, each of whose edges one of the two
    # layers draws and the other draws in another panel: the label layer
    # routes each panel from the edges drawn there, where it once routed each
    # layer's edges of every panel in each, and traced one edge per panel
    # 2.3 mm from where it is drawn on a 10 by 6 inch device
    readme <- withr::with_options(
      list(ggdag.edge_route = route),
      ggdag_adjustment_set(readme_dag(), edge_engine = "ggarrow")
    ) +
      geom_dag_text_auto(aes(label = name), colour = "black")
    for (device in list(c(7, 5), c(10, 6))) {
      found <- label_route_deviations(readme, device[[1]], device[[2]])
      stopifnot(nrow(found) == 33, length(unique(found$panel)) == 3)
      expect_equal(
        found$deviation,
        rep(0, nrow(found)),
        tolerance = 1e-10,
        label = sprintf(
          "the %s routes of the README DAG on a %s by %s inch device",
          route,
          device[[1]],
          device[[2]]
        )
      )
    }
  }
})

test_that("the label engine routes a routed layer with the node size it is given", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # A routed layer given a node size other than the node layer's clears
  # discs of that size where it does not know a node's own and sets the
  # router's constants from it. The label engine routes the layer's edges
  # with the same size, where it once took the size of the nodes and traced
  # orthogonal routes up to 49 mm from where they are drawn.
  for (route in c("spline", "orthogonal")) {
    p <- ggplot(tidy_dagitty(readme_time_ordered_dag()), aes_dag()) +
      geom_dag_point(size = 16) +
      geom_dag_routed_arrows(route = route, node_size = 30) +
      geom_dag_text_auto(colour = "black", size = 3) +
      theme_dag()
    for (device in list(c(7, 5), c(4, 4))) {
      label <- sprintf(
        "the %s routes on a %s by %s inch device",
        route,
        device[[1]],
        device[[2]]
      )
      inputs <- drawn_and_traced_router_inputs(p, device[[1]], device[[2]])
      stopifnot(length(inputs$drawn) > 0)
      expect_equal(
        traced_router_input_mismatches(inputs),
        character(),
        label = label
      )
      found <- label_route_deviations(p, device[[1]], device[[2]])
      stopifnot(nrow(found) == 11)
      expect_equal(
        found$deviation,
        rep(0, nrow(found)),
        tolerance = 1e-10,
        label = label
      )
    }
  }
})

test_that("the automatic labels cut an edge at both ggraph caps the user sets", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  tidy_dag <- tidy_dagitty(controlled_dag())

  plots <- list(
    parameters = ggplot(tidy_dag, aes_dag()) +
      geom_dag_point(size = 30) +
      geom_dag_edges_link(
        start_cap = ggraph::circle(3, "mm"),
        end_cap = ggraph::circle(5, "mm")
      ) +
      geom_dag_label_auto(aes(label = name)),
    mapping = ggplot(tidy_dag, aes_dag()) +
      geom_dag_point(size = 30) +
      geom_dag_edges_link(
        aes(
          start_cap = ggraph::circle(3, "mm"),
          end_cap = ggraph::circle(5, "mm")
        )
      ) +
      geom_dag_label_auto(aes(label = name))
  )

  for (set_as in names(plots)) {
    p <- plots[[set_as]]
    ends <- drawn_and_traced_ends(p)
    drawn <- ends$drawn
    gaps <- sqrt(drawn$tip_dx^2 + drawn$tip_dy^2)
    stopifnot(
      nrow(drawn) > 0,
      all(abs(gaps[drawn$end == "start"] - 3) < 0.05),
      all(abs(gaps[drawn$end == "end"] - 5) < 0.05)
    )

    traced <- ends$traced
    label <- paste("caps set as", set_as)
    expect_equal(unique(traced$cap_fins), 3, label = label)
    expect_equal(unique(traced$cap_head), 5, label = label)
    expect_equal(
      label_tip_mismatches(traced, drawn),
      character(),
      label = label
    )
  }
})

# Visual baselines ----------------------------------------------------------------

test_that("large nodes keep their ggarrow arrowheads clear of the nodes", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggdag(cap_dag(), node_size = 30, edge_engine = "ggarrow")
  ends <- drawn_arrow_ends(p)
  stopifnot(
    length(circle_resect_mismatches(ends)) == 0,
    length(tip_gap_mismatches(ends)) == 0
  )

  expect_doppelganger("ggdag ggarrow resects at node_size 30", p)
})

test_that("orthogonal routes keep their arrowheads clear of a square node", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- square_m_plot(
    orthogonal_square_scene(),
    geom_dag_routed_arrows(route = "orthogonal")
  ) +
    geom_dag_text()
  ends <- drawn_arrow_ends(p)
  stopifnot(
    any(is_square_shape(ends$shape)),
    length(tip_gap_mismatches(ends)) == 0
  )

  expect_doppelganger("orthogonal routes to a square node", p)
})

test_that("orthogonal routes into offset ports keep their arrowheads clear of a square node", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- square_m_plot(
    offset_port_square_scene(),
    geom_dag_routed_arrows(route = "orthogonal")
  ) +
    geom_dag_text()
  ends <- drawn_arrow_ends(p)
  square_heads <- ends[is_square_shape(ends$shape) & ends$end == "head", ]
  stopifnot(
    nrow(square_heads) == 3,
    sum(abs(square_heads$to_y - square_heads$centre_y) > 1) == 2,
    length(port_gap_mismatches(ends)) == 0
  )

  expect_doppelganger("orthogonal offset ports at a square node", p)
})
