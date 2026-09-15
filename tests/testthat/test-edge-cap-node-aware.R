# Under the ggraph edge engine an edge stops a fixed gap of 2 mm outside the
# outline of the node at each of its ends, unless the caller fixes the cap with
# `edge_cap` or the `ggdag.edge_cap` option. At a circle node the cap is a
# circle 2 mm wider than the node, and at a square node a square 2 mm wider
# than the node on every side, so an edge meeting a square at an angle stops
# where it crosses that square. Like every other measure a plot draws, the cap
# scales with the plot's `size` multiplier, gap included. The fixtures and the
# expected geometry live in helper-node-edge-ends.R.

# Per-end caps on the built layers ---------------------------------------------

# One row per edge end of every ggraph edge layer `plot` builds: the panel, the
# node the end sits at, the shape and size that node is drawn with in that
# panel, and the cap ggraph reads for that end. ggraph reads both caps of an
# edge from the first row of its group, and the path of a group runs from the
# start node at `index` 0 to the end node at `index` 1.
edge_end_caps <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  names_at <- node_coords(plot)

  node_index <- which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$geom, c("GeomDagPoint", "GeomDagNode"))
  }))
  drawn_nodes <- built$data[[node_index[[1]]]]
  drawn_nodes <- dplyr::distinct(
    as.data.frame(drawn_nodes)[, c("PANEL", "x", "y", "shape", "size")]
  )

  ends <- purrr::map(dag_edge_layer_indices(plot), \(i) {
    data <- as.data.frame(built$data[[i]])
    if (nrow(data) == 0) {
      return(NULL)
    }
    start_cap <- unclass(built$data[[i]]$start_cap)
    end_cap <- unclass(built$data[[i]]$end_cap)
    groups <- split(seq_len(nrow(data)), paste(data$PANEL, data$group))

    purrr::map(groups, \(rows) {
      first <- rows[[1]]
      from_row <- rows[which.min(data$index[rows])]
      to_row <- rows[which.max(data$index[rows])]
      panel <- data$PANEL[[first]]
      panel_nodes <- drawn_nodes[drawn_nodes$PANEL == panel, ]

      end_at <- function(row, cap, end) {
        name_row <- node_row_at(names_at, data$x[[row]], data$y[[row]])
        node_row <- node_row_at(panel_nodes, data$x[[row]], data$y[[row]])
        data.frame(
          layer = i,
          panel = as.character(panel),
          end = end,
          node = names_at$name[name_row],
          shape = panel_nodes$shape[node_row],
          node_size = panel_nodes$size[node_row],
          cap_geometry = cap$geometry[[first]],
          cap_unit = cap$width_unit[[first]],
          cap_mm = cap$width[[first]] / 2
        )
      }

      edge <- paste(
        names_at$name[node_row_at(
          names_at,
          data$x[[from_row]],
          data$y[[from_row]]
        )],
        "->",
        names_at$name[node_row_at(names_at, data$x[[to_row]], data$y[[to_row]])]
      )
      rbind(
        end_at(from_row, start_cap, "start"),
        end_at(to_row, end_cap, "end")
      ) |>
        cbind(edge = edge)
    }) |>
      purrr::list_rbind()
  })

  purrr::list_rbind(ends)
}

# The edge ends of `plot` whose cap is not 2 mm outside the outline of the node
# there, each described by panel, edge, end, and node. A cap is read as half
# its width, the radius of a circle and the half side of a square. `node_size`
# is the size the caller asked for and `size` the plot multiplier, so a node
# layer drawn at any other size is reported too.
node_aware_cap_mismatches <- function(plot, node_size, size = 1) {
  ends <- edge_end_caps(plot)
  if (nrow(ends) == 0) {
    return("the plot builds no ggraph edge ends")
  }

  expected <- (expected_outline_mm(ends$shape, node_size) + 2) * size
  geometry <- expected_cap_geometry(ends$shape)
  wrong_size <- is.na(ends$node_size) | ends$node_size != node_size * size
  wrong_cap <- is.na(expected) |
    ends$cap_geometry != geometry |
    ends$cap_unit != "mm" |
    abs(ends$cap_mm - expected) > 1e-6
  bad <- wrong_size | wrong_cap

  sprintf(
    "panel %s, edge %s: the %s cap at %s (a %s drawn at size %s) is a %s of %.4f %s; expected a %s of %.4f mm",
    ends$panel[bad],
    ends$edge[bad],
    ends$end[bad],
    ends$node[bad],
    node_shape_name(ends$shape[bad]),
    ends$node_size[bad],
    ends$cap_geometry[bad],
    ends$cap_mm[bad],
    ends$cap_unit[bad],
    geometry[bad],
    expected[bad]
  )
}

# The edge ends of `plot` whose cap is not a fixed circle of `cap_mm` mm.
fixed_cap_mismatches <- function(plot, cap_mm) {
  ends <- edge_end_caps(plot)
  if (nrow(ends) == 0) {
    return("the plot builds no ggraph edge ends")
  }

  bad <- ends$cap_geometry != "circle" |
    ends$cap_unit != "mm" |
    abs(ends$cap_mm - cap_mm) > 1e-6

  sprintf(
    "panel %s, edge %s: the %s cap at %s is a %s of %.4f %s; expected a circle of %.4f mm",
    ends$panel[bad],
    ends$edge[bad],
    ends$end[bad],
    ends$node[bad],
    ends$cap_geometry[bad],
    ends$cap_mm[bad],
    ends$cap_unit[bad],
    cap_mm
  )
}

# Does `plot` put a square node at a start and at an end of its edges? A scene
# meant to check square ends that draws none would pass without checking any.
draws_square_ends <- function(plot) {
  ends <- edge_end_caps(plot)
  squares <- ends$shape %in% c(15, 22)
  all(c("start", "end") %in% ends$end[squares])
}

# The drawn edge tips ----------------------------------------------------------

# Draw `plot` off screen, force the grob tree so that ggraph's capped path grobs
# have cut their paths back at the caps, and return one row per drawn edge: the
# distance, in millimetres, from the node centre at each end of the edge to the
# end of the path that was drawn there. A capped path grob keeps the uncut path
# in native units, whose ends are the node centres, and draws the cut path as a
# child in millimetres, so both are read in the panel viewport it was drawn in.
# The arrowhead of a closed grid arrow has its tip at the end of the path.
drawn_edge_gaps <- function(plot, width = 7, height = 5) {
  file <- tempfile(fileext = ".png")
  open_test_ragg(file, width, height)
  reset_text_descent_cache()
  on.exit(
    {
      grDevices::dev.off()
      unlink(file)
      reset_text_descent_cache()
    },
    add = TRUE
  )

  gtable <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  grid::grid.newpage()
  grid::grid.draw(gtable)
  grid::grid.force()

  paths <- grid::grid.grep(
    "cappedpathgrob",
    grep = TRUE,
    global = TRUE,
    viewports = TRUE
  )

  gaps <- purrr::map(paths, \(path) {
    grob <- grid::grid.get(path)
    if (!inherits(grob, "cappedpathgrob") || length(grob$x) == 0) {
      return(NULL)
    }

    grid::upViewport(0)
    grid::downViewport(attr(path, "vpPath"))
    on.exit(grid::upViewport(0), add = TRUE)

    centre_x <- grid::convertX(grob$x, "mm", valueOnly = TRUE)
    centre_y <- grid::convertY(grob$y, "mm", valueOnly = TRUE)
    edge_ids <- unique(grob$id)

    drawn <- grob$children[[1]]
    drawn_ids <- if (inherits(drawn, "polyline")) unique(drawn$id) else NULL
    if (length(drawn_ids) != length(edge_ids)) {
      return(data.frame(
        edge = edge_ids,
        start_gap = NA_real_,
        end_gap = NA_real_
      ))
    }
    drawn_x <- grid::convertX(drawn$x, "mm", valueOnly = TRUE)
    drawn_y <- grid::convertY(drawn$y, "mm", valueOnly = TRUE)

    purrr::map(seq_along(edge_ids), \(k) {
      uncut <- which(grob$id == edge_ids[[k]])
      cut <- which(drawn$id == drawn_ids[[k]])
      gap <- function(uncut_point, cut_point) {
        sqrt(
          (centre_x[[uncut_point]] - drawn_x[[cut_point]])^2 +
            (centre_y[[uncut_point]] - drawn_y[[cut_point]])^2
        )
      }
      data.frame(
        edge = edge_ids[[k]],
        start_gap = gap(uncut[[1]], cut[[1]]),
        end_gap = gap(uncut[[length(uncut)]], cut[[length(cut)]])
      )
    }) |>
      purrr::list_rbind()
  })

  purrr::list_rbind(gaps)
}

expect_gap_mm <- function(actual, expected, what, tolerance = 0.2) {
  ok <- length(actual) == 1 &&
    !is.na(actual) &&
    abs(actual - expected) <= tolerance
  testthat::expect(
    ok,
    sprintf(
      "%s is drawn %s mm from the node centre; expected %.2f mm, within %.1f mm.",
      what,
      if (length(actual) == 1) sprintf("%.2f", actual) else "<no single edge>",
      expected,
      tolerance
    )
  )
  invisible(actual)
}

# The fontsizes of the point glyphs the node layer of `plot` draws in its first
# panel, and the shapes they are drawn with.
node_glyphs <- function(plot) {
  index <- which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$geom, c("GeomDagPoint", "GeomDagNode"))
  }))
  points_of <- function(grob) {
    if (inherits(grob, "points")) {
      return(list(grob))
    }
    children <- if (inherits(grob, "gList")) grob else grob$children
    purrr::list_flatten(purrr::map(as.list(children), points_of))
  }
  glyphs <- points_of(ggplot2::layer_grob(plot, index[[1]])[[1]])

  data.frame(
    fontsize = unlist(purrr::map(glyphs, \(glyph) glyph$gp$fontsize)),
    pch = unlist(purrr::map(glyphs, \(glyph) glyph$pch))
  )
}

# Caps on the built layers -----------------------------------------------------

test_that("ggdag() stops each edge 2 mm beyond the circle nodes at any size", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  for (node_size in c(8, 16, 30)) {
    p <- ggdag(cap_dag(), node_size = node_size)
    expect_equal(
      node_aware_cap_mismatches(p, node_size),
      character(),
      label = paste0("node-aware caps of ggdag(node_size = ", node_size, ")")
    )
  }
})

test_that("geom_dag() stops each edge 2 mm beyond the circle nodes at any size", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  for (node_size in c(8, 30)) {
    p <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
      geom_dag(node_size = node_size)
    expect_equal(
      node_aware_cap_mismatches(p, node_size),
      character(),
      label = paste0("node-aware caps of geom_dag(node_size = ", node_size, ")")
    )
  }
})

test_that("the node-aware cap scales with the plot's size multiplier", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggdag(cap_dag(), node_size = 20, size = 1.5)

  # the node is drawn at size 30, 11.25 mm in radius, and the 2 mm gap is
  # scaled to 3 mm with it
  expect_equal(
    node_aware_cap_mismatches(p, node_size = 20, size = 1.5),
    character()
  )
  expect_equal(unique(edge_end_caps(p)$cap_mm), 14.25)
})

test_that("ggdag_adjustment_set() stops edges beyond square and circle nodes", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  for (node_size in c(14, 16)) {
    p <- ggdag_adjustment_set(readme_dag(), node_size = node_size)
    expect_true(draws_square_ends(p))
    expect_equal(
      node_aware_cap_mismatches(p, node_size),
      character(),
      label = paste0(
        "node-aware caps of ggdag_adjustment_set(node_size = ",
        node_size,
        ")"
      )
    )
  }
})

test_that("ggdag_adjustment_set() caps a square end by its outline", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  ends <- edge_end_caps(ggdag_adjustment_set(readme_dag(), node_size = 16))

  # `v` is adjusted in the `{v, w1, w2}` panel only, so the edges leaving it
  # start at a square there and at a circle in the other two panels. The solid
  # square's half side is the circle's radius, so both caps reach 8 mm out
  # from the centre, one as a square and one as a circle.
  v_starts <- ends[ends$node == "v" & ends$end == "start", ]
  expect_setequal(unique(v_starts$shape), c(15, 19))
  expect_equal(unique(v_starts$cap_geometry[v_starts$shape == 15]), "rect")
  expect_equal(unique(v_starts$cap_mm[v_starts$shape == 15]), 0.375 * 16 + 2)
  expect_equal(unique(v_starts$cap_geometry[v_starts$shape == 19]), "circle")
  expect_equal(unique(v_starts$cap_mm[v_starts$shape == 19]), 0.375 * 16 + 2)
})

test_that("ggdag_adjust() stops edges beyond square and circle nodes", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  for (node_size in c(8, 30)) {
    p <- ggdag_adjust(controlled_dag(), var = "z", node_size = node_size)
    expect_true(draws_square_ends(p))
    expect_equal(
      node_aware_cap_mismatches(p, node_size),
      character(),
      label = paste0(
        "node-aware caps of ggdag_adjust(node_size = ",
        node_size,
        ")"
      )
    )
  }
})

test_that("the d-relationship plotters stop edges beyond the controlled nodes", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  plotters <- list(
    ggdag_drelationship = ggdag_drelationship,
    ggdag_dseparated = ggdag_dseparated,
    ggdag_dconnected = ggdag_dconnected
  )
  for (plotter in names(plotters)) {
    p <- plotters[[plotter]](
      controlled_dag(),
      from = "x",
      to = "y",
      controlling_for = "z",
      node_size = 30
    )
    expect_true(draws_square_ends(p), label = plotter)
    expect_equal(
      node_aware_cap_mismatches(p, 30),
      character(),
      label = paste0("node-aware caps of ", plotter, "(node_size = 30)")
    )
  }
})

test_that("ggdag_instrumental() stops edges beyond the nodes it draws", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  for (node_size in c(8, 30)) {
    p <- ggdag_instrumental(conditional_iv_dag(), node_size = node_size)
    expect_true(any(edge_end_caps(p)$shape == 15))
    expect_equal(
      node_aware_cap_mismatches(p, node_size),
      character(),
      label = paste0(
        "node-aware caps of ggdag_instrumental(node_size = ",
        node_size,
        ")"
      )
    )
  }

  unconditional <- ggdag_instrumental(
    dagify(y ~ x, x ~ z, exposure = "x", outcome = "y"),
    node_size = 30
  )
  expect_equal(node_aware_cap_mismatches(unconditional, 30), character())
})

test_that("stylized nodes take the extent of the ring they draw", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  # the ring's outermost glyph is the plain node's glyph, at the same size and
  # in the same shape, so the two node styles share one extent
  plain <- node_glyphs(ggdag(cap_dag(), node_size = 30))
  ring <- node_glyphs(ggdag(cap_dag(), node_size = 30, use_stylized = TRUE))
  expect_equal(max(ring$fontsize), max(plain$fontsize))
  expect_equal(unique(ring$pch), unique(plain$pch))

  for (node_size in c(8, 30)) {
    p <- ggdag(cap_dag(), node_size = node_size, use_stylized = TRUE)
    expect_equal(
      node_aware_cap_mismatches(p, node_size),
      character(),
      label = paste0("stylized caps at node_size = ", node_size)
    )

    adjusted <- ggdag_adjust(
      controlled_dag(),
      var = "z",
      node_size = node_size,
      use_stylized = TRUE
    )
    expect_true(draws_square_ends(adjusted))
    expect_equal(
      node_aware_cap_mismatches(adjusted, node_size),
      character(),
      label = paste0("stylized square caps at node_size = ", node_size)
    )
  }
})

test_that("R draws a solid square node with the circle's radius as its half side", {
  skip_if_not_installed("ragg")

  # the square extent above rests on this: the ink of a shape 15 node is as
  # wide as the ink of a borderless shape 16 circle, and wider than a square
  # of the circle's area
  ink_width_mm <- function(shape, size, res = 300) {
    plot <- ggplot(data.frame(x = 0, y = 0), aes(x, y)) +
      geom_dag_point(size = size, shape = shape) +
      theme_void() +
      coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))
    capture <- ragg::agg_capture(
      width = 60,
      height = 60,
      units = "mm",
      res = res
    )
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plot)
    raster <- capture(native = FALSE)
    ink <- raster != "white" & raster != "#FFFFFFFF" & raster != "transparent"
    sum(apply(ink, 2, any)) / res * 25.4
  }

  for (size in c(16, 30)) {
    square <- ink_width_mm(15, size)
    circle <- ink_width_mm(16, size)
    expect_lt(abs(square - circle), 0.3)
    expect_gt(square - circle * sqrt(pi / 4), 1)
  }
})

# The drawn tips ---------------------------------------------------------------

test_that("an edge into a circle node is drawn 2 mm beyond the circle", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  for (node_size in c(8, 30)) {
    gaps <- drawn_edge_gaps(ggdag(dagify(y ~ x), node_size = node_size))
    expect_equal(nrow(gaps), 1)

    circle_gap <- 0.375 * node_size + 2
    expect_gap_mm(
      gaps$start_gap,
      circle_gap,
      paste0("the start of x -> y at node_size = ", node_size)
    )
    expect_gap_mm(
      gaps$end_gap,
      circle_gap,
      paste0("the arrowhead tip of x -> y at node_size = ", node_size)
    )
  }
})

test_that("an edge into a square node is drawn 2 mm outside its outline", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  for (node_size in c(8, 30)) {
    p <- ggdag_adjust(dagify(y ~ x), var = "y", node_size = node_size)
    gaps <- drawn_edge_gaps(p)
    expect_equal(nrow(gaps), 1)

    expect_gap_mm(
      gaps$start_gap,
      0.375 * node_size + 2,
      paste0("the start of x -> y at the circle x, node_size = ", node_size)
    )

    # the tip lies on the square 2 mm outside the node, so the farther of its
    # offsets from the centre is the half side and the gap
    ends <- drawn_edge_ends(p)
    square_end <- ends[ends$end == "end", , drop = FALSE]
    stopifnot(nrow(square_end) == 1, is_square_shape(square_end$shape))
    expect_gap_mm(
      outline_distance_mm(
        square_end$shape,
        square_end$tip_dx,
        square_end$tip_dy
      ),
      0.375 * node_size + 2,
      paste0(
        "the arrowhead tip of x -> y outside the square y, node_size = ",
        node_size
      ),
      tolerance = 0.05
    )
  }

  # an edge that meets the square at an angle stops where it crosses the
  # square, farther from the centre than a circle through its faces would
  # stop it
  angled <- tidy_dagitty(dagify(
    m ~ x + a,
    y ~ m,
    coords = list(
      x = c(x = 0, a = 0, m = 1, y = 2),
      y = c(x = 0, a = 1, m = 0.5, y = 0.5)
    )
  )) |>
    dplyr::mutate(shape = ifelse(name == "m", 15, 19))
  p <- ggplot(angled, aes_dag()) +
    geom_dag_point(aes(shape = shape), size = 30) +
    scale_shape_identity() +
    geom_dag_edges() +
    theme_dag()
  ends <- drawn_edge_ends(p)
  stopifnot(
    nrow(ends) == 6,
    any(
      is_square_shape(ends$shape) & abs(ends$tip_dx) > 2 & abs(ends$tip_dy) > 2
    )
  )
  expect_equal(tip_gap_mismatches(ends), character())
})

# Explicit caps ----------------------------------------------------------------

test_that("an explicit edge_cap stays a fixed cap at every node size", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  for (node_size in c(8, 16, 30)) {
    expect_equal(
      fixed_cap_mismatches(
        ggdag(cap_dag(), node_size = node_size, edge_cap = 5),
        5
      ),
      character(),
      label = paste0("ggdag(edge_cap = 5, node_size = ", node_size, ")")
    )
    expect_equal(
      fixed_cap_mismatches(
        ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
          geom_dag(node_size = node_size, edge_cap = 5),
        5
      ),
      character(),
      label = paste0("geom_dag(edge_cap = 5, node_size = ", node_size, ")")
    )
  }

  # the fixed cap scales with the plot, as it always has
  expect_equal(
    fixed_cap_mismatches(ggdag(cap_dag(), edge_cap = 5, size = 2), 10),
    character()
  )
})

test_that("a set edge_cap option stays a fixed cap at every node size", {
  local_ggdag_option_state()
  ggdag_options_set(edge_cap = 5)

  for (node_size in c(8, 16, 30)) {
    expect_equal(
      fixed_cap_mismatches(ggdag(cap_dag(), node_size = node_size), 5),
      character(),
      label = paste0("ggdag(node_size = ", node_size, ") under the option")
    )
  }
})

# The six plotters that draw controlled nodes as squares, each called on a DAG
# it can draw at `node_size` with `...` passed on.
square_plotter_calls <- function(node_size, ...) {
  list(
    ggdag_adjustment_set = ggdag_adjustment_set(
      controlled_dag(),
      node_size = node_size,
      ...
    ),
    ggdag_adjust = ggdag_adjust(
      controlled_dag(),
      var = "z",
      node_size = node_size,
      ...
    ),
    ggdag_instrumental = ggdag_instrumental(
      conditional_iv_dag(),
      node_size = node_size,
      ...
    ),
    ggdag_drelationship = ggdag_drelationship(
      controlled_dag(),
      from = "x",
      to = "y",
      controlling_for = "z",
      node_size = node_size,
      ...
    ),
    ggdag_dseparated = ggdag_dseparated(
      controlled_dag(),
      from = "x",
      to = "y",
      controlling_for = "z",
      node_size = node_size,
      ...
    ),
    ggdag_dconnected = ggdag_dconnected(
      controlled_dag(),
      from = "x",
      to = "y",
      controlling_for = "z",
      node_size = node_size,
      ...
    )
  )
}

test_that("the square-node plotters keep the proportional edge_cap option", {
  local_ggdag_option_state()
  ggdag_options_set(edge_cap = 4)

  for (node_size in c(8, 30)) {
    plots <- square_plotter_calls(node_size)
    for (plotter in names(plots)) {
      # these plotters scale the option by 10 / 8
      expect_equal(
        fixed_cap_mismatches(plots[[plotter]], 5),
        character(),
        label = paste0(
          plotter,
          "(node_size = ",
          node_size,
          ") under the option"
        )
      )
    }
  }
})

test_that("the square-node plotters keep an explicit edge_cap fixed", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  plots <- square_plotter_calls(30, edge_cap = 5)
  for (plotter in names(plots)) {
    expect_equal(
      fixed_cap_mismatches(plots[[plotter]], 5),
      character(),
      label = paste0(plotter, "(edge_cap = 5, node_size = 30)")
    )
  }
})

# Defaults ---------------------------------------------------------------------

test_that("the default ggdag() caps every end at 8 mm, as an explicit 8 does", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  default_caps <- edge_end_caps(ggdag(cap_dag()))
  explicit_caps <- edge_end_caps(ggdag(cap_dag(), edge_cap = 8))

  expect_equal(fixed_cap_mismatches(ggdag(cap_dag()), 8), character())
  expect_identical(default_caps, explicit_caps)

  default_geom <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) + geom_dag()
  expect_equal(fixed_cap_mismatches(default_geom, 8), character())
})

test_that("unsetting or resetting the edge_cap option brings back node-aware caps", {
  local_ggdag_option_state()

  ggdag_options_set(edge_cap = 5)
  ggdag_options_reset()
  expect_null(ggdag_options_get("edge_cap"))
  expect_equal(
    node_aware_cap_mismatches(ggdag(cap_dag(), node_size = 30), 30),
    character(),
    label = "node-aware caps after ggdag_options_reset()"
  )

  ggdag_options_set(edge_cap = 5)
  ggdag_options_set(edge_cap = NULL)
  expect_equal(
    node_aware_cap_mismatches(ggdag(cap_dag(), node_size = 30), 30),
    character(),
    label = "node-aware caps after unsetting the option"
  )
})

# The ggarrow engine -----------------------------------------------------------

# What the ggarrow edge layers of `plot` are built with and build.
arrow_layer_state <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  indices <- which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$geom, c("GeomArrow", "GeomDAGArrow", "GeomDAGArrowCurve"))
  }))

  purrr::map(indices, \(i) {
    layer <- plot$layers[[i]]
    list(
      geom = class(layer$geom)[[1]],
      geom_params = layer$geom_params,
      aes_params = layer$aes_params,
      stat_params = layer$stat_params,
      data = built$data[[i]]
    )
  })
}

test_that("the ggarrow engine resects each end beyond the node drawn there", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  circle_cap <- 0.375 * 30 + 2

  default_arrows <- arrow_layer_state(
    ggdag(cap_dag(), node_size = 30, edge_engine = "ggarrow")
  )
  explicit_arrows <- arrow_layer_state(
    ggdag(cap_dag(), node_size = 30, edge_engine = "ggarrow", edge_cap = 8)
  )

  # the layers carry the cap of a circle node of the plot's size for an end
  # with no node drawn at it, and resect every drawn end by the node there
  expect_length(default_arrows, 2)
  for (arrows in default_arrows) {
    expect_identical(
      arrows$geom_params$resect,
      list(head = circle_cap, fins = circle_cap)
    )
    expect_equal(unique(arrows$data$resect_head), circle_cap)
    expect_equal(unique(arrows$data$resect_fins), circle_cap)
  }
  # an explicit cap fixes the resection and the layers follow no node
  expect_length(explicit_arrows, 2)
  for (arrows in explicit_arrows) {
    expect_identical(arrows$geom_params$resect, list(head = 8, fins = 8))
    expect_false("resect_head" %in% names(arrows$data))
  }
  expect_false(any(purrr::map_lgl(
    ggdag(cap_dag(), node_size = 30, edge_engine = "ggarrow")$layers,
    \(layer) inherits(layer$geom, "GeomDAGEdgePath")
  )))
})

# Every other plotter ----------------------------------------------------------

# The plotters that draw every node as a circle, each called on a DAG it can
# draw at `node_size` with `...` passed on.
circle_plotter_calls <- function(node_size, ...) {
  list(
    ggdag_status = ggdag_status(cap_dag(), node_size = node_size, ...),
    ggdag_collider = ggdag_collider(cap_dag(), node_size = node_size, ...),
    ggdag_canonical = ggdag_canonical(cap_dag(), node_size = node_size, ...),
    ggdag_exogenous = ggdag_exogenous(cap_dag(), node_size = node_size, ...),
    ggdag_children = ggdag_children(
      cap_dag(),
      "z",
      node_size = node_size,
      ...
    ),
    ggdag_parents = ggdag_parents(cap_dag(), "y", node_size = node_size, ...),
    ggdag_ancestors = ggdag_ancestors(
      cap_dag(),
      "y",
      node_size = node_size,
      ...
    ),
    ggdag_descendants = ggdag_descendants(
      cap_dag(),
      "z",
      node_size = node_size,
      ...
    ),
    ggdag_markov_blanket = ggdag_markov_blanket(
      cap_dag(),
      "x",
      node_size = node_size,
      ...
    ),
    ggdag_adjacent = ggdag_adjacent(
      cap_dag(),
      "x",
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

test_that("every circle-node plotter stops each edge beyond its nodes", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  for (node_size in c(8, 30)) {
    plots <- circle_plotter_calls(node_size)
    expect_length(plots, 24)
    for (plotter in names(plots)) {
      expect_equal(
        node_aware_cap_mismatches(plots[[plotter]], node_size),
        character(),
        label = paste0(
          "node-aware caps of ",
          plotter,
          "(node_size = ",
          node_size,
          ")"
        )
      )
    }
  }
})

test_that("the circle-node plotters keep an explicit edge_cap fixed", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  plots <- circle_plotter_calls(30, edge_cap = 5)
  for (plotter in names(plots)) {
    expect_equal(
      fixed_cap_mismatches(plots[[plotter]], 5),
      character(),
      label = paste0(plotter, "(edge_cap = 5, node_size = 30)")
    )
  }
})

test_that("the fan and the equivalence class cap their own layers by the nodes", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  # both plotters build their ggraph edge layers themselves, and an unset cap
  # handed to them explicitly is the same request as the default
  plots <- list(
    fan = ggdag_paths_fan(cap_dag(), node_size = 30, edge_cap = NULL),
    class = ggdag_equivalent_class(
      dagify(y ~ x, x ~ z, y ~ z),
      node_size = 30,
      edge_cap = NULL
    ),
    scaled_fan = ggdag_paths_fan(cap_dag(), node_size = 20, size = 1.5)
  )

  expect_equal(node_aware_cap_mismatches(plots$fan, 30), character())
  expect_equal(node_aware_cap_mismatches(plots$class, 30), character())
  expect_equal(
    node_aware_cap_mismatches(plots$scaled_fan, 20, size = 1.5),
    character()
  )
  expect_false(anyNA(edge_end_caps(plots$class)$cap_mm))

  # A circle node drawn at the size the plotter is asked for has the extent
  # of the cap the plotter maps, so these scenes draw a wider node layer over
  # the plotter's own: an edge that follows the nodes stops 2 mm beyond the
  # widest node at its end, and one that keeps the mapped cap does not.
  wider <- list(
    fan = ggdag_paths_fan(cap_dag(), node_size = 30) +
      geom_dag_point(size = 40),
    class = ggdag_equivalent_class(
      dagify(y ~ x, x ~ z, y ~ z),
      node_size = 30
    ) +
      geom_dag_point(size = 40)
  )
  for (plotter in names(wider)) {
    ends <- edge_end_caps(wider[[plotter]])
    expect_gt(nrow(ends), 0)
    expect_equal(
      unique(ends$cap_mm),
      0.375 * 40 + 2,
      label = paste(plotter, "caps under a wider node layer")
    )
  }
})

# Draw `plot` off screen and report whether it drew.
draws_on_device <- function(plot) {
  file <- tempfile(fileext = ".png")
  open_test_ragg(file, 7, 5, res = 72)
  on.exit(
    {
      grDevices::dev.off()
      unlink(file)
    },
    add = TRUE
  )
  print(plot)
  invisible(TRUE)
}

test_that("the path and equivalence plotters resect ggarrow edges with the cap unset", {
  skip_if_not_installed("ragg")
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  calls <- list(
    ggdag_paths = \(...) ggdag_paths(cap_dag(), ...),
    ggdag_paths_fan = \(...) ggdag_paths_fan(cap_dag(), ...),
    ggdag_equivalent_dags = \(...) {
      ggdag_equivalent_dags(dagify(y ~ x, x ~ z), ...)
    },
    ggdag_equivalent_class = \(...) {
      ggdag_equivalent_class(dagify(y ~ x, x ~ z, y ~ z), ...)
    }
  )

  for (plotter in names(calls)) {
    call <- calls[[plotter]]
    default <- call(node_size = 30, edge_engine = "ggarrow")
    unset <- call(node_size = 30, edge_engine = "ggarrow", edge_cap = NULL)
    explicit <- call(node_size = 30, edge_engine = "ggarrow", edge_cap = 8)

    expect_no_error(draws_on_device(default), message = plotter)
    expect_no_error(draws_on_device(unset), message = plotter)
    # an unset cap handed to the plotter is the same request as the default:
    # every end follows the node drawn there, a circle of size 30 here
    default_state <- arrow_layer_state(default)
    expect_identical(
      default_state,
      arrow_layer_state(unset),
      label = paste0(plotter, "(edge_cap = NULL) ggarrow layers")
    )
    expect_gt(length(default_state), 0)
    for (arrows in default_state) {
      if (nrow(arrows$data) == 0) {
        next
      }
      expect_equal(
        unique(c(arrows$data$resect_head, arrows$data$resect_fins)),
        0.375 * 30 + 2,
        label = paste0(plotter, "() ggarrow resections")
      )
    }
    # an explicit cap fixes the resection at every end
    for (arrows in arrow_layer_state(explicit)) {
      expect_identical(
        arrows$geom_params$resect,
        list(head = 8, fins = 8),
        label = paste0(plotter, "(edge_cap = 8) ggarrow layers")
      )
    }
  }
})

# Plots assembled by hand ------------------------------------------------------

# The edge ends of `plot` whose cap is not 2 mm outside the node drawn there,
# at whatever size and shape that node is drawn with.
drawn_node_cap_mismatches <- function(plot) {
  ends <- edge_end_caps(plot)
  if (nrow(ends) == 0) {
    return("the plot builds no ggraph edge ends")
  }

  expected <- expected_outline_mm(ends$shape, ends$node_size) + 2
  geometry <- expected_cap_geometry(ends$shape)
  bad <- is.na(expected) |
    ends$cap_geometry != geometry |
    abs(ends$cap_mm - expected) > 1e-6

  sprintf(
    "panel %s, edge %s: the %s cap at %s (a %s drawn at size %s) is a %s of %.4f mm; expected a %s of %.4f mm",
    ends$panel[bad],
    ends$edge[bad],
    ends$end[bad],
    ends$node[bad],
    node_shape_name(ends$shape[bad]),
    ends$node_size[bad],
    ends$cap_geometry[bad],
    ends$cap_mm[bad],
    geometry[bad],
    expected[bad]
  )
}

hand_built_edge_layers <- list(
  geom_dag_edges = geom_dag_edges,
  geom_dag_edges_link = geom_dag_edges_link,
  geom_dag_edges_arc = geom_dag_edges_arc,
  geom_dag_edges_diagonal = geom_dag_edges_diagonal,
  geom_dag_edges_fan = geom_dag_edges_fan
)

test_that("hand-built edge layers stop beyond the nodes in either layer order", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  tidy_dag <- tidy_dagitty(cap_dag())

  scenes <- list(
    list(node_layer = geom_dag_point, name = "geom_dag_point", node_size = 8),
    list(node_layer = geom_dag_point, name = "geom_dag_point", node_size = 30),
    list(node_layer = geom_dag_node, name = "geom_dag_node", node_size = 30)
  )

  for (edges in names(hand_built_edge_layers)) {
    edge_layer <- hand_built_edge_layers[[edges]]
    for (scene in scenes) {
      nodes <- scene$node_layer(size = scene$node_size)
      nodes_first <- ggplot(tidy_dag, aes_dag()) + nodes + edge_layer()
      edges_first <- ggplot(tidy_dag, aes_dag()) + edge_layer() + nodes

      for (order in c("nodes first", "edges first")) {
        p <- if (order == "nodes first") nodes_first else edges_first
        expect_equal(
          node_aware_cap_mismatches(p, scene$node_size),
          character(),
          label = sprintf(
            "%s() with %s(size = %s), %s",
            edges,
            scene$name,
            scene$node_size,
            order
          )
        )
      }
    }
  }
})

test_that("hand-built edge layers stop outside square nodes by their outline", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
    geom_dag_point(shape = 15, size = 30) +
    geom_dag_edges()

  expect_true(all(edge_end_caps(p)$shape == 15))
  expect_equal(node_aware_cap_mismatches(p, 30), character())
  expect_equal(unique(edge_end_caps(p)$cap_geometry), "rect")
  expect_equal(unique(round(edge_end_caps(p)$cap_mm, 2)), 13.25)
})

test_that("hand-built edge layers follow a node size mapped to the data", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
    geom_dag_point(aes(size = x)) +
    geom_dag_edges() +
    scale_size(range = c(8, 30))

  ends <- edge_end_caps(p)
  expect_gt(length(unique(ends$node_size)), 1)
  expect_equal(drawn_node_cap_mismatches(p), character())
})

test_that("a cap the user maps on a hand-built edge layer wins at its end", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
    geom_dag_point(size = 30) +
    geom_dag_edges_link(aes(start_cap = ggraph::circle(5, "mm")))

  ends <- edge_end_caps(p)
  expect_equal(unique(ends$cap_mm[ends$end == "start"]), 5)
  expect_equal(unique(ends$cap_mm[ends$end == "end"]), 0.375 * 30 + 2)

  both <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
    geom_dag_point(size = 30) +
    geom_dag_edges_link(
      aes(
        start_cap = ggraph::circle(5, "mm"),
        end_cap = ggraph::circle(5, "mm")
      )
    )
  expect_equal(fixed_cap_mismatches(both, 5), character())
})

test_that("a cap the plot maps for a hand-built edge layer wins at its end", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  # the layer inherits the plot's mapping, so the cap it draws at the end is
  # the one the plot maps, and the labels cut the edges there
  p <- ggplot(
    tidy_dagitty(controlled_dag()),
    aes_dag(end_cap = ggraph::circle(4, "mm"))
  ) +
    geom_dag_point(size = 30) +
    geom_dag_edges_link() +
    geom_dag_label_auto(aes(label = name))

  ends <- edge_end_caps(p)
  expect_equal(unique(ends$cap_mm[ends$end == "start"]), 0.375 * 30 + 2)
  expect_equal(unique(ends$cap_mm[ends$end == "end"]), 4)
  expect_equal(unique(label_edge_caps(p)$cap_end), 4)
  expect_equal(unique(label_edge_caps(p)$cap_start), 0.375 * 30 + 2)

  # a layer that does not inherit the plot's mapping follows the nodes
  own <- ggplot(
    tidy_dagitty(controlled_dag()),
    aes_dag(end_cap = ggraph::circle(4, "mm"))
  ) +
    geom_dag_point(size = 30) +
    geom_dag_edges_link(aes_dag(), inherit.aes = FALSE)
  expect_equal(node_aware_cap_mismatches(own, 30), character())
})

test_that("a hand-built edge layer with no node layer keeps the 8 mm cap", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) + geom_dag_edges_link()

  expect_equal(edge_cap_radii(p), 8)
})

# Shape scales, shared layers, and panels ---------------------------------------

test_that("a shape scale that names its shapes gives square nodes their outline", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggplot(control_for(controlled_dag(), "z"), aes_dag()) +
    geom_dag_point(aes(shape = adjusted), size = 30) +
    geom_dag_edges() +
    scale_shape_manual(
      values = c(adjusted = "square filled", unadjusted = "circle")
    )

  ends <- edge_end_caps(p)
  square <- ends$shape %in% "square filled"
  expect_true(any(square & ends$end == "start"))
  expect_true(any(square & ends$end == "end"))

  # R draws the filled square with the area of the circle
  expect_equal(unique(ends$cap_geometry[square]), "rect")
  expect_equal(
    unique(round(ends$cap_mm[square], 4)),
    round(0.375 * 30 * sqrt(pi / 4) + 2, 4)
  )
  expect_equal(unique(round(ends$cap_mm[square], 1)), 12)
  expect_equal(unique(ends$cap_geometry[!square]), "circle")
  expect_equal(unique(ends$cap_mm[!square]), 0.375 * 30 + 2)
})

test_that("edge layers shared between plots take the caps of the plot built", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  large <- ggdag(cap_dag(), node_size = 30)
  edge_layers <- large$layers[dag_edge_layer_indices(large)]
  small <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
    geom_dag_point(size = 8)
  small$layers <- c(small$layers, edge_layers)

  expect_equal(node_aware_cap_mismatches(large, 30), character())
  expect_equal(node_aware_cap_mismatches(small, 8), character())
  expect_equal(node_aware_cap_mismatches(large, 30), character())

  # A build that fails after the stats are computed never reaches the step
  # that lets the nodes go, so the shared layers still hold the large nodes
  # when the small plot is built next.
  failing <- large +
    ggplot2::geom_text(ggplot2::aes(label = ggplot2::after_stat(nonexistent)))
  expect_error(ggplot2::ggplot_build(failing))
  expect_equal(node_aware_cap_mismatches(small, 8), character())
  expect_equal(node_aware_cap_mismatches(large, 30), character())
})

test_that("an edge in a panel only the edge layer adds finds that panel's nodes", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  dag_data <- pull_dag_data(tidy_dagitty(dagify(y ~ x)))
  in_panel <- function(panel, shape) {
    dplyr::mutate(dag_data, panel = panel, shape = shape)
  }
  node_data <- rbind(in_panel("b", 15), in_panel("c", 19))
  # the edge layer draws a panel `a` that sorts before the panels the nodes
  # are drawn in, so the whole plot numbers the panels differently from a
  # build of its node layers alone
  edge_data <- rbind(in_panel("a", 19), node_data)

  p <- ggplot(node_data, aes_dag()) +
    geom_dag_point(aes(shape = shape), size = 30) +
    geom_dag_edges_link(data = edge_data) +
    scale_shape_identity() +
    facet_wrap(~panel)

  built <- ggplot2::ggplot_build(p)
  edges <- built$data[[dag_edge_layer_indices(p)]]
  panel_names <- as.character(built$layout$layout$panel)
  caps <- tapply(
    unclass(edges$start_cap)$width / 2,
    panel_names[as.integer(edges$PANEL)],
    unique
  )

  geometries <- tapply(
    unclass(edges$start_cap)$geometry,
    panel_names[as.integer(edges$PANEL)],
    unique
  )

  expect_equal(
    unlist(as.list(caps)),
    c(a = 0.375 * 16 + 2, b = 0.375 * 30 + 2, c = 0.375 * 30 + 2)
  )
  expect_equal(
    unlist(as.list(geometries)),
    c(a = "circle", b = "rect", c = "circle")
  )
})

# The automatic labels ---------------------------------------------------------

test_that("a forced test draw measures its text on the device it draws on", {
  skip_if_not_installed("ragg")

  # R's graphics engine keeps the metrics of the last "M" it measured, keyed
  # by the device's address rather than its resolution, so a device opened
  # where a closed one of another resolution stood could read that device's
  # text height, and draw a panel a pixel or two taller than it is
  plot <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_point()
  panel_mm <- function(built) {
    viewports <- grid::grid.ls(viewports = TRUE, print = FALSE)$name
    grid::seekViewport(grep("^panel", viewports, value = TRUE)[[1]])
    on.exit(grid::upViewport(0), add = TRUE)
    c(
      grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE),
      grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
    )
  }
  measure_on_other_device <- function() {
    file <- tempfile(fileext = ".png")
    ragg::agg_png(file, width = 7, height = 5, units = "in", res = 72)
    on.exit(
      {
        grDevices::dev.off()
        unlink(file)
      },
      add = TRUE
    )
    grid::grobHeight(grid::textGrob("M", gp = grid::gpar(fontsize = 8.8))) |>
      grid::convertHeight("mm")
  }

  expected <- with_forced_plot(plot, panel_mm)
  for (attempt in 1:12) {
    measure_on_other_device()
    # vary what the process holds, and with it where the next device lies
    held <- raw(attempt * 97)
    expect_equal(
      with_forced_plot(plot, panel_mm),
      expected,
      tolerance = 1e-10,
      label = paste("the panel drawn after a 72 dpi device, attempt", attempt)
    )
  }
})

test_that("the automatic labels cut each edge where the drawn edge stops", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  labelled <- dagify(
    y ~ x + z,
    x ~ z,
    z ~ a,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder", a = "Cause")
  )
  p <- ggdag_adjust(labelled, var = "z", node_size = 30, use_labels = TRUE)
  stopifnot(draws_square_ends(p))

  caps <- label_edge_caps(p)
  expect_gt(nrow(caps), 0)

  node_index <- which(purrr::map_lgl(p$layers, \(layer) {
    inherits(layer$geom, "GeomDagPoint")
  }))[[1]]
  drawn <- ggplot2::layer_data(p, node_index)
  drawn <- drawn[drawn$PANEL == 1, , drop = FALSE]
  shape_at <- function(x, y) {
    rows <- purrr::map_int(seq_along(x), \(i) {
      node_row_at(drawn, x[[i]], y[[i]])
    })
    drawn$shape[rows]
  }
  start_shape <- shape_at(caps$x, caps$y)
  end_shape <- shape_at(caps$xend, caps$yend)
  expect_false(anyNA(c(start_shape, end_shape)))
  expect_true(any(c(start_shape, end_shape) == 15))

  circle_start <- !is_square_shape(start_shape)
  circle_end <- !is_square_shape(end_shape)
  expect_equal(
    caps$cap_start[circle_start],
    expected_outline_mm(start_shape[circle_start], 30) + 2
  )
  expect_equal(
    caps$cap_end[circle_end],
    expected_outline_mm(end_shape[circle_end], 30) + 2
  )

  # How far a square end is cut back from the node centre depends on the
  # angle the edge meets the square at, in millimetres, so the traced edges
  # are read where the plot is drawn and their cut ends compared with the tips
  # of the drawn edges.
  skip_if_not_installed("ragg")
  ends <- drawn_and_traced_ends(p)
  drawn <- ends$drawn
  stopifnot(any(
    is_square_shape(drawn$shape) &
      abs(drawn$tip_dx) > 2 &
      abs(drawn$tip_dy) > 2
  ))
  expect_equal(tip_gap_mismatches(drawn), character())
  expect_equal(label_tip_mismatches(ends$traced, drawn), character())
})

test_that("hand-built automatic labels cut edges beyond the nodes", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
    geom_dag_point(size = 30) +
    geom_dag_edges() +
    geom_dag_label_auto(aes(label = name))
  labels_first <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
    geom_dag_label_auto(aes(label = name)) +
    geom_dag_edges() +
    geom_dag_point(size = 30)

  expect_equal(unique(label_edge_caps(p)$cap_start), 0.375 * 30 + 2)
  expect_equal(unique(label_edge_caps(p)$cap_end), 0.375 * 30 + 2)
  expect_equal(unique(label_edge_caps(labels_first)$cap_start), 0.375 * 30 + 2)
  expect_equal(unique(label_edge_caps(labels_first)$cap_end), 0.375 * 30 + 2)
})

test_that("the automatic labels' single cap scales its gap with the plot", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  labelled <- dagify(y ~ x, labels = c(x = "Exposure", y = "Outcome"))

  p <- ggdag(labelled, node_size = 30, size = 1.5, use_labels = TRUE)
  index <- which(purrr::map_lgl(p$layers, \(layer) {
    inherits(layer$stat, "StatNodesLabelAuto")
  }))
  grob <- ggplot2::layer_grob(p, index[[1]])[[1]]

  # the node is drawn at size 45, 16.875 mm in radius, and the 2 mm gap is
  # scaled to 3 mm with it, at each end and in the cap an end without one
  # of its own is cut by
  circle_cap <- 0.375 * 45 + 3
  expect_equal(unique(label_edge_caps(p)$cap_start), circle_cap)
  expect_equal(unique(label_edge_caps(p)$cap_end), circle_cap)
  expect_equal(grob$params$edge_cap, circle_cap)
})

test_that("the automatic labels cut an edge at a cap the user sets at its end", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  tidy_dag <- tidy_dagitty(cap_dag())
  circle_cap <- 0.375 * 30 + 2

  mapped <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_point(size = 30) +
    geom_dag_edges_link(aes(start_cap = ggraph::circle(3, "mm"))) +
    geom_dag_label_auto(aes(label = name))
  labels_first <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_label_auto(aes(label = name)) +
    geom_dag_edges_link(aes(start_cap = ggraph::circle(3, "mm"))) +
    geom_dag_point(size = 30)
  fixed <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_point(size = 30) +
    geom_dag_edges_link(end_cap = ggraph::circle(0.5, "cm")) +
    geom_dag_label_auto(aes(label = name))

  for (p in list(mapped, labels_first)) {
    drawn <- edge_end_caps(p)
    stopifnot(
      all(drawn$cap_mm[drawn$end == "start"] == 3),
      all(drawn$cap_mm[drawn$end == "end"] == circle_cap)
    )
    expect_equal(unique(label_edge_caps(p)$cap_start), 3)
    expect_equal(unique(label_edge_caps(p)$cap_end), circle_cap)
  }
  expect_equal(unique(label_edge_caps(fixed)$cap_start), circle_cap)
  expect_equal(unique(label_edge_caps(fixed)$cap_end), 5)

  # a cap mapped from the data is read for each edge
  per_edge <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_point(size = 30) +
    geom_dag_edges_link(
      aes(start_cap = ggraph::circle(ifelse(name == "z", 3, 5), "mm"))
    ) +
    geom_dag_label_auto(aes(label = name))
  caps <- label_edge_caps(per_edge)
  nodes <- node_coords(per_edge)
  z <- nodes[nodes$name == "z", ]
  from_z <- abs(caps$x - z$x) < 1e-9 & abs(caps$y - z$y) < 1e-9
  expect_true(any(from_z) && !all(from_z))
  expect_equal(unique(caps$cap_start[from_z]), 3)
  expect_equal(unique(caps$cap_start[!from_z]), 5)
  expect_equal(unique(caps$cap_end), circle_cap)
})

test_that("the automatic labels cut each edge between two nodes by the layer that draws it", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  # A directed edge and a bidirected arc between the same two nodes, drawn by
  # two layers with caps of their own. The labels trace the arc and the
  # straight edge, and cut each by the caps of the layer that draws it. The
  # straight edge was once not traced at all, and the arc cut by the caps of
  # the straight layer.
  bow <- tidy_dagitty(dagify(
    y ~ x,
    x ~ ~y,
    coords = list(x = c(x = 0, y = 1), y = c(x = 0, y = 0))
  ))
  circle_cap <- 0.375 * 30 + 2
  plots <- list(
    ggraph = ggplot(bow, aes_dag()) +
      geom_dag_point(size = 30) +
      geom_dag_edges_link(
        data = filter_direction("->"),
        start_cap = ggraph::circle(3, "mm"),
        end_cap = ggraph::circle(3, "mm")
      ) +
      geom_dag_edges_arc(
        data = filter_direction("<->"),
        start_cap = ggraph::circle(10, "mm"),
        end_cap = ggraph::circle(10, "mm")
      ) +
      geom_dag_label_auto(aes(label = name)),
    ggarrow = ggplot(bow, aes_dag()) +
      geom_dag_point(size = 30) +
      geom_dag_arrow(data = filter_direction("->"), resect = 3) +
      geom_dag_arrow_arc(data = filter_direction("<->"), resect = 10) +
      geom_dag_label_auto(aes(label = name))
  )
  for (engine in names(plots)) {
    caps <- label_edge_caps(plots[[engine]])
    caps <- caps[order(!is.na(caps$layer)), , drop = FALSE]
    expect_equal(is.na(caps$layer), c(TRUE, FALSE), label = engine)
    expect_equal(caps$cap_start, c(3, 10), label = engine)
    expect_equal(caps$cap_end, c(3, 10), label = engine)
  }

  # and the plotters' own layers, whose caps follow the nodes, trace both
  for (edges in list(
    geom_dag_edges(),
    geom_dag_arrows(),
    geom_dag_edges(edge_engine = "ggarrow")
  )) {
    p <- ggplot(bow, aes_dag()) +
      geom_dag_point(size = 30) +
      edges +
      geom_dag_label_auto(aes(label = name))
    caps <- label_edge_caps(p)
    expect_equal(nrow(caps), 2)
    expect_equal(unique(c(caps$cap_start, caps$cap_end)), circle_cap)
  }

  # the edges the labels trace stop where the drawn edges do, the tip of a
  # ggarrow arc within the sagitta of the chord its head is drawn along
  skip_if_not_installed("ragg")
  ends <- drawn_and_traced_ends(plots$ggraph)
  stopifnot(nrow(ends$traced) == 2)
  expect_equal(
    label_tip_mismatches(ends$traced, ends$drawn, tolerance = 0.35),
    character()
  )
  ends <- drawn_and_traced_ends(plots$ggarrow)
  stopifnot(nrow(ends$traced) == 2)
  expect_equal(label_cap_mismatches(ends$traced, ends$drawn), character())
})

test_that("the automatic labels cut an edge two layers draw by the nearer cap", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  tidy_dag <- tidy_dagitty(controlled_dag())
  circle_cap <- 0.375 * 30 + 2
  labelled <- function(...) {
    ggplot(tidy_dag, aes_dag()) +
      geom_dag_point(size = 30) +
      list(...) +
      geom_dag_label_auto(aes(label = name))
  }

  # the label keeps clear of the ink of both layers, which reaches out to the
  # smaller cap at each end, whichever layer is added first
  three <- geom_dag_edges_link(end_cap = ggraph::circle(3, "mm"))
  six <- geom_dag_edges_link(end_cap = ggraph::circle(6, "mm"))
  following <- geom_dag_edges_link()
  scenes <- list(
    `3 then 6` = list(plot = labelled(three, six), start = circle_cap),
    `6 then 3` = list(plot = labelled(six, three), start = circle_cap),
    `following then 3` = list(
      plot = labelled(following, three),
      start = circle_cap
    ),
    `3 then following` = list(
      plot = labelled(three, following),
      start = circle_cap
    )
  )
  for (name in names(scenes)) {
    caps <- label_edge_caps(scenes[[name]]$plot)
    expect_equal(unique(caps$cap_end), 3, label = name)
    expect_equal(unique(caps$cap_start), scenes[[name]]$start, label = name)
  }

  # a cap wider than the node's own is not the nearer one where a layer that
  # follows the nodes draws the edge as well
  wide <- labelled(
    following,
    geom_dag_edges_link(end_cap = ggraph::circle(20, "mm"))
  )
  expect_equal(unique(label_edge_caps(wide)$cap_end), circle_cap)
})

test_that("the automatic labels cut an edge two bent layers draw alike by the nearer cap", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  circle_cap <- 0.375 * 30 + 2
  bow <- tidy_dagitty(dagify(
    y ~ x,
    x ~ ~y,
    coords = list(x = c(x = 0, y = 1), y = c(x = 0, y = 0))
  ))
  arc <- function(cap) {
    geom_dag_edges_arc(
      data = filter_direction("<->"),
      end_cap = ggraph::circle(cap, "mm")
    )
  }
  labelled <- function(...) {
    ggplot(bow, aes_dag()) +
      geom_dag_point(size = 30) +
      geom_dag_edges_link(data = filter_direction("->")) +
      list(...) +
      geom_dag_label_auto(aes(label = name))
  }

  # Two arc layers draw the bidirected edge along one path, beside the
  # straight edge a layer following the nodes draws between the same nodes.
  # The arc is traced once, and the label keeps clear of the ink of both arc
  # layers, which reaches out to the nearer of their caps, whichever layer is
  # added first. The caps of the second arc layer are not those of a straight
  # layer, so the straight edge keeps the node's own cap.
  for (order in list(c(4, 10), c(10, 4))) {
    label <- paste("arc caps of", order[[1]], "then", order[[2]])
    caps <- label_edge_caps(labelled(arc(order[[1]]), arc(order[[2]])))
    stopifnot(nrow(caps) == 2)
    caps <- caps[order(!is.na(caps$layer)), , drop = FALSE]
    expect_equal(is.na(caps$layer), c(TRUE, FALSE), label = label)
    expect_equal(caps$cap_end, c(circle_cap, 4), label = label)
    expect_equal(caps$cap_start, c(circle_cap, circle_cap), label = label)
  }
})

test_that("the automatic labels trace the edges of a layer in every panel it is drawn in", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # ggplot2 draws a layer whose data lack a facet variable in every panel,
  # and a facet variable may share its name with a column of the traced
  # edges. In each panel the labels trace every edge drawn there, cut where
  # the drawn edge stops. ggarrow draws the head of an arc straight from its
  # cut, so its tip leaves the traced arc by the sagitta of that chord, half a
  # millimetre on the short, strongly bent arcs of these narrow panels (see
  # `drawn_tip_point()`), and the caps of the ggarrow edges are checked
  # exactly.
  dag_data <- pull_dag_data(tidy_dagitty(controlled_dag()))
  edges <- dag_data[!is.na(dag_data$to), , drop = FALSE]
  two_panels <- function(column) {
    dplyr::bind_rows(
      dplyr::mutate(dag_data, "{column}" := "A"),
      dplyr::mutate(dag_data, "{column}" := "B")
    )
  }
  panelled <- two_panels("panel")
  in_panel <- function(which) {
    \(x) dplyr::filter(x, panel == which, !is.na(to))
  }
  faceted <- function(data, ..., facet = ggplot2::facet_wrap(~panel)) {
    ggplot(data, aes_dag()) +
      geom_dag_point(size = 16) +
      list(...) +
      facet +
      geom_dag_text_auto(aes(label = name), colour = "black", size = 3) +
      theme_dag()
  }

  typed <- two_panels("type")
  gridded <- dplyr::bind_rows(
    dplyr::mutate(two_panels("row"), column = "c1"),
    dplyr::mutate(two_panels("row"), column = "c2")
  )
  scenes <- list(
    `a ggarrow arc layer without the facet column` = faceted(
      panelled,
      geom_dag_arrow(data = in_panel("A")),
      geom_dag_arrow_arc(data = edges, curvature = 0.5)
    ),
    `a ggraph arc layer without the facet column` = faceted(
      panelled,
      geom_dag_edges_link(data = in_panel("A")),
      geom_dag_edges_arc(data = edges)
    ),
    `a ggraph straight layer without the facet column` = faceted(
      panelled,
      geom_dag_edges_link(data = edges),
      geom_dag_edges_arc(data = in_panel("B"))
    ),
    `a ggarrow straight layer without the facet column` = faceted(
      panelled,
      geom_dag_arrow(data = edges),
      geom_dag_arrow_arc(data = in_panel("B"), curvature = 0.3)
    ),
    `a facet variable named type` = faceted(
      typed,
      geom_dag_arrow_arc(aes(resect_head = ifelse(type == "A", 3, 9))),
      facet = ggplot2::facet_wrap(~type)
    ),
    `a grid facet variable missing from a layer` = faceted(
      gridded,
      geom_dag_edges_link(data = \(x) dplyr::filter(x, !is.na(to))),
      geom_dag_edges_arc(
        data = \(x) {
          dplyr::select(dplyr::filter(x, row == "A", column == "c1"), -column)
        }
      ),
      facet = ggplot2::facet_grid(row ~ column)
    )
  )
  drawn_panels <- c(2, 2, 2, 2, 2, 4)

  for (i in seq_along(scenes)) {
    name <- names(scenes)[[i]]
    panels <- drawn_and_traced_panel_ends(scenes[[i]])
    stopifnot(length(panels) == drawn_panels[[i]])
    for (panel in names(panels)) {
      label <- paste(name, "in panel", panel)
      drawn <- panels[[panel]]$drawn
      traced <- panels[[panel]]$traced %||% data.frame()
      stopifnot(nrow(drawn) > 0)
      expect_equal(
        nrow(traced),
        sum(drawn$end %in% c("fins", "start")),
        label = paste("the edges traced for", label)
      )
      expect_equal(
        label_tip_mismatches(traced, drawn, tolerance = 0.6),
        character(),
        label = paste("the tips traced for", label)
      )
      if (all(drawn$end %in% c("fins", "head"))) {
        expect_equal(
          label_cap_mismatches(traced, drawn),
          character(),
          label = paste("the caps traced for", label)
        )
      }
    }
  }
})

test_that("a routed layer that draws no edges sets no caps for the labels", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  # the routed layer carries every row of the plot and draws none of them,
  # so its 3 mm resection stops no edge, and the labels cut the edges the
  # link layer draws where they stop, beyond the nodes
  p <- ggplot(tidy_dagitty(controlled_dag()), aes_dag()) +
    geom_dag_point(size = 30) +
    geom_dag_edges_link() +
    geom_dag_routed_arrows(data_directed = \(x) x[0, ], resect = 3) +
    geom_dag_label_auto(aes(label = name))

  caps <- label_edge_caps(p)
  stopifnot(nrow(caps) == 4)
  expect_equal(unique(c(caps$cap_start, caps$cap_end)), 0.375 * 30 + 2)
})

test_that("a fixed cap reaches the automatic labels as it reaches the edges", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)
  labelled <- dagify(y ~ x, labels = c(x = "Exposure", y = "Outcome"))

  fixed <- ggdag(labelled, node_size = 30, edge_cap = 5, use_labels = TRUE)
  expect_equal(unique(label_edge_caps(fixed)$cap_start), 5)
  expect_equal(unique(label_edge_caps(fixed)$cap_end), 5)

  own_cap <- ggplot(tidy_dagitty(labelled), aes_dag()) +
    geom_dag_point(size = 30) +
    geom_dag_edges() +
    geom_dag_label_auto(edge_cap = 4)
  expect_equal(unique(label_edge_caps(own_cap)$cap_start), 4)
  expect_equal(unique(label_edge_caps(own_cap)$cap_end), 4)
})

# Visual baselines -------------------------------------------------------------

test_that("the README adjustment set draws its edges up to the nodes", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggdag_adjustment_set(readme_dag(), node_size = 14)
  stopifnot(
    draws_square_ends(p),
    length(node_aware_cap_mismatches(p, 14)) == 0
  )

  expect_doppelganger("readme adjustment set with node-aware edge caps", p)
})

test_that("large nodes keep their arrowheads clear of the nodes", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggdag(cap_dag(), node_size = 30)
  stopifnot(length(node_aware_cap_mismatches(p, 30)) == 0)

  expect_doppelganger("ggdag with node-aware edge caps at node_size 30", p)
})

test_that("ggdag_status() keeps large arrowheads clear of the nodes", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggdag_status(cap_dag(), node_size = 30)
  stopifnot(length(node_aware_cap_mismatches(p, 30)) == 0)

  expect_doppelganger("ggdag_status with node-aware edge caps", p)
})

test_that("a hand-built plot keeps its arrowheads clear of square nodes", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  p <- ggplot(tidy_dagitty(cap_dag()), aes_dag()) +
    geom_dag_point(shape = 15, size = 30) +
    geom_dag_edges() +
    geom_dag_text() +
    theme_dag()
  stopifnot(
    all(edge_end_caps(p)$shape == 15),
    length(node_aware_cap_mismatches(p, 30)) == 0
  )

  expect_doppelganger("hand-built square nodes with node-aware caps", p)
})
