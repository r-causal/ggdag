# Under the ggraph edge engine an edge stops a fixed gap of 2 mm beyond the
# drawn extent of the node at each of its ends, unless the caller fixes the cap
# with `edge_cap` or the `ggdag.edge_cap` option. The extent is measured from
# the node centre to the farthest ink of the glyph R draws for the node's shape,
# so an edge arriving at the corner of a square node is as clear of it as an
# edge arriving anywhere on a circle. Like every other measure a plot draws,
# the cap scales with the plot's `size` multiplier, gap included.

# Fixtures ---------------------------------------------------------------------

# A DAG with a confounder, a node that only has a bidirected edge, and so both
# the link and the arc edge layers.
cap_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    z ~ ~w,
    exposure = "x",
    outcome = "y"
  )
}

# A DAG whose controlled node `z` has an edge running into it and two running
# out of it, and is not a collider, so no collider lines are drawn.
controlled_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    z ~ a,
    exposure = "x",
    outcome = "y"
  )
}

# The DAG of the README figure, whose three adjustment sets put square nodes at
# the start and the end of directed edges and at both ends of a bidirected arc.
readme_dag <- function() {
  dagitty::dagitty(
    "dag {
      y <- x <- z1 <- v -> z2 -> y
      z1 <- w1 <-> w2 -> z2
      x <- w1 -> y
      x <- w2 -> y
      x [exposure]
      y [outcome]
    }"
  ) |>
    tidy_dagitty()
}

# A DAG with one unconditional instrument and one instrument conditional on
# `w`, so the instrumental plot draws `w` as a square in one of its panels.
conditional_iv_dag <- function() {
  dagify(
    y ~ x + u + w,
    x ~ z + iu + u + w,
    z ~ w,
    exposure = "x",
    outcome = "y",
    latent = "u"
  )
}

# Expected geometry ------------------------------------------------------------

# The drawn extent, in millimetres, of a node drawn at ggplot2 size `size` with
# point shape `shape`: the distance from its centre to its farthest ink. R draws
# the circles (16, 19, 21) with radius `0.375 * size` mm. It draws the solid
# square (15) with that radius as its half side, so its corners lie `sqrt(2)`
# radii out, and the filled square (22) with the area of the circle, a half side
# of `sqrt(pi / 4)` radii. Any other shape has no extent here, so a scene that
# draws one fails rather than being checked against a guess.
node_extent_mm <- function(shape, size) {
  radius <- 0.375 * size
  dplyr::case_when(
    shape %in% c(16, 19, 21) ~ radius,
    shape == 15 ~ radius * sqrt(2),
    shape == 22 ~ radius * sqrt(pi / 4) * sqrt(2),
    .default = NA_real_
  )
}

node_shape_name <- function(shape) {
  dplyr::case_when(
    shape %in% c(16, 19, 21) ~ "circle",
    shape %in% c(15, 22) ~ "square",
    .default = paste("shape", shape)
  )
}

# Per-end caps on the built layers ---------------------------------------------

# The node whose centre sits at (`x`, `y`) among `nodes`, a data frame with
# `x` and `y` columns, as a row index.
node_row_at <- function(nodes, x, y) {
  distance <- sqrt((nodes$x - x)^2 + (nodes$y - y)^2)
  match_row <- which(distance < 1e-9)
  if (length(match_row) != 1) {
    return(NA_integer_)
  }
  match_row
}

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

# The edge ends of `plot` whose cap is not 2 mm beyond the drawn extent of the
# node there, each described by panel, edge, end, and node. `node_size` is the
# size the caller asked for and `size` the plot multiplier, so a node layer
# drawn at any other size is reported too.
node_aware_cap_mismatches <- function(plot, node_size, size = 1) {
  ends <- edge_end_caps(plot)
  if (nrow(ends) == 0) {
    return("the plot builds no ggraph edge ends")
  }

  expected <- (node_extent_mm(ends$shape, node_size) + 2) * size
  wrong_size <- is.na(ends$node_size) | ends$node_size != node_size * size
  wrong_cap <- is.na(expected) |
    ends$cap_geometry != "circle" |
    ends$cap_unit != "mm" |
    abs(ends$cap_mm - expected) > 1e-6
  bad <- wrong_size | wrong_cap

  sprintf(
    "panel %s, edge %s: the %s cap at %s (a %s drawn at size %s) is a %s of %.4f %s; expected a circle of %.4f mm",
    ends$panel[bad],
    ends$edge[bad],
    ends$end[bad],
    ends$node[bad],
    node_shape_name(ends$shape[bad]),
    ends$node_size[bad],
    ends$cap_geometry[bad],
    ends$cap_mm[bad],
    ends$cap_unit[bad],
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

test_that("ggdag_adjustment_set() caps a square end by its half diagonal", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  ends <- edge_end_caps(ggdag_adjustment_set(readme_dag(), node_size = 16))

  # `v` is adjusted in the `{v, w1, w2}` panel only, so the edges leaving it
  # start at a square there and at a circle in the other two panels
  v_starts <- ends[ends$node == "v" & ends$end == "start", ]
  expect_setequal(unique(v_starts$shape), c(15, 19))
  expect_equal(
    unique(v_starts$cap_mm[v_starts$shape == 15]),
    0.375 * 16 * sqrt(2) + 2
  )
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

test_that("an edge into a square node is drawn 2 mm beyond its corners", {
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
    expect_gap_mm(
      gaps$end_gap,
      0.375 * node_size * sqrt(2) + 2,
      paste0(
        "the arrowhead tip of x -> y at the square y, node_size = ",
        node_size
      )
    )
  }
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

test_that("the ggarrow engine resects exactly as it did before node-aware caps", {
  withr::local_options(ggdag.edge_cap = NULL, ggdag.node_size = NULL)

  default_arrows <- arrow_layer_state(
    ggdag(cap_dag(), node_size = 30, edge_engine = "ggarrow")
  )
  explicit_arrows <- arrow_layer_state(
    ggdag(cap_dag(), node_size = 30, edge_engine = "ggarrow", edge_cap = 8)
  )

  expect_length(default_arrows, 2)
  expect_identical(default_arrows, explicit_arrows)
  for (arrows in default_arrows) {
    expect_identical(arrows$geom_params$resect, list(head = 8, fins = 8))
  }
  expect_false(any(purrr::map_lgl(
    ggdag(cap_dag(), node_size = 30, edge_engine = "ggarrow")$layers,
    \(layer) inherits(layer$geom, "GeomDAGEdgePath")
  )))
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
