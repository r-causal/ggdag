# A panel has to make room for the ink its edges lay down. A curved edge bows
# away from the chord between its two nodes, and a bow the scales never saw is
# drawn outside the panel and cut off, taking whole edges and the nodes beside
# them off the figure.

# The DAG of a case-control study, whose selection node sits far enough below
# the others that the arcs into it bow past the nodes.
case_control_dag <- function() {
  dagify(
    outcome ~ confounder + exposure,
    selection ~ outcome + confounder,
    exposure ~ confounder,
    exposure = "exposure",
    outcome = "outcome",
    coords = time_ordered_coords()
  )
}

case_control_plot <- function(engine, edge_type = "arc") {
  ggdag(
    case_control_dag(),
    edge_type = edge_type,
    text_size = 2.2,
    edge_engine = engine
  )
}

# Every grob in the panel that lays down edge ink. Nodes and their text are
# drawn at a point and carry their own extent on the page, so they are not
# edge ink; neither is the grill, the panel background, or the border.
edge_ink_points <- function(grob) {
  name <- if (is.null(grob$name)) "" else grob$name
  if (grepl("^(grill|panel\\.)", name)) {
    return(NULL)
  }
  if (inherits(grob, "gTree")) {
    return(do.call(rbind, lapply(grob$children, edge_ink_points)))
  }
  if (inherits(grob, c("points", "text", "rect", "zeroGrob", "null"))) {
    return(NULL)
  }
  if (is.null(grob$x) || is.null(grob$y)) {
    return(NULL)
  }
  data.frame(
    x = grid::convertX(grob$x, "npc", valueOnly = TRUE),
    y = grid::convertY(grob$y, "npc", valueOnly = TRUE)
  )
}

# The extent of the drawn edges, in data units, beside the panel range they
# are drawn into. A curve is bent on the device rather than in the data, so
# the figure is drawn at a fixed size and measured there. ggplot2 draws a
# panel in npc, so a drawn point's data position is the panel range read at
# its npc coordinate.
drawn_edge_extent <- function(plot, width = 7, height = 5) {
  file <- tempfile(fileext = ".png")
  open_test_ragg(file, width, height)
  on.exit(
    {
      grDevices::dev.off()
      unlink(file)
    },
    add = TRUE
  )

  built <- ggplot2::ggplot_build(plot)
  gtable <- ggplot2::ggplot_gtable(built)
  grid::grid.newpage()
  grid::grid.draw(gtable)
  grid::grid.force()

  listing <- grid::grid.ls(print = FALSE, viewports = TRUE, flatten = TRUE)
  panel_rows <- grep("^panel-1", listing$name)
  testthat::expect_gt(length(panel_rows), 0)
  vp_path <- listing$vpPath[[panel_rows[[1]]]]
  vp_names <- strsplit(vp_path, "::", fixed = TRUE)[[1]]
  grid::seekViewport(vp_names[[length(vp_names)]])
  on.exit(grid::upViewport(0), add = TRUE, after = FALSE)

  panel <- grid::grid.get(grid::gPath("panel-1"), grep = TRUE, global = TRUE)
  if (grid::is.grob(panel)) {
    panel <- list(panel)
  }
  ink <- do.call(rbind, lapply(panel[[1]]$children, edge_ink_points))
  testthat::expect_gt(nrow(ink), 0)

  panel_params <- built$layout$panel_params[[1]]
  x_range <- panel_params$x.range
  y_range <- panel_params$y.range
  list(
    x = range(x_range[[1]] + ink$x * diff(x_range)),
    y = range(y_range[[1]] + ink$y * diff(y_range)),
    panel_x = x_range,
    panel_y = y_range
  )
}

# The drawn path is a shaft of some width, so its outline reaches half a
# linewidth past the curve the edge is bent along. One part in a hundred of
# the panel is several times that and a small fraction of the overflow a
# bow the panel never saw produces.
expect_edges_inside_panel <- function(plot, label) {
  extent <- drawn_edge_extent(plot)
  x_tolerance <- 0.01 * diff(extent$panel_x)
  y_tolerance <- 0.01 * diff(extent$panel_y)

  testthat::expect_gte(
    extent$x[[1]],
    extent$panel_x[[1]] - x_tolerance,
    label = paste(label, "left edge extent")
  )
  testthat::expect_lte(
    extent$x[[2]],
    extent$panel_x[[2]] + x_tolerance,
    label = paste(label, "right edge extent")
  )
  testthat::expect_gte(
    extent$y[[1]],
    extent$panel_y[[1]] - y_tolerance,
    label = paste(label, "bottom edge extent")
  )
  testthat::expect_lte(
    extent$y[[2]],
    extent$panel_y[[2]] + y_tolerance,
    label = paste(label, "top edge extent")
  )
}

test_that("the ggarrow engine trains the panel on the arcs it draws", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  expect_edges_inside_panel(case_control_plot("ggarrow"), "ggarrow arc")
})

test_that("the ggraph engine trains the panel on the arcs it draws", {
  skip_if_not_installed("ragg")

  expect_edges_inside_panel(case_control_plot("ggraph"), "ggraph arc")
})

test_that("straight edges leave the panel the nodes alone train", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # the panel a scene of nothing but nodes trains: a straight edge ends at
  # the nodes it joins, so it asks for no room of its own
  nodes_only <- ggplot(tidy_dagitty(case_control_dag()), aes_dag()) +
    geom_dag_point()
  node_params <- ggplot_build(nodes_only)$layout$panel_params[[1]]

  edge_types <- c("link", "link_arc")
  engines <- c("ggraph", "ggarrow")
  for (engine in engines) {
    for (edge_type in edge_types) {
      params <- ggplot_build(
        case_control_plot(engine, edge_type = edge_type)
      )$layout$panel_params[[1]]
      expect_equal(
        params$x.range,
        node_params$x.range,
        label = paste(engine, edge_type, "x.range")
      )
      expect_equal(
        params$y.range,
        node_params$y.range,
        label = paste(engine, edge_type, "y.range")
      )
    }
  }
})

test_that("an edge bowed further than its neighbours stays inside the panel", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  chain_dag <- dagify(
    m ~ x,
    y ~ m,
    s ~ y,
    s ~ x,
    coords = list(
      x = c(x = 0, m = 1, y = 2, s = 3),
      y = c(x = 0, m = 0.25, y = 0, s = -0.25)
    )
  )
  chain_edges <- dplyr::filter(
    pull_dag_data(tidy_dagitty(chain_dag)),
    !is.na(.data$xend)
  )

  # the edge that skips the whole chain bows well past the three that do not
  bow_long_edge <- function(span) ifelse(span > 1.5, 0.7, 0.1)
  add_curvature <- function(edges) {
    edges <- dplyr::filter(edges, !is.na(.data$xend))
    edges$edge_curvature <- bow_long_edge(abs(edges$x - edges$xend))
    edges
  }

  arrow_plot <- chain_dag |>
    ggplot(aes_dag()) +
    geom_dag_arrow_arc(
      aes(edge_curvature = edge_curvature),
      data = add_curvature
    ) +
    geom_dag_point()

  expect_edges_inside_panel(arrow_plot, "per-edge ggarrow curvature")

  arc_plot <- chain_dag |>
    ggplot(aes_dag()) +
    geom_dag_edges_arc(
      curvature = bow_long_edge(abs(chain_edges$x - chain_edges$xend))
    ) +
    geom_dag_point()

  expect_edges_inside_panel(arc_plot, "per-edge ggraph curvature")
})

test_that("a curved case-control scene draws whole", {
  skip_if_not_installed("ggarrow")

  expect_doppelganger(
    "curved edges fit the panel",
    case_control_plot("ggarrow") + theme_dag()
  )
})
