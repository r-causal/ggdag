# Helpers for the tests of the runs orthogonal routes end on: where ggarrow
# draws the ornament at each end of a routed path, and whether that ornament
# lies on the straight run the path enters its node along.
#
# ggarrow cuts a path back from its end by the resection plus the reach of
# the ornament there and draws the ornament straight from that cut towards the
# end. When the path's last straight run is at least that long, the cut lies
# on the run, the ornament is drawn along it, and its tip sits the resection
# from the end on the run. When the run is shorter, the cut falls on the
# corner or on the run before it, and the ornament is drawn along the chord
# from there to the end: askew, off the drawn path, and not the gap outside
# the node's face.

# The DAG of the README, laid out in time order as the README lays it out.
readme_time_ordered_dag <- function() {
  dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1 + w2,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2,
    exposure = "x",
    outcome = "y",
    coords = time_ordered_coords()
  )
}

# The README's adjustment set plot, routed orthogonally under ggarrow, with
# its legend at the bottom as the README draws it, and with `theme_dag()` in
# front of that when `dag_theme` is set.
readme_orthogonal_plot <- function(node_size = 16, dag_theme = FALSE) {
  p <- withr::with_options(
    list(ggdag.edge_route = "orthogonal"),
    ggdag_adjustment_set(
      readme_time_ordered_dag(),
      node_size = node_size,
      edge_engine = "ggarrow"
    )
  )
  if (dag_theme) {
    p <- p + theme_dag()
  }
  p + ggplot2::theme(legend.position = "bottom")
}

# The canonical DAG `name` from `canonical_dag_specs`, laid out with
# `time_ordered_coords()`, with the exposure and outcome given.
canonical_time_ordered_dag <- function(name, exposure = NULL, outcome = NULL) {
  spec <- canonical_dag_specs[[name]]
  dag <- dagitty::dagitty(paste0("dag { ", paste(spec, collapse = "; "), " }"))
  if (!is.null(exposure)) {
    dagitty::exposures(dag) <- exposure
  }
  if (!is.null(outcome)) {
    dagitty::outcomes(dag) <- outcome
  }
  withr::with_seed(1234, tidy_dagitty(dag, layout = time_ordered_coords()))
}

# The exposure and outcome of the canonical DAGs whose adjustment sets are
# drawn, each with a set holding a node that edges run into, so that a head
# is drawn at a square.
canonical_adjustment_roles <- list(
  epidemiology = c("edu", "health"),
  deep_confound = c("c", "d"),
  large_epi = c("smoking", "health")
)

# `ggdag()` of the canonical DAG `name`, routed orthogonally under ggarrow.
canonical_orthogonal_plot <- function(name, node_size) {
  ggdag(
    canonical_time_ordered_dag(name),
    node_size = node_size,
    edge_engine = "ggarrow",
    edge_route = "orthogonal"
  ) +
    theme_dag()
}

# `ggdag_adjustment_set()` of the canonical DAG `name`, routed orthogonally
# under ggarrow.
canonical_adjustment_plot <- function(name, node_size = 16) {
  roles <- canonical_adjustment_roles[[name]]
  withr::with_options(
    list(ggdag.edge_route = "orthogonal"),
    ggdag_adjustment_set(
      canonical_time_ordered_dag(name, roles[[1]], roles[[2]]),
      node_size = node_size,
      edge_engine = "ggarrow"
    )
  ) +
    theme_dag()
}

# The README DAG with `w1`, `w2`, and `z1` drawn as squares and the rest as
# circles, at the default size, its directed edges routed orthogonally with a
# head at their ends and fins at their starts.
fins_orthogonal_plot <- function() {
  tidy_dag <- tidy_dagitty(readme_time_ordered_dag()) |>
    dplyr::mutate(shape = ifelse(name %in% c("w1", "w2", "z1"), 15, 19))
  ggplot2::ggplot(tidy_dag, aes_dag()) +
    geom_dag_point(ggplot2::aes(shape = shape), size = 16) +
    ggplot2::scale_shape_identity() +
    geom_dag_routed_arrows(
      route = "orthogonal",
      arrow_fins = ggarrow::arrow_fins_feather()
    ) +
    theme_dag()
}

# The panel of the README adjustment set plot for the set `{w1, w2, z1}`
# alone, with the panel fixed at the millimetres the middle of the three
# panels is drawn at on a 7 by 5 inch device, so that the router is handed the
# same scene whatever device the plot is drawn on. Every set places the nodes
# at the same positions, so the scales train on the same range as in the
# three-panel plot.
readme_middle_panel_plot <- function() {
  p <- readme_orthogonal_plot()
  p$data <- dplyr::filter(p$data, set == "{w1, w2, z1}")
  p +
    ggplot2::theme(
      panel.widths = grid::unit(52.86, "mm"),
      panel.heights = grid::unit(94.77, "mm")
    )
}

# The scenes whose routed paths are pinned in
# fixtures/orthogonal-final-runs.rds, each drawn on the device it is pinned
# at: scenes in which every orthogonal ornament is drawn on its final run.
# Each is drawn with `theme_dag()`, so that the panel does not depend on the
# axis text of the session's default theme.
orthogonal_run_fixture_scenes <- list(
  readme_16_10x6 = list(
    plot = \() readme_orthogonal_plot(16, dag_theme = TRUE),
    width = 10,
    height = 6
  ),
  readme_14_7x5 = list(
    plot = \() readme_orthogonal_plot(14, dag_theme = TRUE),
    width = 7,
    height = 5
  ),
  large_epi_16 = list(
    plot = \() canonical_orthogonal_plot("large_epi", 16),
    width = 7,
    height = 5
  ),
  epidemiology_30 = list(
    plot = \() canonical_orthogonal_plot("epidemiology", 30),
    width = 7,
    height = 5
  ),
  deep_confound_adjustment = list(
    plot = \() canonical_adjustment_plot("deep_confound"),
    width = 7,
    height = 5
  ),
  fins_7x5 = list(
    plot = fins_orthogonal_plot,
    width = 7,
    height = 5
  )
)

# Reading the routed edges --------------------------------------------------------

# The node glyphs of panel `panel`, as `panel_nodes_mm()` reads them, with the
# name of each node, found in the plot's data by the node's position. Call it
# with the panel's viewport pushed.
panel_named_nodes_mm <- function(plot, built, panel) {
  index <- which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$geom, c("GeomDagPoint", "GeomDagNode"))
  }))
  drawn <- purrr::map(index, \(i) {
    data <- as.data.frame(built$data[[i]])
    data[as.integer(data$PANEL) == panel, c("x", "y"), drop = FALSE]
  }) |>
    purrr::list_rbind()
  known <- plot$data
  if (is.tidy_dagitty(known)) {
    known <- pull_dag_data(known)
  }
  known <- unique(as.data.frame(known)[c("name", "x", "y")])
  names <- vapply(
    seq_len(nrow(drawn)),
    \(j) {
      hit <- abs(known$x - drawn$x[[j]]) < 1e-9 &
        abs(known$y - drawn$y[[j]]) < 1e-9
      hit <- unique(known$name[hit])
      if (length(hit) == 1) hit else NA_character_
    },
    character(1)
  )

  nodes <- panel_nodes_mm(plot, built, panel)
  nodes$name <- names
  nodes
}

# Every routed ggarrow edge `plot` draws on a device `width` by `height`
# inches, read from the `arrow_path` grobs the routed layer makes when it is
# drawn: one element per grob, holding its panel, the facet's label, the
# nodes of that panel, and the paths and resections `arrow_grob_paths()`
# reads.
routed_arrow_drawings <- function(plot, width = 7, height = 5) {
  with_forced_plot(
    plot,
    function(built) {
      found <- grid::grid.grep(
        "arrow_path",
        grep = TRUE,
        global = TRUE,
        viewports = TRUE
      )
      layout <- built$layout$layout
      facet <- setdiff(
        names(layout),
        c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD")
      )
      drawings <- purrr::map(found, \(path) {
        name <- as.character(path)
        own <- sub(".*::", "", name)
        grob <- grid::grid.get(path)
        panel <- regmatches(name, regexpr("panel-[0-9]+\\.", name))
        if (
          !grepl("arrow_path", own) ||
            !inherits(grob, "arrow_path") ||
            !grepl("dag_routed_edges", name) ||
            length(panel) == 0
        ) {
          return(NULL)
        }
        panel <- as.integer(sub("panel-([0-9]+)\\.", "\\1", panel))
        grid::upViewport(0)
        grid::downViewport(attr(path, "vpPath"))
        on.exit(grid::upViewport(0), add = TRUE)

        drawn <- arrow_grob_paths(grob)
        drawn$panel <- panel
        drawn$facet <- if (length(facet) > 0) {
          paste(
            as.character(unlist(layout[layout$PANEL == panel, facet])),
            collapse = ", "
          )
        } else {
          NA_character_
        }
        drawn$nodes <- panel_named_nodes_mm(plot, built, panel)
        drawn
      })
      purrr::compact(drawings)
    },
    width = width,
    height = height
  )
}

# The straight run a path of points `x`, `y` (in millimetres) ends on: the
# direction it leaves its last point in, pointing back along the path, and
# the length of the run, which is how far back the points stay on the line
# through the last point in that direction without turning back.
final_run <- function(x, y, tol = 1e-6) {
  n <- length(x)
  u <- c(x[[n - 1]] - x[[n]], y[[n - 1]] - y[[n]])
  u <- u / sqrt(sum(u^2))
  along <- (x - x[[n]]) * u[[1]] + (y - y[[n]]) * u[[2]]
  across <- abs((x - x[[n]]) * u[[2]] - (y - y[[n]]) * u[[1]])
  length <- 0
  for (k in rev(seq_len(n - 1))) {
    if (across[[k]] > tol || along[[k]] < length - tol) {
      break
    }
    length <- along[[k]]
  }
  list(u = u, length = length)
}

# One row per end of every routed ggarrow edge `plot` draws that carries an
# ornament, on a device `width` by `height` inches: the panel and its facet
# label, the edge by the names of its nodes, which end, and the node there;
# the length of the straight run the path enters the node along (`run`) and
# the resection plus the ornament's reach ggarrow cuts the path back by
# (`cut`); the angle, in degrees, between the direction ggarrow draws the
# ornament in and that run (`angle`); and where the tip is drawn, both along
# the run from the path's end (`along`) and across it (`across`), against
# how far along the run the node's face lies plus `gap` (`expected`). A
# square's face is its half side along the run from any port on it, and a
# circle's is `sqrt(r^2 - o^2)` from a port offset `o` from its centre line.
# `tip_dx` and `tip_dy` place the tip about the node's centre.
orthogonal_run_ends <- function(plot, width = 7, height = 5, gap = 2) {
  drawings <- routed_arrow_drawings(plot, width = width, height = height)

  purrr::map(drawings, \(drawn) {
    nodes <- drawn$nodes
    purrr::map(seq_along(drawn$paths), \(k) {
      path <- drawn$paths[[k]]
      n <- length(path$x)
      from <- node_at_end(nodes, path$x[[1]], path$y[[1]])
      to <- node_at_end(nodes, path$x[[n]], path$y[[n]])
      edge <- paste(nodes$name[from], "->", nodes$name[to])

      end_row <- function(end) {
        if (end == "fins") {
          x <- rev(path$x)
          y <- rev(path$y)
          node <- from
          resect <- drawn$fins[[k]]
          reach <- drawn$fins_reach[[k]]
        } else {
          x <- path$x
          y <- path$y
          node <- to
          resect <- drawn$head[[k]]
          reach <- drawn$head_reach[[k]]
        }
        if (reach <= 0) {
          return(NULL)
        }
        run <- final_run(x, y)
        u <- run$u
        end_x <- x[[n]]
        end_y <- y[[n]]

        cut <- resect_cut_point(x, y, resect + reach)
        tip <- drawn_tip_point(x, y, resect, reach)
        heading <- c(end_x, end_y) - cut
        heading <- heading / sqrt(sum(heading^2))
        cosine <- -(heading[[1]] * u[[1]] + heading[[2]] * u[[2]])
        angle <- acos(min(max(cosine, -1), 1)) * 180 / pi

        offset <- abs(
          (end_x - nodes$x[node]) * u[[2]] - (end_y - nodes$y[node]) * u[[1]]
        )
        outline <- expected_outline_mm(nodes$shape[node], nodes$size[node])
        face <- ifelse(
          is_square_shape(nodes$shape[node]),
          outline,
          sqrt(pmax(outline^2 - offset^2, 0))
        )

        data.frame(
          panel = drawn$panel,
          facet = drawn$facet,
          edge = edge,
          end = end,
          node = nodes$name[node],
          shape = node_shape_name(nodes$shape[node]),
          size = nodes$size[node],
          run = run$length,
          cut = resect + reach,
          angle = angle,
          along = (tip[[1]] - end_x) * u[[1]] + (tip[[2]] - end_y) * u[[2]],
          across = abs(
            (tip[[1]] - end_x) * u[[2]] - (tip[[2]] - end_y) * u[[1]]
          ),
          expected = face + gap,
          tip_dx = tip[[1]] - nodes$x[node],
          tip_dy = tip[[2]] - nodes$y[node]
        )
      }
      rbind(end_row("fins"), end_row("head"))
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
}

# Whether each end among `ends`, from `orthogonal_run_ends()`, is drawn on its
# run: the run is at least as long as the cut, so the cut lies on it; the
# ornament is drawn within `angle_tolerance` degrees of the run; and the tip
# lies on the run, `expected` mm along it from the path's end, within
# `tolerance` mm.
on_final_run <- function(ends, tolerance = 0.05, angle_tolerance = 1) {
  !is.na(ends$node) &
    !is.na(ends$angle) &
    ends$run >= ends$cut - 1e-9 &
    ends$angle <= angle_tolerance &
    abs(ends$along - ends$expected) <= tolerance &
    ends$across <= tolerance
}

# The ends among `ends` that are not drawn on their runs, described by panel,
# edge, end, and node, with what was drawn.
orthogonal_run_mismatches <- function(
  ends,
  tolerance = 0.05,
  angle_tolerance = 1
) {
  if (nrow(ends) == 0) {
    return("the plot draws no routed ornaments")
  }
  bad <- !on_final_run(ends, tolerance, angle_tolerance)
  ends <- ends[bad, , drop = FALSE]

  sprintf(
    "panel %s (%s), the %s end of %s at %s %s drawn at size %s: the run is %.3f mm against a cut of %.3f mm, the ornament is drawn %.2f degrees off the run, and the tip is %.3f mm along it and %.3f mm across it, expected %.3f mm along, at (%.2f, %.2f) mm from the centre",
    ends$panel,
    ends$facet,
    ends$end,
    ends$edge,
    ends$shape,
    ends$node,
    ends$size,
    ends$run,
    ends$cut,
    ends$angle,
    ends$along,
    ends$across,
    ends$expected,
    ends$tip_dx,
    ends$tip_dy
  )
}

# The routed ggarrow edges `plot` draws on a device `width` by `height`
# inches: one row per edge with its panel, the ends of its path, and the
# resection of each end, and the path itself, in millimetres, sorted by where
# the edge runs so that the order the layers draw their edges in does not
# matter.
routed_path_record <- function(plot, width, height) {
  drawings <- routed_arrow_drawings(plot, width = width, height = height)
  edges <- purrr::map(drawings, \(drawn) {
    purrr::map(seq_along(drawn$paths), \(k) {
      path <- drawn$paths[[k]]
      n <- length(path$x)
      list(
        row = data.frame(
          panel = drawn$panel,
          from_x = path$x[[1]],
          from_y = path$y[[1]],
          to_x = path$x[[n]],
          to_y = path$y[[n]],
          fins = drawn$fins[[k]],
          head = drawn$head[[k]]
        ),
        path = data.frame(x = path$x, y = path$y)
      )
    })
  }) |>
    purrr::list_flatten()
  rows <- purrr::list_rbind(purrr::map(edges, "row"))
  order <- order(
    rows$panel,
    round(rows$from_x, 6),
    round(rows$from_y, 6),
    round(rows$to_x, 6),
    round(rows$to_y, 6)
  )
  rows <- rows[order, , drop = FALSE]
  rownames(rows) <- NULL
  list(edges = rows, paths = purrr::map(edges, "path")[order])
}

# The record of the routed edges each fixture scene draws.
orthogonal_run_fixture_drawings <- function() {
  lapply(orthogonal_run_fixture_scenes, \(scene) {
    routed_path_record(scene$plot(), scene$width, scene$height)
  })
}
