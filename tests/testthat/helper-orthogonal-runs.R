# Helpers for the tests of what the orthogonal router draws, read from the
# ggarrow grobs of a plot drawn on a device of a fixed size, in millimetres.
# Three invariants hold over every drawn orthogonal path:
#
# 1. Every ornament is drawn on the run its path ends on. ggarrow cuts a path
#    back from its end by the resection plus the reach of the ornament there
#    and draws the ornament straight from that cut towards the end. When the
#    path's last straight run is at least that long, the cut lies on the run,
#    the ornament is drawn along it, and its tip sits the resection from the
#    end on the run. When the run is shorter, the cut falls on the corner or
#    on the run before it, and the ornament is drawn along the chord from
#    there to the end: askew, off the drawn path, and not the gap outside the
#    node's face.
# 2. No path passes within the outline of a node other than the two it runs
#    between (`orthogonal_pass_throughs()`).
# 3. No two paths share a straight stretch, other than the trunk out of one
#    port or the run into one port that the router merges on purpose for the
#    paths of one layer (`orthogonal_coincidences()`).

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

# The README DAG at node size 16 with its directed edges drawn by two routed
# layers, the edges out of `w1` and `w2` by the first and the rest by the
# second, each called with the arguments in `first` and `second`, and with
# automatic labels when `labels` is set. A layer whose arguments are `NULL`
# is left out, so the plot draws the other layer's edges alone.
readme_layer_pair_plot <- function(first, second, labels = TRUE) {
  directed <- filter_direction("->")
  out_of_w <- function(x) dplyr::filter(directed(x), name %in% c("w1", "w2"))
  rest <- function(x) dplyr::filter(directed(x), !name %in% c("w1", "w2"))
  p <- ggplot2::ggplot(tidy_dagitty(readme_time_ordered_dag()), aes_dag()) +
    geom_dag_point(size = 16) +
    theme_dag()
  if (!is.null(first)) {
    p <- p +
      do.call(geom_dag_routed_arrows, c(list(data_directed = out_of_w), first))
  }
  if (!is.null(second)) {
    p <- p +
      do.call(geom_dag_routed_arrows, c(list(data_directed = rest), second))
  }
  if (labels) {
    p <- p + geom_dag_text_auto(colour = "black", size = 3)
  }
  p
}

# Three sources in one layer and their common target `t` just beyond them,
# which leads on to `e`, in two facet panels: in the panel `"both"` the
# arrivals into `t` are drawn by one routed layer and the edge out of `t` by
# another, and in the panel `"one layer"` the edge out of `t` is not there,
# so only the first layer draws edges. The gap between the sources and `t`
# is too narrow for a stub. With `second = FALSE` the plot draws the first
# layer alone.
narrow_facet_plot <- function(second = TRUE, labels = FALSE) {
  dag <- dagitty::dagitty("dag { a -> t; b -> t; c -> t; t -> e }")
  dagitty::coordinates(dag) <- list(
    x = c(a = 0, b = 0, c = 0, t = 0.12, e = 1),
    y = c(a = 0, b = 1, c = 2, t = 1.5, e = 1.5)
  )
  data <- pull_dag_data(tidy_dagitty(dag))
  both <- dplyr::mutate(data, panel = "both")
  one <- dplyr::mutate(data, panel = "one layer")
  gone <- one$name == "t"
  one$to[gone] <- NA
  one$xend[gone] <- NA
  one$yend[gone] <- NA
  one$direction[gone] <- NA

  directed <- filter_direction("->")
  into_t <- function(x) dplyr::filter(directed(x), to == "t")
  out_of_t <- function(x) dplyr::filter(directed(x), name == "t")
  p <- ggplot2::ggplot(dplyr::bind_rows(both, one), aes_dag()) +
    geom_dag_point(size = 16) +
    geom_dag_routed_arrows(route = "orthogonal", data_directed = into_t) +
    ggplot2::facet_wrap(~panel) +
    theme_dag()
  if (second) {
    p <- p +
      geom_dag_routed_arrows(route = "orthogonal", data_directed = out_of_t)
  }
  if (labels) {
    p <- p + geom_dag_text_auto(colour = "black", size = 3)
  }
  p
}

# The narrow gap of `narrow_facet_plot()` in one panel, with the arrivals
# into `t` drawn by one routed layer and the edge out of `t` by another,
# which maps its curvature to 0.2 and is called with the further arguments
# in `second`, and with automatic labels when `labels` is set. The second
# layer draws that edge as an arc, which is never rerouted, so none of the
# edges it draws is routed.
narrow_curved_plot <- function(second = list(), labels = TRUE) {
  dag <- dagitty::dagitty("dag { a -> t; b -> t; c -> t; t -> e }")
  dagitty::coordinates(dag) <- list(
    x = c(a = 0, b = 0, c = 0, t = 0.12, e = 1),
    y = c(a = 0, b = 1, c = 2, t = 1.5, e = 1.5)
  )
  directed <- filter_direction("->")
  into_t <- function(x) dplyr::filter(directed(x), to == "t")
  out_of_t <- function(x) dplyr::filter(directed(x), name == "t")
  second$mapping <- ggplot2::aes(
    edge_curvature = ifelse(is.na(to), NA, 0.2),
    !!!second$mapping
  )
  p <- ggplot2::ggplot(tidy_dagitty(dag), aes_dag()) +
    geom_dag_point(size = 16) +
    geom_dag_routed_arrows(route = "orthogonal", data_directed = into_t) +
    do.call(
      geom_dag_routed_arrows,
      c(list(route = "orthogonal", data_directed = out_of_t), second)
    ) +
    theme_dag()
  if (labels) {
    p <- p + geom_dag_text_auto(colour = "black", size = 3)
  }
  p
}

# A spline scene whose two edges are routed differently together than apart:
# `x -> y` detours around `m`, which sits on its chord, and `z -> y` arrives
# at `y` beside it. Each edge is drawn by a routed layer of its own.
spline_layer_pair_plot <- function() {
  dag <- dagitty::dagitty("dag { x -> y; z -> y; m; w }")
  dagitty::coordinates(dag) <- list(
    x = c(x = 0, m = 1, y = 2, z = 0, w = 1),
    y = c(x = 0, m = 0, y = 0, z = 0.3, w = 1)
  )
  directed <- filter_direction("->")
  from_x <- function(x) dplyr::filter(directed(x), name == "x")
  from_z <- function(x) dplyr::filter(directed(x), name == "z")
  ggplot2::ggplot(tidy_dagitty(dag), aes_dag()) +
    geom_dag_point(size = 16) +
    geom_dag_routed_arrows(data_directed = from_x) +
    geom_dag_routed_arrows(data_directed = from_z) +
    geom_dag_text_auto(colour = "black", size = 3) +
    theme_dag()
}

# The scenes whose routed paths are pinned in
# fixtures/orthogonal-final-runs.rds, each drawn on the device it is pinned
# at: scenes whose paths share no stretch, and which keep the other two
# invariants but where their gaps are too narrow for them. Each is drawn
# with `theme_dag()`, so that the panel does not depend on the axis text of
# the session's default theme.
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

# Whether the panels of each fixture scene, and with them every millimetre
# the router works in, are sized by text drawn outside them. The scenes drawn
# with `ggdag()` or by hand under `theme_dag()` draw no axis text, strips,
# legend, or title, so their panel is the device less the plot margin
# whatever the default font. The adjustment sets facet by set, with a strip
# label above each panel and a legend of the adjusted and unadjusted nodes.
# The strips take their height from the panels' height, and the legend takes
# the width of its labels from the panels' width where it sits beside them,
# as in deep_confound_adjustment, and its height where it sits below them, as
# in the README scenes.
orthogonal_run_fixture_font_sized <- c(
  readme_16_10x6 = TRUE,
  readme_14_7x5 = TRUE,
  large_epi_16 = FALSE,
  epidemiology_30 = FALSE,
  deep_confound_adjustment = TRUE,
  fins_7x5 = FALSE
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

# One row per end of every routed ggarrow edge among `drawings`, from
# `routed_arrow_drawings()`, that carries an ornament: the panel and its
# facet label, the edge by the names of its nodes, which end, and the node
# there;
# the length of the straight run the path enters the node along (`run`) and
# the resection plus the ornament's reach ggarrow cuts the path back by
# (`cut`); the angle, in degrees, between the direction ggarrow draws the
# ornament in and that run (`angle`); and where the tip is drawn, both along
# the run from the path's end (`along`) and across it (`across`), against
# how far along the run the node's face lies plus `gap` (`expected`). A
# square's face is its half side along the run from any port on it, and a
# circle's is `sqrt(r^2 - o^2)` from a port offset `o` from its centre line.
# `tip_dx` and `tip_dy` place the tip about the node's centre.
orthogonal_run_ends <- function(drawings, gap = 2) {
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

# What each routed layer of `plot` hands the router when the plot is drawn
# on a device `width` by `height` inches, one element per call the drawn
# grobs make, in the order they are drawn: the `nodes`, `edges`, `bounds`,
# `cap`, `mode`, and constants (`opts`) of the call. Each grob routes twice,
# once when the plot is drawn and once when it is forced, and the routes the
# automatic label engine traces are left out.
routed_router_inputs <- function(plot, width = 7, height = 5) {
  router <- get("route_edges_mm", envir = asNamespace("ggdag"))
  calls <- list()
  record <- function(nodes, edges, bounds, cap, mode, opts) {
    caller <- paste(deparse(sys.call(-1)[[1]]), collapse = "")
    if (!grepl("route_label_obstacles", caller)) {
      calls[[length(calls) + 1L]] <<- list(
        nodes = nodes,
        edges = edges,
        bounds = bounds,
        cap = cap,
        mode = mode,
        opts = opts
      )
    }
    router(nodes, edges, bounds, cap, mode, opts)
  }
  testthat::with_mocked_bindings(
    with_forced_plot(plot, \(built) NULL, width = width, height = height),
    route_edges_mm = record,
    .package = "ggdag"
  )
  calls
}

# What the routed layers of `plot` and its automatic label layer hand the
# router when the plot is drawn on a device `width` by `height` inches: the
# calls the drawn grobs make, as `routed_router_inputs()` reads them, in
# `drawn`, and the calls the label engine makes to trace the same routes, in
# `traced`, each with the panel viewport it was made in (`panel`).
drawn_and_traced_router_inputs <- function(plot, width = 7, height = 5) {
  router <- get("route_edges_mm", envir = asNamespace("ggdag"))
  calls <- list(drawn = list(), traced = list())
  record <- function(nodes, edges, bounds, cap, mode, opts) {
    caller <- paste(deparse(sys.call(-1)[[1]]), collapse = "")
    grob <- if (grepl("route_label_obstacles", caller)) "traced" else "drawn"
    calls[[grob]][[length(calls[[grob]]) + 1L]] <<- list(
      panel = viewport_panel(unclass(grid::current.vpPath())$path),
      nodes = nodes,
      edges = edges,
      bounds = bounds,
      cap = cap,
      mode = mode,
      opts = opts
    )
    router(nodes, edges, bounds, cap, mode, opts)
  }
  testthat::with_mocked_bindings(
    with_forced_plot(plot, \(built) NULL, width = width, height = height),
    route_edges_mm = record,
    .package = "ggdag"
  )
  calls
}

# The calls among `inputs$traced`, from `drawn_and_traced_router_inputs()`,
# that no drawn call among `inputs$drawn` in the same panel makes alike: the
# same nodes, cap, mode, and constants, and the same edges, whatever order
# each grob lists them in, their positions and sampled arcs within
# `tolerance` mm. Each is described by the edges it routes.
traced_router_input_mismatches <- function(inputs, tolerance = 1e-10) {
  if (length(inputs$traced) == 0) {
    return("the label engine routes nothing")
  }
  # a grob that shows the router no arc lists no fixed paths, or lists one
  # of nothing for every edge, which the router reads alike
  in_order <- function(edges) {
    edges <- as.data.frame(edges)
    fixed <- edges[["fixed_path"]]
    if (!is.null(fixed) && all(vapply(fixed, is.null, logical(1)))) {
      edges$fixed_path <- NULL
    }
    edges <- edges[order(edges$from, edges$to), , drop = FALSE]
    rownames(edges) <- NULL
    edges
  }
  alike <- function(traced, drawn) {
    identical(traced$panel, drawn$panel) &&
      identical(traced$nodes, drawn$nodes) &&
      identical(traced$cap, drawn$cap) &&
      identical(traced$mode, drawn$mode) &&
      identical(traced$opts, drawn$opts) &&
      isTRUE(all.equal(traced$bounds, drawn$bounds, tolerance = tolerance)) &&
      isTRUE(all.equal(
        in_order(traced$edges),
        in_order(drawn$edges),
        tolerance = tolerance
      ))
  }
  unmatched <- purrr::keep(inputs$traced, \(traced) {
    !any(purrr::map_lgl(inputs$drawn, \(drawn) alike(traced, drawn)))
  })
  purrr::map_chr(unmatched, \(traced) {
    sprintf(
      "the label engine routes %s at a cap of %.3f mm with %s narrow rows and a head reach of %.3f mm, as no drawn grob does",
      paste(traced$edges$from, "->", traced$edges$to, collapse = ", "),
      traced$cap,
      if (isTRUE(traced$opts$narrow_rows)) "" else "no",
      traced$opts$head_reach %||% NA_real_
    )
  })
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

# The drawn paths -------------------------------------------------------------------

# Every routed ggarrow path among `drawings`, from
# `routed_arrow_drawings()`, one element per path: the panel and its facet
# label, the index of the grob (the layer) that draws it, the edge by the
# names of its nodes, the row indices of those nodes among the panel's
# `nodes`, and the path itself in millimetres. Paths from every routed layer
# of a panel are read, so two layers that draw one panel are checked against
# each other.
routed_panel_paths <- function(drawings) {
  purrr::imap(drawings, \(drawn, g) {
    nodes <- drawn$nodes
    purrr::map(drawn$paths, \(path) {
      n <- length(path$x)
      from <- node_at_end(nodes, path$x[[1]], path$y[[1]])
      to <- node_at_end(nodes, path$x[[n]], path$y[[n]])
      list(
        panel = drawn$panel,
        facet = drawn$facet,
        grob = g,
        edge = paste(nodes$name[from], "->", nodes$name[to]),
        from = from,
        to = to,
        x = path$x,
        y = path$y,
        nodes = nodes
      )
    })
  }) |>
    purrr::list_flatten()
}

# The traced routes ---------------------------------------------------------------

# The panel viewport a viewport path runs through, as ggplot2 names it:
# `"panel.7-5-7-5"` for a plot of one panel, `"panel-1-1.10-7-10-7"` for a
# facet's panel.
viewport_panel <- function(path) {
  regmatches(path, regexpr("panel[^:]*", path))
}

# The largest distance, in millimetres, from a point of either polyline to the
# nearest point of the other, `x` and `y` of each: zero when the two paths are
# the same points, however they are ordered along the path.
polyline_distance_mm <- function(a, b) {
  one_way <- function(p, q) {
    m <- length(q$x)
    if (m < 2) {
      return(max(sqrt((p$x - q$x[[1]])^2 + (p$y - q$y[[1]])^2)))
    }
    max(vapply(
      seq_along(p$x),
      \(i) {
        min(point_segment_distance(
          p$x[[i]],
          p$y[[i]],
          q$x[-m],
          q$y[-m],
          q$x[-1],
          q$y[-1]
        ))
      },
      numeric(1)
    ))
  }
  max(one_way(a, b), one_way(b, a))
}

# How far the routes the automatic label layer of `plot` traces lie from the
# routed paths the plot draws, on a device `width` by `height` inches, read
# from one drawing: one row per routed path drawn, with its panel, the ends of
# the drawn path, the number of points of the drawn and of the traced path,
# and `deviation`, the largest distance in millimetres between the two. A
# traced route, or the traced path of an edge the layer pins straight, is
# matched to the drawn path of its panel whose ends lie nearest its own, so a
# route traced to other ports than the drawn ones is still found, and
# measured. A drawn path no traced route is matched to has no deviation
# (`NA`).
label_route_deviations <- function(plot, width = 7, height = 5) {
  real_ink <- get("label_ink", envir = asNamespace("ggdag"))
  traced <- list()
  record <- function(edges, cap, ...) {
    vp <- unclass(grid::current.vpPath())
    traced[[viewport_panel(vp$path)]] <<- edges
    real_ink(edges, cap, ...)
  }
  drawings <- testthat::with_mocked_bindings(
    with_forced_plot(
      plot,
      function(built) {
        found <- grid::grid.grep(
          "arrow_path",
          grep = TRUE,
          global = TRUE,
          viewports = TRUE
        )
        purrr::compact(purrr::map(found, \(path) {
          name <- as.character(path)
          grob <- grid::grid.get(path)
          if (
            !inherits(grob, "arrow_path") ||
              !grepl("dag_routed_edges", name)
          ) {
            return(NULL)
          }
          grid::upViewport(0)
          grid::downViewport(attr(path, "vpPath"))
          on.exit(grid::upViewport(0), add = TRUE)
          drawn <- arrow_grob_paths(grob)
          drawn$panel <- viewport_panel(attr(path, "vpPath"))
          drawn
        }))
      },
      width = width,
      height = height
    ),
    label_ink = record,
    .package = "ggdag"
  )
  if (length(traced) == 0) {
    stop("the plot traces no edges for its labels")
  }

  purrr::map(drawings, \(drawn) {
    edges <- traced[[drawn$panel]]
    routed <- if (is.null(edges)) {
      character()
    } else {
      unique(edges$edge_id[grepl("\r(routed|fixed)\r", edges$edge_id)])
    }
    routes <- purrr::map(routed, \(id) {
      rows <- edges$edge_id == id
      list(x = edges$x[rows], y = edges$y[rows])
    })
    purrr::map(drawn$paths, \(path) {
      n <- length(path$x)
      row <- data.frame(
        panel = drawn$panel,
        from_x = path$x[[1]],
        from_y = path$y[[1]],
        to_x = path$x[[n]],
        to_y = path$y[[n]],
        drawn_points = n,
        traced_points = NA_integer_,
        deviation = NA_real_
      )
      if (length(routes) == 0) {
        return(row)
      }
      apart <- purrr::map_dbl(routes, \(route) {
        m <- length(route$x)
        sqrt((route$x[[1]] - path$x[[1]])^2 + (route$y[[1]] - path$y[[1]])^2) +
          sqrt((route$x[[m]] - path$x[[n]])^2 + (route$y[[m]] - path$y[[n]])^2)
      })
      route <- routes[[which.min(apart)]]
      row$traced_points <- length(route$x)
      row$deviation <- polyline_distance_mm(path, route)
      row
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
}

# The straight runs of a sampled path: maximal stretches of consecutive
# points that lie on one line in one direction, each with its ends, its
# length, and the arc length along the path at which it starts and ends.
# The samples of a rounded corner turn at every point, so a corner yields
# runs no longer than the sample spacing, and a straight run between two
# corners yields one run.
path_runs <- function(x, y, tol = 1e-6) {
  n <- length(x)
  seg <- sqrt(diff(x)^2 + diff(y)^2)
  arc <- cumsum(c(0, seg))
  start <- 1L
  runs <- list()
  for (i in seq_len(n - 1L)) {
    ax <- x[[i + 1L]] - x[[start]]
    ay <- y[[i + 1L]] - y[[start]]
    bx <- x[[i + 1L]] - x[[i]]
    by <- y[[i + 1L]] - y[[i]]
    straight <- i == start ||
      (abs(ax * by - ay * bx) <= tol * sqrt(ax^2 + ay^2) &&
        ax * bx + ay * by > 0)
    if (!straight) {
      runs[[length(runs) + 1L]] <- c(start, i)
      start <- i
    }
  }
  runs[[length(runs) + 1L]] <- c(start, n)
  ends <- do.call(rbind, runs)
  data.frame(
    x0 = x[ends[, 1]],
    y0 = y[ends[, 1]],
    x1 = x[ends[, 2]],
    y1 = y[ends[, 2]],
    from = arc[ends[, 1]],
    to = arc[ends[, 2]],
    length = arc[ends[, 2]] - arc[ends[, 1]]
  )
}

# The distance from the point (`px`, `py`) to each segment (`x0`, `y0`) to
# (`x1`, `y1`).
point_segment_distance <- function(px, py, x0, y0, x1, y1) {
  dx <- x1 - x0
  dy <- y1 - y0
  len2 <- dx^2 + dy^2
  t <- ifelse(len2 > 0, ((px - x0) * dx + (py - y0) * dy) / len2, 0)
  t <- pmin(pmax(t, 0), 1)
  sqrt((x0 + t * dx - px)^2 + (y0 + t * dy - py)^2)
}

# How far inside the square of half side `h` about (`cx`, `cy`) each segment
# (`x0`, `y0`) to (`x1`, `y1`) reaches: the depth, in the square's own
# distance, of the segment's point nearest the centre, positive inside the
# square and zero or negative outside it. The depth is the largest `d` for
# which the segment meets the square of half side `h - d`, found by clipping
# the segment against that square; it is searched to a hundredth of a
# millimetre.
segment_square_depth <- function(x0, y0, x1, y1, cx, cy, h) {
  # whether the segments `i` meet the square of half side `h - d`, by Liang
  # and Barsky's clip; `d` is one depth per segment in `i`
  hits <- function(i, d) {
    hh <- h - d
    dx <- x1[i] - x0[i]
    dy <- y1[i] - y0[i]
    p <- rbind(-dx, dx, -dy, dy)
    q <- rbind(
      x0[i] - (cx - hh),
      (cx + hh) - x0[i],
      y0[i] - (cy - hh),
      (cy + hh) - y0[i]
    )
    t0 <- rep(0, length(i))
    t1 <- rep(1, length(i))
    ok <- rep(TRUE, length(i))
    for (k in 1:4) {
      pk <- p[k, ]
      qk <- q[k, ]
      r <- ifelse(pk == 0, 0, qk / pk)
      ok <- ok & !(pk == 0 & qk < 0)
      t0 <- ifelse(pk < 0, pmax(t0, r), t0)
      t1 <- ifelse(pk > 0, pmin(t1, r), t1)
    }
    ok & t0 <= t1
  }
  n <- length(x0)
  depth <- rep(-Inf, n)
  inside <- which(hits(seq_len(n), rep(0, n)))
  if (length(inside) == 0) {
    return(depth)
  }
  # bisection on the depth of the segments that are inside at all
  lo <- rep(0, length(inside))
  hi <- rep(h, length(inside))
  for (step in seq_len(20)) {
    mid <- (lo + hi) / 2
    in_mid <- hits(inside, mid)
    lo <- ifelse(in_mid, mid, lo)
    hi <- ifelse(in_mid, hi, mid)
  }
  depth[inside] <- lo
  depth
}

# The paths among those `routed_panel_paths()` reads that pass within the
# outline of a node other than the two the path runs between, described by
# panel, edge, and node, with how far inside the outline the path reaches. A
# circle's outline is its radius and a square's its half side; a path counts
# as passing within an outline when it reaches more than `tolerance` mm
# inside it, so that a path drawn along a face is not a pass-through. The
# path's own two nodes are passed over: a path starts and ends inside them.
orthogonal_pass_throughs <- function(paths, tolerance = 0.05) {
  purrr::map(paths, \(path) {
    nodes <- path$nodes
    others <- setdiff(seq_len(nrow(nodes)), c(path$from, path$to))
    n <- length(path$x)
    x0 <- path$x[-n]
    y0 <- path$y[-n]
    x1 <- path$x[-1]
    y1 <- path$y[-1]
    purrr::map(others, \(k) {
      outline <- expected_outline_mm(nodes$shape[[k]], nodes$size[[k]])
      depth <- if (is_square_shape(nodes$shape[[k]])) {
        max(segment_square_depth(
          x0,
          y0,
          x1,
          y1,
          nodes$x[[k]],
          nodes$y[[k]],
          outline
        ))
      } else {
        outline -
          min(point_segment_distance(
            nodes$x[[k]],
            nodes$y[[k]],
            x0,
            y0,
            x1,
            y1
          ))
      }
      if (!is.finite(depth) || depth <= tolerance) {
        return(NULL)
      }
      sprintf(
        "panel %s (%s), %s passes %.2f mm inside the outline of the %s %s drawn at size %s",
        path$panel,
        path$facet,
        path$edge,
        depth,
        node_shape_name(nodes$shape[[k]]),
        nodes$name[[k]],
        nodes$size[[k]]
      )
    }) |>
      purrr::compact() |>
      unlist()
  }) |>
    unlist() %||%
    character()
}

# The arc length from the common start of paths `a` and `b` (or, with
# `from_end`, from their common end) over which the two coincide: the
# stretch of one path's points, contiguous from that end, that lie within
# `tolerance` mm of the other. It is measured on the points of each path in
# turn and the longer stretch is taken, since the path whose vertex ends the
# shared stretch measures it exactly while the other's samples fall short of
# the vertex by up to the sample spacing. Zero when the two paths do not
# start (or end) at one point.
shared_stretch <- function(a, b, tolerance, from_end = FALSE) {
  points_of <- function(p) {
    if (from_end) list(x = rev(p$x), y = rev(p$y)) else list(x = p$x, y = p$y)
  }
  a <- points_of(a)
  b <- points_of(b)
  if (sqrt((a$x[[1]] - b$x[[1]])^2 + (a$y[[1]] - b$y[[1]])^2) > 1e-6) {
    return(0)
  }
  stretch_on <- function(p, q) {
    nq <- length(q$x)
    on_q <- vapply(
      seq_along(p$x),
      \(i) {
        min(point_segment_distance(
          p$x[[i]],
          p$y[[i]],
          q$x[-nq],
          q$y[-nq],
          q$x[-1],
          q$y[-1]
        )) <=
          tolerance
      },
      logical(1)
    )
    off <- which(!on_q)
    last <- if (length(off) == 0) length(p$x) else off[[1]] - 1L
    arc <- cumsum(c(0, sqrt(diff(p$x)^2 + diff(p$y)^2)))
    arc[[last]]
  }
  max(stretch_on(a, b), stretch_on(b, a))
}

# The pairs of different paths among those `routed_panel_paths()` reads that
# share a straight stretch: two straight runs of the same panel, from any
# layer, that lie on one line within `tolerance` mm and overlap along it by
# more than `min_length` mm, described by panel, the two edges, and the
# shared stretch. The router merges edges on purpose in two places, and
# those are passed over: the edges leaving one port share a trunk out of it,
# and the edges entering one port share the run into it. A shared stretch
# is one of those merges when the two paths are drawn by one layer and
# coincide from their common start, or to their common end, over the whole
# of it. Paths of two layers are drawn in two colours, so one merged into
# the other's run hides it, and that counts.
orthogonal_coincidences <- function(paths, min_length = 1, tolerance = 0.05) {
  if (length(paths) < 2) {
    return(character())
  }
  runs <- purrr::map(paths, \(path) {
    r <- path_runs(path$x, path$y)
    r$horizontal <- abs(r$y1 - r$y0) <= tolerance
    r$vertical <- abs(r$x1 - r$x0) <= tolerance
    r[r$length > 0 & (r$horizontal | r$vertical), , drop = FALSE]
  })
  total <- purrr::map_dbl(paths, \(path) {
    sum(sqrt(diff(path$x)^2 + diff(path$y)^2))
  })
  panels <- purrr::map_int(paths, "panel")

  found <- list()
  for (i in seq_along(paths)) {
    for (j in seq_along(paths)) {
      if (j <= i || panels[[i]] != panels[[j]]) {
        next
      }
      a <- runs[[i]]
      b <- runs[[j]]
      if (nrow(a) == 0 || nrow(b) == 0) {
        next
      }
      pairs <- expand.grid(ra = seq_len(nrow(a)), rb = seq_len(nrow(b)))
      ra <- a[pairs$ra, ]
      rb <- b[pairs$rb, ]
      same_line <- (ra$horizontal &
        rb$horizontal &
        abs(ra$y0 - rb$y0) <= tolerance) |
        (ra$vertical & rb$vertical & abs(ra$x0 - rb$x0) <= tolerance)
      along_a <- ifelse(ra$horizontal, 1, 2)
      lo_a <- ifelse(along_a == 1, pmin(ra$x0, ra$x1), pmin(ra$y0, ra$y1))
      hi_a <- ifelse(along_a == 1, pmax(ra$x0, ra$x1), pmax(ra$y0, ra$y1))
      lo_b <- ifelse(along_a == 1, pmin(rb$x0, rb$x1), pmin(rb$y0, rb$y1))
      hi_b <- ifelse(along_a == 1, pmax(rb$x0, rb$x1), pmax(rb$y0, rb$y1))
      overlap <- pmin(hi_a, hi_b) - pmax(lo_a, lo_b)
      hit <- which(same_line & overlap > min_length)
      if (length(hit) == 0) {
        next
      }
      # only the paths one layer draws merge on purpose: a path of one layer
      # merged into a run of another is drawn on top of it
      same_layer <- paths[[i]]$grob == paths[[j]]$grob
      prefix_a <- if (same_layer) {
        shared_stretch(paths[[i]], paths[[j]], tolerance)
      } else {
        0
      }
      suffix_a <- if (same_layer) {
        shared_stretch(paths[[i]], paths[[j]], tolerance, TRUE)
      } else {
        0
      }
      for (k in hit) {
        # the shared stretch in the arc length of path `i`
        run <- ra[k, ]
        start <- ifelse(along_a[[k]] == 1, run$x0, run$y0)
        span <- c(pmax(lo_a, lo_b)[[k]], pmin(hi_a, hi_b)[[k]])
        arc <- run$from + abs(span - start)
        arc <- sort(arc)
        merged <- arc[[2]] <= prefix_a + tolerance ||
          arc[[1]] >= total[[i]] - suffix_a - tolerance
        if (merged) {
          next
        }
        found[[length(found) + 1L]] <- sprintf(
          "panel %s (%s), %s and %s share %.2f mm of a %s run at %s = %.2f mm",
          paths[[i]]$panel,
          paths[[i]]$facet,
          paths[[i]]$edge,
          paths[[j]]$edge,
          overlap[[k]],
          if (along_a[[k]] == 1) "horizontal" else "vertical",
          if (along_a[[k]] == 1) "y" else "x",
          if (along_a[[k]] == 1) run$y0 else run$x0
        )
      }
    }
  }
  unlist(found) %||% character()
}

# The heads among `drawings`, from `routed_arrow_drawings()`, that another
# edge covers, described by panel and the two edges. A head is drawn from
# the point ggarrow cuts its path back to towards its tip, and it is covered
# when another path of the same panel, from any layer, comes within
# `clearance` mm of that axis along the stretch ggarrow draws of it: the
# other path's resected ends are not drawn, and its own head lies on what is
# left. Two heads on one row, or a run drawn across a head, cover it.
orthogonal_covered_heads <- function(drawings, clearance = 0.5) {
  drawn_paths <- purrr::imap(drawings, \(drawn, g) {
    purrr::map(seq_along(drawn$paths), \(k) {
      path <- drawn$paths[[k]]
      n <- length(path$x)
      nodes <- drawn$nodes
      from <- node_at_end(nodes, path$x[[1]], path$y[[1]])
      to <- node_at_end(nodes, path$x[[n]], path$y[[n]])
      to_start <- sqrt((path$x - path$x[[1]])^2 + (path$y - path$y[[1]])^2)
      to_end <- sqrt((path$x - path$x[[n]])^2 + (path$y - path$y[[n]])^2)
      ink <- to_start >= drawn$fins[[k]] & to_end >= drawn$head[[k]]
      list(
        panel = drawn$panel,
        facet = drawn$facet,
        id = paste(g, k),
        edge = paste(nodes$name[from], "->", nodes$name[to]),
        x = path$x,
        y = path$y,
        ink_x = path$x[ink],
        ink_y = path$y[ink],
        head = drawn$head[[k]],
        reach = drawn$head_reach[[k]]
      )
    })
  }) |>
    purrr::list_flatten()

  found <- list()
  for (path in drawn_paths) {
    if (path$reach <= 0) {
      next
    }
    tip <- drawn_tip_point(path$x, path$y, path$head, path$reach)
    cut <- resect_cut_point(path$x, path$y, path$head + path$reach)
    if (anyNA(tip) || anyNA(cut)) {
      next
    }
    along <- seq(0, 1, length.out = 11)
    axis_x <- cut[[1]] + along * (tip[[1]] - cut[[1]])
    axis_y <- cut[[2]] + along * (tip[[2]] - cut[[2]])
    for (other in drawn_paths) {
      if (
        other$id == path$id ||
          other$panel != path$panel ||
          length(other$ink_x) < 2
      ) {
        next
      }
      m <- length(other$ink_x)
      nearest <- min(vapply(
        seq_along(axis_x),
        \(i) {
          min(point_segment_distance(
            axis_x[[i]],
            axis_y[[i]],
            other$ink_x[-m],
            other$ink_y[-m],
            other$ink_x[-1],
            other$ink_y[-1]
          ))
        },
        numeric(1)
      ))
      if (nearest < clearance) {
        found[[length(found) + 1L]] <- sprintf(
          "panel %s (%s), the head of %s is covered by %s, drawn %.2f mm from its axis",
          path$panel,
          path$facet,
          path$edge,
          other$edge,
          nearest
        )
      }
    }
  }
  unlist(found) %||% character()
}

# The three invariants over the drawn orthogonal paths of `plot` on a device
# `width` by `height` inches, read from one drawing: `ends`, from
# `orthogonal_run_ends()`; `paths`, from `routed_panel_paths()`; the failures
# of each invariant, `off_run` (an ornament off its run,
# `orthogonal_run_mismatches()`), `inside` (a path within another node's
# outline, `orthogonal_pass_throughs()`), and `shared` (two paths sharing a
# stretch, `orthogonal_coincidences()`); `failures`, every failure of the
# three; and `covered`, the heads another edge covers
# (`orthogonal_covered_heads()`).
orthogonal_invariants <- function(plot, width = 7, height = 5) {
  drawings <- routed_arrow_drawings(plot, width = width, height = height)
  ends <- orthogonal_run_ends(drawings)
  paths <- routed_panel_paths(drawings)
  off_run <- orthogonal_run_mismatches(ends)
  inside <- orthogonal_pass_throughs(paths)
  shared <- orthogonal_coincidences(paths)
  list(
    ends = ends,
    paths = paths,
    off_run = off_run,
    inside = inside,
    shared = shared,
    failures = c(off_run, inside, shared),
    covered = orthogonal_covered_heads(drawings)
  )
}

# The failures alone.
orthogonal_invariant_failures <- function(plot, width = 7, height = 5) {
  orthogonal_invariants(plot, width = width, height = height)$failures
}
