# Tests for the order a quick plotter draws its shadow edges in.
#
# `ggdag_paths()`, `ggdag_paths_fan()`, and `ggdag_adjustment_set()` all draw
# the whole DAG greyed out or faded and then emphasise a subset on top of it:
# the paths of one set, or the edges an adjustment leaves open. The greyed copy
# is context, so it belongs underneath. Ink drawn later covers ink drawn
# earlier, which makes the contract a drawing-order one: within a panel, every
# shadow edge is painted before every emphasised edge, whichever layer each of
# them ends up in.
#
# The order is read back two ways. The cheap reading walks the built layer
# data, which the edge geoms draw row by row in the order they receive them, so
# layer order followed by row order is drawing order. The expensive reading
# renders the plot to an off-screen device, forces the grob tree so that the
# routed edge geom has built its arrows, and compares the drawn runs in
# millimetres: where a shadow run and an emphasised run share a channel, the
# emphasised one has to be the ink on top.

# Helpers ----------------------------------------------------------------------

# A DAG whose paths from x to y fan out over several panels, with its nodes
# pinned so that the routes, and the channels they share, do not depend on a
# layout algorithm.
shadow_path_dag <- function() {
  dagify(
    y ~ x + z + w,
    x ~ z + w,
    z ~ w,
    exposure = "x",
    outcome = "y",
    coords = list(
      x = c(w = 0, z = 1, x = 1, y = 3),
      y = c(w = 1, z = 2, x = 0, y = 0)
    )
  )
}

# A colour as upper case RGB hex, so that "grey80" and "#CCCCCC" compare equal.
normalise_ink <- function(colour) {
  colour[is.na(colour)] <- "transparent"
  channels <- grDevices::col2rgb(colour)
  toupper(grDevices::rgb(
    channels["red", ],
    channels["green", ],
    channels["blue", ],
    maxColorValue = 255
  ))
}

# Does this layer draw edges? The routed edge geom draws them too, and marks
# the rows it draws with the `draw` aesthetic rather than being handed only
# those rows.
is_drawn_edge_layer <- function(layer) {
  is_edge_layer(layer) || inherits(layer$geom, "GeomDAGRoutedArrow")
}

# The edges `plot` draws, one row each, in the order they are drawn: layer by
# layer, and within a layer in the order the geom receives them. A ggraph edge
# layer spreads one edge over many rows of interpolated points, which share a
# group, so consecutive rows of one group are one edge.
edge_ink_order <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  indices <- which(purrr::map_lgl(plot$layers, is_drawn_edge_layer))

  drawn <- purrr::map(indices, function(index) {
    layer_data <- built$data[[index]]
    if (!is.null(layer_data[["draw"]])) {
      layer_data <- layer_data[layer_data$draw, ]
    }
    if (nrow(layer_data) == 0) {
      return(NULL)
    }

    ink <- layer_data[["edge_colour"]] %||% layer_data[["colour"]]
    opacity <- layer_data[["edge_alpha"]] %||% layer_data[["alpha"]]
    edge <- if (
      is_row_counted_edge_layer(plot$layers[[index]]) ||
        !is.null(layer_data[["draw"]])
    ) {
      seq_len(nrow(layer_data))
    } else {
      run_index(layer_data$group)
    }

    tibble::tibble(
      layer = index,
      edge = edge,
      panel = as.integer(layer_data$PANEL),
      ink = normalise_ink(ink),
      opacity = dplyr::coalesce(as.numeric(opacity %||% NA_real_), 1)
    ) |>
      dplyr::distinct(.data$layer, .data$edge, .keep_all = TRUE)
  })

  dplyr::bind_rows(drawn) |>
    dplyr::mutate(drawn = dplyr::row_number())
}

# Which run of equal values each element belongs to, so that consecutive rows
# of one group can be numbered without assuming the groups are ordered.
run_index <- function(x) {
  cumsum(c(TRUE, x[-1] != x[-length(x)]))
}

# Is this edge part of the shadow? A quick plotter shades the shadow either by
# drawing it in grey80 or by fading it, and emphasises its subset at full
# strength.
is_shadow_ink <- function(ink) {
  ink$ink == "#CCCCCC" | ink$opacity < 1
}

# The panels of `plot` where a shadow edge is drawn after an emphasised one,
# and so covers it wherever the two share a channel.
panels_shadowing_emphasis <- function(plot) {
  ink <- edge_ink_order(plot)
  ink$shadow <- is_shadow_ink(ink)

  ink |>
    dplyr::group_by(.data$panel) |>
    dplyr::summarise(
      last_shadow = max(c(-Inf, .data$drawn[.data$shadow])),
      first_emphasis = min(c(Inf, .data$drawn[!.data$shadow])),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$last_shadow > .data$first_emphasis) |>
    dplyr::pull("panel")
}

# The panels of `plot` that draw a shadow edge over an emphasised one, for each
# `edge_route`, so that one comparison names every route that misorders.
panels_shadowing_emphasis_by_route <- function(plot_fn) {
  routes <- c("straight", "spline", "orthogonal")
  offenders <- purrr::map(routes, function(route) {
    ggdag_options_set(edge_engine = "ggarrow", edge_route = route)
    panels_shadowing_emphasis(plot_fn())
  })

  rlang::set_names(offenders, routes)
}

# The arrow grobs `plot` draws in its panels, in drawing order, with the legend
# keys left out: only a grob drawn inside a panel has a panel in its grob path.
# The grobs are read with the device still open, because a routed edge builds
# its arrows in `makeContent()`.
panel_arrow_runs <- function(plot, width = 12, height = 10) {
  file <- tempfile(fileext = ".png")
  ragg::agg_png(file, width = width, height = height, units = "in", res = 96)
  on.exit(
    {
      grDevices::dev.off()
      unlink(file)
    },
    add = TRUE
  )

  grid::grid.newpage()
  grid::grid.draw(ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot)))
  grid::grid.force()

  paths <- vapply(
    grid::grid.grep("arrow_path", grep = TRUE, global = TRUE),
    as.character,
    character(1)
  )
  paths <- paths[grepl("arrow_path", sub(".*::", "", paths))]
  panels <- as.integer(sub(
    ".*panel-([0-9]+)\\..*",
    "\\1",
    paths[grepl("panel-[0-9]+\\.", paths)]
  ))
  paths <- paths[grepl("panel-[0-9]+\\.", paths)]

  runs <- purrr::list_flatten(purrr::map2(paths, panels, function(path, panel) {
    arrow_runs(grid::grid.get(path), panel)
  }))

  # where each run comes in its own panel's drawing order, so that a failure
  # can name it the way the picture is read
  drawn_in <- purrr::map_int(runs, "panel")
  at <- stats::ave(seq_along(drawn_in), drawn_in, FUN = seq_along)
  purrr::map2(runs, at, function(run, position) {
    c(run, list(at = position))
  })
}

# The runs one `arrow_path` grob draws, in millimetres, one element per edge,
# each carrying the ink it is drawn in and the width of its shaft.
arrow_runs <- function(grob, panel) {
  lengths <- unclass(grob$id_rle)$length
  run_of <- rep(seq_along(lengths), lengths)
  x <- grid::convertX(grob$x, "mm", valueOnly = TRUE)
  y <- grid::convertY(grob$y, "mm", valueOnly = TRUE)
  ink <- normalise_ink(rep_len(grob$gp$fill, length(lengths)))
  widths <- rep_len(
    grid::convertWidth(grob$shaft_width, "mm", valueOnly = TRUE),
    length(lengths)
  )

  purrr::map(seq_along(lengths), function(i) {
    list(
      panel = panel,
      ink = ink[[i]],
      width = widths[[i]],
      x = x[run_of == i],
      y = y[run_of == i]
    )
  })
}

# A run resampled at `step` millimetres, so that two runs can be compared point
# by point rather than vertex by vertex.
resample_run <- function(run, step = 0.25) {
  x <- run$x
  y <- run$y
  segments <- seq_len(length(x) - 1)
  points <- purrr::map(segments, function(i) {
    length_mm <- sqrt((x[i + 1] - x[i])^2 + (y[i + 1] - y[i])^2)
    steps <- max(1, ceiling(length_mm / step))
    at <- seq(0, 1, length.out = steps + 1)[-(steps + 1)]
    cbind(x[i] + at * (x[i + 1] - x[i]), y[i] + at * (y[i + 1] - y[i]))
  })

  rbind(do.call(rbind, points), c(x[length(x)], y[length(y)]))
}

# How far two runs travel together, in millimetres. Two shafts share ink where
# their centre lines are closer than a shaft width, and a channel they share is
# an unbroken stretch of that, which tells a shared channel from a crossing.
shared_channel_mm <- function(a, b, step = 0.25) {
  first <- resample_run(a, step)
  second <- resample_run(b, step)
  gaps <- sqrt(apply(
    outer(first[, 1], second[, 1], "-")^2 +
      outer(first[, 2], second[, 2], "-")^2,
    1,
    min
  ))

  together <- rle(gaps < max(a$width, b$width))
  if (!any(together$values)) {
    return(0)
  }

  max(together$lengths[together$values]) * step
}

# Every place a shadow run and an emphasised run share a channel of at least
# `min_mm`, described so that a failure names the panel and the overlap.
shared_channels <- function(runs, min_mm = 2) {
  shadow <- which(purrr::map_chr(runs, "ink") == "#CCCCCC")
  emphasis <- setdiff(seq_along(runs), shadow)
  pairs <- expand.grid(shadow = shadow, emphasis = emphasis)
  pairs <- pairs[
    purrr::map_int(runs[pairs$shadow], "panel") ==
      purrr::map_int(runs[pairs$emphasis], "panel"),
  ]

  overlaps <- purrr::map2_dbl(pairs$shadow, pairs$emphasis, function(i, j) {
    shared_channel_mm(runs[[i]], runs[[j]])
  })

  pairs$shared_mm <- overlaps
  pairs[overlaps >= min_mm, ]
}

# The shared channels where the shadow run is the ink on top, named panel by
# panel so that a failure reads as a picture rather than a pair of indices.
shadow_covering_emphasis <- function(runs, min_mm = 2) {
  shared <- shared_channels(runs, min_mm = min_mm)
  covered <- shared[shared$shadow > shared$emphasis, ]

  sprintf(
    "panel %s: shadow run %s covers emphasised run %s over %.1f mm",
    purrr::map_int(runs[covered$shadow], "panel"),
    purrr::map_int(runs[covered$shadow], "at"),
    purrr::map_int(runs[covered$emphasis], "at"),
    covered$shared_mm
  )
}

# The ink of every edge layer a plotter builds for itself, in layer order.
edge_layer_ink <- function(plot) {
  edge_layers <- purrr::keep(plot$layers, is_drawn_edge_layer)
  purrr::map_chr(edge_layers, function(layer) {
    normalise_ink(layer$aes_params$colour %||% NA_character_)
  })
}

# Tests ------------------------------------------------------------------------

test_that("ggdag_paths() draws its shadow edges beneath the paths it picks out", {
  expect_equal(
    panels_shadowing_emphasis(ggdag_paths(shadow_path_dag())),
    integer(0)
  )
})

test_that("ggdag_paths() draws its shadow edges first under every edge route", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()

  offenders <- panels_shadowing_emphasis_by_route(
    \() ggdag_paths(shadow_path_dag())
  )

  expect_equal(
    offenders,
    list(
      straight = integer(0),
      spline = integer(0),
      orthogonal = integer(0)
    )
  )
})

test_that("ggdag_paths_fan() draws its faded edges beneath the paths it picks out", {
  expect_equal(
    panels_shadowing_emphasis(ggdag_paths_fan(shadow_path_dag())),
    integer(0)
  )
})

test_that("ggdag_paths_fan() draws its faded edges first under the ggarrow engine", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow")

  expect_equal(
    panels_shadowing_emphasis(ggdag_paths_fan(shadow_path_dag())),
    integer(0)
  )
})

test_that("ggdag_adjustment_set() draws its blocked edges beneath the open ones", {
  expect_equal(
    panels_shadowing_emphasis(ggdag_adjustment_set(shadow_path_dag())),
    integer(0)
  )
})

test_that("ggdag_adjustment_set() draws its blocked edges first under every edge route", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()

  offenders <- panels_shadowing_emphasis_by_route(
    \() ggdag_adjustment_set(shadow_path_dag())
  )

  expect_equal(
    offenders,
    list(
      straight = integer(0),
      spline = integer(0),
      orthogonal = integer(0)
    )
  )
})

test_that("ggdag_adjustment_set() adds its shadow edge layers before the emphasised ones", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow")

  ink <- edge_layer_ink(ggdag_adjustment_set(shadow_path_dag()))
  shadow <- which(ink == "#CCCCCC")
  emphasis <- which(ink != "#CCCCCC")

  expect_gt(length(shadow), 0)
  expect_gt(length(emphasis), 0)
  expect_lt(max(shadow), min(emphasis))
})

test_that("a shadow run never covers an emphasised run it shares a channel with", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  local_ggdag_option_state()
  ggdag_options_set(edge_engine = "ggarrow", edge_route = "orthogonal")

  runs <- panel_arrow_runs(ggdag_paths(shadow_path_dag()))

  # the assertion below is only worth making on a picture where the two kinds
  # of run genuinely coincide, so check that this one has such a channel
  expect_gt(nrow(shared_channels(runs)), 0)
  expect_equal(shadow_covering_emphasis(runs), character(0))
})
