# The ends of a reciprocal pair, measured on drawn pictures.
#
# Two nodes joined by an edge each way are a parallel group of two. The
# router spreads the group by translating each member away from the chord,
# which separates the two curves in the middle and leaves them meeting at
# the discs: a lens, wide at mid-chord and pinched to nothing at each end.
# What the reader needs is two edges all the way in, so each member takes
# its own arrival and departure port on each disc, aimed `sep_e` apart.
#
# Every block here renders a plot with the ggarrow engine on an off-screen
# ragg device at 150 dpi, forces the grob tree so that `makeContent()` has
# run, and reads the drawn paths back in millimetres through the panel
# viewport, the way test-label-placement-quality.R does. The measurement
# window is 18 mm around a node centre: the arrow layer resects the first
# and last `cap` = 8 mm of every path and the arrowhead itself is 2 mm, so
# a window much tighter than that holds no ink at all.
#
# Four device sizes, because the router works in millimetres while the
# layout works in data units: the smaller the device, the shorter the chord
# and the wider the angle a fixed mid-chord spread opens at the ends, so a
# separation that holds at 3 x 2.5 inches is not the one that is hardest to
# keep. The two routes are measured separately; they reach the ends by
# different machinery, and the largest size is there because only the large
# end of the range tells the two apart.

# The constants of the default picture: node size 16 draws a disc of radius
# 6 mm, so the edge separation is sep_e = max(0.6 r, 1.5) = 3.6 mm and the
# parallel spread is sep_m = max(r, 2.5) = 6 mm. The cap the arrow layer
# resects is node_size / 2 = 8 mm.
reciprocal_sep_e <- 3.6
reciprocal_sep_m <- 6
reciprocal_cap <- 8
reciprocal_window <- 18

# Scenes -----------------------------------------------------------------------

# The cycle is the subject of every scene here, so the warning the tidied
# DAG raises about it is muffled by class rather than by silencing whatever
# else a scene might say. Copied from test-label-placement-quality.R so
# this file needs no helper it does not own.
without_reciprocal_cycle_warning <- function(expr) {
  withCallingHandlers(
    expr,
    ggdag_cyclic_warning = function(cnd) rlang::cnd_muffle(cnd)
  )
}

# The two-node feedback loop on its own.
reciprocal_pair_dag <- function() {
  without_reciprocal_cycle_warning(
    dagify(ac_use ~ global_temp, global_temp ~ ac_use)
  )
}

# The same loop inside a larger graph, so that the pair is routed among
# other edges rather than alone on the panel.
embedded_reciprocal_dag <- function() {
  without_reciprocal_cycle_warning(dagify(
    ac_use ~ global_temp,
    global_temp ~ ac_use,
    global_temp ~ industry,
    health ~ ac_use,
    wealth ~ industry,
    ac_use ~ wealth
  ))
}

reciprocal_plots <- list(
  isolated = reciprocal_pair_dag,
  embedded = embedded_reciprocal_dag
)

reciprocal_sizes <- list(c(3, 2.5), c(4.5, 3.5), c(7, 5), c(14, 10))

reciprocal_size_key <- function(size) {
  paste0(size[[1]], "x", size[[2]])
}

# What each route delivers at the discs, as a function of the picture.
#
# Orthogonal mode allocates the two ports arithmetically, `sep_e / 2`
# either side of the centre line, so its ends come out `sep_e` apart at
# every size: measured at ten sizes from 3 x 2.5 to 40 x 26 inches, on
# both scenes, every value was 3.600 or 3.601 mm.
#
# Spline mode reaches the separation through the feedback loop in
# `arrival_state()`, which is given four passes. Each pass closes part of
# the angle still owed, and the part one pass closes falls as the chord
# lengthens, so what four passes deliver drifts down with the chord: on
# the isolated pair the closest approach is 3.592 mm at a 145 mm chord,
# 3.498 at 250 mm, 3.395 at 377 mm and 3.011 at 843 mm, and the embedded
# pair sits on the same line. The same loop run to convergence holds every
# one of those chords between 3.44 and 3.55 mm, so what the drift measures
# is the pass budget and not the geometry. This floor tracks the four
# passes: `sep_e` less a tenth of a millimetre to a 150 mm chord, and
# 0.0009 mm more per millimetre of chord beyond that, which the measured
# values clear by between 0.05 and 0.14 mm across the whole range.
reciprocal_floor <- function(route, chord) {
  slack <- if (route == "spline") {
    0.1 + 0.0009 * max(0, chord - 150)
  } else {
    0.05
  }
  reciprocal_sep_e - slack
}

# Measuring a drawn plot -------------------------------------------------------

# Build the DAG's plot under `route`, draw it at `size` inches, and return
# the node centres and the drawn paths of the panel in millimetres. The plot
# is built and drawn under the options because `ggdag()` reads the engine
# when it is called and the routed layer reads the mode when it is added.
reciprocal_scene <- function(dag, size, route) {
  without_reciprocal_cycle_warning(
    reciprocal_drawn_scene(dag, size, route)
  )
}

reciprocal_drawn_scene <- function(dag, size, route) {
  withr::local_options(list(
    ggdag.edge_engine = "ggarrow",
    ggdag.edge_route = route
  ))
  plot <- ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto) +
    theme_dag()

  file <- withr::local_tempfile(fileext = ".png")
  ragg::agg_png(
    file,
    width = size[[1]],
    height = size[[2]],
    units = "in",
    res = 150
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  built <- ggplot2::ggplot_build(plot)
  grid::grid.newpage()
  grid::grid.draw(ggplot2::ggplot_gtable(built))
  grid::grid.force()

  paths <- grid::grid.grep("arrow_path", grep = TRUE, global = TRUE)
  paths <- vapply(paths, as.character, character(1))
  paths <- paths[grepl("^layout::panel", paths)]
  # a forced arrow grob carries the widened polygon as a child, which the
  # same pattern reaches; only the arrow grob itself carries the router's
  # polyline
  paths <- paths[grepl("arrow_path", sub(".*::", "", paths))]
  viewport <- strsplit(paths[[1]], "::", fixed = TRUE)[[1]][[2]]
  grid::seekViewport(viewport)
  on.exit(grid::upViewport(0), add = TRUE)
  panel_width <- grid::convertWidth(grid::unit(1, "npc"), "mm", TRUE)
  panel_height <- grid::convertHeight(grid::unit(1, "npc"), "mm", TRUE)

  drawn <- unlist(
    lapply(paths, function(p) reciprocal_paths_mm(grid::grid.get(p))),
    recursive = FALSE
  )

  ranges <- built$layout$panel_params[[1]]
  nodes <- built$data[[1]]
  list(
    nodes = data.frame(
      x = (nodes$x - ranges$x.range[[1]]) /
        diff(ranges$x.range) *
        panel_width,
      y = (nodes$y - ranges$y.range[[1]]) /
        diff(ranges$y.range) *
        panel_height
    ),
    drawn = drawn
  )
}

# The drawn paths of one `arrow_path` grob, one data frame of millimetres
# per edge. The grob carries the router's own polyline, so this is the line
# the reader's ink is centred on. A grob that carries no points, an empty
# layer, draws none.
reciprocal_paths_mm <- function(grob) {
  if (is.null(grob$x) || length(grob$x) == 0) {
    return(list())
  }
  ids <- grob$id_rle
  ids <- if (inherits(ids, "rle")) {
    inverse.rle(ids)
  } else {
    fields <- unclass(ids)
    rep(fields$group, fields$length)
  }
  points <- data.frame(
    x = grid::convertX(grob$x, "mm", TRUE),
    y = grid::convertY(grob$y, "mm", TRUE)
  )
  unname(split(points, factor(ids, levels = unique(ids))))
}

# The polyline resampled every `spacing` millimetres, so that two runs
# passing each other are caught by a point beside each other.
reciprocal_densify <- function(path, spacing = 0.25) {
  x <- path$x
  y <- path$y
  dense_x <- x[[1]]
  dense_y <- y[[1]]
  for (i in seq_len(length(x) - 1)) {
    dx <- x[[i + 1]] - x[[i]]
    dy <- y[[i + 1]] - y[[i]]
    steps <- max(1, ceiling(sqrt(dx^2 + dy^2) / spacing))
    fraction <- seq_len(steps) / steps
    dense_x <- c(dense_x, x[[i]] + fraction * dx)
    dense_y <- c(dense_y, y[[i]] + fraction * dy)
  }
  data.frame(x = dense_x, y = dense_y)
}

# The polyline with `cap` millimetres dropped at each end, which is where
# the arrow layer resects it, so only ink the reader sees remains.
reciprocal_trim <- function(path, cap = reciprocal_cap) {
  last <- nrow(path)
  to_ends <- pmin(
    sqrt((path$x - path$x[[1]])^2 + (path$y - path$y[[1]])^2),
    sqrt((path$x - path$x[[last]])^2 + (path$y - path$y[[last]])^2)
  )
  path[to_ends > cap, , drop = FALSE]
}

reciprocal_min_dist <- function(a, b) {
  if (nrow(a) == 0 || nrow(b) == 0) {
    return(NA_real_)
  }
  sqrt(min(outer(a$x, b$x, "-")^2 + outer(a$y, b$y, "-")^2))
}

# The index of the node centre nearest a point.
reciprocal_nearest_node <- function(nodes, x, y) {
  which.min((nodes$x - x)^2 + (nodes$y - y)^2)
}

# The pair of drawn paths that run between one pair of nodes in opposite
# directions, with the two node centres they join.
reciprocal_members <- function(scene) {
  ends <- lapply(scene$drawn, function(path) {
    last <- nrow(path)
    c(
      reciprocal_nearest_node(scene$nodes, path$x[[1]], path$y[[1]]),
      reciprocal_nearest_node(scene$nodes, path$x[[last]], path$y[[last]])
    )
  })
  for (i in seq_along(ends)) {
    for (j in seq_along(ends)) {
      if (i < j && identical(ends[[i]], rev(ends[[j]]))) {
        return(list(
          paths = scene$drawn[c(i, j)],
          nodes = scene$nodes[ends[[i]], , drop = FALSE]
        ))
      }
    }
  }
  NULL
}

# The closest approach between the resected ink of the two members, within
# `window` millimetres of each node centre; one value per node.
reciprocal_end_gaps <- function(scene, window = reciprocal_window) {
  pair <- reciprocal_members(scene)
  ink <- lapply(pair$paths, function(path) {
    reciprocal_trim(reciprocal_densify(path))
  })
  vapply(
    seq_len(nrow(pair$nodes)),
    function(k) {
      near <- lapply(ink, function(points) {
        keep <- (points$x - pair$nodes$x[[k]])^2 +
          (points$y - pair$nodes$y[[k]])^2 <=
          window^2
        points[keep, , drop = FALSE]
      })
      reciprocal_min_dist(near[[1]], near[[2]])
    },
    numeric(1)
  )
}

# The length of the pair's chord in millimetres, which is what the floor
# the ends are held to is a function of.
reciprocal_chord <- function(scene) {
  nodes <- reciprocal_members(scene)$nodes
  sqrt(diff(nodes$x)^2 + diff(nodes$y)^2)
}

# The distance between the two members across the middle of their chord:
# each member's offset from the chord where it crosses the chord's
# midpoint, differenced.
reciprocal_mid_gap <- function(scene) {
  pair <- reciprocal_members(scene)
  from <- c(pair$nodes$x[[1]], pair$nodes$y[[1]])
  to <- c(pair$nodes$x[[2]], pair$nodes$y[[2]])
  d <- to - from
  len <- sqrt(sum(d^2))
  u <- d / len
  n <- c(-u[[2]], u[[1]])
  offsets <- vapply(
    pair$paths,
    function(path) {
      dense <- reciprocal_densify(path)
      t <- ((dense$x - from[[1]]) * u[[1]] + (dense$y - from[[2]]) * u[[2]]) /
        len
      k <- which.min(abs(t - 0.5))
      (dense$x[[k]] - from[[1]]) * n[[1]] + (dense$y[[k]] - from[[2]]) * n[[2]]
    },
    numeric(1)
  )
  abs(diff(offsets))
}

# Scenes are built once per plot, size, and route, and shared by the blocks
# that measure them.
reciprocal_cache <- new.env(parent = emptyenv())

reciprocal_cached <- function(name, size, route) {
  key <- paste(name, reciprocal_size_key(size), route)
  if (is.null(reciprocal_cache[[key]])) {
    reciprocal_cache[[key]] <- reciprocal_scene(
      reciprocal_plots[[name]](),
      size,
      route
    )
  }
  reciprocal_cache[[key]]
}

# The ends of a reciprocal pair -------------------------------------------------

test_that("a reciprocal pair keeps sep_e between its ink at every disc", {
  skip_on_cran()
  skip_if_not_installed("ragg")

  failures <- character()
  for (name in names(reciprocal_plots)) {
    for (route in c("spline", "orthogonal")) {
      for (size in reciprocal_sizes) {
        scene <- reciprocal_cached(name, size, route)
        gaps <- reciprocal_end_gaps(scene)
        expect_false(anyNA(gaps))
        bound <- reciprocal_floor(route, reciprocal_chord(scene))
        if (any(gaps < bound)) {
          failures <- c(
            failures,
            sprintf(
              "%s %s %s: %s below %.3f",
              name,
              route,
              reciprocal_size_key(size),
              paste(sprintf("%.2f", gaps), collapse = ", "),
              bound
            )
          )
        }
      }
    }
  }
  expect_equal(failures, character())
})

test_that("separating the ends leaves the middle of a reciprocal pair alone", {
  skip_on_cran()
  skip_if_not_installed("ragg")

  # Orthogonal mode translates the two members sep_m apart and the port
  # work acts on the ends alone, so the mid-chord spread of the isolated
  # loop is the router's own sep_m at every size.
  for (size in reciprocal_sizes) {
    expect_equal(
      reciprocal_mid_gap(reciprocal_cached("isolated", size, "orthogonal")),
      reciprocal_sep_m,
      tolerance = 0.02,
      label = paste("isolated orthogonal", reciprocal_size_key(size))
    )
  }

  # Spline mode bows each member off the chord rather than translating it,
  # so the middle opens to twice the bow's depth, which is a fraction of
  # the chord and not a fixed distance. What the ends owe the middle is
  # that it never closes below the translation the pair would have had;
  # test-route-reciprocal-bow.R pins the depth itself.
  for (size in reciprocal_sizes) {
    expect_gte(
      reciprocal_mid_gap(reciprocal_cached("isolated", size, "spline")),
      reciprocal_sep_m - 0.02
    )
  }
})

# Nothing without a reciprocal pair moves ---------------------------------------

test_that("the canonical DAGs route exactly as they did before", {
  # None of the 22 canonical graphs holds an anti-parallel pair, so every
  # path and every meta field the router draws for them must survive the
  # reciprocal-end work byte for byte. The baseline is pinned in
  # fixtures/route-invariance.rds, regenerated only on purpose with
  # tests/testthat/fixtures/make-route-fixtures.R.
  fixture <- readRDS(test_path("fixtures", "route-invariance.rds"))
  expect_named(fixture, names(canonical_dag_specs))

  for (name in names(canonical_dag_specs)) {
    scene <- canonical_mm_scene(canonical_dag_specs[[name]])
    # the guard the rest of the block rests on
    expect_identical(
      anyDuplicated(rbind(
        data.frame(a = scene$edges$from, b = scene$edges$to),
        data.frame(a = scene$edges$to, b = scene$edges$from)
      )),
      0L,
      label = paste(name, "has no anti-parallel pair")
    )
    for (k in seq_along(c("spline", "orthogonal"))) {
      mode <- c("spline", "orthogonal")[[k]]
      routed <- route_edges_mm(
        scene$nodes,
        scene$edges,
        scene$bounds,
        cap = 8,
        mode = mode,
        opts = route_constants(6)
      )
      expect_identical(
        routed$paths,
        fixture[[name]][[k]]$paths,
        label = paste(name, mode, "paths")
      )
      expect_identical(
        routed$meta,
        fixture[[name]][[k]]$meta,
        label = paste(name, mode, "meta")
      )
    }
  }
})
