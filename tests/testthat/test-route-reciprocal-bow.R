# The shape of a reciprocal pair, measured on drawn pictures.
#
# Two nodes joined by an edge each way are drawn as a lens: each member
# bows to its own side of the shared chord, and the two bows are mirror
# images of one another. Separating the pair at the discs and at mid-chord
# is not enough for that picture. A pair whose members are straight lines
# translated apart is separated everywhere and still reads as two slack
# lines, because neither line curves; what makes a lens is one signed
# curvature per member, deep enough to see and continuous from end to end.
#
# So this file measures the shape of each member on its own, in the frame
# of its own chord, rather than the distance between the two. Every block
# renders a plot with the ggarrow engine on an off-screen ragg device at
# 150 dpi, forces the grob tree so that `makeContent()` has run, and reads
# the drawn paths back in millimetres through the panel viewport, the way
# test-label-placement-quality.R and test-route-reciprocal-ends.R do.
#
# The drawn polyline runs from one node centre to the other, so a member's
# own chord is the pair's chord and the two frames differ only in
# direction. Deflection is reported as a fraction of the chord, since that
# is the quantity the reader sees: a bow of a fixed number of millimetres
# reads as an arc on a short chord and as a straight line on a long one.

# The constants of the default picture: node size 16 draws a disc of
# radius 6 mm, so the clearance margin is m = max(0.5 r, 1.2) = 3 mm and
# the router keeps R = r + m = 9 mm from every disc it is not an endpoint
# of, to within its verification tolerance of 0.1 mm. The parallel spread
# is sep_m = max(r, 2.5) = 6 mm.
bow_node_radius <- 6
bow_clearance <- 9
bow_verify_tol <- 0.1
bow_sep_m <- 6

# The band each member's deflection off its own chord must fall in, as a
# fraction of that chord. The router aims for half its own free-bow
# sagitta cap, `sagitta_max / 2` = 0.11, floored at `sep_m / 2` so that a
# short chord still separates the pair and capped at `4 R` so that a very
# long one does not balloon. Neither the floor nor the cap binds at the
# sizes measured here: the chords run from 53 to 145 mm, while the
# floor binds below 27 mm of chord and the cap above 327 mm. The band is wide
# enough that the interpolated curve need not land on the parabola that
# drives it to the millimetre.
bow_depth_band <- c(0.095, 0.145)

# Scenes -----------------------------------------------------------------------

# The cycle is the subject of every scene here, so the warning the tidied
# DAG raises about it is muffled by class rather than by silencing
# whatever else a scene might say.
without_bow_cycle_warning <- function(expr) {
  withCallingHandlers(
    expr,
    ggdag_cyclic_warning = function(cnd) rlang::cnd_muffle(cnd)
  )
}

bow_pair_dag <- function() {
  without_bow_cycle_warning(
    dagify(ac_use ~ global_temp, global_temp ~ ac_use)
  )
}

# The same loop inside a larger graph, so that the pair bows among other
# nodes rather than alone on the panel.
bow_embedded_dag <- function() {
  without_bow_cycle_warning(dagify(
    ac_use ~ global_temp,
    global_temp ~ ac_use,
    global_temp ~ industry,
    health ~ ac_use,
    wealth ~ industry,
    ac_use ~ wealth
  ))
}

bow_plots <- list(isolated = bow_pair_dag, embedded = bow_embedded_dag)

bow_sizes <- list(c(3, 2.5), c(4.5, 3.5), c(7, 5))

bow_size_key <- function(size) paste0(size[[1]], "x", size[[2]])

# Measuring a drawn plot -------------------------------------------------------

bow_scene <- function(dag, size) {
  without_bow_cycle_warning(bow_drawn_scene(dag, size))
}

bow_drawn_scene <- function(dag, size) {
  withr::local_options(list(
    ggdag.edge_engine = "ggarrow",
    ggdag.edge_route = "spline"
  ))
  plot <- ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto) +
    theme_dag()

  file <- withr::local_tempfile(fileext = ".png")
  open_test_ragg(file, size[[1]], size[[2]], res = 150)
  on.exit(grDevices::dev.off(), add = TRUE)

  built <- ggplot2::ggplot_build(plot)
  grid::grid.newpage()
  grid::grid.draw(ggplot2::ggplot_gtable(built))
  grid::grid.force()

  paths <- grid::grid.grep("arrow_path", grep = TRUE, global = TRUE)
  paths <- vapply(paths, as.character, character(1))
  paths <- paths[grepl("^layout::panel", paths)]
  paths <- paths[grepl("arrow_path", sub(".*::", "", paths))]
  viewport <- strsplit(paths[[1]], "::", fixed = TRUE)[[1]][[2]]
  grid::seekViewport(viewport)
  # the viewport is left before the device is closed: once it is closed,
  # grid would open the default device to leave it, which writes Rplots.pdf
  on.exit(grid::upViewport(0), add = TRUE, after = FALSE)
  panel_width <- grid::convertWidth(grid::unit(1, "npc"), "mm", TRUE)
  panel_height <- grid::convertHeight(grid::unit(1, "npc"), "mm", TRUE)

  drawn <- unlist(
    lapply(paths, function(p) bow_paths_mm(grid::grid.get(p))),
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
# per edge.
bow_paths_mm <- function(grob) {
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

bow_densify <- function(path, spacing = 0.25) {
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

bow_nearest_node <- function(nodes, x, y) {
  which.min((nodes$x - x)^2 + (nodes$y - y)^2)
}

# The pair of drawn paths that run between one pair of nodes in opposite
# directions, with the indices of the two node centres they join.
bow_members <- function(scene) {
  ends <- lapply(scene$drawn, function(path) {
    last <- nrow(path)
    c(
      bow_nearest_node(scene$nodes, path$x[[1]], path$y[[1]]),
      bow_nearest_node(scene$nodes, path$x[[last]], path$y[[last]])
    )
  })
  for (i in seq_along(ends)) {
    for (j in seq_along(ends)) {
      if (i < j && identical(ends[[i]], rev(ends[[j]]))) {
        return(list(paths = scene$drawn[c(i, j)], ends = ends[[i]]))
      }
    }
  }
  NULL
}

# The signed offset of a densified path from the chord that runs from
# `from` to `to`, with the position along that chord as a fraction of its
# length. Passing the member's own endpoints measures it against its own
# chord; passing one member's endpoints for both measures the pair in a
# common frame, where the two sides are opposite signs.
bow_profile <- function(path, from, to) {
  dense <- bow_densify(path)
  d <- to - from
  len <- sqrt(sum(d^2))
  u <- d / len
  normal <- c(-u[[2]], u[[1]])
  list(
    t = ((dense$x - from[[1]]) * u[[1]] + (dense$y - from[[2]]) * u[[2]]) / len,
    offset = (dense$x - from[[1]]) *
      normal[[1]] +
      (dense$y - from[[2]]) * normal[[2]],
    chord = len
  )
}

# One member measured against its own chord.
bow_own_profile <- function(path) {
  last <- nrow(path)
  bow_profile(
    path,
    c(path$x[[1]], path$y[[1]]),
    c(path$x[[last]], path$y[[last]])
  )
}

# The signed offset furthest from zero.
bow_extreme <- function(profile) {
  profile$offset[[which.max(abs(profile$offset))]]
}

# Scenes are built once per plot and size and shared by the blocks that
# measure them.
bow_cache <- new.env(parent = emptyenv())

bow_cached <- function(name, size) {
  key <- paste(name, bow_size_key(size))
  if (is.null(bow_cache[[key]])) {
    bow_cache[[key]] <- bow_scene(bow_plots[[name]](), size)
  }
  bow_cache[[key]]
}

# The rule, and the guard that keeps it off every other picture -----------------

test_that("the bow depth is a fraction of the chord, floored and capped", {
  opts <- route_constants(bow_node_radius)
  # the ordinary picture: half the free-bow sagitta cap
  expect_equal(reciprocal_bow_depth(80, opts), 0.11 * 80)
  # a chord too short for that to separate the pair keeps the half-spread
  expect_equal(reciprocal_bow_depth(20, opts), opts$sep_m / 2)
  # and a chord long enough for it to balloon is held at four padded radii
  expect_equal(reciprocal_bow_depth(600, opts), 4 * opts$R)
})

test_that("a parallel group is reciprocal only when it runs both ways", {
  # The bow is reached through this flag alone, so a scene with no
  # anti-parallel pair cannot take the branch and is routed exactly as it
  # was. A group of duplicates running the same way is spread as before.
  nodes <- data.frame(
    name = c("a", "b", "c"),
    x = c(0, 30, 60),
    y = c(0, 0, 0),
    r = bow_node_radius
  )
  from <- c(1L, 1L, 2L, 3L)
  to <- c(2L, 2L, 3L, 2L)
  groups <- parallel_groups(from, to, nodes, rep(TRUE, 4), sep_m = bow_sep_m)
  expect_identical(groups$reciprocal, c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(groups$shift != 0, rep(TRUE, 4))
})

# Each member is a bow of visible depth ----------------------------------------

test_that("each member of a reciprocal pair bows visibly off its own chord", {
  skip_on_cran()
  skip_if_not_installed("ragg")

  failures <- character()
  for (name in names(bow_plots)) {
    for (size in bow_sizes) {
      pair <- bow_members(bow_cached(name, size))
      expect_false(is.null(pair))
      for (k in seq_along(pair$paths)) {
        profile <- bow_own_profile(pair$paths[[k]])
        ratio <- max(abs(profile$offset)) / profile$chord
        if (ratio < bow_depth_band[[1]] || ratio > bow_depth_band[[2]]) {
          failures <- c(
            failures,
            sprintf(
              "%s %s member %d: %.4f of chord",
              name,
              bow_size_key(size),
              k,
              ratio
            )
          )
        }
      }
    }
  }
  expect_equal(failures, character())
})

test_that("neither member of a reciprocal pair changes the side it bows to", {
  skip_on_cran()
  skip_if_not_installed("ragg")

  # The first and last twentieth of the chord is the noise band: the
  # samples there sit within a few hundredths of a millimetre of the
  # chord, where the sign of the offset carries no shape. Everywhere else
  # a single signed curvature means every sample lies on one side.
  failures <- character()
  for (name in names(bow_plots)) {
    for (size in bow_sizes) {
      pair <- bow_members(bow_cached(name, size))
      for (k in seq_along(pair$paths)) {
        profile <- bow_own_profile(pair$paths[[k]])
        keep <- profile$t >= 0.05 & profile$t <= 0.95
        offsets <- profile$offset[keep]
        wrong <- sign(offsets) == -sign(bow_extreme(profile)) &
          abs(offsets) > 0.05
        if (any(wrong)) {
          failures <- c(
            failures,
            sprintf(
              "%s %s member %d: %d of %d samples on the far side, worst %.3f mm",
              name,
              bow_size_key(size),
              k,
              sum(wrong),
              length(offsets),
              max(abs(offsets[wrong]))
            )
          )
        }
      }
    }
  }
  expect_equal(failures, character())
})

test_that("the two members of a reciprocal pair are mirror images", {
  skip_on_cran()
  skip_if_not_installed("ragg")

  for (name in names(bow_plots)) {
    for (size in bow_sizes) {
      pair <- bow_members(bow_cached(name, size))
      first <- pair$paths[[1]]
      last <- nrow(first)
      from <- c(first$x[[1]], first$y[[1]])
      to <- c(first$x[[last]], first$y[[last]])
      extremes <- vapply(
        pair$paths,
        function(path) bow_extreme(bow_profile(path, from, to)),
        numeric(1)
      )
      label <- paste(name, bow_size_key(size))
      # opposite sides of the shared chord
      expect_lt(extremes[[1]] * extremes[[2]], 0, label = label)
      # and by the same depth, to within a twentieth of a millimetre: the
      # two bows are built from one rule and differ only in sign
      expect_equal(
        abs(extremes[[1]]),
        abs(extremes[[2]]),
        tolerance = 0.05 / abs(extremes[[1]]),
        label = label
      )
    }
  }
})

test_that("a reciprocal bow keeps the router's clearance from every other node", {
  skip_on_cran()
  skip_if_not_installed("ragg")

  # The bow is deeper than the chord it replaces was, so it reaches into
  # the panel where the other nodes are. It is routed by the same
  # machinery as any other curve and must keep the same R from every disc
  # it is not an endpoint of.
  failures <- character()
  for (size in bow_sizes) {
    scene <- bow_cached("embedded", size)
    pair <- bow_members(scene)
    others <- setdiff(seq_len(nrow(scene$nodes)), pair$ends)
    expect_gt(length(others), 0)
    for (k in seq_along(pair$paths)) {
      dense <- bow_densify(pair$paths[[k]])
      gaps <- vapply(
        others,
        function(i) {
          sqrt(min(
            (dense$x - scene$nodes$x[[i]])^2 + (dense$y - scene$nodes$y[[i]])^2
          ))
        },
        numeric(1)
      )
      if (any(gaps < bow_clearance - bow_verify_tol)) {
        failures <- c(
          failures,
          sprintf(
            "%s member %d: %s",
            bow_size_key(size),
            k,
            paste(sprintf("%.2f", gaps), collapse = ", ")
          )
        )
      }
    }
  }
  expect_equal(failures, character())
})
