# Routing beside curvature the user set, and routing edges a caller supplies.
#
# An edge nobody curved carries no curvature of its own, so the routed geom is
# free to detour it around a node in its way; only an explicit value pins an
# edge to an arc or, at `0`, straight through whatever sits on its chord.
# A caller may also hand the layer edge rows of its own, positioned wherever
# it likes; those rows are drawn and their endpoints join the obstacles.

# The forced grob tree of the panel's routed edges.
forced_routed_gtree <- function(plot, width = 7, height = 5) {
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

  paths <- grid::grid.grep("dag_routed_edges", grep = TRUE, global = TRUE)
  paths <- vapply(paths, as.character, character(1))
  # a grep matches the gTree and everything under it; the tree itself is the
  # one whose own name carries the class
  own_name <- sub(".*::", "", paths)
  paths <- paths[grepl("dag_routed_edges", own_name)]
  testthat::expect_length(paths, 1)
  grid::grid.get(paths[[1]])
}

# The children of a forced gTree drawing with class `cl`.
children_of_class <- function(gtree, cl) {
  children <- gtree$children
  if (length(children) == 0) {
    return(list())
  }
  unname(children[vapply(children, inherits, logical(1), what = cl)])
}

# The number of points on each path an `arrow_path` grob draws.
arrow_path_lengths <- function(grob) {
  ids <- grob$id_rle
  fields <- unclass(ids)
  as.integer(fields$length)
}

# The index of the routed layer of `plot`.
routed_index <- function(plot) {
  which(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomDAGRoutedArrow"),
    logical(1)
  ))
}

test_that("an edge the user curved does not stop the others from routing", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  # x -> y has the mediator dead on its chord and nobody has curved it, so it
  # routes; x -> z is curved by hand and is drawn as that arc
  dag <- dagify(
    y ~ x + m,
    m ~ x,
    z ~ x,
    coords = list(
      x = c(x = 0, m = 1, y = 2, z = 1),
      y = c(x = 0, m = 0, y = 0, z = 2)
    )
  )
  tidy_dag <- curve_edge(tidy_dagitty(dag), "x", "z", -0.3)

  p <- ggplot(tidy_dag, aes_dag(edge_curvature = edge_curvature)) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  gtree <- forced_routed_gtree(p)

  # the curved edge is drawn by an arc grob of its own, at the value asked for
  curves <- children_of_class(gtree, "curve_arrow")
  expect_length(curves, 1)
  expect_equal(curves[[1]]$curve$curvature, -0.3)

  # and the blocked edge is still routed: one path with more than two points
  arrows <- children_of_class(gtree, "arrow_path")
  expect_length(arrows, 1)
  lengths <- arrow_path_lengths(arrows[[1]])
  expect_length(lengths, 3)
  expect_identical(sum(lengths > 2), 1L)
})

test_that("an explicit zero still pins an edge straight through its blocker", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
  tidy_dag <- curve_edge(tidy_dagitty(dag), "x", "y", 0)

  p <- ggplot(tidy_dag, aes_dag(edge_curvature = edge_curvature)) +
    geom_dag_routed_arrows() +
    geom_dag_point()

  gtree <- forced_routed_gtree(p)
  expect_length(children_of_class(gtree, "curve_arrow"), 0)

  arrows <- children_of_class(gtree, "arrow_path")
  expect_length(arrows, 1)
  expect_true(all(arrow_path_lengths(arrows[[1]]) == 2L))
})

test_that("the routed layer draws edge rows the caller supplies", {
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
  tidy_dag <- tidy_dagitty(dag)

  # the same edges nudged sideways, so no row of the frame matches a plot row
  edges <- pull_dag_data(tidy_dag)
  edges <- edges[!is.na(edges$to), , drop = FALSE]
  edges$x <- edges$x + 0.01
  edges$xend <- edges$xend + 0.01

  p <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_routed_arrows(data_directed = edges) +
    geom_dag_point()

  index <- routed_index(p)
  expect_length(index, 1)

  built <- ggplot2::layer_data(p, index)
  expect_identical(sum(built$draw), nrow(edges))

  gtree <- forced_routed_gtree(p)
  arrows <- children_of_class(gtree, "arrow_path")
  expect_length(arrows, 1)
  expect_length(arrow_path_lengths(arrows[[1]]), nrow(edges))
})
