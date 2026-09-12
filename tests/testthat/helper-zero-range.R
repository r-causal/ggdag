# A five-node chain laid out along a single horizontal line. Every node shares
# `y = 0`, so the y scale trains to a zero-width range while x spans 4 units.
flat_chain_dag <- function() {
  dagify(
    b ~ a,
    c ~ b,
    d ~ c,
    e ~ d,
    coords = list(
      x = c(a = 0, b = 1, c = 2, d = 3, e = 4),
      y = c(a = 0, b = 0, c = 0, d = 0, e = 0)
    )
  )
}

# The same chain transposed onto a single vertical line, so x is the
# zero-range axis and y spans 4 units.
upright_chain_dag <- function() {
  dagify(
    b ~ a,
    c ~ b,
    d ~ c,
    e ~ d,
    coords = list(
      x = c(a = 0, b = 0, c = 0, d = 0, e = 0),
      y = c(a = 0, b = 1, c = 2, d = 3, e = 4)
    )
  )
}

# A DAG with a single node, so both axes train to a zero-width range.
lone_node_dag <- function() {
  tidy_dagitty(dagitty::dagitty('dag { a [pos="0,0"] }'))
}

# The x and y limits of the first panel of a built plot, after expansion.
built_ranges <- function(p) {
  panel <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  list(x = panel$x.range, y = panel$y.range)
}

# The span every layer of a built plot draws over, on each axis: the range the
# scales trained on before the expansion was added, whatever coordinates the
# layout gave the nodes.
drawn_extent <- function(p) {
  built <- ggplot2::ggplot_build(p)
  axis <- function(fields) {
    values <- unlist(lapply(built$data, function(layer) {
      unlist(layer[intersect(fields, names(layer))], use.names = FALSE)
    }))
    range(values[is.finite(values)])
  }
  list(x = axis(c("x", "xend")), y = axis(c("y", "yend")))
}

# Two nodes a circle layout puts on one horizontal line. `ggraph`'s circle
# layout places node `k` of `n` at angle `2 * pi * k / n`, so the second node
# of a pair sits at `sin(pi)`, which is 1.224647e-16 rather than 0. The two
# nodes are level to every purpose except an exact comparison.
circle_pair_dag <- function() {
  dagify(global_temp ~ ac_use)
}

# Two nodes a thousandth of the x span apart on y, a span small enough to look
# like noise beside the coordinates but large enough to be real.
shallow_pair_dag <- function() {
  dagify(
    b ~ a,
    coords = list(x = c(a = 0, b = 1), y = c(a = 0, b = 0.001))
  )
}

# The node centres of a built plot, in millimetres of its panel viewport.
# The plot is drawn on an off-screen ragg device at `size` inches and the
# grob tree is forced, so the centres read here are the ones the reader sees
# rather than the data the layer carries.
node_centres_mm <- function(p, size) {
  file <- tempfile(fileext = ".png")
  ragg::agg_png(
    file,
    width = size[[1]],
    height = size[[2]],
    units = "in",
    res = 150
  )
  on.exit(
    {
      grDevices::dev.off()
      unlink(file)
    },
    add = TRUE
  )

  gtable <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  grid::grid.newpage()
  grid::grid.draw(gtable)
  grid::grid.force()

  paths <- grid::grid.grep("points", grep = TRUE, global = TRUE)
  paths <- vapply(paths, as.character, character(1))
  paths <- paths[grepl("^layout::panel", paths)]
  stopifnot(length(paths) == 1)

  viewport <- strsplit(paths[[1]], "::", fixed = TRUE)[[1]][[2]]
  grid::seekViewport(viewport)
  on.exit(grid::upViewport(0), add = TRUE)

  nodes <- grid::grid.get(paths[[1]])
  data.frame(
    x = grid::convertX(nodes$x, "mm", TRUE),
    y = grid::convertY(nodes$y, "mm", TRUE)
  )
}
