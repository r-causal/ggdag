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
