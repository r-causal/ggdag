# Millimetre scenes built from the canonical DAG specs, shared by the
# routing invariance fixture generator and the tests that read it. The
# time-ordered layout places each graph, the coordinates are stretched onto
# a 160 x 110 mm panel with a 12 mm inset, and every node is drawn at the
# default radius of 6 mm, which is the picture the router sees at draw time.
canonical_mm_scene <- function(spec, panel = c(160, 110), inset = 12, r = 6) {
  edges <- canonical_dag_edges(spec)
  coords <- compute_time_ordered_layout(edges)
  stretch <- function(v, extent) {
    span <- diff(range(v))
    if (span == 0) {
      return(rep(extent / 2, length(v)))
    }
    inset + (v - min(v)) / span * (extent - 2 * inset)
  }
  directed <- edges[!is.na(edges$to), , drop = FALSE]
  list(
    nodes = data.frame(
      name = coords$name,
      x = stretch(coords$x, panel[[1]]),
      y = stretch(coords$y, panel[[2]]),
      r = r,
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = directed$name,
      to = directed$to,
      curvature = NA_real_,
      stringsAsFactors = FALSE
    ),
    bounds = c(0, 0, panel)
  )
}
