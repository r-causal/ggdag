# Tests for the automatic edge curvature safety net: find_obstructing_nodes()
# detects nodes that sit on a straight edge, and auto_curve_edges() writes a
# deterministic edge_curvature for each blocked edge, curving away from the
# side where the intruding nodes sit. The safety net runs at the end of
# tidy_dagitty() behind ggdag_option("auto_curve", FALSE) and never overrides
# curvature the user set through curved(), curve_edge(), or dagitty control
# points.

# Helpers ----------------------------------------------------------------------

# Build a minimal tidy_dagitty data tibble from a node table and an edge list.
# The column layout mirrors pull_dag_data(): one row per edge plus one terminal
# row (to = NA) for each node with no outgoing edge.
make_dag_data <- function(coords, edges) {
  direction <- edges$direction
  if (is.null(direction)) {
    direction <- rep("->", nrow(edges))
  }
  edge_rows <- tibble::tibble(
    name = edges$name,
    x = as.numeric(coords$x[match(edges$name, coords$name)]),
    y = as.numeric(coords$y[match(edges$name, coords$name)]),
    direction = factor(direction, levels = c("->", "<->", "--")),
    to = edges$to,
    xend = as.numeric(coords$x[match(edges$to, coords$name)]),
    yend = as.numeric(coords$y[match(edges$to, coords$name)])
  )
  terminal <- setdiff(coords$name, edges$name)
  terminal_rows <- tibble::tibble(
    name = terminal,
    x = as.numeric(coords$x[match(terminal, coords$name)]),
    y = as.numeric(coords$y[match(terminal, coords$name)]),
    direction = factor(NA, levels = c("->", "<->", "--")),
    to = NA_character_,
    xend = NA_real_,
    yend = NA_real_
  )
  dplyr::bind_rows(edge_rows, terminal_rows)
}

# The pinned magnitude formula: clamp(0.1 + 1.5 * worst_intrusion /
# edge_length, 0.15, 0.6), where worst_intrusion is the deepest incursion
# depth (node_radius minus the node's distance to the straight edge).
expected_magnitude <- function(worst_intrusion, edge_length) {
  pmin(pmax(0.1 + 1.5 * worst_intrusion / edge_length, 0.15), 0.6)
}

# Minimum distance from a point to the sampled drawn arc.
min_arc_clearance <- function(curvature, x, y, xend, yend, px, py) {
  pts <- sample_curved_edge(x, y, xend, yend, curvature)
  min(sqrt((pts$x - px)^2 + (pts$y - py)^2))
}

curvature_of <- function(data, from, to) {
  data$edge_curvature[which(data$name == from & data$to == to)]
}

# One blocked edge whose formula-candidate arc is itself blocked: the straight
# edge from x to y is blocked by node a just above the chord, and node b sits
# exactly where the candidate arc would pass below it, so the first candidate
# curvature collides and the safety net has to escalate or flip. with_flip_
# blocker = TRUE adds node c on the mirror point above the chord, so flipping
# the side at the candidate magnitude collides too and only escalation works.
escalation_fixture <- function(with_flip_blocker = FALSE) {
  r <- node_radius_data()
  candidate <- expected_magnitude(r - 0.05, 2)
  offset <- curvature_to_ctrl_offset(candidate, 0, 0, 2, 0)
  coords <- data.frame(
    name = c("x", "y", "a", "b", if (with_flip_blocker) "c"),
    x = c(0, 2, 1, 1, if (with_flip_blocker) 1),
    y = c(0, 0, 0.05, -offset, if (with_flip_blocker) offset)
  )
  edges <- data.frame(name = "x", to = "y")
  list(
    data = make_dag_data(coords, edges),
    coords = coords,
    candidate = candidate,
    offset = offset
  )
}

# find_obstructing_nodes -------------------------------------------------------

test_that("find_obstructing_nodes: node on the segment is reported at distance zero", {
  coords <- data.frame(name = c("a", "b", "c"), x = c(0, 1, 2), y = c(0, 0, 0))
  edges <- data.frame(name = "a", to = "c")

  res <- find_obstructing_nodes(coords, edges, node_radius_data())
  expect_s3_class(res, "data.frame")
  expect_named(res, c("edge", "node", "distance"))
  expect_identical(res$edge, 1L)
  expect_identical(res$node, "b")
  expect_equal(res$distance, 0)
})

test_that("find_obstructing_nodes: node inside the radius reports its segment distance", {
  coords <- data.frame(
    name = c("a", "b", "c"),
    x = c(0, 1, 2),
    y = c(0, 0.05, 0)
  )
  edges <- data.frame(name = "a", to = "c")

  res <- find_obstructing_nodes(coords, edges, node_radius_data())
  expect_identical(res$node, "b")
  # the geometry library is the reference for the reported distance
  expect_equal(res$distance, dist_to_edge(1, 0.05, 0, 0, 2, 0))
})

test_that("find_obstructing_nodes: node near but outside the radius is not reported", {
  # b sits 0.2 from the chord, outside the default radius of 26 / 180
  coords <- data.frame(
    name = c("a", "b", "c"),
    x = c(0, 1, 2),
    y = c(0, 0.2, 0)
  )
  edges <- data.frame(name = "a", to = "c")

  res <- find_obstructing_nodes(coords, edges, node_radius_data())
  expect_named(res, c("edge", "node", "distance"))
  expect_identical(nrow(res), 0L)
})

test_that("find_obstructing_nodes: edge endpoints are never obstructions", {
  # the segment is much shorter than the radius, so both endpoints sit within
  # node_radius of it; only non-endpoint nodes may obstruct
  coords <- data.frame(name = c("a", "b"), x = c(0, 0.05), y = c(0, 0))
  edges <- data.frame(name = "a", to = "b")

  res <- find_obstructing_nodes(coords, edges, node_radius_data())
  expect_identical(nrow(res), 0L)
})

test_that("find_obstructing_nodes: zero-length edges report no obstructions", {
  # a and b are distinct nodes at identical coordinates and c sits within the
  # radius of the degenerate point; a zero-length edge cannot be routed, so it
  # reports nothing and must not error
  coords <- data.frame(
    name = c("a", "b", "c"),
    x = c(0, 0, 0),
    y = c(0, 0, 0.05)
  )
  edges <- data.frame(name = "a", to = "b")

  res <- find_obstructing_nodes(coords, edges, node_radius_data())
  expect_identical(nrow(res), 0L)
})

test_that("find_obstructing_nodes: edge indices refer to edges_df rows, with to = NA ignored", {
  coords <- data.frame(name = c("a", "b", "c"), x = c(0, 1, 2), y = c(0, 0, 0))
  edges <- data.frame(name = c("a", "c", "a"), to = c("b", NA, "c"))

  res <- find_obstructing_nodes(coords, edges, node_radius_data())
  # only a -> c is blocked (by b); its row index counts the NA row before it
  expect_identical(res$edge, 3L)
  expect_identical(res$node, "b")
})

test_that("find_obstructing_nodes: node_radius widens the corridor", {
  coords <- data.frame(
    name = c("a", "b", "m"),
    x = c(0, 2, 1),
    y = c(0, 0, 0.5)
  )
  edges <- data.frame(name = "a", to = "b")

  expect_identical(
    nrow(find_obstructing_nodes(coords, edges, node_radius_data())),
    0L
  )

  res <- find_obstructing_nodes(coords, edges, 0.6)
  expect_identical(res$node, "m")
  expect_equal(res$distance, 0.5)
})

test_that("find_obstructing_nodes: multiple obstructions are ordered by edge, then coords order", {
  coords <- data.frame(
    name = c("a", "b", "c", "d", "e", "u", "w", "v"),
    x = c(0, 1, 2, 3, 4, 0, 2, 4),
    y = c(0, 0.05, -0.05, 0.02, 0, 3, 3.05, 3)
  )
  edges <- data.frame(name = c("a", "u"), to = c("e", "v"))

  res <- find_obstructing_nodes(coords, edges, node_radius_data())
  expect_identical(res$edge, c(1L, 1L, 1L, 2L))
  expect_identical(res$node, c("b", "c", "d", "w"))
  expect_equal(res$distance, c(0.05, 0.05, 0.02, 0.05))
})

# auto_curve_edges: side selection ---------------------------------------------

test_that("auto_curve_edges: a node above the chord bows the edge below (positive curvature)", {
  # sample_curved_edge() documents the grid::curveGrob() convention: positive
  # curvature bows below a left-to-right edge. An intruder above the chord
  # must therefore produce positive curvature.
  coords <- data.frame(
    name = c("x", "y", "m"),
    x = c(0, 2, 1),
    y = c(0, 0, 0.06)
  )
  data <- make_dag_data(coords, data.frame(name = "x", to = "y"))

  res <- auto_curve_edges(data)
  curvature <- curvature_of(res, "x", "y")
  expect_gt(curvature, 0)
  expect_equal(
    curvature,
    expected_magnitude(node_radius_data() - 0.06, 2),
    tolerance = 1e-8
  )
})

test_that("auto_curve_edges: a node below the chord bows the edge above (negative curvature)", {
  coords <- data.frame(
    name = c("x", "y", "m"),
    x = c(0, 2, 1),
    y = c(0, 0, -0.06)
  )
  data <- make_dag_data(coords, data.frame(name = "x", to = "y"))

  res <- auto_curve_edges(data)
  curvature <- curvature_of(res, "x", "y")
  expect_lt(curvature, 0)
  expect_equal(
    curvature,
    -expected_magnitude(node_radius_data() - 0.06, 2),
    tolerance = 1e-8
  )
})

test_that("auto_curve_edges: the majority-blocked side decides the bow direction", {
  # two intruders above the chord and one below: the arc bows below, away
  # from the majority
  coords <- data.frame(
    name = c("x", "y", "a", "b", "c"),
    x = c(0, 3, 1, 2, 1.5),
    y = c(0, 0, 0.05, 0.05, -0.05)
  )
  data <- make_dag_data(coords, data.frame(name = "x", to = "y"))

  res <- auto_curve_edges(data)
  curvature <- curvature_of(res, "x", "y")
  expect_gt(curvature, 0)
})

# auto_curve_edges: magnitude --------------------------------------------------

test_that("auto_curve_edges: magnitude follows the pinned formula in mid-range", {
  r <- node_radius_data()
  coords <- data.frame(
    name = c("x", "y", "m"),
    x = c(0, 2, 1),
    y = c(0, 0, 0.05)
  )
  data <- make_dag_data(coords, data.frame(name = "x", to = "y"))

  res <- auto_curve_edges(data)
  raw <- 0.1 + 1.5 * (r - 0.05) / 2
  expect_gt(raw, 0.15)
  expect_lt(raw, 0.6)
  expect_equal(curvature_of(res, "x", "y"), raw, tolerance = 1e-8)
})

test_that("auto_curve_edges: shallow intrusions clamp the magnitude to 0.15", {
  r <- node_radius_data()
  # m barely intrudes: the raw formula value falls below the lower clamp
  coords <- data.frame(
    name = c("x", "y", "m"),
    x = c(0, 2, 1),
    y = c(0, 0, 0.14)
  )
  data <- make_dag_data(coords, data.frame(name = "x", to = "y"))

  expect_lt(0.1 + 1.5 * (r - 0.14) / 2, 0.15)
  res <- auto_curve_edges(data)
  expect_equal(curvature_of(res, "x", "y"), 0.15, tolerance = 1e-8)
})

test_that("auto_curve_edges: deep intrusions on short edges clamp the magnitude to 0.6", {
  r <- node_radius_data()
  # a short edge with a node almost dead on it: the raw formula value
  # exceeds the upper clamp
  coords <- data.frame(
    name = c("x", "y", "m"),
    x = c(0, 0.4, 0.2),
    y = c(0, 0, 0.001)
  )
  data <- make_dag_data(coords, data.frame(name = "x", to = "y"))

  expect_gt(0.1 + 1.5 * (r - 0.001) / 0.4, 0.6)
  res <- auto_curve_edges(data)
  expect_equal(curvature_of(res, "x", "y"), 0.6, tolerance = 1e-8)
})

# auto_curve_edges: escalation -------------------------------------------------

test_that("auto_curve_edges: a candidate arc that still hits a node is escalated or flipped", {
  r <- node_radius_data()
  fixture <- escalation_fixture()
  data <- fixture$data

  # preconditions: b does not block the straight edge, but it sits on the
  # candidate arc's path
  expect_gt(dist_to_edge(1, -fixture$offset, 0, 0, 2, 0), r)
  expect_lt(
    min_arc_clearance(fixture$candidate, 0, 0, 2, 0, 1, -fixture$offset),
    r
  )

  res <- auto_curve_edges(data)
  curvature <- curvature_of(res, "x", "y")
  expect_false(isTRUE(all.equal(curvature, fixture$candidate)))
  # the final arc clears every non-endpoint node
  expect_gt(min_arc_clearance(curvature, 0, 0, 2, 0, 1, 0.05), r)
  expect_gt(min_arc_clearance(curvature, 0, 0, 2, 0, 1, -fixture$offset), r)
})

test_that("auto_curve_edges: escalation increases the magnitude when both sides are blocked", {
  r <- node_radius_data()
  fixture <- escalation_fixture(with_flip_blocker = TRUE)
  data <- fixture$data

  # flipping at the candidate magnitude collides with c on the other side
  expect_lt(
    min_arc_clearance(-fixture$candidate, 0, 0, 2, 0, 1, fixture$offset),
    r
  )

  res <- auto_curve_edges(data)
  curvature <- curvature_of(res, "x", "y")
  expect_gt(abs(curvature), fixture$candidate)
  for (node in c("a", "b", "c")) {
    idx <- which(fixture$coords$name == node)
    expect_gt(
      min_arc_clearance(
        curvature,
        0,
        0,
        2,
        0,
        fixture$coords$x[idx],
        fixture$coords$y[idx]
      ),
      r
    )
  }
})

# auto_curve_edges: NA-only writing and no-op shape ----------------------------

test_that("auto_curve_edges: only unset (NA) curvature is written", {
  # three vertically separated blocked edges, each with an isolated node on
  # its chord; two carry preset curvature values that must survive untouched,
  # including an explicit 0
  coords <- data.frame(
    name = c("x1", "y1", "m1", "x2", "y2", "m2", "x3", "y3", "m3"),
    x = c(0, 2, 1, 0, 2, 1, 0, 2, 1),
    y = c(0, 0, 0, 5, 5, 5, 10, 10, 10)
  )
  edges <- data.frame(name = c("x1", "x2", "x3"), to = c("y1", "y2", "y3"))
  data <- make_dag_data(coords, edges)
  data$edge_curvature <- NA_real_
  data$edge_curvature[data$name == "x1"] <- 0.9
  data$edge_curvature[data$name == "x2"] <- 0

  res <- auto_curve_edges(data)
  expect_identical(curvature_of(res, "x1", "y1"), 0.9)
  expect_identical(curvature_of(res, "x2", "y2"), 0)
  written <- curvature_of(res, "x3", "y3")
  expect_false(is.na(written))
  expect_true(written != 0)
})

test_that("auto_curve_edges: no blocked edge means the data comes back unchanged", {
  # the mediation triangle: m is a full unit from the x -> y chord
  coords <- data.frame(name = c("x", "m", "y"), x = c(0, 1, 2), y = c(0, 1, 0))
  edges <- data.frame(name = c("x", "m", "x"), to = c("m", "y", "y"))
  data <- make_dag_data(coords, edges)

  expect_identical(auto_curve_edges(data), data)

  # an existing curvature column is also left exactly as it was
  data$edge_curvature <- c(0.4, 0, 0, NA)
  expect_identical(auto_curve_edges(data), data)
})

test_that("auto_curve_edges: writing fills other directed edges with 0 and leaves the rest NA", {
  # x -> y is blocked by m; u -> v is an unblocked directed edge; p <-> q is
  # bidirected. When a curvature is written, remaining unset directed edges
  # become 0 so the scalar curvature fallback cannot curve them, while
  # bidirected and terminal rows stay NA (bidirected edges keep the arc their
  # edge layer draws them with).
  coords <- data.frame(
    name = c("x", "y", "m", "u", "v", "p", "q"),
    x = c(0, 2, 1, 0, 2, 0, 2),
    y = c(0, 0, 0, 5, 5, 10, 10)
  )
  edges <- data.frame(
    name = c("x", "u", "p"),
    to = c("y", "v", "q"),
    direction = c("->", "->", "<->")
  )
  data <- make_dag_data(coords, edges)

  res <- auto_curve_edges(data)
  blocked <- curvature_of(res, "x", "y")
  expect_false(is.na(blocked))
  expect_true(blocked != 0)
  expect_identical(curvature_of(res, "u", "v"), 0)
  expect_identical(curvature_of(res, "p", "q"), NA_real_)
  expect_true(all(is.na(res$edge_curvature[is.na(res$to)])))
})

test_that("auto_curve_edges: a bidirected edge with a node on its chord is not curved", {
  # the bidirected edge is drawn as an arc by its edge layer, not as a
  # straight chord, so it is never a candidate for the safety net
  coords <- data.frame(
    name = c("u", "v", "w"),
    x = c(0, 2, 1),
    y = c(0, 0, 0)
  )
  edges <- data.frame(name = "u", to = "v", direction = "<->")
  data <- make_dag_data(coords, edges)

  expect_identical(auto_curve_edges(data), data)
})

test_that("auto_curve_edges: deterministic and consumes no randomness", {
  data <- escalation_fixture(with_flip_blocker = TRUE)$data

  invisible(stats::runif(1))
  seed_before <- get(".Random.seed", envir = globalenv())

  first <- auto_curve_edges(data)
  second <- auto_curve_edges(data)
  expect_identical(first, second)
  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)
})

# auto_curve option ------------------------------------------------------------

test_that("auto_curve option is registered, defaults to FALSE, and round-trips", {
  local_ggdag_option_state()

  expect_true("auto_curve" %in% names(ggdag_defaults))
  expect_identical(ggdag_defaults$auto_curve, FALSE)
  expect_false(ggdag_option("auto_curve", FALSE))

  ggdag_options_set(auto_curve = TRUE)
  expect_true(ggdag_option("auto_curve", FALSE))
})

test_that("auto_curve option rejects non-logical values with a typed error", {
  local_ggdag_option_state()
  expect_true("auto_curve" %in% names(ggdag_defaults))

  expect_error(
    ggdag_options_set(auto_curve = "yes"),
    class = "ggdag_type_error"
  )
  expect_error(ggdag_options_set(auto_curve = 1), class = "ggdag_type_error")
  expect_error(ggdag_options_set(auto_curve = NA), class = "ggdag_type_error")
  expect_error(
    ggdag_options_set(auto_curve = c(TRUE, FALSE)),
    class = "ggdag_type_error"
  )
})

test_that("auto_curve option validation errors are informative", {
  local_ggdag_option_state()
  # the registration must exist before any snapshot is recorded: without it,
  # the unknown-option error would be captured in place of the validation
  # message
  stopifnot("auto_curve" %in% names(ggdag_defaults))

  expect_ggdag_error(ggdag_options_set(auto_curve = "yes"))
  expect_ggdag_error(ggdag_options_set(auto_curve = 1))
  expect_ggdag_error(ggdag_options_set(auto_curve = NA))
})

# tidy_dagitty() integration ---------------------------------------------------

test_that("auto_curve off by default: a blocked layout tidies with no curvature column", {
  local_ggdag_option_state()
  expect_true("auto_curve" %in% names(ggdag_defaults))

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
  data <- pull_dag_data(tidy_dagitty(dag))
  expect_false("edge_curvature" %in% names(data))
})

test_that("auto_curve on: tidy_dagitty() writes curvature for the blocked edge", {
  local_ggdag_option_state()
  ggdag_options_set(auto_curve = TRUE)

  r <- node_radius_data()
  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
  data <- pull_dag_data(tidy_dagitty(dag))

  expect_true("edge_curvature" %in% names(data))
  curvature <- curvature_of(data, "x", "y")
  expect_false(is.na(curvature))
  expect_gte(abs(curvature), 0.15)
  expect_lte(abs(curvature), 0.6)
  # m sits dead on the chord, so the magnitude comes straight from the formula
  # with the deepest possible intrusion; the side is left to the tie-breaking
  # rule
  expect_equal(abs(curvature), expected_magnitude(r, 2), tolerance = 1e-8)
  # the written arc clears the mediator
  expect_gt(min_arc_clearance(curvature, 0, 0, 2, 0, 1, 0), r)
  # the unblocked edges are pinned straight, and the terminal row stays NA
  expect_identical(curvature_of(data, "x", "m"), 0)
  expect_identical(curvature_of(data, "m", "y"), 0)
  expect_true(all(is.na(data$edge_curvature[is.na(data$to)])))
})

test_that("auto_curve on: an unblocked layout tidies identically to the default", {
  local_ggdag_option_state()

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )
  data_off <- pull_dag_data(tidy_dagitty(dag))

  ggdag_options_set(auto_curve = TRUE)
  data_on <- pull_dag_data(tidy_dagitty(dag))

  expect_false("edge_curvature" %in% names(data_on))
  expect_identical(data_on, data_off)
})

test_that("auto_curve on: an engine-produced time-ordered layout is a no-op", {
  local_ggdag_option_state()
  ggdag_options_set(auto_curve = TRUE)

  dag <- dagify(y ~ a, a ~ x)
  data <- pull_dag_data(
    tidy_dagitty(dag, layout = "time_ordered", use_existing_coords = FALSE)
  )

  # the engine guarantees clearance, so no straight edge is blocked and no
  # curvature is written
  coords <- unique(data.frame(name = data$name, x = data$x, y = data$y))
  expect_identical(
    count_node_edge_overlaps(coords, data, node_radius_data()),
    0L
  )
  expect_false("edge_curvature" %in% names(data))
})

test_that("auto_curve never overrides curvature from curved()", {
  local_ggdag_option_state()
  ggdag_options_set(auto_curve = TRUE)

  dag <- dagify(
    y ~ curved(x, 0.45) + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
  data <- pull_dag_data(tidy_dagitty(dag))

  expect_identical(curvature_of(data, "x", "y"), 0.45)
  # once curved() is in play, the other edges are already pinned to 0, which
  # counts as set: the safety net leaves them alone even where blocked
  expect_identical(curvature_of(data, "x", "m"), 0)
  expect_identical(curvature_of(data, "m", "y"), 0)
})

test_that("auto_curve never overrides curvature from curve_edge()", {
  local_ggdag_option_state()
  ggdag_options_set(auto_curve = TRUE)

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
  dag <- curve_edge(dag, "x", "y", -0.35)
  data <- pull_dag_data(tidy_dagitty(dag))

  expect_identical(curvature_of(data, "x", "y"), -0.35)
  expect_identical(curvature_of(data, "x", "m"), 0)
  expect_identical(curvature_of(data, "m", "y"), 0)
})

test_that("auto_curve never overrides curvature from dagitty control points", {
  local_ggdag_option_state()
  ggdag_options_set(auto_curve = TRUE)

  dag <- dagitty::dagitty(
    'dag {
      x [pos="0,0"] m [pos="1,0"] y [pos="2,0"]
      x -> m m -> y
      x -> y [pos="1.0,1.0"]
    }'
  )
  data <- pull_dag_data(tidy_dagitty(dag))

  # the control point at (1, 1) converts to curvature -0.5, and the safety
  # net must not replace it even though m blocks the straight chord
  expect_equal(curvature_of(data, "x", "y"), -0.5, tolerance = 1e-8)
  expect_identical(curvature_of(data, "x", "m"), 0)
  expect_identical(curvature_of(data, "m", "y"), 0)
})

# rendering on the ggarrow engine ----------------------------------------------

test_that("the ggarrow engine draws the curvature that auto_curve wrote", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(auto_curve = TRUE, edge_engine = "ggarrow")

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
  tidy_dag <- tidy_dagitty(dag)
  written <- curvature_of(pull_dag_data(tidy_dag), "x", "y")
  expect_false(is.na(written))

  p <- ggdag(tidy_dag)
  built <- ggplot2::ggplot_build(p)
  drawn <- NULL
  for (i in seq_along(p$layers)) {
    d <- built$data[[i]]
    if (
      inherits(p$layers[[i]]$geom, "GeomDAGArrowCurve") &&
        nrow(d) > 0 &&
        "edge_curvature" %in% names(d)
    ) {
      drawn <- d
      break
    }
  }
  expect_false(is.null(drawn))

  blocked_row <- drawn$x == 0 & drawn$xend == 2
  expect_identical(sum(blocked_row), 1L)
  expect_equal(drawn$edge_curvature[blocked_row], written, tolerance = 1e-8)
  # the incident edges through m are drawn straight
  expect_true(all(drawn$edge_curvature[!blocked_row] == 0))
})

test_that("vdiffr: auto curve routes an edge around a mediator", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(auto_curve = TRUE, edge_engine = "ggarrow")

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
  )
  tidy_dag <- tidy_dagitty(dag)
  curvature <- curvature_of(pull_dag_data(tidy_dag), "x", "y")
  # the safety net must have written a curvature before a baseline is
  # recorded: a snapshot of the unrouted plot would pin the wrong picture
  stopifnot(!is.na(curvature), curvature != 0)

  expect_doppelganger(
    "auto curve routes edge around mediator",
    ggdag(tidy_dag)
  )
})

test_that("vdiffr: auto curve routes several blocked edges at once", {
  skip_if_not_installed("ggarrow")
  local_ggdag_option_state()
  ggdag_options_set(auto_curve = TRUE, edge_engine = "ggarrow")

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    v ~ u + n,
    n ~ u,
    coords = list(
      x = c(x = 0, m = 1, y = 2, u = 0, n = 1, v = 2),
      y = c(x = 0, m = 0, y = 0, u = 1, n = 1, v = 1)
    )
  )
  tidy_dag <- tidy_dagitty(dag)
  data <- pull_dag_data(tidy_dag)
  blocked <- c(curvature_of(data, "x", "y"), curvature_of(data, "u", "v"))
  stopifnot(!anyNA(blocked), all(blocked != 0))

  expect_doppelganger(
    "auto curve routes multiple blocked edges",
    ggdag(tidy_dag)
  )
})
