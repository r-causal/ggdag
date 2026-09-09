# Tests for wiring the ordering and coordinate upgrades into
# compute_time_ordered_layout(): exact layer ordering via order_layers(),
# median-based initialization, isotropic normalization, node_scale-derived
# clearance constants, curvature-aware bidirected overlap correction, the
# symmetry override in the dual-initialization guard, the manual-tiers
# optimization guard, and arc-aware clearance for spanning directed edges
# under the arc edge type.

# Helpers ----------------------------------------------------------------------

# Parse "a->b" strings into an edge data frame shaped like the engine's real
# input: one row per directed edge plus a to = NA row per terminal node.
wiring_edges <- function(edge_strings) {
  parts <- strsplit(edge_strings, "->", fixed = TRUE)
  edges <- data.frame(
    name = vapply(parts, function(p) p[[1]], character(1)),
    to = vapply(parts, function(p) p[[2]], character(1)),
    stringsAsFactors = FALSE
  )
  terminals <- setdiff(unique(edges$to), edges$name)
  if (length(terminals) > 0) {
    edges <- rbind(
      edges,
      data.frame(name = terminals, to = NA_character_, stringsAsFactors = FALSE)
    )
  }
  edges
}

# Sorted consecutive y differences among the named nodes, which share a layer.
sorted_y_gaps <- function(coords, nodes) {
  diff(sort(coords$y[coords$name %in% nodes]))
}

# One coordinate row per node, rounded to the three decimals that
# `dagitty::coordinates<-` stores, so a layout that round-trips through the
# dagitty object compares against one that keeps full precision.
layout_node_coords <- function(.tdy_dag) {
  pull_dag_data(.tdy_dag) |>
    dplyr::distinct(name, x, y) |>
    dplyr::arrange(name) |>
    dplyr::mutate(
      x = round(as.numeric(x), digits = 3),
      y = round(as.numeric(y), digits = 3)
    )
}

# The 22 canonical DAGs from the layout design spec (scratch/time_order.md),
# with per-DAG budgets for straight-line edge crossings in the final layout.
# "current" records what the engine produces before the ordering and
# initialization upgrades; a budget below current pins the improvement the
# upgrades must deliver.
#
# dag               current  budget
# confounding             0       0
# mediation               0       0
# collider                0       0
# iv                      0       0
# front_door              0       0
# m_bias                  1       1
# smoking                 0       0
# epidemiology            1       1
# selection_bias          1       1
# overcontrol             1       1   (symmetric arch keeps one crossing)
# napkin                  0       0
# butterfly               0       0
# complex_chain           0       0
# wide_dag                3       3
# deep_confound           3       0   (strict improvement)
# large_epi              14      12   (strict improvement)
# treatment               4       2   (strict improvement)
# double_iv               0       0
# cascade                 3       3
# triple_confound         4       4
# regression_disc         0       0
# multi_mediator          6       5   (strict improvement)
# The specs themselves come from helper-canonical-dags.R, so the budgets here
# and the layout fixtures always describe the same DAGs.
canonical_wiring_budgets <- c(
  confounding = 0L,
  mediation = 0L,
  collider = 0L,
  iv = 0L,
  front_door = 0L,
  m_bias = 1L,
  smoking = 0L,
  epidemiology = 1L,
  selection_bias = 1L,
  # The symmetry override keeps the even-spacing arch on `overcontrol`, which
  # trades one crossing for mirror symmetry; see the dual-initialization
  # symmetry override tests below.
  overcontrol = 1L,
  napkin = 0L,
  butterfly = 0L,
  complex_chain = 0L,
  wide_dag = 3L,
  deep_confound = 0L,
  large_epi = 12L,
  treatment = 2L,
  double_iv = 0L,
  cascade = 3L,
  triple_confound = 4L,
  regression_disc = 0L,
  multi_mediator = 5L
)

canonical_wiring_dags <- purrr::imap(
  canonical_wiring_budgets,
  \(budget, nm) list(spec = canonical_dag_specs[[nm]], budget = budget)
)

# Crossing gates ---------------------------------------------------------------

test_that("canonical DAGs stay within their crossing budgets", {
  for (nm in names(canonical_wiring_dags)) {
    edges <- wiring_edges(canonical_wiring_dags[[nm]]$spec)
    coords <- compute_time_ordered_layout(edges)
    expect_lte(
      count_edge_crossings(coords, edges),
      canonical_wiring_dags[[nm]]$budget,
      label = paste0(nm, ": edge crossings"),
      expected.label = paste0(nm, ": crossing budget")
    )
  }
})

# Overlap gates ----------------------------------------------------------------

test_that("canonical DAGs draw with zero node-edge overlaps", {
  for (nm in names(canonical_wiring_dags)) {
    edges <- wiring_edges(canonical_wiring_dags[[nm]]$spec)
    coords <- compute_time_ordered_layout(edges)
    expect_identical(
      count_node_edge_overlaps(coords, edges, node_radius_data()),
      0L,
      label = paste0(nm, ": node-edge overlaps")
    )
  }
})

# Isotropy ---------------------------------------------------------------------

test_that("even-spacing path normalizes y on the same scale as x", {
  # Two roots one node_gap (85) apart internally, layers one layer_gap (180)
  # apart. One uniform scale maps both, so the data-space gap is 85 / 180 of
  # the unit x layer gap.
  edges <- wiring_edges(c("a->c", "b->c"))
  coords <- compute_time_ordered_layout(edges, force_y = FALSE)

  expect_equal(
    sorted_y_gaps(coords, c("a", "b")),
    85 / 180,
    tolerance = 1e-6
  )
})

test_that("solved same-layer clearances survive into data space", {
  # Five roots pulled together by one shared child compress to exactly
  # min_spacing (72) internally; the isotropic scale carries that through as
  # 72 / 180 of the unit x layer gap.
  fan <- wiring_edges(c("a->y", "b->y", "c->y", "d->y", "e->y"))
  coords <- compute_time_ordered_layout(fan)

  expect_equal(
    sorted_y_gaps(coords, c("a", "b", "c", "d", "e")),
    rep(72 / 180, 4),
    tolerance = 1e-6
  )
})

test_that("all-singleton layers use the same uniform scale", {
  # Every layer holds a single node, so there is no same-layer gap to measure.
  # The uniform scale still divides y by the layer gap, so the solved
  # clearance of y from the skip edge x -> m sits well under half a layer;
  # the old fallback rescaled by min_spacing and produced roughly twice that.
  edges <- wiring_edges(c("x->m", "x->y", "y->m"))
  coords <- compute_time_ordered_layout(edges)

  clearance <- dist_to_edge(
    coords$x[coords$name == "y"],
    coords$y[coords$name == "y"],
    coords$x[coords$name == "x"],
    coords$y[coords$name == "x"],
    coords$x[coords$name == "m"],
    coords$y[coords$name == "m"]
  )
  expect_gte(clearance, node_radius_data())
  expect_lt(clearance, 0.5)
})

# node_scale -------------------------------------------------------------------

test_that("node_scale = 2 widens same-layer spacing monotonically", {
  fan <- wiring_edges(c("a->y", "b->y", "c->y", "d->y", "e->y"))
  roots <- c("a", "b", "c", "d", "e")

  base <- compute_time_ordered_layout(fan, node_scale = 1)
  scaled <- compute_time_ordered_layout(fan, node_scale = 2)

  expect_gt(
    min(sorted_y_gaps(scaled, roots)),
    min(sorted_y_gaps(base, roots))
  )
})

test_that("explicit legacy arguments override node_scale", {
  fan <- wiring_edges(c("a->y", "b->y", "c->y", "d->y", "e->y"))
  roots <- c("a", "b", "c", "d", "e")

  scaled <- compute_time_ordered_layout(fan, node_scale = 2)
  overridden <- compute_time_ordered_layout(
    fan,
    node_scale = 2,
    min_spacing = 72
  )

  expect_lt(
    min(sorted_y_gaps(overridden, roots)),
    min(sorted_y_gaps(scaled, roots))
  )
})

test_that("node_scale = 1 is the default", {
  fan <- wiring_edges(c("a->y", "b->y", "c->y", "d->y", "e->y"))
  expect_identical(
    compute_time_ordered_layout(fan),
    compute_time_ordered_layout(fan, node_scale = 1)
  )
})

test_that("the node_size option scales time-ordered spacing", {
  local_ggdag_option_state()
  dag <- dagify(y ~ a + b + c + d + e)
  roots <- c("a", "b", "c", "d", "e")

  min_root_gap <- function(.tdy_dag) {
    dag_data <- dplyr::distinct(pull_dag_data(.tdy_dag), name, x, y)
    min(sorted_y_gaps(dag_data, roots))
  }

  default_gap <- min_root_gap(tidy_dagitty(dag, layout = "time_ordered"))

  withr::local_options(list(ggdag.node_size = 32))
  expect_gt(
    min_root_gap(tidy_dagitty(dag, layout = "time_ordered")),
    default_gap
  )
  expect_gt(
    min_root_gap(tidy_dagitty(dag, layout = time_ordered_coords())),
    default_gap
  )
})

test_that("a dplyr-verb rebuild reproduces the direct time-ordered layout", {
  local_ggdag_option_state()
  withr::local_options(list(ggdag.layout = "time_ordered"))
  dag <- dagify(y ~ a + b + c + d + e)

  direct <- tidy_dagitty(dag, layout = "time_ordered")
  rebuilt <- dplyr::select(direct, -x, -y, -xend, -yend)

  expect_equal(layout_node_coords(rebuilt), layout_node_coords(direct))
})

test_that("the node_size option survives a dplyr-verb layout rebuild", {
  local_ggdag_option_state()
  withr::local_options(list(
    ggdag.layout = "time_ordered",
    ggdag.node_size = 24
  ))
  dag <- dagify(y ~ a + b + c + d + e)

  direct <- tidy_dagitty(dag, layout = "time_ordered")
  rebuilt <- dplyr::select(direct, -x, -y, -xend, -yend)

  expect_equal(layout_node_coords(rebuilt), layout_node_coords(direct))
})

test_that("exposure and outcome survive a dplyr-verb layout rebuild", {
  # The exposure and the outcome start on the same layer here, so the direct
  # path's layer-shift adjustment moves the outcome one layer later. A rebuild
  # that drops the exposure and outcome skips the shift and draws them at the
  # same time point.
  local_ggdag_option_state()
  withr::local_options(list(ggdag.layout = "time_ordered"))
  dag <- dagify(x ~ z, y ~ z, exposure = "x", outcome = "y")

  direct <- tidy_dagitty(dag, layout = "time_ordered")
  rebuilt <- dplyr::select(direct, -x, -y, -xend, -yend)

  expect_equal(layout_node_coords(rebuilt), layout_node_coords(direct))
})

test_that("exposure and outcome survive a function-layout rebuild", {
  # The closure from time_ordered_coords() accepts `...`, so the direct path
  # hands it the DAG's exposure and outcome and the same layer-shift applies.
  # A rebuild that calls the closure bare loses that awareness.
  local_ggdag_option_state()
  layout_fn <- time_ordered_coords()
  withr::local_options(list(ggdag.layout = layout_fn))
  dag <- dagify(x ~ z, y ~ z, exposure = "x", outcome = "y")

  direct <- tidy_dagitty(dag, layout = layout_fn)
  rebuilt <- dplyr::select(direct, -x, -y, -xend, -yend)

  expect_equal(layout_node_coords(rebuilt), layout_node_coords(direct))
})

test_that("an isolated node survives a dplyr-verb layout rebuild", {
  # The direct path builds the engine's input by appending isolated nodes
  # after the edge and terminal rows. The rebuild path feeds the tidy data in
  # its stored row order, which follows dagitty's code-unit-sorted vertex
  # ordering (alphabetical for ASCII names), so an isolated node that sorts
  # before its layer-mates enters the engine first. The engine seeds
  # within-layer order from first appearance, so the whole layer ends up on
  # different y coordinates. Without the isolated node the two paths agree,
  # so any mismatch here comes from isolated-node handling alone. The DAG is
  # written as a dagitty string because dagify() has no way to declare a node
  # with no edges.
  local_ggdag_option_state()
  withr::local_options(list(ggdag.layout = "time_ordered"))
  dag <- dagitty::dagitty("dag{u -> x; u -> y; iso}")

  direct <- tidy_dagitty(dag, layout = "time_ordered")
  rebuilt <- dplyr::select(direct, -x, -y, -xend, -yend)

  expect_equal(layout_node_coords(rebuilt), layout_node_coords(direct))
})

# Determinism ------------------------------------------------------------------

test_that("every entry path is deterministic and leaves the RNG alone", {
  napkin_edges <- wiring_edges(canonical_wiring_dags$napkin$spec)
  napkin_dag <- dagify(
    a ~ u1 + u2 + z,
    z ~ u1,
    m ~ a,
    y ~ u2 + m + a
  )

  if (!exists(".Random.seed", envir = globalenv())) {
    set.seed(1)
  }
  seed_before <- get(".Random.seed", envir = globalenv())

  expect_identical(
    compute_time_ordered_layout(napkin_edges),
    compute_time_ordered_layout(napkin_edges)
  )

  td_string <- tidy_dagitty(napkin_dag, layout = "time_ordered")
  td_string2 <- tidy_dagitty(napkin_dag, layout = "time_ordered")
  expect_identical(pull_dag_data(td_string), pull_dag_data(td_string2))

  td_closure <- tidy_dagitty(napkin_dag, layout = time_ordered_coords())
  td_closure2 <- tidy_dagitty(napkin_dag, layout = time_ordered_coords())
  expect_identical(pull_dag_data(td_closure), pull_dag_data(td_closure2))
  expect_identical(pull_dag_data(td_string), pull_dag_data(td_closure))

  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)
})

# Initialization and convergence -----------------------------------------------

test_that("force_directed_y clears the napkin at full and reduced budgets", {
  edges <- wiring_edges(canonical_wiring_dags$napkin$spec)
  directed <- edges[!is.na(edges$to), , drop = FALSE]
  layer_assign <- longest_path_layers(edges)
  layer_nodes <- lapply(seq(0L, max(layer_assign)), function(l) {
    names(layer_assign)[layer_assign == l]
  })
  ordered <- order_layers(layer_nodes, directed, layer_assign)$layer_nodes

  full <- force_directed_y(ordered, layer_assign, directed)
  expect_identical(nrow(find_overlaps(full, directed, layer_assign)), 0L)

  reduced <- force_directed_y(
    ordered,
    layer_assign,
    directed,
    iterations = 60L
  )
  expect_identical(nrow(find_overlaps(reduced, directed, layer_assign)), 0L)
})

test_that("reduced iterations still yield an overlap-free napkin layout", {
  edges <- wiring_edges(canonical_wiring_dags$napkin$spec)
  coords <- compute_time_ordered_layout(edges, iterations = 60L)
  expect_identical(
    count_node_edge_overlaps(coords, edges, node_radius_data()),
    0L
  )
})

# Performance ------------------------------------------------------------------

test_that("the largest canonical DAG lays out in under a second", {
  skip_on_cran()
  skip_on_ci()

  # Lax regression tripwire only. The strict interactive-speed criterion
  # (under 100 ms for the wired pipeline, under 40 ms for order_layers()) is
  # enforced separately from this tripwire.
  edges <- wiring_edges(canonical_wiring_dags$large_epi$spec)
  times <- replicate(
    5,
    system.time(compute_time_ordered_layout(edges))[["elapsed"]]
  )
  expect_lt(median(times), 1.0)
})

# Bidirected edges -------------------------------------------------------------

test_that("a free bidirected pair ends adjacent in its layer", {
  # x and y share a layer with m; no ordering costs a crossing, so the
  # bidirected adjacency nudge must put x and y next to each other, and the
  # drawn arcs must clear every node.
  edges <- data.frame(
    name = c("a", "a", "a", "x"),
    to = c("x", "m", "y", "y"),
    direction = c("->", "->", "->", "<->"),
    stringsAsFactors = FALSE
  )
  coords <- compute_time_ordered_layout(edges)

  second_layer <- coords[coords$x == 2, , drop = FALSE]
  ordering <- second_layer$name[order(second_layer$y)]
  expect_identical(abs(match("x", ordering) - match("y", ordering)), 1L)

  curvature <- ifelse(
    !is.na(edges$direction) & edges$direction == "<->",
    0.3,
    0
  )
  expect_identical(
    count_node_edge_overlaps(
      coords,
      edges,
      node_radius_data(),
      curvature = curvature
    ),
    0L
  )
})

test_that("greedy_post_correction clears bidirected arcs when asked", {
  # x <-> y spans two layers, and traced at 0.3 its arc bows below the chord.
  # z sits right on that arc: invisible to the straight-line check, but a
  # real overlap once the arc is traced. The traced side is passed in, since
  # which side the drawn arc falls on is the caller's to resolve from the
  # edge engine.
  edges <- data.frame(
    name = c("x", "z", "x"),
    to = c("z", "y", "y"),
    direction = c("->", "->", "<->"),
    stringsAsFactors = FALSE
  )
  positions <- list(
    x = c(x = 0, z = 180, y = 360),
    y = c(x = 0, z = -90, y = 0)
  )
  layer_assign <- c(x = 0L, z = 1L, y = 2L)
  curvature <- c(0, 0, 0.3)

  as_coords <- function(positions) {
    data.frame(
      name = names(positions$x),
      x = unname(positions$x),
      y = unname(positions$y),
      stringsAsFactors = FALSE
    )
  }

  # the fixture really does clip the arc but not the chord
  expect_identical(
    count_node_edge_overlaps(as_coords(positions), edges, 26),
    0L
  )
  expect_identical(
    count_node_edge_overlaps(
      as_coords(positions),
      edges,
      26,
      curvature = curvature
    ),
    1L
  )

  corrected <- greedy_post_correction(
    positions,
    edges,
    layer_assign,
    check_bidirected = TRUE,
    trace_curvature = 0.3
  )
  expect_identical(
    count_node_edge_overlaps(
      as_coords(corrected),
      edges,
      26,
      curvature = curvature
    ),
    0L
  )
})

test_that("the layout clears bidirected arcs at the curvature option", {
  local_ggdag_option_state()
  withr::local_options(list(ggdag.curvature = 0.15))

  # x <-> y spans two layers and its drawn arc follows the curvature option,
  # so the engine must model the arc at that curvature when it clears nodes.
  # At 0.15 the arc passes between the chord and the deeper 0.3 bow, exactly
  # where a middle-layer node settles when the engine clears only the 0.3
  # arc: an engine that hardcodes 0.3 leaves m1 clipping the drawn arc. The
  # default edge engine draws the arc on the left of travel, which
  # sample_curved_edge() traces at the negated curvature.
  edges <- data.frame(
    name = c("x", "x", "m1", "m2", "x"),
    to = c("m1", "m2", "y", "y", "y"),
    direction = c("->", "->", "->", "->", "<->"),
    stringsAsFactors = FALSE
  )
  coords <- compute_time_ordered_layout(edges)

  curvature <- ifelse(edges$direction == "<->", -0.15, 0)
  expect_identical(
    count_node_edge_overlaps(
      coords,
      edges,
      node_radius_data(),
      curvature = curvature
    ),
    0L
  )
})

test_that("better_positions scores bidirected rows as drawn arcs", {
  local_ggdag_option_state()
  withr::local_options(list(ggdag.curvature = 0.3))

  # One bidirected edge u <-> v plus an isolated node w. The candidates tie
  # on every straight-line criterion: no crossings, w clears the chord in
  # both, and the stress term skips the disconnected w. Only the traced arc
  # separates them: arc_hugging parks w on the 0.3 arc below the chord,
  # arc_clear keeps w far from it, so arc-aware scoring must choose
  # arc_clear.
  edges <- data.frame(
    name = c("u", "w"),
    to = c("v", NA),
    direction = c("<->", NA),
    stringsAsFactors = FALSE
  )
  arc_hugging <- list(
    x = c(u = 0, v = 360, w = 180),
    y = c(u = 0, v = 0, w = -90)
  )
  arc_clear <- list(
    x = c(u = 0, v = 360, w = 180),
    y = c(u = 0, v = 0, w = 200)
  )

  as_coords <- function(positions) {
    data.frame(
      name = names(positions$x),
      x = unname(positions$x),
      y = unname(positions$y),
      stringsAsFactors = FALSE
    )
  }
  curvature <- ifelse(
    !is.na(edges$direction) & edges$direction == "<->",
    0.3,
    0
  )

  # the fixture really does tie on straight-line overlaps and differ on the
  # arc
  expect_identical(
    count_node_edge_overlaps(as_coords(arc_hugging), edges, 26),
    0L
  )
  expect_identical(
    count_node_edge_overlaps(as_coords(arc_clear), edges, 26),
    0L
  )
  expect_identical(
    count_node_edge_overlaps(
      as_coords(arc_hugging),
      edges,
      26,
      curvature = curvature
    ),
    1L
  )
  expect_identical(
    count_node_edge_overlaps(
      as_coords(arc_clear),
      edges,
      26,
      curvature = curvature
    ),
    0L
  )

  expect_identical(
    better_positions(arc_hugging, arc_clear, edges, 26, trace_curvature = 0.3),
    arc_clear
  )
})

# Regression guards ------------------------------------------------------------

test_that("fixed_time pins survive the wiring", {
  edges <- wiring_edges(c("x->m", "m->y", "x->y"))
  coords <- compute_time_ordered_layout(edges, fixed_time = c(m = 3))

  expect_equal(coords$x[coords$name == "m"], 3)
  expect_lt(coords$x[coords$name == "x"], 3)
  expect_gt(coords$x[coords$name == "y"], 3)
})

test_that("exposure and outcome sharing a layer are still separated", {
  edges <- wiring_edges(c("z->x", "z->y"))
  coords <- compute_time_ordered_layout(
    edges,
    exposure = "x",
    outcome = "y"
  )

  expect_lt(
    coords$x[coords$name == "x"],
    coords$x[coords$name == "y"]
  )
})

test_that("sort_direction still chooses the layering side", {
  edges <- wiring_edges(c("c->b", "b->z", "x0->x1", "x1->x2", "x2->z"))

  left <- compute_time_ordered_layout(edges, sort_direction = "left")
  right <- compute_time_ordered_layout(edges, sort_direction = "right")

  expect_lt(left$x[left$name == "b"], right$x[right$name == "b"])
  expect_lt(left$x[left$name == "c"], left$x[left$name == "b"])
  expect_lt(right$x[right$name == "b"], right$x[right$name == "z"])
})

test_that("barycenter_sort keeps the incumbent order on ties", {
  # one parent connected to every child: all barycenters tie, so the input
  # order must come back untouched from both sweep directions
  layer_nodes <- list("a", c("q", "p", "z"))
  edges <- data.frame(
    name = c("a", "a", "a"),
    to = c("q", "p", "z"),
    stringsAsFactors = FALSE
  )
  out <- barycenter_sort(layer_nodes, edges)
  expect_identical(out[[2]], c("q", "p", "z"))
})

# Dual-initialization symmetry override ----------------------------------------

# TRUE when reflecting the layout about its mid-x maps the node set onto
# itself: every node has a partner (possibly itself) at the mirrored x with
# the same y. The tolerance is one internal pixel expressed in data units,
# where the 180-pixel layer gap maps to one x unit.
coords_mirror_symmetric <- function(coords, tol = 1 / 180) {
  mirrored_x <- max(coords$x) + min(coords$x) - coords$x
  used <- rep(FALSE, nrow(coords))
  for (i in seq_len(nrow(coords))) {
    j <- which(
      !used &
        abs(coords$x - mirrored_x[i]) < tol &
        abs(coords$y - coords$y[i]) < tol
    )
    if (length(j) == 0) {
      return(FALSE)
    }
    used[j[1]] <- TRUE
  }
  TRUE
}

test_that("the multi-mediator chain lays out as a symmetric arch", {
  # A chain of three mediators between x and y is a mirror-symmetric DAG,
  # and the even-spacing candidate solves it as a symmetric arch: x and y
  # together at the lowest height, m1 and m3 level with each other, m2 at
  # the apex. The median candidate removes two more crossings but breaks the
  # arch into a staircase, so the never-worse guard keeps the even candidate
  # whenever it is mirror-symmetric, has no node-edge overlaps, and the
  # median candidate wins on crossings by no more than two. The expected
  # coordinates are the even-spacing candidate's, computed with the engine's
  # default geometry; both layering directions solve the same arch.
  dag <- dagify(m1 ~ x, m2 ~ x + m1, m3 ~ x + m2, y ~ x + m1 + m2 + m3)
  edges <- as.data.frame(get_dagitty_edges(dag))

  for (sort_direction in c("right", "left")) {
    coords <- compute_time_ordered_layout(
      edges,
      sort_direction = sort_direction
    )
    y_of <- function(nm) coords$y[coords$name == nm]

    expect_equal(
      y_of("x"),
      y_of("y"),
      tolerance = 1e-6,
      label = paste0(sort_direction, ": y(x)"),
      expected.label = paste0(sort_direction, ": y(y)")
    )
    expect_equal(
      y_of("m1"),
      y_of("m3"),
      tolerance = 1e-6,
      label = paste0(sort_direction, ": y(m1)"),
      expected.label = paste0(sort_direction, ": y(m3)")
    )
    expect_equal(
      y_of("x"),
      min(coords$y),
      tolerance = 1e-6,
      label = paste0(sort_direction, ": y(x)"),
      expected.label = paste0(sort_direction, ": lowest y")
    )
    expect_gt(
      y_of("m2"),
      y_of("m1"),
      label = paste0(sort_direction, ": y(m2)"),
      expected.label = paste0(sort_direction, ": y(m1)")
    )
    expect_true(
      coords_mirror_symmetric(coords),
      label = paste0(sort_direction, ": mirror symmetry")
    )

    expect_equal(
      dplyr::arrange(coords, name),
      tibble::tibble(
        name = c("m1", "m2", "m3", "x", "y"),
        x = c(2, 3, 4, 1, 5),
        y = c(0.30405713, 0.39622736, 0.30405713, -0.50217081, -0.50217081)
      ),
      tolerance = 1e-6,
      label = paste0(sort_direction, ": layout"),
      expected.label = paste0(sort_direction, ": symmetric arch")
    )
  }
})

test_that("the symmetry override leaves the unaffected corpus untouched", {
  # The override keeps the even-spacing candidate only when that candidate
  # is mirror-symmetric, free of node-edge overlaps, and behind the median
  # candidate by one or two straight-line crossings. Each of these DAGs
  # misses at least one condition: an asymmetric even candidate (smoking,
  # epidemiology, napkin, large_epi, multi_mediator), a crossing gap larger
  # than two (deep_confound), or a median win on the stress tiebreak alone
  # with no crossing advantage (cascade). Their layouts stay exactly as the
  # invariance fixture pins them.
  fixture <- readRDS(test_path("fixtures", "layout-invariance.rds"))
  unaffected <- c(
    "smoking",
    "epidemiology",
    "napkin",
    "deep_confound",
    "large_epi",
    "cascade",
    "multi_mediator"
  )

  for (nm in unaffected) {
    edges <- canonical_dag_edges(canonical_dag_specs[[nm]])
    expect_identical(
      compute_time_ordered_layout(edges),
      fixture[[nm]]$coords,
      label = paste0(nm, ": layout"),
      expected.label = paste0(nm, ": pinned coordinate fixture")
    )
  }
})

test_that("overcontrol keeps the symmetric even-spacing layout", {
  # The even-spacing candidate here is mirror-symmetric and overlap-free
  # with a single crossing; the median candidate clears that crossing but
  # staggers the nodes. The symmetry override keeps the even candidate: w
  # and y sit low, x and z sit high, mirrored about the center. The expected
  # coordinates are the even candidate's, computed with the engine's default
  # geometry. The invariance fixture entry for overcontrol pins this same
  # layout.
  edges <- canonical_dag_edges(canonical_dag_specs$overcontrol)
  coords <- compute_time_ordered_layout(edges)

  expect_true(coords_mirror_symmetric(coords))
  expect_equal(
    dplyr::arrange(coords, name),
    tibble::tibble(
      name = c("w", "x", "y", "z"),
      x = c(1, 2, 4, 3),
      y = c(-0.32002665, 0.32002665, -0.32002665, 0.32002665)
    ),
    tolerance = 1e-6
  )
})

# Manual-tiers optimization guard -----------------------------------------------

test_that("manual tiers keep the user's grid when optimizing gains nothing", {
  # With every node's tier given, the spread grid is the user's own
  # arrangement: tiers on the time axis, tier-mates spread in the order they
  # were listed. The optimizer may replace that grid only when it strictly
  # improves straight-line crossings, or ties crossings and strictly
  # improves node-edge overlaps. This grid is already crossing-free and
  # overlap-free, so nothing can improve on it and it comes back exactly as
  # optimize = FALSE returns it, with z1, z2, and z3 in their listed order.
  time_df <- data.frame(
    name = c("x1", "x2", "y", "z1", "z2", "z3", "a"),
    time = c(1, 1, 2, 3, 3, 3, 4)
  )
  dag <- dagify(z3 ~ y, y ~ x1 + x2, a ~ z1 + z2 + z3)
  edges <- as.data.frame(get_dagitty_edges(dag))
  grid <- time_ordered_coords(time_df, optimize = FALSE)

  by_name <- function(coords) {
    dplyr::arrange(tibble::as_tibble(coords)[c("name", "x", "y")], name)
  }

  coords <- compute_time_ordered_layout(
    edges,
    fixed_layers = c(x1 = 1, x2 = 1, y = 2, z1 = 3, z2 = 3, z3 = 3, a = 4),
    time_points = 1:4
  )
  # the grid the layout function hands back also records the axis time runs
  # along, which the engine's own coordinates do not carry
  expect_equal(
    by_name(coords),
    by_name(grid),
    ignore_attr = "layout_direction"
  )

  z_heights <- coords$y[match(c("z1", "z2", "z3"), coords$name)]
  expect_lt(z_heights[1], z_heights[2])
  expect_lt(z_heights[2], z_heights[3])

  # the user-facing layout closure hands back the same grid; the layout
  # attributes the tidy data carries are not part of the contract
  td <- tidy_dagitty(dag, layout = time_ordered_coords(time_df))
  expect_equal(
    layout_node_coords(td),
    dplyr::mutate(
      by_name(grid),
      x = round(as.numeric(x), digits = 3),
      y = round(as.numeric(y), digits = 3)
    ),
    ignore_attr = TRUE
  )
})

test_that("manual tiers keep the optimized layout when it removes crossings", {
  # In the user's listed order the straight edges of this DAG cross once;
  # the optimizer unwinds that crossing, a strict improvement, so its layout
  # is kept in place of the spread grid. The expected coordinates pin the
  # optimized layout as the engine solves it today.
  dag <- dagify(d ~ c1 + c2 + c3, c1 ~ b1 + b2, c3 ~ a, b1 ~ a)
  edges <- as.data.frame(get_dagitty_edges(dag))
  grid <- time_ordered_coords(
    list("a", c("b1", "b2"), c("c1", "c2", "c3"), "d"),
    optimize = FALSE
  )

  coords <- compute_time_ordered_layout(
    edges,
    fixed_layers = c(a = 1, b1 = 2, b2 = 2, c1 = 3, c2 = 3, c3 = 3, d = 4),
    time_points = 1:4
  )

  expect_identical(count_edge_crossings(grid, edges), 1L)
  expect_identical(count_edge_crossings(coords, edges), 0L)
  expect_equal(
    dplyr::arrange(coords, name),
    tibble::tibble(
      name = c("a", "b1", "b2", "c1", "c2", "c3", "d"),
      x = c(1, 2, 2, 3, 3, 3, 4),
      y = c(
        0.08377394,
        -0.45623187,
        -0.05623187,
        -0.29137027,
        0.10862973,
        0.50862973,
        0.10280062
      )
    ),
    tolerance = 1e-6
  )
})

test_that("manual tiers still optimize when reordering removes a crossing", {
  # a and b share the first tier and their children arrive swapped: in the
  # listed order the straight edges a -> d and b -> c cross, and exchanging
  # c and d clears the crossing. That strict improvement keeps the optimized
  # layout, which pairs each parent with its child on the same side.
  dag <- dagify(d ~ a, c ~ b)
  edges <- as.data.frame(get_dagitty_edges(dag))
  grid <- time_ordered_coords(list(c("a", "b"), c("c", "d")), optimize = FALSE)

  coords <- compute_time_ordered_layout(
    edges,
    fixed_layers = c(a = 1, b = 1, c = 2, d = 2),
    time_points = 1:2
  )

  expect_identical(count_edge_crossings(grid, edges), 1L)
  expect_identical(count_edge_crossings(coords, edges), 0L)

  y_of <- function(nm) coords$y[coords$name == nm]
  expect_equal(y_of("a"), y_of("d"), tolerance = 1e-6)
  expect_equal(y_of("b"), y_of("c"), tolerance = 1e-6)
})

# Arc-aware spanning edges ------------------------------------------------------

test_that("ggraph arc edges draw positive curvature on the left of travel", {
  # sample_curved_edge() offsets its through-point along the right normal of
  # travel, while the ggraph arc edge geom draws positive curvature on the
  # left, so tracing an arc it draws means negating the curvature handed to
  # sample_curved_edge(). This pins the two conventions against each other:
  # any code that traces a ggraph arc must flip the sign, and a silent
  # inversion in either convention fails here. Only the side is at issue;
  # the depth the two engines reach is engine_trace_curvature()'"'"'s to settle.
  edge <- data.frame(
    name = "a",
    x = 0,
    y = 0,
    xend = 2,
    yend = 0,
    direction = factor("->", levels = c("->", "<->"))
  )
  p <- ggplot(edge, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges_arc(curvature = 0.3)
  drawn <- ggplot2::ggplot_build(p)$data[[1]]
  drawn_mid <- drawn$y[which.min(abs(drawn$x - 1))]

  traced <- sample_curved_edge(0, 0, 2, 0, 0.3)
  flipped <- sample_curved_edge(0, 0, 2, 0, -0.3)

  # left of rightward travel is up, so the drawn arc bows above the chord
  expect_gt(drawn_mid, 0)
  # the same positive curvature traces below the chord ...
  expect_lt(traced$y[which.min(abs(traced$x - 1))], 0)
  # ... and only the negated curvature traces the drawn side
  expect_gt(flipped$y[which.min(abs(flipped$x - 1))], 0)

  # reversing travel flips the drawn side
  edge_rev <- data.frame(
    name = "b",
    x = 2,
    y = 0,
    xend = 0,
    yend = 0,
    direction = factor("->", levels = c("->", "<->"))
  )
  p_rev <- ggplot(
    edge_rev,
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
  ) +
    geom_dag_edges_arc(curvature = 0.3)
  drawn_rev <- ggplot2::ggplot_build(p_rev)$data[[1]]
  expect_lt(drawn_rev$y[which.min(abs(drawn_rev$x - 1))], 0)
})

# Minimum distance from a node's center to the drawn arc of a directed edge,
# traced the way the ggraph arc edge geom draws it: on the far side of
# travel, and only as deep as that geom bows. Coordinates are in data units.
drawn_arc_clearance <- function(coords, from, to, node, curvature) {
  at <- function(nm, col) coords[[col]][coords$name == nm]
  arc <- sample_curved_edge(
    at(from, "x"),
    at(from, "y"),
    at(to, "x"),
    at(to, "y"),
    engine_trace_curvature(curvature, "ggraph")
  )
  min(sqrt((at(node, "x") - arc$x)^2 + (at(node, "y") - arc$y)^2))
}

test_that("the arc edge type clears the collider apex at node_size 24", {
  local_ggdag_option_state()
  withr::local_options(list(
    ggdag.layout = "time_ordered",
    ggdag.node_size = 24,
    ggdag.edge_type = "arc"
  ))

  # x -> m spans two layers, and under the arc edge type it is drawn as an
  # arc bowing to the left of travel, right where y settles when only the
  # straight chord is checked. The layout must clear the arc as drawn. The
  # clearance floor is the engine's own overlap threshold, node radius plus
  # 8 internal pixels, mapped to data units by the 180-pixel layer gap.
  td <- tidy_dagitty(dagify(m ~ x + y, y ~ x))
  coords <- dplyr::distinct(pull_dag_data(td), name, x, y)

  clearance <- drawn_arc_clearance(
    coords,
    "x",
    "m",
    "y",
    ggdag_option("curvature", 0.3)
  )
  node_scale <- 24 / 16
  expect_gte(clearance, (26 * node_scale + 8) / 180)
})

test_that("the arc edge type clears the confounder chain's middle node", {
  local_ggdag_option_state()
  withr::local_options(list(
    ggdag.layout = "time_ordered",
    ggdag.edge_width = 1.5,
    ggdag.edge_type = "arc"
  ))

  # z -> y spans two layers and its drawn arc bows toward x, which clears
  # the straight chord but sits inside the engine's overlap threshold of the
  # arc as drawn. The same clearance floor as the collider case applies, at
  # the default node scale.
  td <- tidy_dagitty(dagify(y ~ x + z, x ~ z))
  coords <- dplyr::distinct(pull_dag_data(td), name, x, y)

  clearance <- drawn_arc_clearance(
    coords,
    "z",
    "y",
    "x",
    ggdag_option("curvature", 0.3)
  )
  expect_gte(clearance, (26 + 8) / 180)
})

test_that("the arc option leaves already-clear layouts unchanged", {
  local_ggdag_option_state()
  withr::local_options(list(
    ggdag.layout = "time_ordered",
    ggdag.edge_type = "arc"
  ))

  # Every spanning edge of this DAG already clears its drawn arc, so
  # arc-aware layout must move nothing: the coordinates match the
  # default-option layout to four decimals.
  dag <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2,
    exposure = "x",
    outcome = "y"
  )
  td <- tidy_dagitty(dag)

  expect_equal(
    layout_node_coords(td),
    tibble::tibble(
      name = c("v", "w1", "w2", "x", "y", "z1", "z2"),
      x = c(1, 1, 1, 3, 4, 2, 3),
      y = c(-0.270, 0.130, 0.530, -0.325, 0.396, -0.536, 0.075)
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("the default edge type keeps straight-chord layouts unchanged", {
  local_ggdag_option_state()
  withr::local_options(list(ggdag.edge_type = "link_arc"))

  # Directed edges render as straight links under the default edge type, so
  # arc tracing must not engage: these layouts stay exactly as the
  # straight-chord engine solves them, even at the node scale where the arc
  # edge type moves the middle node.
  collider_edges <- as.data.frame(get_dagitty_edges(dagify(m ~ x + y, y ~ x)))
  collider <- compute_time_ordered_layout(collider_edges, node_scale = 24 / 16)
  expect_equal(
    dplyr::arrange(collider, name),
    tibble::tibble(
      name = c("m", "x", "y"),
      x = c(3, 1, 2),
      y = c(-0.17213682, -0.17213682, 0.34427365)
    ),
    tolerance = 1e-7
  )

  chain_edges <- as.data.frame(get_dagitty_edges(dagify(y ~ x + z, x ~ z)))
  chain <- compute_time_ordered_layout(chain_edges)
  expect_equal(
    dplyr::arrange(chain, name),
    tibble::tibble(
      name = c("x", "y", "z"),
      x = c(2, 3, 1),
      y = c(0.24209197, -0.12104599, -0.12104599)
    ),
    tolerance = 1e-7
  )
})
