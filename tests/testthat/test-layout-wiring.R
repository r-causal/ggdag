# Tests for wiring the ordering and coordinate upgrades into
# compute_time_ordered_layout(): exact layer ordering via order_layers(),
# median-based initialization, isotropic normalization, node_scale-derived
# clearance constants, and curvature-aware bidirected overlap correction.

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
# overcontrol             1       0   (strict improvement)
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
canonical_wiring_dags <- list(
  confounding = list(
    spec = c("z->x", "z->y", "x->y"),
    budget = 0L
  ),
  mediation = list(
    spec = c("x->m", "m->y", "x->y"),
    budget = 0L
  ),
  collider = list(
    spec = c("x->c", "y->c"),
    budget = 0L
  ),
  iv = list(
    spec = c("z->x", "x->y", "u->x", "u->y"),
    budget = 0L
  ),
  front_door = list(
    spec = c("u->x", "u->y", "x->m", "m->y"),
    budget = 0L
  ),
  m_bias = list(
    spec = c("u1->a", "u1->m", "u2->m", "u2->y", "a->y"),
    budget = 1L
  ),
  smoking = list(
    spec = c(
      "genetics->smoking",
      "genetics->cancer",
      "smoking->tar",
      "tar->cancer",
      "smoking->cancer"
    ),
    budget = 0L
  ),
  epidemiology = list(
    spec = c(
      "ses->edu",
      "ses->health",
      "edu->income",
      "edu->health",
      "income->health",
      "age->ses",
      "age->health",
      "gene->health",
      "gene->ses"
    ),
    budget = 1L
  ),
  selection_bias = list(
    spec = c("a->y", "a->s", "u->s", "u->y", "l->a", "l->u"),
    budget = 1L
  ),
  overcontrol = list(
    spec = c("x->z", "z->y", "x->y", "w->x", "w->z"),
    budget = 0L
  ),
  napkin = list(
    spec = c(
      "u1->z",
      "u1->a",
      "u2->a",
      "u2->y",
      "z->a",
      "a->m",
      "m->y",
      "a->y"
    ),
    budget = 0L
  ),
  butterfly = list(
    spec = c("x1->m", "x2->m", "m->y1", "m->y2", "x1->y1", "x2->y2"),
    budget = 0L
  ),
  complex_chain = list(
    spec = c(
      "a->b",
      "b->c",
      "c->d",
      "d->e",
      "a->c",
      "b->d",
      "c->e",
      "a->e"
    ),
    budget = 0L
  ),
  wide_dag = list(
    spec = c(
      "x1->m1",
      "x2->m1",
      "x3->m2",
      "x1->m2",
      "m1->y",
      "m2->y",
      "x2->y",
      "x3->y"
    ),
    budget = 3L
  ),
  deep_confound = list(
    spec = c(
      "u->a",
      "u->b",
      "u->c",
      "a->b",
      "b->c",
      "a->d",
      "c->d",
      "b->d"
    ),
    budget = 0L
  ),
  large_epi = list(
    spec = c(
      "age->ses",
      "age->smoking",
      "age->bmi",
      "age->health",
      "ses->smoking",
      "ses->diet",
      "ses->health",
      "smoking->cancer",
      "smoking->health",
      "diet->bmi",
      "diet->health",
      "bmi->cancer",
      "bmi->health",
      "cancer->health",
      "gene->cancer",
      "gene->bmi",
      "gene->smoking"
    ),
    budget = 12L
  ),
  treatment = list(
    spec = c(
      "c1->x",
      "c2->x",
      "c1->y",
      "c2->y",
      "x->m1",
      "x->m2",
      "m1->y",
      "m2->y",
      "u->m1",
      "u->y"
    ),
    budget = 2L
  ),
  double_iv = list(
    spec = c(
      "z1->x",
      "z2->x",
      "x->m",
      "m->y",
      "u1->x",
      "u1->m",
      "u2->m",
      "u2->y"
    ),
    budget = 0L
  ),
  cascade = list(
    spec = c(
      "a->b",
      "a->d",
      "b->c",
      "c->d",
      "b->e",
      "d->e",
      "c->f",
      "e->f",
      "a->f"
    ),
    budget = 3L
  ),
  triple_confound = list(
    spec = c(
      "u->x",
      "u->y",
      "v->x",
      "v->m",
      "w->m",
      "w->y",
      "x->m",
      "m->y",
      "x->y"
    ),
    budget = 4L
  ),
  regression_disc = list(
    spec = c(
      "z->x",
      "x->y",
      "x->w",
      "w->y",
      "z->w",
      "u->w",
      "u->y"
    ),
    budget = 0L
  ),
  multi_mediator = list(
    spec = c(
      "x->m1",
      "x->m2",
      "x->m3",
      "m1->m2",
      "m2->m3",
      "m1->y",
      "m2->y",
      "m3->y",
      "x->y",
      "u->m2",
      "u->y"
    ),
    budget = 5L
  )
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
  # x <-> y spans two layers, so its drawn arc (curvature 0.3) bows below the
  # chord. z sits right on that arc: invisible to the straight-line check,
  # but a real overlap once the arc is traced.
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
    check_bidirected = TRUE
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
  layer_assign <- c(a = 0L, q = 1L, p = 1L, z = 1L)

  out <- barycenter_sort(layer_nodes, edges, layer_assign)
  expect_identical(out[[2]], c("q", "p", "z"))
})
