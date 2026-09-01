# Tests for time_ordered_coords(optimize = ) and the engine's fixed_layers
# mode: manual tiers routed through the layered layout engine, the naive
# spread preserved under optimize = FALSE, tier-violation warnings, and the
# fixed-tier engine contract.

# Helpers ----------------------------------------------------------------------

# Parse "a->b" strings into an edge data frame shaped like the engine's real
# input: one row per directed edge plus a to = NA row per terminal or
# isolated node.
manual_edges <- function(edge_strings, isolated = character(0)) {
  parts <- strsplit(edge_strings, "->", fixed = TRUE)
  edges <- data.frame(
    name = vapply(parts, function(p) p[[1]], character(1)),
    to = vapply(parts, function(p) p[[2]], character(1)),
    stringsAsFactors = FALSE
  )
  node_only <- union(setdiff(unique(edges$to), edges$name), isolated)
  if (length(node_only) > 0) {
    edges <- rbind(
      edges,
      data.frame(name = node_only, to = NA_character_, stringsAsFactors = FALSE)
    )
  }
  edges
}

# The tibble time_ordered_coords(.vars = ) has always produced: one
# spread_coords() block per tier.
naive_tiers <- function(vars, time_points = seq_along(vars), direction = "x") {
  purrr::map2_dfr(time_points, vars, spread_coords, direction = direction)
}

node_x <- function(coords, node) {
  coords$x[coords$name == node][[1]]
}

# Evaluate expr, muffling and collecting every warning, so a test can assert
# an exact warning count without snapshots.
collect_warnings <- function(expr) {
  seen <- list()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      seen[[length(seen) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = seen)
}

# Return type and default ------------------------------------------------------

test_that("time_ordered_coords(): optimize = TRUE returns a layout closure", {
  vars <- list("a", c("b1", "b2"), "c")
  expect_true(is.function(time_ordered_coords(vars, optimize = TRUE)))

  time_df <- data.frame(
    name = c("a", "b1", "b2", "c"),
    time = c(1, 2, 2, 3)
  )
  expect_true(is.function(time_ordered_coords(time_df, optimize = TRUE)))
})

test_that("time_ordered_coords(): optimize = FALSE reproduces the spread tibble", {
  vars <- list("a", c("b1", "b2"), c("c1", "c2", "c3"), "d")

  expect_equal(
    time_ordered_coords(vars, optimize = FALSE),
    naive_tiers(vars)
  )

  expect_equal(
    time_ordered_coords(vars, time_points = c(1, 2, 4, 8), optimize = FALSE),
    naive_tiers(vars, time_points = c(1, 2, 4, 8))
  )

  expect_equal(
    time_ordered_coords(vars, direction = "y", optimize = FALSE),
    naive_tiers(vars, direction = "y")
  )

  time_df <- data.frame(
    name = c("x1", "x2", "y", "z1", "z2", "z3", "a"),
    time = c(1, 1, 2, 3, 3, 3, 4)
  )
  expect_equal(
    time_ordered_coords(time_df, optimize = FALSE),
    naive_tiers(
      split(time_df$name, time_df$time),
      time_points = sort(unique(time_df$time))
    )
  )
})

test_that("time_ordered_coords(): optimize defaults to TRUE", {
  expect_identical(formals(time_ordered_coords)$optimize, TRUE)

  # behavior: both manual forms return closures without naming the argument
  expect_true(is.function(time_ordered_coords(list("a", "b"))))
  expect_true(is.function(
    time_ordered_coords(data.frame(name = c("a", "b"), time = c(1, 2)))
  ))
})

test_that("auto mode (.vars = NULL) still returns the auto closure", {
  withr::local_seed(1234)
  auto <- time_ordered_coords()
  expect_true(is.function(auto))

  dag <- dagify(d ~ c1 + c2 + c3, c1 ~ b1 + b2, c3 ~ a, b1 ~ a)
  expect_equal(
    pull_dag_data(tidy_dagitty(dag, layout = "time_ordered")),
    pull_dag_data(tidy_dagitty(dag, layout = auto))
  )
})

# Closure semantics ------------------------------------------------------------

test_that("optimized manual tiers group x exactly by the user's tiers", {
  withr::local_seed(1234)
  tiers <- list("a", c("b1", "b2"), c("c1", "c2", "c3"), "d")
  coords_fun <- time_ordered_coords(tiers)
  expect_true(is.function(coords_fun))

  tidy_dag <- dagify(
    d ~ c1 + c2 + c3,
    c1 ~ b1 + b2,
    c3 ~ a,
    b1 ~ a,
    coords = coords_fun
  ) |>
    tidy_dagitty()
  node_data <- dplyr::distinct(pull_dag_data(tidy_dag), name, x, y)

  tier_x <- purrr::map(tiers, function(nodes) {
    unique(node_data$x[node_data$name %in% nodes])
  })
  # x is constant within each tier
  expect_true(all(lengths(tier_x) == 1))
  # and strictly increasing across tiers
  expect_true(all(diff(unlist(tier_x)) > 0))
})

test_that("time_points values become the tier x positions when optimizing", {
  tiers <- list("a", c("b1", "b2"), "c")
  coords_fun <- time_ordered_coords(tiers, time_points = c(1, 3, 9))
  edges <- manual_edges(c("a->b1", "a->b2", "b1->c", "b2->c"))

  coords <- coords_fun(edges)

  expect_equal(node_x(coords, "a"), 1)
  expect_equal(node_x(coords, "b1"), 3)
  expect_equal(node_x(coords, "b2"), 3)
  expect_equal(node_x(coords, "c"), 9)
})

test_that("the user's within-tier order survives when all orderings tie", {
  withr::local_seed(1234)
  # one parent feeding every child: every within-tier permutation ties on
  # crossings, so the incumbent (the user's listed order) must win
  coords_fun <- time_ordered_coords(list("a", c("q", "p", "z")))
  expect_true(is.function(coords_fun))

  tidy_dag <- dagify(
    q ~ a,
    p ~ a,
    z ~ a,
    coords = coords_fun
  ) |>
    tidy_dagitty()
  node_data <- dplyr::distinct(pull_dag_data(tidy_dag), name, x, y)
  second_tier <- node_data[node_data$name %in% c("q", "p", "z"), ]

  expect_identical(second_tier$name[order(second_tier$y)], c("q", "p", "z"))
})

test_that("the engine reorders within tiers to cut crossings", {
  tiers <- list(c("a", "b"), c("c", "d"))
  edges <- manual_edges(c("a->d", "b->c"))

  naive <- time_ordered_coords(tiers, optimize = FALSE)
  naive_crossings <- count_edge_crossings(naive, edges)
  expect_equal(naive_crossings, 1L)

  coords_fun <- time_ordered_coords(tiers)
  optimized <- coords_fun(edges)
  expect_lt(count_edge_crossings(optimized, edges), naive_crossings)
  expect_equal(count_edge_crossings(optimized, edges), 0L)

  # the tiers themselves are untouched by the reordering
  expect_setequal(optimized$name[optimized$x == min(optimized$x)], c("a", "b"))
  expect_setequal(optimized$name[optimized$x == max(optimized$x)], c("c", "d"))
})

test_that("optimized tiers never cross more than the naive spread", {
  fixtures <- list(
    butterfly = list(
      tiers = list(c("x1", "x2"), "m", c("y1", "y2")),
      spec = c("x1->m", "x2->m", "m->y1", "m->y2", "x1->y1", "x2->y2")
    ),
    wide_dag = list(
      tiers = list(c("x1", "x2", "x3"), c("m1", "m2"), "y"),
      spec = c(
        "x1->m1",
        "x2->m1",
        "x3->m2",
        "x1->m2",
        "m1->y",
        "m2->y",
        "x2->y",
        "x3->y"
      )
    )
  )

  for (nm in names(fixtures)) {
    edges <- manual_edges(fixtures[[nm]]$spec)
    naive <- time_ordered_coords(fixtures[[nm]]$tiers, optimize = FALSE)
    optimized <- time_ordered_coords(fixtures[[nm]]$tiers)(edges)
    expect_lte(
      count_edge_crossings(optimized, edges),
      count_edge_crossings(naive, edges),
      label = paste0(nm, ": optimized crossings"),
      expected.label = paste0(nm, ": naive crossings")
    )
  }
})

test_that("optimized manual tiers draw with zero node-edge overlaps", {
  tiers <- list("x", c("m1", "m2", "m3"), "y")
  edges <- manual_edges(
    c("x->m1", "x->m2", "x->m3", "m1->y", "m2->y", "m3->y", "x->y")
  )

  # the naive spread runs the skip edge x -> y straight through the middle
  # tier; the engine must clear it
  naive <- time_ordered_coords(tiers, optimize = FALSE)
  expect_gt(count_node_edge_overlaps(naive, edges, node_radius_data()), 0L)

  coords <- time_ordered_coords(tiers)(edges)
  expect_identical(
    count_node_edge_overlaps(coords, edges, node_radius_data()),
    0L
  )
})

# Tier violations --------------------------------------------------------------

test_that("a backward edge warns once with ggdag_warning and keeps the tiers", {
  coords_fun <- time_ordered_coords(list("early", "late"))
  edges <- manual_edges("late->early")

  captured <- collect_warnings(coords_fun(edges))
  expect_length(captured$warnings, 1)
  expect_s3_class(captured$warnings[[1]], "ggdag_warning")
  expect_match(conditionMessage(captured$warnings[[1]]), "late")
  expect_match(conditionMessage(captured$warnings[[1]]), "early")

  # the user's tiers are authoritative even for the violating edge's endpoints
  coords <- captured$value
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
  expect_lt(node_x(coords, "early"), node_x(coords, "late"))

  # deterministic despite the exclusion
  expect_identical(
    suppressWarnings(coords_fun(edges)),
    suppressWarnings(coords_fun(edges))
  )
})

test_that("a violating edge is still drawn in the tidied DAG", {
  withr::local_seed(1234)
  expect_warning(
    dag <- dagify(
      early ~ late,
      coords = time_ordered_coords(list("early", "late"))
    ),
    class = "ggdag_warning",
    regexp = "late"
  )

  node_data <- pull_dag_data(tidy_dagitty(dag))
  edge_row <- node_data[!is.na(node_data$to), , drop = FALSE]
  expect_identical(edge_row$name, "late")
  expect_identical(edge_row$to, "early")
  expect_true(all(is.finite(c(edge_row$xend, edge_row$yend))))
})

test_that("all tier violations are named in a single warning", {
  coords_fun <- time_ordered_coords(list(c("early", "peer1", "peer2"), "late"))
  edges <- manual_edges(c("late->early", "peer1->peer2"))

  captured <- collect_warnings(coords_fun(edges))
  expect_length(captured$warnings, 1)
  expect_s3_class(captured$warnings[[1]], "ggdag_warning")
  msg <- conditionMessage(captured$warnings[[1]])
  expect_match(msg, "late")
  expect_match(msg, "peer1")

  # both violating edges are excluded from the objectives but the tiers hold
  coords <- captured$value
  expect_identical(node_x(coords, "peer1"), node_x(coords, "peer2"))
  expect_identical(node_x(coords, "peer1"), node_x(coords, "early"))
  expect_lt(node_x(coords, "early"), node_x(coords, "late"))
})

# fixed_layers engine contract -------------------------------------------------

test_that("compute_time_ordered_layout(): fixed_layers skips layer inference", {
  # longest-path layering would put x and y in the same first layer; the
  # user's assignment splits them and must be honored as given
  edges <- manual_edges(c("x->z", "y->z"))
  fixed <- c(x = 1L, y = 2L, z = 3L)

  expect_no_warning(
    coords <- compute_time_ordered_layout(edges, fixed_layers = fixed)
  )

  expect_lt(node_x(coords, "x"), node_x(coords, "y"))
  expect_lt(node_x(coords, "y"), node_x(coords, "z"))
})

test_that("compute_time_ordered_layout(): fixed_layers must cover every node", {
  edges <- manual_edges(c("x->z", "y->z"))
  expect_error(
    compute_time_ordered_layout(edges, fixed_layers = c(x = 1L, z = 2L)),
    class = "ggdag_type_error"
  )
})

test_that("compute_time_ordered_layout(): invalid fixed_layers tiers error", {
  edges <- manual_edges(c("x->z", "y->z"))

  expect_error(
    compute_time_ordered_layout(
      edges,
      fixed_layers = c(x = 1.5, y = 2, z = 3)
    ),
    class = "ggdag_type_error"
  )
  expect_error(
    compute_time_ordered_layout(
      edges,
      fixed_layers = c(x = -1L, y = 1L, z = 2L)
    ),
    class = "ggdag_type_error"
  )
})

test_that("fixed_layers layouts are deterministic", {
  edges <- manual_edges(c("x->z", "y->z"))
  fixed <- c(x = 1L, y = 2L, z = 3L)

  first_run <- compute_time_ordered_layout(edges, fixed_layers = fixed)
  second_run <- compute_time_ordered_layout(edges, fixed_layers = fixed)

  expect_identical(first_run, second_run)
  # and both honor the user's assignment
  expect_lt(node_x(first_run, "x"), node_x(first_run, "y"))
})

test_that("node_scale still scales same-tier spacing under fixed_layers", {
  fan <- manual_edges(c("a->y", "b->y", "c->y", "d->y", "e->y"))
  fixed <- c(a = 1L, b = 1L, c = 1L, d = 1L, e = 1L, y = 2L)
  roots <- c("a", "b", "c", "d", "e")

  min_gap <- function(coords) {
    min(diff(sort(coords$y[coords$name %in% roots])))
  }

  base <- compute_time_ordered_layout(fan, fixed_layers = fixed, node_scale = 1)
  scaled <- compute_time_ordered_layout(
    fan,
    fixed_layers = fixed,
    node_scale = 2
  )

  expect_gt(min_gap(scaled), min_gap(base))
})

# Interaction guards -----------------------------------------------------------

test_that("adjust_exposure_outcome is a no-op under fixed_layers", {
  edges <- manual_edges(c("z->x", "z->y"))
  fixed <- c(z = 1L, x = 2L, y = 2L)

  expect_no_message(
    expect_no_warning(
      with_eo <- compute_time_ordered_layout(
        edges,
        fixed_layers = fixed,
        exposure = "x",
        outcome = "y",
        adjust_exposure_outcome = TRUE
      )
    )
  )
  without_eo <- compute_time_ordered_layout(edges, fixed_layers = fixed)

  expect_identical(with_eo, without_eo)
  # the user put exposure and outcome in the same tier, and there they stay
  expect_identical(node_x(with_eo, "x"), node_x(with_eo, "y"))
})

test_that("fixed_layers keeps an isolated node at its given tier", {
  edges <- manual_edges("x->y", isolated = "w")
  coords <- compute_time_ordered_layout(
    edges,
    fixed_layers = c(x = 1L, y = 2L, w = 2L)
  )

  expect_identical(node_x(coords, "w"), node_x(coords, "y"))
  expect_gt(node_x(coords, "w"), node_x(coords, "x"))
})

test_that("a tier list keeps an isolated node at its given tier", {
  withr::local_seed(1234)
  dag <- dagitty::dagitty("dag{a -> b b -> c w}")
  tidy_dag <- tidy_dagitty(
    dag,
    layout = time_ordered_coords(list("a", c("b", "w"), "c"))
  )
  node_data <- dplyr::distinct(pull_dag_data(tidy_dag), name, x, y)

  expect_identical(
    unique(node_data$x[node_data$name == "w"]),
    unique(node_data$x[node_data$name == "b"])
  )
  expect_lt(
    unique(node_data$x[node_data$name == "a"]),
    unique(node_data$x[node_data$name == "w"])
  )
})

# Back-compat ------------------------------------------------------------------

test_that("as_tidy_dagitty.list() output is unchanged", {
  withr::local_seed(1234)
  time_list <- list(c("a", "b"), "c", "d")
  tidy_dag <- as_tidy_dagitty(time_list)
  node_data <- dplyr::distinct(pull_dag_data(tidy_dag), name, x, y)

  expect_setequal(node_data$name, c("a", "b", "c", "d"))
  expect_true(all(is.finite(node_data$x)))
  expect_true(all(is.finite(node_data$y)))

  # time points group x: a and b share the first, then c, then d
  expect_identical(node_x(node_data, "a"), node_x(node_data, "b"))
  expect_lt(node_x(node_data, "a"), node_x(node_data, "c"))
  expect_lt(node_x(node_data, "c"), node_x(node_data, "d"))

  # the saturated edge list is untouched
  edge_rows <- pull_dag_data(tidy_dag)
  edge_rows <- edge_rows[!is.na(edge_rows$to), , drop = FALSE]
  expect_setequal(
    paste(edge_rows$name, edge_rows$to, sep = "->"),
    c("a->c", "a->d", "b->c", "b->d", "c->d")
  )

  expect_identical(
    pull_dag_data(as_tidy_dagitty(list(c("a", "b"), "c", "d"))),
    pull_dag_data(tidy_dag)
  )
})
