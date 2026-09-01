# Tests for the layer-ordering machinery: insert_dummy_nodes(),
# permutations_lex(), count_crossings_bilayer(), and order_layers().

# Helpers ----------------------------------------------------------------------

# Parse "a->b" strings into an edge data frame with name and to columns.
parse_edges <- function(edge_strings) {
  parts <- strsplit(edge_strings, "->", fixed = TRUE)
  data.frame(
    name = vapply(parts, function(p) p[[1]], character(1)),
    to = vapply(parts, function(p) p[[2]], character(1)),
    stringsAsFactors = FALSE
  )
}

# Build the initial layer_nodes list the same way the layout engine does:
# element i holds the nodes assigned to 0-based layer i - 1, in the order they
# appear in layer_assign.
layers_from_assign <- function(layer_assign) {
  lapply(seq(0L, max(layer_assign)), function(l) {
    names(layer_assign)[layer_assign == l]
  })
}

# Coordinates implied by a layer ordering: x = layer index, y = rank within
# the layer. Straight-line crossings of these coordinates measure how well an
# ordering routes long edges past intermediate layers.
rank_coords <- function(layer_nodes) {
  data.frame(
    name = unlist(layer_nodes),
    x = rep(seq_along(layer_nodes), lengths(layer_nodes)),
    y = unlist(lapply(layer_nodes, seq_along)),
    stringsAsFactors = FALSE
  )
}

# Directed edges as sorted "from->to" keys, ignoring row order.
directed_edge_keys <- function(edges) {
  directed <- edges[!is.na(edges$to), , drop = FALSE]
  sort(paste(directed$name, directed$to, sep = "->"))
}

# All orderings of a character vector, independent of permutations_lex(), for
# brute-force optimality checks.
all_orderings <- function(x) {
  if (length(x) <= 1) {
    return(list(x))
  }
  out <- list()
  for (i in seq_along(x)) {
    for (rest in all_orderings(x[-i])) {
      out <- c(out, list(c(x[i], rest)))
    }
  }
  out
}

napkin_spec <- c(
  "u1->z",
  "u1->a",
  "u2->a",
  "u2->y",
  "z->a",
  "a->m",
  "m->y",
  "a->y"
)

butterfly_spec <- c(
  "x1->m",
  "x2->m",
  "m->y1",
  "m->y2",
  "x1->y1",
  "x2->y2"
)

canonical_specs <- list(
  confounding = c("z->x", "z->y", "x->y"),
  mediation = c("x->m", "m->y", "x->y"),
  iv = c("z->x", "x->y", "u->x", "u->y"),
  m_bias = c("u1->a", "u1->m", "u2->m", "u2->y", "a->y"),
  napkin = napkin_spec,
  butterfly = butterfly_spec,
  complex_chain = c(
    "a->b",
    "b->c",
    "c->d",
    "d->e",
    "a->c",
    "b->d",
    "c->e",
    "a->e"
  ),
  wide_dag = c(
    "x1->m1",
    "x2->m1",
    "x3->m2",
    "x1->m2",
    "m1->y",
    "m2->y",
    "x2->y",
    "x3->y"
  ),
  triple_confound = c(
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
  multi_mediator = c(
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
  )
)

# insert_dummy_nodes -----------------------------------------------------------

test_that("insert_dummy_nodes: adjacent-layer edges pass through unchanged", {
  edges <- data.frame(name = c("a", "b"), to = c("b", "c"))
  layer_assign <- c(a = 0L, b = 1L, c = 2L)

  res <- insert_dummy_nodes(edges, layer_assign)

  expect_named(res, c("edges", "layer_assign", "dummy_map"))
  expect_named(res$dummy_map, c("dummy", "edge_from", "edge_to", "layer"))
  expect_identical(nrow(res$dummy_map), 0L)
  expect_identical(directed_edge_keys(res$edges), c("a->b", "b->c"))
  expect_true(is.integer(res$layer_assign))
  expect_identical(res$layer_assign[names(layer_assign)], layer_assign)
})

test_that("insert_dummy_nodes: a two-layer span gains one chained dummy", {
  edges <- data.frame(name = c("a", "a", "b"), to = c("b", "c", "c"))
  layer_assign <- c(a = 0L, b = 1L, c = 2L)

  res <- insert_dummy_nodes(edges, layer_assign)

  expect_identical(
    directed_edge_keys(res$edges),
    sort(c("a->b", "b->c", "a->.ggdag_dummy_1", ".ggdag_dummy_1->c"))
  )
  expect_identical(res$layer_assign[[".ggdag_dummy_1"]], 1L)
  expect_identical(res$layer_assign[names(layer_assign)], layer_assign)

  expect_identical(res$dummy_map$dummy, ".ggdag_dummy_1")
  expect_identical(res$dummy_map$edge_from, "a")
  expect_identical(res$dummy_map$edge_to, "c")
  expect_identical(res$dummy_map$layer, 1L)
})

test_that("insert_dummy_nodes: dummies number by row order, layers ascending", {
  # row 1 spans three layers (two dummies), row 2 spans two (one dummy)
  edges <- data.frame(name = c("a", "b", "a"), to = c("d", "d", "b"))
  layer_assign <- c(a = 0L, b = 1L, d = 3L)

  res <- insert_dummy_nodes(edges, layer_assign)

  expect_identical(
    res$dummy_map$dummy,
    paste0(".ggdag_dummy_", seq_len(3))
  )
  expect_identical(res$dummy_map$edge_from, c("a", "a", "b"))
  expect_identical(res$dummy_map$edge_to, c("d", "d", "d"))
  expect_identical(res$dummy_map$layer, c(1L, 2L, 2L))

  expect_identical(
    directed_edge_keys(res$edges),
    sort(c(
      "a->.ggdag_dummy_1",
      ".ggdag_dummy_1->.ggdag_dummy_2",
      ".ggdag_dummy_2->d",
      "b->.ggdag_dummy_3",
      ".ggdag_dummy_3->d",
      "a->b"
    ))
  )

  expect_identical(res$layer_assign[[".ggdag_dummy_1"]], 1L)
  expect_identical(res$layer_assign[[".ggdag_dummy_2"]], 2L)
  expect_identical(res$layer_assign[[".ggdag_dummy_3"]], 2L)
})

test_that("insert_dummy_nodes: NA, same-layer, and backward rows untouched", {
  edges <- data.frame(
    name = c("a", "c", "u", "iso"),
    to = c("b", "a", "v", NA)
  )
  layer_assign <- c(a = 0L, b = 1L, c = 2L, u = 1L, v = 1L, iso = 0L)

  res <- insert_dummy_nodes(edges, layer_assign)

  expect_identical(nrow(res$dummy_map), 0L)
  expect_identical(directed_edge_keys(res$edges), c("a->b", "c->a", "u->v"))
  na_rows <- res$edges[is.na(res$edges$to), , drop = FALSE]
  expect_identical(na_rows$name, "iso")
  expect_identical(res$layer_assign[names(layer_assign)], layer_assign)
})

test_that("insert_dummy_nodes: reserved dummy prefix in node names aborts", {
  edges <- data.frame(name = ".ggdag_dummy_1", to = "b")
  layer_assign <- stats::setNames(c(0L, 1L), c(".ggdag_dummy_1", "b"))
  expect_error(
    insert_dummy_nodes(edges, layer_assign),
    class = "ggdag_dag_error"
  )

  # the check is on the prefix, not an exact name
  edges2 <- data.frame(name = "a", to = ".ggdag_dummy_extra")
  layer_assign2 <- stats::setNames(c(0L, 1L), c("a", ".ggdag_dummy_extra"))
  expect_error(
    insert_dummy_nodes(edges2, layer_assign2),
    class = "ggdag_dag_error"
  )
})

test_that("insert_dummy_nodes: napkin spans produce the expected chains", {
  edges <- parse_edges(napkin_spec)
  layer_assign <- longest_path_layers(edges)

  # guard the fixture: the layering this test is built on
  expect_identical(
    layer_assign[c("u1", "z", "u2", "a", "m", "y")],
    c(u1 = 0L, z = 1L, u2 = 1L, a = 2L, m = 3L, y = 4L)
  )

  res <- insert_dummy_nodes(edges, layer_assign)

  # u1 -> a gets one dummy, u2 -> y two, a -> y one, in row order with the
  # layers of each chain ascending
  expect_identical(res$dummy_map$dummy, paste0(".ggdag_dummy_", seq_len(4)))
  expect_identical(res$dummy_map$edge_from, c("u1", "u2", "u2", "a"))
  expect_identical(res$dummy_map$edge_to, c("a", "y", "y", "y"))
  expect_identical(res$dummy_map$layer, c(1L, 2L, 3L, 3L))

  # 5 unchanged edges plus chains of 2, 3, and 2
  expect_identical(nrow(res$edges), 12L)
  spans <- res$layer_assign[res$edges$to] - res$layer_assign[res$edges$name]
  expect_true(all(spans == 1L))

  # the u2 -> y chain threads its two dummies in sequence
  keys <- directed_edge_keys(res$edges)
  expect_true(all(
    c(
      "u2->.ggdag_dummy_2",
      ".ggdag_dummy_2->.ggdag_dummy_3",
      ".ggdag_dummy_3->y"
    ) %in%
      keys
  ))
})

test_that("insert_dummy_nodes: butterfly gets one dummy per skip edge", {
  edges <- parse_edges(butterfly_spec)
  layer_assign <- longest_path_layers(edges)

  expect_identical(
    layer_assign[c("x1", "x2", "m", "y1", "y2")],
    c(x1 = 0L, x2 = 0L, m = 1L, y1 = 2L, y2 = 2L)
  )

  res <- insert_dummy_nodes(edges, layer_assign)

  expect_identical(nrow(res$dummy_map), 2L)
  expect_identical(res$dummy_map$edge_from, c("x1", "x2"))
  expect_identical(res$dummy_map$edge_to, c("y1", "y2"))
  expect_identical(res$dummy_map$layer, c(1L, 1L))
  expect_identical(nrow(res$edges), 8L)
  spans <- res$layer_assign[res$edges$to] - res$layer_assign[res$edges$name]
  expect_true(all(spans == 1L))
})

# permutations_lex -------------------------------------------------------------

test_that("permutations_lex: n = 0 gives one empty permutation", {
  p0 <- permutations_lex(0)
  expect_true(is.matrix(p0))
  expect_identical(dim(p0), c(1L, 0L))
})

test_that("permutations_lex: small cases in exact lexicographic order", {
  expect_true(is.integer(permutations_lex(3)))
  expect_equal(unname(permutations_lex(1)), matrix(1L, nrow = 1))
  expect_equal(unname(permutations_lex(2)), rbind(c(1L, 2L), c(2L, 1L)))
  expect_equal(
    unname(permutations_lex(3)),
    matrix(
      c(1L, 2L, 3L, 1L, 3L, 2L, 2L, 1L, 3L, 2L, 3L, 1L, 3L, 1L, 2L, 3L, 2L, 1L),
      ncol = 3,
      byrow = TRUE
    )
  )
})

test_that("permutations_lex: n = 4 has every permutation once, ascending", {
  p4 <- permutations_lex(4)
  expect_identical(dim(p4), c(24L, 4L))

  sorted_rows <- t(apply(p4, 1, sort))
  expect_true(all(
    sorted_rows == matrix(seq_len(4), nrow = 24, ncol = 4, byrow = TRUE)
  ))
  expect_identical(nrow(unique(p4)), 24L)

  # strictly increasing lexicographic order via a positional key
  key <- drop(p4 %*% 5^(3:0))
  expect_true(all(diff(key) > 0))
})

test_that("permutations_lex: supports n = 7", {
  p7 <- permutations_lex(7)
  expect_identical(dim(p7), c(5040L, 7L))
  expect_equal(unname(p7[1, ]), seq_len(7))
  expect_equal(unname(p7[5040, ]), rev(seq_len(7)))
})

# count_crossings_bilayer ------------------------------------------------------

test_that("count_crossings_bilayer: parallel edges do not cross", {
  expect_identical(
    count_crossings_bilayer(
      c("a", "b"),
      c("x", "y"),
      data.frame(name = c("a", "b"), to = c("x", "y"))
    ),
    0L
  )
})

test_that("count_crossings_bilayer: a crossed matching counts one", {
  expect_identical(
    count_crossings_bilayer(
      c("a", "b"),
      c("x", "y"),
      data.frame(name = c("a", "b"), to = c("y", "x"))
    ),
    1L
  )
})

test_that("count_crossings_bilayer: complete bipartite K3,3 counts nine", {
  left <- c("a", "b", "c")
  right <- c("x", "y", "z")
  edges <- expand.grid(name = left, to = right, stringsAsFactors = FALSE)
  # every pair of distinct sources and distinct targets contributes exactly
  # one crossing: choose(3, 2)^2 = 9
  expect_identical(count_crossings_bilayer(left, right, edges), 9L)
})

test_that("count_crossings_bilayer: edges naming absent nodes are ignored", {
  edges <- data.frame(
    name = c("a", "b", "ghost", "a"),
    to = c("y", "x", "x", "ghost")
  )
  expect_identical(
    count_crossings_bilayer(c("a", "b"), c("x", "y"), edges),
    1L
  )

  empty <- data.frame(name = character(0), to = character(0))
  expect_identical(
    count_crossings_bilayer(c("a", "b"), c("x", "y"), empty),
    0L
  )
})

test_that("count_crossings_bilayer: agrees with count_crossings", {
  left <- c("a", "b", "c")
  right <- c("x", "y", "z")
  edges <- data.frame(
    name = c("a", "a", "b", "c", "c"),
    to = c("y", "z", "x", "x", "y")
  )
  layer_assign <- c(a = 0L, b = 0L, c = 0L, x = 1L, y = 1L, z = 1L)

  expect_identical(count_crossings_bilayer(left, right, edges), 5L)
  expect_identical(
    count_crossings_bilayer(left, right, edges),
    count_crossings(list(left, right), edges, layer_assign)
  )
})

# order_layers: determinism and structure --------------------------------------

test_that("order_layers: identical inputs give identical outputs, no RNG", {
  edges <- parse_edges(napkin_spec)
  layer_assign <- longest_path_layers(edges)
  layer_nodes <- layers_from_assign(layer_assign)

  if (!exists(".Random.seed", envir = globalenv())) {
    set.seed(1)
  }
  seed_before <- get(".Random.seed", envir = globalenv())

  res1 <- order_layers(layer_nodes, edges, layer_assign)
  res2 <- order_layers(layer_nodes, edges, layer_assign)

  expect_identical(res1, res2)
  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)

  expect_named(res1, c("layer_nodes", "augmented"))
  expect_named(res1$augmented, c("layer_nodes", "layer_assign", "edges"))

  # the napkin has skip edges, so the augmented view holds dummies while the
  # real ordering stays dummy free
  aug_names <- unlist(res1$augmented$layer_nodes)
  expect_true(any(startsWith(aug_names, ".ggdag_dummy_")))
  expect_false(any(startsWith(unlist(res1$layer_nodes), ".ggdag_dummy_")))
})

test_that("order_layers: ties keep the incumbent order", {
  # one parent connected to every child: all permutations tie, so the input
  # order must come back untouched
  layer_nodes <- list("a", c("q", "p", "z"))
  edges <- data.frame(name = c("a", "a", "a"), to = c("q", "p", "z"))
  layer_assign <- c(a = 0L, q = 1L, p = 1L, z = 1L)

  out <- order_layers(layer_nodes, edges, layer_assign)
  expect_identical(out$layer_nodes[[2]], c("q", "p", "z"))

  # no directed edges at all: every layer is one big tie
  iso_nodes <- list(c("m", "k"), c("w", "u", "v"))
  iso_edges <- data.frame(
    name = c("m", "k", "w", "u", "v"),
    to = NA_character_
  )
  iso_assign <- c(m = 0L, k = 0L, w = 1L, u = 1L, v = 1L)

  iso_out <- order_layers(iso_nodes, iso_edges, iso_assign)
  expect_identical(iso_out$layer_nodes, iso_nodes)
})

test_that("order_layers: rows with to = NA are ignored", {
  edges <- parse_edges(c("x->m", "m->y", "x->y"))
  layer_assign <- longest_path_layers(edges)
  layer_nodes <- layers_from_assign(layer_assign)
  with_na <- rbind(edges, data.frame(name = "y", to = NA_character_))

  expect_identical(
    order_layers(layer_nodes, edges, layer_assign)$layer_nodes,
    order_layers(layer_nodes, with_na, layer_assign)$layer_nodes
  )
})

# order_layers: exact refinement -----------------------------------------------

test_that("order_layers: exact refinement is locally optimal per layer", {
  edges <- data.frame(
    name = c("a", "a", "b", "b", "c", "d", "e", "e"),
    to = c("c", "e", "c", "d", "g", "f", "g", "f")
  )
  layer_assign <- c(a = 0L, b = 0L, c = 1L, d = 1L, e = 1L, f = 2L, g = 2L)
  # scrambled incumbent with 4 crossings
  layer_nodes <- list(c("b", "a"), c("e", "c", "d"), c("g", "f"))
  expect_identical(count_crossings(layer_nodes, edges, layer_assign), 4L)

  out <- order_layers(
    layer_nodes,
    edges,
    layer_assign,
    sweeps = 8L,
    exact_max = 7L,
    max_refine_passes = 10L
  )
  achieved <- count_crossings(out$layer_nodes, edges, layer_assign)
  expect_lte(achieved, 4L)

  # every layer is at a permutation optimum holding the neighbor layers fixed
  for (i in seq_along(out$layer_nodes)) {
    for (perm in all_orderings(out$layer_nodes[[i]])) {
      candidate <- out$layer_nodes
      candidate[[i]] <- perm
      expect_gte(
        count_crossings(candidate, edges, layer_assign),
        achieved
      )
    }
  }
})

test_that("order_layers: two-layer bipartite reaches the known optimum", {
  # complete bipartite K2,2: every ordering has exactly one crossing
  k22_nodes <- list(c("a", "b"), c("x", "y"))
  k22_edges <- data.frame(
    name = c("a", "a", "b", "b"),
    to = c("x", "y", "x", "y")
  )
  k22_assign <- c(a = 0L, b = 0L, x = 1L, y = 1L)

  k22_out <- order_layers(k22_nodes, k22_edges, k22_assign)
  expect_identical(
    count_crossings(k22_out$layer_nodes, k22_edges, k22_assign),
    1L
  )

  # reversed matching: incumbent has 3 crossings, the optimum has none
  match_nodes <- list(c("a", "b", "c"), c("x", "y", "z"))
  match_edges <- data.frame(name = c("a", "b", "c"), to = c("z", "y", "x"))
  match_assign <- c(a = 0L, b = 0L, c = 0L, x = 1L, y = 1L, z = 1L)
  expect_identical(
    count_crossings(match_nodes, match_edges, match_assign),
    3L
  )

  match_out <- order_layers(match_nodes, match_edges, match_assign)
  expect_identical(
    count_crossings(match_out$layer_nodes, match_edges, match_assign),
    0L
  )
  expect_identical(
    count_crossings_bilayer(
      match_out$layer_nodes[[1]],
      match_out$layer_nodes[[2]],
      match_edges
    ),
    0L
  )
})

# order_layers: dummy augmentation ---------------------------------------------

test_that("order_layers: dummies bring skip edges into the objective", {
  # a -> m -> t is one chain, s -> b a skip edge past the middle layer. The
  # skip edge is invisible to the adjacent-pair objective without dummies, so
  # only dummy augmentation can rescue the b/t order.
  layer_nodes <- list(c("a", "s"), "m", c("b", "t"))
  edges <- data.frame(name = c("a", "m", "s"), to = c("m", "t", "b"))
  layer_assign <- c(a = 0L, s = 0L, m = 1L, b = 2L, t = 2L)

  with_dummies <- order_layers(layer_nodes, edges, layer_assign)
  without <- order_layers(
    layer_nodes,
    edges,
    layer_assign,
    use_dummies = FALSE
  )

  # with dummies the straight-line drawing of the result is crossing free
  expect_identical(
    count_edge_crossings(rank_coords(with_dummies$layer_nodes), edges),
    0L
  )

  # without dummies every layer ties, the incumbent survives, and the skip
  # edge crosses m -> t when drawn straight
  expect_identical(without$layer_nodes, layer_nodes)
  expect_identical(
    count_edge_crossings(rank_coords(without$layer_nodes), edges),
    1L
  )
})

test_that("order_layers: augmented view carries the dummy chain", {
  layer_nodes <- list(c("a", "s"), "m", c("b", "t"))
  edges <- data.frame(name = c("a", "m", "s"), to = c("m", "t", "b"))
  layer_assign <- c(a = 0L, s = 0L, m = 1L, b = 2L, t = 2L)

  aug <- order_layers(layer_nodes, edges, layer_assign)$augmented

  aug_names <- unlist(aug$layer_nodes)
  dummies <- aug_names[startsWith(aug_names, ".ggdag_dummy_")]
  expect_length(dummies, 1L)
  expect_true(dummies %in% aug$layer_nodes[[2]])
  expect_identical(aug$layer_assign[[dummies]], 1L)

  # the chain s -> dummy -> b replaces the skip edge
  keys <- directed_edge_keys(aug$edges)
  expect_true(all(
    c(paste0("s->", dummies), paste0(dummies, "->b")) %in% keys
  ))
  expect_false("s->b" %in% keys)

  # every augmented directed edge spans exactly one layer
  directed <- aug$edges[!is.na(aug$edges$to), , drop = FALSE]
  spans <- aug$layer_assign[directed$to] - aug$layer_assign[directed$name]
  expect_true(all(spans == 1L))

  # real nodes keep their layers in the augmented assignment
  expect_identical(aug$layer_assign[names(layer_assign)], layer_assign)
})

test_that("order_layers: max_dummies = 0 behaves as use_dummies = FALSE", {
  layer_nodes <- list(c("a", "s"), "m", c("b", "t"))
  edges <- data.frame(name = c("a", "m", "s"), to = c("m", "t", "b"))
  layer_assign <- c(a = 0L, s = 0L, m = 1L, b = 2L, t = 2L)

  capped <- order_layers(layer_nodes, edges, layer_assign, max_dummies = 0L)
  without <- order_layers(
    layer_nodes,
    edges,
    layer_assign,
    use_dummies = FALSE
  )

  expect_identical(capped$layer_nodes, without$layer_nodes)
})

# order_layers: bidirected adjacency -------------------------------------------

test_that("order_layers: bidirected pairs end adjacent when free to move", {
  # u <-> v separated by an unrelated node with no crossing consequences
  layer_nodes <- list(c("u", "z", "v"))
  edges <- data.frame(name = c("u", "z", "v"), to = NA_character_)
  layer_assign <- c(u = 0L, z = 0L, v = 0L)

  out <- order_layers(
    layer_nodes,
    edges,
    layer_assign,
    bidirected_pairs = data.frame(name = "u", to = "v")
  )

  ordering <- out$layer_nodes[[1]]
  expect_setequal(ordering, c("u", "z", "v"))
  expect_identical(abs(match("u", ordering) - match("v", ordering)), 1L)
})

test_that("order_layers: adjacency penalty never outvotes a crossing", {
  # z is pulled to the middle by its two parents; putting the bidirected pair
  # u <-> v adjacent costs a full crossing but saves only 0.5 in penalty, so
  # z must stay between them
  layer_nodes <- list(c("p", "q"), c("u", "v", "z"))
  edges <- data.frame(
    name = c("p", "p", "q", "q"),
    to = c("u", "z", "z", "v")
  )
  layer_assign <- c(p = 0L, q = 0L, u = 1L, v = 1L, z = 1L)

  out <- order_layers(
    layer_nodes,
    edges,
    layer_assign,
    bidirected_pairs = data.frame(name = "u", to = "v")
  )

  second <- out$layer_nodes[[2]]
  pos <- match(c("u", "z", "v"), second)
  expect_true(pos[2] > min(pos[1], pos[3]))
  expect_true(pos[2] < max(pos[1], pos[3]))
  expect_identical(count_crossings(out$layer_nodes, edges, layer_assign), 0L)
})

# order_layers: wide layers ----------------------------------------------------

test_that("order_layers: wide layers fall back deterministically", {
  # both layers exceed the default exact_max of 7, exercising the fallback:
  # the contract is no worsening and determinism, not exactness
  left <- paste0("s", seq_len(8))
  right <- paste0("t", seq_len(8))
  edges <- rbind(
    data.frame(name = left, to = rev(right)),
    data.frame(name = c("s1", "s4", "s7"), to = c("t5", "t8", "t2"))
  )
  layer_assign <- stats::setNames(
    c(rep(0L, 8), rep(1L, 8)),
    c(left, right)
  )
  layer_nodes <- list(left, right)
  incumbent <- count_crossings(layer_nodes, edges, layer_assign)
  expect_gt(incumbent, 0L)

  if (!exists(".Random.seed", envir = globalenv())) {
    set.seed(1)
  }
  seed_before <- get(".Random.seed", envir = globalenv())

  out1 <- order_layers(layer_nodes, edges, layer_assign)
  out2 <- order_layers(layer_nodes, edges, layer_assign)

  expect_identical(out1, out2)
  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)

  expect_lte(
    count_crossings(out1$layer_nodes, edges, layer_assign),
    incumbent
  )
  expect_setequal(out1$layer_nodes[[1]], left)
  expect_setequal(out1$layer_nodes[[2]], right)
})

# order_layers: canonical DAGs -------------------------------------------------

test_that("order_layers: never worse than barycenter_sort on canonical DAGs", {
  for (nm in names(canonical_specs)) {
    edges <- parse_edges(canonical_specs[[nm]])
    layer_assign <- longest_path_layers(edges)
    layer_nodes <- layers_from_assign(layer_assign)

    bary <- barycenter_sort(layer_nodes, edges, layer_assign)
    out <- order_layers(layer_nodes, edges, layer_assign)

    expect_lte(
      count_crossings(out$layer_nodes, edges, layer_assign),
      count_crossings(bary, edges, layer_assign),
      label = paste0(nm, ": crossings after order_layers"),
      expected.label = paste0(nm, ": crossings after barycenter_sort")
    )

    # shape preservation: every real node exactly once, in its own layer,
    # with no dummy names leaking out
    expect_identical(
      sort(unlist(out$layer_nodes)),
      sort(names(layer_assign)),
      label = paste0(nm, ": node multiset")
    )
    for (i in seq_along(layer_nodes)) {
      expect_setequal(out$layer_nodes[[i]], layer_nodes[[i]])
    }
    expect_false(
      any(startsWith(unlist(out$layer_nodes), ".ggdag_dummy_")),
      label = paste0(nm, ": dummy names in real layer_nodes")
    )
  }
})
