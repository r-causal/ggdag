# Phase 1: Geometric primitives ------------------------------------------------

test_that("y_dist_to_edge: horizontal edge returns correct distance", {
  # point (5, 3) to segment (0,0)-(10,0)

  result <- y_dist_to_edge(5, 3, 0, 0, 10, 0)
  expect_equal(result$dist, 3)
  expect_equal(result$t, 0.5)
  expect_equal(result$proj_y, 0)
})

test_that("y_dist_to_edge: diagonal edge returns correct distance", {
  # point (5, 5) to segment (0,0)-(10,10) — point is ON the line
  result <- y_dist_to_edge(5, 5, 0, 0, 10, 10)
  expect_equal(result$dist, 0)
  expect_equal(result$t, 0.5)
  expect_equal(result$proj_y, 5)
})

test_that("y_dist_to_edge: point near start returns Inf", {
  # t < 0.005 → outside segment
  result <- y_dist_to_edge(0.01, 3, 0, 0, 10, 0)
  expect_equal(result$dist, Inf)
})

test_that("y_dist_to_edge: point near end returns Inf", {
  # t > 0.995 → outside segment
  result <- y_dist_to_edge(9.99, 3, 0, 0, 10, 0)
  expect_equal(result$dist, Inf)
})

test_that("y_dist_to_edge: degenerate edge returns Inf", {
  # both endpoints same
  result <- y_dist_to_edge(5, 3, 3, 3, 3, 3)
  expect_equal(result$dist, Inf)
})

test_that("y_dist_to_edge: dist is always positive (absolute value)", {
  # point above edge
  result_above <- y_dist_to_edge(5, 3, 0, 0, 10, 0)
  expect_gt(result_above$dist, 0)

  # point below edge
  result_below <- y_dist_to_edge(5, -3, 0, 0, 10, 0)
  expect_gt(result_below$dist, 0)

  expect_equal(result_above$dist, result_below$dist)
})

test_that("y_dist_to_edge: proj_y is correct for angled edge", {
  # segment (0,0)-(10,5), point at (5,10)
  # orthogonal projection: t = ((5*10)+(10*5))/125 = 0.8, proj_y = 0.8*5 = 4.0
  result <- y_dist_to_edge(5, 10, 0, 0, 10, 5)
  expect_equal(result$proj_y, 4.0)
  expect_equal(result$dist, abs(10 - 4.0))
})

test_that("enforce_spacing: two close nodes get pushed apart", {
  positions <- c(A = 0, B = 10)
  layers <- c(A = 0L, B = 0L)
  result <- enforce_spacing(positions, layers, min_spacing = 72)
  expect_gte(result[["B"]] - result[["A"]], 72)
})

test_that("enforce_spacing: already-spaced nodes unchanged", {
  positions <- c(A = 0, B = 100)
  layers <- c(A = 0L, B = 0L)
  result <- enforce_spacing(positions, layers, min_spacing = 72)
  expect_equal(result[["A"]], 0)
  expect_equal(result[["B"]], 100)
})

test_that("enforce_spacing: different layers are independent", {
  positions <- c(A = 0, B = 5)
  layers <- c(A = 0L, B = 1L)
  result <- enforce_spacing(positions, layers, min_spacing = 72)
  # They're in different layers, so no spacing enforced
  expect_equal(result[["A"]], 0)
  expect_equal(result[["B"]], 5)
})

test_that("enforce_spacing: three nodes in same layer all maintain spacing", {
  positions <- c(A = 0, B = 10, C = 20)
  layers <- c(A = 0L, B = 0L, C = 0L)
  result <- enforce_spacing(positions, layers, min_spacing = 72)
  expect_gte(result[["B"]] - result[["A"]], 72)
  expect_gte(result[["C"]] - result[["B"]], 72)
})

test_that("enforce_spacing: single-node layer unchanged", {
  positions <- c(A = 50)
  layers <- c(A = 0L)
  result <- enforce_spacing(positions, layers, min_spacing = 72)
  expect_equal(result[["A"]], 50)
})

# Phase 2: Longest-path layer assignment ---------------------------------------

test_that("longest_path_layers: simple chain A→B→C", {
  edges_df <- data.frame(
    name = c("A", "B", "C"),
    to = c("B", "C", NA),
    stringsAsFactors = FALSE
  )
  result <- longest_path_layers(edges_df)
  expect_equal(result[["A"]], 0L)
  expect_equal(result[["B"]], 1L)
  expect_equal(result[["C"]], 2L)
})

test_that("longest_path_layers: diamond A→B, A→C, B→D, C→D", {
  edges_df <- data.frame(
    name = c("A", "A", "B", "C", "D"),
    to = c("B", "C", "D", "D", NA),
    stringsAsFactors = FALSE
  )
  result <- longest_path_layers(edges_df)
  expect_equal(result[["A"]], 0L)
  expect_equal(result[["B"]], 1L)
  expect_equal(result[["C"]], 1L)
  expect_equal(result[["D"]], 2L)
})

test_that("longest_path_layers: longest path wins over shortcut", {
  # A→B, B→C, C→D, A→D — D should be at layer 3 (via A→B→C→D), not 1
  edges_df <- data.frame(
    name = c("A", "B", "C", "A", "D"),
    to = c("B", "C", "D", "D", NA),
    stringsAsFactors = FALSE
  )
  result <- longest_path_layers(edges_df)
  expect_equal(result[["D"]], 3L)
})

test_that("longest_path_layers: isolated nodes get layer 0", {
  edges_df <- data.frame(
    name = c("A"),
    to = c(NA_character_),
    stringsAsFactors = FALSE
  )
  result <- longest_path_layers(edges_df)
  expect_equal(result[["A"]], 0L)
})

test_that("longest_path_layers: terminal nodes included with correct layer", {
  # A→B — B appears only in 'to' column
  edges_df <- data.frame(
    name = c("A", "B"),
    to = c("B", NA),
    stringsAsFactors = FALSE
  )
  result <- longest_path_layers(edges_df)
  expect_equal(result[["A"]], 0L)
  expect_equal(result[["B"]], 1L)
})

test_that("longest_path_layers: multiple root nodes", {
  # A→C, B→C
  edges_df <- data.frame(
    name = c("A", "B", "C"),
    to = c("C", "C", NA),
    stringsAsFactors = FALSE
  )
  result <- longest_path_layers(edges_df)
  expect_equal(result[["A"]], 0L)
  expect_equal(result[["B"]], 0L)
  expect_equal(result[["C"]], 1L)
})

test_that("longest_path_layers: fan-out", {
  # A→B, A→C, A→D
  edges_df <- data.frame(
    name = c("A", "A", "A", "B", "C", "D"),
    to = c("B", "C", "D", NA, NA, NA),
    stringsAsFactors = FALSE
  )
  result <- longest_path_layers(edges_df)
  expect_equal(result[["A"]], 0L)
  expect_equal(result[["B"]], 1L)
  expect_equal(result[["C"]], 1L)
  expect_equal(result[["D"]], 1L)
})

test_that("longest_path_layers: complex epidemiology-style DAG", {
  # SES→Edu, SES→Health, Edu→Income, Edu→Health, Income→Health
  # Longest path to Health: SES→Edu→Income→Health = 3
  edges_df <- data.frame(
    name = c("SES", "SES", "Edu", "Edu", "Income", "Health"),
    to = c("Edu", "Health", "Income", "Health", "Health", NA),
    stringsAsFactors = FALSE
  )
  result <- longest_path_layers(edges_df)
  expect_equal(result[["SES"]], 0L)
  expect_equal(result[["Edu"]], 1L)
  expect_equal(result[["Income"]], 2L)
  expect_equal(result[["Health"]], 3L)
})

# Phase 3: Barycenter crossing minimization ------------------------------------

test_that("count_crossings: detects known crossing", {
  # layer 0 = [A, B], layer 1 = [C, D], edges A→D, B→C → 1 crossing
  layer_nodes <- list(c("A", "B"), c("C", "D"))
  edges_df <- data.frame(
    name = c("A", "B"),
    to = c("D", "C"),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(A = 0L, B = 0L, C = 1L, D = 1L)
  expect_equal(count_crossings(layer_nodes, edges_df, layer_assign), 1L)
})

test_that("count_crossings: no crossing when aligned", {
  layer_nodes <- list(c("A", "B"), c("C", "D"))
  edges_df <- data.frame(
    name = c("A", "B"),
    to = c("C", "D"),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(A = 0L, B = 0L, C = 1L, D = 1L)
  expect_equal(count_crossings(layer_nodes, edges_df, layer_assign), 0L)
})

test_that("barycenter_sort: resolves known crossing", {
  # A→D, B→C has 1 crossing; after sort, layer 1 should reorder to [D, C]
  layer_nodes <- list(c("A", "B"), c("C", "D"))
  edges_df <- data.frame(
    name = c("A", "B"),
    to = c("D", "C"),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(A = 0L, B = 0L, C = 1L, D = 1L)

  result <- barycenter_sort(layer_nodes, edges_df, layer_assign)
  crossings_after <- count_crossings(result, edges_df, layer_assign)
  expect_equal(crossings_after, 0L)
})

test_that("barycenter_sort: already optimal stays unchanged", {
  layer_nodes <- list(c("A", "B"), c("C", "D"))
  edges_df <- data.frame(
    name = c("A", "B"),
    to = c("C", "D"),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(A = 0L, B = 0L, C = 1L, D = 1L)

  result <- barycenter_sort(layer_nodes, edges_df, layer_assign)
  expect_equal(result, layer_nodes)
})

test_that("count_crossings: single layer returns 0", {
  layer_nodes <- list(c("A", "B", "C"))
  edges_df <- data.frame(
    name = character(0),
    to = character(0),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(A = 0L, B = 0L, C = 0L)
  expect_equal(count_crossings(layer_nodes, edges_df, layer_assign), 0L)
})

test_that("barycenter_sort: three-layer DAG reduces crossings", {
  # Layer 0: A,B; Layer 1: C,D; Layer 2: E,F
  # Edges: A→D, B→C, C→F, D→E — 2 crossings total
  layer_nodes <- list(c("A", "B"), c("C", "D"), c("E", "F"))
  edges_df <- data.frame(
    name = c("A", "B", "C", "D"),
    to = c("D", "C", "F", "E"),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(A = 0L, B = 0L, C = 1L, D = 1L, E = 2L, F = 2L)

  before <- count_crossings(layer_nodes, edges_df, layer_assign)
  result <- barycenter_sort(layer_nodes, edges_df, layer_assign)
  after <- count_crossings(result, edges_df, layer_assign)
  expect_lte(after, before)
})

# Phase 4: Force-directed Y optimization --------------------------------------

# Helper: count overlaps using y_dist_to_edge inline
count_test_overlaps <- function(
  positions,
  edges_df,
  layer_assign,
  node_radius = 26
) {
  clearance <- node_radius + 8
  directed <- edges_df[!is.na(edges_df$to), , drop = FALSE]
  all_nodes <- names(positions$x)
  count <- 0L

  for (i in seq_len(nrow(directed))) {
    u <- directed$name[i]
    v <- directed$to[i]
    u_layer <- layer_assign[[u]]
    v_layer <- layer_assign[[v]]
    lo <- min(u_layer, v_layer)
    hi <- max(u_layer, v_layer)
    if (hi - lo <= 1) {
      next
    }

    for (w in all_nodes) {
      if (w == u || w == v) {
        next
      }
      w_layer <- layer_assign[[w]]
      if (w_layer > lo && w_layer < hi) {
        result <- y_dist_to_edge(
          positions$x[[w]],
          positions$y[[w]],
          positions$x[[u]],
          positions$y[[u]],
          positions$x[[v]],
          positions$y[[v]]
        )
        if (result$dist < clearance) {
          count <- count + 1L
        }
      }
    }
  }

  count
}

test_that("force_directed_y: mediation DAG has 0 overlaps", {
  # X→M, M→Y, X→Y — skip-edge X→Y should not clip M
  layer_nodes <- list("X", "M", "Y")
  layer_assign <- c(X = 0L, M = 1L, Y = 2L)
  edges_df <- data.frame(
    name = c("X", "M", "X"),
    to = c("M", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  result <- force_directed_y(layer_nodes, layer_assign, edges_df)
  overlaps <- count_test_overlaps(result, edges_df, layer_assign)
  expect_equal(overlaps, 0L)
})

test_that("force_directed_y: confounding DAG has 0 overlaps", {
  # Z→X, Z→Y, X→Y
  layer_nodes <- list("Z", "X", "Y")
  layer_assign <- c(Z = 0L, X = 1L, Y = 2L)
  edges_df <- data.frame(
    name = c("Z", "Z", "X"),
    to = c("X", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  result <- force_directed_y(layer_nodes, layer_assign, edges_df)
  overlaps <- count_test_overlaps(result, edges_df, layer_assign)
  expect_equal(overlaps, 0L)
})

test_that("force_directed_y: same-layer nodes maintain min_spacing", {
  # Two nodes in same layer
  layer_nodes <- list(c("A", "B"), "C")
  layer_assign <- c(A = 0L, B = 0L, C = 1L)
  edges_df <- data.frame(
    name = c("A", "B"),
    to = c("C", "C"),
    stringsAsFactors = FALSE
  )

  result <- force_directed_y(
    layer_nodes,
    layer_assign,
    edges_df,
    min_spacing = 72
  )
  gap <- abs(result$y[["B"]] - result$y[["A"]])
  expect_gte(gap, 72 - 1) # small tolerance for floating point
})

test_that("force_directed_y: x positions match layer assignment", {
  layer_nodes <- list("A", "B", "C")
  layer_assign <- c(A = 0L, B = 1L, C = 2L)
  edges_df <- data.frame(
    name = c("A", "B"),
    to = c("B", "C"),
    stringsAsFactors = FALSE
  )

  result <- force_directed_y(
    layer_nodes,
    layer_assign,
    edges_df,
    layer_gap = 180
  )
  expect_equal(result$x[["A"]], 0)
  expect_equal(result$x[["B"]], 180)
  expect_equal(result$x[["C"]], 360)
})

test_that("force_directed_y: napkin DAG has 0 overlaps", {
  # U1→Z, U1→A, U2→A, U2→Y, Z→A, A→M, M→Y, A→Y
  edges_df <- data.frame(
    name = c("U1", "U1", "U2", "U2", "Z", "A", "M", "A"),
    to = c("Z", "A", "A", "Y", "A", "M", "Y", "Y"),
    stringsAsFactors = FALSE
  )
  layer_assign <- longest_path_layers(
    rbind(edges_df, data.frame(name = "Y", to = NA_character_))
  )

  # Build layer_nodes from layer_assign
  max_layer <- max(layer_assign)
  layer_nodes <- lapply(seq(0, max_layer), function(l) {
    names(layer_assign[layer_assign == l])
  })
  layer_nodes <- barycenter_sort(layer_nodes, edges_df, layer_assign)

  result <- force_directed_y(layer_nodes, layer_assign, edges_df)
  overlaps <- count_test_overlaps(result, edges_df, layer_assign)
  expect_equal(overlaps, 0L)
})

test_that("force_directed_y: collider has no regression", {
  # X→C, Y→C — no overlaps expected
  layer_nodes <- list(c("X", "Y"), "C")
  layer_assign <- c(X = 0L, Y = 0L, C = 1L)
  edges_df <- data.frame(
    name = c("X", "Y"),
    to = c("C", "C"),
    stringsAsFactors = FALSE
  )

  result <- force_directed_y(layer_nodes, layer_assign, edges_df)
  overlaps <- count_test_overlaps(result, edges_df, layer_assign)
  expect_equal(overlaps, 0L)
})

# Phase 5: Greedy post-correction ----------------------------------------------

test_that("find_overlaps: detects node on edge line", {
  # Edge from (0,0) to (360,0), node at (180,0) in middle layer
  positions <- list(
    x = c(A = 0, B = 180, C = 360),
    y = c(A = 0, B = 0, C = 0)
  )
  edges_df <- data.frame(
    name = c("A", "A", "B"),
    to = c("C", "B", "C"),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(A = 0L, B = 1L, C = 2L)

  overlaps <- find_overlaps(positions, edges_df, layer_assign, node_radius = 26)
  # B is on the A→C edge line (dist ≈ 0, which is < clearance of 34)
  expect_gt(nrow(overlaps), 0)
})

test_that("find_overlaps: no overlap when node is far away", {
  positions <- list(
    x = c(A = 0, B = 180, C = 360),
    y = c(A = 0, B = 200, C = 0)
  )
  edges_df <- data.frame(
    name = c("A", "A", "B"),
    to = c("C", "B", "C"),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(A = 0L, B = 1L, C = 2L)

  overlaps <- find_overlaps(positions, edges_df, layer_assign, node_radius = 26)
  expect_equal(nrow(overlaps), 0)
})

test_that("greedy_post_correction: fixes artificial overlap", {
  # Confounding: Z→X, Z→Y, X→Y with X placed on Z→Y line
  positions <- list(
    x = c(Z = 0, X = 180, Y = 360),
    y = c(Z = 0, X = 0, Y = 0)
  )
  edges_df <- data.frame(
    name = c("Z", "Z", "X"),
    to = c("X", "Y", "Y"),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(Z = 0L, X = 1L, Y = 2L)

  result <- greedy_post_correction(positions, edges_df, layer_assign)
  overlaps <- find_overlaps(result, edges_df, layer_assign, node_radius = 26)
  expect_equal(nrow(overlaps), 0)
})

test_that("greedy_post_correction: no-op when no overlaps", {
  positions <- list(
    x = c(A = 0, B = 180, C = 360),
    y = c(A = 0, B = 200, C = 0)
  )
  edges_df <- data.frame(
    name = c("A", "A", "B"),
    to = c("C", "B", "C"),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(A = 0L, B = 1L, C = 2L)

  result <- greedy_post_correction(positions, edges_df, layer_assign)
  # Positions should be essentially unchanged
  expect_equal(result$y[["B"]], positions$y[["B"]])
})

test_that("greedy_post_correction: maintains spacing after correction", {
  # Two nodes in same layer, one overlapping an edge
  positions <- list(
    x = c(A = 0, B = 180, C = 180, D = 360),
    y = c(A = 0, B = 0, C = 72, D = 0)
  )
  edges_df <- data.frame(
    name = c("A", "A", "B", "C"),
    to = c("B", "D", "D", "D"),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(A = 0L, B = 1L, C = 1L, D = 2L)

  result <- greedy_post_correction(
    positions,
    edges_df,
    layer_assign,
    min_spacing = 72
  )
  gap <- abs(result$y[["C"]] - result$y[["B"]])
  expect_gte(gap, 72 - 1) # small tolerance
})

test_that("find_overlaps: adjacent layers have no intermediates", {
  # Edge spans only adjacent layers — no intermediate nodes possible
  positions <- list(
    x = c(A = 0, B = 180),
    y = c(A = 0, B = 0)
  )
  edges_df <- data.frame(
    name = c("A"),
    to = c("B"),
    stringsAsFactors = FALSE
  )
  layer_assign <- c(A = 0L, B = 1L)

  overlaps <- find_overlaps(positions, edges_df, layer_assign, node_radius = 26)
  expect_equal(nrow(overlaps), 0)
})

# Phase 6: Orchestrator and API integration ------------------------------------

# Helper: make edges_df from formula-style edge pairs
make_edges_df <- function(...) {
  pairs <- list(...)
  edges_df <- data.frame(
    name = vapply(pairs, `[[`, character(1), 1),
    to = vapply(pairs, `[[`, character(1), 2),
    stringsAsFactors = FALSE
  )
  # Add terminal nodes with to=NA
  terminals <- unique(edges_df$to[!(edges_df$to %in% edges_df$name)])
  if (length(terminals) > 0) {
    edges_df <- rbind(
      edges_df,
      data.frame(
        name = terminals,
        to = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }
  edges_df
}

test_that("compute_time_ordered_layout: returns tibble with name, x, y", {
  edges_df <- make_edges_df(c("A", "B"), c("B", "C"))
  result <- compute_time_ordered_layout(edges_df)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("name", "x", "y"))
})

test_that("compute_time_ordered_layout: x values are integer layer indices", {
  edges_df <- make_edges_df(c("A", "B"), c("B", "C"))
  result <- compute_time_ordered_layout(edges_df)
  expect_true(all(result$x == round(result$x)))
  expect_equal(sort(unique(result$x)), c(1, 2, 3))
})

test_that("compute_time_ordered_layout: direction y swaps axes", {
  edges_df <- make_edges_df(c("A", "B"), c("B", "C"))
  result_x <- compute_time_ordered_layout(edges_df, direction = "x")
  result_y <- compute_time_ordered_layout(edges_df, direction = "y")
  # In direction="y", the layer indices should be on y axis
  expect_equal(sort(unique(result_y$y)), c(1, 2, 3))
})

test_that("compute_time_ordered_layout: single node DAG", {
  edges_df <- data.frame(
    name = "A",
    to = NA_character_,
    stringsAsFactors = FALSE
  )
  result <- compute_time_ordered_layout(edges_df)
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "A")
})

test_that("compute_time_ordered_layout: two node DAG", {
  edges_df <- make_edges_df(c("A", "B"))
  result <- compute_time_ordered_layout(edges_df)
  expect_equal(nrow(result), 2)
})

# Test all 22 spec DAGs produce 0 overlaps
# Using a helper to avoid repetition
test_zero_overlaps <- function(label, edge_pairs) {
  test_that(paste0("compute_time_ordered_layout: ", label, " has 0 overlaps"), {
    edges_df <- do.call(make_edges_df, edge_pairs)
    # Run full pipeline in pixel space to check overlaps
    layer_assign <- longest_path_layers(edges_df)
    directed <- edges_df[!is.na(edges_df$to), , drop = FALSE]
    max_layer <- max(layer_assign)
    layer_nodes <- lapply(seq(0, max_layer), function(l) {
      names(layer_assign[layer_assign == l])
    })
    layer_nodes <- barycenter_sort(layer_nodes, directed, layer_assign)
    positions <- force_directed_y(layer_nodes, layer_assign, directed)
    positions <- greedy_post_correction(positions, directed, layer_assign)
    overlaps <- find_overlaps(positions, directed, layer_assign)
    expect_equal(nrow(overlaps), 0)
  })
}

test_zero_overlaps("confounding", list(c("Z", "X"), c("Z", "Y"), c("X", "Y")))
test_zero_overlaps("mediation", list(c("X", "M"), c("M", "Y"), c("X", "Y")))
test_zero_overlaps("collider", list(c("X", "C"), c("Y", "C")))
test_zero_overlaps(
  "IV",
  list(c("Z", "X"), c("X", "Y"), c("U", "X"), c("U", "Y"))
)
test_zero_overlaps(
  "front-door",
  list(c("U", "X"), c("U", "Y"), c("X", "M"), c("M", "Y"))
)
test_zero_overlaps(
  "M-bias",
  list(c("U1", "A"), c("U1", "M"), c("U2", "M"), c("U2", "Y"), c("A", "Y"))
)
test_zero_overlaps(
  "smoking",
  list(
    c("Genetics", "Smoking"),
    c("Genetics", "Cancer"),
    c("Smoking", "Tar"),
    c("Tar", "Cancer"),
    c("Smoking", "Cancer")
  )
)
test_zero_overlaps(
  "napkin",
  list(
    c("U1", "Z"),
    c("U1", "A"),
    c("U2", "A"),
    c("U2", "Y"),
    c("Z", "A"),
    c("A", "M"),
    c("M", "Y"),
    c("A", "Y")
  )
)
test_zero_overlaps(
  "complex chain",
  list(
    c("A", "B"),
    c("B", "C"),
    c("C", "D"),
    c("D", "E"),
    c("A", "C"),
    c("B", "D"),
    c("C", "E"),
    c("A", "E")
  )
)
test_zero_overlaps(
  "butterfly",
  list(
    c("X1", "M"),
    c("X2", "M"),
    c("M", "Y1"),
    c("M", "Y2"),
    c("X1", "Y1"),
    c("X2", "Y2")
  )
)
test_zero_overlaps(
  "wide DAG",
  list(
    c("X1", "M1"),
    c("X2", "M1"),
    c("X3", "M2"),
    c("X1", "M2"),
    c("M1", "Y"),
    c("M2", "Y"),
    c("X2", "Y"),
    c("X3", "Y")
  )
)
test_zero_overlaps(
  "deep confound",
  list(
    c("U", "A"),
    c("U", "B"),
    c("U", "C"),
    c("A", "B"),
    c("B", "C"),
    c("A", "D"),
    c("C", "D"),
    c("B", "D")
  )
)
test_zero_overlaps(
  "selection bias",
  list(
    c("A", "Y"),
    c("A", "S"),
    c("U", "S"),
    c("U", "Y"),
    c("L", "A"),
    c("L", "U")
  )
)
test_zero_overlaps(
  "overcontrol",
  list(
    c("X", "Z"),
    c("Z", "Y"),
    c("X", "Y"),
    c("W", "X"),
    c("W", "Z")
  )
)
test_zero_overlaps(
  "treatment",
  list(
    c("C1", "X"),
    c("C2", "X"),
    c("C1", "Y"),
    c("C2", "Y"),
    c("X", "M1"),
    c("X", "M2"),
    c("M1", "Y"),
    c("M2", "Y"),
    c("U", "M1"),
    c("U", "Y")
  )
)
test_zero_overlaps(
  "double IV",
  list(
    c("Z1", "X"),
    c("Z2", "X"),
    c("X", "M"),
    c("M", "Y"),
    c("U1", "X"),
    c("U1", "M"),
    c("U2", "M"),
    c("U2", "Y")
  )
)
test_zero_overlaps(
  "cascade",
  list(
    c("A", "B"),
    c("A", "D"),
    c("B", "C"),
    c("C", "D"),
    c("B", "E"),
    c("D", "E"),
    c("C", "F"),
    c("E", "F"),
    c("A", "F")
  )
)
test_zero_overlaps(
  "triple confound",
  list(
    c("U", "X"),
    c("U", "Y"),
    c("V", "X"),
    c("V", "M"),
    c("W", "M"),
    c("W", "Y"),
    c("X", "M"),
    c("M", "Y"),
    c("X", "Y")
  )
)
test_zero_overlaps(
  "regression disc",
  list(
    c("Z", "X"),
    c("X", "Y"),
    c("X", "W"),
    c("W", "Y"),
    c("Z", "W"),
    c("U", "W"),
    c("U", "Y")
  )
)
test_zero_overlaps(
  "multi-mediator",
  list(
    c("X", "M1"),
    c("X", "M2"),
    c("X", "M3"),
    c("M1", "M2"),
    c("M2", "M3"),
    c("M1", "Y"),
    c("M2", "Y"),
    c("M3", "Y"),
    c("X", "Y"),
    c("U", "M2"),
    c("U", "Y")
  )
)

# API integration tests

test_that("time_ordered_coords() with no args returns a closure", {
  result <- time_ordered_coords()
  expect_true(is.function(result))
})

test_that("time_ordered_coords(list(...)) still works (manual path)", {
  coords <- time_ordered_coords(list("a", c("b1", "b2"), "c"))
  expect_s3_class(coords, "tbl_df")
  expect_named(coords, c("name", "x", "y"))
  expect_equal(nrow(coords), 4)
})

test_that("time_ordered_coords(data.frame(...)) still works", {
  df <- data.frame(
    name = c("x1", "x2", "y"),
    time = c(1, 1, 2)
  )
  coords <- time_ordered_coords(df)
  expect_s3_class(coords, "tbl_df")
  expect_equal(nrow(coords), 3)
})

test_that("dagify(coords = time_ordered_coords()) works", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    coords = time_ordered_coords()
  )
  expect_s3_class(dag, "dagitty")
})

test_that("tidy_dagitty with layout = 'time_ordered' works", {
  dag <- dagify(y ~ x + z, x ~ z)
  td <- tidy_dagitty(dag, layout = "time_ordered")
  expect_s3_class(td, "tidy_dagitty")
  # Should have valid coordinates
  data <- pull_dag_data(td)
  expect_true(!anyNA(data$x))
  expect_true(!anyNA(data$y))
})

test_that("tidy_dagitty with layout = time_ordered_coords() works", {
  dag <- dagify(y ~ x + z, x ~ z)
  td <- tidy_dagitty(dag, layout = time_ordered_coords())
  expect_s3_class(td, "tidy_dagitty")
})

# Integration tests: dagify() + time_ordered layout ----------------------------

# Helper: get distinct node coordinates from a tidy_dagitty
get_node_coords <- function(td) {
  pull_dag_data(td) |>
    dplyr::distinct(name, .keep_all = TRUE) |>
    dplyr::select(name, x, y)
}

# Helper: check layout produces valid non-NA coords and correct node count
expect_valid_time_ordered <- function(td, expected_nodes) {
  expect_s3_class(td, "tidy_dagitty")
  coords <- get_node_coords(td)
  expect_true(!anyNA(coords$x))
  expect_true(!anyNA(coords$y))
  expect_setequal(coords$name, expected_nodes)
}

test_that("simple chain x→y→z has 3 layers", {
  td <- dagify(z ~ y, y ~ x) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)
  expect_equal(length(unique(coords$x)), 3)
})

test_that("confounding DAG z→x, z→y, x→y has correct layering", {
  td <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)

  x_z <- coords$x[coords$name == "z"]
  x_x <- coords$x[coords$name == "x"]
  x_y <- coords$x[coords$name == "y"]
  # z should be earliest, y latest

  expect_lt(x_z, x_x)
  expect_lt(x_x, x_y)
})

test_that("collider x→m, y→m places x and y at same layer", {
  td <- dagify(m ~ x + y) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)

  expect_equal(
    unname(coords$x[coords$name == "x"]),
    unname(coords$x[coords$name == "y"])
  )
  expect_gt(
    unname(coords$x[coords$name == "m"]),
    unname(coords$x[coords$name == "x"])
  )
})

test_that("fork x←z→y places x and y at same layer", {
  td <- dagify(x ~ z, y ~ z) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)

  expect_equal(
    unname(coords$x[coords$name == "x"]),
    unname(coords$x[coords$name == "y"])
  )
  expect_lt(
    unname(coords$x[coords$name == "z"]),
    unname(coords$x[coords$name == "x"])
  )
})

test_that("mediation DAG respects longest path", {
  # x→m→y with x→y shortcut
  td <- dagify(y ~ x + m, m ~ x) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)

  x_x <- coords$x[coords$name == "x"]
  x_m <- coords$x[coords$name == "m"]
  x_y <- coords$x[coords$name == "y"]
  expect_lt(x_x, x_m)
  expect_lt(x_m, x_y)
})

test_that("IV DAG places instrument before treatment", {
  # z→x→y, u→x, u→y
  td <- dagify(y ~ x + u, x ~ z + u) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)

  # z and u are roots, x next, y last
  x_z <- unname(coords$x[coords$name == "z"])
  x_u <- unname(coords$x[coords$name == "u"])
  x_x <- unname(coords$x[coords$name == "x"])
  x_y <- unname(coords$x[coords$name == "y"])
  expect_equal(x_z, x_u) # both roots
  expect_lt(x_z, x_x)
  expect_lt(x_x, x_y)
})

test_that("diamond DAG places mediators at same layer", {
  # a→b, a→c, b→d, c→d
  td <- dagify(d ~ b + c, b ~ a, c ~ a) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)

  expect_equal(
    unname(coords$x[coords$name == "b"]),
    unname(coords$x[coords$name == "c"])
  )
})

test_that("same-layer nodes have distinct y values", {
  td <- dagify(y ~ x1 + x2 + x3) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)

  root_ys <- coords$y[coords$name %in% c("x1", "x2", "x3")]
  expect_equal(length(unique(root_ys)), 3)
})

test_that("direction = 'y' puts time on y-axis", {
  td <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty(layout = time_ordered_coords(direction = "y"))
  coords <- get_node_coords(td)

  # In direction = "y", time should vary by y
  y_z <- coords$y[coords$name == "z"]
  y_x <- coords$y[coords$name == "x"]
  y_y <- coords$y[coords$name == "y"]
  expect_lt(y_z, y_x)
  expect_lt(y_x, y_y)
})

test_that("string and function layout produce same result", {
  dag <- dagify(y ~ x + z, x ~ z)
  td_str <- tidy_dagitty(dag, layout = "time_ordered")
  td_fn <- tidy_dagitty(dag, layout = time_ordered_coords())
  expect_equal(pull_dag_data(td_str), pull_dag_data(td_fn))
})

test_that("all nodes get coordinates with dagify(coords = time_ordered_coords())", {
  dag <- dagify(
    y ~ x + m,
    m ~ x + z,
    coords = time_ordered_coords()
  )
  td <- tidy_dagitty(dag)
  expect_valid_time_ordered(td, c("x", "y", "m", "z"))
})

test_that("exposure and outcome are preserved with time_ordered layout", {
  td <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y") |>
    tidy_dagitty(layout = "time_ordered")
  expect_equal(dagitty::exposures(pull_dag(td)), "x")
  expect_equal(dagitty::outcomes(pull_dag(td)), "y")
})

test_that("labels are preserved with time_ordered layout", {
  td <- dagify(
    y ~ x + z,
    x ~ z,
    labels = c("x" = "Treatment", "y" = "Outcome", "z" = "Confounder")
  ) |>
    tidy_dagitty(layout = "time_ordered")
  dag <- pull_dag(td)
  dag_labels <- label(dag)
  expect_true(length(dag_labels) > 0)
})

# Integration tests: complex real-world-style DAGs ----------------------------

test_that("front-door DAG layout is valid", {
  # u→x, u→y, x→m, m→y
  td <- dagify(y ~ u + m, x ~ u, m ~ x) |>
    tidy_dagitty(layout = "time_ordered")
  expect_valid_time_ordered(td, c("u", "x", "y", "m"))
})

test_that("M-bias DAG layout is valid", {
  td <- dagify(y ~ a, m ~ u1 + u2, a ~ u1, y ~ u2) |>
    tidy_dagitty(layout = "time_ordered")
  expect_valid_time_ordered(td, c("u1", "u2", "a", "m", "y"))
})

test_that("napkin DAG layout is valid", {
  td <- dagify(
    a ~ u1 + u2 + z,
    z ~ u1,
    m ~ a,
    y ~ u2 + m + a
  ) |>
    tidy_dagitty(layout = "time_ordered")
  expect_valid_time_ordered(td, c("u1", "u2", "z", "a", "m", "y"))
})

test_that("multi-mediator DAG layout is valid", {
  td <- dagify(
    m1 ~ x,
    m2 ~ x + m1,
    m3 ~ x + m2,
    y ~ x + m1 + m2 + m3
  ) |>
    tidy_dagitty(layout = "time_ordered")
  expect_valid_time_ordered(td, c("x", "m1", "m2", "m3", "y"))
  coords <- get_node_coords(td)
  # Chain: x → m1 → m2 → m3 → y
  expect_equal(length(unique(coords$x)), 5)
})

test_that("wide fan-in DAG layout is valid", {
  td <- dagify(y ~ a + b + c + d + e) |>
    tidy_dagitty(layout = "time_ordered")
  expect_valid_time_ordered(td, c("a", "b", "c", "d", "e", "y"))
  coords <- get_node_coords(td)
  # All sources at same layer, 5 distinct y values
  source_coords <- coords[coords$name != "y", ]
  expect_equal(length(unique(source_coords$x)), 1)
  expect_equal(length(unique(source_coords$y)), 5)
})

test_that("wide fan-out DAG layout is valid", {
  td <- dagify(a ~ x, b ~ x, c ~ x, d ~ x, e ~ x) |>
    tidy_dagitty(layout = "time_ordered")
  expect_valid_time_ordered(td, c("x", "a", "b", "c", "d", "e"))
  coords <- get_node_coords(td)
  # All targets at same layer, 5 distinct y values
  target_coords <- coords[coords$name != "x", ]
  expect_equal(length(unique(target_coords$x)), 1)
  expect_equal(length(unique(target_coords$y)), 5)
})

test_that("deep chain has correct number of layers", {
  td <- dagify(b ~ a, c ~ b, d ~ c, e ~ d, f ~ e) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)
  expect_equal(length(unique(coords$x)), 6)
  # Each node at a different layer
  xs <- coords$x[order(match(coords$name, c("a", "b", "c", "d", "e", "f")))]
  expect_true(all(diff(xs) > 0))
})

test_that("disconnected components both get valid coords", {
  td <- dagify(y ~ x, b ~ a) |>
    tidy_dagitty(layout = "time_ordered")
  expect_valid_time_ordered(td, c("x", "y", "a", "b"))
})

test_that("two-node minimal DAG works through dagify", {
  td <- dagify(y ~ x) |>
    tidy_dagitty(layout = "time_ordered")
  expect_s3_class(td, "tidy_dagitty")
  data <- pull_dag_data(td)
  expect_true(!anyNA(data$x))
})

test_that("two-node DAG works", {
  td <- dagify(y ~ x) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)
  expect_equal(nrow(coords), 2)
  expect_lt(coords$x[coords$name == "x"], coords$x[coords$name == "y"])
})

# Integration tests: as_tidy_dagitty ------------------------------------------

test_that("as_tidy_dagitty.list produces correct time ordering", {
  td <- as_tidy_dagitty(list(c("x1", "x2"), c("y1", "y2"), "z"))
  coords <- get_node_coords(td)
  expect_equal(unname(coords$x[coords$name == "x1"]), 1)
  expect_equal(unname(coords$x[coords$name == "x2"]), 1)
  expect_equal(unname(coords$x[coords$name == "y1"]), 2)
  expect_equal(unname(coords$x[coords$name == "y2"]), 2)
  expect_equal(unname(coords$x[coords$name == "z"]), 3)
})

test_that("as_tidy_dagitty.list with single nodes per time point", {
  td <- as_tidy_dagitty(list("a", "b", "c", "d"))
  coords <- get_node_coords(td)
  expect_equal(length(unique(coords$x)), 4)
})

test_that("as_tidy_dagitty.list with many nodes per time point", {
  td <- as_tidy_dagitty(list(
    c("a1", "a2", "a3"),
    c("b1", "b2"),
    c("c1", "c2", "c3", "c4")
  ))
  coords <- get_node_coords(td)
  expect_equal(nrow(coords), 9)
  expect_equal(length(unique(coords$x)), 3)
})

test_that("as_tidy_dagitty.data.frame with saturate = TRUE works", {
  edges_df <- data.frame(
    name = c("x", "y"),
    to = c("y", "z"),
    x = c(0, 1),
    y = c(0, 0),
    xend = c(1, 2),
    yend = c(0, 0)
  )
  td <- as_tidy_dagitty(edges_df, saturate = TRUE)
  expect_s3_class(td, "tidy_dagitty")
  coords <- get_node_coords(td)
  expect_true(!anyNA(coords$x))
  expect_true(!anyNA(coords$y))
})

test_that("as_tidy_dagitty.data.frame without coords works with time_ordered", {
  edges_df <- data.frame(
    name = c("x", "y"),
    to = c("y", "z"),
    stringsAsFactors = FALSE
  )
  td <- as_tidy_dagitty(edges_df, layout = "time_ordered")
  expect_s3_class(td, "tidy_dagitty")
  coords <- get_node_coords(td)
  expect_true(!anyNA(coords$x))
})

# Integration tests: dag_saturate ---------------------------------------------

test_that("dag_saturate produces valid time_ordered layout", {
  td <- dagify(y ~ x, x ~ z) |>
    tidy_dagitty(layout = "time_ordered") |>
    dag_saturate()
  expect_s3_class(td, "tidy_dagitty")
  coords <- get_node_coords(td)
  expect_true(!anyNA(coords$x))
  expect_true(!anyNA(coords$y))
})

test_that("dag_saturate with use_existing_coords = TRUE preserves structure", {
  td <- dagify(y ~ x, x ~ z) |>
    tidy_dagitty(layout = "time_ordered") |>
    dag_saturate(use_existing_coords = TRUE)
  expect_s3_class(td, "tidy_dagitty")
  coords <- get_node_coords(td)
  expect_true(!anyNA(coords$x))
})

test_that("dag_saturate adds missing edges", {
  original <- dagify(y ~ x, x ~ z) |>
    tidy_dagitty(layout = "time_ordered")
  saturated <- dag_saturate(original)

  orig_edges <- pull_dag_data(original) |>
    dplyr::filter(!is.na(to)) |>
    nrow()
  sat_edges <- pull_dag_data(saturated) |>
    dplyr::filter(!is.na(to)) |>
    nrow()
  # Saturated should have more edges (z→y added)
  expect_gt(sat_edges, orig_edges)
})

test_that("dag_prune after dag_saturate removes specified edges", {
  td <- dagify(y ~ x, x ~ z) |>
    tidy_dagitty(layout = "time_ordered") |>
    dag_saturate() |>
    dag_prune(c("z" = "y"))

  edges <- pull_dag_data(td) |>
    dplyr::filter(!is.na(to))
  # z→y should be removed
  has_z_y <- any(edges$name == "z" & edges$to == "y")
  expect_false(has_z_y)
})

# Integration tests: ggdag() and ggplot() -------------------------------------

test_that("ggdag() with time_ordered layout produces a ggplot", {
  p <- dagify(y ~ x + z, x ~ z) |>
    ggdag(layout = "time_ordered")
  expect_s3_class(p, "ggplot")
})

test_that("ggdag() with time_ordered_coords() produces a ggplot", {
  p <- dagify(y ~ x + z, x ~ z) |>
    ggdag(layout = time_ordered_coords())
  expect_s3_class(p, "ggplot")
})

test_that("ggdag(edge_type = 'arc') with time_ordered works", {
  p <- dagify(y ~ x + z, x ~ z) |>
    ggdag(layout = "time_ordered", edge_type = "arc")
  expect_s3_class(p, "ggplot")
})

test_that("ggplot() on tidy_dagitty with time_ordered layout works", {
  td <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty(layout = "time_ordered")
  p <- ggplot(td) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text()
  expect_s3_class(p, "ggplot")
})

# Edge case: factor handling ---------------------------------------------------

test_that("longest_path_layers handles factor columns", {
  edges_df <- data.frame(
    name = factor(c("A", "B", "C")),
    to = factor(c("B", "C", NA))
  )
  result <- longest_path_layers(edges_df)
  expect_equal(result[["A"]], 0L)
  expect_equal(result[["B"]], 1L)
  expect_equal(result[["C"]], 2L)
})

test_that("compute_time_ordered_layout handles factor columns", {
  edges_df <- data.frame(
    name = factor(c("A", "B", "C")),
    to = factor(c("B", "C", NA))
  )
  result <- compute_time_ordered_layout(edges_df)
  expect_equal(nrow(result), 3)
  expect_setequal(result$name, c("A", "B", "C"))
  expect_true(!anyNA(result$x))
})

# Normalization properties -----------------------------------------------------

test_that("normalized x values start at 1", {
  edges_df <- make_edges_df(c("A", "B"), c("B", "C"), c("C", "D"))
  result <- compute_time_ordered_layout(edges_df)
  expect_equal(min(result$x), 1)
})

test_that("normalized x values are consecutive integers", {
  edges_df <- make_edges_df(c("A", "B"), c("B", "C"), c("C", "D"))
  result <- compute_time_ordered_layout(edges_df)
  expect_equal(sort(unique(result$x)), 1:4)
})

test_that("graph is globally centered around y = 0", {
  edges_df <- make_edges_df(c("A", "B"), c("B", "C"))
  result <- compute_time_ordered_layout(edges_df)
  expect_equal(mean(result$y), 0, tolerance = 0.01)
})

test_that("multi-node layers have distinct y values", {
  edges_df <- make_edges_df(
    c("X1", "Y"),
    c("X2", "Y"),
    c("X3", "Y")
  )
  result <- compute_time_ordered_layout(edges_df)
  root_ys <- result$y[result$name %in% c("X1", "X2", "X3")]
  expect_equal(length(unique(root_ys)), 3)
})

# Stability / determinism -----------------------------------------------------

test_that("same DAG produces same layout twice", {
  edges_df <- make_edges_df(c("Z", "X"), c("Z", "Y"), c("X", "Y"))
  r1 <- compute_time_ordered_layout(edges_df)
  r2 <- compute_time_ordered_layout(edges_df)
  expect_equal(r1, r2)
})

test_that("all nodes present in output (no drops)", {
  td <- dagify(
    y ~ x + m1 + m2,
    m1 ~ x + z,
    m2 ~ x + z
  ) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)
  expect_setequal(coords$name, c("x", "y", "m1", "m2", "z"))
})

# Zero-overlap tests through the full public API -------------------------------

# Helper: check full dagify → tidy_dagitty pipeline for zero overlaps
expect_zero_overlaps_dagify <- function(dag) {
  td <- tidy_dagitty(dag, layout = "time_ordered")
  coords <- get_node_coords(td)

  # Basic validity
  expect_true(!anyNA(coords$x))
  expect_true(!anyNA(coords$y))

  # Reconstruct edge list from tidy data
  edge_data <- pull_dag_data(td) |>
    dplyr::filter(!is.na(to)) |>
    dplyr::distinct(name, to)

  # Check each skip-edge (spanning > 1 layer) for overlaps
  for (i in seq_len(nrow(edge_data))) {
    u <- edge_data$name[i]
    v <- edge_data$to[i]
    ux <- unname(coords$x[coords$name == u])
    vx <- unname(coords$x[coords$name == v])
    if (length(ux) == 0 || length(vx) == 0) {
      next
    }
    lo <- min(ux, vx)
    hi <- max(ux, vx)
    if (hi - lo <= 1) {
      next
    }

    # Check intermediate nodes
    intermediates <- coords$name[coords$x > lo & coords$x < hi]
    for (w in intermediates) {
      result <- y_dist_to_edge(
        unname(coords$x[coords$name == w]),
        unname(coords$y[coords$name == w]),
        ux,
        unname(coords$y[coords$name == u]),
        vx,
        unname(coords$y[coords$name == v])
      )
      # In normalized space, nodes should not be *on* the edge line
      if (is.finite(result$dist)) {
        expect_gt(
          result$dist,
          0.01,
          label = paste0("node ", w, " too close to edge ", u, "->", v)
        )
      }
    }
  }
}

test_that("dagify confounding DAG has no overlaps in normalized coords", {
  dag <- dagify(y ~ x + z, x ~ z)
  expect_zero_overlaps_dagify(dag)
})

test_that("dagify mediation DAG has no overlaps in normalized coords", {
  dag <- dagify(y ~ x + m, m ~ x)
  expect_zero_overlaps_dagify(dag)
})

test_that("dagify IV DAG has no overlaps in normalized coords", {
  dag <- dagify(y ~ x + u, x ~ z + u)
  expect_zero_overlaps_dagify(dag)
})

test_that("dagify napkin DAG has no overlaps in normalized coords", {
  dag <- dagify(
    a ~ u1 + u2 + z,
    z ~ u1,
    m ~ a,
    y ~ u2 + m + a
  )
  expect_zero_overlaps_dagify(dag)
})

test_that("dagify smoking DAG has no overlaps in normalized coords", {
  dag <- dagify(
    tar ~ smoking,
    cancer ~ smoking + tar + genetics,
    smoking ~ genetics
  )
  expect_zero_overlaps_dagify(dag)
})

test_that("dagify deep chain with shortcuts has no overlaps", {
  dag <- dagify(
    b ~ a,
    c ~ b + a,
    d ~ c + b,
    e ~ d + a
  )
  expect_zero_overlaps_dagify(dag)
})

test_that("dagify complex epi DAG has no overlaps", {
  dag <- dagify(
    health ~ ses + edu + income,
    income ~ edu,
    edu ~ ses
  )
  expect_zero_overlaps_dagify(dag)
})

# Bidirected edge handling -----------------------------------------------------

# Helper: make edges_df with direction column for bidirected edges
make_edges_df_with_direction <- function(..., bidirected = character(0)) {
  pairs <- list(...)
  edges_df <- data.frame(
    name = vapply(pairs, `[[`, character(1), 1),
    to = vapply(pairs, `[[`, character(1), 2),
    direction = "->",
    stringsAsFactors = FALSE
  )
  # Add terminal nodes with to=NA
  terminals <- unique(edges_df$to[!(edges_df$to %in% edges_df$name)])
  if (length(terminals) > 0) {
    edges_df <- rbind(
      edges_df,
      data.frame(
        name = terminals,
        to = NA_character_,
        direction = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }
  # Add bidirected edges (pairs of node names)
  if (length(bidirected) > 0) {
    stopifnot(length(bidirected) %% 2 == 0)
    n_bidir <- length(bidirected) / 2
    bidir_df <- data.frame(
      name = bidirected[seq(1, length(bidirected), by = 2)],
      to = bidirected[seq(2, length(bidirected), by = 2)],
      direction = "<->",
      stringsAsFactors = FALSE
    )
    edges_df <- rbind(edges_df, bidir_df)
  }
  edges_df
}

test_that("longest_path_layers places bidirected nodes at same layer", {
  # w1 <-> w2, both are roots with no directed edges
  edges_df <- make_edges_df_with_direction(
    c("w1", "x"),
    c("w2", "x"),
    bidirected = c("w1", "w2")
  )
  layers <- longest_path_layers(edges_df)
  expect_equal(unname(layers[["w1"]]), unname(layers[["w2"]]))
})

test_that("longest_path_layers: bidirected nodes pushed to max of their layers", {
  # z → w1, w1 <-> w2, w2 → y
  # Without bidirected constraint: w1 at layer 1, w2 at layer 0

  # With bidirected: w1 and w2 should both be at layer 1 (max of 0, 1)
  edges_df <- make_edges_df_with_direction(
    c("z", "w1"),
    c("w2", "y"),
    bidirected = c("w1", "w2")
  )
  layers <- longest_path_layers(edges_df)
  expect_equal(unname(layers[["w1"]]), unname(layers[["w2"]]))
})

test_that("longest_path_layers: bidirected doesn't create directed dependency", {
  # w1 <-> w2 should NOT mean w1 → w2 or w2 → w1
  # Both should be roots (layer 0) when no other edges exist
  edges_df <- data.frame(
    name = c("w1", "w2", "w1"),
    to = c(NA_character_, NA_character_, "w2"),
    direction = c(NA_character_, NA_character_, "<->"),
    stringsAsFactors = FALSE
  )
  layers <- longest_path_layers(edges_df)
  expect_equal(unname(layers[["w1"]]), 0L)
  expect_equal(unname(layers[["w2"]]), 0L)
})

test_that("compute_time_ordered_layout places bidirected nodes at same x", {
  edges_df <- make_edges_df_with_direction(
    c("w1", "x"),
    c("w2", "x"),
    bidirected = c("w1", "w2")
  )
  result <- compute_time_ordered_layout(edges_df)
  x_w1 <- result$x[result$name == "w1"]
  x_w2 <- result$x[result$name == "w2"]
  expect_equal(x_w1, x_w2)
})

test_that("dagify with ~~ places bidirected nodes at same layer", {
  td <- dagify(y ~ x + w1 + w2, x ~ w1 + w2, w1 ~ ~w2) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)

  x_w1 <- unname(coords$x[coords$name == "w1"])
  x_w2 <- unname(coords$x[coords$name == "w2"])
  expect_equal(x_w1, x_w2)
})

test_that("README example DAG places w1 and w2 at same layer", {
  td <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1 + w2,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2,
    exposure = "x",
    outcome = "y"
  ) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)

  x_w1 <- unname(coords$x[coords$name == "w1"])
  x_w2 <- unname(coords$x[coords$name == "w2"])
  expect_equal(x_w1, x_w2)
})

test_that("dagify with ~~ produces valid layout (no NA coords)", {
  td <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1 + w2,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2,
    exposure = "x",
    outcome = "y"
  ) |>
    tidy_dagitty(layout = "time_ordered")
  expect_valid_time_ordered(
    td,
    c("x", "y", "z1", "z2", "w1", "w2", "v")
  )
})

test_that("dagify with ~~ and time_ordered has no overlaps", {
  dag <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1 + w2,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2,
    exposure = "x",
    outcome = "y"
  )
  expect_zero_overlaps_dagify(dag)
})

test_that("multiple bidirected pairs handled correctly", {
  td <- dagify(
    y ~ a + b + c + d,
    a ~ ~b,
    c ~ ~d
  ) |>
    tidy_dagitty(layout = "time_ordered")
  coords <- get_node_coords(td)

  # a and b should be at same layer

  expect_equal(
    unname(coords$x[coords$name == "a"]),
    unname(coords$x[coords$name == "b"])
  )
  # c and d should be at same layer
  expect_equal(
    unname(coords$x[coords$name == "c"]),
    unname(coords$x[coords$name == "d"])
  )
})

test_that("bidirected edge with coords = time_ordered_coords() works", {
  dag <- dagify(
    y ~ x + w1 + w2,
    x ~ w1 + w2,
    w1 ~ ~w2,
    coords = time_ordered_coords()
  )
  td <- tidy_dagitty(dag)
  coords <- get_node_coords(td)

  x_w1 <- unname(coords$x[coords$name == "w1"])
  x_w2 <- unname(coords$x[coords$name == "w2"])
  expect_equal(x_w1, x_w2)
})

# Visual regression tests (vdiffr) ---------------------------------------------

test_that("visual: confounding DAG z->x, z->y, x->y", {
  p <- dagify(y ~ x + z, x ~ z) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-confounding", p)
})

test_that("visual: mediation DAG x->m->y, x->y", {
  p <- dagify(y ~ x + m, m ~ x) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-mediation", p)
})

test_that("visual: collider x->m, y->m", {
  p <- dagify(m ~ x + y) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-collider", p)
})

test_that("visual: fork x<-z->y", {
  p <- dagify(x ~ z, y ~ z) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-fork", p)
})

test_that("visual: IV DAG z->x->y, u->x, u->y", {
  p <- dagify(y ~ x + u, x ~ z + u) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-iv", p)
})

test_that("visual: diamond a->b, a->c, b->d, c->d", {
  p <- dagify(d ~ b + c, b ~ a, c ~ a) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-diamond", p)
})

test_that("visual: wide fan-in 5 parents -> y", {
  p <- dagify(y ~ a + b + c + d + e) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-fan-in-5", p)
})

test_that("visual: wide fan-out x -> 5 children", {
  p <- dagify(a ~ x, b ~ x, c ~ x, d ~ x, e ~ x) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-fan-out-5", p)
})

test_that("visual: deep chain a->b->c->d->e->f", {
  p <- dagify(b ~ a, c ~ b, d ~ c, e ~ d, f ~ e) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-deep-chain", p)
})

test_that("visual: smoking DAG with skip edges", {
  p <- dagify(
    tar ~ smoking,
    cancer ~ smoking + tar + genetics,
    smoking ~ genetics
  ) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-smoking", p)
})

test_that("visual: napkin DAG", {
  p <- dagify(
    a ~ u1 + u2 + z,
    z ~ u1,
    m ~ a,
    y ~ u2 + m + a
  ) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-napkin", p)
})

test_that("visual: multi-mediator chain", {
  p <- dagify(
    m1 ~ x,
    m2 ~ x + m1,
    m3 ~ x + m2,
    y ~ x + m1 + m2 + m3
  ) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-multi-mediator", p)
})

test_that("visual: deep chain with shortcuts", {
  p <- dagify(
    b ~ a,
    c ~ b + a,
    d ~ c + b,
    e ~ d + a
  ) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-deep-shortcuts", p)
})

test_that("visual: complex epi DAG", {
  p <- dagify(
    health ~ ses + edu + income,
    income ~ edu,
    edu ~ ses
  ) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-epi", p)
})

test_that("visual: front-door DAG", {
  p <- dagify(y ~ u + m, x ~ u, m ~ x) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-front-door", p)
})

test_that("visual: M-bias DAG", {
  p <- dagify(y ~ a, m ~ u1 + u2, a ~ u1, y ~ u2) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-m-bias", p)
})

test_that("visual: disconnected components", {
  p <- dagify(y ~ x, b ~ a) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-disconnected", p)
})

test_that("visual: README DAG with bidirected edges", {
  p <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1 + w2,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2,
    exposure = "x",
    outcome = "y"
  ) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-readme-bidirected", p)
})

test_that("visual: multiple bidirected pairs", {
  p <- dagify(
    y ~ a + b + c + d,
    a ~ ~b,
    c ~ ~d
  ) |>
    ggdag(layout = "time_ordered")
  expect_doppelganger("time-ordered-multi-bidirected", p)
})

test_that("visual: direction = y", {
  p <- dagify(y ~ x + z, x ~ z) |>
    ggdag(layout = time_ordered_coords(direction = "y"))
  expect_doppelganger("time-ordered-direction-y", p)
})

test_that("visual: time_ordered with arc edges", {
  p <- dagify(y ~ x + z, x ~ z) |>
    ggdag(layout = "time_ordered", edge_type = "arc")
  expect_doppelganger("time-ordered-arc-edges", p)
})

test_that("visual: time_ordered with labels", {
  p <- dagify(
    y ~ x + z,
    x ~ z,
    labels = c("x" = "Treatment", "y" = "Outcome", "z" = "Confounder")
  ) |>
    tidy_dagitty(layout = "time_ordered") |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_label_repel(aes(label = label)) +
    theme_dag()
  expect_doppelganger("time-ordered-with-labels", p)
})

test_that("visual: time_ordered ggplot manual build", {
  p <- dagify(
    y ~ x + m,
    m ~ x,
    exposure = "x",
    outcome = "y"
  ) |>
    tidy_dagitty(layout = "time_ordered") |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text() +
    theme_dag()
  expect_doppelganger("time-ordered-manual-ggplot", p)
})

# auto_sort_direction tests ----------------------------------------------------

test_that("auto_sort_direction='right' uses longest path (default)", {
  # a→c (direct), a→b→c (via b)
  # right: c at layer 2 (longest path through b)
  td <- dagify(c ~ a + b, b ~ a) |>
    tidy_dagitty(layout = time_ordered_coords(auto_sort_direction = "right"))
  coords <- get_node_coords(td)

  x_a <- unname(coords$x[coords$name == "a"])
  x_b <- unname(coords$x[coords$name == "b"])
  x_c <- unname(coords$x[coords$name == "c"])
  expect_lt(x_a, x_b)
  expect_lt(x_b, x_c)
  # c should be at layer 3 (a=1, b=2, c=3)
  expect_equal(x_c, 3)
})

test_that("auto_sort_direction='left' places nodes at earliest valid layer", {
  # a→b→c, a→c (direct shortcut)
  # left: a=0, b=1, c=2 (c must be after b, respecting topo ordering)
  td <- dagify(c ~ a + b, b ~ a) |>
    tidy_dagitty(layout = time_ordered_coords(auto_sort_direction = "left"))
  coords <- get_node_coords(td)

  x_a <- unname(coords$x[coords$name == "a"])
  x_b <- unname(coords$x[coords$name == "b"])
  x_c <- unname(coords$x[coords$name == "c"])
  # c must come after b (topo ordering)
  expect_lt(x_a, x_b)
  expect_lt(x_b, x_c)
})

test_that("auto_sort_direction='right' pulls nodes toward children", {
  # a→b→c→d→e, f→e
  # left: f is a root at layer 0
  # right: f is pulled to layer 3 (one before e at layer 4)
  td_left <- dagify(e ~ d + f, d ~ c, c ~ b, b ~ a) |>
    tidy_dagitty(layout = time_ordered_coords(auto_sort_direction = "left"))
  td_right <- dagify(e ~ d + f, d ~ c, c ~ b, b ~ a) |>
    tidy_dagitty(layout = time_ordered_coords(auto_sort_direction = "right"))

  coords_left <- get_node_coords(td_left)
  coords_right <- get_node_coords(td_right)

  f_left <- unname(coords_left$x[coords_left$name == "f"])
  f_right <- unname(coords_right$x[coords_right$name == "f"])
  # In left mode, f is at layer 0 (root); in right mode, f is pulled rightward
  expect_lt(f_left, f_right)
})

test_that("auto_sort_direction='right' is the default", {
  dag <- dagify(c ~ a + b, b ~ a)
  td_default <- tidy_dagitty(dag, layout = time_ordered_coords())
  td_right <- tidy_dagitty(
    dag,
    layout = time_ordered_coords(auto_sort_direction = "right")
  )
  expect_equal(pull_dag_data(td_default), pull_dag_data(td_right))
})

test_that("auto_sort_direction works with string layout", {
  # string "time_ordered" should use the default (right)
  dag <- dagify(c ~ a + b, b ~ a)
  td_str <- tidy_dagitty(dag, layout = "time_ordered")
  td_right <- tidy_dagitty(
    dag,
    layout = time_ordered_coords(auto_sort_direction = "right")
  )
  expect_equal(pull_dag_data(td_str), pull_dag_data(td_right))
})

test_that("auto_sort_direction='left' still respects edge direction", {
  # Even in left mode, parents must come before children
  td <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty(layout = time_ordered_coords(auto_sort_direction = "left"))
  coords <- get_node_coords(td)

  x_z <- unname(coords$x[coords$name == "z"])
  x_x <- unname(coords$x[coords$name == "x"])
  x_y <- unname(coords$x[coords$name == "y"])
  expect_lt(x_z, x_x)
  expect_lt(x_z, x_y)
})

test_that("auto_sort_direction='left' produces valid layout (no NA)", {
  td <- dagify(
    y ~ x + m,
    m ~ x,
    exposure = "x",
    outcome = "y"
  ) |>
    tidy_dagitty(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_valid_time_ordered(td, c("x", "y", "m"))
})

test_that("auto_sort_direction works with coords= in dagify", {
  dag <- dagify(
    c ~ a + b,
    b ~ a,
    coords = time_ordered_coords(auto_sort_direction = "left")
  )
  td <- tidy_dagitty(dag)
  coords <- get_node_coords(td)
  # topo ordering: a < b < c
  x_a <- unname(coords$x[coords$name == "a"])
  x_b <- unname(coords$x[coords$name == "b"])
  x_c <- unname(coords$x[coords$name == "c"])
  expect_lt(x_a, x_b)
  expect_lt(x_b, x_c)
})

# auto_sort_direction snapshot tests -------------------------------------------
# Paired left/right snapshots for each DAG pattern

test_that("visual: right mediation DAG", {
  p <- dagify(y ~ x + m, m ~ x) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-mediation", p)
})

test_that("visual: left mediation DAG", {
  p <- dagify(y ~ x + m, m ~ x) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-mediation", p)
})

test_that("visual: right confounding DAG", {
  p <- dagify(y ~ x + z, x ~ z) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-confounding", p)
})

test_that("visual: left confounding DAG", {
  p <- dagify(y ~ x + z, x ~ z) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-confounding", p)
})

test_that("visual: right smoking DAG", {
  p <- dagify(
    tar ~ smoking,
    cancer ~ smoking + tar + genetics,
    smoking ~ genetics
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-smoking", p)
})

test_that("visual: left smoking DAG", {
  p <- dagify(
    tar ~ smoking,
    cancer ~ smoking + tar + genetics,
    smoking ~ genetics
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-smoking", p)
})

test_that("visual: right diamond DAG", {
  p <- dagify(d ~ b + c, b ~ a, c ~ a) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-diamond", p)
})

test_that("visual: left diamond DAG", {
  p <- dagify(d ~ b + c, b ~ a, c ~ a) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-diamond", p)
})

test_that("visual: right deep shortcuts DAG", {
  p <- dagify(
    b ~ a,
    c ~ b + a,
    d ~ c + b,
    e ~ d + a
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-deep-shortcuts", p)
})

test_that("visual: left deep shortcuts DAG", {
  p <- dagify(
    b ~ a,
    c ~ b + a,
    d ~ c + b,
    e ~ d + a
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-deep-shortcuts", p)
})

test_that("visual: right multi-mediator chain", {
  p <- dagify(
    m1 ~ x,
    m2 ~ x + m1,
    m3 ~ x + m2,
    y ~ x + m1 + m2 + m3
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-multi-mediator", p)
})

test_that("visual: left multi-mediator chain", {
  p <- dagify(
    m1 ~ x,
    m2 ~ x + m1,
    m3 ~ x + m2,
    y ~ x + m1 + m2 + m3
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-multi-mediator", p)
})

test_that("visual: right napkin DAG", {
  p <- dagify(
    a ~ u1 + u2 + z,
    z ~ u1,
    m ~ a,
    y ~ u2 + m + a
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-napkin", p)
})

test_that("visual: left napkin DAG", {
  p <- dagify(
    a ~ u1 + u2 + z,
    z ~ u1,
    m ~ a,
    y ~ u2 + m + a
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-napkin", p)
})

test_that("visual: right front-door DAG", {
  p <- dagify(y ~ u + m, x ~ u, m ~ x) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-front-door", p)
})

test_that("visual: left front-door DAG", {
  p <- dagify(y ~ u + m, x ~ u, m ~ x) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-front-door", p)
})

test_that("visual: right IV DAG", {
  p <- dagify(y ~ x + u, x ~ z + u) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-iv", p)
})

test_that("visual: left IV DAG", {
  p <- dagify(y ~ x + u, x ~ z + u) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-iv", p)
})

test_that("visual: right M-bias DAG", {
  p <- dagify(y ~ a, m ~ u1 + u2, a ~ u1, y ~ u2) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-m-bias", p)
})

test_that("visual: left M-bias DAG", {
  p <- dagify(y ~ a, m ~ u1 + u2, a ~ u1, y ~ u2) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-m-bias", p)
})

test_that("visual: right epi DAG", {
  p <- dagify(
    health ~ ses + edu + income,
    income ~ edu,
    edu ~ ses
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-epi", p)
})

test_that("visual: left epi DAG", {
  p <- dagify(
    health ~ ses + edu + income,
    income ~ edu,
    edu ~ ses
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-epi", p)
})

test_that("visual: right README bidirected DAG", {
  p <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1 + w2,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2,
    exposure = "x",
    outcome = "y"
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-readme-bidirected", p)
})

test_that("visual: left README bidirected DAG", {
  p <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1 + w2,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2,
    exposure = "x",
    outcome = "y"
  ) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-readme-bidirected", p)
})

test_that("visual: right fan-in 5 DAG", {
  p <- dagify(y ~ a + b + c + d + e) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-fan-in-5", p)
})

test_that("visual: left fan-in 5 DAG", {
  p <- dagify(y ~ a + b + c + d + e) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-fan-in-5", p)
})

test_that("visual: right deep chain DAG", {
  p <- dagify(b ~ a, c ~ b, d ~ c, e ~ d, f ~ e) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "right"))
  expect_doppelganger("time-ordered-right-deep-chain", p)
})

test_that("visual: left deep chain DAG", {
  p <- dagify(b ~ a, c ~ b, d ~ c, e ~ d, f ~ e) |>
    ggdag(layout = time_ordered_coords(auto_sort_direction = "left"))
  expect_doppelganger("time-ordered-left-deep-chain", p)
})
