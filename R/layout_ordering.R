# Within-layer crossing minimization for layered layouts. Builds on the
# barycenter machinery in R/layout_time_ordered.R: dummy nodes route edges
# that span several layers through every intermediate layer, and an exact
# permutation search refines small layers past the barycenter heuristic.

# Node names created by insert_dummy_nodes() start with this reserved prefix.
dummy_node_prefix <- ".ggdag_dummy_"

#' Break multi-layer edges into chains of single-layer edges
#'
#' Every directed edge that spans two or more layers is replaced by a chain
#' through one dummy node per intermediate layer, so that adjacent-layer
#' crossing counts see the full path of the edge. Rows with `to = NA`,
#' same-layer edges, and backward edges pass through untouched.
#'
#' Dummy names are `".ggdag_dummy_<i>"`, numbered deterministically: edges are
#' processed in row order and each chain's dummies are created in ascending
#' layer order.
#'
#' @param edges_df Data frame with columns `name` and `to`. Extra columns are
#'   dropped; the result carries only `name` and `to`.
#' @param layer_assign Named integer vector (node -> 0-based layer index).
#' @return A list with `edges` (data frame of `name` and `to` where every
#'   directed edge spans exactly one layer), `layer_assign` (named integer
#'   vector including the dummies), and `dummy_map` (data frame with columns
#'   `dummy`, `edge_from`, `edge_to`, and `layer` recording which original
#'   edge each dummy subdivides).
#' @noRd
insert_dummy_nodes <- function(edges_df, layer_assign) {
  from_all <- as.character(edges_df$name)
  to_all <- as.character(edges_df$to)

  node_names <- unique(c(names(layer_assign), from_all, to_all))
  node_names <- node_names[!is.na(node_names)]
  reserved <- node_names[startsWith(node_names, dummy_node_prefix)]
  if (length(reserved) > 0) {
    abort(
      c(
        "Node names must not start with the reserved prefix
         {.code .ggdag_dummy_}.",
        "x" = "Found: {.val {reserved}}.",
        "i" = "This prefix is reserved for internal layout dummy nodes.
               Rename these nodes."
      ),
      error_class = "ggdag_dag_error"
    )
  }

  layer_assign <- stats::setNames(as.integer(layer_assign), names(layer_assign))

  edge_name <- vector("list", length(from_all))
  edge_to <- vector("list", length(from_all))
  map_dummy <- character(0)
  map_from <- character(0)
  map_to <- character(0)
  map_layer <- integer(0)
  dummy_layers <- integer(0)
  n_dummies <- 0L

  for (i in seq_along(from_all)) {
    from <- from_all[[i]]
    to <- to_all[[i]]
    known <- !is.na(to) &&
      from %in% names(layer_assign) &&
      to %in% names(layer_assign)
    span <- if (known) {
      layer_assign[[to]] - layer_assign[[from]]
    } else {
      NA_integer_
    }

    if (is.na(span) || span <= 1L) {
      edge_name[[i]] <- from
      edge_to[[i]] <- to
      next
    }

    chain_layers <- seq.int(layer_assign[[from]] + 1L, layer_assign[[to]] - 1L)
    dummies <- paste0(dummy_node_prefix, n_dummies + seq_along(chain_layers))
    n_dummies <- n_dummies + length(chain_layers)

    chain <- c(from, dummies, to)
    edge_name[[i]] <- chain[-length(chain)]
    edge_to[[i]] <- chain[-1L]

    map_dummy <- c(map_dummy, dummies)
    map_from <- c(map_from, rep(from, length(dummies)))
    map_to <- c(map_to, rep(to, length(dummies)))
    map_layer <- c(map_layer, chain_layers)
    dummy_layers <- c(dummy_layers, stats::setNames(chain_layers, dummies))
  }

  list(
    edges = data.frame(
      name = as.character(unlist(edge_name)),
      to = as.character(unlist(edge_to)),
      stringsAsFactors = FALSE
    ),
    layer_assign = c(layer_assign, dummy_layers),
    dummy_map = data.frame(
      dummy = map_dummy,
      edge_from = map_from,
      edge_to = map_to,
      layer = map_layer,
      stringsAsFactors = FALSE
    )
  )
}

#' Enumerate all permutations of `1:n` in lexicographic order
#'
#' @param n A single non-negative whole number. Callers keep `n` small (at
#'   most `exact_max`, 7 by default) because the result has `n!` rows.
#' @return An integer matrix with `n!` rows and `n` columns, one permutation
#'   per row, in strictly increasing lexicographic order. `n = 0` gives a
#'   one-row, zero-column matrix (the single empty permutation).
#' @noRd
permutations_lex <- function(n) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0 || n != trunc(n)) {
    abort(
      c(
        "{.arg n} must be a single non-negative whole number.",
        "x" = "Got {.val {n}}."
      ),
      error_class = "ggdag_type_error"
    )
  }
  n <- as.integer(n)

  if (n == 0L) {
    return(matrix(integer(0), nrow = 1L, ncol = 0L))
  }

  permutations_of(seq_len(n))
}

#' Recursive worker for permutations_lex()
#'
#' Prefixing each element of a sorted vector to the permutations of the rest
#' yields the permutations of the whole in lexicographic order.
#'
#' @param x An integer vector in increasing order.
#' @return An integer matrix, one permutation of `x` per row.
#' @noRd
permutations_of <- function(x) {
  if (length(x) == 1L) {
    return(matrix(x, nrow = 1L, ncol = 1L))
  }
  blocks <- lapply(seq_along(x), function(i) {
    cbind(x[[i]], permutations_of(x[-i]), deparse.level = 0)
  })
  do.call(rbind, blocks)
}

#' Count crossings between two adjacent layers
#'
#' Two edges cross when their endpoints appear in opposite relative order in
#' the two layers, that is when `(pos_i - pos_k) * (pos_j - pos_l) < 0` for
#' edges `i -> j` and `k -> l`.
#'
#' @param left_order Character vector: the left layer, in drawing order.
#' @param right_order Character vector: the right layer, in drawing order.
#' @param edges_between Data frame with `name` (left) and `to` (right)
#'   columns. Rows with `to = NA` or naming nodes absent from either order
#'   are ignored.
#' @return Integer count of crossing pairs.
#' @noRd
count_crossings_bilayer <- function(left_order, right_order, edges_between) {
  keep <- !is.na(edges_between$to) &
    edges_between$name %in% left_order &
    edges_between$to %in% right_order
  edges <- edges_between[keep, , drop = FALSE]

  n <- nrow(edges)
  if (n < 2L) {
    return(0L)
  }

  left_pos <- match(edges$name, left_order)
  right_pos <- match(edges$to, right_order)

  crossings <- 0L
  for (a in seq_len(n - 1L)) {
    b <- seq.int(a + 1L, n)
    crossings <- crossings +
      sum((left_pos[a] - left_pos[b]) * (right_pos[a] - right_pos[b]) < 0L)
  }
  crossings
}

#' One barycenter reordering of a layer against a fixed neighbor layer
#'
#' Each node moves to the mean position of its neighbors in the fixed layer;
#' nodes without neighbors keep their current position as the barycenter.
#' Ties break deterministically by preserving the incoming order.
#'
#' @param layer Character vector: the layer to reorder.
#' @param neighbor_layer Character vector: the fixed adjacent layer.
#' @param directed Data frame of directed edges (`name`, `to`, no `NA`s).
#' @param incoming If `TRUE`, neighbors are parents in `neighbor_layer`;
#'   otherwise children.
#' @return The reordered layer.
#' @noRd
barycenter_reorder <- function(layer, neighbor_layer, directed, incoming) {
  if (length(layer) < 2L) {
    return(layer)
  }

  neighbor_pos <- stats::setNames(seq_along(neighbor_layer), neighbor_layer)
  bary <- as.numeric(seq_along(layer))

  for (j in seq_along(layer)) {
    node <- layer[[j]]
    neighbors <- if (incoming) {
      directed$name[directed$to == node & directed$name %in% neighbor_layer]
    } else {
      directed$to[directed$name == node & directed$to %in% neighbor_layer]
    }
    if (length(neighbors) > 0L) {
      bary[[j]] <- mean(neighbor_pos[neighbors])
    }
  }

  layer[order(bary, seq_along(layer))]
}

#' Initialize a layer ordering with forward and backward barycenter sweeps
#'
#' @param layer_nodes List of character vectors, one per layer.
#' @param edges_df Data frame with `name` and `to` columns.
#' @param sweeps Number of forward plus backward sweep iterations.
#' @return The reordered `layer_nodes` list.
#' @noRd
barycenter_init <- function(layer_nodes, edges_df, sweeps) {
  directed <- edges_df[!is.na(edges_df$to), , drop = FALSE]
  if (nrow(directed) == 0L || length(layer_nodes) < 2L) {
    return(layer_nodes)
  }

  for (s in seq_len(sweeps)) {
    for (i in seq.int(2L, length(layer_nodes))) {
      layer_nodes[[i]] <- barycenter_reorder(
        layer_nodes[[i]],
        layer_nodes[[i - 1L]],
        directed,
        incoming = TRUE
      )
    }
    for (i in seq.int(length(layer_nodes) - 1L, 1L)) {
      layer_nodes[[i]] <- barycenter_reorder(
        layer_nodes[[i]],
        layer_nodes[[i + 1L]],
        directed,
        incoming = FALSE
      )
    }
  }

  layer_nodes
}

#' Minimize within-layer crossings with exact small-layer refinement
#'
#' Runs the full ordering pipeline on a layered graph: optional dummy-node
#' augmentation so multi-layer edges join the objective, barycenter sweeps
#' for a strong start, then refinement passes that sweep the layers in
#' alternating direction. A layer with at most `exact_max` nodes is set to
#' the best of all its permutations holding both neighbor layers fixed; a
#' wider layer uses greedy adjacent transpositions. Only strict improvements
#' are accepted, so the incumbent ordering always wins ties and the whole
#' pipeline is deterministic with no randomness.
#'
#' The per-layer objective is the crossing count against both neighbor
#' layers plus an adjacency penalty of `0.5 * (|rank(u) - rank(v)| - 1)` for
#' every same-layer bidirected pair, which nudges bidirected nodes together
#' without ever outvoting a full crossing.
#'
#' As a guarantee, the refined ordering is compared with the incumbent and
#' with a plain `barycenter_sort()` run on the real graph; whichever has the
#' fewest real-graph adjacent-layer crossings is returned, with the refined
#' ordering preferred on ties.
#'
#' @param layer_nodes List of character vectors, one per layer, in incumbent
#'   order.
#' @param edges_df Data frame with `name` and `to` columns (`direction`
#'   honored when present; bidirected rows are excluded from the crossing
#'   objective).
#' @param layer_assign Named integer vector (node -> 0-based layer index).
#' @param sweeps Barycenter initialization sweep count.
#' @param exact_max Largest layer size refined by exact enumeration.
#' @param max_refine_passes Upper bound on refinement sweeps over the layers.
#' @param use_dummies If `FALSE`, skip dummy augmentation.
#' @param max_dummies Skip dummy augmentation when the graph would need more
#'   dummies than this.
#' @param bidirected_pairs Optional data frame with `name` and `to` columns
#'   listing bidirected pairs for the adjacency penalty.
#' @return A list with `layer_nodes` (the reordered real layers, dummy free)
#'   and `augmented` (a list of `layer_nodes`, `layer_assign`, and `edges`
#'   describing the graph the ordering was computed on; it carries the dummy
#'   chains when augmentation ran and the dummy-free graph otherwise).
#' @noRd
order_layers <- function(
  layer_nodes,
  edges_df,
  layer_assign,
  sweeps = 8L,
  exact_max = 7L,
  max_refine_passes = 10L,
  use_dummies = TRUE,
  max_dummies = 60L,
  bidirected_pairs = NULL
) {
  layer_assign <- stats::setNames(as.integer(layer_assign), names(layer_assign))
  directed <- split_edge_types(edges_df)$directed
  directed <- directed[, c("name", "to"), drop = FALSE]

  spans <- layer_assign[directed$to] - layer_assign[directed$name]
  n_dummies <- sum(pmax(spans - 1L, 0L), na.rm = TRUE)
  augment <- use_dummies && n_dummies <= max_dummies

  if (augment) {
    aug <- insert_dummy_nodes(directed, layer_assign)
    aug_edges <- aug$edges
    aug_assign <- aug$layer_assign
    aug_layers <- layer_nodes
    for (r in seq_len(nrow(aug$dummy_map))) {
      li <- aug$dummy_map$layer[[r]] + 1L
      aug_layers[[li]] <- c(aug_layers[[li]], aug$dummy_map$dummy[[r]])
    }
  } else {
    aug_edges <- directed
    aug_assign <- layer_assign
    aug_layers <- layer_nodes
  }

  n_layers <- length(aug_layers)

  # Same-layer bidirected pairs, grouped by layer, for the adjacency penalty
  bidi_by_layer <- vector("list", n_layers)
  if (!is.null(bidirected_pairs) && nrow(bidirected_pairs) > 0L) {
    u_layer <- layer_assign[bidirected_pairs$name]
    v_layer <- layer_assign[bidirected_pairs$to]
    same <- which(!is.na(u_layer) & !is.na(v_layer) & u_layer == v_layer)
    for (r in same) {
      li <- u_layer[[r]] + 1L
      bidi_by_layer[[li]] <- rbind(
        bidi_by_layer[[li]],
        bidirected_pairs[r, c("name", "to"), drop = FALSE]
      )
    }
  }

  # Edges between each pair of adjacent layers; multi-layer edges belong to
  # no boundary, which is exactly why augmentation matters
  boundary_edges <- list()
  if (n_layers >= 2L) {
    boundary_edges <- lapply(seq_len(n_layers - 1L), function(b) {
      keep <- which(
        aug_assign[aug_edges$name] == b - 1L & aug_assign[aug_edges$to] == b
      )
      aug_edges[keep, , drop = FALSE]
    })
  }

  # Objective for one layer's candidate ordering, neighbors held fixed
  layer_objective <- function(layers, i, ordering) {
    obj <- 0
    if (i > 1L) {
      obj <- obj +
        count_crossings_bilayer(
          layers[[i - 1L]],
          ordering,
          boundary_edges[[i - 1L]]
        )
    }
    if (i < n_layers) {
      obj <- obj +
        count_crossings_bilayer(
          ordering,
          layers[[i + 1L]],
          boundary_edges[[i]]
        )
    }
    pairs <- bidi_by_layer[[i]]
    if (!is.null(pairs)) {
      ranks_u <- match(pairs$name, ordering)
      ranks_v <- match(pairs$to, ordering)
      obj <- obj + 0.5 * sum(abs(ranks_u - ranks_v) - 1L)
    }
    obj
  }

  # Exact refinement: first strict improver in lexicographic order wins, so
  # the incumbent keeps ties
  refine_exact <- function(current, i, perms) {
    best <- current
    best_obj <- layer_objective(aug_layers, i, current)
    for (r in seq_len(nrow(perms))) {
      cand <- current[perms[r, ]]
      obj <- layer_objective(aug_layers, i, cand)
      if (obj < best_obj) {
        best_obj <- obj
        best <- cand
      }
    }
    best
  }

  # Greedy fallback for wide layers: adjacent transpositions in index order,
  # strict improvements only
  refine_greedy <- function(current, i) {
    obj <- layer_objective(aug_layers, i, current)
    repeat {
      improved <- FALSE
      for (j in seq_len(length(current) - 1L)) {
        cand <- current
        cand[c(j, j + 1L)] <- cand[c(j + 1L, j)]
        cand_obj <- layer_objective(aug_layers, i, cand)
        if (cand_obj < obj) {
          current <- cand
          obj <- cand_obj
          improved <- TRUE
        }
      }
      if (!improved) {
        break
      }
    }
    current
  }

  aug_layers <- barycenter_init(aug_layers, aug_edges, sweeps)

  # Layer sizes never change during refinement, so enumerate once per layer
  layer_perms <- lapply(aug_layers, function(l) {
    k <- length(l)
    if (k >= 2L && k <= exact_max) permutations_lex(k) else NULL
  })

  for (pass in seq_len(max_refine_passes)) {
    order_idx <- if (pass %% 2L == 1L) {
      seq_len(n_layers)
    } else {
      rev(seq_len(n_layers))
    }
    changed <- FALSE
    for (i in order_idx) {
      current <- aug_layers[[i]]
      if (length(current) < 2L) {
        next
      }
      new_order <- if (!is.null(layer_perms[[i]])) {
        refine_exact(current, i, layer_perms[[i]])
      } else {
        refine_greedy(current, i)
      }
      if (!identical(new_order, current)) {
        aug_layers[[i]] <- new_order
        changed <- TRUE
      }
    }
    if (!changed) {
      break
    }
  }

  real_layers <- if (augment) {
    lapply(aug_layers, function(l) l[!startsWith(l, dummy_node_prefix)])
  } else {
    aug_layers
  }

  # Never-worse guarantee on the real graph: prefer the refined ordering,
  # then the incumbent, then plain barycenter, breaking ties in that order
  refined_score <- count_crossings(real_layers, directed, layer_assign)
  incumbent_score <- count_crossings(layer_nodes, directed, layer_assign)
  bary_layers <- barycenter_sort(layer_nodes, directed, layer_assign, 40L)
  bary_score <- count_crossings(bary_layers, directed, layer_assign)

  choice <- which.min(c(refined_score, incumbent_score, bary_score))
  if (choice == 1L) {
    final_layers <- real_layers
    augmented <- list(
      layer_nodes = aug_layers,
      layer_assign = aug_assign,
      edges = aug_edges
    )
  } else {
    final_layers <- if (choice == 2L) layer_nodes else bary_layers
    augmented <- list(
      layer_nodes = final_layers,
      layer_assign = layer_assign,
      edges = directed
    )
  }

  list(layer_nodes = final_layers, augmented = augmented)
}
