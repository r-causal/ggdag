# Overlap-free time-ordered layout engine
# See scratch/time_order.md for full algorithm spec

# Stage 0: Geometric primitives ------------------------------------------------

#' Compute vertical distance from a point to a line segment
#'
#' Returns the Y-component of the distance from point (wx, wy) to the line
#' segment (ax, ay)-(bx, by). Used for detecting node-edge overlaps where
#' X positions are fixed by layer assignment.
#'
#' @param wx,wy Coordinates of the point (intermediate node)
#' @param ax,ay Coordinates of the segment start (edge source)
#' @param bx,by Coordinates of the segment end (edge target)
#' @return A list with `dist` (vertical distance), `t` (parameter along
#'   segment, 0=start, 1=end), and `proj_y` (Y coordinate of projection).
#'   Returns `list(dist = Inf)` for degenerate edges or points outside the
#'   segment.
#' @noRd
y_dist_to_edge <- function(wx, wy, ax, ay, bx, by) {
  dx <- bx - ax
  dy <- by - ay
  len2 <- dx * dx + dy * dy

  if (len2 < 1e-10) {
    return(list(dist = Inf))
  }

  t <- ((wx - ax) * dx + (wy - ay) * dy) / len2

  if (t < 0.005 || t > 0.995) {
    return(list(dist = Inf))
  }

  proj_y <- ay + t * dy
  dist <- abs(wy - proj_y)
  list(dist = dist, t = t, proj_y = proj_y)
}

#' Enforce minimum vertical spacing between same-layer nodes
#'
#' Sorts nodes within each layer by Y position and pushes consecutive pairs
#' apart if they are closer than `min_spacing`.
#'
#' @param positions Named numeric vector of Y positions (names = node names)
#' @param layers Named integer vector of layer assignments (names = node names)
#' @param min_spacing Minimum Y gap between consecutive same-layer nodes
#' @return Updated named numeric vector of Y positions
#' @noRd
enforce_spacing <- function(positions, layers, min_spacing) {
  unique_layers <- unique(layers)
  for (layer in unique_layers) {
    nodes_in_layer <- names(layers[layers == layer])
    if (length(nodes_in_layer) < 2) {
      next
    }

    layer_order <- order(positions[nodes_in_layer])
    sorted_nodes <- nodes_in_layer[layer_order]

    for (i in seq(2, length(sorted_nodes))) {
      gap <- positions[[sorted_nodes[i]]] - positions[[sorted_nodes[i - 1]]]
      if (gap < min_spacing) {
        positions[[sorted_nodes[i]]] <- positions[[sorted_nodes[i - 1]]] +
          min_spacing
      }
    }
  }

  positions
}

# Stage 1: Longest-path layer assignment ---------------------------------------

#' Assign nodes to time layers
#'
#' Find all descendants of a node in a directed graph
#'
#' BFS from `node` following directed edges to find all reachable nodes.
#'
#' @param node Character scalar: the starting node.
#' @param directed_edges Data frame with `name` and `to` columns (directed edges
#'   only, no NAs in `to`).
#' @return Character vector of descendant node names (not including `node`).
#' @noRd
find_descendants <- function(node, directed_edges) {
  adj <- list()
  for (i in seq_len(nrow(directed_edges))) {
    src <- directed_edges$name[i]
    tgt <- directed_edges$to[i]
    adj[[src]] <- c(adj[[src]], tgt)
  }

  visited <- character(0)
  queue <- adj[[node]] %||% character(0)
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    if (current %in% visited) {
      next
    }
    visited <- c(visited, current)
    queue <- c(queue, adj[[current]] %||% character(0))
  }
  visited
}

#' Modified Kahn's algorithm (BFS topological sort) that tracks the longest
#' incoming path to each node. With `sort_direction = "left"`, nodes are placed
#' as far left (early) as possible — each node sits one layer after its latest
#' parent. With `sort_direction = "right"` (default), nodes are then pushed
#' rightward so each node sits one layer before its earliest child, placing
#' nodes as close as possible to their descendants.
#'
#' @param edges_df A data frame with columns `name` (source) and `to` (target).
#'   Rows with `to = NA` represent terminal or isolated nodes.
#' @param sort_direction Either `"right"` (close to descendants, default) or
#'   `"left"` (close to ancestors).
#' @return A named integer vector mapping node names to 0-based layer indices.
#' @noRd
longest_path_layers <- function(
  edges_df,
  sort_direction = "right",
  fixed_time = NULL
) {
  edges_df$name <- as.character(edges_df$name)
  edges_df$to <- as.character(edges_df$to)
  all_nodes <- unique(c(edges_df$name, edges_df$to))
  all_nodes <- all_nodes[!is.na(all_nodes)]

  # Separate bidirected edges — these constrain same-layer, not parent→child
  has_direction <- "direction" %in% names(edges_df)
  if (has_direction) {
    is_bidirected <- !is.na(edges_df$to) &
      !is.na(edges_df$direction) &
      edges_df$direction == "<->"
    bidirected <- edges_df[is_bidirected, , drop = FALSE]
    directed <- edges_df[!is.na(edges_df$to) & !is_bidirected, , drop = FALSE]
  } else {
    bidirected <- edges_df[integer(0), , drop = FALSE]
    directed <- edges_df[!is.na(edges_df$to), , drop = FALSE]
  }

  # Build adjacency list and in-degree
  adj <- stats::setNames(vector("list", length(all_nodes)), all_nodes)
  in_deg <- stats::setNames(integer(length(all_nodes)), all_nodes)

  for (i in seq_len(nrow(directed))) {
    src <- directed$name[i]
    tgt <- directed$to[i]
    adj[[src]] <- c(adj[[src]], tgt)
    in_deg[[tgt]] <- in_deg[[tgt]] + 1L
  }

  # Forward pass: longest path from roots (valid topo ordering for both modes)
  dist <- stats::setNames(integer(length(all_nodes)), all_nodes)
  queue <- names(in_deg[in_deg == 0L])

  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]

    for (child in adj[[node]]) {
      new_dist <- dist[[node]] + 1L
      if (new_dist > dist[[child]]) {
        dist[[child]] <- new_dist
      }
      in_deg[[child]] <- in_deg[[child]] - 1L
      if (in_deg[[child]] == 0L) {
        queue <- c(queue, child)
      }
    }
  }

  # Apply fixed_time pins
  if (!is.null(fixed_time) && length(fixed_time) > 0) {
    pin_names <- names(fixed_time)
    fixed_time <- stats::setNames(as.integer(fixed_time), pin_names)

    # Warn and drop unknown nodes
    unknown <- setdiff(pin_names, all_nodes)
    if (length(unknown) > 0) {
      cli::cli_warn(
        c(
          "{.arg fixed_time} contains node{?s} not in the DAG: {.val {unknown}}.",
          "i" = "These will be ignored."
        ),
        class = "ggdag_warning"
      )
      fixed_time <- fixed_time[pin_names %in% all_nodes]
      pin_names <- names(fixed_time)
    }

    if (length(fixed_time) > 0) {
      # Validate: no directed edge has parent pinned >= child pinned
      for (i in seq_len(nrow(directed))) {
        src <- directed$name[i]
        tgt <- directed$to[i]
        if (src %in% pin_names && tgt %in% pin_names) {
          if (fixed_time[[src]] >= fixed_time[[tgt]]) {
            cli::cli_abort(
              c(
                "Pinned times violate DAG ordering.",
                "x" = "{.val {src}} (time {fixed_time[[src]]}) must be before {.val {tgt}} (time {fixed_time[[tgt]]})."
              ),
              class = "ggdag_dag_error"
            )
          }
        }
      }

      # Override pinned nodes
      for (nm in pin_names) {
        dist[[nm]] <- fixed_time[[nm]]
      }

      # Re-propagate: ensure all non-pinned descendants respect ordering
      # Use topo order (process nodes with all parents already processed)
      topo_in_deg <- stats::setNames(integer(length(all_nodes)), all_nodes)
      for (i in seq_len(nrow(directed))) {
        topo_in_deg[[directed$to[i]]] <- topo_in_deg[[directed$to[i]]] + 1L
      }
      topo_queue <- names(topo_in_deg[topo_in_deg == 0L])
      while (length(topo_queue) > 0) {
        node <- topo_queue[1]
        topo_queue <- topo_queue[-1]
        for (child in adj[[node]]) {
          if (!(child %in% pin_names)) {
            min_valid <- dist[[node]] + 1L
            if (dist[[child]] < min_valid) {
              dist[[child]] <- min_valid
            }
          }
          topo_in_deg[[child]] <- topo_in_deg[[child]] - 1L
          if (topo_in_deg[[child]] == 0L) {
            topo_queue <- c(topo_queue, child)
          }
        }
      }
    }
  }

  pinned <- if (!is.null(fixed_time)) names(fixed_time) else character(0)

  # For "right": backward pass pushing nodes toward their children
  # Each node is set to min(child_layer) - 1, using the original (left-aligned)
  # layers so updates don't cascade. Pinned nodes are never moved.
  if (identical(sort_direction, "right")) {
    original <- dist
    for (node in names(dist)) {
      if (node %in% pinned) {
        next
      }
      children <- adj[[node]]
      if (length(children) > 0) {
        min_child_layer <- min(original[children])
        dist[[node]] <- min_child_layer - 1L
      }
    }
  }

  # Enforce same-layer constraint for bidirected pairs
  if (nrow(bidirected) > 0) {
    # Build union-find groups of bidirected nodes
    bidir_groups <- list()
    for (i in seq_len(nrow(bidirected))) {
      u <- bidirected$name[i]
      v <- bidirected$to[i]
      # Find which group each node belongs to
      u_grp <- which(vapply(bidir_groups, function(g) u %in% g, logical(1)))
      v_grp <- which(vapply(bidir_groups, function(g) v %in% g, logical(1)))

      if (length(u_grp) == 0 && length(v_grp) == 0) {
        bidir_groups <- c(bidir_groups, list(c(u, v)))
      } else if (length(u_grp) > 0 && length(v_grp) == 0) {
        bidir_groups[[u_grp[1]]] <- c(bidir_groups[[u_grp[1]]], v)
      } else if (length(u_grp) == 0 && length(v_grp) > 0) {
        bidir_groups[[v_grp[1]]] <- c(bidir_groups[[v_grp[1]]], u)
      } else if (u_grp[1] != v_grp[1]) {
        # Merge groups
        bidir_groups[[u_grp[1]]] <- c(
          bidir_groups[[u_grp[1]]],
          bidir_groups[[v_grp[1]]]
        )
        bidir_groups <- bidir_groups[-v_grp[1]]
      }
    }

    # Set all nodes in each group to the max layer in the group
    for (grp in bidir_groups) {
      grp_nodes <- unique(grp)
      grp_nodes <- grp_nodes[grp_nodes %in% names(dist)]
      if (length(grp_nodes) < 2) {
        next
      }

      # Check for conflicting pins within the bidirected group
      grp_pinned <- grp_nodes[grp_nodes %in% pinned]
      if (length(grp_pinned) >= 2) {
        pin_values <- dist[grp_pinned]
        if (length(unique(pin_values)) > 1) {
          cli::cli_abort(
            c(
              "Conflicting {.arg fixed_time} values in bidirected group.",
              "x" = "Nodes {.val {grp_pinned}} are connected by bidirected edges and must share the same layer,
but are pinned to different times: {.val {pin_values}}."
            ),
            class = "ggdag_dag_error"
          )
        }
      }

      max_layer <- max(dist[grp_nodes])
      for (node in grp_nodes) {
        dist[[node]] <- max_layer
      }
    }
  }

  dist
}

# Stage 2: Barycenter crossing minimization ------------------------------------

#' Count edge crossings between adjacent layers
#'
#' @param layer_nodes List of character vectors, one per layer (ordered by
#'   within-layer position).
#' @param edges_df Data frame with `name` and `to` columns (rows with `to = NA`
#'   ignored).
#' @param layer_assign Named integer vector (node -> 0-based layer index).
#' @return Integer count of crossings.
#' @noRd
count_crossings <- function(layer_nodes, edges_df, layer_assign) {
  directed <- edges_df[!is.na(edges_df$to), , drop = FALSE]
  if (nrow(directed) == 0) {
    return(0L)
  }

  # Build position lookup: node -> 1-based index within its layer
  pos <- integer(0)
  for (i in seq_along(layer_nodes)) {
    for (j in seq_along(layer_nodes[[i]])) {
      pos[[layer_nodes[[i]][j]]] <- j
    }
  }

  crossings <- 0L
  max_layer <- length(layer_nodes) - 1L

  for (li in seq_len(max_layer)) {
    layer_idx <- li - 1L
    # Edges between this layer and next
    between <- directed[
      layer_assign[directed$name] == layer_idx &
        layer_assign[directed$to] == layer_idx + 1L,
      ,
      drop = FALSE
    ]
    if (nrow(between) < 2) {
      next
    }

    for (a in seq_len(nrow(between) - 1)) {
      for (b in seq(a + 1, nrow(between))) {
        u1 <- between$name[a]
        v1 <- between$to[a]
        u2 <- between$name[b]
        v2 <- between$to[b]
        if ((pos[[u1]] - pos[[u2]]) * (pos[[v1]] - pos[[v2]]) < 0) {
          crossings <- crossings + 1L
        }
      }
    }
  }

  crossings
}

#' Reorder nodes within layers to minimize edge crossings
#'
#' Uses the barycenter heuristic with iterative forward/backward sweeps.
#'
#' @param layer_nodes List of character vectors (one per layer).
#' @param edges_df Data frame with `name` and `to` columns.
#' @param layer_assign Named integer vector (node -> 0-based layer).
#' @param sweeps Number of forward+backward sweep iterations.
#' @return Reordered `layer_nodes` list.
#' @noRd
barycenter_sort <- function(layer_nodes, edges_df, layer_assign, sweeps = 40L) {
  directed <- edges_df[!is.na(edges_df$to), , drop = FALSE]
  if (nrow(directed) == 0 || length(layer_nodes) < 2) {
    return(layer_nodes)
  }

  for (s in seq_len(sweeps)) {
    # Forward pass: left to right
    for (i in seq(2, length(layer_nodes))) {
      prev_layer <- layer_nodes[[i - 1]]
      prev_pos <- stats::setNames(seq_along(prev_layer), prev_layer)

      bary <- stats::setNames(
        seq_along(layer_nodes[[i]]) * 1.0,
        layer_nodes[[i]]
      )

      for (node in layer_nodes[[i]]) {
        # Parents = nodes in prev layer with edge to this node
        parents <- directed$name[
          directed$to == node &
            directed$name %in% prev_layer
        ]
        if (length(parents) > 0) {
          bary[[node]] <- mean(prev_pos[parents])
        }
      }

      layer_nodes[[i]] <- names(sort(bary))
    }

    # Backward pass: right to left
    for (i in seq(length(layer_nodes) - 1, 1)) {
      next_layer <- layer_nodes[[i + 1]]
      next_pos <- stats::setNames(seq_along(next_layer), next_layer)

      bary <- stats::setNames(
        seq_along(layer_nodes[[i]]) * 1.0,
        layer_nodes[[i]]
      )

      for (node in layer_nodes[[i]]) {
        # Children = nodes in next layer that this node has edge to
        children <- directed$to[
          directed$name == node &
            directed$to %in% next_layer
        ]
        if (length(children) > 0) {
          bary[[node]] <- mean(next_pos[children])
        }
      }

      layer_nodes[[i]] <- names(sort(bary))
    }
  }

  layer_nodes
}

# Stage 3: Force-directed Y optimization --------------------------------------

#' Optimize Y positions using force simulation
#'
#' Adjusts vertical positions to prevent node-edge overlaps using three forces:
#' barycenter pull, bidirectional edge-avoidance, and same-layer repulsion.
#'
#' @param layer_nodes List of character vectors (from barycenter_sort).
#' @param layer_assign Named integer vector (node -> 0-based layer).
#' @param edges_df Data frame with `name` and `to` columns.
#' @param node_radius Radius of each node circle.
#' @param layer_gap Horizontal distance between time layers.
#' @param node_gap Initial vertical spacing between same-layer nodes.
#' @param min_spacing Minimum Y gap enforced between same-layer nodes.
#' @param clearance Edge-avoidance trigger distance.
#' @param iterations Number of force simulation iterations.
#' @return A list with `$x` and `$y`, both named numeric vectors.
#' @noRd
force_directed_y <- function(
  layer_nodes,
  layer_assign,
  edges_df,
  node_radius = 26,
  layer_gap = 180,
  node_gap = 85,
  min_spacing = 72,
  clearance = node_radius * 2.5 + 12,
  iterations = 350L
) {
  directed <- edges_df[!is.na(edges_df$to), , drop = FALSE]
  all_nodes <- unlist(layer_nodes)

  # Initial positions: evenly spaced within each layer, centered
  x_pos <- stats::setNames(numeric(length(all_nodes)), all_nodes)
  y_pos <- stats::setNames(numeric(length(all_nodes)), all_nodes)

  max_layer_size <- max(lengths(layer_nodes))

  for (i in seq_along(layer_nodes)) {
    nodes <- layer_nodes[[i]]
    n <- length(nodes)
    layer_idx <- i - 1L
    offset <- (max_layer_size - n) * node_gap / 2

    for (j in seq_along(nodes)) {
      x_pos[[nodes[j]]] <- layer_idx * layer_gap
      y_pos[[nodes[j]]] <- offset + (j - 1) * node_gap
    }
  }

  if (nrow(directed) == 0) {
    return(list(x = x_pos, y = y_pos))
  }

  # Precompute intermediates for each edge
  intermediates <- vector("list", nrow(directed))
  for (ei in seq_len(nrow(directed))) {
    u <- directed$name[ei]
    v <- directed$to[ei]
    u_layer <- layer_assign[[u]]
    v_layer <- layer_assign[[v]]
    lo <- min(u_layer, v_layer)
    hi <- max(u_layer, v_layer)

    if (hi - lo <= 1) {
      intermediates[[ei]] <- character(0)
      next
    }

    intermediates[[ei]] <- all_nodes[
      all_nodes != u &
        all_nodes != v &
        layer_assign[all_nodes] > lo &
        layer_assign[all_nodes] < hi
    ]
  }

  # Build neighbor lookup (all nodes connected by any edge)
  neighbors <- stats::setNames(
    vector("list", length(all_nodes)),
    all_nodes
  )
  for (ei in seq_len(nrow(directed))) {
    u <- directed$name[ei]
    v <- directed$to[ei]
    if (u %in% all_nodes && v %in% all_nodes) {
      neighbors[[u]] <- c(neighbors[[u]], v)
      neighbors[[v]] <- c(neighbors[[v]], u)
    }
  }

  # Force simulation
  forces <- stats::setNames(numeric(length(all_nodes)), all_nodes)

  for (iter in seq(0, iterations - 1)) {
    progress <- iter / iterations
    avoid_weight <- 1.2 - progress * 0.5
    bary_weight <- 0.02 + progress * 0.10
    damping <- 0.45 * max(0.12, 1 - progress * 0.7)

    forces[] <- 0

    # FORCE A: Barycenter pull
    for (node in all_nodes) {
      nbrs <- neighbors[[node]]
      if (length(nbrs) > 0) {
        avg_y <- mean(y_pos[nbrs])
        forces[[node]] <- forces[[node]] + (avg_y - y_pos[[node]]) * bary_weight
      }
    }

    # FORCE B: Edge-avoidance (bidirectional)
    for (ei in seq_len(nrow(directed))) {
      inters <- intermediates[[ei]]
      if (length(inters) == 0) {
        next
      }

      u <- directed$name[ei]
      v <- directed$to[ei]

      for (w in inters) {
        result <- y_dist_to_edge(
          x_pos[[w]],
          y_pos[[w]],
          x_pos[[u]],
          y_pos[[u]],
          x_pos[[v]],
          y_pos[[v]]
        )

        if (result$dist < clearance) {
          overlap <- clearance - result$dist
          direction <- if (y_pos[[w]] >= result$proj_y) 1 else -1
          strength <- overlap * avoid_weight * (1 + overlap / clearance)

          forces[[w]] <- forces[[w]] + direction * strength
          forces[[u]] <- forces[[u]] -
            direction * strength * 0.45 * (1 - result$t)
          forces[[v]] <- forces[[v]] - direction * strength * 0.45 * result$t
        }
      }
    }

    # FORCE C: Same-layer repulsion
    for (i in seq_along(layer_nodes)) {
      nodes <- layer_nodes[[i]]
      if (length(nodes) < 2) {
        next
      }

      sorted_idx <- order(y_pos[nodes])
      sorted <- nodes[sorted_idx]

      for (j in seq(2, length(sorted))) {
        gap <- y_pos[[sorted[j]]] - y_pos[[sorted[j - 1]]]
        if (gap < min_spacing) {
          push <- (min_spacing - gap) * 0.5
          forces[[sorted[j - 1]]] <- forces[[sorted[j - 1]]] - push
          forces[[sorted[j]]] <- forces[[sorted[j]]] + push
        }
      }
    }

    # Apply forces
    y_pos <- y_pos + forces * damping
  }

  # Enforce spacing as hard constraint after simulation
  y_pos <- enforce_spacing(y_pos, layer_assign, min_spacing)

  list(x = x_pos, y = y_pos)
}

# Stage 4: Greedy post-correction ----------------------------------------------

#' Find node-edge overlaps
#'
#' @param positions List with `$x` and `$y` (named numeric vectors).
#' @param edges_df Data frame with `name` and `to` columns.
#' @param layer_assign Named integer vector (node -> 0-based layer).
#' @param node_radius Radius of each node circle.
#' @return Data frame with columns: `edge_from`, `edge_to`, `node`, `dist`.
#' @noRd
find_overlaps <- function(positions, edges_df, layer_assign, node_radius = 26) {
  clearance <- node_radius + 8
  directed <- edges_df[!is.na(edges_df$to), , drop = FALSE]
  all_nodes <- names(positions$x)

  overlaps <- data.frame(
    edge_from = character(0),
    edge_to = character(0),
    node = character(0),
    dist = numeric(0),
    stringsAsFactors = FALSE
  )

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
          overlaps <- rbind(
            overlaps,
            data.frame(
              edge_from = u,
              edge_to = v,
              node = w,
              dist = result$dist,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
  }

  overlaps
}

#' Fix remaining overlaps with exact displacements
#'
#' @param positions List with `$x` and `$y` (named numeric vectors).
#' @param edges_df Data frame with `name` and `to` columns.
#' @param layer_assign Named integer vector (node -> 0-based layer).
#' @param node_radius Radius of each node circle.
#' @param min_spacing Minimum Y gap between same-layer nodes.
#' @param max_passes Maximum correction iterations.
#' @return Updated positions list.
#' @noRd
greedy_post_correction <- function(
  positions,
  edges_df,
  layer_assign,
  node_radius = 26,
  min_spacing = 72,
  max_passes = 50L
) {
  target_clearance <- node_radius + 12

  # Enforce spacing first
  positions$y <- enforce_spacing(positions$y, layer_assign, min_spacing)

  for (pass in seq_len(max_passes)) {
    overlaps <- find_overlaps(positions, edges_df, layer_assign, node_radius)
    if (nrow(overlaps) == 0) {
      break
    }

    # Sort by distance ascending (hardest first)
    overlaps <- overlaps[order(overlaps$dist), ]

    for (oi in seq_len(nrow(overlaps))) {
      u <- overlaps$edge_from[oi]
      v <- overlaps$edge_to[oi]
      w <- overlaps$node[oi]

      # Recompute — earlier fixes may have changed positions
      result <- y_dist_to_edge(
        positions$x[[w]],
        positions$y[[w]],
        positions$x[[u]],
        positions$y[[u]],
        positions$x[[v]],
        positions$y[[v]]
      )
      if (result$dist >= target_clearance) {
        next
      }

      needed <- target_clearance - result$dist + 2
      direction <- if (positions$y[[w]] >= result$proj_y) 1 else -1
      t_safe <- max(0.1, min(0.9, result$t))

      # Distribute: 55% to intermediate node, 45% to endpoints
      positions$y[[w]] <- positions$y[[w]] + direction * needed * 0.55
      positions$y[[u]] <- positions$y[[u]] -
        direction * needed * 0.45 * (1 - t_safe)
      positions$y[[v]] <- positions$y[[v]] - direction * needed * 0.45 * t_safe
    }

    # Re-enforce spacing
    positions$y <- enforce_spacing(positions$y, layer_assign, min_spacing)
  }

  positions
}

# Orchestrator -----------------------------------------------------------------

#' Compute overlap-free time-ordered layout
#'
#' Runs the full 4-stage algorithm: longest-path layer assignment, barycenter
#' crossing minimization, force-directed Y optimization, and greedy
#' post-correction. Returns normalized coordinates suitable for ggdag.
#'
#' @param edges_df Data frame with `name` and `to` columns (from `edges2df()`).
#' @param direction Either `"x"` (default, time on x-axis) or `"y"`.
#' @param node_radius Node circle radius for overlap detection.
#' @param layer_gap Horizontal distance between layers (internal).
#' @param node_gap Initial vertical spacing between same-layer nodes.
#' @param min_spacing Minimum Y gap enforced between same-layer nodes.
#' @param iterations Force simulation iterations.
#' @param sweeps Barycenter sweep iterations.
#' @param max_correction_passes Maximum greedy correction iterations.
#' @return A tibble with columns `name`, `x`, `y`.
#' @noRd
compute_time_ordered_layout <- function(
  edges_df,
  direction = "x",
  sort_direction = "right",
  fixed_time = NULL,
  exposure = character(0),
  outcome = character(0),
  adjust_exposure_outcome = TRUE,
  node_radius = 26,
  layer_gap = 180,
  node_gap = 85,
  min_spacing = 72,
  iterations = 350L,
  sweeps = 40L,
  max_correction_passes = 50L,
  ...
) {
  edges_df$name <- as.character(edges_df$name)
  edges_df$to <- as.character(edges_df$to)

  # Filter out bidirected edges — only directed edges drive stages 2-4
  has_direction <- "direction" %in% names(edges_df)
  if (has_direction) {
    is_bidir <- !is.na(edges_df$to) &
      !is.na(edges_df$direction) &
      edges_df$direction == "<->"
    directed <- edges_df[!is.na(edges_df$to) & !is_bidir, , drop = FALSE]
  } else {
    directed <- edges_df[!is.na(edges_df$to), , drop = FALSE]
  }

  # Convert user-facing 1-based fixed_time to internal 0-based layers
  internal_fixed_time <- fixed_time
  if (!is.null(internal_fixed_time) && length(internal_fixed_time) > 0) {
    internal_fixed_time <- stats::setNames(
      as.integer(internal_fixed_time) - 1L,
      names(internal_fixed_time)
    )
  }

  # Stage 1: Layer assignment (handles bidirected internally)
  layer_assign <- longest_path_layers(
    edges_df,
    sort_direction = sort_direction,
    fixed_time = internal_fixed_time
  )

  # Exposure/outcome same-layer adjustment
  if (
    isTRUE(adjust_exposure_outcome) &&
      length(exposure) > 0 &&
      length(outcome) > 0
  ) {
    pinned <- if (!is.null(internal_fixed_time)) {
      names(internal_fixed_time)
    } else {
      character(0)
    }

    for (exp_node in exposure) {
      for (out_node in outcome) {
        if (
          !(exp_node %in% names(layer_assign)) ||
            !(out_node %in% names(layer_assign))
        ) {
          next
        }
        if (layer_assign[[exp_node]] == layer_assign[[out_node]]) {
          # Check if outcome is pinned — if so, skip with message
          if (out_node %in% pinned) {
            cli::cli_inform(
              c(
                "Outcome {.val {out_node}} shares a layer with exposure
{.val {exp_node}}, but was not shifted because it has a
{.arg fixed_time} pin.",
                "i" = "Remove the pin or adjust it manually to separate them."
              ),
              class = "ggdag_message"
            )
            next
          }
          # Shift outcome and all descendants +1
          descendants <- find_descendants(out_node, directed)
          shift_nodes <- c(out_node, descendants)
          for (nd in shift_nodes) {
            if (nd %in% names(layer_assign) && !(nd %in% pinned)) {
              layer_assign[[nd]] <- layer_assign[[nd]] + 1L
            }
          }
        }
      }
    }
  }

  # Stage 2: Build layer_nodes and barycenter sort
  max_layer <- max(layer_assign)
  layer_nodes <- lapply(seq(0, max_layer), function(l) {
    names(layer_assign[layer_assign == l])
  })

  if (nrow(directed) > 0) {
    layer_nodes <- barycenter_sort(layer_nodes, directed, layer_assign, sweeps)

    # Stage 3: Force-directed Y optimization
    positions <- force_directed_y(
      layer_nodes,
      layer_assign,
      directed,
      node_radius = node_radius,
      layer_gap = layer_gap,
      node_gap = node_gap,
      min_spacing = min_spacing,
      clearance = node_radius * 2.5 + 12,
      iterations = iterations
    )

    # Stage 4: Greedy post-correction
    positions <- greedy_post_correction(
      positions,
      directed,
      layer_assign,
      node_radius = node_radius,
      min_spacing = min_spacing,
      max_passes = max_correction_passes
    )
  } else {
    # No edges — just assign positions by layer
    all_nodes <- unlist(layer_nodes)
    x_pos <- stats::setNames(numeric(length(all_nodes)), all_nodes)
    y_pos <- stats::setNames(numeric(length(all_nodes)), all_nodes)
    for (i in seq_along(layer_nodes)) {
      for (j in seq_along(layer_nodes[[i]])) {
        x_pos[[layer_nodes[[i]][j]]] <- (i - 1) * layer_gap
        y_pos[[layer_nodes[[i]][j]]] <- (j - 1) * node_gap
      }
    }
    positions <- list(x = x_pos, y = y_pos)
  }

  # Normalize: x → integer layer indices (1, 2, 3, ...)
  # y → centered per layer, scaled so avg spacing ≈ 1
  normalize_positions(
    positions,
    layer_assign,
    direction,
    fixed_time = fixed_time
  )
}

#' Normalize pixel-space positions to ggdag-friendly coordinates
#'
#' @param positions List with `$x` and `$y` (named numeric vectors).
#' @param layer_assign Named integer vector (node -> 0-based layer).
#' @param direction `"x"` or `"y"` — swap axes if `"y"`.
#' @return A tibble with `name`, `x`, `y`.
#' @noRd
normalize_positions <- function(
  positions,
  layer_assign,
  direction = "x",
  fixed_time = NULL
) {
  node_names <- names(positions$x)

  # x: map layer indices to sequential integers
  # When fixed_time is used, preserve the actual layer values (shifted to start

  # at 1) so pinned nodes keep their requested time point.
  unique_layers <- sort(unique(layer_assign))
  if (!is.null(fixed_time) && length(fixed_time) > 0) {
    # Preserve user's 1-based time points: internal 0-based + 1
    layer_map <- stats::setNames(
      unique_layers + 1L,
      as.character(unique_layers)
    )
  } else {
    layer_map <- stats::setNames(
      seq_along(unique_layers),
      as.character(unique_layers)
    )
  }
  norm_x <- vapply(
    node_names,
    function(n) {
      layer_map[[as.character(layer_assign[[n]])]]
    },
    numeric(1)
  )

  # y: center globally, then scale
  norm_y <- positions$y

  # Center the whole graph at y = 0
  global_center <- mean(norm_y)
  norm_y <- norm_y - global_center

  # Scale so average gap between consecutive same-layer nodes ≈ 1
  all_gaps <- numeric(0)
  for (layer in unique_layers) {
    nodes_in <- node_names[layer_assign[node_names] == layer]
    if (length(nodes_in) > 1) {
      sorted_y <- sort(norm_y[nodes_in])
      gaps <- diff(sorted_y)
      all_gaps <- c(all_gaps, gaps)
    }
  }

  if (length(all_gaps) > 0 && mean(all_gaps) > 0) {
    scale_factor <- mean(all_gaps)
    norm_y <- norm_y / scale_factor
  }

  if (direction == "y") {
    tibble::tibble(name = node_names, x = unname(norm_y), y = unname(norm_x))
  } else {
    tibble::tibble(name = node_names, x = unname(norm_x), y = unname(norm_y))
  }
}
