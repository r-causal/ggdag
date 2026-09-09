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

#' Split an edge data frame into directed and bidirected edges
#'
#' A bidirected edge constrains its two nodes to share a layer; only directed
#' edges order one node before another.
#'
#' @param edges_df A data frame with columns `name` and `to`, optionally
#'   `direction`.
#' @return A list with `directed` and `bidirected` data frames.
#' @noRd
split_edge_types <- function(edges_df) {
  if ("direction" %in% names(edges_df)) {
    is_bidirected <- !is.na(edges_df$to) &
      !is.na(edges_df$direction) &
      edges_df$direction == "<->"
  } else {
    is_bidirected <- rep(FALSE, nrow(edges_df))
  }

  list(
    directed = edges_df[!is.na(edges_df$to) & !is_bidirected, , drop = FALSE],
    bidirected = edges_df[is_bidirected, , drop = FALSE]
  )
}

#' Group nodes joined by bidirected edges
#'
#' Union-find over the bidirected edges, so a chain such as `a <-> b <-> c`
#' becomes one group.
#'
#' @param bidirected Data frame of bidirected edges with `name` and `to`.
#' @return A list of character vectors, one per group.
#' @noRd
bidirected_groups <- function(bidirected) {
  groups <- list()
  for (i in seq_len(nrow(bidirected))) {
    u <- bidirected$name[i]
    v <- bidirected$to[i]
    u_grp <- which(vapply(groups, function(g) u %in% g, logical(1)))
    v_grp <- which(vapply(groups, function(g) v %in% g, logical(1)))

    if (length(u_grp) == 0 && length(v_grp) == 0) {
      groups <- c(groups, list(c(u, v)))
    } else if (length(u_grp) > 0 && length(v_grp) == 0) {
      groups[[u_grp[1]]] <- c(groups[[u_grp[1]]], v)
    } else if (length(u_grp) == 0 && length(v_grp) > 0) {
      groups[[v_grp[1]]] <- c(groups[[v_grp[1]]], u)
    } else if (u_grp[1] != v_grp[1]) {
      groups[[u_grp[1]]] <- c(groups[[u_grp[1]]], groups[[v_grp[1]]])
      groups <- groups[-v_grp[1]]
    }
  }

  lapply(groups, unique)
}

#' Map each node to the representative of its bidirected group
#'
#' Nodes outside any group represent themselves, so the result condenses the
#' graph: one entry per supernode.
#'
#' @param all_nodes Character vector of every node in the graph.
#' @param groups List of bidirected groups.
#' @return A named character vector mapping node name to representative.
#' @noRd
group_representatives <- function(all_nodes, groups) {
  reps <- stats::setNames(all_nodes, all_nodes)
  for (grp in groups) {
    members <- grp[grp %in% all_nodes]
    if (length(members) < 2) {
      next
    }
    reps[members] <- members[[1]]
  }

  reps
}

#' Find the condensed nodes that lie on a cycle
#'
#' Condensing a bidirected group can create a cycle where the directed graph
#' had none: a directed path between two members of a group becomes a
#' self-loop, and two groups joined in both directions become a two-node
#' cycle. Every cycle lies inside one strongly connected component, so a
#' component holding more than one node, or a node with an edge to itself,
#' names exactly the supernodes whose shared layer cannot hold. A supernode
#' merely downstream of one of those is not itself on a cycle and keeps its
#' group.
#'
#' @param reps Named character vector from `group_representatives()`.
#' @param directed Data frame of directed edges with `name` and `to`.
#' @return Character vector of representative names caught in a cycle.
#' @noRd
cyclic_supernodes <- function(reps, directed) {
  supernodes <- unique(unname(reps))
  if (length(supernodes) == 0 || nrow(directed) == 0) {
    return(character(0))
  }

  src <- unname(reps[directed$name])
  tgt <- unname(reps[directed$to])

  # A directed edge between two members of one group condenses to a self-loop,
  # which igraph does not count as a strongly connected component of its own
  self_looped <- unique(src[src == tgt])

  condensed <- igraph::graph_from_data_frame(
    data.frame(from = src, to = tgt, stringsAsFactors = FALSE),
    vertices = data.frame(name = supernodes, stringsAsFactors = FALSE)
  )
  components <- igraph::components(condensed, mode = "strong")
  membership <- components$membership
  on_cycle <- names(membership)[membership %in% which(components$csize >= 2L)]

  unique(c(self_looped, on_cycle))
}

#' Drop the bidirected groups whose shared layer is impossible
#'
#' A bidirected edge asks its two nodes to share a time layer; a directed edge
#' asks its source to come strictly earlier than its target. When the directed
#' edges order two members of a group, the two demands contradict each other,
#' and the same-layer demand is the one that gives way: keeping it would draw
#' a directed edge backwards in time, which is the invariant the layout exists
#' to guarantee. The arrow between the two nodes already shows the reader why
#' they sit on different layers, and `x -> y` alongside `x <-> y` is common
#' enough that saying so every time would make ordinary plots chatty, so the
#' dropped constraint is not reported.
#'
#' When two groups order each other, as `a <-> b` and `c <-> d` do with
#' `a -> c` and `d -> b`, either group alone could be kept. Both are dropped
#' rather than picking one, since nothing in the DAG favors either choice and
#' a silent arbitrary pick would be harder to read than the directed order.
#'
#' @param edges_df A data frame with columns `name` and `to`, optionally
#'   `direction`.
#' @return A list of the bidirected groups that can share a layer.
#' @noRd
resolve_bidirected_groups <- function(edges_df) {
  parts <- split_edge_types(edges_df)
  groups <- bidirected_groups(parts$bidirected)
  if (length(groups) == 0) {
    return(groups)
  }

  all_nodes <- unique(c(edges_df$name, edges_df$to))
  all_nodes <- all_nodes[!is.na(all_nodes)]

  repeat {
    reps <- group_representatives(all_nodes, groups)
    stuck <- cyclic_supernodes(reps, parts$directed)
    if (length(stuck) == 0) {
      break
    }

    impossible <- vapply(
      groups,
      function(grp) {
        members <- grp[grp %in% all_nodes]
        length(members) >= 2 && reps[[members[[1]]]] %in% stuck
      },
      logical(1)
    )
    if (!any(impossible)) {
      break
    }

    groups <- groups[!impossible]
  }

  groups
}

#' Find a condensed node and everything upstream of it
#'
#' Used to report which pins pushed an unpinned node to the layer it holds.
#'
#' @param node Character scalar: a supernode representative.
#' @param adj Condensed adjacency list (representative -> child
#'   representatives).
#' @param topo_order Character vector: a topological order of the condensed
#'   graph.
#' @return Character vector of representatives, including `node`.
#' @noRd
condensed_ancestors <- function(node, adj, topo_order) {
  upstream <- stats::setNames(logical(length(topo_order)), topo_order)
  upstream[[node]] <- TRUE

  # Walking the order backwards visits every parent after its children, so one
  # pass marks the whole ancestry
  for (parent in rev(topo_order)) {
    if (any(upstream[adj[[parent]]])) {
      upstream[[parent]] <- TRUE
    }
  }

  names(upstream)[upstream]
}

#' Collect the nodes that have to move with a shifted node
#'
#' A node's directed descendants follow it forward in time, and so does any
#' node a bidirected edge ties to the same layer, together with that node's
#' own descendants. Closing over both relations keeps the same-layer
#' constraint intact through the exposure/outcome adjustment.
#'
#' @param node Character scalar: the node being shifted.
#' @param directed_edges Data frame of directed edges with `name` and `to`.
#' @param groups List of bidirected groups.
#' @return Character vector of node names, including `node`.
#' @noRd
shift_closure <- function(node, directed_edges, groups) {
  members <- unique(c(node, find_descendants(node, directed_edges)))

  repeat {
    added <- character(0)
    for (grp in groups) {
      if (any(grp %in% members)) {
        added <- c(added, setdiff(grp, members))
      }
    }
    added <- unique(added)
    if (length(added) == 0) {
      break
    }
    members <- unique(c(members, added))
    for (partner in added) {
      members <- unique(c(members, find_descendants(partner, directed_edges)))
    }
  }

  members
}

#' Assign nodes to time layers
#'
#' Modified Kahn's algorithm (BFS topological sort) that tracks the longest
#' incoming path to each node. Nodes joined by bidirected edges have to share
#' a layer, so each group is condensed into a single supernode before the
#' passes run and expanded again afterwards; laying out the condensed graph
#' satisfies the same-layer constraint without moving anything after the fact,
#' which is what let earlier versions place a node before its own parent.
#'
#' With `sort_direction = "left"`, nodes are placed as far left (early) as
#' possible: each node sits one layer after its latest parent. With
#' `sort_direction = "right"` (default), a backward pass over the reversed
#' topological order then moves each node to one layer before its earliest
#' child, placing nodes as close as possible to their descendants.
#'
#' @param edges_df A data frame with columns `name` (source) and `to` (target).
#'   Rows with `to = NA` represent terminal or isolated nodes.
#' @param sort_direction Either `"right"` (close to descendants, default) or
#'   `"left"` (close to ancestors).
#' @param fixed_time Named vector of 0-based layers to pin nodes to.
#' @param groups Bidirected groups from `resolve_bidirected_groups()`. Computed
#'   from `edges_df` when not supplied.
#' @return A named integer vector mapping node names to 0-based layer indices.
#' @noRd
longest_path_layers <- function(
  edges_df,
  sort_direction = "right",
  fixed_time = NULL,
  groups = NULL
) {
  edges_df$name <- as.character(edges_df$name)
  edges_df$to <- as.character(edges_df$to)
  all_nodes <- unique(c(edges_df$name, edges_df$to))
  all_nodes <- all_nodes[!is.na(all_nodes)]

  directed <- split_edge_types(edges_df)$directed
  groups <- groups %||% resolve_bidirected_groups(edges_df)
  reps <- group_representatives(all_nodes, groups)
  supernodes <- unique(unname(reps))

  # Build condensed adjacency list and in-degree, one entry per supernode
  adj <- stats::setNames(vector("list", length(supernodes)), supernodes)
  in_deg <- stats::setNames(integer(length(supernodes)), supernodes)

  for (i in seq_len(nrow(directed))) {
    src <- reps[[directed$name[i]]]
    tgt <- reps[[directed$to[i]]]
    adj[[src]] <- c(adj[[src]], tgt)
    in_deg[[tgt]] <- in_deg[[tgt]] + 1L
  }

  # Forward pass: longest path from roots, i.e. the earliest layer each
  # supernode can occupy. The dequeue order is a topological order.
  dist <- stats::setNames(integer(length(supernodes)), supernodes)
  queue <- names(in_deg[in_deg == 0L])
  topo_order <- character(0)

  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    topo_order <- c(topo_order, node)

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

  earliest <- dist
  pinned_supernodes <- character(0)

  # Apply fixed_time pins
  if (!is.null(fixed_time) && length(fixed_time) > 0) {
    pin_names <- names(fixed_time)
    fixed_time <- stats::setNames(as.integer(fixed_time), pin_names)

    # Warn and drop unknown nodes
    unknown <- setdiff(pin_names, all_nodes)
    if (length(unknown) > 0) {
      warn(c(
        "{.arg fixed_time} contains node{?s} not in the DAG: {.val {unknown}}.",
        "i" = "These will be ignored."
      ))
      fixed_time <- fixed_time[pin_names %in% all_nodes]
      pin_names <- names(fixed_time)
    }
  } else {
    fixed_time <- NULL
  }

  if (length(fixed_time) > 0) {
    pin_supernodes <- stats::setNames(unname(reps[pin_names]), pin_names)
    pinned_supernodes <- unique(unname(pin_supernodes))

    # Members of one bidirected group share a layer, so their pins must agree
    for (supernode in pinned_supernodes) {
      grp_pinned <- pin_names[pin_supernodes == supernode]
      pin_values <- unname(fixed_time[grp_pinned])
      if (length(grp_pinned) >= 2 && length(unique(pin_values)) > 1) {
        abort(
          c(
            "Conflicting {.arg fixed_time} values in bidirected group.",
            "x" = "Nodes {.val {grp_pinned}} are connected by bidirected edges and must share the same layer,
but are pinned to different times: {.val {pin_values + 1L}}."
          ),
          error_class = "ggdag_dag_error"
        )
      }
    }

    # A pin cannot come before the earliest layer a node's ancestors allow
    for (nm in pin_names) {
      floor_layer <- earliest[[pin_supernodes[[nm]]]]
      if (fixed_time[[nm]] < floor_layer) {
        abort(
          c(
            "Pinned time {fixed_time[[nm]] + 1L} for {.val {nm}} is too early.",
            "x" = "{.val {nm}} has ancestors requiring at least time {floor_layer + 1L}."
          ),
          error_class = "ggdag_dag_error"
        )
      }
    }

    # Validate: no directed edge has parent pinned >= child pinned
    for (i in seq_len(nrow(directed))) {
      src <- directed$name[i]
      tgt <- directed$to[i]
      if (src %in% pin_names && tgt %in% pin_names) {
        if (fixed_time[[src]] >= fixed_time[[tgt]]) {
          abort(
            c(
              "Pinned times violate DAG ordering.",
              "x" = "{.val {src}} (time {fixed_time[[src]] + 1L}) must be before {.val {tgt}} (time {fixed_time[[tgt]] + 1L})."
            ),
            error_class = "ggdag_dag_error"
          )
        }
      }
    }

    # Override pinned supernodes
    for (nm in pin_names) {
      dist[[pin_supernodes[[nm]]]] <- fixed_time[[nm]]
    }

    # Re-propagate: ensure all non-pinned descendants respect ordering
    for (node in topo_order) {
      for (child in adj[[node]]) {
        if (child %nin% pinned_supernodes) {
          min_valid <- dist[[node]] + 1L
          if (dist[[child]] < min_valid) {
            dist[[child]] <- min_valid
          }
        }
      }
    }

    # A pinned node the re-propagation could not move may now sit at or before
    # one of its own ancestors. The nodes at the failing edge are often
    # unpinned intermediates, so name the pins that put them there instead.
    for (node in topo_order) {
      for (child in adj[[node]]) {
        if (dist[[child]] <= dist[[node]]) {
          late_pins <- pin_names[pin_supernodes == child]
          early_pins <- pin_names[
            pin_supernodes %in% condensed_ancestors(node, adj, topo_order)
          ]
          abort(
            c(
              "Pinned times violate DAG ordering.",
              "x" = "{.val {late_pins}} {?is/are} pinned to time {dist[[child]] + 1L}, but {.val {early_pins}} push{?es/} it to time {dist[[node]] + 2L} at the earliest.",
              "i" = "Move {.val {late_pins}} later, or move {.val {early_pins}} earlier."
            ),
            error_class = "ggdag_dag_error"
          )
        }
      }
    }
  }

  # For "right": backward pass pushing nodes toward their children. Walking the
  # reversed topological order means every child already holds its final layer,
  # so each node really does land one layer before its earliest child rather
  # than being left behind by a child that moved after it. Pinned supernodes
  # are never moved; sinks keep their forward layers, so the span is unchanged.
  if (identical(sort_direction, "right")) {
    for (node in rev(topo_order)) {
      if (node %in% pinned_supernodes) {
        next
      }
      children <- adj[[node]]
      if (length(children) > 0) {
        dist[[node]] <- min(dist[children]) - 1L
      }
    }
  }

  # Expand the supernodes back out: every member of a group shares its layer
  stats::setNames(unname(dist[reps[all_nodes]]), all_nodes)
}

# Stage 2: Barycenter crossing minimization ------------------------------------

#' Count edge crossings between adjacent layers
#'
#' @param layer_nodes List of character vectors, one per layer (ordered by
#'   within-layer position).
#' @param edges_df Data frame with `name` and `to` columns. Rows with
#'   `to = NA` or naming nodes absent from `layer_assign` are ignored.
#' @param layer_assign Named integer vector (node -> 0-based layer index).
#' @return Integer count of crossings.
#' @noRd
count_crossings <- function(layer_nodes, edges_df, layer_assign) {
  directed <- edges_df[!is.na(edges_df$to), , drop = FALSE]
  if (nrow(directed) == 0) {
    return(0L)
  }

  from_layer <- layer_assign[directed$name]
  to_layer <- layer_assign[directed$to]

  crossings <- 0L
  for (li in seq_len(length(layer_nodes) - 1L)) {
    # Edges between this layer and next
    between <- which(from_layer == li - 1L & to_layer == li)
    if (length(between) < 2L) {
      next
    }
    crossings <- crossings +
      count_crossings_bilayer(
        layer_nodes[[li]],
        layer_nodes[[li + 1L]],
        directed[between, , drop = FALSE]
      )
  }

  crossings
}

#' Reorder nodes within layers to minimize edge crossings
#'
#' Uses the barycenter heuristic with iterative forward/backward sweeps.
#' Each pass runs `barycenter_reorder()`, so a node moves to the mean
#' position of its parents (forward) or children (backward) in the fixed
#' neighbor layer, with a stable sort keeping the incumbent order on ties.
#' Sweeping stops early once a full forward-plus-backward sweep leaves every
#' layer unchanged, since further passes could not move anything.
#'
#' @param layer_nodes List of character vectors (one per layer).
#' @param edges_df Data frame with `name` and `to` columns.
#' @param sweeps Number of forward+backward sweep iterations.
#' @return Reordered `layer_nodes` list.
#' @noRd
barycenter_sort <- function(layer_nodes, edges_df, sweeps = 40L) {
  directed <- edges_df[!is.na(edges_df$to), , drop = FALSE]
  if (nrow(directed) == 0 || length(layer_nodes) < 2) {
    return(layer_nodes)
  }

  parents_of <- split(directed$name, directed$to)
  children_of <- split(directed$to, directed$name)

  for (s in seq_len(sweeps)) {
    before <- layer_nodes

    # Forward pass: left to right
    for (i in seq(2, length(layer_nodes))) {
      layer_nodes[[i]] <- barycenter_reorder(
        layer_nodes[[i]],
        layer_nodes[[i - 1]],
        parents_of
      )
    }

    # Backward pass: right to left
    for (i in seq(length(layer_nodes) - 1, 1)) {
      layer_nodes[[i]] <- barycenter_reorder(
        layer_nodes[[i]],
        layer_nodes[[i + 1]],
        children_of
      )
    }

    if (identical(layer_nodes, before)) {
      break
    }
  }

  layer_nodes
}

# Stage 3: Force-directed Y optimization --------------------------------------

#' Median-based initial Y positions from an augmented layer ordering
#'
#' Dummy-aware median sweeps over the augmented graph described by
#' `order_layers()$augmented`. Positions start evenly spaced within each
#' augmented layer, then alternating forward passes (each node moves to the
#' median of its parents' positions) and backward passes (median of its
#' children's positions) pull connected nodes into vertical alignment; after
#' each layer update the within-layer order is restored with a `node_gap`
#' minimum gap. Because the dummy chains route multi-layer edges through
#' every intermediate layer, long edges pull their endpoints together layer
#' by layer instead of being invisible to the sweeps. The dummies are dropped
#' at the end, so the real nodes' positions can seed `force_directed_y()`.
#'
#' @param augmented The `augmented` element of an `order_layers()` result:
#'   a list with `layer_nodes`, `layer_assign`, and `edges`.
#' @param node_gap Vertical spacing unit between same-layer nodes.
#' @param sweeps Number of forward-plus-backward sweep iterations.
#' @return A named numeric vector of Y positions covering every real node.
#' @noRd
median_y_init <- function(augmented, node_gap = 85, sweeps = 4L) {
  aug_layers <- augmented$layer_nodes
  aug_edges <- augmented$edges[!is.na(augmented$edges$to), , drop = FALSE]

  max_size <- max(lengths(aug_layers))
  y <- numeric(0)
  for (i in seq_along(aug_layers)) {
    nodes <- aug_layers[[i]]
    offset <- (max_size - length(nodes)) * node_gap / 2
    y[nodes] <- offset + (seq_along(nodes) - 1) * node_gap
  }

  fix_layer <- function(y, nodes) {
    yy <- y[nodes]
    if (length(nodes) > 1) {
      for (j in seq(2, length(nodes))) {
        if (yy[j] - yy[j - 1] < node_gap) {
          yy[j] <- yy[j - 1] + node_gap
        }
      }
    }
    y[nodes] <- yy
    y
  }

  for (s in seq_len(sweeps)) {
    for (i in seq_along(aug_layers)[-1]) {
      for (node in aug_layers[[i]]) {
        parents <- aug_edges$name[aug_edges$to == node]
        if (length(parents) > 0) {
          y[[node]] <- stats::median(y[parents])
        }
      }
      y <- fix_layer(y, aug_layers[[i]])
    }
    for (i in rev(seq_along(aug_layers)[-length(aug_layers)])) {
      for (node in aug_layers[[i]]) {
        children <- aug_edges$to[aug_edges$name == node]
        if (length(children) > 0) {
          y[[node]] <- stats::median(y[children])
        }
      }
      y <- fix_layer(y, aug_layers[[i]])
    }
  }

  y[!startsWith(names(y), dummy_node_prefix)]
}

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
#' @param y_init Optional named numeric vector of initial Y positions
#'   covering every node, such as the result of `median_y_init()`. `NULL`
#'   falls back to even spacing within each layer.
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
  iterations = 350L,
  y_init = NULL
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

  if (!is.null(y_init)) {
    y_pos <- y_init[all_nodes]
  }

  if (nrow(directed) == 0) {
    return(list(x = x_pos, y = y_pos))
  }

  # The simulation below runs hundreds of iterations, so every per-iteration
  # lookup works on integer indices into all_nodes precomputed here; the
  # arithmetic itself is untouched, so positions come out bit-identical to
  # the name-indexed loops this replaces

  # Precompute intermediates for each edge, as indices into all_nodes
  n_edges <- nrow(directed)
  edge_from <- match(directed$name, all_nodes)
  edge_to <- match(directed$to, all_nodes)
  intermediates <- vector("list", n_edges)
  for (ei in seq_len(n_edges)) {
    u <- directed$name[ei]
    v <- directed$to[ei]
    u_layer <- layer_assign[[u]]
    v_layer <- layer_assign[[v]]
    lo <- min(u_layer, v_layer)
    hi <- max(u_layer, v_layer)

    if (hi - lo <= 1) {
      intermediates[[ei]] <- integer(0)
      next
    }

    intermediates[[ei]] <- which(
      all_nodes != u &
        all_nodes != v &
        layer_assign[all_nodes] > lo &
        layer_assign[all_nodes] < hi
    )
  }
  # Flatten the edge-avoidance work into one vector per (edge, intermediate)
  # pair, ordered edge by edge and intermediate by intermediate exactly as
  # the nested loops would visit them. The x geometry never changes during
  # the simulation, so it is computed once here.
  pair_w <- unlist(intermediates)
  n_pairs <- length(pair_w)
  pair_count <- lengths(intermediates)
  pair_u <- rep(edge_from, pair_count)
  pair_v <- rep(edge_to, pair_count)
  pair_wx <- x_pos[pair_w]
  pair_ax <- x_pos[pair_u]
  pair_dx <- x_pos[pair_v] - pair_ax

  # Build neighbor lookup (all nodes connected by any edge)
  neighbors <- vector("list", length(all_nodes))
  for (ei in seq_len(n_edges)) {
    u <- edge_from[ei]
    v <- edge_to[ei]
    if (!is.na(u) && !is.na(v)) {
      neighbors[[u]] <- c(neighbors[[u]], v)
      neighbors[[v]] <- c(neighbors[[v]], u)
    }
  }
  # The mean of a single value is that value, so single-neighbor nodes skip
  # the mean() call entirely
  single_neighbor <- which(lengths(neighbors) == 1)
  multi_neighbor <- which(lengths(neighbors) > 1)
  single_neighbor_of <- vapply(
    neighbors[single_neighbor],
    identity,
    integer(1)
  )

  # Same-layer node groups, as indices into all_nodes, with the inner pair
  # walk of the repulsion pass hoisted out of the iteration loop
  layer_index <- lapply(layer_nodes, function(nodes) match(nodes, all_nodes))
  multi_node_layers <- which(lengths(layer_index) >= 2)
  layer_pair_walk <- lapply(layer_index, function(idx) {
    if (length(idx) >= 2) seq(2, length(idx)) else integer(0)
  })

  # Force simulation. Arithmetic on named vectors copies the names at every
  # step, so the simulation runs on bare positions and the names return at
  # the end.
  pair_wx <- unname(pair_wx)
  pair_ax <- unname(pair_ax)
  pair_dx <- unname(pair_dx)
  y_names <- names(y_pos)
  y_pos <- unname(y_pos)
  forces <- numeric(length(all_nodes))

  for (iter in seq(0, iterations - 1)) {
    progress <- iter / iterations
    avoid_weight <- 1.2 - progress * 0.5
    bary_weight <- 0.02 + progress * 0.10
    damping <- 0.45 * max(0.12, 1 - progress * 0.7)

    forces[] <- 0

    # FORCE A: Barycenter pull. Each node only reads positions and writes
    # its own force, so the two groups can run in any order
    forces[single_neighbor] <- forces[single_neighbor] +
      (y_pos[single_neighbor_of] - y_pos[single_neighbor]) * bary_weight
    for (node in multi_neighbor) {
      avg_y <- mean.default(y_pos[neighbors[[node]]])
      forces[node] <- forces[node] + (avg_y - y_pos[node]) * bary_weight
    }

    # FORCE B: Edge-avoidance (bidirectional). The projection geometry of
    # y_dist_to_edge() is computed for every (edge, intermediate) pair at
    # once, elementwise, then the affected nodes accumulate their pushes in
    # the order the original nested loops visited them. A degenerate edge
    # (near-zero length) takes no part, as the per-pair distance helper
    # would have reported an infinite distance for it.
    if (n_pairs > 0) {
      ay <- y_pos[pair_u]
      dy <- y_pos[pair_v] - ay
      len2 <- pair_dx * pair_dx + dy * dy
      wy <- y_pos[pair_w]
      t <- ((pair_wx - pair_ax) * pair_dx + (wy - ay) * dy) / len2
      proj_y <- ay + t * dy
      dist <- abs(wy - proj_y)
      hits <- which(
        len2 >= 1e-10 & t >= 0.005 & t <= 0.995 & dist < clearance
      )

      for (h in hits) {
        w <- pair_w[h]
        u <- pair_u[h]
        v <- pair_v[h]
        overlap <- clearance - dist[h]
        direction <- if (wy[h] >= proj_y[h]) 1 else -1
        strength <- overlap * avoid_weight * (1 + overlap / clearance)

        forces[w] <- forces[w] + direction * strength
        forces[u] <- forces[u] - direction * strength * 0.45 * (1 - t[h])
        forces[v] <- forces[v] - direction * strength * 0.45 * t[h]
      }
    }

    # FORCE C: Same-layer repulsion. An already-sorted layer keeps its
    # order, exactly what the stable sort would return
    for (i in multi_node_layers) {
      nodes <- layer_index[[i]]
      layer_y <- y_pos[nodes]
      sorted <- if (is.unsorted(layer_y)) nodes[order(layer_y)] else nodes

      for (j in layer_pair_walk[[i]]) {
        gap <- y_pos[sorted[j]] - y_pos[sorted[j - 1]]
        if (gap < min_spacing) {
          push <- (min_spacing - gap) * 0.5
          forces[sorted[j - 1]] <- forces[sorted[j - 1]] - push
          forces[sorted[j]] <- forces[sorted[j]] + push
        }
      }
    }

    # Apply forces
    y_pos <- y_pos + forces * damping
  }

  names(y_pos) <- y_names

  # Enforce spacing as hard constraint after simulation
  y_pos <- enforce_spacing(y_pos, layer_assign, min_spacing)

  list(x = x_pos, y = y_pos)
}

# Stage 4: Greedy post-correction ----------------------------------------------

#' Find nodes too close to drawn bidirected arcs
#'
#' Traces each bidirected edge as the arc it is drawn with (curvature
#' `arc_curvature`) and reports every node, other than the two
#' endpoints, whose center comes closer to the arc than `node_radius + 8`,
#' mirroring the straight-line detection threshold in `find_overlaps()`.
#' Unlike the straight-line check, the arc of a same-layer pair bows into
#' the neighboring column, so no layer-between filter applies: every other
#' node is a candidate.
#'
#' @param positions List with `$x` and `$y` (named numeric vectors).
#' @param bidirected Data frame of bidirected edges with `name` and `to`.
#' @param node_radius Radius of each node circle.
#' @param arc_curvature Curvature the drawn arcs are traced at, matching the
#'   `curvature` option the edge geoms read.
#' @return Data frame with columns: `edge_from`, `edge_to`, `node`, `dist`.
#' @noRd
find_arc_overlaps <- function(
  positions,
  bidirected,
  node_radius,
  arc_curvature
) {
  clearance <- node_radius + 8
  all_nodes <- names(positions$x)

  overlaps <- data.frame(
    edge_from = character(0),
    edge_to = character(0),
    node = character(0),
    dist = numeric(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(bidirected))) {
    u <- bidirected$name[i]
    v <- bidirected$to[i]
    pts <- sample_curved_edge(
      positions$x[[u]],
      positions$y[[u]],
      positions$x[[v]],
      positions$y[[v]],
      arc_curvature
    )
    for (w in setdiff(all_nodes, c(u, v))) {
      dist <- min(
        sqrt((positions$x[[w]] - pts$x)^2 + (positions$y[[w]] - pts$y)^2)
      )
      if (dist < clearance) {
        overlaps <- rbind(
          overlaps,
          data.frame(
            edge_from = u,
            edge_to = v,
            node = w,
            dist = dist,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  overlaps
}

#' Find nodes too close to the drawn arcs of spanning directed edges
#'
#' Traces each directed edge that spans two or more layers as the arc the
#' `arc` and `diagonal` edge types draw it with. The edge geoms draw
#' positive curvature on the left of travel, the mirror side of
#' `sample_curved_edge()`'s convention, so the trace negates the curvature
#' to follow the drawn side. Reports every node on a strictly intermediate
#' layer whose center comes closer to the arc than `node_radius + 8`,
#' mirroring the straight-line detection threshold in `find_overlaps()`.
#'
#' @param positions List with `$x` and `$y` (named numeric vectors).
#' @param spanning Data frame of directed edges spanning two or more layers,
#'   with `name` and `to`.
#' @param layer_assign Named integer vector (node -> 0-based layer).
#' @param node_radius Radius of each node circle.
#' @param arc_curvature Curvature the edge geoms draw the arcs with.
#' @return Data frame with columns: `edge_from`, `edge_to`, `node`, `dist`.
#' @noRd
find_spanning_arc_overlaps <- function(
  positions,
  spanning,
  layer_assign,
  node_radius,
  arc_curvature
) {
  clearance <- node_radius + 8
  all_nodes <- names(positions$x)

  overlaps <- data.frame(
    edge_from = character(0),
    edge_to = character(0),
    node = character(0),
    dist = numeric(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(spanning))) {
    u <- spanning$name[i]
    v <- spanning$to[i]
    lo <- min(layer_assign[[u]], layer_assign[[v]])
    hi <- max(layer_assign[[u]], layer_assign[[v]])
    pts <- sample_curved_edge(
      positions$x[[u]],
      positions$y[[u]],
      positions$x[[v]],
      positions$y[[v]],
      -arc_curvature
    )
    for (w in setdiff(all_nodes, c(u, v))) {
      w_layer <- layer_assign[[w]]
      if (w_layer <= lo || w_layer >= hi) {
        next
      }
      dist <- min(
        sqrt((positions$x[[w]] - pts$x)^2 + (positions$y[[w]] - pts$y)^2)
      )
      if (dist < clearance) {
        overlaps <- rbind(
          overlaps,
          data.frame(
            edge_from = u,
            edge_to = v,
            node = w,
            dist = dist,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  overlaps
}

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
#' @param edges_df Data frame with `name` and `to` columns; a `direction`
#'   column is honored when present, and bidirected rows take no part in the
#'   straight-line correction.
#' @param layer_assign Named integer vector (node -> 0-based layer).
#' @param node_radius Radius of each node circle.
#' @param min_spacing Minimum Y gap between same-layer nodes.
#' @param max_passes Maximum correction iterations.
#' @param check_bidirected If `TRUE`, each pass also traces the arcs the
#'   bidirected rows of `edges_df` are drawn with and applies the same
#'   displacement correction to nodes the arcs pass through.
#' @param arc_curvature Curvature the bidirected arcs are traced at. The
#'   default reads the `curvature` option the edge geoms draw with, so the
#'   correction clears the arcs as they will appear.
#' @param spanning_arcs If `TRUE`, directed edges spanning two or more layers
#'   are also traced as the arcs the `arc` and `diagonal` edge types draw
#'   them with, on the drawn side of travel, and an intermediate node the
#'   arc passes through is displaced outward on the side of the straight
#'   chord it already occupies.
#' @return Updated positions list.
#' @noRd
greedy_post_correction <- function(
  positions,
  edges_df,
  layer_assign,
  node_radius = 26,
  min_spacing = 72,
  max_passes = 50L,
  check_bidirected = FALSE,
  arc_curvature = ggdag_option("curvature"),
  spanning_arcs = FALSE
) {
  target_clearance <- node_radius + 12

  parts <- split_edge_types(edges_df)
  directed <- parts$directed
  bidirected <- if (isTRUE(check_bidirected)) {
    parts$bidirected
  } else {
    parts$bidirected[0, , drop = FALSE]
  }
  spanning <- if (isTRUE(spanning_arcs)) {
    spans <- abs(
      layer_assign[directed$to] - layer_assign[directed$name]
    ) >=
      2
    directed[spans, , drop = FALSE]
  } else {
    directed[0, , drop = FALSE]
  }

  # Enforce spacing first
  positions$y <- enforce_spacing(positions$y, layer_assign, min_spacing)

  for (pass in seq_len(max_passes)) {
    overlaps <- find_overlaps(positions, directed, layer_assign, node_radius)
    arc_overlaps <- find_arc_overlaps(
      positions,
      bidirected,
      node_radius,
      arc_curvature
    )
    spanning_overlaps <- find_spanning_arc_overlaps(
      positions,
      spanning,
      layer_assign,
      node_radius,
      arc_curvature
    )
    if (
      nrow(overlaps) == 0 &&
        nrow(arc_overlaps) == 0 &&
        nrow(spanning_overlaps) == 0
    ) {
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

    for (oi in seq_len(nrow(arc_overlaps))) {
      u <- arc_overlaps$edge_from[oi]
      v <- arc_overlaps$edge_to[oi]
      w <- arc_overlaps$node[oi]

      # Recompute against the current arc — earlier fixes may have moved nodes
      pts <- sample_curved_edge(
        positions$x[[u]],
        positions$y[[u]],
        positions$x[[v]],
        positions$y[[v]],
        arc_curvature
      )
      dists <- sqrt(
        (positions$x[[w]] - pts$x)^2 + (positions$y[[w]] - pts$y)^2
      )
      nearest <- which.min(dists)
      if (dists[[nearest]] >= target_clearance) {
        next
      }

      needed <- target_clearance - dists[[nearest]] + 2
      direction <- if (positions$y[[w]] >= pts$y[[nearest]]) 1 else -1
      t_safe <- max(0.1, min(0.9, (nearest - 1) / (nrow(pts) - 1)))

      # Same displacement split as the straight-line correction above
      positions$y[[w]] <- positions$y[[w]] + direction * needed * 0.55
      positions$y[[u]] <- positions$y[[u]] -
        direction * needed * 0.45 * (1 - t_safe)
      positions$y[[v]] <- positions$y[[v]] - direction * needed * 0.45 * t_safe
    }

    for (oi in seq_len(nrow(spanning_overlaps))) {
      u <- spanning_overlaps$edge_from[oi]
      v <- spanning_overlaps$edge_to[oi]
      w <- spanning_overlaps$node[oi]

      # Recompute against the current arc — earlier fixes may have moved
      # nodes. The trace negates the curvature to follow the side the edge
      # geoms draw; see find_spanning_arc_overlaps().
      pts <- sample_curved_edge(
        positions$x[[u]],
        positions$y[[u]],
        positions$x[[v]],
        positions$y[[v]],
        -arc_curvature
      )
      dists <- sqrt(
        (positions$x[[w]] - pts$x)^2 + (positions$y[[w]] - pts$y)^2
      )
      if (min(dists) >= target_clearance) {
        next
      }

      # Displace the node outward on the side of the straight chord it
      # already occupies, past the arc's extent near the node's x, so one
      # move clears the whole bow rather than chasing its nearest point.
      chord_y <- positions$y[[u]] +
        (positions$x[[w]] - positions$x[[u]]) /
          (positions$x[[v]] - positions$x[[u]]) *
          (positions$y[[v]] - positions$y[[u]])
      side <- if (positions$y[[w]] >= chord_y) 1 else -1
      near <- which(
        abs(pts$x - positions$x[[w]]) < 2 * node_radius + 24
      )
      # The window is twice the clearance, so any point close enough to have
      # triggered this correction falls inside it. The guard keeps the extent
      # below well defined should that relationship ever change.
      if (length(near) == 0) {
        next
      }
      arc_extent <- if (side > 0) max(pts$y[near]) else min(pts$y[near])
      positions$y[[w]] <- arc_extent + side * target_clearance
    }

    # Re-enforce spacing
    positions$y <- enforce_spacing(positions$y, layer_assign, min_spacing)
  }

  positions
}

# Fixed layer assignment -------------------------------------------------------

#' Validate a fixed layer assignment and convert it to internal layers
#'
#' Checks that `fixed_layers` is a named vector of whole non-negative tiers,
#' assigns each node at most once, and covers every node in the edge data.
#' The stages downstream index layers from 0 with no gaps, so the given tiers
#' are ranked densely; their relative order is all the stages use, and
#' `normalize_positions()` maps the ranks back to time points at the end.
#'
#' @param fixed_layers Named numeric vector (node name -> tier).
#' @param edges_df Data frame with columns `name` and `to`.
#' @param arg Argument name used in error messages, so a validation failure
#'   speaks in the caller's vocabulary: `time_ordered_coords()` passes
#'   `".vars"`, while direct engine calls keep the default.
#' @return A named integer vector of 0-based dense layer indices, in the
#'   order the nodes were given, so within-tier order survives into the
#'   layer construction.
#' @noRd
validate_fixed_layers <- function(
  fixed_layers,
  edges_df,
  arg = "fixed_layers"
) {
  nms <- names(fixed_layers)
  if (
    length(fixed_layers) == 0 ||
      is.null(nms) ||
      anyNA(nms) ||
      any(nms == "")
  ) {
    abort(
      "{.arg {arg}} must be a named vector (e.g. {.code c(x = 1, z = 2)}).",
      error_class = "ggdag_type_error"
    )
  }
  if (anyDuplicated(nms) > 0) {
    dupes <- unique(nms[duplicated(nms)])
    abort(
      c(
        "{.arg {arg}} must assign each node exactly one tier.",
        "x" = "{.val {dupes}} {?is/are} assigned more than once."
      ),
      error_class = "ggdag_type_error"
    )
  }
  if (
    !is.numeric(fixed_layers) ||
      anyNA(fixed_layers) ||
      !all(is.finite(fixed_layers))
  ) {
    abort(
      "{.arg {arg}} values must be finite numbers.",
      error_class = "ggdag_type_error"
    )
  }
  if (any(fixed_layers != round(fixed_layers))) {
    fractional <- nms[fixed_layers != round(fixed_layers)]
    abort(
      c(
        "{.arg {arg}} values must be whole numbers.",
        "x" = "{.val {fractional}} {?is/are} assigned a fractional tier."
      ),
      error_class = "ggdag_type_error"
    )
  }
  if (any(fixed_layers < 0)) {
    negative <- nms[fixed_layers < 0]
    abort(
      c(
        "{.arg {arg}} values must not be negative.",
        "x" = "{.val {negative}} {?is/are} assigned a negative tier."
      ),
      error_class = "ggdag_type_error"
    )
  }

  edge_nodes <- unique(c(edges_df$name, edges_df$to))
  edge_nodes <- edge_nodes[!is.na(edge_nodes)]
  uncovered <- setdiff(edge_nodes, nms)
  if (length(uncovered) > 0) {
    abort(
      c(
        "{.arg {arg}} must assign a tier to every node.",
        "x" = "No tier for: {.val {uncovered}}."
      ),
      error_class = "ggdag_type_error"
    )
  }

  tiers <- as.numeric(fixed_layers)
  stats::setNames(match(tiers, sort(unique(tiers))) - 1L, nms)
}

#' Warn about and drop edges that violate a fixed layer assignment
#'
#' A directed edge whose source tier is not strictly earlier than its target
#' tier contradicts the given layers, and the layers win: every such edge is
#' named in a single warning and removed from the data the ordering and
#' geometry stages optimize. The caller still draws it, since the returned
#' layout covers both of its endpoints.
#'
#' @param edges_df A data frame with columns `name` and `to`, optionally
#'   `direction`. Bidirected rows carry no time order and are never
#'   violations.
#' @param layer_assign Named integer vector (node -> 0-based layer index).
#' @return `edges_df` without the violating rows.
#' @noRd
drop_tier_violations <- function(edges_df, layer_assign) {
  if ("direction" %in% names(edges_df)) {
    is_bidirected <- !is.na(edges_df$to) &
      !is.na(edges_df$direction) &
      edges_df$direction == "<->"
  } else {
    is_bidirected <- rep(FALSE, nrow(edges_df))
  }

  violating <- rep(FALSE, nrow(edges_df))
  directed_idx <- which(!is.na(edges_df$to) & !is_bidirected)
  violating[directed_idx] <- layer_assign[edges_df$name[directed_idx]] >=
    layer_assign[edges_df$to[directed_idx]]

  n_bad <- sum(violating)
  if (n_bad > 0) {
    bad <- edges_df[violating, , drop = FALSE]
    bad_edges <- paste(bad$name, "->", bad$to)
    warn(
      c(
        "{cli::qty(n_bad)}{?An edge contradicts/Edges contradict} the time ordering of the given tiers.",
        "x" = "{cli::qty(n_bad)}Edge{?s} {.val {bad_edges}} do{?es/} not point to a later tier.",
        "i" = "The tiers are kept as given; {cli::qty(n_bad)}{?this edge is/these edges are} drawn but excluded from layout optimization."
      ),
      warning_class = "ggdag_tier_violation_warning"
    )
  }

  edges_df[!violating, , drop = FALSE]
}

#' Keep a user's spread grid unless optimizing strictly improves on it
#'
#' Under `fixed_layers`, the spread grid is the user's own arrangement:
#' tiers on the time axis, tier-mates spread evenly in the order they were
#' listed. The optimized layout replaces that grid only when it strictly
#' improves straight-line edge crossings, or ties crossings and strictly
#' improves node-edge overlaps; otherwise the grid comes back exactly as
#' `time_ordered_coords(optimize = FALSE)` would return it. Both layouts
#' are scored on the internal pixel scale, one layer gap per axis unit.
#'
#' @param coords Optimized layout tibble with `name`, `x`, `y` in data
#'   units.
#' @param layer_assign Named integer vector (node -> 0-based layer), in the
#'   order the user listed the nodes.
#' @param edges_df Data frame with `name` and `to` columns.
#' @param node_radius Node circle radius in internal pixels.
#' @param time_points Optional numeric vector of axis positions, one per
#'   tier in ascending tier order.
#' @param direction `"x"` or `"y"`.
#' @param layer_gap Internal pixel distance one axis unit maps to.
#' @param curvature Per-row trace curvature for `edges_df`, from
#'   `edge_trace_curvature()`.
#' @return Either `coords` or the spread grid tibble.
#' @noRd
prefer_spread_grid <- function(
  coords,
  layer_assign,
  edges_df,
  node_radius,
  time_points,
  direction,
  layer_gap,
  curvature
) {
  layers <- sort(unique(layer_assign))
  axis_points <- unname(layer_axis_points(layer_assign, time_points))
  tiers <- lapply(layers, function(l) names(layer_assign)[layer_assign == l])
  grid <- purrr::map2_dfr(
    axis_points,
    tiers,
    spread_coords,
    direction = direction
  )

  score <- function(layout) {
    scaled <- data.frame(
      name = layout$name,
      x = layout$x * layer_gap,
      y = layout$y * layer_gap,
      stringsAsFactors = FALSE
    )
    c(
      count_edge_crossings(scaled, edges_df),
      count_node_edge_overlaps(
        scaled,
        edges_df,
        node_radius,
        curvature = curvature
      )
    )
  }

  optimized <- score(coords)
  spread <- score(grid)
  if (
    optimized[[1]] < spread[[1]] ||
      (optimized[[1]] == spread[[1]] && optimized[[2]] < spread[[2]])
  ) {
    coords
  } else {
    grid
  }
}

# Orchestrator -----------------------------------------------------------------

#' Compute overlap-free time-ordered layout
#'
#' Runs the full 4-stage algorithm: longest-path layer assignment, exact
#' within-layer crossing minimization, force-directed Y optimization, and
#' greedy post-correction. Stage 3 runs from two initializations, even
#' spacing and median sweeps, and the geometrically better result is kept.
#' Returns normalized coordinates suitable for ggdag.
#'
#' @param edges_df Data frame with `name` and `to` columns (from `edges2df()`).
#' @param direction Either `"x"` (default, time on x-axis) or `"y"`.
#' @param fixed_layers Optional named numeric vector assigning every node in
#'   `edges_df` to a tier (node name -> tier). When supplied, layer inference
#'   is skipped entirely and the assignment is honored exactly as given;
#'   `fixed_time`, `sort_direction`, `exposure`, `outcome`, and
#'   `adjust_exposure_outcome` take no part. Tiers must be whole non-negative
#'   numbers, and a directed edge that does not point to a strictly later
#'   tier is named in one warning and excluded from the optimization, though
#'   its endpoints still receive coordinates.
#' @param time_points Optional numeric vector mapping tiers to axis
#'   positions, one value per distinct tier in ascending tier order. Only
#'   used with `fixed_layers`; the default positions tiers at 1, 2, 3, and
#'   so on.
#' @param fixed_layers_arg Argument name used in `fixed_layers` error
#'   messages; `time_ordered_coords()` passes `".vars"` so the errors its
#'   layout closures raise name the argument the user actually supplied.
#' @param node_scale Multiplier for the drawn node size, `node_size / 16`
#'   for the default node size of 16. Scales `node_radius` and, through it,
#'   the spacing and clearance defaults below, so larger nodes get room in
#'   proportion. Explicit values for those arguments override the scaled
#'   defaults.
#' @param node_radius Node circle radius for overlap detection.
#' @param arc_curvature Curvature the drawn bidirected arcs are modeled at,
#'   read from the `curvature` option once when the layout is computed so the
#'   correction pass and the never-worse guard clear the arcs as the edge
#'   geoms will draw them.
#' @param edge_type The edge type the DAG will be drawn with, read from the
#'   `edge_type` option once when the layout is computed. Under `"arc"` and
#'   `"diagonal"`, directed edges spanning two or more layers are drawn
#'   curved, so the correction pass and the never-worse guard trace them as
#'   the arcs they are drawn with, on the drawn side of travel; every other
#'   edge type draws them straight and the chord checks apply unchanged.
#' @param layer_gap Horizontal distance between layers (internal).
#' @param node_gap Initial vertical spacing between same-layer nodes.
#' @param min_spacing Minimum Y gap enforced between same-layer nodes.
#' @param iterations Force simulation iterations.
#' @param sweeps Barycenter sweep iterations for the ordering stage.
#' @param max_correction_passes Maximum greedy correction iterations.
#' @return A tibble with columns `name`, `x`, `y`.
#' @noRd
compute_time_ordered_layout <- function(
  edges_df,
  direction = "x",
  sort_direction = "right",
  fixed_time = NULL,
  fixed_layers = NULL,
  time_points = NULL,
  fixed_layers_arg = "fixed_layers",
  exposure = character(0),
  outcome = character(0),
  adjust_exposure_outcome = TRUE,
  force_y = TRUE,
  node_scale = 1,
  node_radius = 26 * node_scale,
  arc_curvature = ggdag_option("curvature"),
  edge_type = ggdag_option("edge_type", "link_arc"),
  layer_gap = 180,
  node_gap = max(85, min_spacing + 13),
  min_spacing = 2 * node_radius + 20,
  iterations = 350L,
  sweeps = 8L,
  max_correction_passes = 50L,
  ...
) {
  edges_df$name <- as.character(edges_df$name)
  edges_df$to <- as.character(edges_df$to)

  # The arc and diagonal edge types draw every directed edge curved, so a
  # spanning edge bows away from its straight chord and the geometry stages
  # must clear the arc as drawn rather than the chord.
  spanning_arcs <- any(edge_type %in% c("arc", "diagonal"))

  # Filter out bidirected edges — only directed edges drive stages 2-4
  directed <- split_edge_types(edges_df)$directed

  if (!is.null(fixed_layers)) {
    # The given tiers replace Stage 1 wholesale: no layer inference runs, no
    # exposure/outcome adjustment applies, and an edge that contradicts the
    # tiers is warned about once and set aside for the stages below.
    layer_assign <- validate_fixed_layers(
      fixed_layers,
      edges_df,
      arg = fixed_layers_arg
    )
    if (
      !is.null(time_points) &&
        length(time_points) != length(unique(layer_assign))
    ) {
      abort(
        c(
          "{.arg time_points} must have one value per tier.",
          "x" = "{.arg time_points} has {length(time_points)} value{?s}, but
                 {.arg {fixed_layers_arg}} holds
                 {length(unique(layer_assign))} tier{?s}."
        ),
        error_class = "ggdag_type_error"
      )
    }
    edges_df <- drop_tier_violations(edges_df, layer_assign)
    directed <- split_edge_types(edges_df)$directed
    fixed_time <- NULL
  } else {
    time_points <- NULL

    # Validate fixed_time
    if (!is.null(fixed_time) && length(fixed_time) > 0) {
      if (
        is.null(names(fixed_time)) ||
          anyNA(names(fixed_time)) ||
          any(names(fixed_time) == "")
      ) {
        abort(
          "{.arg fixed_time} must be a named vector (e.g. {.code c(x = 2, z = 3)})."
        )
      }
      if (
        !is.numeric(fixed_time) ||
          anyNA(fixed_time) ||
          !all(is.finite(fixed_time))
      ) {
        abort(
          "{.arg fixed_time} values must be finite numbers."
        )
      }
      if (any(fixed_time < 1)) {
        abort(
          "{.arg fixed_time} values must be >= 1 (time points are 1-based)."
        )
      }
      # A time point is a layer index, so a fractional pin has no meaning here.
      # Truncating one silently would contradict the promise that a pinned time
      # comes back unchanged, and rounding would guess at the user's intent.
      if (any(fixed_time != round(fixed_time))) {
        fractional <- names(fixed_time)[fixed_time != round(fixed_time)]
        abort(
          c(
            "{.arg fixed_time} values must be whole numbers.",
            "x" = "{.val {fractional}} {?is/are} pinned to a fractional time."
          )
        )
      }
    }

    # Convert user-facing 1-based fixed_time to internal 0-based layers
    internal_fixed_time <- fixed_time
    if (!is.null(internal_fixed_time) && length(internal_fixed_time) > 0) {
      internal_fixed_time <- stats::setNames(
        as.integer(internal_fixed_time) - 1L,
        names(internal_fixed_time)
      )
    }

    # Stage 1: Layer assignment (handles bidirected internally). Resolve the
    # bidirected groups here so the exposure/outcome adjustment below sees the
    # same groups the layering used, including any the layering had to drop.
    groups <- resolve_bidirected_groups(edges_df)
    layer_assign <- longest_path_layers(
      edges_df,
      sort_direction = sort_direction,
      fixed_time = internal_fixed_time,
      groups = groups
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
            # Shift the outcome, its descendants, and anything the same-layer
            # bidirected constraint ties to them, all by +1
            shift_nodes <- shift_closure(out_node, directed, groups)
            shift_nodes <- intersect(shift_nodes, names(layer_assign))

            # The exposure travels with the outcome when a bidirected edge ties
            # the two, or when the outcome is an ancestor of the exposure. The
            # shift cannot separate them then, and applying it anyway moves the
            # pair to a later time point for no reason, which shows once
            # `fixed_time` fixes the layers to absolute time points.
            if (exp_node %in% shift_nodes) {
              cli::cli_inform(
                c(
                  "Outcome {.val {out_node}} shares a layer with exposure
{.val {exp_node}}, but was not shifted because the two move together.",
                  "i" = "Separate them with {.arg fixed_time}, or set
{.code adjust_exposure_outcome = FALSE}."
                ),
                class = "ggdag_message"
              )
              next
            }

            movable <- setdiff(shift_nodes, pinned)
            blocked <- intersect(shift_nodes, pinned)

            candidate <- layer_assign
            candidate[movable] <- candidate[movable] + 1L
            # Only the edges the shift touches can change: everything else keeps
            # the layers it already had
            touched <- directed$name %in%
              shift_nodes |
              directed$to %in% shift_nodes
            advances <- all(
              candidate[directed$name[touched]] <
                candidate[directed$to[touched]]
            )
            if (!advances) {
              # Only a pinned member of the shifted set can hold a node back,
              # and moving the rest would draw a cause and its effect at the
              # same time point
              cli::cli_inform(
                c(
                  "Outcome {.val {out_node}} shares a layer with exposure
{.val {exp_node}}, but was not shifted because {.val {blocked}} {?has/have} a
{.arg fixed_time} pin.",
                  "i" = "Remove the pin or adjust it manually to separate them."
                ),
                class = "ggdag_message"
              )
              next
            }
            layer_assign <- candidate
          }
        }
      }
    }
  }

  # Defense in depth: every stage below enumerates layers from 0 upward, so a
  # negative layer would drop its node from the output entirely. Validation
  # rules them out, and this shift is a no-op whenever the minimum is already
  # 0, which is every case that reaches here.
  if (length(layer_assign) > 0 && min(layer_assign) < 0L) {
    layer_assign <- layer_assign - min(layer_assign)
  }

  # Stage 2: Build layer_nodes and barycenter sort
  if (length(layer_assign) == 0L) {
    return(tibble::tibble(name = character(), x = numeric(), y = numeric()))
  }
  max_layer <- max(layer_assign)
  layer_nodes <- lapply(seq(0, max_layer), function(l) {
    names(layer_assign[layer_assign == l])
  })

  if (nrow(directed) > 0) {
    # Same-layer bidirected pairs feed the adjacency penalty, nudging the
    # members of a pair next to each other so their arc stays short
    ordering <- order_layers(
      layer_nodes,
      directed,
      layer_assign,
      sweeps = sweeps,
      bidirected_pairs = split_edge_types(edges_df)$bidirected
    )
    layer_nodes <- ordering$layer_nodes

    if (isTRUE(force_y)) {
      # Stages 3 and 4: force-directed Y optimization, then greedy
      # post-correction
      run_geometry <- function(y_init) {
        positions <- force_directed_y(
          layer_nodes,
          layer_assign,
          directed,
          node_radius = node_radius,
          layer_gap = layer_gap,
          node_gap = node_gap,
          min_spacing = min_spacing,
          clearance = node_radius * 2.5 + 12,
          iterations = iterations,
          y_init = y_init
        )
        greedy_post_correction(
          positions,
          edges_df,
          layer_assign,
          node_radius = node_radius,
          min_spacing = min_spacing,
          max_passes = max_correction_passes,
          check_bidirected = TRUE,
          arc_curvature = arc_curvature,
          spanning_arcs = spanning_arcs
        )
      }

      # Never-worse guard: the median initialization untangles most DAGs
      # better than even spacing, but not all of them, so both run and the
      # geometrically better result is kept. Ties keep the even-spacing
      # result.
      even_result <- run_geometry(NULL)
      median_result <- run_geometry(
        median_y_init(ordering$augmented, node_gap = node_gap)
      )
      positions <- better_positions(
        even_result,
        median_result,
        edges_df,
        node_radius,
        arc_curvature = arc_curvature,
        layer_assign = layer_assign,
        spanning_arcs = spanning_arcs
      )

      # Symmetry override: a mirror-symmetric DAG reads best drawn as a
      # symmetric figure, so the even-spacing candidate is kept over a
      # median candidate that untangles one or two more crossings, provided
      # the even candidate is itself mirror-symmetric and free of
      # straight-line node-edge overlaps. A median win with no crossing
      # advantage, on overlaps or stress alone, stands: overriding it would
      # trade real clearance for symmetry the crossings never paid for.
      if (!identical(positions, even_result)) {
        even_coords <- data.frame(
          name = names(even_result$x),
          x = unname(even_result$x),
          y = unname(even_result$y),
          stringsAsFactors = FALSE
        )
        median_coords <- data.frame(
          name = names(median_result$x),
          x = unname(median_result$x),
          y = unname(median_result$y),
          stringsAsFactors = FALSE
        )
        crossing_gap <- count_edge_crossings(even_coords, edges_df) -
          count_edge_crossings(median_coords, edges_df)
        if (
          (crossing_gap == 1L || crossing_gap == 2L) &&
            count_node_edge_overlaps(even_coords, edges_df, node_radius) ==
              0L &&
            mirror_symmetric_positions(even_result)
        ) {
          positions <- even_result
        }
      }
    } else {
      # Skip force simulation — evenly space nodes within each layer
      all_nodes <- unlist(layer_nodes)
      x_pos <- stats::setNames(numeric(length(all_nodes)), all_nodes)
      y_pos <- stats::setNames(numeric(length(all_nodes)), all_nodes)
      max_layer_size <- max(lengths(layer_nodes))
      for (i in seq_along(layer_nodes)) {
        nodes <- layer_nodes[[i]]
        n <- length(nodes)
        offset <- (max_layer_size - n) * node_gap / 2
        for (j in seq_along(nodes)) {
          x_pos[[nodes[j]]] <- (i - 1L) * layer_gap
          y_pos[[nodes[j]]] <- offset + (j - 1) * node_gap
        }
      }
      positions <- list(x = x_pos, y = y_pos)
    }
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
  # y → centered, divided by the same uniform scale as x
  coords <- normalize_positions(
    positions,
    layer_assign,
    direction,
    fixed_time = fixed_time,
    time_points = time_points,
    layer_gap = layer_gap
  )

  if (!is.null(fixed_layers)) {
    coords <- prefer_spread_grid(
      coords,
      layer_assign,
      edges_df,
      node_radius,
      time_points = time_points,
      direction = direction,
      layer_gap = layer_gap,
      curvature = edge_trace_curvature(
        edges_df,
        arc_curvature,
        layer_assign = layer_assign,
        spanning_arcs = spanning_arcs
      )
    )
  }

  coords
}

#' Per-edge curvature for tracing edges as they are drawn
#'
#' Builds the curvature vector the scoring helpers trace each edge with:
#' bidirected rows carry the arc curvature the edge layers draw them with,
#' and, when `spanning_arcs` is `TRUE`, directed rows spanning two or more
#' layers carry the negated curvature, because the edge geoms draw positive
#' curvature on the left of travel while `sample_curved_edge()` offsets to
#' the right. Every other row is straight.
#'
#' @param edges_df Data frame with `name` and `to` columns; a `direction`
#'   column marks the bidirected rows.
#' @param arc_curvature Curvature the edge geoms draw arcs with.
#' @param layer_assign Named integer vector (node -> 0-based layer);
#'   required to find spanning rows when `spanning_arcs` is `TRUE`.
#' @param spanning_arcs If `TRUE`, trace spanning directed rows as arcs.
#' @return Numeric vector with one curvature per row of `edges_df`.
#' @noRd
edge_trace_curvature <- function(
  edges_df,
  arc_curvature,
  layer_assign = NULL,
  spanning_arcs = FALSE
) {
  curvature <- rep(0, nrow(edges_df))

  if ("direction" %in% names(edges_df)) {
    bidirected <- !is.na(edges_df$to) &
      !is.na(edges_df$direction) &
      edges_df$direction == "<->"
  } else {
    bidirected <- rep(FALSE, nrow(edges_df))
  }
  curvature[bidirected] <- arc_curvature

  if (isTRUE(spanning_arcs) && !is.null(layer_assign)) {
    directed <- !is.na(edges_df$to) & !bidirected
    spans <- directed
    spans[directed] <- abs(
      layer_assign[edges_df$to[directed]] -
        layer_assign[edges_df$name[directed]]
    ) >=
      2
    curvature[spans] <- -arc_curvature
  }

  curvature
}

#' Test whether a position set is mirror-symmetric about its mid-x
#'
#' `TRUE` when reflecting the layout about the vertical line through its
#' horizontal center maps the node set onto itself: every node has a
#' partner, possibly itself, at the mirrored x with the same y, within
#' `tol` internal pixels.
#'
#' @param positions List with `$x` and `$y` (named numeric vectors).
#' @param tol Matching tolerance in internal pixels.
#' @return `TRUE` or `FALSE`.
#' @noRd
mirror_symmetric_positions <- function(positions, tol = 1) {
  x <- positions$x
  y <- positions$y[names(x)]
  mirrored_x <- max(x) + min(x) - x
  used <- rep(FALSE, length(x))
  for (i in seq_along(x)) {
    j <- which(!used & abs(x - mirrored_x[i]) < tol & abs(y - y[i]) < tol)
    if (length(j) == 0) {
      return(FALSE)
    }
    used[j[1]] <- TRUE
  }
  TRUE
}

#' Choose the better of two candidate position sets
#'
#' Compares two stage 3/4 results computed on the same internal coordinate
#' scale: fewer straight-line edge crossings wins, then fewer node-edge
#' overlaps, then lower stress. Ties keep the first candidate. The overlap
#' count traces bidirected rows as the arcs they are drawn with, so a
#' candidate whose arcs clear the nodes beats one whose arcs pass through
#' them even when the straight-line criteria tie.
#'
#' @param a,b Position lists with `$x` and `$y` (named numeric vectors).
#' @param edges_df Data frame with `name` and `to` columns; a `direction`
#'   column marks the bidirected rows traced as arcs.
#' @param node_radius Radius of each node circle.
#' @param arc_curvature Curvature the bidirected arcs are traced at. The
#'   default reads the `curvature` option the edge geoms draw with.
#' @param layer_assign Named integer vector (node -> 0-based layer), used
#'   with `spanning_arcs` to find the directed edges that span layers.
#' @param spanning_arcs If `TRUE`, directed edges spanning two or more
#'   layers are scored as the arcs the `arc` and `diagonal` edge types draw
#'   them with rather than as straight chords.
#' @return Either `a` or `b`.
#' @noRd
better_positions <- function(
  a,
  b,
  edges_df,
  node_radius,
  arc_curvature = ggdag_option("curvature"),
  layer_assign = NULL,
  spanning_arcs = FALSE
) {
  curvature <- edge_trace_curvature(
    edges_df,
    arc_curvature,
    layer_assign = layer_assign,
    spanning_arcs = spanning_arcs
  )

  score <- function(positions) {
    coords <- data.frame(
      name = names(positions$x),
      x = unname(positions$x),
      y = unname(positions$y),
      stringsAsFactors = FALSE
    )
    c(
      count_edge_crossings(coords, edges_df),
      count_node_edge_overlaps(
        coords,
        edges_df,
        node_radius,
        curvature = curvature
      ),
      layout_stress(coords, edges_df)
    )
  }

  score_a <- score(a)
  score_b <- score(b)
  for (i in seq_along(score_a)) {
    if (score_b[[i]] < score_a[[i]]) {
      return(b)
    }
    if (score_a[[i]] < score_b[[i]]) {
      return(a)
    }
  }
  a
}

#' Axis positions for the layers of a layout
#'
#' One position on the time axis per occupied layer, named by layer index.
#' `time_points` gives them explicitly; otherwise `fixed_time` keeps the
#' user's 1-based time points (internal 0-based plus one) and the plain case
#' numbers the layers from one in ascending order.
#'
#' @param layer_assign Named integer vector (node -> 0-based layer).
#' @param time_points Optional numeric vector of axis positions, one per
#'   distinct layer in ascending layer order; takes precedence over
#'   `fixed_time`.
#' @param fixed_time Named vector of user pins, or `NULL`.
#' @return An integer or numeric vector of axis positions, named by layer
#'   index.
#' @noRd
layer_axis_points <- function(
  layer_assign,
  time_points = NULL,
  fixed_time = NULL
) {
  unique_layers <- sort(unique(layer_assign))
  positions <- if (!is.null(time_points) && length(time_points) > 0) {
    as.numeric(time_points)
  } else if (!is.null(fixed_time) && length(fixed_time) > 0) {
    unique_layers + 1L
  } else {
    seq_along(unique_layers)
  }

  stats::setNames(positions, as.character(unique_layers))
}

#' Normalize pixel-space positions to ggdag-friendly coordinates
#'
#' @param positions List with `$x` and `$y` (named numeric vectors).
#' @param layer_assign Named integer vector (node -> 0-based layer).
#' @param direction `"x"` or `"y"` — swap axes if `"y"`.
#' @param fixed_time Named vector of user pins; its presence switches the
#'   layer-to-x mapping to preserve the pinned 1-based time points.
#' @param time_points Optional numeric vector of axis positions, one per
#'   distinct layer in ascending layer order; used verbatim as the layer-to-x
#'   mapping and taking precedence over `fixed_time`.
#' @param layer_gap Horizontal pixel distance between layers. Both axes are
#'   divided by this one scale, so internal geometry (spacing, clearances,
#'   the bow of arc edges) survives into data space undistorted.
#' @return A tibble with `name`, `x`, `y`.
#' @noRd
normalize_positions <- function(
  positions,
  layer_assign,
  direction = "x",
  fixed_time = NULL,
  time_points = NULL,
  layer_gap = 180
) {
  node_names <- names(positions$x)

  # x: one axis position per layer, so pinned nodes keep the time point they
  # were given and the plain case numbers the layers from one
  layer_map <- layer_axis_points(layer_assign, time_points, fixed_time)
  norm_x <- vapply(
    node_names,
    function(n) {
      layer_map[[as.character(layer_assign[[n]])]]
    },
    numeric(1)
  )

  # y: center the whole graph at y = 0, then divide by the layer gap — the
  # same uniform scale that maps one internal layer to one x unit. A single
  # isotropic scale keeps the solved geometry intact in data space, so a
  # clearance or an arc bow measured internally means the same thing after
  # normalization.
  norm_y <- positions$y - mean(positions$y)
  norm_y <- norm_y / layer_gap

  if (direction == "y") {
    tibble::tibble(name = node_names, x = unname(norm_y), y = unname(norm_x))
  } else {
    tibble::tibble(name = node_names, x = unname(norm_x), y = unname(norm_y))
  }
}
