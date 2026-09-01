# Custom Legend Key Functions for ggdag
# Focused implementation of essential glyph functions for better legend appearance

#' @importFrom grid pointsGrob segmentsGrob gpar arrow unit grobTree
#' @importFrom ggplot2 .pt .stroke alpha

# Helper function to calculate appropriate legend box size
# Mimics ggplot2's default calculation but with a scale factor
calculate_key_box_size <- function(size, linewidth = 0, scale_factor = 1) {
  # Handle NA values by replacing with defaults
  if (is.na(size)) {
    size <- 16
  }
  if (is.na(linewidth)) {
    linewidth <- 0
  }
  if (is.na(scale_factor)) {
    scale_factor <- 1
  }

  # ggplot2's default: (size + linewidth) / 10 converts mm to cm
  # We apply our scale factor to get proportional box size
  ((size * scale_factor) + linewidth) / 10
}

# The engine a legend key should draw with. A plot built with an explicit
# `edge_engine` carries it here in the key's parameters, since the option alone
# would describe a different plot.
key_edge_engine <- function(params) {
  params$edge_engine %||% ggdag_option("edge_engine", "ggraph")
}

key_draws_arrows <- function(params) {
  identical(key_edge_engine(params), "ggarrow") &&
    rlang::is_installed("ggarrow")
}

# Wrap a key glyph so that it draws for a known engine rather than for whichever
# one the option names at drawing time.
dag_key_glyph <- function(key_fn, edge_engine) {
  force(key_fn)
  force(edge_engine)
  function(data, params, size) {
    params$edge_engine <- edge_engine
    key_fn(data, params, size)
  }
}

# Build an arrow grob for legend keys, engine-aware.
# When edge_engine is "ggarrow" and ggarrow is installed, uses ggarrow::grob_arrow()
# to match the actual plot edge rendering. Otherwise uses grid::segmentsGrob().
build_key_arrow_grob <- function(
  x0,
  y0,
  x1,
  y1,
  colour,
  alpha = 1,
  lwd = 0.5 * .pt,
  arrow_length = unit(2, "mm"),
  params = list()
) {
  col <- ggplot2::alpha(colour, alpha)

  if (key_draws_arrows(params)) {
    arrow_head <- ggdag_option("arrow_head", NULL) %||%
      ggarrow::arrow_head_wings()
    ggarrow::grob_arrow(
      x = unit(c(x0, x1), "npc"),
      y = unit(c(y0, y1), "npc"),
      arrow_head = arrow_head,
      arrow_fins = NULL,
      length_head = arrow_length,
      shaft_width = unit(lwd / .pt, "mm"),
      force_arrow = TRUE,
      gp = gpar(
        col = NA,
        fill = col,
        lwd = 0.5 * .pt,
        linejoin = "round"
      )
    )
  } else {
    segmentsGrob(
      x0,
      y0,
      x1,
      y1,
      gp = gpar(
        fill = col,
        col = col,
        lwd = lwd
      ),
      arrow = arrow(length = arrow_length, type = "closed")
    )
  }
}

#' DAG point legend key (25% size)
#'
#' A custom legend key function that draws points at 25% of their normal size
#' with proportionally sized legend boxes. This creates much more compact
#' legends while maintaining visual clarity.
#'
#' @param data A data frame containing aesthetic information for the legend key
#' @param params Additional parameters (not currently used)
#' @param size Legend key size (not currently used)
#'
#' @return A grob object for the legend key
#' @export
draw_key_dag_point <- function(data, params, size) {
  scale_factor <- 0.25
  point_size <- (data$size %||% 16) * scale_factor

  grob <- pointsGrob(
    0.5,
    0.5,
    pch = data$shape %||% 19,
    gp = gpar(
      col = alpha(data$colour %||% "black", data$alpha %||% 1),
      fill = alpha(data$fill %||% data$colour %||% "black", data$alpha %||% 1),
      fontsize = point_size * .pt,
      lwd = (data$stroke %||% 0.5) * .stroke / 3
    )
  )

  box_size <- calculate_key_box_size(
    data$size %||% 16,
    data$linewidth %||% 0,
    scale_factor
  )

  attr(grob, "width") <- box_size
  attr(grob, "height") <- box_size

  grob
}

#' Combined DAG legend key (horizontal node-edge-node)
#'
#' A custom legend key function that displays a complete DAG representation
#' showing two nodes connected by an arrow. This provides a unified legend
#' entry for plots that show both nodes and edges.
#'
#' @param data A data frame containing aesthetic information for the legend key
#' @param params Additional parameters (not currently used)
#' @param size Legend key size (not currently used)
#'
#' @return A grob object for the legend key
#' @export
draw_key_dag_combined <- function(data, params, size) {
  grob <- grobTree(
    # First point
    pointsGrob(
      0.2,
      0.5,
      pch = data$shape %||% 19,
      gp = gpar(
        col = alpha(data$colour %||% "black", data$alpha %||% 1),
        fill = alpha(
          data$fill %||% data$colour %||% "black",
          data$alpha %||% 1
        ),
        fontsize = 3 * .pt
      )
    ),
    # Arrow -- engine-aware rendering
    if (key_draws_arrows(params)) {
      build_key_arrow_grob(
        0.35,
        0.5,
        0.65,
        0.5,
        colour = data$colour %||% "black",
        alpha = data$alpha %||% 1,
        params = params
      )
    } else {
      segmentsGrob(
        0.35,
        0.5,
        0.65,
        0.5,
        gp = gpar(
          fill = alpha(data$colour %||% "black", data$alpha %||% 1),
          col = alpha(data$colour %||% "black", data$alpha %||% 1),
          lwd = 0.5 * .pt
        ),
        arrow = arrow(length = unit(2, "mm"), type = "closed")
      )
    },
    # Second point
    pointsGrob(
      0.8,
      0.5,
      pch = data$shape %||% 19,
      gp = gpar(
        col = alpha(data$colour %||% "black", data$alpha %||% 1),
        fill = alpha(
          data$fill %||% data$colour %||% "black",
          data$alpha %||% 1
        ),
        fontsize = 3 * .pt
      )
    )
  )

  # Wider box for horizontal arrangement, but still proportional
  box_height <- calculate_key_box_size(data$size %||% 16, 0, 0.4)
  box_width <- box_height * 2 # Twice as wide for horizontal layout

  attr(grob, "width") <- box_width
  attr(grob, "height") <- box_height

  grob
}

#' Collider pattern legend key (many-to-one)
#'
#' A custom legend key function that displays a collider pattern with two
#' nodes pointing to one central node. This is particularly useful for
#' visualizing collider relationships in DAGs.
#'
#' @param data A data frame containing aesthetic information for the legend key
#' @param params Additional parameters (not currently used)
#' @param size Legend key size (not currently used)
#'
#' @return A grob object for the legend key
#' @export
draw_key_dag_collider <- function(data, params, size) {
  grob <- grobTree(
    # Upper input point
    pointsGrob(
      0.2,
      0.75,
      pch = data$shape %||% 19,
      gp = gpar(
        col = alpha(data$colour %||% "black", data$alpha %||% 1),
        fill = alpha(
          data$fill %||% data$colour %||% "black",
          data$alpha %||% 1
        ),
        fontsize = 3 * .pt
      )
    ),
    # Lower input point
    pointsGrob(
      0.2,
      0.25,
      pch = data$shape %||% 19,
      gp = gpar(
        col = alpha(data$colour %||% "black", data$alpha %||% 1),
        fill = alpha(
          data$fill %||% data$colour %||% "black",
          data$alpha %||% 1
        ),
        fontsize = 3 * .pt
      )
    ),
    # Output point
    pointsGrob(
      0.8,
      0.5,
      pch = data$shape %||% 19,
      gp = gpar(
        col = alpha(data$colour %||% "black", data$alpha %||% 1),
        fill = alpha(
          data$fill %||% data$colour %||% "black",
          data$alpha %||% 1
        ),
        fontsize = 3 * .pt
      )
    ),
    # Upper arrow -- engine-aware rendering
    if (key_draws_arrows(params)) {
      build_key_arrow_grob(
        0.35,
        0.7,
        0.65,
        0.55,
        colour = data$colour %||% "black",
        alpha = data$alpha %||% 1,
        lwd = 0.4 * .pt,
        arrow_length = unit(1.5, "mm"),
        params = params
      )
    } else {
      segmentsGrob(
        0.35,
        0.7,
        0.65,
        0.55,
        gp = gpar(
          fill = alpha(data$colour %||% "black", data$alpha %||% 1),
          col = alpha(data$colour %||% "black", data$alpha %||% 1),
          lwd = 0.4 * .pt
        ),
        arrow = arrow(length = unit(1.5, "mm"), type = "closed")
      )
    },
    # Lower arrow -- engine-aware rendering
    if (key_draws_arrows(params)) {
      build_key_arrow_grob(
        0.35,
        0.3,
        0.65,
        0.45,
        colour = data$colour %||% "black",
        alpha = data$alpha %||% 1,
        lwd = 0.4 * .pt,
        arrow_length = unit(1.5, "mm"),
        params = params
      )
    } else {
      segmentsGrob(
        0.35,
        0.3,
        0.65,
        0.45,
        gp = gpar(
          fill = alpha(data$colour %||% "black", data$alpha %||% 1),
          col = alpha(data$colour %||% "black", data$alpha %||% 1),
          lwd = 0.4 * .pt
        ),
        arrow = arrow(length = unit(1.5, "mm"), type = "closed")
      )
    }
  )

  # Square box for collider pattern
  box_size <- calculate_key_box_size(data$size %||% 16, 0, 0.5)

  attr(grob, "width") <- box_size
  attr(grob, "height") <- box_size

  grob
}

#' DAG edge legend key (arrow only)
#'
#' A custom legend key function that displays only an arrow (edge) without nodes.
#' This is appropriate for edge-specific legends where nodes are not relevant.
#'
#' @param data A data frame containing aesthetic information for the legend key
#' @param params Additional parameters (not currently used)
#' @param size Legend key size (not currently used)
#'
#' @return A grob object for the legend key
#' @export
draw_key_dag_edge <- function(data, params, size) {
  # always use a single arrow head
  arrow <- params[["arrow"]]
  if (!is.null(arrow)) {
    arrow$ends <- 2L
  }

  # Draw a horizontal line with an arrow
  edge_col <- data$edge_colour %||% data$colour %||% "black"
  edge_alpha <- data$edge_alpha %||% data$alpha %||% 1
  edge_lwd <- (data$edge_width %||% 0.6) * .stroke * 0.7

  if (key_draws_arrows(params)) {
    grob <- build_key_arrow_grob(
      0.2,
      0.5,
      0.8,
      0.5,
      colour = edge_col,
      alpha = edge_alpha,
      lwd = edge_lwd,
      params = params
    )
  } else {
    grob <- segmentsGrob(
      0.2,
      0.5,
      0.8,
      0.5,
      gp = gpar(
        col = alpha(edge_col, edge_alpha),
        fill = alpha(edge_col, edge_alpha),
        lwd = edge_lwd,
        lty = data$edge_linetype %||% data$linetype %||% 1
      ),
      arrow = arrow
    )
  }

  # Use standard box size for consistency
  # Handle both NULL and NA values for linewidth
  linewidth <- data$linewidth
  if (is.null(linewidth) || is.na(linewidth)) {
    linewidth <- 0.5
  }
  box_size <- calculate_key_box_size(16, linewidth, 0.4)

  attr(grob, "width") <- box_size
  attr(grob, "height") <- box_size

  grob
}

#' @importFrom stats setNames
# Helper function to handle missing circular column (issue #119)
handle_missing_circular_column <- function(data) {
  if (!"circular" %in% names(data)) {
    data$circular <- rep(FALSE, nrow(data))
  }
  data$circular[is.na(data$circular)] <- FALSE
  data
}

convert_group_to_integer <- function(data) {
  if (!is.null(data) && "group" %in% names(data) && is.character(data$group)) {
    unique_groups <- unique(data$group)
    group_mapping <- setNames(seq_along(unique_groups), unique_groups)
    data$group <- as.integer(group_mapping[data$group])
  }
  data
}

StatNodes <- ggplot2::ggproto(
  "StatNodes",
  ggplot2::Stat,
  compute_layer = function(data, scales, params) {
    if (all(c("xend", "yend") %in% names(data))) {
      data <- dplyr::select(data, -"xend", -"yend")
    }

    one_row_per_node(data)
  }
)

#' Keep one row per node per panel
#'
#' The tidy data holds a row per edge, so a node with several edges arrives
#' several times over and would be drawn once per row. The copies are drawn on
#' top of one another, and when an analysis column such as `path` is mapped to
#' an aesthetic they are not the same point: an unmarked copy drawn last hides
#' the marked one underneath it. The row carrying the most values is the one
#' that describes the node, so it is the one kept.
#'
#' A position alone does not identify a node: two nodes given the same
#' coordinates sit on top of one another, and collapsing them would draw one
#' node where the DAG has two, hiding the mistake rather than showing it. The
#' rows of one node share a label, so the label tells them apart wherever a
#' layer carries one.
#'
#' @param data A layer's data, with no edge columns.
#' @return `data`, with one row per node per panel.
#' @noRd
one_row_per_node <- function(data) {
  data <- unique(data)

  if (nrow(data) == 0 || any(c("x", "y") %nin% names(data))) {
    return(data)
  }

  panel <- if ("PANEL" %in% names(data)) data$PANEL else 1L
  node <- paste(data$x, data$y, panel, sep = "\r")
  if ("label" %in% names(data)) {
    node <- paste(node, data$label, sep = "\r")
  }

  # `group` follows the aesthetics rather than describing the node, and the
  # position columns are the key itself
  described <- setdiff(names(data), c("x", "y", "PANEL", "group"))
  described <- described[!vapply(data[described], is.list, logical(1))]
  n_missing <- rowSums(is.na(data[described]))

  # `order()` is stable, so among rows describing the node equally well the
  # first one still wins
  marked_first <- order(n_missing)
  keep <- marked_first[!duplicated(node[marked_first])]

  data[sort(keep), , drop = FALSE]
}

generate_disc_points <- function(node_radius, n_node_points) {
  # Dense filled disc: center + 4 concentric rings with staggered angles.
  # Each ring is offset by half a step so points cover diagonals, not just axes.
  n_rings <- 4
  disc_points <- data.frame(dx = 0, dy = 0) # center point
  for (ring in seq_len(n_rings)) {
    r <- node_radius * ring / n_rings
    # Scale points per ring with circumference; generous minimum
    n_ring <- max(6, round(n_node_points * ring / sum(seq_len(n_rings))))
    # Stagger each ring by half a step to avoid cross pattern
    offset <- if (ring %% 2 == 0) pi / n_ring else 0
    ring_angles <- seq(0, 2 * pi, length.out = n_ring + 1)[-1] + offset
    disc_points <- rbind(
      disc_points,
      data.frame(dx = r * cos(ring_angles), dy = r * sin(ring_angles))
    )
  }
  disc_points
}

# The radius ggrepel gives a node when it places a label's segment endpoint.
# ggrepel converts `point.size` to centimetres as `point.size * .pt / .stroke /
# 20`, while a pch-19 node of size `s` is drawn with a radius of `0.375 * s`
# millimetres, so this is the `point.size` whose segment radius matches the
# circle actually on the page.
node_point_size <- function(node_size) {
  node_size * 0.75 * .stroke / .pt
}

# Positions along a straight edge, endpoints excluded.
straight_edge_points <- function(edges, n_edge_points) {
  t_vals <- seq(0, 1, length.out = n_edge_points + 2)[
    -c(1, n_edge_points + 2)
  ]
  do.call(
    rbind,
    lapply(seq_len(nrow(edges)), function(i) {
      data.frame(
        x = edges$x[i] + t_vals * (edges$xend[i] - edges$x[i]),
        y = edges$y[i] + t_vals * (edges$yend[i] - edges$y[i]),
        PANEL = edges$PANEL[i],
        stringsAsFactors = FALSE
      )
    })
  )
}

# The stat that draws each bent edge geometry ggdag offers.
edge_geometry_stat <- function(type) {
  switch(
    type,
    arc = ggraph::StatEdgeArc,
    diagonal = ggraph::StatEdgeDiagonal,
    fan = ggraph::StatEdgeFan
  )
}

# Positions along the path the edge layer draws for these edges. The layer's own
# stat produces them, so the points sit on the curve the reader sees rather than
# on the chord between the two nodes.
drawn_edge_points <- function(geometry, panel, n_edge_points) {
  stat <- edge_geometry_stat(geometry$type[[1]])
  n_drawn <- geometry$n[[1]]
  params <- list(
    strength = geometry$strength[[1]],
    fold = geometry$fold[[1]],
    flipped = geometry$flipped[[1]]
  )

  # the bezier code behind the diagonal and fan geometries reads coordinates as
  # doubles, and a DAG laid out on a grid arrives with integer ones
  control_points <- stat$setup_data(
    data.frame(
      x = as.double(geometry$x),
      y = as.double(geometry$y),
      xend = as.double(geometry$xend),
      yend = as.double(geometry$yend),
      PANEL = panel,
      group = seq_len(nrow(geometry)),
      circular = geometry$circular,
      filter = TRUE,
      from = geometry$from,
      to = geometry$to,
      stringsAsFactors = FALSE
    ),
    params
  )

  if (nrow(control_points) == 0) {
    return(NULL)
  }

  # The path is traced at the resolution the layer draws it at and thinned from
  # there, so every obstacle sits on a corner of the polyline the reader sees
  # rather than between two of them.
  path <- stat$compute_panel(control_points, NULL, n = n_drawn)
  path <- path[path$index %in% thin_path_index(path$index, n_edge_points), ]

  data.frame(
    x = path$x,
    y = path$y,
    PANEL = path$PANEL,
    stringsAsFactors = FALSE
  )
}

# The positions along a drawn path, endpoints excluded, closest to `n` evenly
# spaced ones.
thin_path_index <- function(index, n) {
  available <- sort(unique(index))
  available <- available[available > 0 & available < 1]
  if (length(available) == 0) {
    return(numeric())
  }

  wanted <- seq(0, 1, length.out = n + 2)
  wanted <- wanted[-c(1, n + 2)]
  unique(available[vapply(
    wanted,
    function(target) which.min(abs(available - target)),
    integer(1)
  )])
}

edge_key <- function(x, y, xend, yend) {
  paste(x, y, xend, yend, sep = "\r")
}

node_key <- function(x, y, panel) {
  paste(x, y, panel, sep = "\r")
}

# Invisible points tracing each edge, used as obstacles in ggrepel's repulsion.
# The rows of `edge_geometry` are the edges the plot's bent edge layers draw,
# one row each; an edge no such layer claims is traced as a straight chord.
repel_edge_points <- function(
  edges,
  n_edge_points,
  edge_geometry = NULL,
  layout = NULL
) {
  if (n_edge_points <= 0 || nrow(edges) == 0) {
    return(NULL)
  }

  edge_geometry <- rescale_edge_geometry(edge_geometry, layout)
  drawn_keys <- if (is.null(edge_geometry)) {
    character()
  } else {
    edge_key(
      edge_geometry$x,
      edge_geometry$y,
      edge_geometry$xend,
      edge_geometry$yend
    )
  }

  points <- list()
  for (panel in unique(edges$PANEL)) {
    panel_edges <- edges[edges$PANEL == panel, , drop = FALSE]
    keys <- edge_key(
      panel_edges$x,
      panel_edges$y,
      panel_edges$xend,
      panel_edges$yend
    )

    is_straight <- !keys %in% drawn_keys
    if (any(is_straight)) {
      points[[length(points) + 1]] <- straight_edge_points(
        panel_edges[is_straight, , drop = FALSE],
        n_edge_points
      )
    }

    if (all(is_straight)) {
      next
    }

    # Edges drawn by one layer are traced together: a fan places each edge
    # according to how many others share its pair of nodes.
    drawn <- edge_geometry[drawn_keys %in% keys, , drop = FALSE]
    group <- paste(
      drawn$type,
      drawn$strength,
      drawn$n,
      drawn$fold,
      drawn$flipped,
      sep = "\r"
    )
    for (rows in split(seq_len(nrow(drawn)), group)) {
      points[[length(points) + 1]] <- drawn_edge_points(
        drawn[rows, , drop = FALSE],
        panel,
        n_edge_points
      )
    }
  }

  points <- points[!vapply(points, is.null, logical(1))]
  if (length(points) == 0) {
    return(NULL)
  }

  do.call(rbind, points)
}

# A transforming position scale, `scale_x_log10()` say, moves the data before
# any stat sees it, so edge endpoints read from the plot data have to make the
# same trip before they can be matched against the rows this stat is given.
rescale_edge_geometry <- function(edge_geometry, layout) {
  if (is.null(edge_geometry) || is.null(layout)) {
    return(edge_geometry)
  }

  x_scale <- layout$panel_scales_x[[1]]
  y_scale <- layout$panel_scales_y[[1]]
  edge_geometry$x <- transform_positions(x_scale, edge_geometry$x)
  edge_geometry$xend <- transform_positions(x_scale, edge_geometry$xend)
  edge_geometry$y <- transform_positions(y_scale, edge_geometry$y)
  edge_geometry$yend <- transform_positions(y_scale, edge_geometry$yend)
  edge_geometry
}

transform_positions <- function(scale, values) {
  if (is.null(scale) || isTRUE(scale$is_discrete())) {
    return(values)
  }

  transformed <- tryCatch(scale$transform(values), error = function(e) NULL)
  if (!is.numeric(transformed) || length(transformed) != length(values)) {
    return(values)
  }
  transformed
}

# Invisible points filling the disc each node covers.
repel_node_points <- function(nodes, node_size, n_node_points) {
  if (n_node_points <= 0 || nrow(nodes) == 0) {
    return(NULL)
  }

  # The disc lives in data units, so its radius is scaled from the spread of
  # the nodes. A single node, or nodes stacked at one position, leaves nothing
  # to scale from.
  avg_range <- mean(c(diff(range(nodes$x)), diff(range(nodes$y))))
  if (!is.finite(avg_range) || avg_range == 0) {
    return(NULL)
  }

  disc_points <- generate_disc_points(
    node_size * avg_range / 400,
    n_node_points
  )

  do.call(
    rbind,
    lapply(seq_len(nrow(nodes)), function(i) {
      data.frame(
        x = nodes$x[i] + disc_points$dx,
        y = nodes$y[i] + disc_points$dy,
        PANEL = nodes$PANEL[i],
        stringsAsFactors = FALSE
      )
    })
  )
}

StatNodesRepel <- ggplot2::ggproto(
  "StatNodesRepel",
  ggplot2::Stat,
  optional_aes = c("xend", "yend"),
  extra_params = c(
    "na.rm",
    "node_size",
    "n_edge_points",
    "n_node_points",
    "edge_geometry"
  ),
  compute_layer = function(data, params, layout) {
    node_size <- params$node_size %||% 16
    n_edge_points <- params$n_edge_points %||% 50
    n_node_points <- params$n_node_points %||% 12
    has_edges <- all(c("xend", "yend") %in% names(data))

    # Every node in the layer is drawn, whether or not it carries a label, so
    # the repulsion geometry is taken from all of them before the rows ggrepel
    # will draw are filtered down to the labelled ones.
    all_nodes <- unique(data[, c("x", "y", "PANEL")])

    # Generate fake points along edges before removing xend/yend
    fake_points <- NULL
    if (has_edges) {
      edges <- unique(data[
        !is.na(data$xend),
        c("x", "y", "xend", "yend", "PANEL")
      ])
      edge_points <- repel_edge_points(
        edges,
        n_edge_points,
        params$edge_geometry,
        layout
      )
      if (!is.null(edge_points)) {
        edge_points$label <- ""
        edge_points[["point.size"]] <- 0
        fake_points <- edge_points[, c(
          "x",
          "y",
          "label",
          "point.size",
          "PANEL"
        )]
      }
    }

    if (has_edges) {
      data <- unique(dplyr::select(data, -"xend", -"yend"))
      if ("alpha" %in% names(data)) {
        # rows of one node can differ in an edge-level aesthetic and so survive
        # `unique()`; each would otherwise repel a label of its own
        data <- data |>
          dplyr::filter(!is.na(alpha), !is.na(label)) |>
          one_row_per_node()
      } else {
        data <- data |>
          dplyr::filter(!is.na(label)) |>
          group_by(PANEL) |>
          dplyr::distinct(x, y, label, .keep_all = TRUE) |>
          ungroup()
      }
    } else {
      data <- unique(data)
    }

    if (!"point.size" %in% names(data)) {
      data[["point.size"]] <- node_point_size(node_size)
    }

    # A node whose label is missing is still drawn, so it keeps a row carrying
    # its point size; only the visible label is absent.
    unlabelled <- all_nodes[
      !node_key(all_nodes$x, all_nodes$y, all_nodes$PANEL) %in%
        node_key(data$x, data$y, data$PANEL),
      ,
      drop = FALSE
    ]
    if (nrow(unlabelled) > 0) {
      fake_points <- rbind(
        fake_points,
        data.frame(
          x = unlabelled$x,
          y = unlabelled$y,
          label = "",
          point.size = node_point_size(node_size),
          PANEL = unlabelled$PANEL,
          stringsAsFactors = FALSE
        )
      )
    }

    node_skeleton <- repel_node_points(all_nodes, node_size, n_node_points)
    if (!is.null(node_skeleton)) {
      node_skeleton$label <- ""
      node_skeleton[["point.size"]] <- 0
      fake_points <- rbind(
        fake_points,
        node_skeleton[, c("x", "y", "label", "point.size", "PANEL")]
      )
    }

    if (!is.null(fake_points)) {
      data <- dplyr::bind_rows(data, fake_points)
    }

    data
  }
)

StatDebugRepelPoints <- ggplot2::ggproto(
  "StatDebugRepelPoints",
  ggplot2::Stat,
  optional_aes = c("xend", "yend"),
  extra_params = c(
    "na.rm",
    "node_size",
    "n_edge_points",
    "n_node_points",
    "edge_geometry"
  ),
  compute_layer = function(data, params, layout) {
    node_size <- params$node_size %||% 16
    n_edge_points <- params$n_edge_points %||% 50
    n_node_points <- params$n_node_points %||% 12
    has_edges <- all(c("xend", "yend") %in% names(data))

    fake_points <- NULL

    if (has_edges) {
      edges <- unique(data[
        !is.na(data$xend),
        c("x", "y", "xend", "yend", "PANEL")
      ])
      fake_points <- repel_edge_points(
        edges,
        n_edge_points,
        params$edge_geometry,
        layout
      )
    }

    nodes <- unique(data[, c("x", "y", "PANEL")])
    fake_points <- rbind(
      fake_points,
      repel_node_points(nodes, node_size, n_node_points)
    )

    if (is.null(fake_points) || nrow(fake_points) == 0) {
      return(data.frame(x = numeric(), y = numeric(), PANEL = integer()))
    }

    fake_points
  }
)

make_debug_repel_layer <- function(
  node_size = NULL,
  n_edge_points = NULL,
  n_node_points = NULL,
  edge_geometry = NULL
) {
  # inherit.aes = FALSE avoids inheriting color/fill mappings that don't exist
  # in the debug stat's output. Map x, y, xend, yend explicitly so edge fake
  # points can be computed. xend/yend always exist in DAG data.
  ggplot2::layer(
    data = NULL,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      xend = .data$xend,
      yend = .data$yend
    ),
    stat = StatDebugRepelPoints,
    geom = ggplot2::GeomPoint,
    position = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      colour = "purple",
      size = 3,
      na.rm = TRUE,
      node_size = node_size,
      n_edge_points = n_edge_points,
      n_node_points = n_node_points,
      edge_geometry = edge_geometry
    )
  )
}

dag_layer <- function(
  layer,
  discover = character(),
  default_label = FALSE,
  debug = FALSE
) {
  structure(
    list(
      layer = layer,
      discover = discover,
      default_label = default_label,
      debug = debug
    ),
    class = "dag_layer"
  )
}

#' @export
`$.dag_layer` <- function(x, name) {
  if (name %in% c("layer", "discover", "default_label", "debug")) {
    .subset2(x, name)
  } else {
    .subset2(x, "layer")[[name]]
  }
}

# A ggproto layer is an environment, so filling in what a layer can only learn
# from the plot it joins would otherwise write into the object the caller
# holds. A layer stored in a variable and added to a second plot would carry
# the first plot's node size, edge geometry, and label mapping with it.
clone_layer <- function(layer) {
  cloned <- list2env(
    as.list(layer, all.names = TRUE),
    parent = parent.env(layer)
  )
  class(cloned) <- class(layer)
  cloned
}

# A layer sees only the layers added before it, so anything it takes from the
# rest of the plot depends on the order the plot was assembled in. The plot is
# whole by the time it is built, so the same question asked again there has an
# order-independent answer, and `setup_layer()` is the first step of the build
# that is handed the plot.
plot_aware_layer <- function(layer, resolve) {
  ggplot2::ggproto(
    "DagPlotAwareLayer",
    layer,
    setup_layer = function(self, data, plot) {
      data <- ggplot2::ggproto_parent(layer, self)$setup_layer(data, plot)
      resolve(self, plot)
      data
    }
  )
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.dag_layer <- function(object, plot, ...) {
  layer <- clone_layer(.subset2(object, "layer"))
  discover <- .subset2(object, "discover")
  discover_at_build <- character()

  if ("node_size" %in% discover) {
    if (is.null(layer$stat_params$node_size)) {
      discover_at_build <- c(discover_at_build, "node_size")
      discovered <- discover_node_size(plot)
      if (!is.null(discovered)) {
        layer$stat_params$node_size <- discovered
      }
    }
  }

  if ("edge_geometry" %in% discover) {
    if (is.null(layer$stat_params$edge_geometry)) {
      discover_at_build <- c(discover_at_build, "edge_geometry")
      layer$stat_params$edge_geometry <- discover_edge_geometry(plot)
    }
  }

  if (isTRUE(.subset2(object, "default_label"))) {
    layer <- add_default_label_mapping(layer, plot)
  }

  if (length(discover_at_build) > 0) {
    layer <- plot_aware_layer(layer, function(self, plot) {
      if ("node_size" %in% discover_at_build) {
        self$stat_params$node_size <- discover_node_size(plot)
      }
      if ("edge_geometry" %in% discover_at_build) {
        self$stat_params$edge_geometry <- discover_edge_geometry(plot)
      }
    })
  }

  plot <- ggplot2::ggplot_add(layer, plot, ...)

  if (
    isTRUE(.subset2(object, "debug")) &&
      isTRUE(ggdag_option("debug_repel_points", FALSE))
  ) {
    debug_layer <- make_debug_repel_layer(
      node_size = layer$stat_params$node_size,
      n_edge_points = layer$stat_params$n_edge_points,
      n_node_points = layer$stat_params$n_node_points,
      edge_geometry = layer$stat_params$edge_geometry
    )
    plot <- ggplot2::ggplot_add(debug_layer, plot, ...)
  }

  plot
}

# Node names are drawn only when nothing else maps `label`. A plot-level
# mapping is inherited like any other aesthetic, so the default cannot be
# injected in the constructor, where the plot is not yet visible.
add_default_label_mapping <- function(layer, plot) {
  if (!is.null(layer$mapping$label)) {
    return(layer)
  }

  inherits_plot_aes <- !identical(layer$inherit.aes, FALSE)
  if (inherits_plot_aes && !is.null(plot$mapping$label)) {
    return(layer)
  }

  if (is.null(layer$mapping)) {
    layer$mapping <- ggplot2::aes()
  }
  layer$mapping$label <- ggplot2::aes(label = .data$name)$label

  layer
}

# The geometry the plot's DAG edge layers draw each edge with. Repulsion
# obstacles follow those curves, so a label cannot be placed on top of a drawn
# edge, and edges no bent layer claims stay straight.
discover_edge_geometry <- function(plot) {
  plot_data <- plot$data
  if (inherits(plot_data, "tidy_dagitty")) {
    plot_data <- pull_dag_data(plot_data)
  }
  if (!is.data.frame(plot_data)) {
    return(NULL)
  }

  specs <- list()
  for (existing in plot$layers) {
    spec <- edge_layer_geometry(existing, plot_data)
    if (!is.null(spec)) {
      specs[[length(specs) + 1]] <- spec
    }
  }

  if (length(specs) == 0) {
    return(NULL)
  }

  # Kept row for row: two edges drawn between the same pair of nodes are two
  # rows, and a fan spreads them apart only because there are two of them.
  do.call(rbind, specs)
}

# Which of ggdag's bent edge geometries a layer draws, if any. A straight link
# layer needs no tracing, so it is not one of them.
edge_geometry_type <- function(stat) {
  types <- c(
    arc = "StatEdgeArc",
    diagonal = "StatEdgeDiagonal",
    fan = "StatEdgeFan"
  )
  drawn <- names(types)[vapply(types, inherits, logical(1), x = stat)]
  if (length(drawn) == 0) {
    return(NULL)
  }
  drawn[[1]]
}

edge_layer_geometry <- function(layer, plot_data) {
  type <- edge_geometry_type(layer$stat)
  if (is.null(type)) {
    return(NULL)
  }

  strength <- layer$stat_params$strength
  if (!is.numeric(strength) || length(strength) != 1 || strength == 0) {
    return(NULL)
  }

  layer_data <- resolve_layer_data(layer, plot_data)
  if (is.null(layer_data)) {
    return(NULL)
  }

  layer_data <- layer_data[!is.na(layer_data$xend), , drop = FALSE]
  if (nrow(layer_data) == 0) {
    return(NULL)
  }

  # A fan spreads the edges that share a pair of nodes, so it can only be
  # traced when the node names those edges run between are known.
  column <- function(name, default) {
    if (name %in% names(layer_data)) layer_data[[name]] else default
  }
  from <- as.character(column("name", NA_character_))
  to <- as.character(column("to", NA_character_))
  if (identical(type, "fan") && (anyNA(from) || anyNA(to))) {
    return(NULL)
  }

  circular <- column("circular", FALSE)
  circular[is.na(circular)] <- FALSE

  data.frame(
    x = layer_data$x,
    y = layer_data$y,
    xend = layer_data$xend,
    yend = layer_data$yend,
    circular = circular,
    type = type,
    strength = strength,
    n = layer$stat_params$n %||% 100,
    fold = isTRUE(layer$stat_params$fold),
    flipped = isTRUE(layer$stat_params$flipped),
    from = from,
    to = to,
    stringsAsFactors = FALSE
  )
}

resolve_layer_data <- function(layer, plot_data) {
  layer_data <- layer$data
  if (is.null(layer_data) || inherits(layer_data, "waiver")) {
    layer_data <- plot_data
  } else if (is.function(layer_data)) {
    layer_data <- tryCatch(layer_data(plot_data), error = function(e) NULL)
  }

  if (!is.data.frame(layer_data)) {
    return(NULL)
  }
  if (!all(c("x", "y", "xend", "yend") %in% names(layer_data))) {
    return(NULL)
  }

  layer_data
}

node_size_to_cap <- function(node_size) {
  node_size / 2
}

dag_edge_layer <- function(layer) {
  structure(
    list(layer = layer),
    class = "dag_edge_layer"
  )
}

#' @export
`$.dag_edge_layer` <- function(x, name) {
  if (name == "layer") {
    .subset2(x, "layer")
  } else {
    .subset2(x, "layer")[[name]]
  }
}

# The ends whose cap the user has not set, either as an aesthetic or as a
# fixed value.
unset_edge_caps <- function(layer) {
  ends <- c("start_cap", "end_cap")
  ends[vapply(
    ends,
    function(end) {
      is.null(layer$mapping[[end]]) && is.null(layer$aes_params[[end]])
    },
    logical(1)
  )]
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.dag_edge_layer <- function(object, plot, ...) {
  layer <- clone_layer(.subset2(object, "layer"))
  needs_cap <- unset_edge_caps(layer)

  if (length(needs_cap) > 0) {
    discovered <- discover_node_size(plot)
    if (!is.null(discovered)) {
      cap_mm <- node_size_to_cap(discovered)
      cap_expr <- rlang::expr(ggraph::circle(!!cap_mm, "mm"))
      cap_quo <- rlang::new_quosure(cap_expr, env = rlang::base_env())
      if (is.null(layer$mapping)) {
        layer$mapping <- ggplot2::aes()
      }
      for (end in needs_cap) {
        layer$mapping[[end]] <- cap_quo
      }
      needs_cap <- character()
    }
  }

  if (length(needs_cap) > 0) {
    # No node layer is on the plot yet, which is the order every layer-by-layer
    # example uses. The node layer that follows is in view once the plot is
    # built, so the caps are settled there instead.
    layer <- plot_aware_layer(layer, function(self, plot) {
      discovered <- discover_node_size(plot)
      cap <- if (is.null(discovered)) {
        NULL
      } else {
        ggraph::circle(node_size_to_cap(discovered), "mm")
      }
      for (end in needs_cap) {
        self$aes_params[[end]] <- cap
      }
    })
  }

  ggplot2::ggplot_add(layer, plot, ...)
}

discover_node_size <- function(plot) {
  for (existing in plot$layers) {
    if (
      inherits(existing$geom, "GeomDagNode") ||
        inherits(existing$geom, "GeomDagPoint")
    ) {
      size <- existing$aes_params$size
      if (!is.null(size)) {
        return(size)
      }
      return(existing$geom$default_aes$size %||% 16)
    }
  }
  NULL
}

GeomDagPoint <- ggplot2::ggproto(
  "GeomDagPoint",
  ggplot2::GeomPoint,
  default_aes = ggplot2::aes(
    shape = 19,
    colour = "black",
    size = 16,
    fill = NA,
    alpha = NA,
    stroke = 0.5
  ),
  draw_key = draw_key_dag_point
)

GeomDagNode <- ggplot2::ggproto(
  "GeomDagNode",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour", "internal_colour"),
  default_aes = ggplot2::aes(
    shape = 19,
    colour = "black",
    size = 16,
    fill = NA,
    alpha = NA,
    stroke = 0.5,
    internal_colour = "white"
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    coords <- coord$transform(data, panel_params)
    grid::gList(
      ggname(
        "geom_dag_node",
        grid::pointsGrob(
          coords$x,
          coords$y,
          pch = coords$shape,
          gp = grid::gpar(
            col = alpha(coords$colour, coords$alpha),
            fill = alpha(coords$fill, coords$alpha),
            fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
            lwd = coords$stroke * .stroke / 2
          )
        )
      ),
      ggname(
        "geom_dag_node",
        grid::pointsGrob(
          coords$x,
          coords$y,
          pch = coords$shape,
          gp = grid::gpar(
            col = alpha(coords$internal_colour, coords$alpha),
            fill = alpha(coords$fill, coords$alpha),
            fontsize = (coords$size - 1) * .pt + coords$stroke * .stroke / 2,
            lwd = coords$stroke * .stroke / 2
          )
        )
      ),
      ggname(
        "geom_dag_node",
        grid::pointsGrob(
          coords$x,
          coords$y,
          pch = coords$shape,
          gp = grid::gpar(
            col = alpha(coords$colour, coords$alpha),
            fill = alpha(coords$fill, coords$alpha),
            fontsize = (coords$size - 2) * .pt + coords$stroke * .stroke / 2,
            lwd = coords$stroke * .stroke / 2
          )
        )
      )
    )
  },
  draw_key = draw_key_dag_point
)

GeomDagText <- ggplot2::ggproto(
  "GeomDagText",
  ggplot2::GeomText,
  default_aes = ggplot2::aes(
    colour = "white",
    size = 4,
    angle = 0,
    hjust = 0.5,
    vjust = 0.5,
    alpha = NA,
    family = "",
    fontface = "bold",
    lineheight = 1.2
  )
)

StatEdgeLink <- ggplot2::ggproto(
  "StatEdgeLink",
  ggraph::StatEdgeLink,
  setup_data = function(data, params) {
    data <- data[!is.na(data$xend), ]

    # A DAG with no edges keeps its aesthetic columns and draws nothing;
    # returning NULL here would leave `check_required_aesthetics()` reporting
    # every aesthetic as missing.
    if (nrow(data) == 0) {
      return(data)
    }

    data <- ggraph::StatEdgeLink$setup_data(data, params)
    convert_group_to_integer(data)
  },
  compute_panel = function(data, scales, ...) {
    # Call parent method
    data <- ggraph::StatEdgeLink$compute_panel(data, scales, ...)
    # Convert groups to integer after computation
    convert_group_to_integer(data)
  }
)

StatEdgeArc <- ggplot2::ggproto(
  "StatEdgeArc",
  ggraph::StatEdgeArc,
  setup_data = function(data, params) {
    data <- data[!is.na(data$xend), ]
    data <- handle_missing_circular_column(data)

    if (nrow(data) == 0) {
      return(data)
    }

    data <- ggraph::StatEdgeArc$setup_data(data, params)
    convert_group_to_integer(data)
  },
  compute_panel = function(data, scales, ...) {
    data <- ggraph::StatEdgeArc$compute_panel(data, scales, ...)
    convert_group_to_integer(data)
  },
  default_aes = ggplot2::aes(filter = TRUE)
)

StatEdgeDiagonal <- ggplot2::ggproto(
  "StatEdgeDiagonal",
  ggraph::StatEdgeDiagonal,
  setup_data = function(data, params) {
    data <- data[!is.na(data$xend), ]
    data <- handle_missing_circular_column(data)

    if (nrow(data) == 0) {
      return(data)
    }

    data <- ggraph::StatEdgeDiagonal$setup_data(data, params)
    convert_group_to_integer(data)
  },
  compute_panel = function(data, scales, ...) {
    data <- ggraph::StatEdgeDiagonal$compute_panel(data, scales, ...)
    convert_group_to_integer(data)
  },
  default_aes = ggplot2::aes(filter = TRUE)
)

StatEdgeFan <- ggplot2::ggproto(
  "StatEdgeFan",
  ggraph::StatEdgeFan,
  setup_data = function(data, params) {
    data <- data[!is.na(data$xend), ]

    if (nrow(data) == 0) {
      return(data)
    }

    # `ggraph::StatEdgeFan` identifies parallel edges by the pair of node ids
    # in `from` and `to`, so both columns have to be numbered from one shared
    # set of node names. Numbering each column on its own gives the same node
    # a different id in each, which lets unrelated edges collide into a fan.
    node_names <- sort(unique(c(data$from, data$to)))
    data$from <- match(data$from, node_names)
    data$to <- match(data$to, node_names)

    data <- ggraph::StatEdgeFan$setup_data(data, params)
    convert_group_to_integer(data)
  },
  compute_panel = function(data, scales, ...) {
    data <- ggraph::StatEdgeFan$compute_panel(data, scales, ...)
    convert_group_to_integer(data)
  },
  default_aes = ggplot2::aes(filter = TRUE)
)


GeomDAGEdgePath <- ggplot2::ggproto(
  "GeomDAGEdgePath",
  ggraph::GeomEdgePath,
  setup_data = function(data, params) {
    ggraph::GeomEdgePath$setup_data(data, params)
  },
  handle_na = function(data, params) {
    if (
      all(
        c("x", "y", "edge_width", "edge_colour", "edge_linetype") %in%
          names(data)
      )
    ) {
      data <- ggraph::GeomEdgePath$handle_na(data, params)
    } else {
      # data <- NULL
      data$edge_colour <- "black"
      data$edge_width <- 0.6
      data$edge_linetype <- "solid"
      data <- ggraph::GeomEdgePath$handle_na(data, params)
    }
    data
  },
  optional_aes = c("colour", "color"),
  non_missing_aes = c("direction", "direction_type"),
  default_aes = ggplot2::aes(
    linewidth = NA,
    edge_colour = "black",
    edge_width = 0.6,
    edge_linetype = "solid",
    edge_alpha = NA,
    start_cap = ggraph::circle(8, "mm"),
    end_cap = ggraph::circle(8, "mm"),
    label = NA,
    label_pos = 0.5,
    label_size = 3.88,
    angle = 0,
    hjust = 0.5,
    vjust = 0.5,
    family = "",
    fontface = 1,
    lineheight = 1.2,
    direction = "->",
    direction_type = "->"
  ),
  draw_key = draw_key_dag_edge
)


silence_scales <- function(plot) {
  old_scales <- plot$scales
  plot$scales <- ggproto(
    "ScalesListQuiet",
    old_scales,
    add = silent_add
  )
  plot
}

silent_add <- function(self, scale) {
  if (is.null(scale)) {
    return()
  }

  prev_aes <- self$find(scale$aesthetics)
  if (any(prev_aes)) {
    # Get only the first aesthetic name in the returned vector -- it can
    # sometimes be c("x", "xmin", "xmax", ....)
    scalename <- self$scales[prev_aes][[1]]$aesthetics[1]
  }

  # Remove old scale for this aesthetic (if it exists)
  self$scales <- c(self$scales[!prev_aes], list(scale))
}
