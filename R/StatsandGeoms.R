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
    # Arrow
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
    ),
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
    # Upper arrow
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
    ),
    # Lower arrow
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
  grob <- segmentsGrob(
    0.2,
    0.5,
    0.8,
    0.5,
    gp = gpar(
      col = alpha(
        data$edge_colour %||% data$colour %||% "black",
        data$edge_alpha %||% data$alpha %||% 1
      ),
      fill = alpha(
        data$edge_colour %||% data$colour %||% "black",
        data$edge_alpha %||% data$alpha %||% 1
      ),
      lwd = (data$edge_width %||% 0.6) * .stroke * .7,
      lty = data$edge_linetype %||% data$linetype %||% 1
    ),
    arrow = arrow
  )

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
    data$circular <- FALSE
  }
  data[is.na(data$circular), "circular"] <- FALSE
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
      unique(dplyr::select(data, -xend, -yend))
    } else {
      unique(data)
    }
  }
)

StatNodesRepel <- ggplot2::ggproto(
  "StatNodesRepel",
  ggplot2::Stat,
  compute_layer = function(data, scales, params) {
    if (all(c("xend", "yend") %in% names(data))) {
      data <- unique(dplyr::select(data, -xend, -yend))
      if ("alpha" %in% names(data)) {
        data |>
          dplyr::filter(!is.na(alpha), !is.na(label))
      } else {
        data |>
          dplyr::filter(!is.na(label)) |>
          group_by(PANEL) |>
          dplyr::distinct(label, .keep_all = TRUE) |>
          ungroup()
      }
    } else {
      unique(data)
    }
  }
)

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

    if (nrow(data) > 0) {
      data <- ggraph::StatEdgeLink$setup_data(data, params)
      data <- convert_group_to_integer(data)
    } else {
      data <- NULL
    }
    data
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

    if (nrow(data) > 0) {
      data <- ggraph::StatEdgeArc$setup_data(data, params)
      data <- convert_group_to_integer(data)
    } else {
      data <- NULL
    }
    data
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

    if (nrow(data) > 0) {
      data <- ggraph::StatEdgeDiagonal$setup_data(data, params)
      data <- convert_group_to_integer(data)
    } else {
      data <- NULL
    }
    data
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

    if (nrow(data) > 0) {
      # turn `from` and `to` into integers for `ggraph::StatEdgeFan`
      data$from <- rank(data$from)
      data$to <- rank(data$to)

      data <- ggraph::StatEdgeFan$setup_data(data, params)
      data <- convert_group_to_integer(data)
    } else {
      data <- NULL
    }
    data
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
