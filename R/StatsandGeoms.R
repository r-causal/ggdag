StatNodes <- ggplot2::ggproto("StatNodes", ggplot2::Stat,
  compute_layer = function(data, scales, params) {
    if (all(c("xend", "yend") %in% names(data))) {
      unique(dplyr::select(data, -xend, -yend))
    } else {
      unique(data)
    }
  }
)

StatNodesRepel <- ggplot2::ggproto("StatNodesRepel", ggplot2::Stat,
  compute_layer = function(data, scales, params) {
    if (all(c("xend", "yend") %in% names(data))) {
      data <- unique(dplyr::select(data, -xend, -yend))
      if ("alpha" %in% names(data)) {
        data %>%
          dplyr::filter(!is.na(alpha), !is.na(label))
      } else {
        data %>%
          dplyr::filter(!is.na(label)) %>%
          group_by(PANEL) %>%
          dplyr::distinct(label, .keep_all = TRUE) %>%
          ungroup()
      }
    } else {
      unique(data)
    }
  }
)

GeomDagPoint <- ggplot2::ggproto("GeomDagPoint", ggplot2::GeomPoint,
  default_aes = ggplot2::aes(
    shape = 19, colour = "black", size = 16, fill = NA,
    alpha = NA, stroke = 0.5
  )
)

GeomDagNode <- ggplot2::ggproto("GeomDagNode", ggplot2::Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour", "internal_colour"),
  default_aes = ggplot2::aes(
    shape = 19, colour = "black", size = 16, fill = NA,
    alpha = NA, stroke = 0.5, internal_colour = "white"
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    coords <- coord$transform(data, panel_params)
    grid::gList(
      ggname(
        "geom_dag_node",
        grid::pointsGrob(
          coords$x, coords$y,
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
          coords$x, coords$y,
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
          coords$x, coords$y,
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
  draw_key = ggplot2::draw_key_point
)

GeomDagText <- ggplot2::ggproto("GeomDagText", ggplot2::GeomText, default_aes = ggplot2::aes(
  colour = "white", size = 4, angle = 0, hjust = 0.5,
  vjust = 0.5, alpha = NA, family = "", fontface = "bold", lineheight = 1.2
))

StatEdgeLink <- ggplot2::ggproto("StatEdgeLink", ggraph::StatEdgeLink,
  setup_data = function(data, params) {
    data <- data[!is.na(data$xend), ]

    if (nrow(data) > 0) {
      data <- ggraph::StatEdgeLink$setup_data(data, params)
    } else {
      data <- NULL
    }
    data
  }
)

StatEdgeArc <- ggplot2::ggproto("StatEdgeArc", ggraph::StatEdgeArc,
  setup_data = function(data, params) {
    data <- data[!is.na(data$xend), ]
    data[is.na(data$circular), "circular"] <- FALSE

    if (nrow(data) > 0) {
      data <- ggraph::StatEdgeArc$setup_data(data, params)
    } else {
      data <- NULL
    }
    data
  },
  default_aes = ggplot2::aes(filter = TRUE)
)

StatEdgeDiagonal <- ggplot2::ggproto("StatEdgeDiagonal", ggraph::StatEdgeDiagonal,
  setup_data = function(data, params) {
    data <- data[!is.na(data$xend), ]
    data[is.na(data$circular), "circular"] <- FALSE

    if (nrow(data) > 0) {
      data <- ggraph::StatEdgeDiagonal$setup_data(data, params)
    } else {
      data <- NULL
    }
    data
  },
  default_aes = ggplot2::aes(filter = TRUE)
)

StatEdgeFan <- ggplot2::ggproto("StatEdgeFan", ggraph::StatEdgeFan,
  setup_data = function(data, params) {
    data <- data[!is.na(data$xend), ]

    if (nrow(data) > 0) {
      # turn `from` and `to` into integers for `ggraph::StatEdgeFan`
      data$from <- rank(data$from)
      data$to <- rank(data$to)

      data <- ggraph::StatEdgeFan$setup_data(data, params)
    } else {
      data <- NULL
    }
    data
  },
  default_aes = ggplot2::aes(filter = TRUE)
)


GeomDAGEdgePath <- ggplot2::ggproto("GeomDAGEdgePath", ggraph::GeomEdgePath,
  setup_data = function(data, params) {
    ggraph::GeomEdgePath$setup_data(data, params)
  },
  handle_na = function(data, params) {
    if (all(c("x", "y", "edge_width", "edge_colour", "edge_linetype") %in% names(data))) {
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
  non_missing_aes = c("direction", "direction_type"),
  default_aes = ggplot2::aes(
    linewidth = NA,
    edge_colour = "black", edge_width = 0.6, edge_linetype = "solid",
    edge_alpha = NA, start_cap = ggraph::circle(8, "mm"), end_cap = ggraph::circle(8, "mm"), label = NA,
    label_pos = 0.5, label_size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, family = "", fontface = 1,
    lineheight = 1.2, direction = "->", direction_type = "->"
  )
)


silence_scales <- function(plot) {
  old_scales <- plot$scales
  plot$scales <- ggproto(
    "ScalesListQuiet", old_scales,
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
