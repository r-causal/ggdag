library(ggnetwork)
library(network)
library(sna)
library(ggplot2)
library(purrr)

geom_dag_node <- function(mapping = NULL, data = NULL,
                          position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  # if (is.null(mapping)) mapping <- aes(x = node_x, y = node_y)
  # if (is.null(mapping$x)) mapping$x <- substitute(node_x)
  # if (is.null(mapping$y)) mapping$y <- substitute(node_y)

  layer(
    data = data,
    mapping = mapping,
    stat = ggnetwork:::StatNodes,
    geom = GeomDagNode,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


GeomDagNode <- ggproto("GeomDagNode", Geom,
                       required_aes = c("x", "y"),
                       non_missing_aes = c("size", "shape", "colour", "internal_colour"),
                       default_aes = aes(
                         shape = 19, colour = "black", size = 16, fill = NA,
                         alpha = NA, stroke = 0.5, internal_colour = "white"
                       ),

                       draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                         coords <- coord$transform(data, panel_params)
                         grid::gList(
                           ggplot2:::ggname("geom_dag_node",
                                            grid::pointsGrob(
                                              coords$x, coords$y,
                                              pch = coords$shape,
                                              gp = grid::gpar(
                                                col = alpha(coords$colour, coords$alpha),
                                                fill = alpha(coords$fill, coords$alpha),
                                                # Stroke is added around the outside of the point
                                                fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                                lwd = coords$stroke * .stroke / 2
                                              )
                                            )
                           ),
                           ggplot2:::ggname("geom_dag_node",
                                            grid::pointsGrob(
                                              coords$x, coords$y,
                                              pch = coords$shape,
                                              gp = grid::gpar(
                                                col = alpha(coords$internal_colour, coords$alpha),
                                                fill = alpha(coords$fill, coords$alpha),
                                                # Stroke is added around the outside of the point
                                                fontsize = (coords$size - 1) * .pt + coords$stroke * .stroke / 2,
                                                lwd = coords$stroke * .stroke / 2
                                              )
                                            )
                           ),
                           ggplot2:::ggname("geom_dag_node",
                                            grid::pointsGrob(
                                              coords$x, coords$y,
                                              pch = coords$shape,
                                              gp = grid::gpar(
                                                col = alpha(coords$colour, coords$alpha),
                                                fill = alpha(coords$fill, coords$alpha),
                                                # Stroke is added around the outside of the point
                                                fontsize = (coords$size - 2) * .pt + coords$stroke * .stroke / 2,
                                                lwd = coords$stroke * .stroke / 2
                                              )
                                            )
                           )
                         )
                       },

                       draw_key = draw_key_point
)

geom_dag_text <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      parse = FALSE,
                      nudge_x = 0,
                      nudge_y = 0,
                      check_overlap = FALSE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  if (is.null(mapping)) mapping <- aes(label = name)
  if (is.null(mapping$label)) mapping$label <- substitute(name)
  # if (is.null(mapping)) mapping <- aes(x = node_x, y = node_y, label = name)
  # if (is.null(mapping$x)) mapping$x <- substitute(node_x)
  # if (is.null(mapping$y)) mapping$y <- substitute(node_y)

  layer(
    data = data,
    mapping = mapping,
    stat = ggnetwork:::StatNodes,
    geom = GeomDagText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

GeomDagText <- ggproto("GeomDagText", GeomText, default_aes = aes(
  colour = "white", size = 3.88, angle = 0, hjust = 0.5,
  vjust = 0.5, alpha = NA, family = "", fontface = "bold", lineheight = 1.2
))



geom_dag_text_repel <- function(mapping = NULL,
                                data = NULL,
                                parse = FALSE,
                                ...,
                                box.padding = 0.35,
                                point.padding = 1.5,
                                segment.color = "#666666",
                                fontface = "bold",
                                segment.size = 0.5,
                                arrow = NULL,
                                force = 1,
                                max.iter = 2000,
                                nudge_x = 0,
                                nudge_y = 0,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {

  # if (is.null(mapping)) mapping <- aes(x = node_x, y = node_y)
  # if (is.null(mapping$x)) mapping$x <- substitute(node_x)
  # if (is.null(mapping$y)) mapping$y <- substitute(node_y)
  #
  ggplot2::layer(data = data,
                 mapping = mapping,
                 stat = ggnetwork:::StatNodes,
                 geom = ggrepel::GeomTextRepel,
                 position = "identity",
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(
                   parse = parse,
                   na.rm = na.rm,
                   box.padding = box.padding,
                   point.padding = point.padding,
                   segment.colour = segment.color %||% segment.colour,
                   segment.size = segment.size,
                   fontface = fontface,
                   arrow = arrow,
                   force = force,
                   max.iter = max.iter,
                   nudge_x = nudge_x,
                   nudge_y = nudge_y,
                   ...
                 )
  )

}

geom_dag_label_repel <- function(
  mapping = NULL, data = NULL,
  parse = FALSE,
  ...,
  box.padding = unit(0.35, "lines"),
  label.padding = unit(0.25, "lines"),
  point.padding = unit(1.5, "lines"),
  label.r = unit(0.15, "lines"),
  label.size = 0.25,
  segment.color = "grey50",
  segment.size = 0.5,
  arrow = NULL,
  force = 1,
  max.iter = 2000,
  nudge_x = 0,
  nudge_y = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {

  # if (is.null(mapping)) mapping <- aes(x = node_x, y = node_y)
  # if (is.null(mapping$x)) mapping$x <- substitute(node_x)
  # if (is.null(mapping$y)) mapping$y <- substitute(node_y)

  layer(
    data = data,
    mapping = mapping,
    stat = ggnetwork:::StatNodes,
    geom = ggrepel::GeomLabelRepel,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      box.padding  = box.padding,
      label.padding = label.padding,
      point.padding  = point.padding,
      label.r = label.r,
      label.size = label.size,
      segment.colour = segment.color %||% segment.colour,
      segment.size = segment.size,
      arrow = arrow,
      na.rm = na.rm,
      force = force,
      max.iter = max.iter,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      ...
    )
  )
}

# GeomScaledSegment <- ggproto("GeomScaledSegment", GeomSegment,
#                        required_aes = c("x", "y", "xend", "yend"),
#                        non_missing_aes = c("linetype", "size", "shape"),
#                        default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA, scale_by = -.5))

geom_dag_edges <- function(mapping = NULL, data_directed = NULL, data_bidirected = NULL,
                           size = .6,
                           curvature = 0.1,
                           scale_by = .5,
                           arrow_directed = arrow(length = unit(5, "pt"), type = "closed"),
                           arrow_bidirected = arrow(length = unit(5, "pt"), ends = "both", type = "closed"),
                           position = "identity", na.rm = TRUE, show.legend = NA, inherit.aes = TRUE,
                            ...) {

  if (is.null(data_directed)) data_directed <- function(x) {
    if (suppressWarnings(is.null(x$collider_line))) {
      filter(x, direction != "<->")
    } else {
      filter(x, direction != "<->", !collider_line)
    }
  }
  if (is.null(data_bidirected)) data_bidirected <- function(x) {
    if (suppressWarnings(is.null(x$collider_line))) {
      filter(x, direction == "<->")
    } else {
      filter(x, direction == "<->", !collider_line)
    }
  }

  list(
    layer(mapping = mapping,
          geom = GeomSegment,
          data = data_directed,
          stat = StatScaleEdges,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(scale_by = -scale_by, arrow = arrow_directed, size = size, na.rm = na.rm), ...),
    layer(mapping = mapping,
          geom = GeomCurve,
          data = data_bidirected,
          stat = StatScaleEdges,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(scale_by = -scale_by, arrow = arrow_bidirected, size = size, curvature = curvature, na.rm = na.rm, ...))
  )
}

StatScaleEdges <-
  ggplot2::ggproto("StatScaleEdges", ggplot2::Stat,
                   required_aes = "scale_by",
                   compute_group = function(data, scales, params, scale_by) {
                     #browser()
                     data <- unique(subset(data, !(x == xend & y == yend)))
                     dplyr::mutate(data,
                            x = scale_line(xend, yend, x, y, scale_by)$end_x,
                            y = scale_line(xend, yend, x, y, scale_by)$end_y,
                            xend = scale_line(x, y, xend, yend, scale_by)$end_x,
                            yend = scale_line(x, y, xend, yend, scale_by)$end_y)
                   }
  )

geom_dag_collider_edges <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       size = .6,
                       curvature = 0.5,
                       angle = 90,
                       ncp = 5,
                       arrow = NULL,
                       lineend = "butt",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  if (is.null(data)) data <- function(x) filter(x, direction == "<->", collider_line)
  if (is.null(mapping)) mapping <- aes(linetype = factor(collider_line, levels = TRUE, "activated by \nadjustment \nfor collider"))
  if (is.null(mapping$linetype)) mapping$linetype <- substitute(factor(collider_line, levels = TRUE, "activated by \nadjustment \nfor collider"))

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCurve,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      size = size,
      arrow = arrow,
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

theme_dag <- function(base_size = 12, base_family = "", ...) {
  list(
  scale_linetype_manual(name = NULL, values = "dashed"),
  scale_shape_manual(name = "", drop = FALSE, values = c("unadjusted" = 19, "adjusted" = 15)),
  scale_alpha_manual(name = " ", drop = FALSE, values = c("adjusted" = .1, "unadjusted" = 1)),
  scale_color_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50", breaks = breaks),
  scale_fill_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50"),
  scale_x_continuous(expand = expand_x),
  scale_y_continuous(expand = expand_y),
  theme_minimal(base_size = base_size, base_family = base_family),
  theme(strip.background = element_rect(color = "grey85", fill = "grey85"),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        ...)
  )
}

scale_dag <- function(expand_x = expand_scale(c(.25, .25)),
                      expand_y = expand_scale(c(.10, .10)),
                      breaks = waiver()) {
  list(
    scale_linetype_manual(name = NULL, values = "dashed"),
    scale_shape_manual(name = "", drop = FALSE, values = c("unadjusted" = 19, "adjusted" = 15)),
    scale_alpha_manual(name = " ", drop = FALSE, values = c("adjusted" = .1, "unadjusted" = 1)),
    scale_color_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50", breaks = breaks),
    scale_fill_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50"),
    scale_x_continuous(expand = expand_x),
    scale_y_continuous(expand = expand_y)
  )
}

# theme_dag_facet <- function(base_size = 12, base_family = "", expand = c(.25, .25), breaks = waiver(), ...)
# {
#   list(
#     theme_dag(base_size = base_size, base_family = base_family, breaks = breaks),
#     theme(panel.border = ggplot2::element_rect(fill = NA, color = "grey50"), ...)
#   )
# }

scale_line <- function(start_x, start_y, end_x, end_y, scale_by) {
  if (any(start_x == end_x && start_y == end_y)) {
    warning("Scaling failed: returning original coordinates")
    return(list(start_x = start_x, start_y = start_y, end_x = end_x, end_y = end_y))
  }
  df <- data.frame(start_x, start_y, end_x, end_y)
  df <- df %>%
    add_row(start_x = 0, end_x = 0, start_y = 1, end_y = -1) %>%
    add_row(start_x = 1, end_x = 1, start_y = -1, end_y = 1) %>%
    add_row(start_x = -1, end_x = 1, start_y = 0, end_y = 0) %>%
    add_row(start_x = 1, end_x = -1, start_y = 1, end_y = 1)
  df2 <- df %>%
    mutate(dx = end_x - start_x, dy = end_y - start_y,
           flat_along_x = dy == 0, flat_along_y = dx == 0,
           length = sqrt(dx^2 + dy^2),
           scale = (length + scale_by) / length,
           dx = dx * scale_by,
           dy = dy * scale_by) %>%

    mutate(scaled_end_x = ifelse(flat_along_x & end_x < start_x, end_x + scale_by, NA),
           scaled_end_x = ifelse(is.na(scaled_end_x) & flat_along_x & end_x > start_x, end_x - scale_by, scaled_end_x),
           scaled_end_x = ifelse(is.na(scaled_end_x), start_x + dx, scaled_end_x)) %>%

    mutate(scaled_end_y = ifelse(flat_along_y & end_y < start_y, end_y + scale_by, NA),
           scaled_end_y = ifelse(is.na(scaled_end_y) & flat_along_y & end_y > start_y, end_y - scale_by, scaled_end_y),
           scaled_end_y = ifelse(is.na(scaled_end_y), start_y + dy, scaled_end_y)) %>%

    mutate(scaled_start_x = ifelse(flat_along_x & end_x < start_x, start_x - scale_by, NA),
           scaled_start_x = ifelse(is.na(scaled_start_x) & flat_along_x & end_x > start_x, start_x + scale_by, scaled_start_x),
           scaled_start_x = ifelse(is.na(scaled_start_x), end_x + dx, scaled_start_x)) %>%

    mutate(scaled_start_y = ifelse(flat_along_y & end_y < start_y, start_y - scale_by, NA),
           scaled_start_y = ifelse(is.na(scaled_start_y) & flat_along_y & end_y > start_y, start_y + scale_by, scaled_start_y),
           scaled_start_y = ifelse(is.na(scaled_start_y), end_y + dy, scaled_start_y)) %>%

    select(start_x = scaled_start_x, start_y = scaled_start_y, end_x = scaled_end_x, end_y = scaled_end_y)

    gglines(df) + gglines(df2)

  dx <- end_x - start_x
  dy <- end_y - start_y
  if (dx == 0) {
    # vertical line:
      if (end_y < start_y) {
        scaled_end_y = end_y - scale_by
    } else {
        scaled_end_y = end_y + scale_by
    }
  } else if (dy == 0) {
    # horizontal line:
      if (end_x < start_x) {
        scaled_end_x = end_x - scale_by
      } else {
        scaled_end_x = end_x + scale_by
    }
  } else {
    # non-horizontal, non-vertical line:
      length = sqrt(dx^2 + dy^2)
      scale = (length + scale_by) / length

      dx = ifelse(dx == 0 & end_y < start_y, scale,
                  ifelse(dx == 0 & end_y > start_y, -scale,
                                       dx * scale))
      dy = ifelse(dy == 0 & end_x < start_x, scale,
                  ifelse(dy == 0 & end_x > start_x, -scale,
                         dy * scale))
      scaled_end_x =  ifelse(abs(dy) == abs(scale), end_x + dx, start_x + dx)
      scaled_end_y = start_y + dy
      scaled_start_x = end_x + dx
      scaled_start_y = end_y + dy
  }

  list(start_x = scaled_start_x, start_y = scaled_start_y, end_x = scaled_end_x, end_y = scaled_end_y)
}

# scale_line <- function(start_x, start_y, end_x, end_y, scale_by) {
#
#   line_dist <- sqrt((end_x - start_x)^2 + (end_y - start_y)^2)
#
#   scale_by <-  line_dist / scale_by
#
#   scaled_end_x <- ((1 - scale_by) * start_x) + (scale_by * end_x)
#   scaled_end_y <- ((1 - scale_by) * start_y) + (scale_by * end_y)
#
#   list(start_x = start_x, start_y = start_y, end_x = scaled_end_x, end_y = scaled_end_y)
# }

