

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

                       draw_key = ggplot2::draw_key_point
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

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  if (is.null(mapping)) mapping <- ggplot2::aes(label = name)
  if (is.null(mapping$label)) mapping$label <- substitute(name)
  # if (is.null(mapping)) mapping <- aes(x = node_x, y = node_y, label = name)
  # if (is.null(mapping$x)) mapping$x <- substitute(node_x)
  # if (is.null(mapping$y)) mapping$y <- substitute(node_y)

  ggplot2::layer(
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

GeomDagText <- ggplot2::ggproto("GeomDagText", ggplot2::GeomText, default_aes = ggplot2::aes(
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
  box.padding = grid::unit(0.35, "lines"),
  label.padding = grid::unit(0.25, "lines"),
  point.padding = grid::unit(1.5, "lines"),
  label.r = grid::unit(0.15, "lines"),
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

  ggplot2::layer(
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
                           curvature = 0.2,
                           cap = ggraph::circle(8, 'mm'),
                           arrow_directed = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
                           arrow_bidirected = grid::arrow(length = grid::unit(5, "pt"), ends = "both", type = "closed"),
                           position = "identity", na.rm = TRUE, show.legend = NA, inherit.aes = TRUE, fold = FALSE,
                            ...) {

  if (is.null(data_directed)) data_directed <- function(x) {
    if (suppressWarnings(is.null(x$collider_line))) {
      dplyr::filter(x, direction != "<->")
    } else {
      dplyr::filter(x, direction != "<->", !collider_line)
    }
  }
  if (is.null(data_bidirected)) data_bidirected <- function(x) {
    if (suppressWarnings(is.null(x$collider_line))) {
      dplyr::filter(x, direction == "<->")
    } else {
      dplyr::filter(x, direction == "<->", !collider_line)
    }
  }
  # list(
  #   ggplot2::layer(mapping = mapping,
  #         geom = ggplot2::GeomSegment,
  #         data = data_directed,
  #         stat = StatScaleEdges,
  #         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
  #         params = list(scale_by = -scale_by, arrow = arrow_directed, size = size, na.rm = na.rm), ...),
  #   ggplot2::layer(mapping = mapping,
  #         geom = ggplot2::GeomCurve,
  #         data = data_bidirected,
  #         stat = StatScaleEdges,
  #         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
  #         params = list(scale_by = -scale_by, arrow = arrow_bidirected, size = size, curvature = curvature, na.rm = na.rm, ...))
  # )

  if (is.null(mapping)) {
    arc_mapping <- ggplot2::aes(circular = circular)
  } else {
    arc_mapping <- mapping
    if (is.null(arc_mapping$circular)) arc_mapping$circular <- substitute(circular)
  }

  list(
    ggplot2::layer(mapping = mapping,
                   geom = ggraph::GeomEdgePath,
                   data = data_directed,
                   stat = StatEdgeLink,
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                   params = list(start_cap = cap, end_cap = cap, arrow = arrow_directed, edge_width = size, interpolate = FALSE, na.rm = na.rm), ...),
    ggplot2::layer(mapping = arc_mapping,
                   geom = ggraph::GeomEdgePath,
                   data =  data_bidirected,
                   stat = ggraph::StatEdgeArc,
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                   params = list(start_cap = cap, end_cap = cap, arrow = arrow_bidirected,
                                 edge_width = size, curvature = curvature,
                                 interpolate = FALSE, fold = fold, na.rm = na.rm,
                                 n = 100, lineend = "butt",
                                 linejoin = "round", linemitre = 1,
                                 label_colour = 'black',  label_alpha = 1,
                                 label_parse = FALSE, check_overlap = FALSE,
                                 angle_calc = 'rot', force_flip = TRUE,
                                 label_dodge = NULL, label_push = NULL, ...))
  )
}

StatEdgeLink <- ggplot2::ggproto('StatEdgeLink', ggforce::StatLink,
                        setup_data = function(data, params) {
                          data <- data[!is.na(data$xend), ]
                          ggforce::StatLink$setup_data(data, params)
                        },
                        default_aes = ggplot2::aes(filter = TRUE)

)


geom_dag_edges_link <- function(mapping = NULL, data = NULL,
                                                    position = "identity", arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed"), n = 100,
                                                    lineend = "butt", linejoin = "round", linemitre = 1,
                                                    label_colour = 'black',  label_alpha = 1,
                                                    label_parse = FALSE, check_overlap = FALSE,
                                                    angle_calc = 'rot', force_flip = TRUE,
                                                    label_dodge = NULL, label_push = NULL,
                                                    show.legend = NA,
                                                    start_cap = ggraph::circle(8, 'mm'),
                                                    end_cap = ggraph::circle(8, 'mm'),
                                                    ...) {


  # mapping <- ggraph:::completeEdgeAes(mapping)
  # mapping <- ggraph:::aesIntersect(mapping, ggplot2::aes_(x=~x, y=~y, xend=~xend, yend=~yend))
  #if (is.null(mapping)) mapping <- ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
  ggplot2::layer(data = data, mapping = mapping, stat = StatEdgeLink,
        geom = ggraph:::GeomEdgePath, position = position, show.legend = show.legend,
        inherit.aes = TRUE,
        params = ggraph:::expand_edge_aes(
          list(arrow = arrow, lineend = lineend, linejoin = linejoin,
               linemitre = linemitre, na.rm = FALSE, n = n,
               interpolate = FALSE,
               label_colour = label_colour, label_alpha = label_alpha,
               label_parse = label_parse, check_overlap = check_overlap,
               angle_calc = angle_calc, force_flip = force_flip,
               label_dodge = label_dodge, label_push = label_push,
               start_cap = start_cap,
               end_cap = end_cap,
               ...)
        )
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
  if (is.null(mapping)) mapping <- ggplot2::aes(linetype = factor(collider_line, levels = TRUE, "activated by \nadjustment \nfor collider"))
  if (is.null(mapping$linetype)) mapping$linetype <- substitute(factor(collider_line, levels = TRUE, "activated by \nadjustment \nfor collider"))

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomCurve,
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
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family),
    ggplot2::theme(strip.background = ggplot2::element_rect(color = "grey85", fill = "grey85"),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        ...)
  )
}

scale_dag <- function(expand_x = ggplot2::expand_scale(c(.10, .10)),
                      expand_y = ggplot2::expand_scale(c(.10, .10)),
                      breaks = ggplot2::waiver()) {
  list(
    ggplot2::scale_linetype_manual(name = NULL, values = "dashed"),
    ggplot2::scale_shape_manual(name = "", drop = FALSE, values = c("unadjusted" = 19, "adjusted" = 15)),
    ggplot2::scale_alpha_manual(name = " ", drop = FALSE, values = c("adjusted" = .1, "unadjusted" = 1)),
    ggraph::scale_edge_alpha_manual(name = " ", drop = FALSE, values = c("adjusted" = .1, "unadjusted" = 1)),
    ggplot2::scale_color_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50", breaks = breaks),
    ggplot2::scale_fill_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50"),
    ggplot2::scale_x_continuous(expand = expand_x),
    ggplot2::scale_y_continuous(expand = expand_y)
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
    dplyr::add_row(start_x = 0, end_x = 0, start_y = 1, end_y = -1) %>%
    dplyr::add_row(start_x = 1, end_x = 1, start_y = -1, end_y = 1) %>%
    dplyr::add_row(start_x = -1, end_x = 1, start_y = 0, end_y = 0) %>%
    dplyr::add_row(start_x = 1, end_x = -1, start_y = 1, end_y = 1)
  df2 <- df %>%
    dplyr::mutate(dx = end_x - start_x, dy = end_y - start_y,
           flat_along_x = dy == 0, flat_along_y = dx == 0,
           length = sqrt(dx^2 + dy^2),
           scale = (length + scale_by) / length,
           dx = dx * scale_by,
           dy = dy * scale_by) %>%

    dplyr::mutate(scaled_end_x = ifelse(flat_along_x & end_x < start_x, end_x + scale_by, NA),
           scaled_end_x = ifelse(is.na(scaled_end_x) & flat_along_x & end_x > start_x, end_x - scale_by, scaled_end_x),
           scaled_end_x = ifelse(is.na(scaled_end_x), start_x + dx, scaled_end_x)) %>%

    dplyr::mutate(scaled_end_y = ifelse(flat_along_y & end_y < start_y, end_y + scale_by, NA),
           scaled_end_y = ifelse(is.na(scaled_end_y) & flat_along_y & end_y > start_y, end_y - scale_by, scaled_end_y),
           scaled_end_y = ifelse(is.na(scaled_end_y), start_y + dy, scaled_end_y)) %>%

    dplyr::mutate(scaled_start_x = ifelse(flat_along_x & end_x < start_x, start_x - scale_by, NA),
           scaled_start_x = ifelse(is.na(scaled_start_x) & flat_along_x & end_x > start_x, start_x + scale_by, scaled_start_x),
           scaled_start_x = ifelse(is.na(scaled_start_x), end_x + dx, scaled_start_x)) %>%

    dplyr::mutate(scaled_start_y = ifelse(flat_along_y & end_y < start_y, start_y - scale_by, NA),
           scaled_start_y = ifelse(is.na(scaled_start_y) & flat_along_y & end_y > start_y, start_y + scale_by, scaled_start_y),
           scaled_start_y = ifelse(is.na(scaled_start_y), end_y + dy, scaled_start_y)) %>%

    dplyr::select(start_x = scaled_start_x, start_y = scaled_start_y, end_x = scaled_end_x, end_y = scaled_end_y)

    #gglines(df) + gglines(df2)

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

