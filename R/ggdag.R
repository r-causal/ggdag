

#' Title
#'
#' @param mapping
#' @param data
#' @param position
#' @param ...
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @export
#'
#' @examples
geom_dag_node <- function(mapping = NULL, data = NULL,
                          position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  ggplot2::layer(
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

#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param ...
#' @param parse
#' @param nudge_x
#' @param nudge_y
#' @param check_overlap
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @export
#'
#' @examples
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



#' Title
#'
#' @param mapping
#' @param data
#' @param parse
#' @param ...
#' @param box.padding
#' @param point.padding
#' @param segment.color
#' @param fontface
#' @param segment.size
#' @param arrow
#' @param force
#' @param max.iter
#' @param nudge_x
#' @param nudge_y
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @importFrom purrr %||%
#' @export
#'
#' @examples
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

#' Title
#'
#' @param mapping
#' @param data
#' @param parse
#' @param ...
#' @param box.padding
#' @param label.padding
#' @param point.padding
#' @param label.r
#' @param label.size
#' @param segment.color
#' @param segment.size
#' @param arrow
#' @param force
#' @param max.iter
#' @param nudge_x
#' @param nudge_y
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' @param mapping
#' @param data_directed
#' @param data_bidirected
#' @param size
#' @param curvature
#' @param cap
#' @param arrow_directed
#' @param arrow_bidirected
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param fold
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_dag_edges <- function(mapping = NULL, data_directed = NULL, data_bidirected = NULL,
                           size = .6,
                           curvature = 0.2,
                           cap = ggraph::circle(9, 'mm'),
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


#' Title
#'
#' @param mapping
#' @param data
#' @param position
#' @param arrow
#' @param n
#' @param lineend
#' @param linejoin
#' @param linemitre
#' @param label_colour
#' @param label_alpha
#' @param label_parse
#' @param check_overlap
#' @param angle_calc
#' @param force_flip
#' @param label_dodge
#' @param label_push
#' @param show.legend
#' @param start_cap
#' @param end_cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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



  ggplot2::layer(data = data, mapping = mapping, stat = StatEdgeLink,
        geom = ggraph::GeomEdgePath, position = position, show.legend = show.legend,
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

                     data <- unique(subset(data, !(x == xend & y == yend)))
                     dplyr::mutate(data,
                            x = scale_line(xend, yend, x, y, scale_by)$end_x,
                            y = scale_line(xend, yend, x, y, scale_by)$end_y,
                            xend = scale_line(x, y, xend, yend, scale_by)$end_x,
                            yend = scale_line(x, y, xend, yend, scale_by)$end_y)
                   }
  )

#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param ...
#' @param size
#' @param curvature
#' @param angle
#' @param ncp
#' @param arrow
#' @param lineend
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @export
#'
#' @examples
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

  if (is.null(data)) data <- function(x) dplyr::filter(x, direction == "<->", collider_line)
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

#' Title
#'
#' @param base_size
#' @param base_family
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param expand_x
#' @param expand_y
#' @param breaks
#'
#' @return
#' @export
#'
#' @examples
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
