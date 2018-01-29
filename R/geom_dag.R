#' DAG Nodes
#'
#' @return
#' @export
#'
#' @inheritParams ggplot2::geom_point
#'
#' @section Aesthetics:
#' `geom_dag_node` understand the following aesthetics (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - alpha
#' - colour
#' - internal_colour
#' - fill
#' - shape
#' - size
#' - stroke
#' - filter
#'
#' @examples
#' g <- dagify(m ~ x + y, y ~ x)
#' g %>%
#' tidy_dagitty() %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'     geom_dag_node() +
#'     geom_dag_edges() +
#'     geom_dag_text(col = "black") +
#'     theme_dag() +
#'     scale_dag()
geom_dag_node <- function(mapping = NULL, data = NULL,
                          position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
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


#' Node text
#'
#' @inheritParams ggplot2::geom_text
#'
#' @section Aesthetics:
#' `geom_dag_text` understand the following aesthetics (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - **label**
#' - alpha
#' - angle
#' - colour
#' - family
#' - fontface
#' - group
#' - hjust
#' - lineheight
#' - size
#' - vjust
#'
#' @return
#' @export
#'
#' @examples
#' g <- dagify(m ~ x + y, y ~ x)
#' g %>%
#' tidy_dagitty() %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'     geom_dag_node() +
#'     geom_dag_edges() +
#'     geom_dag_text() +
#'     theme_dag() +
#'     scale_dag()
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

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
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


#' Repulsive textual annotations
#'
#' These functions are minor modifications of thos in the \code{ggrepel}
#' package. geom_dag_text_repel adds text directly to the plot.
#' geom_dag_label_repel draws a rectangle underneath the text, making it easier
#' to read. The text labels repel away from each other and away from the data
#' points.
#'
#' @inheritParams ggrepel::geom_text_repel
#' @inheritParams ggrepel::geom_label_repel
#'
#' @return
#' @importFrom purrr %||%
#' @export
#'
#' @examples
#' g <- dagify(m ~ x + y,
#' y ~ x,
#' exposure = "x",
#' outcome = "y",
#' latent = "m",
#' labels = c("x" = "Exposure", "y" = "Outcome", "m" = "Collider"))
#'
#' g %>% tidy_dagitty() %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'     geom_dag_edges() +
#'     geom_dag_node() +
#'     geom_dag_text_repel(aes(label = name), show.legend = FALSE) +
#'     theme_dag() +
#'     scale_dag()
#'
#' g %>% tidy_dagitty() %>%
#'   dag_label(labels = c("x" = "This is the exposure",
#'     "y" = "Here's the outcome",
#'     "m" = "Here is where they collide")) %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'     geom_dag_edges() +
#'     geom_dag_node() +
#'     geom_dag_text() +
#'     geom_dag_label_repel(aes(label = label, fill = label),
#'       col = "white", show.legend = FALSE) +
#'     theme_dag() +
#'     scale_dag()
#' @rdname repel
#' @name repel
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
                 stat = StatNodes,
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

#' @rdname repel
#' @export
#'
#' @importFrom purrr %||%
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
    stat = StatNodes,
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


#' Directed and bidirected DAG edges
#'
#' @param mapping Set of aesthetic mappings created by aes() or aes_(). If
#'   specified and inherit.aes = TRUE (the default), it is combined with the
#'   default mapping at the top level of the plot. You must supply mapping if
#'   there is no plot mapping.
#' @param data_directed,data_bidirected The data to be displayed in this layer.
#'   There are three options: If NULL, the default, the data is inherited from
#'   the plot data as specified in the call to ggplot(). A data.frame, or other
#'   object, will override the plot data. All objects will be fortified to
#'   produce a data frame. See fortify() for which variables will be created. A
#'   function will be called with a single argument, the plot data. The return
#'   value must be a data.frame., and will be used as the layer data.#'
#' @param curvature The bend of the curve. 1 approximates a halfcircle while 0
#'   will give a straight line. Negative number will change the direction of the
#'   curve. Only used if layout circular = FALSE.
#' @param arrow_directed,arrow_bidirected specification for arrow heads, as
#'   created by arrow()
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param na.rm If FALSE (the default), removes missing values with a warning.
#'   If TRUE silently removes missing values
#' @param show.legend logical. Should this layer be included in the legends? NA,
#'   the default, includes if any aesthetics are mapped. FALSE never includes,
#'   and TRUE always includes. It can also be a named logical vector to finely
#'   select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#'   combining with them. This is most useful for helper functions that define
#'   both data and aesthetics and shouldn't inherit behaviour from the default
#'   plot specification, e.g. borders().
#' @param fold Logical. Should arcs appear on the same side of the nodes despite
#'   different directions. Default to FALSE.
#' @param ... Other arguments passed to ggraph::geom_edge_*()
#'
#' @return
#' @export
#'
#' @examples
#' dagify(y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   w1 ~~ w2) %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'     geom_dag_edges() +
#'     geom_dag_node() +
#'     geom_dag_text() +
#'     theme_dag() +
#'     scale_dag()
geom_dag_edges <- function(mapping = NULL, data_directed = NULL, data_bidirected = NULL,
                           curvature = 0.2,
                           arrow_directed = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
                           arrow_bidirected = grid::arrow(length = grid::unit(5, "pt"), ends = "both", type = "closed"),
                           position = "identity", na.rm = TRUE, show.legend = NA, inherit.aes = TRUE, fold = FALSE,
                           ...) {

  if (is.null(mapping)) {
    mapping <- ggplot2::aes(direction = direction)
  } else if (is.null(mapping$direction)){
    mapping$direction <- substitute(direction)
  }
  arc_mapping <- ggplot2::aes(circular = circular)
  if (!is.null(mapping)) arc_mapping[names(mapping)] <- mapping

  list(
    ggplot2::layer(mapping = mapping,
                   geom = GeomDAGEdgePath,
                   data = data_directed,
                   stat = StatEdgeLink,
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                   check.aes = FALSE,
                   params = list(arrow = arrow_directed, interpolate = FALSE, na.rm = na.rm, direction_type = "->", ...)),
    ggplot2::layer(mapping = arc_mapping,
                   geom = GeomDAGEdgePath,
                   data =  data_bidirected,
                   stat = StatEdgeArc,
                   check.aes = FALSE,
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                   params = list(arrow = arrow_bidirected, curvature = curvature,
                                 interpolate = FALSE, fold = fold, na.rm = na.rm,
                                 n = 100, lineend = "butt",
                                 linejoin = "round", linemitre = 1,
                                 label_colour = 'black',  label_alpha = 1,
                                 label_parse = FALSE, check_overlap = FALSE,
                                 angle_calc = 'rot', force_flip = TRUE,
                                 label_dodge = NULL, label_push = NULL, direction_type = "<->", ...))
  )
}


# geom_dag_edges_link <- function(mapping = NULL, data = NULL,
#                                 position = "identity", arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed"), n = 100,
#                                 lineend = "butt", linejoin = "round", linemitre = 1,
#                                 label_colour = 'black',  label_alpha = 1,
#                                 label_parse = FALSE, check_overlap = FALSE,
#                                 angle_calc = 'rot', force_flip = TRUE,
#                                 label_dodge = NULL, label_push = NULL,
#                                 show.legend = NA,
#                                 ...) {
#
#   ggplot2::layer(data = data, mapping = mapping, stat = StatEdgeLink,
#                  geom = GeomDAGEdgePath, position = position, show.legend = show.legend,
#                  inherit.aes = TRUE,
#                  params =
#                    list(arrow = arrow, lineend = lineend, linejoin = linejoin,
#                         linemitre = linemitre, na.rm = FALSE, n = n,
#                         interpolate = FALSE,
#                         label_colour = label_colour, label_alpha = label_alpha,
#                         label_parse = label_parse, check_overlap = check_overlap,
#                         angle_calc = angle_calc, force_flip = force_flip,
#                         label_dodge = label_dodge, label_push = label_push,
#                         ...)
#
#   )
# }


#' Edges for paths activated by stratification on colliders
#'
#' @inheritParams ggplot2::geom_curve
#'
#' @export
#'
#' @examples
#' library(dagitty)
#'
#' dagify(m ~ a + b, x ~ a, y ~ b) %>%
#'   tidy_dagitty() %>%
#'   control_for("m") %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted)) +
#'     geom_dag_edges() +
#'     geom_dag_collider_edges() +
#'     geom_dag_node() +
#'     geom_dag_text() +
#'     theme_dag() +
#'     scale_dag()

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

#' A minimalist DAG theme
#'
#' @inheritParams ggplot2::theme_minimal
#' @param ... additional arguments passed to \code{theme()}
#'
#' @return
#' @export
#'
#' @examples
theme_dag_blank <- function(base_size = 12, base_family = "", ...) {
  list(
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family),
    ggplot2::theme(strip.background = ggplot2::element_rect(color = "grey85", fill = "grey85"),
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   ...)
  )
}

#' Simple grey theme for DAGs
#'
#' @inheritParams ggplot2::theme_grey
#' @param ... additional arguments passed to \code{theme()}
#'
#' @return
#' @export
#'
#' @rdname theme_dag_grey
#' @rdname theme_dag_grey
theme_dag_grey <- function(base_size = 12, base_family = "", ...) {
  list(
    ggplot2::theme_grey(base_size = base_size, base_family = base_family),
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = "grey92"),
                   panel.grid.minor = ggplot2::element_line(colour = "grey92"),
                   ...)
  )
}

#' @rdname theme_dag_grey
#' @export
theme_dag_gray <- theme_dag_grey

#' @rdname theme_dag_grey
#' @export
theme_dag <- theme_dag_grey

#' Common scale adjustments for DAGs
#'
#' @param expand_x,expand_y Vector of range expansion constants used to add some
#'   padding around the data, to ensure that they are placed some distance away
#'   from the axes. Use the convenience function \code{expand_scale()} to
#'   generate the values for the expand argument.
#' @param breaks One of:
#' - NULL for no breaks
#' - waiver() for the default breaks computed by the transformation object
#' - A numeric vector of positions
#' - A function that takes the limits as input and returns breaks as output

#'
#' @return
#' @export
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
