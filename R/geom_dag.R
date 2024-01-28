#' DAG Nodes
#'
#' `geom_dag_node` and `geom_dag_point` are very similar to
#' [ggplot2::geom_point] but with a few defaults changed. `geom_dag_node` is
#' slightly stylized and includes an internal white circle, while
#' `geom_dag_point` plots a single point.
#'
#' @export
#'
#' @inheritParams ggplot2::geom_point
#'
#' @section Aesthetics: `geom_dag_node` and `geom_dag_point` understand the
#'   following aesthetics (required aesthetics are in bold):
#'
#'   - **x**
#'   - **y**
#'   - alpha
#'   - colour
#'   - fill
#'   - shape
#'   - size
#'   - stroke
#'   - filter
#'
#'   `geom_dag_node` also accepts:
#'
#'   - internal_colour
#'
#' @examples
#' library(ggplot2)
#' g <- dagify(m ~ x + y, y ~ x)
#' p <- g %>%
#'   tidy_dagitty() %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_dag_edges() +
#'   theme_dag()
#'
#' p +
#'   geom_dag_node() +
#'   geom_dag_text()
#'
#' p +
#'   geom_dag_point() +
#'   geom_dag_text()
#' @rdname node_point
#' @name Nodes
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

#' @export
#' @rdname node_point
geom_dag_point <- function(mapping = NULL, data = NULL,
                           position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
    geom = GeomDagPoint,
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
#' @export
#'
#' @examples
#' library(ggplot2)
#' g <- dagify(m ~ x + y, y ~ x)
#' g %>%
#'   tidy_dagitty() %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_dag_point() +
#'   geom_dag_edges() +
#'   geom_dag_text() +
#'   theme_dag()
geom_dag_text <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          parse = FALSE,
                          nudge_x = 0,
                          nudge_y = 0,
                          check_overlap = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
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

#' Node text labels
#'
#' @inheritParams ggplot2::geom_label
#'
#' @section Aesthetics:
#' `geom_dag_label` understand the following aesthetics (required aesthetics are in bold):
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
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggraph)
#' g <- dagify(m ~ x + y, y ~ x)
#'
#' ggdag(g, text = FALSE) + geom_dag_label()
#'
#' g %>%
#'   tidy_dagitty() %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_dag_edges(aes(
#'     start_cap = label_rect(name, padding = margin(2.5, 2.5, 2.5, 2.5, "mm")),
#'     end_cap = label_rect(name, padding = margin(2.5, 2.5, 2.5, 2.5, "mm"))
#'   )) +
#'   geom_dag_label(size = 5, fill = "black", color = "white") +
#'   theme_dag()
geom_dag_label <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           parse = FALSE,
                           nudge_x = 0,
                           nudge_y = 0,
                           check_overlap = FALSE,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
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
    geom = ggplot2::GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' Repulsive textual annotations
#'
#' These functions are minor modifications of those in the `ggrepel`
#' package. geom_dag_text_repel adds text directly to the plot.
#' geom_dag_label_repel draws a rectangle underneath the text, making it easier
#' to read. The text labels repel away from each other and away from the data
#' points.
#'
#' @inheritParams ggrepel::geom_text_repel
#' @inheritParams ggrepel::geom_label_repel
#' @param fontface A character vector. Default is "bold"
#' @param segment.color,segment.size See [ggrepel::geom_text_repel()]
#'
#' @importFrom purrr %||%
#' @export
#'
#' @examples
#' library(ggplot2)
#' g <- dagify(m ~ x + y,
#'   y ~ x,
#'   exposure = "x",
#'   outcome = "y",
#'   latent = "m",
#'   labels = c("x" = "Exposure", "y" = "Outcome", "m" = "Collider")
#' )
#'
#' g %>%
#'   tidy_dagitty() %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_dag_edges() +
#'   geom_dag_point() +
#'   geom_dag_text_repel(aes(label = name), show.legend = FALSE) +
#'   theme_dag()
#'
#' g %>%
#'   tidy_dagitty() %>%
#'   dag_label(labels = c(
#'     "x" = "This is the exposure",
#'     "y" = "Here's the outcome",
#'     "m" = "Here is where they collide"
#'   )) %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_dag_edges() +
#'   geom_dag_point() +
#'   geom_dag_text() +
#'   geom_dag_label_repel(aes(label = label, fill = label),
#'     col = "white", show.legend = FALSE
#'   ) +
#'   theme_dag()
#'
#' @rdname repel
#' @name ggrepel functions
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
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodesRepel,
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
geom_dag_label_repel <- function(mapping = NULL, data = NULL,
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
                                 inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodesRepel,
    geom = ggrepel::GeomLabelRepel,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      box.padding = box.padding,
      label.padding = label.padding,
      point.padding = point.padding,
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

filter_direction <- function(.direction) {
  function(x) {
    x <- dplyr::filter(x, direction == .direction)
    if ("collider_line" %in% names(x)) x <- dplyr::filter(x, !collider_line)

    x
  }
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
#'   value must be a data.frame., and will be used as the layer data.
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
#' @section Aesthetics:
#' `geom_dag_edges` understand the following aesthetics. Bold aesthetics are
#' required.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - start_cap
#' - end_cap
#' - label
#' - label_pos
#' - label_size
#' - angle
#' - hjust
#' - vjust
#' - family
#' - fontface
#' - lineheight
#'
#' `geom_dag_edges` also uses `geom_dag_edges_arc`, which requires the
#' **circular** aesthetic, but this is automatically set.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' dagify(
#'   y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   w1 ~ ~w2
#' ) %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_dag_edges() +
#'   geom_dag_point() +
#'   geom_dag_text() +
#'   theme_dag()
#'
geom_dag_edges <- function(mapping = NULL,
                           data_directed = filter_direction("->"),
                           data_bidirected = filter_direction("<->"),
                           curvature = 0.3,
                           arrow_directed = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
                           arrow_bidirected = grid::arrow(length = grid::unit(5, "pt"), ends = "both", type = "closed"),
                           position = "identity", na.rm = TRUE, show.legend = NA, inherit.aes = TRUE, fold = FALSE,
                           ...) {
  list(
    geom_dag_edges_link(mapping,
                        data = data_directed, arrow = arrow_directed,
                        position = position, na.rm = na.rm,
                        show.legend = show.legend, inherit.aes = inherit.aes,
                        ...
    ),
    geom_dag_edges_arc(mapping,
                       data = data_bidirected, arrow = arrow_bidirected,
                       curvature = curvature, position = position,
                       na.rm = na.rm, show.legend = show.legend,
                       inherit.aes = inherit.aes, fold = fold,
                       ...
    )
  )
}

#' Directed DAG edges
#'
#' @param mapping Set of aesthetic mappings created by aes() or aes_(). If
#'   specified and inherit.aes = TRUE (the default), it is combined with the
#'   default mapping at the top level of the plot. You must supply mapping if
#'   there is no plot mapping.
#' @param data The data to be displayed in this layer.
#'   There are three options: If NULL, the default, the data is inherited from
#'   the plot data as specified in the call to ggplot(). A data.frame, or other
#'   object, will override the plot data. All objects will be fortified to
#'   produce a data frame. See fortify() for which variables will be created. A
#'   function will be called with a single argument, the plot data. The return
#'   value must be a data.frame., and will be used as the layer data.
#' @param curvature The bend of the curve. 1 approximates a halfcircle while 0
#'   will give a straight line. Negative number will change the direction of the
#'   curve. Only used if layout circular = FALSE.
#' @param arrow specification for arrow heads, as created by arrow()
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
#' @section Aesthetics:
#' `geom_dag_edges_link`, `geom_dag_edges_arc`, `geom_dag_edges_diagonal`, and
#' `geom_dag_edges_fan` understand the following aesthetics. Bold aesthetics are
#' required.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - start_cap
#' - end_cap
#' - label
#' - label_pos
#' - label_size
#' - angle
#' - hjust
#' - vjust
#' - family
#' - fontface
#' - lineheight
#'
#' `geom_dag_edges_arc` and `geom_dag_edges_diagonal` also require
#' **circular**, but this is automatically set.
#'
#' `geom_dag_edges_fan` requires **to** and **from**, but these are also
#' automatically set.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- dagify(
#'   y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   L ~ w1 + w2
#' ) %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_dag_point() +
#'   geom_dag_text() +
#'   theme_dag()
#'
#' p + geom_dag_edges_link()
#' p + geom_dag_edges_arc()
#' p + geom_dag_edges_diagonal()
#' p + geom_dag_edges_fan()
#'
#' @rdname geom_dag_edge_functions
#' @name DAG Edges
geom_dag_edges_link <- function(mapping = NULL, data = NULL,
                                arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
                                position = "identity", na.rm = TRUE, show.legend = NA,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    mapping = mapping,
    geom = GeomDAGEdgePath,
    data = data,
    stat = StatEdgeLink,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(arrow = arrow, interpolate = FALSE, na.rm = na.rm, ...)
  )
}

#' @rdname geom_dag_edge_functions
#' @export
geom_dag_edges_arc <- function(mapping = NULL, data = NULL, curvature = .5,
                               arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
                               position = "identity", na.rm = TRUE, show.legend = NA,
                               inherit.aes = TRUE, fold = FALSE,
                               n = 100, lineend = "butt",
                               linejoin = "round", linemitre = 1,
                               label_colour = "black", label_alpha = 1,
                               label_parse = FALSE, check_overlap = FALSE,
                               angle_calc = "rot", force_flip = TRUE,
                               label_dodge = NULL, label_push = NULL, ...) {
  if (is.null(mapping)) {
    mapping <- ggplot2::aes(circular = circular)
  } else if (is.null(mapping$circular)) {
    mapping$circular <- substitute(circular)
  }

  ggplot2::layer(
    mapping = mapping,
    geom = GeomDAGEdgePath,
    data = data,
    stat = StatEdgeArc,
    check.aes = FALSE,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      arrow = arrow, strength = curvature,
      interpolate = FALSE, fold = fold, na.rm = na.rm,
      n = n, lineend = lineend,
      linejoin = linejoin, linemitre = linemitre,
      label_colour = label_colour, label_alpha = label_alpha,
      label_parse = label_parse, check_overlap = check_overlap,
      angle_calc = angle_calc, force_flip = force_flip,
      label_dodge = label_dodge, label_push = label_push, ...
    )
  )
}

#' @inheritParams ggraph::geom_edge_diagonal
#'
#' @rdname geom_dag_edge_functions
#' @export
geom_dag_edges_diagonal <- function(mapping = NULL, data = NULL, position = "identity",
                                    arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
                                    na.rm = TRUE, show.legend = NA, inherit.aes = TRUE,
                                    curvature = 1, n = 100, lineend = "butt",
                                    linejoin = "round", linemitre = 1,
                                    label_colour = "black", label_alpha = 1,
                                    label_parse = FALSE, check_overlap = FALSE,
                                    angle_calc = "rot", force_flip = TRUE,
                                    label_dodge = NULL, label_push = NULL, ...) {
  if (is.null(mapping)) {
    mapping <- ggplot2::aes(circular = circular)
  } else if (is.null(mapping$circular)) {
    mapping$circular <- substitute(circular)
  }

  ggplot2::layer(
    data = data, mapping = mapping, stat = StatEdgeDiagonal,
    geom = GeomDAGEdgePath, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow, na.rm = na.rm, interpolate = FALSE,
      n = n, lineend = lineend, flipped = FALSE, strength = curvature,
      linejoin = linejoin, linemitre = linemitre,
      label_colour = label_colour, label_alpha = label_alpha,
      label_parse = label_parse, check_overlap = check_overlap,
      angle_calc = angle_calc, force_flip = force_flip,
      label_dodge = label_dodge, label_push = label_push, ...
    )
  )
}

#' @inheritParams ggraph::geom_edge_fan
#'
#' @rdname geom_dag_edge_functions
#' @export
geom_dag_edges_fan <- function(mapping = NULL, data = NULL, position = "identity",
                               arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
                               na.rm = TRUE, show.legend = NA, inherit.aes = TRUE,
                               spread = .7, n = 100, lineend = "butt",
                               linejoin = "round", linemitre = 1,
                               label_colour = "black", label_alpha = 1,
                               label_parse = FALSE, check_overlap = FALSE,
                               angle_calc = "rot", force_flip = TRUE,
                               label_dodge = NULL, label_push = NULL, ...) {
  if (is.null(mapping)) {
    mapping <- ggplot2::aes(from = name, to = to)
  } else if (is.null(mapping$from)) {
    mapping$from <- substitute(name)
    mapping$to <- substitute(to)
  }

  ggplot2::layer(
    data = data, mapping = mapping, stat = StatEdgeFan,
    geom = GeomDAGEdgePath, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow, na.rm = na.rm, interpolate = FALSE,
      n = n, lineend = lineend, strength = spread,
      linejoin = linejoin, linemitre = linemitre,
      label_colour = label_colour, label_alpha = label_alpha,
      label_parse = label_parse, check_overlap = check_overlap,
      angle_calc = angle_calc, force_flip = force_flip,
      label_dodge = label_dodge, label_push = label_push, ...
    )
  )
}


#' Edges for paths activated by stratification on colliders
#'
#' Adjusting for a collider activates pathways between the parent of the
#' collider. This geom adds a curved edge between any such parent nodes.
#'
#' @inheritParams ggplot2::geom_curve
#' @param linewidth a numeric vector of length 1. Edge width
#' @param size deprecated. Please use `linewidth`.
#'
#' @export
#'
#' @examples
#' library(dagitty)
#' library(ggplot2)
#' dagify(m ~ a + b, x ~ a, y ~ b) %>%
#'   tidy_dagitty() %>%
#'   control_for("m") %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted)) +
#'   geom_dag_edges() +
#'   geom_dag_collider_edges() +
#'   geom_dag_point() +
#'   geom_dag_text() +
#'   theme_dag() +
#'   scale_adjusted()
geom_dag_collider_edges <- function(mapping = NULL, data = NULL,
                                    stat = "identity", position = "identity",
                                    ...,
                                    linewidth = .6,
                                    size = NULL,
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
  if (!is.null(size)) {
    warning("`size` is deprecated for lines. Please use `linewidth`")
    linewidth <- size
  }

  params <- list(
    arrow = arrow,
    curvature = curvature,
    angle = angle,
    ncp = ncp,
    lineend = lineend,
    na.rm = na.rm,
    ...
  )

  if (ggplot2_version() >= "3.3.6.9000") {
    params$linewidth <- linewidth
  } else {
    params$size <- linewidth
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomCurve,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' Define Aesthetics for Directed Acyclic Graphs (DAGs)
#'
#' `dag_aes()` is a wrapper around `aes()` that specifies `x`, `y`, `xend`, and
#' `yend`, which are required for most DAG visualizations. It merges any
#' additional aesthetics, e.g. `color` or `shape`, with the default aesthetic
#' mappings.
#'
#' @param ... Additional aesthetic mappings passed as arguments. These can
#'   include any aesthetic supported by ggplot2 (e.g., color, size, shape).
#'
#' @return A `ggplot2` aesthetic mapping object that includes both the default
#'   DAG aesthetics and any user-specified aesthetics.
#'
#' @examples
#' library(ggplot2)
#' confounder_triangle() %>%
#'   dag_adjustment_sets() %>%
#'   ggplot(dag_aes(color = adjusted)) +
#'   geom_dag() +
#'   facet_wrap(~ set)
#'
#' @export
dag_aes <- function(...) {
  addtl_aes <- ggplot2::aes(...)
  default_aes <- ggplot2::aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )

  default_aes[names(addtl_aes)] <- addtl_aes

  default_aes
}

#' Add common DAG layers to a ggplot
#'
#' `geom_dag()` is a helper function that adds common DAG layers to a ggplot.
#' The purpose of `geom_dag()` is to simplify making custom DAGs. Most custom
#' DAGs need the same basic layers, and so this function greatly reduces typing.
#' It is not a true geom in that it adds many types of geoms to the plot (by
#' default, edges, nodes, and text). While the underlying layers, all available
#' in ggdag, are true geoms, we usually need a consistent set of layers to make
#' a DAG. `geom_dag()` provides this. Because `geom_dag()` is not a true geom,
#' you'll find that it is awkward for sophisticated customization. When you hit
#' that point, you should use the underlying geoms directly.
#'
#' @param size A numeric value scaling the size of all elements in the DAG. This
#'   allows you to change the scale of the DAG without changing the proportions.
#' @param edge_type The type of edge, one of "link_arc", "link", "arc",
#'   "diagonal".
#' @param node_size The size of the nodes.
#' @param text_size The size of the text.
#' @param label_size The size of the labels.
#' @param text_col The color of the text.
#' @param label_col The color of the labels.
#' @param edge_width The width of the edges.
#' @param edge_cap The size of edge caps (the distance between the arrowheads
#'   and the node borders).
#' @param arrow_length The length of arrows on edges.
#' @param use_edges A logical value. Include a `geom_dag_edges*()` function? If
#'   `TRUE`, which is determined by `edge_type`.
#' @param use_nodes A logical value. Include `geom_dag_point()`?
#' @param use_stylized A logical value. Include `geom_dag_node()`?
#' @param use_text A logical value. Include `geom_dag_text()`?
#' @param use_labels A logical value. Include `geom_dag_label_repel()`?
#' @param label The bare name of a column to use for `geom_dag_label_repel()`.
#'   If `use_labels = TRUE`, the default is to use `label`.
#' @param text The bare name of a column to use for `geom_dag_text()`. If
#'   `use_text = TRUE`, the default is to use `name`.
#' @param node Deprecated.
#' @param stylized Deprecated.
#'
#' @return A list of ggplot2 layer elements
#'
#' @examples
#' # Basic usage with ggdag
#' library(ggplot2)
#' dag <- dagify(y ~ x, z ~ y)
#' ggplot(dag, dag_aes()) + geom_dag()
#' ggplot(dag, dag_aes()) + geom_dag(size = 1.5)
#' ggplot(dag, dag_aes()) + geom_dag(size = 1.5, text_size = 8)
#'
#' @export
geom_dag <- function(size = 1, edge_type = c("link_arc", "link", "arc", "diagonal"),
                     node_size = 16, text_size = 3.88,
                     label_size = text_size,
                     text_col = "white", label_col = "black",
                     edge_width = 0.6, edge_cap = 8, arrow_length = 5,
                     use_edges = TRUE, use_nodes = TRUE, use_stylized = FALSE,
                     use_text = TRUE, use_labels = FALSE, label = NULL,
                     text = NULL, node = deprecated(), stylized = deprecated()) {
  use_nodes <- check_arg_node(node, use_nodes)
  use_stylized <- check_arg_stylized(stylized, use_stylized)

  sizes <- c(
    cap = edge_cap,
    node = node_size,
    text = text_size,
    label = label_size,
    edge = edge_width,
    arrow = arrow_length,
    box_padding = 1.5
  ) * size

  if (isTRUE(use_edges)) {
    edge_type <- match.arg(edge_type)
    if (edge_type == "link_arc") {
      edge_geom <- geom_dag_edges(
        ggplot2::aes(
          start_cap = ggraph::circle(sizes[["cap"]], "mm"),
          end_cap = ggraph::circle(sizes[["cap"]], "mm")
        ),
        edge_width = sizes[["edge"]],
        arrow_directed = grid::arrow(length = grid::unit(sizes[["arrow"]], "pt"), type = "closed"),
        arrow_bidirected = grid::arrow(length = grid::unit(sizes[["arrow"]], "pt"), ends = "both", type = "closed")
      )
    } else {
      edge_function <- edge_type_switch(edge_type)
      edge_geom <- edge_function(
        ggplot2::aes(
          start_cap = ggraph::circle(sizes[["cap"]], "mm"),
          end_cap = ggraph::circle(sizes[["cap"]], "mm")
        ),
        edge_width = sizes[["edge"]],
        arrow = grid::arrow(length = grid::unit(sizes[["arrow"]], "pt"), type = "closed")    )
    }
  } else {
    edge_geom <- NULL
  }

  if (isTRUE(use_nodes)) {
    if (isTRUE(use_stylized)) {
      node_geom <- geom_dag_node(size = sizes[["node"]])
    } else {
      node_geom <- geom_dag_point(size = sizes[["node"]])
    }
  } else {
    node_geom <- NULL
  }

  text <- rlang::enquo(text)

  if (is_quo_logical(text)) {
    deprecate_warn(
      "0.3.0",
      "geom_dag(text = 'no longer accepts logicals')",
      details = paste0(
        "Set `use_text = ", rlang::quo_text(text), "`. ",
        "To use a variable other than node names, set `text = variable_name`"
      ))

    use_text <- as.logical(rlang::quo_text(text))
    text <- NULL
  }

  if (isTRUE(use_text)) {
    text <- rlang::enquo(text)
    if (!rlang::quo_is_null(text)) {
      mapping <- ggplot2::aes(label = !!text)
    } else {
      mapping <- NULL
    }

    text_geom <- geom_dag_text(mapping = mapping, col = text_col, size = text_size)
  } else {
    text_geom <- NULL
  }

  if (is.character(use_labels)) {
    deprecate_warn(
      "0.3.0",
      "geom_dag(use_labels = 'must be a logical')",
      details = paste0(
        "Set `use_labels = TRUE` ",
        "and `label = ", use_labels, "`"
      ))

    label <- rlang::sym(use_labels)
    use_labels <- TRUE
  }

  if (isTRUE(use_labels)) {
    label <- rlang::enquo(label)

    if (rlang::quo_is_null(label)) {
      label <- rlang::quo(label)
    }

    if (rlang::quo_is_symbol(label)) {
      label <- rlang::get_expr(label)
    }

    label_geom <- geom_dag_label_repel(
      ggplot2::aes(label = !!label),
      size = sizes[["label"]] * 1.1,
      col = label_col,
      show.legend = FALSE,
      box.padding = sizes[["box_padding"]],
      max.overlaps = Inf,
      label.padding = 0.1,
    )
  } else {
    label_geom <- NULL
  }

  list(
    edge_geom,
    node_geom,
    text_geom,
    label_geom
  )
}

is_quo_logical <- function(x) {
  rlang::quo_text(x) == "TRUE" || rlang::quo_text(x) == "FALSE"
}

#' Create a new ggplot
#'
#' @inheritParams ggplot2::ggplot
#' @export
#' @rdname ggplot.tidy_dagitty
#' @importFrom ggplot2 ggplot aes
ggplot.tidy_dagitty <- function(data = NULL, mapping = aes(), ...) {
  p <- ggplot2::ggplot(fortify(data), mapping = mapping, ...)

  p <- silence_scales(p)

  p + expand_plot(
    expand_x = expansion(c(.10, .10)),
    expand_y = expansion(c(.10, .10))
  )
}

#' @rdname ggplot.tidy_dagitty
#' @export
ggplot.dagitty <- ggplot.tidy_dagitty
