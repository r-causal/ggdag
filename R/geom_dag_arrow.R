# Caching environment for lazy ggproto objects.
# ggarrow is in Suggests, so ggproto classes that inherit from it
# can only be created after ggarrow is installed and loaded.
the <- new.env(parent = emptyenv())

# Stat: filter tidy_dagitty data to edge rows --------------------------------

StatDAGArrowEdges <- ggplot2::ggproto(
  "StatDAGArrowEdges",
  ggplot2::Stat,
  compute_panel = function(data, scales) {
    data[!is.na(data$xend), , drop = FALSE]
  },
  required_aes = c("x", "y", "xend", "yend"),
  optional_aes = "edge_curvature"
)

# Stat: draw routed edge waypoints, one path per edge ------------------------

# The layer's data function has already turned the edge rows into waypoint
# long format with `route_dag_edges()`, so the stat only settles the drawing
# order: each edge is one group, from the discrete `edge_id` aesthetic, and
# its waypoints are drawn in `seq` order.
StatDAGRoutedEdge <- ggplot2::ggproto(
  "StatDAGRoutedEdge",
  ggplot2::Stat,
  compute_panel = function(data, scales) {
    data[order(data$group, data$seq), , drop = FALSE]
  },
  required_aes = c("x", "y", "edge_id", "seq")
)

# Lazy ggproto factories -----------------------------------------------------

geom_dag_arrow_geom <- function() {
  if (is.null(the$GeomDAGArrow)) {
    the$GeomDAGArrow <- ggplot2::ggproto(
      "GeomDAGArrow",
      ggarrow::GeomArrowSegment,
      default_aes = ggplot2::aes(
        colour = "black",
        linewidth = 1,
        linewidth_head = NULL,
        linewidth_fins = NULL,
        linetype = 1,
        alpha = NA,
        arrow_head = NULL,
        arrow_fins = NULL,
        arrow_mid = NULL,
        resect_head = NULL,
        resect_fins = NULL,
        stroke_colour = NA,
        stroke_width = 0.25
      ),
      draw_panel = function(
        self,
        data,
        panel_params,
        coord,
        linejoin = "round",
        linemitre = 10,
        lineend = "butt",
        na.rm = FALSE,
        arrow = list(
          head = ggarrow::arrow_head_wings(),
          fins = NULL,
          mid = NULL
        ),
        length = list(head = 4, fins = 4, mid = 4),
        justify = 0,
        force_arrow = FALSE,
        mid_place = 0.5,
        resect = list(head = NULL, fins = NULL),
        sep = 0
      ) {
        resect <- inject_dag_resect(resect, data)
        ggplot2::ggproto_parent(ggarrow::GeomArrowSegment, self)$draw_panel(
          data = data,
          panel_params = panel_params,
          coord = coord,
          linejoin = linejoin,
          linemitre = linemitre,
          lineend = lineend,
          na.rm = na.rm,
          arrow = arrow,
          length = length,
          justify = justify,
          force_arrow = force_arrow,
          mid_place = mid_place,
          resect = resect,
          sep = sep
        )
      },
      draw_key = ggarrow::draw_key_arrow
    )
  }
  the$GeomDAGArrow
}

geom_dag_arrow_curve_geom <- function() {
  if (is.null(the$GeomDAGArrowCurve)) {
    the$GeomDAGArrowCurve <- ggplot2::ggproto(
      "GeomDAGArrowCurve",
      ggarrow::GeomArrowCurve,
      default_aes = ggplot2::aes(
        colour = "black",
        linewidth = 1,
        linewidth_head = NULL,
        linewidth_fins = NULL,
        linetype = 1,
        alpha = NA,
        arrow_head = NULL,
        arrow_fins = NULL,
        arrow_mid = NULL,
        resect_head = NULL,
        resect_fins = NULL,
        stroke_colour = NA,
        stroke_width = 0.25
      ),
      draw_panel = function(
        self,
        data,
        panel_params,
        coord,
        linejoin = "round",
        linemitre = 10,
        lineend = "butt",
        na.rm = FALSE,
        arrow = list(
          head = ggarrow::arrow_head_wings(),
          fins = NULL,
          mid = NULL
        ),
        length = list(head = 4, fins = 4, mid = 4),
        justify = 0,
        force_arrow = FALSE,
        mid_place = 0.5,
        resect = list(head = NULL, fins = NULL),
        curvature = 0.5,
        angle = 90,
        ncp = 5,
        sep = 0
      ) {
        resect <- inject_dag_resect(resect, data)

        draw_parent <- function(data, curvature) {
          ggplot2::ggproto_parent(ggarrow::GeomArrowCurve, self)$draw_panel(
            data = data,
            panel_params = panel_params,
            coord = coord,
            linejoin = linejoin,
            linemitre = linemitre,
            lineend = lineend,
            na.rm = na.rm,
            arrow = arrow,
            length = length,
            justify = justify,
            force_arrow = force_arrow,
            mid_place = mid_place,
            resect = resect,
            curvature = curvature,
            angle = angle,
            ncp = ncp,
            sep = sep
          )
        }

        has_edge_curvature <- "edge_curvature" %in%
          names(data) &&
          !all(is.na(data$edge_curvature))

        if (!has_edge_curvature) {
          return(draw_parent(data, curvature))
        }

        if (!is.numeric(data$edge_curvature)) {
          abort(
            "{.field edge_curvature} must be numeric, not {.cls {class(data$edge_curvature)}}.",
            error_class = "ggdag_type_error"
          )
        }

        # Replace NA edge_curvature with scalar fallback
        data$edge_curvature[is.na(data$edge_curvature)] <- curvature

        # Split by curvature value and render each group separately
        curvature_groups <- split(data, data$edge_curvature)
        grobs <- lapply(curvature_groups, function(group_data) {
          group_curvature <- group_data$edge_curvature[1]
          group_data$edge_curvature <- NULL
          draw_parent(group_data, group_curvature)
        })

        grid::gTree(children = do.call(grid::gList, grobs))
      },
      draw_key = ggarrow::draw_key_arrow
    )
  }
  the$GeomDAGArrowCurve
}

geom_dag_routed_arrow_geom <- function() {
  if (is.null(the$GeomDAGRoutedArrow)) {
    the$GeomDAGRoutedArrow <- ggplot2::ggproto(
      "GeomDAGRoutedArrow",
      ggarrow::GeomArrow,
      default_aes = ggplot2::aes(
        colour = "black",
        linewidth = 1,
        linewidth_head = NULL,
        linewidth_fins = NULL,
        linetype = 1,
        alpha = NA,
        arrow_head = NULL,
        arrow_fins = NULL,
        arrow_mid = NULL,
        resect_head = NULL,
        resect_fins = NULL,
        stroke_colour = NA,
        stroke_width = 0.25
      ),
      draw_panel = function(
        self,
        data,
        panel_params,
        coord,
        linejoin = "round",
        linemitre = 10,
        lineend = "butt",
        na.rm = FALSE,
        arrow = list(
          head = ggarrow::arrow_head_wings(),
          fins = NULL,
          mid = NULL
        ),
        length = list(head = 4, fins = 4, mid = 4),
        justify = 0,
        force_arrow = FALSE,
        mid_place = 0.5,
        resect = list(head = NULL, fins = NULL),
        sep = 0
      ) {
        resect <- inject_dag_resect(resect, data)
        ggplot2::ggproto_parent(ggarrow::GeomArrow, self)$draw_panel(
          data = data,
          panel_params = panel_params,
          coord = coord,
          linejoin = linejoin,
          linemitre = linemitre,
          lineend = lineend,
          na.rm = na.rm,
          arrow = arrow,
          length = length,
          justify = justify,
          force_arrow = force_arrow,
          mid_place = mid_place,
          resect = resect,
          sep = sep
        )
      },
      draw_key = ggarrow::draw_key_arrow
    )
  }
  the$GeomDAGRoutedArrow
}

# Helper: inject DAG resection defaults ---------------------------------------

inject_dag_resect <- function(resect, data) {
  edge_cap <- ggdag_option("edge_cap", 8)
  if (is.null(resect$head) && is.null(data$resect_head)) {
    resect$head <- edge_cap
  }
  if (is.null(resect$fins) && is.null(data$resect_fins)) {
    resect$fins <- edge_cap
  }

  # ggarrow measures whatever it is handed, so an end still unset here (its
  # value comes from the `resect_head`/`resect_fins` aesthetic instead) has to
  # arrive as a number rather than as `NULL`.
  list(head = resect$head %||% 0, fins = resect$fins %||% 0)
}

# Layer wrapper: discover node size at add time --------------------------------

dag_arrow_layer <- function(layer) {
  structure(
    list(layer = layer),
    class = "dag_arrow_layer"
  )
}

#' @export
`$.dag_arrow_layer` <- function(x, name) {
  if (name == "layer") {
    .subset2(x, "layer")
  } else {
    .subset2(x, "layer")[[name]]
  }
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.dag_arrow_layer <- function(object, plot, ...) {
  layer <- clone_layer(.subset2(object, "layer"))
  resect <- layer$geom_params$resect

  needs_resect <- c("head", "fins")[
    c(is.null(resect$head), is.null(resect$fins))
  ]

  if (length(needs_resect) > 0) {
    discovered <- discover_node_size(plot)
    if (!is.null(discovered)) {
      cap_mm <- node_size_to_cap(discovered)
      for (end in needs_resect) {
        layer$geom_params$resect[[end]] <- cap_mm
      }
      needs_resect <- character()
    }
  }

  if (length(needs_resect) > 0) {
    # No node layer is on the plot yet, which is the order the layer-by-layer
    # examples use. A node layer added after this one is in view once the plot
    # is built, so the resection is settled there instead.
    layer <- plot_aware_layer(layer, function(self, plot) {
      discovered <- discover_node_size(plot)
      cap_mm <- if (is.null(discovered)) {
        NULL
      } else {
        node_size_to_cap(discovered)
      }
      resect <- self$geom_params$resect
      for (end in needs_resect) {
        resect[[end]] <- cap_mm
      }
      self$geom_params$resect <- resect
    })
  }

  ggplot2::ggplot_add(layer, plot, ...)
}

# Constructor: geom_dag_arrow() -----------------------------------------------

#' Directed DAG edges using ggarrow
#'
#' These geoms draw DAG edges using the ggarrow package for rendering,
#' providing richer arrow styling than the default ggraph-based edge geoms.
#' `geom_dag_arrow()` draws straight directed edges,
#' `geom_dag_arrow_arc()` draws curved edges (typically for bidirected
#' relationships), and `geom_dag_arrows()` is a convenience wrapper that
#' draws both directed and bidirected edges.
#'
#' These geoms require the ggarrow package to be installed. Unlike the
#' ggraph-based edge geoms, these use ggarrow's native parameter names
#' (`resect_head`/`resect_fins` instead of `start_cap`/`end_cap`,
#' `arrow_head`/`arrow_fins` instead of `arrow`).
#'
#' ## Per-edge curvature
#'
#' `geom_dag_arrow_arc()` supports per-edge curvature via the `edge_curvature`
#' aesthetic. Map a numeric column to `aes(edge_curvature = ...)` to give each
#' edge its own curvature value. Edges with `edge_curvature = 0` are drawn as
#' straight lines; positive values curve right, negative values curve left.
#' Any `NA` values fall back to the scalar `curvature` parameter. This is
#' useful in time-ordered DAGs where some edges need to curve around
#' intermediate nodes while adjacent edges stay straight.
#'
#' ## Auto-resection
#'
#' Edges are automatically shortened so that they do not run underneath the
#' nodes. Resection is decided one end at a time: an end you set, through
#' `resect` or through `resect_head`/`resect_fins`, keeps the value you gave
#' it, and every end you leave unset is shortened automatically. Setting only
#' `resect_head`, for instance, leaves the fins end to the automatic value.
#' Pass `0` to an end to draw the edge all the way to the node.
#'
#' The automatic value comes from the node size when the plot has a node layer
#' (`geom_dag_point()` or `geom_dag_node()`), whichever order the two layers
#' were added in, and from the `ggdag.edge_cap` option (default: 8mm) when the
#' plot has none.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. If
#'   specified and `inherit.aes = TRUE` (the default), it is combined with the
#'   default mapping at the top level of the plot.
#' @param data The data to be displayed in this layer. There are three options:
#'   If `NULL`, the default, the data is inherited from the plot data as
#'   specified in the call to [ggplot2::ggplot()]. A `data.frame`, or other
#'   object, will override the plot data. A function will be called with a
#'   single argument, the plot data. The return value must be a `data.frame`,
#'   and will be used as the layer data.
#' @param arrow_head,arrow_fins,arrow_mid Arrow ornament functions from
#'   ggarrow (e.g., `ggarrow::arrow_head_wings()`,
#'   `ggarrow::arrow_head_line()`). Set to `NULL` to suppress an ornament.
#' @param length,length_head,length_fins,length_mid Size of arrow ornaments.
#'   A numeric value sets the size relative to `linewidth`; a
#'   [grid::unit()] sets an absolute size.
#' @param justify A numeric value between 0 and 1 controlling where the arrow
#'   is drawn relative to the path endpoints. 0 (default) places the tip at
#'   the endpoint; 1 places the base at the endpoint.
#' @param force_arrow If `TRUE`, draw arrows even when the path is shorter
#'   than the arrow ornaments. Default `FALSE`.
#' @param mid_place Numeric vector with values between 0 and 1 setting
#'   positions for interior arrows, or a [grid::unit()] for spacing.
#' @param resect A numeric value in millimetres to shorten the arrow from both
#'   ends, or `NULL` (the default) to leave both ends to auto-resection.
#'   Overridden by `resect_head`/`resect_fins` if set. `0` is a value like any
#'   other: it turns auto-resection off and draws the edge up to the node.
#' @param resect_head,resect_fins Numeric values in millimetres to shorten the
#'   arrow from the head or fins end respectively, or `NULL` (the default) to
#'   leave that end to auto-resection.
#' @param lineend Line end style: `"butt"` (default), `"round"`, or
#'   `"square"`.
#' @param linejoin Line join style: `"round"` (default), `"mitre"`, or
#'   `"bevel"`.
#' @param linemitre Line mitre limit (default 10).
#' @param position Position adjustment, either as a string or the result of a
#'   call to a position adjustment function.
#' @param na.rm If `FALSE`, removes missing values with a warning. If `TRUE`
#'   (the default for DAG geoms), silently removes missing values.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than
#'   combining with them.
#' @param ... Other arguments passed on to the layer.
#'
#' @return A [ggplot2::layer()] object that can be added to a plot.
#'
#' @examples
#' library(ggplot2)
#' p <- dagify(
#'   y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   w1 ~ ~w2
#' ) |>
#'   ggplot(aes(
#'     x = .data$x, y = .data$y,
#'     xend = .data$xend, yend = .data$yend
#'   ))
#'
#' # Straight directed edges
#' p + geom_dag_arrow() + geom_dag_point() + geom_dag_text() + theme_dag()
#'
#' # Both directed and bidirected edges
#' p + geom_dag_arrows() + geom_dag_point() + geom_dag_text() + theme_dag()
#'
#' # Custom arrow ornaments
#' p +
#'   geom_dag_arrow(arrow_head = ggarrow::arrow_head_line()) +
#'   geom_dag_point() +
#'   geom_dag_text() +
#'   theme_dag()
#'
#' # Per-edge curvature: curve long-span edges around intermediate nodes
#' time_dag <- dagify(
#'   y ~ x + m,
#'   m ~ x + c,
#'   x ~ c,
#'   coords = time_ordered_coords(force_y = FALSE)
#' )
#'
#' add_curvature <- function(x) {
#'   x <- dplyr::filter(x, !is.na(.data$xend))
#'   span <- abs(x$x - x$xend)
#'   x$edge_curvature <- ifelse(span > min(span) + 0.01, 0.5, 0)
#'   x
#' }
#'
#' time_dag |>
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_dag_arrow_arc(
#'     aes(edge_curvature = edge_curvature),
#'     data = add_curvature,
#'     arrow_fins = NULL
#'   ) +
#'   geom_dag_point() +
#'   geom_dag_text() +
#'   theme_dag()
#'
#' @export
#' @rdname geom_dag_arrow
geom_dag_arrow <- function(
  mapping = NULL,
  data = NULL,
  arrow_head = ggarrow::arrow_head_wings(),
  arrow_fins = NULL,

  arrow_mid = NULL,
  length = 4,
  length_head = NULL,
  length_fins = NULL,
  length_mid = NULL,
  justify = 0,
  force_arrow = FALSE,
  mid_place = 0.5,
  resect = NULL,
  resect_head = NULL,
  resect_fins = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  rlang::check_installed("ggarrow", reason = "to use `geom_dag_arrow()`.")

  resect_head <- resect_head %||% resect
  resect_fins <- resect_fins %||% resect

  length <- list(
    head = length_head %||% length,
    fins = length_fins %||% length,
    mid = length_mid %||% length
  )

  dag_arrow_layer(ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatDAGArrowEdges,
    geom = geom_dag_arrow_geom(),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      arrow = list(head = arrow_head, fins = arrow_fins, mid = arrow_mid),
      length = length,
      justify = justify,
      force_arrow = force_arrow,
      mid_place = mid_place,
      resect = list(head = resect_head, fins = resect_fins),
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      ...
    )
  ))
}

# Constructor: geom_dag_arrow_arc() -------------------------------------------

#' @param curvature A numeric value giving the amount of curvature. Negative
#'   values produce left-hand curves, positive values produce right-hand curves,
#'   and zero produces a straight line.
#' @param angle A numeric value between 0 and 180, giving an amount to skew the
#'   control points of the curve.
#' @param ncp The number of control points used to draw the curve. More control
#'   points creates a smoother curve.
#'
#' @export
#' @rdname geom_dag_arrow
geom_dag_arrow_arc <- function(
  mapping = NULL,
  data = NULL,
  curvature = 0.3,
  angle = 90,
  ncp = 5,
  arrow_head = ggarrow::arrow_head_wings(),
  arrow_fins = NULL,
  arrow_mid = NULL,
  length = 4,
  length_head = NULL,
  length_fins = NULL,
  length_mid = NULL,
  justify = 0,
  force_arrow = FALSE,
  mid_place = 0.5,
  resect = NULL,
  resect_head = NULL,
  resect_fins = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  rlang::check_installed("ggarrow", reason = "to use `geom_dag_arrow_arc()`.")

  resect_head <- resect_head %||% resect
  resect_fins <- resect_fins %||% resect

  length <- list(
    head = length_head %||% length,
    fins = length_fins %||% length,
    mid = length_mid %||% length
  )

  dag_arrow_layer(ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatDAGArrowEdges,
    geom = geom_dag_arrow_curve_geom(),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      arrow = list(head = arrow_head, fins = arrow_fins, mid = arrow_mid),
      length = length,
      justify = justify,
      force_arrow = force_arrow,
      mid_place = mid_place,
      resect = list(head = resect_head, fins = resect_fins),
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      ...
    )
  ))
}

# Constructor: geom_dag_arrows() ----------------------------------------------

#' @param data_directed,data_bidirected The data to be displayed for directed
#'   and bidirected edges respectively. By default, these filter the plot data
#'   by edge direction.
#'
#' @export
#' @rdname geom_dag_arrow
geom_dag_arrows <- function(
  mapping = NULL,
  data_directed = filter_direction("->"),
  data_bidirected = filter_direction("<->"),
  curvature = 0.3,
  arrow_head = ggarrow::arrow_head_wings(),
  arrow_fins = NULL,
  arrow_mid = NULL,
  resect = NULL,
  resect_head = NULL,
  resect_fins = NULL,
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  rlang::check_installed("ggarrow", reason = "to use `geom_dag_arrows()`.")

  # Bidirected edges default to wings on both ends
  bidirected_fins <- arrow_fins %||% ggarrow::arrow_head_wings()

  list(
    geom_dag_arrow(
      mapping = mapping,
      data = data_directed,
      arrow_head = arrow_head,
      arrow_fins = arrow_fins,
      arrow_mid = arrow_mid,
      resect = resect,
      resect_head = resect_head,
      resect_fins = resect_fins,
      position = position,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    ),
    geom_dag_arrow_arc(
      mapping = mapping,
      data = data_bidirected,
      curvature = curvature,
      arrow_head = arrow_head,
      arrow_fins = bidirected_fins,
      arrow_mid = arrow_mid,
      resect = resect,
      resect_head = resect_head,
      resect_fins = resect_fins,
      position = position,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    )
  )
}

# Constructor: geom_dag_routed_arrows() ----------------------------------------

# Layer data for the routed edge layer: the directed edge rows, routed into
# waypoint long format. The obstacle node positions come from the plot data,
# so the routed paths clear every drawn node, not only the ones the edge rows
# mention.
routed_waypoint_data <- function(data_directed, node_radius) {
  force(data_directed)
  force(node_radius)
  function(plot_data) {
    if (inherits(plot_data, "tidy_dagitty")) {
      plot_data <- pull_dag_data(plot_data)
    }
    edges <- if (is.function(data_directed)) {
      data_directed(plot_data)
    } else {
      data_directed %||% plot_data
    }
    route_dag_edges(edges, plot_data, node_radius)
  }
}

# The routed edge layer itself. The waypoint frame has no `xend`/`yend`
# columns, so a plot-level `aes_dag()` mapping cannot evaluate on it: the
# layer maps its own waypoint columns and never inherits, and an
# `edge_curvature` column on the plot data is read by name in
# `route_dag_edges()` instead of through the mapping.
dag_routed_arrow_layer <- function(
  data_directed,
  node_radius,
  arrow_head,
  arrow_fins,
  arrow_mid,
  length,
  justify,
  force_arrow,
  mid_place,
  resect_head,
  resect_fins,
  lineend,
  linejoin,
  linemitre,
  position,
  na.rm,
  show.legend,
  ...
) {
  dag_arrow_layer(ggplot2::layer(
    data = routed_waypoint_data(data_directed, node_radius),
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      edge_id = .data$edge_id,
      seq = .data$seq
    ),
    stat = StatDAGRoutedEdge,
    geom = geom_dag_routed_arrow_geom(),
    position = position,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = rlang::list2(
      arrow = list(head = arrow_head, fins = arrow_fins, mid = arrow_mid),
      length = length,
      justify = justify,
      force_arrow = force_arrow,
      mid_place = mid_place,
      resect = list(head = resect_head, fins = resect_fins),
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      ...
    )
  ))
}

#' Routed DAG edges that detour around nodes
#'
#' `geom_dag_routed_arrows()` draws DAG edges with the ggarrow engine,
#' routing every directed edge whose straight path a node blocks around that
#' node. Each blocked edge takes the shortest path through a visibility graph
#' built over the tangent points of the obstacle circles, expanded to
#' 1.5 node radii so the drawn path keeps a clearance margin of half a node
#' radius, and the corners are smoothed with two passes of Chaikin corner
#' cutting. Unblocked edges stay straight, bidirected edges are drawn as arcs
#' by the same curve geom [geom_dag_arrows()] uses, and the routing is
#' deterministic: the same DAG always draws the same paths.
#'
#' Curvature the user set is never rerouted. When the data carries an
#' `edge_curvature` column, from [curved()], [curve_edge()], or your own
#' code, an edge with a numeric curvature follows that arc, an explicit 0
#' stays straight through whatever sits on its chord, and only edges whose
#' curvature is unset (`NA`) are candidates for routing.
#'
#' The routed layer computes its waypoints from the layer data, so it does
#' not inherit the plot's aesthetic mapping; the `mapping` argument applies
#' to the bidirected arc layer. Edges are resected to the plot's node size
#' exactly as in [geom_dag_arrow()].
#'
#' @inheritParams geom_dag_arrow
#' @inheritParams geom_dag_arrow_arc
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()],
#'   applied to the bidirected arc layer.
#' @param data_directed,data_bidirected The data to be displayed for directed
#'   and bidirected edges respectively. By default, these filter the plot
#'   data by edge direction.
#' @param node_radius Drawn node radius in data units, the scale on which the
#'   routing works: an edge is blocked when a node sits strictly within
#'   1.5 node radii of its straight path, and the routed path clears the
#'   obstacle by the same expanded radius. Defaults to the radius the
#'   default-size node is drawn at.
#'
#' @return A list of [ggplot2::layer()] objects that can be added to a plot.
#'
#' @examples
#' library(ggplot2)
#' dag <- dagify(
#'   y ~ x + m,
#'   m ~ x,
#'   coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 0, y = 0))
#' )
#'
#' # the x -> y edge detours around the mediator sitting on its path
#' dag |>
#'   ggplot(aes_dag()) +
#'   geom_dag_routed_arrows() +
#'   geom_dag_point() +
#'   geom_dag_text() +
#'   theme_dag()
#'
#' @seealso [geom_dag_arrow()], [geom_dag_arrows()], and [geom_dag_edges()]
#'   for the other edge geoms, and the `auto_route` option in
#'   [ggdag_options_set()] to swap routed edges into `geom_dag()` and the
#'   quick plotting functions.
#'
#' @export
geom_dag_routed_arrows <- function(
  mapping = NULL,
  data_directed = filter_direction("->"),
  data_bidirected = filter_direction("<->"),
  node_radius = node_radius_data(),
  curvature = 0.3,
  arrow_head = ggarrow::arrow_head_wings(),
  arrow_fins = NULL,
  arrow_mid = NULL,
  length = 4,
  length_head = NULL,
  length_fins = NULL,
  length_mid = NULL,
  justify = 0,
  force_arrow = FALSE,
  mid_place = 0.5,
  resect = NULL,
  resect_head = NULL,
  resect_fins = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  ...
) {
  rlang::check_installed(
    "ggarrow",
    reason = "to use `geom_dag_routed_arrows()`."
  )

  resect_head <- resect_head %||% resect
  resect_fins <- resect_fins %||% resect

  arrow_length <- list(
    head = length_head %||% length,
    fins = length_fins %||% length,
    mid = length_mid %||% length
  )

  # Bidirected edges default to wings on both ends
  bidirected_fins <- arrow_fins %||% ggarrow::arrow_head_wings()

  list(
    dag_routed_arrow_layer(
      data_directed = data_directed,
      node_radius = node_radius,
      arrow_head = arrow_head,
      arrow_fins = arrow_fins,
      arrow_mid = arrow_mid,
      length = arrow_length,
      justify = justify,
      force_arrow = force_arrow,
      mid_place = mid_place,
      resect_head = resect_head,
      resect_fins = resect_fins,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      position = position,
      na.rm = na.rm,
      show.legend = show.legend,
      ...
    ),
    geom_dag_arrow_arc(
      mapping = mapping,
      data = data_bidirected,
      curvature = curvature,
      arrow_head = arrow_head,
      arrow_fins = bidirected_fins,
      arrow_mid = arrow_mid,
      length = length,
      length_head = length_head,
      length_fins = length_fins,
      length_mid = length_mid,
      justify = justify,
      force_arrow = force_arrow,
      mid_place = mid_place,
      resect = resect,
      resect_head = resect_head,
      resect_fins = resect_fins,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      position = position,
      na.rm = na.rm,
      show.legend = show.legend,
      ...
    )
  )
}
