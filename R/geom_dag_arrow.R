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
  required_aes = c("x", "y", "xend", "yend")
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
        resect = list(head = 0, fins = 0),
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
        resect = list(head = 0, fins = 0),
        curvature = 0.5,
        angle = 90,
        ncp = 5,
        sep = 0
      ) {
        resect <- inject_dag_resect(resect, data)
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
      },
      draw_key = ggarrow::draw_key_arrow
    )
  }
  the$GeomDAGArrowCurve
}

# Helper: inject DAG resection defaults ---------------------------------------

inject_dag_resect <- function(resect, data) {
  edge_cap <- ggdag_option("edge_cap", 8)
  if (identical(resect$head, 0) && is.null(data$resect_head)) {
    resect$head <- edge_cap
  }
  if (identical(resect$fins, 0) && is.null(data$resect_fins)) {
    resect$fins <- edge_cap
  }
  resect
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
  layer <- object$layer
  resect <- layer$geom_params$resect

  needs_head <- identical(resect$head, 0)
  needs_fins <- identical(resect$fins, 0)

  if (needs_head || needs_fins) {
    discovered <- discover_node_size(plot)
    if (!is.null(discovered)) {
      cap_mm <- node_size_to_cap(discovered)
      if (needs_head) {
        layer$geom_params$resect$head <- cap_mm
      }
      if (needs_fins) {
        layer$geom_params$resect$fins <- cap_mm
      }
    }
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
#' Auto-resection: when neither `resect` nor `resect_head`/`resect_fins` are
#' set by the user, edges are automatically shortened from both ends to avoid
#' overlapping with nodes. If a node layer (`geom_dag_point()` or
#' `geom_dag_node()`) is already added to the plot, the resection is derived
#' from the node size. Otherwise, the `ggdag.edge_cap` option (default: 8mm)
#' is used as a fallback.
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
#' @param resect A numeric value in millimetres to shorten the arrow from
#'   both ends. Overridden by `resect_head`/`resect_fins` if set.
#' @param resect_head,resect_fins Numeric values in millimetres to shorten
#'   the arrow from the head or fins end respectively.
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
  resect = 0,
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
  arrow_fins = ggarrow::arrow_head_wings(),
  arrow_mid = NULL,
  length = 4,
  length_head = NULL,
  length_fins = NULL,
  length_mid = NULL,
  justify = 0,
  force_arrow = FALSE,
  mid_place = 0.5,
  resect = 0,
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
  resect = 0,
  resect_head = NULL,
  resect_fins = NULL,
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  rlang::check_installed("ggarrow", reason = "to use `geom_dag_arrows()`.")

  list(
    geom_dag_arrow(
      mapping = mapping,
      data = data_directed,
      arrow_head = arrow_head,
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
