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
#' @param key_glyph A function to use for drawing the legend key glyph for nodes.
#'   If `NULL` (the default), the glyph is chosen automatically based on the
#'   `unified_legend` setting. When provided, this overrides the automatic
#'   selection. Common options include `draw_key_dag_point`,
#'   `draw_key_dag_combined`, and `draw_key_dag_collider`.
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
#'
#'   `geom_dag_node` also accepts:
#'
#'   - internal_colour
#'
#' @examples
#' library(ggplot2)
#' g <- dagify(m ~ x + y, y ~ x)
#' p <- g |>
#'   tidy_dagitty() |>
#'   ggplot(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
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
geom_dag_node <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  key_glyph = NULL
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
    geom = GeomDagNode,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    key_glyph = key_glyph,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname node_point
geom_dag_point <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  key_glyph = NULL
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
    geom = GeomDagPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    key_glyph = key_glyph,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' Node text
#'
#' @inheritParams ggplot2::geom_text
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
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
#' g |>
#'   tidy_dagitty() |>
#'   ggplot(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
#'   geom_dag_point() +
#'   geom_dag_edges() +
#'   geom_dag_text() +
#'   theme_dag()
geom_dag_text <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      abort(
        "Specify either {.arg position} or {.arg nudge_x}/{.arg nudge_y}, not both."
      )
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  stat_to_use <- if (identical(stat, "identity")) StatNodes else stat

  layer <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat_to_use,
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

  dag_layer(layer, default_label = TRUE)
}

#' Node text labels
#'
#' @inheritParams ggplot2::geom_label
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
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
#' g |>
#'   tidy_dagitty() |>
#'   ggplot(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
#'   geom_dag_edges(aes(
#'     start_cap = label_rect(name, padding = margin(2.5, 2.5, 2.5, 2.5, "mm")),
#'     end_cap = label_rect(name, padding = margin(2.5, 2.5, 2.5, 2.5, "mm"))
#'   )) +
#'   geom_dag_label(size = 5, fill = "black", color = "white") +
#'   theme_dag()
geom_dag_label <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      abort(
        "Specify either {.arg position} or {.arg nudge_x}/{.arg nudge_y}, not both."
      )
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  stat_to_use <- if (identical(stat, "identity")) StatNodes else stat

  layer <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat_to_use,
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

  dag_layer(layer, default_label = TRUE)
}

#' Repulsive textual annotations
#'
#' These functions are minor modifications of those in the ggrepel package.
#' `geom_dag_text_repel()` adds text directly to the plot.
#' `geom_dag_label_repel()` draws a rectangle underneath the text, making it
#' easier to read. The text labels repel away from each other and away from the
#' data points. `geom_dag_label_repel2()` is a slightly stylized version of
#' `geom_dag_label_repel()` that often looks better on DAGs.
#' `geom_dag_text_repel2()` is a slightly stylized version of
#' `geom_dag_text_repel()` that often looks better on DAGs.
#'
#' @inheritParams ggrepel::geom_text_repel
#' @inheritParams ggrepel::geom_label_repel
#' @param fontface A character vector. Default is "bold"
#' @param linewidth Width of the label border in `geom_dag_label_repel2()`.
#'   Default is 0 (no border). Set to a positive value to show borders.
#' @param node_size The size of the DAG nodes, used to compute the
#'   `point.size` aesthetic so that labels repel from the node boundary
#'   rather than the node center, and to size the skeleton discs described
#'   under Details. Defaults to `NULL`, which auto-discovers the size from a
#'   node layer (`geom_dag_node()` or `geom_dag_point()`) already added to the
#'   plot. Falls back to 16 if no node layer is found.
#' @param n_edge_points Number of invisible points to interpolate along each
#'   edge. These "fake" points participate in ggrepel's repulsion calculation
#'   so that labels avoid overlapping edges. Defaults to `NULL`, which uses
#'   the `StatNodesRepel` default of 50. Set to 0 to disable edge-aware
#'   repulsion.
#' @param n_node_points Target number of invisible points filling the disc
#'   that covers each node: a center point plus four concentric rings. Each
#'   ring holds at least six points, so every value from 1 to 16 gives the
#'   same 25 points per node and the parameter only starts to take effect
#'   above 16. Defaults to `NULL`, which uses the `StatNodesRepel` default of
#'   12. Set to 0 to disable node skeleton repulsion.
#' @param segment.color,segment.size See [ggrepel::geom_text_repel()]
#' @param segment.alpha Transparency of the line segment. Set to NULL (default) to
#'   use ggrepel's default behavior, or provide a value between 0 and 1
#'
#' @details
#' These geoms are wrappers around [ggrepel::geom_text_repel()] and
#' [ggrepel::geom_label_repel()] that use the custom `StatNodesRepel`
#' for better handling of DAG data. All arguments available in ggrepel
#' functions are supported.
#'
#' Labels are kept off nodes and edges by two mechanisms. The `point.size`
#' aesthetic, computed from `node_size`, is converted by ggrepel at draw time
#' and so describes the same circle at every device size. The invisible points
#' along edges and the disc filling each node, on the other hand, are placed in
#' data units: the disc radius is `node_size` times the average spread of the
#' nodes divided by 400 (the same divisor sizes the debug overlay described in
#' [ggdag_options_set()]). Because the panel converts data units to
#' millimetres at drawing time, that disc is congruent with the drawn node only
#' on a panel about 180 mm wide. On a narrower device the disc is smaller than
#' the node it stands for and a label may come to rest on the node; on a wider
#' one it is larger and labels are pushed further away than they need to be.
#' Set `n_node_points = 0` to rely on `point.size` alone.
#'
#' Points along an edge trace the path that edge is drawn along, including the
#' arc of a bidirected edge and of [geom_dag_edges_arc()]. Edges drawn by
#' [geom_dag_edges_diagonal()], [geom_dag_edges_fan()], and the ggarrow engine
#' are traced along the straight line between their nodes.
#'
#' Additional segment parameters can be passed through `...`, including:
#' - `segment.linetype`: Line style
#' - `segment.alpha`: Line transparency
#' - `segment.curvature`: Curve amount
#' - `segment.angle`: Curve angle
#' - `segment.ncp`: Number of control points
#' - `segment.shape`: Control point position
#' - `segment.square`: Square formation control points
#' - `segment.squareShape`: Square formation shape
#' - `segment.inflect`: Add inflection point
#' - `segment.debug`: Show debug information
#'
#' You can also pass `point.size` and `point.colour` through `...`.
#'
#' @importFrom purrr %||%
#' @export
#'
#' @examples
#' library(ggplot2)
#' g <- dagify(
#'   m ~ x + y,
#'   y ~ x,
#'   exposure = "x",
#'   outcome = "y",
#'   latent = "m",
#'   labels = c("x" = "Exposure", "y" = "Outcome", "m" = "Collider")
#' )
#'
#' g |>
#'   tidy_dagitty() |>
#'   ggplot(aes_dag()) +
#'   geom_dag_edges() +
#'   geom_dag_point() +
#'   geom_dag_text_repel(aes(label = name), show.legend = FALSE) +
#'   theme_dag()
#'
#' # Use nudge_x and nudge_y to push labels away from nodes
#' g |>
#'   tidy_dagitty() |>
#'   ggplot(aes_dag()) +
#'   geom_dag_edges() +
#'   geom_dag_point() +
#'   geom_dag_text_repel(
#'     aes(label = name),
#'     nudge_x = 0.1,
#'     nudge_y = 0.1
#'   ) +
#'   theme_dag()
#'
#' # Use position_nudge_repel for the same effect
#' g |>
#'   tidy_dagitty() |>
#'   ggplot(aes_dag()) +
#'   geom_dag_edges() +
#'   geom_dag_point() +
#'   geom_dag_text_repel(
#'     aes(label = name),
#'     position = ggrepel::position_nudge_repel(x = 0.1, y = 0.1)
#'   ) +
#'   theme_dag()
#'
#' g |>
#'   tidy_dagitty() |>
#'   dag_label(labels = c(
#'     "x" = "This is the exposure",
#'     "y" = "Here's the outcome",
#'     "m" = "Here is where they collide"
#'   )) |>
#'   ggplot(aes_dag()) +
#'   geom_dag_edges() +
#'   geom_dag_point() +
#'   geom_dag_text() +
#'   geom_dag_label_repel(
#'     aes(label = label, fill = label),
#'     col = "white",
#'     show.legend = FALSE
#'   ) +
#'   theme_dag()
#'
#' # Use directional repulsion
#' g |>
#'   tidy_dagitty() |>
#'   ggplot(aes_dag()) +
#'   geom_dag_edges() +
#'   geom_dag_point() +
#'   geom_dag_text_repel(
#'     aes(label = name),
#'     direction = "y",
#'     seed = 1234
#'   ) +
#'   theme_dag()
#'
#' # Customize segment appearance
#' g |>
#'   tidy_dagitty() |>
#'   ggplot(aes_dag()) +
#'   geom_dag_edges() +
#'   geom_dag_point() +
#'   geom_dag_text_repel(
#'     aes(label = name),
#'     segment.linetype = 2,
#'     segment.alpha = 0.5,
#'     segment.curvature = -0.3
#'   ) +
#'   theme_dag()
#'
#' @rdname repel
#' @name ggrepel functions
geom_dag_text_repel <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  parse = FALSE,
  ...,
  node_size = NULL,
  n_edge_points = NULL,
  n_node_points = NULL,
  box.padding = 1.25,
  point.padding = 1,
  min.segment.length = 0.5,
  segment.color = "#666666",
  segment.alpha = 1,
  fontface = "bold",
  segment.size = 0.5,
  arrow = NULL,
  force = 1,
  force_pull = 1,
  max.time = 0.5,
  max.iter = 2000,
  max.overlaps = Inf,
  nudge_x = 0,
  nudge_y = 0,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  na.rm = FALSE,
  show.legend = NA,
  direction = c("both", "y", "x"),
  seed = NA,
  verbose = getOption("verbose", default = FALSE),
  inherit.aes = TRUE
) {
  dots <- rlang::list2(...)

  # Use StatNodesRepel if stat is "identity", otherwise use provided stat
  stat_to_use <- if (identical(stat, "identity")) StatNodesRepel else stat
  uses_repel_stat <- inherits(stat_to_use, "StatNodesRepel")

  # If nudge_x or nudge_y are provided and position is "identity",
  # convert to position_nudge_repel for proper behavior
  if (
    identical(position, "identity") && (any(nudge_x != 0) || any(nudge_y != 0))
  ) {
    position <- ggrepel::position_nudge_repel(x = nudge_x, y = nudge_y)
  }

  # Build params list
  params <- list(
    parse = parse,
    na.rm = na.rm,
    node_size = node_size,
    n_edge_points = n_edge_points,
    n_node_points = n_node_points,
    box.padding = box.padding,
    point.padding = point.padding,
    min.segment.length = min.segment.length,
    segment.colour = resolve_segment_colour(
      segment.color,
      dots,
      missing(segment.color)
    ),
    segment.size = segment.size,
    fontface = fontface,
    arrow = arrow,
    force = force,
    force_pull = force_pull,
    max.time = max.time,
    max.iter = max.iter,
    max.overlaps = max.overlaps,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    xlim = xlim,
    ylim = ylim,
    direction = match.arg(direction),
    seed = seed,
    verbose = verbose
  )

  # Add segment.alpha if provided
  if (!is.null(segment.alpha)) {
    params$segment.alpha <- segment.alpha
  }

  # Add any additional parameters from dots
  params <- c(params, dots[!names(dots) %in% names(params)])

  # The skeleton parameters configure `StatNodesRepel`; another stat would
  # only report them as unknown.
  if (!uses_repel_stat) {
    params[c("node_size", "n_edge_points", "n_node_points")] <- NULL
  }

  layer <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat_to_use,
    geom = ggrepel::GeomTextRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )

  dag_layer(
    layer,
    discover = if (uses_repel_stat) {
      c("node_size", "edge_geometry")
    } else {
      character()
    },
    debug = uses_repel_stat
  )
}

#' @rdname repel
#' @export
#'
#' @importFrom purrr %||%
geom_dag_label_repel <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  parse = FALSE,
  ...,
  node_size = NULL,
  n_edge_points = NULL,
  n_node_points = NULL,
  box.padding = grid::unit(1.25, "lines"),
  label.padding = grid::unit(0.25, "lines"),
  point.padding = grid::unit(1, "lines"),
  label.r = grid::unit(0.15, "lines"),
  label.size = 0.25,
  min.segment.length = 0.5,
  segment.color = "grey50",
  segment.alpha = 1,
  segment.size = 0.5,
  arrow = NULL,
  force = 1,
  force_pull = 1,
  max.time = 0.5,
  max.iter = 2000,
  max.overlaps = Inf,
  nudge_x = 0,
  nudge_y = 0,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  na.rm = FALSE,
  show.legend = NA,
  direction = c("both", "y", "x"),
  seed = NA,
  verbose = getOption("verbose", default = FALSE),
  inherit.aes = TRUE
) {
  dots <- rlang::list2(...)

  # Use StatNodesRepel if stat is "identity", otherwise use provided stat
  stat_to_use <- if (identical(stat, "identity")) StatNodesRepel else stat
  uses_repel_stat <- inherits(stat_to_use, "StatNodesRepel")

  # If nudge_x or nudge_y are provided and position is "identity",
  # convert to position_nudge_repel for proper behavior
  if (
    identical(position, "identity") && (any(nudge_x != 0) || any(nudge_y != 0))
  ) {
    position <- ggrepel::position_nudge_repel(x = nudge_x, y = nudge_y)
  }

  # Build params list
  params <- list(
    parse = parse,
    node_size = node_size,
    n_edge_points = n_edge_points,
    n_node_points = n_node_points,
    box.padding = box.padding,
    label.padding = label.padding,
    point.padding = point.padding,
    label.r = label.r,
    label.size = label.size,
    min.segment.length = min.segment.length,
    segment.colour = resolve_segment_colour(
      segment.color,
      dots,
      missing(segment.color)
    ),
    segment.size = segment.size,
    arrow = arrow,
    na.rm = na.rm,
    force = force,
    force_pull = force_pull,
    max.time = max.time,
    max.iter = max.iter,
    max.overlaps = max.overlaps,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    xlim = xlim,
    ylim = ylim,
    direction = match.arg(direction),
    seed = seed,
    verbose = verbose
  )

  # Add segment.alpha if provided
  if (!is.null(segment.alpha)) {
    params$segment.alpha <- segment.alpha
  }

  # Add any additional parameters from dots
  params <- c(params, dots[!names(dots) %in% names(params)])

  # The skeleton parameters configure `StatNodesRepel`; another stat would
  # only report them as unknown.
  if (!uses_repel_stat) {
    params[c("node_size", "n_edge_points", "n_node_points")] <- NULL
  }

  layer <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat_to_use,
    geom = ggrepel::GeomLabelRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )

  dag_layer(
    layer,
    discover = if (uses_repel_stat) {
      c("node_size", "edge_geometry")
    } else {
      character()
    },
    debug = uses_repel_stat
  )
}

#' @rdname repel
#' @export
geom_dag_label_repel2 <- function(
  mapping = NULL,
  data = NULL,
  box.padding = 2,
  max.overlaps = Inf,
  label.size = NA,
  linewidth = 0,
  ...
) {
  geom_dag_label_repel(
    mapping = mapping,
    data = data,
    box.padding = box.padding,
    max.overlaps = max.overlaps,
    label.size = label.size,
    linewidth = linewidth,
    ...
  )
}

#' @rdname repel
#' @export
geom_dag_text_repel2 <- function(
  mapping = NULL,
  data = NULL,
  box.padding = 2,
  max.overlaps = Inf,
  ...
) {
  geom_dag_text_repel(
    mapping = mapping,
    data = data,
    box.padding = box.padding,
    max.overlaps = max.overlaps,
    ...
  )
}

# ggrepel accepts either spelling of the segment colour, and so do these
# wrappers. `segment.color` has a documented default here, so it can only give
# way to the British spelling when the caller left it alone.
resolve_segment_colour <- function(segment.color, dots, color_missing) {
  if (color_missing) {
    dots[["segment.colour"]] %||% segment.color
  } else {
    segment.color
  }
}

filter_direction <- function(.direction) {
  function(x) {
    x <- dplyr::filter(x, .data$direction == .direction)
    if ("collider_line" %in% names(x)) {
      x <- dplyr::filter(x, !.data$collider_line)
    }

    x
  }
}

# The cap an edge layer leaves at each of its ends, as an aesthetic on the
# mapping the layer is built with, so that the automatic cap discovery in
# `ggplot_add.dag_edge_layer()` leaves the size the caller asked for alone.
with_edge_caps <- function(mapping, cap) {
  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }

  cap_quo <- rlang::new_quosure(
    rlang::expr(ggraph::circle(!!cap, "mm")),
    env = rlang::base_env()
  )
  mapping$start_cap <- cap_quo
  mapping$end_cap <- cap_quo

  mapping
}

# The edge types a plotter can draw. A composite plotter that builds its own
# edge layers checks the type it is given here, before it settles which engine
# to draw with: `quick_plot_dag_edges()` checks the type on the ggraph branch,
# but the ggarrow branch never reaches it.
check_edge_type <- function(edge_type) {
  match.arg(edge_type, c("link_arc", "link", "arc", "diagonal"))
}

# The ggraph edge layers a quick plotter builds for itself, sized the way
# `geom_dag()` sizes the ones it builds: every measure scales with `size`, and
# the arrowhead length arrives in points. The edge type is checked here, the one
# place every composite plotter passes it through, because `edge_type_switch()`
# answers an unknown type with `NULL`.
quick_plot_dag_edges <- function(
  mapping = NULL,
  edge_type = "link_arc",
  edge_cap,
  edge_width,
  arrow_length,
  size,
  data = NULL,
  data_directed = filter_direction("->"),
  data_bidirected = filter_direction("<->"),
  show.legend = NA,
  ...
) {
  edge_type <- match.arg(
    edge_type,
    c("link_arc", "link", "arc", "diagonal")
  )
  mapping <- with_edge_caps(mapping, edge_cap * size)
  arrow_size <- grid::unit(arrow_length * size, "pt")

  if (identical(edge_type, "link_arc")) {
    return(geom_dag_edges(
      mapping,
      data_directed = data_directed,
      data_bidirected = data_bidirected,
      edge_width = edge_width * size,
      arrow_directed = grid::arrow(length = arrow_size, type = "closed"),
      arrow_bidirected = grid::arrow(
        length = arrow_size,
        ends = "both",
        type = "closed"
      ),
      show.legend = show.legend,
      ...
    ))
  }

  edge_type_switch(edge_type)(
    mapping,
    data = data,
    edge_width = edge_width * size,
    arrow = grid::arrow(length = arrow_size, type = "closed"),
    show.legend = show.legend,
    ...
  )
}

# `geom_dag_edges()` builds a layer for directed edges and one for bidirected
# edges, and most DAGs have edges of only one kind. A ggraph edge layer with no
# rows builds to a data frame with no columns at all, which loses the caps,
# widths, and arrowheads the layer was handed, so keep only the layers that
# have an edge to draw.
drop_empty_edge_layers <- function(layers, dag_data) {
  if (inherits(layers, "dag_edge_layer")) {
    layers <- list(layers)
  }

  purrr::keep(layers, \(layer) nrow(edge_layer_data(layer, dag_data)) > 0)
}

# The rows a layer draws, whether it was handed them outright, handed a
# function to pick them with, or left to inherit them from the plot.
edge_layer_data <- function(layer, dag_data) {
  layer_data <- layer$data

  if (is.function(layer_data)) {
    return(layer_data(dag_data))
  }

  if (is.null(layer_data) || inherits(layer_data, "waiver")) {
    return(dag_data)
  }

  layer_data
}

# Helper function to expand edge aesthetics
# Maps colour/color to edge_colour/edge_color if not already set
expand_edge_aes <- function(mapping) {
  if (is.null(mapping)) {
    return(mapping)
  }

  # Get the aesthetic names
  aes_names <- names(mapping)

  # Check if colour is mapped but edge_colour is not
  if ("colour" %in% aes_names && !"edge_colour" %in% aes_names) {
    mapping$edge_colour <- mapping$colour
  }

  # Check if color is mapped but edge_color is not
  if ("color" %in% aes_names && !"edge_color" %in% aes_names) {
    mapping$edge_color <- mapping$color
  }

  mapping
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
#' @param na.rm If `TRUE`, the default, missing values are removed silently. A
#'   node with no outgoing edge has a missing edge end, so the edge layers drop
#'   those rows rather than warning about them. If `FALSE`, missing values are
#'   removed with a warning.
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
#' ) |>
#'   ggplot(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
#'   geom_dag_edges() +
#'   geom_dag_point() +
#'   geom_dag_text() +
#'   theme_dag()
#'
geom_dag_edges <- function(
  mapping = NULL,
  data_directed = filter_direction("->"),
  data_bidirected = filter_direction("<->"),
  curvature = 0.3,
  arrow_directed = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
  arrow_bidirected = grid::arrow(
    length = grid::unit(5, "pt"),
    ends = "both",
    type = "closed"
  ),
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  fold = FALSE,
  ...
) {
  mapping <- expand_edge_aes(mapping)

  list(
    geom_dag_edges_link(
      mapping,
      data = data_directed,
      arrow = arrow_directed,
      position = position,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    ),
    geom_dag_edges_arc(
      mapping,
      data = data_bidirected,
      arrow = arrow_bidirected,
      curvature = curvature,
      position = position,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      fold = fold,
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
#' @param na.rm If `TRUE`, the default, missing values are removed silently. A
#'   node with no outgoing edge has a missing edge end, so the edge layers drop
#'   those rows rather than warning about them. If `FALSE`, missing values are
#'   removed with a warning.
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
#' ) |>
#'   ggplot(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
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
geom_dag_edges_link <- function(
  mapping = NULL,
  data = NULL,
  arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  mapping <- expand_edge_aes(mapping)

  layer <- ggplot2::layer(
    mapping = mapping,
    geom = GeomDAGEdgePath,
    data = data,
    stat = StatEdgeLink,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(arrow = arrow, interpolate = FALSE, na.rm = na.rm, ...)
  )

  dag_edge_layer(layer)
}

#' @rdname geom_dag_edge_functions
#' @export
geom_dag_edges_arc <- function(
  mapping = NULL,
  data = NULL,
  curvature = 0.5,
  arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  fold = FALSE,
  n = 100,
  lineend = "butt",
  linejoin = "round",
  linemitre = 1,
  label_colour = "black",
  label_alpha = 1,
  label_parse = FALSE,
  check_overlap = FALSE,
  angle_calc = "rot",
  force_flip = TRUE,
  label_dodge = NULL,
  label_push = NULL,
  ...
) {
  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }

  mapping <- expand_edge_aes(mapping)

  layer <- ggplot2::layer(
    mapping = mapping,
    geom = GeomDAGEdgePath,
    data = data,
    stat = StatEdgeArc,
    check.aes = FALSE,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      strength = curvature,
      interpolate = FALSE,
      fold = fold,
      na.rm = na.rm,
      n = n,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      label_colour = label_colour,
      label_alpha = label_alpha,
      label_parse = label_parse,
      check_overlap = check_overlap,
      angle_calc = angle_calc,
      force_flip = force_flip,
      label_dodge = label_dodge,
      label_push = label_push,
      ...
    )
  )

  dag_edge_layer(layer)
}

#' @inheritParams ggraph::geom_edge_diagonal
#'
#' @rdname geom_dag_edge_functions
#' @export
geom_dag_edges_diagonal <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  curvature = 1,
  n = 100,
  lineend = "butt",
  linejoin = "round",
  linemitre = 1,
  label_colour = "black",
  label_alpha = 1,
  label_parse = FALSE,
  check_overlap = FALSE,
  angle_calc = "rot",
  force_flip = TRUE,
  label_dodge = NULL,
  label_push = NULL,
  ...
) {
  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }

  mapping <- expand_edge_aes(mapping)

  layer <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEdgeDiagonal,
    geom = GeomDAGEdgePath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      na.rm = na.rm,
      interpolate = FALSE,
      n = n,
      lineend = lineend,
      flipped = FALSE,
      strength = curvature,
      linejoin = linejoin,
      linemitre = linemitre,
      label_colour = label_colour,
      label_alpha = label_alpha,
      label_parse = label_parse,
      check_overlap = check_overlap,
      angle_calc = angle_calc,
      force_flip = force_flip,
      label_dodge = label_dodge,
      label_push = label_push,
      ...
    )
  )

  dag_edge_layer(layer)
}

#' @inheritParams ggraph::geom_edge_fan
#'
#' @rdname geom_dag_edge_functions
#' @export
geom_dag_edges_fan <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  spread = 0.7,
  n = 100,
  lineend = "butt",
  linejoin = "round",
  linemitre = 1,
  label_colour = "black",
  label_alpha = 1,
  label_parse = FALSE,
  check_overlap = FALSE,
  angle_calc = "rot",
  force_flip = TRUE,
  label_dodge = NULL,
  label_push = NULL,
  ...
) {
  if (is.null(mapping)) {
    mapping <- ggplot2::aes(from = .data$name, to = .data$to)
  } else if (is.null(mapping$from)) {
    mapping$from <- rlang::expr(.data$name)
    mapping$to <- rlang::expr(.data$to)
  }

  mapping <- expand_edge_aes(mapping)

  layer <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEdgeFan,
    geom = GeomDAGEdgePath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      na.rm = na.rm,
      interpolate = FALSE,
      n = n,
      lineend = lineend,
      strength = spread,
      linejoin = linejoin,
      linemitre = linemitre,
      label_colour = label_colour,
      label_alpha = label_alpha,
      label_parse = label_parse,
      check_overlap = check_overlap,
      angle_calc = angle_calc,
      force_flip = force_flip,
      label_dodge = label_dodge,
      label_push = label_push,
      ...
    )
  )

  dag_edge_layer(layer)
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
#' dagify(m ~ a + b, x ~ a, y ~ b) |>
#'   tidy_dagitty() |>
#'   control_for("m") |>
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted)) +
#'   geom_dag_edges() +
#'   geom_dag_collider_edges() +
#'   geom_dag_point() +
#'   geom_dag_text() +
#'   theme_dag() +
#'   scale_adjusted()
geom_dag_collider_edges <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  linewidth = 0.6,
  size = NULL,
  curvature = 0.5,
  angle = 90,
  ncp = 5,
  arrow = NULL,
  lineend = "butt",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (is.null(data)) {
    data <- function(x) {
      dplyr::filter(x, .data$direction == "<->", .data$collider_line)
    }
  }
  if (is.null(mapping)) {
    mapping <- ggplot2::aes(
      linetype = factor(
        .data$collider_line,
        levels = TRUE,
        "activated by \nadjustment \nfor collider"
      )
    )
  }
  if (is.null(mapping$linetype)) {
    mapping$linetype <- substitute(factor(
      .data$collider_line,
      levels = TRUE,
      "activated by \nadjustment \nfor collider"
    ))
  }
  if (!is.null(size)) {
    warn(
      c(
        "{.arg size} is deprecated for lines.",
        "i" = "Please use {.arg linewidth} instead."
      ),
      warning_class = "ggdag_deprecated"
    )
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
#' `aes_dag()` is a wrapper around `aes()` that specifies `x`, `y`, `xend`, and
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
#' confounder_triangle() |>
#'   dag_adjustment_sets() |>
#'   ggplot(aes_dag(color = .data$adjusted)) +
#'   geom_dag() +
#'   facet_wrap(~set)
#'
#' @export
aes_dag <- function(...) {
  addtl_aes <- ggplot2::aes(...)
  default_aes <- ggplot2::aes(
    x = .data$x,
    y = .data$y,
    xend = .data$xend,
    yend = .data$yend
  )

  default_aes[names(addtl_aes)] <- addtl_aes

  default_aes
}

# Layer data for an edge layer that also filters by edge direction. The user's
# data, whether a function or a data frame, is applied first and the direction
# filter narrows what it returns.
compose_edge_data <- function(user_data, dir_filter) {
  if (is.null(user_data)) {
    return(dir_filter)
  }
  if (is.function(user_data)) {
    return(function(x) dir_filter(user_data(x)))
  }
  dir_filter(user_data)
}

# `geom_dag()` gives its ggarrow edge layers the per-edge curvature aesthetic
# through `ggplot_add.geom_dag_layers()`. The quick plotters that build edge
# layers of their own, to colour or fade them by an analysis column, add it
# here instead.
with_edge_curvature <- function(mapping, dag_data) {
  if ("edge_curvature" %nin% names(dag_data)) {
    return(mapping)
  }

  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }
  mapping$edge_curvature <- rlang::quo(.data$edge_curvature)
  mapping
}

# The ggarrow edge layers a quick plotter builds for itself. Directed edges go
# through the arc geom at zero curvature, which draws them straight but leaves
# room for a per-edge curvature value to bend them.
quick_plot_arrow_edges <- function(
  mapping = NULL,
  data_directed,
  data_bidirected,
  arrow_head,
  arrow_fins,
  resect,
  linewidth,
  length,
  show.legend = NA,
  ...
) {
  list(
    geom_dag_arrow_arc(
      mapping = mapping,
      data = data_directed,
      curvature = 0,
      arrow_head = arrow_head,
      arrow_fins = arrow_fins,
      resect = resect,
      linewidth = linewidth,
      length = length,
      show.legend = show.legend,
      ...
    ),
    geom_dag_arrow_arc(
      mapping = mapping,
      data = data_bidirected,
      curvature = ggdag_option("curvature", 0.3),
      arrow_head = arrow_head,
      arrow_fins = arrow_fins %||% ggarrow::arrow_head_wings(),
      resect = resect,
      linewidth = linewidth,
      length = length,
      show.legend = show.legend,
      ...
    )
  )
}

# ggarrow reads a bare arrow length as a multiple of the shaft width, so the
# length ggdag documents in points has to travel as an absolute unit.
arrow_length_unit <- function(arrow_length) {
  grid::unit(arrow_length, "pt")
}

# Build ggarrow edge layers for geom_dag()
geom_dag_ggarrow_edges <- function(
  edge_type,
  sizes,
  show.legend = NA,
  data = NULL
) {
  rlang::check_installed(
    "ggarrow",
    reason = "to use edge_engine = \"ggarrow\"."
  )

  arrow_head <- ggdag_option("arrow_head", NULL) %||%
    ggarrow::arrow_head_wings()
  arrow_fins <- ggdag_option("arrow_fins", NULL)
  arrow_mid <- ggdag_option("arrow_mid", NULL)
  curvature <- ggdag_option("curvature", 0.3)
  resect <- sizes[["cap"]]
  linewidth <- sizes[["edge"]]
  arrow_length <- arrow_length_unit(sizes[["arrow"]])

  dag_mapping <- aes_dag()

  switch(
    edge_type,
    "link_arc" = list(
      geom_dag_arrow_arc(
        mapping = dag_mapping,
        data = compose_edge_data(data, filter_direction("->")),
        arrow_head = arrow_head,
        arrow_fins = arrow_fins,
        arrow_mid = arrow_mid,
        curvature = 0,
        resect = resect,
        linewidth = linewidth,
        length = arrow_length,
        show.legend = show.legend
      ),
      geom_dag_arrow_arc(
        mapping = dag_mapping,
        data = compose_edge_data(data, filter_direction("<->")),
        arrow_head = arrow_head,
        arrow_fins = arrow_fins %||% ggarrow::arrow_head_wings(),
        arrow_mid = arrow_mid,
        curvature = curvature,
        resect = resect,
        linewidth = linewidth,
        length = arrow_length,
        show.legend = show.legend
      )
    ),
    "link" = geom_dag_arrow(
      mapping = dag_mapping,
      data = data,
      arrow_head = arrow_head,
      arrow_fins = arrow_fins,
      arrow_mid = arrow_mid,
      resect = resect,
      linewidth = linewidth,
      length = arrow_length,
      show.legend = show.legend
    ),
    "arc" = list(
      geom_dag_arrow_arc(
        mapping = dag_mapping,
        data = compose_edge_data(data, filter_direction("->")),
        arrow_head = arrow_head,
        arrow_fins = arrow_fins,
        arrow_mid = arrow_mid,
        curvature = curvature,
        resect = resect,
        linewidth = linewidth,
        length = arrow_length,
        show.legend = show.legend
      ),
      geom_dag_arrow_arc(
        mapping = dag_mapping,
        data = compose_edge_data(data, filter_direction("<->")),
        arrow_head = arrow_head,
        arrow_fins = arrow_fins %||% ggarrow::arrow_head_wings(),
        arrow_mid = arrow_mid,
        curvature = curvature,
        resect = resect,
        linewidth = linewidth,
        length = arrow_length,
        show.legend = show.legend
      )
    ),
    "diagonal" = list(
      geom_dag_arrow_arc(
        mapping = dag_mapping,
        data = compose_edge_data(data, filter_direction("->")),
        arrow_head = arrow_head,
        arrow_fins = arrow_fins,
        arrow_mid = arrow_mid,
        curvature = curvature,
        resect = resect,
        linewidth = linewidth,
        length = arrow_length,
        show.legend = show.legend
      ),
      geom_dag_arrow_arc(
        mapping = dag_mapping,
        data = compose_edge_data(data, filter_direction("<->")),
        arrow_head = arrow_head,
        arrow_fins = arrow_fins %||% ggarrow::arrow_head_wings(),
        arrow_mid = arrow_mid,
        curvature = curvature,
        resect = resect,
        linewidth = linewidth,
        length = arrow_length,
        show.legend = show.legend
      )
    )
  )
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
#' @inheritParams ggplot2::geom_point
#' @param size A numeric value scaling the size of all elements in the DAG. This
#'   allows you to change the scale of the DAG without changing the proportions.
#' @param edge_type The type of edge, one of "link_arc", "link", "arc",
#'   "diagonal".
#' @param edge_engine The engine used to draw edges. Either `"ggraph"`
#'   (default) or `"ggarrow"`. When `"ggarrow"`, edges are drawn using
#'   [ggarrow][ggarrow::ggarrow-package] geoms, which support additional
#'   customization via the `arrow_head`, `arrow_fins`, `arrow_mid`, and
#'   `curvature` global options (see [ggdag_options_set()]).
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
#' @param use_labels A logical value. Include a label geom? The specific geom
#'   used is controlled by `label_geom`.
#' @param label_geom A geom function to use for drawing labels when
#'   `use_labels = TRUE`. Default is `geom_dag_label_repel`. Other options
#'   include `geom_dag_label`, `geom_dag_text_repel`, `geom_dag_label_repel2`,
#'   and `geom_dag_text_repel2`.
#' @param n_edge_points Number of invisible points to interpolate along each
#'   edge for label repulsion. Passed to repel label geoms. Defaults to `NULL`
#'   (uses `StatNodesRepel` default of 50). Set to 0 to disable.
#' @param n_node_points Target number of invisible skeleton points filling the
#'   disc that covers each node for label repulsion: a center point plus four
#'   concentric rings, each holding at least six points, so every value from 1
#'   to 16 gives the same 25 points per node. Passed to repel label geoms.
#'   Defaults to `NULL` (uses `StatNodesRepel` default of 12). Set to 0 to
#'   disable. The disc is measured in data units, so it matches the drawn node
#'   only on a panel about 180 mm wide; see [geom_dag_label_repel()].
#' @param unified_legend A logical value. When `TRUE` and both `use_edges` and
#'   `use_nodes` are `TRUE`, creates a unified legend entry showing both nodes
#'   and edges in a single key, and hides the separate edge legend. This creates
#'   a single, more compact legend. Default is `TRUE`.
#' @param key_glyph A function to use for drawing the legend key glyph for nodes.
#'   If `NULL` (the default), the glyph is chosen automatically based on the
#'   `unified_legend` setting. When provided, this overrides the automatic
#'   selection. Common options include `draw_key_dag_point`,
#'   `draw_key_dag_combined`, and `draw_key_dag_collider`.
#' @param label The bare name of a column to use for labels.
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
#' ggplot(dag, aes_dag()) +
#'   geom_dag()
#' ggplot(dag, aes_dag()) +
#'   geom_dag(size = 1.5)
#' ggplot(dag, aes_dag()) +
#'   geom_dag(size = 1.5, text_size = 8)
#'
#' # Using different label geoms
#' dag_labeled <- dagify(
#'   y ~ x,
#'   z ~ y,
#'   labels = c(x = "Exposure", y = "Outcome", z = "Mediator")
#' )
#'
#' # Default: repelling labels
#' ggplot(dag_labeled, aes_dag()) +
#'   geom_dag(use_labels = TRUE)
#'
#' # Static labels
#' ggplot(dag_labeled, aes_dag()) +
#'   geom_dag(use_labels = TRUE, label_geom = geom_dag_label)
#'
#' # Repelling text instead of labels
#' ggplot(dag_labeled, aes_dag()) +
#'   geom_dag(use_labels = TRUE, label_geom = geom_dag_text_repel)
#'
#' @export
geom_dag <- function(
  data = NULL,
  size = 1,
  edge_type = c("link_arc", "link", "arc", "diagonal"),
  edge_engine = ggdag_option("edge_engine", "ggraph"),
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option("edge_cap", 8),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  n_edge_points = NULL,
  n_node_points = NULL,
  unified_legend = TRUE,
  key_glyph = NULL,
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  use_nodes <- check_arg_node(node, use_nodes)
  use_stylized <- check_arg_stylized(stylized, use_stylized)
  edge_engine <- match.arg(edge_engine, c("ggraph", "ggarrow"))

  sizes <- c(
    cap = edge_cap,
    node = node_size,
    text = text_size,
    label = label_size,
    edge = edge_width,
    arrow = arrow_length,
    box_padding = 1.5
  ) *
    size

  if (isTRUE(use_edges)) {
    # Hide edge legend when using unified legend with both edges and nodes
    edge_show_legend <- !(isTRUE(unified_legend) && isTRUE(use_nodes))

    if (missing(edge_type)) {
      edge_type <- ggdag_option("edge_type", "link_arc")
    }
    edge_type <- match.arg(edge_type)

    if (identical(edge_engine, "ggarrow")) {
      edge_geom <- geom_dag_ggarrow_edges(
        edge_type = edge_type,
        sizes = sizes,
        show.legend = edge_show_legend,
        data = data
      )
    } else {
      if (edge_type == "link_arc") {
        edge_geom <- geom_dag_edges(
          ggplot2::aes(
            start_cap = ggraph::circle(sizes[["cap"]], "mm"),
            end_cap = ggraph::circle(sizes[["cap"]], "mm")
          ),
          data_directed = compose_edge_data(data, filter_direction("->")),
          data_bidirected = compose_edge_data(data, filter_direction("<->")),
          edge_width = sizes[["edge"]],
          arrow_directed = grid::arrow(
            length = grid::unit(sizes[["arrow"]], "pt"),
            type = "closed"
          ),
          arrow_bidirected = grid::arrow(
            length = grid::unit(sizes[["arrow"]], "pt"),
            ends = "both",
            type = "closed"
          ),
          show.legend = edge_show_legend
        )
      } else {
        edge_function <- edge_type_switch(edge_type)
        edge_geom <- edge_function(
          ggplot2::aes(
            start_cap = ggraph::circle(sizes[["cap"]], "mm"),
            end_cap = ggraph::circle(sizes[["cap"]], "mm")
          ),
          data = data,
          edge_width = sizes[["edge"]],
          arrow = grid::arrow(
            length = grid::unit(sizes[["arrow"]], "pt"),
            type = "closed"
          ),
          show.legend = edge_show_legend
        )
      }
    }
  } else {
    edge_geom <- NULL
  }

  if (isTRUE(use_nodes)) {
    # Determine key glyph: use provided glyph or select based on unified_legend
    node_key_glyph <- if (!is.null(key_glyph)) {
      key_glyph
    } else if (isTRUE(unified_legend) && isTRUE(use_edges)) {
      draw_key_dag_combined
    } else {
      draw_key_dag_point
    }

    # the key draws the ornament this plot's edges are drawn with, which the
    # argument settles even when the global option says otherwise
    node_key_glyph <- dag_key_glyph(node_key_glyph, edge_engine)

    if (isTRUE(use_stylized)) {
      node_geom <- geom_dag_node(
        size = sizes[["node"]],
        data = data,
        key_glyph = node_key_glyph
      )
    } else {
      node_geom <- geom_dag_point(
        size = sizes[["node"]],
        data = data,
        key_glyph = node_key_glyph
      )
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
        "Set `use_text = ",
        rlang::quo_text(text),
        "`. ",
        "To use a variable other than node names, set `text = variable_name`"
      )
    )

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

    text_geom <- geom_dag_text(
      mapping = mapping,
      data = data,
      col = text_col,
      size = sizes[["text"]]
    )
  } else {
    text_geom <- NULL
  }

  if (is.character(use_labels)) {
    deprecate_warn(
      "0.3.0",
      "geom_dag(use_labels = 'must be a logical')",
      details = paste0(
        "Set `use_labels = TRUE` ",
        "and `label = ",
        use_labels,
        "`"
      )
    )

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

    # Build common parameters
    common_params <- list(
      mapping = ggplot2::aes(label = !!label),
      data = data,
      size = sizes[["label"]] * 1.1,
      col = label_col,
      show.legend = FALSE
    )

    # Add parameters that might be used by repel functions
    # These will be ignored by geoms that don't use them
    if (
      identical(label_geom, geom_dag_label_repel) ||
        identical(label_geom, geom_dag_label_repel2)
    ) {
      common_params$node_size <- sizes[["node"]]
      common_params$n_edge_points <- n_edge_points
      common_params$n_node_points <- n_node_points
      common_params$box.padding <- sizes[["box_padding"]]
      common_params$max.overlaps <- Inf
      common_params$label.padding <- 0.1
    } else if (
      identical(label_geom, geom_dag_text_repel) ||
        identical(label_geom, geom_dag_text_repel2)
    ) {
      common_params$node_size <- sizes[["node"]]
      common_params$n_edge_points <- n_edge_points
      common_params$n_node_points <- n_node_points
      common_params$box.padding <- sizes[["box_padding"]]
      common_params$max.overlaps <- Inf
    }

    # The label layer stays wrapped so that it can read the edge layers of the
    # plot it is added to; `node_size` is already threaded here, so the wrapper
    # leaves it alone.
    label_geom_result <- do.call(label_geom, common_params)
  } else {
    label_geom_result <- NULL
  }

  result <- list(
    node_geom,
    edge_geom,
    text_geom,
    label_geom_result
  )

  structure(result, class = "geom_dag_layers")
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.geom_dag_layers <- function(object, plot, ...) {
  plot_data <- plot$data
  if (inherits(plot_data, "tidy_dagitty")) {
    plot_data <- pull_dag_data(plot_data)
  }
  has_curvature <- "edge_curvature" %in% names(plot_data)
  wants_curve <- wants_edge_curvature(plot_data)
  curvature_ignored <- FALSE

  for (item in flatten_dag_layers(object)) {
    if (has_curvature && inherits(item, "dag_arrow_layer")) {
      item <- inject_edge_curvature(item)
    }
    if (wants_curve && inherits(item, "dag_edge_layer")) {
      curvature_ignored <- TRUE
    }
    plot <- ggplot2::ggplot_add(item, plot, ...)
  }

  if (curvature_ignored) {
    warn_ignored_edge_curvature()
  }

  plot
}

# `geom_dag()` hands back a list that can hold further lists, because an edge
# type such as `link_arc` needs a layer per edge direction. The wrapped layer
# classes are lists too, so they are the leaves of the walk rather than
# something to iterate into: routing them through `ggplot_add()` themselves is
# what gives them their caps and their discovered node size.
flatten_dag_layers <- function(object) {
  leaves <- list()
  for (item in object) {
    if (is.null(item)) {
      next
    }
    is_branch <- is.list(item) &&
      !inherits(item, "ggproto") &&
      !inherits(item, "dag_arrow_layer") &&
      !inherits(item, "dag_edge_layer") &&
      !inherits(item, "dag_layer")
    if (is_branch) {
      leaves <- c(leaves, flatten_dag_layers(item))
    } else {
      leaves <- c(leaves, list(item))
    }
  }
  leaves
}

# A layer is an environment, so the mapping goes onto a copy: the caller may be
# holding the layer this one was built from.
inject_edge_curvature <- function(item) {
  layer <- clone_layer(.subset2(item, "layer"))
  layer$mapping$edge_curvature <- rlang::quo(.data$edge_curvature)
  dag_arrow_layer(layer)
}

# Whether the data behind a plot asks for a curvature on some individual edge.
# A column of zeros is the shape `tidy_dagitty()` leaves behind once any edge
# has been curved and then uncurved, and asks for nothing.
wants_edge_curvature <- function(dag_data) {
  "edge_curvature" %in%
    names(dag_data) &&
    any(dag_data$edge_curvature != 0, na.rm = TRUE)
}

# Report a per-edge curvature that the ggraph edge layers about to be added
# cannot draw. Called by each function that builds ggraph edge layers of its
# own, so that the plotters which pass `use_edges = FALSE` to `geom_dag()` are
# as loud about it as `geom_dag()` itself.
warn_if_curvature_ignored <- function(dag_data) {
  if (wants_edge_curvature(dag_data)) {
    warn_ignored_edge_curvature()
  }
  invisible(NULL)
}

# The ggraph edge geoms draw each edge with the curvature of their own edge
# type and have nowhere to put a per-edge value, so a curvature the plot asked
# for would otherwise disappear without a word.
warn_ignored_edge_curvature <- function() {
  warn(
    c(
      "Per-edge curvature is drawn by the ggarrow edge engine only.",
      "x" = "The {.val ggraph} engine is drawing these edges, so the {.field edge_curvature} values are ignored.",
      "i" = 'Set {.code edge_engine = "ggarrow"}, or {.code ggdag_options_set(edge_engine = "ggarrow")}, to draw them.'
    ),
    warning_class = "ggdag_edge_curvature_warning"
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

  p +
    expand_plot(
      expand_x = expansion(c(0.10, 0.10)),
      expand_y = expansion(c(0.10, 0.10))
    )
}

#' @rdname ggplot.tidy_dagitty
#' @export
ggplot.dagitty <- ggplot.tidy_dagitty
