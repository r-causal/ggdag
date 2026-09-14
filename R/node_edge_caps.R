# Edge caps that follow the nodes ----------------------------------------------
#
# Under the ggraph edge engine, an edge whose cap is not set stops a fixed gap
# beyond the drawn extent of the node at each of its ends, so an arrowhead
# keeps the same distance from a node of any size and shape. The extent is
# measured from the node centre to the farthest ink of the glyph R draws for
# the node's shape: an edge arriving at the corner of a square node is as clear
# of it as an edge arriving anywhere on a circle. Like every other measure a
# plot draws, the gap scales with the plot's `size`. The plotters, `geom_dag()`,
# and the `geom_dag_edges*()` layers a user adds by hand all follow the nodes
# this way, and the automatic label geoms cut the edges they keep clear of at
# the same ends.
#
# ggraph reads both caps of an edge from the first row of its group, so a cap
# is settled for each edge before the edge stat expands the edge into the
# points of its path. The node at each end is found by its position in the
# panel the edge is drawn in, since a faceted plot can draw the same node with
# a different shape in each panel. The shape and size come from building the
# plot's node layers on their own, which draws each node with exactly the
# scales, mappings, and parameters the whole plot draws it with, a shape scale
# of the user's own included.
#
# Building the node layers on their own trains the scales on those layers
# alone. A scale that another layer trains as well, such as a size scale that
# a text layer maps too, can then give a node a different size here than in
# the whole plot, and the caps follow the size here.

# The gap, in millimetres at `size = 1`, between the end of an edge and the
# farthest ink of the node there.
node_edge_gap_mm <- 2

# The geoms that draw a DAG's nodes.
node_glyph_geoms <- c("GeomDagPoint", "GeomDagNode")

# The distance, in millimetres, from the centre of a node drawn at ggplot2 size
# `size` with point shape `shape` to the farthest ink of its glyph. R draws the
# circles (16, 19, 21) with radius `node_radius_mm(size)`. It draws the solid
# square (15) with that radius as its half side, so its corners lie `sqrt(2)`
# radii out, and the filled square (22) with the area of the circle, a half
# side of `sqrt(pi / 4)` radii. The stylized node draws its outermost ring with
# the plain node's glyph, so it shares these extents. Any other shape is taken
# to be as wide as the circle.
node_extent_mm <- function(shape, size) {
  radius <- node_radius_mm(size)
  pch <- shape_pch(rep_len(shape, length(radius)))

  reach <- rep_len(1, length(radius))
  reach[pch %in% 15] <- sqrt(2)
  reach[pch %in% 22] <- sqrt(pi / 4) * sqrt(2)
  radius * reach
}

# The point shape a ggplot2 shape value is drawn with. ggplot2 translates a
# shape name into its number only when the points are drawn, so a shape set by
# name reaches the built data as that name.
shape_pch <- function(shape) {
  if (!is.character(shape)) {
    return(shape)
  }

  named <- c("square" = 15, "square filled" = 22)
  pch <- unname(named[shape])
  digits <- is.na(pch) & grepl("^[0-9]+$", shape)
  pch[digits] <- as.numeric(shape[digits])
  pch
}

# The single cap, in millimetres before the plot's `size`, that a layer taking
# one cap is given for `edge_cap`. A set cap is itself. An unset cap is the cap
# of a circle node of `node_size` under the ggraph engine, where the edge
# layers then follow the node at each end and map this cap only for an end
# with no node drawn at it, and the 8 mm resection the ggarrow engine has
# always drawn with.
single_edge_cap <- function(edge_cap, node_size, edge_engine = "ggraph") {
  if (!is.null(edge_cap)) {
    return(edge_cap)
  }
  if (identical(edge_engine, "ggarrow")) {
    return(8)
  }

  node_radius_mm(node_size) + node_edge_gap_mm
}

# The cap a plotter that draws controlled nodes as squares passes on. Such a
# plotter scales a set `ggdag.edge_cap` option by 10 / 8 in its formals and
# leaves an unset cap unset, so the ggraph engine stops each edge beyond the
# node there; the ggarrow engine takes a single resection, and keeps the 10 mm
# these plotters have always drawn with.
resolve_square_plot_edge_cap <- function(edge_cap, edge_engine) {
  if (is.null(edge_cap) && identical(edge_engine, "ggarrow")) {
    return(10)
  }

  edge_cap
}

# The ggraph edge layers a plotter builds itself, made to follow the nodes when
# the plotter was handed no cap. The layers map the cap of a circle node of
# `node_size`, and the plot is drawn at `size`.
follow_nodes_when_unset <- function(layers, edge_cap, node_size, size) {
  if (!is.null(edge_cap)) {
    return(layers)
  }

  with_node_aware_caps(
    layers,
    gap = node_edge_gap_mm * size,
    fallback_extent = node_radius_mm(node_size * size)
  )
}

# Wrap the ggraph edge layers in `layers`, a `dag_edge_layer` or a list of
# them, so that each edge end stops `gap` mm beyond the node drawn there. The
# layers map the cap of a circle node of the size the plot asks for, `gap` mm
# beyond `fallback_extent` mm, which stands at an end with no node drawn at it,
# as in a plot without a node layer, and which keeps the automatic cap
# discovery in `ggplot_add.dag_edge_layer()` away from them.
with_node_aware_caps <- function(layers, gap, fallback_extent) {
  if (inherits(layers, "dag_edge_layer")) {
    layers[["layer"]] <- node_aware_cap_layer(
      .subset2(layers, "layer"),
      gap = gap,
      fallback_extent = fallback_extent
    )
    return(layers)
  }

  lapply(
    layers,
    with_node_aware_caps,
    gap = gap,
    fallback_extent = fallback_extent
  )
}

# The plot is handed to a layer while its aesthetics are computed, and the
# panels only once its stat is, so the plot is held on the layer in between and
# released there. The caps of the ends named in `ends` are written over the
# `start_cap` and `end_cap` columns before the stat carries them onto every
# point of the edge's path; an end left out keeps the cap the user gave it. The
# layer keeps the class of the layer it wraps and is known by its
# `node_aware_caps` field instead, which also tells the automatic label geoms
# to cut the edges they trace by the same caps, with the same `node_cap_gap`
# and `node_cap_fallback`, at the `node_cap_ends` that follow the nodes.
node_aware_cap_layer <- function(
  layer,
  gap,
  fallback_extent,
  ends = c("start_cap", "end_cap")
) {
  ggplot2::ggproto(
    NULL,
    layer,
    node_aware_caps = TRUE,
    node_cap_gap = gap,
    node_cap_fallback = fallback_extent,
    node_cap_ends = ends,
    compute_aesthetics = function(self, data, plot) {
      self$node_cap_plot <- plot
      ggplot2::ggproto_parent(layer, self)$compute_aesthetics(data, plot)
    },
    compute_statistic = function(self, data, layout) {
      plot <- self$node_cap_plot
      self$node_cap_plot <- NULL

      has_ends <- all(c("PANEL", "x", "y", "xend", "yend") %in% names(data))
      if (!is.null(plot) && nrow(data) > 0 && has_ends) {
        nodes <- build_node_extents(self, plot, layout)
        if ("start_cap" %in% ends) {
          start <- edge_end_extents(data$PANEL, data$x, data$y, nodes)
          start[is.na(start)] <- fallback_extent
          data$start_cap <- ggraph::circle(start + gap, "mm")
        }
        if ("end_cap" %in% ends) {
          end <- edge_end_extents(data$PANEL, data$xend, data$yend, nodes)
          end[is.na(end)] <- fallback_extent
          data$end_cap <- ggraph::circle(end + gap, "mm")
        }
      }

      ggplot2::ggproto_parent(layer, self)$compute_statistic(data, layout)
    },
    finish_statistics = function(self, data) {
      self$node_cap_nodes <- NULL
      ggplot2::ggproto_parent(layer, self)$finish_statistics(data)
    }
  )
}

# The automatic label layer traces the plot's edges and cuts each one back
# where the drawn edge stops. Where the plot's ggraph edges follow the nodes,
# the layer's stat is handed the nodes and the gap those edges stop at, and
# works out the cap at each end of each edge it traces. The plot is held and
# released as the edge layers hold it, and the nodes are let go with the
# stat's parameters once the build finishes.
node_aware_label_layer <- function(layer) {
  ggplot2::ggproto(
    NULL,
    layer,
    compute_aesthetics = function(self, data, plot) {
      self$node_cap_plot <- plot
      ggplot2::ggproto_parent(layer, self)$compute_aesthetics(data, plot)
    },
    compute_statistic = function(self, data, layout) {
      plot <- self$node_cap_plot
      self$node_cap_plot <- NULL
      self$stat_params$edge_end_caps <- label_edge_end_caps(self, plot, layout)
      ggplot2::ggproto_parent(layer, self)$compute_statistic(data, layout)
    },
    finish_statistics = function(self, data) {
      self$node_cap_nodes <- NULL
      self$stat_params$edge_end_caps <- NULL
      ggplot2::ggproto_parent(layer, self)$finish_statistics(data)
    }
  )
}

# What the label stat of `layer` needs to cut the edges of `plot` where they
# stop: the nodes drawn on `layout`, the gap and fallback extent of the first
# edge layer that follows them, and the caps the user set at the ends those
# layers leave alone. `NULL` for a plot whose edges take a single cap, which
# the label geom is given as `edge_cap`.
label_edge_end_caps <- function(layer, plot, layout) {
  if (is.null(plot)) {
    return(NULL)
  }
  edge_layer <- purrr::detect(plot$layers, \(other) {
    isTRUE(other$node_aware_caps)
  })
  if (is.null(edge_layer)) {
    return(NULL)
  }

  list(
    nodes = build_node_extents(layer, plot, layout),
    gap = edge_layer$node_cap_gap,
    fallback = edge_layer$node_cap_fallback,
    set = set_edge_caps(plot, layout)
  )
}

# The caps the user set, as an aesthetic or as a fixed value, at the ends that
# the edge layers of `plot` following the nodes leave alone, in millimetres.
# One row per edge such a layer draws, placed on the position scales of
# `layout` as the label stat sees it, with `NA` at an end that follows the
# nodes. `NULL` when no layer that follows the nodes has an end the user set.
set_edge_caps <- function(plot, layout) {
  plot_data <- plot$data
  if (inherits(plot_data, "tidy_dagitty")) {
    plot_data <- pull_dag_data(plot_data)
  }

  caps <- purrr::map(plot$layers, \(layer) {
    if (!isTRUE(layer$node_aware_caps)) {
      return(NULL)
    }
    set <- setdiff(c("start_cap", "end_cap"), layer$node_cap_ends)
    if (length(set) == 0) {
      return(NULL)
    }
    data <- resolve_layer_data(layer, plot_data)
    if (is.null(data)) {
      return(NULL)
    }
    data <- data[!is.na(data$xend), , drop = FALSE]
    if (nrow(data) == 0) {
      return(NULL)
    }

    edges <- data.frame(
      x = data$x,
      y = data$y,
      xend = data$xend,
      yend = data$yend,
      start = NA_real_,
      end = NA_real_
    )
    if ("start_cap" %in% set) {
      edges$start <- set_cap_mm(layer, "start_cap", data)
    }
    if ("end_cap" %in% set) {
      edges$end <- set_cap_mm(layer, "end_cap", data)
    }
    edges
  })

  caps <- purrr::list_rbind(caps)
  if (nrow(caps) == 0) {
    return(NULL)
  }
  rescale_edge_geometry(caps, layout)
}

# The cap `layer` draws at `end` of each edge in `data`, the rows it draws, in
# millimetres. A fixed cap is read as it is, and a mapped one is evaluated
# against the rows the way the layer evaluates it. A cap drawn in a shape
# other than a circle is taken as the circle that fits inside it. `NA` for a
# cap that is not a ggraph geometry or is measured in units that depend on
# the device, such as `"npc"` or `"lines"`, which then follows the nodes.
set_cap_mm <- function(layer, end, data) {
  cap <- layer$aes_params[[end]]
  if (is.null(cap) && !is.null(layer$mapping[[end]])) {
    cap <- tryCatch(
      rlang::eval_tidy(layer$mapping[[end]], data = data),
      error = function(cnd) NULL
    )
  }
  if (!inherits(cap, "ggraph_geometry")) {
    return(NA_real_)
  }

  fields <- unclass(cap)
  width <- fields$width * absolute_unit_mm[fields$width_unit]
  height <- fields$height * absolute_unit_mm[fields$height_unit]
  radius <- unname(pmin(width, height)) / 2
  if (length(radius) == 1 || length(radius) == nrow(data)) {
    return(rep_len(radius, nrow(data)))
  }

  NA_real_
}

# The millimetres in one of each grid unit that measures the same on every
# device.
absolute_unit_mm <- c(
  mm = 1,
  cm = 10,
  `in` = 25.4,
  inch = 25.4,
  inches = 25.4,
  pt = 25.4 / 72.27,
  points = 25.4 / 72.27,
  bigpts = 25.4 / 72,
  pc = 12 * 25.4 / 72.27,
  picas = 12 * 25.4 / 72.27
)

# The nodes of `plot` drawn on `layout`. A plot draws its directed and
# bidirected edges with a layer each, and its labels with another, and all of
# them ask for the same nodes, so the first layer to build them keeps them on
# itself for the others in the same build, which is the one drawn on the same
# `layout`, and lets them go once the build finishes.
build_node_extents <- function(layer, plot, layout) {
  found <- found_node_extents(plot, layout) %||%
    list(nodes = panel_node_extents(plot, layout), layout = layout)
  layer$node_cap_nodes <- found
  found$nodes
}

# The nodes another layer of `plot` found while it was built on `layout`, as a
# list holding them in `nodes`, or `NULL` if none has. A build that fails
# before it finishes leaves the nodes it found on its layers, so nodes found
# on any other `layout` belong to another build and are passed over.
found_node_extents <- function(plot, layout) {
  for (other in plot$layers) {
    found <- other$node_cap_nodes
    if (!is.null(found) && identical(found$layout, layout)) {
      return(found)
    }
  }

  NULL
}

# One row per node glyph the node layers of `plot` draw: the panel it is drawn
# in, numbered as in `layout`, its position, and its extent in millimetres.
# `NULL` for a plot that draws no nodes, or whose node layers cannot be built,
# which the build of the whole plot then reports.
panel_node_extents <- function(plot, layout) {
  is_node <- purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$geom, node_glyph_geoms)
  })
  if (!any(is_node)) {
    return(NULL)
  }

  # a layer is an environment, and building it writes into it, so the node
  # layers are built as copies and the whole plot's build is left alone
  node_plot <- plot
  node_plot$layers <- lapply(plot$layers[is_node], clone_layer)
  built <- build_node_layers(node_plot)
  if (is.null(built)) {
    return(NULL)
  }

  panels <- panel_translation(built$layout, layout)
  nodes <- purrr::map(built$data, \(data) {
    if (nrow(data) == 0 || !all(c("PANEL", "x", "y") %in% names(data))) {
      return(NULL)
    }
    data.frame(
      panel = panels[match(as.character(data$PANEL), names(panels))],
      x = data$x,
      y = data$y,
      extent = node_extent_mm(data$shape %||% 19, data$size %||% 16)
    )
  })

  purrr::list_rbind(nodes)
}

# The build of the whole plot builds these same node layers and reports what
# they signal, so the copies built here stay quiet rather than report it twice.
build_node_layers <- function(plot) {
  tryCatch(
    withCallingHandlers(
      ggplot2::ggplot_build(plot),
      warning = function(cnd) tryInvokeRestart("muffleWarning"),
      message = function(cnd) tryInvokeRestart("muffleMessage")
    ),
    error = function(cnd) NULL
  )
}

# The panel of `to` that each panel of `from` stands for, named by the panel of
# `from`. Both layouts facet the same plot, so a panel is the same panel in
# both when it holds the same values of the facet variables. They can number
# the panels differently: a layer drawing a facet value no node layer draws
# adds a panel to the whole plot that the node layers alone do not have.
panel_translation <- function(from, to) {
  from_panels <- as.character(from$layout$PANEL)
  to_panels <- as.character(to$layout$PANEL)
  vars <- to$facet$vars()
  shared <- all(vars %in% names(from$layout)) && all(vars %in% names(to$layout))

  if (length(vars) == 0 || !shared) {
    translated <- to_panels[match(from_panels, to_panels)]
    return(stats::setNames(translated, from_panels))
  }

  panel_key <- function(layout) {
    do.call(paste, c(lapply(layout[vars], as.character), sep = "\r"))
  }
  translated <- to_panels[match(panel_key(from$layout), panel_key(to$layout))]
  stats::setNames(translated, from_panels)
}

# The extent of the node drawn at each (`x`, `y`) in panel `panel`, the widest
# where node layers overlap there, or `NA` where no node is drawn.
edge_end_extents <- function(panel, x, y, nodes) {
  extents <- rep(NA_real_, length(x))
  if (is.null(nodes) || nrow(nodes) == 0) {
    return(extents)
  }

  panel <- as.character(panel)
  scale <- max(1, abs(c(nodes$x, nodes$y)), na.rm = TRUE)
  tolerance <- sqrt(.Machine$double.eps) * scale

  for (i in seq_along(x)) {
    here <- nodes$panel == panel[[i]] &
      abs(nodes$x - x[[i]]) <= tolerance &
      abs(nodes$y - y[[i]]) <= tolerance
    here <- here %in% TRUE
    if (any(here)) {
      extents[[i]] <- max(nodes$extent[here])
    }
  }

  extents
}

# The cap, in millimetres, at the start (`start`) and the end (`end`) of the
# edge each row of `points` traces, for the automatic label stat, and the cap
# (`fallback`) of an end with no node drawn at it, which the label geom cuts
# an edge without a cap of its own by. `edges` are the edges the stat traced,
# one row each, and `caps` what `label_edge_end_caps()` found, or `NULL`. An
# end the user set a cap at is cut by that cap. Every tracer names the points
# of an edge by an id that starts with the edge's key, so a point finds its
# edge by that key and its panel. A routed edge is cut where the router says,
# and an edge with no caps found here takes the label geom's single cap, so
# both are `NA`.
traced_edge_caps <- function(edges, points, caps) {
  none <- rep(NA_real_, nrow(points))
  if (is.null(caps) || nrow(edges) == 0) {
    return(list(start = none, end = none, fallback = none))
  }

  start <- edge_end_extents(edges$PANEL, edges$x, edges$y, caps$nodes)
  end <- edge_end_extents(edges$PANEL, edges$xend, edges$yend, caps$nodes)
  start[is.na(start)] <- caps$fallback
  end[is.na(end)] <- caps$fallback
  start <- start + caps$gap
  end <- end + caps$gap

  if (!is.null(caps$set)) {
    set_at <- match(
      edge_key(edges$x, edges$y, edges$xend, edges$yend),
      edge_key(caps$set$x, caps$set$y, caps$set$xend, caps$set$yend)
    )
    set_start <- caps$set$start[set_at]
    set_end <- caps$set$end[set_at]
    start[!is.na(set_start)] <- set_start[!is.na(set_start)]
    end[!is.na(set_end)] <- set_end[!is.na(set_end)]
  }

  edge_ids <- paste(
    edge_key(edges$x, edges$y, edges$xend, edges$yend),
    edges$PANEL,
    sep = "\r"
  )
  point_keys <- sub(
    "^([^\r]*\r[^\r]*\r[^\r]*\r[^\r]*).*$",
    "\\1",
    points$edge_id
  )
  at <- match(paste(point_keys, points$PANEL, sep = "\r"), edge_ids)
  routed <- !is.na(spec_column(points, "route_style", NA_character_))
  at[routed] <- NA_integer_

  list(
    start = start[at],
    end = end[at],
    fallback = rep(caps$fallback + caps$gap, nrow(points))
  )
}
