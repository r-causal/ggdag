# Edge ends that follow the nodes ----------------------------------------------
#
# An edge whose cap is not set stops a fixed gap outside the outline of the
# node at each of its ends, under either edge engine, so an arrowhead keeps
# the same distance from a node of any size and shape. The outline of a circle
# node is its radius, and the outline of a square node is its half side: an
# edge meeting a square at an angle stops where it crosses the square drawn
# the gap out from the node's own, and an edge meeting a circle stops on the
# circle the gap wider than the node. Like every other measure a plot draws,
# the gap scales with the plot's `size`. The plotters, `geom_dag()`, and the
# edge layers a user adds by hand all follow the nodes this way, and the
# automatic label geoms cut the edges they keep clear of at the same ends.
#
# The node at each end is found by its position in the panel the edge is
# drawn in, since a faceted plot can draw the same node with a different shape
# in each panel. The shape and size come from building the plot's node layers
# on their own, which draws each node with exactly the scales, mappings, and
# parameters the whole plot draws it with, a shape scale of the user's own
# included.
#
# Under the ggraph engine the cap is a geometry ggraph cuts the drawn path at:
# a circle at a circle node and a square at a square node, both settled for
# each edge before the edge stat expands the edge into the points of its path,
# since ggraph reads both caps of an edge from the first row of its group.
#
# Under the ggarrow engine an end is resected by a straight-line distance from
# the end of the path. At a circle node that distance is the outline plus the
# gap whatever angle the edge arrives at. At a square node it depends on the
# angle, which is known only once the panel is drawn in millimetres, so the
# layers settle the circle resections and the square half sides when the plot
# is built and turn the half sides into resections when the plot is drawn,
# where the path is known in millimetres (`square_end_resect()`).
#
# Building the node layers on their own trains the scales on those layers
# alone. A scale that another layer trains as well, such as a size scale that
# a text layer maps too, can then give a node a different size here than in
# the whole plot, and the caps follow the size here.

# The gap, in millimetres at `size = 1`, between the end of an edge and the
# outline of the node there.
node_edge_gap_mm <- 2

# The geoms that draw a DAG's nodes.
node_glyph_geoms <- c("GeomDagPoint", "GeomDagNode")

# The point shapes R draws as squares: the solid square and the filled square.
square_pch <- c(15, 22)

# The distance, in millimetres, from the centre of a node drawn at ggplot2 size
# `size` with point shape `shape` to its outline. R draws the circles (16, 19,
# 21) with radius `node_radius_mm(size)`. It draws the solid square (15) with
# that radius as its half side, and the filled square (22) with the area of
# the circle, a half side of `sqrt(pi / 4)` radii. The stylized node draws its
# outermost ring with the plain node's glyph, so it shares these outlines. Any
# other shape is taken to be as wide as the circle.
node_outline_mm <- function(shape, size) {
  radius <- node_radius_mm(size)
  pch <- shape_pch(rep_len(shape, length(radius)))

  reach <- rep_len(1, length(radius))
  reach[pch %in% 22] <- sqrt(pi / 4)
  radius * reach
}

# Whether each node shape is drawn as a square, whose outline is flat.
node_is_square <- function(shape) {
  shape_pch(shape) %in% square_pch
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
# of a circle node of `node_size`, which the edge layers then map, or resect
# by, only at an end with no node drawn at it, since they follow the node at
# each end otherwise.
single_edge_cap <- function(edge_cap, node_size) {
  edge_cap %||% (node_radius_mm(node_size) + node_edge_gap_mm)
}

# The edge layers a plotter builds itself, made to follow the nodes when the
# plotter was handed no cap. The layers take the cap of a circle node of
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

# Wrap the edge layers in `layers`, a `dag_edge_layer`, a `dag_arrow_layer`,
# or a list of them, so that each edge end stops `gap` mm beyond the node
# drawn there. The layers take the cap of a circle node of the size the plot
# asks for, `gap` mm beyond `fallback_extent` mm, which stands at an end with
# no node drawn at it, as in a plot without a node layer, and which keeps the
# automatic cap discovery in `ggplot_add.dag_edge_layer()` and
# `ggplot_add.dag_arrow_layer()` away from them.
with_node_aware_caps <- function(layers, gap, fallback_extent) {
  if (inherits(layers, "dag_edge_layer")) {
    layers[["layer"]] <- node_aware_cap_layer(
      .subset2(layers, "layer"),
      gap = gap,
      fallback_extent = fallback_extent
    )
    return(layers)
  }
  if (inherits(layers, "dag_arrow_layer")) {
    layers[["layer"]] <- node_aware_resect_layer(
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

# The ggraph engine --------------------------------------------------------------

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
      if (!is.null(plot) && nrow(data) > 0 && has_ends && length(ends) > 0) {
        nodes <- build_node_extents(self, plot, layout)
        if ("start_cap" %in% ends) {
          data$start_cap <- node_cap_geometry(
            edge_end_nodes(data$PANEL, data$x, data$y, nodes),
            gap,
            fallback_extent
          )
        }
        if ("end_cap" %in% ends) {
          data$end_cap <- node_cap_geometry(
            edge_end_nodes(data$PANEL, data$xend, data$yend, nodes),
            gap,
            fallback_extent
          )
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

# The ggraph cap geometry of the edge ends that meet the nodes in `at`, from
# `edge_end_nodes()`: a circle `gap` mm wider than a circle node, a square
# `gap` mm wider on every side than a square node, and the circle of a node of
# `fallback` mm where no node is drawn.
node_cap_geometry <- function(at, gap, fallback) {
  outline <- at$outline
  outline[is.na(outline)] <- fallback
  ggraph::geometry(
    type = ifelse(at$square, "rect", "circle"),
    width = 2 * (outline + gap),
    width_unit = "mm"
  )
}

# The ggarrow engine -------------------------------------------------------------

# The ggarrow ends, named as ggarrow names them, and the ggraph caps they
# stand where.
arrow_end_caps <- c(fins = "start_cap", head = "end_cap")

# The ggarrow counterpart of `node_aware_cap_layer()`. ggarrow resects an end
# by the `resect_fins` and `resect_head` aesthetics when the data carry them,
# so the layer writes those columns for the ends named in `ends`, in
# millimetres: the outline of the node at the end plus `gap`, or the fallback
# resection where no node is drawn there, which is the layer's own resection
# once it has one and the `ggdag.edge_cap` option otherwise. A square end's
# resection depends on the angle the edge meets the square at, which is known
# only when the plot is drawn, so the column holds the half side of the square
# the tip lies on and `.ggdag_square_fins` or `.ggdag_square_head` marks the
# end for `square_end_resect()` to settle at draw time. Every row also carries
# the outline and shape of the node at each end (`.ggdag_node_*`), whether or
# not that end follows the node, so that the routed edge geom can hand the
# router the shape of every node it clears, and the gap the ends stop at
# (`.ggdag_node_gap`). The node-aware fields are the ones the ggraph wrapper
# sets, with the ends named as the label engine names them.
node_aware_resect_layer <- function(
  layer,
  gap,
  fallback_extent,
  ends = c("fins", "head")
) {
  ggplot2::ggproto(
    NULL,
    layer,
    node_aware_caps = TRUE,
    node_cap_gap = gap,
    node_cap_fallback = fallback_extent,
    node_cap_ends = unname(arrow_end_caps[ends]),
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
        # an end with no node drawn at it is resected by the layer's own
        # resection where the layer has settled one, which is the cap of a
        # circle node of the size it discovered; a plot that draws no nodes
        # at all leaves the layer none, and the ends then take the
        # `ggdag.edge_cap` option, as `inject_dag_resect()` gives it to a
        # layer whose data carry no resection
        resect <- self$geom_params$resect
        fallback <- lapply(stats::setNames(nm = ends), \(end) {
          own <- resect[[end]]
          if (is.numeric(own) && length(own) == 1 && is.finite(own)) {
            own - gap
          } else if (is.null(nodes)) {
            ggdag_option("edge_cap", fallback_extent + gap) - gap
          } else {
            fallback_extent
          }
        })
        if (length(fallback) > 0) {
          self$node_cap_fallback <- fallback[[1]]
        }
        data <- node_resect_columns(data, nodes, gap, fallback, ends)
      }

      ggplot2::ggproto_parent(layer, self)$compute_statistic(data, layout)
    },
    finish_statistics = function(self, data) {
      self$node_cap_nodes <- NULL
      ggplot2::ggproto_parent(layer, self)$finish_statistics(data)
    }
  )
}

# The per-row columns `node_aware_resect_layer()` writes on `data`, whose rows
# run from (`x`, `y`) to (`xend`, `yend`) and meet `nodes`. `fallback` holds
# the outline, in millimetres, an end in `ends` with no node drawn at it is
# resected beyond, named by end.
node_resect_columns <- function(data, nodes, gap, fallback, ends) {
  fins <- edge_end_nodes(data$PANEL, data$x, data$y, nodes)
  head <- edge_end_nodes(data$PANEL, data$xend, data$yend, nodes)
  at <- list(fins = fins, head = head)

  data$.ggdag_node_gap <- rep(gap, nrow(data))
  for (end in c("fins", "head")) {
    data[[paste0(".ggdag_node_", end)]] <- at[[end]]$outline
    data[[paste0(".ggdag_node_square_", end)]] <- at[[end]]$square
    data[[paste0(".ggdag_follow_", end)]] <- rep(end %in% ends, nrow(data))
    data[[paste0(".ggdag_square_", end)]] <- rep(FALSE, nrow(data))
  }

  for (end in ends) {
    outline <- at[[end]]$outline
    found <- !is.na(outline)
    outline[!found] <- fallback[[end]]
    data[[paste0("resect_", end)]] <- outline + gap
    data[[paste0(".ggdag_square_", end)]] <- found & at[[end]]$square
  }

  data
}

# The straight-line resection, in millimetres from the end of the path
# (`x`, `y`) in millimetres, that puts the point ggarrow cuts the path back
# to on the square of half side `half` centred at (`cx`, `cy`). ggarrow cuts a
# path where its straight-line distance from the end falls below the
# resection, between the last point at least that far and the next point,
# placed by that distance along the segment, so the crossing is found on the
# segment between the last point outside the square and the point after it
# and translated into the distance ggarrow reads it back from. A path that
# already ends outside the square is not cut; one that lies inside it
# throughout is cut back to its start.
square_end_resect <- function(x, y, cx, cy, half) {
  n <- length(x)
  if (n < 2) {
    return(0)
  }
  dx <- x - cx
  dy <- y - cy
  outside <- pmax(abs(dx), abs(dy)) >= half
  if (outside[[n]]) {
    return(0)
  }
  to_end <- sqrt((x - x[[n]])^2 + (y - y[[n]])^2)
  if (!any(outside)) {
    return(to_end[[1]])
  }

  k <- max(which(outside))
  t <- square_entry(dx[[k]], dy[[k]], dx[[k + 1]], dy[[k + 1]], half)
  to_end[[k]] + t * (to_end[[k + 1]] - to_end[[k]])
}

# Where the segment from (`x0`, `y0`), outside the square of half side `half`
# about the origin, to (`x1`, `y1`), inside it, enters the square, as the
# fraction of the segment. A coordinate enters its band when the segment
# crosses the nearer of the band's two sides, and the point is inside the
# square once both coordinates are.
square_entry <- function(x0, y0, x1, y1, half) {
  entry <- function(c0, c1) {
    if (abs(c1 - c0) < 1e-12) {
      return(-Inf)
    }
    min((half - c0) / (c1 - c0), (-half - c0) / (c1 - c0))
  }
  min(1, max(entry(x0, x1), entry(y0, y1), 0))
}

# The discs, faces, and caps the router is handed for nodes drawn with
# `outline` and `square`, `NA` and `FALSE` where the node's shape is not
# known. A square is cleared around its half diagonal (`r`), so a detour
# passes its corners as clear as it passes a circle, while the ports on its
# faces are placed within its half side (`face`), the flat outline a run
# meets. A circle's face is its radius. An unknown node is the circle of
# `radius` mm the layer was told its nodes are. The `cap` of a node is where
# the edges stop at it: `gap` mm beyond its outline when the edges `follow`
# the nodes, and the layer's single `cap` otherwise, which is what an end
# the user or the plotter fixed is resected by whatever node it meets, so
# that the head zones, arrival arms, and bows the router keeps agree with
# the ink. An unknown node takes the single cap either way. A scene whose
# edges follow the nodes but for those of a layer whose heads are fixed
# stops the edges at a known node at the farther of its own cap and the
# largest cap of those layers, `fixed_cap`.
router_node_geometry <- function(
  outline,
  square,
  gap,
  radius,
  cap,
  follow = TRUE,
  fixed_cap = NULL
) {
  known <- !is.na(outline)
  square <- known & square %in% TRUE
  face <- ifelse(known, outline, radius)
  r <- ifelse(square, outline * sqrt(2), face)
  node_cap <- if (isTRUE(follow)) {
    ifelse(known, outline + gap, cap)
  } else {
    rep_len(cap, length(outline))
  }
  if (isTRUE(follow) && !is.null(fixed_cap)) {
    node_cap[known] <- pmax(node_cap[known], fixed_cap)
  }
  list(r = r, face = face, cap = node_cap, square = square)
}

# The automatic labels ----------------------------------------------------------

# The automatic label layer traces the plot's edges and cuts each one back
# where the drawn edge stops. Where the plot's edges follow the nodes, the
# layer's stat is handed the nodes and the gap those edges stop at, and works
# out the cap at each end of each edge it traces. The plot is held and released
# as the edge layers hold it, and the nodes are let go with the stat's
# parameters once the build finishes.
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
# edge layer that follows them, or of the first node-aware edge layer where
# none does, and the caps the user set at the ends the edge layers leave
# alone. The nodes are found whether or not any end follows them, since a
# routed layer clears every node by its own shape and outline however its
# ends are capped, and a cap the user set wins over the node's at its end.
# `NULL` for a plot whose edges take a single cap, which the label geom is
# given as `edge_cap`.
label_edge_end_caps <- function(layer, plot, layout) {
  if (is.null(plot)) {
    return(NULL)
  }
  aware <- purrr::keep(plot$layers, \(other) isTRUE(other$node_aware_caps))
  if (length(aware) == 0) {
    return(NULL)
  }
  following <- purrr::keep(aware, \(other) length(other$node_cap_ends) > 0)
  edge_layer <- c(following, aware)[[1]]

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
# nodes, and the index of the drawing layer among the plot's layers in
# `layer`. A layer whose ends all follow the nodes has rows as well, so that
# an edge two layers draw is known to be drawn up to the node's own cap by
# one of them. `NULL` when no layer follows the nodes.
set_edge_caps <- function(plot, layout) {
  plot_data <- plot$data
  if (inherits(plot_data, "tidy_dagitty")) {
    plot_data <- pull_dag_data(plot_data)
  }

  caps <- purrr::map(seq_along(plot$layers), \(index) {
    layer <- plot$layers[[index]]
    if (!isTRUE(layer$node_aware_caps)) {
      return(NULL)
    }
    set <- setdiff(c("start_cap", "end_cap"), layer$node_cap_ends)
    data <- resolve_layer_data(layer, plot_data)
    if (is.null(data)) {
      return(NULL)
    }
    # a routed layer carries every row of the plot and marks the rows it
    # draws, and its caps stop those alone
    drawn <- if (".ggdag_draw" %in% names(data)) {
      !is.na(data$.ggdag_draw) & data$.ggdag_draw
    } else {
      rep(TRUE, nrow(data))
    }
    data <- data[drawn & !is.na(data$xend), , drop = FALSE]
    if (nrow(data) == 0) {
      return(NULL)
    }

    edges <- data.frame(
      x = data$x,
      y = data$y,
      xend = data$xend,
      yend = data$yend,
      start = NA_real_,
      end = NA_real_,
      layer = index
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
# millimetres. A ggraph layer's fixed cap is read as it is, and a mapped one
# is evaluated against the rows the way the layer evaluates it. A cap drawn in
# a shape other than a circle is taken as the circle that fits inside it. A
# ggarrow layer's resection is read the same way from its `resect_fins` or
# `resect_head`, as an aesthetic or as the layer's parameter. `NA` for a cap
# that is not a ggraph geometry or a number of millimetres, or is measured in
# units that depend on the device, such as `"npc"` or `"lines"`, which then
# follows the nodes.
set_cap_mm <- function(layer, end, data) {
  if (inherits(layer$geom, dag_arrow_geoms)) {
    return(set_resect_mm(
      layer,
      names(arrow_end_caps)[arrow_end_caps == end],
      data
    ))
  }

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

  radius <- ggraph_cap_radius_mm(cap)
  if (length(radius) == 1 || length(radius) == nrow(data)) {
    return(rep_len(radius, nrow(data)))
  }

  NA_real_
}

# The radius, in millimetres, of the circle that fits inside each ggraph cap
# geometry in `cap`, `NA` for a cap measured in units that depend on the
# device.
ggraph_cap_radius_mm <- function(cap) {
  fields <- unclass(cap)
  width <- fields$width * absolute_unit_mm[fields$width_unit]
  height <- fields$height * absolute_unit_mm[fields$height_unit]
  unname(pmin(width, height)) / 2
}

# The geoms that draw a DAG's edges with ggarrow.
dag_arrow_geoms <- c("GeomDAGArrow", "GeomDAGArrowCurve", "GeomDAGRoutedArrow")

# The resection, in millimetres, a ggarrow `layer` cuts from the `end` (`"fins"`
# or `"head"`) of each edge in `data`: the aesthetic where the layer maps it,
# and the layer's own parameter otherwise.
set_resect_mm <- function(layer, end, data) {
  aesthetic <- paste0("resect_", end)
  resect <- layer$aes_params[[aesthetic]]
  if (is.null(resect) && !is.null(layer$mapping[[aesthetic]])) {
    resect <- tryCatch(
      rlang::eval_tidy(layer$mapping[[aesthetic]], data = data),
      error = function(cnd) NULL
    )
  }
  resect <- resect %||% layer$geom_params$resect[[end]]
  if (grid::is.unit(resect)) {
    if (!all(grid::unitType(resect) %in% names(absolute_unit_mm))) {
      return(NA_real_)
    }
    resect <- grid::convertWidth(resect, "mm", valueOnly = TRUE)
  }
  if (!is.numeric(resect)) {
    return(NA_real_)
  }
  if (length(resect) == 1 || length(resect) == nrow(data)) {
    return(rep_len(as.numeric(resect), nrow(data)))
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

# The nodes ---------------------------------------------------------------------

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
# in, numbered as in `layout`, its position, its outline in millimetres, and
# whether it is a square. `NULL` for a plot that draws no nodes, or whose node
# layers cannot be built, which the build of the whole plot then reports.
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
    shape <- data$shape %||% 19
    size <- data$size %||% 16
    data.frame(
      panel = panels[match(as.character(data$PANEL), names(panels))],
      x = data$x,
      y = data$y,
      outline = node_outline_mm(shape, size),
      square = rep_len(node_is_square(shape), nrow(data))
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

# The node drawn at each (`x`, `y`) in panel `panel`: its `outline` in
# millimetres and whether it is a `square`, the widest where node layers
# overlap there, a square before a circle of the same outline, and `NA` with
# `FALSE` where no node is drawn.
edge_end_nodes <- function(panel, x, y, nodes) {
  outline <- rep(NA_real_, length(x))
  square <- rep(FALSE, length(x))
  if (is.null(nodes) || nrow(nodes) == 0) {
    return(data.frame(outline = outline, square = square))
  }

  panel <- as.character(panel)
  scale <- max(1, abs(c(nodes$x, nodes$y)), na.rm = TRUE)
  tolerance <- sqrt(.Machine$double.eps) * scale

  for (i in seq_along(x)) {
    here <- nodes$panel == panel[[i]] &
      abs(nodes$x - x[[i]]) <= tolerance &
      abs(nodes$y - y[[i]]) <= tolerance
    here <- which(here %in% TRUE)
    if (length(here) > 0) {
      widest <- here[order(-nodes$outline[here], !nodes$square[here])][[1]]
      outline[[i]] <- nodes$outline[[widest]]
      square[[i]] <- nodes$square[[widest]]
    }
  }

  data.frame(outline = outline, square = square)
}

# The cap, in millimetres, at the start (`start`) and the end (`end`) of the
# edge each row of `points` traces, for the automatic label stat, whether each
# is the half side of a square (`start_square`, `end_square`) rather than a
# straight-line distance, and the cap (`fallback`) of an end with no node
# drawn at it, which the label geom cuts an edge without a cap of its own by.
# `edges` are the edges the stat traced, one row each, and `caps` what
# `label_edge_end_caps()` found, or `NULL`. An end the user set a cap at is
# cut by that cap. Every tracer names the points of an edge by an id that
# starts with the edge's key, so a point finds its edge by that key and its
# panel. Two layers can draw an edge between the same two nodes, a directed
# edge and a bidirected arc say, with caps of their own, so a point traced
# from the edges of a layer, which carries that layer's index in
# `route_layer`, takes the cap that layer sets. A point traced as a chord is
# traced once for every straight layer that draws it, the layers no traced
# point names, and takes at each end the nearest cap among them, the node's
# own where one of them follows the node there and no set cap is nearer:
# the label keeps clear of the ink of all of them, which reaches out to the
# nearest. An edge with no caps found here takes the label geom's single cap,
# so its caps are `NA`.
traced_edge_caps <- function(edges, points, caps) {
  none <- rep(NA_real_, nrow(points))
  no_square <- rep(FALSE, nrow(points))
  if (is.null(caps) || nrow(edges) == 0) {
    return(list(
      start = none,
      end = none,
      start_square = no_square,
      end_square = no_square,
      fallback = none
    ))
  }

  start <- edge_end_nodes(edges$PANEL, edges$x, edges$y, caps$nodes)
  end <- edge_end_nodes(edges$PANEL, edges$xend, edges$yend, caps$nodes)
  start_cap <- start$outline
  end_cap <- end$outline
  start_cap[is.na(start_cap)] <- caps$fallback
  end_cap[is.na(end_cap)] <- caps$fallback
  start_cap <- start_cap + caps$gap
  end_cap <- end_cap + caps$gap
  start_square <- start$square
  end_square <- end$square

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
  start_cap <- start_cap[at]
  end_cap <- end_cap[at]
  start_square <- start_square[at] %in% TRUE
  end_square <- end_square[at] %in% TRUE

  if (!is.null(caps$set)) {
    point_layer <- spec_column(points, "route_layer", NA_integer_)
    traced_layers <- unique(point_layer[!is.na(point_layer)])
    set_layer <- spec_column(caps$set, "layer", NA_integer_)
    set_keys <- edge_key(caps$set$x, caps$set$y, caps$set$xend, caps$set$yend)

    # a point a layer traced takes the caps that layer sets
    set_at <- match(
      paste(point_keys, point_layer),
      paste(set_keys, set_layer)
    )
    set_at[is.na(point_layer)] <- NA_integer_
    set_start <- caps$set$start[set_at]
    set_end <- caps$set$end[set_at]

    # a chord takes the nearest cap of the straight layers that draw it
    straight <- !(set_layer %in% traced_layers)
    chord <- is.na(point_layer)
    if (any(straight) && any(chord)) {
      nearest <- function(values) {
        found <- !is.na(values)
        list(
          set = tapply(
            ifelse(found, values, Inf)[straight],
            set_keys[straight],
            min
          ),
          follows = tapply(!found[straight], set_keys[straight], any)
        )
      }
      chord_cap <- function(values, node_cap) {
        drawn <- nearest(values)
        at <- match(point_keys[chord], names(drawn$set))
        set <- unname(drawn$set[at])
        follows <- unname(drawn$follows[at]) %in% TRUE
        nearer <- is.finite(set) & (!follows | set < node_cap[chord])
        ifelse(nearer, set, NA_real_)
      }
      set_start[chord] <- chord_cap(caps$set$start, start_cap)
      set_end[chord] <- chord_cap(caps$set$end, end_cap)
    }

    start_cap[!is.na(set_start)] <- set_start[!is.na(set_start)]
    end_cap[!is.na(set_end)] <- set_end[!is.na(set_end)]
    start_square[!is.na(set_start)] <- FALSE
    end_square[!is.na(set_end)] <- FALSE
  }

  list(
    start = start_cap,
    end = end_cap,
    start_square = start_square,
    end_square = end_square,
    fallback = rep(caps$fallback + caps$gap, nrow(points))
  )
}
