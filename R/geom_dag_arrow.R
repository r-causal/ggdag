# Caching environment for lazy ggproto objects.
# ggarrow is in Suggests, so ggproto classes that inherit from it
# can only be created after ggarrow is installed and loaded.
the <- new.env(parent = emptyenv())

# Stat: filter tidy_dagitty data to edge rows --------------------------------

StatDAGArrowEdges <- ggplot2::ggproto(
  "StatDAGArrowEdges",
  ggplot2::Stat,
  # `curvature` and `unset` say how the layer bends its edges, and the arc
  # constructor passes them to the geom and to the stat alike. A layer that
  # draws chords passes neither, and leaves the panel to the endpoints.
  compute_panel = function(data, scales, curvature = NULL, unset = "chord") {
    data <- data[!is.na(data$xend), , drop = FALSE]
    if (is.null(curvature)) {
      return(data)
    }
    reserve_curved_edge_room(data, scales, curvature, unset)
  },
  required_aes = c("x", "y", "xend", "yend"),
  optional_aes = "edge_curvature"
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
        grob <- ggplot2::ggproto_parent(
          ggarrow::GeomArrowSegment,
          self
        )$draw_panel(
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
        square_end_grob(
          grob,
          data,
          coord$transform(data[c("x", "y", "xend", "yend")], panel_params),
          units = "npc"
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
        curvature = 0.3,
        angle = 90,
        ncp = 5,
        sep = 0,
        unset = "chord"
      ) {
        resect <- inject_dag_resect(resect, data)

        draw_parent <- function(data, curvature) {
          grob <- ggplot2::ggproto_parent(
            ggarrow::GeomArrowCurve,
            self
          )$draw_panel(
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
          square_end_grob(
            grob,
            data,
            coord$transform(data[c("x", "y", "xend", "yend")], panel_params),
            units = "npc"
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

        # What an unset row means is the layer's to say. A directed layer
        # draws it as a chord, so curving one edge does not bend the others.
        # A bidirected layer draws it at the layer's own curvature, because
        # the arc is how a bidirected edge is read and curving one of them
        # must not flatten the rest.
        data$edge_curvature[is.na(data$edge_curvature)] <- if (
          identical(unset, "curvature")
        ) {
          curvature
        } else {
          0
        }

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
      ggplot2::Geom,
      required_aes = c("x", "y"),
      optional_aes = c(
        "xend",
        "yend",
        "edge_curvature",
        "draw",
        "routed",
        "linewidth_head",
        "linewidth_fins",
        "arrow_head",
        "arrow_fins",
        "arrow_mid",
        "resect_head",
        "resect_fins"
      ),
      # the layer carries every node of the panel so that the router can
      # treat them all as obstacles, and a node row has no `xend`
      non_missing_aes = character(),
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
      draw_key = ggarrow::draw_key_arrow,
      # the line width the router keeps room for the layer's ornaments at is
      # the one the layer sets, or the geom's default, and never a width
      # mapped per edge: the automatic label engine routes the layer's edges
      # again from what it can read before the plot is drawn, and a mapped
      # width is known only after the scales have been trained
      setup_params = function(self, data, params) {
        params$ornament_linewidth <- routed_linewidth(
          params$linewidth,
          self$default_aes$linewidth
        )
        params
      },
      draw_panel = function(
        self,
        data,
        panel_params,
        coord,
        route = "spline",
        edge_route_options = NULL,
        layer_axis = "auto",
        node_size = NULL,
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
        ornament_linewidth = 1,
        lineend = "butt",
        linejoin = "round",
        linemitre = 10,
        na.rm = FALSE
      ) {
        if (!coord$is_linear()) {
          warn(
            c(
              "Routed edges are drawn in linear coordinates only.",
              "x" = "This coordinate system bends the panel, so the detours it draws would mean nothing.",
              "i" = "Use {.fn ggplot2::coord_cartesian} or {.fn ggplot2::coord_fixed} to route these edges."
            ),
            warning_class = "ggdag_routed_coord_warning"
          )
        }

        resect <- inject_dag_resect(resect, data)

        starts <- coord$transform(data[c("x", "y")], panel_params)
        has_end <- !is.na(data$xend) & !is.na(data$yend)
        ends <- coord$transform(
          data.frame(x = data$xend[has_end], y = data$yend[has_end]),
          panel_params
        )

        start_keys <- routed_position_keys(starts$x, starts$y)
        end_keys <- routed_position_keys(ends$x, ends$y)

        # the obstacle nodes come from the helper the automatic label stat
        # collects its own from, so the two layers hand the router the same
        # node set in the same order
        panel <- data.frame(x = starts$x, y = starts$y)
        panel$xend <- NA_real_
        panel$yend <- NA_real_
        panel$xend[has_end] <- ends$x
        panel$yend[has_end] <- ends$y
        nodes <- panel_node_centers(panel)
        nodes$name <- routed_position_keys(nodes$x, nodes$y)
        nodes <- nodes[c("name", "x", "y")]
        nodes <- routed_node_shapes(nodes, data, start_keys, end_keys, has_end)

        drawn <- if ("draw" %in% names(data)) {
          !is.na(data$draw) & data$draw
        } else {
          rep(TRUE, nrow(data))
        }
        # in orthogonal mode the layer routes the edges the plot's other
        # routed layers draw with the same settings as well, so that the
        # rows and ports of one scene are shared out once: two layers
        # routed apart would draw two heads on one row. The other modes
        # route what they draw. `routed` names the layer that draws each
        # row of the scene another layer draws, and is 0 elsewhere
        union <- identical(route, "orthogonal") && "routed" %in% names(data)
        routed_by <- if (union) as.numeric(data$routed) else NULL
        routed <- if (union) {
          drawn | (!is.na(routed_by) & routed_by > 0)
        } else {
          drawn
        }
        keep <- which(has_end & routed)
        if (length(keep) == 0 || !any(drawn[keep])) {
          return(ggplot2::zeroGrob())
        }

        end_index <- match(keep, which(has_end))
        edges <- data[keep, , drop = FALSE]
        edges$x <- starts$x[keep]
        edges$y <- starts$y[keep]
        edges$xend <- ends$x[end_index]
        edges$yend <- ends$y[end_index]
        edges$.ggdag_from <- start_keys[keep]
        edges$.ggdag_to <- end_keys[end_index]
        edges$.ggdag_draw <- drawn[keep]

        grid::gTree(
          nodes = nodes,
          edges = edges,
          params = list(
            route = route,
            edge_route_options = edge_route_options,
            layer_axis = layer_axis,
            node_size = node_size %||% ggdag_option("node_size"),
            node_gap = data$.ggdag_node_gap[1] %||% node_edge_gap_mm,
            arrow = arrow,
            length = length,
            justify = justify,
            force_arrow = force_arrow,
            mid_place = mid_place,
            resect = resect,
            ornament_linewidth = ornament_linewidth,
            lineend = lineend,
            linejoin = linejoin,
            linemitre = linemitre,
            # whether another routed layer draws edges of the scene in this
            # panel as well
            union_layers = union &&
              any(!drawn[keep] & !is.na(routed_by[keep]) & routed_by[keep] > 0)
          ),
          cl = "dag_routed_edges"
        )
      }
    )
  }
  the$GeomDAGRoutedArrow
}

# How finely an arc the user set is sampled before the router is shown where
# it goes. The label engine builds the same path from the same millimetres,
# so both grobs price their detours against one polyline.
routed_fixed_path_n <- 32

# A node's identity within one panel, from the position it is drawn at. The
# endpoints of an edge are the coordinates of the nodes it runs between, so
# the same node reaches the router under one name however many edges mention
# it.
routed_position_keys <- function(x, y) {
  paste(sprintf("%.12g", x), sprintf("%.12g", y))
}

#' Route and draw the edges of one panel
#'
#' Runs at draw time, inside the panel viewport, where positions in npc
#' convert to true millimetres: it calls `route_edges_mm()` on the node discs
#' and edge chords of the panel and emits the routed paths as one ggarrow
#' arrow grob, plus one curve grob per curvature the user set.
#'
#' @param x A `dag_routed_edges` gTree built by
#'   `GeomDAGRoutedArrow$draw_panel()`.
#' @return `x`, with children set to the drawn grobs.
#' @exportS3Method grid::makeContent
#' @noRd
makeContent.dag_routed_edges <- function(x) {
  nodes <- x$nodes
  edges <- x$edges
  par <- x$params

  mm_x <- function(value) {
    if (length(value) == 0) {
      return(numeric())
    }
    grid::convertX(grid::unit(value, "npc"), "mm", valueOnly = TRUE)
  }
  mm_y <- function(value) {
    if (length(value) == 0) {
      return(numeric())
    }
    grid::convertY(grid::unit(value, "npc"), "mm", valueOnly = TRUE)
  }
  panel_width <- grid::convertWidth(
    grid::unit(1, "npc"),
    "mm",
    valueOnly = TRUE
  )
  panel_height <- grid::convertHeight(
    grid::unit(1, "npc"),
    "mm",
    valueOnly = TRUE
  )

  # the router clears the disc of every node and stops each edge at the node's
  # own cap where the heads follow the nodes, a circle of the layer's node
  # size where a node's shape is not known; the layer's single cap is the
  # reference the router's constants are set from, and the cap of every node
  # where the heads were fixed by the user or the plotter
  radius <- node_radius_mm(par$node_size)
  cap <- routed_cap_mm(edges, par$resect)
  geometry <- router_node_geometry(
    nodes$outline %||% rep(NA_real_, nrow(nodes)),
    nodes$square %||% rep(FALSE, nrow(nodes)),
    par$node_gap %||% node_edge_gap_mm,
    radius,
    cap,
    follow = isTRUE(edges$.ggdag_follow_head[1])
  )
  nodes_mm <- data.frame(
    name = nodes$name,
    x = mm_x(nodes$x),
    y = mm_y(nodes$y),
    r = geometry$r,
    face = geometry$face,
    cap = geometry$cap,
    square = geometry$square,
    stringsAsFactors = FALSE
  )

  start_x <- mm_x(edges$x)
  start_y <- mm_y(edges$y)
  end_x <- mm_x(edges$xend)
  end_y <- mm_y(edges$yend)

  curvature <- routed_edge_curvature(edges)
  is_arc <- !is.na(curvature) & curvature != 0

  edge_input <- data.frame(
    from = edges$.ggdag_from,
    to = edges$.ggdag_to,
    curvature = curvature,
    stringsAsFactors = FALSE
  )
  # an arc the user asked for is never rerouted, but the router still has to
  # see where it goes, so it arrives as a placed obstacle rather than a chord
  if (any(is_arc)) {
    edge_input$fixed_path <- lapply(seq_len(nrow(edge_input)), function(i) {
      if (!is_arc[[i]]) {
        return(NULL)
      }
      sample_curved_edge(
        start_x[[i]],
        start_y[[i]],
        end_x[[i]],
        end_y[[i]],
        curvature = curvature[[i]],
        n = routed_fixed_path_n
      )
    })
  }

  # the router keeps the run at each end long enough for the ornament drawn
  # there, so it is told how far the layer's heads and fins reach
  reach <- routed_ornament_reaches(par, par$ornament_linewidth)
  routed <- route_edges_mm(
    nodes = nodes_mm,
    edges = edge_input,
    bounds = c(0, 0, panel_width, panel_height),
    cap = cap,
    mode = par$route,
    opts = route_opts_from(
      par$edge_route_options,
      radius,
      layer_axis = par$layer_axis %||% "auto",
      head_reach = reach$head,
      fins_reach = reach$fins,
      narrow_rows = isTRUE(par$union_layers)
    )
  )

  # the rows the layer routes but does not draw are the edges of the other
  # routed layers of the plot, placed here so that the layers agree
  drawn <- edges$.ggdag_draw %||% rep(TRUE, nrow(edges))
  drawn <- !is.na(drawn) & drawn
  children <- list()
  paths <- which(!is_arc & drawn)
  if (length(paths) > 0) {
    children <- c(
      children,
      list(routed_arrow_grob(
        edges[paths, , drop = FALSE],
        routed$paths[paths],
        par,
        routed$meta[paths, , drop = FALSE],
        nodes_mm
      ))
    )
  }
  for (group in split(which(is_arc & drawn), curvature[is_arc & drawn])) {
    arc_edges <- edges[group, , drop = FALSE]
    children <- c(
      children,
      list(square_end_grob(
        routed_curve_grob(
          arc_edges,
          start_x[group],
          start_y[group],
          end_x[group],
          end_y[group],
          curvature[[group[[1]]]],
          par
        ),
        arc_edges,
        ends = data.frame(
          x = start_x[group],
          y = start_y[group],
          xend = end_x[group],
          yend = end_y[group]
        ),
        units = "mm"
      ))
    )
  }

  children <- children[!vapply(children, is.null, logical(1))]
  grid::setChildren(x, do.call(grid::gList, children))
}

# The curvature each drawn edge asks for: `NA` to route, `0` to stay straight
# through whatever sits on the chord, and any other number to follow that arc.
routed_edge_curvature <- function(edges) {
  if (!"edge_curvature" %in% names(edges)) {
    return(rep(NA_real_, nrow(edges)))
  }
  curvature <- edges$edge_curvature
  if (!is.numeric(curvature)) {
    abort(
      "{.field edge_curvature} must be numeric, not {.cls {class(curvature)}}.",
      error_class = "ggdag_type_error"
    )
  }
  as.numeric(curvature)
}

# The single cap, in millimetres, the router sets its constants from and
# stops an edge at where the node there is not known: the layer's own head
# resection, which follows the plot's node size, or the `ggdag.edge_cap`
# option where the layer has none. A `resect_head` the user mapped per edge
# overrides the layer parameter, so the longest of the mapped values stands
# instead; a `resect_head` the layer wrote itself to follow the nodes is the
# per-node cap the router reads from the nodes, and is passed over here.
routed_cap_mm <- function(edges, resect) {
  mapped <- if (isTRUE(edges$.ggdag_follow_head[1])) NULL else edges$resect_head
  cap <- mapped %||% resect$head %||% ggdag_option("edge_cap", 8)
  cap <- suppressWarnings(as.numeric(cap))
  cap <- cap[is.finite(cap)]
  if (length(cap) == 0) {
    return(0)
  }
  max(cap)
}

# How far the ornament ggarrow draws at the end of a path reaches back along
# it from the point the layer cuts the path at, in millimetres, for a shaft
# `width` mm wide and an ornament `length` (a multiple of the width, or a
# unit). ggarrow scales a matrix ornament to the larger of the length and the
# width over the shape's height, takes its reach from the shape's own
# `resect` and `length` attributes or its extent along x, and cuts the path
# back by that reach times the part of the ornament that lies beyond the
# cut, `1 - justify`. A function ornament declares its reach in the
# `resect` attribute of what it draws; a list of ornaments, one per edge,
# reaches as far as the farthest of them. No ornament reaches at all.
arrow_ornament_reach_mm <- function(ornament, length, width, justify = 0) {
  if (is.null(ornament)) {
    return(0)
  }
  if (is.list(ornament) && !is.matrix(ornament)) {
    reaches <- vapply(
      ornament,
      function(one) arrow_ornament_reach_mm(one, length, width, justify),
      numeric(1)
    )
    return(if (length(reaches) == 0) 0 else max(reaches))
  }
  length_mm <- if (grid::is.unit(length)) {
    unit_length_mm(length) %||% (4 * width)
  } else {
    (length %||% 4) * width
  }
  reach <- if (is.function(ornament)) {
    drawn <- tryCatch(
      ornament(length = length_mm, width = width, resect = 0),
      error = function(e) NULL
    )
    attr(drawn, "resect") %||% 0
  } else {
    scale <- max(length_mm, width / diff(range(ornament[, "y"])))
    (attr(ornament, "length") %||% max(ornament[, "x"])) *
      scale *
      (attr(ornament, "resect") %||% 1)
  }
  reach * (1 - (justify %||% 0))
}

# A length given as a grid unit, in millimetres. An absolute unit converts
# on its own, so the label engine can ask before any device is open without
# opening one; a unit relative to a viewport needs the device the layer is
# drawn on, and is `NULL` when none is open yet.
unit_length_mm <- function(length) {
  mm_per_unit <- c(
    mm = 1,
    cm = 10,
    inches = 25.4,
    points = 25.4 / 72.27,
    bigpts = 25.4 / 72,
    picas = 12 * 25.4 / 72.27,
    dida = 25.4 / 72.27 * 1238 / 1157,
    cicero = 12 * 25.4 / 72.27 * 1238 / 1157
  )
  type <- grid::unitType(length)
  if (all(type %in% names(mm_per_unit))) {
    return(max(as.numeric(length) * mm_per_unit[type]))
  }
  if (grDevices::dev.cur() == 1L) {
    return(NULL)
  }
  max(grid::convertWidth(length, "mm", valueOnly = TRUE))
}

# The reach of the head and of the fins a routed layer draws, from the
# layer's parameters `par` (`arrow`, `length`, and `justify`) and the line
# widths of its edges, in the ggplot2 `linewidth` scale: the widest shaft
# carries the largest ornament, and the router keeps room for that one.
# The routed grob and the label engine both measure at the single width
# `routed_linewidth()` gives the layer.
routed_ornament_reaches <- function(par, linewidth) {
  linewidth <- suppressWarnings(as.numeric(linewidth))
  linewidth <- linewidth[is.finite(linewidth)]
  width <- (if (length(linewidth) == 0) 1 else max(linewidth)) * .pt / .stroke
  list(
    head = arrow_ornament_reach_mm(
      par$arrow$head,
      par$length$head,
      width,
      par$justify
    ),
    fins = arrow_ornament_reach_mm(
      par$arrow$fins,
      par$length$fins,
      width,
      par$justify
    )
  )
}

# The line width a routed layer's ornaments are measured at for the router:
# the width the layer sets, `set`, or the geom's default where it sets none
# or sets one that is not a number.
routed_linewidth <- function(set, default = 1) {
  width <- suppressWarnings(as.numeric(set))
  width <- width[is.finite(width)]
  if (length(width) == 0) {
    width <- suppressWarnings(as.numeric(default))
    width <- width[is.finite(width)]
  }
  if (length(width) == 0) 1 else max(width)
}

# The ornaments a routed layer draws, as the label engine is handed them to
# measure their reach when the plot is drawn, where a length relative to the
# panel converts as it does for the drawn grob: the layer's `arrow`, `length`,
# and `justify`, and the line width the geom measures them at.
routed_layer_ornaments <- function(layer) {
  params <- layer$geom_params
  list(
    arrow = params$arrow,
    length = params$length,
    justify = params$justify,
    linewidth = routed_linewidth(
      layer$aes_params$linewidth,
      layer$geom$default_aes$linewidth
    )
  )
}

# The routed layer's nodes with the outline and shape each is drawn with, as
# the layer's rows carry them for the node at each of their ends, keyed by
# position: `outline` in millimetres, `NA` for a node whose shape the rows do
# not carry, and whether the node is a `square`. `start_keys` name the node
# at the start of every row and `end_keys` the node at the end of the rows
# in `has_end`.
routed_node_shapes <- function(nodes, data, start_keys, end_keys, has_end) {
  nodes$outline <- rep(NA_real_, nrow(nodes))
  nodes$square <- rep(FALSE, nrow(nodes))
  if (!all(c(".ggdag_node_fins", ".ggdag_node_head") %in% names(data))) {
    return(nodes)
  }

  found <- data.frame(
    key = c(start_keys, end_keys),
    outline = c(data$.ggdag_node_fins, data$.ggdag_node_head[has_end]),
    square = c(
      data$.ggdag_node_square_fins,
      data$.ggdag_node_square_head[has_end]
    )
  )
  found <- found[!is.na(found$outline), , drop = FALSE]
  found <- found[order(-found$outline, !found$square), , drop = FALSE]
  at <- match(nodes$name, found$key)
  known <- !is.na(at)
  nodes$outline[known] <- found$outline[at[known]]
  nodes$square[known] <- found$square[at[known]]
  nodes
}

# The same millimetres, computed from a routed layer before it is drawn, so
# that the routing spec the label engine is given names the cap the edges are
# drawn with. The layer's own parameters are read the way `draw_panel()` reads
# them, including a `resect_head` mapped per edge. A resection the layer has
# not settled yet is resolved from the whole plot, the way the routed layer
# resolves it at build, so that the answer does not depend on whether the
# label layer or the node layer was added first.
routed_layer_cap_mm <- function(layer, layer_data, plot = NULL) {
  mapped <- layer$mapping$resect_head
  head <- if (!is.null(mapped)) {
    tryCatch(
      rlang::eval_tidy(mapped, data = layer_data),
      error = function(e) NULL
    )
  } else if ("resect_head" %in% names(layer_data)) {
    layer_data$resect_head
  } else {
    NULL
  }
  if (!is.numeric(head)) {
    head <- NULL
  }

  edges <- list(resect_head = head)
  params <- layer$geom_params$resect %||% list(head = NULL, fins = NULL)
  if (is.null(params$head) && !is.null(plot)) {
    discovered <- discover_node_size(plot)
    if (!is.null(discovered)) {
      params$head <- node_size_to_cap(discovered)
    }
  }
  resect <- inject_dag_resect(params, edges)
  routed_cap_mm(edges, resect)
}

# A value that may already be a unit, as a unit of `units`.
routed_unit <- function(value, units) {
  if (grid::is.unit(value)) value else grid::unit(value, units)
}

# A resect per edge in millimetres: the value the layer resects by, read as
# millimetres the way `routed_cap_mm()` reads it, plus the extra arc length
# the router measured for that edge's port.
routed_resect_mm <- function(value, extra) {
  base <- suppressWarnings(as.numeric(value))
  base[!is.finite(base)] <- 0
  rep_len(base, length(extra)) + extra
}

# The routed and straight paths of a panel as one ggarrow arrow grob. This is
# `ggarrow::GeomArrow$draw_panel()` with the panel's native units replaced by
# the millimetres the router works in: per-edge colour, alpha, width, line
# type, ornaments, and resection reach ggarrow exactly as they always do.
# When the router reports a resect per edge (`meta$resect_head` and
# `meta$resect_fins`, in orthogonal mode), each end is resected by that arc
# length instead: an offset port's path ends on the port's own line at the
# node's coordinate, so the resect is shortened by the run hidden under the
# disc and the tip sits the same distance past the disc face as a centre
# port's, with the head drawn along the run; a resect the user mapped or set
# is moved by the same amount the router moved the node's cap, which `nodes`
# carry. An end that follows a square node is resected where its path, known
# here in millimetres, crosses the square its tip lies on.
routed_arrow_grob <- function(edges, paths, par, meta = NULL, nodes = NULL) {
  n_points <- vapply(paths, nrow, integer(1))
  drawable <- n_points >= 2
  edges <- edges[drawable, , drop = FALSE]
  paths <- paths[drawable]
  n_points <- n_points[drawable]
  if (nrow(edges) == 0) {
    return(NULL)
  }
  resect_head <- edges$resect_head %||% par$resect$head
  resect_fins <- edges$resect_fins %||% par$resect$fins
  if (!is.null(meta) && !is.null(meta$resect_head)) {
    meta <- meta[drawable, , drop = FALSE]
    cap <- routed_cap_mm(edges, par$resect)
    cap_head <- nodes$cap[match(edges$.ggdag_to, nodes$name)] %||% cap
    cap_fins <- nodes$cap[match(edges$.ggdag_from, nodes$name)] %||% cap
    cap_head[is.na(cap_head)] <- cap
    cap_fins[is.na(cap_fins)] <- cap
    resect_head <- routed_resect_mm(resect_head, meta$resect_head - cap_head)
    resect_fins <- routed_resect_mm(resect_fins, meta$resect_fins - cap_fins)
  }
  if (!is.null(nodes)) {
    head_at <- match(edges$.ggdag_to, nodes$name)
    fins_at <- match(edges$.ggdag_from, nodes$name)
    resect_head <- square_path_resects(
      paths,
      rep_len(resect_head, nrow(edges)),
      edges$.ggdag_square_head %in% TRUE,
      nodes$x[head_at],
      nodes$y[head_at],
      reverse = FALSE
    )
    resect_fins <- square_path_resects(
      paths,
      rep_len(resect_fins, nrow(edges)),
      edges$.ggdag_square_fins %in% TRUE,
      nodes$x[fins_at],
      nodes$y[fins_at],
      reverse = TRUE
    )
  }

  id <- rep(seq_along(paths), n_points)
  width <- grid::unit(rep(edges$linewidth, n_points) * .pt / .stroke, "mm")
  last <- cumsum(n_points)
  first <- c(1L, last[-length(last)] + 1L)

  length_head <- par$length$head
  if (!grid::is.unit(length_head)) {
    length_head <- (length_head %||% 4) * width[last]
  }
  length_fins <- par$length$fins
  if (!grid::is.unit(length_fins)) {
    length_fins <- (length_fins %||% 4) * width[first]
  }

  ggarrow::grob_arrow(
    x = grid::unit(unlist(lapply(paths, function(p) p$x)), "mm"),
    y = grid::unit(unlist(lapply(paths, function(p) p$y)), "mm"),
    id = id,
    arrow_head = edges$arrow_head %||% par$arrow$head,
    arrow_fins = edges$arrow_fins %||% par$arrow$fins,
    arrow_mid = edges$arrow_mid %||% par$arrow$mid,
    length_head = length_head,
    length_fins = length_fins,
    length_mid = par$length$mid %||% 4,
    justify = par$justify,
    force_arrow = par$force_arrow,
    mid_place = par$mid_place,
    shaft_width = width,
    resect_head = routed_unit(resect_head, "mm"),
    resect_fins = routed_unit(resect_fins, "mm"),
    gp = grid::gpar(
      col = edges$stroke_colour,
      fill = alpha(edges$colour, edges$alpha),
      lty = edges$linetype,
      lwd = edges$stroke_width * .pt,
      linejoin = par$linejoin,
      linemitre = par$linemitre,
      lineend = par$lineend
    )
  )
}

# The edges of one curvature as an ggarrow curve grob, so that an arc the user
# set is drawn exactly as the un-routed arc layer draws it.
routed_curve_grob <- function(edges, x, y, xend, yend, curvature, par) {
  head_width <- grid::unit(
    (edges$linewidth_head %||% edges$linewidth) * .pt / .stroke,
    "mm"
  )
  fins_width <- grid::unit(
    (edges$linewidth_fins %||% edges$linewidth) * .pt / .stroke,
    "mm"
  )

  length_head <- par$length$head
  if (!grid::is.unit(length_head)) {
    length_head <- (length_head %||% 4) * head_width
  }
  length_fins <- par$length$fins
  if (!grid::is.unit(length_fins)) {
    length_fins <- (length_fins %||% 4) * fins_width
  }

  ggarrow::grob_arrow_curve(
    grid::unit(x, "mm"),
    grid::unit(y, "mm"),
    grid::unit(xend, "mm"),
    grid::unit(yend, "mm"),
    curvature = curvature,
    angle = 90,
    ncp = 5,
    square = FALSE,
    squareShape = 1,
    inflect = FALSE,
    open = TRUE,
    arrow_head = edges$arrow_head %||% par$arrow$head,
    arrow_fins = edges$arrow_fins %||% par$arrow$fins,
    arrow_mid = edges$arrow_mid %||% par$arrow$mid,
    length_head = length_head,
    length_fins = length_fins,
    length_mid = par$length$mid %||% 4,
    justify = par$justify,
    force_arrow = par$force_arrow,
    mid_place = par$mid_place,
    width_head = head_width,
    width_fins = fins_width,
    resect_head = routed_unit(edges$resect_head %||% par$resect$head, "mm"),
    resect_fins = routed_unit(edges$resect_fins %||% par$resect$fins, "mm"),
    gp = grid::gpar(
      col = edges$stroke_colour,
      fill = alpha(edges$colour, edges$alpha),
      lty = edges$linetype,
      lwd = edges$stroke_width * .pt,
      linejoin = par$linejoin,
      linemitre = par$linemitre,
      lineend = par$lineend
    )
  )
}

# Square ends settled at draw time --------------------------------------------

# The resection of each path in `paths` (data frames of `x` and `y` in
# millimetres) at one end: `resect` as it stands, the half side of a square
# for the ends in `square`, which is turned into the straight-line resection
# that puts the tip on the square about the node centre (`cx`, `cy`). The end
# is the finish of the path, or its start when `reverse` is set.
square_path_resects <- function(paths, resect, square, cx, cy, reverse) {
  for (i in which(square & !is.na(cx))) {
    x <- paths[[i]]$x
    y <- paths[[i]]$y
    if (reverse) {
      x <- rev(x)
      y <- rev(y)
    }
    resect[[i]] <- square_end_resect(x, y, cx[[i]], cy[[i]], resect[[i]])
  }
  resect
}

# The ggarrow grob `grob` drawn for the rows `data`, wrapped so that an end
# following a square node is resected where its drawn path crosses the square
# its tip lies on, whose half side the grob holds as that end's resection
# until then. `ends` holds the start and end of each row's chord in `units`,
# which are the centres of the nodes there. A grob whose ends all follow
# circles, or none, is returned as it is: a circle's resection is the same
# straight-line distance whatever angle the edge arrives at.
square_end_grob <- function(grob, data, ends, units) {
  head <- data$.ggdag_square_head %in% TRUE
  fins <- data$.ggdag_square_fins %in% TRUE
  if (!any(head | fins)) {
    return(grob)
  }

  grid::gTree(
    arrow = grob,
    ends = data.frame(
      x = ends$x,
      y = ends$y,
      xend = ends$xend,
      yend = ends$yend,
      head = head,
      fins = fins
    ),
    units = units,
    cl = "dag_square_ends"
  )
}

#' Resect the ends of a drawn ggarrow grob at the squares they meet
#'
#' Runs at draw time, inside the panel viewport, where the paths ggarrow is
#' about to draw are known in millimetres. Each path is matched to the row it
#' draws by its ends, and an end marked as meeting a square is resected where
#' the path crosses the square of the half side the row carries, centred on
#' the node there.
#'
#' @param x A `dag_square_ends` gTree built by `square_end_grob()`.
#' @return `x`, with the resected arrow grob as its child.
#' @exportS3Method grid::makeContent
#' @noRd
makeContent.dag_square_ends <- function(x) {
  arrow <- x$arrow
  ends <- x$ends
  drawn <- arrow_grob_paths_mm(arrow)
  paths <- drawn$paths
  n <- length(paths)
  if (n == 0) {
    return(grid::setChildren(x, grid::gList(arrow)))
  }

  from_x <- grid::convertX(grid::unit(ends$x, x$units), "mm", valueOnly = TRUE)
  from_y <- grid::convertY(grid::unit(ends$y, x$units), "mm", valueOnly = TRUE)
  to_x <- grid::convertX(grid::unit(ends$xend, x$units), "mm", valueOnly = TRUE)
  to_y <- grid::convertY(grid::unit(ends$yend, x$units), "mm", valueOnly = TRUE)

  # the row each path draws, by the ends both share
  row <- vapply(
    paths,
    function(path) {
      last <- length(path$x)
      off <- (from_x - path$x[[1]])^2 +
        (from_y - path$y[[1]])^2 +
        (to_x - path$x[[last]])^2 +
        (to_y - path$y[[last]])^2
      which.min(off)
    },
    integer(1)
  )
  head <- rep_len(drawn$head, n)
  fins <- rep_len(drawn$fins, n)
  head <- square_path_resects(
    paths,
    head,
    ends$head[row],
    to_x[row],
    to_y[row],
    reverse = FALSE
  )
  fins <- square_path_resects(
    paths,
    fins,
    ends$fins[row],
    from_x[row],
    from_y[row],
    reverse = TRUE
  )
  arrow <- set_arrow_grob_resects(arrow, head, fins)
  grid::setChildren(x, grid::gList(arrow))
}

# The paths a ggarrow grob is about to draw, in millimetres, one list of `x`
# and `y` per path in the order the grob draws them, with the resection ggarrow
# cuts from the fins end (`fins`) and the head end (`head`) of each. An
# `arrow_path` grob, which the segment geom and the routed layer draw, holds
# its points and resections as fields. A `curve_arrow` grob, which the arc
# geom draws, builds its paths from the curve it holds when it is drawn, so
# they are built here the same way. Call it with the grob's viewport pushed.
arrow_grob_paths_mm <- function(grob) {
  mm_length <- function(value) {
    if (is.null(value)) {
      return(0)
    }
    if (grid::is.unit(value)) {
      return(grid::convertWidth(value, "mm", valueOnly = TRUE))
    }
    as.numeric(value)
  }

  if (inherits(grob, "arrow_path")) {
    fields <- unclass(grob$id_rle)
    id <- rep(seq_along(fields$length), fields$length)
    x <- grid::convertX(grob$x, "mm", valueOnly = TRUE)
    y <- grid::convertY(grob$y, "mm", valueOnly = TRUE)
    paths <- unname(lapply(split(seq_along(id), id), function(i) {
      list(x = x[i], y = y[i])
    }))
    return(list(
      paths = paths,
      fins = mm_length(grob$resect$fins),
      head = mm_length(grob$resect$head)
    ))
  }

  curve <- grid::makeContent(grob$curve)$children[[1]]
  if (inherits(curve, "xspline")) {
    points <- grid::xsplinePoints(curve)
    if (all(c("x", "y") %in% names(points))) {
      points <- list(points)
    }
    paths <- lapply(points, function(p) {
      list(
        x = grid::convertX(p$x, "mm", valueOnly = TRUE),
        y = grid::convertY(p$y, "mm", valueOnly = TRUE)
      )
    })
  } else {
    x0 <- grid::convertX(curve$x0, "mm", valueOnly = TRUE)
    y0 <- grid::convertY(curve$y0, "mm", valueOnly = TRUE)
    x1 <- grid::convertX(curve$x1, "mm", valueOnly = TRUE)
    y1 <- grid::convertY(curve$y1, "mm", valueOnly = TRUE)
    paths <- lapply(seq_along(x0), function(i) {
      list(x = c(x0[[i]], x1[[i]]), y = c(y0[[i]], y1[[i]]))
    })
  }
  list(
    paths = paths,
    fins = mm_length(grob$params$resect_fins),
    head = mm_length(grob$params$resect_head)
  )
}

# `grob`, an `arrow_path` or `curve_arrow` grob, resected by `head` and
# `fins` millimetres at the head and fins end of each of its paths.
set_arrow_grob_resects <- function(grob, head, fins) {
  if (inherits(grob, "arrow_path")) {
    grob$resect <- list(
      head = grid::unit(head, "mm"),
      fins = grid::unit(fins, "mm")
    )
  } else {
    grob$params$resect_head <- grid::unit(head, "mm")
    grob$params$resect_fins <- grid::unit(fins, "mm")
  }
  grob
}

# Helper: inject DAG resection defaults ---------------------------------------

# An end the layer has no resection for takes the `ggdag.edge_cap` option,
# 8 mm unless it is set. ggarrow reads a `resect_head` or `resect_fins`
# column of the data before the layer's value, so where the data carry one,
# whether the user mapped it or the layer wrote it to follow the nodes, the
# layer's value only stands in for the rows without a node and names the
# single cap the routed layer sets its router's constants from.
inject_dag_resect <- function(resect, data) {
  edge_cap <- ggdag_option("edge_cap", 8)
  list(head = resect$head %||% edge_cap, fins = resect$fins %||% edge_cap)
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

# A ggarrow edge layer a user adds by hand stops each end whose resection the
# user left unset, as a parameter or as an aesthetic, 2 mm beyond the node
# drawn there, as the plotters do. The nodes are read when the plot is built,
# so the node layers can come before or after the edges. An end with no node
# drawn at it keeps the resection of a circle node of the size the node layer
# is drawn at, or the `ggdag.edge_cap` option when the plot has no node layer.
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.dag_arrow_layer <- function(object, plot, ...) {
  layer <- clone_layer(.subset2(object, "layer"))
  resect <- layer$geom_params$resect

  needs_resect <- c("head", "fins")[
    c(is.null(resect$head), is.null(resect$fins))
  ]
  if (!isTRUE(layer$node_aware_caps)) {
    follows <- needs_resect[vapply(
      needs_resect,
      function(end) is.null(layer$mapping[[paste0("resect_", end)]]),
      logical(1)
    )]
    layer <- node_aware_resect_layer(
      layer,
      gap = node_edge_gap_mm,
      fallback_extent = node_radius_mm(
        discover_node_size(plot) %||% GeomDagPoint$default_aes$size
      ),
      ends = follows
    )
  }

  # A routed layer clears the drawn node discs, so it needs the node size
  # itself and not only the cap derived from it.
  needs_node_size <- inherits(layer$geom, "GeomDAGRoutedArrow") &&
    is.null(layer$geom_params$node_size)

  # It also routes along the axis the layout ran its layers along, which the
  # plot's data carries. An axis named at the call site is the user's own and
  # is left alone.
  if (
    inherits(layer$geom, "GeomDAGRoutedArrow") &&
      identical(layer$geom_params$layer_axis %||% "auto", "auto")
  ) {
    layer$geom_params$layer_axis <- layout_layer_axis(plot$data)
  }

  if (length(needs_resect) > 0 || needs_node_size) {
    discovered <- discover_node_size(plot)
    if (!is.null(discovered)) {
      cap_mm <- node_size_to_cap(discovered)
      for (end in needs_resect) {
        layer$geom_params$resect[[end]] <- cap_mm
      }
      needs_resect <- character()
      if (needs_node_size) {
        layer$geom_params$node_size <- discovered
        needs_node_size <- FALSE
      }
    }
  }

  if (length(needs_resect) > 0 || needs_node_size) {
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
      if (needs_node_size) {
        self$geom_params$node_size <- discovered
      }
    })
  }

  # a routed layer routes the edges of the plot's other routed layers with
  # the same settings along with its own
  if (inherits(layer$geom, "GeomDAGRoutedArrow")) {
    layer <- routed_union_layer(layer)
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
#' Once any edge carries a value, the edges left unset (`NA`) are drawn as
#' straight lines too, unless `unset = "curvature"` asks for the layer's own
#' `curvature` instead, as the bidirected layers of [geom_dag_arrows()] and
#' [geom_dag()] do. The scalar `curvature` parameter is also the layer's
#' value for the case where no edge carries one. This is useful in
#' time-ordered DAGs where some edges need to curve around intermediate nodes
#' while adjacent edges stay straight.
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
#' An end left to the automatic value stops 2 mm outside the outline of the
#' node drawn there, following the size and shape of that node whichever
#' order the layers were added in: on the circle 2 mm wider than a circle
#' node, and on the square 2 mm wider on every side than a square node,
#' wherever the edge meets it. An end with no node drawn at it stops 2 mm
#' beyond a circle of the plot's node size, or 8 mm from the end when the
#' plot has no node layer (`geom_dag_point()` or `geom_dag_node()`) at all,
#' which the `ggdag.edge_cap` option sets.
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
#' @param unset What an edge whose `edge_curvature` is unset (`NA`) is drawn
#'   as, once some other edge of the layer carries a value. `"chord"` (the
#'   default) draws it straight, which is what a directed layer wants:
#'   curving one edge does not bend the others. `"curvature"` draws it at the
#'   layer's own `curvature`, which is what a bidirected layer wants: curving
#'   one bidirected edge does not flatten the rest.
#'
#' @export
#' @rdname geom_dag_arrow
geom_dag_arrow_arc <- function(
  mapping = NULL,
  data = NULL,
  curvature = 0.3,
  angle = 90,
  ncp = 5,
  unset = c("chord", "curvature"),
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

  unset <- match.arg(unset)

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
      unset = unset,
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
      unset = "curvature",
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

# Layer data for the routed edge layer: the plain plot rows, with a logical
# `.ggdag_draw` column marking the rows this layer draws. The router needs
# every node of the panel as an obstacle, including the ones that only
# bidirected edges touch, and the scales must see the rows every other DAG
# layer sees, so the layer carries the whole frame and draws part of it.
routed_edge_data <- function(data_directed) {
  force(data_directed)
  function(plot_data) {
    if (inherits(plot_data, "tidy_dagitty")) {
      plot_data <- pull_dag_data(plot_data)
    }
    drawn <- if (is.function(data_directed)) {
      data_directed(plot_data)
    } else {
      data_directed %||% plot_data
    }
    row_key <- function(df, columns) {
      do.call(paste, c(as.list(df[columns]), sep = "\r"))
    }
    shared <- intersect(names(plot_data), names(drawn))
    if (length(shared) == 0 || nrow(drawn) == 0) {
      plot_data$.ggdag_draw <- rep(FALSE, nrow(plot_data))
      plot_data$.ggdag_route <- plot_data$.ggdag_draw
      return(plot_data)
    }

    plot_keys <- row_key(plot_data, shared)
    drawn_keys <- row_key(drawn, shared)
    plot_data$.ggdag_draw <- plot_keys %in% drawn_keys
    # the rows the layer routes: its own, until the plot is built and the
    # other routed layers' rows join them (`mark_routed_union()`)
    plot_data$.ggdag_route <- plot_data$.ggdag_draw

    # A caller may hand the layer edges of its own, positioned wherever it
    # likes; those rows are not among the plot's, so they are appended rather
    # than matched. They are drawn, and their endpoints join the obstacles.
    extra <- drawn[!(drawn_keys %in% plot_keys), , drop = FALSE]
    if (nrow(extra) == 0) {
      return(plot_data)
    }
    extra$.ggdag_draw <- TRUE
    extra$.ggdag_route <- TRUE
    dplyr::bind_rows(plot_data, extra)
  }
}

# The `draw` aesthetic tells the routed geom which of its rows to draw, and
# `routed` which of its rows to route. The rest are the panel's obstacles.
with_routed_draw <- function(mapping) {
  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }
  mapping$draw <- rlang::quo(.data$.ggdag_draw)
  mapping$routed <- rlang::quo(.data$.ggdag_route)
  mapping
}

# The settings a routed layer routes with. Two routed layers of one plot
# with the same settings route one scene, so each of them routes the other's
# edges as well as its own and draws only its own. The line width the
# ornaments are measured at is one of them: the stubs of a scene hold the
# reach of its heads, and two layers whose heads reach apart would share out
# one scene's rows with two ladders.
routed_layer_settings <- function(layer) {
  params <- layer$geom_params
  list(
    route = params$route,
    layer_axis = params$layer_axis,
    edge_route_options = params$edge_route_options,
    node_size = params$node_size,
    resect = params$resect,
    arrow = params$arrow,
    length = params$length,
    justify = params$justify,
    linewidth = routed_linewidth(
      layer$aes_params$linewidth,
      layer$geom$default_aes$linewidth
    )
  )
}

# Whether the routed layers `layer` and `other` route one scene: both draw
# with the routed geom, in orthogonal mode, with the same settings.
routed_layers_share_scene <- function(layer, other) {
  if (
    !inherits(layer$geom, "GeomDAGRoutedArrow") ||
      !inherits(other$geom, "GeomDAGRoutedArrow")
  ) {
    return(FALSE)
  }
  settings <- layer$routed_settings %||% routed_layer_settings(layer)
  identical(settings$route, "orthogonal") &&
    identical(other$routed_settings %||% routed_layer_settings(other), settings)
}

# The scene each of `layers`, the layers of a plot, is routed in, as the index
# of the first layer of that scene: an orthogonal routed layer shares the
# scene of the first routed layer with the same settings, and every other
# layer is a scene of its own.
routed_layer_scenes <- function(layers) {
  scenes <- seq_along(layers)
  for (i in seq_along(layers)) {
    for (j in seq_len(i - 1L)) {
      if (
        scenes[[j]] == j && routed_layers_share_scene(layers[[i]], layers[[j]])
      ) {
        scenes[[i]] <- j
        break
      }
    }
  }
  scenes
}

# The routed layer `layer` with the rows of the plot's other routed layers
# marked for routing when the plot is built, which is the first point at
# which every layer of the plot is in view. A row is matched by the columns
# the two layers' data share, so the row of one panel matches only itself.
routed_union_layer <- function(layer) {
  ggplot2::ggproto(
    "DagRoutedUnionLayer",
    layer,
    setup_layer = function(self, data, plot) {
      data <- ggplot2::ggproto_parent(layer, self)$setup_layer(data, plot)
      mark_routed_union(self, data, plot)
    }
  )
}

# The data of the routed layer `self` with `.ggdag_route` marking the rows of
# its scene that the plot's other routed layers draw, which it routes along
# with its own: each such row holds the index among the plot's layers of the
# layer that draws it, and every other row holds 0. The layer draws the rows
# `.ggdag_draw` marks, and routes those as well. A panel in which another
# layer draws edges of the scene gives the arrivals out of a narrow gap rows
# of their own, since the layer drawn later would hide the other's edge on a
# shared row; the rows say which panels those are once the plot is split
# into panels.
mark_routed_union <- function(self, data, plot) {
  if (!is.data.frame(data) || !".ggdag_route" %in% names(data)) {
    return(data)
  }
  own <- !is.na(data$.ggdag_draw) & data$.ggdag_draw
  owner <- rep(0L, nrow(data))
  for (i in seq_along(plot$layers)) {
    other <- plot$layers[[i]]
    if (identical(other, self) || !routed_layers_share_scene(self, other)) {
      next
    }
    other_data <- layer_source_data(other, plot)
    if (is.null(other_data) || !".ggdag_draw" %in% names(other_data)) {
      next
    }
    drawn <- other_data[
      !is.na(other_data$.ggdag_draw) & other_data$.ggdag_draw,
      ,
      drop = FALSE
    ]
    shared <- setdiff(
      intersect(names(data), names(drawn)),
      c(".ggdag_draw", ".ggdag_route")
    )
    if (length(shared) == 0 || nrow(drawn) == 0) {
      next
    }
    row_key <- function(df) do.call(paste, c(as.list(df[shared]), sep = "\r"))
    theirs <- !own & owner == 0L & row_key(data) %in% row_key(drawn)
    owner[theirs] <- i
  }
  data$.ggdag_route <- owner
  data
}

# The routed edge layer itself. Its data are the plot rows, so it inherits the
# plot's DAG mapping like any other layer; every geometric decision is made in
# millimetres at draw time.
dag_routed_arrow_layer <- function(
  mapping = NULL,
  data_directed = NULL,
  route = "spline",
  clearance = NULL,
  edge_sep = NULL,
  edge_sep_min = NULL,
  edge_route_options = NULL,
  layer_axis = "auto",
  node_size = NULL,
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
  inherit.aes = TRUE,
  call = rlang::caller_env(),
  ...
) {
  # the three millimetre arguments are per-call overrides of the object's
  # fields, so they are folded in here and the layer carries one object. The
  # call travels with them: what the user wrote is a layer, not this builder.
  edge_route_options <- merge_edge_route_options(
    edge_route_options,
    clearance = clearance,
    edge_sep = edge_sep,
    edge_sep_min = edge_sep_min,
    call = call
  )

  layer <- ggplot2::layer(
    data = routed_edge_data(data_directed),
    mapping = with_routed_draw(mapping),
    stat = ggplot2::StatIdentity,
    geom = geom_dag_routed_arrow_geom(),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      route = route,
      edge_route_options = edge_route_options,
      layer_axis = layer_axis,
      node_size = node_size,
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
  )
  # the settings as the call gave them, before the plot fills in the node
  # size and resections: the layers of one plot are resolved one at a time
  # when it is built, so only these compare alike from every layer
  layer$routed_settings <- routed_layer_settings(layer)
  dag_arrow_layer(layer)
}

#' Routed DAG edges that detour around nodes
#'
#' `geom_dag_routed_arrows()` draws DAG edges with the ggarrow engine,
#' routing every directed edge whose path a node blocks around that node.
#' The routing happens when the plot is drawn, in the millimetres of the
#' device, so the detour clears the drawn node discs whatever the shape of
#' the panel and the picture re-routes when the plot is resized. Unblocked
#' edges stay straight, bidirected edges are drawn as arcs by the same curve
#' geom [geom_dag_arrows()] uses, and the routing is deterministic: the same
#' DAG at the same size always draws the same paths. In spline mode an
#' unblocked edge that would run through another edge's drawn arrowhead is
#' nudged past it by a few millimetres, so it still reads as straight and
#' the arrowhead is drawn whole.
#'
#' Curvature the user set is never rerouted. When the data carries an
#' `edge_curvature` column, from [curved()], [curve_edge()], or your own
#' code, an edge with a numeric curvature is drawn as that arc, an explicit
#' `0` stays straight through whatever sits on its chord, and only edges
#' whose curvature is unset (`NA`) are candidates for routing. An arc the
#' user set still counts as an obstacle the other edges route around.
#'
#' The layer carries every row of the plot data, so the router can treat
#' every drawn node as an obstacle, and draws the rows `data_directed`
#' selects. Edge rows of a data frame you supply that are not among the plot
#' rows are drawn as well, and their endpoints join the obstacles. Scales and
#' legends therefore see exactly what the other DAG layers see. Edges are
#' resected to the plot's node size exactly as in [geom_dag_arrow()], and the
#' same node size gives the router the radius of the discs it must clear. The
#' automatic label geoms take their obstacles from the plot's own nodes, so a
#' label may be placed across an edge appended this way.
#'
#' In orthogonal mode the routes of one panel share its rows, ports, and
#' slots out among themselves, so a plot that draws its edges in more than
#' one orthogonal routed layer, such as the blocked and the open edges of
#' [ggdag_adjustment_set()], routes them together: each layer routes the
#' edges the other routed layers draw with the same settings, line width
#' included, along with its own, and draws only its own. Where a gap between
#' layers is too narrow for a stub, in a panel where more than one of those
#' layers draws edges, the edges that arrive at a node out of it each take a
#' row of their own rather than merging onto the node's center row, since the
#' layer drawn later would hide the other's edge there. The stub the gaps
#' between layers keep behind an arrowhead holds the longest arrowhead or fins
#' the layer draws at the line width it sets; a line width mapped to the data
#' does not move the routes.
#'
#' A routed path is stroked at one width along its length, so
#' `linewidth_head` and `linewidth_fins` taper only the arcs drawn for
#' curvature the user set.
#'
#' @inheritParams geom_dag_arrow
#' @inheritParams geom_dag_arrow_arc
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()],
#'   applied to both the routed layer and the bidirected arc layer.
#' @param data The data to be displayed. `NULL`, the default, uses the plot
#'   data. Both layers filter what it holds by edge direction, so a subset of
#'   the plot's rows narrows the directed and the bidirected edges alike.
#' @param data_directed,data_bidirected The data to be displayed for directed
#'   and bidirected edges respectively. By default, these filter the plot
#'   data by edge direction. Data named for one direction replaces the filter
#'   for that direction: a function there is applied to whatever `data`
#'   gives it, and a data frame there is that direction's data as it stands.
#' @param route How to route the edges: `"spline"` (the default) draws a
#'   blocked edge as a smooth curve around the obstacle and leaves unblocked
#'   edges straight. `"orthogonal"` draws every edge as axis-aligned runs
#'   with rounded corners, passing the intermediate layers in the gaps
#'   between them or along a channel beyond them.
#' @param clearance The daylight in millimetres a routed path keeps beyond
#'   the node discs, or `NULL` (the default) for the router's own margin,
#'   half a node radius with a floor of 1.2 mm.
#' @param edge_sep The gap in millimetres between two routed paths sharing a
#'   detour, or `NULL` (the default) for the router's own separation.
#' @param edge_sep_min The gap in millimetres the orthogonal router may
#'   tighten `edge_sep` to when a gap between layers is too narrow for its
#'   slots at the full separation, or `NULL` (the default) for a quarter of
#'   the node radius with a floor of 1.5 mm. A value above a separation the
#'   router derives for itself is reduced to it; a value above an `edge_sep`
#'   you set as well is an error. Set it equal to `edge_sep` to keep the
#'   separation fixed. Spline routing does not use it.
#' @param edge_route_options An object from [edge_route_options()] carrying
#'   the rest of the constants the router draws with, or `NULL` (the default)
#'   for the router's own. `clearance`, `edge_sep`, and `edge_sep_min` above
#'   override the object's fields of those names for this layer.
#' @param layer_axis The axis the layout's layers run along, one of `"auto"`
#'   (the default), `"x"`, or `"y"`. Routing sends a detour along the
#'   within-layer axis, so a layout laid out down the panel rather than across
#'   it needs the axis it runs along. Under `"auto"`, a layout that ordered
#'   time down the panel, such as [time_ordered_coords()] with
#'   `direction = "y"`, is taken at its word, and every other scene has its
#'   layers inferred from the node positions. The direction is read from the
#'   DAG the plot is drawn from, which carries the axis the layout recorded;
#'   name the axis yourself for a layout ggdag did not compute.
#' @param node_size The size of the drawn nodes, in the units
#'   [geom_dag_point()] takes, giving the router the radius of the discs it
#'   clears. `NULL`, the default, takes it from the plot's node layer.
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
#'   for the other edge geoms, and the `edge_route` option in
#'   [ggdag_options_set()] to swap routed edges into `geom_dag()` and the
#'   quick plotting functions.
#'
#' @export
geom_dag_routed_arrows <- function(
  mapping = NULL,
  data = NULL,
  data_directed = filter_direction("->"),
  data_bidirected = filter_direction("<->"),
  route = c("spline", "orthogonal"),
  clearance = NULL,
  edge_sep = NULL,
  edge_sep_min = NULL,
  edge_route_options = NULL,
  layer_axis = c("auto", "x", "y"),
  node_size = NULL,
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
  inherit.aes = TRUE,
  ...
) {
  rlang::check_installed(
    "ggarrow",
    reason = "to use `geom_dag_routed_arrows()`."
  )

  route <- match.arg(route)
  layer_axis <- match.arg(layer_axis)

  # The layer draws directed and bidirected edges with two layers of its own,
  # so `data` narrows the rows each of them then filters by direction, the
  # way `geom_dag()` composes its own `data`. Data named for one direction is
  # that direction's own and stands as it is.
  data_directed <- compose_edge_data(data, data_directed)
  data_bidirected <- compose_edge_data(data, data_bidirected)

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
      mapping = mapping,
      data_directed = data_directed,
      route = route,
      clearance = clearance,
      edge_sep = edge_sep,
      edge_sep_min = edge_sep_min,
      edge_route_options = edge_route_options,
      layer_axis = layer_axis,
      node_size = node_size,
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
      inherit.aes = inherit.aes,
      call = rlang::current_env(),
      ...
    ),
    geom_dag_arrow_arc(
      mapping = mapping,
      data = data_bidirected,
      curvature = curvature,
      unset = "curvature",
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
      inherit.aes = inherit.aes,
      ...
    )
  )
}
