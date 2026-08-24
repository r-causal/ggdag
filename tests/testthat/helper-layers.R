# Helpers for inspecting the layers a quick plotter builds, and the sizes those
# layers draw with.

# The layers of `plot` drawn by `geom_class`, in drawing order.
layers_by_geom <- function(plot, geom_class) {
  is_match <- purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$geom, geom_class)
  })

  plot$layers[is_match]
}

# How many layers `plot` draws with `geom_class`.
count_geom_layers <- function(plot, geom_class) {
  length(layers_by_geom(plot, geom_class))
}

# The stat class of every layer, in drawing order.
layer_stat_classes <- function(plot) {
  purrr::map_chr(plot$layers, \(layer) class(layer$stat)[[1]])
}

# The positions of the layers `plot` draws its ggraph edges with.
dag_edge_layer_indices <- function(plot) {
  which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$geom, "GeomDAGEdgePath")
  }))
}

# The cap radii, in mm, that the built edge layers of `plot` leave at the ends
# of their edges. `ggraph::circle()` records a bounding box, so the width it
# stores is twice the radius its caller asked for.
# An edge layer that carries no cap at all reports `NA`, so that a layer the
# sizes never reached is visible in the result rather than being dropped.
edge_cap_radii <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  radii <- purrr::map_dbl(dag_edge_layer_indices(plot), \(i) {
    caps <- built$data[[i]][["start_cap"]]
    if (is.null(caps)) {
      return(NA_real_)
    }
    radius <- unique(unclass(caps)$width) / 2
    if (length(radius) != 1) {
      return(NA_real_)
    }
    radius
  })

  unique(radii)
}

# The widths the built edge layers of `plot` draw their edges with.
edge_widths <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  widths <- purrr::map_dbl(dag_edge_layer_indices(plot), \(i) {
    width <- unique(built$data[[i]][["edge_width"]])
    if (length(width) != 1) {
      return(NA_real_)
    }
    width
  })

  unique(widths)
}

# The arrowhead lengths, in points, of the edge layers of `plot` that draw
# arrowheads at all.
edge_arrow_lengths <- function(plot) {
  arrow_lengths <- purrr::map(dag_edge_layer_indices(plot), \(i) {
    arrow <- plot$layers[[i]]$geom_params$arrow
    if (is.null(arrow)) {
      return(NULL)
    }
    as.numeric(grid::convertUnit(arrow$length, "pt"))
  })

  sort(unique(unlist(arrow_lengths)))
}

# The node text `plot` actually draws, sorted so that layer order does not
# decide the comparison.
built_text_labels <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  indices <- which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$geom, "GeomDagText")
  }))

  sort(unique(unlist(purrr::map(indices, \(i) built$data[[i]][["label"]]))))
}

# The expression a repelling label layer maps `label` to, as written.
repel_label_expr <- function(plot) {
  repel_layers <- purrr::keep(plot$layers, \(layer) {
    inherits(layer$stat, "StatNodesRepel")
  })

  unname(purrr::map_chr(
    repel_layers,
    \(layer) rlang::as_label(layer$mapping$label)
  ))
}

# Does `plot` draw its edges with the ggarrow engine?
uses_ggarrow_edges <- function(plot) {
  any(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$geom, "GeomArrow") ||
      inherits(layer$geom, "GeomDAGArrow") ||
      inherits(layer$geom, "GeomDAGArrowCurve")
  }))
}

# The coordinates `plot` places each node at, one row per node, ordered by name.
node_coords <- function(plot) {
  plot$data |>
    dplyr::distinct(name, x, y) |>
    dplyr::arrange(name)
}

# The coordinates `tidy_dag` places each node at, in the same shape.
tidy_node_coords <- function(tidy_dag) {
  pull_dag_data(tidy_dag) |>
    dplyr::distinct(name, x, y) |>
    dplyr::arrange(name)
}
