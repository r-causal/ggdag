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
  # converting a unit to points needs a device, and with none open grid would
  # open the default one, which writes Rplots.pdf
  withr::local_pdf(NULL)
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

# The node size each repelling label layer of `plot` repels around, in drawing
# order. A layer that carries no node size at all reports `NA`, so that a layer
# the size never reached is visible in the result rather than being dropped.
repel_node_sizes <- function(plot) {
  repel_layers <- purrr::keep(plot$layers, \(layer) {
    inherits(layer$stat, "StatNodesRepel")
  })

  unname(purrr::map_dbl(repel_layers, \(layer) {
    node_size <- layer$stat_params$node_size
    if (is.null(node_size)) NA_real_ else node_size
  }))
}

# The parameters of the automatic label layer of `plot`.
auto_label_params <- function(plot) {
  index <- which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$stat, "StatNodesLabelAuto")
  }))
  expect_length(index, 1)
  layer <- plot$layers[[index]]
  c(layer$stat_params, layer$geom_params)
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

# The node rows `plot` actually draws, across every node layer it has.
built_node_data <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  indices <- which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$geom, "GeomDagPoint") || inherits(layer$geom, "GeomDagNode")
  }))

  dplyr::bind_rows(purrr::map(indices, \(i) built$data[[i]]))
}

# The bend the ggraph arc layers among `layers` were built with, in drawing
# order. `geom_dag_edges_arc()` hands ggraph's arc stat the curvature it was
# given as `strength`.
arc_edge_strengths <- function(layers) {
  arc_layers <- purrr::keep(layers, \(layer) {
    inherits(layer$stat, "StatEdgeArc")
  })

  unname(purrr::map_dbl(arc_layers, \(layer) layer$stat_params$strength))
}

# The bend the ggarrow arc layers among `layers` were built with, in drawing
# order. `geom_dag_arrow_arc()` keeps its curvature as a geom parameter.
arrow_arc_curvatures <- function(layers) {
  arc_layers <- purrr::keep(
    layers,
    \(layer) inherits(layer$geom, "GeomDAGArrowCurve")
  )

  unname(purrr::map_dbl(arc_layers, \(layer) layer$geom_params$curvature))
}

# The labels of the legend keys `plot` draws nothing in. ggplot2 builds a
# legend key from the layer rows that carry its value, so a scale whose breaks
# name a level none of its layers has rows for renders that key as a label
# beside an empty box.
empty_legend_keys <- function(plot) {
  # a gtable measures its text on a device, and with none open grid would open
  # the default one, which writes Rplots.pdf
  withr::local_pdf(NULL)
  boxes <- ggplot2::ggplotGrob(plot) |>
    (\(gtable) gtable$grobs[grepl("guide-box", gtable$layout$name)])()

  legends <- purrr::keep(
    purrr::list_flatten(purrr::map(boxes, \(box) as.list(box$grobs))),
    \(grob) inherits(grob, "gtable")
  )

  purrr::list_c(purrr::map(legends, empty_keys_of_legend), ptype = character())
}

# The labels of the keys of one legend gtable that draw only their background.
empty_keys_of_legend <- function(legend) {
  rows <- legend$layout$t[grepl("^key-.*-bg$", legend$layout$name)]
  keys <- legend$grobs[grepl("^key-.*-bg$", legend$layout$name)]
  empty <- purrr::map_lgl(keys, key_draws_nothing)

  purrr::map_chr(rows[empty], \(row) key_label(legend, row))
}

# Does a legend key grob draw nothing but the background rectangle behind it?
key_draws_nothing <- function(key) {
  glyphs <- purrr::discard(as.list(key$children), \(x) inherits(x, "rect"))

  all(purrr::map_lgl(glyphs, \(x) inherits(x, "zeroGrob")))
}

# The text of the label the legend gtable puts in the same row as a key.
key_label <- function(legend, row) {
  index <- which(legend$layout$t == row & grepl("^label-", legend$layout$name))
  if (length(index) == 0) {
    return(NA_character_)
  }

  paste(legend$grobs[[index[1]]]$children[[1]]$label, collapse = " ")
}
