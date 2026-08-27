# Test Helper Functions for Edge Analysis in ggdag plots
# These functions help identify and count edges to detect bugs like issue #167

# Function to identify if a ggplot layer draws edges
is_edge_layer <- function(layer) {
  geom_class <- class(layer$geom)[1]

  # ggdag edge geoms
  ggdag_edge_geoms <- c(
    "GeomDAGEdgePath", # Used by geom_dag_edges_link, etc.
    "GeomSegment", # Used by some edge types
    "GeomCurve" # Used by geom_dag_collider_edges
  )

  # ggraph edge geoms (inherited by ggdag)
  ggraph_edge_geoms <- c(
    "GeomEdgeArc",
    "GeomEdgeDiagonal",
    "GeomEdgeFan",
    "GeomEdgePath",
    "GeomEdgeLink"
  )

  # ggarrow edge geoms, drawn by the ggarrow engine
  ggarrow_edge_geoms <- c("GeomDAGArrow", "GeomDAGArrowCurve")

  matched <- geom_class %in%
    c(ggdag_edge_geoms, ggraph_edge_geoms, ggarrow_edge_geoms)

  matched || inherits(layer$geom, "GeomArrow")
}

# Edges in a ggarrow layer are one row each and share a single group, so the
# group count every other edge layer is measured by would report one.
is_row_counted_edge_layer <- function(layer) {
  inherits(layer$stat, "StatDAGArrowEdges")
}

# Count edge layers in a plot
count_edge_layers <- function(plot) {
  edge_layers <- purrr::map_lgl(plot$layers, is_edge_layer)
  sum(edge_layers)
}

# Count total edges across all layers using unique groups
count_total_edges <- function(plot) {
  built_plot <- ggplot2::ggplot_build(plot)
  edge_layers <- purrr::map_lgl(plot$layers, is_edge_layer)
  edge_layer_indices <- which(edge_layers)

  if (length(edge_layer_indices) == 0) {
    return(0)
  }

  total_edges <- 0
  for (i in edge_layer_indices) {
    layer_data <- built_plot$data[[i]]

    if (nrow(layer_data) > 0 && is_row_counted_edge_layer(plot$layers[[i]])) {
      total_edges <- total_edges + nrow(layer_data)
    } else if (nrow(layer_data) > 0 && "group" %in% names(layer_data)) {
      if ("PANEL" %in% names(layer_data)) {
        # For faceted plots: count unique group-panel combinations
        group_panel_combos <- layer_data |>
          dplyr::select(group, PANEL) |>
          dplyr::distinct() |>
          nrow()
        total_edges <- total_edges + group_panel_combos
      } else {
        # For non-faceted plots: count unique groups
        unique_groups <- length(unique(layer_data$group))
        total_edges <- total_edges + unique_groups
      }
    }
  }

  total_edges
}

# Detect overlapping edges by comparing coordinates
detect_duplicate_edges <- function(plot) {
  built_plot <- ggplot2::ggplot_build(plot)
  edge_layers <- purrr::map_lgl(plot$layers, is_edge_layer)
  edge_layer_indices <- which(edge_layers)

  if (length(edge_layer_indices) == 0) {
    return(list(
      duplicates = FALSE,
      max_count = 0,
      duplicate_edges = character(0)
    ))
  }

  all_edges <- purrr::map_df(edge_layer_indices, function(i) {
    layer_data <- built_plot$data[[i]]
    if (
      nrow(layer_data) > 0 &&
        all(c("x", "y", "xend", "yend") %in% names(layer_data))
    ) {
      if (!"PANEL" %in% names(layer_data)) {
        layer_data$PANEL <- factor(1)
      }
      layer_data |>
        dplyr::select(x, y, xend, yend, PANEL) |>
        dplyr::mutate(layer = i)
    }
  })

  if (nrow(all_edges) == 0) {
    return(list(
      duplicates = FALSE,
      max_count = 0,
      duplicate_edges = character(0)
    ))
  }

  # Create edge identifiers (normalize direction). The same edge drawn in two
  # facets is two edges, not a duplicate, so the panel is part of the identity.
  all_edges <- all_edges |>
    dplyr::mutate(
      edge_id = purrr::pmap_chr(
        list(x, y, xend, yend, PANEL),
        function(x1, y1, x2, y2, panel) {
          # Normalize edge direction for comparison
          coords <- sort(c(paste0(x1, ",", y1), paste0(x2, ",", y2)))
          paste0(panel, ": ", coords[1], "->", coords[2])
        }
      )
    )

  # Count duplicates
  edge_counts <- table(all_edges$edge_id)
  duplicates <- any(edge_counts > 1)
  max_count <- max(edge_counts)

  list(
    duplicates = duplicates,
    max_count = max_count,
    duplicate_edges = names(edge_counts[edge_counts > 1])
  )
}

# Main analysis function - comprehensive edge analysis
analyze_plot_edges <- function(plot) {
  list(
    edge_layers = count_edge_layers(plot),
    total_edges = count_total_edges(plot),
    duplicates = detect_duplicate_edges(plot),
    has_edge_layers = count_edge_layers(plot) > 0
  )
}
