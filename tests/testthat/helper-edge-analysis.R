# Test Helper Functions for Edge Analysis in ggdag plots
# These functions help identify and count edges to detect bugs like issue #167

# Function to identify if a ggplot layer draws edges
is_edge_layer <- function(layer) {
  geom_class <- class(layer$geom)[1]
  
  # ggdag edge geoms
  ggdag_edge_geoms <- c(
    "GeomDAGEdgePath",      # Used by geom_dag_edges_link, etc.
    "GeomSegment",          # Used by some edge types
    "GeomCurve"             # Used by geom_dag_collider_edges
  )
  
  # ggraph edge geoms (inherited by ggdag)  
  ggraph_edge_geoms <- c(
    "GeomEdgeArc",
    "GeomEdgeDiagonal", 
    "GeomEdgeFan",
    "GeomEdgePath",
    "GeomEdgeLink"
  )
  
  geom_class %in% c(ggdag_edge_geoms, ggraph_edge_geoms)
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
  
  if (length(edge_layer_indices) == 0) return(0)
  
  total_edges <- 0
  for (i in edge_layer_indices) {
    layer_data <- built_plot$data[[i]]
    if (nrow(layer_data) > 0 && "group" %in% names(layer_data)) {
      # Count unique groups (each group represents one edge)
      unique_groups <- length(unique(layer_data$group))
      total_edges <- total_edges + unique_groups
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
    return(list(duplicates = FALSE, max_count = 0, duplicate_edges = character(0)))
  }
  
  all_edges <- purrr::map_df(edge_layer_indices, function(i) {
    layer_data <- built_plot$data[[i]]
    if (nrow(layer_data) > 0 && all(c("x", "y", "xend", "yend") %in% names(layer_data))) {
      layer_data |>
        dplyr::select(x, y, xend, yend) |>
        dplyr::mutate(layer = i)
    }
  })
  
  if (nrow(all_edges) == 0) {
    return(list(duplicates = FALSE, max_count = 0, duplicate_edges = character(0)))
  }
  
  # Create edge identifiers (normalize direction)
  all_edges <- all_edges |>
    dplyr::mutate(
      edge_id = purrr::pmap_chr(list(x, y, xend, yend), function(x1, y1, x2, y2) {
        # Normalize edge direction for comparison
        coords <- sort(c(paste0(x1, ",", y1), paste0(x2, ",", y2)))
        paste0(coords[1], "->", coords[2])
      })
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

# For debugging - print layer information
inspect_plot_layers <- function(plot) {
  layer_info <- purrr::map_df(seq_along(plot$layers), function(i) {
    layer <- plot$layers[[i]]
    data.frame(
      layer_num = i,
      geom_class = class(layer$geom)[1],
      stat_class = class(layer$stat)[1], 
      is_edge = is_edge_layer(layer),
      data_rows = if (is.null(layer$data)) 0 else nrow(layer$data)
    )
  })
  
  print(layer_info)
  invisible(layer_info)
}

# Test the helpers with the bug
test_equivalent_class_bug <- function() {
  cat("Testing ggdag_equivalent_class bug with use_edges parameter\n")
  
  # Create a DAG with reversible edges
  dag <- dagify(y ~ x + z, x ~ z)
  
  # Test with use_edges = TRUE (should have edges)
  cat("\n--- Testing use_edges = TRUE ---\n")
  p1 <- ggdag_equivalent_class(dag, use_edges = TRUE)
  analysis1 <- analyze_plot_edges(p1)
  cat("Edge layers:", analysis1$edge_layers, "\n")
  cat("Total edges:", analysis1$total_edges, "\n")
  cat("Has edge layers:", analysis1$has_edge_layers, "\n")
  
  # Test with use_edges = FALSE (should have NO edges, but currently has edges due to bug)
  cat("\n--- Testing use_edges = FALSE ---\n")
  p2 <- ggdag_equivalent_class(dag, use_edges = FALSE)
  analysis2 <- analyze_plot_edges(p2)
  cat("Edge layers:", analysis2$edge_layers, "\n")
  cat("Total edges:", analysis2$total_edges, "\n") 
  cat("Has edge layers:", analysis2$has_edge_layers, "\n")
  
  # Check for duplicates
  cat("\n--- Checking for duplicate edges ---\n")
  cat("Duplicates in use_edges=TRUE:", analysis1$duplicates$duplicates, "\n")
  cat("Duplicates in use_edges=FALSE:", analysis2$duplicates$duplicates, "\n")
  
  # The bug is confirmed if use_edges = FALSE still has edge layers
  if (analysis2$has_edge_layers) {
    cat("\n*** BUG CONFIRMED: use_edges = FALSE still has", analysis2$edge_layers, "edge layers! ***\n")
  } else {
    cat("\n--- No bug detected: use_edges = FALSE correctly has no edges ---\n")
  }
  
  invisible(list(with_edges = analysis1, without_edges = analysis2))
}