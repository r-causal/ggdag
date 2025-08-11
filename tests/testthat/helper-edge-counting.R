# Helper functions for counting edges in DAGs
# These functions provide exact edge counts for testing ggdag visualizations

#' Count total edges in a DAG
#' @param dag A dagitty or tidy_dagitty object
#' @return Number of edges in the DAG
count_dag_edges <- function(dag) {
  if (inherits(dag, "tidy_dagitty")) {
    dag <- pull_dag(dag)
  }
  dagitty::edges(dag) |> nrow()
}

#' Count adjustment sets for a DAG
#' @param dag A dagitty or tidy_dagitty object
#' @param exposure Character, name of exposure variable
#' @param outcome Character, name of outcome variable
#' @return Number of valid adjustment sets
count_adjustment_sets <- function(dag, exposure = NULL, outcome = NULL) {
  if (inherits(dag, "tidy_dagitty")) {
    dag <- pull_dag(dag)
  }

  # If not specified, try to get from DAG
  if (is.null(exposure)) {
    exposure <- dagitty::exposures(dag)
  }
  if (is.null(outcome)) {
    outcome <- dagitty::outcomes(dag)
  }

  sets <- dagitty::adjustmentSets(dag, exposure = exposure, outcome = outcome)
  length(sets)
}

#' Count open paths between two nodes
#' @param dag A dagitty or tidy_dagitty object
#' @param from Character, starting node
#' @param to Character, ending node
#' @param limit Maximum number of paths to check
#' @return Number of open paths
count_open_paths <- function(dag, from, to, limit = 100) {
  if (inherits(dag, "tidy_dagitty")) {
    dag <- pull_dag(dag)
  }

  paths_result <- dagitty::paths(dag, from = from, to = to, limit = limit)
  sum(paths_result$open)
}

#' Count edges in open paths between two nodes
#' @param dag A dagitty or tidy_dagitty object
#' @param from Character, starting node
#' @param to Character, ending node
#' @param limit Maximum number of paths to check
#' @return Total number of edges in all open paths
count_edges_in_open_paths <- function(dag, from, to, limit = 100) {
  if (inherits(dag, "tidy_dagitty")) {
    dag <- pull_dag(dag)
  }

  paths_result <- dagitty::paths(dag, from = from, to = to, limit = limit)
  open_paths_idx <- which(paths_result$open)

  edges_count <- 0
  for (idx in open_paths_idx) {
    path_str <- paths_result$paths[idx]
    # Count arrows in the path string
    n_arrows <- lengths(regmatches(path_str, gregexpr("<-|->|<->", path_str)))
    edges_count <- edges_count + n_arrows
  }
  edges_count
}

#' Count reversible edges in a DAG (for equivalent class)
#' @param dag A dagitty or tidy_dagitty object
#' @return Number of edges that can be reversed
count_reversible_edges <- function(dag) {
  if (inherits(dag, "tidy_dagitty")) {
    dag <- pull_dag(dag)
  }

  # Get all equivalent DAGs
  equiv_dags <- dagitty::equivalentDAGs(dag, n = 100)

  if (length(equiv_dags) <= 1) {
    return(0) # No reversible edges if only one equivalent DAG
  }

  # Compare edge orientations across equivalent DAGs
  original_edges <- dagitty::edges(dag)
  reversible_count <- 0

  # For each edge in original DAG, check if it appears reversed in any equivalent DAG
  for (i in seq_len(nrow(original_edges))) {
    edge <- original_edges[i, ]
    is_reversible <- FALSE

    for (equiv_dag in equiv_dags) {
      equiv_edges <- dagitty::edges(equiv_dag)
      # Check if edge exists in reverse direction
      if (any(equiv_edges$v == edge$w & equiv_edges$w == edge$v)) {
        is_reversible <- TRUE
        break
      }
    }

    if (is_reversible) {
      reversible_count <- reversible_count + 1
    }
  }

  reversible_count
}

#' Count nodes with a specific status
#' @param dag A tidy_dagitty object with node status
#' @param status Character, the status to count (e.g., "exposure", "outcome", "latent")
#' @return Number of nodes with the specified status
count_nodes_with_status <- function(dag, status) {
  if (!inherits(dag, "tidy_dagitty")) {
    dag <- tidy_dagitty(dag)
  }

  dag_data <- pull_dag_data(dag)
  sum(dag_data$status == status, na.rm = TRUE)
}

#' Count instrumental variables
#' @param dag A dagitty or tidy_dagitty object
#' @param exposure Character, name of exposure variable
#' @param outcome Character, name of outcome variable
#' @return Number of instrumental variables
count_instrumental_variables <- function(dag, exposure = NULL, outcome = NULL) {
  if (inherits(dag, "tidy_dagitty")) {
    dag <- pull_dag(dag)
  }

  # If not specified, try to get from DAG
  if (is.null(exposure)) {
    exposure <- dagitty::exposures(dag)
  }
  if (is.null(outcome)) {
    outcome <- dagitty::outcomes(dag)
  }

  ivs <- dagitty::instrumentalVariables(
    dag,
    exposure = exposure,
    outcome = outcome
  )

  # Count unique instrumental variables
  if (length(ivs) == 0) {
    return(0)
  }

  # Extract IV names from the list structure
  iv_names <- unique(unlist(lapply(ivs, function(x) x$Z)))
  length(iv_names)
}

#' Count collider nodes
#' @param dag A dagitty or tidy_dagitty object
#' @return Number of collider nodes
count_colliders <- function(dag) {
  if (inherits(dag, "tidy_dagitty")) {
    dag <- pull_dag(dag)
  }

  # Get all nodes
  nodes <- names(dag)
  collider_count <- 0

  for (node in nodes) {
    # A node is a collider if it has at least 2 parents
    parents <- dagitty::parents(dag, node)
    if (length(parents) >= 2) {
      collider_count <- collider_count + 1
    }
  }

  collider_count
}

#' Calculate expected edge-panel combinations for faceted plots
#' @param n_edges Number of edges in the DAG
#' @param n_facets Number of facets/panels in the plot
#' @return Expected number of edge-panel combinations
calculate_edge_panel_combinations <- function(n_edges, n_facets) {
  n_edges * n_facets
}

#' Helper to verify edge counts match expectations
#' @param plot A ggplot object
#' @param expected_edges Expected number of edges
#' @param test_name Optional name for the test
expect_edge_count <- function(plot, expected_edges, test_name = NULL) {
  analysis <- analyze_plot_edges(plot)

  info_msg <- if (!is.null(test_name)) {
    paste0(
      test_name,
      ": Expected ",
      expected_edges,
      " edges, got ",
      analysis$total_edges
    )
  } else {
    paste0("Expected ", expected_edges, " edges, got ", analysis$total_edges)
  }

  testthat::expect_equal(analysis$total_edges, expected_edges, info = info_msg)
}
