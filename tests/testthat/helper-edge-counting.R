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

#' Count the edges that survive edge capping and reach the device
#'
#' `count_total_edges()` counts edge groups in the built plot data, which is
#' before ggraph trims each path back to the node caps. An edge whose whole
#' path is consumed by the caps still appears in the built data but never
#' draws, so the only way to see it is to render the plot and walk the forced
#' grob tree.
#'
#' @param plot A ggplot object
#' @param width,height Device size in inches
#' @return Number of edges actually drawn
count_drawn_edges <- function(plot, width = 10, height = 8) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path, width = width, height = height)
  on.exit(
    {
      grDevices::dev.off()
      unlink(path)
    },
    add = TRUE
  )

  print(plot)
  grid::grid.force()

  grob_names <- unique(grep(
    "cappedpathgrob",
    grid::grid.ls(print = FALSE)$name,
    value = TRUE
  ))

  drawn <- vapply(
    grob_names,
    function(nm) {
      children <- grid::grid.get(nm)$children
      if (length(children) == 0) {
        return(0L)
      }

      child <- children[[1]]
      if (inherits(child, "zeroGrob")) 0L else length(unique(child$id))
    },
    integer(1)
  )

  sum(drawn)
}
