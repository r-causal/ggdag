#' Find Open Paths Between Variables
#'
#' `dag_paths` finds open paths between a given exposure and outcome.
#' `ggdag_paths` and `ggdag_paths_fan` plot all open paths. See
#' [dagitty::paths()] for details.
#'
#' @inheritParams dagitty::paths
#' @param .dag,.tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param from character vector of length 1, name of exposure variable. Default
#'   is `NULL`, in which case it will check the input DAG for exposure.
#' @param to character vector of length 1, name of exposure variable. Default is
#'   `NULL`, in which case it will check the input DAG for exposure.
#' @param adjust_for character vector, a set of variables to control for.
#'   Default is `NULL`.
#' @param directed logical. Should only directed paths be shown?
#' @param paths_only logical. Should only open paths be returned? Default is
#'   `FALSE`, which includes every variable and edge in the DAG regardless
#'   if they are part of the path.
#' @param shadow logical. Show edges which are not on an open path?
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @inheritParams geom_dag
#' @param spread the width of the fan spread
#'
#' @return a `tidy_dagitty` with a `path` column for path variables, a `set`
#'   grouping column, and a `path_type` column classifying paths as "backdoor"
#'   or "direct", or a `ggplot`.
#'
#' @examples
#' confounder_triangle(x_y_associated = TRUE) |>
#'   dag_paths(from = "x", to = "y")
#'
#' confounder_triangle(x_y_associated = TRUE) |>
#'   ggdag_paths(from = "x", to = "y")
#'
#' butterfly_bias(x_y_associated = TRUE) |>
#'   ggdag_paths_fan(shadow = TRUE)
#'
#' @rdname paths
#' @name Pathways
#' @importFrom magrittr %$%
#' @export
dag_paths <- function(
  .dag,
  from = NULL,
  to = NULL,
  adjust_for = NULL,
  limit = 100,
  directed = FALSE,
  paths_only = FALSE,
  ...
) {
  .tdy_dag <- if_not_tidy_daggity(.dag, ...)

  if (is.null(from)) {
    from <- dagitty::exposures(pull_dag(.tdy_dag))
  }
  if (is.null(to)) {
    to <- dagitty::outcomes(pull_dag(.tdy_dag))
  }
  if (is.null(from) || is.null(to)) {
    abort(
      c(
        "Both {.arg from} (exposure) and {.arg to} (outcome) must be set.",
        "i" = "Use {.code dag_paths(dag, from = \"x\", to = \"y\")} to specify paths."
      ),
      error_class = "ggdag_missing_error"
    )
  }

  # Get all paths (directed = FALSE)
  all_paths_raw <- dagitty::paths(
    pull_dag(.tdy_dag),
    from,
    to,
    Z = adjust_for,
    limit = limit,
    directed = FALSE
  )

  # Convert to tibble, handling empty paths
  if (length(all_paths_raw$paths) == 0) {
    all_paths_info <- tibble::tibble(paths = character(), open = logical())
  } else {
    all_paths_info <- dplyr::as_tibble(all_paths_raw)
  }

  # Get directed paths to identify causal paths
  causal_paths_raw <- dagitty::paths(
    pull_dag(.tdy_dag),
    from,
    to,
    Z = adjust_for,
    limit = limit,
    directed = TRUE
  )

  # Convert to tibble, handling empty paths
  if (length(causal_paths_raw$paths) == 0) {
    causal_paths_info <- tibble::tibble(paths = character(), open = logical())
  } else {
    causal_paths_info <- dplyr::as_tibble(causal_paths_raw)
  }

  # Filter for open paths
  if (nrow(all_paths_info) == 0 || all(all_paths_info$open == FALSE)) {
    all_open_paths <- character(0)
  } else {
    all_open_paths <- all_paths_info |>
      dplyr::filter(open) |>
      dplyr::pull(paths)
  }

  if (nrow(causal_paths_info) == 0 || all(causal_paths_info$open == FALSE)) {
    causal_open_paths <- character(0)
  } else {
    causal_open_paths <- causal_paths_info |>
      dplyr::filter(open) |>
      dplyr::pull(paths)
  }

  # Determine path types
  pathways <- all_open_paths
  path_types <- ifelse(pathways %in% causal_open_paths, "direct", "backdoor")

  vars <- c(from = from, to = to)

  # Handle case where no open paths exist
  if (length(pathways) == 0) {
    # Add a path column with all NA values and a set column
    update_dag_data(.tdy_dag) <- pull_dag_data(.tdy_dag) |>
      dplyr::mutate(path = NA_character_, path_type = NA_character_, set = "1")
    return(.tdy_dag)
  }

  update_dag_data(.tdy_dag) <- purrr::map2_df(
    pathways,
    path_types,
    function(.x, .path_type) {
      path_df <- .x |>
        dag2() |>
        dagitty::edges() |>
        dplyr::select(.from = v, .to = w) |>
        dplyr::mutate(
          .from = as.character(.from),
          .to = as.character(.to),
          path = "open path",
          path_type = .path_type
        ) |>
        (\(x) {
          ggdag_left_join(
            pull_dag_data(.tdy_dag),
            x,
            by = c("name" = ".from", "to" = ".to")
          )
        })()

      any_x_unopend <- any(path_df$name == vars[[1]] & is.na(path_df$path))
      if (any_x_unopend) {
        x_has_no_children <- any(
          path_df$name == vars[[1]] & is.na(path_df$to)
        )
        if (x_has_no_children) {
          path_df[path_df$name == vars[[1]], "path"] <- "open path"
          path_df[path_df$name == vars[[1]], "path_type"] <- .path_type
        } else {
          path_df <- path_df |>
            filter(name == vars[[1]]) |>
            dplyr::slice(1) |>
            dplyr::mutate(
              path = "open path",
              path_type = .path_type,
              to = NA,
              direction = NA,
              xend = NA,
              yend = NA
            ) |>
            (\(x) dplyr::bind_rows(path_df, x))()
        }
      }

      y_has_no_children <- any(path_df$name == vars[[2]] & is.na(path_df$to))
      if (y_has_no_children) {
        path_df[path_df$name == vars[[2]], "path"] <- "open path"
        path_df[path_df$name == vars[[2]], "path_type"] <- .path_type
      } else {
        path_df <- path_df |>
          filter(name == vars[[2]]) |>
          dplyr::slice(1) |>
          dplyr::mutate(
            path = "open path",
            path_type = .path_type,
            to = NA,
            direction = NA,
            xend = NA,
            yend = NA
          ) |>
          (\(x) dplyr::bind_rows(path_df, x))()
      }

      path_df
    },
    .id = "set"
  )

  if (paths_only) {
    .tdy_dag <- dplyr::filter(.tdy_dag, path == "open path")
  }

  .tdy_dag
}


#' @rdname paths
#' @export
ggdag_paths <- function(
  .tdy_dag,
  from = NULL,
  to = NULL,
  adjust_for = NULL,
  limit = 100,
  directed = FALSE,
  shadow = TRUE,
  ...,
  size = 1,
  edge_type = c("link_arc", "link", "arc", "diagonal"),
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  p <- if_not_tidy_daggity(.tdy_dag, ...) |>
    dag_paths(
      from = from,
      to = to,
      adjust_for = adjust_for,
      limit = limit,
      directed = directed
    ) |>
    ggplot2::ggplot(aes_dag(color = .data$path_type)) +
    ggplot2::facet_wrap(~ forcats::fct_inorder(as.factor(set))) +
    breaks(c("direct", "backdoor"), name = "path") +
    expand_plot(
      expand_x = expansion(c(0.25, 0.25)),
      expand_y = expansion(c(0.1, 0.1))
    )

  if (use_edges) {
    f_bidirected <- if (!shadow) {
      function(x) {
        dplyr::filter(
          x,
          path == "open path",
          direction == "<->"
        )
      }
    } else {
      filter_direction("<->")
    }

    f_directed <- if (!shadow) {
      function(x) {
        dplyr::filter(
          x,
          path == "open path",
          direction == "->"
        )
      }
    } else {
      filter_direction("->")
    }

    p <- p +
      geom_dag_edges(
        data_directed = f_directed,
        data_bidirected = f_bidirected,
        ggplot2::aes(edge_colour = .data$path_type),
        show.legend = if (shadow) TRUE else FALSE
      )

    if (shadow) {
      vals <- c("unadjusted" = "black", "adjusted" = "grey80")
    } else {
      vals <- c("unadjusted" = "black", "adjusted" = "#FFFFFF00")
    }

    p <- p +
      ggraph::scale_edge_color_discrete(
        name = "path",
        drop = FALSE,
        na.value = if (shadow) "grey80" else "#FFFFFF00",
        na.translate = if (shadow) TRUE else FALSE,
        limits = c("direct", "backdoor")
      ) +
      ggplot2::scale_color_discrete(
        name = "path",
        drop = FALSE,
        na.value = if (shadow) "grey80" else "#FFFFFF00",
        na.translate = if (shadow) TRUE else FALSE,
        limits = c("direct", "backdoor")
      )
  }

  p <- p +
    geom_dag(
      data = if (!shadow) function(x) dplyr::filter(x, path == "open path"),
      size = size,
      node_size = node_size,
      text_size = text_size,
      label_size = label_size,
      text_col = text_col,
      label_col = label_col,
      edge_width = edge_width,
      edge_cap = edge_cap,
      arrow_length = arrow_length,
      use_edges = FALSE,
      use_nodes = use_nodes,
      use_stylized = use_stylized,
      use_text = use_text,
      use_labels = use_labels,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )

  p
}

#' @rdname paths
#' @export
ggdag_paths_fan <- function(
  .tdy_dag,
  from = NULL,
  to = NULL,
  adjust_for = NULL,
  limit = 100,
  directed = FALSE,
  ...,
  shadow = TRUE,
  spread = .7,
  size = 1,
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  p <- if_not_tidy_daggity(.tdy_dag, ...) |>
    dag_paths(
      from = from,
      to = to,
      adjust_for = adjust_for,
      limit = limit,
      directed = directed,
      paths_only = !shadow
    ) |>
    ggplot2::ggplot(aes_dag())

  if (use_edges) {
    p <- p +
      geom_dag_edges_fan(
        ggplot2::aes(edge_colour = set, edge_alpha = path),
        spread = spread
      ) +
      ggplot2::scale_alpha_manual(
        drop = FALSE,
        values = c("open path" = 1),
        na.value = .35,
        breaks = "open path",
        limits = "open path"
      ) +
      ggraph::scale_edge_alpha_manual(
        drop = FALSE,
        values = c("open path" = 1),
        na.value = .15,
        breaks = "open path",
        guide = "none",
        limits = "open path"
      ) +
      ggraph::scale_edge_colour_discrete(name = "open path", drop = FALSE) +
      ggplot2::scale_color_discrete(drop = FALSE, breaks = "open path")
  }

  p <- p +
    expand_plot(
      expand_x = expansion(c(0.25, 0.25)),
      expand_y = expansion(c(0.1, 0.1))
    )

  p <- p +
    geom_dag(
      size = size,
      node_size = node_size,
      text_size = text_size,
      label_size = label_size,
      text_col = text_col,
      label_col = label_col,
      edge_width = edge_width,
      edge_cap = edge_cap,
      arrow_length = arrow_length,
      use_edges = FALSE,
      use_nodes = use_nodes,
      use_stylized = use_stylized,
      use_text = use_text,
      use_labels = use_labels,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )

  p
}

# Helper function to extract edges from paths
extract_edges_from_paths <- function(paths) {
  if (length(paths) == 0) {
    return(tibble::tibble(from = character(), to = character()))
  }

  edges_list <- purrr::map(paths, \(path) {
    path_dag <- dag2(path)
    edges <- dagitty::edges(path_dag)
    tibble::tibble(
      from = as.character(edges$v),
      to = as.character(edges$w)
    )
  })

  dplyr::bind_rows(edges_list) |>
    dplyr::distinct()
}

#' Classify DAG edges as backdoor or direct
#'
#' `edge_backdoor()` identifies edges as being on backdoor paths or direct
#' causal paths between an exposure and outcome. This function adds edge-level
#' information to the tidy DAG object, classifying each edge based on the types
#' of paths it appears on.
#'
#' @param .dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param from character vector of length 1, name of exposure variable. Default
#'   is `NULL`, in which case it will check the input DAG for exposure.
#' @param to character vector of length 1, name of exposure variable. Default is
#'   `NULL`, in which case it will check the input DAG for exposure.
#' @param adjust_for character vector, a set of variables to control for.
#'   Default is `NULL`.
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @param open_only logical. If `TRUE` (default), only considers open paths. If
#'   `FALSE`, includes information about closed paths as well.
#'
#' @return A `tidy_dagitty` object with additional columns:
#'   * `path_type`: "backdoor", "direct", or "both" classification for each edge
#'   * `open`: logical indicating if the edge is part of an open path
#'
#' @details
#' Edges are classified by examining the paths between exposure and outcome:
#' * Direct edges appear only on directed causal paths
#' * Backdoor edges appear only on backdoor paths
#' * Both edges appear on both direct and backdoor paths
#'
#' When `open_only = TRUE` (default), `path_type` will be NA for edges that are
#' only part of closed paths.
#'
#' @examples
#' # Create a DAG with both direct and backdoor paths
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ z,
#'   exposure = "x",
#'   outcome = "y"
#' )
#'
#' # Classify edges
#' edge_backdoor(dag)
#'
#' # Include closed paths
#' edge_backdoor(dag, open_only = FALSE)
#'
#' @export
edge_backdoor <- function(
  .dag,
  from = NULL,
  to = NULL,
  adjust_for = NULL,
  open_only = TRUE,
  ...
) {
  .tdy_dag <- if_not_tidy_daggity(.dag, ...)

  if (is.null(from)) {
    from <- dagitty::exposures(pull_dag(.tdy_dag))
    if (length(from) == 0) from <- NULL
  }
  if (is.null(to)) {
    to <- dagitty::outcomes(pull_dag(.tdy_dag))
    if (length(to) == 0) to <- NULL
  }
  if (is.null(from) || is.null(to)) {
    abort(
      c(
        "Both {.arg from} (exposure) and {.arg to} (outcome) must be set.",
        "i" = "Use {.code edge_backdoor(dag, from = \"x\", to = \"y\")} to specify paths."
      ),
      error_class = "ggdag_missing_error"
    )
  }

  # Get all paths (both open and closed)
  all_paths <- dagitty::paths(
    pull_dag(.tdy_dag),
    from,
    to,
    Z = adjust_for,
    directed = FALSE
  )

  # Get directed causal paths
  causal_paths <- dagitty::paths(
    pull_dag(.tdy_dag),
    from,
    to,
    Z = adjust_for,
    directed = TRUE
  )

  # Separate open and closed paths
  if (length(all_paths$paths) == 0) {
    all_open_paths <- character(0)
    all_closed_paths <- character(0)
  } else {
    all_open_paths <- all_paths$paths[all_paths$open]
    all_closed_paths <- all_paths$paths[!all_paths$open]
  }

  if (length(causal_paths$paths) == 0) {
    causal_open_paths <- character(0)
    causal_closed_paths <- character(0)
  } else {
    causal_open_paths <- causal_paths$paths[causal_paths$open]
    causal_closed_paths <- causal_paths$paths[!causal_paths$open]
  }

  # Calculate backdoor paths (open non-causal paths)
  backdoor_open_paths <- setdiff(all_open_paths, causal_open_paths)
  backdoor_closed_paths <- setdiff(all_closed_paths, causal_closed_paths)

  # Extract edges from different path types
  backdoor_open_edges <- extract_edges_from_paths(backdoor_open_paths)
  backdoor_closed_edges <- extract_edges_from_paths(backdoor_closed_paths)
  direct_open_edges <- extract_edges_from_paths(causal_open_paths)
  direct_closed_edges <- extract_edges_from_paths(causal_closed_paths)

  # Add classification to edges
  if (nrow(backdoor_open_edges) > 0) {
    backdoor_open_edges$edge_type <- "backdoor"
    backdoor_open_edges$open <- TRUE
  }

  if (nrow(backdoor_closed_edges) > 0) {
    backdoor_closed_edges$edge_type <- "backdoor"
    backdoor_closed_edges$open <- FALSE
  }

  if (nrow(direct_open_edges) > 0) {
    direct_open_edges$edge_type <- "direct"
    direct_open_edges$open <- TRUE
  }

  if (nrow(direct_closed_edges) > 0) {
    direct_closed_edges$edge_type <- "direct"
    direct_closed_edges$open <- FALSE
  }

  # Combine all edge classifications
  all_edge_info <- dplyr::bind_rows(
    backdoor_open_edges,
    if (!open_only) backdoor_closed_edges,
    direct_open_edges,
    if (!open_only) direct_closed_edges
  )

  # Handle empty edge case
  if (nrow(all_edge_info) == 0) {
    # No paths found, return original data with NA columns
    dag_data <- pull_dag_data(.tdy_dag)
    updated_data <- dag_data |>
      dplyr::mutate(
        path_type = NA_character_,
        open = NA
      )
    update_dag_data(.tdy_dag) <- updated_data
    return(.tdy_dag)
  }

  # Classify edges based on which path types they appear on
  edge_classifications <- all_edge_info |>
    dplyr::group_by(from, to) |>
    dplyr::summarise(
      path_type = if (
        "backdoor" %in% .data$edge_type && "direct" %in% .data$edge_type
      ) {
        "both"
      } else if ("backdoor" %in% .data$edge_type) {
        "backdoor"
      } else {
        "direct"
      },
      open = any(.data$open),
      .groups = "drop"
    )

  # Join with the tidy dag data
  dag_data <- pull_dag_data(.tdy_dag)

  # Add the edge classification
  updated_data <- dag_data |>
    dplyr::left_join(
      edge_classifications,
      by = c("name" = "from", "to" = "to")
    )

  # If open_only = TRUE, set path_type to NA for closed paths
  if (open_only) {
    updated_data <- updated_data |>
      dplyr::mutate(
        path_type = ifelse(open %in% TRUE, path_type, NA_character_)
      )
  }

  # Update the tidy dag
  update_dag_data(.tdy_dag) <- updated_data

  .tdy_dag
}
