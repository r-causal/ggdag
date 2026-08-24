#' Classify paths as direct, backdoor, or other
#'
#' A backdoor path is one whose first edge points into the exposure, so it is
#' recognizable from the printed path: `dagitty::paths()` prints a path starting
#' at `from`, and node names cannot contain spaces, so the first arrow is the
#' first token after the start node. Both `<-` and `<->` (latent confounding)
#' point into the exposure. Anything left over, such as a path opened by
#' conditioning on a collider, is neither causal nor backdoor.
#'
#' @param paths character vector of printed paths.
#' @param directed_paths character vector of printed directed paths.
#' @return A character vector of "direct", "backdoor", and "other".
#' @noRd
classify_path_types <- function(paths, directed_paths) {
  dplyr::case_when(
    paths %in% directed_paths ~ "direct",
    stringr::str_detect(paths, "^\\S+ <-") ~ "backdoor",
    .default = "other"
  )
}

#' Turn `dagitty::paths()` output into a tibble
#'
#' `dagitty` returns empty path descriptions when it cannot print a path, which
#' would otherwise reach `dag2()` as an unparseable string, so drop them.
#'
#' @param paths_obj the list returned by `dagitty::paths()`.
#' @return A tibble with `paths` and `open` columns.
#' @noRd
path_results_to_tibble <- function(paths_obj) {
  if (length(paths_obj$paths) == 0) {
    return(tibble::tibble(paths = character(), open = logical()))
  }

  tibble::tibble(paths = paths_obj$paths, open = paths_obj$open) |>
    dplyr::filter(nzchar(.data$paths))
}

#' Stop when path enumeration is asked for more than one endpoint
#'
#' `dagitty` prints a path only when the path graph has a single source, and
#' returns empty descriptions otherwise, so paths must be enumerated one
#' exposure-outcome pair at a time.
#'
#' @param from,to the resolved endpoints.
#' @param fn the calling function's name, for the message.
#' @return `NULL`, invisibly, or an error.
#' @noRd
check_single_endpoints <- function(from, to, fn, call = rlang::caller_env()) {
  if (length(from) <= 1 && length(to) <= 1) {
    return(invisible(NULL))
  }

  abort(
    c(
      "{.fun {fn}} supports a single exposure and a single outcome.",
      "x" = "Got {length(from)} value{?s} for {.arg from} and {length(to)} value{?s} for {.arg to}.",
      "i" = "Call {.fun {fn}} once per exposure-outcome pair, or use {.fun query_paths}, which enumerates every pair."
    ),
    error_class = "ggdag_type_error",
    call = call
  )
}

#' Recover the underlying DAG data from a previous `dag_paths()` result
#'
#' `dag_paths()` repeats the DAG data once per open path and adds a node-only
#' row for an endpoint that a path reaches only as a target. Recomputing paths
#' from its own output would otherwise treat those repeats and extra rows as
#' part of the DAG.
#'
#' @param .tdy_dag a `tidy_dagitty` object.
#' @return A `tidy_dagitty` object without path columns.
#' @noRd
strip_path_results <- function(.tdy_dag) {
  .df <- pull_dag_data(.tdy_dag)

  if (nrow(.df) > 0 && all(c("path", "set") %in% names(.df))) {
    .df <- dplyr::filter(.df, .data$set == .df$set[[1]])
    # `tidy_dagitty()` gives a node-only row only to a node with no outgoing
    # edges, so a node-only row for an edge source is one of the added rows
    edge_sources <- unique(.df$name[!is.na(.df$to)])
    .df <- dplyr::filter(
      .df,
      !is.na(.data$to) | .data$name %nin% edge_sources
    )
  }

  update_dag_data(.tdy_dag) <- dplyr::select(
    .df,
    -dplyr::any_of(c("path", "path_type", "set"))
  )

  .tdy_dag
}

#' Find Open Paths Between Variables
#'
#' `dag_paths` finds open paths between a given exposure and outcome.
#' `ggdag_paths` and `ggdag_paths_fan` plot all open paths. See
#' [dagitty::paths()] for details.
#'
#' @inheritParams dag_params
#' @inheritParams path_params
#' @inheritParams dagitty::paths
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
#'   grouping column, and a `path_type` column classifying paths as "direct" (a
#'   directed causal path), "backdoor" (a path whose first edge points into the
#'   exposure), or "other" (any other path, such as one through a collider), or
#'   a `ggplot`.
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
  .tdy_dag <- strip_path_results(.tdy_dag)

  if (is.null(from)) {
    from <- dagitty::exposures(pull_dag(.tdy_dag))
  }
  if (is.null(to)) {
    to <- dagitty::outcomes(pull_dag(.tdy_dag))
  }
  if (is_empty_or_null(from) || is_empty_or_null(to)) {
    abort(
      c(
        "Both {.arg from} (exposure) and {.arg to} (outcome) must be set.",
        "i" = "Use {.code dag_paths(dag, from = \"x\", to = \"y\")} to specify paths."
      ),
      error_class = "ggdag_missing_error"
    )
  }
  check_single_endpoints(from, to, "dag_paths")

  # Get all paths of the requested type
  all_paths_raw <- dagitty::paths(
    pull_dag(.tdy_dag),
    from,
    to,
    Z = adjust_for,
    limit = limit,
    directed = directed
  )

  # Get directed paths to identify causal paths
  causal_paths_raw <- dagitty::paths(
    pull_dag(.tdy_dag),
    from,
    to,
    Z = adjust_for,
    limit = limit,
    directed = TRUE
  )

  all_paths_info <- path_results_to_tibble(all_paths_raw)
  causal_paths_info <- path_results_to_tibble(causal_paths_raw)

  # Filter for open paths
  all_open_paths <- all_paths_info$paths[all_paths_info$open]
  causal_open_paths <- causal_paths_info$paths[causal_paths_info$open]

  # Determine path types
  pathways <- all_open_paths
  path_types <- classify_path_types(pathways, causal_open_paths)

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
      # a node pair can carry both a directed and a bidirected edge, and each
      # lies on a different path, so the edge symbol is part of the join key
      path_edges <- .x |>
        dag2() |>
        dagitty::edges() |>
        dplyr::transmute(
          .from = as.character(.data$v),
          .to = as.character(.data$w),
          .direction = as.character(.data$e),
          path = "open path",
          path_type = .path_type
        )

      path_df <- pull_dag_data(.tdy_dag) |>
        dplyr::mutate(.direction = as.character(.data$direction)) |>
        (\(x) {
          ggdag_left_join(
            x,
            path_edges,
            by = c("name" = ".from", "to" = ".to", ".direction" = ".direction")
          )
        })() |>
        dplyr::select(-".direction")

      x_unmarked <- !any(path_df$name == vars[[1]] & !is.na(path_df$path))
      if (x_unmarked) {
        x_has_no_children <- any(
          path_df$name == vars[[1]] & is.na(path_df$to)
        )
        if (x_has_no_children) {
          path_df[path_df$name == vars[[1]], "path"] <- "open path"
          path_df[path_df$name == vars[[1]], "path_type"] <- .path_type
        } else {
          path_df <- path_df |>
            filter(.data$name == vars[[1]]) |>
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

      y_unmarked <- !any(path_df$name == vars[[2]] & !is.na(path_df$path))
      if (y_unmarked) {
        y_has_no_children <- any(path_df$name == vars[[2]] & is.na(path_df$to))
        if (y_has_no_children) {
          path_df[path_df$name == vars[[2]], "path"] <- "open path"
          path_df[path_df$name == vars[[2]], "path_type"] <- .path_type
        } else {
          path_df <- path_df |>
            filter(.data$name == vars[[2]]) |>
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

      path_df
    },
    .id = "set"
  )

  if (paths_only) {
    .tdy_dag <- dplyr::filter(.tdy_dag, .data$path == "open path")
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
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option("edge_cap", 8),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  edge_engine = ggdag_option("edge_engine", "ggraph"),
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  if (missing(edge_type)) {
    edge_type <- ggdag_option("edge_type", "link_arc")
  }
  edge_engine <- match.arg(edge_engine, c("ggraph", "ggarrow"))

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
    breaks(c("direct", "backdoor", "other"), name = "path") +
    expand_plot(
      expand_x = expansion(c(0.25, 0.25)),
      expand_y = expansion(c(0.1, 0.1))
    )

  if (use_edges) {
    f_bidirected <- if (!shadow) {
      function(x) {
        dplyr::filter(
          x,
          .data$path == "open path",
          .data$direction == "<->"
        )
      }
    } else {
      filter_direction("<->")
    }

    f_directed <- if (!shadow) {
      function(x) {
        dplyr::filter(
          x,
          .data$path == "open path",
          .data$direction == "->"
        )
      }
    } else {
      filter_direction("->")
    }

    if (identical(edge_engine, "ggarrow")) {
      rlang::check_installed(
        "ggarrow",
        reason = "to use edge_engine = \"ggarrow\"."
      )
      resect <- edge_cap * size
      arrow_head <- ggdag_option("arrow_head", NULL) %||%
        ggarrow::arrow_head_wings()
      arrow_fins <- ggdag_option("arrow_fins", NULL)

      p <- p +
        quick_plot_arrow_edges(
          mapping = with_edge_curvature(
            ggplot2::aes(colour = .data$path_type),
            p$data
          ),
          data_directed = f_directed,
          data_bidirected = f_bidirected,
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          resect = resect,
          linewidth = edge_width * size,
          length = arrow_length_unit(arrow_length * size),
          show.legend = FALSE
        )

      p <- p +
        ggplot2::scale_color_discrete(
          name = "path",
          drop = FALSE,
          na.value = if (shadow) "grey80" else "#FFFFFF00",
          na.translate = TRUE,
          limits = c("direct", "backdoor", "other")
        )
    } else {
      warn_if_curvature_ignored(p$data)

      p <- p +
        geom_dag_edges(
          data_directed = f_directed,
          data_bidirected = f_bidirected,
          ggplot2::aes(edge_colour = .data$path_type)
        )

      p <- p +
        ggraph::scale_edge_color_discrete(
          name = "path",
          drop = FALSE,
          na.value = if (shadow) "grey80" else "#FFFFFF00",
          na.translate = if (shadow) TRUE else FALSE,
          limits = c("direct", "backdoor", "other"),
          guide = "none"
        ) +
        ggplot2::scale_color_discrete(
          name = "path",
          drop = FALSE,
          na.value = if (shadow) "grey80" else "#FFFFFF00",
          na.translate = TRUE,
          limits = c("direct", "backdoor", "other")
        )
    }
  }

  p <- p +
    geom_dag(
      data = if (!shadow) {
        function(x) dplyr::filter(x, .data$path == "open path")
      },
      size = size,
      node_size = node_size,
      text_size = text_size,
      label_size = label_size,
      text_col = text_col,
      label_col = label_col,
      edge_width = edge_width,
      edge_cap = edge_cap,
      arrow_length = arrow_length,
      edge_engine = edge_engine,
      use_edges = FALSE,
      use_nodes = use_nodes,
      use_stylized = use_stylized,
      use_text = use_text,
      use_labels = use_labels,
      label_geom = label_geom,
      unified_legend = TRUE,
      key_glyph = draw_key_dag_combined,
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
  spread = 0.7,
  size = 1,
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option("edge_cap", 8),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  unified_legend = TRUE,
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
        ggplot2::aes(edge_colour = .data$set, edge_alpha = .data$path),
        spread = spread
      ) +
      ggplot2::scale_alpha_manual(
        drop = FALSE,
        values = c("open path" = 1),
        na.value = 0.35,
        breaks = "open path",
        limits = "open path"
      ) +
      ggraph::scale_edge_alpha_manual(
        drop = FALSE,
        values = c("open path" = 1),
        na.value = 0.15,
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
      label_geom = label_geom,
      unified_legend = unified_legend,
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
    return(tibble::tibble(
      from = character(),
      to = character(),
      direction = character()
    ))
  }

  edges_list <- purrr::map(paths, \(path) {
    path_dag <- dag2(path)
    edges <- dagitty::edges(path_dag)
    tibble::tibble(
      from = as.character(edges$v),
      to = as.character(edges$w),
      # a node pair can carry both a directed and a bidirected edge, each on a
      # different path, so the edge symbol identifies which one this is
      direction = as.character(edges$e)
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
#' @inheritParams dag_params
#' @inheritParams path_params
#' @param adjust_for character vector, a set of variables to control for.
#'   Default is `NULL`.
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @param open_only logical. If `TRUE` (default), only considers open paths. If
#'   `FALSE`, includes information about closed paths as well.
#'
#' @return A `tidy_dagitty` object with additional columns:
#'   * `path_type`: "direct", "backdoor", "other", or "both" classification for
#'     each edge
#'   * `open`: logical indicating if the edge is part of an open path
#'
#' @details
#' Edges are classified by examining the paths between exposure and outcome:
#' * Direct edges appear only on directed causal paths
#' * Backdoor edges appear only on backdoor paths, whose first edge points into
#'   the exposure
#' * Other edges appear only on paths that are neither causal nor backdoor, such
#'   as a path through a collider
#' * Both edges appear on more than one of those kinds of path
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
  check_single_endpoints(from, to, "edge_backdoor")

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

  all_paths_info <- path_results_to_tibble(all_paths)
  causal_paths_info <- path_results_to_tibble(causal_paths)

  # Classify each path, then attribute that class to the edges it uses
  all_paths_info$path_type <- classify_path_types(
    all_paths_info$paths,
    causal_paths_info$paths
  )

  if (open_only) {
    all_paths_info <- dplyr::filter(all_paths_info, .data$open)
  }

  all_edge_info <- purrr::pmap(
    all_paths_info,
    function(paths, open, path_type) {
      edges <- extract_edges_from_paths(paths)
      if (nrow(edges) == 0) {
        return(edges)
      }

      edges$edge_type <- path_type
      edges$open <- open
      edges
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::distinct()

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
    dplyr::group_by(.data$from, .data$to, .data$direction) |>
    dplyr::summarise(
      path_type = if (dplyr::n_distinct(.data$edge_type) > 1) {
        "both"
      } else {
        .data$edge_type[[1]]
      },
      open = any(.data$open),
      .groups = "drop"
    )

  # Join with the tidy dag data
  dag_data <- pull_dag_data(.tdy_dag)

  # Add the edge classification
  updated_data <- dag_data |>
    dplyr::mutate(.direction = as.character(.data$direction)) |>
    dplyr::left_join(
      edge_classifications,
      by = c("name" = "from", "to" = "to", ".direction" = "direction")
    ) |>
    dplyr::select(-".direction")

  # If open_only = TRUE, set path_type to NA for closed paths
  if (open_only) {
    updated_data <- updated_data |>
      dplyr::mutate(
        path_type = dplyr::if_else(
          !is.na(open) & open,
          .data$path_type,
          NA_character_
        )
      )
  }

  # Update the tidy dag
  update_dag_data(.tdy_dag) <- updated_data

  .tdy_dag
}
