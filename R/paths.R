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
#' @return a `tidy_dagitty` with a `path` column for path variables and a `set`
#'   grouping column or a `ggplot`.
#'
#' @examples
#' confounder_triangle(x_y_associated = TRUE) %>%
#'   dag_paths(from = "x", to = "y")
#'
#' confounder_triangle(x_y_associated = TRUE) %>%
#'   ggdag_paths(from = "x", to = "y")
#'
#' butterfly_bias(x_y_associated = TRUE) %>%
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
    stop("`exposure` and `outcome` must be set!")
  }

  pathways <- dagitty::paths(
    pull_dag(.tdy_dag),
    from,
    to,
    Z = adjust_for,
    limit = limit,
    directed = directed
  ) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(open) %>%
    dplyr::pull(paths)

  vars <- c(from = from, to = to)

  # Handle case where no open paths exist
  if (length(pathways) == 0) {
    # Add a path column with all NA values and a set column
    update_dag_data(.tdy_dag) <- pull_dag_data(.tdy_dag) %>%
      dplyr::mutate(path = NA_character_, set = "1")
    return(.tdy_dag)
  }

  update_dag_data(.tdy_dag) <- pathways %>%
    purrr::map_df(
      function(.x) {
        path_df <- .x %>%
          dag2() %>%
          dagitty::edges() %>%
          dplyr::select(.from = v, .to = w) %>%
          dplyr::mutate(
            .from = as.character(.from),
            .to = as.character(.to),
            path = "open path"
          ) %>%
          ggdag_left_join(
            pull_dag_data(.tdy_dag),
            .,
            by = c("name" = ".from", "to" = ".to")
          )

        any_x_unopend <- any(path_df$name == vars[[1]] & is.na(path_df$path))
        if (any_x_unopend) {
          x_has_no_children <- any(
            path_df$name == vars[[1]] & is.na(path_df$to)
          )
          if (x_has_no_children) {
            path_df[path_df$name == vars[[1]], "path"] <- "open path"
          } else {
            path_df <- path_df %>%
              filter(name == vars[[1]]) %>%
              dplyr::slice(1) %>%
              dplyr::mutate(path = "open path", to = NA, direction = NA) %>%
              dplyr::bind_rows(path_df, .)
          }
        }

        y_has_no_children <- any(path_df$name == vars[[2]] & is.na(path_df$to))
        if (y_has_no_children) {
          path_df[path_df$name == vars[[2]], "path"] <- "open path"
        } else {
          path_df <- path_df %>%
            filter(name == vars[[2]]) %>%
            dplyr::slice(1) %>%
            dplyr::mutate(path = "open path", to = NA, direction = NA) %>%
            dplyr::bind_rows(path_df, .)
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
  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    dag_paths(
      from = from,
      to = to,
      adjust_for = adjust_for,
      limit = limit,
      directed = directed
    ) %>%
    ggplot2::ggplot(aes_dag(color = path)) +
    geom_dag_edges(
      ggplot2::aes(edge_colour = path),
      show.legend = if (shadow) TRUE else FALSE
    ) +
    ggplot2::facet_wrap(~ forcats::fct_inorder(as.factor(set))) +
    breaks("open path") +
    expand_plot(
      expand_x = expansion(c(0.25, 0.25)),
      expand_y = expansion(c(0.1, 0.1))
    )

  if (shadow) {
    vals <- c("unadjusted" = "black", "adjusted" = "grey80")
  } else {
    vals <- c("unadjusted" = "black", "adjusted" = "#FFFFFF00")
  }

  p <- p +
    ggraph::scale_edge_color_discrete(
      drop = FALSE,
      na.value = if (shadow) "grey80" else "#FFFFFF00",
      na.translate = if (shadow) TRUE else FALSE,
      limits = "open path"
    ) +
    ggplot2::scale_color_discrete(
      drop = FALSE,
      na.value = if (shadow) "grey80" else "#FFFFFF00",
      na.translate = if (shadow) TRUE else FALSE,
      limits = "open path"
    )

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
  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    dag_paths(
      from = from,
      to = to,
      adjust_for = adjust_for,
      limit = limit,
      directed = directed,
      paths_only = !shadow
    ) %>%
    ggplot2::ggplot(aes_dag()) +
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
    ggplot2::scale_color_discrete(drop = FALSE, breaks = "open path") +
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
