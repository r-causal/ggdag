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
#' @param shadow logical. Show edges which are not on an open path? Ignored if
#'   `paths_only` is `TRUE`.
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @param node_size size of DAG node
#' @param text_size size of DAG text
#' @param label_size size of label text
#' @param text_col color of DAG text
#' @param label_col label color
#' @param node logical. Should nodes be included in the DAG?
#' @param stylized logical. Should DAG nodes be stylized? If so, use
#'   `geom_dag_nodes` and if not use `geom_dag_point`
#' @param text logical. Should text be included in the DAG?
#' @param use_labels a string. Variable to use for `geom_dag_label_repel()`.
#'   Default is `NULL`.
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
dag_paths <- function(.dag, from = NULL, to = NULL, adjust_for = NULL, limit = 100, directed = FALSE, paths_only = FALSE, ...) {
  .tdy_dag <- if_not_tidy_daggity(.dag, ...)

  if (is.null(from)) from <- dagitty::exposures(pull_dag(.tdy_dag))
  if (is.null(to)) to <- dagitty::outcomes(pull_dag(.tdy_dag))
  if (is.null(from) || is.null(to)) stop("`exposure` and `outcome` must be set!")

  pathways <- dagitty::paths(pull_dag(.tdy_dag), from, to, Z = adjust_for, limit = limit, directed = directed) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(open) %>%
    dplyr::pull(paths)


  vars <- c(from = from, to = to)

  update_dag_data(.tdy_dag) <- pathways %>%
    purrr::map_df(function(.x) {
      path_df <- .x %>%
        dag2() %>%
        dagitty::edges() %>%
        dplyr::select(.from = v, .to = w) %>%
        dplyr::mutate(
          .from = as.character(.from),
          .to = as.character(.to),
          path = "open path"
        ) %>%
        ggdag_left_join(pull_dag_data(.tdy_dag), ., by = c("name" = ".from", "to" = ".to"))

      any_x_unopend <- any(path_df$name == vars[[1]] & is.na(path_df$path))
      if (any_x_unopend) {
        x_has_no_children <- any(path_df$name == vars[[1]] & is.na(path_df$to))
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
    }, .id = "set")

  if (paths_only) .tdy_dag <- dplyr::filter(.tdy_dag, path == "open path")

  .tdy_dag
}


#' @rdname paths
#' @export
ggdag_paths <- function(.tdy_dag, from = NULL, to = NULL, adjust_for = NULL, limit = 100, directed = FALSE, shadow = FALSE, ...,
                        node_size = 16, text_size = 3.88, label_size = text_size, text_col = "white", label_col = text_col,
                        node = TRUE, stylized = FALSE, text = TRUE, use_labels = NULL) {
  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    dag_paths(from = from, to = to, adjust_for = adjust_for, limit = limit, directed = directed, paths_only = !shadow) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, col = path, alpha = path)) +
    geom_dag_edges(ggplot2::aes(edge_alpha = path, edge_colour = path)) +
    ggplot2::facet_wrap(~ forcats::fct_inorder(as.factor(set))) +
    ggplot2::scale_alpha_manual(drop = FALSE, values = c("open path" = 1), na.value = .35, breaks = "open path", limits = "open path") +
    ggraph::scale_edge_alpha_manual(drop = FALSE, values = c("open path" = 1), na.value = .35, breaks = "open path", limits = "open path") +
    ggraph::scale_edge_colour_hue(drop = FALSE, breaks = "open path") +
    ggplot2::scale_color_hue(drop = FALSE, breaks = "open path") +
    expand_plot(
      expand_x = expansion(c(0.25, 0.25)),
      expand_y = expansion(c(0.1, 0.1))
    )


  if (node) {
    if (stylized) {
      p <- p +
        geom_dag_node(
          data = function(.x) dplyr::filter(.x, is.na(path)),
          size = node_size
        ) +
        geom_dag_node(
          data = function(.x) dplyr::filter(.x, !is.na(path)),
          size = node_size
        )
    } else {
      p <- p +
        geom_dag_point(
          data = function(.x) dplyr::filter(.x, is.na(path)),
          size = node_size
        ) +
        geom_dag_point(
          data = function(.x) dplyr::filter(.x, !is.na(path)),
          size = node_size
        )
    }
  }
  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)
  if (!is.null(use_labels)) {
    p <- p +
      geom_dag_label_repel(
        ggplot2::aes(
          label = !!rlang::sym(use_labels),
          fill = path
        ),
        size = label_size,
        col = label_col, show.legend = FALSE
      )
  }
  p
}

#' @rdname paths
#' @export
ggdag_paths_fan <- function(.tdy_dag, from = NULL, to = NULL, adjust_for = NULL, limit = 100, directed = FALSE, ..., shadow = FALSE,
                            spread = .7, node_size = 16, text_size = 3.88, label_size = text_size, text_col = "white", label_col = text_col,
                            node = TRUE, stylized = FALSE, text = TRUE, use_labels = NULL) {
  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    dag_paths(from = from, to = to, adjust_for = adjust_for, limit = limit, directed = directed, paths_only = !shadow) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges_fan(
      ggplot2::aes(edge_colour = set, edge_alpha = path),
      spread = spread
    ) +
    ggplot2::scale_alpha_manual(drop = FALSE, values = c("open path" = 1), na.value = .35, breaks = "open path", limits = "open path") +
    ggraph::scale_edge_alpha_manual(drop = FALSE, values = c("open path" = 1), na.value = .15, breaks = "open path", guide = "none", limits = "open path") +
    ggraph::scale_edge_colour_hue(name = "open path", drop = FALSE) +
    ggplot2::scale_color_hue(drop = FALSE, breaks = "open path") +
    expand_plot(
      expand_x = expansion(c(0.25, 0.25)),
      expand_y = expansion(c(0.1, 0.1))
    )

  if (node) {
    if (stylized) {
      p <- p + geom_dag_node(ggplot2::aes(alpha = path), size = node_size, col = "black", show.legend = FALSE)
    } else {
      p <- p + geom_dag_point(ggplot2::aes(alpha = path), size = node_size, col = "black", show.legend = FALSE)
    }
  }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) {
    p <- p +
      geom_dag_label_repel(
        ggplot2::aes(
          label = !!rlang::sym(use_labels),
          fill = path
        ),
        size = text_size,
        col = label_col,
        show.legend = FALSE
      )
  }
  p
}
