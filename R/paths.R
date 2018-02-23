#' Find Pathways Between Variables
#'
#' \code{node_paths} finds the pathways between a given exposure and
#' outcome. \code{ggdag_paths} plots all pathways. See
#' \code{dagitty::\link[dagitty]{paths}} for details.
#'
#' @param .dag,.tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param from character vector of length 1, name of exposure variable.
#'   Default is \code{NULL}, in which case it will check the input DAG for
#'   exposure.
#' @param to character vector of length 1, name of exposure variable.
#'   Default is \code{NULL}, in which case it will check the input DAG for
#'   exposure.
#' @param adjust_for character vector, a set of variables to control for.
#'   Default is \code{NULL}.
#' @param directed logical. Should only directed paths be shown?
#' @param ... additional arguments passed to \code{tidy_dagitty()}
#' @param node_size size of DAG node
#' @param text_size size of DAG text
#' @param text_col color of DAG text
#' @param node logical. Should nodes be included in the DAG?
#' @param text logical. Should text be included in the DAG?
#' @param use_labels a string. Variable to use for
#' @param spread the width of the fan spread
#'   \code{geom_dag_repel_label()}. Default is \code{NULL}.
#'
#' @return a \code{tidy_dagitty} with a \code{path} column for
#'   path variables and a \code{set} grouping column or a \code{ggplot}
#' @export
#'
#' @examples
#' confounder_triangle(x_y_associated = TRUE) %>%
#'   ggdag_paths(from = "x", to = "y")
#'
#' @rdname paths
#' @name Pathways
#' @importFrom magrittr %$%
dag_paths <- function(.dag, from = NULL, to = NULL, adjust_for = NULL, directed = FALSE, ...) {

  .tdy_dag <- if_not_tidy_daggity(.dag, ...)

  if (is.null(from)) from <- dagitty::exposures(.tdy_dag$dag)
  if (is.null(to)) to <- dagitty::outcomes(.tdy_dag$dag)
  if (is.null(from) || is.null(to)) stop("`exposure` and `outcome` must be set!")

  pathways <- dagitty::paths(.tdy_dag$dag, from, to, Z = adjust_for, directed = directed) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(open) %>%
    dplyr::pull(paths)


  vars <- c(from = from, to = to)

  .tdy_dag$data <- pathways %>%
    purrr::map_df(function(.x) {
      path_df <- .x %>%
        dag() %>%
        dagitty::edges() %>%
        dplyr::select(.from = v, .to = w) %>%
        dplyr::mutate(.from = as.character(.from),
               .to = as.character(.to),
               path = "open path") %>%
        dplyr::left_join(.tdy_dag$data, ., by = c("from" = ".from", "to" = ".to"))

      path_df <- path_df %>%
        filter(name == vars[[1]]) %>%
        dplyr::slice(1) %>%
        dplyr::select(name, x, y, .ggraph.orig_index, circular, .ggraph.index) %$%
        dplyr::add_row(path_df, name = name, x = x, y = y,
                       .ggraph.orig_index = .ggraph.orig_index,
                       circular = circular, .ggraph.index = .ggraph.index,
                       path = "open path")

      path_df %>%
        filter(name == vars[[2]]) %>%
        dplyr::slice(1) %>%
        dplyr::select(name, x, y, .ggraph.orig_index, circular, .ggraph.index) %$%
        dplyr::add_row(path_df, name = name, x = x, y = y,
                       .ggraph.orig_index = .ggraph.orig_index,
                       circular = circular, .ggraph.index = .ggraph.index,
                       path = "open path")
    }, .id = "set")

  .tdy_dag
}


#' @rdname paths
#' @export
ggdag_paths <- function(.tdy_dag, from = NULL, to = NULL, adjust_for = NULL, directed = FALSE, ...,
                                 node_size = 16, text_size = 3.88, text_col = "white",
                                 node = TRUE, text = TRUE, use_labels = NULL) {


  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    dag_paths(from = from, to = to, adjust_for = adjust_for, directed = directed) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, col = path, alpha = path)) +
    geom_dag_edges(ggplot2::aes(edge_alpha = path, edge_colour = path)) +
    ggplot2::facet_wrap(~forcats::fct_inorder(as.factor(set), ordered = TRUE)) +
    theme_dag() +
    ggplot2::scale_alpha_manual(name = "", drop = FALSE, values = c("open path" = 1), na.value = .1, breaks = "open path") +
    ggraph::scale_edge_alpha_manual(name = " ", drop = FALSE, values = c("open path" = 1), na.value = .1, breaks = "open path") +
    ggraph::scale_edge_color_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50", breaks = "open path") +
    ggplot2::scale_color_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50", breaks = "open path") +
    ggplot2::scale_fill_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50", breaks = "open path") +
    ggplot2::scale_x_continuous(expand = expand_scale(c(0.25, 0.25))) +
    ggplot2::scale_y_continuous(expand = expand_scale(c(0.1, 0.1)))


  if (node) p <- p + geom_dag_node(size = node_size)
  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)
  if (!is.null(use_labels)) p <- p +
      geom_dag_label_repel(ggplot2::aes_string(label = use_labels,
                                               fill = "path"),
                           col = "white", show.legend = FALSE)
  p
}

#' @rdname paths
#' @export
ggdag_paths_fan <- function(.tdy_dag, from = NULL, to = NULL, adjust_for = NULL, directed = FALSE, ...,
                        spread = .7, node_size = 16, text_size = 2, text_col = "white",
                        node = TRUE, text = TRUE, use_labels = NULL) {


  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    dag_paths(from = from, to = to, adjust_for = adjust_for, directed = directed) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, col = path, alpha = path)) +
    geom_dag_edges_fan(ggplot2::aes(edge_colour = set, edge_alpha = path), spread = spread) +
    theme_dag() +
    ggplot2::scale_alpha_manual(name = "", drop = FALSE, values = c("open path" = 1), na.value = .1, breaks = "open path") +
    ggraph::scale_edge_alpha_manual(name = " ", drop = FALSE, values = c("open path" = 1), na.value = .1, breaks = "open path") +
    ggraph::scale_edge_color_brewer(drop = FALSE, palette = "Set2", na.value = "grey50") +
    ggplot2::scale_color_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50", breaks = "open path") +
    ggplot2::scale_fill_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50", breaks = "open path") +
    ggplot2::scale_x_continuous(expand = expand_scale(c(0.25, 0.25))) +
    ggplot2::scale_y_continuous(expand = expand_scale(c(0.1, 0.1)))

  if (node) p <- p + geom_dag_node(size = node_size, col = "black")
  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)
  if (!is.null(use_labels)) p <- p +
      geom_dag_label_repel(ggplot2::aes_string(label = use_labels,
                                               fill = "path"),
                           col = "white", show.legend = FALSE)
  p
}


