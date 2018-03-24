#' Generating Equivalent Models
#'
#' Returns a set of complete partially directed acyclic graphs (CPDAGs) given an
#' input DAG. CPDAGs are Markov equivalent to the input graph. See
#' [dagitty::equivalentDAGs()] for details.
#' `node_equivalent_dags()` returns a set of DAGs, while
#' `node_equivalent_class()` tags reversable edges.
#' `ggdag_equivalent_dags()` plots all equivalent DAGs, while
#' `ggdag_equivalent_class()` plots all reversable edges as undirected.
#'
#' @param .dag input graph, an object of class `tidy_dagitty` or `dagitty`
#' @param n maximal number of returned graphs.
#' @param .tdy_dag an object of class `tidy_dagitty` or `dagitty`
#' @param node_size size of DAG node
#' @param text_size size of DAG text
#' @param label_size size of label text
#' @param text_col color of DAG text
#' @param label_col color of label text
#' @param node logical. Should nodes be included in the DAG?
#' @param stylized logical. Should DAG nodes be stylized? If so, use
#'   `geom_dag_nodes` and if not use `geom_dag_point`
#' @param text logical. Should text be included in the DAG?
#' @param use_labels a string. Variable to use for `geom_dag_repel_label()`.
#'   Default is `NULL`.
#' @inheritParams tidy_dagitty
#' @inheritParams scale_dag
#'
#' @return a `tidy_dagitty` with at least one DAG, including a `dag`
#'   column to identify graph set for equivalent DAGs or a `reversable`
#'   column for equivalent classes, or a `ggplot`
#' @export
#'
#' @examples
#' g_ex <- dagify(y ~ x + z, x ~ z)
#'
#' g_ex %>% node_equivalent_class()
#'
#' g_ex %>% ggdag_equivalent_dags()
#'
#' @rdname equivalent
#' @name Equivalent DAGs and Classes
#' @export
node_equivalent_dags <- function(.dag, n = 100, layout = "auto", ...) {

  .dag <- if_not_tidy_daggity(.dag, layout = layout, ...)

  layout_coords <- .dag$data %>%
    dplyr::select(name, x, y) %>%
    dplyr::distinct() %>%
    coords2list()

  dagitty::coordinates(.dag$dag) <- layout_coords

  .dag$data <- dagitty::equivalentDAGs(.dag$dag, n = n) %>%
    purrr::map_df(~as.data.frame(tidy_dagitty(.dagitty = .x)), .id = "dag") %>%
    dplyr::as.tbl()
  .dag
}

#' @rdname equivalent
#' @export
ggdag_equivalent_dags <- function(.tdy_dag, ..., node_size = 16, text_size = 3.88,
                                  label_size = text_size, text_col = "white", label_col = text_col,
                                  node = TRUE, stylized = TRUE, text = TRUE,
                                  use_labels = NULL) {

  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_equivalent_dags(...)

  p <- ggplot2::ggplot(.tdy_dag, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    theme_dag()

  if (node) {
    if (stylized) {
        p <- p + geom_dag_node(size = node_size)
      } else {
        p <- p + geom_dag_point(size = node_size)
      }
    }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) p <- p +
      geom_dag_label_repel(ggplot2::aes_string(label = use_labels), size = text_size,
                           col = label_col, show.legend = FALSE)

  if (dplyr::n_distinct(.tdy_dag$data$dag) > 1) {
    p <- p +
      ggplot2::facet_wrap(~dag) +
      scale_dag(expand_x = expand_scale(c(0.25, 0.25)))
  } else {
    p <- p + scale_dag()
  }

  p
}

#' @rdname equivalent
#' @export
node_equivalent_class <- function(.dag, layout = "auto") {
  .dag <- if_not_tidy_daggity(.dag, layout = layout)
  .dag$data <- dagitty::equivalenceClass(.dag$dag) %>%
    dagitty::edges(.) %>%
    dplyr::filter(e == "--") %>%
    dplyr::select(name = v, reversable = e) %>%
    dplyr::mutate(name = as.character(name)) %>%
    dplyr::left_join(.dag$data, .,  by = "name") %>%
    dplyr::mutate(reversable = !is.na(reversable))

  .dag
}

#' @rdname equivalent
#' @export
ggdag_equivalent_class <- function(.tdy_dag,
                                   expand_x = expand_scale(c(.10, .10)),
                                   expand_y = expand_scale(c(.10, .10)),
                                   breaks = ggplot2::waiver(), ...,
                                   node_size = 16, text_size = 3.88, label_size = text_size,
                                   text_col = "white", label_col = text_col,
                                   node = TRUE, stylized = TRUE,
                                   text = TRUE, use_labels = NULL) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_equivalent_class(...)

  reversable_lines <- dplyr::filter(.tdy_dag$data, reversable)
  non_reversable_lines <- dplyr::filter(.tdy_dag$data, !reversable)
  p <- .tdy_dag %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, edge_alpha = reversable)) +
    geom_dag_edges(data_directed = dplyr::filter(non_reversable_lines, direction != "<->"),
                   data_bidirected = dplyr::filter(non_reversable_lines, direction == "<->")) +
    geom_dag_edges_link(data = reversable_lines, arrow = NULL) +
    theme_dag() +
    ggplot2::scale_color_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50", breaks = breaks) +
    ggplot2::scale_fill_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50") +
    ggplot2::scale_x_continuous(expand = expand_x) +
    ggplot2::scale_y_continuous(expand = expand_y) +
    ggraph::scale_edge_alpha_manual(name = "Reversable", drop = FALSE, values = c(.1, 1))

  if (node) {
    if (stylized) {
        p <- p + geom_dag_node(size = node_size)
      } else {
        p <- p + geom_dag_point(size = node_size)
      }
    }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) p <- p +
      geom_dag_label_repel(ggplot2::aes_string(label = use_labels), size = text_size,
                           col = label_col, show.legend = FALSE)
  p
}
