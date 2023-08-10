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
#' @param use_labels a string. Variable to use for `geom_dag_label_repel()`.
#'   Default is `NULL`.
#' @inheritParams tidy_dagitty
#' @inheritParams scale_adjusted
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
  extra_columns <- has_extra_columns(.dag)

  layout_coords <- .dag %>%
    pull_dag_data() %>%
    dplyr::select(name, x, y) %>%
    dplyr::distinct() %>%
    coords2list()

  updated_dag <- pull_dag(.dag)
  dagitty::coordinates(updated_dag) <- layout_coords
  update_dag(.dag) <- updated_dag

  if (extra_columns) extra_column_df <- select_extra_columns(.dag)

  update_dag_data(.dag) <- dagitty::equivalentDAGs(pull_dag(.dag), n = n) %>%
    purrr::map_df(map_equivalence, .id = "dag") %>%
    dplyr::as_tibble()

  if (extra_columns) {
    .dag <- dplyr::left_join(.dag, extra_column_df, by = "name")
  }

  .dag
}

has_extra_columns <- function(.x) {
  !purrr::is_empty(get_extra_column_names(.x))
}

get_extra_column_names <- function(.x) {
  standard_names <- c("name", "x", "y", "direction", "to", "xend", "yend", "circular")
  dag_columns <- names(pull_dag_data(.x))
  setdiff(dag_columns, standard_names)
}

select_extra_columns <- function(.x) {
  .x %>%
    pull_dag_data() %>%
    dplyr::select(name, get_extra_column_names(.x))
}

map_equivalence <- function(.x) {
  as.data.frame(tidy_dagitty(.dagitty = .x))
}

#' @rdname equivalent
#' @export
ggdag_equivalent_dags <- function(.tdy_dag, ..., node_size = 16, text_size = 3.88,
                                  label_size = text_size, text_col = "white", label_col = "black",
                                  node = TRUE, stylized = FALSE, text = TRUE,
                                  use_labels = NULL) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_equivalent_dags(...)

  p <- ggplot2::ggplot(.tdy_dag, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges()

  if (node) {
    if (stylized) {
      p <- p + geom_dag_node(size = node_size)
    } else {
      p <- p + geom_dag_point(size = node_size)
    }
  }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) {
    p <- p +
      geom_dag_label_repel(
        ggplot2::aes(label = !!rlang::sym(use_labels)),
        size = text_size,
        col = label_col, show.legend = FALSE
      )
  }

  if (dplyr::n_distinct(pull_dag_data(.tdy_dag)$dag) > 1) {
    p <- p +
      ggplot2::facet_wrap(~ dag) +
      expand_plot(
        expand_x = expansion(c(0.25, 0.25)),
        expand_y = expansion(c(0.25, 0.25))
      )
  }

  p
}

hash <- function(x, y) {
  purrr::pmap_chr(list(x, y), ~ paste0(sort(c(.x, .y)), collapse = "_"))
}

#' @rdname equivalent
#' @export
node_equivalent_class <- function(.dag, layout = "auto") {
  .dag <- if_not_tidy_daggity(.dag, layout = layout)
  ec_data <- dagitty::equivalenceClass(pull_dag(.dag)) %>%
    dagitty::edges(.) %>%
    dplyr::filter(e == "--") %>%
    dplyr::select(name = v, reversable = e, to = w) %>%
    dplyr::mutate_at(c("name", "to"), as.character) %>%
    dplyr::mutate(hash = hash(name, to)) %>%
    dplyr::select(hash, reversable)

  .dag <- .dag %>%
    dplyr::mutate(hash = hash(name, to)) %>%
    dplyr::left_join(ec_data, by = "hash") %>%
    dplyr::mutate(reversable = !is.na(reversable)) %>%
    dplyr::select(-hash)

  .dag
}

#' @rdname equivalent
#' @inheritParams expand_plot
#' @export
ggdag_equivalent_class <- function(.tdy_dag,
                                   expand_x = expansion(c(.10, .10)),
                                   expand_y = expansion(c(.10, .10)),
                                   breaks = ggplot2::waiver(), ...,
                                   node_size = 16, text_size = 3.88, label_size = text_size,
                                   text_col = "white", label_col = text_col,
                                   node = TRUE, stylized = FALSE,
                                   text = TRUE, use_labels = NULL) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_equivalent_class(...)

  reversable_lines <- dplyr::filter(pull_dag_data(.tdy_dag), reversable)
  non_reversable_lines <- dplyr::filter(pull_dag_data(.tdy_dag), !reversable)
  p <- .tdy_dag %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, edge_alpha = reversable)) +
    geom_dag_edges(
      data_directed = dplyr::filter(non_reversable_lines, direction != "<->"),
      data_bidirected = dplyr::filter(non_reversable_lines, direction == "<->")
    ) +
    geom_dag_edges_link(data = reversable_lines, arrow = NULL) +
    breaks(breaks) +
    ggraph::scale_edge_alpha_manual(name = "Reversable", drop = FALSE, values = c(.30, 1))

  if (node) {
    if (stylized) {
      p <- p + geom_dag_node(size = node_size)
    } else {
      p <- p + geom_dag_point(size = node_size)
    }
  }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) {
    p <- p +
      geom_dag_label_repel(
        ggplot2::aes(label = !!rlang::sym(use_labels)),
        size = text_size,
        col = label_col,
        show.legend = FALSE
      )
  }
  p
}
