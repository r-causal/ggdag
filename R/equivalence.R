#' Title
#'
#' @param .dag
#' @param n
#'
#' @return
#' @export
#'
#' @examples
node_equivalent_dags <- function(.dag, n = 100, layout = "auto") {
  .dag <- if_not_tidy_daggity(.dag)
  .dag$data <- dagitty::equivalentDAGs(.dag$dag, n = n) %>%
    purrr::map_df(~as.data.frame(tidy_dagitty(.x, layout = layout)), .id = "dag") %>%
    dplyr::as.tbl()
  .dag
}

#' Title
#'
#' @param .tdy_dag
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_equivalent_dags <- function(.tdy_dag, ...) {

  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_equivalent_dags(...)

  p <- ggdag(.tdy_dag)

  if (dplyr::n_distinct(.tdy_dag$data$dag) > 1) p <- p + ggplot2::facet_wrap(~dag)

  p
}

#' Title
#'
#' @param .dag
#' @param layout
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param .tdy_dag
#' @param cap
#' @param ...
#' @param expand_x
#' @param expand_y
#' @param breaks
#'
#' @return
#' @export
#'
#' @examples
ggdag_equivalent_class <- function(.tdy_dag, cap = ggraph::circle(8, 'mm'),
                                   expand_x = ggplot2::expand_scale(c(.10, .10)),
                                   expand_y = ggplot2::expand_scale(c(.10, .10)),
                                   breaks = ggplot2::waiver(), ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_equivalent_class(...)

  reversable_lines <- dplyr::filter(.tdy_dag$data, reversable)
  non_reversable_lines <- dplyr::filter(.tdy_dag$data, !reversable)
  .tdy_dag %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, edge_alpha = reversable)) +
    geom_dag_edges(data_directed = dplyr::filter(non_reversable_lines, direction != "<->"),
                   data_bidirected = dplyr::filter(non_reversable_lines, direction == "<->")) +
    ggraph::geom_edge_link(data = reversable_lines, start_cap = cap, end_cap = cap) +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    ggplot2::scale_color_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50", breaks = breaks) +
    ggplot2::scale_fill_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50") +
    ggplot2::scale_x_continuous(expand = expand_x) +
    ggplot2::scale_y_continuous(expand = expand_y) +
    ggraph::scale_edge_alpha_manual(name = "Reversable", drop = FALSE, values = c(.1, 1))
}
