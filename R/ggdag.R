#' Title
#'
#' @param .tdy_dag
#'
#' @return
#' @export
#'
#' @examples
ggdag <- function(.tdy_dag, ...) {
  if_not_tidy_daggity(.tdy_dag, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text() +
    theme_dag() +
    scale_dag()
}

#' Title
#'
#' @param .tdy_dag
#' @param size
#' @param label_rect_size
#'
#' @return
#' @export
#'
#' @examples
ggdag_classic <- function(.tdy_dag, size = 8, label_rect_size = NULL, ...) {
  if_not_tidy_daggity(.tdy_dag, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges(ggplot2::aes(start_cap = ggraph::label_rect(name, fontsize = size * 3.57),
                                end_cap = ggraph::label_rect(to, fontsize = size * 3.57))) +
    ggplot2::geom_text(ggplot2::aes(label = name), size = size) +
    theme_dag() +
    scale_dag()
}
