#' Title
#'
#' @param .dag
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
node_exogenous <- function(.dag, ...) {
  .dag <- if_not_tidy_daggity(.dag)
  exogenous_vars <- dagitty::exogenousVariables(.dag$dag)
  .dag$data <- .dag$data %>% dplyr::mutate(exogenous = ifelse(name %in% exogenous_vars, "exogenous", NA))
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
ggdag_exogenous <- function(.tdy_dag, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_exogenous() %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = exogenous)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks  = "exogenous")
}
