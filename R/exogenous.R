#' Find Exogenous Variables
#'
#' \code{node_exogenous} tags exogenous variables given an exposure and
#' outcome. \code{ggdag_exogenous} plots all exogenous variables. See
#' \code{dagitty::\link[dagitty]{exogenousVariables}} for details.
#'
#' @param .dag,.tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param ... additional arguments passed to \code{tidy_dagitty()}
#'
#' @return a \code{tidy_dagitty} with an \code{exogenous} column for
#'   exogenous variables or a \code{ggplot}
#' @export
#'
#' @examples
#' dag <- dagify(y ~ x1 + x2 + x3, b ~ x1 + x2)
#' ggdag_exogenous(dag)
#' node_exogenous(dag)
#'
#' @rdname exogenous
#' @name Exogenous Variables
node_exogenous <- function(.dag, ...) {
  .dag <- if_not_tidy_daggity(.dag, ...)
  exogenous_vars <- dagitty::exogenousVariables(.dag$dag)
  .dag$data <- .dag$data %>% dplyr::mutate(exogenous = ifelse(name %in% exogenous_vars, "exogenous", NA))
  .dag
}

#' @rdname exogenous
#' @export
ggdag_exogenous <- function(.tdy_dag, ...) {
  if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_exogenous() %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = exogenous)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks  = "exogenous")
}
