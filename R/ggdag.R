#' Quickly plot a DAG in ggplot2
#'
#' \code{ggdag()} is a wrapper to quickly plot DAGs.
#'
#' @param .tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param ... additional arguments passed to \code{tidy_dagitty()}
#'
#' @return a \code{ggplot}
#' @export
#'
#' @examples
#'
#' dag  <- dagify(y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   w1 ~~ w2)
#'
#' ggdag(dag)
#' ggdag(dag) + theme_dag_blank()
#'
#' ggdag(dagitty::randomDAG(5, .5))
#'
#' @seealso \code{\link{ggdag_classic}}
ggdag <- function(.tdy_dag, ...) {
  if_not_tidy_daggity(.tdy_dag, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text() +
    theme_dag() +
    scale_dag()
}

#' Quickly plot a DAG in ggplot2
#'
#' \code{ggdag_classic()} is a wrapper to quickly plot DAGs in a more
#' traditional style.
#'
#' @param .tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param ... additional arguments passed to \code{tidy_dagitty()}
#' @param size text size, with a default of 8.
#' @param label_rect_size specify the \code{fontsize} argument in
#'   \code{ggraph::label_rect}; default is \code{NULL}, in which case it is
#'   scaled relative ti \code{size}
#'
#' @return a \code{ggplot}
#' @export
#'
#' @examples
#'
#' dag  <- dagify(y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   w1 ~~ w2)
#'
#' ggdag_classic(dag)
#' ggdag_classic(dag) + theme_dag_blank()
#'
#' ggdag_classic(dagitty::randomDAG(5, .5))
#'
#' @seealso \code{\link{ggdag}}
ggdag_classic <- function(.tdy_dag, size = 8, label_rect_size = NULL, ...) {
  if_not_tidy_daggity(.tdy_dag, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges(ggplot2::aes(start_cap = ggraph::label_rect(name, fontsize = size * 3.57),
                                end_cap = ggraph::label_rect(to, fontsize = size * 3.57))) +
    ggplot2::geom_text(ggplot2::aes(label = name), size = size) +
    theme_dag() +
    scale_dag()
}
