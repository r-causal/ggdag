#' Quickly plot a DAG in ggplot2
#'
#' \code{ggdag()} is a wrapper to quickly plot DAGs.
#'
#' @param .tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param ... additional arguments passed to \code{tidy_dagitty()}
#' @param node_size size of DAG node
#' @param text_size size of DAG text
#' @param text_col color of DAG text
#' @param node logical. Should nodes be included in the DAG?
#' @param text logical. Should text be included in the DAG?
#' @param use_labels a string. Variable to use for
#'   \code{geom_dag_repel_label()}. Default is \code{NULL}.
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
ggdag <- function(.tdy_dag, ..., node_size = 16, text_size = 3.88,
                  text_col = "white", node = TRUE, text = TRUE,
                  use_labels = NULL) {
  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    theme_dag() +
    scale_dag()

  if (node) p <- p + geom_dag_node(size = node_size)
  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)
  if (!is.null(use_labels)) p <- p +
      geom_dag_label_repel(ggplot2::aes_string(label = use_labels),
                           show.legend = FALSE)
  p
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
#' @param text_label text variable, with a default of "name"
#' @param text_col text color, with a default of "black"
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
ggdag_classic <- function(.tdy_dag, ..., size = 8, label_rect_size = NULL,
                          text_label = "name", text_col = "black") {

  fontsize <- ifelse(!is.null(label_rect_size), label_rect_size, size * 3.57)

  if_not_tidy_daggity(.tdy_dag, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges(ggplot2::aes(start_cap = ggraph::label_rect(name, fontsize = fontsize),
                                end_cap = ggraph::label_rect(to, fontsize = fontsize))) +
    ggplot2::geom_text(ggplot2::aes_string(label = text_label), size = size, col = text_col) +
    theme_dag() +
    scale_dag()
}
