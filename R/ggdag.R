#' Quickly plot a DAG in ggplot2
#'
#' `ggdag()` is a wrapper to quickly plot DAGs.
#'
#' @param .tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @param edge_type a character vector, the edge geom to use. One of:
#'   "link_arc", which accounts for directed and bidirected edges, "link",
#'   "arc", or "diagonal"
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
#'
#' @return a `ggplot`
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
#' @seealso [ggdag_classic()]
ggdag <- function(.tdy_dag, ..., edge_type = "link_arc", node_size = 16, text_size = 3.88,
                  label_size = text_size,
                  text_col = "white", label_col = "black",
                  node = TRUE, stylized = FALSE, text = TRUE,
                  use_labels = NULL) {
  edge_function <- edge_type_switch(edge_type)

  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    edge_function()

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

#' Quickly plot a DAG in ggplot2
#'
#' `ggdag_classic()` is a wrapper to quickly plot DAGs in a more
#' traditional style.
#'
#' @param .tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @param size text size, with a default of 8.
#' @param label_rect_size specify the `fontsize` argument in
#'   `ggraph::label_rect`; default is `NULL`, in which case it is
#'   scaled relative ti `size`
#' @param text_label text variable, with a default of "name"
#' @param text_col text color, with a default of "black"
#'
#' @return a `ggplot`
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
#' @seealso [ggdag()]
ggdag_classic <- function(.tdy_dag, ..., size = 8, label_rect_size = NULL,
                          text_label = "name", text_col = "black") {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag, ...)

  fontsize <- ifelse(!is.null(label_rect_size), label_rect_size, size * 3.57)

  p <- .tdy_dag %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggplot2::geom_text(ggplot2::aes_string(label = text_label), size = size, col = text_col)

  if (any(.tdy_dag$data$direction == "<->" & !is.na(.tdy_dag$data$direction))) {
    p <- p + geom_dag_edges(ggplot2::aes(start_cap = ggraph::label_rect(name, fontsize = fontsize),
                                end_cap = ggraph::label_rect(to, fontsize = fontsize)))
  } else {
    p <- p + geom_dag_edges_link(ggplot2::aes(start_cap = ggraph::label_rect(name, fontsize = fontsize),
                                         end_cap = ggraph::label_rect(to, fontsize = fontsize)))
  }

  p
}
