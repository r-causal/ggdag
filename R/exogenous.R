#' Find Exogenous Variables
#'
#' `node_exogenous` tags exogenous variables given an exposure and
#' outcome. `ggdag_exogenous` plots all exogenous variables. See
#' [dagitty::exogenousVariables()] for details.
#'
#' @param .dag,.tdy_dag input graph, an object of class `tidy_dagitty` or
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
#' @return a `tidy_dagitty` with an `exogenous` column for
#'   exogenous variables or a `ggplot`
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
ggdag_exogenous <- function(.tdy_dag, ..., node_size = 16, text_size = 3.88,
                            edge_type = "link_arc",
                            label_size = text_size,
                            text_col = "white", label_col = text_col,
                            node = TRUE, stylized = FALSE, text = TRUE,
                            use_labels = NULL) {
  edge_function <- edge_type_switch(edge_type)

  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_exogenous() %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = exogenous)) +
    edge_function() +
    scale_adjusted() +
    breaks("exogenous")

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
        ggplot2::aes(
          label = !!rlang::sym(use_labels),
          fill = exogenous
        ),
        size = text_size,
        col = label_col, show.legend = FALSE
      )
  }
  p
}
