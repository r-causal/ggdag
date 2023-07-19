#' Canonicalize a DAG
#'
#' Takes an input graph with bidirected edges and replaces every bidirected edge
#' x <-> y with a substructure x <- L -> y, where L is a latent variable. See
#' [dagitty::canonicalize()] for details. Undirected edges
#' are not currently supported in `ggdag`.
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
#' @return a `tidy_dagitty` that includes L or a `ggplot`
#' @export
#'
#' @examples
#' dag <- dagify(y ~ x + z, x ~ ~z)
#'
#' ggdag(dag)
#'
#' node_canonical(dag)
#' ggdag_canonical(dag)
#'
#' @rdname canonicalize
#' @name Canonicalize DAGs
node_canonical <- function(.dag, ...) {
  .dag <- if_not_tidy_daggity(.dag)
  dagitty::canonicalize(.dag$dag)$g %>% tidy_dagitty(...)
}

#' @rdname canonicalize
#' @export
ggdag_canonical <- function(.tdy_dag, ..., edge_type = "link_arc", node_size = 16, text_size = 3.88,
                            label_size = text_size,
                            text_col = "white", label_col = text_col,
                            node = TRUE, stylized = FALSE, text = TRUE,
                            use_labels = NULL) {
  if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_canonical() %>%
    ggdag(
      node_size = node_size, text_size = text_size, label_size,
      edge_type = edge_type, text_col = text_col, label_col = label_col,
      node = node, stylized = stylized, text = text, use_labels = use_labels
    )
}
