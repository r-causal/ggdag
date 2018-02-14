#' Canonicalize a DAG
#'
#' Takes an input graph with bidirected edges and replaces every bidirected edge
#' x <-> y with a substructure x <- L -> y, where L is a latent variable. See
#' \code{dagitty::\link[dagitty]{canonicalize}} for details. Undirected edges
#' are not currently supported in \code{ggdag}.
#'
#' @param .dag,.tdy_dag input graph, an object of class \code{tidy_dagitty} or
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
#' @return a \code{tidy_dagitty} that includes L or a \code{ggplot}
#' @export
#'
#' @examples
#' dag <- dagify(y ~ x + z, x ~~ z)
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
ggdag_canonical <- function(.tdy_dag, ..., node_size = 16, text_size = 3.88,
                            text_col = "white", node = TRUE, text = TRUE,
                            use_labels = NULL) {
  if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_canonical() %>%
    ggdag(node_size = node_size, text_size = text_size,
          text_col = text_col, node = node, text = text,
          use_labels = use_labels)
}
