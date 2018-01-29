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
ggdag_canonical <- function(.tdy_dag, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)
  node_canonical(.tdy_dag) %>% ggdag(...)
}
