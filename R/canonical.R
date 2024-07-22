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
#' @inheritParams geom_dag
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
  dagitty::canonicalize(pull_dag(.dag))$g %>%
    tidy_dagitty(..., use_existing_coords = FALSE)
}

#' @rdname canonicalize
#' @export
ggdag_canonical <- function(
    .tdy_dag,
    ...,
    edge_type = "link_arc",
    node_size = 16,
    text_size = 3.88,
    label_size = text_size,
    text_col = "white",
    label_col = text_col,
    use_edges = TRUE,
    use_nodes = TRUE,
    use_stylized = FALSE,
    use_text = TRUE,
    use_labels = NULL,
    label = NULL,
    text = NULL,
    node = deprecated(),
    stylized = deprecated()) {
  if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_canonical() %>%
    ggdag(
      node_size = node_size,
      text_size = text_size,
      label_size,
      edge_type = edge_type,
      text_col = text_col,
      label_col = label_col,
      use_edges = use_edges,
      use_nodes = use_nodes,
      use_stylized = use_stylized,
      use_text = use_text,
      use_labels = use_labels
    )
}
