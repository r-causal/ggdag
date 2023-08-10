#' Find variable status
#'
#' Detects variable status given a DAG (exposure, outcome, latent). See
#' [dagitty::VariableStatus()] for details.
#'
#' `node_collider` tags variable status and `ggdag_collider` plots all
#' variable statuses.
#'
#' @param .dag,.tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @param edge_type a character vector, the edge geom to use. One of:
#'   "link_arc", which accounts for directed and bidirected edges, "link",
#'   "arc", or "diagonal"
#' @param as_factor treat `status` variable as factor
#' @param node_size size of DAG node
#' @param text_size size of DAG text
#' @param label_size size of label text
#' @param text_col color of DAG text
#' @param label_col color of label text
#' @param node logical. Should nodes be included in the DAG?
#' @param stylized logical. Should DAG nodes be stylized? If so, use
#'   `geom_dag_nodes` and if not use `geom_dag_point`
#' @param text logical. Should text be included in the DAG?
#' @param use_labels a string. Variable to use for `geom_dag_label_repel()`.
#'   Default is `NULL`.
#'
#' @return a `tidy_dagitty` with a `status` column for
#'   variable status or a `ggplot`
#' @export
#'
#' @examples
#' dag <- dagify(l ~ x + y,
#'   y ~ x,
#'   exposure = "x",
#'   outcome = "y",
#'   latent = "l"
#' )
#'
#' node_status(dag)
#' ggdag_status(dag)
#'
#' @rdname status
#' @name Variable Status
node_status <- function(.dag, as_factor = TRUE, ...) {
  .tdy_dag <- if_not_tidy_daggity(.dag, ...)
  .exposures <- dagitty::exposures(pull_dag(.tdy_dag))
  .outcomes <- dagitty::outcomes(pull_dag(.tdy_dag))
  .latents <- dagitty::latents(pull_dag(.tdy_dag))
  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    status = dplyr::case_when(
      name %in% .exposures ~ "exposure",
      name %in% .outcomes ~ "outcome",
      name %in% .latents ~ "latent",
      TRUE ~ NA
    )
  )

  if (as_factor) {
    .tdy_dag <- dplyr::mutate(.tdy_dag, status = factor(status, exclude = NA))
  }

  .tdy_dag
}

#' @rdname status
#' @export
ggdag_status <- function(.tdy_dag, ..., edge_type = "link_arc", node_size = 16, text_size = 3.88,
                         label_size = text_size,
                         text_col = "white", label_col = text_col,
                         node = TRUE, stylized = FALSE, text = TRUE,
                         use_labels = NULL) {
  edge_function <- edge_type_switch(edge_type)

  p <- if_not_tidy_daggity(.tdy_dag) %>%
    node_status(...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = status)) +
    edge_function() +
    scale_adjusted() +
    breaks(c("exposure", "outcome", "latent"))

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
          fill = status
        ),
        size = text_size,
        col = label_col,
        show.legend = FALSE
      )
  }
  p
}
