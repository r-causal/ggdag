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
#' @param as_factor treat `status` variable as factor
#' @inheritParams geom_dag
#'
#' @return a `tidy_dagitty` with a `status` column for
#'   variable status or a `ggplot`
#' @export
#'
#' @examples
#' dag <- dagify(
#'   l ~ x + y,
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
ggdag_status <- function(
  .tdy_dag,
  ...,
  size = 1,
  edge_type = c("link_arc", "link", "arc", "diagonal"),
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  p <- if_not_tidy_daggity(.tdy_dag) |>
    node_status(...) |>
    ggplot2::ggplot(aes_dag(color = status)) +
    scale_adjusted(include_color = FALSE) +
    breaks(c("exposure", "outcome", "latent"))

  p <- p +
    geom_dag(
      size = size,
      edge_type = edge_type,
      node_size = node_size,
      text_size = text_size,
      label_size = label_size,
      text_col = text_col,
      label_col = label_col,
      edge_width = edge_width,
      edge_cap = edge_cap,
      arrow_length = arrow_length,
      use_edges = use_edges,
      use_nodes = use_nodes,
      use_stylized = use_stylized,
      use_text = use_text,
      use_labels = use_labels,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )

  p
}
