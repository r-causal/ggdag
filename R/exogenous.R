#' Find Exogenous Variables
#'
#' `node_exogenous` tags exogenous variables given an exposure and
#' outcome. `ggdag_exogenous` plots all exogenous variables. See
#' [dagitty::exogenousVariables()] for details.
#'
#' @param .dag,.tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @inheritParams geom_dag
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
  exogenous_vars <- dagitty::exogenousVariables(pull_dag(.dag))

  dplyr::mutate(
    .dag,
    exogenous = ifelse(.data$name %in% exogenous_vars, "exogenous", NA)
  )
}

#' @rdname exogenous
#' @export
ggdag_exogenous <- function(
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
  label_geom = geom_dag_label_repel,
  unified_legend = TRUE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  p <- if_not_tidy_daggity(.tdy_dag, ...) |>
    node_exogenous() |>
    ggplot2::ggplot(aes_dag(color = .data$exogenous)) +
    scale_adjusted(include_color = FALSE) +
    breaks("exogenous")

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
      label_geom = label_geom,
      unified_legend = unified_legend,
      key_glyph = draw_key_dag_point,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )

  p
}
