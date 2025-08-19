#' Quickly plot a DAG in ggplot2
#'
#' `ggdag()` is a wrapper to quickly plot DAGs.
#'
#' @param .tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @inheritParams geom_dag
#'
#' @return a `ggplot`
#' @export
#'
#' @examples
#'
#' dag <- dagify(
#'   y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   w1 ~ ~w2
#' )
#'
#' ggdag(dag)
#' ggdag(dag) + theme_dag()
#'
#' ggdag(dagitty::randomDAG(5, .5))
#'
#' @seealso [ggdag_classic()]
ggdag <- function(
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
  unified_legend = TRUE,
  key_glyph = NULL,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  if_not_tidy_daggity(.tdy_dag, ...) |>
    ggplot2::ggplot(aes_dag()) +
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
      unified_legend = unified_legend,
      key_glyph = key_glyph,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )
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
#' @param use_edges logical value whether to include edges
#'
#' @return a `ggplot`
#' @export
#'
#' @examples
#'
#' dag <- dagify(
#'   y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   w1 ~ ~w2
#' )
#'
#' ggdag_classic(dag)
#' ggdag_classic(dag) + theme_dag_blank()
#'
#' ggdag_classic(dagitty::randomDAG(5, .5))
#'
#' @seealso [ggdag()]
ggdag_classic <- function(
  .tdy_dag,
  ...,
  size = 8,
  label_rect_size = NULL,
  text_label = "name",
  text_col = "black",
  use_edges = TRUE
) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag, ...)

  fontsize <- ifelse(!is.null(label_rect_size), label_rect_size, size * 3.57)

  p <- .tdy_dag |>
    ggplot2::ggplot(aes_dag()) +
    ggplot2::geom_text(
      ggplot2::aes(label = !!rlang::sym(text_label)),
      size = size,
      col = text_col
    )

  if (use_edges) {
    if (
      any(
        pull_dag_data(.tdy_dag)$direction == "<->" &
          !is.na(pull_dag_data(.tdy_dag)$direction)
      )
    ) {
      p <- p +
        geom_dag_edges(ggplot2::aes(
          start_cap = ggraph::label_rect(name, fontsize = fontsize),
          end_cap = ggraph::label_rect(to, fontsize = fontsize)
        ))
    } else {
      p <- p +
        geom_dag_edges_link(ggplot2::aes(
          start_cap = ggraph::label_rect(name, fontsize = fontsize),
          end_cap = ggraph::label_rect(to, fontsize = fontsize)
        ))
    }
  }

  p
}
