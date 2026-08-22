#' Convert DAGS to tidygraph
#'
#' A thin wrapper to convert `tidy_dagitty` and `dagitty` objects to
#' `tbl_graph`, which can then be used to work in `tidygraph` and
#' `ggraph` directly. See [tidygraph::as_tbl_graph()].
#'
#' @param x an object of class `tidy_dagitty` or `dagitty`
#' @param directed logical. Should the constructed graph be directed? Default
#'   is `TRUE`
#' @param ... currently unused
#'
#' @return a `tbl_graph`
#' @export
#'
#' @examples
#'
#' library(ggraph)
#' library(tidygraph)
#' butterfly_bias() |>
#'   as_tbl_graph() |>
#'   ggraph() +
#'   geom_edge_diagonal() +
#'   geom_node_point()
#'
#' @importFrom tidygraph as_tbl_graph
#' @rdname as_tbl_graph
#' @name as_tbl_graph
as_tbl_graph.tidy_dagitty <- function(x, directed = TRUE, ...) {
  dag_data <- pull_dag_data(x)

  # build from an explicit node table so nodes with no edges become vertices
  nodes <- dplyr::distinct(dag_data, name = as.character(.data$name))
  edges <- dag_data |>
    dplyr::filter(!is.na(.data$to)) |>
    dplyr::rename(from = "name") |>
    dplyr::relocate("from", "to")

  tidygraph::tbl_graph(
    nodes = nodes,
    edges = edges,
    directed = directed,
    node_key = "name"
  )
}

#' @export
#' @name as_tbl_graph
as_tbl_graph.dagitty <- function(x, directed = TRUE, ...) {
  dag_edges <- dagitty::edges(x)

  # dagitty::edges() returns a zero-column data frame when there are no edges
  edges <- if (nrow(dag_edges) == 0 || ncol(dag_edges) == 0) {
    data.frame(
      from = character(0),
      to = character(0),
      direction = character(0)
    )
  } else {
    dag_edges |>
      dplyr::transmute(
        from = as.character(.data$v),
        to = as.character(.data$w),
        direction = as.character(.data$e)
      )
  }

  tidygraph::tbl_graph(
    nodes = data.frame(name = names(x)),
    edges = edges,
    directed = directed,
    node_key = "name"
  )
}
