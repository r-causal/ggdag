#' Convert DAGS to tidygraph
#'
#' A thin wrapper to convert `tidy_dagitty` and `dagitty` objects to
#' `tbl_graph`, which can then be used to work in `tidygraph` and
#' `ggraph` directly. See [tidygraph::as_tbl_graph()].
#'
#' @param x an object of class `tidy_dagitty` or `dagitty`
#' @param directed logical. Should the constructed graph be directed? Default
#'   is `TRUE`
#' @param ... other arguments passed to `as_tbl_graph`
#'
#' @return a `tbl_graph`
#' @export
#'
#' @examples
#'
#' library(ggraph)
#' library(tidygraph)
#' butterfly_bias() %>%
#'   as_tbl_graph() %>%
#'   ggraph() +
#'   geom_edge_diagonal() +
#'   geom_node_point()
#'
#' @importFrom tidygraph as_tbl_graph
#' @rdname as_tbl_graph
#' @name as_tbl_graph
as_tbl_graph.tidy_dagitty <- function(x, directed = TRUE, ...) {
  x <- dplyr::filter(x, !is.na(to))
  tidygraph::as_tbl_graph(x$data, directed = directed, ...)
}

#' @export
#' @name as_tbl_graph
as_tbl_graph.dagitty <- function(x, directed = TRUE, ...) {
  dagitty::edges(x) %>%
    dplyr::select(from = v, to = w, direction = e) %>%
    tidygraph::as_tbl_graph(directed = directed, ...)
}
