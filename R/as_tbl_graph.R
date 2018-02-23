#' Convert DAGS to tidygraph
#'
#' A thin wrapper to convert \code{tidy_dagitty} and \code{dagitty} objects to
#' \code{tbl_graph}, which can then be used to work in \code{tidygraph} and
#' \code{ggraph} directly.
#'
#' @param x an object of class \code{tidy_dagitty} or \code{dagitty}
#' @param directed logical. Should the constructed graph be directed? Default
#'   is \code{TRUE}
#' @param ... other arguments passed to \code{as_tbl_graph}
#'
#' @return a \code{tbl_graph}
#' @export
#'
#' @examples
#'
#' library(ggraph)
#' butterfly_bias() %>%
#'  ggraph() +
#'    geom_edge_diagonal() +
#'    geom_node_point()
#'
#' @importFrom tidygraph as_tbl_graph
#' @rdname as_tbl_graph
#' @name as_tbl_graph
as_tbl_graph.tidy_dagitty <- function(x, directed = TRUE, ...) {
  x <- dplyr::filter(x, !is.na(to))
  tidygraph::as_tbl_graph(directed = directed, ...)
}

#' @export
#' @name as_tbl_graph
as_tbl_graph.dagitty <- function(x, directed = TRUE, ...) {
  dagitty::edges(x) %>%
    dplyr::select(from = v, to = w, direction = e) %>%
    tidygraph::as_tbl_graph(directed = directed, ...)
}
