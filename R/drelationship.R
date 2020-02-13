#' D-relationship between variables
#'
#' D-separation is a key concept in causal structural models. Variables are
#' d-separated if there are no open paths between them. The `node_d*()`
#' functions label variables as d-connected or d-separated. The
#' `ggdag_d*()` functions plot the results. The `*_dconnected()`,
#' `*_dseparated()`, and `*_drelationship()` functions essentially
#' produce the same output and are just different ways of thinking about the
#' relationship. See [dagitty::dseparated()] for details.
#'
#' @param .tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @param edge_type a character vector, the edge geom to use. One of:
#'   "link_arc", which accounts for directed and bidirected edges, "link",
#'   "arc", or "diagonal"
#' @param from a character vector, the starting variable (must by in DAG). If
#'   `NULL`, checks DAG for exposure variable.
#' @param to a character vector, the ending variable (must by in DAG). If
#'   `NULL`, checks DAG for outcome variable.
#' @param controlling_for a character vector, variables in the DAG to control
#'   for.
#' @param node_size size of DAG node
#' @param text_size size of DAG text
#' @param label_size size of label text
#' @param text_col color of DAG text
#' @param label_col color of label text
#' @param node logical. Should nodes be included in the DAG?
#' @param stylized logical. Should DAG nodes be stylized? If so, use
#'   `geom_dag_nodes` and if not use `geom_dag_point`
#' @param text logical. Should text be included in the DAG?
#' @param use_labels a string. Variable to use for `geom_dag_repel_label()`.
#'   Default is `NULL`.
#' @param collider_lines logical. Should the plot show paths activated by
#'   adjusting for a collider?
#' @param as_factor logical. Should the `d_relationship` variable be a
#'   factor?
#'
#' @return a `tidy_dagitty` with a `d_relationship` column for
#'   variable D relationship or a `ggplot`
#' @export
#'
#' @examples
#' dag <- dagify(m ~ x + y)
#' dag %>% ggdag_drelationship("x", "y")
#' dag %>% ggdag_drelationship("x", "y", controlling_for = "m")
#'
#' dag %>%
#'   node_dseparated("x", "y") %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
#'   geom_dag_edges() +
#'   geom_dag_collider_edges() +
#'   geom_dag_node() +
#'   geom_dag_text(col = "white") +
#'   theme_dag() + scale_adjusted()
#'
#' dag %>%
#'   node_dconnected("x", "y", controlling_for = "m") %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
#'   geom_dag_edges() +
#'   geom_dag_collider_edges() +
#'   geom_dag_node() +
#'   geom_dag_text(col = "white") +
#'   theme_dag() +
#'   scale_adjusted()
#'
#' dagify(m ~ x + y, m_jr ~ m) %>%
#'   tidy_dagitty(layout = "nicely") %>%
#'   node_dconnected("x", "y", controlling_for = "m_jr") %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
#'   geom_dag_edges() +
#'   geom_dag_collider_edges() +
#'   geom_dag_node() +
#'   geom_dag_text(col = "white") +
#'   theme_dag() +
#'   scale_adjusted()
#' @rdname d_relationship
#' @name Assess d-separation between variables
node_dconnected <- function(.tdy_dag, from = NULL, to = NULL, controlling_for = NULL, as_factor = TRUE, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  if (is.null(from)) from <- dagitty::exposures(.tdy_dag$dag)
  if (is.null(to)) to <- dagitty::outcomes(.tdy_dag$dag)
  if (is_empty_or_null(from) || is_empty_or_null(to)) stop("`from` and `to` must be set!")

  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag$data$collider_line <- FALSE
    .tdy_dag$data$adjusted <- "unadjusted"
    controlling_for <- c()
  }

  .dconnected <- dagitty::dconnected(.tdy_dag$dag, from, to, controlling_for)

  .from <- from
  .to <- to

  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                                 d_relationship = ifelse(name %in% c(.from, .to) & .dconnected, "d-connected",
                                                         ifelse(name %in% c(.from, .to) & !.dconnected, "d-separated",
                                                                NA)))
  if (as_factor) .tdy_dag$data$d_relationship <- factor(.tdy_dag$data$d_relationship, levels = c("d-connected", "d-separated"), exclude = NA)

  .tdy_dag
}

#' @rdname d_relationship
#' @export
node_dseparated <- function(.tdy_dag, from = NULL, to = NULL, controlling_for = NULL, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  if (is.null(from)) from <- dagitty::exposures(.tdy_dag$dag)
  if (is.null(to)) to <- dagitty::outcomes(.tdy_dag$dag)
  if (is_empty_or_null(from) || is_empty_or_null(to)) stop("`from` and `to` must be set!")

  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag$data$collider_line <- FALSE
    .tdy_dag$data$adjusted <- "unadjusted"
    controlling_for <- c()
  }

  .dseparated <- dagitty::dseparated(.tdy_dag$dag, from, to, controlling_for)

  .from <- from
  .to <- to

  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                                 d_relationship = ifelse(name %in% c(.from, .to) & !.dseparated, "d-connected",
                                                         ifelse(name %in% c(.from, .to) & .dseparated, "d-separated",
                                                                NA)))
  if (as_factor) .tdy_dag$data$d_relationship <- factor(.tdy_dag$data$d_relationship, levels = c("d-connected", "d-separated"), exclude = NA)
  .tdy_dag
}

#' @rdname d_relationship
#' @export
node_drelationship <- function(.tdy_dag, from = NULL, to = NULL, controlling_for = NULL, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  if (is.null(from)) from <- dagitty::exposures(.tdy_dag$dag)
  if (is.null(to)) to <- dagitty::outcomes(.tdy_dag$dag)
  if (is_empty_or_null(from) || is_empty_or_null(to)) stop("`from` and `to` must be set!")

  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag$data$collider_line <- FALSE
    .tdy_dag$data$adjusted <- "unadjusted"
    controlling_for <- c()
  }


  .dseparated <- dagitty::dseparated(.tdy_dag$dag, from, to, controlling_for)
  .from <- from
  .to <- to
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                                 d_relationship = ifelse(name %in% c(.from, .to) & !.dseparated, "d-connected",
                                                         ifelse(name %in% c(.from, .to) & .dseparated, "d-separated",
                                                                NA)))
  if (as_factor) .tdy_dag$data$d_relationship <- factor(.tdy_dag$data$d_relationship, levels = c("d-connected", "d-separated"), exclude = NA)
  .tdy_dag
}

#' @rdname d_relationship
#' @export
ggdag_drelationship <- function(.tdy_dag, from = NULL, to = NULL, controlling_for = NULL, ..., edge_type = "link_arc",
                                node_size = 16, text_size = 3.88, label_size = text_size,
                                text_col = "white", label_col = text_col,
                                node = TRUE, stylized = FALSE, text = TRUE, use_labels = NULL, collider_lines = TRUE) {
  edge_function <- edge_type_switch(edge_type)

  p <- if_not_tidy_daggity(.tdy_dag) %>%
    node_drelationship(from = from, to = to, controlling_for = controlling_for, ...)  %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
      edge_function(start_cap = ggraph::circle(10, "mm"),
                       end_cap = ggraph::circle(10, "mm")) +
      scale_adjusted() +
      breaks(c("d-connected", "d-separated"), name = "d-relationship") +
      expand_plot(expand_y = expansion(c(0.2, 0.2)))

  if (collider_lines) p <- p + geom_dag_collider_edges()
  if (node) {
    if (stylized) {
        p <- p + geom_dag_node(size = node_size)
      } else {
        p <- p + geom_dag_point(size = node_size)
      }
    }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) p <- p +
      geom_dag_label_repel(ggplot2::aes_string(label = use_labels,
                                               fill = "d_relationship"), size = text_size,
                           col = label_col, show.legend = FALSE)
  p
}

#' @rdname d_relationship
#' @export
ggdag_dseparated <- function(.tdy_dag, from = NULL, to = NULL, controlling_for = NULL, ..., edge_type = "link_arc",
                             node_size = 16, text_size = 3.88, label_size = text_size,
                             text_col = "white", label_col = text_col,
                             node = TRUE, stylized = FALSE, text = TRUE, use_labels = NULL, collider_lines = TRUE) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to,
                      controlling_for = controlling_for, ..., edge_type = edge_type,
                      node_size = node_size, text_size = text_size, label_size = label_size,
                      text_col = text_col, label_col = label_col, node = node, stylized = stylized, text = text,
                      use_labels = use_labels, collider_lines = collider_lines)
}

#' @rdname d_relationship
#' @export
ggdag_dconnected <- function(.tdy_dag, from = NULL, to = NULL, controlling_for = NULL, ..., edge_type = "link_arc",
                             node_size = 16, text_size = 3.88, label_size = text_size,
                             text_col = "white", label_col = text_col,
                             node = TRUE, stylized = FALSE, text = TRUE, use_labels = NULL, collider_lines = TRUE) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to,
                      controlling_for = controlling_for, ..., edge_type = edge_type,
                      node_size = node_size, text_size = text_size, label_size = label_size,
                      text_col = text_col, label_col = label_col, node = node, stylized = stylized, text = text,
                      use_labels = use_labels, collider_lines = collider_lines)
}
