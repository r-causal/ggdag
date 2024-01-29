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
#' @param from a character vector, the starting variable (must by in DAG). If
#'   `NULL`, checks DAG for exposure variable.
#' @param to a character vector, the ending variable (must by in DAG). If
#'   `NULL`, checks DAG for outcome variable.
#' @param controlling_for a character vector, variables in the DAG to control
#'   for.
#' @inheritParams geom_dag
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
#' library(ggplot2)
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
#'   theme_dag() +
#'   scale_adjusted()
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

  if (is.null(from)) from <- dagitty::exposures(pull_dag(.tdy_dag))
  if (is.null(to)) to <- dagitty::outcomes(pull_dag(.tdy_dag))
  if (is_empty_or_null(from) || is_empty_or_null(to)) stop("`from` and `to` must be set!")

  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag <- .tdy_dag %>%
      dplyr::mutate(collider_line = FALSE, adjusted = "unadjusted")
    controlling_for <- c()
  }

  .dconnected <- dagitty::dconnected(pull_dag(.tdy_dag), from, to, controlling_for)

  .from <- from
  .to <- to

  .tdy_dag <- dplyr::mutate(.tdy_dag,
                            d_relationship = ifelse(name %in% c(.from, .to) & .dconnected, "d-connected",
                                                    ifelse(name %in% c(.from, .to) & !.dconnected, "d-separated",
                                                           NA
                                                    )
                            )
  )
  if (as_factor) {
    .tdy_dag <- mutate(
      .tdy_dag,
      d_relationship = factor(
        d_relationship,
        levels = c("d-connected", "d-separated"),
        exclude = NA
      )
    )
  }

  .tdy_dag
}

#' @rdname d_relationship
#' @export
node_dseparated <- function(.tdy_dag, from = NULL, to = NULL, controlling_for = NULL, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  if (is.null(from)) from <- dagitty::exposures(pull_dag(.tdy_dag))
  if (is.null(to)) to <- dagitty::outcomes(pull_dag(.tdy_dag))
  if (is_empty_or_null(from) || is_empty_or_null(to)) stop("`from` and `to` must be set!")

  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag <- .tdy_dag %>%
      dplyr::mutate(collider_line = FALSE, adjusted = "unadjusted")
    controlling_for <- c()
  }

  .dseparated <- dagitty::dseparated(pull_dag(.tdy_dag), from, to, controlling_for)

  .from <- from
  .to <- to

  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    d_relationship = ifelse(name %in% c(.from, .to) & !.dseparated, "d-connected",
                            ifelse(name %in% c(.from, .to) & .dseparated, "d-separated",
                                   NA
                            )
    )
  )
  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      d_relationship = factor(
        d_relationship,
        levels = c("d-connected", "d-separated"),
        exclude = NA
      )
    )
  }
  .tdy_dag
}

#' @rdname d_relationship
#' @export
node_drelationship <- function(.tdy_dag, from = NULL, to = NULL, controlling_for = NULL, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  if (is.null(from)) from <- dagitty::exposures(pull_dag(.tdy_dag))
  if (is.null(to)) to <- dagitty::outcomes(pull_dag(.tdy_dag))
  if (is_empty_or_null(from) || is_empty_or_null(to)) stop("`from` and `to` must be set!")

  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      collider_line = FALSE,
      adjusted = "unadjusted"
    )
    controlling_for <- c()
  }


  .dseparated <- dagitty::dseparated(pull_dag(.tdy_dag), from, to, controlling_for)
  .from <- from
  .to <- to
  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    d_relationship = dplyr::case_when(
      name %in% c(.from, .to) & !.dseparated ~ "d-connected",
      name %in% c(.from, .to) & .dseparated ~ "d-separated",
      TRUE ~ NA_character_
    )
  )
  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      d_relationship = factor(
        d_relationship,
        levels = c("d-connected", "d-separated"),
        exclude = NA
      )
    )
  }
  .tdy_dag
}

#' @rdname d_relationship
#' @export
ggdag_drelationship <- function(.tdy_dag, from = NULL, to = NULL, controlling_for = NULL, ...,
                                edge_type = "link_arc", size = 1,
                                node_size = 16, text_size = 3.88,
                                label_size = text_size,
                                text_col = "white", label_col = "black",
                                edge_width = 0.6, edge_cap = 10, arrow_length = 5,
                                use_edges = TRUE,
                                use_nodes = TRUE, use_stylized = FALSE, use_text = TRUE,
                                use_labels = FALSE, label = NULL, text = NULL, node = deprecated(),
                                stylized = deprecated(), collider_lines = TRUE) {

  p <- if_not_tidy_daggity(.tdy_dag) %>%
    node_drelationship(from = from, to = to, controlling_for = controlling_for, ...) %>%
    ggplot2::ggplot(aes_dag(shape = adjusted, col = d_relationship))

    if (collider_lines) p <- p + geom_dag_collider_edges()

    p <- p + geom_dag(
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
    ) +
    scale_adjusted() +
    breaks(c("d-connected", "d-separated"), name = "d-relationship") +
    expand_plot(expand_y = expansion(c(0.2, 0.2)))

  p
}

#' @rdname d_relationship
#' @export
ggdag_dseparated <- function(.tdy_dag, from = NULL, to = NULL, controlling_for = NULL, ...,
                             edge_type = "link_arc", size = 1,
                             node_size = 16, text_size = 3.88,
                             label_size = text_size,
                             text_col = "white", label_col = "black",
                             edge_width = 0.6, edge_cap = 10, arrow_length = 5,
                             use_nodes = TRUE, use_stylized = FALSE, use_text = TRUE,
                             use_labels = FALSE, label = NULL, text = NULL, node = deprecated(),
                             stylized = deprecated(), collider_lines = TRUE) {
  ggdag_drelationship(
    .tdy_dag = .tdy_dag, from = from, to = to, controlling_for = controlling_for, ...,
    edge_type = edge_type, size = size,
    node_size = node_size, text_size = text_size,
    label_size = label_size,
    text_col = text_col, label_col = label_col,
    edge_width = edge_width, edge_cap = edge_cap, arrow_length = arrow_length,
    use_nodes = use_nodes, use_stylized = use_stylized, use_text = use_text,
    use_labels = use_labels, label = !!rlang::enquo(label), text = !!rlang::enquo(text), node = node,
    stylized = stylized, collider_lines = collider_lines
  )
}

#' @rdname d_relationship
#' @export
ggdag_dconnected <- function(.tdy_dag, from = NULL, to = NULL, controlling_for = NULL, ...,
                             edge_type = "link_arc", size = 1,
                             node_size = 16, text_size = 3.88,
                             label_size = text_size,
                             text_col = "white", label_col = "black",
                             edge_width = 0.6, edge_cap = 10, arrow_length = 5,
                             use_nodes = TRUE, use_stylized = FALSE, use_text = TRUE,
                             use_labels = FALSE, label = NULL, text = NULL, node = deprecated(),
                             stylized = deprecated(), collider_lines = TRUE) {
  ggdag_drelationship(
    .tdy_dag = .tdy_dag, from = from, to = to, controlling_for = controlling_for, ...,
    edge_type = edge_type, size = size,
    node_size = node_size, text_size = text_size,
    label_size = label_size,
    text_col = text_col, label_col = label_col,
    edge_width = edge_width, edge_cap = edge_cap, arrow_length = arrow_length,
    use_nodes = use_nodes, use_stylized = use_stylized, use_text = use_text,
    use_labels = use_labels, label = !!rlang::enquo(label), text = !!rlang::enquo(text), node = node,
    stylized = stylized, collider_lines = collider_lines
  )
}
