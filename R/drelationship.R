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
#' @inheritParams dag_params
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @inheritParams path_params
#' @inheritParams geom_dag
#' @inheritParams path_params
#' @inheritParams dag_params
#'
#' @return a `tidy_dagitty` with a `d_relationship` column for
#'   variable D relationship or a `ggplot`
#' @export
#'
#' @examples
#' library(ggplot2)
#' dag <- dagify(m ~ x + y)
#' dag |> ggdag_drelationship("x", "y")
#' dag |> ggdag_drelationship("x", "y", controlling_for = "m")
#'
#' dag |>
#'   node_dseparated("x", "y") |>
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend,
#'              col = d_relationship)) +
#'   geom_dag_edges() +
#'   geom_dag_node() +
#'   geom_dag_text(col = "white") +
#'   theme_dag()
#'
#' dag |>
#'   node_dconnected("x", "y", controlling_for = "m") |>
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted,
#'              col = d_relationship)) +
#'   geom_dag_edges() +
#'   geom_dag_collider_edges() +
#'   geom_dag_node() +
#'   geom_dag_text(col = "white") +
#'   theme_dag() +
#'   scale_adjusted(include_color = FALSE)
#'
#' dagify(m ~ x + y, m_jr ~ m) |>
#'   tidy_dagitty(layout = "nicely") |>
#'   node_dconnected("x", "y", controlling_for = "m_jr") |>
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted,
#'              col = d_relationship)) +
#'   geom_dag_edges() +
#'   geom_dag_collider_edges() +
#'   geom_dag_node() +
#'   geom_dag_text(col = "white") +
#'   theme_dag() +
#'   scale_adjusted(include_color = FALSE)
#' @rdname d_relationship
#' @name Assess d-separation between variables
node_dconnected <- function(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  as_factor = TRUE,
  ...
) {
  label_d_relationship(
    if_not_tidy_daggity(.tdy_dag, ...),
    from = from,
    to = to,
    controlling_for = controlling_for,
    as_factor = as_factor
  )
}

#' @rdname d_relationship
#' @export
node_dseparated <- function(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  as_factor = TRUE,
  ...
) {
  label_d_relationship(
    if_not_tidy_daggity(.tdy_dag, ...),
    from = from,
    to = to,
    controlling_for = controlling_for,
    as_factor = as_factor
  )
}

#' @rdname d_relationship
#' @export
node_drelationship <- function(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  as_factor = TRUE,
  ...
) {
  label_d_relationship(
    if_not_tidy_daggity(.tdy_dag, ...),
    from = from,
    to = to,
    controlling_for = controlling_for,
    as_factor = as_factor
  )
}

#' Resolve and validate the endpoints and conditioning set of a d-relationship
#'
#' `from` and `to` fall back to the DAG's exposures and outcomes. The
#' conditioning set is flattened before validation so that the documented
#' `list(c(...))` format reaches both the validator and dagitty as node names.
#'
#' @param .tdy_dag A `tidy_dagitty` object.
#' @param from,to Character vectors of node names, or `NULL`.
#' @param controlling_for A character vector, a list of character vectors, or
#'   `NULL`.
#' @param call The environment to report errors from.
#' @return A list with elements `from`, `to`, and `controlling_for`.
#' @noRd
prepare_d_relationship <- function(
  .tdy_dag,
  from,
  to,
  controlling_for,
  call = rlang::caller_env()
) {
  if (is.null(from)) {
    from <- dagitty::exposures(pull_dag(.tdy_dag))
  }
  if (is.null(to)) {
    to <- dagitty::outcomes(pull_dag(.tdy_dag))
  }
  if (is_empty_or_null(from) || is_empty_or_null(to)) {
    abort(
      c(
        "Both {.arg from} and {.arg to} must be set.",
        "i" = "Set {.arg from} to specify the starting variable.",
        "i" = "Set {.arg to} to specify the ending variable."
      ),
      error_class = "ggdag_missing_error",
      call = call
    )
  }

  validate_nodes_exist(
    .tdy_dag,
    c(from, to),
    arg = c("from", "to"),
    call = call
  )

  controlling_for <- flatten_node_names(controlling_for)
  if (!is.null(controlling_for)) {
    validate_nodes_exist(
      .tdy_dag,
      controlling_for,
      arg = "controlling_for",
      call = call
    )
  }

  list(from = from, to = to, controlling_for = controlling_for)
}

#' Label each endpoint with its own d-relationship
#'
#' `dagitty::dconnected()` answers a set-level question: whether any node of
#' `from` is d-connected to any node of `to`. Each endpoint is therefore checked
#' against the opposite set on its own, so that a node d-separated from
#' everything it is compared with is labeled as such even when its neighbors in
#' the same set are not.
#'
#' @inheritParams prepare_d_relationship
#' @param as_factor Logical. Should the column be a factor?
#' @return A `tidy_dagitty` with a `d_relationship` column.
#' @noRd
label_d_relationship <- function(
  .tdy_dag,
  from,
  to,
  controlling_for,
  as_factor,
  call = rlang::caller_env()
) {
  args <- prepare_d_relationship(
    .tdy_dag,
    from = from,
    to = to,
    controlling_for = controlling_for,
    call = call
  )

  if (!is.null(args$controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, args$controlling_for)
  }

  endpoints <- unique(c(args$from, args$to))
  connected <- purrr::map_lgl(endpoints, function(.node) {
    others <- if (.node %in% args$from) args$to else args$from
    dagitty::dconnected(
      pull_dag(.tdy_dag),
      .node,
      others,
      args$controlling_for
    )
  })
  labels <- ifelse(connected, "d-connected", "d-separated")
  names(labels) <- endpoints

  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    d_relationship = unname(labels[as.character(.data$name)])
  )

  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      d_relationship = factor(
        .data$d_relationship,
        levels = c("d-connected", "d-separated"),
        exclude = NA
      )
    )
  }

  .tdy_dag
}

#' @rdname d_relationship
#' @export
ggdag_drelationship <- function(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  ...,
  edge_type = ggdag_option("edge_type", "link_arc"),
  size = 1,
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option_proportional("edge_cap", 8, 10),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  unified_legend = TRUE,
  key_glyph = draw_key_dag_point,
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
) {
  df <- node_drelationship(
    .tdy_dag,
    from = from,
    to = to,
    controlling_for = controlling_for,
    ...
  )

  has_adjusted <- "adjusted" %in% names(pull_dag_data(df))
  if (has_adjusted) {
    mapping <- aes_dag(shape = .data$adjusted, color = .data$d_relationship)
  } else {
    mapping <- aes_dag(color = .data$d_relationship)
  }

  p <- ggplot2::ggplot(df, mapping)

  if (has_adjusted && collider_lines) {
    p <- p + geom_dag_collider_edges()
  }

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
      key_glyph = key_glyph,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    ) +
    scale_adjusted(include_color = FALSE) +
    breaks(c("d-connected", "d-separated"), name = "d-relationship") +
    expand_plot(expand_y = expansion(c(0.2, 0.2)))

  p
}

#' @rdname d_relationship
#' @export
ggdag_dseparated <- function(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  ...,
  edge_type = ggdag_option("edge_type", "link_arc"),
  size = 1,
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option_proportional("edge_cap", 8, 10),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  unified_legend = TRUE,
  key_glyph = draw_key_dag_point,
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
) {
  ggdag_drelationship(
    .tdy_dag = .tdy_dag,
    from = from,
    to = to,
    controlling_for = controlling_for,
    ...,
    edge_type = edge_type,
    size = size,
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
    label = !!rlang::enquo(label),
    text = !!rlang::enquo(text),
    node = node,
    stylized = stylized,
    unified_legend = unified_legend,
    key_glyph = key_glyph,
    collider_lines = collider_lines
  )
}

#' @rdname d_relationship
#' @export
ggdag_dconnected <- function(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  ...,
  edge_type = ggdag_option("edge_type", "link_arc"),
  size = 1,
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option_proportional("edge_cap", 8, 10),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  unified_legend = TRUE,
  key_glyph = draw_key_dag_point,
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
) {
  ggdag_drelationship(
    .tdy_dag = .tdy_dag,
    from = from,
    to = to,
    controlling_for = controlling_for,
    ...,
    edge_type = edge_type,
    size = size,
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
    label = !!rlang::enquo(label),
    text = !!rlang::enquo(text),
    node = node,
    stylized = stylized,
    unified_legend = unified_legend,
    key_glyph = key_glyph,
    collider_lines = collider_lines
  )
}
