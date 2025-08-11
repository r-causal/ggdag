#' Generating Equivalent Models
#'
#' Returns a set of complete partially directed acyclic graphs (CPDAGs) given an
#' input DAG. CPDAGs are Markov equivalent to the input graph. See
#' [dagitty::equivalentDAGs()] for details.
#' `node_equivalent_dags()` returns a set of DAGs, while
#' `node_equivalent_class()` tags reversable edges.
#' `ggdag_equivalent_dags()` plots all equivalent DAGs, while
#' `ggdag_equivalent_class()` plots all reversable edges as undirected.
#'
#' @param .dag input graph, an object of class `tidy_dagitty` or `dagitty`
#' @param n maximal number of returned graphs.
#' @param .tdy_dag an object of class `tidy_dagitty` or `dagitty`
#' @inheritParams geom_dag
#' @inheritParams tidy_dagitty
#' @inheritParams scale_adjusted
#'
#' @return a `tidy_dagitty` with at least one DAG, including a `dag`
#'   column to identify graph set for equivalent DAGs or a `reversable`
#'   column for equivalent classes, or a `ggplot`
#' @export
#'
#' @examples
#' g_ex <- dagify(y ~ x + z, x ~ z)
#'
#' g_ex |> node_equivalent_class()
#'
#' g_ex |> ggdag_equivalent_dags()
#'
#' @rdname equivalent
#' @name Equivalent DAGs and Classes
#' @export
node_equivalent_dags <- function(.dag, n = 100, layout = "auto", ...) {
  .dag <- if_not_tidy_daggity(.dag, layout = layout, ...)
  extra_columns <- has_extra_columns(.dag)

  layout_coords <- .dag |>
    pull_dag_data() |>
    dplyr::select(name, x, y) |>
    dplyr::distinct() |>
    coords2list()

  updated_dag <- pull_dag(.dag)
  dagitty::coordinates(updated_dag) <- layout_coords
  update_dag(.dag) <- updated_dag

  if (extra_columns) {
    extra_column_df <- select_extra_columns(.dag)
  }

  update_dag_data(.dag) <- dagitty::equivalentDAGs(pull_dag(.dag), n = n) |>
    purrr::map_df(map_equivalence, .id = "dag") |>
    dplyr::as_tibble()

  if (extra_columns) {
    .dag <- dplyr::left_join(.dag, extra_column_df, by = "name")
  }

  .dag
}

has_extra_columns <- function(.x) {
  !purrr::is_empty(get_extra_column_names(.x))
}

get_extra_column_names <- function(.x) {
  standard_names <- c(
    "name",
    "x",
    "y",
    "direction",
    "to",
    "xend",
    "yend",
    "circular"
  )
  dag_columns <- names(pull_dag_data(.x))
  setdiff(dag_columns, standard_names)
}

select_extra_columns <- function(.x) {
  .x |>
    pull_dag_data() |>
    dplyr::select(name, get_extra_column_names(.x))
}

map_equivalence <- function(.x) {
  as.data.frame(tidy_dagitty(.dagitty = .x))
}

#' @rdname equivalent
#' @export
ggdag_equivalent_dags <- function(
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
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) |>
    node_equivalent_dags(...)

  p <- ggplot2::ggplot(.tdy_dag, aes_dag())

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

  if (dplyr::n_distinct(pull_dag_data(.tdy_dag)$dag) > 1) {
    p <- p +
      ggplot2::facet_wrap(~dag) +
      expand_plot(
        expand_x = expansion(c(0.25, 0.25)),
        expand_y = expansion(c(0.25, 0.25))
      )
  }

  p
}

hash <- function(x, y) {
  purrr::pmap_chr(list(x, y), \(.x, .y) paste0(sort(c(.x, .y)), collapse = "_"))
}

#' @rdname equivalent
#' @export
node_equivalent_class <- function(.dag, layout = "auto") {
  .dag <- if_not_tidy_daggity(.dag, layout = layout)
  ec_data <- dagitty::equivalenceClass(pull_dag(.dag)) |>
    dagitty::edges() |>
    dplyr::filter(e == "--") |>
    dplyr::select(name = v, reversable = e, to = w) |>
    dplyr::mutate_at(c("name", "to"), as.character) |>
    dplyr::mutate(hash = hash(name, to)) |>
    dplyr::select(hash, reversable)

  .dag <- .dag |>
    dplyr::mutate(hash = hash(name, to)) |>
    dplyr::left_join(ec_data, by = "hash") |>
    dplyr::mutate(reversable = !is.na(reversable)) |>
    dplyr::select(-hash)

  .dag
}

#' @rdname equivalent
#' @inheritParams expand_plot
#' @export
ggdag_equivalent_class <- function(
  .tdy_dag,
  ...,
  size = 1,
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
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) |>
    node_equivalent_class(...)

  reversable_lines <- dplyr::filter(pull_dag_data(.tdy_dag), reversable)
  non_reversable_lines <- dplyr::filter(pull_dag_data(.tdy_dag), !reversable)
  p <- .tdy_dag |>
    ggplot2::ggplot(aes_dag(edge_alpha = reversable))

  if (use_edges) {
    p <- p +
      geom_dag_edges(
        data_directed = dplyr::filter(non_reversable_lines, direction != "<->"),
        data_bidirected = dplyr::filter(non_reversable_lines, direction == "<->")
      ) +
      geom_dag_edges_link(data = reversable_lines, arrow = NULL) +
      breaks(breaks) +
      ggraph::scale_edge_alpha_manual(
        name = "Reversable",
        drop = FALSE,
        values = c(.30, 1)
      )
  }

  p <- p +
    geom_dag(
      size = size,
      node_size = node_size,
      text_size = text_size,
      label_size = label_size,
      text_col = text_col,
      label_col = label_col,
      edge_width = edge_width,
      edge_cap = edge_cap,
      arrow_length = arrow_length,
      use_edges = FALSE,
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
