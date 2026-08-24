#' Generating Equivalent Models
#'
#' Analyze the Markov equivalence class of an input DAG: the DAGs that encode
#' the same conditional independencies as the input graph. See
#' [dagitty::equivalentDAGs()] and [dagitty::equivalenceClass()] for details.
#' `node_equivalent_dags()` returns a set of DAGs, while
#' `node_equivalent_class()` tags reversable edges.
#' `ggdag_equivalent_dags()` plots all equivalent DAGs, while
#' `ggdag_equivalent_class()` plots all reversable edges as undirected.
#'
#' @details
#' `node_equivalent_dags()` restores columns that the input `tidy_dagitty`
#' carries beyond the standard ones, such as `label` or `status`, by joining
#' them back on node name. Only node-level columns survive: a column whose
#' value varies across the edges of a node cannot be matched to the edges of
#' the equivalent DAGs, so the value of its first edge is used for every row of
#' that node.
#'
#' @param .dag input graph, an object of class `tidy_dagitty` or `dagitty`
#' @param n maximal number of returned graphs.
#' @inheritParams dag_params
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
node_equivalent_dags <- function(
  .dag,
  n = 100,
  layout = ggdag_option("layout", "nicely"),
  ...
) {
  .dag <- if_not_tidy_daggity(.dag, layout = layout, ...)
  # drop the results of an earlier application so the join does not suffix
  .dag <- dplyr::select(.dag, -dplyr::any_of("dag"))
  extra_columns <- has_extra_columns(.dag)

  layout_coords <- .dag |>
    pull_dag_data() |>
    dplyr::select("name", "x", "y") |>
    dplyr::distinct() |>
    coords2list()

  updated_dag <- pull_dag(.dag)
  dagitty::coordinates(updated_dag) <- layout_coords
  update_dag(.dag) <- updated_dag

  if (extra_columns) {
    # extra columns come from edge-level rows, so keep one row per node to
    # join by name without multiplying the rows of the equivalent DAGs
    extra_column_df <- .dag |>
      select_extra_columns() |>
      dplyr::distinct(.data$name, .keep_all = TRUE)
  }

  update_dag_data(.dag) <- dagitty::equivalentDAGs(pull_dag(.dag), n = n) |>
    purrr::map_df(map_equivalence, .id = "dag") |>
    dplyr::as_tibble() |>
    dplyr::mutate(dag = as.integer(.data$dag))

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
    dplyr::select("name", get_extra_column_names(.x))
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
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option("edge_cap", 8),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  unified_legend = TRUE,
  key_glyph = NULL,
  edge_engine = ggdag_option("edge_engine", "ggraph"),
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  if (missing(edge_type)) {
    edge_type <- ggdag_option("edge_type", "link_arc")
  }
  edge_engine <- match.arg(edge_engine, c("ggraph", "ggarrow"))

  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) |>
    node_equivalent_dags(...)

  p <- ggplot2::ggplot(.tdy_dag, aes_dag())

  p <- p +
    geom_dag(
      size = size,
      edge_type = edge_type,
      edge_engine = edge_engine,
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

#' @rdname equivalent
#' @export
node_equivalent_class <- function(
  .dag,
  layout = ggdag_option("layout", "nicely")
) {
  .dag <- if_not_tidy_daggity(.dag, layout = layout)
  # drop the results of an earlier application so the join does not suffix
  .dag <- dplyr::select(.dag, -dplyr::any_of("reversable"))

  class_edges <- dagitty::equivalenceClass(pull_dag(.dag)) |>
    dagitty::edges()

  # dagitty returns a zero-column data frame for a DAG with no edges
  if (nrow(class_edges) == 0 || ncol(class_edges) == 0) {
    return(dplyr::mutate(.dag, reversable = FALSE))
  }

  # match on the endpoints themselves rather than on a pasted key, which
  # collides for node names that contain the separator
  ec_data <- class_edges |>
    dplyr::filter(.data$e == "--") |>
    dplyr::transmute(
      edge_start = pmin(as.character(.data$v), as.character(.data$w)),
      edge_end = pmax(as.character(.data$v), as.character(.data$w)),
      reversable = TRUE
    )

  .dag |>
    dplyr::mutate(
      edge_start = pmin(.data$name, .data$to),
      edge_end = pmax(.data$name, .data$to)
    ) |>
    dplyr::left_join(ec_data, by = c("edge_start", "edge_end")) |>
    dplyr::mutate(
      # both endpoints of a bidirected edge match an undirected edge of the
      # equivalence class, but only the directed edge between them is the one
      # the class leaves free to reverse
      reversable = !is.na(.data$reversable) &
        !is.na(.data$direction) &
        .data$direction != "<->"
    ) |>
    dplyr::select(-"edge_start", -"edge_end")
}

#' @rdname equivalent
#' @inheritParams expand_plot
#' @export
ggdag_equivalent_class <- function(
  .tdy_dag,
  ...,
  size = 1,
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option("edge_cap", 8),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  unified_legend = TRUE,
  key_glyph = NULL,
  edge_engine = ggdag_option("edge_engine", "ggraph"),
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  edge_engine <- match.arg(edge_engine, c("ggraph", "ggarrow"))

  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) |>
    node_equivalent_class(...)

  reversable_lines <- dplyr::filter(pull_dag_data(.tdy_dag), .data$reversable)
  non_reversable_lines <- dplyr::filter(
    pull_dag_data(.tdy_dag),
    !.data$reversable
  )

  if (identical(edge_engine, "ggarrow")) {
    p <- .tdy_dag |>
      ggplot2::ggplot(aes_dag())
  } else {
    p <- .tdy_dag |>
      ggplot2::ggplot(aes_dag(edge_alpha = .data$reversable))
  }

  if (use_edges) {
    if (identical(edge_engine, "ggarrow")) {
      rlang::check_installed(
        "ggarrow",
        reason = "to use edge_engine = \"ggarrow\"."
      )
      resect <- edge_cap * size
      arrow_head <- ggdag_option("arrow_head", NULL) %||%
        ggarrow::arrow_head_wings()
      arrow_fins <- ggdag_option("arrow_fins", NULL)

      edge_mapping <- with_edge_curvature(
        ggplot2::aes(alpha = .data$reversable),
        p$data
      )

      p <- p +
        quick_plot_arrow_edges(
          mapping = edge_mapping,
          data_directed = function(x) {
            dplyr::filter(x, !.data$reversable, .data$direction == "->")
          },
          data_bidirected = function(x) {
            dplyr::filter(x, !.data$reversable, .data$direction == "<->")
          },
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          resect = resect,
          linewidth = edge_width * size,
          length = arrow_length_unit(arrow_length * size),
          show.legend = TRUE
        ) +
        geom_dag_arrow_arc(
          mapping = edge_mapping,
          data = reversable_lines,
          curvature = 0,
          arrow_head = NULL,
          arrow_fins = NULL,
          resect = resect,
          linewidth = edge_width * size,
          length = arrow_length_unit(arrow_length * size),
          show.legend = TRUE
        ) +
        breaks() +
        ggplot2::scale_alpha_manual(
          name = "Reversable",
          drop = FALSE,
          values = c("FALSE" = 0.30, "TRUE" = 1),
          limits = c("FALSE", "TRUE")
        )
    } else {
      p <- p +
        geom_dag_edges(
          data_directed = dplyr::filter(
            non_reversable_lines,
            .data$direction != "<->"
          ),
          data_bidirected = dplyr::filter(
            non_reversable_lines,
            .data$direction == "<->"
          )
        ) +
        geom_dag_edges_link(data = reversable_lines, arrow = NULL) +
        breaks() +
        ggraph::scale_edge_alpha_manual(
          name = "Reversable",
          drop = FALSE,
          values = c(0.30, 1)
        )
    }
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
      edge_engine = edge_engine,
      use_edges = FALSE,
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
    )

  p
}
