#' Saturate or prune an existing DAG
#'
#' `dag_saturate()` takes a tidy DAG object and, optionally using existing
#' coordinates, saturates the DAG based on time ordering of the nodes. To create
#' a saturated DAG from scratch, see [`as_tidy_dagitty.list()`]. `dag_prune()`
#' takes an existing DAG and removes edges. This is most useful when used
#' together with saturated DAG.
#'
#' @details
#' Bidirected edges carry no time-ordering information, so `dag_saturate()`
#' assigns time order from the directed edges alone and then passes the input's
#' bidirected edges through to the saturated DAG unchanged. A saturated model
#' therefore never implies an independence that the input denies.
#'
#' `dag_prune()` errors if `edges` is empty, and if it names an edge the DAG
#' does not contain, including an edge written in the reverse direction. A node
#' whose every edge is pruned is kept as an isolated node.
#'
#' A pair of nodes can hold a directed edge and a bidirected edge at the same
#' time, and endpoints alone name both of them. `dag_prune()` errors on such a
#' pair rather than pruning both; name the direction as well, with the data
#' frame form of `edges`, to prune one of them.
#'
#' @inheritParams dag_params
#' @param use_existing_coords Logical, indicating whether to use existing node
#'   coordinates.
#' @param edges The edges to remove, in either of two forms. A named character
#'   vector where the name is the starting node and the value is the end node,
#'   e.g. `c("x" = "y")` removes the edge going from `x` to `y`. Or a data frame
#'   with a `name` and a `to` column, which says the same thing, and an optional
#'   `direction` column of `"->"`, `"<->"`, or `"--"`, which names one of the
#'   edges a pair of nodes holds.
#' @inheritParams tidy_dagitty
#'
#' @return A `tidy_dagitty` object
#'
#' @export
#' @examples
#' # Example usage:
#' dag <- dagify(y ~ x, x ~ z)
#' saturated_dag <- dag_saturate(dag)
#'
#' saturated_dag |>
#'   ggdag(edge_type = "arc")
#'
#' saturated_dag |>
#'   dag_prune(c("x" = "y")) |>
#'   ggdag(edge_type = "arc")
#' @seealso [as_tidy_dagitty.list()]
dag_saturate <- function(
  .tdy_dag,
  use_existing_coords = FALSE,
  layout = "time_ordered",
  seed = NULL,
  ...
) {
  .dag <- pull_dag(.tdy_dag)
  edges_df <- .dag |>
    get_dagitty_edges() |>
    edges2df() |>
    add_isolated_nodes(names(.dag))

  bidirected_edges <- edges_df |>
    dplyr::filter(!is.na(.data$to), .data$direction == "<->")

  # a bidirected edge says nothing about which node comes first, so the time
  # ordering comes from the directed edges alone; the bidirected edges are
  # added back to the saturated DAG below
  layer_assign <- edges_df |>
    dplyr::filter(is.na(.data$to) | .data$direction != "<->") |>
    add_isolated_nodes(names(.dag)) |>
    longest_path_layers()

  df_time_order <- tibble::tibble(
    name = names(layer_assign),
    order = unname(layer_assign)
  ) |>
    dplyr::arrange(order)

  coords <- stored_coordinates(.dag, use_existing_coords)

  .adjusted <- dagitty::adjustedNodes(.dag)

  saturated_dag <- split(df_time_order$name, df_time_order$order) |>
    as_tidy_dagitty(
      exposure = dagitty::exposures(.dag),
      outcome = dagitty::outcomes(.dag),
      latent = dagitty::latents(.dag),
      labels = label(.dag),
      coords = coords,
      seed = seed,
      layout = layout,
      ...
    )

  saturated_dag <- add_bidirected_edges(saturated_dag, bidirected_edges)
  add_adjusted_nodes(saturated_dag, .adjusted)
}

#' Read the coordinates a `dagitty` object already stores
#'
#' `dagitty::coordinates()` reports a list of all-`NA` vectors for a DAG that
#' has never had coordinates set, rather than `NULL`. Passing that shape on as
#' if it were a real layout leaves the layout unresolved further down the
#' pipeline, so treat it as no coordinates at all.
#'
#' @param .dag A `dagitty` object.
#' @param use_existing_coords Whether the caller asked for the stored
#'   coordinates.
#' @return The stored coordinates, or `NULL`.
#' @noRd
stored_coordinates <- function(.dag, use_existing_coords) {
  if (!isTRUE(use_existing_coords)) {
    return(NULL)
  }

  coords <- dagitty::coordinates(.dag)
  if (is.null(coords) || all(is.na(unlist(coords)))) {
    return(NULL)
  }

  coords
}

#' Add the input's bidirected edges to a saturated DAG
#'
#' Saturation works from time ordering, which only directed edges express, so
#' the bidirected edges of the input have to be put back. Their endpoints take
#' the coordinates of the saturated layout.
#'
#' @param .tdy_dag The saturated `tidy_dagitty` object.
#' @param bidirected_edges A data frame of `name`/`to` pairs.
#' @return `.tdy_dag`, with a `<->` row per bidirected edge.
#' @noRd
add_bidirected_edges <- function(.tdy_dag, bidirected_edges) {
  if (nrow(bidirected_edges) == 0) {
    return(.tdy_dag)
  }

  dag_data <- pull_dag_data(.tdy_dag)
  node_coords <- dplyr::distinct(dag_data, .data$name, .data$x, .data$y)

  new_edges <- bidirected_edges |>
    dplyr::select("name", "to") |>
    dplyr::left_join(node_coords, by = "name") |>
    dplyr::left_join(
      dplyr::rename(node_coords, to = "name", xend = "x", yend = "y"),
      by = "to"
    ) |>
    dplyr::mutate(direction = "<->")

  updated_data <- dplyr::bind_rows(
    # a node joined by a bidirected edge is no longer edge-free
    dplyr::filter(
      dag_data,
      !is.na(.data$to) | .data$name %nin% bidirected_edges$name
    ),
    new_edges
  )

  # `dplyr::bind_rows()` drops the factor levels of `direction` when the new
  # rows carry it as character, so restore them
  updated_data$direction <- factor(
    as.character(updated_data$direction),
    levels = c("->", "<->", "--"),
    exclude = NA
  )

  .labels <- label(.tdy_dag)
  update_dag_data(.tdy_dag) <- updated_data
  .tdy_dag <- update_dag(.tdy_dag)

  # the rows added above carry no label of their own
  set_dag_labels(.tdy_dag, .labels)
}

#' Put labels back on a rebuilt DAG
#'
#' The rows added for the bidirected edges carry no label, and a node whose
#' only row is one of them would otherwise be drawn without one, so the labels
#' go back through the `tidy_dagitty` method, which rejoins the `label` column
#' as well as setting the attribute.
#'
#' @param .tdy_dag A `tidy_dagitty` object.
#' @param .labels The labels to restore, possibly `NULL`.
#' @return `.tdy_dag`, labeled.
#' @noRd
set_dag_labels <- function(.tdy_dag, .labels) {
  if (is.null(.labels)) {
    return(.tdy_dag)
  }

  label(.tdy_dag) <- .labels

  .tdy_dag
}

#' Carry adjustment status into a rebuilt DAG
#'
#' `dag_saturate()` forwards exposure, outcome, and latent status explicitly;
#' adjustment status is the same kind of node attribute and has to travel with
#' them.
#'
#' @param .tdy_dag The saturated `tidy_dagitty` object.
#' @param .adjusted The adjusted node names of the input DAG.
#' @return `.tdy_dag`, adjusted where the input was.
#' @noRd
add_adjusted_nodes <- function(.tdy_dag, .adjusted) {
  if (length(.adjusted) == 0) {
    return(.tdy_dag)
  }

  updated_dag <- pull_dag(.tdy_dag)
  dagitty::adjustedNodes(updated_dag) <- .adjusted
  update_dag(.tdy_dag) <- updated_dag

  dplyr::mutate(
    .tdy_dag,
    adjusted = factor(
      ifelse(.data$name %in% .adjusted, "adjusted", "unadjusted"),
      exclude = NA
    )
  )
}

#' @export
#' @rdname dag_saturate
dag_prune <- function(.tdy_dag, edges) {
  edges <- as_edge_specs(edges)

  dag_data <- pull_dag_data(.tdy_dag)
  # rows are matched and dropped by position, which grouping would make
  # per-group, so set it aside and restore it at the end
  groups <- dplyr::group_vars(dag_data)
  dag_data <- dplyr::ungroup(dag_data)

  matches <- match_edge_rows(dag_data, edges)
  validate_pruned_edges_exist(edges, matches)
  validate_pruned_edges_unambiguous(matches)

  pruned_data <- dplyr::filter(dag_data, dplyr::row_number() %nin% matches$row)

  # a node keeps its place in the DAG even when every one of its edges is
  # pruned, so give it back as a row with no edge
  lost_nodes <- setdiff(all_node_names(dag_data), all_node_names(pruned_data))
  if (length(lost_nodes) > 0) {
    pruned_data <- dplyr::bind_rows(
      pruned_data,
      edge_free_rows(dag_data, lost_nodes)
    )
  }

  if (length(groups) > 0) {
    pruned_data <- dplyr::group_by(pruned_data, !!!rlang::syms(groups))
  }

  update_dag_data(.tdy_dag) <- pruned_data
  update_dag(.tdy_dag)
}

#' Build a row with no edge for each node a prune left behind
#'
#' A node that a prune emptied of edges keeps its coordinates and its
#' node-level columns. A node that is only ever the end of an edge has no row
#' of its own to take them from, so its row is built from an edge that pointed
#' at it, whose end coordinates are the node's own.
#'
#' @param dag_data The data of the `tidy_dagitty` being pruned.
#' @param lost_nodes The names of the nodes that need a row.
#' @return One row per node in `lost_nodes`.
#' @noRd
edge_free_rows <- function(dag_data, lost_nodes) {
  named_rows <- dag_data |>
    dplyr::filter(.data$name %in% lost_nodes) |>
    dplyr::distinct(.data$name, .keep_all = TRUE)

  end_only <- setdiff(lost_nodes, dag_data$name)
  end_rows <- dag_data |>
    dplyr::filter(.data$to %in% end_only) |>
    dplyr::distinct(.data$to, .keep_all = TRUE) |>
    dplyr::mutate(name = .data$to, x = .data$xend, y = .data$yend)

  # every other column describes the node the borrowed row belonged to, which
  # is the node at the other end of the edge
  node_columns <- setdiff(
    names(end_rows),
    c("name", "to", "direction", "x", "y", "xend", "yend", "circular")
  )
  end_rows <- dplyr::mutate(
    end_rows,
    dplyr::across(dplyr::all_of(node_columns), na_like)
  )

  dplyr::bind_rows(named_rows, end_rows) |>
    dplyr::mutate(
      to = NA_character_,
      direction = na_like(.data$direction),
      xend = NA_real_,
      yend = NA_real_
    )
}

#' Find the rows of a DAG's data that a set of edge specifications names
#'
#' A specification names an edge by its endpoints, and the `name` of the
#' specification is the starting node, so a directed edge is named in its own
#' direction only. A bidirected or undirected edge has no direction of its own,
#' so either orientation of its endpoints names it. A specification that names a
#' direction as well matches only edges of that direction.
#'
#' @param dag_data The data of the `tidy_dagitty` being pruned.
#' @param edges A data frame of `name`/`to`/`direction` specifications, where
#'   `direction` is missing where the specification named none.
#' @return A data frame pairing the position of each matched specification with
#'   the position and description of the row it matched.
#' @noRd
match_edge_rows <- function(dag_data, edges) {
  # number the rows before dropping any, so a position still identifies a row
  # of the whole data, and match against edges only: a node-only row carries no
  # edge for `edges` to name
  rows <- dag_data |>
    dplyr::mutate(row = dplyr::row_number()) |>
    dplyr::select("name", "to", "direction", "row") |>
    dplyr::filter(!is.na(.data$to)) |>
    dplyr::mutate(direction = as.character(.data$direction))

  symmetric_rows <- dplyr::filter(
    rows,
    .data$direction %in% c("<->", "--")
  )

  specs <- edges |>
    dplyr::mutate(edge = dplyr::row_number()) |>
    dplyr::rename(asked_for = "direction")

  dplyr::bind_rows(
    dplyr::inner_join(specs, rows, by = c("name", "to"), na_matches = "never"),
    dplyr::inner_join(
      dplyr::select(specs, "edge", "asked_for", name = "to", to = "name"),
      symmetric_rows,
      by = c("name", "to"),
      na_matches = "never"
    )
  ) |>
    dplyr::filter(
      is.na(.data$asked_for) | .data$asked_for == .data$direction
    ) |>
    dplyr::distinct(
      .data$edge,
      .data$row,
      .data$asked_for,
      .data$name,
      .data$to,
      .data$direction
    )
}

#' Missing values of a vector's own type
#'
#' Assigning `NA` through `replace()` keeps a factor a factor, levels and all,
#' where rebuilding the column from `ifelse()` output does not.
#'
#' @param x A vector.
#' @return `x`, all missing.
#' @noRd
na_like <- function(x) {
  replace(x, seq_along(x), NA)
}

#' Read the `edges` argument of `dag_prune()` as one specification per row
#'
#' Both accepted forms describe the same thing, an edge named by its endpoints
#' and, optionally, its direction, so they are read into one shape here and the
#' pruning itself works from that.
#'
#' @param edges A named character vector, or a data frame with `name` and `to`
#'   columns and an optional `direction` column.
#' @param call The calling environment, for the error messages.
#' @return A tibble of `name`, `to`, and `direction`, the last missing wherever
#'   the specification named no direction.
#' @noRd
as_edge_specs <- function(edges, call = rlang::caller_env()) {
  n_specs <- if (is.data.frame(edges)) nrow(edges) else length(edges)
  if (n_specs == 0) {
    # pruning nothing would return the DAG unchanged, which reads as success
    abort(
      c(
        "{.arg edges} must name at least one edge to prune.",
        "i" = "Use the form {.code c(\"from\" = \"to\")}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  if (!is.data.frame(edges)) {
    validate_edge_specs(edges, call = call)

    return(tibble::tibble(
      name = names(edges),
      to = unname(edges),
      direction = NA_character_
    ))
  }

  assert_columns_exist(edges, c("name", "to"), call = call)

  specs <- tibble::tibble(
    name = as.character(edges$name),
    to = as.character(edges$to),
    direction = if ("direction" %in% names(edges)) {
      as.character(edges$direction)
    } else {
      NA_character_
    }
  )

  if (anyNA(specs$name) || anyNA(specs$to)) {
    abort(
      c(
        "Every edge in {.arg edges} must name two nodes.",
        "x" = "{.field name} and {.field to} must have no missing values."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  validate_direction(specs, call = call)

  specs
}

#' Check that every edge to prune names two nodes
#'
#' `edges` is read as name-to-value pairs, so an element with no name describes
#' no edge and would otherwise be dropped without a word. A missing value names
#' no node either, and a node-only row of the DAG's data records its `to` as
#' missing, so a missing value would otherwise match that row and prune a node.
#'
#' @param edges The `edges` argument of `dag_prune()`.
#' @param call The calling environment, for the error message.
#' @return `edges`, invisibly.
#' @noRd
validate_edge_specs <- function(edges, call = rlang::caller_env()) {
  if (is.null(names(edges)) || !all(nzchar(names(edges)))) {
    abort(
      c(
        "Every element of {.arg edges} must be named.",
        "i" = "Use the form {.code c(\"from\" = \"to\")}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  if (!is.character(edges) || anyNA(edges) || anyNA(names(edges))) {
    abort(
      c(
        "Every element of {.arg edges} and its name must be a node name.",
        "x" = "{.arg edges} must be a character vector with no missing values.",
        "i" = "Use the form {.code c(\"from\" = \"to\")}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  invisible(edges)
}

#' Check that every edge to prune is in the DAG
#'
#' Pruning an edge the DAG does not have would otherwise return the DAG
#' unchanged, which reads as success.
#'
#' @param edges A data frame of `name`/`to` pairs.
#' @param matches The output of `match_edge_rows()`.
#' @param call The calling environment, for the error message.
#' @return `edges`, invisibly.
#' @noRd
validate_pruned_edges_exist <- function(
  edges,
  matches,
  call = rlang::caller_env()
) {
  missing_edges <- edges[seq_len(nrow(edges)) %nin% matches$edge, ]

  if (nrow(missing_edges) > 0) {
    # one entry per missing edge, however many times it was asked for
    missing_edges <- unique(paste(
      missing_edges$name,
      ifelse(is.na(missing_edges$direction), "->", missing_edges$direction),
      missing_edges$to
    ))
    abort(
      c(
        "{.arg edges} must name edges that are in the DAG.",
        "x" = "Not in the DAG: {.val {missing_edges}}.",
        "i" = "The name of an element of {.arg edges} is the starting node and
               its value is the end node."
      ),
      error_class = "ggdag_missing_edges_error",
      call = call
    )
  }

  invisible(edges)
}

#' Check that every edge to prune is one edge
#'
#' A pair of nodes can hold a directed edge and a bidirected one at once, and a
#' specification that names the pair alone names both. Pruning both would remove
#' an edge the caller said nothing about, so the direction has to be named.
#'
#' @param matches The output of `match_edge_rows()`.
#' @param call The calling environment, for the error message.
#' @return `matches`, invisibly.
#' @noRd
validate_pruned_edges_unambiguous <- function(
  matches,
  call = rlang::caller_env()
) {
  ambiguous <- matches |>
    dplyr::filter(is.na(.data$asked_for)) |>
    dplyr::group_by(.data$edge) |>
    dplyr::filter(dplyr::n_distinct(.data$direction) > 1) |>
    dplyr::ungroup()

  if (nrow(ambiguous) > 0) {
    matched_edges <- unique(paste(
      ambiguous$name,
      ambiguous$direction,
      ambiguous$to
    ))
    abort(
      c(
        "Every edge in {.arg edges} must name one edge.",
        "x" = "More than one edge matches: {.val {matched_edges}}.",
        "i" = "Name the direction as well, with a data frame such as
               {.code data.frame(name = \"x\", to = \"y\", direction = \"->\")}."
      ),
      error_class = "ggdag_ambiguous_edge_error",
      call = call
    )
  }

  invisible(matches)
}
