#' Pull components from DAG objects
#'
#' `pull_dag()` and `pull_dag_data()` are generic methods to pull components of
#' DAG objects, e.g. `tidy_dagitty`, such as the `dagitty` object or the data
#' frame associated with it. These methods are recommended over extracting
#' components manually, e.g. `my_dag$data`, because the internal structure of
#' these objects may change over time. Similarly, use `update_dag()` if you want
#' to sync the data back to the DAG object or override it with another DAG; use
#' `update_dag_data()` to do update the data frame. This is useful with
#' `pull_dag_data()`.
#'
#' @param x a `tidy_dagitty` or `dagitty` object.
#' @param value a value to set, either a `dagitty` or `data.frame` object,
#'   depending on the function.
#' @param ... For `dagitty` objects, passed to `tidy_dagitty()` if needed,
#'   otherwise currently unused.
#'
#' @return a DAG object, e.g. `dagitty`, or data frame
#'
#' @examples
#'
#' tidy_dagitty_obj <- dagify(y ~ x + z, x ~ z) |>
#'   tidy_dagitty()
#' dag <- pull_dag(tidy_dagitty_obj)
#' dag_data <- pull_dag_data(tidy_dagitty_obj)
#'
#' tidy_dagitty_obj |>
#'   # rename both endpoints of every edge, or the recompiled DAG will hold a
#'   # mix of the old and new names
#'   dplyr::mutate(name = toupper(name), to = toupper(to)) |>
#'   # recreate the DAG component
#'   update_dag()
#'
#' dag_data$label <- paste0(dag_data$name, "(observed)")
#' update_dag_data(tidy_dagitty_obj) <- dag_data
#'
#' @export
pull_dag <- function(x, ...) {
  UseMethod("pull_dag")
}

#' @export
pull_dag.default <- function(x, ...) {
  abort(
    c(
      "{.fun pull_dag} requires a {.cls tidy_dagitty} or {.cls dagitty} object.",
      "x" = "You provided a {.cls {class(x)}} object."
    ),
    error_class = "ggdag_type_error"
  )
}

#' @export
#' @rdname pull_dag
pull_dag.tidy_dagitty <- function(x, ...) {
  x$dag
}

#' @export
#' @rdname pull_dag
pull_dag.dagitty <- function(x, ...) {
  x
}

#' @export
#' @rdname pull_dag
pull_dag_data <- function(x, ...) {
  UseMethod("pull_dag_data")
}

#' @export
pull_dag_data.default <- function(x, ...) {
  abort(
    c(
      "{.fun pull_dag_data} requires a {.cls tidy_dagitty} or {.cls dagitty} object.",
      "x" = "You provided a {.cls {class(x)}} object."
    ),
    error_class = "ggdag_type_error"
  )
}

#' @export
#' @rdname pull_dag
pull_dag_data.tidy_dagitty <- function(x, ...) {
  x$data
}

#' @export
#' @rdname pull_dag
pull_dag_data.dagitty <- function(x, ...) {
  tidy_dagitty(x, ...)$data
}

#' @export
#' @rdname pull_dag
`update_dag_data<-` <- function(x, value) {
  UseMethod("update_dag_data<-")
}

#' @export
#' @rdname pull_dag
`update_dag_data<-.tidy_dagitty` <- function(x, value) {
  dag <- pull_dag(x)
  previous <- pull_dag_data(x)
  layout <- ggdag_option("layout", "nicely")
  # the layout is computed from the whole edge list, so the grouping is set
  # aside here as it is in `prep_dag_data()`
  rebuilt <- rebuilt_coordinates(dplyr::ungroup(value), layout, dag)

  x$data <- prep_dag_data(
    value,
    layout = layout,
    coords = rebuilt$coords,
    dag = dag,
    call = rlang::caller_env()
  )

  x$dag <- set_layout_direction(
    dag,
    incoming_layout_direction(value, previous, dag, rebuilt)
  )

  x
}

#' The coordinates a rebuilt layout gives the incoming data
#'
#' `prep_dag_data()` regenerates the layout when the data arrives without a
#' full set of coordinate columns. Working them out before it does costs
#' nothing, because they are handed to it rather than computed twice, and it
#' is what lets the setter mark the DAG with the axis the new layout ran its
#' layers along.
#'
#' @param value The incoming data.
#' @param layout The layout the data will be laid out with.
#' @param dag The `dagitty` object the data belongs to.
#' @return A list of whether the layout is being rebuilt at all and, if it is,
#'   the coordinates it gives, which are `NULL` when ggraph is to lay the DAG
#'   out.
#' @noRd
rebuilt_coordinates <- function(value, layout, dag) {
  # data that does not name its nodes is rejected by `prep_dag_data()`, which
  # is left to report it rather than laying the DAG out first
  rebuilding <- all(c("name", "to") %in% names(value)) &&
    incomplete_coordinates(value)

  if (!rebuilding) {
    return(list(rebuilding = FALSE, coords = NULL))
  }

  list(rebuilding = TRUE, coords = layout_coordinates(value, layout, dag))
}

#' The axis the DAG's layers run along once the data is replaced
#'
#' The mark describes where the nodes sit, so it holds only while the data
#' holds the coordinates it was recorded for. A rebuilt layout takes the mark
#' of the layout that rebuilt it; data that keeps its coordinates keeps the
#' mark the DAG already carries; and coordinates written in from anywhere else
#' belong to no layout ggdag laid out, so they leave the DAG unmarked.
#'
#' @param value The incoming data.
#' @param previous The data it replaces.
#' @param dag The `dagitty` object the data belongs to.
#' @param rebuilt The report of `rebuilt_coordinates()`.
#' @return `"x"`, `"y"`, or `NULL`.
#' @noRd
incoming_layout_direction <- function(value, previous, dag, rebuilt) {
  if (isTRUE(rebuilt$rebuilding)) {
    return(layout_direction(rebuilt$coords))
  }

  if (unmoved_coordinates(value, previous)) {
    return(layout_direction(dag))
  }

  NULL
}

#' Does the incoming data still hold the coordinates it is replacing?
#'
#' A verb that leaves the coordinates alone leaves the layout that computed
#' them in place, however else it reshapes the data: rows are dropped, added,
#' and reordered by the analysis functions, so the nodes the two data frames
#' have in common are what is compared.
#'
#' @param value The incoming data.
#' @param previous The data it replaces.
#' @return `TRUE` when every node the two share sits where it did.
#' @noRd
unmoved_coordinates <- function(value, previous) {
  needed <- c("name", "x", "y")
  if (any(needed %nin% names(value)) || any(needed %nin% names(previous))) {
    return(FALSE)
  }

  incoming <- node_positions(value)
  before <- node_positions(previous)
  shared <- intersect(names(incoming), names(before))

  length(shared) > 0 && identical(incoming[shared], before[shared])
}

#' Where each node of a data frame sits
#'
#' A node holds the same position on each of its rows, so the first row of
#' each speaks for it. The positions are compared as text, which reads a
#' coordinate the same whether it is stored as an integer or a double.
#'
#' @param data A data frame with `name`, `x`, and `y` columns.
#' @return A character vector of positions, named by node.
#' @noRd
node_positions <- function(data) {
  speaks_for_node <- !is.na(data$name) & !duplicated(data$name)

  stats::setNames(
    paste(data$x[speaks_for_node], data$y[speaks_for_node]),
    data$name[speaks_for_node]
  )
}

#' Does the data arrive without a full set of coordinates?
#'
#' `prep_dag_data()` regenerates the layout, and so consults the `coords` it
#' is given, only when a coordinate column is missing. Everything that has to
#' know whether the coordinates it is holding will be used asks here, so that
#' the question is settled in one place.
#'
#' @param value A data frame that may carry coordinate columns.
#' @return `TRUE` when any of `x`, `y`, `xend`, or `yend` is absent.
#' @noRd
incomplete_coordinates <- function(value) {
  any(c("x", "y", "xend", "yend") %nin% names(value))
}

prep_dag_data <- function(
  value,
  layout = ggdag_option("layout", "nicely"),
  coords = NULL,
  dag = NULL,
  ...,
  call = rlang::caller_env()
) {
  if (any(c("name", "to") %nin% names(value))) {
    assert_columns_exist(value, c("name", "to"), call = call)
  }

  check_verboten_layout(layout)

  validate_direction(value, call = call)

  # the layout work below reorders and reshapes columns, which grouping would
  # interfere with, so set it aside and restore it at the end
  groups <- dplyr::group_vars(value)
  value <- dplyr::ungroup(value)

  if (is.data.frame(coords)) {
    coords <- layout_coords_list(coords)
  }

  if ("direction" %nin% names(value)) {
    # rows with no edge are node-only rows, not directed edges
    value$direction <- ifelse(is.na(value$to), NA_character_, "->")
  }

  if (incomplete_coordinates(value)) {
    # a partial set of coordinate columns can't be reconciled with a freshly
    # generated layout, so drop them and regenerate all four consistently
    value <- dplyr::select(value, -dplyr::any_of(c("x", "y", "xend", "yend")))

    if (is.null(coords)) {
      coords <- layout_coordinates(value, layout, dag)
    }

    coords_df <- value |>
      dplyr::select("name", "to") |>
      dplyr::filter(!is.na(.data$name), !is.na(.data$to)) |>
      generate_layout(
        layout = layout,
        vertices = all_node_names(value),
        coords = coords,
        ...
      )

    value <- value |>
      tidy_dag_edges_and_coords(coords_df)
  }

  if (!is.factor(value$direction)) {
    value$direction <- factor(
      value$direction,
      levels = c("->", "<->", "--"),
      exclude = NA
    )
  }

  # Remove circular column if all values are FALSE (issue #119)
  if ("circular" %in% names(value) && !any(value$circular)) {
    value$circular <- NULL
  }

  value <- dplyr::as_tibble(value)
  groups <- intersect(groups, names(value))

  if (length(groups) > 0) {
    value <- dplyr::group_by(value, !!!rlang::syms(groups))
  }

  value
}

#' Work out the coordinates a layout specification asks for
#'
#' Only the layouts ggdag resolves itself are computed here; the rest are left
#' to `generate_layout()`, which passes them to ggraph. The result is thrown
#' away unless a coordinate column is missing, so it is computed at the point
#' of use rather than for every call: a layout is expensive, and the
#' time-ordered one reports on its own work.
#'
#' @param value A data frame of edges.
#' @param layout A layout name, data frame, or function.
#' @param dag The `dagitty` object the data came from, or `NULL` when there is
#'   none, as when a DAG is first built from a data frame.
#' @return A list of `x` and `y`, or `NULL` if ggraph is to lay the DAG out.
#' @noRd
layout_coordinates <- function(value, layout, dag = NULL) {
  if (is.data.frame(layout)) {
    return(layout_coords_list(layout))
  }

  if (is.function(layout) || identical(layout, "time_ordered")) {
    return(compute_layout_coords(layout, value, all_node_names(value), dag))
  }

  NULL
}

#' Compute coordinates for a layout ggdag resolves itself
#'
#' Both entry points to a resolved layout go through this builder: the direct
#' path in `tidy_dagitty()` and the rebuild in `prep_dag_data()` when a dplyr
#' verb removes the coordinate columns. Sharing it keeps the two paths in
#' step: the engine's input always lists edge rows first, then terminal
#' nodes, then isolated nodes, and the layout always sees the DAG's exposure,
#' outcome, and the node size option.
#'
#' @param layout A layout function, or the string `"time_ordered"`.
#' @param edges_df A data frame with `name` and `to` columns; rows with a
#'   missing `to` are node-only rows and are rebuilt in canonical order.
#' @param nodes A character vector of every node in the DAG.
#' @param dag The `dagitty` object the edges came from, for its exposure and
#'   outcome, or `NULL` when there is none.
#' @param layout_args A named list of arguments for a layout named by string,
#'   which the caller split out of its own `...`.
#' @return A list of `x` and `y` coordinates named by node.
#' @noRd
compute_layout_coords <- function(
  layout,
  edges_df,
  nodes,
  dag = NULL,
  layout_args = list()
) {
  exposure <- if (is.null(dag)) character(0) else dagitty::exposures(dag)
  outcome <- if (is.null(dag)) character(0) else dagitty::outcomes(dag)

  # isolated nodes never appear in the edge list, so add them explicitly or
  # the layout will not position them; they go last so the engine seeds
  # within-layer order from the edges rather than from stored row order
  input <- edges_df |>
    dplyr::filter(!is.na(.data$to)) |>
    edges2df() |>
    add_isolated_nodes(nodes)

  # a layout named by string is the function it stands for, built with the
  # arguments the caller passed for it
  if (is_built_in_layout(layout)) {
    layout <- resolve_built_in_layout(layout, layout_args)
  }

  # manual time periods laid out without the engine are coordinates already
  if (is.data.frame(layout)) {
    return(layout_coords_list(layout))
  }

  coords <- if ("..." %in% names(formals(layout))) {
    layout(input, exposure = exposure, outcome = outcome)
  } else {
    layout(input)
  }

  layout_coords_list(coords)
}

#' Check that edge directions are ones ggdag understands
#'
#' The tidy data and the `dagitty` component are built from the same `direction`
#' column, so an unrecognized value would leave the two out of step: the data
#' would record no edge while the DAG contains one.
#'
#' @param value A data frame that may have a `direction` column.
#' @param call The calling environment, for the error message.
#' @return `value`, invisibly.
#' @noRd
validate_direction <- function(value, call = rlang::caller_env()) {
  if ("direction" %nin% names(value)) {
    return(invisible(value))
  }

  directions <- as.character(value$direction)
  unsupported <- setdiff(
    unique(directions[!is.na(directions)]),
    c("->", "<->", "--")
  )

  if (length(unsupported) > 0) {
    abort(
      c(
        "{.field direction} must be one of {.val {c('->', '<->', '--')}}.",
        "x" = "Unsupported values: {.val {unsupported}}.",
        "i" = "To reverse an edge, swap the {.field name} and {.field to} values."
      ),
      error_class = "ggdag_dag_error",
      call = call
    )
  }

  invisible(value)
}

#' @export
#' @rdname pull_dag
update_dag <- function(x, ...) {
  UseMethod("update_dag")
}

#' @export
#' @rdname pull_dag
`update_dag<-` <- function(x, value) {
  UseMethod("update_dag<-")
}

#' @export
#' @rdname pull_dag
`update_dag.tidy_dagitty` <- function(x, ...) {
  if (...length() > 0) {
    abort(
      c(
        "{.fun update_dag} takes no other arguments.",
        "x" = "It rebuilds the {.cls dagitty} component from {.arg x}'s own data.",
        "i" = "To install a different DAG, use {.code update_dag(x) <- value}."
      ),
      error_class = "ggdag_type_error"
    )
  }

  update_dag(x) <- recompile_dag(x)
  x
}

#' @export
#' @rdname pull_dag
`update_dag<-.tidy_dagitty` <- function(x, value) {
  if (!dagitty::is.dagitty(value)) {
    abort(
      c(
        "{.arg value} must be a {.cls dagitty} object.",
        "x" = "You provided a {.cls {class(value)}} object."
      ),
      error_class = "ggdag_type_error"
    )
  }
  x$dag <- value
  x
}

recompile_dag <- function(.dag) {
  new_dag <- .dag |>
    pull_dag_data() |>
    compile_dag_from_df()

  if ("status" %in% names(pull_dag_data(.dag))) {
    .exposures <- return_status(.dag, "exposure")
    .outcomes <- return_status(.dag, "outcome")
    .latents <- return_status(.dag, "latent")
  } else {
    .exposures <- dagitty::exposures(pull_dag(.dag))
    .outcomes <- dagitty::outcomes(pull_dag(.dag))
    .latents <- dagitty::latents(pull_dag(.dag))
  }

  if ("adjusted" %in% names(pull_dag_data(.dag))) {
    .adjusted <- dplyr::filter(.dag, .data$adjusted == "adjusted") |>
      pull_dag_data() |>
      dplyr::pull(.data$name) |>
      empty2list()
  } else {
    .adjusted <- dagitty::adjustedNodes(pull_dag(.dag))
  }

  dagitty::exposures(new_dag) <- .exposures
  dagitty::outcomes(new_dag) <- .outcomes
  dagitty::latents(new_dag) <- .latents

  dagitty::adjustedNodes(new_dag) <- .adjusted

  dagitty::coordinates(new_dag) <- .dag |>
    pull_dag_data() |>
    select("name", "x", "y") |>
    coords2list()

  # `dagitty::coordinates<-` rebuilds the object and strips custom attributes,
  # so the labels and the axis the layout ran along are set afterwards. The
  # coordinates are the ones the data was laid out with, so the axis that
  # describes them is still the DAG's own.
  new_dag <- set_node_labels(new_dag, label(pull_dag(.dag)))

  keep_layout_direction(new_dag, pull_dag(.dag))
}

compile_dag_from_df <- function(.df, call = rlang::caller_env()) {
  if (nrow(.df) == 0) {
    abort(
      c(
        "Can't compile a {.cls dagitty} object from an empty data frame.",
        "i" = "{.arg .df} needs at least one row naming a node."
      ),
      error_class = "ggdag_dag_error",
      call = call
    )
  }

  check_representable_names(all_node_names(.df), call = call)

  if ("direction" %nin% names(.df)) {
    .df$direction <- "->"
  }

  edge_rows <- .df |>
    dplyr::filter(!is.na(.data$to)) |>
    dplyr::mutate(direction = as.character(.data$direction))

  edge_formulas <- edge_rows |>
    dplyr::group_by(.data$name, .data$direction) |>
    dplyr::summarise(
      to_formula = paste(
        "{",
        paste(quote_dagitty_name(.data$to), collapse = " "),
        "}"
      ),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      dag_formula = paste(
        quote_dagitty_name(.data$name),
        .data$direction,
        .data$to_formula
      )
    ) |>
    dplyr::pull()

  # nodes with no edges are only kept if they get their own bare statement
  isolated <- setdiff(all_node_names(.df), c(edge_rows$name, edge_rows$to))

  c(edge_formulas, quote_dagitty_name(isolated)) |>
    paste(collapse = "; ") |>
    (\(x) paste("dag {", x, "}"))() |>
    dagitty::dagitty()
}

return_status <- function(.dag, .status) {
  if (is.tidy_dagitty(.dag)) {
    .dag <- pull_dag_data(.dag)
  }

  dplyr::filter(.dag, .data$status == .status) |>
    dplyr::pull(.data$name) |>
    empty2list()
}

empty2list <- function(.x) {
  if (purrr::is_empty(.x)) {
    list()
  } else {
    .x
  }
}
