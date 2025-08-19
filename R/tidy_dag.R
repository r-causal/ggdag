#' Tidy a `dagitty` object
#'
#' @param .dagitty a `dagitty`
#' @param seed a numeric seed for reproducible layout generation
#' @param layout a layout available in `ggraph`. See [ggraph::create_layout()]
#'   for details. Alternatively, `"time_ordered"` will use
#'   `time_ordered_coords()` to algorithmically sort the graph by time.
#' @param ... optional arguments passed to `ggraph::create_layout()`
#' @param use_existing_coords (Advanced). Logical. Use the coordinates produced
#'   by `dagitty::coordinates(.dagitty)`? If the coordinates are empty,
#'   `tidy_dagitty()` will generate a layout. Generally, setting this to `FALSE`
#'   is thus only useful when there is a difference in the variables coordinates
#'   and the variables in the DAG, as sometimes happens when recompiling a DAG.
#'
#' @return a `tidy_dagitty` object
#' @export
#'
#' @examples
#' library(dagitty)
#' library(ggplot2)
#'
#' dag <- dagitty("dag {
#'   Y <- X <- Z1 <- V -> Z2 -> Y
#'   Z1 <- W1 <-> W2 -> Z2
#'   X <- W1 -> Y
#'   X <- W2 -> Y
#'   X [exposure]
#'   Y [outcome]
#'   }")
#'
#' tidy_dagitty(dag)
#'
#' tidy_dagitty(dag, layout = "fr") |>
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_dag_node() +
#'   geom_dag_text() +
#'   geom_dag_edges() +
#'   theme_dag()
tidy_dagitty <- function(
  .dagitty,
  seed = NULL,
  layout = "nicely",
  ...,
  use_existing_coords = TRUE
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (!dagitty::is.dagitty(.dagitty)) {
    abort(
      c(
        "Input must be a {.cls dagitty} object.",
        "x" = "You provided a {.cls {class(.dagitty)}} object."
      ),
      error_class = "ggdag_type_error"
    )
  }

  if (dagitty::graphType(.dagitty) != "dag") {
    abort(
      c(
        "{.arg .dagitty} must be of graph type {.val dag}.",
        "x" = "Graph type is {.val {dagitty::graphType(.dagitty)}}."
      ),
      error_class = "ggdag_dag_error"
    )
  }

  dag_edges <- get_dagitty_edges(.dagitty)

  if (layout == "time_ordered") {
    coords <- dag_edges |>
      edges2df() |>
      auto_time_order() |>
      time_ordered_coords() |>
      coords2list()

    dagitty::coordinates(.dagitty) <- coords
  } else {
    check_verboten_layout(layout)
  }

  coords_df <- dag_edges |>
    dplyr::select(name, to) |>
    generate_layout(
      layout = layout,
      vertices = names(.dagitty),
      coords = if (isTRUE(use_existing_coords)) dagitty::coordinates(.dagitty),
      ...
    )

  tidy_dag <- dag_edges |>
    tidy_dag_edges_and_coords(coords_df)

  coords <- tidy_dag |>
    dplyr::distinct(name, x, y) |>
    coords2list()

  .labels <- label(.dagitty)
  dagitty::coordinates(.dagitty) <- coords
  label(.dagitty) <- .labels

  new_tidy_dagitty(tidy_dag, .dagitty)
}


#' Convert objects into `tidy_dagitty` objects
#'
#' An alternative API and specification to [tidy_dagitty()], `as_tidy_dagitty()`
#' allows you to create `tidy_dagitty` objects from data frames and lists. There
#' is also a method for `dagitty` objects, which is a thin wrapper for
#' [tidy_dagitty()]. To create a DAG from a list, each element of the list
#' should be a character vector, and the order of the elements should be the
#' time order in which they appear in the DAG, e.g. element 1 occurs at time
#' point 1. To create a DAG from a data frame, it must contain `name` and `to`
#' columns, representing the nodes and any edges leading from the nodes. If
#' there are `x`, `y`, `xend`, and `yend` columns, they will be used as
#' coordinates. Otherwise, `layout` will be used. See [tidy_dagitty] for more
#' information about layouts. Additionally, you can specify status (one of
#' `exposure`, `outcome`, or `latent`) by including a `status` column. Any other
#' columns in the data set will also be joined to the `tidy_dagitty` data.
#'
#' @param x An object to convert into a `tidy_dagitty`. Currently supports
#'   `dagitty` and `data.frame` objects.
#' @inheritParams tidy_dagitty
#' @inheritParams dagify
#' @param saturate Logical. Saturate the DAG such that there is an edge going
#'   from every point in the future from a given node? Setting this to `TRUE`
#'   will potentially lead to more edges than present in `x`.
#'
#' @return a `tidy_dagitty` object
#' @export
#'
#' @examples
#'
#' data.frame(name = c("c", "c", "x"), to = c("x", "y", "y")) |>
#'   as_tidy_dagitty()
#'
#' time_points <- list(c("a", "b", "c"), "d", c("e", "f", "g"), "z")
#'
#' time_points |>
#'   # create a saturated, time-ordered DAG
#'   as_tidy_dagitty() |>
#'   # remove the edge from `c` to `f`
#'   dag_prune(c("c" = "f"))
#'
#' @seealso [tidy_dagitty()], [pull_dag()]
as_tidy_dagitty <- function(x, ...) {
  UseMethod("as_tidy_dagitty")
}

#' @export
#' @rdname as_tidy_dagitty
as_tidy_dagitty.dagitty <- function(x, seed = NULL, layout = "nicely", ...) {
  tidy_dagitty(x, seed = seed, layout = layout, ...)
}

#' @export
#' @rdname as_tidy_dagitty
as_tidy_dagitty.data.frame <- function(
  x,
  exposure = NULL,
  outcome = NULL,
  latent = NULL,
  labels = NULL,
  coords = NULL,
  seed = NULL,
  layout = "nicely",
  saturate = FALSE,
  ...
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  tidy_dag <- prep_dag_data(x, layout = layout, coords = coords, ...)
  .dagitty <- compile_dag_from_df(x)

  if ("status" %in% names(tidy_dag)) {
    dagitty::exposures(.dagitty) <- return_status(tidy_dag, "exposure")
    dagitty::outcomes(.dagitty) <- return_status(tidy_dag, "outcome")
    dagitty::latents(.dagitty) <- return_status(tidy_dag, "latent")
  }

  if (!is.null(exposure)) {
    dagitty::exposures(.dagitty) <- exposure
  }

  if (!is.null(outcome)) {
    dagitty::outcomes(.dagitty) <- outcome
  }

  if (!is.null(latent)) {
    dagitty::latents(.dagitty) <- latent
  }

  if (!is.null(labels)) {
    label(.dagitty) <- labels
  }

  if ("adjusted" %in% names(tidy_dag)) {
    .adjusted <- dplyr::filter(tidy_dag, adjusted == "adjusted") |>
      dplyr::pull(name) |>
      empty2list()

    dagitty::adjustedNodes(.dagitty) <- .adjusted
  }

  dagitty::coordinates(.dagitty) <- tidy_dag |>
    select(name, x, y) |>
    coords2list()

  .tdy_dagitty <- new_tidy_dagitty(tidy_dag, .dagitty)

  if (isTRUE(saturate)) {
    .tdy_dagitty <- dag_saturate(.tdy_dagitty, use_existing_coords = TRUE)
  }

  .tdy_dagitty
}

#' @export
#' @rdname as_tidy_dagitty
as_tidy_dagitty.list <- function(
  x,
  exposure = NULL,
  outcome = NULL,
  latent = NULL,
  labels = NULL,
  coords = NULL,
  seed = NULL,
  layout = "time_ordered",
  ...
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  dag_edges <- purrr::map(
    seq_len(length(x) - 1),
    saturate_edges,
    time_points = x
  ) |>
    dplyr::bind_rows()

  dag_edges |>
    as_tidy_dagitty(
      exposure = exposure,
      outcome = outcome,
      latent = latent,
      labels = labels,
      coords = coords,
      seed = seed,
      layout = layout,
      ...
    )
}

saturate_edges <- function(.x, time_points) {
  current_elements <- time_points[[.x]]
  future_elements <- unlist(time_points[(.x + 1):length(time_points)])
  expand.grid(name = current_elements, to = future_elements)
}

new_tidy_dagitty <- function(tidy_dag, .dagitty) {
  .tdy_dag <- list(data = tidy_dag, dag = .dagitty)
  class(.tdy_dag) <- "tidy_dagitty"
  if (has_labels(.dagitty)) {
    label(.tdy_dag) <- label(.dagitty)
  }

  .tdy_dag
}

tidy_dag_edges_and_coords <- function(dag_edges, coords_df) {
  if ("direction" %nin% names(dag_edges)) {
    dag_edges$direction <- "->"
  }

  dag_edges |>
    dplyr::mutate(
      name = as.character(name),
      to = as.character(to),
      direction = factor(direction, levels = c("->", "<->", "--"), exclude = NA)
    ) |>
    (\(x) ggdag_left_join(coords_df, x, by = "name"))() |>
    ggdag_left_join(
      coords_df |> dplyr::select(name, x, y),
      by = c("to" = "name"),
      suffix = c("", "end")
    ) |>
    dplyr::select(name, x, y, direction, to, xend, yend, dplyr::everything())
}

generate_layout <- function(.df, layout, vertices = NULL, coords = NULL, ...) {
  ig <- igraph::graph_from_data_frame(.df, vertices = vertices)

  if (is.null(coords)) {
    no_existing_coords <- TRUE
  } else {
    no_existing_coords <- coords |>
      purrr::map_lgl(\(.x) all(is.na(.x))) |>
      all()
  }

  if (no_existing_coords) {
    ggraph_layout <- ggraph_create_layout(
      ig,
      layout = layout,
      ...
    )
  } else {
    nodes <- names(igraph::V(ig))
    coords$x <- coords$x[nodes]
    coords$y <- coords$y[nodes]
    ggraph_layout <- ggraph_create_layout(
      ig,
      layout = "manual",
      x = coords$x,
      y = coords$y,
      ...
    )
  }

  layout_df <- ggraph_layout |>
    dplyr::select(name, x, y, circular) |>
    dplyr::as_tibble()

  # Remove circular column if all values are FALSE (issue #119)
  if (all(!layout_df$circular)) {
    layout_df$circular <- NULL
  }

  layout_df
}

check_verboten_layout <- function(layout) {
  if (layout %in% c("dendogram")) {
    abort(
      c(
        "Layout type {.val {layout}} is not supported in ggdag.",
        "i" = "See {.help ggraph::create_layout} for available layouts."
      )
    )
  }
}

#' Test for object class for tidy_dagitty
#'
#' @param x object to be tested
#' @export
is.tidy_dagitty <- function(x) {
  inherits(x, "tidy_dagitty")
}

#' Fortify a `tidy_dagitty` object for `ggplot2`
#'
#' @param model an object of class `tidy_dagitty` or `dagitty`
#' @param data (not used)
#' @param ... (not used)
#'
#' @export
#' @importFrom ggplot2 fortify
#'
#' @rdname fortify
#' @name fortify
fortify.tidy_dagitty <- function(model, data = NULL, ...) {
  pull_dag_data(model)
}

#' @rdname fortify
#' @export
fortify.dagitty <- function(model, data = NULL, ...) {
  model |>
    tidy_dagitty() |>
    pull_dag_data()
}

#' Convert a `tidy_dagitty` object to data.frame
#'
#' @param x an object of class `tidy_dagitty`
#' @param row.names NULL or a character vector giving the row names for the data
#'   frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column
#'   names (to syntactic names: see make.names) is optional. Note that all of
#'   R's base package `as.data.frame()` methods use optional only for column names
#'   treatment, basically with the meaning of `data.frame(*, check.names =
#'   !optional)`
#' @param ... optional arguments passed to `as.data.frame()`
#'
#' @export
as.data.frame.tidy_dagitty <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  as.data.frame(
    pull_dag_data(x),
    row.names = row.names,
    optional = optional,
    ...
  )
}

#' Convert a `tidy_dagitty` object to tbl_df
#'
#' @inheritParams dag_params
#'
#' @export
#' @importFrom dplyr tbl_df
tbl_df.tidy_dagitty <- function(.tdy_dag) {
  pull_dag_data(.tdy_dag)
}

#' Convert a `tidy_dagitty` object to tbl
#'
#' @param x an object of class `tidy_dagitty`
#' @param row.names NULL or a character vector giving the row names for the data
#'   frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column
#'   names (to syntactic names: see make.names) is optional. Note that all of
#'   R's base package `as.data.frame()` methods use optional only for column names
#'   treatment, basically with the meaning of `data.frame(*, check.names =
#'   !optional)`
#' @param ... optional arguments passed to [`dplyr::as_tibble()`]
#'
#' @export
#' @importFrom dplyr as.tbl as_tibble
as.tbl.tidy_dagitty <- function(x, row.names = NULL, optional = FALSE, ...) {
  dplyr::as.tbl(
    pull_dag_data(x),
    row.names = row.names,
    optional = optional,
    ...
  )
}

#' @export
#' @rdname as.tbl.tidy_dagitty
as_tibble.tidy_dagitty <- function(x, row.names = NULL, optional = FALSE, ...) {
  dplyr::as_tibble(
    pull_dag_data(x),
    row.names = row.names,
    optional = optional,
    ...
  )
}

# Helper function to combine pillar subtle styling with cli formatting
subtle_inline <- function(text, .envir = parent.frame()) {
  pillar::style_subtle(cli::format_inline(text, .envir = .envir))
}

#' Provide a succinct summary of a tidy_dagitty object
#'
#' @param x an object of class `tidy_dagitty`
#' @param ... Ignored
#'
#' @return A named character vector
#' @export
#' @keywords internal
#' @importFrom pillar tbl_sum
tbl_sum.tidy_dagitty <- function(x, ...) {
  coll <- function(x, ...) paste(x, collapse = ", ", ...)

  # Get DAG component once
  dag <- pull_dag(x)

  # Get counts from DAG component
  node_count <- length(names(dag))
  edge_count <- nrow(dagitty::edges(dag))

  # Proper pluralization
  node_text <- if (node_count == 1) "node" else "nodes"
  edge_text <- if (edge_count == 1) "edge" else "edges"

  summary_info <- c(
    "A `dagitty` DAG with" = paste0(
      node_count,
      " ",
      node_text,
      " and ",
      edge_count,
      " ",
      edge_text
    )
  )

  if (has_exposure(x)) {
    summary_info <- c(summary_info, "Exposure" = coll(dagitty::exposures(dag)))
  }

  if (has_outcome(x)) {
    summary_info <- c(summary_info, "Outcome" = coll(dagitty::outcomes(dag)))
  }

  if (has_latent(x)) {
    summary_info <- c(
      summary_info,
      "Latent Variable" = coll(dagitty::latents(dag))
    )
  }

  if (has_collider_path(x)) {
    summary_info <- c(
      summary_info,
      "Paths opened by conditioning on a collider" = coll(collider_paths(x))
    )
  }

  # Check for special analysis results
  data <- pull_dag_data(x)

  # Adjustment sets
  if (all(c("adjusted", "set") %in% names(data))) {
    unique_sets <- unique(data$set)
    n_sets <- length(unique_sets)

    # Check if it's the special case of unconditionally closed paths
    if (n_sets == 1 && grepl("Backdoor.*Closed", unique_sets[1])) {
      summary_info <- c(
        summary_info,
        "Adjustment sets" = "0 (Backdoor paths unconditionally closed)"
      )
    } else {
      set_text <- if (n_sets == 1) "set" else "sets"
      summary_info <- c(
        summary_info,
        "Adjustment sets" = paste0(
          n_sets,
          " ",
          set_text,
          ": ",
          paste(unique_sets, collapse = ", ")
        )
      )
    }
  }

  # Paths
  if (all(c("path", "set") %in% names(data))) {
    dag <- pull_dag(x)
    paths_obj <- dagitty::paths(dag)

    if (!is.null(paths_obj) && length(paths_obj$paths) > 0) {
      open_paths <- paths_obj$paths[paths_obj$open]

      if (length(open_paths) > 0) {
        # Format paths with curly braces
        formatted_paths <- paste0("{", open_paths, "}")

        path_text <- if (length(open_paths) == 1) "open path" else "open paths"
        summary_info <- c(
          summary_info,
          "Paths" = paste0(
            length(open_paths),
            " ",
            path_text,
            ": ",
            paste(formatted_paths, collapse = ", ")
          )
        )
      } else {
        # No open paths
        summary_info <- c(summary_info, "Paths" = "0 open paths")
      }
    }
  }

  summary_info
}

#' Format a `tidy_dagitty` object
#'
#' @param x an object of class `tidy_dagitty`
#' @param ... optional arguments passed to format
#' @param n Number of rows to show
#' @param width Width of output
#' @param n_extra Number of extra columns to print
#'
#' @return Character vector of formatted output
#' @export
#' @keywords internal
format.tidy_dagitty <- function(
  x,
  ...,
  n = NULL,
  width = NULL,
  n_extra = NULL
) {
  # Get the header from tbl_sum
  header <- tbl_sum(x)

  # Add DAG section header
  formatted_output <- subtle_inline("# {.strong DAG:}")

  # Format the header
  for (i in seq_along(header)) {
    formatted_output <- c(
      formatted_output,
      subtle_inline("# {names(header)[i]}: {header[i]}")
    )
  }

  # Add separator
  formatted_output <- c(formatted_output, pillar::style_subtle("#"))

  # Add Data section header
  formatted_output <- c(formatted_output, subtle_inline("# {.strong Data:}"))

  # Format the data using pillar's format for the tibble
  data_formatted <- format(
    pull_dag_data(x),
    n = n,
    width = width,
    n_extra = n_extra,
    ...
  )

  # Combine everything
  c(formatted_output, data_formatted, pillar::style_subtle("#"))
}

#' @export
#' @keywords internal
#' @importFrom pillar tbl_format_footer
tbl_format_footer.tidy_dagitty <- function(x, setup, ...) {
  # Add our custom footer
  info_line <- subtle_inline(
    "# {cli::symbol$info} Use `{.topic [pull_dag()](pull_dag)}` to retrieve the DAG object and `{.topic [pull_dag_data()](pull_dag_data)}` for the data frame"
  )

  info_line
}

#' Print a `tidy_dagitty`
#'
#' @param x an object of class `tidy_dagitty`
#' @param ... optional arguments passed to `format()`
#'
#' @export
print.tidy_dagitty <- function(x, ...) {
  # Use pillar's formatting system to include footer
  formatted <- format(x, ...)

  # Add footer
  footer <- tbl_format_footer(x, setup = NULL)

  writeLines(c(formatted, footer))
  invisible(x)
}

#  not available in the current CRAN version of dagitty
# is_acyclic <- function(g) {
#   dagitty::isAcyclic(g)
# }

#' Manipulate DAG coordinates
#'
#' @param coord_list a named list of coordinates
#' @param coord_df a data.frame with columns x, y, and name
#'
#' @return either a list or a data.frame with DAG node coordinates
#' @export
#'
#' @examples
#' library(dagitty)
#' coords <- list(
#'   x = c(A = 1, B = 2, D = 3, C = 3, F = 3, E = 4, G = 5, H = 5, I = 5),
#'   y = c(A = 0, B = 0, D = 1, C = 0, F = -1, E = 0, G = 1, H = 0, I = -1)
#' )
#' coord_df <- coords2df(coords)
#' coords2list(coord_df)
#'
#' x <- dagitty("dag{
#'              G <-> H <-> I <-> G
#'              D <- B -> C -> I <- F <- B <- A
#'              H <- E <- C -> G <- D
#'              }")
#' coordinates(x) <- coords2list(coord_df)
#'
#' @rdname coordinates
#' @name coordinates
coords2df <- function(coord_list) {
  coord_df <- purrr::map(coord_list, tibble::enframe) |>
    purrr::reduce(ggdag_left_join, by = "name")
  names(coord_df) <- c("name", "x", "y")
  coord_df
}

#' @rdname coordinates
#' @export
coords2list <- function(coord_df) {
  x <- coord_df |>
    dplyr::select(name, x) |>
    tibble::deframe()
  y <- coord_df |>
    dplyr::select(name, y) |>
    tibble::deframe()
  list(x = x, y = y)
}
