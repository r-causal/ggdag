#' Tidy a `dagitty` object
#'
#' @param .dagitty a `dagitty`
#' @param seed a numeric seed for reproducible layout generation
#' @param layout a layout available in `ggraph`. See [ggraph::create_layout()]
#'   for details. Alternatively, `"time_ordered"` will use
#'   `time_ordered_coords()` to algorithmically sort the graph by time.
#' @param ... optional arguments passed to `ggraph::create_layout()`
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
#' tidy_dagitty(dag, layout = "fr") %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_dag_node() +
#'   geom_dag_text() +
#'   geom_dag_edges() +
#'   theme_dag()
tidy_dagitty <- function(.dagitty, seed = NULL, layout = "nicely", ...) {
  if (!is.null(seed)) set.seed(seed)

  if (dagitty::graphType(.dagitty) != "dag") {
    stop("`.dagitty` must be of graph type `dag`")
  }

  dag_edges <- get_dagitty_edges(.dagitty)

  if (layout == "time_ordered") {
    coords <- dag_edges %>%
      edges2df() %>%
      auto_time_order() %>%
      time_ordered_coords() %>%
      coords2list()

    dagitty::coordinates(.dagitty) <- coords
  } else {
    check_verboten_layout(layout)
  }

  coords_df <- dag_edges %>%
    dplyr::select(name, to) %>%
    generate_layout(
      layout = layout,
      vertices = names(.dagitty),
      coords = dagitty::coordinates(.dagitty),
      ...
    )

  tidy_dag <- dag_edges %>%
    tidy_dag_edges_and_coords(coords_df)

  new_tidy_dagitty(tidy_dag, .dagitty)
}


#' Convert objects into `tidy_dagitty` objects
#'
#' An alternative API and specification to [tidy_dagitty()], `as_tidy_dagitty()`
#' allows you to create `tidy_dagitty` objects from data frames. There is also a
#' method for `dagitty` objects, which is a thin wrapper for [tidy_dagitty()].
#' To create a DAG from a data frame, it must contain `name` and `to` columns,
#' representing the nodes and any edges leading from the nodes. If there are
#' `x`, `y`, `xend`, and `yend` columns, they will be used as coordinates.
#' Otherwise, `layout` will be used. See [tidy_dagitty] for more information
#' about layouts. Additionally, you can specify status (one of `exposure`,
#' `outcome`, or `latent`) by including a `status` column. Any other columns in
#' the data set will also be joined to the `tidy_dagitty` data.
#'
#' @param x An object to convert into a `tidy_dagitty`. Currently supports
#'   `dagitty` and `data.frame` objects.
#' @inheritParams tidy_dagitty
#'
#' @return a `tidy_dagitty` object
#' @export
#'
#' @examples
#'
#' data.frame(name = c("c", "c", "x"), to = c("x", "y", "y")) %>%
#'   as_tidy_dagitty()
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
as_tidy_dagitty.data.frame <- function(x, seed = NULL, layout = "nicely", ...) {
  if (!is.null(seed)) set.seed(seed)
  tidy_dag <- prep_dag_data(x, layout = layout, ...)
  .dagitty <- compile_dag_from_df(x)
  if ("status" %in% names(tidy_dag)) {
    dagitty::exposures(.dagitty) <- return_status(tidy_dag, "exposure")
    dagitty::outcomes(.dagitty) <- return_status(tidy_dag, "outcome")
    dagitty::latents(.dagitty) <- return_status(tidy_dag, "latent")
  }

  if ("adjusted" %in% names(tidy_dag)) {
    .adjusted <- dplyr::filter(tidy_dag, adjusted == "adjusted") %>%
      dplyr::pull(name) %>%
      empty2list()

    dagitty::adjustedNodes(.dagitty) <- .adjusted
  }

  dagitty::coordinates(.dagitty) <- tidy_dag %>%
    select(name, x, y) %>%
    coords2list()

  new_tidy_dagitty(tidy_dag, .dagitty)
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

  dag_edges %>%
    dplyr::mutate(
      name = as.character(name),
      to = as.character(to),
      direction = factor(direction, levels = c("->", "<->", "--"), exclude = NA)
    ) %>%
    ggdag_left_join(coords_df, ., by = "name") %>%
    ggdag_left_join(
      coords_df %>% dplyr::select(name, x, y),
      by = c("to" = "name"),
      suffix = c("", "end")
    ) %>%
    dplyr::select(name, x, y, direction, to, xend, yend, dplyr::everything())
}

generate_layout <- function(.df, layout, vertices = NULL, coords = NULL, ...) {
  ig <- igraph::graph_from_data_frame(.df, vertices = vertices)

  if (is.null(coords)) {
    no_existing_coords <- TRUE
  } else {
    no_existing_coords <- coords %>%
      purrr::map_lgl(~ all(is.na(.x))) %>%
      all()
  }


  if (no_existing_coords) {
    ggraph_layout <- suppressMessages(ggraph::create_layout(
      ig,
      layout = layout,
      ...
    ))
  } else {
    ggraph_layout <- suppressMessages(ggraph::create_layout(
      ig,
      layout = "manual",
      x = coords$x,
      y = coords$y,
      ...
    ))
  }

  ggraph_layout %>%
    dplyr::select(name, x, y, circular) %>%
    dplyr::as_tibble()
}

check_verboten_layout <- function(layout) {
  if (layout %in% c("dendogram")) {
    stop("Layout type `", layout, "` not supported in ggdag", call. = FALSE)
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
  model %>%
    tidy_dagitty() %>%
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
as.data.frame.tidy_dagitty <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(pull_dag_data(x), row.names = row.names, optional = optional, ...)
}

#' Convert a `tidy_dagitty` object to tbl_df
#'
#' @param .tdy_dag an object of class `tidy_dagitty`
#'
#' @export
#' @importFrom dplyr tbl_df
tbl_df.tidy_daggity <- function(.tdy_dag) {
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
as.tbl.tidy_daggity <- function(x, row.names = NULL, optional = FALSE, ...) {
  dplyr::as.tbl(pull_dag_data(x), row.names = row.names, optional = optional, ...)
}

#' @export
#' @rdname as.tbl.tidy_daggity
as_tibble.tidy_daggity <- function(x, row.names = NULL, optional = FALSE, ...) {
  dplyr::as_tibble(pull_dag_data(x), row.names = row.names, optional = optional, ...)
}

#' Print a `tidy_dagitty`
#'
#' @param x an object of class `tidy_dagitty`
#' @param ... optional arguments passed to `print()`
#'
#' @export
print.tidy_dagitty <- function(x, ...) {
  cat_subtle <- function(...) cat(pillar::style_subtle(paste(...)))
  coll <- function(x, ...) paste(x, collapse = ", ", ...)

  cat_subtle("# A DAG with ", n_nodes(x), " nodes and ", n_edges(x), " edges\n", sep = "")
  cat_subtle("#\n")
  if (has_exposure(x)) cat_subtle("# Exposure: ", coll(dagitty::exposures(pull_dag(x))), "\n", sep = "")
  if (has_outcome(x)) cat_subtle("# Outcome: ", coll(dagitty::outcomes(pull_dag(x))), "\n", sep = "")
  if (has_latent(x)) cat_subtle("# Latent Variable: ", coll(dagitty::latents(pull_dag(x))), "\n", sep = "")
  if (has_collider_path(x)) {
    cat_subtle("# Paths opened by conditioning on a collider: ",
               coll(collider_paths(x)), "\n",
               sep = ""
    )
  }
  if (any(c(
    has_collider_path(x),
    has_exposure(x),
    has_outcome(x),
    has_latent(x)
  ))) {
    cat_subtle("#\n")
  }

  print(pull_dag_data(x), ...)
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
  coord_df <- purrr::map(coord_list, tibble::enframe) %>% purrr::reduce(ggdag_left_join, by = "name")
  names(coord_df) <- c("name", "x", "y")
  coord_df
}

#' @rdname coordinates
#' @export
coords2list <- function(coord_df) {
  x <- coord_df %>%
    dplyr::select(name, x) %>%
    tibble::deframe()
  y <- coord_df %>%
    dplyr::select(name, y) %>%
    tibble::deframe()
  list(x = x, y = y)
}
