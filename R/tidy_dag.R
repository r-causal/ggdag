#' Tidy a \code{dagitty} object
#'
#' @param .dagitty a \code{dagitty}
#' @param seed a numeric seed for reproducible layout generation
#' @param layout a layout available in \code{ggraph}. See \code{ggraph::\link[ggraph]{create_layout}} for details.
#' @param ... optional arguments passed to \code{ggraph::create_layout()}
#'
#' @return a \code{tidy_dagitty} object
#' @export
#'
#' @examples
#' library(dagitty)
#' library(ggplot2)
#'
#' dag <- dagitty( "dag {
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
#'     geom_dag_node() +
#'     geom_dag_text() +
#'     geom_dag_edges() +
#'     theme_dag() +
#'     scale_dag()
tidy_dagitty <- function(.dagitty, seed = NULL, layout = "nicely", ...) {
  if (!is.null(seed)) set.seed(seed)

  if (dagitty::graphType(.dagitty) != "dag") stop("`.dagitty` must be of graph type `dag`")
  .dag <- .dagitty

  no_existing_coords <- dagitty::coordinates(.dagitty) %>%
    purrr::map_lgl(~all(is.na(.x))) %>%
    all()

  ggraph_layout <- dagitty::edges(.dagitty) %>%
    dplyr::select(v, w) %>%
    igraph::graph_from_data_frame() %>%
    {suppressMessages(ggraph::create_layout(., layout, ...))}

  if (no_existing_coords) {
    coords <- coords2list(ggraph_layout)
  } else {
    coords <- dagitty::coordinates(.dagitty)
  }

  labels <- names(coords$x)

  dag_edges <- dagitty::edges(.dagitty)

  tidy_dag <- dplyr::left_join(tibble::enframe(coords$x, value = "x"),
                        tibble::enframe(coords$y, value = "y"),
                        by = "name")

  tidy_dag <- dag_edges %>%
    dplyr::select(-x, -y) %>%
    dplyr::mutate(v = as.character(v),
           w = as.character(w),
           direction = factor(e, levels = c("<-", "->", "<->"), exclude = NA),
           type = ifelse(e == "<->", "bidirected", "directed"),
           type = factor(type, levels = c("directed", "bidirected"), exclude = NA)) %>%
    dplyr::left_join(tidy_dag, ., by = c("name" = "v")) %>%
    dplyr::left_join(tidy_dag, by = c("w" = "name"),  suffix = c("", "end")) %>%
    dplyr::mutate(from = name) %>%
    dplyr::select(name, from, x, y, direction, type, to = w, xend, yend) %>%
    dplyr::left_join(dplyr::select(ggraph_layout, -x, -y), by = "name") %>%
    {warning(print(.))}
    #dplyr::arrange(.ggraph.orig_index) %>%


  .tdy_dag <- list(data = tidy_dag, dag = .dag)
  class(.tdy_dag) <- "tidy_dagitty"
  if (has_labels(.dag)) {
    label(.tdy_dag) <- label(.dag)
  }

    .tdy_dag
}

#' Test for object class for tidy_dagitty
#'
#' @param x object to be tested
#' @export
#'
#' @importFrom methods is
is.tidy_dagitty <- function(x) {
  is(x, "tidy_dagitty")
}

#' Fortify a \code{tidy_dagitty} object for \code{ggplot2}
#'
#' @param model an object of class \code{tidy_dagitty} or \code{dagitty}
#' @param data (not used)
#' @param ... (not used)
#'
#' @export
#' @importFrom ggplot2 fortify
#'
#' @rdname fortify
#' @name fortify
fortify.tidy_dagitty <- function(model, data = NULL, ...) {
  model$data
}

#' @rdname fortify
#' @export
fortify.dagitty <- function(model, data = NULL, ...) {
  model %>% tidy_dagitty() %>% .$data
}

#' Convert a \code{tidy_dagitty} object to data.frame
#'
#' @param x an object of class \code{tidy_dagitty}
#' @param row.names NULL or a character vector giving the row names for the data
#'   frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column
#'   names (to syntactic names: see make.names) is optional. Note that all of
#'   R's base package \code{as.data.frame()} methods use optional only for column names
#'   treatment, basically with the meaning of \code{data.frame(*, check.names =
#'   !optional)}
#' #' @param ... optional arguments passed to \code{as.data.frame()}
#'
#' @export
as.data.frame.tidy_dagitty <- function(x, row.names, optional, ...) {
  as.data.frame(x$data, row.names, optional, ...)
}

#' Convert a \code{tidy_dagitty} object to tbl_df
#'
#' @param .tdy_dag an object of class \code{tidy_dagitty}
#'
#' @export
#' @importFrom dplyr tbl_df
tbl_df.tidy_daggity <- function(.tdy_dag) {
  .tdy_dag$data
}

#' Convert a \code{tidy_dagitty} object to tbl
#'
#' @param x an object of class \code{tidy_dagitty}
#' @param row.names NULL or a character vector giving the row names for the data
#'   frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column
#'   names (to syntactic names: see make.names) is optional. Note that all of
#'   R's base package \code{as.data.frame()} methods use optional only for column names
#'   treatment, basically with the meaning of \code{data.frame(*, check.names =
#'   !optional)}
#' #' @param ... optional arguments passed to \code{as.tbl()}
#'
#' @export
#' @importFrom dplyr as.tbl
as.tbl.tidy_daggity <- function(x, row.names = NULL, optional = FALSE, ...) {
  dplyr::as.tbl(x$data, row.names = row.names, optional = optional, ...)
}

#' Print a \code{tidy_dagitty}
#'
#' @param x an object of class \code{tidy_dagitty}
#' @param ... optional arguments passed to \code{print()}
#'
#' @export
print.tidy_dagitty <- function(x, ...) {
  print(x$data, ...)
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
#' @return
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
#' x <- dagitty('dag{
#'              G <-> H <-> I <-> G
#'              D <- B -> C -> I <- F <- B <- A
#'              H <- E <- C -> G <- D
#'              }')
#' coordinates(x) <- coords2list(coord_df)
#'
#' @rdname coordinates
#' @name coordinates
coords2df <- function(coord_list) {
  coord_df <- purrr::map(coord_list, tibble::enframe) %>% purrr::reduce(dplyr::left_join, by = "name")
  names(coord_df) <- c("name", "x", "y")
  coord_df
}

#' @rdname coordinates
#' @export
coords2list <- function(coord_df) {
  x <- coord_df %>% dplyr::select(name, x) %>% tibble::deframe()
  y <- coord_df %>% dplyr::select(name, y) %>% tibble::deframe()
  list(x = x, y = y)
}
