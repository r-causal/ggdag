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
#' tidy_dagitty_obj <- dagify(y ~ x + z, x ~ z) %>%
#'   tidy_dagitty()
#' dag <- pull_dag(tidy_dagitty_obj)
#' dag_data <- pull_dag_data(tidy_dagitty_obj)
#'
#' tidy_dagitty_obj %>%
#'   dplyr::mutate(name = toupper(name)) %>%
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
  x$data <- prep_dag_data(value)
  x
}

prep_dag_data <- function(value, layout = "nicely", coords = NULL, ...) {
  if (any(c("name", "to") %nin% names(value))) {
    stop("Columns `name` and `to` not found")
  }

  if (layout == "time_ordered") {
    coords <- value %>%
      edges2df() %>%
      auto_time_order() %>%
      time_ordered_coords() %>%
      coords2list()
  }

  if ("direction" %nin% names(value)) {
    value$direction <- "->"
  }

  if (any(c("x", "y", "xend", "yend") %nin% names(value))) {
    coords_df <- value %>%
      dplyr::select(name, to) %>%
      dplyr::filter(!is.na(name), !is.na(to)) %>%
      generate_layout(
        layout = layout,
        coords = coords,
        ...
      )

    value <- value %>%
      tidy_dag_edges_and_coords(coords_df)
  }

  # TODO: remove this when circular is changed
  if ("circular" %nin% names(value)) {
    value$circular <- FALSE
  }

  dplyr::as_tibble(value)
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
  update_dag(x) <- recompile_dag(x)
  x
}

#' @export
#' @rdname pull_dag
`update_dag<-.tidy_dagitty` <- function(x, value) {
  stopifnot(dagitty::is.dagitty(value))
  x$dag <- value
  x
}

recompile_dag <- function(.dag) {
  new_dag <- .dag %>%
    pull_dag_data() %>%
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
    .adjusted <- dplyr::filter(.dag, adjusted == "adjusted") %>%
      pull_dag_data() %>%
      dplyr::pull(name) %>%
      empty2list()
  } else {
    .adjusted <- dagitty::adjustedNodes(pull_dag(.dag))
  }

  dagitty::exposures(new_dag) <- .exposures
  dagitty::outcomes(new_dag) <- .outcomes
  dagitty::latents(new_dag) <- .latents

  dagitty::adjustedNodes(new_dag) <- .adjusted

  dagitty::coordinates(new_dag) <- .dag %>%
    pull_dag_data() %>%
    select(name, x, y) %>%
    coords2list()

  new_dag
}

compile_dag_from_df <- function(.df) {
  if ("direction" %nin% names(.df)) {
    .df$direction <- "<-"
  }

  .df %>%
    dplyr::filter(!is.na(to)) %>%
    dplyr::mutate(
      direction = as.character(direction),
      direction = ifelse(direction == "<-", "->", direction)
    ) %>%
    dplyr::group_by(to, direction) %>%
    dplyr::summarise(from_formula = paste("{", paste(name, collapse = " "), "}"), .groups = "drop") %>%
    dplyr::transmute(dag_formula = paste(to, direction, from_formula)) %>%
    dplyr::pull() %>%
    paste(collapse = "; ") %>%
    paste("dag {", ., "}") %>%
    dagitty::dagitty()
}

return_status <- function(.dag, .status) {
  if (is.tidy_dagitty(.dag)) .dag <- pull_dag_data(.dag)

  dplyr::filter(.dag, status == .status) %>%
    dplyr::pull(name) %>%
    empty2list()
}

empty2list <- function(.x) {
  if (purrr::is_empty(.x)) {
    list()
  } else {
    .x
  }
}

