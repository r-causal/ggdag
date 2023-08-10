#' DAG labels
#'
#' Label or otherwise retrieve labels from objects of either class
#' `tidy_dagitty` or `dagitty`
#'
#' @param x an object of either class `tidy_dagitty` or `dagitty`
#' @param value a character vector
#' @param .tdy_dag an object of class `tidy_dagitty`
#'
#' @return `label` returns the label attribute of x
#' @export
#'
#' @examples
#' labelled_dag <- dagify(y ~ z, x ~ z) %>%
#'   tidy_dagitty() %>%
#'   dag_label(labels = c("x" = "exposure", "y" = "outcome", "z" = "confounder"))
#'
#' has_labels(labelled_dag)
#' @rdname label
#' @name DAG Labels
`label<-` <- function(x, value) {
  UseMethod("label<-")
}

#' @rdname label
#' @export
`label<-.dagitty` <- function(x, value) {
  attr(x, "labels") <- value
  x
}

#' @rdname label
#' @export
`label<-.tidy_dagitty` <- function(x, value) {
  attr(x$dag, "labels") <- value

  if (!is.null(pull_dag_data(x)[["label"]])) {
    x <- dplyr::select(x, -label)
  }

  dplyr::left_join(
    x,
    tibble::enframe(value, value = "label"),
    by = "name"
  )
}

#' @param labels a character vector
#'
#' @rdname label
#' @export
dag_label <- function(.tdy_dag, labels = NULL) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)
  if (!is.null(labels) & !is.null(pull_dag_data(.tdy_dag)[["label"]])) {
    .tdy_dag <- dplyr::select(.tdy_dag, -label)
  }
  if (is.null(labels)) labels <- label(pull_dag(.tdy_dag))
  if (is.null(labels)) {
    warning("no labels provided")
    return(.tdy_dag)
  }

  label(.tdy_dag) <- labels

  .tdy_dag
}

#' @rdname label
#' @export
label <- function(.tdy_dag) {
  attr(.tdy_dag, "labels")
}

#' @rdname label
#' @export
has_labels <- function(.tdy_dag) {
  !is.null(attr(.tdy_dag, "labels"))
}
