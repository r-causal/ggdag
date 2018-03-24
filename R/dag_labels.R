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
#'  tidy_dagitty() %>%
#'  dag_label(labels = c("x" = "exposure", "y" = "outcome", "z" = "confounder"))
#'
#'  has_labels(labelled_dag)
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
  attr(x, "labels") <- value

  if (!is.null(x$data[["label"]])) x$data <- x$data %>% dplyr::select(-label)

  x$data <- dplyr::left_join(x$data, tibble::enframe(value, value = "label"), by = "name")
  x
}

#' @param labels a character vector
#'
#' @rdname label
#' @export
dag_label <- function(.tdy_dag, labels = NULL) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)
  if (!is.null(labels) & !is.null(.tdy_dag$data[["label"]])) .tdy_dag$data <- .tdy_dag$data %>% dplyr::select(-label)
  if (is.null(labels)) labels <- label(.tdy_dag$dag)
  if (is.null(labels)) { warning("no labels provided"); return(.tdy_dag) }

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
