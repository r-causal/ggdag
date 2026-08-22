#' DAG labels
#'
#' Label or otherwise retrieve labels from objects of either class
#' `tidy_dagitty` or `dagitty`
#'
#' @param x an object of either class `tidy_dagitty` or `dagitty`
#' @param value a named character vector, where the names are node names
#' @inheritParams dag_params
#'
#' @return `label` returns the label attribute of x
#' @export
#'
#' @examples
#' labelled_dag <- dagify(y ~ z, x ~ z) |>
#'   tidy_dagitty() |>
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
  validate_labels(value)
  attr(x, "labels") <- value
  x
}

#' @rdname label
#' @export
`label<-.tidy_dagitty` <- function(x, value) {
  validate_labels(value)
  attr(x$dag, "labels") <- value

  if (!is.null(pull_dag_data(x)[["label"]])) {
    x <- dplyr::select(x, -label)
  }

  if (is.null(value)) {
    return(x)
  }

  dplyr::left_join(
    x,
    tibble::enframe(value, value = "label"),
    by = "name"
  )
}

#' Check that labels are usable
#'
#' Labels are joined to the DAG data by node name, so every label must carry the
#' name of the node it belongs to, and no node may be named twice. `NULL` is
#' allowed and clears any labels.
#'
#' @param value The labels being assigned.
#' @param arg The argument name to report.
#' @param call The calling environment, for the error message.
#' @return `value`, invisibly.
#' @noRd
validate_labels <- function(
  value,
  arg = "labels",
  call = rlang::caller_env()
) {
  if (is.null(value)) {
    return(invisible(value))
  }

  node_names <- names(value)
  is_fully_named <- is.character(value) &&
    length(value) > 0 &&
    !is.null(node_names) &&
    !anyNA(node_names) &&
    all(node_names != "")

  if (!is_fully_named) {
    abort(
      c(
        "{.arg {arg}} must be a named character vector.",
        "x" = "Each label must be named for the node it belongs to.",
        "i" = 'For example: {.code c(x = "Exposure", y = "Outcome")}.'
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  duplicate_names <- unique(node_names[duplicated(node_names)])
  if (length(duplicate_names) > 0) {
    abort(
      c(
        "{.arg {arg}} must name each node at most once.",
        "x" = "Duplicated node name{?s}: {.val {duplicate_names}}.",
        "i" = "Labels are joined to the DAG data by name, so a repeated name
               would give a node more than one row."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  invisible(value)
}

#' @param labels a named character vector, where the names are node names
#'
#' @rdname label
#' @export
dag_label <- function(.tdy_dag, labels = NULL) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)
  if (!is.null(labels) && !is.null(pull_dag_data(.tdy_dag)[["label"]])) {
    .tdy_dag <- dplyr::select(.tdy_dag, -label)
  }
  if (is.null(labels)) {
    labels <- label(pull_dag(.tdy_dag))
  }
  if (is.null(labels)) {
    warn("No labels provided")
    return(.tdy_dag)
  }

  label(.tdy_dag) <- labels

  .tdy_dag
}

#' @rdname label
#' @export
label <- function(.tdy_dag) {
  attr(labelled_component(.tdy_dag), "labels")
}

#' @rdname label
#' @export
has_labels <- function(.tdy_dag) {
  !is.null(label(.tdy_dag))
}

#' Label a DAG only when there is something to label with
#'
#' `c()` drops `NULL` arguments but keeps zero-length vectors, dropping their
#' names in the process. A caller whose label lookup came back empty therefore
#' produces an unnamed, zero-length vector, which carries no information and
#' must be treated like no labels at all.
#'
#' @param .dag A `dagitty` object.
#' @param labels A character vector of labels, possibly empty.
#' @return `.dag`, labelled if `labels` has any elements.
#' @noRd
set_node_labels <- function(.dag, labels) {
  if (length(labels) > 0) {
    label(.dag) <- labels
  }

  .dag
}

# labels live on the `dagitty` component, so look there for tidy DAGs
labelled_component <- function(.tdy_dag) {
  if (is.tidy_dagitty(.tdy_dag)) {
    pull_dag(.tdy_dag)
  } else {
    .tdy_dag
  }
}
