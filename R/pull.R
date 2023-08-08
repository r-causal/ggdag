#' Pull components from DAG objects
#'
#' `pull_dag()` and `pull_dag_data()` are generic methods to pull components of
#' DAG objects, e.g. `tidy_dagitty`, such as the `dagitty` object or the data
#' frame associated with it. These methods are recommended over extracting
#' components manually, e.g. `my_dag$data`, because the internal structure of
#' these objects may change over time.
#'
#' @param x a `tidy_dagitty` or `dagitty` object.
#' @param ... For `dagitty` objects, passed to `tidy_dagitty()` if needed,
#'   otherwise currently unused.
#'
#' @return a DAG object, e.g. `dagitty`, or data frame
#'
#' @examples
#'
#' tidy_dagitty_obj <- dagify(y ~ x + z, x ~ z)
#' dag <- pull_dag(tidy_dagitty_obj)
#' dag_data <- pull_dag_data(tidy_dagitty_obj)
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

