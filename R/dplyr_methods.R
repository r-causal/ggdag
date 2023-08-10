## dplyr methods:

#' Dplyr verb methods for `tidy_dagitty` objects
#'
#' Dplyr verb methods for `tidy_dagitty` objects.
#' @param .data data object of class `tidy_dagitty`
#' @param .dots,x,y,by,copy,suffix,.keep_all see corresponding function in
#'   package `dplyr`
#' @param ... other arguments passed to the `dplyr` function
#'
#' @name dplyr
#' @examples
#' library(dplyr)
#' tidy_dagitty(m_bias()) %>%
#'   group_by(name) %>%
#'   summarize(n = n())
#' @export
#' @importFrom dplyr select
select.tidy_dagitty <- function(.data, ...) {
  update_dag_data(.data) <- dplyr::select(pull_dag_data(.data), ...)
  .data
}

#' @export
dplyr::filter

#' @name dplyr
#' @export
#' @importFrom dplyr filter
filter.tidy_dagitty <- function(.data, ...) {
  update_dag_data(.data) <- dplyr::filter(pull_dag_data(.data), ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr mutate
mutate.tidy_dagitty <- function(.data, ...) {
  update_dag_data(.data) <- dplyr::mutate(pull_dag_data(.data), ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr summarise
summarise.tidy_dagitty <- function(.data, ...) {
  dplyr::summarise(pull_dag_data(.data), ...)
}

#' @name dplyr
#' @export
#' @importFrom dplyr distinct
distinct.tidy_dagitty <- function(.data, ..., .keep_all = FALSE) {
  dplyr::distinct(pull_dag_data(.data), ..., .keep_all = .keep_all)
}

#' @name dplyr
#' @export
#' @importFrom dplyr arrange
arrange.tidy_dagitty <- function(.data, ...) {
  update_dag_data(.data) <- dplyr::arrange(pull_dag_data(.data), ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr group_by
group_by.tidy_dagitty <- function(.data, ...) {
  .data$data <- dplyr::group_by(pull_dag_data(.data), ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr ungroup
ungroup.tidy_dagitty <- function(x, ...) {
  update_dag_data(x) <- dplyr::ungroup(pull_dag_data(x), ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr transmute
transmute.tidy_dagitty <- function(.data, ...) {
  update_dag_data(.data) <- dplyr::transmute(pull_dag_data(.data), ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr distinct
distinct.tidy_dagitty <- function(.data, ..., .keep_all = FALSE) {
  update_dag_data(.data) <- dplyr::distinct(pull_dag_data(.data), ..., .keep_all = .keep_all)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr full_join
full_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  update_dag_data(x) <- dplyr::full_join(pull_dag_data(x), y, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr inner_join
inner_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  update_dag_data(x) <- dplyr::inner_join(pull_dag_data(x), y, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr left_join
left_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  update_dag_data(x) <- ggdag_left_join(pull_dag_data(x), y, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr right_join
right_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  update_dag_data(x) <- dplyr::right_join(pull_dag_data(x), y, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr anti_join
anti_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, ...) {
  update_dag_data(x) <- dplyr::anti_join(pull_dag_data(x), y, by = by, copy = copy, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr semi_join
semi_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, ...) {
  update_dag_data(x) <- dplyr::semi_join(pull_dag_data(x), y, by = by, copy = copy, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr slice
slice.tidy_dagitty <- function(.data, ..., .dots = list()) {
  update_dag_data(.data) <- dplyr::slice(pull_dag_data(.data), ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr select_
select_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  update_dag_data(.data) <- dplyr::select_(pull_dag_data(.data), ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr filter_
filter_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  update_dag_data(.data) <- dplyr::filter_(pull_dag_data(.data), ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr mutate_
mutate_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  update_dag_data(.data) <- dplyr::mutate_(pull_dag_data(.data), ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr summarise_
summarise_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  update_dag_data(.data) <- dplyr::summarise_(pull_dag_data(.data), ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr arrange_
arrange_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  update_dag_data(.data) <- dplyr::arrange_(pull_dag_data(.data), ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr slice_
slice_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  update_dag_data(.data) <- dplyr::slice_(pull_dag_data(.data), ...)
  .data
}
