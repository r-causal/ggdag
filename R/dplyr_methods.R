#' @export
#' @importFrom dplyr select
select.tidy_daggity <- function(.data, ...) {
  .data$data <- dplyr::select(.data$data, ...)
  .data
}

# Required to export filter, otherwise:
# Warning: declared S3 method 'filter.tbl_time' not found
# because of stats::filter

#' @export
#'
dplyr::filter

#' @export
#' @importFrom dplyr filter
filter.tidy_daggity <- function(.data, ...) {
  .data$data <- dplyr::filter(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr mutate
mutate.tidy_daggity <- function(.data, ...) {
  .data$data <- dplyr::mutate(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr summarise
summarise.tidy_daggity <- function(.data, ...) {
  .data$data <- dplyr::summarise(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr arrange
arrange.tidy_daggity <- function(.data, ...) {
  .data$data <- dplyr::arrange(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr group_by
group_by.tidy_daggity <- function(.data, ...) {
  .data$data <- dplyr::group_by(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr ungroup
ungroup.tidy_daggity <- function(.data, ...) {
  .data$data <- dplyr::ungroup(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr transmute
transmute.tidy_daggity <- function(.data, ...) {
  .data$data <- dplyr::transmute(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr distinct
distinct.tidy_daggity <- function(.data, ..., .keep_all = FALSE) {
  .data$data <- dplyr::distinct(.data$data, ..., .keep_all = .keep_all)
  .data
}

#' @export
#' @importFrom dplyr full_join
full_join.tidy_daggity <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  x$data <- dplyr::full_join(x$data, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @export
#' @importFrom dplyr inner_join
inner_join.tidy_daggity <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  x$data <- dplyr::inner_join(x$data, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @export
#' @importFrom dplyr left_join
left_join.tidy_daggity <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  x$data <- dplyr::left_join(x$data, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @export
#' @importFrom dplyr right_join
right_join.tidy_daggity <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  x$data <- dplyr::right_join(x$data, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @export
#' @importFrom dplyr anti_join
anti_join.tidy_daggity <- function(x, y, by = NULL, copy = FALSE, ...) {
  x$data <- dplyr::anti_join(x$data, by = by, copy = copy, ...)
  x
}

#' @export
#' @importFrom dplyr semi_join
semi_join.tidy_daggity <- function(x, y, by = NULL, copy = FALSE, ...) {
  x$data <- dplyr::semi_join(x$data, by = by, copy = copy, ...)
  x
}

#' @export
#' @importFrom dplyr slice
slice.tidy_daggity <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::slice(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr select_
select_.tidy_daggity <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::select_(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr filter_
filter_.tidy_daggity <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::filter_(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr mutate_
mutate_.tidy_daggity <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::mutate_(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr summarise_
summarise_.tidy_daggity <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::summarise_(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr arrange_
arrange_.tidy_daggity <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::arrange_(.data$data, ...)
  .data
}

#' @export
#' @importFrom dplyr slice_
slice_.tidy_daggity <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::slice_(.data$data, ...)
  .data
}
