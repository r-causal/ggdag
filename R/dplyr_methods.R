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
#' @name dplyr
#' @importFrom dplyr select
select.tidy_dagitty <- function(.data, ...) {
  .data$data <- dplyr::select(.data$data, ...)
  .data
}

#' @export
dplyr::filter

#' @name dplyr
#' @export
#' @importFrom dplyr filter
filter.tidy_dagitty <- function(.data, ...) {
  .data$data <- dplyr::filter(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr mutate
mutate.tidy_dagitty <- function(.data, ...) {
  .data$data <- dplyr::mutate(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr summarise
summarise.tidy_dagitty <- function(.data, ...) {
  dplyr::summarise(.data$data, ...)
}

#' @name dplyr
#' @export
#' @importFrom dplyr distinct
distinct.tidy_dagitty <- function(.data, ..., .keep_all = FALSE) {
  dplyr::distinct(.data$data, ..., .keep_all = .keep_all)
}

#' @name dplyr
#' @export
#' @importFrom dplyr arrange
arrange.tidy_dagitty <- function(.data, ...) {
  .data$data <- dplyr::arrange(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr group_by
group_by.tidy_dagitty <- function(.data, ...) {
  .data$data <- dplyr::group_by(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr ungroup
ungroup.tidy_dagitty <- function(.data, ...) {
  .data$data <- dplyr::ungroup(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr transmute
transmute.tidy_dagitty <- function(.data, ...) {
  .data$data <- dplyr::transmute(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr distinct
distinct.tidy_dagitty <- function(.data, ..., .keep_all = FALSE) {
  .data$data <- dplyr::distinct(.data$data, ..., .keep_all = .keep_all)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr full_join
full_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  x$data <- dplyr::full_join(x$data, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr inner_join
inner_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  x$data <- dplyr::inner_join(x$data, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr left_join
left_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  x$data <- dplyr::left_join(x$data, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr right_join
right_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  x$data <- dplyr::right_join(x$data, by = by, copy = copy, suffix = suffix, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr anti_join
anti_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, ...) {
  x$data <- dplyr::anti_join(x$data, by = by, copy = copy, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr semi_join
semi_join.tidy_dagitty <- function(x, y, by = NULL, copy = FALSE, ...) {
  x$data <- dplyr::semi_join(x$data, by = by, copy = copy, ...)
  x
}

#' @name dplyr
#' @export
#' @importFrom dplyr slice
slice.tidy_dagitty <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::slice(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr select_
select_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::select_(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr filter_
filter_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::filter_(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr mutate_
mutate_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::mutate_(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr summarise_
summarise_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::summarise_(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr arrange_
arrange_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::arrange_(.data$data, ...)
  .data
}

#' @name dplyr
#' @export
#' @importFrom dplyr slice_
slice_.tidy_dagitty <- function(.data, ..., .dots = list()) {
  .data$data <- dplyr::slice_(.data$data, ...)
  .data
}
