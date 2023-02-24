
#' Create a time-ordered coordinate data frame
#'
#' `time_ordered_coords()` is a helper function to create time-ordered DAGs.
#' Pass the results to the `coords` argument of `dagify()`. The default is to
#' assume you want variables to go from left to right in order by time.
#' Variables are spread along the y-axis using a simple algorithm to stack
#' them. You can also work along the y-axis by setting `direction = "y"`.
#'
#' @param ... Character vectors, where each vector represents a single time
#'   period.
#' @param time_points A vector of time points. Default is `NULL`, which creates
#'   a sequence from 1 to the number of variables.
#' @param direction A character string indicating the axis along which the
#'   variables should be time-ordered. Either "x" or "y". Default is "x".
#'
#' @return A tibble with three columns: `name`, `x`, and `y`.
#'
#' @examples
#' coords <- time_ordered_coords(
#'   # time point 1
#'   "a",
#'   # time point 2
#'   c("b1", "b2"),
#'   # time point 3
#'   c("c1", "c2", "c3"),
#'   # time point 4
#'   "d"
#' )
#'
#' dagify(
#'   d ~ c1 + c2 + c3,
#'   c1 ~ b1 + b2,
#'   c3 ~ a,
#'   b1 ~ a,
#'   coords = coords
#' ) %>% ggdag()
#'
#' @export
#' @seealso [dagify()], [coords2df()], [coords2list()]
time_ordered_coords <- function(..., time_points = NULL, direction = c("x", "y")) {
  .vars <- list(...)
  direction <- match.arg(direction)
  purrr::map2_dfr(
    time_points %||% seq_along(.vars),
    .vars,
    spread_coords,
    direction = direction
  )
}
time_ordered_coords <- function(..., time_points = NULL, direction = c("x", "y")) {
  .vars <- list(...)
  direction <- match.arg(direction)
  purrr::map2_dfr(
    time_points %||% seq_along(.vars),
    .vars,
    spread_coords,
    direction = direction
  )
}

spread_coords <- function(.time, .vars, direction) {
  n <- length(.vars)
  if (direction == "x") {
    tibble::tibble(
      name = .vars,
      x = .time,
      y = calculate_spread(n)
    )
  } else {
    tibble::tibble(
      name = .vars,
      x = calculate_spread(n),
      y = .time
    )
  }
}

calculate_spread <- function(n) {
  spread <- seq(-floor(n / 2), ceiling(n / 2) - 1)
  if (n %% 2 == 0) {
    spread[spread >= 0] <- spread[spread >= 0] + 1
  }

  spread
}
