#' Create a time-ordered coordinate data frame
#'
#' `time_ordered_coords()` is a helper function to create time-ordered DAGs.
#' Pass the results to the `coords` argument of `dagify()`. If `.vars` if not
#' specified, these coordinates will be determined automatically. If you want to
#' be specific, you can also use a list or data frame. The default is to assume
#' you want variables to go from left to right in order by time. Variables are
#' spread along the y-axis using a simple algorithm to stack them. You can also
#' work along the y-axis by setting `direction = "y"`.
#'
#' @param .vars A list of character vectors, where each vector represents a
#'   single time period. Alternatively, a data frame where the first column is
#'   the variable name and the second column is the time period.
#' @param time_points A vector of time points. Default is `NULL`, which creates
#'   a sequence from 1 to the number of variables.
#' @param direction A character string indicating the axis along which the
#'   variables should be time-ordered. Either "x" or "y". Default is "x".
#' @param auto_sort_direction If `.vars` is `NULL`: nodes will be placed as far
#'   `"left"` or `"right"` of in the graph as is reasonable. Default is right,
#'   meaning the nodes will be as close as possible in time to their
#'   descendants.
#' @param fixed_time A named numeric vector pinning specific nodes to time
#'   points (e.g., `c(x = 3, z = 1)`). Only used in auto mode (`.vars =
#'   NULL`). Other nodes are placed automatically while respecting these
#'   constraints. Pinned times are 1-based and preserved in the output.
#' @param adjust_exposure_outcome If `TRUE` (default), automatically shift the
#'   outcome forward by one time point when it shares a layer with the
#'   exposure. All descendants of the outcome are also shifted. Only applies in
#'   auto mode and when the DAG has exposure and outcome set.
#' @param force_y If `TRUE` (default), run force-directed Y optimization to
#'   minimize node-edge overlaps. If `FALSE`, nodes are evenly spaced within
#'   each layer using barycenter ordering only. Setting to `FALSE` is useful
#'   when edges will be curved or auto-routed, where tight Y positioning is
#'   less important. Only used in auto mode (`.vars = NULL`).
#'
#' @return A tibble with three columns: `name`, `x`, and `y`.
#'
#' @examples
#'
#' dagify(
#'   d ~ c1 + c2 + c3,
#'   c1 ~ b1 + b2,
#'   c3 ~ a,
#'   b1 ~ a,
#'   coords = time_ordered_coords()
#' ) |> ggdag()
#'
#' coords <- time_ordered_coords(list(
#'   # time point 1
#'   "a",
#'   # time point 2
#'   c("b1", "b2"),
#'   # time point 3
#'   c("c1", "c2", "c3"),
#'   # time point 4
#'   "d"
#' ))
#'
#' dagify(
#'   d ~ c1 + c2 + c3,
#'   c1 ~ b1 + b2,
#'   c3 ~ a,
#'   b1 ~ a,
#'   coords = coords
#' ) |> ggdag()
#'
#' # or use a data frame
#' x <- data.frame(
#'   name = c("x1", "x2", "y", "z1", "z2", "z3", "a"),
#'   time = c(1, 1, 2, 3, 3, 3, 4)
#' )
#' dagify(
#'   z3 ~ y,
#'   y ~ x1 + x2,
#'   a ~ z1 + z2 + z3,
#'   coords = time_ordered_coords(x)
#' ) |>
#'   ggdag()
#'
#' @export
#' @seealso [dagify()], [coords2df()], [coords2list()]
time_ordered_coords <- function(
  .vars = NULL,
  time_points = NULL,
  direction = c("x", "y"),
  auto_sort_direction = c("right", "left"),
  fixed_time = NULL,
  adjust_exposure_outcome = TRUE,
  force_y = TRUE
) {
  direction <- match.arg(direction)
  auto_sort_direction <- match.arg(auto_sort_direction)

  if (is.null(.vars)) {
    auto_time_ordered_coords <- function(.df, ...) {
      compute_time_ordered_layout(
        .df,
        direction = direction,
        sort_direction = auto_sort_direction,
        fixed_time = fixed_time,
        adjust_exposure_outcome = adjust_exposure_outcome,
        force_y = force_y,
        ...
      )
    }

    return(auto_time_ordered_coords)
  }

  if (is.data.frame(.vars)) {
    stopifnot(ncol(.vars) >= 2)
    time_points <- sort(unique(.vars[[2]]))
    .vars <- split(.vars[[1]], .vars[[2]])
  }

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
  if (n == 0) {
    return(numeric(0))
  }

  spread <- seq(-floor(n / 2), ceiling(n / 2) - 1)
  if (n %% 2 == 0) {
    spread[spread >= 0] <- spread[spread >= 0] + 1
  }

  spread
}
