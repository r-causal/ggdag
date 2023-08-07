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
#' ) %>% ggdag()
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
#' ) %>% ggdag()
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
#' ) %>%
#'   ggdag()
#'
#' @export
#' @seealso [dagify()], [coords2df()], [coords2list()]
time_ordered_coords <- function(.vars = NULL, time_points = NULL, direction = c("x", "y"), auto_sort_direction = c("right", "left")) {
  direction <- match.arg(direction)

  if (is.null(.vars)) {
    auto_time_ordered_coords <- function(.df) {
      .df <- auto_time_order(.df, sort_direction = auto_sort_direction)
      time_ordered_coords(.df, direction = direction)
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
  spread <- seq(-floor(n / 2), ceiling(n / 2) - 1)
  if (n %% 2 == 0) {
    spread[spread >= 0] <- spread[spread >= 0] + 1
  }

  spread
}

auto_time_order <- function(graph, sort_direction = c("right", "left")) {
  sort_direction <- match.arg(sort_direction)
  names(graph)[1:2] <- c("name", "to")
  graph2 <- graph
  orders <- dplyr::tibble(name = character(), order = integer())

  order_value <- 1

  while (nrow(graph) > 0) {
    no_incoming <- graph %>%
      dplyr::filter(!(name %in% to)) %>%
      dplyr::pull(name)

    # Add the names and order values to the orders data frame
    orders <- dplyr::add_row(orders, name = no_incoming, order = order_value)

    # Remove the rows with no incoming edges
    graph <- graph %>%
      dplyr::filter(!name %in% no_incoming)

    order_value <- order_value + 1
  }

  # Merge orders with the original tibble
  final_result <- dplyr::left_join(orders, graph, by = "name") %>%
    dplyr::select(name, order) %>%
    dplyr::distinct()

  if (sort_direction == "left") {
    return(final_result)
  }

  final_result %>%
    ggdag_left_join(graph2, by = "name") %>%
    dplyr::group_by(name) %>%
    dplyr::group_modify(~ right_sort_coords(.x, final_result)) %>%
    dplyr::ungroup()
}

right_sort_coords <- function(.x, .orders) {
  coords <- .orders %>%
    dplyr::filter(name %in% .x$to) %>%
    dplyr::pull(order)

  if (length(coords) == 0) {
    dplyr::tibble(order = .x$order)
  } else {
    dplyr::tibble(order = min(coords) - 1)
  }
}

