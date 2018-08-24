#  variables used in various NSE calls
utils::globalVariables(
  c(
    ".",
    "x",
    "y",
    "xend",
    "yend",
    "adjusted",
    "collider_line",
    "collider",
    "name",
    "from",
    "to",
    "direction",
    "Var1",
    "Var2",
    "ancestor",
    "children",
    "circular",
    "collider_line",
    "colliders",
    "d_relationship",
    "descendant",
    "direction",
    "e",
    "exogenous",
    "from",
    "instrumental",
    "name",
    "parent",
    "reversable",
    "segment.colour",
    "status",
    "to",
    'type',
    "v",
    "w",
    ".ggraph.orig_index",
    ".ggraph.index",
    "from",
    ".from",
    ".to",
    "path",
    "paths",
    "set",
    "adjacent",
    "blanket",
    "collider_path_nodes"
  )
)

if_not_tidy_daggity <- function(.dagitty, ...) {
  if (!is.tidy_dagitty(.dagitty)) return(tidy_dagitty(.dagitty, ...))
  .dagitty
}

unique_pairs <- function(x, exclude_identical = TRUE) {
  pairs <- expand.grid(x, x) %>% purrr::map_dfc(as.character)
  if (exclude_identical) pairs <- pairs %>% dplyr::filter(Var1 != Var2)
  pairs[!duplicated(t(apply(pairs, 1, sort))), ]
}

formula2char <- function(fmla) {
  char_fmla <- as.character(fmla)
  rhs_vars <- char_fmla[[3]] %>% stringr::str_split(" \\+ ") %>% purrr::pluck(1)
  bidirectional <- any(stringr::str_detect(rhs_vars, "~"))
  rhs_vars <- stringr::str_replace_all(rhs_vars, "~", "")
  arrows <- ifelse(bidirectional, "<->", "<-")
  rhs_vars_coll <- paste0("{", paste(rhs_vars, collapse = " "), "}")
  paste(char_fmla[[2]], arrows, rhs_vars_coll)
}

edge_type_switch <- function(edge_type) {
  switch(edge_type, "link_arc" = geom_dag_edges,
         "link" = geom_dag_edges_link,
         "arc" = geom_dag_edges_arc,
         "diagonal" = geom_dag_edges_diagonal)
}

is_empty_or_null <- function(x) {
  is.null(x) || purrr::is_empty(x)
}

is_false <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && !x

has_exposure <- function(x) {
  is_false(purrr::is_empty(dagitty::exposures(x$dag)))
}

has_outcome <- function(x) {
  is_false(purrr::is_empty(dagitty::outcomes(x$dag)))
}

has_latent <- function(x) {
  is_false(purrr::is_empty(dagitty::latents(x$dag)))
}

has_collider_path <- function(x) {
  x <- if_not_tidy_daggity(x)
  suppressWarnings(is_false(is.null(x$data$collider_line)))
}

n_nodes <- function(x) {
  dplyr::n_distinct(x$data$name)
}

n_edges <- function(x) {
  sum(!is.na(x$data$direction)) - n_collder_paths(x)
}

n_collder_paths <- function(x) {
  if (has_collider_path(x)) n <- sum(x$data$collider_line) else n <- 0
  n
}

collider_paths <- function(x) {
  if (has_collider_path(x)) {
    paths <- x$data %>%
      dplyr::filter(collider_line) %>%
      dplyr::mutate(collider_path_nodes = paste(name, "<->", to)) %>%
      dplyr::pull(collider_path_nodes)
  } else {
    paths <- c()
  }
  paths
}

#' Generate expansion vector for scales.
#'
#' This is a convenience function for generating scale expansion vectors
#' for the `expand` argument of
#' `scale_*_continuous()` and
#' `scale_*_discrete()`.
#' The expansions vectors are used to add some space between
#' the data and the axes.
#'
#' @export
#' @param mult vector of multiplicative range expansion factors.
#'   If length 1, both the lower and upper limits of the scale
#'   are expanded outwards by `mult`. If length 2, the lower limit
#'   is expanded by `mult[1]` and the upper limit by `mult[2]`.
#' @param add vector of additive range expansion constants.
#'   If length 1, both the lower and upper limits of the scale
#'   are expanded outwards by `add` units. If length 2, the
#'   lower limit is expanded by `add[1]` and the upper
#'   limit by `add[2]`.
#' @examples
#' # No space below the bars but 10% above them
#' ggplot(mtcars) +
#'   geom_bar(aes(x = factor(cyl))) +
#'   scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
#'
#' # Add 2 units of space on the left and right of the data
#' ggplot(subset(diamonds, carat > 2), aes(cut, clarity)) +
#'   geom_jitter() +
#'   scale_x_discrete(expand = expand_scale(add = 2))
#'
#' # Reproduce the default range expansion used
#' # when the ‘expand’ argument is not specified
#' ggplot(subset(diamonds, carat > 2), aes(cut, price)) +
#'   geom_jitter() +
#'   scale_x_discrete(expand = expand_scale(add = .6)) +
#'   scale_y_continuous(expand = expand_scale(mult = .05))
expand_scale <- function(mult = 0, add = 0) {
  stopifnot(is.numeric(mult) && is.numeric(add))
  stopifnot((length(mult) %in% 1:2) && (length(add) %in% 1:2))

  mult <- rep(mult, length.out = 2)
  add <- rep(add, length.out = 2)
  c(mult[1], add[1], mult[2], add[2])
}

