#' Title
#'
#' @param .tdy_dag
#' @param exposure
#' @param outcome
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom utils capture.output
dag_adjustment_sets <- function(.tdy_dag, exposure = NULL, outcome = NULL, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)
  sets <- dagitty::adjustmentSets(.tdy_dag$dag, exposure = exposure, outcome = outcome, ...)
  is_empty_set <- purrr::is_empty(sets)
  if (is_empty_set) stop("dagitty failed to process sets. Check that it is a DAG with `is_acyclic()`")
  sets <- sets %>%
    capture.output() %>%
    stringr::str_replace(" \\{\\}", "(Unconditionally Independent)") %>%
    stringr::str_replace("\\{ ", "") %>%
    stringr::str_replace(" \\}", "") %>%
    stringr::str_trim() %>%
    purrr::map(~stringr::str_split(.x, ", ") %>%
                 purrr::pluck(1))


  .tdy_dag$data <-
    purrr::map_df(sets,
                  ~dplyr::mutate(.tdy_dag$data, adjusted = ifelse(name %in% .x, "adjusted", "unadjusted"), set = paste(.x, collapse = ", "))
    )

  .tdy_dag
}


#' Title
#'
#' @param .tdy_dag
#' @param exposure
#' @param outcome
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_adjustment_set <- function(.tdy_dag, exposure = NULL, outcome = NULL, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    dag_adjustment_sets(exposure = exposure, outcome = outcome, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = adjusted)) +
    geom_dag_edges(ggplot2::aes(edge_alpha = adjusted)) +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    ggplot2::facet_wrap(~set) +
    theme_dag() +
    scale_dag(expand_x = expand_scale(c(0.25, 0.25)))
}

#' Title
#'
#' @param g
#' @param z
#' @param x
#' @param y
#' @param direct
#'
#' @return
#' @export
#'
#' @examples
is_confounder <- function(g, z, x, y, direct = FALSE) {
  if (direct) {
    z_descendants <- dagitty::children(g, z)
  } else {
    z_descendants <- dagitty::descendants(g, z)[-1]
  }
  all(c(x, y) %in% z_descendants)
}

#' Title
#'
#' @param .tdy_dag
#' @param adjust_for
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
control_for <- function(.tdy_dag, adjust_for, as_factor = TRUE) {
  .tdy_dag <- activate_collider_paths(.tdy_dag, adjust_for)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, adjusted = ifelse(name %in% adjust_for, "adjusted", "unadjusted"))
  if (as_factor) .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, adjusted = factor(adjusted, exclude = NA))
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param adjust_for
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_adjust <- function(.tdy_dag, adjust_for, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    control_for(adjust_for, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted)) +
    geom_dag_edges(ggplot2::aes(edge_alpha = adjusted)) +
    geom_dag_collider_edges() +
    geom_dag_node() +
    geom_dag_text() +
    theme_dag() +
    scale_dag()
}
