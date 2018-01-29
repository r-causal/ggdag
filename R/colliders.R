
#' Title
#'
#' @param .tdy_dag
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_collider <- function(.tdy_dag, as_factor = TRUE) {
  vars <- unique(.tdy_dag$data$name)
  colliders <- purrr::map_lgl(vars, ~is_collider(.tdy_dag, .x))
  names(colliders) <- vars
  .tdy_dag$data <- dplyr::left_join(.tdy_dag$data,
                                    tibble::enframe(colliders, value = "colliders"), by = "name")
  purrr::map(vars[colliders], ~dagitty::parents(.tdy_dag$dag, .x))
  if (as_factor) .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, colliders = factor(as.numeric(colliders), levels = 0:1, labels = c("Non-Collider", "Collider")))
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_collider <- function(.tdy_dag, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_collider(...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = forcats::fct_rev(colliders))) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag()
}

#' Title
#'
#' @param .tdy_dag
#' @param adjust_for
#'
#' @return
#' @export
#'
#' @examples
activate_collider_paths <- function(.tdy_dag, adjust_for) {
  vars <- unique(.tdy_dag$data$name)
  colliders <- purrr::map_lgl(vars, ~is_collider(.tdy_dag, .x))
  downstream_colliders <- purrr::map_lgl(vars, ~is_downstream_collider(.tdy_dag, .x))
  collider_names <- unique(c(vars[colliders], vars[downstream_colliders]))

  if (!any((collider_names %in% adjust_for))) return(.tdy_dag)
  adjusted_colliders <- collider_names[collider_names %in% adjust_for]
  collider_paths <- purrr::map(adjusted_colliders, ~dagitty::ancestors(.tdy_dag$dag, .x)[-1])

  activated_pairs <- purrr::map(collider_paths, unique_pairs)

  collider_lines <- purrr::map_df(activated_pairs, function(.pairs_df) {
    .pairs_df %>% dplyr::rowwise() %>% dplyr::do({
      df <- .
      name <- df[["Var1"]]
      to <- df[["Var2"]]
      start_coords <- .tdy_dag$data %>% dplyr::filter(name == df[["Var1"]]) %>% dplyr::select(x, y) %>% dplyr::slice(1)
      end_coords <- .tdy_dag$data %>% dplyr::filter(name == df[["Var2"]]) %>% dplyr::select(x, y) %>% dplyr::slice(1)

      .tdy_dag$data %>% dplyr::add_row(.before = 1, name = name, from = name, x = start_coords[[1, 1]], y = start_coords[[1, 2]],
                                       to = to, xend = end_coords[[1, 1]], yend = end_coords[[1, 2]],
                                       direction = factor("<->", levels = c("<-", "->", "<->"), exclude = NA),
                                       type = factor("bidirected", levels = c("directed", "bidirected"), exclude = NA)) %>% dplyr::slice(1)
    })
  })
  collider_lines$collider_line <- TRUE
  .tdy_dag$data$collider_line <- FALSE
  .tdy_dag$data <- dplyr::bind_rows(.tdy_dag$data, collider_lines)
  .tdy_dag
}

#' Title
#'
#' @param .dag
#' @param .var
#'
#' @return
#' @export
#'
#' @examples
is_collider <- function(.dag, .var) {
  if (is.tidy_dagitty(.dag)) .dag <- .dag$dag
  n_parents <- dagitty::parents(.dag, .var)
  length(n_parents) > 1
}

#' Title
#'
#' @param .dag
#' @param .var
#'
#' @return
#' @export
#'
#' @examples
is_downstream_collider <- function(.dag, .var) {
  if (is.tidy_dagitty(.dag)) .dag <- .dag$dag
  var_ancestors <- dagitty::ancestors(.dag, .var)[-1]
  any(purrr::map_lgl(var_ancestors, ~is_collider(.dag, .x)))
}
