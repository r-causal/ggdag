#' Saturate or prune an existing DAG
#'
#' `dag_saturate()` takes a tidy DAG object and, optionally using existing
#' coordinates, saturates the DAG based on time ordering of the nodes. To create
#' a saturated DAG from scratch, see [`as_tidy_dagitty.list()`]. `dag_prune()`
#' takes an existing DAG and removes edges. This is most useful when used
#' together with saturated DAG.
#'
#' @param .tdy_dag A tidy DAG object.
#' @param use_existing_coords Logical, indicating whether to use existing node
#'   coordinates.
#' @param edges A named character vector where the name is the starting node and
#'   the value is the end node, e.g. `c("x" = "y")` will remove the edge going
#'   from `x` to `y`.
#' @inheritParams tidy_dagitty
#'
#' @return A `tidy_dagitty` object
#'
#' @export
#' @examples
#' # Example usage:
#' dag <- dagify(y ~ x, x ~ z)
#' saturated_dag <- dag_saturate(dag)
#'
#' saturated_dag %>%
#'   ggdag(edge_type = "arc")
#'
#' saturated_dag %>%
#'   dag_prune(c("x" = "y")) %>%
#'   ggdag(edge_type = "arc")
#' @seealso [as_tidy_dagitty.list()]
dag_saturate <- function(
  .tdy_dag,
  use_existing_coords = FALSE,
  layout = "time_ordered",
  seed = NULL,
  ...
) {
  .dag <- pull_dag(.tdy_dag)
  df_time_order <- .tdy_dag %>%
    pull_dag_data() %>%
    dplyr::select(name, to) %>%
    auto_time_order() %>%
    dplyr::arrange(order)

  if (isTRUE(use_existing_coords)) {
    coords <- dagitty::coordinates(.dag)
  } else {
    coords <- NULL
  }

  split(df_time_order$name, df_time_order$order) %>%
    as_tidy_dagitty(
      exposure = dagitty::exposures(.dag),
      outcome = dagitty::outcomes(.dag),
      latent = dagitty::latents(.dag),
      labels = labels(.dag),
      coords = coords,
      seed = seed,
      layout = layout,
      ...
    )
}

#' @export
#' @rdname dag_saturate
dag_prune <- function(.tdy_dag, edges) {
  stopifnot(!is.null(names(edges)))
  edges <- tibble::enframe(edges, name = "name", value = "to")

  single_edges <- .tdy_dag %>%
    pull_dag_data() %>%
    dplyr::group_by(name) %>%
    dplyr::filter(dplyr::n() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(edges, by = c("name", "to")) %>%
    dplyr::select(name)

  .tdy_dag %>%
    dplyr::mutate(
      direction = ifelse(name %in% single_edges$name, NA, direction),
      direction = factor(
        direction,
        levels = 1:3,
        labels = c("->", "<->", "--")
      ),
      to = ifelse(name %in% single_edges$name, NA_character_, to),
      xend = ifelse(name %in% single_edges$name, NA_real_, xend),
      yend = ifelse(name %in% single_edges$name, NA_real_, yend)
    ) %>%
    dplyr::anti_join(edges, by = c("name", "to")) %>%
    update_dag()
}
