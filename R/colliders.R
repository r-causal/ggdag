#' Find colliders
#'
#' Detects any colliders given a DAG.
#' \code{node_collider} tags colliders and \code{ggdag_collider} plots all
#' exogenous variables.
#'
#' @param .dag,.tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param ... additional arguments passed to \code{tidy_dagitty()}
#' @param as_factor treat \code{collider} variable as factor
#'
#' @return a \code{tidy_dagitty} with a \code{collider} column for
#'   colliders or a \code{ggplot}
#' @export
#'
#' @examples
#' dag <- dagify(m ~ x + y, y ~ x)
#'
#' node_collider(dag)
#' ggdag_collider(dag)
#'
#' @rdname colliders
#' @name Colliders
node_collider <- function(.dag, as_factor = TRUE, ...) {
  .tdy_dag <- if_not_tidy_daggity(.dag, ...)
  vars <- unique(.tdy_dag$data$name)
  colliders <- purrr::map_lgl(vars, ~is_collider(.tdy_dag, .x))
  names(colliders) <- vars
  .tdy_dag$data <- dplyr::left_join(.tdy_dag$data,
                                    tibble::enframe(colliders, value = "colliders"), by = "name")
  purrr::map(vars[colliders], ~dagitty::parents(.tdy_dag$dag, .x))
  if (as_factor) .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, colliders = factor(as.numeric(colliders), levels = 0:1, labels = c("Non-Collider", "Collider")))
  .tdy_dag
}

#' @rdname colliders
#' @export
ggdag_collider <- function(.tdy_dag, ...) {
  if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_collider() %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = forcats::fct_rev(colliders))) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag()
}

#' Activate paths opened by stratifying on a collider
#'
#' Stratifying on colliders can open biasing pathways between variables.
#' \code{activate_collider_paths} activates any such pathways given a variable
#' or set of variables to adjust for and adds them to the \code{tidy_dagitty}.
#'
#' @param .tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param adjust_for a character vector, the variable(s) to adjust for.
#' @param ... additional arguments passed to \code{tidy_dagitty()}
#'
#' @return a \code{tidy_dagitty} with additional rows for collider-activated
#'   pathways
#' @export
#'
#' @examples
#' dag <- dagify(m ~ x + y, x ~ y)
#'
#' collided_dag <- activate_collider_paths(dag, adjust_for = "m")
#' collided_dag
#'
#' @seealso \code{\link{control_for}}, \code{\link{ggdag_adjust}},
#'   \code{\link{geom_dag_collider_edges}}
activate_collider_paths <- function(.tdy_dag, adjust_for, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag, ...)
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

#' Detecting colliders in DAGs
#'
#' @param .dag an input graph, an object of class \code{tidy_dagitty} or \code{dagitty}
#' @param .var a character vector of length 1, the potential collider to check
#'
#' @return Logical. Is the variable a collider or downstream collider?
#' @export
#'
#' @examples
#' dag <- dagify(m ~ x + y, m_jr ~ m)
#' is_collider(dag, "m")
#' is_downstream_collider(dag, "m_jr")
#'
#' #  a downstream collider is also treated as a collider
#' is_collider(dag, "m_jr")
#'
#' #  but a direct collider is not treated as a downstream collider
#' is_downstream_collider(dag, "m")
#'
#' @rdname is_collider
#' @name Test if Variable Is Collider
is_collider <- function(.dag, .var) {
  if (is.tidy_dagitty(.dag)) .dag <- .dag$dag
  n_parents <- dagitty::parents(.dag, .var)
  collider <- length(n_parents) > 1
  downstream_collider <- is_downstream_collider(.dag, .var)
  any(c(collider, downstream_collider))
}

#' @rdname is_collider
#' @export
is_downstream_collider <- function(.dag, .var) {
  if (is.tidy_dagitty(.dag)) .dag <- .dag$dag
  var_ancestors <- dagitty::ancestors(.dag, .var)[-1]
  any(purrr::map_lgl(var_ancestors, ~is_collider(.dag, .x)))
}
