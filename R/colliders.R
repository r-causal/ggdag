#' Find colliders
#'
#' Detects any colliders given a DAG.
#' `node_collider` tags colliders and `ggdag_collider` plots all
#' exogenous variables.
#'
#' @param .dag,.tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param as_factor treat `collider` variable as factor
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @inheritParams geom_dag
#'
#' @return a `tidy_dagitty` with a `collider` column for
#'   colliders or a `ggplot`
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
  vars <- unique(pull_dag_data(.tdy_dag)$name)
  colliders <- purrr::map_lgl(vars, ~ is_collider(.tdy_dag, .x))
  names(colliders) <- vars
  .tdy_dag <- dplyr::left_join(
    .tdy_dag,
    tibble::enframe(colliders, value = "colliders"),
    by = "name"
  )
  purrr::map(vars[colliders], ~ dagitty::parents(pull_dag(.tdy_dag), .x))
  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      colliders = factor(
        as.numeric(colliders),
        levels = 0:1,
        labels = c("Non-Collider", "Collider")
      )
    )
  }

  .tdy_dag
}

#' @rdname colliders
#' @export
ggdag_collider <- function(
    .tdy_dag,
    ...,
    size = 1,
    edge_type = c("link_arc", "link", "arc", "diagonal"),
    node_size = 16,
    text_size = 3.88,
    label_size = text_size,
    text_col = "white",
    label_col = "black",
    edge_width = 0.6,
    edge_cap = 8,
    arrow_length = 5,
    use_edges = TRUE,
    use_nodes = TRUE,
    use_stylized = FALSE,
    use_text = TRUE,
    use_labels = FALSE,
    text = NULL,
    label = NULL,
    node = deprecated(),
    stylized = deprecated()) {
  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_collider() %>%
    dplyr::mutate(colliders = forcats::fct_rev(colliders)) %>%
    ggplot2::ggplot(aes_dag(color = colliders))

  p <- p + geom_dag(
    size = size,
    edge_type = edge_type,
    node_size = node_size,
    text_size = text_size,
    label_size = label_size,
    text_col = text_col,
    label_col = label_col,
    edge_width = edge_width,
    edge_cap = edge_cap,
    arrow_length = arrow_length,
    use_edges = use_edges,
    use_nodes = use_nodes,
    use_stylized = use_stylized,
    use_text = use_text,
    use_labels = use_labels,
    text = !!rlang::enquo(text),
    label = !!rlang::enquo(label),
    node = node,
    stylized = stylized
  )

  p
}

#' Activate paths opened by stratifying on a collider
#'
#' Stratifying on colliders can open biasing pathways between variables.
#' `activate_collider_paths` activates any such pathways given a variable
#' or set of variables to adjust for and adds them to the `tidy_dagitty`.
#'
#' @param .tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param adjust_for a character vector, the variable(s) to adjust for.
#' @param ... additional arguments passed to `tidy_dagitty()`
#'
#' @return a `tidy_dagitty` with additional rows for collider-activated
#'   pathways
#' @export
#'
#' @examples
#' dag <- dagify(m ~ x + y, x ~ y)
#'
#' collided_dag <- activate_collider_paths(dag, adjust_for = "m")
#' collided_dag
#'
#' @seealso [control_for()], [ggdag_adjust()],
#'   [geom_dag_collider_edges()]
activate_collider_paths <- function(.tdy_dag, adjust_for, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag, ...)
  vars <- unique(pull_dag_data(.tdy_dag)$name)
  colliders <- purrr::map_lgl(vars, ~ is_collider(.tdy_dag, .x))
  downstream_colliders <- purrr::map_lgl(vars, ~ is_downstream_collider(.tdy_dag, .x))
  collider_names <- unique(c(vars[colliders], vars[downstream_colliders]))

  if (!any((collider_names %in% adjust_for))) {
    return(dplyr::mutate(.tdy_dag, collider_line = FALSE))
  }
  adjusted_colliders <- collider_names[collider_names %in% adjust_for]
  collider_paths <- purrr::map(adjusted_colliders, ~ dagitty::ancestors(pull_dag(.tdy_dag), .x)[-1])


  activated_pairs <- purrr::map(collider_paths, unique_pairs)

  collider_lines <- purrr::map_df(
    activated_pairs,
    dagify_colliders,
    .tdy_dag = .tdy_dag
  )

  collider_lines$collider_line <- TRUE
  .tdy_dag <- dplyr::mutate(.tdy_dag, collider_line = FALSE)
  update_dag_data(.tdy_dag) <- dplyr::bind_rows(pull_dag_data(.tdy_dag), collider_lines)
  .tdy_dag
}

dagify_colliders <- function(.pairs_df, .tdy_dag) {
  .pairs_df %>%
    join_lhs_coords(.tdy_dag) %>%
    join_rhs_coords(.tdy_dag) %>%
    dplyr::mutate(direction = factor("<->", levels = c("<-", "->", "<->"), exclude = NA)) %>%
    dplyr::rename(name = Var1, to = Var2)
}

join_lhs_coords <- function(.x, .y) {
  ggdag_left_join(
    .x,
    pull_dag_data(.y) %>% dplyr::select(name, x, y),
    by = c("Var1" = "name")
  )
}

join_rhs_coords <- function(.x, .y) {
  ggdag_left_join(
    .x,
    pull_dag_data(.y) %>% dplyr::select(name, xend = x, yend = y),
    by = c("Var2" = "name")
  )
}

#' Detecting colliders in DAGs
#'
#' @param .dag an input graph, an object of class `tidy_dagitty` or `dagitty`
#' @param .var a character vector of length 1, the potential collider to check
#' @param downstream Logical. Check for downstream colliders? Default is `TRUE`.
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
is_collider <- function(.dag, .var, downstream = TRUE) {
  if (is.tidy_dagitty(.dag)) .dag <- pull_dag(.dag)
  n_parents <- dagitty::parents(.dag, .var)
  collider <- length(n_parents) > 1
  downstream_collider <- is_downstream_collider(.dag, .var)
  if (downstream) {
    any(c(collider, downstream_collider))
  } else {
    collider
  }
}

#' @rdname is_collider
#' @export
is_downstream_collider <- function(.dag, .var) {
  if (is.tidy_dagitty(.dag)) .dag <- pull_dag(.dag)
  var_ancestors <- dagitty::ancestors(.dag, .var)[-1]
  any(purrr::map_lgl(var_ancestors, ~ is_collider(.dag, .x)))
}
