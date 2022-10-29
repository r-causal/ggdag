#' Find colliders
#'
#' Detects any colliders given a DAG.
#' `node_collider` tags colliders and `ggdag_collider` plots all
#' exogenous variables.
#'
#' @param .dag,.tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @param edge_type a character vector, the edge geom to use. One of:
#'   "link_arc", which accounts for directed and bidirected edges, "link",
#'   "arc", or "diagonal"
#' @param as_factor treat `collider` variable as factor
#' @param node_size size of DAG node
#' @param text_size size of DAG text
#' @param label_size size of label text
#' @param text_col color of DAG text
#' @param label_col color of label text
#' @param node logical. Should nodes be included in the DAG?
#' @param stylized logical. Should DAG nodes be stylized? If so, use
#'   `geom_dag_nodes` and if not use `geom_dag_point`
#' @param text logical. Should text be included in the DAG?
#' @param use_labels a string. Variable to use for `geom_dag_repel_label()`.
#'   Default is `NULL`.
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
  vars <- unique(.tdy_dag$data$name)
  colliders <- purrr::map_lgl(vars, ~ is_collider(.tdy_dag, .x))
  names(colliders) <- vars
  .tdy_dag$data <- dplyr::left_join(.tdy_dag$data,
    tibble::enframe(colliders, value = "colliders"),
    by = "name"
  )
  purrr::map(vars[colliders], ~ dagitty::parents(.tdy_dag$dag, .x))
  if (as_factor) .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, colliders = factor(as.numeric(colliders), levels = 0:1, labels = c("Non-Collider", "Collider")))
  .tdy_dag
}

#' @rdname colliders
#' @export
ggdag_collider <- function(.tdy_dag, ..., edge_type = "link_arc", node_size = 16,
                           text_size = 3.88, label_size = text_size,
                           text_col = "white", label_col = text_col,
                           node = TRUE, stylized = FALSE, text = TRUE,
                           use_labels = NULL) {
  edge_function <- edge_type_switch(edge_type)

  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_collider() %>%
    dplyr::mutate(colliders = forcats::fct_rev(colliders)) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = colliders)) +
    edge_function() +
    scale_adjusted()

  if (node) {
    if (stylized) {
      p <- p + geom_dag_node(size = node_size)
    } else {
      p <- p + geom_dag_point(size = node_size)
    }
  }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) {
    p <- p +
      geom_dag_label_repel(
        ggplot2::aes(
          label = !!rlang::sym(use_labels),
          fill = colliders
        ),
        size = text_size,
        col = label_col, show.legend = FALSE
      )
  }
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
  vars <- unique(.tdy_dag$data$name)
  colliders <- purrr::map_lgl(vars, ~ is_collider(.tdy_dag, .x))
  downstream_colliders <- purrr::map_lgl(vars, ~ is_downstream_collider(.tdy_dag, .x))
  collider_names <- unique(c(vars[colliders], vars[downstream_colliders]))

  if (!any((collider_names %in% adjust_for))) {
    return(dplyr::mutate(.tdy_dag, collider_line = FALSE))
  }
  adjusted_colliders <- collider_names[collider_names %in% adjust_for]
  collider_paths <- purrr::map(adjusted_colliders, ~ dagitty::ancestors(.tdy_dag$dag, .x)[-1])

  activated_pairs <- purrr::map(collider_paths, unique_pairs)

  collider_lines <- purrr::map_df(
    activated_pairs,
    dagify_colliders,
    .tdy_dag = .tdy_dag
  )

  collider_lines$collider_line <- TRUE
  .tdy_dag$data$collider_line <- FALSE
  .tdy_dag$data <- dplyr::bind_rows(.tdy_dag$data, collider_lines)
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
  dplyr::left_join(
    .x,
    .y$data %>% dplyr::select(name, x, y),
    by = c("Var1" = "name")
  )
}

join_rhs_coords <- function(.x, .y) {
  dplyr::left_join(
    .x,
    .y$data %>% dplyr::select(name, xend = x, yend = y),
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
  if (is.tidy_dagitty(.dag)) .dag <- .dag$dag
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
  if (is.tidy_dagitty(.dag)) .dag <- .dag$dag
  var_ancestors <- dagitty::ancestors(.dag, .var)[-1]
  any(purrr::map_lgl(var_ancestors, ~ is_collider(.dag, .x)))
}
