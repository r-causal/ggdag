#' Covariate Adjustment Sets
#'
#' See \code{dagitty::\link[dagitty]{adjustmentSets}} for details.
#'
#' @param .tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param exposure a character vector, the exposure variable. Default is
#'   \code{NULL}, in which case it will be determined from the DAG.
#' @param outcome a character vector, the outcome variable. Default is
#'   \code{NULL}, in which case it will be determined from the DAG.
#' @param ... additional arguments to \code{adjustmentSets}
#' @param node_size size of DAG node
#' @param text_size size of DAG text
#' @param text_col color of DAG text
#' @param node logical. Should nodes be included in the DAG?
#' @param text logical. Should text be included in the DAG?
#' @param use_labels a string. Variable to use for
#'   \code{geom_dag_repel_label()}. Default is \code{NULL}.
#'
#' @return a \code{tidy_dagitty} with an \code{adjusted} column and \code{set}
#'   column, indicating adjustment status and DAG ID, respectively, for the
#'   adjustment sets or a \code{ggplot}
#' @export
#'
#' @examples
#' dag <- dagify(y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   w1 ~~ w2,
#'   exposure = "x",
#'   outcome = "y")
#'
#' tidy_dagitty(dag) %>% dag_adjustment_sets()
#'
#' ggdag_adjustment_set(dag)
#'
#' ggdag_adjustment_set(dagitty::randomDAG(10, .5),
#'   exposure = "x3",
#'   outcome = "x5")
#'
#' @importFrom utils capture.output
#'
#' @rdname adjustment_sets
#' @name Covariate Adjustment Sets
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


#' @rdname adjustment_sets
#' @export
ggdag_adjustment_set <- function(.tdy_dag, exposure = NULL, outcome = NULL, ...,
                                 node_size = 16, text_size = 3.88, text_col = "white",
                                 node = TRUE, text = TRUE, use_labels = NULL) {
  p <- if_not_tidy_daggity(.tdy_dag) %>%
    dag_adjustment_sets(exposure = exposure, outcome = outcome, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = adjusted)) +
    geom_dag_edges(ggplot2::aes(edge_alpha = adjusted)) +
    ggplot2::facet_wrap(~set) +
    theme_dag() +
    scale_dag(expand_x = expand_scale(c(0.25, 0.25)))

  if (node) p <- p + geom_dag_node(size = node_size)
  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)
  if (!is.null(use_labels)) p <- p +
      geom_dag_label_repel(ggplot2::aes_string(label = use_labels,
                                               fill = "adjusted"),
                           col = "white", show.legend = FALSE)
  p
}

#' Assess if a variable confounds a relationship
#'
#' @param .tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param z a character vector, the potential confounder
#' @param x,y a character vector, the variables z may confound.
#' @param direct logical. Only consider direct confounding? Default is
#'   \code{FALSE}
#'
#' @return Logical. Is the variable a confounder?
#' @export
#'
#' @examples
#' dag <- dagify(y ~ z, x ~ z)
#'
#' is_confounder(dag, "z", "x", "y")
#' is_confounder(dag, "x", "z", "y")
#'
is_confounder <- function(.tdy_dag, z, x, y, direct = FALSE) {
  dag <- if_not_tidy_daggity(.tdy_dag)$dag

  if (direct) {
    z_descendants <- dagitty::children(dag, z)
  } else {
    z_descendants <- dagitty::descendants(dag, z)[-1]
  }
  all(c(x, y) %in% z_descendants)
}

#' Adjust for variables and activate any biasing paths that result
#'
#' @param .tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param adjust_for a character vector, the variable(s) to adjust for.
#' @param ... additional arguments passed to \code{tidy_dagitty()}
#' @param node_size size of DAG node
#' @param text_size size of DAG text
#' @param text_col color of DAG text
#' @param node logical. Should nodes be included in the DAG?
#' @param text logical. Should text be included in the DAG?
#' @param use_labels a string. Variable to use for
#'   \code{geom_dag_repel_label()}. Default is \code{NULL}.
#' @param as_factor logical. Should the \code{adjusted} column be a factor?
#'
#' @return a \code{tidy_dagitty} with a \code{adjusted} column for adjusted
#'   variables, as well as any biasing paths that arise, or a \code{ggplot}
#' @export
#'
#' @examples
#' dag <- dagify(m ~ a + b, x ~ a, y ~ b)
#'
#' control_for(dag, adjust_for = "m")
#' ggdag_adjust(dag, adjust_for = "m")
#'
#' @rdname control_for
#' @name Adjust for variables
control_for <- function(.tdy_dag, adjust_for, as_factor = TRUE, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag, ...)
  .tdy_dag <- activate_collider_paths(.tdy_dag, adjust_for)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, adjusted = ifelse(name %in% adjust_for, "adjusted", "unadjusted"))
  if (as_factor) .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, adjusted = factor(adjusted, exclude = NA))
  .tdy_dag
}

#' @rdname control_for
#' @export
ggdag_adjust <- function(.tdy_dag, adjust_for, ...,
                         node_size = 16, text_size = 3.88, text_col = "white",
                         node = TRUE, text = TRUE, use_labels = NULL) {
  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    control_for(adjust_for) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted)) +
    geom_dag_edges(ggplot2::aes(edge_alpha = adjusted)) +
    geom_dag_collider_edges() +
    theme_dag() +
    scale_dag()

  if (node) p <- p + geom_dag_node(size = node_size)
  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)
  if (!is.null(use_labels)) p <- p +
      geom_dag_label_repel(ggplot2::aes_string(label = use_labels,
                                               fill = "adjusted"),
                           col = "white", show.legend = FALSE)
  p
}
