#' Find Instrumental Variables
#'
#' \code{node_instrumental} tags instrumental variables given an exposure and
#' outcome. \code{ggdag_instrumental} plots all instrumental variables. See
#' \code{dagitty::\link[dagitty]{instrumentalVariables}} for details.
#'
#' @param .dag,.tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param exposure character vector of length 1, name of exposure variable.
#'   Default is \code{NULL}, in which case it will check the input DAG for
#'   exposure.
#' @param outcome character vector of length 1, name of exposure variable.
#'   Default is \code{NULL}, in which case it will check the input DAG for
#'   exposure.
#' @param ... additional arguments passed to \code{tidy_dagitty()}
#' @param node_size size of DAG node
#' @param text_size size of DAG text
#' @param text_col color of DAG text
#' @param node logical. Should nodes be included in the DAG?
#' @param text logical. Should text be included in the DAG?
#' @param use_labels a string. Variable to use for
#'   \code{geom_dag_repel_label()}. Default is \code{NULL}.
#'
#' @return a \code{tidy_dagitty} with an \code{instrumental} column for
#'   instrumental variables or a \code{ggplot}
#' @export
#'
#' @examples
#' library(dagitty)
#'
#' node_instrumental(dagitty("dag{ i->x->y; x<->y }"), "x", "y")
#' ggdag_instrumental(dagitty("dag{ i->x->y; i2->x->y; x<->y }"), "x", "y")
#'
#' @rdname instrumental
#' @name Instrumental Variables
node_instrumental <- function(.dag, exposure = NULL, outcome = NULL, ...) {
  .dag <- if_not_tidy_daggity(.dag, ...)
  instrumental_vars <- dagitty::instrumentalVariables(.dag$dag,
                                                      exposure = exposure,
                                                      outcome = outcome)


  i_vars <- purrr::map(instrumental_vars, "I")
  if (purrr::is_empty(i_vars)) {
    .dag$data$adjusted <- factor("unadjusted", levels = c("unadjusted", "adjusted"), exclude = NA)
    .dag$data$instrumental <- NA
    return(.dag)
  }
  adjust_for_vars <- purrr::map(instrumental_vars, "Z")

  .dag$data <- purrr::map2_df(i_vars, adjust_for_vars, function(.i, .z) {
    conditional_vars <- ifelse(is.null(.z), "", paste("|", paste(.z, collapse = ", ")))
    .dag$data$instrumental_name <- paste(.i, conditional_vars) %>% stringr::str_trim()
    if (!is.null(adjust_for_vars)) {
      .dag <- .dag %>% control_for(adjust_for_vars)
    } else {
      .dag$data$adjusted <- factor("unadjusted", levels = c("unadjusted", "adjusted"), exclude = NA)
    }
    .dag$data <- .dag$data %>% dplyr::mutate(instrumental = ifelse(name == .i, "instrumental", NA))
    .dag$data
  })
  .dag
}

#' @rdname instrumental
#' @export
ggdag_instrumental <- function(.tdy_dag, exposure = NULL, outcome = NULL, ...,
                               node_size = 16, text_size = 3.88, text_col = "white",
                               node = TRUE, text = TRUE, use_labels = NULL) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_instrumental(exposure = exposure, outcome = outcome, ...)
  p <- .tdy_dag %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = instrumental, shape = adjusted)) +
    geom_dag_edges() +
    theme_dag() +
    scale_dag(breaks  = "instrumental")

  if (node) p <- p + geom_dag_node(size = node_size)
  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)
  if (!is.null(use_labels)) p <- p +
    geom_dag_label_repel(ggplot2::aes_string(label = use_labels,
                                             fill = "instrumental"),
                         col = "white", show.legend = FALSE)
  if (dplyr::n_distinct(.tdy_dag$data$instrumental_name) > 1) p <- p + ggplot2::facet_wrap(~instrumental_name)
  p
}
