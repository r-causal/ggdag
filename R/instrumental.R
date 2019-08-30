#' Find Instrumental Variables
#'
#' `node_instrumental` tags instrumental variables given an exposure and
#' outcome. `ggdag_instrumental` plots all instrumental variables. See
#' [dagitty::instrumentalVariables()] for details.
#'
#' @param .dag,.tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param exposure character vector of length 1, name of exposure variable.
#'   Default is `NULL`, in which case it will check the input DAG for
#'   exposure.
#' @param outcome character vector of length 1, name of exposure variable.
#'   Default is `NULL`, in which case it will check the input DAG for
#'   exposure.
#' @param ... additional arguments passed to `tidy_dagitty()`
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
#' @return a `tidy_dagitty` with an `instrumental` column for
#'   instrumental variables or a `ggplot`
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
    if (!is.null(.z)) {
      .dag <- .dag %>% control_for(.z)
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
                               node_size = 16, text_size = 3.88, label_size = text_size,
                               text_col = "white", label_col = text_col,
                               node = TRUE, stylized = FALSE, text = TRUE, use_labels = NULL) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_instrumental(exposure = exposure, outcome = outcome, ...)
  p <- .tdy_dag %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = instrumental, shape = adjusted)) +
    geom_dag_edges() +
    scale_adjusted() +
    breaks("instrumental")

  if (node) {
    if (stylized) {
        p <- p + geom_dag_node(size = node_size)
      } else {
        p <- p + geom_dag_point(size = node_size)
      }
    }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) p <- p +
      geom_dag_label_repel(ggplot2::aes_string(label = use_labels,
                                               fill = "instrumental"), size = text_size,
                           col = label_col, show.legend = FALSE)
  if (dplyr::n_distinct(.tdy_dag$data$instrumental_name) > 1) p <- p + ggplot2::facet_wrap(~instrumental_name)
  p
}
