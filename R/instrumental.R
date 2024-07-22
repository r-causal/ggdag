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
#' @inheritParams geom_dag
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
  instrumental_vars <- dagitty::instrumentalVariables(
    pull_dag(.dag),
    exposure = exposure,
    outcome = outcome
  )


  i_vars <- purrr::map(instrumental_vars, "I")
  if (purrr::is_empty(i_vars)) {
    .dag <- dplyr::mutate(
      .dag,
      instrumental = NA
    )
    return(.dag)
  }
  adjust_for_vars <- purrr::map(instrumental_vars, "Z")

  update_dag_data(.dag) <- purrr::map2_df(i_vars, adjust_for_vars, function(.i, .z) {
    conditional_vars <- ifelse(is.null(.z), "", paste("|", paste(.z, collapse = ", ")))
    .dag <- .dag %>% dplyr::mutate(
      instrumental_name = paste(.i, conditional_vars) %>% stringr::str_trim()
    )
    if (!is.null(.z)) {
      .dag <- .dag %>% control_for(.z, activate_colliders = FALSE)
    }
    .dag <- .dag %>% dplyr::mutate(
      instrumental = ifelse(name == .i, "instrumental", NA)
    )

    pull_dag_data(.dag)
  })

  .dag
}

#' @rdname instrumental
#' @export
ggdag_instrumental <- function(
    .tdy_dag,
    exposure = NULL,
    outcome = NULL,
    ...,
    size = 1,
    edge_type = c("link_arc", "link", "arc", "diagonal"),
    node_size = 16,
    text_size = 3.88,
    label_size = text_size,
    text_col = "white",
    label_col = "black",
    edge_width = 0.6,
    edge_cap = 10,
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
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_instrumental(exposure = exposure, outcome = outcome, ...)
  has_instrumental <- !all(is.na((pull_dag_data(.tdy_dag)$instrumental)))
  has_adjusted <- "adjusted" %in% names(pull_dag_data(.tdy_dag))
  mapping <- aes_dag()
  if (has_adjusted) {
    mapping$shape <- substitute(adjusted)
  }

  if (has_instrumental) {
    mapping$colour <- substitute(instrumental)
  }

  p <- .tdy_dag %>%
    ggplot2::ggplot(mapping)
  if (has_adjusted) p <- p + scale_adjusted()
  if (has_instrumental) p <- p + breaks("instrumental")

  p <- p +
    geom_dag(
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

  if (has_instrumental) {
    p <- p + ggplot2::facet_wrap(~instrumental_name)
  } else {
    p <- p + ggplot2::facet_wrap(~"{No instrumental variables present}")
  }
  p
}
