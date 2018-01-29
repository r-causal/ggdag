#' Title
#'
#' @param .dag
#' @param exposure
#' @param outcome
#'
#' @return
#' @export
#'
#' @examples
node_instrumental <- function(.dag, exposure = NULL, outcome = NULL) {
  .dag <- if_not_tidy_daggity(.dag)
  instrumental_vars <- dagitty::instrumentalVariables(.dag$dag,
                                                      exposure = exposure,
                                                      outcome = outcome)


  i_vars <- purrr::map(instrumental_vars, "I")
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
ggdag_instrumental <- function(.tdy_dag, exposure = NULL, outcome = NULL, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_instrumental(exposure = exposure, outcome = outcome, ...)
  p <- .tdy_dag %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = instrumental, shape = adjusted)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks  = "instrumental")

  if (dplyr::n_distinct(.tdy_dag$data$instrumental_name) > 1) p <- p + ggplot2::facet_wrap(~instrumental_name)
  p
}
