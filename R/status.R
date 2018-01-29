#' Title
#'
#' @param .tdy_dag
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_status <- function(.tdy_dag, as_factor = TRUE) {
  .exposures <- dagitty::exposures(.tdy_dag$dag)
  .outcomes <- dagitty::outcomes(.tdy_dag$dag)
  .latents <- dagitty::latents(.tdy_dag$dag)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                                 status = ifelse(name %in% .exposures, "exposure",
                                                 ifelse(name %in% .outcomes, "outcome",
                                                        ifelse(name %in% .latents, "latent",
                                                               NA))))
  if (as_factor) .tdy_dag$data$status <- factor(.tdy_dag$data$status, exclude = NA)
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
ggdag_status <- function(.tdy_dag, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_status(...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = status)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks = c("exposure", "outcome", "latent"))
}
