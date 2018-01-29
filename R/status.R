#' Find variable status
#'
#' Detects variable status given a DAG (exposure, outcome, latent). See
#' \code{dagitty::\link[dagitty]{VariableStatus}} for details.
#'
#' \code{node_collider} tags variable status and \code{ggdag_collider} plots all
#' variable statuses.
#'
#' @param .dag,.tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param ... additional arguments passed to \code{tidy_dagitty()}
#' @param as_factor treat \code{status} variable as factor
#'
#' @return a \code{tidy_dagitty} with an \code{status} column for
#'   variable status or a \code{ggplot}
#' @export
#'
#' @examples
#' dag <- dagify(l ~ x + y,
#'   y ~ x,
#'   exposure = "x",
#'   outcome = "y",
#'   latent = "l")
#'
#' node_status(dag)
#' ggdag_status(dag)
#'
#' @rdname status
#' @name Variable Status
node_status <- function(.tdy_dag, as_factor = TRUE, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag, ...)
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

#' @rdname status
#' @export
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
