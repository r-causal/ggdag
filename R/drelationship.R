#' D-relationship between variables
#'
#' D-separation is a key concept in causal structural models. Variables are
#' d-separated if there are no open paths between them. The \code{node_d*()}
#' functions label variables as d-connected or d-separated. The
#' \code{ggdag_d*()} functions plot the results. The \code{*_dconnected()},
#' \code{*_dseparated()}, and \code{*_drelationship()} functions essentially
#' produce the same output and are just different ways of thinking about the
#' relationship. See \code{dagitty::\link[dagitty]{dseparated}} for details.
#'
#' @param .tdy_dag input graph, an object of class \code{tidy_dagitty} or
#'   \code{dagitty}
#' @param ... additional arguments passed to \code{tidy_dagitty()}
#' @param from a character vector, the starting variable (must by in DAG)
#' @param to a character vector, the ending variable (must by in DAG)
#' @param controlling_for a character vector, variables in the DAG to control
#'   for.
#' @param as_factor logical. Should the \code{d_relationship} variable be a
#'   factor?
#'
#' @return a \code{tidy_dagitty} with an \code{d_relationship} column for
#'   variable D relationship or a \code{ggplot}
#' @export
#'
#' @examples
#' dag <- dagify(m ~ x + y)
#' dag %>% ggdag_drelationship("x", "y")
#' dag %>% ggdag_drelationship("x", "y", controlling_for = "m")
#'
#' dag %>%
#'   node_dseparated("x", "y") %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
#'   geom_dag_edges() +
#'   geom_dag_collider_edges() +
#'   geom_dag_node() +
#'   geom_dag_text(col = "white") +
#'   theme_dag() + scale_dag()
#'
#' dag %>%
#'   node_dconnected("x", "y", controlling_for = "m") %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
#'   geom_dag_edges() +
#'   geom_dag_collider_edges() +
#'   geom_dag_node() +
#'   geom_dag_text(col = "white") +
#'   theme_dag() +
#'   scale_dag()
#'
#' dagify(m ~ x + y, m_jr ~ m) %>%
#'   tidy_dagitty(layout = "nicely") %>%
#'   node_dconnected("x", "y", controlling_for = "m_jr") %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
#'   geom_dag_edges() +
#'   geom_dag_collider_edges() +
#'   geom_dag_node() +
#'   geom_dag_text(col = "white") +
#'   theme_dag() +
#'   scale_dag()
#' @rdname d_relationship
#' @name Assess d-separation between variables
node_dconnected <- function(.tdy_dag, from, to, controlling_for = NULL, as_factor = TRUE, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag$data$collider_line <- FALSE
    .tdy_dag$data$adjusted <- "unadjusted"
    controlling_for <- c()
  }

  .dconnected <- dagitty::dconnected(.tdy_dag$dag, from, to, controlling_for)

  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                                 d_relationship = ifelse(name %in% c(from, to) & .dconnected, "d-connected",
                                                         ifelse(name %in% c(from, to) & !.dconnected, "d-separated",
                                                                NA)))
  if (as_factor) .tdy_dag$data$d_relationship <- factor(.tdy_dag$data$d_relationship, levels = c("d-connected", "d-separated"), exclude = NA)

  .tdy_dag
}

#' @rdname d_relationship
#' @export
node_dseparated <- function(.tdy_dag, from, to, controlling_for = NULL, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag$data$collider_line <- FALSE
    .tdy_dag$data$adjusted <- "unadjusted"
    controlling_for <- c()
  }

  .dseparated <- dagitty::dseparated(.tdy_dag$dag, from, to, controlling_for)

  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                                 d_relationship = ifelse(name %in% c(from, to) & !.dseparated, "d-connected",
                                                         ifelse(name %in% c(from, to) & .dseparated, "d-separated",
                                                                NA)))
  if (as_factor) .tdy_dag$data$d_relationship <- factor(.tdy_dag$data$d_relationship, levels = c("d-connected", "d-separated"), exclude = NA)
  .tdy_dag
}

#' @rdname d_relationship
#' @export
node_drelationship <- function(.tdy_dag, from, to, controlling_for = NULL, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag$data$collider_line <- FALSE
    .tdy_dag$data$adjusted <- "unadjusted"
    controlling_for <- c()
  }


  .dseparated <- dagitty::dseparated(.tdy_dag$dag, from, to, controlling_for)

  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                                 d_relationship = ifelse(name %in% c(from, to) & !.dseparated, "d-connected",
                                                         ifelse(name %in% c(from, to) & .dseparated, "d-separated",
                                                                NA)))
  if (as_factor) .tdy_dag$data$d_relationship <- factor(.tdy_dag$data$d_relationship, levels = c("d-connected", "d-separated"), exclude = NA)
  .tdy_dag
}

#' @rdname d_relationship
#' @export
ggdag_drelationship <- function(.tdy_dag, from, to, controlling_for = NULL, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_drelationship(from = from, to = to, controlling_for = controlling_for, ...)  %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
    geom_dag_edges() +
    geom_dag_collider_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag()
}

#' @rdname d_relationship
#' @export
ggdag_dseparated <- function(.tdy_dag, from, to, controlling_for = NULL, ...) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to,
                      controlling_for = controlling_for, ...)
}

#' @rdname d_relationship
#' @export
ggdag_dconnected <- function(.tdy_dag, from, to, controlling_for = NULL, ...) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to,
                      controlling_for = controlling_for, ...)
}
