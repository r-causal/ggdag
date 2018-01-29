#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_dconnected <- function(.tdy_dag, from, to, controlling_for = NULL, as_factor = TRUE) {
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

#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_dseparated <- function(.tdy_dag, from, to, controlling_for = NULL, as_factor = TRUE) {
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

#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_drelationship <- function(.tdy_dag, from, to, controlling_for = NULL, as_factor = TRUE) {
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

#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_dseparated <- function(.tdy_dag, from, to, controlling_for = NULL, ...) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to,
                      controlling_for = controlling_for, ...)
}

#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_dconnected <- function(.tdy_dag, from, to, controlling_for = NULL, ...) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to,
                      controlling_for = controlling_for, ...)
}
