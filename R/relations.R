#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_children <- function(.tdy_dag, .var, as_factor = TRUE) {
  .children <- dagitty::children(.tdy_dag$dag, .var)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                                 children = ifelse(name %in% .children, "child",
                                                   ifelse(name == .var, "parent",
                                                          NA)))
  if (as_factor) .tdy_dag$data$children <- factor(.tdy_dag$data$children, exclude = NA)
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_children <- function(.tdy_dag, .var, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_children(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = children)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks  = c("parent", "child"))
}


#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_parents <- function(.tdy_dag, .var, as_factor = TRUE) {
  .parent <- dagitty::parents(.tdy_dag$dag, .var)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                                 parent = ifelse(name %in% .parent, "parent",
                                                 ifelse(name == .var, "child",
                                                        NA)))
  if (as_factor) .tdy_dag$data$parent <- factor(.tdy_dag$data$parent, exclude = NA)
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_parents <- function(.tdy_dag, .var, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_parents(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = parent)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks  = c("parent", "child"))
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_ancestors <- function(.tdy_dag, .var, as_factor = TRUE) {
  .ancestors <- dagitty::ancestors(.tdy_dag$dag, .var)[-1]
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                                 ancestor = ifelse(name %in% .ancestors, "ancestor",
                                                   ifelse(name == .var, "descendant",
                                                          NA)))
  if (as_factor) .tdy_dag$data$ancestor <- factor(.tdy_dag$data$ancestor, exclude = NA)
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_ancestors <- function(.tdy_dag, .var, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_ancestors(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = ancestor)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks  = c("ancestor", "descendant"))
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_descendants <- function(.tdy_dag, .var, as_factor = TRUE) {
  .descendants <- dagitty::descendants(.tdy_dag$dag, .var)[-1]
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                                 descendant = ifelse(name %in% .descendants, "descendant",
                                                     ifelse(name == .var, "ancestor",
                                                            NA)))
  if (as_factor) .tdy_dag$data$descendant <- factor(.tdy_dag$data$descendant, exclude = NA)
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_descendants <- function(.tdy_dag, .var, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_descendants(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = descendant)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks  = c("ancestor", "descendant"))
}
