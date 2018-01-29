#' Title
#'
#' @param .dag
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
node_canonical <- function(.dag, ...) {
  .dag <- if_not_tidy_daggity(.dag)
  dagitty::canonicalize(.dag$dag)$g %>% tidy_dagitty(...)
}

#' Title
#'
#' @param .tdy_dag
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_canonical <- function(.tdy_dag, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)
  node_canonical(.tdy_dag) %>% ggdag(...)
}
