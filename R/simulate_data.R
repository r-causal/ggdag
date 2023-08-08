#' Simulate Data from Structural Equation Model
#'
#' This is a thin wrapper for the `simulateSEM()`function in `dagitty`
#' that works with tidied dagitty objects. It treats the input DAG as a
#' structural equation model, generating random path coefficients and simulating
#' corresponding data. See [dagitty::simulateSEM()] for
#' details.
#'
#' @param .tdy_dag the input DAG, which can be a `tidy_dagitty` or
#'   `dagitty` object.
#' @param b.default default path coefficient applied to arrows for which no
#'   coefficient is defined in the model syntax.
#' @param b.lower lower bound for random path coefficients, applied if b.default
#'   = NULL.
#' @param b.upper upper bound for path coefficients.
#' @param eps residual variance (only meaningful if standardized=FALSE).
#' @param N number of samples to generate.
#' @param standardized whether a standardized output is desired (all variables
#'   have variance 1).
#'
#' @return a `tbl`with N values for each variable in .tdy_dag
#' @export
#'
#' @examples
#' dagify(y ~ z, x ~ z) %>%
#'   tidy_dagitty() %>%
#'   simulate_data()
simulate_data <- function(.tdy_dag, b.default = NULL, b.lower = -0.6, b.upper = 0.6, eps = 1,
                          N = 500, standardized = TRUE) {
  if_not_tidy_daggity(.tdy_dag) %>%
    pull_dag() %>%
    dagitty::simulateSEM(
      b.default = b.default, b.lower = b.lower, b.upper = b.upper, eps = eps,
      N = N, standardized = standardized
    ) %>%
    dplyr::as_tibble()
}
