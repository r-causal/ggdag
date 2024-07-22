#' Query and Test Conditional Independence in a DAG
#'
#' `query_conditional_independence()` queries conditional independencies implied
#' by a given DAG. These serve as potential robustness checks for your DAG.
#' `test_conditional_independence()` runs the tests of independence implied by
#' the DAG on a given dataset. `ggdag_conditional_independence()` plots the
#' results as a forest plot.
#'
#' @param .tdy_dag A tidy DAG object.
#' @inheritParams dagitty::impliedConditionalIndependencies
#' @inheritParams dagitty::localTests
#' @param .test_result A data frame containing the results of conditional
#'   independence tests created by `test_conditional_independence()`.
#' @param vline_linewidth Line width for the vertical line indicating no effect.
#' @param vline_color Color of the vertical line.
#' @param pointrange_fatten Factor to fatten the point range.
#'
#' @return Either a tibble summarizing the conditional independencies in the DAG
#'   or test results, or a ggplot of the results.
#'
#' @export
query_conditional_independence <- function(.tdy_dag, type = "missing.edge", max.results = Inf) {
  if (!dagitty::is.dagitty(.tdy_dag) && !is.tidy_dagitty(.tdy_dag)) {
    stop("Expected a DAG object for `.tdy_dag`, got ", class(.tdy_dag), ".", call. = FALSE)
  }

  ici <- dagitty::impliedConditionalIndependencies(
    pull_dag(.tdy_dag),
    type = type,
    max.results = max.results
  )

  ici %>%
    purrr::imap(~ tibble::tibble(
      set = .y,
      a = .x$X,
      b = .x$Y,
      conditioned_on = if (rlang::is_empty(.x$Z)) list(NA_character_) else list(.x$Z)
    )) %>%
    purrr::list_rbind() %>%
    tibble::as_tibble()
}

#' @rdname query_conditional_independence
#' @export
test_conditional_independence <- function(
    .tdy_dag,
    data = NULL,
    type = c(
      "cis",
      "cis.loess",
      "cis.chisq",
      "cis.pillai",
      "tetrads",
      "tetrads.within",
      "tetrads.between",
      "tetrads.epistemic"
    ),
    tests = NULL,
    sample.cov = NULL,
    sample.nobs = NULL,
    conf.level = 0.95,
    R = NULL,
    max.conditioning.variables = NULL,
    abbreviate.names = FALSE,
    tol = NULL,
    loess.pars = NULL) {
  if (!dagitty::is.dagitty(.tdy_dag) && !is.tidy_dagitty(.tdy_dag)) {
    stop("Expected a DAG object for `.tdy_dag`, got ", class(.tdy_dag), ".", call. = FALSE)
  }

  test_results <- dagitty::localTests(
    pull_dag(.tdy_dag),
    data = data,
    type = type,
    tests = tests,
    sample.cov = sample.cov,
    sample.nobs = sample.nobs,
    conf.level = conf.level,
    R = R,
    max.conditioning.variables = max.conditioning.variables,
    abbreviate.names = abbreviate.names,
    tol = tol,
    loess.pars = loess.pars
  ) %>%
    tibble::as_tibble(rownames = "independence")

  test_results
}


#' @rdname query_conditional_independence
#' @export
ggdag_conditional_independence <- function(
    .test_result,
    vline_linewidth = .8,
    vline_color = "grey70",
    pointrange_fatten = 3) {
  stopifnot(is.data.frame(.test_result))
  stopifnot(nrow(.test_result) > 0)
  estimate <- names(.test_result)[[2]]
  upper_ci <- names(.test_result)[[ncol(.test_result)]]
  lower_ci <- names(.test_result)[[ncol(.test_result) - 1]]

  .test_result$independence <- stringr::str_replace_all(
    .test_result$independence,
    stringr::fixed("_||_"),
    "&"
  )

  ggplot2::ggplot(
    .test_result,
    ggplot2::aes(
      x = .data[[estimate]],
      y = independence
    )
  ) +
    ggplot2::geom_vline(xintercept = 0, linewidth = vline_linewidth, color = vline_color) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        xmax = .data[[upper_ci]],
        xmin = .data[[lower_ci]]
      ),
      fatten = pointrange_fatten
    )
}
