#' Query and Test Conditional Independence in a DAG
#'
#' `query_conditional_independence()` queries conditional independencies implied
#' by a given DAG. These serve as potential robustness checks for your DAG.
#' `test_conditional_independence()` runs the tests of independence implied by
#' the DAG on a given dataset. `ggdag_conditional_independence()` plots the
#' results as a forest plot.
#'
#' @inheritParams dag_params
#' @inheritParams dagitty::impliedConditionalIndependencies
#' @inheritParams dagitty::localTests
#' @param .test_result A data frame containing the results of conditional
#'   independence tests created by `test_conditional_independence()`.
#' @param sort Logical indicating whether to sort the results by estimate value.
#'   Default is `TRUE`.
#' @param vline_linewidth Line width for the vertical line indicating no effect.
#' @param vline_color Color of the vertical line.
#' @param pointrange_fatten Factor to fatten the point range.
#'
#' @return Either a tibble summarizing the conditional independencies in the DAG
#'   or test results, or a ggplot of the results.
#'
#' @export
query_conditional_independence <- function(
  .tdy_dag,
  type = "missing.edge",
  max.results = Inf
) {
  assert_dag_type(.tdy_dag, arg = ".tdy_dag")

  ici <- dagitty::impliedConditionalIndependencies(
    pull_dag(.tdy_dag),
    type = type,
    max.results = max.results
  )

  ici |>
    purrr::imap(
      \(.x, .y) {
        cond_vars <- if (rlang::is_empty(.x$Z)) {
          NA_character_
        } else {
          .x$Z
        }

        tibble::tibble(
          set = .y,
          a = .x$X,
          b = .x$Y,
          conditioning_set = create_set_string(cond_vars),
          conditioned_on = list(cond_vars)
        )
      }
    ) |>
    purrr::list_rbind() |>
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
  loess.pars = NULL
) {
  assert_dag_type(.tdy_dag, arg = ".tdy_dag")

  # Validate that either data or sample.cov is provided
  if (is.null(data) && is.null(sample.cov)) {
    abort(
      c(
        "Either {.arg data} or {.arg sample.cov} must be provided.",
        "i" = "Use {.arg data} to provide raw data.",
        "i" = "Use {.arg sample.cov} to provide a covariance matrix."
      ),
      error_class = "ggdag_missing_data_error"
    )
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
  ) |>
    tibble::as_tibble(rownames = "independence")

  test_results
}


#' @rdname query_conditional_independence
#' @export
ggdag_conditional_independence <- function(
  .test_result,
  sort = TRUE,
  vline_linewidth = .8,
  vline_color = "grey70",
  pointrange_fatten = 3
) {
  if (!is.data.frame(.test_result)) {
    abort(
      c(
        "{.arg .test_result} must be a data frame.",
        "x" = "You provided a {.cls {class(.test_result)}} object."
      ),
      error_class = "ggdag_type_error"
    )
  }
  if (nrow(.test_result) == 0) {
    abort(
      "{.arg .test_result} must contain at least one row of test results.",
      error_class = "ggdag_missing_error"
    )
  }
  estimate <- names(.test_result)[[2]]
  upper_ci <- names(.test_result)[[ncol(.test_result)]]
  lower_ci <- names(.test_result)[[ncol(.test_result) - 1]]

  .test_result$independence <- stringr::str_replace_all(
    .test_result$independence,
    stringr::fixed("_||_"),
    "&"
  )

  if (isTRUE(sort)) {
    .test_result <- .test_result %>%
      dplyr::arrange(.data[[estimate]]) %>%
      dplyr::mutate(independence = forcats::fct_inorder(.data$independence))
  }

  ggplot2::ggplot(
    .test_result,
    ggplot2::aes(
      x = .data[[estimate]],
      y = .data$independence
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linewidth = vline_linewidth,
      color = vline_color
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        xmax = .data[[upper_ci]],
        xmin = .data[[lower_ci]]
      ),
      fatten = pointrange_fatten
    )
}
