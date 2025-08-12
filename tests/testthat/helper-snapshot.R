#' Expect ggdag error with snapshot
#' @noRd
expect_ggdag_error <- function(expr) {
  testthat::expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    expr
  )
}

#' Expect ggdag warning with snapshot
#' @noRd
expect_ggdag_warning <- function(expr) {
  testthat::expect_snapshot(expr)
}
