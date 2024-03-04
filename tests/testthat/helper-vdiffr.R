expect_doppelganger <- function(title, fig, ...) {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(ggplot2_version() >= "3.3.6.9000")
  vdiffr::expect_doppelganger(title, fig, ...)
}
