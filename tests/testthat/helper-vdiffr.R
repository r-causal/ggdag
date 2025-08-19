expect_doppelganger <- function(title, fig, ...) {
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(ggplot2_version() >= "3.3.6.9000")
  # TODO: remove skip when devel ubuntu not failing
  # see https://github.com/r-causal/ggdag/actions/runs/7699530598/job/20981341511
  # also https://github.com/r-causal/ggdag/actions/runs/7701021564/job/20986141655?pr=132

  testthat::skip_on_ci()
  vdiffr::expect_doppelganger(title, fig, ...)
}
