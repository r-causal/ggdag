expect_doppelganger <- function(title, fig, ...) {
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(ggplot2_version() >= "3.3.6.9000")
  # TODO: remove skip when devel ubuntu not failing
  # see https://github.com/r-causal/ggdag/actions/runs/7699530598/job/20981341511
  testthat::skip_on_os("linux")
  vdiffr::expect_doppelganger(title, fig, ...)
}
