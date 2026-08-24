expect_doppelganger <- function(title, fig, ...) {
  testthat::skip_if_not_installed("vdiffr")

  # The baselines are generated on macOS and only reproduce there: platform
  # math libraries differ in the last ulp, which tips collinear-vertex
  # elision in arrow outlines, drifts force-directed layouts and repel
  # positions, and changes the floating-point dust some layouts print as
  # axis labels. Text metrics are not the problem (vdiffr bundles fonts).
  # The macOS CI runner reproduces the baselines exactly, so visual
  # regressions are still caught there.
  testthat::skip_on_os(c("windows", "linux", "solaris"))

  # Repel-based layers consume the RNG stream while the figure is drawn, so the
  # SVG depends on the RNG state at write time. Tests run in parallel and each
  # worker reaches a given expectation having consumed a different amount of the
  # stream, so pin the state here rather than relying on the seed set in
  # helper-load_dag.R. The previous state is restored when this frame exits.
  withr::local_seed(1234)

  vdiffr::expect_doppelganger(title, fig, ...)
}
