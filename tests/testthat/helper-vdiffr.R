expect_doppelganger <- function(title, fig, ...) {
  testthat::skip_if_not_installed("vdiffr")
  # TODO: remove skip when devel ubuntu not failing
  # EDIT: Done. leaving comment for record temporarily.
  # see https://github.com/r-causal/ggdag/actions/runs/7699530598/job/20981341511
  # also https://github.com/r-causal/ggdag/actions/runs/7701021564/job/20986141655?pr=132

  # Repel-based layers consume the RNG stream while the figure is drawn, so the
  # SVG depends on the RNG state at write time. Tests run in parallel and each
  # worker reaches a given expectation having consumed a different amount of the
  # stream, so pin the state here rather than relying on the seed set in
  # helper-load_dag.R. The previous state is restored when this frame exits.
  withr::local_seed(1234)

  vdiffr::expect_doppelganger(title, fig, ...)
}
