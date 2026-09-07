# Placement invariance and performance pins for the automatic label engine.
#
# `place_dag_labels()` (R/label_auto.R) is the draw-time engine that decides
# where every automatic label goes, and `makeContent.dag_labels_auto()` builds
# its millimetre inputs from the panel. It is by far the most expensive part of
# drawing a labelled DAG: on a 30-node scene with spline edges it is 97% of the
# draw. Making it faster must not move a single label, so the two halves of
# this file are pinned separately.
#
# The invariance test pins the exact placement of the four scenes in
# helper-label-perf.R, at 7 x 5 inches under both edge routes, against
# fixtures/label-placements-7x5.rds. Regenerate that fixture only on purpose,
# with tests/testthat/fixtures/make-label-perf-fixtures.R, and only when the
# pictures are meant to change.
#
# The performance tests are opt-in: they run only when the environment variable
# GGDAG_RUN_PERF_TESTS is set to "1", for example
#   GGDAG_RUN_PERF_TESTS=1 Rscript -e 'devtools::test(filter = "label-auto-perf")'
# following the contract in test-layout-perf.R and test-route-edges-mm.R. They
# time `place_dag_labels()` on the exact inputs the grob hands it, so they
# measure the engine and not the rest of the render.

# Placement invariance ---------------------------------------------------------

test_that("automatic label placement matches the pinned 7 x 5 fixture", {
  skip_on_cran()
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
  # The pinned placements are exact millimetre geometry, so they carry the same
  # platform exposure as the vdiffr baselines: text metrics and the last ulp of
  # the layout arithmetic both differ off macOS. See helper-vdiffr.R.
  skip_on_os(c("windows", "linux", "solaris"))

  fixture <- readRDS(test_path("fixtures", "label-placements-7x5.rds"))
  expect_named(
    fixture,
    as.vector(outer(
      names(perf_label_dags),
      c("straight", "spline"),
      paste,
      sep = "|"
    ))
  )

  for (key in names(fixture)) {
    parts <- strsplit(key, "|", fixed = TRUE)[[1]]
    placed <- perf_scene_placement(parts[[1]], parts[[2]])
    expected <- fixture[[key]]

    expect_equal(
      placed$boxes,
      expected$boxes,
      tolerance = 1e-9,
      label = paste(key, "label boxes"),
      expected.label = paste(key, "pinned label boxes")
    )
    expect_equal(
      placed$leaders,
      expected$leaders,
      tolerance = 1e-9,
      label = paste(key, "leader segments"),
      expected.label = paste(key, "pinned leader segments")
    )
    expect_identical(
      placed$unresolved,
      expected$unresolved,
      label = paste(key, "unresolved labels"),
      expected.label = paste(key, "pinned unresolved labels")
    )
  }
})

# Performance ------------------------------------------------------------------

# The gate every pin below shares.
skip_unless_perf <- function() {
  skip_on_cran()
  skip_on_ci()
  skip_if(
    Sys.getenv("GGDAG_RUN_PERF_TESTS") != "1",
    "GGDAG_RUN_PERF_TESTS is not set to 1"
  )
  skip_if_not_installed("bench")
  skip_if_not_installed("ggarrow")
  skip_if_not_installed("ragg")
}

test_that("place_dag_labels() places a ten-node labelled scene interactively", {
  skip_unless_perf()

  timing <- perf_cached_bench("ten_node", "spline")
  # Ten labels over 751 ink points is the smallest scene a reader is likely to
  # draw with labels on every node. The budget is what placement-identical
  # work in R can reach on this scene, which is what keeps the whole render
  # inside about a tenth of a second.
  expect_lt(
    timing$median,
    0.06,
    label = sprintf(
      "ten-node spline engine median (%.0f ms)",
      timing$median * 1000
    )
  )
})

test_that("place_dag_labels() places a 30-node labelled scene promptly", {
  skip_unless_perf()

  spline <- perf_cached_bench("very_big", "spline")
  straight <- perf_cached_bench("very_big", "straight")
  # Thirty labels over 4,823 ink points under spline routing is the worst
  # scene the engine sees, and 1.5 s is what placement-identical work in R can
  # reach on it. Straight routing does 42% less work, so it gets a
  # proportionately tighter budget.
  expect_lt(
    spline$median,
    1.5,
    label = sprintf(
      "very big spline engine median (%.0f ms)",
      spline$median * 1000
    )
  )
  expect_lt(
    straight$median,
    0.9,
    label = sprintf(
      "very big straight engine median (%.0f ms)",
      straight$median * 1000
    )
  )
})

test_that("place_dag_labels() allocates modestly on a 30-node labelled scene", {
  skip_unless_perf()

  timing <- perf_cached_bench("very_big", "spline")
  # The engine runs at a near-constant 3.4 GB of allocation per second across
  # every problem size measured, so what it allocates is what it costs. This
  # pin is the time pin restated in the currency the engine actually spends.
  # Routed ink gives the placement search more to work against and the
  # engine searches harder on it, which is what the 4.5 GB bound allows for.
  expect_lt(
    timing$mem_alloc,
    4.5 * 1024^3,
    label = sprintf(
      "very big spline engine allocation (%.1f GB)",
      timing$mem_alloc / 1024^3
    )
  )
})
