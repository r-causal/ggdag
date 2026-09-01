# Output invariance and performance pins for the layer-ordering engine.
#
# The invariance tests pin the exact current output of order_layers() and
# compute_time_ordered_layout() on the 22 canonical DAGs against
# fixtures/layout-invariance.rds, regenerated only on purpose with
# tests/testthat/fixtures/make-layout-fixtures.R. Any optimization of the
# ordering engine or the wired layout path must reproduce this output
# identically.
#
# The performance tests are opt-in: they run only when the environment
# variable GGDAG_RUN_PERF_TESTS is set to a non-empty value, for example
#   GGDAG_RUN_PERF_TESTS=1 Rscript -e 'devtools::test(filter = "layout-perf")'
# They pin interactive speed on large_epi, the largest canonical DAG:
# order_layers() under 40 ms and compute_time_ordered_layout() under 100 ms
# (median over bench::mark() iterations).

# Invariance -------------------------------------------------------------------

test_that("order_layers is deterministic and matches the pinned fixture", {
  fixture <- readRDS(test_path("fixtures", "layout-invariance.rds"))
  expect_identical(names(fixture), names(canonical_dag_specs))

  for (nm in names(canonical_dag_specs)) {
    edges <- canonical_dag_edges(canonical_dag_specs[[nm]])
    inputs <- canonical_ordering_inputs(edges)

    first <- canonical_order_layers(inputs)
    second <- canonical_order_layers(inputs)

    expect_identical(
      first,
      second,
      label = paste0(nm, ": first order_layers() run"),
      expected.label = paste0(nm, ": second order_layers() run")
    )
    expect_identical(
      first,
      fixture[[nm]]$ordering,
      label = paste0(nm, ": order_layers() output"),
      expected.label = paste0(nm, ": pinned ordering fixture")
    )
  }
})

test_that("time-ordered coordinates are deterministic and match the fixture", {
  fixture <- readRDS(test_path("fixtures", "layout-invariance.rds"))

  for (nm in names(canonical_dag_specs)) {
    edges <- canonical_dag_edges(canonical_dag_specs[[nm]])

    first <- compute_time_ordered_layout(edges)
    second <- compute_time_ordered_layout(edges)

    expect_identical(
      first,
      second,
      label = paste0(nm, ": first layout run"),
      expected.label = paste0(nm, ": second layout run")
    )
    expect_identical(
      first,
      fixture[[nm]]$coords,
      label = paste0(nm, ": layout coordinates"),
      expected.label = paste0(nm, ": pinned coordinate fixture")
    )
  }
})

# Performance ------------------------------------------------------------------

test_that("order_layers reaches interactive speed on large_epi", {
  skip_on_cran()
  skip_on_ci()
  # Opt-in pin; see the header comment for the GGDAG_RUN_PERF_TESTS contract.
  skip_if(
    Sys.getenv("GGDAG_RUN_PERF_TESTS") == "",
    "GGDAG_RUN_PERF_TESTS is not set"
  )
  skip_if_not_installed("bench")

  edges <- canonical_dag_edges(canonical_dag_specs$large_epi)
  inputs <- canonical_ordering_inputs(edges)

  timing <- bench::mark(
    canonical_order_layers(inputs),
    iterations = 10,
    filter_gc = FALSE
  )
  expect_lt(as.numeric(timing$median), 0.040)
})

test_that("compute_time_ordered_layout reaches interactive speed on large_epi", {
  skip_on_cran()
  skip_on_ci()
  # Opt-in pin; see the header comment for the GGDAG_RUN_PERF_TESTS contract.
  skip_if(
    Sys.getenv("GGDAG_RUN_PERF_TESTS") == "",
    "GGDAG_RUN_PERF_TESTS is not set"
  )
  skip_if_not_installed("bench")

  edges <- canonical_dag_edges(canonical_dag_specs$large_epi)

  timing <- bench::mark(
    compute_time_ordered_layout(edges),
    iterations = 10,
    filter_gc = FALSE
  )
  expect_lt(as.numeric(timing$median), 0.100)
})
