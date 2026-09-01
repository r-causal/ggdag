# Regenerates tests/testthat/fixtures/layout-invariance.rds: the pinned
# order_layers() results and compute_time_ordered_layout() coordinates for
# the 22 canonical DAGs, captured from the current implementation. The
# invariance tests in test-layout-perf.R compare against this file with
# expect_identical(), so regenerate it only when the layout engine's output
# is intended to change.
#
# Run non-interactively from the package root:
#   Rscript tests/testthat/fixtures/make-layout-fixtures.R

pkgload::load_all(".", quiet = TRUE)
source(file.path("tests", "testthat", "helper-canonical-dags.R"))

fixture <- lapply(canonical_dag_specs, function(spec) {
  edges <- canonical_dag_edges(spec)
  inputs <- canonical_ordering_inputs(edges)
  list(
    ordering = canonical_order_layers(inputs),
    coords = compute_time_ordered_layout(edges)
  )
})

path <- file.path("tests", "testthat", "fixtures", "layout-invariance.rds")
saveRDS(fixture, path, version = 3)
cat("Wrote", path, "with", length(fixture), "canonical DAGs.\n")
