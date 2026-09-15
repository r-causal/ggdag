# Regenerates tests/testthat/fixtures/route-invariance.rds: the paths and
# meta that route_edges_mm() draws for the 22 canonical DAGs in spline and
# orthogonal mode, captured from the current implementation. None of those
# graphs holds an anti-parallel pair, so the reciprocal-end work must leave
# every one of them untouched; test-route-reciprocal-ends.R compares against
# this file, requiring the doubles to match to within floating-point noise
# (1e-10) and every other field to match exactly. Regenerate it only when the
# router's output for a graph without a reciprocal pair is meant to change.
#
# Run non-interactively from the package root:
#   Rscript tests/testthat/fixtures/make-route-fixtures.R

pkgload::load_all(".", quiet = TRUE)
source(file.path("tests", "testthat", "helper-canonical-dags.R"))
source(file.path("tests", "testthat", "helper-route-scenes.R"))

fixture <- lapply(canonical_dag_specs, function(spec) {
  scene <- canonical_mm_scene(spec)
  lapply(c("spline", "orthogonal"), function(mode) {
    routed <- route_edges_mm(
      scene$nodes,
      scene$edges,
      scene$bounds,
      cap = 8,
      mode = mode,
      opts = route_constants(6)
    )
    list(paths = routed$paths, meta = routed$meta)
  })
})

path <- file.path("tests", "testthat", "fixtures", "route-invariance.rds")
saveRDS(fixture, path, version = 3)
cat("Wrote", path, "with", length(fixture), "canonical DAGs.\n")
