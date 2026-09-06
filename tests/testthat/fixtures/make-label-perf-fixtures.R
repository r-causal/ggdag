# Regenerates tests/testthat/fixtures/label-placements-7x5.rds: the label
# placement each scene in helper-label-perf.R draws at 7 x 5 inches under the
# ggarrow engine, for both the straight and the spline edge routes. The
# placement test in test-label-auto-perf.R recomputes these and compares, so
# any optimisation of the placement engine has to reproduce them exactly.
#
# Regenerate only when the placement is meant to change, which is a decision
# about how the pictures look, not about how fast they are drawn.
#
# Run non-interactively from the package root:
#   Rscript tests/testthat/fixtures/make-label-perf-fixtures.R

pkgload::load_all(".", quiet = TRUE)

# The suite-wide state from helper-load_dag.R, which the tests run under.
set.seed(1234)
options(ggdag.layout = "time_ordered")

source(file.path("tests", "testthat", "helper-label-perf.R"))

fixture <- list()
for (route in c("straight", "spline")) {
  for (scene in names(perf_label_dags)) {
    fixture[[paste(scene, route, sep = "|")]] <- perf_scene_placement(
      scene,
      route
    )
  }
}

path <- file.path("tests", "testthat", "fixtures", "label-placements-7x5.rds")
saveRDS(fixture, path, version = 3)
cat("Wrote", path, "with", length(fixture), "placements:\n")
for (nm in names(fixture)) {
  placement <- fixture[[nm]]
  cat(sprintf(
    "  %-20s labels=%2d leaders=%2d unresolved=%d\n",
    nm,
    nrow(placement$boxes),
    nrow(placement$leaders),
    length(placement$unresolved)
  ))
}
