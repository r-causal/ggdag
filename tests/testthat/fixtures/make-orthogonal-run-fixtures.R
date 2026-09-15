# Regenerates tests/testthat/fixtures/orthogonal-final-runs.rds, the routed
# ggarrow edges of the scenes in `orthogonal_run_fixture_scenes`
# (helper-orthogonal-runs.R), read back from the forced grobs on the device
# each scene is pinned at: the paths and the resection of each end, in
# millimetres.
#
# The paths of these scenes share no stretch, and test-orthogonal-short-final-run.R
# requires the drawings to match this file to within floating-point noise
# (1e-10): a change that lengthens the runs other scenes end on must leave
# these routes as they are. Regenerate it only when the routes of these
# scenes are meant to change, in a fresh R process, so that the first plot
# drawn is drawn at the resolution the fixture is read at.
#
# Run non-interactively from the package root:
#   Rscript tests/testthat/fixtures/make-orthogonal-run-fixtures.R

pkgload::load_all(".", quiet = TRUE)
source(file.path("tests", "testthat", "helper-canonical-dags.R"))
source(file.path("tests", "testthat", "helper-node-edge-ends.R"))
source(file.path("tests", "testthat", "helper-orthogonal-runs.R"))

options(
  ggdag.edge_cap = NULL,
  ggdag.node_size = NULL,
  ggdag.edge_route = NULL
)

fixture <- orthogonal_run_fixture_drawings()
path <- file.path("tests", "testthat", "fixtures", "orthogonal-final-runs.rds")
saveRDS(fixture, path, version = 3)
cat("Wrote", path, "with", length(fixture), "scenes.\n")
