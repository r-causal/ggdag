# Regenerates two fixtures of the ggarrow edges the plotters draw, read back
# from the forced grobs on a 7 by 5 inch device: the paths and the resection
# of each end, in millimetres.
#
# tests/testthat/fixtures/ggarrow-default-resects.rds holds the edges
# `ggdag()` draws, at the default node size, for three canonical DAGs drawn
# straight and routed in spline and orthogonal mode. Every node in these
# scenes is a circle of the default size, where an edge that stops 2 mm
# outside the node is resected by the 8 mm the ggarrow engine drew with
# before its resection followed the nodes, so test-edge-resect-node-aware.R
# requires the drawings to match this file to within floating-point noise
# (1e-10). Regenerate it only when the ggarrow edges drawn at the default node
# size are meant to change.
#
# tests/testthat/fixtures/ggarrow-explicit-cap-routes.rds holds the routed
# edges of the explicit-cap scenes in helper-node-edge-ends.R: an explicit
# `edge_cap` fixes every end, so the router is handed that cap for every node
# and routes exactly as it did before the caps followed the nodes, whatever
# size and shape the nodes are drawn at. Regenerate it only when the routes
# drawn under an explicit cap are meant to change.
#
# Run non-interactively from the package root:
#   Rscript tests/testthat/fixtures/make-resect-fixtures.R

pkgload::load_all(".", quiet = TRUE)
source(file.path("tests", "testthat", "helper-canonical-dags.R"))
source(file.path("tests", "testthat", "helper-epidemiology-dag.R"))
source(file.path("tests", "testthat", "helper-node-edge-ends.R"))

options(
  ggdag.edge_cap = NULL,
  ggdag.node_size = NULL,
  ggdag.edge_route = NULL
)

write_fixture <- function(fixture, name) {
  path <- file.path("tests", "testthat", "fixtures", name)
  saveRDS(fixture, path, version = 3)
  cat("Wrote", path, "with", length(fixture), "scenes.\n")
}

write_fixture(default_resect_drawings(), "ggarrow-default-resects.rds")
write_fixture(explicit_cap_drawings(), "ggarrow-explicit-cap-routes.rds")
