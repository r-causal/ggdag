# Regenerates tests/testthat/fixtures/ggarrow-default-resects.rds: the paths
# and resections of the ggarrow edges `ggdag()` draws, at the default node
# size, for three canonical DAGs drawn straight and routed in spline and
# orthogonal mode, on a 7 by 5 inch device. Every node in these scenes is a
# circle of the default size, where an edge that stops 2 mm outside the node
# is resected by the 8 mm the ggarrow engine drew with before its resection
# followed the nodes, so test-edge-resect-node-aware.R requires the drawings
# to match this file to within floating-point noise (1e-10). Regenerate it
# only when the ggarrow edges drawn at the default node size are meant to
# change.
#
# Run non-interactively from the package root:
#   Rscript tests/testthat/fixtures/make-resect-fixtures.R

pkgload::load_all(".", quiet = TRUE)
source(file.path("tests", "testthat", "helper-canonical-dags.R"))
source(file.path("tests", "testthat", "helper-node-edge-ends.R"))

options(
  ggdag.edge_cap = NULL,
  ggdag.node_size = NULL,
  ggdag.edge_route = NULL
)
fixture <- default_resect_drawings()

path <- file.path(
  "tests",
  "testthat",
  "fixtures",
  "ggarrow-default-resects.rds"
)
saveRDS(fixture, path, version = 3)
cat("Wrote", path, "with", length(fixture), "scenes.\n")
