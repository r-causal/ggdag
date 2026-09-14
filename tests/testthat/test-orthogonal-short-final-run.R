# Three invariants hold over every path the orthogonal router draws, read in
# millimetres from the drawn grobs (see helper-orthogonal-runs.R):
#
# 1. Every ornament is drawn on the run its path ends on. An orthogonal route
#    ends on a straight run into its node, and ggarrow draws the ornament at
#    that end from the point it cuts the path back to, the resection plus the
#    ornament's reach from the end, towards the end. The ornament is drawn on
#    its run only when the run is at least that long; a shorter run puts the
#    cut on the corner or on the run before it, and the head is drawn askew,
#    off the drawn path, and short of or past the gap outside the node's face.
# 2. No path passes within the outline of a node other than the two it runs
#    between, by more than 0.05 mm.
# 3. No two paths share a collinear stretch longer than 1 mm, whichever layer
#    draws each, other than the trunk out of one port and the run into one
#    port, which the router merges on purpose.

# The invariants -------------------------------------------------------------------

test_that("every orthogonal ornament is drawn along the run its path ends on", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )
  p <- fins_orthogonal_plot()

  # On a 7 inch device every head and every fins ornament, at the circles and
  # at the squares, is drawn on its run: the run holds the cut, the ornament
  # points along it, and its tip sits 2 mm outside the face on it.
  found <- orthogonal_invariants(p, width = 7, height = 5)
  ends <- found$ends
  stopifnot(
    sum(ends$end == "head") == 11,
    sum(ends$end == "fins") == 11,
    any(ends$shape == "square" & ends$end == "head"),
    any(ends$shape == "square" & ends$end == "fins")
  )
  expect_equal(found$failures, character())

  # A narrower panel narrows the gaps between the layers, and the runs out of
  # and into the nodes with them, but every ornament is still drawn on its
  # run. On a 4 inch device the runs out of `v`, `x`, and `z1` are 1 mm
  # shorter than the cut of their fins, and the run into `y` from `w1` is
  # 0.4 mm shorter than the cut of its head.
  found <- orthogonal_invariants(p, width = 4, height = 5)
  stopifnot(
    sum(found$ends$end == "head") == 11,
    sum(found$ends$end == "fins") == 11
  )
  expect_equal(found$failures, character())
})

# The README scene ----------------------------------------------------------------

test_that("the README adjustment sets keep the three invariants on every device", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # The README draws its adjustment sets on a 7 by 5 inch device. There every
  # gap between the layers is 11.75 mm wide, narrower than the node discs
  # are, so no slot in a gap clears the discs of both layers, and the routes
  # that hold their heads must leave and enter through the nodes' other
  # faces. The narrower devices narrow the gaps further, and the 10 by 6 inch
  # device widens them to 18 mm, still too narrow for a slot that holds a
  # head.
  devices <- list(
    c(7, 5),
    c(7, 6),
    c(6.5, 6),
    c(6, 5),
    c(5.5, 5),
    c(5, 4),
    c(10, 6)
  )
  for (node_size in c(16, 14)) {
    for (dag_theme in c(FALSE, TRUE)) {
      p <- readme_orthogonal_plot(node_size, dag_theme = dag_theme)
      for (device in devices) {
        label <- sprintf(
          "the README adjustment sets at node size %s%s on a %s by %s inch device",
          node_size,
          if (dag_theme) " with theme_dag()" else "",
          device[[1]],
          device[[2]]
        )
        found <- orthogonal_invariants(
          p,
          width = device[[1]],
          height = device[[2]]
        )
        ends <- found$ends
        stopifnot(
          length(unique(ends$panel)) == 3,
          all(ends$end == "head")
        )
        expect_equal(found$failures, character(), label = label)

        # the edge the README's picture lost: in the panel of the set
        # `{w1, w2, z1}` it was routed through `v` and drawn on top of
        # `v -> z1`, head included
        w1_z1 <- ends[ends$facet == "{w1, w2, z1}" & ends$edge == "w1 -> z1", ]
        expect_equal(nrow(w1_z1), 1, label = paste("w1 -> z1 in", label))
        expect_equal(w1_z1$shape, "square", label = paste("z1 in", label))
        expect_equal(
          orthogonal_run_mismatches(w1_z1),
          character(),
          label = paste("the head of w1 -> z1 in", label)
        )
      }
    }
  }
})

# Breadth -------------------------------------------------------------------------

test_that("the canonical DAGs keep the three invariants", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # When this test was written, one plain scene broke the invariants: the
  # large_epi DAG at node size 30, whose age -> smoking head was drawn from
  # a 2.2 mm run against a 12.76 mm cut, and whose age -> smoking and
  # ses -> smoking ran through the disc of gene. Every other scene here kept
  # them.
  for (name in names(canonical_dag_specs)) {
    for (node_size in c(16, 30)) {
      label <- sprintf(
        "the orthogonal routes of %s at node size %s",
        name,
        node_size
      )
      found <- orthogonal_invariants(canonical_orthogonal_plot(name, node_size))
      stopifnot(nrow(found$ends) == length(canonical_dag_specs[[name]]))
      expect_equal(found$failures, character(), label = label)
    }
  }

  # The adjustment set plots draw their blocked and their open edges in two
  # layers. When this test was written the two were routed apart, so in
  # deep_confound the blocked b -> d and the open c -> d took one slot and
  # were drawn on one line for 16 mm, and in large_epi age -> smoking ran
  # through the square gene.
  for (name in names(canonical_adjustment_roles)) {
    label <- paste("the orthogonal routes of the adjustment sets of", name)
    found <- orthogonal_invariants(canonical_adjustment_plot(name))
    stopifnot(any(found$ends$shape == "square"))
    expect_equal(found$failures, character(), label = label)
  }
})

# No regression -------------------------------------------------------------------

test_that("the scenes that keep the invariants keep their routes", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # Every scene here keeps the three invariants, so a router that changes
  # the routes of scenes that break them must draw these as it did. The
  # routes are pinned in fixtures/orthogonal-final-runs.rds, regenerated
  # only on purpose with tests/testthat/fixtures/make-orthogonal-run-fixtures.R.
  # Doubles must match to within floating-point noise (1e-10).
  fixture <- readRDS(test_path("fixtures", "orthogonal-final-runs.rds"))
  expect_named(fixture, names(orthogonal_run_fixture_scenes))

  for (scene in names(orthogonal_run_fixture_scenes)) {
    spec <- orthogonal_run_fixture_scenes[[scene]]
    p <- spec$plot()
    expect_equal(
      orthogonal_invariant_failures(p, spec$width, spec$height),
      character(),
      label = paste(scene, "invariants")
    )

    current <- routed_path_record(p, spec$width, spec$height)
    expect_equal(
      current$edges,
      fixture[[scene]]$edges,
      tolerance = 1e-10,
      label = paste(scene, "edges and resections")
    )
    expect_equal(
      current$paths,
      fixture[[scene]]$paths,
      tolerance = 1e-10,
      label = paste(scene, "paths")
    )
  }
})

# Visual baseline -----------------------------------------------------------------

test_that("the README's middle panel draws its orthogonal heads on their runs", {
  skip_if_not_installed("ragg")
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  p <- readme_middle_panel_plot()
  found <- orthogonal_invariants(p, width = 7, height = 5)
  stopifnot(
    length(unique(found$ends$panel)) == 1,
    nrow(found$ends) == 11,
    length(found$failures) == 0
  )

  expect_doppelganger("readme middle panel heads on runs", p)
})
