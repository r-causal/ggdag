# An orthogonal route ends on a straight run into its node, and ggarrow draws
# the ornament at that end from the point it cuts the path back to, the
# resection plus the ornament's reach from the end, towards the end. Every
# ornament is therefore drawn on its run only when the run is at least that
# long; a shorter run puts the cut on the corner or on the run before it, and
# the head is drawn askew, off the drawn path, and short of or past the gap
# outside the node's face. See helper-orthogonal-runs.R.

# The invariant -------------------------------------------------------------------

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
  ends <- orthogonal_run_ends(p, width = 7, height = 5)
  stopifnot(
    sum(ends$end == "head") == 11,
    sum(ends$end == "fins") == 11,
    any(ends$shape == "square" & ends$end == "head"),
    any(ends$shape == "square" & ends$end == "fins")
  )
  expect_equal(orthogonal_run_mismatches(ends), character())

  # A narrower panel narrows the gaps between the layers, and the runs out of
  # and into the nodes with them, but every ornament is still drawn on its
  # run. On a 4 inch device the runs out of `v`, `x`, and `z1` are 1 mm
  # shorter than the cut of their fins, and the run into `y` from `w1` is
  # 0.4 mm shorter than the cut of its head.
  ends <- orthogonal_run_ends(p, width = 4, height = 5)
  stopifnot(
    sum(ends$end == "head") == 11,
    sum(ends$end == "fins") == 11
  )
  expect_equal(orthogonal_run_mismatches(ends), character())
})

# The README scene ----------------------------------------------------------------

test_that("the README adjustment sets draw every orthogonal head on its final run", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  devices <- list(c(7, 5), c(10, 6))
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
        ends <- orthogonal_run_ends(
          p,
          width = device[[1]],
          height = device[[2]]
        )
        stopifnot(
          length(unique(ends$panel)) == 3,
          all(ends$end == "head")
        )
        expect_equal(
          orthogonal_run_mismatches(ends),
          character(),
          label = label
        )

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

test_that("the head of w1 -> z1 into the square z1 is drawn on its final run where the gap before z1 is narrow", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # On these devices the gap between the layers of w1 and z1 in the panel of
  # the set `{w1, w2, z1}` is too narrow for the run into z1 to hold the
  # resection and the head, and the head of w1 -> z1 is drawn from a cut on
  # the corner: 9.45 mm of run against a 9.76 mm cut on a 7 by 6 inch device,
  # and 8.51 mm against 9.76 mm, with the tip 2.4 mm off the run, on a 6.5 by
  # 6 inch device.
  p <- readme_orthogonal_plot()
  for (device in list(c(7, 6), c(6.5, 6))) {
    label <- sprintf(
      "the head of w1 -> z1 in the README adjustment sets on a %s by %s inch device",
      device[[1]],
      device[[2]]
    )
    ends <- orthogonal_run_ends(p, width = device[[1]], height = device[[2]])
    w1_z1 <- ends[ends$facet == "{w1, w2, z1}" & ends$edge == "w1 -> z1", ]
    stopifnot(nrow(w1_z1) == 1, w1_z1$shape == "square")

    expect_equal(orthogonal_run_mismatches(w1_z1), character(), label = label)
  }
})

# Breadth -------------------------------------------------------------------------

test_that("the canonical DAGs draw every orthogonal head on its final run", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # When this test was written, one scene drew a head off its run: the
  # large_epi DAG at node size 30, whose age -> smoking head was drawn from a
  # 2.2 mm run against a 12.76 mm cut. Every other scene here drew each of
  # its heads on its run.
  for (name in names(canonical_dag_specs)) {
    for (node_size in c(16, 30)) {
      label <- sprintf(
        "the orthogonal heads of %s at node size %s",
        name,
        node_size
      )
      ends <- orthogonal_run_ends(canonical_orthogonal_plot(name, node_size))
      stopifnot(nrow(ends) == length(canonical_dag_specs[[name]]))
      expect_equal(orthogonal_run_mismatches(ends), character(), label = label)
    }
  }

  for (name in names(canonical_adjustment_roles)) {
    label <- paste("the orthogonal heads of the adjustment sets of", name)
    ends <- orthogonal_run_ends(canonical_adjustment_plot(name))
    stopifnot(any(ends$shape == "square"))
    expect_equal(orthogonal_run_mismatches(ends), character(), label = label)
  }
})

# No regression -------------------------------------------------------------------

test_that("the scenes that draw every ornament on its run keep their routes", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # Every orthogonal ornament in these scenes is drawn on its run, so a
  # router that lengthens the runs elsewhere must draw them as it did. The
  # routes are pinned in fixtures/orthogonal-final-runs.rds, regenerated only
  # on purpose with tests/testthat/fixtures/make-orthogonal-run-fixtures.R.
  # Doubles must match to within floating-point noise (1e-10).
  fixture <- readRDS(test_path("fixtures", "orthogonal-final-runs.rds"))
  expect_named(fixture, names(orthogonal_run_fixture_scenes))

  for (scene in names(orthogonal_run_fixture_scenes)) {
    spec <- orthogonal_run_fixture_scenes[[scene]]
    p <- spec$plot()
    ends <- orthogonal_run_ends(p, width = spec$width, height = spec$height)
    stopifnot(length(orthogonal_run_mismatches(ends)) == 0)

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
  ends <- orthogonal_run_ends(p, width = 7, height = 5)
  stopifnot(
    length(unique(ends$panel)) == 1,
    nrow(ends) == 11,
    length(orthogonal_run_mismatches(ends)) == 0
  )

  expect_doppelganger("readme middle panel heads on runs", p)
})
