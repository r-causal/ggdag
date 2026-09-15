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
#    port that the router merges on purpose for the paths of one layer.
#
# The first two do not hold where the gap between two layers is narrower
# than a node's cap, the reach of its head, and a corner: no vertical run in
# such a gap clears the nodes of both layers and still leaves the run into a
# head long enough for it. The scenes and devices where they fail are pinned
# as known failures below; the third holds everywhere.

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

# The README adjustment set plot at each node size, with and without
# theme_dag(), on each device, as one label.
readme_scene_label <- function(node_size, dag_theme, device) {
  sprintf(
    "the README adjustment sets at node size %s%s on a %s by %s inch device",
    node_size,
    if (dag_theme) " with theme_dag()" else "",
    device[[1]],
    device[[2]]
  )
}

readme_devices <- list(
  c(7, 5),
  c(7, 6),
  c(6.5, 6),
  c(6, 5),
  c(5.5, 5),
  c(5, 4),
  c(10, 6)
)

test_that("the README adjustment sets keep their edges apart on every device", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # The README draws its adjustment sets on a 7 by 5 inch device. There every
  # gap between the layers is 11.75 mm wide, narrower than the node discs
  # are, and the blocked and the open edges are drawn by two layers. Each
  # layer was once routed without the other, so `w1 -> z1`, blocked in the
  # panel of the set `{w1, w2, z1}`, was drawn on the slot and the row of the
  # open `v -> z1`, head included, and every arrival out of a narrow gap was
  # merged onto its target's centre row whichever layer drew it. The layers
  # now route one scene, and no edge of one is drawn on a run of the other.
  #
  # Where the gaps leave room for them, the first two invariants hold as
  # well: every ornament is on its run on the 10 by 6 inch device, and at
  # node size 14 with theme_dag() on the 7 by 5 and 7 by 6 inch devices, and
  # no path passes within a node at node size 14 with theme_dag() on the 10
  # by 6 inch device. Elsewhere they are known failures, pinned below.
  off_run_holds <- function(node_size, dag_theme, device) {
    identical(device, c(10, 6)) ||
      (node_size == 14 &&
        dag_theme &&
        (identical(device, c(7, 5)) || identical(device, c(7, 6))))
  }
  inside_holds <- function(node_size, dag_theme, device) {
    node_size == 14 && dag_theme && identical(device, c(10, 6))
  }

  for (node_size in c(16, 14)) {
    for (dag_theme in c(FALSE, TRUE)) {
      p <- readme_orthogonal_plot(node_size, dag_theme = dag_theme)
      for (device in readme_devices) {
        label <- readme_scene_label(node_size, dag_theme, device)
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
        expect_equal(found$shared, character(), label = label)
        if (off_run_holds(node_size, dag_theme, device)) {
          expect_equal(found$off_run, character(), label = label)
        }
        if (inside_holds(node_size, dag_theme, device)) {
          expect_equal(found$inside, character(), label = label)
        }

        # the edge the README's picture lost is drawn, at its square
        w1_z1 <- ends[ends$facet == "{w1, w2, z1}" & ends$edge == "w1 -> z1", ]
        expect_equal(nrow(w1_z1), 1, label = paste("w1 -> z1 in", label))
        expect_equal(w1_z1$shape, "square", label = paste("z1 in", label))

        # and on the device the README draws on, no edge in any panel has
        # its head covered by another edge's head or run
        if (identical(device, c(7, 5))) {
          expect_equal(
            found$covered,
            character(),
            label = paste("the heads of", label)
          )
        }
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

  # Every plain scene keeps the three invariants but the large_epi DAG at
  # node size 30, whose gaps are too narrow for its nodes: its age -> smoking
  # head is drawn from a 2.2 mm run against a 12.76 mm cut, and its
  # age -> smoking and ses -> smoking run through the disc of gene. Those
  # are known failures; its paths still share no stretch.
  for (name in names(canonical_dag_specs)) {
    for (node_size in c(16, 30)) {
      label <- sprintf(
        "the orthogonal routes of %s at node size %s",
        name,
        node_size
      )
      found <- orthogonal_invariants(canonical_orthogonal_plot(name, node_size))
      stopifnot(nrow(found$ends) == length(canonical_dag_specs[[name]]))
      if (name == "large_epi" && node_size == 30) {
        expect_equal(found$shared, character(), label = label)
      } else {
        expect_equal(found$failures, character(), label = label)
      }
    }
  }

  # The adjustment set plots draw their blocked and their open edges in two
  # layers. When this test was written the two were routed apart, so in
  # deep_confound the blocked b -> d and the open c -> d took one slot and
  # were drawn on one line for 16 mm. Routed as one scene, no two of their
  # edges share a stretch. In deep_confound the run into the head of u -> b
  # is 0.53 mm shorter than its cut, and in large_epi age -> smoking runs
  # 0.75 mm inside the square gene: both are known failures.
  known <- list(
    epidemiology = character(),
    deep_confound = "off_run",
    large_epi = "inside"
  )
  for (name in names(canonical_adjustment_roles)) {
    label <- paste("the orthogonal routes of the adjustment sets of", name)
    found <- orthogonal_invariants(canonical_adjustment_plot(name))
    stopifnot(any(found$ends$shape == "square"))
    for (invariant in setdiff(
      c("off_run", "inside", "shared"),
      known[[name]]
    )) {
      expect_equal(
        found[[invariant]],
        character(),
        label = paste(label, invariant)
      )
    }
  }
})

# Known failures ------------------------------------------------------------------

test_that("gaps too narrow for a cap, a head, and a corner break the first two invariants where recorded", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # A gap between two layers narrower than a node's cap plus the reach of its
  # head plus a corner cannot hold a vertical run that clears the nodes of
  # both layers and still leaves the run into a head long enough for it. The
  # router places the run anyway, so edges are drawn through the nodes beside
  # the gap (invariant 2) and heads are drawn askew off short runs
  # (invariant 1). These are the numbers of such failures in every scene
  # where the first two invariants break, as the router draws them now. A
  # change to the spacing of narrow gaps that clears some of them should
  # lower these counts, and the table is to be updated with it: a count that
  # rises is a regression.
  readme <- data.frame(
    node_size = rep(c(16, 16, 14, 14), each = 7),
    dag_theme = rep(c(FALSE, TRUE, FALSE, TRUE), each = 7),
    device = rep(seq_along(readme_devices), 4),
    off_run = c(
      c(11, 12, 12, 18, 21, 24, 0),
      c(3, 3, 12, 12, 18, 21, 0),
      c(6, 6, 12, 15, 18, 24, 0),
      c(0, 0, 6, 12, 13, 18, 0)
    ),
    inside = c(
      c(12, 12, 12, 12, 12, 12, 12),
      c(12, 12, 12, 12, 12, 12, 9),
      c(12, 12, 12, 12, 12, 12, 9),
      c(12, 12, 12, 12, 12, 12, 0)
    )
  )
  readme <- readme[readme$off_run > 0 | readme$inside > 0, , drop = FALSE]

  for (i in seq_len(nrow(readme))) {
    row <- readme[i, ]
    device <- readme_devices[[row$device]]
    label <- readme_scene_label(row$node_size, row$dag_theme, device)
    found <- orthogonal_invariants(
      readme_orthogonal_plot(row$node_size, dag_theme = row$dag_theme),
      width = device[[1]],
      height = device[[2]]
    )
    expect_equal(
      length(found$off_run),
      row$off_run,
      label = paste("the ornaments off their runs in", label)
    )
    expect_equal(
      length(found$inside),
      row$inside,
      label = paste("the paths within a node in", label)
    )
  }

  scenes <- list(
    list(
      label = "large_epi at node size 30",
      plot = \() canonical_orthogonal_plot("large_epi", 30),
      off_run = 1,
      inside = 2
    ),
    list(
      label = "the adjustment sets of deep_confound",
      plot = \() canonical_adjustment_plot("deep_confound"),
      off_run = 2,
      inside = 0
    ),
    list(
      label = "the adjustment sets of large_epi",
      plot = \() canonical_adjustment_plot("large_epi"),
      off_run = 0,
      inside = 1
    )
  )
  for (scene in scenes) {
    found <- orthogonal_invariants(scene$plot())
    expect_equal(
      length(found$off_run),
      scene$off_run,
      label = paste("the ornaments off their runs in", scene$label)
    )
    expect_equal(
      length(found$inside),
      scene$inside,
      label = paste("the paths within a node in", scene$label)
    )
  }
})

# What the router is told ---------------------------------------------------------

test_that("the router keeps room for the reach of the ornaments ggarrow draws", {
  # a shaft of line width 1, in millimetres
  width <- ggplot2::.pt / ggplot2::.stroke

  # a matrix ornament reaches as far as ggarrow cuts the path back for it,
  # which the test helpers model from the shape itself
  wings <- ggarrow::arrow_head_wings()
  feather <- ggarrow::arrow_fins_feather()
  expect_equal(
    arrow_ornament_reach_mm(wings, 4, width),
    ornament_reach_mm(wings, 4 * width, width)
  )
  expect_equal(
    arrow_ornament_reach_mm(feather, 4, width),
    ornament_reach_mm(feather, 4 * width, width)
  )
  expect_equal(round(arrow_ornament_reach_mm(wings, 4, width), 3), 3.011)
  expect_equal(round(arrow_ornament_reach_mm(wings, 3, 2 * width), 3), 4.517)

  # a length in absolute units is read in millimetres without a device, the
  # longest of several standing for them all
  expect_equal(
    arrow_ornament_reach_mm(wings, grid::unit(5, "pt"), width),
    arrow_ornament_reach_mm(wings, 5 * 25.4 / 72.27 / width, width)
  )
  expect_equal(unit_length_mm(grid::unit(c(2, 5), c("pt", "mm"))), 5)

  # the part of the ornament past the cut is `1 - justify` of it; no
  # ornament reaches nowhere, and one per edge reaches as far as the
  # farthest
  expect_equal(
    arrow_ornament_reach_mm(wings, 4, width, justify = 0.5),
    arrow_ornament_reach_mm(wings, 4, width) / 2
  )
  expect_equal(arrow_ornament_reach_mm(NULL, 4, width), 0)
  expect_equal(
    arrow_ornament_reach_mm(list(wings, NULL), 6, width),
    arrow_ornament_reach_mm(wings, 6, width)
  )

  # a layer's widest shaft carries its largest ornaments
  reach <- routed_ornament_reaches(
    list(
      arrow = list(head = wings, fins = NULL),
      length = list(head = 4, fins = 4),
      justify = 0
    ),
    c(0.5, 1, NA)
  )
  expect_equal(reach$head, arrow_ornament_reach_mm(wings, 4, width))
  expect_equal(reach$fins, 0)

  # the router is handed both reaches, and assumes a 2 mm head and no fins
  # when it is handed none; the 2 mm head the spline router keeps other edges
  # clear of stays as it is
  opts <- route_opts_from(NULL, 6, head_reach = 3.5, fins_reach = 1)
  expect_equal(c(opts$head, opts$head_reach, opts$fins_reach), c(2, 3.5, 1))
  opts <- route_opts_from(NULL, 6)
  expect_equal(c(opts$head, opts$head_reach, opts$fins_reach), c(2, 2, 0))
})

test_that("a length in absolute units is measured without opening a device", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # The adjustment set plotters give their heads a length in points. The
  # label engine measures the reach of those heads when the plot is built,
  # before any device is open, and converting the length through grid there
  # would open the default device, which writes Rplots.pdf.
  skip_if(grDevices::dev.cur() != 1L, "a graphics device is already open")
  withr::local_dir(withr::local_tempdir())
  p <- readme_orthogonal_plot() + geom_dag_text_auto(colour = "black")
  built <- ggplot2::ggplot_build(p)
  expect_s3_class(built, "ggplot_built")
  expect_equal(grDevices::dev.cur(), c("null device" = 1L))
  expect_false(file.exists("Rplots.pdf"))
})

test_that("the arrivals out of a narrow gap take rows when the scene holds several layers", {
  # Three sources in one layer and their common target in the next, 10 mm
  # apart: too narrow a gap for a stub. The router merges the three arrivals
  # onto the target's centre row, as one layer draws them; told the scene
  # holds the edges of several layers, it gives each a row of its own, since
  # an edge merged into the run of another layer's edge is drawn under it.
  nodes <- data.frame(
    name = c("a", "b", "c", "t"),
    x = c(0, 0, 0, 10),
    y = c(0, 24, 48, 12),
    r = 6
  )
  edges <- data.frame(from = c("a", "b", "c"), to = "t")
  arrival_rows <- function(narrow_rows) {
    routed <- route_edges_mm(
      nodes,
      edges,
      c(-20, -20, 40, 70),
      cap = 8,
      mode = "orthogonal",
      opts = route_opts_from(NULL, 6, narrow_rows = narrow_rows)
    )
    stopifnot(routed$ortho$gaps$rung == 4)
    vapply(routed$paths, \(path) path$y[[nrow(path)]], numeric(1))
  }

  expect_equal(arrival_rows(FALSE), c(12, 12, 12))
  rows <- arrival_rows(TRUE)
  expect_equal(sort(rows), c(8.4, 12, 15.6))
  expect_false(route_opts_from(NULL, 6)$narrow_rows)
})

test_that("orthogonal routed layers with the same settings route one scene", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # The README DAG's edges drawn by one routed layer, and split between two
  # layers with the same settings: the edges out of `w1` and `w2` in one and
  # the rest in the other. Each layer routes the other's edges with its own
  # and draws only its own, so the two draw the paths the one layer draws.
  # Routed apart, each would share the rows and the slots of the gaps out
  # afresh, and draw two routes on one line. No gap here is too narrow for a
  # stub, where the two layers would also keep their arrivals off one row.
  dag <- tidy_dagitty(readme_time_ordered_dag())
  directed <- filter_direction("->")
  out_of_w <- function(x) dplyr::filter(directed(x), name %in% c("w1", "w2"))
  rest <- function(x) dplyr::filter(directed(x), !name %in% c("w1", "w2"))
  base <- ggplot2::ggplot(dag, aes_dag()) +
    geom_dag_point(size = 16) +
    theme_dag()
  one <- base + geom_dag_routed_arrows(route = "orthogonal")
  two <- base +
    geom_dag_routed_arrows(route = "orthogonal", data_directed = out_of_w) +
    geom_dag_routed_arrows(route = "orthogonal", data_directed = rest)

  drawn_one <- routed_path_record(one, 7, 5)
  drawn_two <- routed_path_record(two, 7, 5)
  stopifnot(nrow(drawn_one$edges) == 11)
  expect_equal(drawn_two, drawn_one, tolerance = 1e-10)
  expect_equal(orthogonal_invariant_failures(two, 7, 5), character())
})

test_that("the label engine routes the orthogonal layers of a plot as they are drawn", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # The automatic labels keep clear of the edges by routing them again, so
  # the label grob has to route the scene the two routed layers of the
  # adjustment set plot share, rows for the arrivals out of narrow gaps
  # included, and trace the paths they draw.
  router <- get("route_edges_mm", envir = asNamespace("ggdag"))
  p <- readme_orthogonal_plot() + geom_dag_text_auto(colour = "black")
  for (device in list(c(7, 5), c(10, 6))) {
    calls <- list()
    record <- function(nodes, edges, ...) {
      caller <- paste(deparse(sys.call(-1)[[1]]), collapse = "")
      routed <- router(nodes, edges, ...)
      calls[[length(calls) + 1L]] <<- list(
        grob = if (grepl("route_label_obstacles", caller)) {
          "labels"
        } else {
          "edges"
        },
        nodes = nodes,
        edges = paste(edges$from, edges$to),
        paths = routed$paths
      )
      routed
    }
    testthat::with_mocked_bindings(
      with_forced_plot(
        p,
        \(built) NULL,
        width = device[[1]],
        height = device[[2]]
      ),
      route_edges_mm = record,
      .package = "ggdag"
    )
    grobs <- vapply(calls, `[[`, character(1), "grob")
    traced <- calls[grobs == "labels"]
    drawn <- calls[grobs == "edges"]
    stopifnot(length(traced) >= 3, length(drawn) >= 6)

    for (trace in traced) {
      same_scene <- Filter(
        \(call) {
          setequal(call$edges, trace$edges) &&
            isTRUE(all.equal(call$nodes$x, trace$nodes$x)) &&
            isTRUE(all.equal(call$nodes$y, trace$nodes$y))
        },
        drawn
      )
      expect_gte(length(same_scene), 2)
      for (call in same_scene) {
        expect_equal(
          call$paths[match(trace$edges, call$edges)],
          trace$paths,
          tolerance = 1e-10,
          label = sprintf(
            "the paths drawn on a %s by %s inch device",
            device[[1]],
            device[[2]]
          )
        )
      }
    }
  }
})

test_that("orthogonal routed layers whose ornaments reach apart route one scene", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # The stub a gap keeps behind a head holds the reach of the head, which
  # grows with the line width and the head's length. Two layers whose heads
  # reach apart and whose settings are otherwise alike still route one
  # scene, with the stubs of the longer reach: routed apart, each shares out
  # the rows and slots of the gaps afresh, and a 0.5 and a 2 wide layer drew
  # three runs on one line on a 7 by 5 inch device, the longest 24.7 mm, and
  # five on a 4 by 4 inch device. Each layer hands the router the same scene,
  # the two share no run, and the label engine traces what they draw. The
  # 6 mm heads of the second pair reach far enough that on a 4 by 4 inch
  # device their layer alone draws `w2 -> z2` and `z1 -> x` along 1.17 mm of
  # one run, so that pair keeps its runs apart on the larger devices only.
  ortho <- list(route = "orthogonal")
  pairs <- list(
    `different line widths` = list(
      c(ortho, list(linewidth = 0.5)),
      c(ortho, list(linewidth = 2))
    ),
    `heads of different shapes and lengths` = list(
      c(ortho, list(length = 3)),
      c(
        ortho,
        list(arrow_head = ggarrow::arrow_head_wings(offset = 30), length = 6)
      )
    )
  )

  for (name in names(pairs)) {
    pair <- pairs[[name]]
    for (device in list(c(7, 5), c(5, 4), c(4, 4))) {
      label <- sprintf(
        "%s on a %s by %s inch device",
        name,
        device[[1]],
        device[[2]]
      )
      plot <- readme_layer_pair_plot(pair[[1]], pair[[2]], labels = FALSE)
      inputs <- routed_router_inputs(plot, device[[1]], device[[2]])
      stopifnot(length(inputs) == 4)
      for (input in inputs[-1]) {
        expect_identical(input, inputs[[1]], label = label)
      }
      if (name == "different line widths" || !identical(device, c(4, 4))) {
        expect_equal(
          orthogonal_invariants(plot, device[[1]], device[[2]])$shared,
          character(),
          label = label
        )
      }
    }
    for (device in list(c(7, 5), c(4, 4))) {
      label <- sprintf(
        "the routes traced for %s on a %s by %s inch device",
        name,
        device[[1]],
        device[[2]]
      )
      found <- label_route_deviations(
        readme_layer_pair_plot(pair[[1]], pair[[2]]),
        device[[1]],
        device[[2]]
      )
      stopifnot(nrow(found) == 11)
      expect_equal(found$traced_points, found$drawn_points, label = label)
      expect_equal(
        found$deviation,
        rep(0, nrow(found)),
        tolerance = 1e-10,
        label = label
      )
    }
  }
})

test_that("orthogonal routed layers of one scene route each edge as its own layer does", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # Two layers of one scene each route the other's edges with their own, so
  # every edge reaches the router as the layer that draws it resolves it: a
  # head resection or a curvature one layer maps is read from that layer's
  # mapping, and the other layer's mapping, or its lack of one, never applies
  # to it. Otherwise the two copies of the scene differ, and each draws its
  # edges off rows and slots the other did not share out.
  ortho <- list(route = "orthogonal")
  pairs <- list(
    `a head resection mapped by one layer` = list(
      c(
        ortho,
        list(
          mapping = ggplot2::aes(resect_head = ifelse(name == "w1", 10, 6))
        )
      ),
      ortho
    ),
    `a curvature mapped by one layer` = list(
      c(
        ortho,
        list(
          mapping = ggplot2::aes(
            edge_curvature = ifelse(
              name %in% c("w1", "z1") & to == "x",
              0.3,
              NA
            )
          )
        )
      ),
      ortho
    )
  )

  for (name in names(pairs)) {
    pair <- pairs[[name]]
    for (device in list(c(7, 5), c(5, 4))) {
      label <- sprintf(
        "%s on a %s by %s inch device",
        name,
        device[[1]],
        device[[2]]
      )
      plot <- readme_layer_pair_plot(pair[[1]], pair[[2]], labels = FALSE)
      inputs <- routed_router_inputs(plot, device[[1]], device[[2]])
      stopifnot(length(inputs) == 4)
      for (input in inputs[-1]) {
        expect_identical(input, inputs[[1]], label = label)
      }
      expect_equal(
        orthogonal_invariants(plot, device[[1]], device[[2]])$shared,
        character(),
        label = label
      )
      found <- label_route_deviations(
        readme_layer_pair_plot(pair[[1]], pair[[2]]),
        device[[1]],
        device[[2]]
      )
      expect_equal(
        found$deviation,
        rep(0, nrow(found)),
        tolerance = 1e-10,
        label = paste("the routes traced for", label)
      )
    }
  }
})

test_that("the label engine traces the routes every pair of routed layers draws", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # The automatic labels keep clear of the edges by routing them again, so
  # the label grob has to route exactly the scenes the routed layers draw:
  # one scene for orthogonal layers with the same settings, and a scene per
  # layer otherwise. A reach the label grob measured apart from the drawn
  # grob, from a line width the layer maps or a head length relative to the
  # panel, would route the same scene to other rows.
  ortho <- list(route = "orthogonal")
  pairs <- list(
    `the same settings` = list(ortho, ortho),
    `heads of different shapes and equal reach` = list(
      c(ortho, list(arrow_head = ggarrow::arrow_head_wings(offset = 20))),
      c(ortho, list(arrow_head = ggarrow::arrow_head_wings(offset = 30)))
    ),
    `a node size given to one layer and found by the other` = list(
      c(ortho, list(node_size = 16)),
      ortho
    ),
    `different line widths` = list(
      c(ortho, list(linewidth = 0.5)),
      c(ortho, list(linewidth = 2))
    ),
    `a line width mapped by one layer` = list(
      c(
        ortho,
        list(mapping = ggplot2::aes(linewidth = I(ifelse(name == "z1", 2, 1))))
      ),
      ortho
    ),
    `a head length relative to the panel` = list(
      c(ortho, list(length = grid::unit(0.03, "npc"))),
      c(ortho, list(length = grid::unit(0.03, "npc")))
    )
  )
  plots <- purrr::map(pairs, \(pair) {
    readme_layer_pair_plot(pair[[1]], pair[[2]])
  })
  plots$`two spline layers` <- spline_layer_pair_plot()

  for (name in names(plots)) {
    for (device in list(c(7, 5), c(4, 4))) {
      found <- label_route_deviations(plots[[name]], device[[1]], device[[2]])
      label <- sprintf(
        "the routes traced for %s on a %s by %s inch device",
        name,
        device[[1]],
        device[[2]]
      )
      stopifnot(nrow(found) >= 2)
      expect_equal(found$traced_points, found$drawn_points, label = label)
      expect_equal(
        found$deviation,
        rep(0, nrow(found)),
        tolerance = 1e-10,
        label = label
      )
    }
  }
})

test_that("a facet panel whose edges one routed layer draws routes as that layer alone", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # The arrivals out of a narrow gap take rows of their own only in a panel
  # where more than one routed layer draws edges, since only there could the
  # layer drawn later hide the other's edge. The panel where one layer draws
  # every edge merges them onto the target's centre row, as the plot with
  # that layer alone does, and the label engine traces each panel as it is
  # drawn.
  for (device in list(c(7, 5), c(5, 4))) {
    label <- sprintf("on a %s by %s inch device", device[[1]], device[[2]])
    two <- routed_path_record(narrow_facet_plot(), device[[1]], device[[2]])
    one <- routed_path_record(
      narrow_facet_plot(second = FALSE),
      device[[1]],
      device[[2]]
    )
    stopifnot(
      sum(two$edges$panel == 1) == 4,
      sum(two$edges$panel == 2) == 3
    )
    expect_equal(
      two$paths[two$edges$panel == 2],
      one$paths[one$edges$panel == 2],
      tolerance = 1e-10,
      label = paste("the one-layer panel", label)
    )
    second_panel <- function(record) {
      edges <- record$edges[record$edges$panel == 2, , drop = FALSE]
      rownames(edges) <- NULL
      edges
    }
    expect_equal(
      second_panel(two),
      second_panel(one),
      tolerance = 1e-10,
      label = paste("the ends of the one-layer panel", label)
    )

    found <- label_route_deviations(
      narrow_facet_plot(labels = TRUE),
      device[[1]],
      device[[2]]
    )
    stopifnot(length(unique(found$panel)) == 2)
    expect_equal(
      found$deviation,
      rep(0, nrow(found)),
      tolerance = 1e-10,
      label = paste("the traced routes", label)
    )
  }
})

test_that("a routed layer that draws only arcs in a panel is one of its scene's layers", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # The second layer draws only the arc out of `t`, which is never rerouted,
  # yet it draws edges of the scene in the panel: the arrivals out of the
  # narrow gap take rows of their own, and the stubs and the caps hold that
  # layer's reach and head resection. The label engine once built the scene
  # from the routed edges alone and traced two arrivals 3.6 mm from where
  # they are drawn on a 5 by 4 inch device.
  seconds <- list(
    `the same settings` = list(),
    `longer heads` = list(linewidth = 2, length = 8),
    `a head resection mapped` = list(
      mapping = ggplot2::aes(resect_head = 14)
    )
  )
  for (name in names(seconds)) {
    for (device in list(c(7, 5), c(5, 4))) {
      label <- sprintf(
        "%s on a %s by %s inch device",
        name,
        device[[1]],
        device[[2]]
      )
      plot <- narrow_curved_plot(seconds[[name]])
      inputs <- drawn_and_traced_router_inputs(plot, device[[1]], device[[2]])
      stopifnot(
        length(inputs$drawn) == 4,
        isTRUE(inputs$drawn[[1]]$opts$narrow_rows)
      )
      expect_equal(
        traced_router_input_mismatches(inputs),
        character(),
        label = label
      )
      found <- label_route_deviations(plot, device[[1]], device[[2]])
      stopifnot(nrow(found) == 3)
      expect_equal(
        found$deviation,
        rep(0, nrow(found)),
        tolerance = 1e-10,
        label = paste("the routes traced for", label)
      )
    }
  }
})

test_that("a curvature one routed layer maps to a single value pins that layer's edges", {
  skip_if_not_installed("ragg")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # ggplot2 gives a mapping to a single value to every row, so the layer
  # draws each of its edges as that arc, or straight at zero, and never
  # reroutes them. The label engine once read a single value as no mapping
  # and routed those edges as well, and traced the other layer's routes
  # 4.5 mm from where they are drawn on a 7 by 5 inch device.
  ortho <- list(route = "orthogonal")
  for (curvature in c(0.3, 0)) {
    first <- c(
      ortho,
      list(mapping = ggplot2::aes(edge_curvature = !!curvature))
    )
    for (device in list(c(7, 5), c(4, 4))) {
      label <- sprintf(
        "a curvature of %s on a %s by %s inch device",
        curvature,
        device[[1]],
        device[[2]]
      )
      plot <- readme_layer_pair_plot(first, ortho)
      inputs <- drawn_and_traced_router_inputs(plot, device[[1]], device[[2]])
      pinned <- inputs$drawn[[1]]$edges$curvature %in% curvature
      stopifnot(length(inputs$drawn) == 4, sum(pinned) == 6)
      expect_equal(
        traced_router_input_mismatches(inputs),
        character(),
        label = label
      )
      found <- label_route_deviations(plot, device[[1]], device[[2]])
      stopifnot(nrow(found) == if (curvature == 0) 11 else 5)
      expect_equal(
        found$deviation,
        rep(0, nrow(found)),
        tolerance = 1e-10,
        label = paste("the routes traced for", label)
      )
    }
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

  # The routes of these scenes are pinned in
  # fixtures/orthogonal-final-runs.rds, regenerated only on purpose with
  # tests/testthat/fixtures/make-orthogonal-run-fixtures.R. Doubles must match
  # to within floating-point noise (1e-10). Every scene keeps its paths
  # apart, and each keeps the first two invariants but where its gaps are too
  # narrow for them, which the known failures above record: the README
  # scenes pass within nodes, and in deep_confound the run into the head of
  # u -> b is short of its cut.
  known <- list(
    readme_16_10x6 = "inside",
    readme_14_7x5 = "inside",
    deep_confound_adjustment = "off_run"
  )
  fixture <- readRDS(test_path("fixtures", "orthogonal-final-runs.rds"))
  expect_named(fixture, names(orthogonal_run_fixture_scenes))

  for (scene in names(orthogonal_run_fixture_scenes)) {
    spec <- orthogonal_run_fixture_scenes[[scene]]
    p <- spec$plot()
    found <- orthogonal_invariants(p, spec$width, spec$height)
    for (invariant in setdiff(
      c("off_run", "inside", "shared"),
      known[[scene]]
    )) {
      expect_equal(
        found[[invariant]],
        character(),
        label = paste(scene, invariant)
      )
    }

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

test_that("the README's middle panel keeps its orthogonal edges apart", {
  skip_if_not_installed("ragg")
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")
  withr::local_options(
    ggdag.edge_cap = NULL,
    ggdag.node_size = NULL,
    ggdag.edge_route = NULL
  )

  # Every edge of the panel is drawn and no two share a stretch. Its gaps are
  # too narrow for every head to sit on its run and every path to clear the
  # nodes, so the first two invariants are not asserted here.
  p <- readme_middle_panel_plot()
  found <- orthogonal_invariants(p, width = 7, height = 5)
  stopifnot(
    length(unique(found$ends$panel)) == 1,
    nrow(found$ends) == 11,
    length(found$shared) == 0
  )

  expect_doppelganger("readme middle panel edges kept apart", p)
})
