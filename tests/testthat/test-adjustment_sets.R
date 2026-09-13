test_that("adjustment sets drawn correctly", {
  p1 <- ggdag_adjustment_set(test_dag)
  expect_doppelganger("ggdag_adjustment_set() renders", p1)

  # Add edge count test
  n_edges <- count_dag_edges(test_dag)
  n_sets <- count_adjustment_sets(test_dag)
  expect_edge_count(p1, n_edges * n_sets, "ggdag_adjustment_set default")

  p2 <- ggdag_adjustment_set(test_dag, shadow = FALSE)
  expect_doppelganger("ggdag_adjustment_set() renders without shadows", p2)

  # Shadow parameter doesn't affect edge count for adjustment sets
  expect_edge_count(p2, n_edges * n_sets, "ggdag_adjustment_set shadow=FALSE")
})

test_that("adjustment sets drawn correctly with width set low", {
  withr::with_options(
    list(width = 20),
    {
      p <- ggdag_adjustment_set(test_dag)
      expect_doppelganger("ggdag_adjustment_set() renders with low width", p)

      # Add edge count test - width doesn't affect edge count
      n_edges <- count_dag_edges(test_dag)
      n_sets <- count_adjustment_sets(test_dag)
      expect_edge_count(
        p,
        n_edges * n_sets,
        "ggdag_adjustment_set with low width"
      )
    }
  )
})

# Unit tests for dag_adjustment_sets
test_that("dag_adjustment_sets identifies correct adjustment sets", {
  # Simple confounding
  simple_dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  adj_sets <- dag_adjustment_sets(simple_dag)
  dag_data <- pull_dag_data(adj_sets)

  # Check that z is marked as adjusted
  z_adjusted <- dag_data[dag_data$name == "z", "adjusted"]
  expect_true(all(z_adjusted == "adjusted"))

  # Check that x and y are not adjusted
  x_adjusted <- dag_data[dag_data$name == "x", "adjusted"]
  y_adjusted <- dag_data[dag_data$name == "y", "adjusted"]
  expect_true(all(x_adjusted == "unadjusted"))
  expect_true(all(y_adjusted == "unadjusted"))

  # Check set column is created
  expect_true("set" %in% names(dag_data))
})

test_that("ggdag_adjustment_set respects use_edges parameter", {
  # Create a DAG with adjustment set: y ~ x + z, x ~ z
  # This should have 3 edges total: y <- x, y <- z, x <- z
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  # Test with use_edges = TRUE (should have edges)
  p_with_edges <- ggdag_adjustment_set(dag, use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  expect_equal(
    analysis_with$total_edges,
    3,
    info = "Should have exactly 3 edges when use_edges = TRUE"
  )
  expect_gt(
    analysis_with$edge_layers,
    0,
    "Should have at least 1 edge layer when use_edges = TRUE"
  )

  # Test with use_edges = FALSE (should have NO edges)
  p_without_edges <- ggdag_adjustment_set(dag, use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without_edges)

  expect_false(
    analysis_without$has_edge_layers,
    paste(
      "Should have NO edge layers when use_edges = FALSE, but found:",
      analysis_without$edge_layers
    )
  )
  expect_equal(
    analysis_without$total_edges,
    0,
    info = paste(
      "Should have 0 edges when use_edges = FALSE, but found:",
      analysis_without$total_edges
    )
  )
  expect_equal(
    analysis_without$edge_layers,
    0,
    info = "Should have 0 edge layers when use_edges = FALSE"
  )
})

test_that("ggdag_adjust respects use_edges parameter", {
  # Create a DAG and control for z: y ~ x + z, x ~ z
  # This should have 3 edges total: y <- x, y <- z, x <- z
  dag <- dagify(y ~ x + z, x ~ z)

  # Test with use_edges = TRUE (should have edges)
  p_with_edges <- ggdag_adjust(dag, var = "z", use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  expect_equal(
    analysis_with$total_edges,
    3,
    info = "Should have exactly 3 edges when use_edges = TRUE"
  )
  expect_gt(
    analysis_with$edge_layers,
    0,
    "Should have at least 1 edge layer when use_edges = TRUE"
  )

  # Test with use_edges = FALSE (should have NO edges)
  p_without_edges <- ggdag_adjust(dag, var = "z", use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without_edges)

  expect_false(
    analysis_without$has_edge_layers,
    paste(
      "Should have NO edge layers when use_edges = FALSE, but found:",
      analysis_without$edge_layers
    )
  )
  expect_equal(
    analysis_without$total_edges,
    0,
    info = paste(
      "Should have 0 edges when use_edges = FALSE, but found:",
      analysis_without$total_edges
    )
  )
  expect_equal(
    analysis_without$edge_layers,
    0,
    info = "Should have 0 edge layers when use_edges = FALSE"
  )
})

test_that("ggdag_adjustment_set handles complex DAG with bidirected edges", {
  # Complex DAG with bidirected edge from ?ggdag
  dag <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2, # bidirected edge
    exposure = "x",
    outcome = "y"
  )

  # Count edges in base DAG
  dag_edges <- dagitty::edges(dag)
  n_dag_edges <- nrow(dag_edges)
  expect_equal(n_dag_edges, 11, info = "Base DAG should have 11 edges")

  # Get adjustment sets to know how many facets we'll have
  adj_sets <- dag_adjustment_sets(dag)
  dag_data <- pull_dag_data(adj_sets)
  n_sets <- length(unique(dag_data$set))

  # Test with use_edges = TRUE
  # Expected: n_edges * n_sets = 11 * n_sets
  p_with_edges <- ggdag_adjustment_set(dag, use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  expect_equal(
    analysis_with$total_edges,
    n_dag_edges * n_sets,
    info = paste("Should have", n_dag_edges * n_sets, "edge-panel combinations")
  )

  # Test with use_edges = FALSE
  p_without_edges <- ggdag_adjustment_set(dag, use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without_edges)

  expect_false(
    analysis_without$has_edge_layers,
    "Should have NO edge layers when use_edges = FALSE"
  )
  expect_equal(
    analysis_without$total_edges,
    0,
    info = "Should have 0 edges when use_edges = FALSE"
  )
})

test_that("ggdag_adjust handles complex DAG with bidirected edges", {
  # Complex DAG with bidirected edge from ?ggdag
  dag <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2, # bidirected edge
    exposure = "x",
    outcome = "y"
  )

  # Count edges in base DAG
  dag_edges <- dagitty::edges(dag)
  n_dag_edges <- nrow(dag_edges)
  expect_equal(n_dag_edges, 11, info = "Base DAG should have 11 edges")

  # Test adjusting for w1
  p_with_edges <- ggdag_adjust(dag, var = "w1", use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  # ggdag_adjust doesn't create facets, so just n_dag_edges
  expect_equal(
    analysis_with$total_edges,
    n_dag_edges,
    info = "Should have exactly 11 edges when use_edges = TRUE"
  )

  # Test with use_edges = FALSE
  p_without_edges <- ggdag_adjust(dag, var = "w1", use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without_edges)

  expect_false(
    analysis_without$has_edge_layers,
    "Should have NO edge layers when use_edges = FALSE"
  )
  expect_equal(
    analysis_without$total_edges,
    0,
    info = "Should have 0 edges when use_edges = FALSE"
  )
})

test_that("dag_adjustment_sets handles multiple adjustment sets", {
  # DAG with multiple valid adjustment sets
  multi_dag <- dagify(
    y ~ x + a + b,
    x ~ a + b,
    a ~ c,
    b ~ c,
    exposure = "x",
    outcome = "y"
  )

  # Get the dagitty result directly
  dagitty_sets <- dagitty::adjustmentSets(multi_dag)
  expected_n_sets <- length(dagitty_sets)

  adj_sets <- dag_adjustment_sets(multi_dag)
  dag_data <- pull_dag_data(adj_sets)

  # Should have same number of adjustment sets as dagitty
  n_sets <- length(unique(dag_data$set))
  expect_equal(n_sets, expected_n_sets)

  # Each set should be properly formatted
  expect_true(all(grepl("^\\{.*\\}$", unique(dag_data$set))))
})

test_that("dag_adjustment_sets handles no valid adjustment sets with warning", {
  # Create a DAG where backdoor paths cannot be closed
  # Simple confounding through latent variable
  no_adjust_dag <- dagify(
    y ~ x + u,
    x ~ u,
    exposure = "x",
    outcome = "y",
    latent = "u"
  )

  expect_ggdag_warning(
    adj_sets <- dag_adjustment_sets(no_adjust_dag)
  )

  dag_data <- pull_dag_data(adj_sets)
  # Should have special marker for no adjustment sets
  expect_true(any(grepl("No Way to Block", dag_data$set)))
})

test_that("dag_adjustment_sets handles unconditionally closed paths", {
  # No backdoor paths
  no_backdoor <- dagify(
    y ~ x,
    x ~ z,
    w ~ z,
    exposure = "x",
    outcome = "y"
  )

  adj_sets <- dag_adjustment_sets(no_backdoor)
  dag_data <- pull_dag_data(adj_sets)

  # Should indicate paths are unconditionally closed
  expect_true(any(grepl("Unconditionally Closed", dag_data$set)))
})

test_that("dag_adjustment_sets works without specified exposure/outcome", {
  # Should infer from DAG
  infer_dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  # Remove exposure/outcome from call
  adj_sets <- dag_adjustment_sets(infer_dag)
  expect_s3_class(adj_sets, "tidy_dagitty")

  # Should still find adjustment sets
  dag_data <- pull_dag_data(adj_sets)
  expect_true(any(dag_data$adjusted == "adjusted"))
})

test_that("dag_adjustment_sets passes additional arguments to adjustmentSets", {
  # Test with type argument
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  # Get canonical adjustment sets
  adj_sets <- dag_adjustment_sets(dag, type = "canonical")
  expect_s3_class(adj_sets, "tidy_dagitty")
})

# Unit tests for is_confounder
test_that("is_confounder correctly identifies confounders", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  # z is a confounder
  expect_true(is_confounder(dag, "z", "x", "y"))

  # x is not a confounder of itself and y
  expect_false(is_confounder(dag, "x", "x", "y"))

  # y is not a confounder
  expect_false(is_confounder(dag, "y", "x", "y"))
})

test_that("is_confounder handles direct parameter", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    z ~ w,
    exposure = "x",
    outcome = "y"
  )

  # z is a direct confounder
  expect_true(is_confounder(dag, "z", "x", "y", direct = TRUE))

  # w is not a direct confounder (affects through z)
  expect_false(is_confounder(dag, "w", "x", "y", direct = TRUE))

  # But w is an indirect confounder
  expect_true(is_confounder(dag, "w", "x", "y", direct = FALSE))
})

test_that("is_confounder works with tidy_dagitty objects", {
  dag <- dagify(
    y ~ x + z,
    x ~ z
  ) |>
    tidy_dagitty()

  expect_true(is_confounder(dag, "z", "x", "y"))
})

test_that("is_confounder() requires a cause of both variables", {
  # z is an upstream cause of x only; it reaches y through x, so it opens no
  # backdoor path and is not a confounder
  expect_false(is_confounder(dagify(y ~ x, x ~ z), "z", "x", "y"))

  # x causes y only through the mediator m, so x does not confound m and y
  expect_false(is_confounder(dagify(m ~ x, y ~ m), "x", "m", "y"))

  # z reaches x only through y
  expect_false(is_confounder(dagify(y ~ z, x ~ y), "z", "x", "y"))

  # a bidirected edge is not a causal path out of z
  expect_false(is_confounder(dagify(y ~ x, x ~ ~z), "z", "x", "y"))

  # regression: genuine forks are still confounders
  expect_true(is_confounder(dagify(y ~ x + z, x ~ z), "z", "x", "y"))
  expect_true(is_confounder(dagify(x ~ z, y ~ z, z ~ w), "w", "x", "y"))

  # direct = TRUE is unchanged
  fork <- dagify(y ~ x + z, x ~ z, z ~ w)
  expect_true(is_confounder(fork, "z", "x", "y", direct = TRUE))
  expect_false(is_confounder(fork, "w", "x", "y", direct = TRUE))
})

test_that("adjustment set functions require exposure and outcome", {
  no_endpoints <- dagify(y ~ x + z, x ~ z)

  expect_error(
    dag_adjustment_sets(no_endpoints),
    class = "ggdag_missing_error"
  )
  expect_error(
    ggdag_adjustment_set(no_endpoints),
    class = "ggdag_missing_error"
  )

  # endpoints set in the DAG, or supplied as arguments, still work
  with_endpoints <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )
  expect_s3_class(dag_adjustment_sets(with_endpoints), "tidy_dagitty")
  expect_s3_class(
    dag_adjustment_sets(no_endpoints, exposure = "x", outcome = "y"),
    "tidy_dagitty"
  )
  expect_s3_class(
    ggdag_adjustment_set(no_endpoints, exposure = "x", outcome = "y"),
    "gg"
  )
})

test_that("adjustment set guards are informative", {
  no_endpoints <- dagify(y ~ x + z, x ~ z)

  expect_ggdag_error(dag_adjustment_sets(no_endpoints))
  expect_ggdag_error(ggdag_adjustment_set(no_endpoints))
  expect_ggdag_error(ggdag_adjust(dagify(y ~ x)))
  expect_ggdag_error(ggdag_adjust(dagify(y ~ x), var = character(0)))
})

# Unit tests for control_for
test_that("control_for updates DAG with adjusted variables", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  controlled <- control_for(dag, "z")
  dag_data <- pull_dag_data(controlled)

  # z should be adjusted
  z_data <- dag_data[dag_data$name == "z", ]
  expect_true(all(z_data$adjusted == "adjusted"))

  # Others should be unadjusted
  other_data <- dag_data[dag_data$name != "z", ]
  expect_true(all(other_data$adjusted == "unadjusted"))
})

test_that("control_for handles multiple variables", {
  dag <- dagify(
    y ~ x + a + b,
    x ~ a + b
  )

  controlled <- control_for(dag, c("a", "b"))
  dag_data <- pull_dag_data(controlled)

  adjusted_vars <- dag_data[dag_data$name %in% c("a", "b"), ]
  expect_true(all(adjusted_vars$adjusted == "adjusted"))
})

test_that("control_for handles var with no matches", {
  dag <- dagify(y ~ x)

  # Should error when variable doesn't exist in DAG
  expect_ggdag_error(
    control_for(dag, "z")
  )
})

# Unit tests for ggdag_adjust
test_that("ggdag_adjust creates plot with adjusted variables", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  p <- ggdag_adjust(dag, "z")
  expect_s3_class(p, "ggplot")

  # Check that the plot has the expected layers
  expect_true(length(p$layers) > 0)
})

test_that("ggdag_adjust works with different parameters", {
  dag <- dagify(
    y ~ x + z,
    x ~ z
  )

  # Test with collider_lines
  p1 <- ggdag_adjust(dag, "z", collider_lines = FALSE)
  expect_s3_class(p1, "ggplot")

  # Test with different variables to adjust
  p2 <- ggdag_adjust(dag, var = c("z"))
  expect_s3_class(p2, "ggplot")

  # Test with empty adjustment (no variables)
  expect_error(
    ggdag_adjust(dag, var = character(0)),
    class = "ggdag_missing_error"
  )
})

test_that("ggdag_adjust handles node styling", {
  dag <- dagify(
    y ~ x + z,
    x ~ z
  )

  # Test use_labels with new API
  expect_ggdag_warning(
    p1 <- ggdag_adjust(dag, "z", use_labels = "label")
  )
  expect_s3_class(p1, "ggplot")

  # Test with text = FALSE (deprecated)
  expect_ggdag_warning(
    p2 <- ggdag_adjust(dag, "z", text = FALSE)
  )
  expect_s3_class(p2, "ggplot")

  # Test with new API
  p3 <- ggdag_adjust(dag, "z", use_labels = TRUE)
  expect_s3_class(p3, "ggplot")
})

# Test extract_sets helper function
test_that("extract_sets processes adjustment sets correctly", {
  # Create mock adjustment sets
  sets <- list(c("a", "b"), c("c"), character(0))
  names(sets) <- NULL

  extracted <- extract_sets(sets)

  expect_length(extracted, 3)
  expect_equal(extracted[[1]], c("a", "b"))
  expect_equal(extracted[[2]], "c")
  expect_equal(extracted[[3]], "(Backdoor Paths Unconditionally Closed)")
})

test_that("ggdag_adjustment_set() sizes the edge layers it builds itself", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  p <- ggdag_adjustment_set(
    dag,
    size = 2,
    edge_cap = 3,
    edge_width = 2,
    arrow_length = 20
  )

  expect_equal(edge_cap_radii(p), 6)
  expect_equal(edge_widths(p), 4)
  expect_equal(edge_arrow_lengths(p), 40)
})

test_that("ggdag_adjustment_set() honors the proportional edge_cap option", {
  withr::local_options(ggdag.edge_cap = 4)
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  # the adjustment set functions scale the option by 10/8
  expect_equal(edge_cap_radii(ggdag_adjustment_set(dag)), 5)
})

test_that("ggdag_adjust() sizes the edge layers it builds itself", {
  dag <- dagify(y ~ x + z, x ~ z)

  p <- ggdag_adjust(
    dag,
    var = "z",
    size = 2,
    edge_cap = 3,
    edge_width = 2,
    arrow_length = 20
  )

  expect_equal(edge_cap_radii(p), 6)
  expect_equal(edge_widths(p), 4)
  expect_equal(edge_arrow_lengths(p), 40)
})

test_that("ggdag_adjust() honors edge_type in the edge layers it builds itself", {
  dag <- dagify(y ~ x + z, x ~ z)

  stats <- layer_stat_classes(ggdag_adjust(dag, var = "z", edge_type = "arc"))

  expect_true("StatEdgeArc" %in% stats)
  expect_false("StatEdgeLink" %in% stats)
})

test_that("ggdag_adjustment_set() and ggdag_adjust() accept unified_legend", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  expect_s3_class(ggdag_adjustment_set(dag, unified_legend = FALSE), "gg")
  expect_s3_class(ggdag_adjust(dag, var = "z", unified_legend = FALSE), "gg")
})

test_that("ggdag_adjustment_set() accepts key_glyph", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  expect_s3_class(
    ggdag_adjustment_set(dag, key_glyph = draw_key_dag_point),
    "gg"
  )
})

test_that("ggdag_adjustment_set() draws the edge sizes it is given", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  expect_doppelganger(
    "ggdag_adjustment_set() with wide capped edges",
    ggdag_adjustment_set(dag, edge_cap = 16, edge_width = 2)
  )
})

test_that("ggdag_adjust() rejects an edge type it cannot draw", {
  dag <- dagify(y ~ x + z, x ~ z)

  expect_error(
    ggdag_adjust(dag, var = "z", edge_type = "bogus"),
    "should be one of"
  )
  expect_s3_class(ggdag_adjust(dag, var = "z", edge_type = "diagonal"), "gg")
})

test_that("ggdag_adjust() rejects an edge type it cannot draw on either engine", {
  dag <- dagify(y ~ x + z, x ~ z)

  expect_error(
    ggdag_adjust(dag, var = "z", edge_type = "bogus", edge_engine = "ggarrow"),
    "should be one of"
  )
  expect_s3_class(
    ggdag_adjust(
      dag,
      var = "z",
      edge_type = "diagonal",
      edge_engine = "ggarrow"
    ),
    "gg"
  )
})

# A DAG whose only backdoor path is blocked at the collider `m`. Adjusting for
# `m` activates the path between its parents, so the tidy data carries
# `collider_line` rows, and every adjustment set contains `m`: the paths that
# adjustment activates are ones an adjustment set still closes.
collider_adjusted_dag <- function() {
  dagify(
    m ~ a + b,
    x ~ a,
    y ~ b + x,
    exposure = "x",
    outcome = "y"
  ) |>
    adjust_for("m")
}

# A DAG where `showed_up` is both a collider and a descendant of the exposure.
# Adjusting for it activates paths that no set of the remaining variables
# closes, so `dag_adjustment_sets()` reports no adjustment set at all.
unclosable_collider_dag <- function() {
  dagify(
    podcast ~ mood + humor + prepared,
    exam ~ mood + prepared + showed_up,
    showed_up ~ podcast + mood + prepared,
    exposure = "podcast",
    outcome = "exam"
  ) |>
    adjust_for("showed_up")
}

# `dag_adjustment_sets()` warns whenever no adjustment set closes the backdoor
# paths, which is the case `unclosable_collider_dag()` is built for. The
# warning itself is pinned by its own test below.
adjustment_set_plot <- function(...) {
  suppressWarnings(
    ggdag_adjustment_set(...),
    classes = "ggdag_failed_to_close_backdoor_warning"
  )
}

# The rows the edge layers drawn by `geom_class` are handed, one element per
# layer.
edge_layer_rows <- function(plot, geom_class) {
  purrr::map(
    layers_by_geom(plot, geom_class),
    \(layer) edge_layer_data(layer, plot$data)
  )
}

# The geom the edge layers of `engine` are drawn with.
engine_edge_geom <- function(engine) {
  if (identical(engine, "ggarrow")) "GeomDAGArrowCurve" else "GeomDAGEdgePath"
}

test_that("ggdag_adjustment_set() warns when no adjustment set closes the backdoor paths", {
  expect_warning(
    ggdag_adjustment_set(unclosable_collider_dag()),
    class = "ggdag_failed_to_close_backdoor_warning"
  )
})

test_that("ggdag_adjustment_set() draws activated collider paths when nothing closes the backdoor paths", {
  purrr::walk(c("ggraph", "ggarrow"), \(engine) {
    p <- adjustment_set_plot(unclosable_collider_dag(), edge_engine = engine)

    collider_layers <- layers_by_geom(p, "GeomCurve")
    expect_length(collider_layers, 1)

    drawn <- edge_layer_data(collider_layers[[1]], p$data)
    expect_gt(nrow(drawn), 0)
    expect_equal(
      drawn,
      dplyr::filter(p$data, .data$direction == "<->", .data$collider_line)
    )
  })
})

test_that("ggdag_adjustment_set() leaves out activated collider paths when an adjustment set closes the backdoor paths", {
  purrr::walk(c("ggraph", "ggarrow"), \(engine) {
    p <- ggdag_adjustment_set(collider_adjusted_dag(), edge_engine = engine)
    expect_length(layers_by_geom(p, "GeomCurve"), 0)
  })
})

test_that("ggdag_adjustment_set() draws activated collider paths on request whatever the adjustment sets", {
  purrr::walk(c("ggraph", "ggarrow"), \(engine) {
    plots <- list(
      ggdag_adjustment_set(
        collider_adjusted_dag(),
        collider_lines = TRUE,
        edge_engine = engine
      ),
      adjustment_set_plot(
        unclosable_collider_dag(),
        collider_lines = TRUE,
        edge_engine = engine
      )
    )

    purrr::walk(plots, \(p) {
      collider_layers <- layers_by_geom(p, "GeomCurve")
      expect_length(collider_layers, 1)

      drawn <- edge_layer_data(collider_layers[[1]], p$data)
      expect_gt(nrow(drawn), 0)
      expect_true(all(drawn$collider_line))
    })
  })
})

test_that("ggdag_adjustment_set() suppresses activated collider paths on request", {
  purrr::walk(c("ggraph", "ggarrow"), \(engine) {
    plots <- list(
      ggdag_adjustment_set(
        collider_adjusted_dag(),
        collider_lines = FALSE,
        edge_engine = engine
      ),
      adjustment_set_plot(
        unclosable_collider_dag(),
        collider_lines = FALSE,
        edge_engine = engine
      )
    )

    purrr::walk(plots, \(p) expect_length(layers_by_geom(p, "GeomCurve"), 0))
  })
})

test_that("ggdag_adjustment_set() keeps activated collider paths out of its edges", {
  purrr::walk(c("ggraph", "ggarrow"), \(engine) {
    purrr::walk(list(NULL, TRUE, FALSE), \(collider_lines) {
      plots <- list(
        ggdag_adjustment_set(
          collider_adjusted_dag(),
          collider_lines = collider_lines,
          edge_engine = engine
        ),
        adjustment_set_plot(
          unclosable_collider_dag(),
          collider_lines = collider_lines,
          edge_engine = engine
        )
      )

      purrr::walk(plots, \(p) {
        edge_rows <- edge_layer_rows(p, engine_edge_geom(engine))
        expect_gt(length(edge_rows), 0)
        expect_false(any(purrr::map_lgl(edge_rows, \(rows) {
          any(rows$collider_line)
        })))
      })
    })
  })
})

test_that("ggdag_adjustment_set() rejects a collider_lines it cannot read", {
  td <- collider_adjusted_dag()

  expect_error(
    ggdag_adjustment_set(td, collider_lines = "yes"),
    class = "ggdag_type_error"
  )
  expect_error(
    ggdag_adjustment_set(td, collider_lines = NA),
    class = "ggdag_type_error"
  )
  expect_error(
    ggdag_adjustment_set(td, collider_lines = c(TRUE, FALSE)),
    class = "ggdag_type_error"
  )
})

test_that("ggdag_adjustment_set() adds no collider layer without activated paths", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  p <- ggdag_adjustment_set(dag)
  expect_length(layers_by_geom(p, "GeomCurve"), 0)
  expect_equal(
    unname(purrr::map_chr(p$layers, \(layer) class(layer$geom)[[1]])),
    c("GeomDAGEdgePath", "GeomDagPoint", "GeomDagText")
  )

  # adjusting for a non-collider adds the column but activates no path
  adjusted <- adjust_for(dag, "z")
  expect_length(layers_by_geom(ggdag_adjustment_set(adjusted), "GeomCurve"), 0)
})

test_that("ggdag_adjust() draws activated collider paths on either engine", {
  td <- collider_adjusted_dag()

  purrr::walk(c("ggraph", "ggarrow"), \(engine) {
    p <- ggdag_adjust(td, edge_engine = engine)

    collider_layers <- layers_by_geom(p, "GeomCurve")
    expect_length(collider_layers, 1)

    drawn <- edge_layer_data(collider_layers[[1]], p$data)
    expect_gt(nrow(drawn), 0)
    expect_true(all(drawn$collider_line))

    edge_rows <- edge_layer_rows(p, engine_edge_geom(engine))
    expect_gt(length(edge_rows), 0)
    expect_false(any(purrr::map_lgl(edge_rows, \(rows) {
      any(rows$collider_line)
    })))
  })
})

test_that("ggdag_adjust() draws activated collider paths where ggdag_adjustment_set() does not", {
  td <- collider_adjusted_dag()

  expect_length(layers_by_geom(ggdag_adjust(td), "GeomCurve"), 1)
  expect_length(layers_by_geom(ggdag_adjustment_set(td), "GeomCurve"), 0)
})

test_that("ggdag_adjustment_set() renders activated collider paths", {
  expect_doppelganger(
    "ggdag_adjustment_set() with activated collider paths",
    ggdag_adjustment_set(collider_adjusted_dag(), collider_lines = TRUE)
  )
})

test_that("ggdag_adjustment_set() renders no collider paths where a set closes the backdoors", {
  expect_doppelganger(
    "ggdag_adjustment_set() with no collider paths",
    ggdag_adjustment_set(collider_adjusted_dag())
  )
})

# The index of the single layer `plot` draws its activated collider paths with.
collider_layer_index <- function(plot) {
  which(purrr::map_lgl(plot$layers, \(layer) inherits(layer$geom, "GeomCurve")))
}

# The rows the activated collider path layer of `plot` is drawn from, after the
# scales have resolved every aesthetic.
built_collider_data <- function(plot) {
  index <- collider_layer_index(plot)
  expect_length(index, 1)
  ggplot2::ggplot_build(plot)$data[[index]]
}

# The colours the `adjusted` scale of `plot` puts on its nodes.
built_adjusted_colours <- function(plot) {
  sort(unique(built_node_data(plot)$colour))
}

# The colour `geom_dag_collider_edges()` draws with where no `adjusted` colour
# scale is in force, which is the neutral default the annotation keeps.
neutral_collider_colour <- function(tidy_dag) {
  plot <- ggplot2::ggplot(node_collider(tidy_dag), aes_dag()) +
    geom_dag_collider_edges()

  unique(built_collider_data(plot)$colour)
}

test_that("activated collider paths draw in their own neutral colour", {
  td <- unclosable_collider_dag()
  p <- adjustment_set_plot(td)

  drawn <- built_collider_data(p)
  expect_gt(nrow(drawn), 0)
  expect_length(unique(drawn$colour), 1)
  expect_false(unique(drawn$colour) %in% built_adjusted_colours(p))
  expect_equal(unique(drawn$colour), neutral_collider_colour(td))
})

test_that("activated collider paths take no colour from the adjustment scale", {
  td <- collider_adjusted_dag()
  p <- ggdag_adjustment_set(td, collider_lines = TRUE)

  drawn <- built_collider_data(p)
  expect_gt(nrow(drawn), 0)
  expect_length(unique(drawn$colour), 1)
  expect_false(unique(drawn$colour) %in% built_adjusted_colours(p))
  expect_equal(unique(drawn$colour), neutral_collider_colour(td))
})

test_that("activated collider paths look the same with and without an adjustment set", {
  no_set <- built_collider_data(adjustment_set_plot(unclosable_collider_dag()))
  with_set <- built_collider_data(
    ggdag_adjustment_set(collider_adjusted_dag(), collider_lines = TRUE)
  )

  expect_equal(unique(no_set$colour), unique(with_set$colour))
})

test_that("a caller colour overrides the activated collider path colour", {
  p <- ggdag_adjustment_set(collider_adjusted_dag(), collider_lines = FALSE) +
    geom_dag_collider_edges(colour = "purple")

  expect_equal(unique(built_collider_data(p)$colour), "purple")
})

test_that("activated collider paths stay dashed and draw no arrowheads", {
  p <- adjustment_set_plot(unclosable_collider_dag())

  expect_equal(unique(built_collider_data(p)$linetype), "dashed")

  collider_layers <- layers_by_geom(p, "GeomCurve")
  expect_length(collider_layers, 1)
  expect_null(collider_layers[[1]]$geom_params$arrow)
})

test_that("ggdag_adjustment_set() renders collider paths with no way to block the backdoor paths", {
  expect_doppelganger(
    "collider paths with no way to block backdoor paths",
    adjustment_set_plot(unclosable_collider_dag())
  )
})
