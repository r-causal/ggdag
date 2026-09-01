test_that("ggdag_defaults contains all expected options", {
  expected_names <- c(
    "node_size",
    "text_size",
    "label_size",
    "text_col",
    "label_col",
    "edge_width",
    "edge_cap",
    "arrow_length",
    "use_edges",
    "use_nodes",
    "use_stylized",
    "use_text",
    "use_labels",
    "label_geom",
    "edge_type",
    "layout",
    "edge_engine",
    "arrow_head",
    "arrow_fins",
    "arrow_mid",
    "curvature",
    "debug_repel_points"
  )
  expect_named(ggdag_defaults, expected_names, ignore.order = TRUE)
})

test_that("ggdag_options_set() sets options and returns old values invisibly", {
  withr::local_options(ggdag.node_size = NULL)

  result <- withVisible(ggdag_options_set(node_size = 24))
  expect_false(result$visible)
  expect_null(result$value$node_size)
  expect_equal(getOption("ggdag.node_size"), 24)

  result2 <- ggdag_options_set(node_size = 32)
  expect_equal(result2$node_size, 24)
  expect_equal(getOption("ggdag.node_size"), 32)
})

test_that("ggdag_options_get() retrieves a single option", {
  withr::local_options(ggdag.node_size = 20)
  expect_equal(ggdag_options_get("node_size"), 20)
})

test_that("ggdag_options_get() with no argument returns all set options", {
  withr::local_options(
    ggdag.node_size = 20,
    ggdag.text_size = 5
  )
  result <- ggdag_options_get()
  expect_true("node_size" %in% names(result))
  expect_true("text_size" %in% names(result))
  expect_equal(result$node_size, 20)
  expect_equal(result$text_size, 5)
})

test_that("ggdag_options_reset() clears all ggdag options to NULL", {
  local_ggdag_option_state()
  withr::local_options(ggdag.node_size = 20, ggdag.text_size = 5)
  ggdag_options_reset()
  expect_null(getOption("ggdag.node_size"))
  expect_null(getOption("ggdag.text_size"))
})

test_that("ggdag_option() returns global option when set, default otherwise", {
  withr::local_options(ggdag.node_size = NULL)
  expect_equal(ggdag_option("node_size", 16), 16)

  withr::local_options(ggdag.node_size = 24)
  expect_equal(ggdag_option("node_size", 16), 24)
})

test_that("ggdag_option_proportional() returns override_default when unset", {
  withr::local_options(ggdag.edge_cap = NULL)
  expect_equal(ggdag_option_proportional("edge_cap", 8, 10), 10)
})

test_that("ggdag_option_proportional() scales by ratio when option is set", {
  withr::local_options(ggdag.edge_cap = 16)
  # 16 * (10 / 8) = 20
  expect_equal(ggdag_option_proportional("edge_cap", 8, 10), 20)

  withr::local_options(ggdag.edge_cap = 8)
  # 8 * (10 / 8) = 10 (same as override_default)
  expect_equal(ggdag_option_proportional("edge_cap", 8, 10), 10)
})

test_that("validation rejects non-numeric for numeric params", {
  expect_ggdag_error(ggdag_options_set(node_size = "big"))
  expect_ggdag_error(ggdag_options_set(text_size = TRUE))
  expect_ggdag_error(ggdag_options_set(edge_cap = -1))
})

test_that("validation rejects non-logical for logical params", {
  expect_ggdag_error(ggdag_options_set(use_edges = "yes"))
  expect_ggdag_error(ggdag_options_set(use_nodes = 1))
})

test_that("validation rejects invalid edge_type", {
  expect_ggdag_error(ggdag_options_set(edge_type = "squiggle"))
})

test_that("validation rejects non-function for label_geom", {
  expect_ggdag_error(ggdag_options_set(label_geom = "not_a_function"))
})

test_that("withr::local_options works with ggdag options", {
  expect_null(getOption("ggdag.node_size"))
  withr::local_options(ggdag.node_size = 30)
  expect_equal(ggdag_option("node_size", 16), 30)
})

# Visual snapshot tests ----------------------------------------------------
# Each test covers a function category with custom global options to verify
# that the options system propagates correctly through all layers.

# Core: ggdag() and geom_dag()
test_that("ggdag() respects global node_size and text_size", {
  withr::local_options(ggdag.node_size = 24, ggdag.text_size = 6)
  p <- ggdag(test_dag)
  expect_doppelganger("opts-ggdag-large-nodes", p)
})

test_that("ggdag() respects global text_col", {
  withr::local_options(ggdag.text_col = "black")
  p <- ggdag(test_dag)
  expect_doppelganger("opts-ggdag-black-text", p)
})

test_that("ggdag() respects global edge_type", {
  withr::local_options(ggdag.edge_type = "arc")
  p <- ggdag(test_dag)

  # both edge types draw through GeomDAGEdgePath, so the stat is what tells
  # arc edges from the default link_arc pair
  stats <- vapply(p$layers, function(l) class(l$stat)[1], character(1))
  expect_true("StatEdgeArc" %in% stats)
  expect_false("StatEdgeLink" %in% stats)

  expect_doppelganger("opts-ggdag-arc-edges", p)
})

test_that("ggdag() respects global edge_width", {
  withr::local_options(ggdag.edge_width = 1.5)
  p <- ggdag(test_dag)
  expect_doppelganger("opts-ggdag-thick-edges", p)
})

test_that("ggdag() respects global use_stylized", {
  withr::local_options(ggdag.use_stylized = TRUE)
  p <- ggdag(test_dag)
  expect_doppelganger("opts-ggdag-stylized", p)
})

test_that("ggdag() respects global use_text = FALSE", {
  withr::local_options(ggdag.use_text = FALSE)
  p <- ggdag(test_dag)
  expect_doppelganger("opts-ggdag-no-text", p)
})

test_that("ggdag() respects global use_nodes = FALSE", {
  withr::local_options(ggdag.use_nodes = FALSE)
  p <- ggdag(test_dag)
  expect_doppelganger("opts-ggdag-no-nodes", p)
})

test_that("ggdag() respects global use_edges = FALSE", {
  withr::local_options(ggdag.use_edges = FALSE)
  p <- ggdag(test_dag)
  expect_doppelganger("opts-ggdag-no-edges", p)
})

test_that("ggdag() respects global edge_cap", {
  withr::local_options(ggdag.edge_cap = 2)
  p <- ggdag(test_dag)
  expect_doppelganger("opts-ggdag-small-edge-cap", p)
})

test_that("ggdag() respects global arrow_length", {
  withr::local_options(ggdag.arrow_length = 12)
  p <- ggdag(test_dag)
  expect_doppelganger("opts-ggdag-large-arrows", p)
})

test_that("ggdag() respects multiple options at once", {
  withr::local_options(
    ggdag.node_size = 24,
    ggdag.text_size = 6,
    ggdag.text_col = "black",
    ggdag.edge_width = 1.2,
    ggdag.use_stylized = TRUE
  )
  p <- ggdag(test_dag)
  expect_doppelganger("opts-ggdag-multi-options", p)
})

# Quick plots
test_that("ggdag_m_bias() respects global options", {
  withr::local_options(ggdag.node_size = 24, ggdag.text_size = 6)
  p <- ggdag_m_bias()
  expect_doppelganger("opts-m-bias-large-nodes", p)
})

test_that("ggdag_confounder_triangle() respects global options", {
  withr::local_options(
    ggdag.node_size = 24,
    ggdag.text_col = "black",
    ggdag.use_stylized = TRUE
  )
  p <- ggdag_confounder_triangle()
  expect_doppelganger("opts-confounder-triangle-custom", p)
})

test_that("ggdag_collider_triangle() respects global options", {
  withr::local_options(ggdag.edge_type = "arc", ggdag.edge_width = 1.5)
  p <- ggdag_collider_triangle()
  expect_doppelganger("opts-collider-triangle-arc-thick", p)
})

test_that("ggdag_quartet_collider() respects global options", {
  withr::local_options(ggdag.use_text = FALSE, ggdag.node_size = 10)
  p <- ggdag_quartet_collider()
  expect_doppelganger("opts-quartet-collider-no-text-small", p)
})

test_that("ggdag_quartet_confounder() respects global options", {
  withr::local_options(ggdag.edge_type = "diagonal")
  p <- ggdag_quartet_confounder()
  expect_doppelganger("opts-quartet-confounder-diagonal", p)
})

test_that("ggdag_quartet_mediator() respects global options", {
  withr::local_options(ggdag.node_size = 24, ggdag.text_size = 6)
  p <- ggdag_quartet_mediator()
  expect_doppelganger("opts-quartet-mediator-large-nodes", p)
})

test_that("ggdag_quartet_m_bias() respects global options", {
  withr::local_options(ggdag.use_stylized = TRUE, ggdag.text_col = "red")
  p <- ggdag_quartet_m_bias()
  expect_doppelganger("opts-quartet-m-bias-stylized-red", p)
})

test_that("ggdag_quartet_time_collider() respects global options", {
  withr::local_options(ggdag.edge_width = 1.5, ggdag.arrow_length = 10)
  p <- ggdag_quartet_time_collider()
  expect_doppelganger("opts-quartet-time-collider-thick-arrows", p)
})

test_that("ggdag_butterfly_bias() respects global options", {
  withr::local_options(ggdag.node_size = 20, ggdag.edge_cap = 4)
  p <- ggdag_butterfly_bias()
  expect_doppelganger("opts-butterfly-bias-custom", p)
})

test_that("ggdag_mediation_triangle() respects global options", {
  withr::local_options(ggdag.use_edges = FALSE)
  p <- ggdag_mediation_triangle()
  expect_doppelganger("opts-mediation-triangle-no-edges", p)
})

# Relations functions
test_that("ggdag_children() respects global options", {
  withr::local_options(ggdag.node_size = 24, ggdag.text_size = 6)
  p <- ggdag_children(test_dag, "w1")
  expect_doppelganger("opts-children-large-nodes", p)
})

test_that("ggdag_parents() respects global options", {
  withr::local_options(ggdag.edge_type = "arc")
  p <- ggdag_parents(test_dag, "y")
  expect_doppelganger("opts-parents-arc-edges", p)
})

test_that("ggdag_ancestors() respects global options", {
  withr::local_options(ggdag.use_stylized = TRUE, ggdag.text_col = "black")
  p <- ggdag_ancestors(test_dag, "y")
  expect_doppelganger("opts-ancestors-stylized-black-text", p)
})

test_that("ggdag_descendants() respects global options", {
  withr::local_options(ggdag.edge_width = 1.5, ggdag.arrow_length = 10)
  p <- ggdag_descendants(test_dag, "v")
  expect_doppelganger("opts-descendants-thick-arrows", p)
})

test_that("ggdag_markov_blanket() respects global options", {
  withr::local_options(ggdag.node_size = 20, ggdag.use_text = FALSE)
  p <- ggdag_markov_blanket(test_dag, "z1")
  expect_doppelganger("opts-markov-blanket-no-text", p)
})

test_that("ggdag_adjacent() respects global options", {
  withr::local_options(ggdag.edge_cap = 4, ggdag.node_size = 12)
  p <- ggdag_adjacent(test_dag, "x")
  expect_doppelganger("opts-adjacent-small-cap-nodes", p)
})

# Status and exogenous
test_that("ggdag_status() respects global options", {
  withr::local_options(ggdag.node_size = 24, ggdag.text_size = 6)
  p <- ggdag_status(test_dag)
  expect_doppelganger("opts-status-large-nodes", p)
})

test_that("ggdag_exogenous() respects global options", {
  dag <- dagify(y ~ x1 + x2 + x3, b ~ x1 + x2)
  withr::local_options(ggdag.use_stylized = TRUE, ggdag.edge_type = "arc")
  p <- ggdag_exogenous(dag)
  expect_doppelganger("opts-exogenous-stylized-arc", p)
})

# Adjustment sets (proportional edge_cap)
test_that("ggdag_adjustment_set() respects global options", {
  withr::local_options(ggdag.node_size = 24, ggdag.text_size = 6)
  p <- ggdag_adjustment_set(test_dag)
  expect_doppelganger("opts-adjustment-set-large-nodes", p)
})

test_that("ggdag_adjustment_set() proportional edge_cap works", {
  withr::local_options(ggdag.edge_cap = 4)
  p <- ggdag_adjustment_set(test_dag)
  expect_doppelganger("opts-adjustment-set-small-cap", p)
})

test_that("ggdag_adjust() respects global options", {
  withr::local_options(
    ggdag.node_size = 24,
    ggdag.text_col = "black",
    ggdag.use_stylized = TRUE
  )
  p <- ggdag_adjust(test_dag, var = "z1")
  expect_doppelganger("opts-adjust-stylized-black-text", p)
})

# D-relationship functions (proportional edge_cap)
test_that("ggdag_drelationship() respects global options", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.node_size = 24, ggdag.text_size = 6)
  p <- ggdag_drelationship(dag, "x", "y")
  expect_doppelganger("opts-drelationship-large-nodes", p)
})

test_that("ggdag_dseparated() respects global options", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.edge_width = 1.5, ggdag.edge_type = "arc")
  p <- ggdag_dseparated(dag, "x", "y")
  expect_doppelganger("opts-dseparated-thick-arc", p)
})

test_that("ggdag_dconnected() respects global options", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.use_stylized = TRUE, ggdag.text_col = "black")
  p <- ggdag_dconnected(dag, "x", "y")
  expect_doppelganger("opts-dconnected-stylized-black", p)
})

test_that("ggdag_drelationship() proportional edge_cap works", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.edge_cap = 4)
  p <- ggdag_drelationship(dag, "x", "y")
  expect_doppelganger("opts-drelationship-small-cap", p)
})

# Instrumental (proportional edge_cap)
test_that("ggdag_instrumental() respects global options", {
  dag <- dagify(y ~ x + u, x ~ u + z, exposure = "x", outcome = "y")
  withr::local_options(ggdag.node_size = 24, ggdag.text_size = 6)
  p <- ggdag_instrumental(dag)
  expect_doppelganger("opts-instrumental-large-nodes", p)
})

test_that("ggdag_instrumental() proportional edge_cap works", {
  dag <- dagify(y ~ x + u, x ~ u + z, exposure = "x", outcome = "y")
  withr::local_options(ggdag.edge_cap = 4)
  p <- ggdag_instrumental(dag)
  expect_doppelganger("opts-instrumental-small-cap", p)
})

# Paths
test_that("ggdag_paths() respects global options", {
  withr::local_options(ggdag.node_size = 24, ggdag.text_size = 6)
  p <- ggdag_paths(test_dag)
  expect_doppelganger("opts-paths-large-nodes", p)
})

test_that("ggdag_paths_fan() respects global options", {
  withr::local_options(ggdag.use_stylized = TRUE, ggdag.text_col = "black")
  p <- ggdag_paths_fan(test_dag)
  expect_doppelganger("opts-paths-fan-stylized-black", p)
})

# Colliders
test_that("ggdag_collider() respects global options", {
  dag <- dagify(m ~ x + y, y ~ x)
  withr::local_options(ggdag.node_size = 24, ggdag.edge_type = "arc")
  p <- ggdag_collider(dag)
  expect_doppelganger("opts-collider-large-arc", p)
})

# Equivalence
test_that("ggdag_equivalent_dags() respects global options", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.node_size = 20, ggdag.text_size = 5)
  p <- ggdag_equivalent_dags(dag)
  expect_doppelganger("opts-equivalent-dags-custom-size", p)
})

test_that("ggdag_equivalent_class() respects global options", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.use_stylized = TRUE)
  p <- ggdag_equivalent_class(dag)
  expect_doppelganger("opts-equivalent-class-stylized", p)
})

# Canonical
test_that("ggdag_canonical() respects global options", {
  dag <- dagify(y ~ x + z, x ~ ~z)
  withr::local_options(ggdag.node_size = 24, ggdag.text_size = 6)
  p <- ggdag_canonical(dag)
  expect_doppelganger("opts-canonical-large-nodes", p)
})

# Explicit args still override global options
test_that("explicit args override global options in ggdag()", {
  withr::local_options(ggdag.node_size = 24, ggdag.text_size = 6)
  p <- ggdag(test_dag, node_size = 10, text_size = 2)
  expect_doppelganger("opts-ggdag-explicit-override", p)
})

test_that("explicit args override global options in quick plots", {
  withr::local_options(ggdag.node_size = 24, ggdag.use_stylized = TRUE)
  p <- ggdag_m_bias(node_size = 10, use_stylized = FALSE)
  expect_doppelganger("opts-m-bias-explicit-override", p)
})

# Layout option ---------------------------------------------------------

test_that("layout option validation accepts strings", {
  withr::local_options(ggdag.layout = "fr")
  expect_equal(ggdag_options_get("layout"), "fr")
})

test_that("layout option validation accepts functions", {
  withr::local_options(ggdag.layout = time_ordered_coords())
  expect_true(is.function(ggdag_options_get("layout")))
})

test_that("layout option validation rejects invalid types", {
  expect_ggdag_error(
    ggdag_options_set(layout = 42)
  )
  expect_ggdag_error(
    ggdag_options_set(layout = TRUE)
  )
})

test_that("ggdag_option returns layout default when unset", {
  withr::local_options(ggdag.layout = NULL)
  expect_equal(ggdag_option("layout", "nicely"), "nicely")
})

test_that("ggdag_option returns layout value when set", {
  withr::local_options(ggdag.layout = "fr")
  expect_equal(ggdag_option("layout", "nicely"), "fr")
})

test_that("tidy_dagitty() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "circle")
  td <- tidy_dagitty(dag, use_existing_coords = FALSE)
  coords <- pull_dag_data(td) |>
    dplyr::select(name, x, y) |>
    dplyr::distinct()
  # The circle layout places every node on the unit circle. Any spread of
  # coordinates passes for the default layout too, so the radius is what tells
  # the option through from the option ignored.
  expect_equal(sqrt(coords$x^2 + coords$y^2), rep(1, nrow(coords)))
})

test_that("explicit layout arg overrides global layout option", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "circle")
  td_circle <- tidy_dagitty(dag, use_existing_coords = FALSE)
  td_star <- tidy_dagitty(dag, layout = "star", use_existing_coords = FALSE)
  coords_circle <- pull_dag_data(td_circle) |>
    dplyr::select(name, x, y) |>
    dplyr::distinct()
  coords_star <- pull_dag_data(td_star) |>
    dplyr::select(name, x, y) |>
    dplyr::distinct()
  # Different layouts should produce different coordinates
  expect_false(all(coords_circle$x == coords_star$x))
})

test_that("existing dagitty coords take precedence over global layout option", {
  # DAGs with explicit coordinates should use those coords,
  # not the global layout option
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    coords = list(x = c(x = 1, y = 3, z = 2), y = c(x = 1, y = 1, z = 2))
  )
  withr::local_options(ggdag.layout = "circle")
  td <- tidy_dagitty(dag)
  coords <- pull_dag_data(td) |>
    dplyr::select(name, x, y) |>
    dplyr::distinct() |>
    dplyr::arrange(name)
  # Explicit coords should be preserved despite global layout option
  expect_equal(unname(coords$x[coords$name == "x"]), 1)
  expect_equal(unname(coords$x[coords$name == "y"]), 3)
  expect_equal(unname(coords$x[coords$name == "z"]), 2)
})

# Layout option visual snapshot tests -----------------------------------------

# Core: ggdag() with circle layout
test_that("ggdag() respects global layout option (circle)", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag(dag, use_existing_coords = FALSE)
  expect_doppelganger("opts-ggdag-circle-layout", p)
})

test_that("ggdag() respects global layout option (linear)", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "linear")
  p <- ggdag(dag, use_existing_coords = FALSE)
  expect_doppelganger("opts-ggdag-linear-layout", p)
})

test_that("ggdag() respects global layout option (star)", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "star")
  p <- ggdag(dag, use_existing_coords = FALSE)
  expect_doppelganger("opts-ggdag-star-layout", p)
})

# Layout combined with other options
test_that("ggdag() respects layout + visual options together", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(
    ggdag.layout = "circle",
    ggdag.node_size = 24,
    ggdag.text_size = 6,
    ggdag.use_stylized = TRUE
  )
  p <- ggdag(dag, use_existing_coords = FALSE)
  expect_doppelganger("opts-ggdag-circle-layout-stylized", p)
})

# Relations with layout
test_that("ggdag_children() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag_children(dag, "x", use_existing_coords = FALSE)
  expect_doppelganger("opts-children-circle-layout", p)
})

test_that("ggdag_parents() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag_parents(dag, "y", use_existing_coords = FALSE)
  expect_doppelganger("opts-parents-circle-layout", p)
})

test_that("ggdag_ancestors() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag_ancestors(dag, "y", use_existing_coords = FALSE)
  expect_doppelganger("opts-ancestors-circle-layout", p)
})

# Status with layout
test_that("ggdag_status() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag_status(dag, use_existing_coords = FALSE)
  expect_doppelganger("opts-status-circle-layout", p)
})

# Exogenous with layout
test_that("ggdag_exogenous() respects global layout option", {
  dag <- dagify(y ~ x1 + x2 + x3, b ~ x1 + x2)
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag_exogenous(dag, use_existing_coords = FALSE)
  expect_doppelganger("opts-exogenous-circle-layout", p)
})

# Adjustment sets with layout (pre-tidy since ... doesn't reach tidy_dagitty)
test_that("ggdag_adjustment_set() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")
  withr::local_options(ggdag.layout = "circle")
  td <- tidy_dagitty(dag, use_existing_coords = FALSE)
  p <- ggdag_adjustment_set(td)
  expect_doppelganger("opts-adjustment-set-circle-layout", p)
})

test_that("ggdag_adjust() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")
  withr::local_options(ggdag.layout = "circle")
  td <- tidy_dagitty(dag, use_existing_coords = FALSE)
  p <- ggdag_adjust(td, var = "z")
  expect_doppelganger("opts-adjust-circle-layout", p)
})

# D-relationship with layout (pre-tidy since ... doesn't reach tidy_dagitty)
test_that("ggdag_drelationship() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "circle")
  td <- tidy_dagitty(dag, use_existing_coords = FALSE)
  p <- ggdag_drelationship(td, "x", "y")
  expect_doppelganger("opts-drelationship-circle-layout", p)
})

test_that("ggdag_dseparated() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "circle")
  td <- tidy_dagitty(dag, use_existing_coords = FALSE)
  p <- ggdag_dseparated(td, "x", "y")
  expect_doppelganger("opts-dseparated-circle-layout", p)
})

# Instrumental with layout (pre-tidy since ... doesn't reach tidy_dagitty)
test_that("ggdag_instrumental() respects global layout option", {
  dag <- dagify(y ~ x + u, x ~ u + z, exposure = "x", outcome = "y")
  withr::local_options(ggdag.layout = "circle")
  td <- tidy_dagitty(dag, use_existing_coords = FALSE)
  p <- ggdag_instrumental(td)
  expect_doppelganger("opts-instrumental-circle-layout", p)
})

# Paths with layout
test_that("ggdag_paths() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag_paths(dag, use_existing_coords = FALSE)
  expect_doppelganger("opts-paths-circle-layout", p)
})

test_that("ggdag_paths_fan() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag_paths_fan(dag, use_existing_coords = FALSE)
  expect_doppelganger("opts-paths-fan-circle-layout", p)
})

# Colliders with layout
test_that("ggdag_collider() respects global layout option", {
  dag <- dagify(m ~ x + y, y ~ x)
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag_collider(dag, use_existing_coords = FALSE)
  expect_doppelganger("opts-collider-circle-layout", p)
})

# Equivalence with layout
test_that("ggdag_equivalent_dags() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag_equivalent_dags(dag)
  expect_doppelganger("opts-equivalent-dags-circle-layout", p)
})

test_that("ggdag_equivalent_class() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag_equivalent_class(dag)
  expect_doppelganger("opts-equivalent-class-circle-layout", p)
})

# Canonical with layout
test_that("ggdag_canonical() respects global layout option", {
  dag <- dagify(y ~ x + z, x ~ ~z)
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag_canonical(dag)
  expect_doppelganger("opts-canonical-circle-layout", p)
})

# Explicit layout arg overrides global layout option
test_that("explicit layout arg overrides global layout option in ggdag()", {
  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.layout = "circle")
  p <- ggdag(dag, layout = "star", use_existing_coords = FALSE)
  expect_doppelganger("opts-ggdag-layout-explicit-override", p)
})

# Edge engine option -------------------------------------------------------

test_that("edge_engine option stores and retrieves correctly", {
  withr::local_options(ggdag.edge_engine = NULL)
  ggdag_options_set(edge_engine = "ggarrow")
  expect_equal(ggdag_options_get("edge_engine"), "ggarrow")
})

test_that("edge_engine option accepts 'ggraph'", {
  withr::local_options(ggdag.edge_engine = NULL)
  ggdag_options_set(edge_engine = "ggraph")
  expect_equal(ggdag_options_get("edge_engine"), "ggraph")
})

test_that("edge_engine option rejects invalid values", {
  expect_ggdag_error(ggdag_options_set(edge_engine = "invalid"))
  expect_ggdag_error(ggdag_options_set(edge_engine = 42))
  expect_ggdag_error(ggdag_options_set(edge_engine = TRUE))
})

test_that("ggdag_option returns 'ggraph' default for edge_engine when unset", {
  withr::local_options(ggdag.edge_engine = NULL)
  expect_equal(ggdag_option("edge_engine", "ggraph"), "ggraph")
})

test_that("ggdag_options_reset clears edge_engine option", {
  local_ggdag_option_state()
  withr::local_options(ggdag.edge_engine = "ggarrow")
  ggdag_options_reset()
  expect_null(getOption("ggdag.edge_engine"))
})

# ggarrow parameter options ------------------------------------------------

test_that("arrow_head option accepts NULL", {
  withr::local_options(ggdag.arrow_head = NULL)
  expect_null(ggdag_options_get("arrow_head"))
})

test_that("arrow_head option accepts functions", {
  withr::local_options(ggdag.arrow_head = NULL)
  mock_ornament <- function(...) NULL
  ggdag_options_set(arrow_head = mock_ornament)
  expect_true(is.function(ggdag_options_get("arrow_head")))
})

test_that("arrow_head option accepts matrices", {
  withr::local_options(ggdag.arrow_head = NULL)
  mock_matrix <- matrix(c(0, 1, 1, 0, 0.5, 0.5), ncol = 2)
  ggdag_options_set(arrow_head = mock_matrix)
  expect_true(is.matrix(ggdag_options_get("arrow_head")))
})

test_that("arrow_head option rejects invalid types", {
  expect_ggdag_error(ggdag_options_set(arrow_head = "not_valid"))
  expect_ggdag_error(ggdag_options_set(arrow_head = 42))
  expect_ggdag_error(ggdag_options_set(arrow_head = TRUE))
})

test_that("arrow_fins option accepts functions and matrices", {
  withr::local_options(ggdag.arrow_fins = NULL)
  mock_ornament <- function(...) NULL
  ggdag_options_set(arrow_fins = mock_ornament)
  expect_true(is.function(ggdag_options_get("arrow_fins")))
})

test_that("arrow_fins option rejects invalid types", {
  expect_ggdag_error(ggdag_options_set(arrow_fins = "not_valid"))
})

test_that("arrow_mid option accepts functions and matrices", {
  withr::local_options(ggdag.arrow_mid = NULL)
  mock_ornament <- function(...) NULL
  ggdag_options_set(arrow_mid = mock_ornament)
  expect_true(is.function(ggdag_options_get("arrow_mid")))
})

test_that("arrow_mid option rejects invalid types", {
  expect_ggdag_error(ggdag_options_set(arrow_mid = "not_valid"))
})

test_that("curvature option stores and retrieves correctly", {
  withr::local_options(ggdag.curvature = NULL)
  ggdag_options_set(curvature = 0.5)
  expect_equal(ggdag_options_get("curvature"), 0.5)
})

test_that("curvature option rejects non-numeric", {
  expect_ggdag_error(ggdag_options_set(curvature = "bad"))
  expect_ggdag_error(ggdag_options_set(curvature = TRUE))
})

test_that("debug_repel_points is settable through the options API", {
  local_ggdag_option_state()

  repel_plot <- function() {
    g <- dagify(y ~ x, coords = list(x = c(x = 0, y = 1), y = c(x = 0, y = 0)))
    ggplot(tidy_dagitty(g), aes_dag()) +
      geom_dag_edges() +
      geom_dag_point() +
      geom_dag_label_repel(aes(label = name), seed = 1)
  }
  layer_stats <- function(p) {
    vapply(p$layers, function(l) class(l$stat)[1], character(1))
  }

  expect_no_error(ggdag_options_set(debug_repel_points = TRUE))
  expect_true(isTRUE(ggdag_options_get("debug_repel_points")))
  expect_true("StatDebugRepelPoints" %in% layer_stats(repel_plot()))

  ggdag_options_reset()
  expect_null(ggdag_options_get("debug_repel_points"))
  expect_false("StatDebugRepelPoints" %in% layer_stats(repel_plot()))
})

test_that("debug_repel_points rejects non-logical values", {
  expect_error(
    ggdag_options_set(debug_repel_points = "yes"),
    class = "ggdag_type_error"
  )
})

test_that("ggdag_options_set() unsets an option given NULL", {
  local_ggdag_option_state()

  ggdag_options_set(node_size = 20)
  expect_equal(ggdag_options_get("node_size"), 20)

  expect_no_error(ggdag_options_set(node_size = NULL))
  expect_null(ggdag_options_get("node_size"))
})

test_that("ggdag_options_set() accepts NULL for label_size, its documented default", {
  local_ggdag_option_state()

  ggdag_options_set(label_size = 12)
  expect_no_error(ggdag_options_set(label_size = NULL))
  expect_null(ggdag_options_get("label_size"))
  expect_equal(ggdag_defaults$label_size, ggdag_options_get("label_size"))
})

test_that("ggdag_options_set() round trips through its previous values", {
  local_ggdag_option_state()

  ggdag_options_set(node_size = NULL, text_size = 5)
  old <- ggdag_options_set(node_size = 20, text_size = 8)

  expect_null(old$node_size)
  expect_equal(old$text_size, 5)

  expect_no_error(do.call(ggdag_options_set, old))
  expect_null(ggdag_options_get("node_size"))
  expect_equal(ggdag_options_get("text_size"), 5)
})

test_that("ggdag_options_set() rejects NA for numeric options", {
  local_ggdag_option_state()

  # `NA_real_` reaches the numeric comparison in the validator, where it makes
  # the `if` condition missing rather than false
  expect_error(
    ggdag_options_set(node_size = NA_real_),
    class = "ggdag_type_error"
  )
  expect_error(
    ggdag_options_set(edge_cap = NA_integer_),
    class = "ggdag_type_error"
  )
  expect_null(ggdag_options_get("node_size"))
})

test_that("ggdag_options_set() rejects NA for logical, character, and layout options", {
  local_ggdag_option_state()

  expect_error(ggdag_options_set(use_edges = NA), class = "ggdag_type_error")
  expect_error(
    ggdag_options_set(text_col = NA_character_),
    class = "ggdag_type_error"
  )
  expect_error(
    ggdag_options_set(curvature = NA_real_),
    class = "ggdag_type_error"
  )
  expect_error(
    ggdag_options_set(layout = NA_character_),
    class = "ggdag_type_error"
  )

  expect_null(ggdag_options_get("use_edges"))
  expect_null(ggdag_options_get("text_col"))
  expect_null(ggdag_options_get("curvature"))
  # helper-load_dag.R sets a layout for the whole suite, so what the rejected
  # value must not have done is replace it
  expect_equal(ggdag_options_get("layout"), "time_ordered")
})

test_that("ggdag_options_set() reports NA values through cli", {
  local_ggdag_option_state()

  expect_ggdag_error(ggdag_options_set(use_edges = NA))
})

test_that("ggdag_options_set() rejects an unnamed value", {
  local_ggdag_option_state()

  # an unnamed value names no option, and was stored under the bare `ggdag.`
  # prefix, where nothing reads it again
  expect_error(ggdag_options_set(20), class = "ggdag_type_error")
  expect_error(
    ggdag_options_set(20, text_size = 5),
    class = "ggdag_type_error"
  )
  expect_null(getOption("ggdag."))
  expect_null(ggdag_options_get("text_size"))
})

test_that("ggdag_options_set() still takes named values, including NULL", {
  local_ggdag_option_state()

  expect_no_error(ggdag_options_set(node_size = 20, text_size = 5))
  expect_equal(ggdag_options_get("node_size"), 20)

  expect_no_error(ggdag_options_set(node_size = NULL))
  expect_null(ggdag_options_get("node_size"))
  expect_equal(ggdag_options_get("text_size"), 5)
})

test_that("ggdag_options_set() reports an unnamed value through cli", {
  local_ggdag_option_state()

  expect_ggdag_error(ggdag_options_set(20))
})

# Keep this test last: it guards the suite-wide layout option that
# helper-load_dag.R sets, which the tests above are free to change but must
# restore. A failure here means a test in this file leaked an option change into
# every file that runs after it in the same worker.
test_that("this file leaves the suite-wide layout option intact", {
  expect_equal(getOption("ggdag.layout"), "time_ordered")
})
