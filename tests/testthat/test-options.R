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
    "layout"
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
  # Circle layout places nodes on a unit circle
  # Verify that not all nodes are on the same x or y (would indicate default)
  expect_true(length(unique(round(coords$x, 2))) > 1)
  expect_true(length(unique(round(coords$y, 2))) > 1)
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
