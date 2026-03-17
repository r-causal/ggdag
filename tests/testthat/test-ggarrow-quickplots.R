# Tests for ggarrow engine support in ggdag_* quick-plot functions
# Covers bd-1gc.21 (engine-aware edges) and bd-nf6 (legend key glyphs)

coords_confounder <- tibble::tribble(
  ~name , ~x , ~y ,
  "x"   ,  0 ,  0 ,
  "y"   ,  2 ,  0 ,
  "z"   ,  1 ,  1
)

confounder_dag <- dagify(
  x ~ z,
  y ~ x + z,
  exposure = "x",
  outcome = "y",
  coords = coords_confounder
)

# ggdag_paths() with ggarrow engine ----------------------------------------

test_that("ggdag_paths() works with ggarrow engine", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  p <- ggdag_paths(confounder_dag, from = "x", to = "y")
  expect_s3_class(p, "ggplot")
  expect_doppelganger("ggdag_paths ggarrow engine", p)
})

test_that("ggdag_paths() with ggarrow engine and shadow = FALSE", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  p <- ggdag_paths(confounder_dag, from = "x", to = "y", shadow = FALSE)
  expect_s3_class(p, "ggplot")
  expect_doppelganger("ggdag_paths ggarrow no shadow", p)
})

test_that("ggdag_paths() with ggarrow supports edge_engine parameter", {
  skip_if_not_installed("ggarrow")

  # Explicit parameter should work even without global option

  p <- ggdag_paths(
    confounder_dag,
    from = "x",
    to = "y",
    edge_engine = "ggarrow"
  )
  expect_s3_class(p, "ggplot")
})

# ggdag_adjustment_set() with ggarrow engine --------------------------------

test_that("ggdag_adjustment_set() works with ggarrow engine", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  p <- ggdag_adjustment_set(confounder_dag)
  expect_s3_class(p, "ggplot")
  expect_doppelganger("ggdag_adjustment_set ggarrow engine", p)
})

test_that("ggdag_adjustment_set() with ggarrow engine and shadow = FALSE", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  p <- ggdag_adjustment_set(confounder_dag, shadow = FALSE)
  expect_s3_class(p, "ggplot")
  expect_doppelganger("ggdag_adjustment_set ggarrow no shadow", p)
})

test_that("ggdag_adjustment_set() with ggarrow supports edge_engine parameter", {
  skip_if_not_installed("ggarrow")

  p <- ggdag_adjustment_set(confounder_dag, edge_engine = "ggarrow")
  expect_s3_class(p, "ggplot")
})

# ggdag_equivalent_class() with ggarrow engine ------------------------------

test_that("ggdag_equivalent_class() works with ggarrow engine", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggdag_equivalent_class(dag)
  expect_s3_class(p, "ggplot")
  expect_doppelganger("ggdag_equivalent_class ggarrow engine", p)
})

test_that("ggdag_equivalent_class() with ggarrow supports edge_engine parameter", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggdag_equivalent_class(dag, edge_engine = "ggarrow")
  expect_s3_class(p, "ggplot")
})

# Legend key glyphs with ggarrow engine ------------------------------------

test_that("draw_key_dag_combined uses ggarrow rendering when engine is ggarrow", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  data <- list(
    colour = "blue",
    fill = "blue",
    alpha = 1,
    shape = 19,
    size = 16
  )
  grob <- draw_key_dag_combined(data, list(), 1)
  expect_s3_class(grob, "gTree")

  # Extract classes of all children in the grobTree
  child_classes <- vapply(
    grob$children,
    function(g) class(g)[1],
    character(1)
  )
  # When ggarrow is active, the arrow portion should be an arrow_path grob
  expect_true(
    any(grepl("arrow_path", child_classes)),
    info = paste(
      "Expected arrow_path grob but found:",
      paste(child_classes, collapse = ", ")
    )
  )
})

test_that("draw_key_dag_collider uses ggarrow rendering when engine is ggarrow", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  data <- list(
    colour = "red",
    fill = "red",
    alpha = 1,
    shape = 19,
    size = 16
  )
  grob <- draw_key_dag_collider(data, list(), 1)
  expect_s3_class(grob, "gTree")

  child_classes <- vapply(
    grob$children,
    function(g) class(g)[1],
    character(1)
  )
  expect_true(
    any(grepl("arrow_path", child_classes)),
    info = paste(
      "Expected arrow_path grob but found:",
      paste(child_classes, collapse = ", ")
    )
  )
})

test_that("draw_key_dag_edge uses ggarrow rendering when engine is ggarrow", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  data <- list(
    colour = "black",
    edge_colour = NULL,
    alpha = 1,
    edge_alpha = NULL,
    edge_width = 0.6,
    edge_linetype = 1,
    linewidth = 0.5,
    linetype = 1
  )
  grob <- draw_key_dag_edge(data, list(), 1)

  # draw_key_dag_edge returns the grob directly (not wrapped in grobTree)
  expect_true(
    any(grepl("arrow_path", class(grob))),
    info = paste(
      "Expected arrow_path grob but found:",
      paste(class(grob), collapse = ", ")
    )
  )
})

test_that("draw_key_dag_combined uses grid::arrow when engine is ggraph", {
  withr::local_options(ggdag.edge_engine = "ggraph")

  data <- list(
    colour = "blue",
    fill = "blue",
    alpha = 1,
    shape = 19,
    size = 16
  )
  grob <- draw_key_dag_combined(data, list(), 1)
  expect_s3_class(grob, "gTree")

  child_classes <- vapply(
    grob$children,
    function(g) class(g)[1],
    character(1)
  )
  # Should contain a segments grob, not an arrow_path
  expect_true(
    any(grepl("segments", child_classes)),
    info = paste(
      "Expected segments grob but found:",
      paste(child_classes, collapse = ", ")
    )
  )
  expect_false(any(grepl("arrow_path", child_classes)))
})
