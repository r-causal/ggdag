set.seed(1234)

test_that("repelled labels work", {
  g <- dagify(
    m ~ x + y,
    y ~ x,
    exposure = "x",
    outcome = "y",
    latent = "m",
    labels = c("x" = "Exposure", "y" = "Outcome", "m" = "Collider")
  )

  p1 <- g |>
    tidy_dagitty() |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(aes(label = name), show.legend = FALSE, seed = 1234)

  p2 <- g |>
    tidy_dagitty() |>
    dag_label(
      labels = c(
        "x" = "This is the exposure",
        "y" = "Here's the outcome",
        "m" = "Here is where they collide"
      )
    ) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text() +
    geom_dag_label_repel(
      aes(label = label, fill = label),
      col = "white",
      show.legend = FALSE,
      seed = 1234
    )

  p3 <- g |>
    tidy_dagitty() |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_label_repel2(
      aes(label = name),
      show.legend = FALSE,
      seed = 1234
    )

  p4 <- g |>
    tidy_dagitty() |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel2(
      aes(label = name),
      show.legend = FALSE,
      seed = 1234
    )

  expect_doppelganger("geom_dag_text_repel() repels names", p1)
  expect_doppelganger("geom_dag_label_repel() repels labels", p2)
  expect_doppelganger("geom_dag_label_repel2() repels labels", p3)
  expect_doppelganger("geom_dag_text_repel2() repels names", p4)
})

test_that("repel directional parameters work", {
  withr::local_seed(1234)
  g <- dagify(
    m ~ x + y,
    y ~ x,
    exposure = "x",
    outcome = "y"
  )

  # Test direction = "y"
  p_direction_y <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      direction = "y"
    )

  expect_doppelganger("repel direction y only", p_direction_y)

  # Test direction = "x"
  p_direction_x <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      direction = "x"
    )

  expect_doppelganger("repel direction x only", p_direction_x)
})

test_that("repel segment parameters work", {
  withr::local_seed(1234)
  g <- dagify(
    m ~ x + y,
    y ~ x,
    z ~ x + y,
    exposure = "x",
    outcome = "y"
  )

  # Test segment.linetype
  p_linetype <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      segment.linetype = 2
    )

  expect_doppelganger("repel segment linetype dashed", p_linetype)

  # Test segment.alpha
  p_alpha <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      segment.alpha = 0.3
    )

  expect_doppelganger("repel segment alpha 0.3", p_alpha)

  # Test segment.curvature
  p_curve <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      segment.curvature = -0.5
    )

  expect_doppelganger("repel segment curvature negative", p_curve)

  # Test combination of segment parameters
  p_combo <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_label_repel(
      aes(label = name),
      segment.linetype = 3,
      segment.alpha = 0.5,
      segment.curvature = 0.3,
      segment.size = 1.5
    )

  expect_doppelganger("repel segment parameters combined", p_combo)
})

test_that("repel min.segment.length works", {
  withr::local_seed(1234)
  g <- dagify(
    y ~ x,
    z ~ x,
    a ~ x
  )

  # With default min.segment.length
  p_default <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(aes(label = name))

  expect_doppelganger("repel default min segment length", p_default)

  # With longer min.segment.length
  p_longer <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      min.segment.length = 2
    )

  expect_doppelganger("repel longer min segment length", p_longer)

  # With zero min.segment.length (all segments shown)
  p_zero <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      min.segment.length = 0
    )

  expect_doppelganger("repel zero min segment length", p_zero)
})

test_that("repel xlim and ylim constraints work", {
  withr::local_seed(1234)
  g <- dagify(
    y ~ x + z,
    z ~ a + b,
    x ~ a + b
  )

  # Constrain labels to center region
  p_constrained <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      xlim = c(-0.5, 0.5),
      ylim = c(-0.5, 0.5)
    )

  expect_doppelganger("repel constrained to center", p_constrained)
})

test_that("repel max.overlaps parameter works", {
  withr::local_seed(1234)
  # Create a dense DAG where labels will overlap
  g <- dagify(
    y ~ x1 + x2 + x3 + x4,
    x1 ~ z,
    x2 ~ z,
    x3 ~ z,
    x4 ~ z
  )

  # With default max.overlaps
  p_default_overlaps <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(aes(label = name))

  expect_doppelganger("repel default max overlaps", p_default_overlaps)

  # With restricted max.overlaps
  p_restricted <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      max.overlaps = 2
    )

  expect_doppelganger("repel max overlaps 2", p_restricted)

  # With infinite max.overlaps (show all)
  p_infinite <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      max.overlaps = Inf
    )

  expect_doppelganger("repel infinite max overlaps", p_infinite)
})

test_that("repel seed parameter ensures reproducibility", {
  g <- dagify(
    y ~ x + z,
    z ~ x
  )

  # Same seed should produce same layout
  p_seed1 <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      seed = 9999
    )

  p_seed2 <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      seed = 9999
    )

  # These should be identical
  expect_doppelganger("repel seed 9999 first", p_seed1)
  expect_doppelganger("repel seed 9999 second", p_seed2)

  # Different seed should produce different layout
  p_seed_different <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      seed = 1111
    )

  expect_doppelganger("repel seed 1111", p_seed_different)
})

test_that("repel point.size parameter works", {
  withr::local_seed(1234)
  g <- dagify(
    y ~ x,
    z ~ x
  )

  # Ignore data points completely
  p_no_points <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      point.size = NA
    )

  expect_doppelganger("repel ignoring data points", p_no_points)

  # Large point padding
  p_large_padding <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(
      aes(label = name),
      point.padding = 3
    )

  expect_doppelganger("repel large point padding", p_large_padding)
})

test_that("repel2 functions with custom defaults work visually", {
  withr::local_seed(1234)
  g <- dagify(
    y ~ x + m,
    m ~ x,
    exposure = "x",
    outcome = "y"
  )

  # Test text repel2 with overlapping labels
  p_text_repel2 <- g |>
    tidy_dagitty() |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel2(
      aes(label = name)
    )

  expect_doppelganger("text repel2 custom defaults", p_text_repel2)

  # Test label repel2 with overlapping labels
  p_label_repel2 <- g |>
    tidy_dagitty() |>
    adjust_for("m") |>
    ggplot(aes_dag()) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_label_repel2(
      aes(label = name, fill = adjusted)
    )

  expect_doppelganger("label repel2 custom defaults", p_label_repel2)
})

test_that("different edge types work", {
  withr::local_seed(1234)
  p <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    L ~ w1 + w2
  ) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point() +
    geom_dag_text()

  expect_doppelganger(
    "geom_dag_edges_link() is straight",
    p + geom_dag_edges_link()
  )
  expect_doppelganger("geom_dag_edges_arc() is arcy", p + geom_dag_edges_arc())
  expect_doppelganger(
    "geom_dag_edges_diagonal() is arcy",
    p + geom_dag_edges_diagonal()
  )
  expect_doppelganger("geom_dag_edges_fan() is fany", p + geom_dag_edges_fan())
})
test_that("labels also work", {
  withr::local_seed(1234)
  g <- dagify(
    m ~ x + y,
    y ~ x,
    exposure = "x",
    outcome = "y",
    latent = "m",
    labels = c("x" = "Exposure", "y" = "Outcome", "m" = "Collider")
  )

  p1 <- g |>
    tidy_dagitty() |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_label(aes(label = label))

  expect_doppelganger("geom_dag_label() labels", p1)
})

test_that("circular layouts work correctly", {
  # Test linear circular layout
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    a ~ b,
    b ~ c,
    c ~ a
  )

  expect_warning(
    p_circular <- dag |>
      tidy_dagitty(layout = "linear", circular = TRUE) |>
      ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_dag_edges_arc() +
      geom_dag_point() +
      geom_dag_text(),
    class = "ggdag_cyclic_warning"
  )

  expect_doppelganger("circular layout with arc edges", p_circular)

  # Test that circular column is present in circular layouts
  expect_warning(
    tidy_dag_circular <- tidy_dagitty(dag, layout = "linear", circular = TRUE),
    class = "ggdag_cyclic_warning"
  )
  expect_true("circular" %in% names(pull_dag_data(tidy_dag_circular)))
  expect_true(all(pull_dag_data(tidy_dag_circular)$circular))
})

test_that("color aesthetic is mapped to edge_color for edge geoms", {
  dag <- dagify(y ~ x + z, x ~ z)
  tidy_dag <- tidy_dagitty(dag)

  # Test color mapping for geom_dag_edges_link
  p1 <- ggplot(tidy_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges_link(aes(color = name))

  # Build the plot to check the actual data
  built_p1 <- ggplot2::ggplot_build(p1)
  edge_data <- built_p1$data[[1]]

  # Verify that edge_colour is set from the name variable
  expect_true("edge_colour" %in% names(edge_data))
  # Should have different colors for different edges
  expect_true(length(unique(edge_data$edge_colour)) > 1)

  # Test colour mapping (British spelling)
  p2 <- ggplot(tidy_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges_arc(aes(colour = name))

  built_p2 <- ggplot2::ggplot_build(p2)
  edge_data2 <- built_p2$data[[1]]
  expect_true("edge_colour" %in% names(edge_data2))
  expect_true(length(unique(edge_data2$edge_colour)) > 1)

  # Test that existing edge_color mapping is preserved
  p3 <- ggplot(tidy_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges_diagonal(aes(color = name, edge_color = to))

  layer_mapping3 <- p3$layers[[1]]$mapping
  # Note: aes() converts color to colour internally
  expect_equal(rlang::as_label(layer_mapping3$edge_colour), "to")

  # Build to verify the edge_color is actually from 'to' not 'name'
  built_p3 <- ggplot2::ggplot_build(p3)
  edge_data3 <- built_p3$data[[1]]

  # The edge colors should be based on 'to' values, not 'name' values
  # We can check this by verifying the number of unique colors matches 'to' categories
  expect_true("edge_colour" %in% names(edge_data3))

  # Test that mapping is NOT applied when not explicitly provided to edge geom
  p4 <- ggplot(
    tidy_dag,
    aes(x = x, y = y, xend = xend, yend = yend, color = name)
  ) +
    geom_dag_edges_link()

  # The edge geom should not have edge_color mapping since color was in plot aes
  layer_mapping4 <- p4$layers[[1]]$mapping
  expect_false("edge_color" %in% names(layer_mapping4))
  expect_false("color" %in% names(layer_mapping4))
})
