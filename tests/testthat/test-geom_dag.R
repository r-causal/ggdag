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

  p1 <- g %>%
    tidy_dagitty() %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(aes(label = name), show.legend = FALSE, seed = 1234)

  p2 <- g %>%
    tidy_dagitty() %>%
    dag_label(
      labels = c(
        "x" = "This is the exposure",
        "y" = "Here's the outcome",
        "m" = "Here is where they collide"
      )
    ) %>%
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

  p3 <- g %>%
    tidy_dagitty() %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_label_repel2(
      aes(label = name),
      show.legend = FALSE,
      seed = 1234
    )

  p4 <- g %>%
    tidy_dagitty() %>%
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

test_that("different edge types work", {
  withr::local_seed(1234)
  p <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    L ~ w1 + w2
  ) %>%
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

  p1 <- g %>%
    tidy_dagitty() %>%
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

  p_circular <- dag %>%
    tidy_dagitty(layout = "linear", circular = TRUE) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges_arc() +
    geom_dag_point() +
    geom_dag_text()

  expect_doppelganger("circular layout with arc edges", p_circular)

  # Test that circular column is present in circular layouts
  tidy_dag_circular <- tidy_dagitty(dag, layout = "linear", circular = TRUE)
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
