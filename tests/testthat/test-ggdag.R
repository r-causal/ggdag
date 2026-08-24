test_that("basic ggdag quick functions works", {
  p1 <- ggdag(test_dag)
  p2 <- ggdag_classic(test_dag)
  expect_doppelganger("ggdag() plots basic DAG", p1)
  expect_doppelganger("ggdag_classic() plots basic DAG classically", p2)
})

test_that("ggdag() takes the geom_dag() formals its ... would otherwise swallow", {
  passthrough <- c("edge_engine", "n_edge_points", "n_node_points")
  expect_true(all(passthrough %in% names(formals(ggdag))))
})

test_that("ggdag() forwards edge_engine to geom_dag()", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  control <- ggplot(tidy_dagitty(dag), aes_dag()) +
    geom_dag(edge_engine = "ggarrow")

  expect_true(uses_ggarrow_edges(ggdag(dag, edge_engine = "ggarrow")))
  expect_equal(
    purrr::map_chr(
      ggdag(dag, edge_engine = "ggarrow")$layers,
      \(layer) class(layer$geom)[[1]]
    ),
    purrr::map_chr(control$layers, \(layer) class(layer$geom)[[1]])
  )
})

test_that("ggdag() forwards edge_engine for a DAG that carries coordinates", {
  skip_if_not_installed("ggarrow")

  expect_true(uses_ggarrow_edges(ggdag(m_bias(), edge_engine = "ggarrow")))
})

test_that("ggdag() forwards the repel point counts to the label layer", {
  dag <- dagify(y ~ x + z, x ~ z, labels = c(x = "X", y = "Y", z = "Z"))

  p <- ggdag(dag, use_labels = TRUE, n_edge_points = 0, n_node_points = 4)
  repel_layers <- purrr::keep(p$layers, \(layer) {
    inherits(layer$stat, "StatNodesRepel")
  })

  expect_length(repel_layers, 1)
  expect_equal(repel_layers[[1]]$stat_params$n_edge_points, 0)
  expect_equal(repel_layers[[1]]$stat_params$n_node_points, 4)
})
