set.seed(1234)

expect_names <- function(object, expectation) {
  obj_names <- names(object)
  expect_true(all(expectation %in% obj_names))
}

test_that("dags ............", {
  # non-reversible dag
  p1 <- ggdag_equivalent_dags(test_dag)
  p2 <- ggdag_equivalent_class(test_dag)
  expect_doppelganger("ggdag_equivalent_dags() plots no equivalent dags", p1)
  expect_doppelganger("ggdag_equivalent_class() plots no reversible edges", p2)

  # reversible dag
  g_ex <- dagify(y ~ x + z, x ~ z)
  p3 <- ggdag_equivalent_dags(g_ex)
  p4 <- ggdag_equivalent_class(g_ex)
  expect_doppelganger("ggdag_equivalent_dags() plots 6 equivalent dags", p3)
  expect_doppelganger("ggdag_equivalent_class() plots all reversible edges", p4)

  # equivalent dags work with labels and maintain other columns from original dag
  labelled_dag <- dagify(
    y ~ x,
    y ~ z,
    x ~ z,
    labels = c(
      "y" = "Outcome",
      "x" = "Exposure",
      "z" = "Confounder"
    ),
    exposure = "x",
    outcome = "y"
  ) %>%
    tidy_dagitty()

  labelled_dag2 <- labelled_dag %>%
    # also add node status
    node_status() %>%
    node_equivalent_dags()

  expect_names(pull_dag_data(labelled_dag2), c("label", "status"))

  p5 <- ggdag_equivalent_dags(labelled_dag, use_labels = TRUE)
  expect_doppelganger("ggdag_equivalent_class() plots labels", p5)
})

test_that("ggdag_equivalent_class respects use_edges parameter (issue #167)", {
  # Create a DAG with reversible edges: y ~ x + z, x ~ z
  # This DAG has 3 edges total: y <- x, y <- z, x <- z
  # In equivalent class: y <- z is reversible, others are not
  dag <- dagify(y ~ x + z, x ~ z)
  
  # Test with use_edges = TRUE (should have 3 edges)
  p_with_edges <- ggdag_equivalent_class(dag, use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with_edges)
  
  expect_true(analysis_with$has_edge_layers, "Should have edge layers when use_edges = TRUE")
  expect_equal(analysis_with$total_edges, 3, 
               info = "Should have exactly 3 edges when use_edges = TRUE")
  expect_gt(analysis_with$edge_layers, 0, "Should have at least 1 edge layer when use_edges = TRUE")
  
  # Test with use_edges = FALSE (should have NO edges)
  p_without_edges <- ggdag_equivalent_class(dag, use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without_edges)
  
  expect_false(analysis_without$has_edge_layers, 
               paste("Should have NO edge layers when use_edges = FALSE, but found:", 
                     analysis_without$edge_layers))
  expect_equal(analysis_without$total_edges, 0,
               info = paste("Should have 0 edges when use_edges = FALSE, but found:", 
                           analysis_without$total_edges))
  expect_equal(analysis_without$edge_layers, 0, 
               info = "Should have 0 edge layers when use_edges = FALSE")
})
