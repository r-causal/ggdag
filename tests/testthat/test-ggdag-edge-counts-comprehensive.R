# Comprehensive edge count tests for all major ggdag_*() functions
# This ensures they properly handle use_edges parameter and don't overplot

test_that("all ggdag_*() functions have exact expected edge counts", {
  # Create a standard test DAG with known structure
  test_dag <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2, # bidirected edge
    exposure = "x",
    outcome = "y"
  )

  # This DAG has 11 edges total
  n_edges <- nrow(dagitty::edges(test_dag))
  expect_equal(n_edges, 11)

  # Helper function to test a ggdag function
  test_ggdag_function <- function(
    plot_func,
    func_name,
    ...,
    expected_with_edges
  ) {
    # Test with use_edges = TRUE
    p_with <- plot_func(test_dag, ..., use_edges = TRUE)
    analysis_with <- analyze_plot_edges(p_with)

    # Test with use_edges = FALSE
    p_without <- plot_func(test_dag, ..., use_edges = FALSE)
    analysis_without <- analyze_plot_edges(p_without)

    # Check exact edge counts
    expect_equal(
      analysis_with$total_edges,
      expected_with_edges,
      info = paste(
        func_name,
        "should have exactly",
        expected_with_edges,
        "edges when use_edges = TRUE"
      )
    )

    expect_equal(
      analysis_without$total_edges,
      0,
      info = paste(
        func_name,
        "should have exactly 0 edges when use_edges = FALSE"
      )
    )

    invisible(list(
      with_edges = analysis_with,
      without_edges = analysis_without
    ))
  }

  # Most functions show the whole DAG without faceting = 11 edges
  test_ggdag_function(
    ggdag_ancestors,
    "ggdag_ancestors",
    .var = "x",
    expected_with_edges = 11
  )
  test_ggdag_function(
    ggdag_descendants,
    "ggdag_descendants",
    .var = "v",
    expected_with_edges = 11
  )
  test_ggdag_function(
    ggdag_parents,
    "ggdag_parents",
    .var = "y",
    expected_with_edges = 11
  )
  test_ggdag_function(
    ggdag_children,
    "ggdag_children",
    .var = "v",
    expected_with_edges = 11
  )
  test_ggdag_function(
    ggdag_adjacent,
    "ggdag_adjacent",
    .var = "w1",
    expected_with_edges = 11
  )
  test_ggdag_function(
    ggdag_markov_blanket,
    "ggdag_markov_blanket",
    .var = "x",
    expected_with_edges = 11
  )
  test_ggdag_function(ggdag_status, "ggdag_status", expected_with_edges = 11)
  test_ggdag_function(
    ggdag_exogenous,
    "ggdag_exogenous",
    expected_with_edges = 11
  )
  test_ggdag_function(
    ggdag_drelationship,
    "ggdag_drelationship",
    from = "x",
    to = "y",
    expected_with_edges = 11
  )
  test_ggdag_function(
    ggdag_dconnected,
    "ggdag_dconnected",
    from = "x",
    to = "y",
    expected_with_edges = 11
  )
  test_ggdag_function(
    ggdag_dseparated,
    "ggdag_dseparated",
    from = "v",
    to = "y",
    expected_with_edges = 11
  )
  test_ggdag_function(
    ggdag_collider,
    "ggdag_collider",
    expected_with_edges = 11
  )
  # ggdag_instrumental facets by instrumental variables
  iv_list <- dagitty::instrumentalVariables(
    test_dag,
    exposure = "x",
    outcome = "y"
  )
  n_instrumental <- length(iv_list)
  # If no instrumental variables, it shows one panel with message
  n_panels <- max(n_instrumental, 1)
  test_ggdag_function(
    ggdag_instrumental,
    "ggdag_instrumental",
    exposure = "x",
    outcome = "y",
    expected_with_edges = 11 * n_panels
  )

  # ggdag_canonical might modify the DAG structure, need to check
  p_canonical <- ggdag_canonical(test_dag)
  canonical_data <- ggplot2::ggplot_build(p_canonical)$data[[1]]
  test_ggdag_function(
    ggdag_canonical,
    "ggdag_canonical",
    # after canonicalization, the bidirected edge becomes two directed edges
    # resulting in 12 edges total
    expected_with_edges = 12
  )
})

test_that("simple example DAG functions have exact expected edge counts", {
  # Collider triangle: x -> m <- y (2 edges)
  p_collider_with <- ggdag_collider_triangle(use_edges = TRUE)
  analysis_collider_with <- analyze_plot_edges(p_collider_with)
  expect_equal(
    analysis_collider_with$total_edges,
    2,
    info = "Collider triangle should have exactly 2 edges"
  )

  p_collider_without <- ggdag_collider_triangle(use_edges = FALSE)
  analysis_collider_without <- analyze_plot_edges(p_collider_without)
  expect_equal(analysis_collider_without$total_edges, 0)

  # Confounder triangle: z -> x, z -> y (2 edges by default, 3 if x_y_associated = TRUE)
  p_confounder_with <- ggdag_confounder_triangle(use_edges = TRUE)
  analysis_confounder_with <- analyze_plot_edges(p_confounder_with)
  expect_equal(
    analysis_confounder_with$total_edges,
    2,
    info = "Confounder triangle should have exactly 2 edges (no x->y by default)"
  )

  # Test with x_y_associated = TRUE
  p_confounder_with_xy <- ggdag_confounder_triangle(
    x_y_associated = TRUE,
    use_edges = TRUE
  )
  analysis_confounder_with_xy <- analyze_plot_edges(p_confounder_with_xy)
  expect_equal(
    analysis_confounder_with_xy$total_edges,
    3,
    info = "Confounder triangle with x_y_associated should have exactly 3 edges"
  )

  p_confounder_without <- ggdag_confounder_triangle(use_edges = FALSE)
  analysis_confounder_without <- analyze_plot_edges(p_confounder_without)
  expect_equal(analysis_confounder_without$total_edges, 0)

  # M-bias: count edges in the created DAG
  m_bias_dag <- m_bias()
  n_edges_m_bias <- nrow(dagitty::edges(m_bias_dag))

  p_m_bias_with <- ggdag_m_bias(use_edges = TRUE)
  analysis_m_bias_with <- analyze_plot_edges(p_m_bias_with)
  expect_equal(
    analysis_m_bias_with$total_edges,
    n_edges_m_bias,
    info = paste("M-bias should have exactly", n_edges_m_bias, "edges")
  )

  p_m_bias_without <- ggdag_m_bias(use_edges = FALSE)
  analysis_m_bias_without <- analyze_plot_edges(p_m_bias_without)
  expect_equal(analysis_m_bias_without$total_edges, 0)

  # Butterfly bias: count edges in the created DAG
  butterfly_dag <- butterfly_bias()
  n_edges_butterfly <- nrow(dagitty::edges(butterfly_dag))

  p_butterfly_with <- ggdag_butterfly_bias(use_edges = TRUE)
  analysis_butterfly_with <- analyze_plot_edges(p_butterfly_with)
  expect_equal(
    analysis_butterfly_with$total_edges,
    n_edges_butterfly,
    info = paste(
      "Butterfly bias should have exactly",
      n_edges_butterfly,
      "edges"
    )
  )

  p_butterfly_without <- ggdag_butterfly_bias(use_edges = FALSE)
  analysis_butterfly_without <- analyze_plot_edges(p_butterfly_without)
  expect_equal(analysis_butterfly_without$total_edges, 0)

  # Mediation triangle: x -> m -> y (2 edges by default, 3 if x_y_associated = TRUE)
  p_mediation_with <- ggdag_mediation_triangle(use_edges = TRUE)
  analysis_mediation_with <- analyze_plot_edges(p_mediation_with)
  expect_equal(
    analysis_mediation_with$total_edges,
    2,
    info = "Mediation triangle should have exactly 2 edges (no x->y by default)"
  )

  # Test with x_y_associated = TRUE
  p_mediation_with_xy <- ggdag_mediation_triangle(
    x_y_associated = TRUE,
    use_edges = TRUE
  )
  analysis_mediation_with_xy <- analyze_plot_edges(p_mediation_with_xy)
  expect_equal(
    analysis_mediation_with_xy$total_edges,
    3,
    info = "Mediation triangle with x_y_associated should have exactly 3 edges"
  )

  p_mediation_without <- ggdag_mediation_triangle(use_edges = FALSE)
  analysis_mediation_without <- analyze_plot_edges(p_mediation_without)
  expect_equal(analysis_mediation_without$total_edges, 0)
})

test_that("ggdag and ggdag_classic have exact expected edge counts", {
  dag <- dagify(y ~ x + z, x ~ z)
  n_edges <- nrow(dagitty::edges(dag))
  expect_equal(n_edges, 3)

  # ggdag
  p_ggdag_with <- ggdag(dag, use_edges = TRUE)
  analysis_ggdag_with <- analyze_plot_edges(p_ggdag_with)
  expect_equal(
    analysis_ggdag_with$total_edges,
    3,
    info = "ggdag should have exactly 3 edges"
  )

  p_ggdag_without <- ggdag(dag, use_edges = FALSE)
  analysis_ggdag_without <- analyze_plot_edges(p_ggdag_without)
  expect_equal(analysis_ggdag_without$total_edges, 0)

  # ggdag_classic
  p_classic_with <- ggdag_classic(dag, use_edges = TRUE)
  analysis_classic_with <- analyze_plot_edges(p_classic_with)
  expect_equal(
    analysis_classic_with$total_edges,
    3,
    info = "ggdag_classic should have exactly 3 edges"
  )

  p_classic_without <- ggdag_classic(dag, use_edges = FALSE)
  analysis_classic_without <- analyze_plot_edges(p_classic_without)
  expect_equal(analysis_classic_without$total_edges, 0)
})

test_that("ggdag_equivalent_dags has exact expected edge counts", {
  # Simple DAG with reversible edges
  dag <- dagify(y ~ x + z, x ~ z)
  n_edges <- nrow(dagitty::edges(dag))
  expect_equal(n_edges, 3)

  # Count equivalent DAGs
  equiv_dags <- node_equivalent_dags(dag)
  dag_data <- pull_dag_data(equiv_dags)
  n_dags <- length(unique(dag_data$dag))

  # Each equivalent DAG shows all edges, so n_edges * n_dags
  p_with <- ggdag_equivalent_dags(dag, use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with)
  expect_equal(
    analysis_with$total_edges,
    n_edges * n_dags,
    info = paste("ggdag_equivalent_dags should have", n_edges * n_dags, "edges")
  )

  p_without <- ggdag_equivalent_dags(dag, use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without)
  expect_equal(analysis_without$total_edges, 0)
})
