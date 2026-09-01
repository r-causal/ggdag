test_that("quick plots render correctly", {
  expect_doppelganger("ggdag_m_bias() is an M", ggdag_m_bias())
  expect_doppelganger(
    "ggdag_butterfly_bias() is a butterfly",
    ggdag_butterfly_bias()
  )
  expect_doppelganger(
    "ggdag_confounder_triangle() is triangle",
    ggdag_confounder_triangle()
  )
  expect_doppelganger(
    "ggdag_collider_triangle() is triangle, too",
    ggdag_collider_triangle()
  )

  # Causal quartet plots
  expect_doppelganger(
    "ggdag_quartet_collider() shows collider structure",
    ggdag_quartet_collider()
  )
  expect_doppelganger(
    "ggdag_quartet_confounder() shows confounder structure",
    ggdag_quartet_confounder()
  )
  expect_doppelganger(
    "ggdag_quartet_mediator() shows mediator structure",
    ggdag_quartet_mediator()
  )
  expect_doppelganger(
    "ggdag_quartet_m_bias() shows m-bias structure",
    ggdag_quartet_m_bias()
  )
  expect_doppelganger(
    "ggdag_quartet_time_collider() shows time-varying structure",
    ggdag_quartet_time_collider()
  )
})

test_that("quick plots forward edge_cap correctly", {
  # Custom edge_cap should visually change where edges stop
  expect_doppelganger(
    "ggdag_m_bias-large-edge-cap",
    ggdag_m_bias(edge_cap = 20)
  )
  expect_doppelganger(
    "ggdag_confounder_triangle-large-edge-cap",
    ggdag_confounder_triangle(edge_cap = 20)
  )
  expect_doppelganger(
    "ggdag_quartet_collider-large-edge-cap",
    ggdag_quartet_collider(edge_cap = 20)
  )
})

test_that("quick plots forward size parameters correctly", {
  # Large nodes with matching edge caps
  expect_doppelganger(
    "ggdag_m_bias-large-nodes",
    ggdag_m_bias(node_size = 24, edge_cap = 12)
  )
  # Scaled via size multiplier
  expect_doppelganger(
    "ggdag_m_bias-size-2x",
    ggdag_m_bias(size = 2)
  )
  # Custom edge_width and arrow_length
  expect_doppelganger(
    "ggdag_butterfly_bias-thick-edges",
    ggdag_butterfly_bias(edge_width = 1.5, arrow_length = 10)
  )
})

test_that("quick plots forward edge_type correctly", {
  expect_doppelganger(
    "ggdag_collider_triangle-arc-edges",
    ggdag_collider_triangle(edge_type = "arc")
  )
  expect_doppelganger(
    "ggdag_quartet_mediator-diagonal-edges",
    ggdag_quartet_mediator(edge_type = "diagonal")
  )
})

# The ten wrappers in R/quick_plots.R, each callable with no arguments.
quick_plot_wrappers <- function() {
  list(
    ggdag_m_bias = ggdag_m_bias,
    ggdag_butterfly_bias = ggdag_butterfly_bias,
    ggdag_confounder_triangle = ggdag_confounder_triangle,
    ggdag_collider_triangle = ggdag_collider_triangle,
    ggdag_mediation_triangle = ggdag_mediation_triangle,
    ggdag_quartet_collider = ggdag_quartet_collider,
    ggdag_quartet_confounder = ggdag_quartet_confounder,
    ggdag_quartet_mediator = ggdag_quartet_mediator,
    ggdag_quartet_m_bias = ggdag_quartet_m_bias,
    ggdag_quartet_time_collider = ggdag_quartet_time_collider
  )
}

test_that("quick plot wrappers forward text to the text layer", {
  labelled <- confounder_triangle(
    x = "Exposure",
    y = "Outcome",
    z = "Confounder"
  )

  expect_equal(
    built_text_labels(
      ggdag_confounder_triangle(
        x = "Exposure",
        y = "Outcome",
        z = "Confounder",
        text = label
      )
    ),
    built_text_labels(ggdag(labelled, text = label))
  )
  expect_equal(
    built_text_labels(
      ggdag_confounder_triangle(
        x = "Exposure",
        y = "Outcome",
        z = "Confounder",
        text = label
      )
    ),
    sort(c("Exposure", "Outcome", "Confounder"))
  )
})

test_that("quartet wrappers forward text to the text layer", {
  expect_equal(
    built_text_labels(
      ggdag_quartet_collider(x = "E", y = "O", z = "C", text = label)
    ),
    sort(c("E", "O", "C"))
  )
})

test_that("quick plot wrappers forward label to the repelling label layer", {
  expect_equal(
    repel_label_expr(ggdag_m_bias(use_labels = TRUE, label = name)),
    repel_label_expr(ggdag(m_bias(), use_labels = TRUE, label = name))
  )
  expect_equal(
    repel_label_expr(ggdag_m_bias(use_labels = TRUE, label = name)),
    "name"
  )
  expect_equal(
    repel_label_expr(ggdag_quartet_collider(use_labels = TRUE, label = name)),
    "name"
  )
})

test_that("quick plot wrappers still honor the deprecated logical text argument", {
  withr::local_options(lifecycle_verbosity = "warning")

  expect_warning(
    p <- ggdag_m_bias(text = FALSE),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(count_geom_layers(p, "GeomDagText"), 0)
})

test_that("quick plot wrappers accept unified_legend and key_glyph", {
  purrr::walk(quick_plot_wrappers(), function(wrapper) {
    expect_s3_class(wrapper(unified_legend = FALSE), "gg")
    expect_s3_class(wrapper(key_glyph = draw_key_dag_point), "gg")
  })
})

test_that("unified_legend = FALSE shows the edge legend in a quick plot wrapper", {
  edge_layer <- layers_by_geom(
    ggdag_m_bias(unified_legend = FALSE),
    "GeomDAGEdgePath"
  )[[1]]

  expect_true(edge_layer$show.legend)
})

test_that("quick plot wrappers accept edge_engine", {
  skip_if_not_installed("ggarrow")

  purrr::walk(quick_plot_wrappers(), function(wrapper) {
    p <- wrapper(edge_engine = "ggarrow")
    expect_s3_class(p, "gg")
    expect_true(uses_ggarrow_edges(p))
  })
})

test_that("a quick plot wrapper chooses the engine without the global option", {
  skip_if_not_installed("ggarrow")

  expect_doppelganger(
    "ggdag_m_bias() with the ggarrow engine",
    ggdag_m_bias(edge_engine = "ggarrow")
  )
})

test_that("quartet_time_collider() labels only the nodes it creates", {
  dag <- quartet_time_collider(
    x0 = "X0",
    x1 = "X1",
    x2 = "X2",
    x3 = "X3",
    y1 = "Y1",
    y2 = "Y2",
    y3 = "Y3",
    z1 = "Z1",
    z2 = "Z2",
    z3 = "Z3"
  )

  nodes <- c("x1", "x2", "y2", "y3", "z2", "z3")
  expect_setequal(names(dag), nodes)
  expect_setequal(names(label(dag)), nodes)

  # every stored label reaches a node in the tidy data
  drawn <- pull_dag_data(tidy_dagitty(dag)) |>
    dplyr::distinct(name, label)
  expect_setequal(drawn$label, unname(label(dag)))
})
