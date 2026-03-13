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
