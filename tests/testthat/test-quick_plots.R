set.seed(1234)

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
