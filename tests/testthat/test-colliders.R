context("ggdag_collider")
set.seed(1234)

test_that("colliders correctly identified", {
  test_dag <- dagify(m ~ x + y, y ~ x)
  p <- ggdag_collider(test_dag)
  expect_doppelganger("ggdag_collider() highlights `m`", p)
})

test_that("colliders and downstream colliders are detected", {
  test_dag <- dagify(m ~ x + y, m_jr ~ m)
  expect_true(is_collider(test_dag, "m"))
  expect_true(is_downstream_collider(test_dag, "m_jr"))
  expect_false(is_collider(test_dag, "x"))
  expect_false(is_collider(test_dag, "y"))
  expect_false(is_downstream_collider(test_dag, "x"))
  expect_false(is_downstream_collider(test_dag, "y"))
})

test_that("many colliders activated are processed correctly", {
  x <- dagify(
    m ~ a + b + d + e + f + g,
    x ~ a,
    y ~ b + x,
    exposure = "x",
    outcome = "y"
  ) %>%
    activate_collider_paths(adjust_for = c("m"))

  expect_true(is.tidy_dagitty(x))
})
