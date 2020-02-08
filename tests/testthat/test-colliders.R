context("ggdag_collider")
set.seed(1234)

test_that("colliders correctly identified", {
  dag <- dagify(m ~ x + y, y ~ x)
  p <- ggdag_collider(dag)
  vdiffr::expect_doppelganger("ggdag_collider() highlights `m`", p)
})

test_that("colliders and downstream colliders are detected", {
  dag <- dagify(m ~ x + y, m_jr ~ m)
  expect_true(is_collider(dag, "m"))
  expect_true(is_downstream_collider(dag, "m_jr"))
  expect_false(is_collider(dag, "x"))
  expect_false(is_collider(dag, "y"))
  expect_false(is_downstream_collider(dag, "x"))
  expect_false(is_downstream_collider(dag, "y"))
})

