set.seed(1234)

test_that("dags identify IVs correctly", {
  p <- ggdag_instrumental(dagitty::dagitty("dag{ i->x->y; i2->x->y; x<->y }"), "x", "y")
  expect_doppelganger("ggdag_instrumental() identifies `i` and `i2` as instrumental", p)
})

test_that("dags without IVs are shown correctly", {
  no_iv <- dagify(
    y ~ t + x1 + x2 + x4,
    t ~ x1 + x3,
    x2 ~ x3 + x4,
    exposure = "t",
    outcome = "y",
    latent = c("x1", "x4")
  )

  p <- ggdag_instrumental(no_iv)
  expect_doppelganger("ggdag_instrumental() identifies nothing as instrumental", p)
})

test_that("dags with colliders + IVs are shown correctly", {
  iv_collider <- dagify(
    y ~ t + x1 + x2 + x4,
    t ~ x1 + x3,
    x2 ~ x3 + x4,
    exposure = "t",
    outcome = "y",
    latent = c("x1")
  )

  p <- ggdag_instrumental(iv_collider)
  expect_doppelganger("ggdag_instrumental() instrumental plus collider", p)
})
