test_that("dagify creates correct dagitty", {
  test_dag <- dagify(y ~ x + z, x ~ z)
  expect_equal(test_dag[[1]], "dag {\nx\ny\nz\nx -> y\nz -> x\nz -> y\n}\n")
  expect_s3_class(test_dag, "dagitty")
})

test_that("dagify rejects self-loops with helpful error", {
  expect_ggdag_error(
    dagify(x ~ x)
  )

  expect_ggdag_error(
    dagify(y ~ x + y)
  )

  expect_ggdag_error(
    dagify(y ~ x, x ~ z, z ~ z)
  )
})

test_that("dagify validates exposure and outcome constraints", {
  expect_ggdag_error(
    dagify(y ~ x, exposure = "x", outcome = "x")
  )

  expect_ggdag_error(
    dagify(y ~ x, exposure = c("x", "y"), outcome = c("y", "z"))
  )
})

test_that("dagify validates latent variable constraints", {
  expect_ggdag_error(
    dagify(y ~ x + u, x ~ u, exposure = "u", latent = "u")
  )

  expect_ggdag_error(
    dagify(y ~ x + u, x ~ u, outcome = "u", latent = "u")
  )
})

test_that("dagify validates variables exist in DAG", {
  expect_ggdag_error(
    dagify(y ~ x, exposure = "z")
  )

  expect_ggdag_error(
    dagify(y ~ x, outcome = "z")
  )

  expect_ggdag_error(
    dagify(y ~ x, latent = "z")
  )

  expect_ggdag_error(
    dagify(y ~ x, exposure = c("x", "z", "w"))
  )
})

test_that("dagify accepts valid DAG specifications", {
  expect_silent(
    dag1 <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")
  )
  expect_s3_class(dag1, "dagitty")

  expect_silent(
    dag2 <- dagify(
      y ~ x + u,
      x ~ u,
      latent = "u",
      exposure = "x",
      outcome = "y"
    )
  )
  expect_s3_class(dag2, "dagitty")

  expect_silent(
    dag3 <- dagify(y ~ x + z, x ~ ~z, exposure = "x", outcome = "y")
  )
  expect_s3_class(dag3, "dagitty")
})
