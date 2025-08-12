test_that("dagify creates correct dagitty", {
  test_dag <- dagify(y ~ x + z, x ~ z)
  expect_equal(test_dag[[1]], "dag {\nx\ny\nz\nx -> y\nz -> x\nz -> y\n}\n")
  expect_s3_class(test_dag, "dagitty")
})

test_that("dagify rejects self-loops with helpful error", {
  expect_error(
    dagify(x ~ x),
    "DAGs cannot contain cycles. Variable 'x' cannot cause itself.",
    fixed = TRUE
  )

  expect_error(
    dagify(y ~ x + y),
    "DAGs cannot contain cycles. Variable 'y' cannot cause itself.",
    fixed = TRUE
  )

  expect_error(
    dagify(y ~ x, x ~ z, z ~ z),
    "DAGs cannot contain cycles. Variable 'z' cannot cause itself.",
    fixed = TRUE
  )
})

test_that("dagify validates exposure and outcome constraints", {
  expect_error(
    dagify(y ~ x, exposure = "x", outcome = "x"),
    "A variable cannot be both exposure and outcome. Found: x",
    fixed = TRUE
  )

  expect_error(
    dagify(y ~ x, exposure = c("x", "y"), outcome = c("y", "z")),
    "A variable cannot be both exposure and outcome. Found: y",
    fixed = TRUE
  )
})

test_that("dagify validates latent variable constraints", {
  expect_error(
    dagify(y ~ x + u, x ~ u, exposure = "u", latent = "u"),
    "Latent variables cannot also be exposures. Found: u",
    fixed = TRUE
  )

  expect_error(
    dagify(y ~ x + u, x ~ u, outcome = "u", latent = "u"),
    "Latent variables cannot also be outcomes. Found: u",
    fixed = TRUE
  )
})

test_that("dagify validates variables exist in DAG", {
  expect_error(
    dagify(y ~ x, exposure = "z"),
    "Exposure variable(s) not found in DAG: z",
    fixed = TRUE
  )

  expect_error(
    dagify(y ~ x, outcome = "z"),
    "Outcome variable(s) not found in DAG: z",
    fixed = TRUE
  )

  expect_error(
    dagify(y ~ x, latent = "z"),
    "Latent variable(s) not found in DAG: z",
    fixed = TRUE
  )

  expect_error(
    dagify(y ~ x, exposure = c("x", "z", "w")),
    "Exposure variable(s) not found in DAG: z, w",
    fixed = TRUE
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
