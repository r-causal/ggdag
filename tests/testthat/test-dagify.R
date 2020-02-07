test_that("dagify creates correct dagitty", {
  dag <- dagify(y ~ x + z, x~ z)
  expect_equal(dag[[1]], "dag {\nx\ny\nz\nx -> y\nz -> x\nz -> y\n}\n")
  expect_s3_class(dag, "dagitty")
})
