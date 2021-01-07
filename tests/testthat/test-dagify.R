test_that("dagify creates correct dagitty", {
  test_dag <- dagify(y ~ x + z, x~ z)
  expect_equal(test_dag[[1]], "dag {\nx\ny\nz\nx -> y\nz -> x\nz -> y\n}\n")
  expect_s3_class(test_dag, "dagitty")
})
