test_that("pull_dag and pull_dag_data return the correct objects", {
  # Create a dagitty object
  dag <- dagify(y ~ x + z, x ~ z)

  # Create a tidy_dagitty object
  tidy_dag <- tidy_dagitty(dag, seed = 1234)

  # Test that pull_dag returns the correct dagitty object
  expect_equal(pull_dag(dag), dag)

  # Test that pull_dag returns the correct dagitty object from a tidy_dagitty object
  expect_equal(pull_dag(tidy_dag), dag)

  # Test that pull_dag_data returns the correct data frame
  expect_equal(pull_dag_data(dag, seed = 1234), pull_dag_data(tidy_dag))

  # Test that pull_dag_data returns the correct data frame from a tidy_dagitty object
  expect_equal(pull_dag_data(tidy_dag), pull_dag_data(tidy_dag))
})
