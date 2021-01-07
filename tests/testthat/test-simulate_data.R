set.seed(1234)
test_that("dags simulate correctly", {
  dag_sim <- simulate_data(test_dag, N = 50)
  expect_s3_class(dag_sim, "tbl")
  expect_length(dag_sim, 7)
  expect_equal(nrow(dag_sim), 50)
})
