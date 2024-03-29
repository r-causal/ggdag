set.seed(1234)

test_that("basic ggdag quick functions works", {
  p1 <- ggdag(test_dag)
  p2 <- ggdag_classic(test_dag)
  expect_doppelganger("ggdag() plots basic DAG", p1)
  expect_doppelganger("ggdag_classic() plots basic DAG classically", p2)
})
