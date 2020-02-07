test_that("nodes are labelled correctly", {
  labelled_dag <- dagify(y ~ z, x ~ z) %>%
    tidy_dagitty() %>%
    dag_label(labels = c("x" = "exposure", "y" = "outcome", "z" = "confounder"))

  expect_true(has_labels(labelled_dag))
  expect_true("label" %in% names(labelled_dag$data))
  expect_equal(unname(label(labelled_dag)), c("exposure", "outcome", "confounder"))
  expect_named(label(labelled_dag), c("x", "y", "z"), ignore.order = TRUE)
})
