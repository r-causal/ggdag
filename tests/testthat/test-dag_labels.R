test_that("nodes are labelled correctly", {
  labelled_dag <- dagify(y ~ z, x ~ z) %>%
    tidy_dagitty() %>%
    dag_label(labels = c("x" = "exposure", "y" = "outcome", "z" = "confounder"))

  expect_true(has_labels(pull_dag(labelled_dag)))
  expect_true("label" %in% names(pull_dag_data(labelled_dag)))
  expect_equal(unname(label(pull_dag(labelled_dag))), c("exposure", "outcome", "confounder"))
  expect_named(label(pull_dag(labelled_dag)), c("x", "y", "z"), ignore.order = TRUE)
})
