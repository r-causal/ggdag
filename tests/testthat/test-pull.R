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

test_that("updating DAG and DAG data work", {
  tidy_dagitty_obj <- dagify(y ~ x + z, x ~ z) %>% tidy_dagitty()
  dag <- pull_dag(tidy_dagitty_obj)
  dag_data <- pull_dag_data(tidy_dagitty_obj)

  tidy_dagitty_obj <- tidy_dagitty_obj %>%
    dplyr::mutate(name = toupper(name), to = toupper(to)) %>%
    # recreate the DAG component
    update_dag()

  big_dag <- dagify(Y ~ X + Z, X ~ Z)
  expect_identical(
    names(big_dag),
    names(pull_dag(tidy_dagitty_obj))
  )

  tidy_dagitty_obj <- tidy_dagitty_obj %>%
    dplyr::left_join(data.frame(
      name = c("Y", "X", "Z"),
      status = c("exposure", "outcome", "latent"),
      adjusted = c("unadjusted", "unadjusted", "adjusted")
    ), by = "name") %>%
    # recreate the DAG component
    update_dag()

  status_dag <- pull_dag(tidy_dagitty_obj)
  expect_identical(dagitty::exposures(status_dag), "Y")
  expect_identical(dagitty::outcomes(status_dag), "X")
  expect_identical(dagitty::latents(status_dag), "Z")
  expect_identical(dagitty::adjustedNodes(status_dag), "Z")

  update_dag(tidy_dagitty_obj) <- big_dag
  expect_identical(pull_dag(tidy_dagitty_obj), big_dag)

  dag_data$label <- paste0(dag_data$name, "(observed)")
  update_dag_data(tidy_dagitty_obj) <- dag_data

  expect_identical(pull_dag_data(tidy_dagitty_obj), dag_data)
})
