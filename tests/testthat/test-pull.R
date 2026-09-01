test_that("pull_dag and pull_dag_data return the correct objects", {
  # Create a dagitty object
  dag <- dagify(y ~ x + z, x ~ z, coords = time_ordered_coords())

  # Create a tidy_dagitty object
  tidy_dag <- tidy_dagitty(dag, seed = 1234)

  # Test that pull_dag returns the correct dagitty object
  expect_equal(pull_dag(dag), dag)

  # Test that pull_dag returns the correct dagitty object from a tidy_dagitty object
  expect_equal(pull_dag(tidy_dag), dag)

  # Test that pull_dag_data returns the correct data frame
  expect_equal(pull_dag_data(dag, seed = 1234), pull_dag_data(tidy_dag))

  # Test that pull_dag_data returns the data component of a tidy_dagitty object
  expect_identical(pull_dag_data(tidy_dag), tidy_dag$data)
})

test_that("updating DAG and DAG data work", {
  tidy_dagitty_obj <- dagify(y ~ x + z, x ~ z) |> tidy_dagitty()
  dag <- pull_dag(tidy_dagitty_obj)
  dag_data <- pull_dag_data(tidy_dagitty_obj)

  tidy_dagitty_obj <- tidy_dagitty_obj |>
    dplyr::mutate(name = toupper(name), to = toupper(to)) |>
    # recreate the DAG component
    update_dag()

  big_dag <- dagify(Y ~ X + Z, X ~ Z)
  expect_identical(
    names(big_dag),
    names(pull_dag(tidy_dagitty_obj))
  )

  tidy_dagitty_obj <- tidy_dagitty_obj |>
    dplyr::left_join(
      data.frame(
        name = c("Y", "X", "Z"),
        status = c("exposure", "outcome", "latent"),
        adjusted = c("unadjusted", "unadjusted", "adjusted")
      ),
      by = "name"
    ) |>
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

test_that("update_dag() preserves isolated nodes", {
  dag <- dagitty::dagitty("dag { a -> b ; c }")
  tidy_dag <- tidy_dagitty(dag, seed = 42)
  expect_true("c" %in% pull_dag_data(tidy_dag)$name)

  round_tripped <- update_dag(tidy_dag)
  expect_setequal(names(pull_dag(round_tripped)), c("a", "b", "c"))
  expect_setequal(unique(pull_dag_data(round_tripped)$name), c("a", "b", "c"))
})

test_that("update_dag() round-trips node names with spaces", {
  dag <- dagitty::dagitty('dag { "my var" -> y }')
  tidy_dag <- tidy_dagitty(dag, seed = 42)
  expect_setequal(names(pull_dag(tidy_dag)), c("my var", "y"))

  round_tripped <- update_dag(tidy_dag)
  expect_setequal(names(pull_dag(round_tripped)), c("my var", "y"))
  expect_setequal(
    names(dagitty::coordinates(pull_dag(round_tripped))$x),
    c("my var", "y")
  )
})

test_that("a dplyr verb on complete coordinates computes no layout", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))

  local_mocked_bindings(
    compute_time_ordered_layout = function(...) {
      stop("computed a layout that was already there")
    }
  )

  expect_no_error(dplyr::mutate(.tdy_dag, marked = TRUE))
  expect_no_error(dplyr::filter(.tdy_dag, name != "z"))
  expect_no_error(dplyr::arrange(.tdy_dag, name))
})

test_that("a missing coordinate column still regenerates the layout", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))
  without_x <- dplyr::select(pull_dag_data(.tdy_dag), -"x")

  local_mocked_bindings(
    compute_time_ordered_layout = function(...) {
      stop("regenerated the layout")
    }
  )

  expect_error(
    update_dag_data(.tdy_dag) <- without_x,
    "regenerated the layout"
  )
})

test_that("a rebuilt layout covers a node a verb added", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))
  dag_data <- pull_dag_data(.tdy_dag) |>
    dplyr::mutate(direction = as.character(direction)) |>
    dplyr::select(-"x", -"y", -"xend", -"yend") |>
    dplyr::bind_rows(tibble::tibble(name = "w", to = "y", direction = "->"))

  update_dag_data(.tdy_dag) <- dag_data
  rebuilt <- pull_dag_data(.tdy_dag)

  expect_true(all(c("x", "y", "xend", "yend") %in% names(rebuilt)))
  expect_false(anyNA(rebuilt$x))
})
