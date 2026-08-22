test_that("nodes are labelled correctly", {
  labelled_dag <- dagify(y ~ z, x ~ z) |>
    tidy_dagitty() |>
    dag_label(labels = c("x" = "exposure", "y" = "outcome", "z" = "confounder"))

  expect_true(has_labels(pull_dag(labelled_dag)))
  expect_true("label" %in% names(pull_dag_data(labelled_dag)))
  expect_equal(
    unname(label(pull_dag(labelled_dag))),
    c("exposure", "outcome", "confounder")
  )
  expect_named(
    label(pull_dag(labelled_dag)),
    c("x", "y", "z"),
    ignore.order = TRUE
  )
})

test_that("label() and has_labels() work on tidy_dagitty objects", {
  labels <- c("x" = "exposure", "y" = "outcome", "z" = "confounder")
  labelled_dag <- dagify(y ~ z, x ~ z) |>
    tidy_dagitty() |>
    dag_label(labels = labels)

  expect_true(has_labels(labelled_dag))
  expect_equal(label(labelled_dag), labels)
  expect_equal(label(labelled_dag), label(pull_dag(labelled_dag)))

  unlabelled_dag <- tidy_dagitty(dagify(y ~ x))
  expect_false(has_labels(unlabelled_dag))
  expect_null(label(unlabelled_dag))
})

test_that("`label<-` makes labels reachable from the tidy_dagitty", {
  labels <- c("x" = "The Exposure", "y" = "The Outcome")
  tidy_dag <- tidy_dagitty(dagify(y ~ x))
  label(tidy_dag) <- labels

  expect_true(has_labels(tidy_dag))
  expect_equal(label(tidy_dag), labels)
  expect_true("label" %in% names(pull_dag_data(tidy_dag)))
})

test_that("labels must be a named character vector", {
  tidy_dag <- tidy_dagitty(dagify(y ~ x))
  .dag <- dagify(y ~ x)

  expect_error(
    dagify(y ~ x, labels = c("The Exposure", "The Outcome")),
    class = "ggdag_type_error"
  )
  expect_error(
    dag_label(tidy_dag, labels = c("The Exposure", "The Outcome")),
    class = "ggdag_type_error"
  )
  expect_error(
    dag_label(tidy_dag, labels = c("x" = "The Exposure", "The Outcome")),
    class = "ggdag_type_error"
  )
  expect_error(
    {
      label(.dag) <- c("The Exposure", "The Outcome")
    },
    class = "ggdag_type_error"
  )
  expect_error(
    {
      label(tidy_dag) <- c("The Exposure", "The Outcome")
    },
    class = "ggdag_type_error"
  )
  expect_error(
    as_tidy_dagitty(
      data.frame(name = "x", to = "y"),
      labels = c("The Exposure", "The Outcome")
    ),
    class = "ggdag_type_error"
  )

  # fully named labels still round-trip
  labels <- c("x" = "The Exposure", "y" = "The Outcome")
  labelled_dag <- dagify(y ~ x, labels = labels) |> tidy_dagitty()
  expect_equal(label(pull_dag(labelled_dag)), labels)
  dag_data <- pull_dag_data(labelled_dag)
  expect_equal(unique(dag_data$label[dag_data$name == "x"]), "The Exposure")
})

test_that("zero-length labels are rejected like other unnamed labels", {
  .dag <- dagify(y ~ x)
  expect_error(
    {
      label(.dag) <- character(0)
    },
    class = "ggdag_type_error"
  )
})

test_that("labels naming the same node twice are rejected", {
  tidy_dag <- tidy_dagitty(dagify(y ~ x))
  .dag <- dagify(y ~ x)
  duplicated_labels <- c("x" = "The Exposure", "x" = "Also The Exposure")

  expect_error(
    dagify(y ~ x, labels = duplicated_labels),
    class = "ggdag_type_error"
  )
  expect_error(
    dag_label(tidy_dag, labels = duplicated_labels),
    class = "ggdag_type_error"
  )
  expect_error(
    {
      label(.dag) <- duplicated_labels
    },
    class = "ggdag_type_error"
  )
  expect_error(
    {
      label(tidy_dag) <- duplicated_labels
    },
    class = "ggdag_type_error"
  )
})

test_that("duplicated label names produce an informative message", {
  # never record a baseline from the pre-fix duplicated join rows
  skip_if_not(inherits(
    tryCatch(
      dagify(y ~ x, labels = c("x" = "The Exposure", "x" = "Also x")),
      error = identity
    ),
    "ggdag_type_error"
  ))

  expect_ggdag_error(
    dagify(y ~ x, labels = c("x" = "The Exposure", "x" = "Also x"))
  )
})

test_that("unnamed labels produce an informative message", {
  # never record a baseline from the pre-fix dplyr join error
  skip_if_not(inherits(
    tryCatch(
      dagify(y ~ x, labels = c("The Exposure", "The Outcome")),
      error = identity
    ),
    "ggdag_type_error"
  ))

  expect_ggdag_error(dagify(y ~ x, labels = c("The Exposure", "The Outcome")))
})
