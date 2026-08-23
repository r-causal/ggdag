d_labels <- function(.result, nodes) {
  .df <- dplyr::distinct(
    dplyr::select(pull_dag_data(.result), "name", "d_relationship")
  )
  as.character(.df$d_relationship[match(nodes, .df$name)])
}

added_columns <- function(.result, .dag) {
  setdiff(
    names(pull_dag_data(.result)),
    names(pull_dag_data(tidy_dagitty(.dag)))
  )
}

test_that("d relationships correctly identified", {
  withr::local_seed(1234)
  test_dag <- dagify(m ~ x + y) |>
    tidy_dagitty()
  p1 <- ggdag_drelationship(test_dag, "x", "y")
  p2 <- ggdag_drelationship(test_dag, "x", "y", controlling_for = "m")
  p3 <- ggdag_drelationship(
    test_dag,
    "x",
    "y",
    controlling_for = "m",
    collider_lines = FALSE
  )
  expect_doppelganger("ggdag_drelationship() d-separates x and y", p1)
  expect_doppelganger(
    "ggdag_drelationship() d-connects x and y",
    p2 + theme_test() + theme(legend.position = "none")
  )
  expect_doppelganger("ggdag_drelationship() d-connects xy: no collider", p3)

  p4 <- ggdag_dseparated(test_dag, "x", "y")
  p5 <- ggdag_dconnected(test_dag, "x", "y")
  expect_doppelganger("ggdag_dseparated() d-separates x and y", p4)
  expect_doppelganger("ggdag_dconnected() d-separates x and y", p5)
})

test_that("node_d*() label each endpoint with its own d-relationship", {
  dag <- dagify(y ~ a, b ~ q)
  nodes <- c("a", "b", "y")
  # `a` is d-connected to `y`; `b` sits in a separate component
  expected <- c("d-connected", "d-separated", "d-connected")

  expect_equal(
    d_labels(node_dconnected(dag, from = c("a", "b"), to = "y"), nodes),
    expected
  )
  expect_equal(
    d_labels(node_dseparated(dag, from = c("a", "b"), to = "y"), nodes),
    expected
  )
  expect_equal(
    d_labels(node_drelationship(dag, from = c("a", "b"), to = "y"), nodes),
    expected
  )

  # nodes outside `from` and `to` stay unlabeled
  expect_true(is.na(
    d_labels(node_dconnected(dag, from = c("a", "b"), to = "y"), "q")
  ))
})

test_that("node_d*() label each `to` node with its own d-relationship", {
  dag <- dagify(y ~ a, b ~ q)
  nodes <- c("a", "y", "q")
  expected <- c("d-connected", "d-connected", "d-separated")

  expect_equal(
    d_labels(node_dconnected(dag, from = "a", to = c("y", "q")), nodes),
    expected
  )
  expect_equal(
    d_labels(node_dseparated(dag, from = "a", to = c("y", "q")), nodes),
    expected
  )
  expect_equal(
    d_labels(node_drelationship(dag, from = "a", to = c("y", "q")), nodes),
    expected
  )
})

test_that("node_d*() label auto-filled exposures individually", {
  dag <- dagify(y ~ a, b ~ q, exposure = c("a", "b"), outcome = "y")
  nodes <- c("a", "b", "y")
  expected <- c("d-connected", "d-separated", "d-connected")

  expect_equal(d_labels(node_dconnected(dag), nodes), expected)
  expect_equal(d_labels(node_dseparated(dag), nodes), expected)
  expect_equal(d_labels(node_drelationship(dag), nodes), expected)
})

test_that("ggdag_dconnected() colors each endpoint by its own d-relationship", {
  withr::local_seed(1234)
  dag <- dagify(y ~ a, b ~ q, exposure = c("a", "b"), outcome = "y") |>
    tidy_dagitty()

  expect_doppelganger(
    "ggdag_dconnected() labels endpoints individually",
    ggdag_dconnected(dag, from = c("a", "b"), to = "y")
  )
  expect_doppelganger(
    "ggdag_dseparated() labels endpoints individually",
    ggdag_dseparated(dag, from = c("a", "b"), to = "y")
  )
})

test_that("node_d*() accept the documented `list(c(...))` controlling_for", {
  dag <- dagify(m ~ x + y, m2 ~ x + y)

  as_list <- node_dconnected(
    dag,
    "x",
    "y",
    controlling_for = list(c("m", "m2"))
  )
  as_character <- node_dconnected(dag, "x", "y", controlling_for = c("m", "m2"))
  expect_equal(pull_dag_data(as_list), pull_dag_data(as_character))
  expect_equal(d_labels(as_list, c("x", "y")), rep("d-connected", 2))

  as_list <- node_dseparated(
    dag,
    "x",
    "y",
    controlling_for = list(c("m", "m2"))
  )
  as_character <- node_dseparated(dag, "x", "y", controlling_for = c("m", "m2"))
  expect_equal(pull_dag_data(as_list), pull_dag_data(as_character))
  expect_equal(d_labels(as_list, c("x", "y")), rep("d-connected", 2))

  as_list <- node_drelationship(
    dag,
    "x",
    "y",
    controlling_for = list(c("m", "m2"))
  )
  as_character <- node_drelationship(
    dag,
    "x",
    "y",
    controlling_for = c("m", "m2")
  )
  expect_equal(pull_dag_data(as_list), pull_dag_data(as_character))
  expect_equal(d_labels(as_list, c("x", "y")), rep("d-connected", 2))
})

test_that("control_for() accepts the documented `list(c(...))` format", {
  dag <- dagify(m ~ x + y, m2 ~ x + y)

  expect_equal(
    pull_dag_data(control_for(dag, list(c("m", "m2")))),
    pull_dag_data(control_for(dag, c("m", "m2")))
  )
})

test_that("node_d*() raise a classed error for nodes not in the DAG", {
  dag <- dagify(m ~ x + y)

  expect_error(
    node_dconnected(dag, "x", "nope"),
    class = "ggdag_missing_nodes_error"
  )
  expect_error(
    node_dseparated(dag, "x", "nope"),
    class = "ggdag_missing_nodes_error"
  )
  expect_error(
    node_drelationship(dag, "x", "nope"),
    class = "ggdag_missing_nodes_error"
  )
  expect_error(
    node_dconnected(dag, "nope", "y"),
    class = "ggdag_missing_nodes_error"
  )
  expect_error(
    node_dseparated(dag, "nope", "y"),
    class = "ggdag_missing_nodes_error"
  )
  expect_error(
    node_drelationship(dag, "nope", "y"),
    class = "ggdag_missing_nodes_error"
  )
})

test_that("node_d*() forward `...` to tidy_dagitty()", {
  dag <- dagify(m ~ x + y)
  node_coords <- function(.result) {
    .df <- dplyr::distinct(
      dplyr::select(pull_dag_data(.result), "name", "x", "y")
    )
    dplyr::arrange(.df, .data$name)
  }

  expected <- node_coords(
    node_dconnected(tidy_dagitty(dag, layout = "circle"), "x", "y")
  )

  expect_equal(
    node_coords(node_dconnected(dag, "x", "y", layout = "circle")),
    expected
  )
  expect_equal(
    node_coords(node_dseparated(dag, "x", "y", layout = "circle")),
    expected
  )
  expect_equal(
    node_coords(node_drelationship(dag, "x", "y", layout = "circle")),
    expected
  )
})

test_that("the three node_d*() functions add the same columns", {
  dag <- dagify(m ~ x + y)

  expect_equal(
    added_columns(node_dconnected(dag, "x", "y"), dag),
    "d_relationship"
  )
  expect_equal(
    added_columns(node_dseparated(dag, "x", "y"), dag),
    "d_relationship"
  )
  expect_equal(
    added_columns(node_drelationship(dag, "x", "y"), dag),
    "d_relationship"
  )

  controlled <- c("collider_line", "adjusted", "d_relationship")
  expect_setequal(
    added_columns(node_dconnected(dag, "x", "y", controlling_for = "m"), dag),
    controlled
  )
  expect_setequal(
    added_columns(node_dseparated(dag, "x", "y", controlling_for = "m"), dag),
    controlled
  )
  expect_setequal(
    added_columns(
      node_drelationship(dag, "x", "y", controlling_for = "m"),
      dag
    ),
    controlled
  )
})

test_that("the `shape = adjusted` recipe builds on all three controlled outputs", {
  dag <- dagify(m ~ x + y)
  mapping <- aes_dag(shape = .data$adjusted, color = .data$d_relationship)

  expect_no_error(ggplot2::ggplot_build(
    ggplot2::ggplot(
      node_dconnected(dag, "x", "y", controlling_for = "m"),
      mapping
    ) +
      geom_dag()
  ))
  expect_no_error(ggplot2::ggplot_build(
    ggplot2::ggplot(
      node_dseparated(dag, "x", "y", controlling_for = "m"),
      mapping
    ) +
      geom_dag()
  ))
  expect_no_error(ggplot2::ggplot_build(
    ggplot2::ggplot(
      node_drelationship(dag, "x", "y", controlling_for = "m"),
      mapping
    ) +
      geom_dag()
  ))
})

test_that("node_d*() name the offending argument when a node is missing", {
  dag <- dagify(m ~ x + y)

  expect_ggdag_error(node_dconnected(dag, "x", "nope"))
  expect_ggdag_error(node_dseparated(dag, "nope", "y"))
  expect_ggdag_error(node_drelationship(
    dag,
    "x",
    "y",
    controlling_for = "nope"
  ))
})

test_that("node_d*() treat an empty controlling_for as no adjustment", {
  dag <- dagify(m ~ x + y)
  uncontrolled <- pull_dag_data(node_dconnected(dag, "x", "y"))

  expect_no_error(
    result <- node_dconnected(dag, "x", "y", controlling_for = list())
  )
  expect_equal(added_columns(result, dag), "d_relationship")
  expect_equal(pull_dag_data(result), uncontrolled)

  expect_equal(
    added_columns(
      node_dseparated(dag, "x", "y", controlling_for = character(0)),
      dag
    ),
    "d_relationship"
  )
})
