test_that("dags cannonicalize correctly", {
  # Get edge count for all tests
  n_edges <- count_dag_edges(test_dag)

  p1 <- ggdag_children(test_dag, "w1")
  expect_doppelganger(
    "ggdag_children() identifies `y`, `x`, and `z1`",
    p1
  )
  expect_edge_count(p1, n_edges, "ggdag_children")

  p2 <- ggdag_parents(test_dag, "y")
  expect_doppelganger(
    "ggdag_parents() identifies `z2`, `x`, `w1`, and `w2`",
    p2
  )
  expect_edge_count(p2, n_edges, "ggdag_parents")

  p3 <- ggdag_ancestors(test_dag, "x")
  expect_doppelganger(
    "ggdag_ancestors() identifies `v`, `w1`, and `z1`",
    p3
  )
  expect_edge_count(p3, n_edges, "ggdag_ancestors")

  p4 <- ggdag_descendants(test_dag, "w1")
  expect_doppelganger(
    "ggdag_descendants() identifies `y`, `x`, and `z1`",
    p4
  )
  expect_edge_count(p4, n_edges, "ggdag_descendants")
})

# Node-relation labels for a vector `.var`. `case_when()` tests the relation
# before the queried set, so a queried variable that is also a relative of
# another queried variable keeps the relation label.
relation_labels <- function(.result, column, nodes) {
  .df <- dplyr::distinct(
    dplyr::select(pull_dag_data(.result), "name", dplyr::all_of(column))
  )
  as.character(.df[[column]][match(nodes, .df$name)])
}

relation_dag <- function() {
  dagify(y ~ x + z2 + w2 + w1, x ~ z1 + w1, z1 ~ w1 + v, z2 ~ w2 + v)
}

relation_nodes <- c("v", "w1", "w2", "x", "y", "z1", "z2")

test_that("node_parents() labels every queried variable with a vector `.var`", {
  dag <- relation_dag()

  expect_no_warning(result <- node_parents(dag, c("x", "y")))
  expect_equal(
    relation_labels(result, "parent", relation_nodes),
    c(NA, "parent", "parent", "parent", "child", "parent", "parent")
  )
})

test_that("node_children() labels every queried variable with a vector `.var`", {
  dag <- relation_dag()

  expect_no_warning(result <- node_children(dag, c("w1", "v")))
  expect_equal(
    relation_labels(result, "children", relation_nodes),
    c("parent", "parent", NA, "child", "child", "child", "child")
  )
})

test_that("node_ancestors() drops every queried variable from the set", {
  dag <- relation_dag()

  expect_no_warning(result <- node_ancestors(dag, c("x", "y")))
  expect_equal(
    relation_labels(result, "ancestor", relation_nodes),
    c(
      "ancestor",
      "ancestor",
      "ancestor",
      "descendant",
      "descendant",
      "ancestor",
      "ancestor"
    )
  )
})

test_that("node_descendants() drops every queried variable from the set", {
  dag <- relation_dag()

  expect_no_warning(result <- node_descendants(dag, c("w1", "v")))
  expect_equal(
    relation_labels(result, "descendant", relation_nodes),
    c(
      "ancestor",
      "ancestor",
      NA,
      "descendant",
      "descendant",
      "descendant",
      "descendant"
    )
  )
})

test_that("node_markov_blanket() labels every queried variable", {
  dag <- relation_dag()

  expect_no_warning(result <- node_markov_blanket(dag, c("x", "y")))
  expect_equal(
    relation_labels(result, "blanket", relation_nodes),
    c(
      NA,
      "Markov blanket",
      "Markov blanket",
      "center variable",
      "center variable",
      "Markov blanket",
      "Markov blanket"
    )
  )
})

test_that("node_adjacent() labels every queried variable", {
  dag <- relation_dag()

  expect_no_warning(result <- node_adjacent(dag, c("x", "y")))
  expect_equal(
    relation_labels(result, "adjacent", relation_nodes),
    c(
      NA,
      "adjacent",
      "adjacent",
      "adjacent",
      "adjacent",
      "adjacent",
      "adjacent"
    )
  )
})

test_that("node_*() relation labels are unchanged for a scalar `.var`", {
  dag <- relation_dag()

  expect_equal(
    relation_labels(node_parents(dag, "y"), "parent", relation_nodes),
    c(NA, "parent", "parent", "parent", "child", NA, "parent")
  )
  expect_equal(
    relation_labels(node_ancestors(dag, "x"), "ancestor", relation_nodes),
    c("ancestor", "ancestor", NA, "descendant", NA, "ancestor", NA)
  )
})

test_that("ggdag_*() relation plots accept a vector `.var`", {
  withr::local_seed(1234)
  dag <- relation_dag()

  expect_doppelganger(
    "ggdag_parents() with two variables",
    ggdag_parents(dag, c("x", "y"))
  )
  expect_doppelganger(
    "ggdag_ancestors() with two variables",
    ggdag_ancestors(dag, c("x", "y"))
  )
})
