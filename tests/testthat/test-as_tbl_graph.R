test_that("daggity and tidy dags become tidy tbls", {
  library(tidygraph, warn.conflicts = FALSE)
  dagitty_tbl_graph <- as_tbl_graph(butterfly_bias())
  tidy_dagitty_tbl_graph <- as_tbl_graph(butterfly_bias() |> tidy_dagitty())
  expect_s3_class(dagitty_tbl_graph, "tbl_graph")
  expect_s3_class(tidy_dagitty_tbl_graph, "tbl_graph")
})

test_that("as_tbl_graph keeps isolated nodes", {
  library(tidygraph, warn.conflicts = FALSE)
  dag <- dagitty::dagitty("dag { x -> y; z }")

  from_dagitty <- as_tbl_graph(dag)
  expect_setequal(
    as.data.frame(tidygraph::activate(from_dagitty, nodes))$name,
    c("x", "y", "z")
  )
  expect_equal(igraph::gsize(from_dagitty), 1)
  expect_true(
    "direction" %in%
      names(as.data.frame(tidygraph::activate(from_dagitty, edges)))
  )

  from_tidy <- as_tbl_graph(tidy_dagitty(dag, seed = 42))
  expect_setequal(
    as.data.frame(tidygraph::activate(from_tidy, nodes))$name,
    c("x", "y", "z")
  )
  expect_equal(igraph::gsize(from_tidy), 1)
})

test_that("as_tbl_graph handles DAGs with no edges", {
  library(tidygraph, warn.conflicts = FALSE)
  dag <- dagitty::dagitty("dag { x; y }")

  from_dagitty <- as_tbl_graph(dag)
  expect_setequal(
    as.data.frame(tidygraph::activate(from_dagitty, nodes))$name,
    c("x", "y")
  )
  expect_equal(igraph::gsize(from_dagitty), 0)

  from_tidy <- as_tbl_graph(tidy_dagitty(dag, seed = 42))
  expect_setequal(
    as.data.frame(tidygraph::activate(from_tidy, nodes))$name,
    c("x", "y")
  )
  expect_equal(igraph::gsize(from_tidy), 0)
})
