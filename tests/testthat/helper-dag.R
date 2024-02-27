expect_equal_dag <- function(object, expected, ...) {
  object_graph <- attr(object, "graph")
  attr(object, "graph") <- NULL
  expected_graph <- attr(expected, "graph")
  attr(expected, "graph") <- NULL
  expect_equal(object, expected, ...)
  expect_equal(
    tidygraph::as_tibble(object_graph, "nodes"),
    tidygraph::as_tibble(expected_graph, "nodes"),
    ...
  )
  expect_equal(
    tidygraph::as_tibble(object_graph, "edges"),
    tidygraph::as_tibble(expected_graph, "edges"),
    ...
  )
}
