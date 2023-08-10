test_that("daggity and tidy dags become tidy tbls", {
  library(tidygraph, warn.conflicts = FALSE)
  dagitty_tbl_graph <- as_tbl_graph(butterfly_bias())
  tidy_dagitty_tbl_graph <- as_tbl_graph(butterfly_bias() %>% tidy_dagitty())
  expect_s3_class(dagitty_tbl_graph, "tbl_graph")
  expect_s3_class(tidy_dagitty_tbl_graph, "tbl_graph")
})
