expect_names <- function(object, expectation) {
  obj_names <- names(object)
  expect_true(all(expectation %in% obj_names))
}

test_that("dags ............", {
  withr::local_seed(1234)
  # non-reversible dag
  p1 <- ggdag_equivalent_dags(test_dag)
  p2 <- ggdag_equivalent_class(test_dag)
  expect_doppelganger("ggdag_equivalent_dags() plots no equivalent dags", p1)
  expect_doppelganger("ggdag_equivalent_class() plots no reversible edges", p2)

  # reversible dag
  g_ex <- dagify(y ~ x + z, x ~ z)
  p3 <- ggdag_equivalent_dags(g_ex)
  p4 <- ggdag_equivalent_class(g_ex)
  expect_doppelganger("ggdag_equivalent_dags() plots 6 equivalent dags", p3)
  expect_doppelganger("ggdag_equivalent_class() plots all reversible edges", p4)

  # equivalent dags work with labels and maintain other columns from original dag
  labelled_dag <- dagify(
    y ~ x,
    y ~ z,
    x ~ z,
    labels = c(
      "y" = "Outcome",
      "x" = "Exposure",
      "z" = "Confounder"
    ),
    exposure = "x",
    outcome = "y"
  ) |>
    tidy_dagitty()

  labelled_dag2 <- labelled_dag |>
    # also add node status
    node_status() |>
    node_equivalent_dags()

  expect_names(pull_dag_data(labelled_dag2), c("label", "status"))

  p5 <- ggdag_equivalent_dags(labelled_dag, use_labels = TRUE)
  expect_doppelganger("ggdag_equivalent_class() plots labels", p5)
})

test_that("ggdag_equivalent_class respects use_edges parameter (issue #167)", {
  # Create a DAG with reversible edges: y ~ x + z, x ~ z
  # This DAG has 3 edges total: y <- x, y <- z, x <- z
  # In equivalent class: y <- z is reversible, others are not
  dag <- dagify(y ~ x + z, x ~ z)

  # Test with use_edges = TRUE (should have 3 edges)
  p_with_edges <- ggdag_equivalent_class(dag, use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  expect_equal(
    analysis_with$total_edges,
    3,
    info = "Should have exactly 3 edges when use_edges = TRUE"
  )
  expect_gt(
    analysis_with$edge_layers,
    0,
    "Should have at least 1 edge layer when use_edges = TRUE"
  )

  # Test with use_edges = FALSE (should have NO edges)
  p_without_edges <- ggdag_equivalent_class(dag, use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without_edges)

  expect_false(
    analysis_without$has_edge_layers,
    paste(
      "Should have NO edge layers when use_edges = FALSE, but found:",
      analysis_without$edge_layers
    )
  )
  expect_equal(
    analysis_without$total_edges,
    0,
    info = paste(
      "Should have 0 edges when use_edges = FALSE, but found:",
      analysis_without$total_edges
    )
  )
  expect_equal(
    analysis_without$edge_layers,
    0,
    info = "Should have 0 edge layers when use_edges = FALSE"
  )
})

test_that("ggdag_equivalent_class handles complex DAG with bidirected edges", {
  # Complex DAG with bidirected edge from ?ggdag
  dag <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2, # bidirected edge
    exposure = "x",
    outcome = "y"
  )

  # Count edges in base DAG
  dag_edges <- dagitty::edges(dag)
  n_dag_edges <- nrow(dag_edges)
  expect_equal(n_dag_edges, 11, info = "Base DAG should have 11 edges")

  # Test with use_edges = TRUE
  p_with_edges <- ggdag_equivalent_class(dag, use_edges = TRUE)
  analysis_with <- analyze_plot_edges(p_with_edges)

  expect_true(
    analysis_with$has_edge_layers,
    "Should have edge layers when use_edges = TRUE"
  )
  expect_equal(
    analysis_with$total_edges,
    11,
    info = "Should have exactly 11 edges when use_edges = TRUE"
  )

  # Test with use_edges = FALSE
  p_without_edges <- ggdag_equivalent_class(dag, use_edges = FALSE)
  analysis_without <- analyze_plot_edges(p_without_edges)

  expect_false(
    analysis_without$has_edge_layers,
    "Should have NO edge layers when use_edges = FALSE"
  )
  expect_equal(
    analysis_without$total_edges,
    0,
    info = "Should have 0 edges when use_edges = FALSE"
  )
})

test_that("ggdag_equivalent_class() leaves mapped colour and fill legends intact", {
  dag <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")

  p <- ggdag_equivalent_class(dag) +
    ggplot2::aes(color = name, fill = name)
  built <- ggplot2::ggplot_build(p)

  expect_equal(
    as.character(built$plot$scales$get_scales("colour")$get_breaks()),
    c("x", "y", "z")
  )
  expect_equal(
    as.character(built$plot$scales$get_scales("fill")$get_breaks()),
    c("x", "y", "z")
  )

  p_arrow <- ggdag_equivalent_class(dag, edge_engine = "ggarrow") +
    ggplot2::aes(color = name)
  built_arrow <- ggplot2::ggplot_build(p_arrow)

  expect_equal(
    as.character(built_arrow$plot$scales$get_scales("colour")$get_breaks()),
    c("x", "y", "z")
  )

  expect_doppelganger(
    "ggdag_equivalent_class() with a colour aesthetic",
    p
  )
})

test_that("node_equivalent_dags() does not duplicate rows when extra columns exist", {
  plain <- dagify(y ~ x, y ~ z, x ~ z) |>
    tidy_dagitty() |>
    node_equivalent_dags()
  plain_data <- pull_dag_data(plain)

  labelled <- dagify(
    y ~ x,
    y ~ z,
    x ~ z,
    labels = c(
      "y" = "Outcome",
      "x" = "Exposure",
      "z" = "Confounder"
    ),
    exposure = "x",
    outcome = "y"
  ) |>
    tidy_dagitty() |>
    node_status() |>
    node_equivalent_dags()
  labelled_data <- pull_dag_data(labelled)

  expect_equal(nrow(labelled_data), nrow(plain_data))
  expect_equal(sum(duplicated(labelled_data)), 0)
  expect_equal(
    dplyr::count(labelled_data, dag = .data$dag)$n,
    dplyr::count(plain_data, dag = .data$dag)$n
  )
})

test_that("equivalence functions handle DAGs with no edges", {
  edgeless <- dagitty::dagitty("dag { x y }")

  ec <- node_equivalent_class(edgeless)
  expect_s3_class(ec, "tidy_dagitty")
  expect_false(any(pull_dag_data(ec)$reversable))

  expect_s3_class(ggdag_equivalent_class(edgeless), "ggplot")
})

test_that("equivalent DAG facets are ordered numerically", {
  # a complete 4-node DAG has 24 equivalent DAGs, enough to expose
  # lexicographic ordering of the `dag` identifier
  complete_dag <- dagify(d ~ a + b + c, c ~ a + b, b ~ a)

  dag_ids <- pull_dag_data(node_equivalent_dags(complete_dag))$dag
  expect_type(dag_ids, "integer")

  p <- ggdag_equivalent_dags(complete_dag)
  panels <- ggplot2::ggplot_build(p)$layout$layout
  expect_equal(
    panels$dag[order(panels$PANEL)],
    seq_len(nrow(panels))
  )
})

test_that("node_equivalent_class() does not confuse node names containing underscores", {
  # hash("a_b", "c") and hash("a", "b_c") collide when edge keys are
  # built by pasting sorted names with "_"
  dag <- dagify(b_c ~ a, c ~ a_b + d)

  edges <- pull_dag_data(node_equivalent_class(dag)) |>
    dplyr::filter(!is.na(.data$to))

  # a_b -> c and d -> c form the v-structure a_b -> c <- d, so both are compelled
  expect_false(edges$reversable[edges$name == "a_b" & edges$to == "c"])
  expect_false(edges$reversable[edges$name == "d" & edges$to == "c"])
  # only a -- b_c is reversible in the equivalence class
  expect_true(edges$reversable[edges$name == "a" & edges$to == "b_c"])
})

test_that("node_equivalent_class() is idempotent", {
  g_ex <- dagify(y ~ x + z, x ~ z)

  once <- node_equivalent_class(g_ex)
  twice <- node_equivalent_class(once)

  expect_equal(pull_dag_data(twice), pull_dag_data(once))
})

test_that("node_equivalent_dags() is idempotent", {
  withr::local_seed(1234)
  g_ex <- dagify(y ~ x + z, x ~ z)

  once <- node_equivalent_dags(g_ex)
  twice <- node_equivalent_dags(once)

  expect_false(any(c("dag.x", "dag.y") %in% names(pull_dag_data(twice))))
  expect_equal(pull_dag_data(twice), pull_dag_data(once))
})

test_that("ggdag_equivalent_dags() facets a DAG that already holds equivalent DAGs", {
  withr::local_seed(1234)
  once <- node_equivalent_dags(dagify(y ~ x + z, x ~ z))

  expect_no_warning(built <- ggplot2::ggplot_build(ggdag_equivalent_dags(once)))

  panels <- built$layout$layout
  expect_equal(nrow(panels), 6)
  expect_equal(panels$dag[order(panels$PANEL)], seq_len(nrow(panels)))
})

test_that("node_equivalent_class() does not mark bidirected edges reversable", {
  withr::local_seed(1234)
  # the equivalence class of this DAG holds a -- b and a <-> b, so only the
  # directed edge is reversable
  dag <- dagify(b ~ a, a ~ ~b)

  edges <- pull_dag_data(node_equivalent_class(dag)) |>
    dplyr::filter(!is.na(to))
  directed <- dplyr::filter(edges, direction == "->")
  bidirected <- dplyr::filter(edges, direction == "<->")

  expect_equal(nrow(directed), 1)
  expect_equal(nrow(bidirected), 1)
  expect_true(directed$reversable)
  expect_false(bidirected$reversable)
})

test_that("visual: ggdag_equivalent_class() with a directed and a bidirected edge", {
  withr::local_seed(1234)
  dag <- dagify(b ~ a, a ~ ~b)
  edges <- pull_dag_data(node_equivalent_class(dag)) |>
    dplyr::filter(!is.na(to))
  # never record a baseline while the bidirected edge is marked reversable
  skip_if_not(!any(edges$reversable[edges$direction == "<->"]))
  expect_doppelganger(
    "ggdag_equivalent_class() with a bidirected edge",
    ggdag_equivalent_class(dag)
  )
})

test_that("equivalence functions use the package-wide default layout", {
  expect_equal(
    formals(node_equivalent_dags)$layout,
    formals(tidy_dagitty)$layout
  )
  expect_equal(
    formals(node_equivalent_class)$layout,
    formals(tidy_dagitty)$layout
  )
})

test_that("ggdag_equivalent_dags() passes ... to tidy_dagitty()", {
  dag <- dagify(y ~ x + z, x ~ z)

  expected <- tidy_node_coords(tidy_dagitty(dag, layout = "circle"))
  actual <- node_coords(ggdag_equivalent_dags(dag, layout = "circle"))

  expect_setequal(actual$name, expected$name)
  # the equivalent DAGs carry their coordinates through dagitty, which stores
  # them to three decimal places
  expect_equal(
    dplyr::arrange(actual, name)$x,
    dplyr::arrange(expected, name)$x,
    tolerance = 1e-3
  )
  expect_equal(
    dplyr::arrange(actual, name)$y,
    dplyr::arrange(expected, name)$y,
    tolerance = 1e-3
  )
})

test_that("ggdag_equivalent_class() passes ... to tidy_dagitty()", {
  dag <- dagify(y ~ x + z, x ~ z)

  expected <- tidy_node_coords(tidy_dagitty(dag, layout = "circle"))
  actual <- node_coords(ggdag_equivalent_class(dag, layout = "circle"))

  expect_equal(actual$name, expected$name)
  expect_equal(actual$x, expected$x)
  expect_equal(actual$y, expected$y)
})

test_that("node_equivalent_class() forwards ... to tidy_dagitty()", {
  expect_true("..." %in% names(formals(node_equivalent_class)))

  dag <- dagify(y ~ x + z, x ~ z)
  coords <- tidy_node_coords(node_equivalent_class(dag, layout = "circle"))
  expected <- tidy_node_coords(tidy_dagitty(dag, layout = "circle"))

  expect_equal(coords$x, expected$x)
  expect_equal(coords$y, expected$y)
})

test_that("ggdag_equivalent_class() sizes the edge layers it builds itself", {
  # the equivalence class of this DAG leaves the two directed edges reversable
  # and the bidirected one fixed, so the plot draws a layer of each kind
  dag <- dagify(y ~ x + z, x ~ ~z)

  p <- ggdag_equivalent_class(
    dag,
    size = 2,
    edge_cap = 3,
    edge_width = 2,
    arrow_length = 20
  )

  expect_equal(edge_cap_radii(p), 6)
  expect_equal(edge_widths(p), 4)
  # the reversable edges are drawn without an arrowhead, so only the layers
  # that draw one report a length
  expect_equal(edge_arrow_lengths(p), 40)
})
