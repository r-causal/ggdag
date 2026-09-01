# Return the first layer of `p` drawn by `geom_class`, or NULL if there is none.
find_layer <- function(p, geom_class) {
  is_match <- purrr::map_lgl(p$layers, \(layer) {
    inherits(layer$geom, geom_class)
  })

  if (!any(is_match)) {
    return(NULL)
  }

  p$layers[[which(is_match)[[1]]]]
}

test_that("dags canonicalize correctly", {
  withr::local_seed(1234)
  test_dag <- dagify(y ~ x + z, x ~ ~z)
  p <- ggdag_canonical(test_dag)
  expect_doppelganger("ggdag_canonical() expands ~~", p)
})

test_that("node_canonical() expands bidirected edges via latent variables", {
  dag <- dagify(y ~ x + z, x ~ ~z)
  canon <- node_canonical(dag)

  expect_s3_class(canon, "tidy_dagitty")

  canon_data <- pull_dag_data(canon)
  edges <- dplyr::filter(canon_data, !is.na(.data$to))
  expected_edges <- dagitty::edges(dagitty::canonicalize(dag)$g)

  expect_setequal(
    paste(edges$name, edges$to),
    paste(expected_edges$v, expected_edges$w)
  )
  expect_equal(dagitty::latents(pull_dag(canon)), "L1")
  expect_false(any(as.character(canon_data$direction) == "<->", na.rm = TRUE))
})

test_that("node_canonical() preserves exposures and outcomes", {
  dag <- dagify(y ~ x + z, x ~ ~z, exposure = "x", outcome = "y")
  canon <- node_canonical(dag)

  expect_equal(dagitty::exposures(pull_dag(canon)), "x")
  expect_equal(dagitty::outcomes(pull_dag(canon)), "y")
})

test_that("node_canonical() keeps the labels of the original DAG", {
  dag <- dagify(
    y ~ x + z,
    x ~ ~z,
    labels = c(y = "Outcome", x = "Exposure", z = "Confounder")
  )
  canon <- node_canonical(dag)

  expect_true(has_labels(canon))
  expect_equal(
    label(canon),
    c(y = "Outcome", x = "Exposure", z = "Confounder")
  )

  canon_data <- pull_dag_data(canon)
  expect_equal(
    canon_data$label[canon_data$name == "x"][[1]],
    "Exposure"
  )
  # the latent variable dagitty introduces has no label of its own
  expect_true(all(is.na(canon_data$label[canon_data$name == "L1"])))

  p <- ggdag_canonical(dag, use_labels = TRUE)
  expect_s3_class(p, "ggplot")
  expect_equal(
    rlang::as_label(find_layer(p, "GeomLabelRepel")$mapping$label),
    "label"
  )
  expect_doppelganger("ggdag_canonical() keeps labels", p)
})

test_that("ggdag_canonical() returns a ggplot", {
  dag <- dagify(y ~ x + z, x ~ ~z)
  expect_s3_class(ggdag_canonical(dag), "ggplot")
})

test_that("ggdag_canonical() defaults label_col to black", {
  dag <- dagify(y ~ x + z, x ~ ~z)

  expect_equal(
    formals(ggdag_canonical)$label_col,
    formals(ggdag_collider)$label_col
  )

  p <- ggdag_canonical(dag, use_labels = TRUE, label = name)
  label_layer <- find_layer(p, "GeomLabelRepel")
  expect_equal(label_layer$aes_params$colour, "black")

  expect_doppelganger("ggdag_canonical() draws black labels", p)
})

test_that("ggdag_canonical() accepts the standard quick plot arguments", {
  dag <- dagify(y ~ x + z, x ~ ~z)

  standard_args <- c(
    "size",
    "edge_width",
    "edge_cap",
    "arrow_length",
    "unified_legend",
    "key_glyph"
  )
  expect_true(all(standard_args %in% names(formals(ggdag_canonical))))

  p <- ggdag_canonical(dag, size = 2)
  node_layer <- find_layer(p, "GeomDagPoint")
  expect_equal(node_layer$aes_params$size, 32)
})

test_that("ggdag_canonical() forwards text, label, node, and stylized", {
  withr::local_options(lifecycle_verbosity = "quiet")
  dag <- dagify(y ~ x + z, x ~ ~z)

  p_text <- ggdag_canonical(dag, text = to)
  expect_equal(
    rlang::as_label(find_layer(p_text, "GeomDagText")$mapping$label),
    "to"
  )

  p_label <- ggdag_canonical(dag, use_labels = TRUE, label = to)
  expect_equal(
    rlang::as_label(find_layer(p_label, "GeomLabelRepel")$mapping$label),
    "to"
  )

  p_stylized <- ggdag_canonical(dag, stylized = TRUE)
  expect_false(is.null(find_layer(p_stylized, "GeomDagNode")))
  expect_null(find_layer(p_stylized, "GeomDagPoint"))

  p_node <- ggdag_canonical(dag, node = FALSE)
  expect_null(find_layer(p_node, "GeomDagPoint"))
})

test_that("ggdag_canonical() passes ... to the tidy_dagitty() that lays out the canonical DAG", {
  dag <- dagify(y ~ x + z, x ~ ~z)

  expected <- tidy_node_coords(node_canonical(dag, layout = "circle"))
  actual <- node_coords(ggdag_canonical(dag, layout = "circle"))

  expect_equal(actual$name, expected$name)
  expect_equal(actual$x, expected$x)
  expect_equal(actual$y, expected$y)
})

test_that("ggdag_canonical() carries the standard quick-plotter defaults", {
  # the sibling quick plotters fall back to `FALSE` when no label option is
  # set, and validate `edge_type` against the full set of choices
  expect_equal(
    formals(ggdag_canonical)$use_labels,
    formals(ggdag_status)$use_labels
  )
  expect_equal(
    formals(ggdag_canonical)$edge_type,
    formals(ggdag_status)$edge_type
  )
})
