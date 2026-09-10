test_that("ggdag() supports label_geom parameter", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder")
  )

  # Test with default (geom_dag_label_repel)
  p_default <- ggdag(dag, use_labels = TRUE)
  expect_s3_class(p_default, "gg")

  # Test with static labels
  p_static <- ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label)
  expect_s3_class(p_static, "gg")

  # Test with text repel
  p_text_repel <- ggdag(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_s3_class(p_text_repel, "gg")
})

test_that("adjustment set functions support label_geom parameter", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder")
  )

  # ggdag_adjustment_set
  p_adj <- ggdag_adjustment_set(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_s3_class(p_adj, "gg")

  # ggdag_adjust
  p_adjusted <- ggdag_adjust(
    dag,
    "z",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_s3_class(p_adjusted, "gg")
})

test_that("path functions support label_geom parameter", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder")
  )

  # ggdag_paths
  p_paths <- ggdag_paths(dag, use_labels = TRUE, label_geom = geom_dag_label)
  expect_s3_class(p_paths, "gg")

  # ggdag_paths_fan
  p_fan <- ggdag_paths_fan(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_s3_class(p_fan, "gg")
})

test_that("status function supports label_geom parameter", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder")
  )

  p <- ggdag_status(dag, use_labels = TRUE, label_geom = geom_dag_label_repel2)
  expect_s3_class(p, "gg")
})

test_that("relation functions support label_geom parameter", {
  dag <- dagify(
    y ~ x + z2 + w2 + w1,
    x ~ z1 + w1,
    z1 ~ w1 + v,
    z2 ~ w2 + v,
    w1 ~ ~w2,
    labels = c(
      x = "X",
      y = "Y",
      z1 = "Z1",
      z2 = "Z2",
      w1 = "W1",
      w2 = "W2",
      v = "V"
    )
  )

  # Test all 6 relation functions
  p_children <- ggdag_children(
    dag,
    "w1",
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_s3_class(p_children, "gg")

  p_parents <- ggdag_parents(
    dag,
    "y",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_s3_class(p_parents, "gg")

  p_ancestors <- ggdag_ancestors(
    dag,
    "x",
    use_labels = TRUE,
    label_geom = geom_dag_label_repel2
  )
  expect_s3_class(p_ancestors, "gg")

  p_descendants <- ggdag_descendants(
    dag,
    "w1",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel2
  )
  expect_s3_class(p_descendants, "gg")

  p_markov <- ggdag_markov_blanket(
    dag,
    "x",
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_s3_class(p_markov, "gg")

  p_adjacent <- ggdag_adjacent(
    dag,
    "x",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_s3_class(p_adjacent, "gg")
})

test_that("d-relationship functions support label_geom parameter", {
  dag <- dagify(
    m ~ x + y,
    labels = c(x = "X", y = "Y", m = "M")
  )

  p_drel <- ggdag_drelationship(
    dag,
    "x",
    "y",
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_s3_class(p_drel, "gg")

  p_dsep <- ggdag_dseparated(
    dag,
    "x",
    "y",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_s3_class(p_dsep, "gg")

  p_dconn <- ggdag_dconnected(
    dag,
    "x",
    "y",
    use_labels = TRUE,
    label_geom = geom_dag_label_repel2
  )
  expect_s3_class(p_dconn, "gg")
})

test_that("collider function supports label_geom parameter", {
  dag <- dagify(
    m ~ x + y,
    y ~ x,
    labels = c(x = "X", y = "Y", m = "Collider")
  )

  p <- ggdag_collider(dag, use_labels = TRUE, label_geom = geom_dag_text_repel2)
  expect_s3_class(p, "gg")
})

test_that("instrumental function supports label_geom parameter", {
  dag <- dagify(
    i ~ z,
    x ~ i,
    y ~ x,
    x ~ ~y,
    labels = c(i = "Instrument", x = "X", y = "Y", z = "Z")
  )

  p <- ggdag_instrumental(
    dag,
    "x",
    "y",
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_s3_class(p, "gg")
})

test_that("exogenous function supports label_geom parameter", {
  dag <- dagify(
    y ~ x1 + x2 + x3,
    b ~ x1 + x2,
    labels = c(y = "Y", x1 = "X1", x2 = "X2", x3 = "X3", b = "B")
  )

  p <- ggdag_exogenous(dag, use_labels = TRUE, label_geom = geom_dag_text_repel)
  expect_s3_class(p, "gg")
})

test_that("equivalence functions support label_geom parameter", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    labels = c(x = "X", y = "Y", z = "Z")
  )

  p_dags <- ggdag_equivalent_dags(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_s3_class(p_dags, "gg")

  p_class <- ggdag_equivalent_class(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_s3_class(p_class, "gg")
})

test_that("quick plot functions support label_geom parameter", {
  # Test m_bias
  p_mbias <- ggdag_m_bias(
    x = "X",
    y = "Y",
    m = "M",
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_s3_class(p_mbias, "gg")

  # Test butterfly_bias
  p_butterfly <- ggdag_butterfly_bias(
    x = "X",
    y = "Y",
    m = "M",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_s3_class(p_butterfly, "gg")

  # Test confounder_triangle
  p_conf <- ggdag_confounder_triangle(
    x = "X",
    y = "Y",
    z = "Z",
    use_labels = TRUE,
    label_geom = geom_dag_label_repel2
  )
  expect_s3_class(p_conf, "gg")

  # Test collider_triangle
  p_coll <- ggdag_collider_triangle(
    x = "X",
    y = "Y",
    m = "M",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel2
  )
  expect_s3_class(p_coll, "gg")

  # Test mediation_triangle
  p_med <- ggdag_mediation_triangle(
    x = "X",
    y = "Y",
    m = "M",
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_s3_class(p_med, "gg")

  # Test quartet functions
  p_q_coll <- ggdag_quartet_collider(
    x = "X",
    y = "Y",
    z = "Z",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_s3_class(p_q_coll, "gg")

  p_q_conf <- ggdag_quartet_confounder(
    x = "X",
    y = "Y",
    z = "Z",
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_s3_class(p_q_conf, "gg")

  p_q_med <- ggdag_quartet_mediator(
    x = "X",
    y = "Y",
    z = "Z",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_s3_class(p_q_med, "gg")

  p_q_mbias <- ggdag_quartet_m_bias(
    x = "X",
    y = "Y",
    z = "Z",
    use_labels = TRUE,
    label_geom = geom_dag_label_repel2
  )
  expect_s3_class(p_q_mbias, "gg")

  p_q_time <- ggdag_quartet_time_collider(
    x2 = "X2",
    y3 = "Y3",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel2
  )
  expect_s3_class(p_q_time, "gg")
})

test_that("canonical function supports label_geom parameter", {
  dag <- dagify(y ~ x + z, x ~ ~z)

  # Note: canonical creates new nodes without labels, so labels won't show
  # but the parameter should still be accepted
  p <- ggdag_canonical(dag, use_labels = TRUE, label_geom = geom_dag_label)
  expect_s3_class(p, "gg")
})

test_that("label_geom works with custom geom functions", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    labels = c(x = "X", y = "Y", z = "Z")
  )

  # Create a custom label geom function
  custom_label_geom <- function(mapping = NULL, ...) {
    geom_dag_label(mapping = mapping, fill = "yellow", ...)
  }

  p <- ggdag(dag, use_labels = TRUE, label_geom = custom_label_geom)
  expect_s3_class(p, "gg")
})

# label_wrap on the quick plots -----------------------------------------------
#
# `label_wrap` reaches a quick plot's label layer the way `use_labels` and
# `label_geom` do, whether it is written in the call or set as an option.

# The parameters of the automatic label layer of `plot`.
auto_label_params <- function(plot) {
  index <- which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$stat, "StatNodesLabelAuto")
  }))
  expect_length(index, 1)
  layer <- plot$layers[[index]]
  c(layer$stat_params, layer$geom_params)
}

test_that("a quick plot hands label_wrap to the auto label geom", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(
      x = "Physical activity",
      y = "Cardiovascular disease",
      z = "Socioeconomic status"
    )
  )

  plot <- ggdag_paths(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_label_auto,
    label_wrap = 6
  )

  expect_equal(auto_label_params(plot)[["wrap"]], 6)
})

test_that("the label_wrap option reaches a quick plot's auto label geom", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(
      x = "Physical activity",
      y = "Cardiovascular disease",
      z = "Socioeconomic status"
    )
  )

  withr::local_options(ggdag.label_wrap = 8)
  plot <- ggdag_adjustment_set(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )

  expect_equal(auto_label_params(plot)[["wrap"]], 8)
})

# Wrapping a label geom -------------------------------------------------------
#
# `geom_dag()` sets `size` and `col` on the label layer it assembles. A wrapper
# of the shape `function(...) geom_geom(..., size = value)` writes those names a
# second time, and the value the wrapper writes is the one the user asked for.

# The single label layer of `plot`, whichever label geom drew it.
dag_label_layer <- function(plot) {
  index <- which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$stat, "StatNodesLabelAuto") ||
      inherits(layer$stat, "StatNodesRepel")
  }))
  expect_length(index, 1)
  plot$layers[[index]]
}

# A small labelled DAG for the wrapper tests.
wrapper_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    labels = c(x = "Ex", y = "Why", z = "Zed")
  )
}

test_that("a size in a geom_dag_label_auto() wrapper reaches the label layer", {
  wrapper <- function(...) geom_dag_label_auto(..., size = 4.6)

  plot <- expect_no_warning(
    ggdag(wrapper_dag(), use_labels = TRUE, label_geom = wrapper)
  )

  expect_equal(dag_label_layer(plot)$aes_params$size, 4.6)
})

test_that("a size in a geom_dag_text_auto() wrapper reaches the label layer", {
  wrapper <- function(...) geom_dag_text_auto(..., size = 4.6)

  plot <- expect_no_warning(
    ggdag(wrapper_dag(), use_labels = TRUE, label_geom = wrapper)
  )

  expect_equal(dag_label_layer(plot)$aes_params$size, 4.6)
})

test_that("a size in a geom_dag_label_repel() wrapper reaches the label layer", {
  wrapper <- function(...) geom_dag_label_repel(..., size = 4.6)

  plot <- expect_no_warning(
    ggdag(wrapper_dag(), use_labels = TRUE, label_geom = wrapper)
  )

  expect_equal(dag_label_layer(plot)$aes_params$size, 4.6)
})

test_that("a size in a geom_dag_text_repel() wrapper reaches the label layer", {
  wrapper <- function(...) geom_dag_text_repel(..., size = 4.6)

  plot <- expect_no_warning(
    ggdag(wrapper_dag(), use_labels = TRUE, label_geom = wrapper)
  )

  expect_equal(dag_label_layer(plot)$aes_params$size, 4.6)
})

test_that("a col in a label geom wrapper reaches the label layer", {
  wrapper <- function(...) geom_dag_label_auto(..., col = "navy")

  plot <- expect_no_warning(
    ggdag(wrapper_dag(), use_labels = TRUE, label_geom = wrapper)
  )

  expect_equal(dag_label_layer(plot)$aes_params$colour, "navy")
})

test_that("a wrapper parameter geom_dag() does not set reaches the layer", {
  wrapper <- function(...) geom_dag_label_auto(..., fill = "lightyellow")

  plot <- expect_no_warning(
    ggdag(wrapper_dag(), use_labels = TRUE, label_geom = wrapper)
  )

  expect_equal(dag_label_layer(plot)$aes_params$fill, "lightyellow")
})
