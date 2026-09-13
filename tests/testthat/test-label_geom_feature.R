# Identifying the label layer ---------------------------------------------------
#
# `geom_dag()` maps the DAG's `label` column onto the layer it builds for the
# labels. The node text layer maps `.data$name` instead, so the mapping names
# the label layer whichever geom drew it.

# The single label layer of `plot`.
dag_label_layer <- function(plot) {
  index <- which(purrr::map_lgl(plot$layers, \(layer) {
    label <- layer$mapping$label
    !is.null(label) && identical(rlang::quo_get_expr(label), quote(label))
  }))
  expect_length(index, 1)
  plot$layers[[index]]
}

# The label layer of `plot` was drawn by `geom_class`. Both label repel geoms
# draw with GeomLabelRepel, so `label_size` tells them apart:
# geom_dag_label_repel2() draws the label without a box border.
expect_label_geom <- function(plot, geom_class, label_size = NULL) {
  expect_s3_class(plot, "gg")
  layer <- dag_label_layer(plot)
  expect_equal(class(layer$geom)[[1]], geom_class)
  if (!is.null(label_size)) {
    expect_equal(layer$geom_params$label.size, label_size)
  }
  invisible(layer)
}

test_that("ggdag() supports label_geom parameter", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder")
  )

  # Test with default (geom_dag_label_repel)
  p_default <- ggdag(dag, use_labels = TRUE)
  expect_label_geom(p_default, "GeomLabelRepel", label_size = 0.25)

  # Test with static labels
  p_static <- ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label)
  expect_label_geom(p_static, "GeomLabel")

  # Test with text repel
  p_text_repel <- ggdag(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_label_geom(p_text_repel, "GeomTextRepel")
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
  expect_label_geom(p_adj, "GeomLabel")

  # ggdag_adjust
  p_adjusted <- ggdag_adjust(
    dag,
    "z",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_label_geom(p_adjusted, "GeomTextRepel")
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
  expect_label_geom(p_paths, "GeomLabel")

  # ggdag_paths_fan
  p_fan <- ggdag_paths_fan(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_label_geom(p_fan, "GeomTextRepel")
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
  expect_label_geom(p, "GeomLabelRepel", label_size = NA)
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
  expect_label_geom(p_children, "GeomLabel")

  p_parents <- ggdag_parents(
    dag,
    "y",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_label_geom(p_parents, "GeomTextRepel")

  p_ancestors <- ggdag_ancestors(
    dag,
    "x",
    use_labels = TRUE,
    label_geom = geom_dag_label_repel2
  )
  expect_label_geom(p_ancestors, "GeomLabelRepel", label_size = NA)

  p_descendants <- ggdag_descendants(
    dag,
    "w1",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel2
  )
  expect_label_geom(p_descendants, "GeomTextRepel")

  p_markov <- ggdag_markov_blanket(
    dag,
    "x",
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_label_geom(p_markov, "GeomLabel")

  p_adjacent <- ggdag_adjacent(
    dag,
    "x",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_label_geom(p_adjacent, "GeomTextRepel")
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
  expect_label_geom(p_drel, "GeomLabel")

  p_dsep <- ggdag_dseparated(
    dag,
    "x",
    "y",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_label_geom(p_dsep, "GeomTextRepel")

  p_dconn <- ggdag_dconnected(
    dag,
    "x",
    "y",
    use_labels = TRUE,
    label_geom = geom_dag_label_repel2
  )
  expect_label_geom(p_dconn, "GeomLabelRepel", label_size = NA)
})

test_that("collider function supports label_geom parameter", {
  dag <- dagify(
    m ~ x + y,
    y ~ x,
    labels = c(x = "X", y = "Y", m = "Collider")
  )

  p <- ggdag_collider(dag, use_labels = TRUE, label_geom = geom_dag_text_repel2)
  expect_label_geom(p, "GeomTextRepel")
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
  expect_label_geom(p, "GeomLabel")
})

test_that("exogenous function supports label_geom parameter", {
  dag <- dagify(
    y ~ x1 + x2 + x3,
    b ~ x1 + x2,
    labels = c(y = "Y", x1 = "X1", x2 = "X2", x3 = "X3", b = "B")
  )

  p <- ggdag_exogenous(dag, use_labels = TRUE, label_geom = geom_dag_text_repel)
  expect_label_geom(p, "GeomTextRepel")
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
  expect_label_geom(p_dags, "GeomLabel")

  p_class <- ggdag_equivalent_class(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_label_geom(p_class, "GeomTextRepel")
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
  expect_label_geom(p_mbias, "GeomLabel")

  # Test butterfly_bias
  p_butterfly <- ggdag_butterfly_bias(
    x = "X",
    y = "Y",
    m = "M",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_label_geom(p_butterfly, "GeomTextRepel")

  # Test confounder_triangle
  p_conf <- ggdag_confounder_triangle(
    x = "X",
    y = "Y",
    z = "Z",
    use_labels = TRUE,
    label_geom = geom_dag_label_repel2
  )
  expect_label_geom(p_conf, "GeomLabelRepel", label_size = NA)

  # Test collider_triangle
  p_coll <- ggdag_collider_triangle(
    x = "X",
    y = "Y",
    m = "M",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel2
  )
  expect_label_geom(p_coll, "GeomTextRepel")

  # Test mediation_triangle
  p_med <- ggdag_mediation_triangle(
    x = "X",
    y = "Y",
    m = "M",
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_label_geom(p_med, "GeomLabel")

  # Test quartet functions
  p_q_coll <- ggdag_quartet_collider(
    x = "X",
    y = "Y",
    z = "Z",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_label_geom(p_q_coll, "GeomTextRepel")

  p_q_conf <- ggdag_quartet_confounder(
    x = "X",
    y = "Y",
    z = "Z",
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_label_geom(p_q_conf, "GeomLabel")

  p_q_med <- ggdag_quartet_mediator(
    x = "X",
    y = "Y",
    z = "Z",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_label_geom(p_q_med, "GeomTextRepel")

  p_q_mbias <- ggdag_quartet_m_bias(
    x = "X",
    y = "Y",
    z = "Z",
    use_labels = TRUE,
    label_geom = geom_dag_label_repel2
  )
  expect_label_geom(p_q_mbias, "GeomLabelRepel", label_size = NA)

  p_q_time <- ggdag_quartet_time_collider(
    x2 = "X2",
    y3 = "Y3",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel2
  )
  expect_label_geom(p_q_time, "GeomTextRepel")
})

test_that("canonical function supports label_geom parameter", {
  dag <- dagify(y ~ x + z, x ~ ~z, labels = c(x = "X", y = "Y", z = "Z"))

  # canonical turns the bidirected edge into a new latent node, which carries
  # no label of its own; the labelled nodes still reach the label geom
  p <- ggdag_canonical(dag, use_labels = TRUE, label_geom = geom_dag_label)
  layer <- expect_label_geom(p, "GeomLabel")
  expect_setequal(stats::na.omit(layer$data$label), c("X", "Y", "Z"))
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
  layer <- expect_label_geom(p, "GeomLabel")
  expect_equal(layer$aes_params$fill, "yellow")
})

# label_wrap on the quick plots -----------------------------------------------
#
# `label_wrap` reaches a quick plot's label layer the way `use_labels` and
# `label_geom` do, whether it is written in the call or set as an option.

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

# `wrap` and `edge_cap` are the two parameters `geom_dag()` threads that a
# wrapper cannot be handed through its own dots, because they are named on
# the call `geom_dag()` makes rather than passed to the wrapper. A wrapper is
# the documented way to restyle labels, so it gets what a direct call gets.

# The first grob under `grob` whose name matches, gtable cells included: a
# gtable keeps its cells in `grobs` rather than in `children`, so
# `grid::getGrob()` does not reach them.
find_named_grob <- function(grob, pattern) {
  if (grepl(pattern, grob$name %||% "")) {
    return(grob)
  }
  for (child in c(grob$children, grob$grobs)) {
    found <- find_named_grob(child, pattern)
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}

# The parameters the automatic label engine is given when the plot is drawn.
drawn_label_params <- function(plot) {
  gtable <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  tree <- find_named_grob(gtable, "dag_labels_auto")
  expect_false(is.null(tree))
  tree$params
}

# A labelled DAG at a node size whose drawn edges stop well short of the
# node, so the cap the engine traces with is visible in the picture.
big_node_plot <- function(label_geom) {
  ggdag(
    dagify(
      y ~ m + x,
      m ~ x,
      labels = c(
        x = "Exposure node",
        m = "Mediator node",
        y = "Outcome node"
      ),
      coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
    ),
    use_labels = TRUE,
    use_text = FALSE,
    node_size = 30,
    edge_cap = 15,
    label_geom = label_geom
  )
}

test_that("a geom_dag_label_auto() wrapper keeps the plot's label_wrap", {
  wrapper <- function(...) geom_dag_label_auto(...)

  direct <- ggdag(
    wrapper_dag(),
    use_labels = TRUE,
    label_wrap = 6,
    label_geom = geom_dag_label_auto
  )
  wrapped <- ggdag(
    wrapper_dag(),
    use_labels = TRUE,
    label_wrap = 6,
    label_geom = wrapper
  )

  expect_equal(auto_label_params(wrapped)[["wrap"]], 6)
  expect_equal(
    auto_label_params(wrapped)[["wrap"]],
    auto_label_params(direct)[["wrap"]]
  )
  expect_equal(drawn_label_params(wrapped)$wrap, 6)
})

test_that("a geom_dag_text_auto() wrapper keeps the plot's label_wrap", {
  wrapper <- function(...) geom_dag_text_auto(...)

  plot <- ggdag(
    wrapper_dag(),
    use_labels = TRUE,
    label_wrap = 6,
    label_geom = wrapper
  )

  expect_equal(auto_label_params(plot)[["wrap"]], 6)
})

test_that("a geom_dag_label_auto() wrapper keeps the plot's edge_cap", {
  wrapper <- function(...) geom_dag_label_auto(...)

  direct <- big_node_plot(geom_dag_label_auto)
  wrapped <- big_node_plot(wrapper)

  expect_equal(auto_label_params(wrapped)[["edge_cap"]], 15)
  expect_equal(
    auto_label_params(wrapped)[["edge_cap"]],
    auto_label_params(direct)[["edge_cap"]]
  )
  # the cap decides where the traced edge ink ends, so it has to reach the
  # engine and not only the layer
  expect_equal(drawn_label_params(wrapped)$edge_cap, 15)
  expect_equal(
    drawn_label_params(wrapped)$edge_cap,
    drawn_label_params(direct)$edge_cap
  )
})

test_that("a wrapper's own wrap and edge_cap win over the plot's", {
  wrapper <- function(...) geom_dag_label_auto(..., wrap = 4, edge_cap = 3)

  plot <- ggdag(
    wrapper_dag(),
    use_labels = TRUE,
    label_wrap = 6,
    edge_cap = 15,
    label_geom = wrapper
  )

  expect_equal(auto_label_params(plot)[["wrap"]], 4)
  expect_equal(auto_label_params(plot)[["edge_cap"]], 3)
})

test_that("a wrapper around a repel label geom is unchanged", {
  # the repel geoms take neither parameter, and a wrapper of one must not be
  # handed either
  wrapper <- function(...) geom_dag_label_repel(...)

  plot <- expect_no_warning(
    ggdag(
      wrapper_dag(),
      use_labels = TRUE,
      label_wrap = 6,
      edge_cap = 15,
      label_geom = wrapper
    )
  )

  layer <- dag_label_layer(plot)
  expect_null(layer$geom_params$wrap)
  expect_null(layer$geom_params$edge_cap)
  expect_null(layer$stat_params$wrap)
  expect_null(layer$stat_params$edge_cap)
})
