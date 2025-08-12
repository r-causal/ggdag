test_that("geom_dag_node creates correct layer", {
  dag <- dagify(y ~ x + z, x ~ z)
  tidy_dag <- tidy_dagitty(dag)

  # Basic usage
  layer <- geom_dag_node()
  expect_s3_class(layer, "LayerInstance")
  expect_s3_class(layer$geom, "GeomDagNode")
  expect_s3_class(layer$stat, "StatNodes")

  # With custom parameters
  layer_custom <- geom_dag_node(
    size = 10,
    color = "blue",
    fill = "red",
    alpha = 0.5,
    na.rm = TRUE,
    show.legend = FALSE
  )
  expect_true("size" %in% names(layer_custom$aes_params))
  expect_true("colour" %in% names(layer_custom$aes_params))
  expect_true("fill" %in% names(layer_custom$aes_params))
  expect_true("alpha" %in% names(layer_custom$aes_params))
  expect_true(layer_custom$geom_params$na.rm)
  expect_false(layer_custom$show.legend)

  # With mapping
  layer_mapped <- geom_dag_node(mapping = aes(color = name))
  expect_equal(rlang::quo_text(layer_mapped$mapping$colour), "name")
})

test_that("geom_dag_point creates correct layer", {
  # Basic usage
  layer <- geom_dag_point()
  expect_s3_class(layer, "LayerInstance")
  expect_s3_class(layer$geom, "GeomDagPoint")
  expect_s3_class(layer$stat, "StatNodes")

  # With parameters
  layer_custom <- geom_dag_point(size = 5, shape = 17)
  expect_true("size" %in% names(layer_custom$aes_params))
  expect_true("shape" %in% names(layer_custom$aes_params))
})

test_that("geom_dag_text creates correct layer", {
  # Basic usage
  layer <- geom_dag_text()
  expect_s3_class(layer, "LayerInstance")
  expect_s3_class(layer$stat, "StatNodes")

  # With custom parameters
  layer_custom <- geom_dag_text(
    col = "red",
    size = 5
  )
  expect_true("colour" %in% names(layer_custom$aes_params))
  expect_true("size" %in% names(layer_custom$aes_params))
})

test_that("geom_dag_label creates correct layer", {
  # Basic usage
  layer <- geom_dag_label()
  expect_s3_class(layer, "LayerInstance")
  expect_s3_class(layer$stat, "StatNodes")

  # With custom parameters
  layer_custom <- geom_dag_label(
    col = "white",
    fill = "black",
    size = 4,
    alpha = 0.8,
    label.r = grid::unit(0.5, "lines"),
    label.padding = grid::unit(0.5, "lines")
  )
  expect_true("colour" %in% names(layer_custom$aes_params))
  expect_true("fill" %in% names(layer_custom$aes_params))
  expect_true("size" %in% names(layer_custom$aes_params))
  expect_true("alpha" %in% names(layer_custom$aes_params))
  # label.r and label.padding are passed through params
  expect_true(
    "label.r" %in%
      names(layer_custom$params) ||
      "label.r" %in% names(layer_custom$geom_params)
  )
  expect_true(
    "label.padding" %in%
      names(layer_custom$params) ||
      "label.padding" %in% names(layer_custom$geom_params)
  )
})

test_that("edge type switch works correctly", {
  expect_equal(edge_type_switch("link"), geom_dag_edges_link)
  expect_equal(edge_type_switch("arc"), geom_dag_edges_arc)
  expect_equal(edge_type_switch("diagonal"), geom_dag_edges_diagonal)
  expect_equal(edge_type_switch("link_arc"), geom_dag_edges)

  # Invalid type returns NULL (switch default behavior)
  expect_null(edge_type_switch("invalid"))
})

test_that("geom_dag_edges creates correct layers", {
  # Basic usage - returns a list of two layers
  layers <- geom_dag_edges()
  expect_type(layers, "list")
  expect_length(layers, 2)
  expect_s3_class(layers[[1]], "LayerInstance")
  expect_s3_class(layers[[2]], "LayerInstance")
})

test_that("geom_dag_edges_arc creates correct layer", {
  layer <- geom_dag_edges_arc(
    curvature = 0.5,
    arrow = grid::arrow(length = grid::unit(10, "pt"))
  )
  expect_s3_class(layer, "LayerInstance")
  expect_equal(class(layer$geom)[1], "GeomDAGEdgePath")
  expect_equal(class(layer$stat)[1], "StatEdgeArc")
  # Check that parameters are set in the correct place
  expect_true("arrow" %in% names(layer$geom_params))
  expect_true("strength" %in% names(layer$stat_params))
  expect_equal(layer$stat_params$strength, 0.5)
})

test_that("geom_dag_edges_diagonal creates correct layer", {
  layer <- geom_dag_edges_diagonal()
  expect_s3_class(layer, "LayerInstance")
  expect_equal(class(layer$geom)[1], "GeomDAGEdgePath")
  expect_equal(class(layer$stat)[1], "StatEdgeDiagonal")

  # With parameters
  layer_custom <- geom_dag_edges_diagonal(
    curvature = 0.5,
    arrow = grid::arrow(type = "open")
  )
  # Parameters are split between geom_params and stat_params
  expect_true("arrow" %in% names(layer_custom$geom_params))
  expect_true("strength" %in% names(layer_custom$stat_params))
  expect_equal(layer_custom$stat_params$strength, 0.5)
})

test_that("geom_dag_edges_fan creates correct layer", {
  layer <- geom_dag_edges_fan()
  expect_s3_class(layer, "LayerInstance")
  expect_equal(class(layer$geom)[1], "GeomDAGEdgePath")
  expect_equal(class(layer$stat)[1], "StatEdgeFan")

  # With spread parameter - passed as strength internally
  layer_spread <- geom_dag_edges_fan(spread = 10)
  expect_true("strength" %in% names(layer_spread$stat_params))
  expect_equal(layer_spread$stat_params$strength, 10)
})

test_that("geom_dag_collider_edges creates correct layer", {
  dag <- dagify(m ~ x + y, y ~ x) |>
    tidy_dagitty() |>
    control_for("m")

  layer <- geom_dag_collider_edges()
  expect_s3_class(layer, "LayerInstance")
  # geom_dag_collider_edges uses the standard stat
  expect_equal(class(layer$stat)[1], "StatIdentity")
  expect_equal(class(layer$geom)[1], "GeomCurve")

  # With custom parameters
  layer_custom <- geom_dag_collider_edges(
    linewidth = 2,
    curvature = 0.2,
    arrow = NULL
  )
  # Check params exist in either params or geom_params
  all_params <- c(layer_custom$params, layer_custom$geom_params)
  expect_true("curvature" %in% names(all_params))
  expect_equal(all_params$curvature, 0.2)
  expect_null(all_params$arrow)
  # linewidth is in aes_params, not params
  expect_true("linewidth" %in% names(layer_custom$aes_params))
  expect_equal(layer_custom$aes_params$linewidth, 2)

  # Test deprecated size parameter
  expect_ggdag_warning(
    layer_size <- geom_dag_collider_edges(size = 2)
  )
})

test_that("filter_direction works correctly", {
  # filter_direction returns a function
  filter_func <- filter_direction("->")
  expect_type(filter_func, "closure")

  # Create a data frame with edges
  edges_data <- data.frame(
    direction = factor(
      c("->", "<->", "--", "->"),
      levels = c("->", "<->", "--")
    )
  )

  # Apply the filter
  filtered <- filter_func(edges_data)
  expect_equal(nrow(filtered), 2)
  expect_true(all(filtered$direction == "->"))

  # Test with collider_line column
  edges_with_collider <- edges_data
  edges_with_collider$collider_line <- c(FALSE, TRUE, FALSE, TRUE)
  filtered_collider <- filter_func(edges_with_collider)
  expect_equal(nrow(filtered_collider), 1)
})

test_that("geom_dag main function works with different options", {
  # Basic usage - all defaults
  geoms <- geom_dag()
  expect_length(geoms, 4) # edges, nodes, text, labels
  expect_type(geoms[[1]], "list") # edges returns a list
  expect_s3_class(geoms[[2]], "LayerInstance") # nodes
  expect_s3_class(geoms[[3]], "LayerInstance") # text
  expect_null(geoms[[4]]) # labels off by default

  # Without edges
  geoms_no_edges <- geom_dag(use_edges = FALSE)
  expect_null(geoms_no_edges[[1]])

  # Without nodes
  geoms_no_nodes <- geom_dag(use_nodes = FALSE)
  expect_null(geoms_no_nodes[[2]])

  # Without text
  geoms_no_text <- geom_dag(use_text = FALSE)
  expect_null(geoms_no_text[[3]])

  # With labels
  geoms_labels <- geom_dag(use_labels = TRUE)
  expect_s3_class(geoms_labels[[4]], "LayerInstance")

  # With stylized nodes
  geoms_stylized <- geom_dag(use_stylized = TRUE)
  expect_equal(class(geoms_stylized[[2]]$geom)[1], "GeomDagNode")

  # With different edge types
  geoms_arc <- geom_dag(edge_type = "arc")
  expect_s3_class(geoms_arc[[1]], "LayerInstance")

  geoms_diagonal <- geom_dag(edge_type = "diagonal")
  expect_s3_class(geoms_diagonal[[1]], "LayerInstance")

  # Test size parameter scaling
  geoms_sizes <- geom_dag(
    size = 2,
    node_size = 20,
    text_size = 5,
    label_size = 4,
    edge_width = 1.5,
    edge_cap = 8,
    arrow_length = 10
  )
  # size = 2 should scale node_size to 40 (20 * 2)
  expect_equal(geoms_sizes[[2]]$aes_params$size, 40)
  # text_size is passed directly without size multiplier
  expect_equal(geoms_sizes[[3]]$aes_params$size, 5)

  # Test deprecated parameters
  expect_ggdag_warning(
    geom_dag(node = "deprecated")
  )

  expect_ggdag_warning(
    geom_dag(stylized = "deprecated")
  )

  # Test deprecated text parameter with logical
  expect_ggdag_warning(
    geoms_text_logical <- geom_dag(text = FALSE)
  )
  expect_null(geoms_text_logical[[3]])

  # Test deprecated use_labels with character
  expect_ggdag_warning(
    geoms_labels_char <- geom_dag(use_labels = "label")
  )
  expect_s3_class(geoms_labels_char[[4]], "LayerInstance")
})

test_that("bidirected edge handling works", {
  # geom_dag doesn't have only_directed parameter
  # bidirected edges are handled by geom_dag_edges which returns two layers
  dag <- dagify(
    y ~ x,
    x ~ ~y
  )

  edges <- geom_dag_edges()
  expect_length(edges, 2)
  # First layer is for directed edges, second for bidirected
  expect_s3_class(edges[[1]], "LayerInstance")
  expect_s3_class(edges[[2]], "LayerInstance")
})

test_that("text and label enquo handling works", {
  # Custom text variable
  geoms_custom_text <- geom_dag(text = label)
  text_mapping <- geoms_custom_text[[3]]$mapping
  expect_equal(rlang::quo_text(text_mapping$label), "label")

  # Custom label variable
  geoms_custom_label <- geom_dag(use_labels = TRUE, label = custom_label)
  label_mapping <- geoms_custom_label[[4]]$mapping
  expect_equal(rlang::quo_text(label_mapping$label), "custom_label")
})

test_that("ggname creates properly named grobs", {
  grob <- ggname("test_prefix", grid::textGrob("test"))
  # ggname adds a suffix to make names unique
  expect_match(grob$name, "^test_prefix\\.text")

  # With custom grob
  custom_grob <- grid::circleGrob()
  named_grob <- ggname("circle_prefix", custom_grob)
  expect_match(named_grob$name, "^circle_prefix\\.circle")
})

test_that("is_quo_logical identifies logical quosures", {
  expect_true(is_quo_logical(rlang::quo(TRUE)))
  expect_true(is_quo_logical(rlang::quo(FALSE)))
  expect_false(is_quo_logical(rlang::quo(x)))
  expect_false(is_quo_logical(rlang::quo("TRUE")))
  expect_false(is_quo_logical(rlang::quo(1)))
})
