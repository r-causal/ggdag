test_that("node_children identifies children correctly", {
  # Create test DAG
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w
  )

  # Test basic functionality
  result <- node_children(dag, "w")
  result_df <- pull_dag_data(result)

  # w should be parent
  w_row <- result_df[result_df$name == "w", ]
  expect_equal(as.character(w_row$children[1]), "parent")

  # x and z should be children
  x_row <- result_df[result_df$name == "x", ]
  z_row <- result_df[result_df$name == "z", ]
  expect_equal(as.character(x_row$children[1]), "child")
  expect_equal(as.character(z_row$children[1]), "child")

  # y should be NA (not direct child)
  y_row <- result_df[result_df$name == "y", ]
  expect_true(is.na(y_row$children[1]))

  # Test as_factor parameter
  result_char <- node_children(dag, "w", as_factor = FALSE)
  result_char_df <- pull_dag_data(result_char)
  expect_type(result_char_df$children, "character")

  # Test with tidy_dagitty input
  tidy_dag <- tidy_dagitty(dag)
  result_tidy <- node_children(tidy_dag, "x")
  expect_s3_class(result_tidy, "tidy_dagitty")

  # x has no children
  result_df2 <- pull_dag_data(result_tidy)
  x_row2 <- result_df2[result_df2$name == "x", ]
  expect_equal(as.character(x_row2$children[1]), "parent")
})

test_that("node_parents identifies parents correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w
  )

  # Test basic functionality
  result <- node_parents(dag, "y")
  result_df <- pull_dag_data(result)

  # y should be child
  y_row <- result_df[result_df$name == "y", ]
  expect_equal(as.character(y_row$parent[1]), "child")

  # x and z should be parents
  x_row <- result_df[result_df$name == "x", ]
  z_row <- result_df[result_df$name == "z", ]
  expect_equal(as.character(x_row$parent[1]), "parent")
  expect_equal(as.character(z_row$parent[1]), "parent")

  # w should be NA (not direct parent)
  w_row <- result_df[result_df$name == "w", ]
  expect_true(is.na(w_row$parent[1]))

  # Test as_factor = FALSE
  result_char <- node_parents(dag, "y", as_factor = FALSE)
  result_char_df <- pull_dag_data(result_char)
  expect_type(result_char_df$parent, "character")
})

test_that("node_ancestors identifies ancestors correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ w + v,
    z ~ w,
    w ~ u
  )

  # Test ancestors of y
  result <- node_ancestors(dag, "y")
  result_df <- pull_dag_data(result)

  # y should be descendant
  y_row <- result_df[result_df$name == "y", ]
  expect_equal(as.character(y_row$ancestor[1]), "descendant")

  # x, z, w, v, u should be ancestors
  ancestors <- c("x", "z", "w", "v", "u")
  for (anc in ancestors) {
    anc_row <- result_df[result_df$name == anc, ]
    expect_equal(as.character(anc_row$ancestor[1]), "ancestor")
  }

  # Test node with no ancestors
  result_u <- node_ancestors(dag, "u")
  result_u_df <- pull_dag_data(result_u)
  u_row <- result_u_df[result_u_df$name == "u", ]
  expect_equal(as.character(u_row$ancestor[1]), "descendant")

  # All others should be NA
  other_rows <- result_u_df[result_u_df$name != "u", ]
  expect_true(all(is.na(other_rows$ancestor)))
})

test_that("node_descendants identifies descendants correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w,
    a ~ y
  )

  # Test descendants of w
  result <- node_descendants(dag, "w")
  result_df <- pull_dag_data(result)

  # w should be ancestor
  w_row <- result_df[result_df$name == "w", ]
  expect_equal(as.character(w_row$descendant[1]), "ancestor")

  # x, z, y, a should be descendants
  descendants <- c("x", "z", "y", "a")
  for (desc in descendants) {
    desc_row <- result_df[result_df$name == desc, ]
    expect_equal(as.character(desc_row$descendant[1]), "descendant")
  }

  # Test node with no descendants
  result_a <- node_descendants(dag, "a")
  result_a_df <- pull_dag_data(result_a)
  a_row <- result_a_df[result_a_df$name == "a", ]
  expect_equal(as.character(a_row$descendant[1]), "ancestor")
})

test_that("node_markov_blanket identifies Markov blanket correctly", {
  # Create DAG with clear Markov blanket
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w,
    a ~ x,
    b ~ y
  )

  # Test Markov blanket of x
  result <- node_markov_blanket(dag, "x")
  result_df <- pull_dag_data(result)

  # x should be center variable
  x_row <- result_df[result_df$name == "x", ]
  expect_equal(as.character(x_row$blanket[1]), "center variable")

  # Markov blanket of x includes: w (parent), y and a (children), z (co-parent of y)
  blanket_nodes <- c("w", "y", "a", "z")
  for (node in blanket_nodes) {
    node_row <- result_df[result_df$name == node, ]
    expect_equal(as.character(node_row$blanket[1]), "Markov blanket")
  }

  # b should not be in blanket
  b_row <- result_df[result_df$name == "b", ]
  expect_true(is.na(b_row$blanket[1]))

  # Test as_factor = FALSE
  result_char <- node_markov_blanket(dag, "x", as_factor = FALSE)
  result_char_df <- pull_dag_data(result_char)
  expect_type(result_char_df$blanket, "character")
})

test_that("node_adjacent identifies adjacent nodes correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w,
    a ~ b
  )

  # Test adjacent nodes of x
  result <- node_adjacent(dag, "x")
  result_df <- pull_dag_data(result)

  # x should be center variable
  x_row <- result_df[result_df$name == "x", ]
  expect_equal(as.character(x_row$adjacent[1]), "center variable")

  # w and y should be adjacent (directly connected)
  adjacent_nodes <- c("w", "y")
  for (node in adjacent_nodes) {
    node_row <- result_df[result_df$name == node, ]
    expect_equal(as.character(node_row$adjacent[1]), "adjacent")
  }

  # z should not be adjacent (not directly connected)
  z_row <- result_df[result_df$name == "z", ]
  expect_true(is.na(z_row$adjacent[1]))

  # a and b should not be adjacent
  a_row <- result_df[result_df$name == "a", ]
  b_row <- result_df[result_df$name == "b", ]
  expect_true(is.na(a_row$adjacent[1]))
  expect_true(is.na(b_row$adjacent[1]))
})

test_that("ggdag_children creates correct plots", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w
  )

  # Basic plot
  p1 <- ggdag_children(dag, "w")
  expect_s3_class(p1, "gg")
  expect_true("children" %in% names(p1$data))

  # Check color aesthetic is mapped
  expect_true("colour" %in% names(p1$mapping))

  # Test with parameters
  p2 <- ggdag_children(
    dag,
    "w",
    size = 2,
    node_size = 20,
    text_size = 5,
    edge_type = "arc",
    use_stylized = TRUE
  )
  expect_s3_class(p2, "gg")

  # Test with use_nodes = FALSE
  p3 <- ggdag_children(dag, "w", use_nodes = FALSE)
  has_point_layer <- any(purrr::map_lgl(p3$layers, \(l) {
    inherits(l$geom, "GeomDagPoint")
  }))
  expect_false(has_point_layer)
})

test_that("ggdag_parents creates correct plots", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w
  )

  # Basic plot
  p1 <- ggdag_parents(dag, "y")
  expect_s3_class(p1, "gg")
  expect_true("parent" %in% names(p1$data))

  # Check that scale includes both parent and child
  color_scale <- purrr::keep(p1$scales$scales, \(s) "colour" %in% s$aesthetics)
  expect_length(color_scale, 1)
  expect_equal(color_scale[[1]]$breaks, c("parent", "child"))
})

test_that("ggdag_ancestors creates correct plots", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w
  )

  # Basic plot
  p1 <- ggdag_ancestors(dag, "y")
  expect_s3_class(p1, "gg")
  expect_true("ancestor" %in% names(p1$data))

  # Test edge parameters
  p2 <- ggdag_ancestors(
    dag,
    "y",
    edge_width = 1.2,
    arrow_length = 10,
    edge_cap = 15
  )
  expect_s3_class(p2, "gg")
})

test_that("ggdag_descendants creates correct plots", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w
  )

  # Basic plot
  p1 <- ggdag_descendants(dag, "w")
  expect_s3_class(p1, "gg")
  expect_true("descendant" %in% names(p1$data))

  # Test text parameters
  p2 <- ggdag_descendants(
    dag,
    "w",
    text_col = "red",
    label_col = "blue",
    use_labels = TRUE
  )
  expect_s3_class(p2, "gg")
})

test_that("ggdag_markov_blanket creates correct plots", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w
  )

  # Basic plot
  p1 <- ggdag_markov_blanket(dag, "x")
  expect_s3_class(p1, "gg")
  expect_true("blanket" %in% names(p1$data))

  # Test with use_text = FALSE
  p2 <- ggdag_markov_blanket(dag, "x", use_text = FALSE)
  # The function should create a valid plot
  expect_s3_class(p2, "gg")
})

test_that("ggdag_adjacent creates correct plots", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w
  )

  # Basic plot
  p1 <- ggdag_adjacent(dag, "x")
  expect_s3_class(p1, "gg")
  expect_true("adjacent" %in% names(p1$data))

  # Test all parameters together
  p2 <- ggdag_adjacent(
    dag,
    "x",
    size = 1.5,
    edge_type = "diagonal",
    node_size = 18,
    text_size = 4,
    label_size = 4,
    text_col = "black",
    label_col = "white",
    edge_width = 0.8,
    edge_cap = 10,
    arrow_length = 6,
    use_edges = TRUE,
    use_nodes = TRUE,
    use_stylized = FALSE,
    use_text = TRUE,
    use_labels = FALSE
  )
  expect_s3_class(p2, "gg")
})

test_that("relations functions handle edge cases", {
  # Single node DAG
  expect_ggdag_error(dagify(x ~ x))

  # Disconnected DAG
  disconnected_dag <- dagify(
    y ~ x,
    b ~ a
  )

  result <- node_children(disconnected_dag, "a")
  result_df <- pull_dag_data(result)
  # a has child b
  b_row <- result_df[result_df$name == "b", ]
  expect_equal(as.character(b_row$children[1]), "child")
  # x and y are unrelated
  x_row <- result_df[result_df$name == "x", ]
  y_row <- result_df[result_df$name == "y", ]
  expect_true(is.na(x_row$children[1]))
  expect_true(is.na(y_row$children[1]))

  # Variable not in DAG
  dag <- dagify(y ~ x)
  expect_ggdag_error(node_children(dag, "z"))
})

test_that("complex DAG relationships work correctly", {
  # Create complex DAG
  complex_dag <- dagify(
    d ~ c + b,
    c ~ a,
    b ~ a,
    e ~ d,
    f ~ e,
    g ~ f,
    h ~ g + a
  )

  # Test ancestors of h (should include all except h)
  result <- node_ancestors(complex_dag, "h")
  result_df <- pull_dag_data(result)
  ancestors <- result_df[result_df$ancestor == "ancestor", ]$name
  expect_setequal(ancestors, c("a", "b", "c", "d", "e", "f", "g"))

  # Test descendants of a (should include all except a)
  result2 <- node_descendants(complex_dag, "a")
  result_df2 <- pull_dag_data(result2)
  descendants <- result_df2[result_df2$descendant == "descendant", ]$name
  expect_setequal(descendants, c("b", "c", "d", "e", "f", "g", "h"))

  # Test Markov blanket of d
  result3 <- node_markov_blanket(complex_dag, "d")
  result_df3 <- pull_dag_data(result3)
  blanket <- result_df3[result_df3$blanket == "Markov blanket", ]$name
  # Parents: b, c; Children: e
  expect_true(all(c("b", "c", "e") %in% blanket))
})

test_that("bidirected edges handled in adjacent nodes", {
  # DAG with bidirected edge
  dag <- dagify(
    y ~ x,
    x ~ ~z
  )

  # z should be adjacent to x (bidirected edge)
  result <- node_adjacent(dag, "x")
  result_df <- pull_dag_data(result)
  z_row <- result_df[result_df$name == "z", ]
  expect_equal(as.character(z_row$adjacent[1]), "adjacent")

  # y should also be adjacent (directed edge)
  y_row <- result_df[result_df$name == "y", ]
  expect_equal(as.character(y_row$adjacent[1]), "adjacent")
})

test_that("deprecated parameters show warnings", {
  dag <- dagify(y ~ x)

  expect_warning(
    ggdag_children(dag, "x", node = "deprecated"),
    "deprecated"
  )

  expect_warning(
    ggdag_parents(dag, "x", stylized = "deprecated"),
    "deprecated"
  )
})
