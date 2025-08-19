test_that("query_adjustment_sets works correctly", {
  # Simple confounding
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  result <- query_adjustment_sets(dag)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("set_id", "type", "effect", "set", "variables"))
  expect_equal(nrow(result), 1)
  expect_equal(result$type, "minimal")
  expect_equal(result$effect, "total")
  expect_equal(result$set, "{z}")
  expect_equal(result$variables[[1]], "z")

  # Multiple adjustment sets
  dag2 <- dagify(
    y ~ x + a + b + c,
    x ~ a + b,
    a ~ d,
    b ~ d,
    exposure = "x",
    outcome = "y"
  )

  result2 <- query_adjustment_sets(dag2, type = "all")
  expect_gt(nrow(result2), 1)
  expect_true(all(result2$type == "all"))

  # No adjustment needed
  dag3 <- dagify(
    y ~ x,
    exposure = "x",
    outcome = "y"
  )

  result3 <- query_adjustment_sets(dag3)
  expect_equal(nrow(result3), 1)
  expect_equal(result3$set, "{}")
  expect_equal(length(result3$variables[[1]]), 0)

  # Test with explicit exposure/outcome
  result4 <- query_adjustment_sets(dag, exposure = "z", outcome = "y")
  expect_equal(nrow(result4), 1)
})

test_that("query_adjustment_sets handles errors correctly", {
  dag <- dagify(y ~ x)

  # No exposure or outcome
  expect_error(query_adjustment_sets(dag), "No exposure variable")

  dag2 <- dagify(y ~ x, exposure = "x")
  expect_error(query_adjustment_sets(dag2), "No outcome variable")
})

test_that("query_paths works correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w,
    exposure = "x",
    outcome = "y"
  )

  result <- query_paths(dag)
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("path_id", "from", "to", "path", "path_type", "variables", "open")
  )
  expect_gt(nrow(result), 0)
  expect_true(all(result$from == "x"))
  expect_true(all(result$to == "y"))
  # Check variables are extracted correctly
  path1_vars <- result$variables[[1]]
  expect_true(all(c("x", "y") %in% path1_vars))

  # Test with conditioning
  result2 <- query_paths(dag, conditioned_on = "z")
  expect_true(any(result2$open != result$open))

  # Test directed paths
  result3 <- query_paths(dag, directed = TRUE)
  expect_lte(nrow(result3), nrow(result))

  # No paths
  dag2 <- dagify(
    y ~ z,
    x ~ w
  )
  result4 <- query_paths(dag2, from = "x", to = "y")
  expect_equal(nrow(result4), 0)
})

test_that("query_paths correctly classifies path types", {
  # Simple DAG with direct and backdoor paths
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  result <- query_paths(dag)
  expect_true("path_type" %in% names(result))

  # Check that we have both types of paths
  expect_true("direct" %in% result$path_type)
  expect_true("backdoor" %in% result$path_type)

  # Direct path should be x -> y
  direct_paths <- result[result$path_type == "direct", ]
  expect_equal(nrow(direct_paths), 1)
  expect_true(grepl("x -> y", direct_paths$path))

  # Backdoor path should be x <- z -> y
  backdoor_paths <- result[result$path_type == "backdoor", ]
  expect_equal(nrow(backdoor_paths), 1)
  expect_true(grepl("z", backdoor_paths$path))

  # DAG with only direct paths
  dag2 <- dagify(
    y ~ x,
    exposure = "x",
    outcome = "y"
  )

  result2 <- query_paths(dag2)
  expect_true(all(result2$path_type == "direct"))

  # DAG with only backdoor paths
  dag3 <- dagify(
    y ~ z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  result3 <- query_paths(dag3)
  expect_true(all(result3$path_type == "backdoor"))

  # When directed = TRUE, all paths should be direct
  result4 <- query_paths(dag, directed = TRUE)
  expect_true(all(result4$path_type == "direct"))
})

test_that("query_instrumental works correctly", {
  # Classic IV setup
  dag <- dagify(
    y ~ x + u,
    x ~ z + u,
    exposure = "x",
    outcome = "y",
    latent = "u"
  )

  result <- query_instrumental(dag)
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("instrument", "exposure", "outcome", "conditioning_set", "conditioned_on")
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$instrument, "z")
  expect_equal(result$conditioning_set, "{}")

  # No instrumental variables
  dag2 <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  )

  result2 <- query_instrumental(dag2)
  expect_equal(nrow(result2), 0)
})

test_that("query_dseparated and query_dconnected work correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w
  )

  # Test d-separation
  result <- query_dseparated(dag, from = "x", to = "z")
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "from_set",
      "from",
      "to_set",
      "to",
      "conditioning_set",
      "conditioned_on",
      "dseparated"
    )
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$from_set, "{x}")
  expect_equal(result$to_set, "{z}")
  expect_equal(result$conditioning_set, "{}")
  expect_false(result$dseparated) # x and z are d-connected through w

  # Test with conditioning
  result2 <- query_dseparated(dag, from = "x", to = "z", conditioned_on = "w")
  expect_equal(result2$conditioning_set, "{w}")
  expect_true(result2$dseparated) # conditioning on w d-separates x and z

  # Test d-connection
  result3 <- query_dconnected(dag, from = "x", to = "z")
  expect_named(
    result3,
    c(
      "from_set",
      "from",
      "to_set",
      "to",
      "conditioning_set",
      "conditioned_on",
      "dconnected"
    )
  )
  expect_true(result3$dconnected)
  expect_equal(result3$dconnected, !result$dseparated)

  # Test with multiple variables
  result4 <- query_dseparated(dag, from = c("x", "w"), to = "y")
  expect_equal(result4$from_set, "{w, x}")
  expect_equal(length(result4$from[[1]]), 2)
})

test_that("query_dseparated validates inputs", {
  dag <- dagify(y ~ x)

  expect_error(
    query_dseparated(dag, from = 1, to = "y"),
    "must be a character vector"
  )
  expect_error(
    query_dseparated(dag, from = "x", to = 1),
    "must be a character vector"
  )
})

test_that("query_colliders works correctly", {
  # Simple collider
  dag <- dagify(
    z ~ x + y,
    w ~ z
  )

  result <- query_colliders(dag)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("node", "parent_set", "parents", "is_activated"))
  expect_equal(nrow(result), 1)
  expect_equal(result$node, "z")
  expect_equal(result$parent_set, "{x, y}")
  expect_setequal(result$parents[[1]], c("x", "y"))
  expect_false(result$is_activated)

  # No colliders
  dag2 <- dagify(
    y ~ x,
    z ~ x
  )

  result2 <- query_colliders(dag2)
  expect_equal(nrow(result2), 0)

  # Multiple colliders
  dag3 <- dagify(
    z ~ x + y,
    w ~ a + b
  )

  result3 <- query_colliders(dag3)
  expect_equal(nrow(result3), 2)
  expect_setequal(result3$node, c("z", "w"))
})

test_that("query_exogenous works correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ w
  )

  result <- query_exogenous(dag)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("node", "n_descendants"))
  expect_setequal(result$node, c("w", "z"))

  # Check descendant counts
  w_row <- result[result$node == "w", ]
  expect_equal(w_row$n_descendants, 2) # x and y

  z_row <- result[result$node == "z", ]
  expect_equal(z_row$n_descendants, 1) # y

  # No exogenous variables (cyclic would be invalid, so this is theoretical)
  dag2 <- dagify(y ~ x)
  result2 <- query_exogenous(dag2)
  expect_equal(result2$node, "x")
})

test_that("query_parents works correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ w
  )

  # Query all nodes
  result <- query_parents(dag)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("node", "parent_set", "parents", "n_parents"))
  expect_equal(nrow(result), 4)

  # Check specific nodes
  y_row <- result[result$node == "y", ]
  expect_equal(y_row$parent_set, "{x, z}")
  expect_setequal(y_row$parents[[1]], c("x", "z"))
  expect_equal(y_row$n_parents, 2)

  w_row <- result[result$node == "w", ]
  expect_true(is.na(w_row$parent_set))
  expect_true(is.na(w_row$parents[[1]][1]))
  expect_equal(w_row$n_parents, 0)

  # Query specific variable
  result2 <- query_parents(dag, .var = "y")
  expect_equal(nrow(result2), 1)
  expect_equal(result2$node, "y")
})

test_that("query_children works correctly", {
  dag <- dagify(
    y ~ x + z,
    w ~ x
  )

  result <- query_children(dag)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("node", "child_set", "children", "n_children"))

  x_row <- result[result$node == "x", ]
  expect_equal(x_row$child_set, "{w, y}")
  expect_setequal(x_row$children[[1]], c("y", "w"))
  expect_equal(x_row$n_children, 2)

  y_row <- result[result$node == "y", ]
  expect_true(is.na(y_row$child_set))
  expect_true(is.na(y_row$children[[1]][1]))
  expect_equal(y_row$n_children, 0)
})

test_that("query_ancestors works correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w
  )

  result <- query_ancestors(dag)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("node", "ancestor_set", "ancestors", "n_ancestors"))

  y_row <- result[result$node == "y", ]
  expect_equal(y_row$ancestor_set, "{w, x, z}")
  expect_setequal(y_row$ancestors[[1]], c("x", "z", "w"))
  expect_equal(y_row$n_ancestors, 3)

  w_row <- result[result$node == "w", ]
  expect_true(is.na(w_row$ancestor_set))
  expect_true(is.na(w_row$ancestors[[1]][1]))
  expect_equal(w_row$n_ancestors, 0)
})

test_that("query_descendants works correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w
  )

  result <- query_descendants(dag)
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("node", "descendant_set", "descendants", "n_descendants")
  )

  w_row <- result[result$node == "w", ]
  expect_equal(w_row$descendant_set, "{x, y, z}")
  expect_setequal(w_row$descendants[[1]], c("x", "z", "y"))
  expect_equal(w_row$n_descendants, 3)

  y_row <- result[result$node == "y", ]
  expect_true(is.na(y_row$descendant_set))
  expect_true(is.na(y_row$descendants[[1]][1]))
  expect_equal(y_row$n_descendants, 0)
})

test_that("query_markov_blanket works correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ w,
    z ~ w,
    a ~ x
  )

  result <- query_markov_blanket(dag)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("node", "blanket", "blanket_vars", "blanket_size"))

  # Check x's Markov blanket
  x_row <- result[result$node == "x", ]
  # Should include: w (parent), y and a (children), z (co-parent of y)
  expect_equal(x_row$blanket, "{a, w, y, z}")
  expect_setequal(x_row$blanket_vars[[1]], c("w", "y", "a", "z"))
  expect_equal(x_row$blanket_size, 4)

  # Query specific variable
  result2 <- query_markov_blanket(dag, .var = "x")
  expect_equal(nrow(result2), 1)
  expect_equal(result2$node, "x")
})

test_that("helper functions work correctly", {
  # Test .empty_set_as_list
  empty <- .empty_set_as_list()
  expect_equal(empty, list(NA_character_))

  # Test .dagitty_set_to_tibble
  set_list <- list(c("a", "b"), c("c"))
  result <- .dagitty_set_to_tibble(set_list)
  expect_s3_class(result, "tbl_df")
  expect_named(result, "variables")
  expect_equal(nrow(result), 2)

  # Empty set
  result2 <- .dagitty_set_to_tibble(list())
  expect_equal(nrow(result2), 0)

  # Custom name
  result3 <- .dagitty_set_to_tibble(set_list, "custom_name")
  expect_named(result3, "custom_name")
})

test_that("all query functions validate input", {
  # Not a DAG
  not_dag <- list(x = 1)

  expect_error(query_adjustment_sets(not_dag))
  expect_error(query_paths(not_dag))
  expect_error(query_instrumental(not_dag))
  expect_error(query_dseparated(not_dag, "x", "y"))
  expect_error(query_colliders(not_dag))
  expect_error(query_exogenous(not_dag))
  expect_error(query_parents(not_dag))
  expect_error(query_children(not_dag))
  expect_error(query_ancestors(not_dag))
  expect_error(query_descendants(not_dag))
  expect_error(query_markov_blanket(not_dag))
  expect_error(query_status(not_dag))
})

test_that("query_status works correctly", {
  # DAG with all status types
  dag <- dagify(
    l ~ x + y,
    y ~ x,
    exposure = "x",
    outcome = "y",
    latent = "l"
  )

  result <- query_status(dag)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("name", "status"))
  expect_equal(nrow(result), 3)

  # Check specific statuses
  expect_equal(result$status[result$name == "x"], "exposure")
  expect_equal(result$status[result$name == "y"], "outcome")
  expect_equal(result$status[result$name == "l"], "latent")

  # Query specific variables
  result2 <- query_status(dag, .var = c("x", "l"))
  expect_equal(nrow(result2), 2)
  expect_setequal(result2$name, c("x", "l"))

  # DAG with no special status
  dag2 <- dagify(
    y ~ x + z,
    x ~ w
  )

  result3 <- query_status(dag2)
  expect_true(all(is.na(result3$status)))

  # Mixed status
  dag3 <- dagify(
    y ~ x + z,
    x ~ w,
    exposure = "x"
  )

  result4 <- query_status(dag3)
  expect_equal(sum(!is.na(result4$status)), 1)
  expect_equal(result4$status[result4$name == "x"], "exposure")
})
