test_that("colliders correctly identified", {
  withr::local_seed(1234)
  test_dag <- dagify(m ~ x + y, y ~ x)
  p <- ggdag_collider(test_dag)
  expect_doppelganger("ggdag_collider() highlights `m`", p)
})

test_that("colliders and downstream colliders are detected", {
  test_dag <- dagify(m ~ x + y, m_jr ~ m)
  expect_true(is_collider(test_dag, "m"))
  expect_true(is_downstream_collider(test_dag, "m_jr"))
  expect_false(is_collider(test_dag, "x"))
  expect_false(is_collider(test_dag, "y"))
  expect_false(is_downstream_collider(test_dag, "x"))
  expect_false(is_downstream_collider(test_dag, "y"))
})

# The unordered pairs joined by an activated collider line, sorted and
# deduplicated so assertions do not depend on row order or edge orientation.
collider_line_pairs <- function(.tdy_dag) {
  .df <- pull_dag_data(.tdy_dag)
  lines <- .df[which(.df$collider_line), , drop = FALSE]
  if (nrow(lines) == 0) {
    return(character(0))
  }

  sort(unique(paste(
    pmin(lines$name, lines$to),
    pmax(lines$name, lines$to),
    sep = "-"
  )))
}

test_that("many colliders activated are processed correctly", {
  dag <- dagify(
    m ~ a + b + d + e + f + g,
    x ~ a,
    y ~ b + x,
    exposure = "x",
    outcome = "y"
  )
  x <- activate_collider_paths(dag, adjust_for = c("m"))

  expect_true(is.tidy_dagitty(x))

  # every pair of m's parents is d-connected by adjusting for m
  expected_pairs <- sort(apply(
    utils::combn(c("a", "b", "d", "e", "f", "g"), 2),
    2,
    paste,
    collapse = "-"
  ))
  expect_equal(collider_line_pairs(x), expected_pairs)

  # one row per activated pair, and no original row is marked as one
  expect_equal(n_collider_paths(x), length(expected_pairs))
  expect_equal(
    sum(!pull_dag_data(x)$collider_line),
    nrow(pull_dag_data(tidy_dagitty(dag)))
  )
})

test_that("activating a non-collider adds no collider lines", {
  dag <- dagify(m ~ x + y, y ~ x)
  activated <- activate_collider_paths(dag, adjust_for = "x")

  expect_equal(collider_line_pairs(activated), character(0))
  expect_equal(n_collider_paths(activated), 0)
  expect_false(any(pull_dag_data(activated)$collider_line))
})

chain_dag <- function(n) {
  dagitty::dagitty(paste0(
    "dag{",
    paste(paste0("x", seq_len(n - 1), " -> x", seq(2, n)), collapse = "; "),
    "}"
  ))
}

# Two parallel chains cross-linked at every step, so every node past the first
# level is a collider, plus a single-parent tail that is only downstream of one
ladder_dag <- function(depth) {
  edges <- purrr::map(seq_len(depth - 1), \(i) {
    c(
      paste0("a", i, " -> a", i + 1),
      paste0("b", i, " -> a", i + 1),
      paste0("a", i, " -> b", i + 1),
      paste0("b", i, " -> b", i + 1)
    )
  })

  dagitty::dagitty(paste0(
    "dag{",
    paste(c(unlist(edges), paste0("a", depth, " -> tail")), collapse = "; "),
    "}"
  ))
}

test_that("colliders are detected several generations downstream", {
  dag <- dagify(m ~ x + y, m_jr ~ m, m_sr ~ m_jr)

  expect_true(is_downstream_collider(dag, "m_sr"))
  expect_true(is_collider(dag, "m_sr"))
  expect_false(is_collider(dag, "m_sr", downstream = FALSE))
  expect_true(is_collider(dag, "m", downstream = FALSE))
  expect_false(is_downstream_collider(dag, "m"))
})

test_that("collider detection is correct on a dense multi-parent DAG", {
  dag <- ladder_dag(5)

  expect_false(is_collider(dag, "a1"))
  expect_false(is_collider(dag, "b1"))
  expect_true(is_collider(dag, "a2", downstream = FALSE))
  expect_true(is_collider(dag, "a5"))
  expect_true(is_downstream_collider(dag, "tail"))
  expect_false(is_collider(dag, "tail", downstream = FALSE))
  expect_true(is_collider(dag, "tail"))
})

test_that("is_collider() stays linear on a deep chain", {
  dag <- chain_dag(14)

  elapsed <- system.time(result <- is_collider(dag, "x14"))[["elapsed"]]

  expect_false(result)
  expect_lt(elapsed, 5)
})

test_that("node_collider() stays linear on a deep chain", {
  dag <- chain_dag(12)

  elapsed <- system.time(result <- node_collider(dag))[["elapsed"]]

  expect_true(all(pull_dag_data(result)$colliders == "Non-Collider"))
  expect_lt(elapsed, 5)
})

test_that("is_collider() distinguishes direct from downstream colliders", {
  dag <- dagify(m ~ x + y, m_jr ~ m)

  expect_true(is_collider(dag, "m", downstream = FALSE))
  expect_false(is_collider(dag, "m_jr", downstream = FALSE))
  expect_true(is_collider(dag, "m_jr", downstream = TRUE))
})

test_that("bidirected edges count toward collider status", {
  # a -> m <-> b: two arrowheads point into m
  parent_spouse <- dagify(m ~ a, m ~ ~b)
  expect_true(is_collider(parent_spouse, "m", downstream = FALSE))
  expect_true(is_collider(parent_spouse, "m"))
  expect_false(is_collider(parent_spouse, "a"))
  expect_false(is_collider(parent_spouse, "b"))

  # a <-> m <-> b: two arrowheads, no directed parents at all
  two_spouses <- dagify(m ~ ~a, m ~ ~b)
  expect_true(is_collider(two_spouses, "m", downstream = FALSE))
  expect_true(is_collider(two_spouses, "m"))

  # a single bidirected edge is one arrowhead, so not a collider
  one_spouse <- dagify(m ~ ~a)
  expect_false(is_collider(one_spouse, "m"))
  expect_false(is_collider(one_spouse, "a"))
})

test_that("variables downstream of a bidirected collider are colliders", {
  dag <- dagify(m ~ a, m ~ ~b, m_jr ~ m)

  expect_true(is_downstream_collider(dag, "m_jr"))
  expect_true(is_collider(dag, "m_jr"))
  expect_false(is_collider(dag, "m_jr", downstream = FALSE))
})

test_that("node_collider() labels bidirected colliders", {
  dag <- dagify(m ~ a, m ~ ~b)
  .df <- pull_dag_data(node_collider(dag))
  labels <- stats::setNames(as.character(.df$colliders), .df$name)

  expect_equal(labels[["m"]], "Collider")
  expect_equal(labels[["a"]], "Non-Collider")
  expect_equal(labels[["b"]], "Non-Collider")
})

test_that("node_collider() returns a `colliders` column", {
  expect_true(
    "colliders" %in% names(pull_dag_data(node_collider(dagify(m ~ x + y))))
  )
})

test_that("activate_collider_paths() opens bidirected collider pairs", {
  parent_spouse <- dagify(m ~ a, m ~ ~b)
  activated <- activate_collider_paths(parent_spouse, adjust_for = "m")
  expect_equal(collider_line_pairs(activated), "a-b")
  expect_equal(n_collider_paths(activated), 1)

  two_spouses <- dagify(m ~ ~a, m ~ ~b)
  activated_spouses <- activate_collider_paths(two_spouses, adjust_for = "m")
  expect_equal(collider_line_pairs(activated_spouses), "a-b")
  expect_equal(n_collider_paths(activated_spouses), 1)

  # `b` is a collider on the a -> m <-> b <- c path, so adjusting for `m` opens
  # nothing between `a` and `c` even though `c` is upstream of `m`'s partner
  spouse_ancestor <- dagify(m ~ a, m ~ ~b, b ~ c)
  activated_ancestor <- activate_collider_paths(
    spouse_ancestor,
    adjust_for = "m"
  )
  expect_equal(collider_line_pairs(activated_ancestor), "a-b")
})

test_that("activate_collider_paths() draws each activated pair exactly once", {
  # the control_for() documentation example
  doc_control_for <- dagify(m ~ a + b, x ~ a, y ~ b)
  activated <- activate_collider_paths(doc_control_for, adjust_for = "m")
  expect_equal(collider_line_pairs(activated), "a-b")
  expect_equal(n_collider_paths(activated), 1)

  # the activate_collider_paths() documentation example
  doc_activate <- dagify(m ~ x + y, x ~ y)
  activated_doc <- activate_collider_paths(doc_activate, adjust_for = "m")
  expect_equal(collider_line_pairs(activated_doc), "x-y")
  expect_equal(n_collider_paths(activated_doc), 1)

  # a pair shared by two adjusted colliders is drawn once, not twice
  shared <- dagify(m1 ~ x + y, m2 ~ x + y)
  activated_shared <- activate_collider_paths(
    shared,
    adjust_for = c("m1", "m2")
  )
  expect_equal(collider_line_pairs(activated_shared), "x-y")
  expect_equal(n_collider_paths(activated_shared), 1)
})

test_that("activate_collider_paths() only pairs ancestors the adjustment opens", {
  # a reaches m only through b, so adjusting for m opens nothing between them
  chain <- dagify(b ~ a, m ~ b + y)
  activated_chain <- activate_collider_paths(chain, adjust_for = "m")
  expect_equal(collider_line_pairs(activated_chain), c("a-y", "b-y"))

  # m lies on every path from x or y to m_jr, so only x-y is opened
  downstream <- dagify(m ~ x + y, m_jr ~ m)
  activated_downstream <- activate_collider_paths(
    downstream,
    adjust_for = "m_jr"
  )
  expect_equal(collider_line_pairs(activated_downstream), "x-y")

  # u <- c -> v is open with or without the adjustment, so it is not activated,
  # but adjusting for m opens the collider at v between c and u
  confounded <- dagify(u ~ c, v ~ c + u, m ~ v + y)
  activated_confounded <- activate_collider_paths(confounded, adjust_for = "m")
  expect_equal(
    collider_line_pairs(activated_confounded),
    c("c-u", "c-y", "u-y", "v-y")
  )
})

test_that("node_collider() is idempotent", {
  dag <- dagify(m ~ x + y, y ~ x)

  once <- node_collider(dag)
  expect_equal(node_collider(once), once)

  once_lgl <- node_collider(dag, as_factor = FALSE)
  expect_equal(node_collider(once_lgl, as_factor = FALSE), once_lgl)
})

test_that("ggdag_collider() accepts pre-computed collider data", {
  dag <- dagify(m ~ x + y, y ~ x)

  expect_no_error(ggdag_collider(node_collider(dag)))
})

test_that("bidirected colliders are drawn", {
  withr::local_seed(1234)
  dag <- dagify(m ~ a, m ~ ~b)

  expect_doppelganger("ggdag_collider() highlights a bidirected `m`", {
    ggdag_collider(dag)
  })
  expect_doppelganger("ggdag_adjust() biases through a bidirected `m`", {
    ggdag_adjust(dag, var = "m")
  })
})

test_that("activated collider lines are drawn once per opened pair", {
  withr::local_seed(1234)

  expect_doppelganger("ggdag_adjust() draws one line per activated pair", {
    ggdag_adjust(dagify(m ~ a + b, x ~ a, y ~ b), var = "m")
  })
  expect_doppelganger("ggdag_adjust() omits unopened ancestor pairs", {
    ggdag_adjust(dagify(b ~ a, m ~ b + y), var = "m")
  })
})
