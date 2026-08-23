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

test_that("many colliders activated are processed correctly", {
  x <- dagify(
    m ~ a + b + d + e + f + g,
    x ~ a,
    y ~ b + x,
    exposure = "x",
    outcome = "y"
  ) |>
    activate_collider_paths(adjust_for = c("m"))

  expect_true(is.tidy_dagitty(x))
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
