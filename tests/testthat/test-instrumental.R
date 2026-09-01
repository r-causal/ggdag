test_that("dags identify IVs correctly", {
  withr::local_seed(1234)
  dag <- dagitty::dagitty("dag{ i->x->y; i2->x->y; x<->y }")
  p <- ggdag_instrumental(dag, "x", "y")
  expect_doppelganger(
    "ggdag_instrumental() identifies `i` and `i2` as instrumental",
    p
  )

  # Add edge count test - instrumental plots are faceted by IV
  n_edges <- count_dag_edges(dag)
  # There are 2 IVs, so 2 panels
  expect_edge_count(p, n_edges * 2, "ggdag_instrumental with 2 IVs")
})

test_that("dags without IVs are shown correctly", {
  withr::local_seed(1234)
  no_iv <- dagify(
    y ~ t + x1 + x2 + x4,
    t ~ x1 + x3,
    x2 ~ x3 + x4,
    exposure = "t",
    outcome = "y",
    latent = c("x1", "x4")
  )

  p <- ggdag_instrumental(no_iv)
  expect_doppelganger(
    "ggdag_instrumental() identifies nothing as instrumental",
    p
  )

  # Add edge count test
  n_edges <- count_dag_edges(no_iv)
  expect_edge_count(p, n_edges, "ggdag_instrumental with no IVs")
})

test_that("dags with colliders + IVs are shown correctly", {
  withr::local_seed(1234)
  iv_collider <- dagify(
    y ~ t + x1 + x2 + x4,
    t ~ x1 + x3,
    x2 ~ x3 + x4,
    exposure = "t",
    outcome = "y",
    latent = c("x1")
  )

  p <- ggdag_instrumental(iv_collider)
  expect_doppelganger("ggdag_instrumental() instrumental plus collider", p)

  # Add edge count test
  n_edges <- count_dag_edges(iv_collider)
  expect_edge_count(p, n_edges, "ggdag_instrumental with collider")
})

# A DAG with one unconditional instrument (`iu`) and one instrument conditional
# on `w` (`z`), so the two kinds are present in the same result.
mixed_iv_dag <- function() {
  dagify(
    y ~ x + u + w,
    x ~ z + iu + u + w,
    z ~ w,
    exposure = "x",
    outcome = "y",
    latent = "u"
  )
}

test_that("node_instrumental() labels unconditional instruments as unadjusted", {
  dag <- mixed_iv_dag()
  .df <- pull_dag_data(node_instrumental(dag))

  expect_setequal(unique(.df$instrumental_name), c("iu", "z | w"))
  expect_true("adjusted" %in% names(.df))
  expect_false(anyNA(.df$adjusted))
  expect_setequal(
    as.character(unique(.df$adjusted)),
    c("adjusted", "unadjusted")
  )

  # the unconditional facet has no adjusted variables
  unconditional <- .df[.df$instrumental_name == "iu", ]
  expect_true(all(unconditional$adjusted == "unadjusted"))

  # the conditional facet still marks its conditioning variable
  conditional <- .df[.df$instrumental_name == "z | w", ]
  expect_true(all(conditional$adjusted[conditional$name == "w"] == "adjusted"))
})

test_that("ggdag_instrumental() draws every node when instrument kinds are mixed", {
  withr::local_seed(1234)
  dag <- mixed_iv_dag()
  p <- ggdag_instrumental(dag)

  built <- ggplot2::ggplot_build(p)
  point_data <- built$data[[1]]
  expect_gt(nrow(point_data), 0)
  expect_false(anyNA(point_data$shape))

  expect_doppelganger("ggdag_instrumental() mixed conditional instruments", p)
})

test_that("instrumental functions error without one exposure and one outcome", {
  no_endpoints <- dagify(y ~ x + i, x ~ i)

  expect_error(
    node_instrumental(no_endpoints),
    class = "ggdag_missing_error"
  )
  expect_error(
    ggdag_instrumental(no_endpoints),
    class = "ggdag_missing_error"
  )

  two_exposures <- dagify(
    y ~ x1 + x2,
    x1 ~ i,
    x2 ~ i,
    exposure = c("x1", "x2"),
    outcome = "y"
  )
  expect_error(
    node_instrumental(two_exposures),
    class = "ggdag_missing_error"
  )

  # endpoints supplied as arguments still work
  expect_no_error(node_instrumental(
    no_endpoints,
    exposure = "x",
    outcome = "y"
  ))
})

test_that("instrumental endpoint guards are informative", {
  no_endpoints <- dagify(y ~ x + i, x ~ i)
  two_exposures <- dagify(
    y ~ x1 + x2,
    x1 ~ i,
    x2 ~ i,
    exposure = c("x1", "x2"),
    outcome = "y"
  )

  expect_ggdag_error(node_instrumental(no_endpoints))
  expect_ggdag_error(node_instrumental(two_exposures))
  expect_ggdag_error(ggdag_instrumental(no_endpoints))
})

test_that("ggdag_instrumental() passes ... to tidy_dagitty()", {
  dag <- dagitty::dagitty("dag{ i->x->y; x<->y }")

  expected <- tidy_node_coords(tidy_dagitty(dag, layout = "circle"))
  actual <- node_coords(ggdag_instrumental(dag, "x", "y", layout = "circle"))

  expect_equal(actual$name, expected$name)
  expect_equal(actual$x, expected$x)
  expect_equal(actual$y, expected$y)
})
