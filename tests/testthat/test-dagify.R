test_that("dagify creates correct dagitty", {
  test_dag <- dagify(y ~ x + z, x ~ z)
  expect_equal(test_dag[[1]], "dag {\nx\ny\nz\nx -> y\nz -> x\nz -> y\n}\n")
  expect_s3_class(test_dag, "dagitty")
})

test_that("dagify rejects self-loops with helpful error", {
  expect_ggdag_error(
    dagify(x ~ x)
  )

  expect_ggdag_error(
    dagify(y ~ x + y)
  )

  expect_ggdag_error(
    dagify(y ~ x, x ~ z, z ~ z)
  )
})

test_that("dagify validates exposure and outcome constraints", {
  expect_ggdag_error(
    dagify(y ~ x, exposure = "x", outcome = "x")
  )

  expect_ggdag_error(
    dagify(y ~ x, exposure = c("x", "y"), outcome = c("y", "z"))
  )
})

test_that("dagify validates latent variable constraints", {
  expect_ggdag_error(
    dagify(y ~ x + u, x ~ u, exposure = "u", latent = "u")
  )

  expect_ggdag_error(
    dagify(y ~ x + u, x ~ u, outcome = "u", latent = "u")
  )
})

test_that("dagify validates variables exist in DAG", {
  expect_ggdag_error(
    dagify(y ~ x, exposure = "z")
  )

  expect_ggdag_error(
    dagify(y ~ x, outcome = "z")
  )

  expect_ggdag_error(
    dagify(y ~ x, latent = "z")
  )

  expect_ggdag_error(
    dagify(y ~ x, exposure = c("x", "z", "w"))
  )
})

test_that("dagify accepts valid DAG specifications", {
  expect_silent(
    dag1 <- dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")
  )
  expect_s3_class(dag1, "dagitty")

  expect_silent(
    dag2 <- dagify(
      y ~ x + u,
      x ~ u,
      latent = "u",
      exposure = "x",
      outcome = "y"
    )
  )
  expect_s3_class(dag2, "dagitty")

  expect_silent(
    dag3 <- dagify(y ~ x + z, x ~ ~z, exposure = "x", outcome = "y")
  )
  expect_s3_class(dag3, "dagitty")
})

# -- curved() formula syntax ---------------------------------------------------

test_that("curved() errors when called directly", {
  expect_ggdag_error(curved("x"))
  expect_ggdag_error(curved("x", 0.5))
})

test_that("extract_curved_edges() finds curved() in formula RHS", {
  result <- extract_curved_edges(list(y ~ z + curved(c)))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "c")
  expect_equal(result$to, "y")
  expect_equal(result$edge_curvature, 0.3)
})

test_that("extract_curved_edges() respects custom curvature", {
  result <- extract_curved_edges(list(y ~ z + curved(c, 0.5)))
  expect_equal(result$edge_curvature, 0.5)
})

test_that("extract_curved_edges() handles multiple curved vars", {
  result <- extract_curved_edges(list(y ~ curved(a) + curved(b, 0.5)))
  expect_equal(nrow(result), 2)
  expect_equal(result$name, c("a", "b"))
  expect_equal(result$to, c("y", "y"))
  expect_equal(result$edge_curvature, c(0.3, 0.5))
})

test_that("extract_curved_edges() ignores non-curved vars", {
  result <- extract_curved_edges(list(y ~ z + curved(c)))
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "c")
})

test_that("extract_curved_edges() returns empty tibble when no curved()", {
  result <- extract_curved_edges(list(y ~ z + x))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("name", "to", "edge_curvature"))
})

test_that("extract_curved_edges() works across multiple formulas", {
  result <- extract_curved_edges(list(
    y ~ z + curved(c),
    m ~ curved(x, -0.4)
  ))
  expect_equal(nrow(result), 2)
  expect_equal(result$name, c("c", "x"))
  expect_equal(result$to, c("y", "m"))
  expect_equal(result$edge_curvature, c(0.3, -0.4))
})

test_that("strip_curved() removes curved() wrapper from formulas", {
  stripped <- strip_curved(y ~ z + curved(c))
  expect_equal(all.vars(stripped), c("y", "z", "c"))
  # Should produce the same dagitty string as y ~ z + c
  expect_equal(formula2char(stripped), formula2char(y ~ z + c))
})

test_that("strip_curved() handles curved() with curvature arg", {
  stripped <- strip_curved(y ~ curved(c, 0.5))
  expect_equal(formula2char(stripped), formula2char(y ~ c))
})

test_that("strip_curved() is no-op when no curved() present", {
  fmla <- y ~ z + x
  stripped <- strip_curved(fmla)
  expect_equal(formula2char(stripped), formula2char(fmla))
})

test_that("dagify() with curved() stores curved_edges attr", {
  dag <- dagify(y ~ z + curved(c), c ~ z)
  curved_edges <- attr(dag, "curved_edges")
  expect_s3_class(curved_edges, "tbl_df")
  expect_equal(nrow(curved_edges), 1)
  expect_equal(curved_edges$name, "c")
  expect_equal(curved_edges$to, "y")
  expect_equal(curved_edges$edge_curvature, 0.3)
})

test_that("dagify() without curved() has no curved_edges attr", {
  dag <- dagify(y ~ z + c, c ~ z)
  expect_null(attr(dag, "curved_edges"))
})

test_that("dagify() with curved() creates valid dagitty", {
  dag <- dagify(y ~ z + curved(c, 0.5), c ~ z)
  expect_s3_class(dag, "dagitty")
  # The DAG structure should be identical to without curved()
  dag_plain <- dagify(y ~ z + c, c ~ z)
  expect_equal(names(dag), names(dag_plain))
})

test_that("curved() works with bidirected edges", {
  dag <- dagify(y ~ x + ~ curved(z, 0.5))
  curved_edges <- attr(dag, "curved_edges")
  expect_equal(nrow(curved_edges), 1)
  expect_equal(curved_edges$name, "z")
  expect_equal(curved_edges$to, "y")
  expect_equal(curved_edges$edge_curvature, 0.5)
})

# -- tidy_dagitty curved_edges integration ------------------------------------

test_that("tidy_dagitty() picks up curved_edges from dagitty attr", {
  dag <- dagify(
    y ~ z + curved(c, 0.5),
    c ~ z,
    coords = list(x = c(z = 1, c = 2, y = 3), y = c(z = 0, c = 1, y = 0))
  )
  td <- tidy_dagitty(dag)
  dat <- pull_dag_data(td)

  expect_true("edge_curvature" %in% names(dat))
  c_to_y <- dat[dat$name == "c" & dat$to == "y" & !is.na(dat$to), ]
  expect_equal(c_to_y$edge_curvature, 0.5)
})

test_that("non-curved edges get NA edge_curvature", {
  dag <- dagify(
    y ~ z + curved(c, 0.5),
    c ~ z,
    coords = list(x = c(z = 1, c = 2, y = 3), y = c(z = 0, c = 1, y = 0))
  )
  td <- tidy_dagitty(dag)
  dat <- pull_dag_data(td)

  z_to_y <- dat[dat$name == "z" & dat$to == "y" & !is.na(dat$to), ]
  expect_true(is.na(z_to_y$edge_curvature))
})

test_that("tidy_dagitty() picks up curved_edges without explicit coords", {
  dag <- dagify(y ~ curved(x, 0.5))
  td <- tidy_dagitty(dag)
  dat <- pull_dag_data(td)

  expect_true("edge_curvature" %in% names(dat))
  x_to_y <- dat[dat$name == "x" & dat$to == "y" & !is.na(dat$to), ]
  expect_equal(x_to_y$edge_curvature, 0.5)
})

test_that("curved() rejects non-literal curvature values", {
  expect_error(
    dagify(y ~ curved(x, "high")),
    "curvature"
  )
  expect_error(
    dagify(y ~ curved(x, TRUE)),
    "curvature"
  )
})

test_that("curved_edges attr survives pull_dag() round-trip", {
  dag <- dagify(y ~ curved(x, 0.5))
  td <- tidy_dagitty(dag)
  dag2 <- pull_dag(td)
  ce <- attr(dag2, "curved_edges")
  expect_false(is.null(ce))
  expect_equal(ce$edge_curvature, 0.5)

  # Re-tidying should still produce edge_curvature

  td2 <- tidy_dagitty(dag2)
  dat2 <- pull_dag_data(td2)
  expect_true("edge_curvature" %in% names(dat2))
  x_to_y <- dat2[dat2$name == "x" & dat2$to == "y" & !is.na(dat2$to), ]
  expect_equal(x_to_y$edge_curvature, 0.5)
})

test_that("tidy_dagitty() without curved() has no edge_curvature column", {
  dag <- dagify(y ~ z + c, c ~ z)
  td <- tidy_dagitty(dag)
  dat <- pull_dag_data(td)
  expect_false("edge_curvature" %in% names(dat))
})

test_that("curved() end-to-end with geom_dag_arrow_arc snapshot", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + curved(m, 0.5),
    m ~ x,
    coords = list(x = c(x = 1, m = 2, y = 3), y = c(x = 0, m = 0, y = 0))
  )
  p <- dag |>
    tidy_dagitty() |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(aes(edge_curvature = edge_curvature)) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("dagify curved formula end-to-end", p)
})

test_that("curved() with ggdag() snapshot", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + curved(m, 0.5),
    m ~ x,
    coords = list(x = c(x = 1, m = 2, y = 3), y = c(x = 0, m = 0, y = 0))
  )
  withr::local_options(ggdag.edge_engine = "ggarrow")
  p <- ggdag(dag)

  expect_doppelganger("dagify curved with ggdag", p)
})

test_that("curved() mixed with straight edges snapshot", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + curved(m, 0.5),
    m ~ x + curved(c, -0.4),
    x ~ c,
    coords = list(
      x = c(c = 1, x = 2, m = 3, y = 4),
      y = c(c = 0, x = 0, m = 0, y = 0)
    )
  )
  p <- dag |>
    tidy_dagitty() |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(aes(edge_curvature = edge_curvature)) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("dagify curved mixed straight and curved", p)
})

test_that("curved() with negative curvature snapshot", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + curved(m, -0.5),
    m ~ x,
    coords = list(x = c(x = 1, m = 2, y = 3), y = c(x = 0, m = 0, y = 0))
  )
  p <- dag |>
    tidy_dagitty() |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(aes(edge_curvature = edge_curvature)) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("dagify curved negative curvature", p)
})
