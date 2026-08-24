test_that("dagify creates correct dagitty", {
  test_dag <- dagify(y ~ x + z, x ~ z)
  expect_equal(test_dag[[1]], "dag {\nx\ny\nz\nx -> y\nz -> x\nz -> y\n}\n")
  expect_s3_class(test_dag, "dagitty")
})

test_that("dag() accepts a character vector of dagitty statements", {
  from_vector <- dag(c("x -> y", "y -> z"))
  from_args <- dag("x -> y", "y -> z")

  expect_s3_class(from_vector, "dagitty")
  expect_equal(dagitty::edges(from_vector), dagitty::edges(from_args))
  expect_equal(from_vector[[1]], from_args[[1]])
})

test_that("dag() still handles a single statement containing several nodes", {
  expect_setequal(names(dag("{x m} -> y")), c("x", "m", "y"))
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

# -- input validation ----------------------------------------------------------

test_that("dagify() rejects one-sided formulas", {
  expect_error(dagify(~x), class = "ggdag_type_error")
  expect_ggdag_error(dagify(~x))
})

test_that("dagify() rejects character input", {
  expect_error(dagify("y ~ x"), class = "ggdag_type_error")
  expect_ggdag_error(dagify("y ~ x"))
})

# -- mixed directed and bidirected terms ---------------------------------------

# edges as "v e w" strings, sorted so row order does not matter
dag_edge_strings <- function(.dag) {
  .edges <- dagitty::edges(.dag)
  sort(paste(.edges$v, .edges$e, .edges$w))
}

test_that("dagify() keeps directed terms directed when a formula mixes arrows", {
  # `y ~ x + ~z` parses as x + (~z), so only z is bidirected
  mixed <- dagify(y ~ x + ~z)
  separate <- dagify(y ~ x, y ~ ~z)

  expect_equal(dag_edge_strings(mixed), dag_edge_strings(separate))
  expect_equal(dag_edge_strings(mixed), c("x -> y", "y <-> z"))
})

test_that("dagify() mixed-arrow formulas agree on adjustment sets", {
  mixed <- dagify(y ~ x + ~z, m ~ x + z, exposure = "x", outcome = "y")
  separate <- dagify(y ~ x, y ~ ~z, m ~ x + z, exposure = "x", outcome = "y")

  expect_equal(
    dagitty::adjustmentSets(mixed),
    dagitty::adjustmentSets(separate)
  )
})

test_that("dagify() still bidirects the whole RHS after a leading tilde", {
  # `y ~ ~x + z` parses as ~(x + z), so both terms are bidirected
  whole <- dagify(y ~ ~ x + z)

  expect_equal(
    dag_edge_strings(whole),
    dag_edge_strings(dagify(y ~ ~x, y ~ ~z))
  )
  expect_true(all(dagitty::edges(whole)$e == "<->"))
})

test_that("dagify() lets parentheses limit how far a tilde reaches", {
  # `(~z)` bidirects only z, so w stays directed
  parenthesized <- dagify(y ~ x + (~z) + w)

  expect_equal(
    dag_edge_strings(parenthesized),
    c("w -> y", "x -> y", "y <-> z")
  )
  expect_equal(
    dag_edge_strings(parenthesized),
    dag_edge_strings(dagify(y ~ x + w, y ~ ~z))
  )
})

test_that("dagify() bidirects a lone parenthesized tilde", {
  expect_equal(dag_edge_strings(dagify(y ~ (~z))), "y <-> z")
})

test_that("dagify() looks through parentheses around a group of terms", {
  expect_equal(
    dag_edge_strings(dagify(y ~ (x + z))),
    dag_edge_strings(dagify(y ~ x + z))
  )
})

test_that("curved() in a mixed-arrow formula keeps the directed edge directed", {
  curved_mixed <- dagify(y ~ x + ~ curved(z, 0.5))

  expect_equal(
    dag_edge_strings(curved_mixed),
    dag_edge_strings(dagify(y ~ x, y ~ ~z))
  )
})

# -- node names outside dagitty's bareword class -------------------------------

test_that("dagify() accepts node names dagitty has to quote", {
  expect_setequal(names(dagify(hjärta ~ coração)), c("hjärta", "coração"))
  expect_setequal(names(dagify(y ~ `my var`)), c("y", "my var"))
})

test_that("dagify() produces unchanged dag strings for ordinary names", {
  expect_equal(
    dagify(y ~ x + z, x ~ z)[[1]],
    "dag {\nx\ny\nz\nx -> y\nz -> x\nz -> y\n}\n"
  )
  expect_equal(dagify(y ~ ~x)[[1]], "dag {\nx\ny\nx <-> y\n}\n")
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

test_that("dagify() treats ggdag::curved() the same as curved()", {
  expect_equal(
    attr(dagify(y ~ x + ggdag::curved(m, 0.5), m ~ x), "curved_edges"),
    attr(dagify(y ~ x + curved(m, 0.5), m ~ x), "curved_edges")
  )
})

test_that("dagify() survives other namespace-qualified calls on a formula RHS", {
  expect_no_error(dagify(y ~ x + base::identity(m), m ~ x))
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

test_that("curved() reaches the tidy data for a bidirected edge", {
  # dagitty stores a bidirected edge in written order, which is the reverse of
  # the order `curved()` records it in, so matching on one order alone loses
  # the curvature the user asked for
  tidy_dag <- tidy_dagitty(dagify(y ~ ~ curved(z, 0.5)), seed = 1234)
  dag_data <- pull_dag_data(tidy_dag)
  edge_row <- dag_data[!is.na(dag_data$to), ]

  expect_equal(nrow(edge_row), 1)
  expect_equal(abs(edge_row$edge_curvature), 0.5)
})

test_that("curve_edge() reaches a bidirected edge given either way round", {
  tidy_dag <- tidy_dagitty(dagify(y ~ ~z), seed = 1234)

  stored_order <- pull_dag_data(curve_edge(tidy_dag, "y", "z", 0.5))
  written_order <- pull_dag_data(curve_edge(tidy_dag, "z", "y", 0.5))

  expect_equal(
    abs(stored_order$edge_curvature[!is.na(stored_order$to)]),
    0.5
  )
  expect_equal(
    abs(written_order$edge_curvature[!is.na(written_order$to)]),
    0.5
  )
})

test_that("curve_edge() replaces a bidirected curvature named the other way", {
  tidy_dag <- tidy_dagitty(dagify(y ~ ~z), seed = 1234)

  recurved <- tidy_dag |>
    curve_edge("y", "z", 0.5) |>
    curve_edge("z", "y", 0.2)

  # the second call names the same edge, so it replaces the first curvature
  # rather than being recorded behind it
  expect_equal(nrow(attr(pull_dag(recurved), "curved_edges")), 1)

  dag_data <- pull_dag_data(recurved)
  expect_equal(abs(dag_data$edge_curvature[!is.na(dag_data$to)]), 0.2)
})

test_that("curve_edge() matches a directed edge in its own orientation only", {
  tidy_dag <- tidy_dagitty(
    dagify(
      y ~ x + m,
      m ~ x,
      coords = list(x = c(x = 1, m = 2, y = 3), y = c(x = 0, m = 0, y = 0))
    ),
    seed = 1234
  )

  recurved <- tidy_dag |>
    curve_edge("m", "y", 0.5) |>
    curve_edge("m", "y", 0.2)

  curved_edges <- attr(pull_dag(recurved), "curved_edges")
  expect_equal(nrow(curved_edges), 1)
  expect_equal(curved_edges$edge_curvature, 0.2)

  # a directed edge has a direction of its own, so the reversed pair names no
  # edge at all
  expect_error(
    curve_edge(tidy_dag, "y", "m", 0.2),
    class = "ggdag_dag_error"
  )
})

test_that("set_curve_edges() reaches a bidirected edge given either way round", {
  tidy_dag <- tidy_dagitty(dagify(y ~ ~z), seed = 1234)

  reversed <- set_curve_edges(
    tidy_dag,
    data.frame(from = "z", to = "y", curvature = 0.5)
  )
  dag_data <- pull_dag_data(reversed)

  expect_equal(abs(dag_data$edge_curvature[!is.na(dag_data$to)]), 0.5)
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

test_that("non-curved edges get edge_curvature=0 when curved() is used", {
  dag <- dagify(
    y ~ z + curved(c, 0.5),
    c ~ z,
    coords = list(x = c(z = 1, c = 2, y = 3), y = c(z = 0, c = 1, y = 0))
  )
  td <- tidy_dagitty(dag)
  dat <- pull_dag_data(td)

  z_to_y <- dat[dat$name == "z" & dat$to == "y" & !is.na(dat$to), ]
  expect_equal(z_to_y$edge_curvature, 0)
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

test_that("curved() non-literal curvature error carries the ggdag classes", {
  expect_error(dagify(y ~ curved(x, a)), class = "ggdag_type_error")
  expect_error(dagify(y ~ curved(x, a)), class = "ggdag_error")
  expect_ggdag_error(dagify(y ~ curved(x, a)))
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

test_that("non-curved edges default to edge_curvature=0 when curved() is used", {
  dag <- dagify(
    y ~ x + curved(m, 0.5),
    m ~ x,
    coords = list(x = c(x = 1, m = 2, y = 3), y = c(x = 0, m = 0, y = 0))
  )
  td <- tidy_dagitty(dag)
  dat <- pull_dag_data(td)

  # The curved edge should have its specified value
  curved_row <- dat[dat$name == "m" & dat$to == "y" & !is.na(dat$to), ]
  expect_equal(curved_row$edge_curvature, 0.5)

  # Non-curved edges should be 0, not NA
  straight_rows <- dat[!is.na(dat$to) & dat$name != "m", ]
  expect_true(all(straight_rows$edge_curvature == 0))

  # Node-only rows (no outgoing edge) should be NA
  node_rows <- dat[is.na(dat$to), ]
  expect_true(all(is.na(node_rows$edge_curvature)))
})

test_that("curved() end-to-end with geom_dag_arrow_arc snapshot", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ curved(x, -0.5) + curved(m, 0.5),
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
    y ~ curved(x, -0.5) + curved(m, 0.5),
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
    y ~ curved(x, -0.5) + m,
    m ~ x + curved(c, 0.4),
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
    y ~ curved(x, 0.5) + curved(m, -0.5),
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

# -- curve_edge() and set_curve_edges() ----------------------------------------

test_that("curve_edge() adds curved_edges attr to dagitty", {
  dag <- dagify(y ~ x + m, m ~ x)
  dag2 <- curve_edge(dag, from = "m", to = "y", curvature = 0.5)

  ce <- attr(dag2, "curved_edges")
  expect_false(is.null(ce))
  expect_equal(nrow(ce), 1)
  expect_equal(ce$name, "m")
  expect_equal(ce$to, "y")
  expect_equal(ce$edge_curvature, 0.5)
})

test_that("curve_edge() updates existing curvature", {
  dag <- dagify(y ~ curved(x, 0.3))
  dag2 <- curve_edge(dag, from = "x", to = "y", curvature = 0.7)

  ce <- attr(dag2, "curved_edges")
  expect_equal(nrow(ce), 1)
  expect_equal(ce$edge_curvature, 0.7)
})

test_that("curve_edge() on tidy_dagitty updates edge_curvature column", {
  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 1, m = 2, y = 3), y = c(x = 0, m = 0, y = 0))
  )
  td <- tidy_dagitty(dag)
  td2 <- curve_edge(td, from = "m", to = "y", curvature = 0.5)

  dat <- pull_dag_data(td2)
  curved_row <- dat[dat$name == "m" & dat$to == "y" & !is.na(dat$to), ]
  expect_equal(curved_row$edge_curvature, 0.5)

  # The dagitty object should also have the attr
  ce <- attr(pull_dag(td2), "curved_edges")
  expect_false(is.null(ce))
})

test_that("curve_edge() errors for invalid node names", {
  dag <- dagify(y ~ x)
  expect_error(
    curve_edge(dag, from = "nonexistent", to = "y", curvature = 0.5),
    class = "ggdag_dag_error"
  )
  expect_error(
    curve_edge(dag, from = "x", to = "nonexistent", curvature = 0.5),
    class = "ggdag_dag_error"
  )
})

test_that("curve_edge() errors when the edge does not exist", {
  dag <- dagify(y ~ x + m, m ~ x)
  # the edge runs m -> y, so these endpoints are swapped
  expect_error(
    curve_edge(dag, from = "y", to = "m", curvature = 0.7),
    class = "ggdag_dag_error"
  )
  expect_ggdag_error(curve_edge(dag, from = "y", to = "m", curvature = 0.7))
})

test_that("curve_edge() on a tidy_dagitty errors when the edge does not exist", {
  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 1, m = 2, y = 3), y = c(x = 0, m = 0, y = 0))
  )
  td <- tidy_dagitty(dag)

  expect_error(
    curve_edge(td, from = "y", to = "m", curvature = 0.7),
    class = "ggdag_dag_error"
  )
})

test_that("curve_edge() accepts either orientation of a bidirected edge", {
  dag <- dagify(y ~ ~x)

  forward <- attr(
    curve_edge(dag, from = "x", to = "y", curvature = 0.4),
    "curved_edges"
  )
  backward <- attr(
    curve_edge(dag, from = "y", to = "x", curvature = 0.4),
    "curved_edges"
  )

  expect_equal(forward$edge_curvature, 0.4)
  expect_equal(backward$edge_curvature, 0.4)
})

test_that("curve_edge() accepts either orientation of an undirected edge", {
  dag <- dag("x -- y")

  forward <- attr(
    curve_edge(dag, from = "x", to = "y", curvature = 0.5),
    "curved_edges"
  )
  backward <- attr(
    curve_edge(dag, from = "y", to = "x", curvature = 0.5),
    "curved_edges"
  )

  expect_equal(forward$edge_curvature, 0.5)
  expect_equal(backward$edge_curvature, 0.5)
})

test_that("set_curve_edges() errors when an edge does not exist", {
  dag <- dagify(y ~ x + m, m ~ x)
  swapped <- data.frame(
    from = c("y", "y"),
    to = c("m", "x"),
    curvature = c(0.7, 0.2)
  )

  expect_error(set_curve_edges(dag, swapped), class = "ggdag_dag_error")
  expect_ggdag_error(set_curve_edges(dag, swapped))
})

test_that("set_curve_edges() replaces all curvatures from a data frame", {
  dag <- dagify(y ~ x + m, m ~ x)
  edges <- data.frame(
    from = c("x", "m"),
    to = c("y", "y"),
    curvature = c(0.3, -0.4)
  )
  dag2 <- set_curve_edges(dag, edges)

  ce <- attr(dag2, "curved_edges")
  expect_equal(nrow(ce), 2)
  expect_equal(ce$edge_curvature[ce$name == "x"], 0.3)
  expect_equal(ce$edge_curvature[ce$name == "m"], -0.4)
})

test_that("set_curve_edges() on tidy_dagitty updates data", {
  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 1, m = 2, y = 3), y = c(x = 0, m = 0, y = 0))
  )
  td <- tidy_dagitty(dag)
  edges <- data.frame(
    from = c("m"),
    to = c("y"),
    curvature = c(0.6)
  )
  td2 <- set_curve_edges(td, edges)

  dat <- pull_dag_data(td2)
  curved_row <- dat[dat$name == "m" & dat$to == "y" & !is.na(dat$to), ]
  expect_equal(curved_row$edge_curvature, 0.6)

  # Non-curved edges should be 0
  straight_rows <- dat[!is.na(dat$to) & !(dat$name == "m" & dat$to == "y"), ]
  expect_true(all(straight_rows$edge_curvature == 0))
})

test_that("set_curve_edges() validates required columns", {
  dag <- dagify(y ~ x)
  expect_error(
    set_curve_edges(dag, data.frame(from = "x", to = "y")),
    class = "ggdag_type_error"
  )
})

test_that("curve_edge() snapshot with geom_dag_arrow_arc", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 1, m = 2, y = 3), y = c(x = 0, m = 0, y = 0))
  )
  td <- tidy_dagitty(dag)
  td <- curve_edge(td, from = "x", to = "y", curvature = -0.5)

  p <- td |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(aes(edge_curvature = edge_curvature)) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("curve_edge post-hoc curvature", p)
})

test_that("set_curve_edges() snapshot with geom_dag_arrow_arc", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + m,
    m ~ x,
    coords = list(x = c(x = 1, m = 2, y = 3), y = c(x = 0, m = 0, y = 0))
  )
  td <- tidy_dagitty(dag)
  edges <- data.frame(
    from = c("x", "m"),
    to = c("y", "y"),
    curvature = c(-0.5, 0.4)
  )
  td <- set_curve_edges(td, edges)

  p <- td |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(aes(edge_curvature = edge_curvature)) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("set_curve_edges post-hoc batch curvature", p)
})

# -- dagitty edge control points -----------------------------------------------

test_that("dagitty control points produce edge_curvature with dagitty coords", {
  # Mediation DAG: X -> Y should curve above M to avoid overlap.
  # Note: dagitty has a JS bug where edge control points with x=0 are dropped
  # (if(e.layout_pos_x) is falsy for 0), so the control point x must be non-zero.
  dag <- dagitty::dagitty(
    'dag {
      bb="-2.5,-0.5,2.5,2.5"
      X [exposure,pos="-2.000,1.000"]
      Y [outcome,pos="2.000,1.000"]
      M [pos="0.000,1.000"]
      Z [pos="0.000,0.000"]
      X -> M
      M -> Y
      X -> Y [pos="0.500,1.800"]
      Z -> X
      Z -> Y
    }'
  )
  td <- tidy_dagitty(dag)
  dat <- pull_dag_data(td)

  expect_true("edge_curvature" %in% names(dat))

  # X -> Y has a control point, should have non-NA curvature
  xy <- dat[dat$name == "X" & dat$to == "Y" & !is.na(dat$to), ]
  expect_false(is.na(xy$edge_curvature))

  # Edges without control points should be straight (0)
  xm <- dat[dat$name == "X" & dat$to == "M" & !is.na(dat$to), ]
  expect_equal(xm$edge_curvature, 0)

  my <- dat[dat$name == "M" & dat$to == "Y" & !is.na(dat$to), ]
  expect_equal(my$edge_curvature, 0)
})

test_that("control points are ignored with non-dagitty layout", {
  dag <- dagitty::dagitty(
    'dag {
      bb="-2.5,-0.5,2.5,2.5"
      X [exposure,pos="-2.000,1.000"]
      Y [outcome,pos="2.000,1.000"]
      M [pos="0.000,1.000"]
      X -> M
      M -> Y
      X -> Y [pos="0.500,1.800"]
    }'
  )
  td <- tidy_dagitty(dag, layout = "fr", use_existing_coords = FALSE)
  dat <- pull_dag_data(td)

  # No edge_curvature column should be created from control points
  # when using a non-dagitty layout
  expect_false("edge_curvature" %in% names(dat))
})

test_that("curved() takes priority over dagitty control points", {
  dag <- dagify(
    y ~ x + curved(z, 0.8),
    z ~ x,
    coords = list(
      x = c(x = -2, z = -0.5, y = 1),
      y = c(x = 1, z = 0.5, y = 1)
    )
  )

  # Manually inject a control point on the dagitty object for z->y edge
  # by modifying the dagitty string to include a pos attribute
  dag_str <- 'dag {
    x [pos="-2.000,1.000"]
    y [pos="1.000,1.000"]
    z [pos="-0.500,0.500"]
    x -> y
    x -> z
    z -> y [pos="0.500,-1.000"]
  }'
  dag2 <- dagitty::dagitty(dag_str)
  attr(dag2, "curved_edges") <- tibble::tibble(
    name = "z",
    to = "y",
    edge_curvature = 0.8
  )

  td <- tidy_dagitty(dag2)
  dat <- pull_dag_data(td)

  # curved() value (0.8) should win over the control point
  zy <- dat[dat$name == "z" & dat$to == "y" & !is.na(dat$to), ]
  expect_equal(zy$edge_curvature, 0.8)
})

test_that("edges without control points are straight (0)", {
  dag <- dagitty::dagitty(
    'dag {
      A [pos="0,0"]
      B [pos="1,0"]
      C [pos="2,0"]
      A -> B
      B -> C [pos="1.5,-0.5"]
    }'
  )
  td <- tidy_dagitty(dag)
  dat <- pull_dag_data(td)

  ab <- dat[dat$name == "A" & dat$to == "B" & !is.na(dat$to), ]
  expect_equal(ab$edge_curvature, 0)

  bc <- dat[dat$name == "B" & dat$to == "C" & !is.na(dat$to), ]
  expect_false(is.na(bc$edge_curvature))
})

test_that("dagitty control points snapshot", {
  skip_if_not_installed("ggarrow")

  # Mediation DAG: X -> Y arcs above M via control point
  dag <- dagitty::dagitty(
    'dag {
      bb="-2.5,-0.5,2.5,2.5"
      X [exposure,pos="-2.000,1.000"]
      Y [outcome,pos="2.000,1.000"]
      M [pos="0.000,1.000"]
      Z [pos="0.000,0.000"]
      X -> M
      M -> Y
      X -> Y [pos="0.500,1.800"]
      Z -> X
      Z -> Y
    }'
  )

  p <- dag |>
    tidy_dagitty() |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(aes(edge_curvature = edge_curvature)) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag() +
    expand_plot(expand_y = expansion(c(0.4, 0.4)))

  expect_doppelganger("dagitty control points", p)
})

test_that("set_curve_edges() rejects one bidirected edge named both ways round", {
  dag <- dagify(y ~ z, x ~ ~y)
  both_ways <- data.frame(
    from = c("x", "y"),
    to = c("y", "x"),
    curvature = c(0.5, -0.5)
  )

  # a bidirected edge has no direction of its own, so both rows name the same
  # edge and the data frame contradicts itself
  expect_error(set_curve_edges(dag, both_ways), class = "ggdag_dag_error")
  expect_ggdag_error(set_curve_edges(dag, both_ways))
})

test_that("set_curve_edges() rejects one directed edge named twice", {
  dag <- dagify(y ~ x + m, m ~ x)
  twice <- data.frame(
    from = c("x", "x"),
    to = c("y", "y"),
    curvature = c(0.5, -0.5)
  )

  expect_error(set_curve_edges(dag, twice), class = "ggdag_dag_error")
})

test_that("set_curve_edges() accepts an edge named once in either orientation", {
  dag <- dagify(y ~ z, x ~ ~y)

  forward <- set_curve_edges(
    dag,
    data.frame(from = "x", to = "y", curvature = 0.5)
  )
  backward <- set_curve_edges(
    dag,
    data.frame(from = "y", to = "x", curvature = 0.5)
  )

  expect_equal(nrow(attr(forward, "curved_edges")), 1)
  expect_equal(nrow(attr(backward, "curved_edges")), 1)
})
