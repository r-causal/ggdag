test_that("geom_dag_arrow() returns a valid ggplot layer", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow()

  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 1)
})

test_that("geom_dag_arrow_arc() returns a valid ggplot layer", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc()

  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 1)
})

test_that("geom_dag_arrows() returns a list of two layers", {
  skip_if_not_installed("ggarrow")

  layers <- geom_dag_arrows()
  expect_type(layers, "list")
  expect_length(layers, 2)
})

test_that("geom_dag_arrows() produces a valid plot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrows() +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_s3_class(p, "ggplot")
})

test_that("filter_direction() works with ggarrow geoms", {
  skip_if_not_installed("ggarrow")

  p_directed <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(data = filter_direction("->"))

  p_bidirected <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(data = filter_direction("<->"))

  expect_s3_class(p_directed, "ggplot")
  expect_s3_class(p_bidirected, "ggplot")
})

test_that("resection defaults respect ggdag.edge_cap option", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_cap = 12)

  # with no resection set anywhere, both ends fall back to the option
  expect_equal(
    inject_dag_resect(list(head = NULL, fins = NULL), data.frame()),
    list(head = 12, fins = 12)
  )

  # a resection the user asked for is left alone
  expect_equal(
    inject_dag_resect(list(head = 3, fins = 3), data.frame()),
    list(head = 3, fins = 3)
  )
})

test_that("an explicit resect = 0 turns auto-resection off", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_cap = 12)

  # 0 is a resection a user can ask for, not a marker for "nothing was set"
  expect_equal(
    inject_dag_resect(list(head = 0, fins = 0), data.frame()),
    list(head = 0, fins = 0)
  )
})

test_that("resect = 0 and resect = 0L resect the same amount", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x, coords = list(x = c(x = 0, y = 1), y = c(x = 0, y = 0)))
  base <- ggplot(dag, aes(x = x, y = y, xend = xend, yend = yend))

  double_zero <- (base + geom_dag_arrow(resect = 0))$layers[[1]]
  integer_zero <- (base + geom_dag_arrow(resect = 0L))$layers[[1]]

  effective <- function(layer) {
    inject_dag_resect(layer$geom_params$resect, data.frame())
  }

  expect_equal(effective(double_zero), list(head = 0, fins = 0))
  expect_equal(effective(double_zero), effective(integer_zero))
})

test_that("auto-resection fills only the ends the user left unset", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_cap = 12)

  # the documented behaviour is per end: an end the user named keeps its
  # value, the other end still picks up the automatic one
  expect_equal(
    inject_dag_resect(list(head = 4, fins = NULL), data.frame()),
    list(head = 4, fins = 12)
  )
  expect_equal(
    inject_dag_resect(list(head = NULL, fins = 4), data.frame()),
    list(head = 12, fins = 4)
  )

  # and naming one end alone leaves the other unset on the layer
  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(resect_head = 4)

  expect_equal(p$layers[[1]]$geom_params$resect, list(head = 4, fins = NULL))
})

test_that("geom_dag_arrow() explicit zero resection snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(resect = 0) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow explicit zero resection", p)
})

test_that("custom arrow ornaments work", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_head_line()) +
    geom_dag_point() +
    theme_dag()

  expect_s3_class(p, "ggplot")
})

# Visual regression tests ---------------------------------------------------

# -- geom_dag_arrow(): straight directed edges --------------------------------

test_that("geom_dag_arrow() default snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow() +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow directed edges", p)
})

test_that("geom_dag_arrow() with arrow_head_line snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_head_line()) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow line head ornament", p)
})

test_that("geom_dag_arrow() with arrow_head_minimal snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_head_minimal()) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow minimal head ornament", p)
})

test_that("geom_dag_arrow() with fins snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = ggarrow::arrow_head_wings(),
      arrow_fins = ggarrow::arrow_fins_feather()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow with feather fins", p)
})

test_that("geom_dag_arrow() with fins_line snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = ggarrow::arrow_head_wings(),
      arrow_fins = ggarrow::arrow_fins_line()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow with line fins", p)
})

test_that("geom_dag_arrow() with mid arrows snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = NULL,
      arrow_mid = ggarrow::arrow_head_wings(),
      mid_place = 0.5
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow mid arrows only", p)
})

# -- Resection variations ----------------------------------------------------

test_that("geom_dag_arrow() small resection snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(resect = 4) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow small resection", p)
})

test_that("geom_dag_arrow() large resection snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(resect = 12) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow large resection", p)
})

test_that("geom_dag_arrow() asymmetric resection snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(resect_head = 4, resect_fins = 12) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow asymmetric resection", p)
})

test_that("geom_dag_arrow() with edge_cap option snapshot", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_cap = 14)

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow() +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow edge_cap option 14", p)
})

# -- Linewidth variations ----------------------------------------------------

test_that("geom_dag_arrow() thin linewidth snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(linewidth = 0.3) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow thin linewidth", p)
})

test_that("geom_dag_arrow() thick linewidth snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(linewidth = 2) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow thick linewidth", p)
})

# -- Styling: colour, alpha, linetype ----------------------------------------

test_that("geom_dag_arrow() custom colour snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(colour = "steelblue") +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow steelblue colour", p)
})

test_that("geom_dag_arrow() with alpha snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(alpha = 0.3) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow with alpha", p)
})

test_that("geom_dag_arrow() dashed linetype snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(linetype = "dashed") +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow dashed linetype", p)
})

test_that("geom_dag_arrow() stroke_colour snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      aes(stroke_colour = "red"),
      linewidth = 2,
      stroke_width = 1
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow with stroke colour", p)
})

# -- geom_dag_arrow_arc(): curved edges ---------------------------------------

test_that("geom_dag_arrow_arc() default snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      arrow_fins = ggarrow::arrow_head_wings()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc bidirected edges", p)
})

test_that("geom_dag_arrow_arc() directed edges default single-headed", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(data = filter_direction("->")) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc directed edges", p)
})

test_that("geom_dag_arrow_arc() all edges default single-headed", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc() +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc all directed", p)
})

test_that("geom_dag_arrow_arc() high curvature snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      curvature = 0.8,
      arrow_fins = ggarrow::arrow_head_wings()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc high curvature", p)
})

test_that("geom_dag_arrow_arc() negative curvature snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      curvature = -0.3,
      arrow_fins = ggarrow::arrow_head_wings()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc negative curvature", p)
})

# -- per-edge curvature --------------------------------------------------------

test_that("edge_curvature absent uses scalar curvature param", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      curvature = 0.5
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  grob <- ggplotGrob(p)
  expect_s3_class(grob, "gtable")
})

test_that("edge_curvature column gives each edge its own curvature", {
  skip_if_not_installed("ggarrow")

  # Time-ordered DAG where edges that skip time points need curvature
  # to route around intermediate nodes (c->m skips x, x->y skips m)
  time_dag <- dagify(
    y ~ x + m,
    m ~ x + c,
    x ~ c,
    coords = time_ordered_coords(force_y = FALSE)
  )

  add_mixed_curvature <- function(x) {
    x <- dplyr::filter(x, !is.na(.data$xend))
    # Edges spanning >1 time step curve around intermediate nodes;
    # adjacent edges stay straight
    span <- abs(x$x - x$xend)
    x$edge_curvature <- ifelse(span > min(span) + 0.01, 0.5, 0)
    x
  }

  p <- time_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      aes(edge_curvature = edge_curvature),
      data = add_mixed_curvature,
      arrow_fins = NULL
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc mixed edge_curvature", p)
})

test_that("NA edge_curvature falls back to curvature param", {
  skip_if_not_installed("ggarrow")

  add_partial_curvature <- function(x) {
    x <- dplyr::filter(x, !is.na(.data$xend))
    x$edge_curvature <- NA_real_
    # Give only the bidirected edge a distinct curvature
    x$edge_curvature[x$direction == "<->"] <- -0.8
    x
  }

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      aes(edge_curvature = edge_curvature),
      data = add_partial_curvature,
      arrow_fins = NULL,
      curvature = 0.3
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc NA fallback curvature", p)
})

test_that("all-NA edge_curvature is equivalent to no column", {
  skip_if_not_installed("ggarrow")

  add_all_na <- function(x) {
    x <- dplyr::filter(x, !is.na(.data$xend))
    x$edge_curvature <- NA_real_
    x
  }

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      aes(edge_curvature = edge_curvature),
      data = add_all_na,
      arrow_fins = NULL,
      curvature = 0.5
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  grob <- ggplotGrob(p)
  expect_s3_class(grob, "gtable")
})

test_that("uniform edge_curvature matches scalar curvature", {
  skip_if_not_installed("ggarrow")

  add_uniform_curvature <- function(x) {
    x <- dplyr::filter(x, !is.na(.data$xend))
    x$edge_curvature <- 0.8
    x
  }

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      aes(edge_curvature = edge_curvature),
      data = add_uniform_curvature,
      arrow_fins = NULL
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc uniform edge_curvature", p)
})

# -- link_arc routes directed edges through geom_dag_arrow_arc() ---------------

test_that("geom_dag_ggarrow_edges link_arc uses geom_dag_arrow_arc for directed", {
  skip_if_not_installed("ggarrow")

  layers <- geom_dag_ggarrow_edges(
    edge_type = "link_arc",
    sizes = c(cap = 8, edge = 0.6, arrow = 5),
    show.legend = NA
  )

  expect_type(layers, "list")
  expect_length(layers, 2)
  # Both layers should use GeomDAGArrowCurve (arc geom), not GeomArrowSegment
  geom_classes <- vapply(layers, function(l) class(l$geom)[1], character(1))
  expect_true(all(geom_classes == "GeomDAGArrowCurve"))
})

test_that("geom_dag_arrow_arc() with line ornaments snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      arrow_head = ggarrow::arrow_head_line(),
      arrow_fins = ggarrow::arrow_head_line()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc line ornaments", p)
})

test_that("geom_dag_arrow_arc() head only (one-directional arc) snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      arrow_fins = NULL
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc head only", p)
})

test_that("geom_dag_arrow_arc() thick linewidth snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      linewidth = 2,
      arrow_fins = ggarrow::arrow_head_wings()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc thick linewidth", p)
})

# -- geom_dag_arrows(): combined dispatcher -----------------------------------

test_that("geom_dag_arrows() default snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrows() +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrows combined edges", p)
})

test_that("geom_dag_arrows() with custom curvature snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrows(curvature = 0.6) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrows high curvature", p)
})

test_that("geom_dag_arrows() with line ornaments snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrows(arrow_head = ggarrow::arrow_head_line()) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrows line head ornament", p)
})

# -- Arrow length variations --------------------------------------------------

test_that("geom_dag_arrow() large length snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(length = 8) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow large ornament length", p)
})

test_that("geom_dag_arrow() small length snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(length = 2) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow small ornament length", p)
})

test_that("geom_dag_arrow() absolute length_head via unit snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(length_head = grid::unit(10, "mm")) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow absolute head length", p)
})

# -- justify parameter -------------------------------------------------------

test_that("geom_dag_arrow() justify=1 snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(justify = 1) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow justify 1", p)
})

# -- Combined with other ggdag layers ----------------------------------------

test_that("geom_dag_arrow() with stylized nodes snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow() +
    geom_dag_node() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow with geom_dag_node", p)
})

test_that("geom_dag_arrows() with geom_dag_point snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrows() +
    geom_dag_point() +
    theme_dag()

  expect_doppelganger("geom_dag_arrows with points no text", p)
})

# -- Different DAG structures -------------------------------------------------

test_that("geom_dag_arrow() simple chain snapshot", {
  skip_if_not_installed("ggarrow")
  withr::local_seed(1234)

  p <- dagify(y ~ x, x ~ z) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow() +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow simple chain", p)
})

test_that("geom_dag_arrow() fork structure snapshot", {
  skip_if_not_installed("ggarrow")
  withr::local_seed(1234)

  p <- dagify(y ~ x, z ~ x) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow() +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow fork", p)
})

test_that("geom_dag_arrow() collider structure snapshot", {
  skip_if_not_installed("ggarrow")
  withr::local_seed(1234)

  p <- dagify(m ~ x + y) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow() +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow collider", p)
})

test_that("geom_dag_arrows() with multiple bidirected edges snapshot", {
  skip_if_not_installed("ggarrow")
  withr::local_seed(1234)

  p <- dagify(
    y ~ x,
    x ~ ~z,
    y ~ ~z
  ) |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrows() +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrows multiple bidirected", p)
})

# -- Wing parameter variations -----------------------------------------------

test_that("geom_dag_arrow() wide wings snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_head_wings(offset = 40)) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow wide wings", p)
})

test_that("geom_dag_arrow() narrow wings snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_head_wings(offset = 10)) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow narrow wings", p)
})

# -- Node-size-aware resection ------------------------------------------------

test_that("dag_arrow_layer discovers node size from geom_dag_point()", {
  skip_if_not_installed("ggarrow")

  # geom_dag_point() added BEFORE the arrow layer — resection should adapt
  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(size = 20) +
    geom_dag_arrow() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow discovers large node size", p)
})

test_that("dag_arrow_layer discovers node size from geom_dag_node()", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_node(size = 24) +
    geom_dag_arrow() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow discovers node size from dag_node", p)
})

test_that("dag_arrow_layer falls back to edge_cap when no node layer", {
  skip_if_not_installed("ggarrow")

  # No node layer added before the arrow — should use edge_cap fallback
  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow fallback no node layer", p)
})

test_that("dag_arrow_layer respects user-set resect over discovery", {
  skip_if_not_installed("ggarrow")

  # User explicitly sets resect — should NOT be overridden by discovery

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(size = 20) +
    geom_dag_arrow(resect = 3) +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow user resect overrides discovery", p)
})

# -- Variable-width arrows (linewidth_head / linewidth_fins) ------------------

test_that("geom_dag_arrow() tapered linewidth snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      aes(linewidth_head = 2, linewidth_fins = 0.2),
      arrow_head = NULL
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow tapered linewidth", p)
})

# -- Stroke styling -----------------------------------------------------------

test_that("geom_dag_arrow() thick stroke snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      linewidth = 2,
      stroke_width = 1.5,
      colour = "steelblue",
      stroke_colour = "black"
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow thick stroke", p)
})

# -- Three-part arrows (head + mid + fins) ------------------------------------

test_that("geom_dag_arrow() head and fins snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = ggarrow::arrow_head_wings(),
      arrow_fins = ggarrow::arrow_fins_feather()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow head and fins", p)
})

test_that("geom_dag_arrow() three-part arrows snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = ggarrow::arrow_head_wings(),
      arrow_mid = ggarrow::arrow_head_wings(),
      arrow_fins = ggarrow::arrow_fins_line()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow three-part", p)
})

# -- mid_place variations -----------------------------------------------------

test_that("geom_dag_arrow() mid_place near start snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = NULL,
      arrow_mid = ggarrow::arrow_head_wings(),
      mid_place = 0.25
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow mid_place 0.25", p)
})

test_that("geom_dag_arrow() mid_place near end snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = NULL,
      arrow_mid = ggarrow::arrow_head_wings(),
      mid_place = 0.75
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow mid_place 0.75", p)
})

# -- force_arrow --------------------------------------------------------------

test_that("geom_dag_arrow() force_arrow snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      length = 8,
      force_arrow = TRUE,
      resect = 12
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow force_arrow", p)
})

# -- Cup ornaments ------------------------------------------------------------

test_that("geom_dag_arrow() cup head ornament snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_cup()) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow cup head", p)
})

test_that("geom_dag_arrow() cup fins snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = ggarrow::arrow_head_wings(),
      arrow_fins = ggarrow::arrow_cup()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow cup fins", p)
})

test_that("geom_dag_arrow() cup butt lineend snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_cup(lineend = "butt")) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow cup butt lineend", p)
})

test_that("geom_dag_arrow() cup with narrow angle snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_cup(angle = 90)) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow cup narrow angle", p)
})

test_that("geom_dag_arrow() cup with wide angle snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_cup(angle = 270)) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow cup wide angle", p)
})

test_that("geom_dag_arrow() cup head and fins snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = ggarrow::arrow_cup(),
      arrow_fins = ggarrow::arrow_cup()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow cup head and fins", p)
})

test_that("geom_dag_arrow() cup with thick linewidth snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = ggarrow::arrow_cup(),
      linewidth = 2
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow cup thick linewidth", p)
})

test_that("geom_dag_arrow() cup with no head arrow snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = NULL,
      arrow_fins = ggarrow::arrow_cup(angle = 180)
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow cup fins only", p)
})

test_that("geom_dag_arrow_arc() cup with angle snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      arrow_head = ggarrow::arrow_cup(angle = 120),
      arrow_fins = ggarrow::arrow_cup(angle = 120)
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc cup with angle", p)
})

# -- Feather fins variations --------------------------------------------------

test_that("geom_dag_arrow() feather fins indent snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_fins = ggarrow::arrow_fins_feather(indent = 0.5)
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow feather fins indent", p)
})

# -- Halfwing and halfline heads ----------------------------------------------

test_that("geom_dag_arrow() halfwing head snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_head_halfwing()) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow halfwing head", p)
})

test_that("geom_dag_arrow() halfline head snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_head_halfline()) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow halfline head", p)
})

# -- Line head with lineend variations ----------------------------------------

test_that("geom_dag_arrow() line head round lineend snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = ggarrow::arrow_head_line(),
      lineend = "round"
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow line head round end", p)
})

# -- Minimal head with angle variation ----------------------------------------

test_that("geom_dag_arrow() minimal head wide angle snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = ggarrow::arrow_head_minimal(angle = 60)) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow minimal wide angle", p)
})

# -- Fins minimal ornament ----------------------------------------------------

test_that("geom_dag_arrow() minimal fins snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_fins = ggarrow::arrow_fins_minimal()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow minimal fins", p)
})

# -- Arc with stroke styling --------------------------------------------------

test_that("geom_dag_arrow_arc() stroke styling snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      linewidth = 2,
      stroke_width = 1,
      colour = "tomato",
      stroke_colour = "grey30",
      arrow_fins = ggarrow::arrow_head_wings()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc stroke styling", p)
})

# -- Arc ornament variations --------------------------------------------------

test_that("geom_dag_arrow_arc() cup ornaments snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      arrow_head = ggarrow::arrow_cup(),
      arrow_fins = ggarrow::arrow_cup()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc cup ornaments", p)
})

test_that("geom_dag_arrow_arc() minimal ornaments snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      arrow_head = ggarrow::arrow_head_minimal(),
      arrow_fins = ggarrow::arrow_fins_minimal()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc minimal ornaments", p)
})

# -- Arc node-size discovery --------------------------------------------------

test_that("dag_arrow_layer discovers node size for arc layers", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(size = 22) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      arrow_fins = ggarrow::arrow_head_wings()
    ) +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc discovers node size", p)
})

# -- Combined dispatcher with node-size discovery -----------------------------

test_that("geom_dag_arrows() with large nodes snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_node(size = 22) +
    geom_dag_arrows() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrows with large nodes", p)
})

# -- Custom ornament matrix ---------------------------------------------------

test_that("geom_dag_arrow() custom matrix ornament snapshot", {
  skip_if_not_installed("ggarrow")

  diamond <- cbind(
    x = c(1, 0.5, 0, 0.5),
    y = c(0, 0.25, 0, -0.25)
  )
  attr(diamond, "notch_angle") <- pi / 2

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = diamond) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow custom diamond ornament", p)
})

# -- No ornaments (plain segments) --------------------------------------------

test_that("geom_dag_arrow() no ornaments snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(arrow_head = NULL, arrow_fins = NULL) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow no ornaments", p)
})

# -- Wings inset variation ----------------------------------------------------

test_that("geom_dag_arrow() wings with inset snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow(
      arrow_head = ggarrow::arrow_head_wings(offset = 30, inset = 50)
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow wings with inset", p)
})

# -- Arc with different ncp ---------------------------------------------------

test_that("geom_dag_arrow_arc() high ncp snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      ncp = 15,
      arrow_fins = ggarrow::arrow_head_wings()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc high ncp", p)
})

# -- Arc with different angle -------------------------------------------------

test_that("geom_dag_arrow_arc() skewed angle snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      angle = 45,
      arrow_fins = ggarrow::arrow_head_wings()
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc skewed angle", p)
})

# geom_dag() ggarrow dispatch tests ----------------------------------------

test_that("geom_dag() with edge_engine='ggraph' returns ggraph edge layers (default)", {
  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag) + geom_dag(edge_engine = "ggraph")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  # Should contain ggraph edge geoms, not ggarrow
  expect_false(any(grepl("DAGArrow", layer_classes)))
})

test_that("geom_dag() with edge_engine='ggarrow' returns ggarrow edge layers", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag) + geom_dag(edge_engine = "ggarrow")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  # Should contain ggarrow-based geoms

  expect_true(any(grepl("DAGArrow", layer_classes)))
})

test_that("geom_dag() with edge_engine='ggarrow' and edge_type='link' uses straight arrows", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag) + geom_dag(edge_engine = "ggarrow", edge_type = "link")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("DAGArrow", layer_classes)))
  # Should NOT have curve geom for link-only
  expect_false(any(grepl("DAGArrowCurve", layer_classes)))
})

test_that("geom_dag() with edge_engine='ggarrow' and edge_type='arc' uses curved arrows", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag) + geom_dag(edge_engine = "ggarrow", edge_type = "arc")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("DAGArrowCurve", layer_classes)))
})

test_that("geom_dag() picks up global edge_engine option", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.edge_engine = "ggarrow")
  p <- ggplot(dag) + geom_dag()
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("DAGArrow", layer_classes)))
})

test_that("geom_dag() with edge_engine='ggarrow' renders without error", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag, aes_dag()) + geom_dag(edge_engine = "ggarrow") + theme_dag()
  expect_doppelganger("geom_dag ggarrow engine link_arc", p)
})

test_that("geom_dag() with edge_engine='ggarrow' and edge_type='link' renders", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow", edge_type = "link") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow engine link", p)
})

test_that("geom_dag() with edge_engine='ggarrow' and edge_type='arc' renders", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow", edge_type = "arc") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow engine arc", p)
})

test_that("geom_dag() with edge_engine='ggarrow' respects arrow_head option", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.arrow_head = ggarrow::arrow_head_line())
  p <- ggplot(dag, aes_dag()) + geom_dag(edge_engine = "ggarrow") + theme_dag()
  expect_doppelganger("geom_dag ggarrow line arrows", p)
})

test_that("geom_dag() with edge_engine='ggarrow' respects curvature option", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ ~z)
  withr::local_options(ggdag.curvature = 0.8)
  p <- ggplot(dag, aes_dag()) + geom_dag(edge_engine = "ggarrow") + theme_dag()
  expect_doppelganger("geom_dag ggarrow high curvature", p)
})

# -- Comprehensive ggarrow visual tests ------------------------------------------

test_that("geom_dag() ggarrow renders complex DAG with bidirected edges", {
  skip_if_not_installed("ggarrow")

  # test_dag has bidirected w1 ~~ w2 — exercises the link_arc split

  p <- ggplot(test_dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow complex dag with bidirected", p)
})

test_that("geom_dag() ggarrow link_arc with only bidirected edges", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(x ~ ~y, y ~ ~z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow only bidirected", p)
})

test_that("geom_dag() ggarrow link type with bidirected edges", {
  skip_if_not_installed("ggarrow")

  # link type should still only produce straight arrows; bidirected get no

  # special curved treatment (same as ggraph link)
  dag <- dagify(y ~ x + z, x ~ ~z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow", edge_type = "link") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow link with bidirected", p)
})

test_that("geom_dag() ggarrow arc type with bidirected edges", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ ~z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow", edge_type = "arc") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow arc with bidirected", p)
})

test_that("geom_dag() ggarrow diagonal type renders", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow", edge_type = "diagonal") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow diagonal", p)
})

test_that("geom_dag() ggarrow diagonal type with bidirected edges", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ ~z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow", edge_type = "diagonal") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow diagonal with bidirected", p)
})

test_that("geom_dag() ggarrow with use_stylized nodes", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow", use_stylized = TRUE) +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow stylized nodes", p)
})

test_that("geom_dag() ggarrow with use_text=FALSE", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow", use_text = FALSE) +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow no text", p)
})

test_that("geom_dag() ggarrow with labels", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + z,
    x ~ z,
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder")
  )
  p <- ggplot(dag, aes_dag()) +
    geom_dag(
      edge_engine = "ggarrow",
      use_labels = TRUE,
      label = label,
      use_text = FALSE
    ) +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow with labels", p)
})

test_that("geom_dag() ggarrow with size scaling", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow", size = 2) +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow size 2", p)
})

test_that("geom_dag() ggarrow with arrow_fins option", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  withr::local_options(ggdag.arrow_fins = ggarrow::arrow_head_wings())
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow arrow fins", p)
})

test_that("geom_dag() ggarrow with negative curvature", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ ~z)
  withr::local_options(ggdag.curvature = -0.5)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow negative curvature", p)
})

test_that("geom_dag() ggarrow with zero curvature (straight bidirected)", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ ~z)
  withr::local_options(ggdag.curvature = 0)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow zero curvature", p)
})

test_that("geom_dag() ggarrow with use_edges=FALSE still renders nodes", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow", use_edges = FALSE) +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow no edges", p)
})

# -- Quick-plot wrappers with ggarrow engine ------------------------------------

test_that("ggdag() renders with ggarrow engine via global option", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_engine = "ggarrow")
  dag <- dagify(y ~ x + z, x ~ z, coords = time_ordered_coords())
  p <- ggdag(dag)
  expect_doppelganger("ggdag ggarrow engine", p)
})

test_that("ggdag() ggarrow with bidirected edges", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_engine = "ggarrow")
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    x ~ ~y,
    coords = time_ordered_coords()
  )
  p <- ggdag(dag)
  expect_doppelganger("ggdag ggarrow bidirected", p)
})

test_that("ggdag_status() renders with ggarrow engine", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_engine = "ggarrow")
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    coords = time_ordered_coords()
  )
  p <- ggdag_status(dag)
  expect_doppelganger("ggdag_status ggarrow engine", p)
})

test_that("ggdag_adjustment_set() renders with ggarrow engine", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_engine = "ggarrow")
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    coords = time_ordered_coords()
  )
  p <- ggdag_adjustment_set(dag)
  expect_doppelganger("ggdag_adjustment_set ggarrow engine", p)
})

test_that("ggdag_collider() renders with ggarrow engine", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_engine = "ggarrow")
  dag <- dagify(
    m ~ x + y,
    y ~ x,
    coords = time_ordered_coords()
  )
  p <- ggdag_collider(dag)
  expect_doppelganger("ggdag_collider ggarrow engine", p)
})

test_that("ggdag_paths() renders with ggarrow engine", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_engine = "ggarrow")
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    coords = time_ordered_coords()
  )
  p <- ggdag_paths(dag)
  expect_doppelganger("ggdag_paths ggarrow engine", p)
})

test_that("ggdag_m_bias() renders with ggarrow engine", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_engine = "ggarrow")
  p <- ggdag_m_bias()
  expect_doppelganger("ggdag_m_bias ggarrow engine", p)
})

test_that("ggdag_confounder_triangle() renders with ggarrow engine", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_engine = "ggarrow")
  p <- ggdag_confounder_triangle()
  expect_doppelganger("ggdag_confounder_triangle ggarrow engine", p)
})

test_that("ggdag_mediation_triangle() renders with ggarrow engine", {
  skip_if_not_installed("ggarrow")

  withr::local_options(ggdag.edge_engine = "ggarrow")
  p <- ggdag_mediation_triangle()
  expect_doppelganger("ggdag_mediation_triangle ggarrow engine", p)
})

# -- ggarrow with color aesthetics (status, adjustment) -------------------------

test_that("geom_dag() ggarrow with status coloring", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    coords = time_ordered_coords()
  ) |>
    tidy_dagitty() |>
    node_status()

  p <- ggplot(dag, aes_dag(color = status)) +
    geom_dag(edge_engine = "ggarrow") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow status coloring", p)
})

test_that("geom_dag() ggarrow with adjustment set coloring", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    coords = time_ordered_coords()
  ) |>
    tidy_dagitty() |>
    dag_adjustment_sets()

  p <- ggplot(dag, aes_dag(color = adjusted)) +
    geom_dag(edge_engine = "ggarrow") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow adjustment coloring", p)
})

# -- Edge cases -----------------------------------------------------------------

test_that("geom_dag() ggarrow with minimal two-node DAG", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow minimal two node", p)
})

test_that("geom_dag() ggarrow with many nodes and dense edges", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x + z1 + z2 + z3,
    x ~ z1 + z2,
    z1 ~ z3,
    z2 ~ z3,
    z1 ~ ~z2,
    coords = time_ordered_coords()
  )
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow") +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow dense dag", p)
})

test_that("geom_dag() ggarrow with global option + custom arrow ornaments", {
  skip_if_not_installed("ggarrow")

  withr::local_options(
    ggdag.edge_engine = "ggarrow",
    ggdag.arrow_head = ggarrow::arrow_head_line(),
    ggdag.arrow_fins = ggarrow::arrow_head_line(),
    ggdag.curvature = 0.5
  )

  dag <- dagify(y ~ x + z, x ~ ~z, coords = time_ordered_coords())
  p <- ggplot(dag, aes_dag()) +
    geom_dag() +
    theme_dag()
  expect_doppelganger("geom_dag ggarrow all ornaments via options", p)
})

test_that("geom_dag_arrow_arc() errors on non-numeric edge_curvature", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x,
    coords = list(x = c(x = 1, y = 2), y = c(x = 0, y = 0))
  )
  td <- tidy_dagitty(dag)
  dat <- pull_dag_data(td)
  dat$edge_curvature <- ifelse(is.na(dat$to), NA, "high")

  p <- ggplot(dat, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(aes(edge_curvature = edge_curvature))

  expect_error(ggplotGrob(p), "edge_curvature")
})

test_that("geom_dag_arrow_arc() non-numeric error carries the ggdag classes", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(
    y ~ x,
    coords = list(x = c(x = 1, y = 2), y = c(x = 0, y = 0))
  )
  td <- tidy_dagitty(dag)
  dat <- pull_dag_data(td)
  dat$edge_curvature <- ifelse(is.na(dat$to), NA, "high")

  p <- ggplot(dat, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(aes(edge_curvature = edge_curvature))

  # ggplot2 wraps draw-time errors, so the ggdag condition is the parent
  err <- tryCatch(ggplotGrob(p), error = function(e) e)
  expect_s3_class(err$parent, "ggdag_type_error")
  expect_s3_class(err$parent, "ggdag_error")
})

# -- ggarrow edges honour the size arguments geom_dag() documents -------------

ggarrow_edge_layers <- function(plot) {
  keep <- vapply(
    plot$layers,
    function(l) grepl("^GeomDAGArrow", class(l$geom)[1]),
    logical(1)
  )
  which(keep)
}

test_that("the ggarrow engine honours edge_width and the size multiplier", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)

  edge_linewidth <- function(...) {
    p <- ggplot(dag, aes_dag()) + geom_dag(edge_engine = "ggarrow", ...)
    built <- ggplot2::ggplot_build(p)
    widths <- unlist(lapply(ggarrow_edge_layers(p), function(i) {
      d <- built$data[[i]]
      if (nrow(d) == 0) NULL else unique(d$linewidth)
    }))
    unique(widths)
  }

  # the documented default edge width, the same one the ggraph engine uses
  expect_equal(edge_linewidth(), 0.6)
  expect_equal(edge_linewidth(edge_width = 3), 3)
  # `size` scales every element, edges included
  expect_equal(edge_linewidth(size = 3), 1.8)
})

test_that("the ggarrow engine honours arrow_length", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)

  arrow_length_of <- function(...) {
    p <- ggplot(dag, aes_dag()) + geom_dag(edge_engine = "ggarrow", ...)
    p$layers[[ggarrow_edge_layers(p)[1]]]$geom_params$length
  }

  # a bare number means multiples of the shaft width to ggarrow, so the
  # length has to travel as an absolute unit to mean points
  expect_equal(arrow_length_of()$head, grid::unit(5, "pt"))
  expect_equal(arrow_length_of(arrow_length = 12)$head, grid::unit(12, "pt"))
  expect_equal(arrow_length_of(size = 2)$head, grid::unit(10, "pt"))
})

test_that("geom_dag() ggarrow custom edge width snapshot", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggplot(dag, aes_dag()) +
    geom_dag(edge_engine = "ggarrow", edge_width = 2, arrow_length = 10) +
    theme_dag()

  expect_doppelganger("geom_dag ggarrow wide edges", p)
})

# -- bidirected edges keep their arc when another edge is curved --------------

bidirected_arc_curvature <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  for (i in seq_along(plot$layers)) {
    if (!inherits(plot$layers[[i]]$geom, "GeomDAGArrowCurve")) {
      next
    }
    d <- built$data[[i]]
    # the u <-> v edge is the only edge drawn at y == 1
    if (nrow(d) > 0 && all(d$y == 1) && all(d$yend == 1)) {
      if (!"edge_curvature" %in% names(d)) {
        return(NA_real_)
      }
      return(d$edge_curvature)
    }
  }
  NULL
}

test_that("curving one edge leaves bidirected edges on their own arc", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  coords <- list(
    x = c(a = 0, b = 1, u = 0, v = 1),
    y = c(a = 0, b = 0, u = 1, v = 1)
  )
  dag <- dagify(b ~ a, u ~ ~v, coords = coords)

  # with nothing curved, the bidirected layer's own curvature draws the arc
  expect_true(all(is.na(bidirected_arc_curvature(ggdag(dag)))))

  # curving the unrelated a -> b edge must not straighten u <-> v: a data
  # value of 0 would override the layer's curvature
  curved <- curve_edge(dag, "a", "b", 0.5)
  expect_true(all(is.na(bidirected_arc_curvature(ggdag(curved)))))
})

test_that("ggdag() ggarrow curved directed edge with bidirected arc snapshot", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  coords <- list(
    x = c(a = 0, b = 1, u = 0, v = 1),
    y = c(a = 0, b = 0, u = 1, v = 1)
  )
  dag <- dagify(b ~ a, u ~ ~v, coords = coords)
  p <- ggdag(curve_edge(dag, "a", "b", 0.5))

  expect_doppelganger("ggdag ggarrow curved edge keeps bidirected arc", p)
})

# -- a stored arrow layer must not carry one plot's answers to the next -------

test_that("one stored arrow layer reads each plot it joins", {
  skip_if_not_installed("ggarrow")

  tidy_dag <- tidy_dagitty(dagify(y ~ x, z ~ x))
  arrow_layer <- geom_dag_arrow()

  with_nodes <- ggplot(tidy_dag, aes_dag()) +
    geom_dag_point(size = 32) +
    arrow_layer
  without_nodes <- ggplot(tidy_dag, aes_dag()) + arrow_layer

  expect_equal(with_nodes$layers[[2]]$geom_params$resect$head, 16)
  # the second plot has no node layer, so nothing is discovered there
  expect_null(without_nodes$layers[[1]]$geom_params$resect$head)
  # and the stored layer is still the blank one that was created
  expect_null(arrow_layer$layer$geom_params$resect$head)
})
