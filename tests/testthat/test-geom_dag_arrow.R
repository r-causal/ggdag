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
  expect_equal(ggdag_option("edge_cap", 8), 12)
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
    geom_dag_arrow_arc(data = filter_direction("<->")) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc bidirected edges", p)
})

test_that("geom_dag_arrow_arc() high curvature snapshot", {
  skip_if_not_installed("ggarrow")

  p <- test_dag |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_arrow_arc(
      data = filter_direction("<->"),
      curvature = 0.8
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
      curvature = -0.3
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc negative curvature", p)
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
      linewidth = 2
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
      stroke_colour = "grey30"
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
    geom_dag_arrow_arc(data = filter_direction("<->")) +
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
      ncp = 15
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
      angle = 45
    ) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

  expect_doppelganger("geom_dag_arrow_arc skewed angle", p)
})
