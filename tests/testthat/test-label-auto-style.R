# `geom_dag_label_auto()` draws its labels the way `ggplot2::geom_label()` and
# `geom_dag_label_repel()` do: a white rounded box with a thin border in the
# text colour. `geom_dag_label_auto2()` is to it what `geom_dag_label_repel2()`
# is to `geom_dag_label_repel()`, the same layer drawn without the border. The
# automatic geoms ignore `box.padding`, so the border is the whole of what the
# second geom restyles, and there is no text counterpart: it would draw exactly
# what `geom_dag_text_auto()` draws.
#
# The border is drawn after the labels are placed, so it moves none of them.
# The drawn boxes are read off a forced grob tree on an off-screen ragg device
# at 150 dpi through `perf_measure_render()` from `helper-label-perf.R`, while
# the device is still open.

# A five-node DAG whose labels are long enough that the panel has to make room
# for them, laid out by the default time-ordered layout.
style_room_dag <- function() {
  dagify(
    exam ~ podcast + prepared + mood,
    podcast ~ mood + humor + prepared,
    exposure = "podcast",
    outcome = "exam",
    labels = c(
      podcast = "Listened to a podcast",
      exam = "Exam score",
      mood = "Mood before the exam",
      humor = "Sense of humor",
      prepared = "Time spent preparing"
    )
  )
}

style_room_labels <- c(
  "Listened to a podcast",
  "Exam score",
  "Mood before the exam",
  "Sense of humor",
  "Time spent preparing"
)

# The labelled scene drawn with `label_geom` through `ggdag()`.
style_plot <- function(label_geom, ...) {
  ggdag(
    style_room_dag(),
    use_labels = TRUE,
    label_geom = label_geom,
    ...
  ) +
    theme_dag()
}

# `colour` as an eight-digit hex string, so that a named colour and the hex
# string the geom resolves it to compare equal. A missing colour stays missing.
hex_colour <- function(colour) {
  if (is.null(colour) || is.na(colour)) {
    return(NA_character_)
  }
  channels <- grDevices::col2rgb(colour, alpha = TRUE)
  grDevices::rgb(
    channels[[1]],
    channels[[2]],
    channels[[3]],
    channels[[4]],
    maxColorValue = 255
  )
}

# One row per drawn label box of `plot`: the label, the colour and line width
# its border is stroked with, whether that stroke leaves any ink, the box fill,
# and the colour of the text inside it.
label_box_style <- function(plot, size = c(7, 5)) {
  perf_measure_render(plot, size, function(tree) {
    children <- tree$children
    names <- vapply(children, function(child) child$name %||% "", character(1))
    boxes <- unname(children[grepl("roundrect", names)])
    texts <- unname(children[grepl("text", names)])
    stopifnot(length(boxes) > 0, length(boxes) == length(texts))

    do.call(
      rbind,
      lapply(seq_along(boxes), function(i) {
        gp <- boxes[[i]]$gp
        border_colour <- hex_colour(gp$col)
        border_lwd <- gp$lwd %||% NA_real_
        alpha <- if (is.na(border_colour)) {
          0
        } else {
          grDevices::col2rgb(border_colour, alpha = TRUE)[[4]]
        }
        data.frame(
          label = as.character(texts[[i]]$label),
          border_colour = border_colour,
          border_lwd = border_lwd,
          border_visible = alpha > 0 && !is.na(border_lwd) && border_lwd > 0,
          fill = hex_colour(gp$fill),
          text_colour = hex_colour(texts[[i]]$gp$col),
          stringsAsFactors = FALSE
        )
      })
    )
  })
}

# The layer a constructor returns, unwrapped from its `dag_layer`.
unwrapped_layer <- function(layer) {
  if (inherits(layer, "dag_layer")) {
    return(layer$layer)
  }
  layer
}

# The parameter names two layers disagree on, across every kind of parameter a
# layer carries.
style_layer_differences <- function(one, other) {
  one <- unwrapped_layer(one)
  other <- unwrapped_layer(other)
  kinds <- c("aes_params", "geom_params", "stat_params")
  differences <- purrr::map(kinds, \(kind) {
    names <- union(names(one[[kind]]), names(other[[kind]]))
    names[
      !purrr::map_lgl(names, \(name) {
        identical(one[[kind]][[name]], other[[kind]][[name]])
      })
    ]
  })

  sort(purrr::list_c(differences, ptype = character()))
}

# The ranges the panel of `plot` ends up with, after every layer has trained the
# position scales and the expansion has been applied.
style_panel_ranges <- function(plot) {
  params <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]

  list(x = params$x.range, y = params$y.range)
}

# The bordered default ----------------------------------------------------------

test_that("geom_dag_label_auto() builds a layer with a 0.25 mm border", {
  layer <- geom_dag_label_auto(ggplot2::aes(label = label))

  expect_equal(layer$geom_params$label.size, 0.25)
})

test_that("geom_dag_label_auto() draws a border in the text colour by default", {
  skip_if_not_installed("ragg")

  plot <- ggplot(style_room_dag(), aes_dag()) +
    geom_dag_point() +
    geom_dag_edges() +
    geom_dag_label_auto(ggplot2::aes(label = label)) +
    theme_dag()
  style <- label_box_style(plot)

  expect_setequal(style$label, style_room_labels)
  expect_true(all(style$border_visible))
  expect_equal(style$border_lwd, rep(0.25 * ggplot2::.pt, nrow(style)))
  expect_equal(style$border_colour, rep("#000000FF", nrow(style)))
  expect_equal(style$border_colour, style$text_colour)
  expect_equal(style$fill, rep("#FFFFFFFF", nrow(style)))
})

test_that("the border of geom_dag_label_auto() follows the text colour", {
  skip_if_not_installed("ragg")

  plot <- ggplot(style_room_dag(), aes_dag()) +
    geom_dag_point() +
    geom_dag_edges() +
    geom_dag_label_auto(ggplot2::aes(label = label), colour = "navy") +
    theme_dag()
  style <- label_box_style(plot)

  expect_true(all(style$border_visible))
  expect_equal(style$border_colour, rep(hex_colour("navy"), nrow(style)))
  expect_equal(style$border_colour, style$text_colour)
})

test_that("ggdag() draws the automatic labels with a border", {
  skip_if_not_installed("ragg")

  style <- label_box_style(style_plot(geom_dag_label_auto))

  expect_setequal(style$label, style_room_labels)
  expect_true(all(style$border_visible))
  expect_equal(style$border_lwd, rep(0.25 * ggplot2::.pt, nrow(style)))
  expect_equal(style$border_colour, style$text_colour)
  expect_equal(style$fill, rep("#FFFFFFFF", nrow(style)))
})

# The borderless variant --------------------------------------------------------

test_that("geom_dag_label_auto2() is the automatic label layer without a border", {
  plain <- geom_dag_label_auto(ggplot2::aes(label = label))
  borderless <- geom_dag_label_auto2(ggplot2::aes(label = label))

  expect_s3_class(borderless, "dag_layer")
  expect_true(inherits(borderless$stat, "StatNodesLabelAuto"))
  expect_true(inherits(borderless$geom, "GeomDagLabelAuto"))
  expect_equal(borderless$geom_params$label.size, NA)

  # the border is the only thing the two layers disagree on
  expect_equal(style_layer_differences(plain, borderless), "label.size")
  expect_identical(borderless$discover, plain$discover)
  expect_identical(borderless$default_label, plain$default_label)
  expect_identical(borderless$debug, plain$debug)
})

test_that("geom_dag_label_auto2() is tagged for geom_dag() as geom_dag_label_auto() is", {
  expect_true(isTRUE(attr(geom_dag_label_auto2, "dag_node_aware")))
  expect_setequal(
    attr(geom_dag_label_auto2, "dag_node_aware_extra"),
    attr(geom_dag_label_auto, "dag_node_aware_extra")
  )
  expect_equal(
    attr(geom_dag_label_auto2, "dag_node_aware_box_padding"),
    attr(geom_dag_label_auto, "dag_node_aware_box_padding")
  )
})

test_that("geom_dag_label_auto2() draws white boxes with no border", {
  skip_if_not_installed("ragg")

  plot <- ggplot(style_room_dag(), aes_dag()) +
    geom_dag_point() +
    geom_dag_edges() +
    geom_dag_label_auto2(ggplot2::aes(label = label)) +
    theme_dag()
  style <- label_box_style(plot)

  expect_setequal(style$label, style_room_labels)
  expect_false(any(style$border_visible))
  expect_equal(style$fill, rep("#FFFFFFFF", nrow(style)))
})

# The border and placement ------------------------------------------------------

test_that("the border moves no label", {
  skip_if_not_installed("ragg")

  bordered <- perf_measure_render(
    style_plot(geom_dag_label_auto),
    c(7, 5),
    perf_placement
  )
  borderless <- perf_measure_render(
    style_plot(geom_dag_label_auto2),
    c(7, 5),
    perf_placement
  )

  expect_setequal(bordered$boxes$label, style_room_labels)
  expect_equal(borderless$boxes, bordered$boxes, tolerance = 1e-9)
  expect_equal(borderless$leaders, bordered$leaders, tolerance = 1e-9)
  expect_identical(borderless$unresolved, bordered$unresolved)
})

# Through geom_dag() and ggdag() ------------------------------------------------

test_that("geom_dag() threads the node-aware parameters to geom_dag_label_auto2()", {
  layers <- geom_dag(
    use_labels = TRUE,
    label_geom = geom_dag_label_auto2,
    node_size = 20,
    edge_cap = 12,
    label_wrap = 10
  )
  label_item <- layers[[4]]

  expect_s3_class(label_item, "dag_layer")
  expect_true(inherits(label_item$geom, "GeomDagLabelAuto"))
  params <- c(label_item$stat_params, label_item$geom_params)
  expect_equal(params[["node_size"]], 20)
  expect_equal(params[["edge_cap"]], 12)
  expect_equal(params[["wrap"]], 10)
  expect_equal(params[["label.size"]], NA)
})

test_that("ggdag() hands geom_dag_label_auto2() the plot's edge_cap and label_wrap", {
  skip_if_not_installed("ragg")

  plot <- style_plot(
    geom_dag_label_auto2,
    node_size = 30,
    edge_cap = 15,
    label_wrap = 6
  )

  params <- auto_label_params(plot)
  expect_equal(params[["edge_cap"]], 15)
  expect_equal(params[["wrap"]], 6)
  expect_equal(params[["label.size"]], NA)

  # the cap decides where the traced edge ink ends, so it has to reach the
  # engine and not only the layer
  drawn <- perf_measure_render(plot, c(7, 5), function(tree) tree$params)
  expect_equal(drawn$edge_cap, 15)
  expect_equal(drawn$wrap, 6)
  expect_equal(drawn$label.size, NA)
})

test_that("a label.size in a geom_dag_label_auto2() wrapper wins", {
  skip_if_not_installed("ragg")

  wrapper <- function(...) geom_dag_label_auto2(..., label.size = 0.5)
  plot <- expect_no_warning(
    style_plot(wrapper, edge_cap = 15, label_wrap = 6)
  )

  params <- auto_label_params(plot)
  expect_equal(params[["label.size"]], 0.5)
  expect_equal(params[["edge_cap"]], 15)
  expect_equal(params[["wrap"]], 6)

  style <- label_box_style(plot)
  expect_true(all(style$border_visible))
  expect_equal(style$border_lwd, rep(0.5 * ggplot2::.pt, nrow(style)))
})

# Label room --------------------------------------------------------------------

test_that("geom_dag_label_auto2() reserves the panel room geom_dag_label_auto() does", {
  bare <- style_panel_ranges(ggdag(style_room_dag()))
  plain <- style_panel_ranges(
    ggdag(style_room_dag(), use_labels = TRUE, label_geom = geom_dag_label_auto)
  )
  borderless <- style_panel_ranges(
    ggdag(
      style_room_dag(),
      use_labels = TRUE,
      label_geom = geom_dag_label_auto2
    )
  )

  expect_gt(diff(borderless$x), diff(bare$x))
  expect_gt(diff(borderless$y), diff(bare$y))
  expect_equal(borderless, plain)

  dag <- tidy_dagitty(style_room_dag())
  reserved <- c("xmin", "xmax", "ymin", "ymax")
  plain_data <- ggplot2::layer_data(
    ggplot2::ggplot(dag, aes_dag()) +
      geom_dag_label_auto(ggplot2::aes(label = label)),
    1
  )
  borderless_data <- ggplot2::layer_data(
    ggplot2::ggplot(dag, aes_dag()) +
      geom_dag_label_auto2(ggplot2::aes(label = label)),
    1
  )

  expect_true(all(reserved %in% names(borderless_data)))
  expect_equal(borderless_data[reserved], plain_data[reserved])
})

# Visual baselines --------------------------------------------------------------
#
# Each picture is measured before it is recorded, so a baseline cannot be taken
# from a plot whose boxes have the wrong border.

test_that("label-auto-style visuals: bordered automatic labels", {
  skip_if_not_installed("ragg")

  p <- ggdag(
    perf_ten_node_dag(),
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ) +
    theme_dag()

  style <- label_box_style(p)
  stopifnot(
    "every box is drawn with a border" = all(style$border_visible),
    "every border is 0.25 mm wide" = isTRUE(all.equal(
      style$border_lwd,
      rep(0.25 * ggplot2::.pt, nrow(style))
    )),
    "every border is stroked in the text colour" = identical(
      style$border_colour,
      style$text_colour
    )
  )

  expect_doppelganger("label-auto-bordered-ten-nodes", p)
})

test_that("label-auto-style visuals: borderless automatic labels", {
  skip_if_not_installed("ragg")

  p <- ggdag(
    perf_ten_node_dag(),
    use_labels = TRUE,
    label_geom = geom_dag_label_auto2
  ) +
    theme_dag()

  style <- label_box_style(p)
  stopifnot(
    "no box is drawn with a border" = !any(style$border_visible),
    "every box is filled white" = all(style$fill == "#FFFFFFFF")
  )

  expect_doppelganger("label-auto-borderless-ten-nodes", p)
})
