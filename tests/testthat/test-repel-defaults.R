# Tests for the recalibrated repel label defaults. The repel family places
# labels adjacent to their nodes with deterministic positions:
#
# * geom_dag_text_repel() and geom_dag_label_repel() default to box.padding
#   0.5, point.padding 0.5, min.segment.length 1, force_pull 2, max.time 1,
#   max.iter 10000, and seed 1234, so the deterministic iteration cap rather
#   than the wall clock bounds the simulation and renders reproduce across
#   sessions. geom_dag_label_repel() keeps its 0.25-line label.padding.
# * The *2 variants keep their more-spaced identity with box.padding 0.75.
# * geom_dag(use_labels = TRUE) threads box.padding 0.5 to node-aware label
#   geoms and no longer overrides label.padding, so the constructor default
#   survives.
# * Rendering the default repel labels is reproducible and leaves the
#   session RNG state untouched.
#
# Everything here is snapshot-free.

# A confounder triangle with a label on every node and fixed coordinates.
confounder_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder"),
    coords = list(x = c(x = 0, z = 1, y = 2), y = c(x = 0, z = 1, y = 0))
  )
}

# The value of a padding default in lines. ggrepel reads a bare numeric as
# lines, so a plain number and a lines unit pin the same contract.
padding_lines <- function(padding) {
  value <- eval(padding)
  if (grid::is.unit(value)) {
    stopifnot(identical(grid::unitType(value), "lines"))
    as.numeric(value)
  } else {
    value
  }
}

# Node centers and repel label boxes of `plot`, in inches on a fixed-size
# svg device (the same device the vdiffr baselines render on). Everything
# is converted inside the panel viewport, so node and label geometry share
# one coordinate system. Labels are sorted so grob order cannot decide a
# comparison.
rendered_repel_geometry <- function(plot) {
  file <- tempfile(fileext = ".svg")
  svg_device <- utils::getFromNamespace("svglite", "vdiffr")
  svg_device(file, width = 10, height = 8)
  on.exit(
    {
      grDevices::dev.off()
      unlink(file)
    },
    add = TRUE
  )

  print(plot)
  grid::grid.force()

  listing <- grid::grid.ls(print = FALSE, viewports = TRUE, fullNames = TRUE)
  grob_rows <- listing$type != "vpListing"
  full_names <- listing$name[grob_rows]
  vp_paths <- listing$vpPath[grob_rows]

  # The node-name text layer lives in the panel viewport; navigate there so
  # native units convert against the panel scales.
  node_text_index <- grep("^text\\[GRID\\.text", full_names)
  node_text_index <- node_text_index[grepl(
    "panel",
    vp_paths[node_text_index]
  )][1]
  path <- sub("^viewport\\[ROOT\\]::", "", vp_paths[node_text_index])
  parts <- gsub("^viewport\\[|\\]$", "", strsplit(path, "::")[[1]])
  grid::upViewport(0)
  grid::downViewport(do.call(grid::vpPath, as.list(parts)))

  in_x <- function(u) as.numeric(grid::convertX(u, "in"))
  in_y <- function(u) as.numeric(grid::convertY(u, "in"))

  node_text <- grid::grid.get(gsub(
    "^text\\[|\\]$",
    "",
    full_names[node_text_index]
  ))
  nodes <- data.frame(
    name = as.character(node_text$label),
    x = in_x(node_text$x),
    y = in_y(node_text$y),
    stringsAsFactors = FALSE
  )
  nodes <- nodes[order(nodes$name), , drop = FALSE]
  rownames(nodes) <- NULL

  # Each repel label draws a rounded rect inside a viewport that carries the
  # box center and extent, paired with a text grob of the same index.
  rect_names <- gsub(
    "^forcedgrob\\[|\\]$",
    "",
    grep("rectrepelgrob", full_names, value = TRUE)
  )
  labels <- do.call(
    rbind,
    lapply(rect_names, function(name) {
      rect_grob <- grid::grid.get(name)
      text_grob <- grid::grid.get(sub("rect", "text", name))
      data.frame(
        label = as.character(text_grob$label),
        x = in_x(rect_grob$vp$x),
        y = in_y(rect_grob$vp$y),
        width = as.numeric(grid::convertWidth(rect_grob$vp$width, "in")),
        height = as.numeric(grid::convertHeight(rect_grob$vp$height, "in")),
        stringsAsFactors = FALSE
      )
    })
  )
  labels <- labels[order(labels$label), , drop = FALSE]
  rownames(labels) <- NULL

  list(nodes = nodes, labels = labels)
}

# A repel plot built entirely from defaults through geom_dag().
default_repel_plot <- function() {
  ggplot(confounder_dag(), aes_dag()) +
    geom_dag(use_labels = TRUE) +
    theme_dag()
}

# Defaults pins ----------------------------------------------------------------

test_that("geom_dag_text_repel() defaults are recalibrated", {
  defaults <- formals(geom_dag_text_repel)

  expect_equal(padding_lines(defaults$box.padding), 0.5)
  expect_equal(padding_lines(defaults$point.padding), 0.5)
  expect_equal(eval(defaults$min.segment.length), 1)
  expect_equal(eval(defaults$force_pull), 2)
  expect_equal(eval(defaults$max.time), 1)
  expect_equal(eval(defaults$max.iter), 10000)
  expect_equal(eval(defaults$seed), 1234)
})

test_that("geom_dag_label_repel() defaults are recalibrated", {
  defaults <- formals(geom_dag_label_repel)

  expect_equal(padding_lines(defaults$box.padding), 0.5)
  expect_equal(padding_lines(defaults$point.padding), 0.5)
  expect_equal(eval(defaults$min.segment.length), 1)
  expect_equal(eval(defaults$force_pull), 2)
  expect_equal(eval(defaults$max.time), 1)
  expect_equal(eval(defaults$max.iter), 10000)
  expect_equal(eval(defaults$seed), 1234)

  # The constructor's own label padding is the correct default and stays.
  expect_equal(padding_lines(defaults$label.padding), 0.25)
})

test_that("the *2 variants keep a more-spaced box padding of 0.75", {
  expect_equal(padding_lines(formals(geom_dag_text_repel2)$box.padding), 0.75)
  expect_equal(padding_lines(formals(geom_dag_label_repel2)$box.padding), 0.75)
})

# Threading through geom_dag() -------------------------------------------------

test_that("geom_dag() threads box.padding 0.5 and no label.padding override", {
  recorded <- new.env(parent = emptyenv())
  recorder <- function(...) {
    recorded$args <- rlang::list2(...)
    NULL
  }
  aware <- dag_node_aware(recorder, extra = "label.padding")

  geom_dag(use_labels = TRUE, label_geom = aware)

  expect_equal(recorded$args$box.padding, 0.5)
  expect_false("label.padding" %in% names(recorded$args))
})

test_that("the built repel label layer keeps the constructor label padding", {
  layers <- geom_dag(use_labels = TRUE, label_geom = geom_dag_label_repel)
  label_item <- layers[[4]]
  expect_s3_class(label_item, "dag_layer")

  params <- c(label_item$stat_params, label_item$geom_params)
  expect_equal(padding_lines(params[["box.padding"]]), 0.5)
  expect_equal(padding_lines(params[["label.padding"]]), 0.25)
})

# Determinism ------------------------------------------------------------------

test_that("rendering the default repel labels twice places them identically", {
  skip_if_not_installed("vdiffr")

  p <- default_repel_plot()

  withr::local_seed(4321)

  first <- rendered_repel_geometry(p)
  second <- rendered_repel_geometry(p)

  expect_gt(nrow(first$labels), 0)
  expect_equal(first, second)
})

test_that("rendering the default repel labels leaves the session RNG alone", {
  skip_if_not_installed("vdiffr")

  p <- default_repel_plot()

  withr::local_seed(4321)
  seed_before <- get(".Random.seed", envir = globalenv())

  invisible(rendered_repel_geometry(p))

  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)
})

# Quality tripwires ------------------------------------------------------------

test_that("default repel labels stay adjacent to their nodes", {
  skip_if_not_installed("vdiffr")

  withr::local_seed(1234)
  geometry <- rendered_repel_geometry(default_repel_plot())

  label_node <- c(Exposure = "x", Outcome = "y", Confounder = "z")
  expect_setequal(geometry$labels$label, names(label_node))

  # A coarse tripwire, not an aesthetics pin: on the 10 x 8 inch device every
  # label box center sits within 0.8 inches of its node center. The
  # recalibrated defaults place them closer; runaway repulsion pushes at
  # least one label past this bound.
  for (i in seq_len(nrow(geometry$labels))) {
    label <- geometry$labels[i, ]
    node <- geometry$nodes[
      geometry$nodes$name == label_node[[label$label]],
    ]
    distance <- sqrt((label$x - node$x)^2 + (label$y - node$y)^2)
    expect_lt(
      distance,
      0.8,
      label = sprintf(
        "distance from label '%s' to node '%s' (%.3f in)",
        label$label,
        node$name,
        distance
      )
    )
  }
})

test_that("no default repel label box covers another node's center", {
  skip_if_not_installed("vdiffr")

  withr::local_seed(1234)
  geometry <- rendered_repel_geometry(default_repel_plot())

  for (i in seq_len(nrow(geometry$labels))) {
    label <- geometry$labels[i, ]
    covered <- abs(geometry$nodes$x - label$x) < label$width / 2 &
      abs(geometry$nodes$y - label$y) < label$height / 2
    expect_false(
      any(covered),
      label = sprintf(
        "label '%s' covers a node center",
        label$label
      )
    )
  }
})
