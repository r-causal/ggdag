# Tests for ggarrow engine support in ggdag_* quick-plot functions

coords_confounder <- tibble::tribble(
  ~name,
  ~x,
  ~y,
  "x",
  0,
  0,
  "y",
  2,
  0,
  "z",
  1,
  1
)

confounder_dag <- dagify(
  x ~ z,
  y ~ x + z,
  exposure = "x",
  outcome = "y",
  coords = coords_confounder
)

# ggdag_paths() with ggarrow engine ----------------------------------------

test_that("ggdag_paths() works with ggarrow engine", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  p <- ggdag_paths(confounder_dag, from = "x", to = "y")
  expect_s3_class(p, "ggplot")
  expect_doppelganger("ggdag_paths ggarrow engine", p)
})

test_that("ggdag_paths() with ggarrow engine and shadow = FALSE", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  p <- ggdag_paths(confounder_dag, from = "x", to = "y", shadow = FALSE)
  expect_s3_class(p, "ggplot")
  expect_doppelganger("ggdag_paths ggarrow no shadow", p)
})

test_that("ggdag_paths() with ggarrow supports edge_engine parameter", {
  skip_if_not_installed("ggarrow")

  # Explicit parameter should work even without global option

  p <- ggdag_paths(
    confounder_dag,
    from = "x",
    to = "y",
    edge_engine = "ggarrow"
  )
  expect_s3_class(p, "ggplot")
})

# ggdag_adjustment_set() with ggarrow engine --------------------------------

test_that("ggdag_adjustment_set() works with ggarrow engine", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  p <- ggdag_adjustment_set(confounder_dag)
  expect_s3_class(p, "ggplot")
  expect_doppelganger("ggdag_adjustment_set ggarrow engine", p)
})

test_that("ggdag_adjustment_set() with ggarrow engine and shadow = FALSE", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  p <- ggdag_adjustment_set(confounder_dag, shadow = FALSE)
  expect_s3_class(p, "ggplot")
  expect_doppelganger("ggdag_adjustment_set ggarrow no shadow", p)
})

test_that("ggdag_adjustment_set() with ggarrow supports edge_engine parameter", {
  skip_if_not_installed("ggarrow")

  p <- ggdag_adjustment_set(confounder_dag, edge_engine = "ggarrow")
  expect_s3_class(p, "ggplot")
})

# ggdag_equivalent_class() with ggarrow engine ------------------------------

test_that("ggdag_equivalent_class() works with ggarrow engine", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggdag_equivalent_class(dag)
  expect_s3_class(p, "ggplot")
  expect_doppelganger("ggdag_equivalent_class ggarrow engine", p)
})

test_that("ggdag_equivalent_class() with ggarrow supports edge_engine parameter", {
  skip_if_not_installed("ggarrow")

  dag <- dagify(y ~ x + z, x ~ z)
  p <- ggdag_equivalent_class(dag, edge_engine = "ggarrow")
  expect_s3_class(p, "ggplot")
})

# Legend key glyphs with ggarrow engine ------------------------------------

test_that("draw_key_dag_combined uses ggarrow rendering when engine is ggarrow", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  data <- list(
    colour = "blue",
    fill = "blue",
    alpha = 1,
    shape = 19,
    size = 16
  )
  grob <- draw_key_dag_combined(data, list(), 1)
  expect_s3_class(grob, "gTree")

  # Extract classes of all children in the grobTree
  child_classes <- vapply(
    grob$children,
    function(g) class(g)[1],
    character(1)
  )
  # When ggarrow is active, the arrow portion should be an arrow_path grob
  expect_true(
    any(grepl("arrow_path", child_classes)),
    info = paste(
      "Expected arrow_path grob but found:",
      paste(child_classes, collapse = ", ")
    )
  )
})

test_that("draw_key_dag_collider uses ggarrow rendering when engine is ggarrow", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  data <- list(
    colour = "red",
    fill = "red",
    alpha = 1,
    shape = 19,
    size = 16
  )
  grob <- draw_key_dag_collider(data, list(), 1)
  expect_s3_class(grob, "gTree")

  child_classes <- vapply(
    grob$children,
    function(g) class(g)[1],
    character(1)
  )
  expect_true(
    any(grepl("arrow_path", child_classes)),
    info = paste(
      "Expected arrow_path grob but found:",
      paste(child_classes, collapse = ", ")
    )
  )
})

test_that("draw_key_dag_edge uses ggarrow rendering when engine is ggarrow", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  data <- list(
    colour = "black",
    edge_colour = NULL,
    alpha = 1,
    edge_alpha = NULL,
    edge_width = 0.6,
    edge_linetype = 1,
    linewidth = 0.5,
    linetype = 1
  )
  grob <- draw_key_dag_edge(data, list(), 1)

  # draw_key_dag_edge returns the grob directly (not wrapped in grobTree)
  expect_true(
    any(grepl("arrow_path", class(grob))),
    info = paste(
      "Expected arrow_path grob but found:",
      paste(class(grob), collapse = ", ")
    )
  )
})

test_that("draw_key_dag_combined uses grid::arrow when engine is ggraph", {
  withr::local_options(ggdag.edge_engine = "ggraph")

  data <- list(
    colour = "blue",
    fill = "blue",
    alpha = 1,
    shape = 19,
    size = 16
  )
  grob <- draw_key_dag_combined(data, list(), 1)
  expect_s3_class(grob, "gTree")

  child_classes <- vapply(
    grob$children,
    function(g) class(g)[1],
    character(1)
  )
  # Should contain a segments grob, not an arrow_path
  expect_true(
    any(grepl("segments", child_classes)),
    info = paste(
      "Expected segments grob but found:",
      paste(child_classes, collapse = ", ")
    )
  )
  expect_false(any(grepl("arrow_path", child_classes)))
})

# Per-edge curvature in the ggarrow quick plots ------------------------------

curved_confounder_dag <- function() {
  curve_edge(confounder_dag, from = "x", to = "y", curvature = 0.6)
}

# the curvature each drawn edge ends up with, keyed by the edge it belongs to;
# an edge layer that never sees the column contributes nothing
arrow_edge_curvature <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  wanted <- c("x", "y", "xend", "yend", "edge_curvature")
  rows <- list()
  for (i in seq_along(plot$layers)) {
    if (!grepl("^GeomDAGArrow", class(plot$layers[[i]]$geom)[1])) {
      next
    }
    d <- built$data[[i]]
    if (nrow(d) == 0 || !all(wanted %in% names(d))) {
      next
    }
    rows[[length(rows) + 1]] <- d[, wanted]
  }
  if (length(rows) == 0) {
    empty <- as.data.frame(stats::setNames(
      rep(list(numeric()), length(wanted)),
      wanted
    ))
    return(empty)
  }
  unique(do.call(rbind, rows))
}

test_that("ggdag_paths() draws per-edge curvature with the ggarrow engine", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  p <- ggdag_paths(curved_confounder_dag(), from = "x", to = "y")
  drawn <- arrow_edge_curvature(p)

  expect_gt(nrow(drawn), 0)
  # the x -> y edge was curved; the two z edges were not
  x_to_y <- drawn[drawn$x == 0 & drawn$xend == 2, ]
  expect_equal(unique(x_to_y$edge_curvature), 0.6)
  expect_setequal(unique(drawn$edge_curvature), c(0, 0.6))
})

test_that("ggdag_adjustment_set() draws per-edge curvature with the ggarrow engine", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  p <- ggdag_adjustment_set(curved_confounder_dag())
  drawn <- arrow_edge_curvature(p)

  expect_gt(nrow(drawn), 0)
  x_to_y <- drawn[drawn$x == 0 & drawn$xend == 2, ]
  expect_equal(unique(x_to_y$edge_curvature), 0.6)
})

test_that("ggdag_paths() ggarrow curved edge snapshot", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggarrow")

  p <- ggdag_paths(curved_confounder_dag(), from = "x", to = "y")
  expect_doppelganger("ggdag_paths ggarrow curved edge", p)
})

# The remaining ggdag_* plotters that build their own edges -------------------

test_that("ggdag_adjust() draws ggarrow edges when the engine asks for them", {
  skip_if_not_installed("ggarrow")

  arrow_layers <- function(p) {
    sum(vapply(
      p$layers,
      function(l) grepl("^GeomDAGArrow", class(l$geom)[1]),
      logical(1)
    ))
  }

  withr::local_options(ggdag.edge_engine = "ggarrow")
  expect_gt(arrow_layers(ggdag_adjust(confounder_dag, "z")), 0)
})

test_that("ggdag_adjust() takes edge_engine directly", {
  skip_if_not_installed("ggarrow")

  p <- ggdag_adjust(confounder_dag, "z", edge_engine = "ggarrow")
  expect_s3_class(p, "ggplot")
  expect_gt(
    sum(vapply(
      p$layers,
      function(l) grepl("^GeomDAGArrow", class(l$geom)[1]),
      logical(1)
    )),
    0
  )
})

test_that("ggdag_equivalent_dags() takes edge_engine directly", {
  skip_if_not_installed("ggarrow")

  p <- ggdag_equivalent_dags(
    dagify(y ~ x + z, x ~ z),
    edge_engine = "ggarrow"
  )
  expect_s3_class(p, "ggplot")
  expect_gt(
    sum(vapply(
      p$layers,
      function(l) grepl("^GeomDAGArrow", class(l$geom)[1]),
      logical(1)
    )),
    0
  )
})

# Legend glyphs follow the engine the plot was asked for ----------------------

guide_grob_classes <- function(plot) {
  gtable <- ggplot2::ggplotGrob(plot)
  classes <- character()
  collect <- function(grob) {
    classes <<- c(classes, class(grob)[1])
    if (!is.null(grob$children)) {
      invisible(lapply(grob$children, collect))
    }
    if (!is.null(grob$grobs)) {
      invisible(lapply(grob$grobs, collect))
    }
  }
  for (i in grep("guide-box", gtable$layout$name)) {
    collect(gtable$grobs[[i]])
  }
  unique(classes)
}

test_that("legend glyphs follow an edge_engine given as an argument", {
  skip_if_not_installed("ggarrow")
  withr::local_options(ggdag.edge_engine = "ggraph")

  # the plot draws ggarrow edges because the argument says so, so its key
  # has to draw a ggarrow ornament rather than a grid segment
  p <- ggdag_paths(
    confounder_dag,
    from = "x",
    to = "y",
    edge_engine = "ggarrow"
  )
  classes <- guide_grob_classes(p)

  expect_true(any(grepl("arrow_path", classes)))
  expect_false(any(grepl("^segments", classes)))
})
