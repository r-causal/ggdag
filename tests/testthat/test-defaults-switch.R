# The package lays a DAG out in time order and, when labels are asked for,
# draws them with `geom_dag_label_auto()`. Labels stay off by default, and
# every other default is what it was. The classic look, the `"nicely"` layout
# with repelled labels, stays one call to `ggdag_options_set()` away.
#
# Every test here unsets the ggdag options first, so that it exercises the
# defaults the package carries rather than any option a helper sets for the
# whole suite.

# Helpers ----------------------------------------------------------------------

# Unset every ggdag option for the calling test, restoring them when it exits.
local_unset_ggdag_options <- function(.env = parent.frame()) {
  unset <- stats::setNames(
    rep(list(NULL), length(ggdag_defaults)),
    paste0("ggdag.", names(ggdag_defaults))
  )
  withr::local_options(unset, .local_envir = .env)
}

# The name `fn` has in the ggdag namespace, so that a mismatch between two
# geoms reads as the geoms' names rather than as two function bodies.
geom_name <- function(fn) {
  ns <- asNamespace("ggdag")
  candidates <- ls(ns, pattern = "^geom_dag_")
  matches <- purrr::keep(candidates, \(name) identical(get(name, ns), fn))
  if (length(matches) == 0) {
    return("<not a ggdag geom>")
  }
  matches[[1]]
}

# One row per node of a tidy DAG or of the data a plot was built with: its name
# and coordinates, as a plain data frame ordered by name.
node_coordinates <- function(data) {
  coords <- data.frame(
    name = data$name,
    x = as.numeric(data$x),
    y = as.numeric(data$y)
  )
  coords <- dplyr::distinct(coords)
  coords <- coords[order(coords$name), ]
  rownames(coords) <- NULL
  coords
}

# The node coordinates of the data `plot` was built with.
built_node_coordinates <- function(plot) {
  node_coordinates(ggplot2::ggplot_build(plot)$plot$data)
}

# The node coordinates the time-ordered layout gives `dag`.
time_ordered_coordinates <- function(dag) {
  node_coordinates(pull_dag_data(tidy_dagitty(dag, layout = "time_ordered")))
}

# Whether every directed edge in `data` runs from left to right.
directed_edges_point_right <- function(data) {
  directed <- data[
    !is.na(data$to) & !is.na(data$direction) & data$direction == "->",
  ]
  nrow(directed) > 0 && all(directed$xend > directed$x)
}

# The layers among `layers` that draw the DAG's `label` column. The node text
# layer maps the node names instead, so the mapping singles out the label
# layer whichever geom draws it.
label_column_layers <- function(layers) {
  purrr::keep(layers, \(layer) {
    label <- layer$mapping$label
    !is.null(label) && identical(rlang::quo_get_expr(label), quote(label))
  })
}

# The single label layer among `layers` is the bordered automatic label geom.
expect_automatic_label_layer <- function(layers, what) {
  label_layers <- label_column_layers(layers)
  expect_length(label_layers, 1)
  layer <- label_layers[[1]]
  expect_identical(
    class(layer$geom)[[1]],
    "GeomDagLabelAuto",
    label = paste("the label geom of", what)
  )
  expect_identical(
    class(layer$stat)[[1]],
    "StatNodesLabelAuto",
    label = paste("the label stat of", what)
  )
  expect_identical(
    layer$geom_params$label.size,
    0.25,
    label = paste("the label border of", what)
  )
}

# A confounder triangle: z comes first, then x, then y.
triangle_dag <- function() {
  dagify(y ~ x + z, x ~ z, exposure = "x", outcome = "y")
}

labelled_triangle_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder")
  )
}

# Every `ggdag_option()` call in `expr` that reads `option`. Missing arguments,
# as in `x[, 1]`, are skipped, since there is nothing in them to look at.
ggdag_option_calls <- function(expr, option) {
  if (rlang::is_call(expr, "ggdag_option") && identical(expr[[2]], option)) {
    return(list(expr))
  }
  if (!is.call(expr) && !is.pairlist(expr)) {
    return(list())
  }
  parts <- as.list(expr)
  found <- list()
  for (i in seq_along(parts)) {
    if (identical(parts[[i]], quote(expr = ))) {
      next
    }
    found <- c(found, ggdag_option_calls(parts[[i]], option))
  }
  found
}

# Every function in the ggdag namespace, exported or not, whose formal `option`
# defaults to a `ggdag_option()` call, with that call and the environment the
# default is evaluated in.
option_formal_sites <- function(option) {
  ns <- asNamespace("ggdag")
  sites <- list()
  for (fn_name in sort(ls(ns, all.names = TRUE))) {
    fn <- get(fn_name, envir = ns)
    if (!is.function(fn) || is.primitive(fn)) {
      next
    }
    fn_formals <- formals(fn)
    if (!option %in% names(fn_formals)) {
      next
    }
    if (!rlang::is_call(fn_formals[[option]], "ggdag_option")) {
      next
    }
    sites[[fn_name]] <- list(
      call = fn_formals[[option]],
      env = environment(fn)
    )
  }
  sites
}

# Every function in the ggdag namespace whose body calls `ggdag_option()` for
# `option`, with those calls and the function's environment.
option_body_sites <- function(option) {
  ns <- asNamespace("ggdag")
  sites <- list()
  for (fn_name in sort(ls(ns, all.names = TRUE))) {
    fn <- get(fn_name, envir = ns)
    if (!is.function(fn) || is.primitive(fn)) {
      next
    }
    calls <- ggdag_option_calls(body(fn), option)
    if (length(calls) > 0) {
      sites[[fn_name]] <- list(calls = calls, env = environment(fn))
    }
  }
  sites
}

# Every function in the ggdag namespace, exported or not, whose formal `option`
# has a default, with that default.
defaulted_formals <- function(option) {
  ns <- asNamespace("ggdag")
  defaults <- list()
  for (fn_name in sort(ls(ns, all.names = TRUE))) {
    fn <- get(fn_name, envir = ns)
    if (!is.function(fn) || is.primitive(fn)) {
      next
    }
    fn_formals <- formals(fn)
    if (!option %in% names(fn_formals)) {
      next
    }
    if (identical(fn_formals[[option]], quote(expr = ))) {
      next
    }
    defaults[fn_name] <- list(fn_formals[[option]])
  }
  defaults
}

# A realistic epidemiology DAG: the effect of smoking on lung cancer.
epidemiology_dag <- function() {
  dagify(
    cancer ~ smoking + tar + age + ses + genetics + diet + occupation,
    tar ~ smoking,
    smoking ~ age + ses,
    diet ~ ses,
    occupation ~ ses,
    exposure = "smoking",
    outcome = "cancer",
    labels = c(
      age = "Age",
      ses = "Socioeconomic status",
      smoking = "Smoking",
      diet = "Diet",
      genetics = "Genetic risk",
      tar = "Tar in the lungs",
      occupation = "Occupation",
      cancer = "Lung cancer"
    )
  )
}

# The defaults table ------------------------------------------------------------

test_that("the defaults table switches the layout and label geom and nothing else", {
  expect_identical(ggdag_defaults$layout, "time_ordered")
  expect_identical(geom_name(ggdag_defaults$label_geom), "geom_dag_label_auto")
  expect_false(ggdag_defaults$use_labels)

  unchanged <- list(
    node_size = 16,
    text_size = 3.88,
    label_size = NULL,
    text_col = "white",
    label_col = "black",
    edge_width = 0.6,
    edge_cap = 8,
    arrow_length = 5,
    use_edges = TRUE,
    use_nodes = TRUE,
    use_stylized = FALSE,
    use_text = TRUE,
    use_labels = FALSE,
    edge_type = "link_arc",
    edge_engine = "ggraph",
    arrow_head = NULL,
    arrow_fins = NULL,
    arrow_mid = NULL,
    edge_route = "straight",
    edge_route_options = NULL,
    label_wrap = NULL,
    curvature = 0.3,
    debug_repel_points = FALSE
  )

  expect_true(all(names(unchanged) %in% names(ggdag_defaults)))
  for (name in names(unchanged)) {
    expect_identical(
      ggdag_defaults[[name]],
      unchanged[[name]],
      label = paste0("ggdag_defaults$", name)
    )
  }
})

# Fallbacks agree with the table ------------------------------------------------

test_that("every `layout` formal falls back to the time-ordered layout", {
  local_unset_ggdag_options()
  sites <- option_formal_sites("layout")
  expect_true(all(c("tidy_dagitty", "prep_dag_data") %in% names(sites)))

  for (fn_name in names(sites)) {
    site <- sites[[fn_name]]
    expect_identical(
      eval(site$call, envir = site$env),
      "time_ordered",
      label = paste0("the default of `layout` in ", fn_name, "()")
    )
  }
})

test_that("every `label_geom` formal falls back to the automatic label geom", {
  local_unset_ggdag_options()
  sites <- option_formal_sites("label_geom")
  expect_true(all(c("geom_dag", "ggdag", "ggdag_status") %in% names(sites)))

  for (fn_name in names(sites)) {
    site <- sites[[fn_name]]
    expect_identical(
      geom_name(eval(site$call, envir = site$env)),
      "geom_dag_label_auto",
      label = paste0("the default of `label_geom` in ", fn_name, "()")
    )
  }
})

test_that("every defaulted `layout` or `label_geom` formal reads its option unless it is listed here", {
  # formals whose default is deliberately not the option, each with the reason
  not_the_option <- list(
    layout = c(
      repel_edge_points = "the ggplot2 panel layout whose scales move the edges, not a DAG layout"
    ),
    label_geom = character()
  )

  for (option in names(not_the_option)) {
    defaults <- defaulted_formals(option)
    allowed <- as.character(names(not_the_option[[option]]))
    reads_option <- purrr::map_lgl(defaults, \(default) {
      rlang::is_call(default, "ggdag_option") &&
        identical(default[[2]], option)
    })

    expect_identical(
      setdiff(names(defaults)[!reads_option], allowed),
      character(),
      label = paste0(
        "the functions whose `",
        option,
        "` default neither reads the option nor is listed"
      )
    )
    # a listed function that now reads the option, or no longer exists, has
    # outgrown its entry
    expect_identical(
      setdiff(allowed, names(defaults)[!reads_option]),
      character(),
      label = paste0("the stale entries listed for `", option, "`")
    )
  }
})

test_that("every `ggdag_option()` call for the layout or label geom in a function body agrees with the table", {
  local_unset_ggdag_options()

  layout_sites <- option_body_sites("layout")
  expect_true("update_dag_data<-.tidy_dagitty" %in% names(layout_sites))
  for (fn_name in names(layout_sites)) {
    site <- layout_sites[[fn_name]]
    for (call in site$calls) {
      expect_identical(
        eval(call, envir = site$env),
        "time_ordered",
        label = paste0("the `layout` fallback in the body of ", fn_name, "()")
      )
    }
  }

  label_geom_sites <- option_body_sites("label_geom")
  for (fn_name in names(label_geom_sites)) {
    site <- label_geom_sites[[fn_name]]
    for (call in site$calls) {
      expect_identical(
        geom_name(eval(call, envir = site$env)),
        "geom_dag_label_auto",
        label = paste0(
          "the `label_geom` fallback in the body of ",
          fn_name,
          "()"
        )
      )
    }
  }
})

test_that("update_dag_data<- rebuilds dropped coordinates in time order", {
  local_unset_ggdag_options()
  dag <- triangle_dag()
  tidy_dag <- tidy_dagitty(dag)

  update_dag_data(tidy_dag) <- dplyr::select(
    pull_dag_data(tidy_dag),
    "name",
    "to",
    "direction"
  )

  # the setter keeps the layout's coordinates at full precision, where
  # `tidy_dagitty()` hands them back to three decimal places
  expect_equal(
    node_coordinates(pull_dag_data(tidy_dag)),
    time_ordered_coordinates(dag),
    tolerance = 1e-3
  )
  expect_true(directed_edges_point_right(pull_dag_data(tidy_dag)))
})

# Behaviour with nothing set ----------------------------------------------------

test_that("tidy_dagitty() lays a DAG out in time order with nothing set", {
  local_unset_ggdag_options()
  dag <- triangle_dag()
  tidy_dag <- tidy_dagitty(dag)

  expect_equal(
    node_coordinates(pull_dag_data(tidy_dag)),
    time_ordered_coordinates(dag)
  )
  expect_true(directed_edges_point_right(pull_dag_data(tidy_dag)))
})

test_that("ggdag() lays out in time order and draws no labels with nothing set", {
  local_unset_ggdag_options()
  dag <- labelled_triangle_dag()
  p <- ggdag(dag)

  expect_equal(built_node_coordinates(p), time_ordered_coordinates(dag))
  expect_true(directed_edges_point_right(ggplot2::ggplot_build(p)$plot$data))
  expect_length(label_column_layers(p$layers), 0)
  expect_equal(count_geom_layers(p, "GeomDagLabelAuto"), 0)
  expect_equal(count_geom_layers(p, "GeomLabelRepel"), 0)
})

test_that("the quick plotters lay out in time order with nothing set", {
  local_unset_ggdag_options()
  dag <- triangle_dag()
  expected <- time_ordered_coordinates(dag)

  plots <- list(
    ggdag_status = ggdag_status(dag),
    ggdag_adjustment_set = ggdag_adjustment_set(dag),
    ggdag_paths = ggdag_paths(dag),
    ggdag_equivalent_dags = ggdag_equivalent_dags(dag),
    ggdag_parents = ggdag_parents(dag, "y"),
    ggdag_dseparated = ggdag_dseparated(dag, controlling_for = "z"),
    ggdag_collider = ggdag_collider(dag),
    ggdag_exogenous = ggdag_exogenous(dag),
    ggdag_instrumental = ggdag_instrumental(dag)
  )

  for (plotter in names(plots)) {
    expect_equal(
      built_node_coordinates(plots[[plotter]]),
      expected,
      label = paste0("the node coordinates of ", plotter, "()")
    )
  }
})

test_that("use_labels = TRUE draws bordered automatic labels with nothing set", {
  local_unset_ggdag_options()
  dag <- labelled_triangle_dag()

  expect_automatic_label_layer(
    ggdag(dag, use_labels = TRUE)$layers,
    "ggdag()"
  )
  expect_automatic_label_layer(
    geom_dag(use_labels = TRUE),
    "geom_dag()"
  )
  expect_automatic_label_layer(
    ggdag_status(dag, use_labels = TRUE)$layers,
    "ggdag_status()"
  )
  expect_automatic_label_layer(
    ggdag_adjustment_set(dag, use_labels = TRUE)$layers,
    "ggdag_adjustment_set()"
  )
})

# The classic recipe ------------------------------------------------------------

test_that("the classic options bring back the layout and labels the defaults replaced", {
  withr::local_preserve_seed()
  local_unset_ggdag_options()
  dag <- labelled_triangle_dag()

  # with nothing set, the defaults are the new ones
  expect_automatic_label_layer(
    ggdag(dag, use_labels = TRUE)$layers,
    "ggdag() with nothing set"
  )
  expect_equal(
    node_coordinates(pull_dag_data(tidy_dagitty(dag))),
    time_ordered_coordinates(dag)
  )

  old <- ggdag_options_set(layout = "nicely", label_geom = geom_dag_label_repel)
  withr::defer(do.call(ggdag_options_set, old))

  label_layers <- label_column_layers(ggdag(dag, use_labels = TRUE)$layers)
  expect_length(label_layers, 1)
  expect_s3_class(label_layers[[1]]$geom, "GeomLabelRepel")

  expect_equal(
    node_coordinates(pull_dag_data(tidy_dagitty(dag, seed = 1234))),
    node_coordinates(
      pull_dag_data(tidy_dagitty(dag, seed = 1234, layout = "nicely"))
    )
  )
})

test_that("the layout option reaches dag_saturate() and as_tidy_dagitty.list()", {
  withr::local_preserve_seed()
  local_unset_ggdag_options()
  dag <- epidemiology_dag()
  time_points <- list(c("age", "ses"), c("smoking", "diet"), "tar", "cancer")

  time_ordered_saturated <- node_coordinates(
    pull_dag_data(dag_saturate(dag, layout = "time_ordered"))
  )
  time_ordered_list <- node_coordinates(
    pull_dag_data(as_tidy_dagitty(time_points, layout = "time_ordered"))
  )

  old <- ggdag_options_set(layout = "nicely")
  withr::defer(do.call(ggdag_options_set, old))

  saturated <- node_coordinates(pull_dag_data(dag_saturate(dag, seed = 1234)))
  expect_equal(
    saturated,
    node_coordinates(
      pull_dag_data(dag_saturate(dag, seed = 1234, layout = "nicely"))
    )
  )
  # the option has to move the nodes, or the comparison above proves nothing
  expect_false(isTRUE(all.equal(saturated, time_ordered_saturated)))

  from_list <- node_coordinates(
    pull_dag_data(as_tidy_dagitty(time_points, seed = 1234))
  )
  expect_equal(
    from_list,
    node_coordinates(
      pull_dag_data(as_tidy_dagitty(
        time_points,
        seed = 1234,
        layout = "nicely"
      ))
    )
  )
  expect_false(isTRUE(all.equal(from_list, time_ordered_list)))
})

# Awkward graphs under the defaults ---------------------------------------------

# Draw `plot` on an off-screen device `size` inches wide and high, and return the
# warnings and messages that building and drawing it raised. `plot` is a
# promise, so a warning raised while the plot is constructed is collected as
# well.
drawing_conditions <- function(plot, size = c(10, 8)) {
  conditions <- list()
  file <- withr::local_tempfile(fileext = ".png")
  withCallingHandlers(
    {
      ragg::agg_png(
        file,
        width = size[[1]],
        height = size[[2]],
        units = "in",
        res = 72
      )
      withr::defer(grDevices::dev.off())
      print(plot)
    },
    warning = function(cnd) {
      conditions <<- c(conditions, list(cnd))
      invokeRestart("muffleWarning")
    },
    message = function(cnd) {
      conditions <<- c(conditions, list(cnd))
      invokeRestart("muffleMessage")
    }
  )
  conditions
}

# `plot` builds and draws, raising no warning or message except those of the
# classes in `expected`, each of which it raises at least once.
expect_draws_with_only <- function(
  plot,
  expected = character(),
  size = c(10, 8)
) {
  skip_if_not_installed("ragg")
  conditions <- drawing_conditions(plot, size)
  is_expected <- purrr::map_lgl(conditions, \(cnd) inherits(cnd, expected))
  expect_identical(
    purrr::map_chr(conditions[!is_expected], conditionMessage),
    character()
  )
  for (class in expected) {
    expect_true(
      any(purrr::map_lgl(conditions, \(cnd) inherits(cnd, class))),
      label = paste("a condition of class", class)
    )
  }
}

# `dag` with a label for every node.
with_node_labels <- function(dag) {
  nodes <- names(dag)
  label(dag) <- stats::setNames(paste("Variable", toupper(nodes)), nodes)
  dag
}

# A large DAG written out in full, so that every run draws the same graph:
# thirty nodes and 51 directed edges in six time periods, from four sources
# (x1 to x4) to four sinks (x27 to x30), with some edges skipping a period.
thirty_node_dag <- function() {
  dagitty::dagitty(
    "dag {
      x1 -> { x5 x6 x12 }
      x2 -> { x6 x7 x16 }
      x3 -> { x7 x8 x13 }
      x4 -> { x8 x9 }
      x5 -> { x10 x11 }
      x6 -> { x11 x12 x19 }
      x7 -> { x13 x14 }
      x8 -> { x14 x15 }
      x9 -> { x15 x21 }
      x10 -> { x16 x17 }
      x11 -> { x17 x18 }
      x12 -> { x18 x19 }
      x13 -> { x19 x20 }
      x14 -> { x20 x21 }
      x15 -> { x21 x25 }
      x16 -> x22
      x17 -> { x22 x23 }
      x18 -> { x23 x24 }
      x19 -> x24
      x20 -> { x25 x26 }
      x21 -> x26
      x22 -> x27
      x23 -> { x27 x28 }
      x24 -> { x28 x29 }
      x25 -> { x29 x30 }
      x26 -> x30
    }"
  )
}

test_that("a two-node cycle draws under the defaults", {
  local_unset_ggdag_options()
  dag <- dagitty::dagitty("dag { x -> y; y -> x }")

  expect_draws_with_only(ggdag(dag), "ggdag_cyclic_warning")
  expect_draws_with_only(
    ggdag(with_node_labels(dag), use_labels = TRUE),
    "ggdag_cyclic_warning"
  )
})

test_that("a three-node cycle draws under the defaults", {
  local_unset_ggdag_options()
  dag <- dagitty::dagitty("dag { x -> y; y -> z; z -> x }")

  expect_draws_with_only(ggdag(dag), "ggdag_cyclic_warning")
  expect_draws_with_only(
    ggdag(with_node_labels(dag), use_labels = TRUE),
    "ggdag_cyclic_warning"
  )
})

test_that("a self-loop draws under the defaults", {
  local_unset_ggdag_options()
  dag <- dagitty::dagitty("dag { x -> x; x -> y }")

  expect_draws_with_only(ggdag(dag), "ggdag_cyclic_warning")
  expect_draws_with_only(
    ggdag(with_node_labels(dag), use_labels = TRUE),
    "ggdag_cyclic_warning"
  )
})

test_that("a disconnected graph draws under the defaults", {
  local_unset_ggdag_options()
  dag <- dagify(y ~ x, b ~ a)

  expect_draws_with_only(ggdag(dag))
  expect_draws_with_only(ggdag(with_node_labels(dag), use_labels = TRUE))
})

test_that("a graph with an isolated node draws under the defaults", {
  local_unset_ggdag_options()
  dag <- dagitty::dagitty("dag { x -> y; z }")

  expect_draws_with_only(ggdag(dag))
  expect_draws_with_only(ggdag(with_node_labels(dag), use_labels = TRUE))
})

test_that("a single node draws under the defaults", {
  local_unset_ggdag_options()
  dag <- dagitty::dagitty("dag { x }")

  expect_draws_with_only(ggdag(dag))
  expect_draws_with_only(ggdag(with_node_labels(dag), use_labels = TRUE))
})

test_that("a graph with only bidirected edges draws under the defaults", {
  local_unset_ggdag_options()
  dag <- dagify(x ~ ~y, y ~ ~z)

  expect_draws_with_only(ggdag(dag))
  expect_draws_with_only(ggdag(with_node_labels(dag), use_labels = TRUE))
})

test_that("a DAG with its own coordinates keeps them under the defaults", {
  local_unset_ggdag_options()
  given <- data.frame(
    name = c("x", "y", "z"),
    x = c(0, 2, 1),
    y = c(0, 0, 1)
  )
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    coords = list(
      x = stats::setNames(given$x, given$name),
      y = stats::setNames(given$y, given$name)
    )
  )

  p <- ggdag(dag)
  expect_equal(built_node_coordinates(p), given)
  expect_draws_with_only(p)

  p_labelled <- ggdag(with_node_labels(dag), use_labels = TRUE)
  expect_equal(built_node_coordinates(p_labelled), given)
  expect_draws_with_only(p_labelled)
})

test_that("a thirty-node DAG draws under the defaults", {
  local_unset_ggdag_options()
  dag <- thirty_node_dag()

  # thirty labels need a large device to all find room clear of the drawing
  expect_draws_with_only(ggdag(dag), size = c(16, 12))
  expect_draws_with_only(
    ggdag(with_node_labels(dag), use_labels = TRUE),
    size = c(16, 12)
  )
})

# The default pictures ----------------------------------------------------------

test_that("ggdag() draws an epidemiology DAG in time order with nothing set", {
  local_unset_ggdag_options()
  p <- ggdag(epidemiology_dag())

  built <- ggplot2::ggplot_build(p)
  stopifnot(
    "every directed edge points left to right" = directed_edges_point_right(
      built$plot$data
    )
  )

  expect_doppelganger("default ggdag of an epidemiology DAG", p)
})

test_that("ggdag(use_labels = TRUE) draws automatic labels on an epidemiology DAG with nothing set", {
  local_unset_ggdag_options()
  p <- ggdag(epidemiology_dag(), use_labels = TRUE)

  built <- ggplot2::ggplot_build(p)
  label_layers <- label_column_layers(p$layers)
  stopifnot(
    "every directed edge points left to right" = directed_edges_point_right(
      built$plot$data
    ),
    "the label layer is drawn by GeomDagLabelAuto" = length(label_layers) ==
      1 &&
      inherits(label_layers[[1]]$geom, "GeomDagLabelAuto")
  )

  expect_doppelganger("default labelled ggdag of an epidemiology DAG", p)
})
