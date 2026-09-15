# `label_wrap` travels with `use_labels` and `label_geom`: a quick plot that
# can draw labels can say how wide those labels are allowed to be. The block
# below walks every exported quick plot that takes `use_labels` rather than a
# hand-written list, so a new one is covered the day it is exported.

# Exported functions whose formals include `use_labels`, other than `ggdag()`
# itself, which already carries `label_wrap`.
label_wrap_quick_plots <- function() {
  exported <- getNamespaceExports(asNamespace("ggdag"))
  candidates <- sort(exported[grepl("^ggdag", exported)])
  takes_labels <- vapply(
    candidates,
    function(name) {
      object <- get(name, envir = asNamespace("ggdag"))
      is.function(object) && "use_labels" %in% names(formals(object))
    },
    logical(1)
  )
  setdiff(candidates[takes_labels], "ggdag")
}

# The arguments each quick plot needs beyond the DAG. A function that builds
# its own DAG from node names is called with no DAG at all, and with the names
# it labels that DAG with: without them it draws no labels, and so no label
# layer for `label_wrap` to reach.
label_wrap_extra_args <- list(
  ggdag_adjacent = list(.var = "x"),
  ggdag_adjust = list(var = "z"),
  ggdag_ancestors = list(.var = "y"),
  ggdag_children = list(.var = "z"),
  ggdag_dconnected = list(from = "x", to = "y"),
  ggdag_descendants = list(.var = "z"),
  ggdag_drelationship = list(from = "x", to = "y"),
  ggdag_dseparated = list(from = "x", to = "y"),
  ggdag_markov_blanket = list(.var = "x"),
  ggdag_parents = list(.var = "y"),
  ggdag_paths = list(from = "x", to = "y"),
  ggdag_paths_fan = list(from = "x", to = "y"),
  ggdag_butterfly_bias = list(x = "X", y = "Y", m = "M"),
  ggdag_collider_triangle = list(x = "X", y = "Y", m = "M"),
  ggdag_confounder_triangle = list(x = "X", y = "Y", z = "Z"),
  ggdag_m_bias = list(x = "X", y = "Y", m = "M"),
  ggdag_mediation_triangle = list(x = "X", y = "Y", m = "M"),
  ggdag_quartet_collider = list(x = "X", y = "Y", z = "Z"),
  ggdag_quartet_confounder = list(x = "X", y = "Y", z = "Z"),
  ggdag_quartet_m_bias = list(x = "X", y = "Y", z = "Z"),
  ggdag_quartet_mediator = list(x = "X", y = "Y", z = "Z"),
  ggdag_quartet_time_collider = list(x2 = "X2", y3 = "Y3")
)

label_wrap_dagless <- c(
  "ggdag_butterfly_bias",
  "ggdag_collider_triangle",
  "ggdag_confounder_triangle",
  "ggdag_m_bias",
  "ggdag_mediation_triangle",
  "ggdag_quartet_collider",
  "ggdag_quartet_confounder",
  "ggdag_quartet_m_bias",
  "ggdag_quartet_mediator",
  "ggdag_quartet_time_collider"
)

label_wrap_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z + i,
    exposure = "x",
    outcome = "y",
    labels = c(
      x = "Physical activity",
      y = "Cardiovascular disease",
      z = "Socioeconomic status",
      i = "Distance to a gym"
    )
  )
}

test_that("every quick plot that takes use_labels takes label_wrap", {
  functions <- label_wrap_quick_plots()
  expect_gt(length(functions), 0)

  for (name in functions) {
    arguments <- formals(get(name, envir = asNamespace("ggdag")))
    expect_true(
      "label_wrap" %in% names(arguments),
      info = paste0(name, "() has a `label_wrap` formal")
    )
    expect_identical(
      deparse(arguments[["label_wrap"]]),
      'ggdag_option("label_wrap", NULL)',
      info = paste0(name, "() defaults `label_wrap` to the option")
    )
  }
})

test_that("every quick plot builds with label_wrap set", {
  dag <- label_wrap_dag()

  for (name in label_wrap_quick_plots()) {
    quick_plot <- get(name, envir = asNamespace("ggdag"))
    args <- c(
      if (!name %in% label_wrap_dagless) list(dag),
      label_wrap_extra_args[[name]],
      list(
        use_labels = TRUE,
        label_geom = geom_dag_label_auto,
        label_wrap = 10
      )
    )

    plot <- do.call(quick_plot, args)
    expect_s3_class(plot, "gg")
    expect_equal(
      auto_label_params(plot)[["wrap"]],
      10,
      info = paste0(name, "() hands `label_wrap` to its label layer")
    )
    expect_no_error(ggplot2::ggplot_build(plot))
  }
})
