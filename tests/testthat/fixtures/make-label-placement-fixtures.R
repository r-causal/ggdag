# Regenerates tests/testthat/fixtures/label-placement-inputs.rds: the exact
# mm-space inputs that the automatic label geoms hand to place_dag_labels()
# for the label-auto visual baseline plots, captured at the vdiffr device
# (svglite, 10 x 8 inches). The placement-quality tests in
# test-label-auto-placement.R call place_dag_labels() on these inputs
# directly, so they exercise the engine on real scenes without opening a
# graphics device. Regenerate only when the plots in test-label-auto-geom.R's
# visual tests change or when the stat/geom pipeline that produces the engine
# inputs is intended to change.
#
# Run non-interactively from the package root:
#   Rscript tests/testthat/fixtures/make-label-placement-fixtures.R

pkgload::load_all(".", quiet = TRUE)

# The visual tests run with the suite-wide options set in helper-load_dag.R.
options(ggdag.layout = "time_ordered")

# These plots must mirror the visual baseline tests in test-label-auto-geom.R
# exactly, so the captured inputs are the ones behind the committed
# _snaps/label-auto-geom baselines.

labelled_triangle <- dagify(
  y ~ m + x,
  m ~ x,
  exposure = "x",
  outcome = "y",
  labels = c(x = "Exposure", m = "Mediator", y = "Outcome"),
  coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
)

ten_node_dag <- dagify(
  b ~ a,
  c ~ a,
  d ~ b,
  e ~ b + c,
  f ~ c,
  g ~ d + e,
  h ~ e + f,
  x ~ g,
  y ~ g + h + x,
  exposure = "x",
  outcome = "y",
  labels = c(
    a = "Genetics",
    b = "Diet",
    c = "Exercise",
    d = "Weight",
    e = "Blood pressure",
    f = "Cholesterol",
    g = "Medication",
    h = "Stress",
    x = "Treatment",
    y = "Outcome"
  )
)

dense_dag <- dagify(
  y ~ a + b + c + x,
  x ~ a + b,
  a ~ c,
  b ~ c,
  exposure = "x",
  outcome = "y",
  labels = c(
    a = "Alcohol consumption",
    b = "Body mass index",
    c = "Socioeconomic status",
    x = "Physical activity",
    y = "Cardiovascular disease"
  ),
  coords = list(
    x = c(c = 0, a = 1, b = 1, x = 2, y = 3),
    y = c(c = 0, a = 0.5, b = -0.5, x = 0, y = 0)
  )
)

curved_dag <- dagify(
  y ~ x + z,
  z ~ x,
  labels = c(x = "Exposure", y = "Outcome", z = "Mediator"),
  coords = list(x = c(x = 0, z = 1, y = 2), y = c(x = 0, z = 1, y = 0))
) |>
  tidy_dagitty() |>
  curve_edge("x", "y", 0.3)

faceted_dag <- dagify(
  y ~ x + z,
  x ~ z,
  exposure = "x",
  outcome = "y",
  labels = c(x = "Exposure", y = "Outcome", z = "Confounder"),
  coords = list(x = c(x = 0, z = 1, y = 2), y = c(x = 0, z = 1, y = 0))
)

plots <- list(
  mediation_triangle = ggdag(
    labelled_triangle,
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ) +
    theme_dag(),
  ten_node = ggdag(
    ten_node_dag,
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ) +
    theme_dag(),
  curved_edge = ggdag(
    curved_dag,
    edge_engine = "ggarrow",
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ) +
    theme_dag(),
  text_variant = ggdag(
    labelled_triangle,
    use_labels = TRUE,
    label_geom = geom_dag_text_auto
  ) +
    theme_dag(),
  dense = ggdag(
    dense_dag,
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ) +
    theme_dag(),
  faceted_paths = ggdag_paths(
    faceted_dag,
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )
)

# Intercept place_dag_labels() at draw time to record the mm-space inputs the
# geom hands it. The label text lives in the caller's `labels` data frame, so
# it is read from the calling frame the way the engine cannot see it.
captures <- new.env(parent = emptyenv())
captures$log <- list()
original_place <- get("place_dag_labels", envir = asNamespace("ggdag"))
recording_place <- function(labels, nodes, edges, bounds, gap = 1.5, ...) {
  out <- original_place(labels, nodes, edges, bounds, gap = gap, ...)
  text <- tryCatch(
    get("labels", envir = parent.frame())$label,
    error = function(e) rep(NA_character_, nrow(labels))
  )
  captures$log[[length(captures$log) + 1]] <- list(
    case = captures$case,
    text = text,
    labels = labels,
    nodes = nodes,
    edges = edges,
    bounds = bounds,
    gap = gap
  )
  out
}
environment(recording_place) <- asNamespace("ggdag")
assignInNamespace("place_dag_labels", recording_place, ns = "ggdag")

render_on_vdiffr_device <- function(case, plot, extra_options = list()) {
  captures$case <- case
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file))
  withr::with_options(
    extra_options,
    withr::with_seed(1234, {
      vdiffr::write_svg(plot, file, title = case)
    })
  )
}

for (case in names(plots)) {
  render_on_vdiffr_device(case, plots[[case]])
}

# The debug overlay reads its option both when the plot is built and when it
# is drawn, so the plot is constructed inside the option scope.
withr::with_options(list(ggdag.debug_repel_points = TRUE), {
  debug_plot <- ggdag(
    labelled_triangle,
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ) +
    theme_dag()
  render_on_vdiffr_device("debug_overlay", debug_plot)
})

assignInNamespace("place_dag_labels", original_place, ns = "ggdag")

# The faceted plot places labels once per panel, so its case name appears
# twice in the log; number repeats so every fixture entry has a unique name.
case_names <- vapply(captures$log, function(cp) cp$case, character(1))
for (case in unique(case_names)) {
  hits <- which(case_names == case)
  if (length(hits) > 1) {
    case_names[hits] <- paste0(case, "_panel", seq_along(hits))
  }
}
fixture <- stats::setNames(captures$log, case_names)
fixture <- lapply(fixture, function(cp) cp[setdiff(names(cp), "case")])

# The label text is recovered from the geom's calling frame; a silent
# capture failure would leave NA text and break the placement tests'
# text-based lookups, so it must fail here instead.
for (cp in fixture) {
  stopifnot(!anyNA(cp$text))
}

path <- file.path("tests", "testthat", "fixtures", "label-placement-inputs.rds")
saveRDS(fixture, path, version = 3)
cat("Wrote", path, "with", length(fixture), "captured scenes:\n")
for (nm in names(fixture)) {
  cp <- fixture[[nm]]
  cat(sprintf(
    "  %-28s labels=%2d nodes=%2d edge_points=%3d bounds=[%.1f x %.1f]\n",
    nm,
    nrow(cp$labels),
    nrow(cp$nodes),
    nrow(cp$edges),
    cp$bounds[[3]],
    cp$bounds[[4]]
  ))
}
