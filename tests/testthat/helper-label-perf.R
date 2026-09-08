# Scenes and measurement harness for test-label-auto-perf.R.
#
# The four labelled DAGs below are the scenes the label placement engine was
# profiled on. They are defined here rather than sourced from anywhere else so
# the tests are self-contained: three of them mirror scenes used elsewhere in
# the suite, and `perf_very_big_dag()` is a 30-node, 56-edge life-course DAG
# that is by far the most expensive labelled scene the engine sees.
#
# The harness renders a scene to an off-screen ragg device at a fixed size and
# reads the placement back out of the forced grob tree in millimetres, the way
# test-label-placement-quality.R reads a drawn plot. Nothing here writes a
# snapshot.

perf_ten_node_labels <- c(
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

perf_ten_node_dag <- function() {
  dagify(
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
    labels = perf_ten_node_labels
  )
}

perf_dense_dag <- function() {
  dagify(
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
}

# Every time-ordered pair of the ten-node DAG becomes an edge, so the panel
# fills with ink and the engine's obstacle set grows by a factor of five.
perf_saturated_dag <- function() {
  dag_saturate(perf_ten_node_dag())
}

# A life-course DAG of 30 nodes and 56 edges across early life, childhood,
# adulthood, and late-life outcomes, laid out by the time-ordered layout. Most
# edges join neighbouring tiers and a handful span two or three.
perf_very_big_dag <- function() {
  dagify(
    birth_weight ~ genetics + parental_ses,
    nutrition ~ parental_ses + birth_weight,
    education ~ parental_ses,
    adversity ~ parental_ses,
    inflammation ~ air_pollution + bmi,
    diet ~ nutrition + income,
    occupation ~ education,
    income ~ education + occupation,
    smoking ~ education + adversity + stress,
    healthcare_access ~ education + income,
    depression ~ adversity + social_support + stress,
    phys_act ~ social_support + income + depression,
    medication ~ healthcare_access + depression,
    chol ~ genetics + smoking,
    cvd ~ smoking + phys_act + diabetes + bp + chol + inflammation,
    cancer ~ smoking + alcohol + inflammation,
    alcohol ~ stress,
    bp ~ alcohol + phys_act + bmi + medication,
    bmi ~ genetics + diet + phys_act + sleep,
    sleep ~ stress,
    insulin_resistance ~ bmi,
    diabetes ~ insulin_resistance,
    ckd ~ diabetes + bp,
    frailty ~ ckd,
    mortality ~ ckd + frailty + cvd + cancer,
    exposure = "phys_act",
    outcome = "cvd",
    labels = c(
      genetics = "Genetics",
      parental_ses = "Parental SES",
      birth_weight = "Birth weight",
      air_pollution = "Air pollution",
      nutrition = "Child nutrition",
      education = "Education",
      adversity = "Adversity",
      social_support = "Social support",
      occupation = "Occupation",
      income = "Income",
      healthcare_access = "Healthcare access",
      smoking = "Smoking",
      alcohol = "Alcohol",
      diet = "Diet",
      phys_act = "Physical activity",
      sleep = "Sleep",
      stress = "Stress",
      depression = "Depression",
      bmi = "BMI",
      bp = "Blood pressure",
      chol = "Cholesterol",
      diabetes = "Diabetes",
      insulin_resistance = "Insulin resistance",
      inflammation = "Inflammation",
      ckd = "Kidney disease",
      medication = "Medication",
      frailty = "Frailty",
      cvd = "Heart disease",
      cancer = "Cancer",
      mortality = "Mortality"
    )
  )
}

perf_label_dags <- list(
  ten_node = perf_ten_node_dag,
  dense = perf_dense_dag,
  saturated = perf_saturated_dag,
  very_big = perf_very_big_dag
)

perf_label_plot <- function(dag) {
  ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label_auto) + theme_dag()
}

# Run `expr` with a scene's plotting options in force. The plot has to be built
# as well as drawn inside this scope, because `ggdag()` reads the edge engine
# when it is called and the routed edge layer reads the routing mode when it is
# added to the plot.
with_perf_options <- function(route, expr) {
  withr::with_options(
    list(
      ggdag.layout = "time_ordered",
      ggdag.edge_engine = "ggarrow",
      ggdag.edge_route = route
    ),
    expr
  )
}

# Force `plot` on an off-screen ragg device of `size` inches at 150 dpi and
# call `measure()` on its single `dag_labels_auto` gTree.
#
# `grid::forceGrob()` runs every `makeContent()` method exactly once, where
# drawing the gtable and then calling `grid::grid.force()` runs the placement
# engine twice, which doubles the cost of the scenes here for no extra signal.
# The forced tree is the same one the drawn plot carries: the boxes it holds
# are the polygons `makeContext.roundrect()` produces, positioned by a viewport
# in the absolute millimetres the engine placed them at, so nothing measured
# below depends on the viewport the tree would have been drawn in.
perf_measure_render <- function(plot, size, measure) {
  file <- tempfile(fileext = ".png")
  ragg::agg_png(
    file,
    width = size[[1]],
    height = size[[2]],
    units = "in",
    res = 150
  )
  on.exit(
    {
      grDevices::dev.off()
      unlink(file)
    },
    add = TRUE
  )

  gtable <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  grid::grid.newpage()
  forced <- grid::forceGrob(gtable)
  tree <- grid::getGrob(forced, "dag_labels_auto", grep = TRUE, global = TRUE)
  stopifnot(inherits(tree, "dag_labels_auto"))
  measure(tree)
}

# The placement one `dag_labels_auto` gTree drew: one row per label with its
# box in millimetres of the panel, the leader segments in the order the tree
# holds them, and the labels the engine could not place cleanly. Forcing turns
# each rounded rectangle into a polygon whose geometry moves to the viewport
# `makeContext.roundrect()` attaches, in the absolute millimetres the engine
# placed it at, and boxes and texts are emitted in label order.
perf_placement <- function(tree) {
  children <- tree$children
  names <- vapply(children, function(child) child$name %||% "", character(1))
  boxes <- unname(children[grepl("roundrect", names)])
  texts <- unname(children[grepl("text", names)])
  leaders <- unname(children[grepl("segments", names)])
  stopifnot(
    length(boxes) == length(texts),
    length(texts) == nrow(tree$labels),
    length(boxes) + length(texts) + length(leaders) == length(children)
  )

  boxes <- do.call(
    rbind,
    lapply(seq_along(boxes), function(i) {
      box <- boxes[[i]]
      data.frame(
        label = as.character(texts[[i]]$label),
        x = grid::convertX(box$vp$x, "mm", TRUE),
        y = grid::convertY(box$vp$y, "mm", TRUE),
        width = grid::convertWidth(box$vp$width, "mm", TRUE),
        height = grid::convertHeight(box$vp$height, "mm", TRUE),
        stringsAsFactors = FALSE
      )
    })
  )

  leaders <- if (length(leaders) == 0) {
    data.frame(
      x0 = numeric(0),
      y0 = numeric(0),
      x1 = numeric(0),
      y1 = numeric(0)
    )
  } else {
    do.call(
      rbind,
      lapply(leaders, function(leader) {
        data.frame(
          x0 = grid::convertX(leader$x0, "mm", TRUE),
          y0 = grid::convertY(leader$y0, "mm", TRUE),
          x1 = grid::convertX(leader$x1, "mm", TRUE),
          y1 = grid::convertY(leader$y1, "mm", TRUE)
        )
      })
    )
  }

  list(
    boxes = boxes,
    leaders = leaders,
    unresolved = as.character(tree$unresolved)
  )
}

# The placement of one scene under one routing mode, at `size` inches.
perf_scene_placement <- function(scene, route, size = c(7, 5)) {
  with_perf_options(route, {
    plot <- perf_label_plot(perf_label_dags[[scene]]())
    perf_measure_render(plot, size, perf_placement)
  })
}

# The exact arguments `makeContent.dag_labels_auto()` hands to
# `place_dag_labels()` while `plot` is drawn, together with whatever
# `measure()` reads off the forced tree, from one render. A block that needs
# both the millimetre geometry the engine worked in and the boxes the render
# drew takes them from here rather than rendering the scene twice. The trace
# is removed before this returns.
#
# The tracer runs inside the engine's own frame, so it reaches the store
# through an option rather than through any environment the caller could pass
# it. The option name is deliberately outside the `ggdag.` namespace the
# package reads.
perf_traced_render <- function(plot, size, measure) {
  captured <- new.env(parent = emptyenv())
  captured$store <- list()
  old_options <- options(ggdag_label_perf_capture = captured)
  on.exit(options(old_options), add = TRUE)

  suppressMessages(trace(
    "place_dag_labels",
    where = asNamespace("ggdag"),
    tracer = quote({
      store <- getOption("ggdag_label_perf_capture")
      store$store[[length(store$store) + 1]] <- list(
        labels = labels,
        nodes = nodes,
        edges = edges,
        bounds = bounds,
        gap = gap,
        reach = reach,
        leader = leader
      )
    }),
    print = FALSE
  ))
  on.exit(
    suppressMessages(untrace("place_dag_labels", where = asNamespace("ggdag"))),
    add = TRUE,
    after = FALSE
  )

  measured <- perf_measure_render(plot, size, measure)
  stopifnot(length(captured$store) >= 1)
  list(
    inputs = captured$store[[length(captured$store)]],
    measured = measured
  )
}

# The exact arguments `makeContent.dag_labels_auto()` hands to
# `place_dag_labels()` for one scene, captured by tracing the engine during a
# real render.
perf_engine_inputs <- function(scene, route, size = c(7, 5)) {
  with_perf_options(route, {
    plot <- perf_label_plot(perf_label_dags[[scene]]())
    perf_traced_render(plot, size, function(tree) invisible(NULL))$inputs
  })
}

# The engine inputs and the placement one render of `dag` produced, for the
# labelled scenes the placement fixture does not carry.
perf_dag_capture <- function(dag, route, size = c(7, 5)) {
  with_perf_options(route, {
    plot <- perf_label_plot(dag)
    captured <- perf_traced_render(plot, size, perf_placement)
    list(inputs = captured$inputs, placement = captured$measured)
  })
}

# Time `place_dag_labels()` on captured inputs, returning the median seconds
# and the allocation of one call.
perf_engine_bench <- function(inputs, iterations = 5) {
  # The inputs come from a render, and a promise forced inside the benchmarked
  # expression would put that render under `Rprofmem()`.
  force(inputs)
  timing <- bench::mark(
    place_dag_labels(
      inputs$labels,
      inputs$nodes,
      inputs$edges,
      bounds = inputs$bounds,
      gap = inputs$gap,
      reach = inputs$reach,
      leader = inputs$leader
    ),
    iterations = iterations,
    check = FALSE,
    filter_gc = FALSE
  )
  list(
    median = as.numeric(timing$median),
    mem_alloc = as.numeric(timing$mem_alloc)
  )
}

# The engine benchmark of one scene and route, computed once per session. A
# single measurement of the very big spline scene costs about twenty seconds,
# and both the time pin and the allocation pin read the same one.
perf_bench_cache <- new.env(parent = emptyenv())

perf_cached_bench <- function(scene, route) {
  key <- paste(scene, route)
  if (is.null(perf_bench_cache[[key]])) {
    perf_bench_cache[[key]] <- perf_engine_bench(perf_engine_inputs(
      scene,
      route
    ))
  }
  perf_bench_cache[[key]]
}
