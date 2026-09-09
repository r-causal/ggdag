# Overlap policy of the automatic label geoms, measured on drawn pictures.
#
# `max.overlaps` on `geom_dag_label_auto()` and `geom_dag_text_auto()` takes
# ggrepel's meaning, on the count of things a label's final box still hits
# after placement: the node discs it penetrates, the drawn edges within the
# engine's margins, the other boxes it overlaps, and the panel bounds it
# spills. `Inf`, the default, draws every label. A finite value drops, after
# placement and without a second run of the engine, every label whose box
# hits more than that many things, so `max.overlaps = 0` drops exactly the
# labels the drawn `dag_labels_auto` tree reports as `unresolved`.
# `geom_dag()` and `ggdag()` keep passing `Inf`, so a quick plot never loses
# a label the user did not ask to lose.
#
# Whenever a label is unresolved, or dropped for exceeding `max.overlaps`,
# the draw signals one warning of class `ggdag_label_unresolved_warning`
# naming the labels. One warning per draw, whatever the drawing did
# underneath: the placement engine runs once per panel, and again over the
# same scene whenever the grobs are forced a second time, so a faceted plot
# and a plot that is drawn and then forced each warn exactly once.
#
# How the warnings are counted: `unresolved_warnings()` installs a calling
# handler that records each condition and muffles it. Muffling is what makes
# the count honest, because R collapses repeated identical warnings from one
# expression into a single report, and a handler that let them through would
# count one where the drawing signalled several.
#
# The scene with labels the engine cannot place is the saturated ten-node DAG
# of `helper-label-perf.R` at 4 x 3 inches: 41 edges on a panel of about
# 97 x 72 mm, where `Weight`, `Blood pressure`, and `Outcome` come to rest on
# ink. Every render here is on an off-screen ragg device at 150 dpi, the
# millimetres the engine works in, and every measurement is taken while that
# device is still open.
#
# No snapshot is written here except the wording of the warning.

saturated_size <- c(4, 3)

# `perf_label_plot()` on the saturated scene, with one `max.overlaps` value
# under test. `geom_dag()` hands every node-aware label geom
# `max.overlaps = Inf`, which would match the same formal twice if a value
# were passed beside it, so the wrapper takes that argument itself and hands
# the auto geom the value under test. Everything else is the plot
# `perf_label_plot()` builds.
saturated_plot <- function(overlaps = Inf, geom = geom_dag_label_auto) {
  label_geom <- dag_node_aware(
    function(..., max.overlaps = Inf) geom(..., max.overlaps = overlaps),
    extra = "edge_cap"
  )
  ggdag(
    perf_saturated_dag(),
    use_labels = TRUE,
    label_geom = label_geom
  ) +
    theme_dag()
}

# Draw on an off-screen ragg device of `size` inches at 150 dpi, closed when
# the calling frame exits.
local_label_device <- function(size, .env = parent.frame()) {
  file <- tempfile(fileext = ".png")
  ragg::agg_png(
    file,
    width = size[[1]],
    height = size[[2]],
    units = "in",
    res = 150
  )
  withr::defer(
    {
      grDevices::dev.off()
      unlink(file)
    },
    envir = .env
  )
  invisible(file)
}

# Every `dag_labels_auto` gTree of a grob tree, in panel order.
label_trees <- function(grob) {
  if (inherits(grob, "dag_labels_auto")) {
    return(list(grob))
  }
  children <- grob$children
  if (is.null(children)) {
    return(list())
  }
  unlist(lapply(children, label_trees), recursive = FALSE)
}

# What one panel drew, rather than what its stat carried: a dropped label
# leaves no grob behind. Forcing turns each rounded rectangle into a polygon
# whose geometry moves to the viewport `makeContext.roundrect()` attaches, in
# the absolute millimetres the engine placed it at, and boxes and texts are
# emitted in label order.
label_scene <- function(tree) {
  names <- vapply(
    tree$children,
    function(child) child$name %||% "",
    character(1)
  )
  boxes <- unname(tree$children[grepl("roundrect", names)])
  texts <- unname(tree$children[grepl("text", names)])
  leaders <- unname(tree$children[grepl("segments", names)])

  box_table <- if (length(boxes) == 0) {
    data.frame(
      x = numeric(0),
      y = numeric(0),
      width = numeric(0),
      height = numeric(0)
    )
  } else {
    do.call(
      rbind,
      lapply(boxes, function(box) {
        data.frame(
          x = grid::convertX(box$vp$x, "mm", TRUE),
          y = grid::convertY(box$vp$y, "mm", TRUE),
          width = grid::convertWidth(box$vp$width, "mm", TRUE),
          height = grid::convertHeight(box$vp$height, "mm", TRUE)
        )
      })
    )
  }

  leader_table <- if (length(leaders) == 0) {
    data.frame(x1 = numeric(0), y1 = numeric(0))
  } else {
    do.call(
      rbind,
      lapply(leaders, function(leader) {
        data.frame(
          x1 = grid::convertX(leader$x1, "mm", TRUE),
          y1 = grid::convertY(leader$y1, "mm", TRUE)
        )
      })
    )
  }

  list(
    unresolved = as.character(tree$unresolved),
    dropped = as.character(tree$dropped),
    labels = as.character(tree$labels$label),
    texts = vapply(
      texts,
      function(text) as.character(text$label),
      character(1)
    ),
    boxes = box_table,
    leaders = leader_table
  )
}

# One panel scene per panel of one forced render of `plot`.
# `grid::forceGrob()` runs every `makeContent()` method exactly once per
# panel.
forced_label_scenes <- function(plot, size) {
  local_label_device(size)
  gtable <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  grid::grid.newpage()
  lapply(label_trees(grid::forceGrob(gtable)), label_scene)
}

# The `ggdag_label_unresolved_warning` conditions signalled while `expr` runs.
unresolved_warnings <- function(expr) {
  seen <- list()
  withCallingHandlers(
    expr,
    ggdag_label_unresolved_warning = function(cnd) {
      seen[[length(seen) + 1]] <<- cnd
      rlang::cnd_muffle(cnd)
    }
  )
  seen
}

# The messages of the conditions one draw signalled.
warning_messages <- function(warnings) {
  vapply(warnings, conditionMessage, character(1))
}

# Measuring a placement is not pinning the warning, so the blocks that
# measure one drop the condition on the floor.
without_unresolved_warning <- function(expr) {
  withCallingHandlers(
    expr,
    ggdag_label_unresolved_warning = function(cnd) rlang::cnd_muffle(cnd)
  )
}

# One `print()` of `plot`: the draw a reader gets.
draw_once <- function(plot, size) {
  local_label_device(size)
  print(plot)
  invisible(NULL)
}

# The same draw with the scene forced afterwards, which runs the placement
# engine a second time over the grobs already drawn.
draw_and_force <- function(plot, size) {
  local_label_device(size)
  print(plot)
  grid::grid.force()
  invisible(NULL)
}

draw_saturated <- function() {
  draw_once(perf_label_plot(perf_saturated_dag()), saturated_size)
}

# The saturated scene costs a couple of seconds to render and several blocks
# read the same picture, so each value under test is rendered once.
saturated_cache <- new.env(parent = emptyenv())

saturated_scene <- function(
  overlaps = Inf,
  geom = geom_dag_label_auto,
  key = NULL
) {
  key <- key %||% paste0("label-", overlaps)
  if (is.null(saturated_cache[[key]])) {
    saturated_cache[[key]] <- without_unresolved_warning(
      forced_label_scenes(saturated_plot(overlaps, geom), saturated_size)
    )[[1]]
  }
  saturated_cache[[key]]
}

# A scene with room for every label.
clear_labelled_dag <- function() {
  dagify(
    y ~ m + x,
    m ~ x,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", m = "Mediator", y = "Outcome"),
    coords = list(x = c(x = 0, m = 1, y = 2), y = c(x = 0, m = 1, y = 0))
  )
}

# The scene ------------------------------------------------------------------

test_that("the saturated scene at 4 x 3 leaves three labels unresolved", {
  skip_if_not_installed("ragg")

  # The pin the warning below names. A label reported here is drawn at the
  # engine's least-bad candidate; every other box is clear.
  scene <- without_unresolved_warning(
    forced_label_scenes(
      perf_label_plot(perf_saturated_dag()),
      saturated_size
    )
  )[[1]]

  expect_identical(scene$unresolved, c("Weight", "Blood pressure", "Outcome"))
})

# The warning ----------------------------------------------------------------

test_that("a draw with unresolved labels warns once, naming them", {
  skip_if_not_installed("ragg")

  warnings <- unresolved_warnings(draw_saturated())
  messages <- warning_messages(warnings)

  expect_length(warnings, 1)
  expect_true(all(vapply(
    warnings,
    inherits,
    logical(1),
    "ggdag_label_unresolved_warning"
  )))
  expect_true(any(grepl("Blood pressure", messages, fixed = TRUE)))
  expect_true(any(grepl("Outcome", messages, fixed = TRUE)))
})

test_that("the unresolved warning reads in the package's cli style", {
  skip_if_not_installed("ragg")

  # `expect_snapshot()` records whatever the draw emits, and a draw that does
  # not warn would pin a baseline with no warning in it, so the condition is
  # confirmed before the snapshot is taken.
  stopifnot(
    "the saturated 4 x 3 draw signals ggdag_label_unresolved_warning" = length(
      unresolved_warnings(draw_saturated())
    ) ==
      1
  )

  expect_ggdag_warning(draw_saturated())
})

test_that("a scene where every label is placed cleanly draws silently", {
  skip_if_not_installed("ragg")

  plot <- ggdag(
    clear_labelled_dag(),
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  ) +
    theme_dag()

  scene <- forced_label_scenes(plot, c(7, 5))[[1]]
  expect_identical(scene$unresolved, character(0))
  expect_length(unresolved_warnings(draw_once(plot, c(7, 5))), 0)
})

test_that("dropping every unresolved label still warns, naming them", {
  skip_if_not_installed("ragg")

  warnings <- unresolved_warnings(
    draw_once(saturated_plot(0), saturated_size)
  )
  messages <- warning_messages(warnings)

  expect_length(warnings, 1)
  expect_true(any(grepl("Blood pressure", messages, fixed = TRUE)))
  expect_true(any(grepl("Outcome", messages, fixed = TRUE)))
})

test_that("dropping some of them names the ones that stayed as well", {
  skip_if_not_installed("ragg")

  # An allowance between the counts of two unresolved labels keeps one and
  # drops the others. The one that stayed is still sitting on the ink, so
  # the reader is owed its name beside the names of the two that went.
  scene <- saturated_scene(5)
  kept <- setdiff(scene$unresolved, scene$dropped)
  stopifnot(
    "an allowance of five keeps one unresolved label and drops two" = identical(
      kept,
      "Weight"
    ) &&
      identical(scene$dropped, c("Blood pressure", "Outcome"))
  )

  warnings <- unresolved_warnings(
    draw_once(saturated_plot(5), saturated_size)
  )
  messages <- warning_messages(warnings)

  expect_length(warnings, 1)
  expect_true(any(grepl("Weight", messages, fixed = TRUE)))
  expect_true(any(grepl("Blood pressure", messages, fixed = TRUE)))
  expect_true(any(grepl("Outcome", messages, fixed = TRUE)))
})

test_that("forcing a drawn scene a second time does not warn again", {
  skip_if_not_installed("ragg")

  # `print()` runs the engine once and `grid::grid.force()` runs it again
  # over the same drawn grobs, which is one draw and must be one warning.
  warnings <- unresolved_warnings(
    draw_and_force(perf_label_plot(perf_saturated_dag()), saturated_size)
  )

  expect_length(warnings, 1)
})

test_that("the same grobs drawn smaller warn about that draw", {
  skip_if_not_installed("ragg")

  # One gtable drawn on two devices is two draws of one picture. Every label
  # is placed clear at 7 x 5, so that draw says nothing; the same grobs at
  # 4 x 3 leave three on the ink, and the reader who resized the window is
  # owed the warning for the picture in front of them. Drawing that picture
  # again replays a draw that has already warned and stays quiet.
  gtable <- ggplot2::ggplot_gtable(
    ggplot2::ggplot_build(perf_label_plot(perf_saturated_dag()))
  )
  draw_gtable <- function(size) {
    local_label_device(size)
    grid::grid.newpage()
    grid::grid.draw(gtable)
    invisible(NULL)
  }

  expect_length(unresolved_warnings(draw_gtable(c(7, 5))), 0)

  warnings <- unresolved_warnings(draw_gtable(saturated_size))
  expect_length(warnings, 1)
  expect_true(any(grepl(
    "Blood pressure",
    warning_messages(warnings),
    fixed = TRUE
  )))

  expect_length(unresolved_warnings(draw_gtable(saturated_size)), 0)
})

test_that("a faceted plot warns once for the whole draw", {
  skip_if_not_installed("ragg")

  plot <- ggdag_paths(
    perf_ten_node_dag(),
    use_labels = TRUE,
    label_geom = geom_dag_label_auto
  )

  # The engine runs once per panel. More than one panel of this plot has
  # labels it cannot place, so a warning per panel would be several.
  scenes <- without_unresolved_warning(forced_label_scenes(plot, c(7, 5)))
  crowded <- vapply(
    scenes,
    function(scene) length(scene$unresolved) > 0,
    logical(1)
  )
  expect_gt(sum(crowded), 1)

  warnings <- unresolved_warnings(draw_once(plot, c(7, 5)))
  messages <- warning_messages(warnings)

  expect_length(warnings, 1)

  # The warning belongs to the draw, so it names the union over the panels
  # rather than whatever the panel that completed the tally was left with.
  # No label of this DAG is a substring of another, so a name is in the
  # message only if the warning put it there.
  union <- unique(unlist(lapply(scenes, function(scene) scene$unresolved)))
  every_label <- unname(perf_ten_node_labels)
  named <- vapply(
    every_label,
    function(label) any(grepl(label, messages, fixed = TRUE)),
    logical(1)
  )
  expect_setequal(every_label[named], union)
})

# max.overlaps ---------------------------------------------------------------

test_that("max.overlaps = Inf draws every label, the unresolved ones too", {
  skip_if_not_installed("ragg")

  scene <- saturated_scene(Inf)

  expect_setequal(scene$texts, scene$labels)
  expect_identical(nrow(scene$boxes), length(scene$texts))
  expect_true(all(scene$unresolved %in% scene$texts))
})

test_that("max.overlaps = 0 drops exactly the unresolved labels", {
  skip_if_not_installed("ragg")

  kept <- saturated_scene(Inf)
  dropped <- kept$unresolved
  expect_gt(length(dropped), 0)

  scene <- saturated_scene(0)

  expect_setequal(scene$texts, setdiff(kept$labels, dropped))
  expect_identical(nrow(scene$boxes), length(scene$texts))
  expect_false(any(dropped %in% scene$texts))
})

test_that("the text variant drops the same labels", {
  skip_if_not_installed("ragg")

  kept <- saturated_scene(Inf, geom = geom_dag_text_auto, key = "text-Inf")
  scene <- saturated_scene(0, geom = geom_dag_text_auto, key = "text-0")

  expect_gt(length(kept$unresolved), 0)
  expect_identical(nrow(scene$boxes), 0L)
  expect_setequal(scene$texts, setdiff(kept$labels, kept$unresolved))
})

test_that("a dropped label leaves no leader behind", {
  skip_if_not_installed("ragg")

  # `label_leader_grob()` draws from the node disc to the point of the box
  # nearest it, so every leader ends on the boundary of the box it belongs
  # to. A leader whose far end lies on no drawn box belongs to a label that
  # is not there. Both unresolved labels of this scene have a leader, so
  # dropping them takes two leaders with them.
  kept <- saturated_scene(Inf)
  scene <- saturated_scene(0)

  on_a_box <- function(leaders, boxes) {
    vapply(
      seq_len(nrow(leaders)),
      function(i) {
        any(
          leaders$x1[i] >= boxes$x - boxes$width / 2 - 1e-6 &
            leaders$x1[i] <= boxes$x + boxes$width / 2 + 1e-6 &
            leaders$y1[i] >= boxes$y - boxes$height / 2 - 1e-6 &
            leaders$y1[i] <= boxes$y + boxes$height / 2 + 1e-6
        )
      },
      logical(1)
    )
  }

  expect_true(all(on_a_box(kept$leaders, kept$boxes)))
  expect_true(all(on_a_box(scene$leaders, scene$boxes)))
  expect_lt(nrow(scene$leaders), nrow(kept$leaders))
})

test_that("dropping never moves a label that stays", {
  skip_if_not_installed("ragg")

  # Dropping is post hoc: the engine is not run a second time without the
  # dropped labels, so a neighbour does not benefit from the room they leave.
  kept <- saturated_scene(Inf)
  stays <- !(kept$texts %in% kept$unresolved)
  scene <- saturated_scene(0)

  expect_equal(
    scene$boxes[order(scene$texts), c("x", "y")],
    kept$boxes[stays, ][order(kept$texts[stays]), c("x", "y")],
    ignore_attr = TRUE
  )
})

test_that("a finite max.overlaps keeps at least what zero keeps", {
  skip_if_not_installed("ragg")

  # The count a box is judged on starts at zero for a resolved label, so a
  # larger allowance can only keep more labels.
  none <- length(saturated_scene(0)$texts)
  some <- length(saturated_scene(1)$texts)
  every <- length(saturated_scene(Inf)$texts)

  expect_lte(none, some)
  expect_lte(some, every)
  expect_lt(none, every)
})

test_that("max.overlaps = 0 keeps every label of an uncrowded scene", {
  skip_if_not_installed("ragg")

  plot <- ggplot(clear_labelled_dag(), aes_dag()) +
    geom_dag_point() +
    geom_dag_edges() +
    geom_dag_label_auto(aes(label = label), max.overlaps = 0) +
    theme_dag()

  scene <- forced_label_scenes(plot, c(7, 5))[[1]]

  expect_identical(scene$unresolved, character(0))
  expect_setequal(scene$texts, c("Exposure", "Mediator", "Outcome"))
  expect_length(unresolved_warnings(draw_once(plot, c(7, 5))), 0)
})

test_that("the quick plots keep every label", {
  skip_if_not_installed("ragg")

  # `geom_dag()` passes `max.overlaps = Inf` to every node-aware label geom,
  # so `ggdag(use_labels = TRUE)` draws a crowded DAG's labels whatever the
  # geom default is, and says so once through the warning.
  layers <- geom_dag(
    use_labels = TRUE,
    label_geom = geom_dag_label_auto,
    node_size = 20
  )
  label_item <- layers[[4]]
  params <- c(label_item$stat_params, label_item$geom_params)
  expect_identical(params[["max.overlaps"]], Inf)

  scene <- without_unresolved_warning(
    forced_label_scenes(
      perf_label_plot(perf_saturated_dag()),
      saturated_size
    )
  )[[1]]
  expect_setequal(scene$texts, scene$labels)
})
