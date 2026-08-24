# a stable, printable summary of a DAG's rows, so a comparison that fails
# reports the edges rather than a wall of tibble columns
edge_signature <- function(.tdy_dag) {
  if (is.null(.tdy_dag)) {
    return(NA_character_)
  }

  dag_data <- pull_dag_data(.tdy_dag)
  sort(paste(dag_data$name, as.character(dag_data$direction), dag_data$to))
}

test_that("dag_saturate returns a saturated DAG", {
  withr::local_seed(1234)
  .dag <- tidy_dagitty(dagify(y ~ x, x ~ z))
  .saturated_dag <- dag_saturate(.dag)
  expect_s3_class(.saturated_dag, "tidy_dagitty")
  expect_gt(nrow(pull_dag_data(.saturated_dag)), nrow(pull_dag_data(.dag)))
  expect_equal(nrow(pull_dag_data(.saturated_dag)), 4)
  p1 <- ggdag(.saturated_dag)
  expect_doppelganger("dag_saturate returns a saturated DAG", p1)
})

test_that("dag_saturate() keeps isolated nodes", {
  withr::local_seed(1234)
  .dag <- dagitty::dagitty("dag{x -> y; z}")
  .saturated_dag <- dag_saturate(.dag)

  expect_setequal(
    unique(pull_dag_data(.saturated_dag)$name),
    c("x", "y", "z")
  )
  expect_setequal(names(pull_dag(.saturated_dag)), c("x", "y", "z"))
})

test_that("visual: dag_saturate() keeps isolated nodes", {
  withr::local_seed(1234)
  .saturated_dag <- dag_saturate(dagitty::dagitty("dag{x -> y; z}"))
  # never record a baseline from a saturation that lost the isolated node
  skip_if_not(
    setequal(unique(pull_dag_data(.saturated_dag)$name), c("x", "y", "z"))
  )
  expect_doppelganger(
    "dag_saturate keeps isolated nodes",
    ggdag(.saturated_dag)
  )
})

test_that("use_existing_coords works as expected", {
  .tdy_dag <- dagify(y ~ x + z, x ~ z) |>
    tidy_dagitty()
  result_with_coords <- dag_saturate(.tdy_dag, use_existing_coords = TRUE)
  expect_equal(
    dagitty::coordinates(pull_dag(result_with_coords)),
    dagitty::coordinates(pull_dag(.tdy_dag))
  )
})

test_that("dag_saturate() carries the DAG's labels through", {
  withr::local_seed(1234)
  labels <- c("x" = "Exposure", "y" = "Outcome", "z" = "Confounder")
  .dag <- dagify(y ~ x, x ~ z, labels = labels)
  .saturated_dag <- dag_saturate(tidy_dagitty(.dag))

  expect_equal(label(pull_dag(.saturated_dag)), labels)

  dag_data <- pull_dag_data(.saturated_dag)
  expect_true("label" %in% names(dag_data))
  expect_equal(unique(dag_data$label[dag_data$name == "z"]), "Confounder")
})

test_that("visual: dag_saturate() keeps labels", {
  withr::local_seed(1234)
  .saturated_dag <- dagify(
    y ~ x,
    x ~ z,
    labels = c("x" = "Exposure", "y" = "Outcome", "z" = "Confounder")
  ) |>
    tidy_dagitty() |>
    dag_saturate()
  # never record a baseline from a saturation that lost the labels
  skip_if_not("label" %in% names(pull_dag_data(.saturated_dag)))
  expect_doppelganger(
    "dag_saturate keeps labels",
    ggdag(.saturated_dag, use_labels = TRUE)
  )
})

test_that("dag_saturate() works on DAGs with a single time point", {
  withr::local_seed(1234)
  single_node <- dag_saturate(dagitty::dagitty("dag{x}"))
  expect_s3_class(single_node, "tidy_dagitty")
  expect_setequal(unique(pull_dag_data(single_node)$name), "x")
  expect_setequal(names(pull_dag(single_node)), "x")
  expect_equal(n_edges(single_node), 0)

  edge_free <- dag_saturate(dagitty::dagitty("dag{x; y}"))
  expect_s3_class(edge_free, "tidy_dagitty")
  expect_setequal(unique(pull_dag_data(edge_free)$name), c("x", "y"))
  expect_setequal(names(pull_dag(edge_free)), c("x", "y"))
  expect_equal(n_edges(edge_free), 0)
})

test_that("edges are correctly pruned from the DAG", {
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))
  expect_equal(nrow(pull_dag_data(.tdy_dag)), 4)
  expect_equal(
    .tdy_dag |>
      pull_dag_data() |>
      dplyr::filter(name == "z", to == "x") |>
      nrow(),
    1
  )
  pruned_dag <- dag_prune(.tdy_dag, c("z" = "x"))
  expect_equal(nrow(pull_dag_data(pruned_dag)), 3)
  expect_equal(
    pruned_dag |>
      pull_dag_data() |>
      dplyr::filter(name == "z", to == "x") |>
      nrow(),
    0
  )
})

test_that("dag_prune() keeps a node when all of its edges are pruned at once", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(x ~ z, y ~ z))
  pruned_dag <- dag_prune(.tdy_dag, c("z" = "x", "z" = "y"))

  expect_setequal(unique(pull_dag_data(pruned_dag)$name), c("x", "y", "z"))
  expect_setequal(names(pull_dag(pruned_dag)), c("x", "y", "z"))
  expect_equal(n_edges(pruned_dag), 0)

  z_rows <- dplyr::filter(pull_dag_data(pruned_dag), name == "z")
  expect_equal(nrow(z_rows), 1)
  expect_true(is.na(z_rows$to))
  expect_true(is.na(z_rows$direction))
})

test_that("dag_prune() batch pruning agrees with sequential pruning", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(x ~ z, y ~ z))

  batch <- dag_prune(.tdy_dag, c("z" = "x", "z" = "y"))
  sequential <- .tdy_dag |>
    dag_prune(c("z" = "x")) |>
    dag_prune(c("z" = "y"))

  expect_equal(edge_signature(batch), edge_signature(sequential))
  expect_setequal(names(pull_dag(batch)), names(pull_dag(sequential)))
})

test_that("visual: dag_prune() keeps a node when all of its edges are pruned", {
  withr::local_seed(1234)
  pruned_dag <- dag_prune(
    tidy_dagitty(dagify(x ~ z, y ~ z)),
    c("z" = "x", "z" = "y")
  )
  # never record a baseline from a prune that lost the node
  skip_if_not(
    setequal(unique(pull_dag_data(pruned_dag)$name), c("x", "y", "z"))
  )
  expect_doppelganger(
    "dag_prune keeps a fully pruned node",
    ggdag(pruned_dag)
  )
})

test_that("dag_prune() keeps edge directions when direction levels are reordered", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z)) |>
    dplyr::mutate(direction = forcats::fct_relevel(direction, "<->"))

  pruned_dag <- dag_prune(.tdy_dag, c("z" = "x"))
  dag_data <- pull_dag_data(pruned_dag)
  edge_directions <- as.character(dag_data$direction[!is.na(dag_data$to)])

  expect_length(edge_directions, 2)
  expect_setequal(edge_directions, "->")
})

test_that("dag_prune() keeps edge directions for coordinate-carrying input", {
  .tdy_dag <- as_tidy_dagitty(data.frame(
    name = c("z", "z", "x", "y"),
    to = c("x", "y", "y", NA),
    x = c(0, 0, 1, 2),
    y = c(0, 0, 1, 0),
    xend = c(1, 2, 2, NA),
    yend = c(1, 0, 0, NA)
  ))

  pruned_dag <- dag_prune(.tdy_dag, c("z" = "x"))
  dag_data <- pull_dag_data(pruned_dag)
  edge_rows <- dag_data[!is.na(dag_data$to), ]

  expect_false(anyNA(edge_rows$direction))
  expect_setequal(as.character(edge_rows$direction), "->")
  expect_equal(
    nrow(dplyr::filter(dag_data, name == "z", to == "x")),
    0
  )
})

test_that("dag_prune() errors on edges that are not in the DAG", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))

  # 'w' is not a node in the DAG
  expect_error(
    dag_prune(.tdy_dag, c("x" = "w")),
    class = "ggdag_missing_edges_error"
  )
  # the edge is x -> y, so this specification is reversed
  expect_error(
    dag_prune(.tdy_dag, c("y" = "x")),
    class = "ggdag_missing_edges_error"
  )
  expect_error(
    dag_prune(.tdy_dag, c("x" = "w")),
    class = "ggdag_error"
  )
})

test_that("missing edges produce an informative message", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))
  # never record a baseline from the pre-fix silent no-op
  skip_if_not(inherits(
    tryCatch(dag_prune(.tdy_dag, c("y" = "x")), error = identity),
    "ggdag_missing_edges_error"
  ))

  expect_ggdag_error(dag_prune(.tdy_dag, c("y" = "x")))
})

test_that("dag_prune() rejects an unnamed edges vector", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))

  expect_error(dag_prune(.tdy_dag, "y"), class = "ggdag_type_error")
})

test_that("dag_prune() rejects a partially named edges vector", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))

  expect_error(
    dag_prune(.tdy_dag, c("z" = "x", "y")),
    class = "ggdag_type_error"
  )
})

test_that("unnamed edges produce an informative message", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))
  # never record a baseline from the raw stopifnot() message
  skip_if_not(inherits(
    tryCatch(dag_prune(.tdy_dag, "y"), error = identity),
    "ggdag_type_error"
  ))

  expect_ggdag_error(dag_prune(.tdy_dag, "y"))
  expect_ggdag_error(dag_prune(.tdy_dag, c("z" = "x", "y")))
})

test_that("dag_saturate() keeps bidirected edges", {
  withr::local_seed(1234)
  .saturated_dag <- dag_saturate(dagify(y ~ x, y ~ z, x ~ ~z))
  dag_data <- pull_dag_data(.saturated_dag)

  bidirected <- dplyr::filter(dag_data, direction == "<->")
  expect_equal(nrow(bidirected), 1)
  expect_setequal(c(bidirected$name, bidirected$to), c("x", "z"))
  expect_false(anyNA(c(
    bidirected$x,
    bidirected$y,
    bidirected$xend,
    bidirected$yend
  )))

  directed <- dplyr::filter(dag_data, direction == "->")
  expect_setequal(paste(directed$name, directed$to), c("x y", "z y"))

  saturated_edges <- dagitty::edges(pull_dag(.saturated_dag))
  expect_true(any(
    as.character(saturated_edges$e) == "<->" &
      as.character(saturated_edges$v) %in% c("x", "z") &
      as.character(saturated_edges$w) %in% c("x", "z")
  ))

  # the input denies x _||_ z, so the saturated model must not imply it
  expect_length(
    dagitty::impliedConditionalIndependencies(pull_dag(.saturated_dag)),
    0
  )
})

test_that("dag_saturate() handles a directed and a bidirected edge on one pair", {
  withr::local_seed(1234)
  .saturated_dag <- dag_saturate(dagify(y ~ x, x ~ ~y))
  dag_data <- pull_dag_data(.saturated_dag)

  expect_setequal(unique(dag_data$name), c("x", "y"))
  expect_equal(n_edges(.saturated_dag), 2)
  expect_setequal(
    as.character(dag_data$direction[!is.na(dag_data$to)]),
    c("->", "<->")
  )
  expect_equal(
    nrow(dplyr::filter(dag_data, name == "x", to == "y", direction == "->")),
    1
  )
})

test_that("visual: dag_saturate() keeps bidirected edges", {
  withr::local_seed(1234)
  .saturated_dag <- dag_saturate(dagify(y ~ x, y ~ z, x ~ ~z))
  # never record a baseline from a saturation that lost the bidirected edge
  skip_if_not(
    any(pull_dag_data(.saturated_dag)$direction == "<->", na.rm = TRUE)
  )
  expect_doppelganger(
    "dag_saturate keeps bidirected edges",
    ggdag(.saturated_dag)
  )
})

test_that("dag_saturate() carries adjusted nodes through", {
  withr::local_seed(1234)
  .adjusted_dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y"
  ) |>
    control_for(var = "z", activate_colliders = FALSE)

  .saturated_dag <- dag_saturate(.adjusted_dag)

  expect_equal(
    as.character(dagitty::adjustedNodes(pull_dag(.saturated_dag))),
    "z"
  )

  dag_data <- pull_dag_data(.saturated_dag)
  expect_true("adjusted" %in% names(dag_data))

  # `[[` rather than `$` so a missing column does not warn before the
  # expectations below report it
  adjusted_col <- as.character(dag_data[["adjusted"]])
  expect_setequal(adjusted_col[dag_data$name == "z"], "adjusted")
  expect_setequal(adjusted_col[dag_data$name != "z"], "unadjusted")
})

test_that("dag_saturate() treats all-NA stored coordinates as absent", {
  withr::local_seed(1234)
  .dag <- dagify(y ~ x, x ~ z)

  expect_no_error(dag_saturate(.dag, use_existing_coords = TRUE))

  stored <- tryCatch(
    dag_saturate(.dag, use_existing_coords = TRUE),
    error = function(e) NULL
  )
  computed <- dag_saturate(.dag, use_existing_coords = FALSE)
  expect_equal(edge_signature(stored), edge_signature(computed))
})

test_that("dag_saturate() labels a node whose only edge is bidirected", {
  withr::local_seed(1234)
  .saturated_dag <- dag_saturate(dagify(
    x ~ y,
    z ~ y,
    x ~ ~z,
    labels = c("x" = "X", "y" = "Y", "z" = "Z")
  ))

  dag_data <- pull_dag_data(.saturated_dag)
  expect_false(anyNA(dag_data[["label"]]))
  expect_setequal(unique(dag_data$label[dag_data$name == "x"]), "X")

  built <- ggplot2::ggplot_build(ggdag(.saturated_dag, use_labels = TRUE))
  drawn <- unlist(purrr::map(built$data, ~ as.character(.x[["label"]])))
  expect_true(all(c("X", "Y", "Z") %in% drawn))
})

test_that("visual: dag_saturate() labels a bidirected node", {
  withr::local_seed(1234)
  .saturated_dag <- dag_saturate(dagify(
    x ~ y,
    z ~ y,
    x ~ ~z,
    labels = c("x" = "X", "y" = "Y", "z" = "Z")
  ))
  # never record a baseline from a saturation that lost a node's label
  skip_if_not(!anyNA(pull_dag_data(.saturated_dag)[["label"]]))
  expect_doppelganger(
    "dag_saturate labels a bidirected node",
    ggdag(.saturated_dag, use_labels = TRUE)
  )
})

test_that("dag_prune() keeps a node that only ever ends an edge", {
  .tdy_dag <- as_tidy_dagitty(data.frame(
    name = "x",
    to = "y",
    x = 0,
    y = 0,
    xend = 1,
    yend = 1
  ))

  pruned_dag <- dag_prune(.tdy_dag, c("x" = "y"))
  dag_data <- pull_dag_data(pruned_dag)

  expect_setequal(unique(dag_data$name), c("x", "y"))
  expect_setequal(names(pull_dag(pruned_dag)), c("x", "y"))
  expect_equal(n_edges(pruned_dag), 0)

  y_row <- dplyr::filter(dag_data, name == "y")
  expect_equal(nrow(y_row), 1)
  expect_true(is.na(y_row$to))
  # the node's own coordinates are the end of the edge that pointed at it
  expect_equal(c(y_row$x, y_row$y), c(1, 1))
})

test_that("dag_prune() prunes a bidirected edge given in either orientation", {
  withr::local_seed(1234)
  .saturated_dag <- dag_saturate(dagify(y ~ x, y ~ z, x ~ ~z))

  stored_orientation <- dag_prune(.saturated_dag, c("x" = "z"))
  reversed_orientation <- dag_prune(.saturated_dag, c("z" = "x"))

  expect_equal(
    edge_signature(stored_orientation),
    edge_signature(reversed_orientation)
  )
  expect_equal(
    nrow(dplyr::filter(
      pull_dag_data(stored_orientation),
      !is.na(direction),
      direction == "<->"
    )),
    0
  )
  # the directed edges are untouched, and are still named in their own direction
  expect_setequal(
    as.character(na.omit(pull_dag_data(stored_orientation)$direction)),
    "->"
  )
  expect_error(
    dag_prune(.saturated_dag, c("y" = "x")),
    class = "ggdag_missing_edges_error"
  )
})

test_that("dag_prune() reports a repeated missing edge once", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))

  err <- expect_error(
    dag_prune(.tdy_dag, c("x" = "w", "x" = "w")),
    class = "ggdag_missing_edges_error"
  )
  expect_match(conditionMessage(err), "x -> w", fixed = TRUE)
  expect_length(
    gregexpr("x -> w", conditionMessage(err), fixed = TRUE)[[1]],
    1
  )
})

test_that("dag_prune() prunes the same edges on grouped data", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))

  grouped <- dplyr::group_by(.tdy_dag, name)
  pruned_grouped <- dag_prune(grouped, c("z" = "x"))
  pruned <- dag_prune(.tdy_dag, c("z" = "x"))

  expect_equal(edge_signature(pruned_grouped), edge_signature(pruned))
  expect_equal(dplyr::group_vars(pull_dag_data(pruned_grouped)), "name")
})

test_that("dag_prune() rejects a missing value in edges", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x))

  expect_error(
    dag_prune(.tdy_dag, c("y" = NA_character_)),
    class = "ggdag_type_error"
  )
  expect_error(dag_prune(.tdy_dag, c("y" = NA)), class = "ggdag_type_error")
  expect_error(dag_prune(.tdy_dag, c("y" = 1)), class = "ggdag_type_error")

  # a node-only row records its `to` as missing, and is not an edge to prune
  expect_equal(nrow(dplyr::filter(pull_dag_data(.tdy_dag), is.na(to))), 1)

  # the control: a real edge on the same DAG still prunes
  pruned_dag <- dag_prune(.tdy_dag, c("x" = "y"))
  expect_equal(n_edges(pruned_dag), 0)
  expect_setequal(unique(pull_dag_data(pruned_dag)$name), c("x", "y"))
})

test_that("dag_prune() carries the DAG's labels through", {
  withr::local_seed(1234)
  labels <- c("x" = "Exposure", "y" = "Outcome", "z" = "Confounder")
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z, labels = labels))

  pruned_dag <- dag_prune(.tdy_dag, c("z" = "x"))

  expect_equal(label(pull_dag(pruned_dag)), labels)
  dag_data <- pull_dag_data(pruned_dag)
  expect_equal(unique(dag_data$label[dag_data$name == "z"]), "Confounder")
})

test_that("dag_prune() rejects a bare spec that names two parallel edges", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x, x ~ ~y))
  expect_equal(n_edges(.tdy_dag), 2)

  expect_error(
    dag_prune(.tdy_dag, c("x" = "y")),
    class = "ggdag_ambiguous_edge_error"
  )

  # the reverse orientation names the bidirected edge alone, since the directed
  # edge is named in its own direction only
  reverse_pruned <- dag_prune(.tdy_dag, c("y" = "x"))
  kept <- dplyr::filter(pull_dag_data(reverse_pruned), !is.na(to))
  expect_equal(nrow(kept), 1)
  expect_equal(as.character(kept$direction), "->")
})

test_that("dag_prune() reports what a bare spec matched", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x, x ~ ~y))

  expect_ggdag_error(dag_prune(.tdy_dag, c("x" = "y")))
})

test_that("dag_prune() prunes the one edge a direction names", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x, x ~ ~y))

  directed_pruned <- dag_prune(
    .tdy_dag,
    data.frame(name = "x", to = "y", direction = "->")
  )
  kept <- dplyr::filter(pull_dag_data(directed_pruned), !is.na(to))
  expect_equal(nrow(kept), 1)
  expect_equal(as.character(kept$direction), "<->")

  bidirected_pruned <- dag_prune(
    .tdy_dag,
    data.frame(name = "x", to = "y", direction = "<->")
  )
  kept <- dplyr::filter(pull_dag_data(bidirected_pruned), !is.na(to))
  expect_equal(nrow(kept), 1)
  expect_equal(as.character(kept$direction), "->")

  # a bidirected edge has no direction of its own, so either orientation names it
  reversed_pruned <- dag_prune(
    .tdy_dag,
    data.frame(name = "y", to = "x", direction = "<->")
  )
  expect_equal(
    edge_signature(reversed_pruned),
    edge_signature(bidirected_pruned)
  )
})

test_that("dag_prune() takes a data frame of edges without a direction", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x + z, x ~ z))

  from_df <- dag_prune(.tdy_dag, data.frame(name = "z", to = "x"))
  from_vector <- dag_prune(.tdy_dag, c("z" = "x"))

  expect_equal(edge_signature(from_df), edge_signature(from_vector))
})

test_that("dag_prune() rejects an edges data frame it cannot read", {
  withr::local_seed(1234)
  .tdy_dag <- tidy_dagitty(dagify(y ~ x, x ~ ~y))

  expect_error(
    dag_prune(.tdy_dag, data.frame(from = "x", to = "y")),
    class = "ggdag_columns_error"
  )
  expect_error(
    dag_prune(.tdy_dag, data.frame(name = "x", to = "y", direction = "<-")),
    class = "ggdag_dag_error"
  )
  expect_error(
    dag_prune(.tdy_dag, data.frame(name = "x", to = NA_character_)),
    class = "ggdag_type_error"
  )
  # a direction the pair does not have names no edge
  expect_error(
    dag_prune(
      .tdy_dag,
      data.frame(name = "y", to = "x", direction = "->")
    ),
    class = "ggdag_missing_edges_error"
  )
})
