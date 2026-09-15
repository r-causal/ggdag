# a stable, printable summary of a DAG's edges, so a comparison that fails
# reports the edges rather than a wall of tibble columns
directed_edge_signature <- function(.tdy_dag) {
  dag_data <- pull_dag_data(.tdy_dag)
  edge_rows <- dag_data[!is.na(dag_data$to), , drop = FALSE]
  sort(paste(edge_rows$name, as.character(edge_rows$direction), edge_rows$to))
}

node_coords <- function(.tdy_dag, .name) {
  dag_data <- pull_dag_data(.tdy_dag)
  node_rows <- dag_data[dag_data$name == .name, c("x", "y")]
  unique(node_rows)
}

test_that("dag_saturate() keeps an edge-free node out of the saturation", {
  withr::local_seed(1234)
  dag <- dagitty::dagitty("dag{x -> y; z}")

  expect_no_message(expect_no_warning(saturated <- dag_saturate(dag)))

  # no node loss, in the data or in the recompiled DAG
  expect_setequal(unique(pull_dag_data(saturated)$name), c("x", "y", "z"))
  expect_setequal(names(pull_dag(saturated)), c("x", "y", "z"))

  # z has no directed edges, so its time position cannot be inferred and it
  # takes no part in the saturation: the only edge is the input's own
  expect_identical(directed_edge_signature(saturated), "x -> y")

  # z stays in the DAG as a node-only row
  z_rows <- dplyr::filter(pull_dag_data(saturated), name == "z")
  expect_true(nrow(z_rows) >= 1)
  expect_true(all(is.na(z_rows$to)))

  # the result is deterministic
  expect_identical(
    pull_dag_data(dag_saturate(dagitty::dagitty("dag{x -> y; z}"))),
    pull_dag_data(dag_saturate(dagitty::dagitty("dag{x -> y; z}")))
  )
})

test_that("dag_saturate() adds no edges among only edge-free nodes", {
  withr::local_seed(1234)
  dag <- dagitty::dagitty("dag{x z w}")

  expect_no_message(expect_no_warning(saturated <- dag_saturate(dag)))

  expect_setequal(unique(pull_dag_data(saturated)$name), c("x", "z", "w"))
  expect_setequal(names(pull_dag(saturated)), c("x", "z", "w"))
  expect_identical(directed_edge_signature(saturated), character(0))
})

test_that("dag_saturate() keeps a single-node DAG intact", {
  withr::local_seed(1234)
  dag <- dagitty::dagitty("dag{x}")

  expect_no_message(expect_no_warning(saturated <- dag_saturate(dag)))

  expect_identical(unique(pull_dag_data(saturated)$name), "x")
  expect_identical(names(pull_dag(saturated)), "x")
  expect_identical(directed_edge_signature(saturated), character(0))
})

test_that("time-ordered layout keeps isolated nodes without falling back", {
  withr::local_seed(1234)
  dags <- list(
    one_isolated = dagitty::dagitty("dag{x -> y; z}"),
    all_isolated = dagitty::dagitty("dag{x z w}"),
    single_node = dagitty::dagitty("dag{x}")
  )

  for (dag in dags) {
    expect_no_message(
      expect_no_warning(
        tidy_dag <- tidy_dagitty(dag, layout = "time_ordered")
      )
    )

    dag_data <- pull_dag_data(tidy_dag)
    expect_setequal(unique(dag_data$name), names(dag))
    expect_true(all(is.finite(dag_data$x)))
    expect_true(all(is.finite(dag_data$y)))

    # the layout is deterministic, not a random fallback
    expect_identical(
      dag_data,
      pull_dag_data(tidy_dagitty(dag, layout = "time_ordered"))
    )
  }
})

test_that("isolated nodes sit in the first layer at their own coordinates", {
  withr::local_seed(1234)
  tidy_dag <- tidy_dagitty(
    dagitty::dagitty("dag{x -> y; z}"),
    layout = "time_ordered"
  )
  dag_data <- pull_dag_data(tidy_dag)

  z_coords <- node_coords(tidy_dag, "z")
  x_coords <- node_coords(tidy_dag, "x")
  y_coords <- node_coords(tidy_dag, "y")

  # an isolated node has no parents, so it belongs to the first time point
  expect_equal(z_coords$x, min(dag_data$x))
  expect_equal(z_coords$x, x_coords$x)
  expect_lt(z_coords$x, y_coords$x)

  # but it does not sit on top of another node
  expect_false(isTRUE(all.equal(z_coords, x_coords, check.attributes = FALSE)))

  # a DAG of only isolated nodes shares a layer without overlapping
  all_isolated <- tidy_dagitty(
    dagitty::dagitty("dag{x z w}"),
    layout = "time_ordered"
  )
  isolated_data <- dplyr::distinct(
    pull_dag_data(all_isolated),
    name,
    x,
    y
  )
  expect_equal(length(unique(isolated_data$x)), 1)
  expect_equal(length(unique(isolated_data$y)), nrow(isolated_data))
})

test_that("time_ordered_coords() closure keeps isolated nodes", {
  withr::local_seed(1234)
  dags <- list(
    one_isolated = dagitty::dagitty("dag{x -> y; z}"),
    all_isolated = dagitty::dagitty("dag{x z w}"),
    single_node = dagitty::dagitty("dag{x}")
  )

  for (dag in dags) {
    expect_no_message(
      expect_no_warning(
        tidy_dag <- tidy_dagitty(dag, layout = time_ordered_coords())
      )
    )

    dag_data <- pull_dag_data(tidy_dag)
    expect_setequal(unique(dag_data$name), names(dag))
    expect_true(all(is.finite(dag_data$x)))
    expect_true(all(is.finite(dag_data$y)))
  }
})

test_that("dagify() with time_ordered_coords() is unchanged for connected DAGs", {
  withr::local_seed(1234)

  expect_no_message(
    expect_no_warning(
      tidy_dag <- dagify(y ~ x, coords = time_ordered_coords()) |>
        tidy_dagitty()
    )
  )

  dag_data <- pull_dag_data(tidy_dag)
  expect_setequal(unique(dag_data$name), c("x", "y"))
  expect_true(all(is.finite(dag_data$x)))
  expect_true(all(is.finite(dag_data$y)))
  expect_identical(directed_edge_signature(tidy_dag), "x -> y")
})

test_that("data frames with to = NA rows keep isolated nodes when laid out", {
  withr::local_seed(1234)
  dfs <- list(
    one_isolated = data.frame(name = c("x", "z"), to = c("y", NA)),
    all_isolated = data.frame(name = c("x", "z", "w"), to = NA_character_),
    single_node = data.frame(name = "x", to = NA_character_)
  )

  for (df in dfs) {
    expect_no_message(
      expect_no_warning(
        tidy_dag <- as_tidy_dagitty(df, layout = "time_ordered")
      )
    )

    all_nodes <- union(df$name, df$to[!is.na(df$to)])
    dag_data <- pull_dag_data(tidy_dag)
    expect_setequal(unique(dag_data$name), all_nodes)
    expect_setequal(names(pull_dag(tidy_dag)), all_nodes)
    expect_true(all(is.finite(dag_data$x)))
    expect_true(all(is.finite(dag_data$y)))
  }
})

test_that("coordinate regeneration keeps isolated nodes", {
  withr::local_seed(1234)
  withr::local_options(ggdag.layout = "time_ordered")

  tidy_dag <- as_tidy_dagitty(
    data.frame(name = c("x", "z"), to = c("y", NA)),
    layout = "time_ordered"
  )

  # a data frame with no coordinate columns forces a fresh layout
  update_dag_data(tidy_dag) <- data.frame(
    name = c("x", "z", "w"),
    to = c("y", NA, NA)
  )
  tidy_dag <- update_dag(tidy_dag)

  dag_data <- pull_dag_data(tidy_dag)
  expect_setequal(unique(dag_data$name), c("x", "y", "z", "w"))
  expect_setequal(names(pull_dag(tidy_dag)), c("x", "y", "z", "w"))
  isolated_rows <- dplyr::filter(dag_data, name %in% c("z", "w"))
  expect_true(all(is.finite(isolated_rows$x)))
  expect_true(all(is.finite(isolated_rows$y)))
})

test_that("dag_saturate() into the time-ordered layout keeps all nodes", {
  withr::local_seed(1234)
  saturated <- dag_saturate(dagitty::dagitty("dag{x -> y; z}"))

  expect_no_message(
    expect_no_warning(
      tidy_dag <- tidy_dagitty(pull_dag(saturated), layout = "time_ordered")
    )
  )

  dag_data <- pull_dag_data(tidy_dag)
  expect_setequal(unique(dag_data$name), c("x", "y", "z"))
  expect_setequal(names(pull_dag(tidy_dag)), c("x", "y", "z"))
  expect_true(all(is.finite(dag_data$x)))
  expect_true(all(is.finite(dag_data$y)))
  expect_identical(directed_edge_signature(tidy_dag), "x -> y")
})

test_that("the time-ordered layout is unchanged for a fully connected DAG", {
  withr::local_seed(1234)
  napkin <- function() {
    dagify(
      a ~ u1 + u2 + z,
      z ~ u1,
      m ~ a,
      y ~ u2 + m + a
    ) |>
      tidy_dagitty(layout = "time_ordered")
  }

  expect_no_message(expect_no_warning(first_run <- napkin()))
  second_run <- napkin()

  expect_setequal(
    unique(pull_dag_data(first_run)$name),
    c("u1", "u2", "z", "a", "m", "y")
  )
  expect_true(all(is.finite(pull_dag_data(first_run)$x)))
  expect_true(all(is.finite(pull_dag_data(first_run)$y)))
  expect_identical(pull_dag_data(first_run), pull_dag_data(second_run))
})
