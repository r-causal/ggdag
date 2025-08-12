test_that("time_ordered_coords with direction = 'y' works", {
  # Create a time-ordered data frame
  time_df <- data.frame(
    name = c("a", "b", "c", "d", "e"),
    time = c(1, 1, 2, 2, 3),
    stringsAsFactors = FALSE
  )

  # Test with direction = "y" (vertical layout)
  coords_y <- time_ordered_coords(time_df, direction = "y")

  # Check structure
  expect_s3_class(coords_y, "data.frame")
  expect_true(all(c("name", "x", "y") %in% names(coords_y)))

  # In vertical layout, time should vary by y, not x
  # Time 1 nodes should have same y
  time1_nodes <- coords_y[coords_y$name %in% c("a", "b"), ]
  expect_equal(length(unique(time1_nodes$y)), 1)

  # Different time points should have different y values
  expect_true(
    coords_y$y[coords_y$name == "a"] != coords_y$y[coords_y$name == "c"]
  )
  expect_true(
    coords_y$y[coords_y$name == "c"] != coords_y$y[coords_y$name == "e"]
  )
})

test_that("calculate_spread handles even number of nodes", {
  # Test with 4 nodes (even number)
  spread_even <- calculate_spread(4)
  expect_length(spread_even, 4)
  expect_equal(mean(spread_even), 0) # Should be centered around 0
  expect_equal(spread_even[1], -spread_even[4]) # Should be symmetric
  expect_equal(spread_even[2], -spread_even[3])

  # Test with 2 nodes
  spread_2 <- calculate_spread(2)
  expect_length(spread_2, 2)
  expect_equal(spread_2[1], -spread_2[2])

  # Test with 6 nodes
  spread_6 <- calculate_spread(6)
  expect_length(spread_6, 6)
  expect_equal(mean(spread_6), 0, tolerance = 1e-10)
})

test_that("auto_time_order with different sort_directions", {
  # DAG where right sort actually changes order values
  # a -> b -> c
  # a -> d
  # f -> c
  # d and c point to nodes not in the name column
  edges_df <- data.frame(
    name = c("a", "a", "b", "d", "f", "c"),
    to = c("b", "d", "c", "e", "c", "e"),
    stringsAsFactors = FALSE
  )

  result_left <- auto_time_order(edges_df, sort_direction = "left")
  result_right <- auto_time_order(edges_df, sort_direction = "right")

  # Extract orders for each node
  get_order <- function(df, node) df$order[df$name == node]

  # LEFT sort: standard topological order
  expect_equal(get_order(result_left, "a"), 1)
  expect_equal(get_order(result_left, "f"), 1)
  expect_equal(get_order(result_left, "b"), 2)
  expect_equal(get_order(result_left, "d"), 2)
  expect_equal(get_order(result_left, "c"), 3)

  # RIGHT sort: f gets pulled forward because it points to c
  # f initially has order 1, c has order 3
  # f's new order = min(3) - 1 = 2
  expect_equal(get_order(result_right, "a"), 1)
  expect_equal(get_order(result_right, "f"), 2) # Changed!
  expect_equal(get_order(result_right, "b"), 2)
  expect_equal(get_order(result_right, "d"), 2)
  expect_equal(get_order(result_right, "c"), 3)
})

test_that("time_ordered_coords with different time points", {
  time_df <- data.frame(
    name = c("a", "b", "c", "d", "e"),
    time = c(1, 1, 2, 3, 3),
    stringsAsFactors = FALSE
  )

  # Test basic functionality
  coords <- time_ordered_coords(time_df)

  # Check structure
  expect_s3_class(coords, "data.frame")
  expect_true(all(c("name", "x", "y") %in% names(coords)))

  # Time points should match input
  x_vals <- unique(coords$x)
  expect_equal(sort(x_vals), c(1, 2, 3))

  # Nodes at same time should have different y values
  time1_nodes <- coords[coords$x == 1, ]
  expect_equal(nrow(time1_nodes), 2)
  expect_true(time1_nodes$y[1] != time1_nodes$y[2])
})

test_that("time_ordered_coords only returns name, x, y columns", {
  # Add extra columns to the data frame
  time_df <- data.frame(
    name = c("a", "b", "c"),
    time = c(1, 1, 2),
    status = c("exposure", "latent", "outcome"),
    value = c(1.5, 2.5, 3.5),
    stringsAsFactors = FALSE
  )

  coords <- time_ordered_coords(time_df)

  # Should only have name, x, y columns
  expect_equal(names(coords), c("name", "x", "y"))
  expect_equal(nrow(coords), 3)
})

test_that("auto_time_order handles disconnected nodes", {
  # Create edges with a disconnected node
  edges_df <- data.frame(
    name = c("a", "b", "isolated"),
    to = c("b", "c", NA),
    stringsAsFactors = FALSE
  )

  result <- auto_time_order(edges_df)

  # All nodes should have order assigned
  expect_false(any(is.na(result$order)))

  # Isolated node should get an order value
  isolated_order <- result$order[result$name == "isolated"]
  expect_true(is.numeric(isolated_order))
})

test_that("auto_time_order handles complex DAG structures", {
  # Create a more complex DAG
  edges_df <- data.frame(
    name = c("w", "w", "x", "x", "y", "y", "z", "m"),
    to = c("x", "y", "m", "z", "m", "z", "outcome", "outcome"),
    stringsAsFactors = FALSE
  )

  result <- auto_time_order(edges_df)

  # Check that only nodes in "name" column are included
  unique_names <- unique(edges_df$name)
  expect_equal(sort(unique(result$name)), sort(unique_names))

  # Check order values make sense
  # w should have lower order than x and y (comes first)
  w_order <- result$order[result$name == "w"]
  x_order <- result$order[result$name == "x"]
  y_order <- result$order[result$name == "y"]

  if (length(w_order) > 0 && length(x_order) > 0 && length(y_order) > 0) {
    expect_true(w_order[1] < x_order[1])
    expect_true(w_order[1] < y_order[1])
  }

  # outcome should have highest order
  outcome_order <- result$order[result$name == "outcome"]
  if (length(outcome_order) > 0) {
    expect_equal(outcome_order[1], max(result$order))
  }
})

test_that("calculate_spread handles edge cases", {
  # Single node
  spread_1 <- calculate_spread(1)
  expect_length(spread_1, 1)
  expect_equal(spread_1, 0)

  # Zero nodes
  spread_0 <- calculate_spread(0)
  expect_length(spread_0, 0)

  # Large number of nodes
  spread_10 <- calculate_spread(10)
  expect_length(spread_10, 10)
  expect_equal(mean(spread_10), 0, tolerance = 1e-10)
  expect_true(all(diff(spread_10) > 0)) # Should be increasing
})

test_that("coords2list and coords2df are inverses", {
  # Create a coordinates data frame
  coords_df <- data.frame(
    name = c("a", "b", "c"),
    x = c(0, 1, 2),
    y = c(0, 1, 0),
    stringsAsFactors = FALSE
  )

  # Convert to list and back
  coords_list <- coords2list(coords_df)
  coords_df2 <- coords2df(coords_list)

  # Should be identical (possibly reordered)
  coords_df_sorted <- coords_df[order(coords_df$name), ]
  coords_df2_sorted <- coords_df2[order(coords_df2$name), ]

  expect_equal(coords_df_sorted$name, coords_df2_sorted$name)
  expect_equal(coords_df_sorted$x, coords_df2_sorted$x)
  expect_equal(coords_df_sorted$y, coords_df2_sorted$y)
})
