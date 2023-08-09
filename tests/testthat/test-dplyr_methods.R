test_that("dplyr methods work on tidy dags: `summarize()`", {
  library(dplyr, warn.conflicts = FALSE)
  x <- tidy_dagitty(m_bias()) %>%
    group_by(name) %>%
    summarize(n = n())

  expect_s3_class(x, "tbl")
  expect_length(x, 2)
  expect_equal(nrow(x), 5)
  expect_named(x, c("name", "n"))
})
