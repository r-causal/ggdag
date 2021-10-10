set.seed(1234)

test_that("repelled labels work", {
  g <- dagify(m ~ x + y,
              y ~ x,
              exposure = "x",
              outcome = "y",
              latent = "m",
              labels = c("x" = "Exposure", "y" = "Outcome", "m" = "Collider"))

  p1 <- g %>% tidy_dagitty() %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text_repel(aes(label = name), show.legend = FALSE)

  p2 <- g %>%
    tidy_dagitty() %>%
    dag_label(labels = c("x" = "This is the exposure",
                         "y" = "Here's the outcome",
                         "m" = "Here is where they collide")) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges() +
    geom_dag_point() +
    geom_dag_text() +
    geom_dag_label_repel(aes(label = label, fill = label),
                         col = "white", show.legend = FALSE)


  expect_doppelganger("geom_dag_text_repel() repels names", p1)
  expect_doppelganger("geom_dag_label_repel() repels labels", p2)
})

test_that("different edge types work", {
  p <- dagify(y ~ x + z2 + w2 + w1,
              x ~ z1 + w1,
              z1 ~ w1 + v,
              z2 ~ w2 + v,
              L ~ w1 + w2) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point() +
    geom_dag_text()

  expect_doppelganger("geom_dag_edges_link() is straight", p + geom_dag_edges_link())
  expect_doppelganger("geom_dag_edges_arc() is arcy", p + geom_dag_edges_arc())
  expect_doppelganger("geom_dag_edges_diagonal() is arcy", p + geom_dag_edges_diagonal())
  expect_doppelganger("geom_dag_edges_fan() is fany", p + geom_dag_edges_fan())
})
