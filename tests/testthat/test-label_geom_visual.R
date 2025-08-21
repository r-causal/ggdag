# Set seed for reproducibility
set.seed(1234)

test_that("ggdag() visual output with different label geoms", {
  # Create a simple DAG with labels
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder")
  )

  # Test default behavior (geom_dag_label_repel)
  p_default <- ggdag(dag, use_labels = TRUE)
  expect_doppelganger("ggdag default label_repel", p_default)

  # Test with static labels
  p_static <- ggdag(dag, use_labels = TRUE, label_geom = geom_dag_label)
  expect_doppelganger("ggdag static labels", p_static)

  # Test with text repel (no boxes)
  p_text <- ggdag(dag, use_labels = TRUE, label_geom = geom_dag_text_repel)
  expect_doppelganger("ggdag text repel", p_text)
})

test_that("ggdag_adjustment_set() visual output with different label geoms", {
  dag <- dagify(
    y ~ x + z + w,
    x ~ z + w,
    exposure = "x",
    outcome = "y",
    labels = c(
      x = "Treatment",
      y = "Outcome",
      z = "Confounder 1",
      w = "Confounder 2"
    )
  )

  # Test with default repelled labels
  p_default <- ggdag_adjustment_set(dag, use_labels = TRUE)
  expect_doppelganger("adjustment_set default labels", p_default)

  # Test with static labels
  p_static <- ggdag_adjustment_set(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_doppelganger("adjustment_set static labels", p_static)
})

test_that("ggdag_paths() visual output with different label geoms", {
  dag <- dagify(
    y ~ x + m,
    m ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(x = "X", y = "Y", z = "Z", m = "Mediator")
  )

  # Test with default repelled labels
  p_default <- ggdag_paths(dag, use_labels = TRUE)
  expect_doppelganger("paths default labels", p_default)

  # Test with text without boxes
  p_text <- ggdag_paths(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_doppelganger("paths text repel", p_text)
})

test_that("ggdag_collider() visual output with different label geoms", {
  dag <- dagify(
    m ~ x + y,
    y ~ x,
    labels = c(x = "Exposure", y = "Outcome", m = "Collider")
  )

  # Test with default repelled labels
  p_default <- ggdag_collider(dag, use_labels = TRUE)
  expect_doppelganger("collider default labels", p_default)

  # Test with static labels
  p_static <- ggdag_collider(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_doppelganger("collider static labels", p_static)

  # Test with ggrepel2 labels
  p_repel2 <- ggdag_collider(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_label_repel2
  )
  expect_doppelganger("collider label_repel2", p_repel2)
})

test_that("ggdag_status() visual output with different label geoms", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    z ~ w,
    exposure = "x",
    outcome = "y",
    latent = "w",
    labels = c(
      x = "Exposure",
      y = "Outcome",
      z = "Measured\nConfounder",
      w = "Unmeasured\nConfounder"
    )
  )

  # Test with default labels
  p_default <- ggdag_status(dag, use_labels = TRUE)
  expect_doppelganger("status default labels", p_default)

  # Test with text repel2
  p_text2 <- ggdag_status(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_text_repel2
  )
  expect_doppelganger("status text_repel2", p_text2)
})

test_that("quick plot functions visual output with label geoms", {
  # Test m_bias with labels
  p_mbias <- ggdag_m_bias(
    x = "Treatment",
    y = "Outcome",
    m = "Collider",
    a = "Cause A",
    b = "Cause B",
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_doppelganger("m_bias static labels", p_mbias)

  # Test confounder triangle with text
  p_conf <- ggdag_confounder_triangle(
    x = "Exposure",
    y = "Outcome",
    z = "Confounder",
    use_labels = TRUE,
    label_geom = geom_dag_text_repel
  )
  expect_doppelganger("confounder_triangle text labels", p_conf)
})

test_that("label geoms work with faceted plots", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    labels = c(x = "X", y = "Y", z = "Z")
  )

  # Test equivalent DAGs with labels
  p_equiv <- ggdag_equivalent_dags(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_label
  )
  expect_doppelganger("equivalent_dags faceted labels", p_equiv)
})

test_that("label size and color parameters work correctly", {
  dag <- dagify(
    y ~ x + z,
    x ~ z,
    exposure = "x",
    outcome = "y",
    labels = c(x = "Exposure", y = "Outcome", z = "Confounder")
  )

  # Test with custom label size and color
  p_custom <- ggdag(
    dag,
    use_labels = TRUE,
    label_geom = geom_dag_label,
    label_size = 5,
    label_col = "blue"
  )
  expect_doppelganger("ggdag custom label params", p_custom)
})
