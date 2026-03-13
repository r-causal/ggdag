set.seed(1234)
# Use a deterministic layout globally so snapshots don't drift between runs.
# Individual tests that need a different layout should set their own via
# withr::local_options(ggdag.layout = ...) and withr::local_seed(...).
options(ggdag.layout = "time_ordered")

# Enable to render purple dots at all fake repel points (edge + node disc)
# for visual review of ggrepel placement boundaries:
# options(ggdag.debug_repel_points = TRUE)

test_dag <- dagify(
  y ~ x + z2 + w2 + w1,
  x ~ z1 + w1,
  z1 ~ w1 + v,
  z2 ~ w2 + v,
  w1 ~ ~w2,
  exposure = "x",
  outcome = "y"
)

test_dag <- tidy_dagitty(test_dag)
