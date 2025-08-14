# Test script for issue #175 fix
library(ggdag)
library(dagitty)

# Create the test DAG from the issue
dag <- dagify(
  y ~ x,
  x ~ z,
  z ~ w,
  labels = c(x = "X", y = "Y", z = "Z", w = "W"),
  exposure = "x",
  outcome = "y"
)

# Test the print output
cat("Testing tidy_dagitty print output:\n")
cat("==================================\n\n")

# This should now print everything together
tidy_dag <- tidy_dagitty(dag)
print(tidy_dag)

cat("\n\nTesting format output:\n")
cat("======================\n")
formatted <- format(tidy_dag)
cat(formatted, sep = "\n")