## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 5, 
  fig.align = "center",
  fig.dpi = 320,
  warning = FALSE,
  message = FALSE
)
set.seed(2939)

## ----set_theme-----------------------------------------------------------
#  set theme of all DAGs to `theme_dag()`
library(ggdag)
theme_set(theme_dag())

## ------------------------------------------------------------------------
confounder_triangle(x = "Coffee", y = "Lung Cancer", z = "Smoking") %>% 
  ggdag_dconnected(text = FALSE, use_labels = "label")

## ------------------------------------------------------------------------
coffee_dag <- dagify(cancer ~ smoking,
       smoking ~ addictive,
       coffee ~ addictive,
       exposure = "coffee",
       outcome = "cancer",
       labels = c("coffee" = "Coffee", "cancer" = "Lung Cancer", 
                  "smoking" = "Smoking", "addictive" = "Addictive \nBehavior")) %>% 
  tidy_dagitty(layout = "tree")

ggdag(coffee_dag, text = FALSE, use_labels = "label")

## ---- fig.width = 7.5----------------------------------------------------
ggdag_adjustment_set(coffee_dag, text = FALSE, use_labels = "label", shadow = TRUE)

## ------------------------------------------------------------------------
collider_triangle() %>% 
  ggdag_dseparated(controlling_for = "m")

## ------------------------------------------------------------------------
m_bias(x = "Education", y = "Diabetes", a = "Income during Childhood", 
       b = "Genetic Risk \nfor Diabetes", m = "Mother's Diabetes") %>% 
  ggdag(use_labels = "label")

## ---- fig.width = 7.5----------------------------------------------------
m_bias(x = "Education", y = "Diabetes", a = "Income during \nChildhood", 
       b = "Genetic Risk \nfor Diabetes", m = "Mother's Diabetes") %>% 
  ggdag_dseparated(controlling_for = "m", use_labels = "label")

## ------------------------------------------------------------------------
coords <- dagitty::coordinates(m_bias()) %>% 
  coords2df()
coords$name <- c("readiness", "pain", "surgery", "ready_tool", "pain_change")

surgical_dag <- dagify(ready_tool ~ readiness,
                       surgery ~ readiness + pain,
                       pain_change ~ ready_tool + pain,
                       exposure = "ready_tool",
                       outcome = "pain_change",
                       latent = "readiness",
                       labels = c(ready_tool = "Measured \nReadiness", 
                                  pain_change = "Change \nin Pain", 
                                  readiness = "Underlying \nReadiness", 
                                  pain = "Baseline \nPain", 
                                  surgery = "Surgical \nStatus"),
                       coords = coords2list(coords)) %>% 
  control_for("surgery")

ggdag_adjust(surgical_dag, text = FALSE, use_labels = "label", collider_lines = FALSE)

## ------------------------------------------------------------------------
ggdag_adjustment_set(surgical_dag, text = FALSE, use_labels = "label", shadow = TRUE)

## ------------------------------------------------------------------------
ggdag_butterfly_bias(edge_type = "diagonal")

## ------------------------------------------------------------------------
ggdag_adjustment_set(butterfly_bias(), shadow = TRUE)

## ------------------------------------------------------------------------
# set coordinates
coords <- tibble::tribble(
  ~name,            ~x,  ~y,
  "bladder_cancer",    1,   0,
  "vitamins",          0,   0,
  "diagnosed_bc",      1,   1,
  "recalled_vits",     0,   1,
  "bc_error",          1,   2,
  "vits_error",        0,   2,
)

bladder_dag <- dagify(diagnosed_bc ~ bc_error + bladder_cancer,
                      recalled_vits ~ vitamins + vits_error,
                      vits_error ~ bladder_cancer,
                      labels = c(bladder_cancer = "Bladder Cancer",
                                 vitamins = "Childhood Vitamin \nIntake",
                                 diagnosed_bc = "Diagnosed \nBladder Cancer",
                                 recalled_vits = "Memory of \nTaking Vitamins", 
                                 bc_error = "Measurement Error, \nDiagnosis",
                                 vits_error = "Measurement Error, \nVitamins"),
                      coords = coords)
ggdag(bladder_dag, text = FALSE, use_labels = "label")

## ------------------------------------------------------------------------
# set coordinates
coords <- tibble::tribble(
  ~name,              ~x,  ~y,
  "honors",            1,   3,
  "depression",        2,   3,
  "cesd",              2,   2,
  "baseline_error",    2,   1,
  "depression_change", 3,   3,
  "cesd_change",       3,   2,
  "followup_error",    3,   1
)

cesd_dag <- dagify(depression ~ honors,
       cesd ~ depression + baseline_error,
       cesd_change ~ depression_change + followup_error + baseline_error,
                      labels = c(honors = "Honors Degree",
                                 depression = "Depression",
                                 cesd = "CES-D",
                                 cesd_change = "Change \nin CES-D",
                                 depression_change = "Change in \nDepression", 
                                 baseline_error = "Measurement Error, \nBaseline",
                                 followup_error = "Measurement Error, \nFollow-up"),
       coords = coords)

cesd_dag %>% 
  ggdag_dconnected(from = "honors", to = "cesd_change", controlling_for = "cesd", 
                   text = FALSE, use_labels = "label", collider_lines = FALSE)

## ------------------------------------------------------------------------
coords <- tibble::tribble(
  ~name,           ~x,  ~y,
  "glioma",         1,   2,
  "hospitalized",   2,   3,
  "broken_bone",    3,   2,
  "reckless",       4,   1,
  "smoking",        5,   2
)

dagify(hospitalized ~ broken_bone + glioma,
       broken_bone ~ reckless,
       smoking ~ reckless,
       labels = c(hospitalized = "Hospitalization",
                  broken_bone = "Broken Bone",
                  glioma = "Glioma",
                  reckless = "Reckless \nBehavior",
                  smoking = "Smoking"),
       coords = coords) %>% 
  ggdag_dconnected("glioma", "smoking", controlling_for = "hospitalized", 
                   text = FALSE, use_labels = "label", collider_lines = FALSE)

## ------------------------------------------------------------------------
dagify(follow_up ~ symptoms,
       symptoms ~ new_rx + dx_severity,
       cd4 ~ dx_severity,
       labels = c(
         follow_up = "Follow-Up",
         symptoms = "Symptoms",
         new_rx = "New HIV Drug",
         dx_severity = "Underyling \nHIV Severity",
         cd4 = "CD4 Count"
       )) %>% 
  ggdag_adjust("follow_up", layout = "mds", text = FALSE, 
               use_labels = "label", collider_lines = FALSE)

