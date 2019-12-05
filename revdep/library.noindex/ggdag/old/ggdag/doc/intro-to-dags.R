## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 4, 
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

## ---- fig.width = 4------------------------------------------------------
dagify(y ~ x) %>% 
  ggdag()

## ---- fig.width = 4------------------------------------------------------
dagify(y ~~ x) %>% 
  ggdag()

## ---- fig.width = 4------------------------------------------------------
#  canonicalize the DAG: Add the latent variable in to the graph
dagify(y ~~ x) %>% 
  ggdag_canonical() 

## ---- fig.width = 4------------------------------------------------------
dagify(y ~ x,
       x ~ a,
       a ~ y) %>% 
  ggdag() 

## ------------------------------------------------------------------------
smoking_ca_dag <- dagify(cardiacarrest ~ cholesterol,
       cholesterol ~ smoking + weight,
       smoking ~ unhealthy,
       weight ~ unhealthy,
       labels = c("cardiacarrest" = "Cardiac\n Arrest", 
                  "smoking" = "Smoking",
                  "cholesterol" = "Cholesterol",
                  "unhealthy" = "Unhealthy\n Lifestyle",
                  "weight" = "Weight"),
       latent = "unhealthy",
       exposure = "smoking",
       outcome = "cardiacarrest")

ggdag(smoking_ca_dag, text = FALSE, use_labels = "label")

## ------------------------------------------------------------------------
ggdag_paths(smoking_ca_dag, text = FALSE, use_labels = "label", shadow = TRUE)

## ------------------------------------------------------------------------
ggdag_adjustment_set(smoking_ca_dag, text = FALSE, use_labels = "label", shadow = TRUE)

## ---- fig.width = 4------------------------------------------------------
fever_dag <- collider_triangle(x = "Influenza", 
                  y = "Chicken Pox", 
                  m = "Fever") 

ggdag(fever_dag, text = FALSE, use_labels = "label")

## ------------------------------------------------------------------------
ggdag_dseparated(fever_dag, text = FALSE, use_labels = "label")

## ------------------------------------------------------------------------
ggdag_dseparated(fever_dag, controlling_for = "m", 
                 text = FALSE, use_labels = "label")

## ------------------------------------------------------------------------
dagify(fever ~ flu + pox, 
        acetaminophen ~ fever,
        labels = c("flu" = "Influenza",
                   "pox" = "Chicken Pox",
                   "fever" = "Fever",
                   "acetaminophen" = "Acetaminophen")) %>% 
ggdag_dseparated(from = "flu", to = "pox", controlling_for = "acetaminophen",
                 text = FALSE, use_labels = "label")

## ------------------------------------------------------------------------
ggdag_dseparated(smoking_ca_dag, controlling_for = c("weight", "cholesterol"), 
                 text = FALSE, use_labels = "label", collider_lines = FALSE)

