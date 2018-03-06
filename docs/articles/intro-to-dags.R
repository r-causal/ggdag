## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 4, 
  fig.height = 4, 
  fig.align = "center",
  fig.dpi = 200,
  warning = FALSE,
  message = FALSE
)
set.seed(2939)

## ------------------------------------------------------------------------
library(ggdag)
dagify(y ~ x) %>% 
  ggdag()

## ------------------------------------------------------------------------
dagify(y ~~ x) %>% 
  ggdag()

## ------------------------------------------------------------------------
#  canonicalize the DAG: Add the latent variable in to the graph
dagify(y ~~ x) %>% 
  ggdag_canonical() 

## ------------------------------------------------------------------------
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

