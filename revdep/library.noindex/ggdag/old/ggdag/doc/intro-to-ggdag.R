## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5, 
  fig.height = 5, 
  fig.align = "center",
  fig.dpi = 320,
  warning=FALSE,
  message=FALSE
)
set.seed(2939)

## ----dagitty-------------------------------------------------------------
library(dagitty)
library(ggdag)

dag <- dagitty("dag{y <- z -> x}")
tidy_dagitty(dag)

## ----dagify--------------------------------------------------------------
dagified <- dagify(x ~ z,
                   y ~ z,
                   exposure = "x",
                   outcome = "y")
tidy_dagitty(dagified)

## ----ggdag_layout--------------------------------------------------------
ggdag(dag, layout = "circle")

## ----dag_str-------------------------------------------------------------
tidy_dag <- tidy_dagitty(dagified)
str(tidy_dag)

## ----parents-------------------------------------------------------------
node_parents(tidy_dag, "x")

## ----pathways------------------------------------------------------------
bigger_dag <- dagify(y ~ x + a + b,
                     x ~ a + b,
                     exposure = "x",
                     outcome = "y")
#  automatically searches the paths between the variables labelled exposure and 
#  outcome
dag_paths(bigger_dag)  

## ------------------------------------------------------------------------
library(dplyr)
#  find how many variables are in between x and y in each path
bigger_dag %>% 
  dag_paths() %>%
  group_by(set) %>%
  filter(!is.na(path) & !is.na(name)) %>% 
  summarize(n_vars_between = n() - 1L)

## ----ggdag_path, fig.width=6.5-------------------------------------------
ggdag_paths(bigger_dag)

## ----ggdag_parents-------------------------------------------------------
ggdag_parents(bigger_dag, "x")

## ----ggdag_adjustment_---------------------------------------------------
#  quickly get the miniminally sufficient adjustment sets to adjust for when 
#  analyzing the effect of x on y
ggdag_adjustment_set(bigger_dag)

## ------------------------------------------------------------------------
bigger_dag %>% 
    node_parents("x") %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = parent)) +
      geom_dag_point() +
      geom_dag_edges() +
      geom_dag_text(col = "white") +
      theme_dag() +
      scale_color_hue(breaks  = c("parent", "child")) #  ignores NA in legend

## ------------------------------------------------------------------------
dagify(y ~ x,
       m ~ x + y) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point() +
    geom_dag_edges_arc() +
    geom_dag_text() +
    theme_dag()

## ----canonical-----------------------------------------------------------
dagify(y ~ x + z,
       x ~~ z) %>% 
    node_canonical() %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_dag_point() +
      geom_dag_edges_diagonal() +
      geom_dag_text() +
      theme_dag()

