# library(igraph)
# g2 <- dagify(y ~ x + z2 + w2 + w1,
#              x ~ z1 + w1,
#              z1 ~ w1 + v,
#              z2 ~ w2 + v,
#              w1 ~~ w2)
# test_coords <- dagitty::edges(g2) %>% dplyr::select(v, w) %>% t() %>% c() %>% igraph::graph() %>% layout.fruchterman.reingold %>% as.data.frame()
# names(test_coords) <- c("x", "y")
# test_coords$name <- names(g2)
# g2 <- dagify(y ~ x + z2 + w2 + w1,
#              x ~ z1 + w1,
#              z1 ~ w1 + v,
#              z2 ~ w2 + v,
#              w1 ~~ w2,
#              exposure = "x",
#              outcome = "y", coords = test_coords)
# g2 %>% tidy_dagitty() %>% ggdag()
