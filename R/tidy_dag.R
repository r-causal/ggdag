library(dagitty)
library(dplyr)
library(ggplot2)
library(tibble)
library(ggnetwork)
library(purrr)

g2 <- dagitty( "dag {
    Y <- X <- Z1 <- V -> Z2 -> Y
               Z1 <- W1 <-> W2 -> Z2
               X <- W1 -> Y
               X <- W2 -> Y
               X [exposure]
               Y [outcome]
               }")


g2_dag <- as.dagitty(graphLayout(g2))
plot(g2_dag)
x <- g2_dag

`label<-` <- function(x, values) {
  UseMethod("label<-")
}

`label<-.dagitty` <- function(x, values) {
  attr(x, "labels") <- values
  x
}

`label<-.tidy_dagitty` <- function(x, values) {
  attr(x, "labels") <- values
  
  if (!is.null(x$data[["label"]])) x$data <- x$data %>% dplyr::select(-label)
  
  x$data <- left_join(x$data, tibble::enframe(values, value = "label"), by = "name")
  x
}

dag_label <- function(.tdy_dag, labels = NULL) {
  
  if (!is.null(labels)) .tdy_dag$data <- .tdy_dag$data %>% dplyr::select(-label)
  if (is.null(labels)) labels <- label(.tdy_dag$dag)
  if (is.null(labels)) { warning("no labels provided"); return(.tdy_dag); }
  
  .tdy_dag$data <- left_join(.tdy_dag$data, tibble::enframe(labels, value = "label"), by = "name")
  
  
  .tdy_dag
}

label <- function(.tdy_dag) {
  attr(.tdy_dag, "labels")
}

has_labels <- function(.tdy_dag) {
  !is.null(attr(.tdy_dag, "labels"))
}


# dag_control_point <- function(x, y, xend, yend, acode, asp) {
#   dagitty:::.autoControlPoint(x, y, xend, yend, asp, 0.2 * as.integer(acode == 3))
# }

# dag_line_segment <- function(.x, .y, .wx, .wy, .a, .b, .c, .d, scale_by = .5) {
# 
#   .args <- list(.x, .y, .wx, .wy, .a, .b, .c, .d, scale_by)
#   
#   purrr::pmap_df(.args, function(.x, .y, .wx, .wy, .a, .b, .c, .d, scale_by) {
#     .vals <- c(.x, .y, .wx, .wy, .a, .b, .c, .d)
#     if (any(is.na(.vals))) return(list(x = NA, y = NA))
#     results <- suppressWarnings(dagitty:::.lineSegBoxIntersect(.x - (.wx/scale_by), 
#                                                     .y - (.wy/scale_by), 
#                                                     .x + (.wx/scale_by), 
#                                                     .y + (.wy/scale_by), 
#                                                     .a, .b, .c, .d))
#     need_warning <- is.null(results)
#     iter <- 1
#     while (is.null(results) & iter <= 100) {
#       scale_by <- scale_by + .1
#       results <- suppressWarnings(dagitty:::.lineSegBoxIntersect(.x - (.wx/scale_by), 
#                                                                  .y - (.wy/scale_by), 
#                                                                  .x + (.wx/scale_by), 
#                                                                  .y + (.wy/scale_by), 
#                                                                  .a, .b, .c, .d))
#       iter = iter + 1
#     }
#     
#     if (need_warning) warning(paste("`scale_by` resized to ", round(1/scale_by, 3)), call. = FALSE)
#     results
#   })
# }

tidy_dagitty <- function(.dagitty, scale_by = .5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  dagitty:::.supportsTypes(.dagitty, c("dag"))
  .dag <- .dagitty
  
  no_existing_coords <- coordinates(.dagitty) %>% 
    purrr::map_lgl(~all(is.na(.x))) %>% 
    all()
  
  if (no_existing_coords) .dagitty <- as.dagitty(graphLayout(.dagitty))

  coords <- coordinates(.dagitty)
  labels <- names(coords$x)

  # wx <- purrr::map_df(labels, ~tibble(name = .x, wx = strwidth(paste0("xxxxxx", .x), "figure", cex = 2))) 
  # wy <- purrr::map_df(labels, ~tibble(name = .x, wy = strheight(paste0("\n\n\n", .x), "figure", cex = 2))) 
  # asp <- par("pin")[1]/diff(par("usr")[1:2])/(par("pin")[2]/diff(par("usr")[3:4]))
  # 
  # scale_by <- 1/scale_by
  dag_edges <- dagitty::edges(.dagitty)
  
  tidy_dag <- left_join(tibble::enframe(coords$x, value = "x"),
                        tibble::enframe(coords$y, value = "y"),
                        by = "name")
  
  tidy_dag <- dag_edges %>% 
    select(-x, -y) %>% 
    mutate(v = as.character(v), 
           w = as.character(w),
           direction = factor(e, levels = c("<-", "->", "<->"), exclude = NA), 
           type = ifelse(e == "<->", "bidirected", "directed"),
           type = factor(type, levels = c("directed", "bidirected"), exclude = NA)) %>% 
    left_join(tidy_dag, ., by = c("name" = "v")) %>% 
    left_join(tidy_dag, by = c("w" = "name"),  suffix = c("", "end")) %>% 
    rename(going_to = w) %>% 
    select(-e) %>% 
    mutate(y = -y, yend = -yend) 
  
  # tidy_dag <- dag_edges %>% 
  #   rename(cp_x = x, cp_y = y) %>% 
  #   mutate(v = as.character(v), w = as.character(w)) %>% 
  #   left_join(tidy_dag, ., by = c("name" = "v")) %>% 
  #   left_join(tidy_dag, by = c("w" = "name"),  suffix = c("", "end")) %>% 
  #   mutate(acode = ifelse(e == "<->", 3, ifelse(e == "--", 0, 2)),
  #          acode = factor(acode, levels = 0:3, labels = c("undirected", "?", "directed", "bidirected")),
  #          cp_x = ifelse(is.na(cp_x),
  #                        dag_control_point(x, y, xend, yend, acode, asp)$x,
  #                        cp_x),
  #          cp_y = ifelse(is.na(cp_y),
  #                        dag_control_point(x, y, xend, yend, acode, asp)$y,
  #                        cp_y)) %>% 
  #   left_join(wx, by = "name") %>% 
  #   left_join(wy, by = "name") %>% 
  #   left_join(wx, by = c("w" = "name"),  suffix = c("", "end")) %>% 
  #   left_join(wy, by = c("w" = "name"),  suffix = c("", "end")) %>% 
  #   mutate(b_x = dag_line_segment(x, y, wx, wy, x, y, cp_x, cp_y, scale_by)$x,
  #          b_y = dag_line_segment(x, y, wx, wy, x, y, cp_x, cp_y, scale_by)$y,
  #          b_xend = dag_line_segment(xend, yend, wxend, wyend, cp_x, cp_y, xend, yend, scale_by)$x,
  #          b_yend = dag_line_segment(xend, yend, wxend, wyend, cp_x, cp_y, xend, yend, scale_by)$y,
  #          direction = factor(e, levels = c("<-", "->", "<->"), exclude = NA), 
  #          type = factor(as.character(acode), levels = c("directed", "bidirected"), exclude = NA)) %>% 
  #   rename(going_to = w) %>% 
  #   select(name, node_x = x, node_y = y, x = b_x, y = b_y, going_to, -e, -acode, direction, type, xend = b_xend, yend = b_yend) %>% 
  #   mutate(node_y = -node_y, y = -y, yend = -yend) 
  
  .tdy_dag <- list(data = tidy_dag, dag = .dag)
  class(.tdy_dag) <- "tidy_dagitty"
  if (has_labels(.dag)) {
    label(.tdy_dag) <- label(.dag)
  }

    .tdy_dag
}

is.tidy_dagitty <- function(x) is(x, "tidy_dagitty")

tidy.tidy_dagitty <- function(.tdy_dag) {
  .tdy_dag$data
}

fortify.tidy_dagitty <- function(.tdy_dag) {
  .tdy_dag$data
}

as.data.frame.tidy_dagitty <- function(.tdy_dag, ...) {
  as.data.frame(.tdy_dag$data, ...)
}

tbl_df.tidy_daggity <- function(.tdy_dag) {
  .tdy_dag$data
}

as.tbl.tidy_daggity <- function(.tdy_dag, ...) {
  as.tbl(.tdy_dag$data, ...)
}

print.tidy_dagitty <- function(.tdy_dag) {
  print(.tdy_dag$data)
}

adjustment_sets <- function(.tdy_dag, exposure = NULL, outcome = NULL, ...) {
  sets <- adjustmentSets(.tdy_dag$dag, exposure = exposure, outcome = outcome, ...)
  is_empty_set <- purrr::is_empty(sets)
  if (is_empty_set) stop("dagitty failed to process sets. Check that it is a DAG with `IsAcyclic()`")
  sets <- sets %>% 
    capture.output() %>% 
    stringr::str_replace(" \\{\\}", "(Unconditionally Independent)") %>% 
    stringr::str_replace("\\{ ", "") %>% 
    stringr::str_replace(" \\}", "") %>% 
    purrr::map(~stringr::str_split(.x, ", ") %>% 
    purrr::pluck(1))
  
  
  .tdy_dag$data <- 
    purrr::map_df(sets, 
                  ~mutate(.tdy_dag$data, adjusted = ifelse(name %in% .x, "adjusted", "unadjusted"), set = paste(.x, collapse = ", ")) 
    )
  
  .tdy_dag
}

if_not_tidy_daggity <- function(.dagitty) {
  if (!is.tidy_dagitty(.dagitty)) return(tidy_dagitty(.dagitty))
  .dagitty
}

tidy_dag <- tidy_dagitty(g2)

ggdag <- function(.tdy_dag, scale_by = .5) {
  if_not_tidy_daggity(.tdy_dag) %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
      geom_dag_edges(scale_by = scale_by) +
      geom_dag_node() +
      geom_dag_text() +
      theme_dag() 
}

ggdag(randomDAG(10, .5))

tidy_dag %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text() +
    theme_dag() 

ggdag_adjustment_set <- function(.tdy_dag, scale_by = .5, exposure = NULL, outcome = NULL, ...) {
  if_not_tidy_daggity(.tdy_dag) %>% 
    adjustment_sets(exposure = exposure, outcome = outcome, ...) %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = adjusted)) + 
      geom_dag_edges(aes(alpha = adjusted), scale_by = scale_by) +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      facet_wrap(~set) +
      theme_dag() 
}

ggdag_adjustment_set(randomDAG(10, .5), scale_by = .5, exposure = "x3", outcome = "x5")

tidy_dagitty(g2) %>% adjustment_sets() %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = adjusted)) + 
    geom_dag_edges(aes(alpha = adjusted), scale = .4) +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    facet_wrap(~set) +
    theme_dag()

formula2char <- function(fmla) {
  char_fmla <- as.character(fmla)
  rhs_vars <- char_fmla[[3]] %>% stringr::str_split(" \\+ ") %>% purrr::pluck(1)
  bidirectional <- any(stringr::str_detect(rhs_vars, "~"))
  rhs_vars <- stringr::str_replace_all(rhs_vars, "~", "")
  arrows <- ifelse(bidirectional, "<->", "<-")
  char_dag_fmla <- rhs_vars %>% map_chr(~paste(char_fmla[[2]], arrows, .x))
  paste(char_dag_fmla, collapse = "\n")
}

dagify <- function(..., exposure = NULL, outcome = NULL, latent = NULL, labels = NULL, coords = NULL) {
  fmlas <- list(...)
  dag_txt <- purrr::map_chr(fmlas, formula2char)
  dag_txt <- paste(dag_txt, collapse = "\n") %>% 
    paste("dag {", ., "}")
  dgty <- dagitty::dagitty(dag_txt)
  if (!is.null(exposure)) dagitty::exposures(dgty) <- exposure
  if (!is.null(outcome)) dagitty::outcomes(dgty) <- outcome
  if (!is.null(latent)) dagitty::latents(dgty) <- latent
  if (!is.null(labels)) label(dgty) <- labels
  if (!is.null(coords)) {
    if (is.data.frame(coords)) {
      coordinates(dgty) <- coords2list(coords) 
    } else if (is.list(coords)) {
      coordinates(dgty) <- coords
    } else {
    stop("`coords` must be of class `list` or `data.frame`")
    }
  }
  dgty
}

g <- dagitty("dag{ x-> m <- y <-x }") # collider
exposures(g) <- "x"
outcomes(g) <- "y"
latents(g) <- "m"
adjustmentSets(g)
plot(graphLayout(g))

g <- dagify(m ~ x + y, 
            y ~ x,
            exposure = "x",
            outcome = "y",
            latent = "m",
            labels = c("x" = "Exposure", "y" = "Outcome", "m" = "Collider")) # collider


g %>% tidy_dagitty() %>% 
  dag_label(labels = c("x" = "This is the exposure", "y" = "Here's the outcome", "m" = "Here is where they collide")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
    geom_dag_edges(scale_by = .3) +
    geom_dag_node() +
    geom_dag_text() +
    geom_dag_label_repel(aes(label = label, fill = label), col = "white", show.legend = FALSE) +
    theme_dag() 

g2 <- dagify(y ~ x + z2 + w2 + w1,
             x ~ z1 + w1, 
             z1 ~ w1 + v,
             z2 ~ w2 + v,
             w1 ~~ w2,
            exposure = "x",
            outcome = "y")

ggdag(g2)

ggdag_adjustment_set(g2)

is_collider <- function(.dag, .var) {
  if (is.tidy_dagitty(.dag)) .dag <- .dag$dag
  n_parents <- parents(.dag, .var)
  length(n_parents) > 1
}

is_collider(g, "m")

is_acyclic <- function(g) isAcyclic(g)
is_acyclic(g)

is_confounder <- function(g, z, x, y, direct = FALSE) {
   if (direct) {
     z_descendants <- dagitty::children(g, z)
   } else {
     z_descendants <- dagitty::descendants(g, z)[-1]
   }
   all(c(x, y) %in% z_descendants)
}

is_confounder(g2, "v", "x", "y")

node_collider <- function(.tdy_dag, as_factor = TRUE) {
  vars <- unique(.tdy_dag$data$name)
  colliders <- purrr::map_lgl(vars, ~is_collider(.tdy_dag, .x))
  names(colliders) <- vars
  .tdy_dag$data <- left_join(.tdy_dag$data, 
                             tibble::enframe(colliders, value = "colliders"))
  purrr::map(vars[colliders], ~dagitty::parents(.tdy_dag$dag, .x))
  if (as_factor) .tdy_dag$data <- mutate(.tdy_dag$data, colliders = factor(as.numeric(colliders), levels = 0:1, labels = c("Non-Collider", "Collider")))
  .tdy_dag
}

ggdag_collider <- function(.tdy_dag, scale_by = .5, ...) {
  if_not_tidy_daggity(.tdy_dag) %>% 
    node_collider(...) %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = forcats::fct_rev(colliders))) + 
      geom_dag_edges(scale_by = scale_by) +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() 
}

g %>% tidy_dagitty() %>% node_collider() %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = forcats::fct_rev(colliders))) + 
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    geom_dag_label_repel(aes(label = label, fill = forcats::fct_rev(colliders)), col = "white", show.legend = FALSE) +
    theme_dag() 

ggdag_collider(g) + geom_dag_label_repel(aes(label = label, fill = forcats::fct_rev(colliders)), col = "white", show.legend = FALSE)


#  switch to tidystringdist::tidy_comb_all()?
unique_pairs <- function(x, exclude_identical = TRUE) {
  pairs <- expand.grid(x, x) %>% purrr::map_dfc(as.character)
  if (exclude_identical) pairs <- pairs %>% dplyr::filter(Var1 != Var2)
  pairs[!duplicated(t(apply(pairs, 1, sort))), ] 
}

activate_collider_paths <- function(.tdy_dag, adjust_for) {
  vars <- unique(.tdy_dag$data$name)
  colliders <- purrr::map_lgl(vars, ~is_collider(.tdy_dag, .x))
  collider_names <- vars[colliders]
  
  if (!any((collider_names %in% adjust_for))) return(.tdy_dag)
  adjusted_colliders <- collider_names[collider_names %in% adjust_for]
  collider_paths <- purrr::map(adjusted_colliders, ~dagitty::parents(.tdy_dag$dag, .x))
  
  activated_pairs <- purrr::map(collider_paths, unique_pairs)
  
  collider_lines <- purrr::map_df(activated_pairs, function(.pairs_df) {
    .pairs_df %>% rowwise() %>% do({
      df <- .
      name <- df[["Var1"]]
      going_to <- df[["Var2"]]
      start_coords <- .tdy_dag$data %>% filter(name == df[["Var1"]]) %>% select(x, y) %>% slice(1)
      end_coords <- .tdy_dag$data %>% filter(name == df[["Var2"]]) %>% select(x, y) %>% slice(1)
      
      .tdy_dag$data %>% add_row(.before = 1, name = name, x = start_coords[[1, 1]], y = start_coords[[1, 2]],
                                going_to = going_to, xend = end_coords[[1, 1]], yend = end_coords[[1, 2]], 
                                direction = factor("<->", levels = c("<-", "->", "<->"), exclude = NA), 
                                type = factor("bidirected", levels = c("directed", "bidirected"), exclude = NA)) %>% slice(1)
    })
  })
  collider_lines$collider_line <- TRUE
  .tdy_dag$data$collider_line <- FALSE
  .tdy_dag$data <- bind_rows(.tdy_dag$data, collider_lines)
  .tdy_dag
}

control_for <- function(.tdy_dag, adjust_for, as_factor = TRUE) {
  .tdy_dag <- activate_collider_paths(.tdy_dag, adjust_for)
  .tdy_dag$data <- mutate(.tdy_dag$data, adjusted = ifelse(name %in% adjust_for, "adjusted", "unadjusted"))
  if (as_factor) .tdy_dag$data <- mutate(.tdy_dag$data, adjusted = factor(adjusted, exclude = NA))
  .tdy_dag
}

ggdag_adjust <- function(.tdy_dag, adjust_for, scale_by = .5, ...) {
  if_not_tidy_daggity(.tdy_dag) %>% 
    control_for(adjust_for, ...) %>%   
      ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted)) + 
      geom_dag_edges(aes(alpha = adjusted), scale_by = scale_by) +
      geom_dag_collider_edges() +
      geom_dag_node() +
      geom_dag_text() +
      theme_dag() 
}

dagify(m ~ a + b, x ~ a, y ~ b) %>% ggdag_adjust(adjust_for = "m")

dagify(m ~ a + b, x ~ a, y ~ b) %>% tidy_dagitty(scale_by = 3.5) %>% control_for("m") %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted)) + 
    geom_dag_edges(aes(alpha = adjusted)) +
    geom_dag_collider_edges() +
    geom_dag_node() +
    geom_dag_text() +
    theme_dag() 

dagify(m ~ a + b, x ~ a, y ~ b + x) %>% ggdag_adjust(adjust_for = "m")

coords2df <- function(coord_list) {
  coord_df <- map(coord_list, tibble::enframe) %>% reduce(left_join, by = "name")
  names(coord_df) <- c("name", "x", "y")
  coord_df
}

coords2list <- function(coord_df) {
  x <- coord_df %>% select(name, x) %>% tibble::deframe()     
  y <- coord_df %>% select(name, y) %>% tibble::deframe()     
  list(x = x, y = y)
}

coords <- list(x = c(A = 1, B = 2, D = 3, C = 3, F = 3, E = 4, G = 5, H = 5, I = 5),
               y = c(A = 0, B = 0, D = 1, C = 0, F = -1, E = 0, G = 1, H = 0, I = -1))
coord_df <- coords2df(coords)
coords2list(coord_df)

x <- dagitty('dag{
G <-> H <-> I <-> G
             D <- B -> C -> I <- F <- B <- A
             H <- E <- C -> G <- D
             }')
coordinates(x) <- coords2list(coord_df)

plot(x)

x %>% tidy_dagitty() %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text() +
    ggnetwork::theme_blank()


dagify(G ~~ H,
       G ~~ I,
       I ~~ G,
       H ~~ I,
       D ~ B,
       C ~ B,
       I ~ C + F,
       F ~ B,
       B ~ A,
       H ~ E,
       C ~ E + G,
       G ~ D, coords = coord_df) %>% 
  tidy_dagitty() %>% node_collider() %>% control_for("C") %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = colliders, shape = adjusted)) + 
      geom_dag_edges() +
      geom_dag_collider_edges() +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag(expand_x = waiver())

node_status <- function(.tdy_dag, as_factor = TRUE) {
  .exposures <- dagitty::exposures(.tdy_dag$dag)
  .outcomes <- dagitty::outcomes(.tdy_dag$dag)
  .latents <- dagitty::latents(.tdy_dag$dag)
  .tdy_dag$data <- mutate(.tdy_dag$data, 
                          status = ifelse(name %in% .exposures, "exposure",
                                          ifelse(name %in% .outcomes, "outcome",
                                                 ifelse(name %in% .latents, "latent",
                                                        NA))))
  if (as_factor) .tdy_dag$data$status <- factor(.tdy_dag$data$status, exclude = NA)
  .tdy_dag
}

ggdag_status <- function(.tdy_dag, scale_by = .5, ...) {
  if_not_tidy_daggity(.tdy_dag) %>% 
    node_status(...) %>%   
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = status)) + 
    geom_dag_edges(scale_by = scale_by) +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag(breaks = c("exposure", "outcome", "latent")) 
}

ggdag_status(tidy_dag)

tidy_dag %>% node_status() %>% adjustment_sets() %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = status, shape = adjusted)) + 
    geom_dag_edges(aes(alpha = adjusted)) +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    geom_dag_label_repel(aes(label = status, fill = status), col = "white", nudge_x = 1, nudge_y = 1, show.legend = FALSE) +
    facet_wrap(~set) + 
    theme_dag(breaks = c("exposure", "outcome", "latent"))

node_children <- function(.tdy_dag, .var, as_factor = TRUE) {
  .children <- dagitty::children(.tdy_dag$dag, .var)
  .tdy_dag$data <- mutate(.tdy_dag$data, 
                          children = ifelse(name %in% .children, "child",
                                          ifelse(name == .var, "parent",
                                                        NA)))
  if (as_factor) .tdy_dag$data$children <- factor(.tdy_dag$data$children, exclude = NA)
  .tdy_dag
}

ggdag_children <- function(.tdy_dag, .var, scale_by = .5, ...) {
  if_not_tidy_daggity(.tdy_dag) %>% 
    node_children(.var, ...) %>%   
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = children)) + 
      geom_dag_edges(scale_by = scale_by) +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag(breaks  = c("parent", "child"))
}

ggdag_children(tidy_dag, "W1")

tidy_dag %>% node_children("W1") %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = children)) + 
  geom_dag_edges() +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  geom_dag_label_repel(aes(label = children, fill = children), col = "white", show.legend = FALSE) +
  theme_dag(breaks  = c("parent", "child"))

node_parents <- function(.tdy_dag, .var, as_factor = TRUE) {
  .parent <- dagitty::parents(.tdy_dag$dag, .var)
  .tdy_dag$data <- mutate(.tdy_dag$data, 
                          parent = ifelse(name %in% .parent, "parent",
                                            ifelse(name == .var, "child",
                                                   NA)))
  if (as_factor) .tdy_dag$data$parent <- factor(.tdy_dag$data$parent, exclude = NA)
  .tdy_dag
}

ggdag_parents <- function(.tdy_dag, .var, scale_by = .5, ...) {
  if_not_tidy_daggity(.tdy_dag) %>% 
    node_parents(.var, ...) %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = parent)) + 
      geom_dag_edges(scale_by = scale_by) +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag(breaks  = c("parent", "child"))
}

ggdag_parents(tidy_dag, "Y")

node_ancestors <- function(.tdy_dag, .var, as_factor = TRUE) {
  .ancestors <- dagitty::ancestors(.tdy_dag$dag, .var)[-1]
  .tdy_dag$data <- mutate(.tdy_dag$data, 
                          ancestor = ifelse(name %in% .ancestors, "ancestor",
                                          ifelse(name == .var, "descendant",
                                                 NA)))
  if (as_factor) .tdy_dag$data$ancestor <- factor(.tdy_dag$data$ancestor, exclude = NA)
  .tdy_dag
}

ggdag_ancestors <- function(.tdy_dag, .var, scale_by = .5, ...) {
  if_not_tidy_daggity(.tdy_dag) %>% 
    node_ancestors(.var, ...) %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = ancestor)) + 
    geom_dag_edges(scale_by = scale_by) +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag(breaks  = c("ancestor", "descendant"))
}

ggdag_ancestors(tidy_dag, "X")

node_descendants <- function(.tdy_dag, .var, as_factor = TRUE) {
  .descendants <- dagitty::descendants(.tdy_dag$dag, .var)[-1]
  .tdy_dag$data <- mutate(.tdy_dag$data, 
                          descendant = ifelse(name %in% .descendants, "descendant",
                                            ifelse(name == .var, "ancestor",
                                                   NA)))
  if (as_factor) .tdy_dag$data$descendant <- factor(.tdy_dag$data$descendant, exclude = NA)
  .tdy_dag
}

ggdag_descendants <- function(.tdy_dag, .var, scale_by = .5, ...) {
  if_not_tidy_daggity(.tdy_dag) %>% 
    node_descendants(.var, ...) %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = descendant)) + 
    geom_dag_edges(scale_by = scale_by) +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag(breaks  = c("ancestor", "descendant"))
}

ggdag_descendants(tidy_dag, "W1")

tidy_dag %>% node_parents("Y") %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = parent)) + 
  geom_dag_edges() +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  geom_dag_label_repel(aes(label = parent, fill = parent), col = "white", show.legend = FALSE) +
  theme_dag(breaks  = c("parent", "child"))

node_dconnected <- function(.tdy_dag, from, to, controlling_for = NULL, as_factor = TRUE) {
  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag$data$collider_line <- FALSE
    .tdy_dag$data$adjusted <- "unadjusted"
    controlling_for <- c()
  }
  
  .dconnected <- dagitty::dconnected(.tdy_dag$dag, from, to, controlling_for)
  
  .tdy_dag$data <- mutate(.tdy_dag$data, 
                          d_relationship = ifelse(name %in% c(from, to) & .dconnected, "d-connected",
                                                  ifelse(name %in% c(from, to) & !.dconnected, "d-separated",
                                                         NA)))
  if (as_factor) .tdy_dag$data$d_relationship <- factor(.tdy_dag$data$d_relationship, levels = c("d-connected", "d-separated"), exclude = NA)
  
  .tdy_dag
}

node_dseparated <- function(.tdy_dag, from, to, controlling_for = NULL, as_factor = TRUE) {
  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag$data$collider_line <- FALSE
    .tdy_dag$data$adjusted <- "unadjusted"
    controlling_for <- c()
  }
  
  .dseparated <- dagitty::dseparated(.tdy_dag$dag, from, to, controlling_for)
  
  .tdy_dag$data <- mutate(.tdy_dag$data, 
                          d_relationship = ifelse(name %in% c(from, to) & !.dseparated, "d-connected",
                                          ifelse(name %in% c(from, to) & .dseparated, "d-separated",
                                                 NA)))
  if (as_factor) .tdy_dag$data$d_relationship <- factor(.tdy_dag$data$d_relationship, levels = c("d-connected", "d-separated"), exclude = NA)
  .tdy_dag
}

node_drelationship <- function(.tdy_dag, from, to, controlling_for = NULL, as_factor = TRUE) {
  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag$data$collider_line <- FALSE
    .tdy_dag$data$adjusted <- "unadjusted"
    controlling_for <- c()
  }
  
  
  .dseparated <- dagitty::dseparated(.tdy_dag$dag, from, to, controlling_for)
  
  .tdy_dag$data <- mutate(.tdy_dag$data, 
                          d_relationship = ifelse(name %in% c(from, to) & !.dseparated, "d-connected",
                                                  ifelse(name %in% c(from, to) & .dseparated, "d-separated",
                                                         NA)))
  if (as_factor) .tdy_dag$data$d_relationship <- factor(.tdy_dag$data$d_relationship, levels = c("d-connected", "d-separated"), exclude = NA)
  .tdy_dag
}

ggdag_drelationship <- function(.tdy_dag, from, to, controlling_for = NULL, scale_by = .5, ...) {
  if_not_tidy_daggity(.tdy_dag) %>% 
    node_drelationship(from = from, to = to, controlling_for = controlling_for, ...)  %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) + 
      geom_dag_edges(scale_by = scale_by) +
      geom_dag_collider_edges() +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() 
}

ggdag_dseparated <- function(.tdy_dag, from, to, controlling_for = NULL, scale_by = .5, ...) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to, 
                      controlling_for = controlling_for, scale_by = scale_by, ...)
}

ggdag_dconnected <- function(.tdy_dag, .var, scale_by = .5, ...) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to, 
                      controlling_for = controlling_for, scale_by = scale_by, ...)
}

dagify(m ~ x + y) %>% ggdag_drelationship("x", "y")
dagify(m ~ x + y) %>% ggdag_drelationship("x", "y", controlling_for = "m")

dagify(m ~ x + y) %>% tidy_dagitty() %>% node_dseparated("x", "y")   %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) + 
  geom_dag_edges() +
  geom_dag_collider_edges() +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() 

dagify(m ~ x + y) %>% tidy_dagitty() %>% node_dconnected("x", "y", controlling_for = "m") %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) + 
  geom_dag_edges() +
  geom_dag_collider_edges() +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() 

dagify(m ~ x + y, y ~ x) %>% tidy_dagitty() %>% node_dconnected("x", "y", controlling_for = "m") %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) + 
    geom_dag_edges() +
    geom_dag_collider_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() 

