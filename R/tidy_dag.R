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

  x$data <- dplyr::left_join(x$data, tibble::enframe(values, value = "label"), by = "name")
  x
}

dag_label <- function(.tdy_dag, labels = NULL) {

  if (!is.null(labels)) .tdy_dag$data <- .tdy_dag$data %>% dplyr::select(-label)
  if (is.null(labels)) labels <- label(.tdy_dag$dag)
  if (is.null(labels)) { warning("no labels provided"); return(.tdy_dag); }

  .tdy_dag$data <- dplyr::left_join(.tdy_dag$data, tibble::enframe(labels, value = "label"), by = "name")


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

# dag_line_segment <- function(.x, .y, .wx, .wy, .a, .b, .c, .d, cap = ggraph::circle(8, 'mm')) {
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

tidy_dagitty <- function(.dagitty, cap = ggraph::circle(8, 'mm'), seed = NULL, layout = "nicely", ...) {
  if (!is.null(seed)) set.seed(seed)

  dagitty:::.supportsTypes(.dagitty, c("dag"))
  .dag <- .dagitty

  no_existing_coords <- dagitty::coordinates(.dagitty) %>%
    purrr::map_lgl(~all(is.na(.x))) %>%
    all()

  ggraph_layout <- dagitty::edges(.dagitty) %>%
    dplyr::select(v, w) %>%
    igraph::graph_from_data_frame() %>%
    {suppressMessages(ggraph::create_layout(., layout, ...))}

  if (no_existing_coords) {
    #.dagitty <- dagitty::as.dagitty(dagitty::graphLayout(.dagitty))
    dagitty::coordinates(.dagitty) <- coords2list(ggraph_layout)
  }
  coords <- dagitty::coordinates(.dagitty)
  labels <- names(coords$x)

  # wx <- purrr::map_df(labels, ~tibble(name = .x, wx = strwidth(paste0("xxxxxx", .x), "figure", cex = 2)))
  # wy <- purrr::map_df(labels, ~tibble(name = .x, wy = strheight(paste0("\n\n\n", .x), "figure", cex = 2)))
  # asp <- par("pin")[1]/diff(par("usr")[1:2])/(par("pin")[2]/diff(par("usr")[3:4]))
  #
  # scale_by <- 1/scale_by
  dag_edges <- dagitty::edges(.dagitty)

  tidy_dag <- dplyr::left_join(tibble::enframe(coords$x, value = "x"),
                        tibble::enframe(coords$y, value = "y"),
                        by = "name")

  tidy_dag <- dag_edges %>%
    dplyr::select(-x, -y) %>%
    dplyr::mutate(v = as.character(v),
           w = as.character(w),
           direction = factor(e, levels = c("<-", "->", "<->"), exclude = NA),
           type = ifelse(e == "<->", "bidirected", "directed"),
           type = factor(type, levels = c("directed", "bidirected"), exclude = NA)) %>%
    dplyr::left_join(tidy_dag, ., by = c("name" = "v")) %>%
    dplyr::left_join(tidy_dag, by = c("w" = "name"),  suffix = c("", "end")) %>%
    dplyr::mutate(from = name) %>%
    #dplyr::rename() %>%
    dplyr::select(name, from, x, y, direction, type, to = w, xend, yend) %>%
    dplyr::left_join(dplyr::select(ggraph_layout, -x, -y), by = "name")
    #dplyr::mutate(y = -y, yend = -yend)

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
  dplyr::as.tbl(.tdy_dag$data, ...)
}

print.tidy_dagitty <- function(.tdy_dag) {
  print(.tdy_dag$data)
}

adjustment_sets <- function(.tdy_dag, exposure = NULL, outcome = NULL, ...) {
  sets <- dagitty::adjustmentSets(.tdy_dag$dag, exposure = exposure, outcome = outcome, ...)
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
                  ~dplyr::mutate(.tdy_dag$data, adjusted = ifelse(name %in% .x, "adjusted", "unadjusted"), set = paste(.x, collapse = ", "))
    )

  .tdy_dag
}

if_not_tidy_daggity <- function(.dagitty) {
  if (!is.tidy_dagitty(.dagitty)) return(tidy_dagitty(.dagitty))
  .dagitty
}

ggdag <- function(.tdy_dag, cap = ggraph::circle(8, 'mm')) {
  if_not_tidy_daggity(.tdy_dag) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_dag_edges(cap = cap) +
      geom_dag_node() +
      geom_dag_text() +
      theme_dag() +
      scale_dag()
}

ggdag_adjustment_set <- function(.tdy_dag, cap = ggraph::circle(8, 'mm'), exposure = NULL, outcome = NULL, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    adjustment_sets(exposure = exposure, outcome = outcome, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = adjusted)) +
      geom_dag_edges(ggplot2::aes(edge_alpha = adjusted), cap = cap) +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      ggplot2::facet_wrap(~set) +
      theme_dag() +
      scale_dag()
}


formula2char <- function(fmla) {
  char_fmla <- as.character(fmla)
  rhs_vars <- char_fmla[[3]] %>% stringr::str_split(" \\+ ") %>% purrr::pluck(1)
  bidirectional <- any(stringr::str_detect(rhs_vars, "~"))
  rhs_vars <- stringr::str_replace_all(rhs_vars, "~", "")
  arrows <- ifelse(bidirectional, "<->", "<-")
  rhs_vars_coll <- paste0("{", paste(rhs_vars, collapse = " "), "}")
  paste(char_fmla[[2]], arrows, rhs_vars_coll)
  # char_dag_fmla <- rhs_vars %>% purrr::map_chr(~paste(char_fmla[[2]], arrows, .x))
  # paste(char_dag_fmla, collapse = "\n")
}

dag <- function(...) {
  dag_string <- paste(..., sep = "; ")
  paste0("dag{", dag_string, "}")

  #dag_list <- rlang::enquos(..., .unquote_names = FALSE)
  #browser()
  #dag_list
  #purrr::map_chr(dag_list, rlang::expr_name(dag_list))
}

dagify <- function(..., exposure = NULL, outcome = NULL, latent = NULL, labels = NULL, coords = NULL) {
  fmlas <- list(...)
  dag_txt <- purrr::map_chr(fmlas, formula2char)
  dag_txt <- paste(dag_txt, collapse = "; ") %>%
    paste("dag {", ., "}")
  dgty <- dagitty::dagitty(dag_txt)
  if (!is.null(exposure)) dagitty::exposures(dgty) <- exposure
  if (!is.null(outcome)) dagitty::outcomes(dgty) <- outcome
  if (!is.null(latent)) dagitty::latents(dgty) <- latent
  if (!is.null(labels)) label(dgty) <- labels
  if (!is.null(coords)) {
    if (is.data.frame(coords)) {
      dagitty::coordinates(dgty) <- coords2list(coords)
    } else if (is.list(coords)) {
      dagitty::coordinates(dgty) <- coords
    } else {
    stop("`coords` must be of class `list` or `data.frame`")
    }
  }
  dgty
}

is_collider <- function(.dag, .var) {
  if (is.tidy_dagitty(.dag)) .dag <- .dag$dag
  n_parents <- dagitty::parents(.dag, .var)
  length(n_parents) > 1
}


is_acyclic <- function(g) dagitty::isAcyclic(g)

is_confounder <- function(g, z, x, y, direct = FALSE) {
   if (direct) {
     z_descendants <- dagitty::children(g, z)
   } else {
     z_descendants <- dagitty::descendants(g, z)[-1]
   }
   all(c(x, y) %in% z_descendants)
}

node_collider <- function(.tdy_dag, as_factor = TRUE) {
  vars <- unique(.tdy_dag$data$name)
  colliders <- purrr::map_lgl(vars, ~is_collider(.tdy_dag, .x))
  names(colliders) <- vars
  .tdy_dag$data <- dplyr::left_join(.tdy_dag$data,
                             tibble::enframe(colliders, value = "colliders"))
  purrr::map(vars[colliders], ~dagitty::parents(.tdy_dag$dag, .x))
  if (as_factor) .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, colliders = factor(as.numeric(colliders), levels = 0:1, labels = c("Non-Collider", "Collider")))
  .tdy_dag
}

ggdag_collider <- function(.tdy_dag, cap = ggraph::circle(8, 'mm'), ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_collider(...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = forcats::fct_rev(colliders))) +
      geom_dag_edges(cap = cap) +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() +
      scale_dag()
}


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
    .pairs_df %>% dplyr::rowwise() %>% dplyr::do({
      df <- .
      name <- df[["Var1"]]
      to <- df[["Var2"]]
      start_coords <- .tdy_dag$data %>% filter(name == df[["Var1"]]) %>% dplyr::select(x, y) %>% dplyr::slice(1)
      end_coords <- .tdy_dag$data %>% filter(name == df[["Var2"]]) %>% dplyr::select(x, y) %>% dplyr::slice(1)

      .tdy_dag$data %>% dplyr::add_row(.before = 1, name = name, from = name, x = start_coords[[1, 1]], y = start_coords[[1, 2]],
                                to = to, xend = end_coords[[1, 1]], yend = end_coords[[1, 2]],
                                direction = factor("<->", levels = c("<-", "->", "<->"), exclude = NA),
                                type = factor("bidirected", levels = c("directed", "bidirected"), exclude = NA)) %>% dplyr::slice(1)
    })
  })
  collider_lines$collider_line <- TRUE
  .tdy_dag$data$collider_line <- FALSE
  .tdy_dag$data <- dplyr::bind_rows(.tdy_dag$data, collider_lines)
  .tdy_dag
}

control_for <- function(.tdy_dag, adjust_for, as_factor = TRUE) {
  .tdy_dag <- activate_collider_paths(.tdy_dag, adjust_for)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, adjusted = ifelse(name %in% adjust_for, "adjusted", "unadjusted"))
  if (as_factor) .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, adjusted = factor(adjusted, exclude = NA))
  .tdy_dag
}

ggdag_adjust <- function(.tdy_dag, adjust_for, cap = ggraph::circle(8, 'mm'), ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    control_for(adjust_for, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted)) +
      geom_dag_edges(ggplot2::aes(edge_alpha = adjusted), cap = cap) +
      geom_dag_collider_edges() +
      geom_dag_node() +
      geom_dag_text() +
      theme_dag() +
      scale_dag()
}

coords2df <- function(coord_list) {
  coord_df <- purrr::map(coord_list, tibble::enframe) %>% purrr::reduce(dplyr::left_join, by = "name")
  names(coord_df) <- c("name", "x", "y")
  coord_df
}

coords2list <- function(coord_df) {
  x <- coord_df %>% dplyr::select(name, x) %>% tibble::deframe()
  y <- coord_df %>% dplyr::select(name, y) %>% tibble::deframe()
  list(x = x, y = y)
}

node_status <- function(.tdy_dag, as_factor = TRUE) {
  .exposures <- dagitty::exposures(.tdy_dag$dag)
  .outcomes <- dagitty::outcomes(.tdy_dag$dag)
  .latents <- dagitty::latents(.tdy_dag$dag)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                          status = ifelse(name %in% .exposures, "exposure",
                                          ifelse(name %in% .outcomes, "outcome",
                                                 ifelse(name %in% .latents, "latent",
                                                        NA))))
  if (as_factor) .tdy_dag$data$status <- factor(.tdy_dag$data$status, exclude = NA)
  .tdy_dag
}

ggdag_status <- function(.tdy_dag, cap = ggraph::circle(8, 'mm'), ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_status(...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = status)) +
    geom_dag_edges(cap = cap) +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks = c("exposure", "outcome", "latent"))
}

node_children <- function(.tdy_dag, .var, as_factor = TRUE) {
  .children <- dagitty::children(.tdy_dag$dag, .var)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                          children = ifelse(name %in% .children, "child",
                                          ifelse(name == .var, "parent",
                                                        NA)))
  if (as_factor) .tdy_dag$data$children <- factor(.tdy_dag$data$children, exclude = NA)
  .tdy_dag
}

ggdag_children <- function(.tdy_dag, .var, cap = ggraph::circle(8, 'mm'), ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_children(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = children)) +
      geom_dag_edges(cap = cap) +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() +
      scale_dag(breaks  = c("parent", "child"))
}


node_parents <- function(.tdy_dag, .var, as_factor = TRUE) {
  .parent <- dagitty::parents(.tdy_dag$dag, .var)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                          parent = ifelse(name %in% .parent, "parent",
                                            ifelse(name == .var, "child",
                                                   NA)))
  if (as_factor) .tdy_dag$data$parent <- factor(.tdy_dag$data$parent, exclude = NA)
  .tdy_dag
}

ggdag_parents <- function(.tdy_dag, .var, cap = ggraph::circle(8, 'mm'), ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_parents(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = parent)) +
      geom_dag_edges(cap = cap) +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() +
      scale_dag(breaks  = c("parent", "child"))
}

node_ancestors <- function(.tdy_dag, .var, as_factor = TRUE) {
  .ancestors <- dagitty::ancestors(.tdy_dag$dag, .var)[-1]
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                          ancestor = ifelse(name %in% .ancestors, "ancestor",
                                          ifelse(name == .var, "descendant",
                                                 NA)))
  if (as_factor) .tdy_dag$data$ancestor <- factor(.tdy_dag$data$ancestor, exclude = NA)
  .tdy_dag
}

ggdag_ancestors <- function(.tdy_dag, .var, cap = ggraph::circle(8, 'mm'), ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_ancestors(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = ancestor)) +
    geom_dag_edges(cap = cap) +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks  = c("ancestor", "descendant"))
}

node_descendants <- function(.tdy_dag, .var, as_factor = TRUE) {
  .descendants <- dagitty::descendants(.tdy_dag$dag, .var)[-1]
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                          descendant = ifelse(name %in% .descendants, "descendant",
                                            ifelse(name == .var, "ancestor",
                                                   NA)))
  if (as_factor) .tdy_dag$data$descendant <- factor(.tdy_dag$data$descendant, exclude = NA)
  .tdy_dag
}

ggdag_descendants <- function(.tdy_dag, .var, cap = ggraph::circle(8, 'mm'), ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_descendants(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = descendant)) +
      geom_dag_edges(cap = cap) +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() +
      scale_dag(breaks  = c("ancestor", "descendant"))
}

node_dconnected <- function(.tdy_dag, from, to, controlling_for = NULL, as_factor = TRUE) {
  if (!is.null(controlling_for)) {
    .tdy_dag <- control_for(.tdy_dag, controlling_for)
  } else {
    .tdy_dag$data$collider_line <- FALSE
    .tdy_dag$data$adjusted <- "unadjusted"
    controlling_for <- c()
  }

  .dconnected <- dagitty::dconnected(.tdy_dag$dag, from, to, controlling_for)

  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
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

  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
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

  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                          d_relationship = ifelse(name %in% c(from, to) & !.dseparated, "d-connected",
                                                  ifelse(name %in% c(from, to) & .dseparated, "d-separated",
                                                         NA)))
  if (as_factor) .tdy_dag$data$d_relationship <- factor(.tdy_dag$data$d_relationship, levels = c("d-connected", "d-separated"), exclude = NA)
  .tdy_dag
}

ggdag_drelationship <- function(.tdy_dag, from, to, controlling_for = NULL, cap = ggraph::circle(8, 'mm'), ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_drelationship(from = from, to = to, controlling_for = controlling_for, ...)  %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
      geom_dag_edges(cap = cap) +
      geom_dag_collider_edges() +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() +
      scale_dag()
}

ggdag_dseparated <- function(.tdy_dag, from, to, controlling_for = NULL, cap = ggraph::circle(8, 'mm'), ...) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to,
                      controlling_for = controlling_for, cap = cap, ...)
}

ggdag_dconnected <- function(.tdy_dag, from, to, controlling_for = NULL, cap = ggraph::circle(8, 'mm'), ...) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to,
                      controlling_for = controlling_for, cap = cap, ...)
}
