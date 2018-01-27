#' Title
#'
#' @param x
#' @param values
#'
#' @return
#' @export
#'
#' @examples
#'

`label<-` <- function(x, value) {
  UseMethod("label<-")
}

#' Title
#'
#' @param x
#' @param value
#'
#' @return
#' @export
#'
#' @examples
`label<-.dagitty` <- function(x, value) {
  attr(x, "labels") <- value
  x
}

`label<-.tidy_dagitty` <- function(x, value) {
  attr(x, "labels") <- value

  if (!is.null(x$data[["label"]])) x$data <- x$data %>% dplyr::select(-label)

  x$data <- dplyr::left_join(x$data, tibble::enframe(value, value = "label"), by = "name")
  x
}

#' Title
#'
#' @param .tdy_dag
#' @param labels
#'
#' @return
#' @export
#'
#' @examples
dag_label <- function(.tdy_dag, labels = NULL) {

  if (!is.null(labels)) .tdy_dag$data <- .tdy_dag$data %>% dplyr::select(-label)
  if (is.null(labels)) labels <- label(.tdy_dag$dag)
  if (is.null(labels)) { warning("no labels provided"); return(.tdy_dag); }

  .tdy_dag$data <- dplyr::left_join(.tdy_dag$data, tibble::enframe(labels, value = "label"), by = "name")


  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#'
#' @return
#' @export
#'
#' @examples
label <- function(.tdy_dag) {
  attr(.tdy_dag, "labels")
}

has_labels <- function(.tdy_dag) {
  !is.null(attr(.tdy_dag, "labels"))
}



#' Title
#'
#' @param .dagitty
#' @param cap
#' @param seed
#' @param layout
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tidy_dagitty <- function(.dagitty, seed = NULL, layout = "nicely", ...) {
  if (!is.null(seed)) set.seed(seed)

  if (dagitty::graphType(.dagitty) != "dag") stop("`.dagitty` must be of graph type `dag`")
  .dag <- .dagitty

  no_existing_coords <- dagitty::coordinates(.dagitty) %>%
    purrr::map_lgl(~all(is.na(.x))) %>%
    all()

  ggraph_layout <- dagitty::edges(.dagitty) %>%
    dplyr::select(v, w) %>%
    igraph::graph_from_data_frame() %>%
    {suppressMessages(ggraph::create_layout(., layout, ...))}

  if (no_existing_coords) {
    coords <- coords2list(ggraph_layout)
  } else {
    coords <- dagitty::coordinates(.dagitty)
  }

  labels <- names(coords$x)

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
    dplyr::select(name, from, x, y, direction, type, to = w, xend, yend) %>%
    dplyr::left_join(dplyr::select(ggraph_layout, -x, -y), by = "name") %>%
    dplyr::arrange(.ggraph.orig_index)

  .tdy_dag <- list(data = tidy_dag, dag = .dag)
  class(.tdy_dag) <- "tidy_dagitty"
  if (has_labels(.dag)) {
    label(.tdy_dag) <- label(.dag)
  }

    .tdy_dag
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom methods is
is.tidy_dagitty <- function(x) {
  is(x, "tidy_dagitty")
}



#' Title
#'
#' @param .tdy_dag
#'
#' @export
#' @importFrom ggplot2 fortify
fortify.tidy_dagitty <- function(.tdy_dag) {
  .tdy_dag$data
}

#' Title
#'
#' @param .tdy_dag
#'
#' @param ...
#'
#' @export
as.data.frame.tidy_dagitty <- function(x, row.names, optional, ...) {
  as.data.frame(x$data, row.names, optional, ...)
}

#' Title
#'
#' @param .tdy_dag
#'
#' @export
#' @importFrom dplyr tbl_df
tbl_df.tidy_daggity <- function(.tdy_dag) {
  .tdy_dag$data
}

#' Title
#'
#' @param x
#'
#' @param row.names
#' @param optional
#' @param ...
#'
#' @export
#' @importFrom dplyr as.tbl
as.tbl.tidy_daggity <- function(x, row.names = NULL, optional = FALSE, ...) {
  dplyr::as.tbl(x$data, row.names = row.names, optional = optional, ...)
}

#' Title
#'
#' @param x
#'
#' @param ...
#'
#' @export
print.tidy_dagitty <- function(x, ...) {
  print(x$data, ...)
}

#' Title
#'
#' @param .tdy_dag
#' @param exposure
#' @param outcome
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom utils capture.output
adjustment_sets <- function(.tdy_dag, exposure = NULL, outcome = NULL, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)
  sets <- dagitty::adjustmentSets(.tdy_dag$dag, exposure = exposure, outcome = outcome, ...)
  is_empty_set <- purrr::is_empty(sets)
  if (is_empty_set) stop("dagitty failed to process sets. Check that it is a DAG with `is_acyclic()`")
  sets <- sets %>%
    capture.output() %>%
    stringr::str_replace(" \\{\\}", "(Unconditionally Independent)") %>%
    stringr::str_replace("\\{ ", "") %>%
    stringr::str_replace(" \\}", "") %>%
    stringr::str_trim() %>%
    purrr::map(~stringr::str_split(.x, ", ") %>%
    purrr::pluck(1))


  .tdy_dag$data <-
    purrr::map_df(sets,
                  ~dplyr::mutate(.tdy_dag$data, adjusted = ifelse(name %in% .x, "adjusted", "unadjusted"), set = paste(.x, collapse = ", "))
    )

  .tdy_dag
}

if_not_tidy_daggity <- function(.dagitty, ...) {
  if (!is.tidy_dagitty(.dagitty)) return(tidy_dagitty(.dagitty, ...))
  .dagitty
}

#' Title
#'
#' @param .tdy_dag
#'
#' @return
#' @export
#'
#' @examples
ggdag <- function(.tdy_dag) {
  if_not_tidy_daggity(.tdy_dag) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_dag_edges() +
      geom_dag_node() +
      geom_dag_text() +
      theme_dag() +
      scale_dag()
}

#' Title
#'
#' @param .tdy_dag
#' @param size
#' @param label_rect_size
#'
#' @return
#' @export
#'
#' @examples
ggdag_classic <- function(.tdy_dag, size = 8, label_rect_size = NULL, ...) {
  if_not_tidy_daggity(.tdy_dag, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges(ggplot2::aes(start_cap = ggraph::label_rect(name, fontsize = size * 3.57),
                                end_cap = ggraph::label_rect(to, fontsize = size * 3.57))) +
    ggplot2::geom_text(ggplot2::aes(label = name), size = size) +
    theme_dag() +
    scale_dag()
}

#' Title
#'
#' @param .tdy_dag
#' @param exposure
#' @param outcome
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_adjustment_set <- function(.tdy_dag, exposure = NULL, outcome = NULL, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    adjustment_sets(exposure = exposure, outcome = outcome, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = adjusted)) +
      geom_dag_edges(ggplot2::aes(edge_alpha = adjusted)) +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      ggplot2::facet_wrap(~set) +
      theme_dag() +
      scale_dag(expand_x = expand_scale(c(0.25, 0.25)))
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

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dag <- function(...) {
  dag_string <- paste(..., sep = "; ")
  dagitty::dagitty(paste0("dag{", dag_string, "}"))
}

#' Title
#'
#' @param ...
#' @param exposure
#' @param outcome
#' @param latent
#' @param labels
#' @param coords
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param .dag
#' @param .var
#'
#' @return
#' @export
#'
#' @examples
is_collider <- function(.dag, .var) {
  if (is.tidy_dagitty(.dag)) .dag <- .dag$dag
  n_parents <- dagitty::parents(.dag, .var)
  length(n_parents) > 1
}

#' Title
#'
#' @param .dag
#' @param .var
#'
#' @return
#' @export
#'
#' @examples
is_downstream_collider <- function(.dag, .var) {
  if (is.tidy_dagitty(.dag)) .dag <- .dag$dag
  var_ancestors <- dagitty::ancestors(.dag, .var)[-1]
  any(purrr::map_lgl(var_ancestors, ~is_collider(.dag, .x)))
}


#' Title
#'
#' @param g
#'
#' @return
#' @export
#'
#' @examples
is_acyclic <- function(g) {
  dagitty::isAcyclic(g)
}

#' Title
#'
#' @param g
#' @param z
#' @param x
#' @param y
#' @param direct
#'
#' @return
#' @export
#'
#' @examples
is_confounder <- function(g, z, x, y, direct = FALSE) {
   if (direct) {
     z_descendants <- dagitty::children(g, z)
   } else {
     z_descendants <- dagitty::descendants(g, z)[-1]
   }
   all(c(x, y) %in% z_descendants)
}

#' Title
#'
#' @param .tdy_dag
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_collider <- function(.tdy_dag, as_factor = TRUE) {
  vars <- unique(.tdy_dag$data$name)
  colliders <- purrr::map_lgl(vars, ~is_collider(.tdy_dag, .x))
  names(colliders) <- vars
  .tdy_dag$data <- dplyr::left_join(.tdy_dag$data,
                             tibble::enframe(colliders, value = "colliders"), by = "name")
  purrr::map(vars[colliders], ~dagitty::parents(.tdy_dag$dag, .x))
  if (as_factor) .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, colliders = factor(as.numeric(colliders), levels = 0:1, labels = c("Non-Collider", "Collider")))
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_collider <- function(.tdy_dag, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_collider(...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = forcats::fct_rev(colliders))) +
      geom_dag_edges() +
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

#' Title
#'
#' @param .tdy_dag
#' @param adjust_for
#'
#' @return
#' @export
#'
#' @examples
activate_collider_paths <- function(.tdy_dag, adjust_for) {
  vars <- unique(.tdy_dag$data$name)
  colliders <- purrr::map_lgl(vars, ~is_collider(.tdy_dag, .x))
  downstream_colliders <- purrr::map_lgl(vars, ~is_downstream_collider(.tdy_dag, .x))
  collider_names <- unique(c(vars[colliders], vars[downstream_colliders]))

  if (!any((collider_names %in% adjust_for))) return(.tdy_dag)
  adjusted_colliders <- collider_names[collider_names %in% adjust_for]
  collider_paths <- purrr::map(adjusted_colliders, ~dagitty::ancestors(.tdy_dag$dag, .x)[-1])

  activated_pairs <- purrr::map(collider_paths, unique_pairs)

  collider_lines <- purrr::map_df(activated_pairs, function(.pairs_df) {
    .pairs_df %>% dplyr::rowwise() %>% dplyr::do({
      df <- .
      name <- df[["Var1"]]
      to <- df[["Var2"]]
      start_coords <- .tdy_dag$data %>% dplyr::filter(name == df[["Var1"]]) %>% dplyr::select(x, y) %>% dplyr::slice(1)
      end_coords <- .tdy_dag$data %>% dplyr::filter(name == df[["Var2"]]) %>% dplyr::select(x, y) %>% dplyr::slice(1)

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

#' Title
#'
#' @param .tdy_dag
#' @param adjust_for
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
control_for <- function(.tdy_dag, adjust_for, as_factor = TRUE) {
  .tdy_dag <- activate_collider_paths(.tdy_dag, adjust_for)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, adjusted = ifelse(name %in% adjust_for, "adjusted", "unadjusted"))
  if (as_factor) .tdy_dag$data <- dplyr::mutate(.tdy_dag$data, adjusted = factor(adjusted, exclude = NA))
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param adjust_for
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_adjust <- function(.tdy_dag, adjust_for, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    control_for(adjust_for, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted)) +
      geom_dag_edges(ggplot2::aes(edge_alpha = adjusted)) +
      geom_dag_collider_edges() +
      geom_dag_node() +
      geom_dag_text() +
      theme_dag() +
      scale_dag()
}

#' Title
#'
#' @param coord_list
#'
#' @return
#' @export
#'
#' @examples
coords2df <- function(coord_list) {
  coord_df <- purrr::map(coord_list, tibble::enframe) %>% purrr::reduce(dplyr::left_join, by = "name")
  names(coord_df) <- c("name", "x", "y")
  coord_df
}

#' Title
#'
#' @param coord_df
#'
#' @return
#' @export
#'
#' @examples
coords2list <- function(coord_df) {
  x <- coord_df %>% dplyr::select(name, x) %>% tibble::deframe()
  y <- coord_df %>% dplyr::select(name, y) %>% tibble::deframe()
  list(x = x, y = y)
}

#' Title
#'
#' @param .tdy_dag
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param .tdy_dag
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_status <- function(.tdy_dag, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_status(...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = status)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks = c("exposure", "outcome", "latent"))
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_children <- function(.tdy_dag, .var, as_factor = TRUE) {
  .children <- dagitty::children(.tdy_dag$dag, .var)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                          children = ifelse(name %in% .children, "child",
                                          ifelse(name == .var, "parent",
                                                        NA)))
  if (as_factor) .tdy_dag$data$children <- factor(.tdy_dag$data$children, exclude = NA)
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_children <- function(.tdy_dag, .var, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_children(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = children)) +
      geom_dag_edges() +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() +
      scale_dag(breaks  = c("parent", "child"))
}


#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_parents <- function(.tdy_dag, .var, as_factor = TRUE) {
  .parent <- dagitty::parents(.tdy_dag$dag, .var)
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                          parent = ifelse(name %in% .parent, "parent",
                                            ifelse(name == .var, "child",
                                                   NA)))
  if (as_factor) .tdy_dag$data$parent <- factor(.tdy_dag$data$parent, exclude = NA)
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_parents <- function(.tdy_dag, .var, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_parents(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = parent)) +
      geom_dag_edges() +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() +
      scale_dag(breaks  = c("parent", "child"))
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_ancestors <- function(.tdy_dag, .var, as_factor = TRUE) {
  .ancestors <- dagitty::ancestors(.tdy_dag$dag, .var)[-1]
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                          ancestor = ifelse(name %in% .ancestors, "ancestor",
                                          ifelse(name == .var, "descendant",
                                                 NA)))
  if (as_factor) .tdy_dag$data$ancestor <- factor(.tdy_dag$data$ancestor, exclude = NA)
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_ancestors <- function(.tdy_dag, .var, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_ancestors(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = ancestor)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks  = c("ancestor", "descendant"))
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
node_descendants <- function(.tdy_dag, .var, as_factor = TRUE) {
  .descendants <- dagitty::descendants(.tdy_dag$dag, .var)[-1]
  .tdy_dag$data <- dplyr::mutate(.tdy_dag$data,
                          descendant = ifelse(name %in% .descendants, "descendant",
                                            ifelse(name == .var, "ancestor",
                                                   NA)))
  if (as_factor) .tdy_dag$data$descendant <- factor(.tdy_dag$data$descendant, exclude = NA)
  .tdy_dag
}

#' Title
#'
#' @param .tdy_dag
#' @param .var
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_descendants <- function(.tdy_dag, .var, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_descendants(.var, ...) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = descendant)) +
      geom_dag_edges() +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() +
      scale_dag(breaks  = c("ancestor", "descendant"))
}

#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param as_factor
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_drelationship <- function(.tdy_dag, from, to, controlling_for = NULL, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_drelationship(from = from, to = to, controlling_for = controlling_for, ...)  %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
      geom_dag_edges() +
      geom_dag_collider_edges() +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() +
      scale_dag()
}

#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_dseparated <- function(.tdy_dag, from, to, controlling_for = NULL, ...) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to,
                      controlling_for = controlling_for, ...)
}

#' Title
#'
#' @param .tdy_dag
#' @param from
#' @param to
#' @param controlling_for
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_dconnected <- function(.tdy_dag, from, to, controlling_for = NULL, ...) {
  ggdag_drelationship(.tdy_dag = .tdy_dag, from = from, to = to,
                      controlling_for = controlling_for, ...)
}

#' Title
#'
#' @param .dag
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
node_canonical <- function(.dag, ...) {
  .dag <- if_not_tidy_daggity(.dag)
  dagitty::canonicalize(.dag$dag)$g %>% tidy_dagitty(...)
}

#' Title
#'
#' @param .tdy_dag
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_canonical <- function(.tdy_dag, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)
  node_canonical(.tdy_dag) %>% ggdag(...)
}

#' Title
#'
#' @param .dag
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
node_exogenous <- function(.dag, ...) {
  .dag <- if_not_tidy_daggity(.dag)
  exogenous_vars <- dagitty::exogenousVariables(.dag$dag)
  .dag$data <- .dag$data %>% dplyr::mutate(exogenous = ifelse(name %in% exogenous_vars, "exogenous", NA))
  .dag
}

#' Title
#'
#' @param .tdy_dag
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_exogenous <- function(.tdy_dag, ...) {
  if_not_tidy_daggity(.tdy_dag) %>%
    node_exogenous() %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = exogenous)) +
    geom_dag_edges() +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() +
    scale_dag(breaks  = "exogenous")
}

#' Title
#'
#' @param .dag
#' @param exposure
#' @param outcome
#'
#' @return
#' @export
#'
#' @examples
node_instrumental <- function(.dag, exposure = NULL, outcome = NULL) {
  .dag <- if_not_tidy_daggity(.dag)
  instrumental_vars <- dagitty::instrumentalVariables(.dag$dag,
                                                      exposure = exposure,
                                                      outcome = outcome)


  i_vars <- purrr::map(instrumental_vars, "I")
  adjust_for_vars <- purrr::map(instrumental_vars, "Z")

  .dag$data <- purrr::map2_df(i_vars, adjust_for_vars, function(.i, .z) {
      conditional_vars <- ifelse(is.null(.z), "", paste("|", paste(.z, collapse = ", ")))
      .dag$data$instrumental_name <- paste(.i, conditional_vars) %>% stringr::str_trim()
      if (!is.null(adjust_for_vars)) {
        .dag <- .dag %>% control_for(adjust_for_vars)
      } else {
        .dag$data$adjusted <- factor("unadjusted", levels = c("unadjusted", "adjusted"), exclude = NA)
      }
      .dag$data <- .dag$data %>% dplyr::mutate(instrumental = ifelse(name == .i, "instrumental", NA))
      .dag$data
  })
    .dag
}

#' Title
#'
#' @param .tdy_dag
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_instrumental <- function(.tdy_dag, exposure = NULL, outcome = NULL, ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_instrumental(exposure = exposure, outcome = outcome, ...)
  p <- .tdy_dag %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = instrumental, shape = adjusted)) +
      geom_dag_edges() +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() +
      scale_dag(breaks  = "instrumental")

  if (dplyr::n_distinct(.tdy_dag$data$instrumental_name) > 1) p <- p + ggplot2::facet_wrap(~instrumental_name)
  p
}

#' Title
#'
#' @param .dag
#' @param n
#'
#' @return
#' @export
#'
#' @examples
node_equivalent_dags <- function(.dag, n = 100, layout = "auto") {
  .dag <- if_not_tidy_daggity(.dag)
  .dag$data <- dagitty::equivalentDAGs(.dag$dag, n = n) %>%
    purrr::map_df(~as.data.frame(tidy_dagitty(.x, layout = layout)), .id = "dag") %>%
    dplyr::as.tbl()
  .dag
}

#' Title
#'
#' @param .tdy_dag
#' @param cap
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggdag_equivalent_dags <- function(.tdy_dag, ...) {

  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_equivalent_dags(...)

  p <- ggdag(.tdy_dag)

  if (dplyr::n_distinct(.tdy_dag$data$dag) > 1) p <- p + ggplot2::facet_wrap(~dag)

  p
}

#' Title
#'
#' @param .dag
#' @param layout
#'
#' @return
#' @export
#'
#' @examples
node_equivalent_class <- function(.dag, layout = "auto") {
  .dag <- if_not_tidy_daggity(.dag, layout = layout)
  .dag$data <- dagitty::equivalenceClass(.dag$dag) %>%
    dagitty::edges(.) %>%
    dplyr::filter(e == "--") %>%
    dplyr::select(name = v, reversable = e) %>%
    dplyr::mutate(name = as.character(name)) %>%
    dplyr::left_join(.dag$data, .,  by = "name") %>%
    dplyr::mutate(reversable = !is.na(reversable))

  .dag
}

#' Title
#'
#' @param .tdy_dag
#' @param cap
#' @param ...
#' @param expand_x
#' @param expand_y
#' @param breaks
#'
#' @return
#' @export
#'
#' @examples
ggdag_equivalent_class <- function(.tdy_dag, cap = ggraph::circle(8, 'mm'),
                                   expand_x = ggplot2::expand_scale(c(.10, .10)),
                                   expand_y = ggplot2::expand_scale(c(.10, .10)),
                                   breaks = ggplot2::waiver(), ...) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) %>%
    node_equivalent_class(...)

  reversable_lines <- dplyr::filter(.tdy_dag$data, reversable)
  non_reversable_lines <- dplyr::filter(.tdy_dag$data, !reversable)
  .tdy_dag %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, edge_alpha = reversable)) +
      geom_dag_edges(data_directed = dplyr::filter(non_reversable_lines, direction != "<->"),
                     data_bidirected = dplyr::filter(non_reversable_lines, direction == "<->")) +
      ggraph::geom_edge_link(data = reversable_lines, start_cap = cap, end_cap = cap) +
      geom_dag_node() +
      geom_dag_text(col = "white") +
      theme_dag() +
      ggplot2::scale_color_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50", breaks = breaks) +
      ggplot2::scale_fill_brewer(name = "", drop = FALSE, palette = "Set2", na.value = "grey50") +
      ggplot2::scale_x_continuous(expand = expand_x) +
      ggplot2::scale_y_continuous(expand = expand_y) +
      ggraph::scale_edge_alpha_manual(name = "Reversable", drop = FALSE, values = c(.1, 1))
}
