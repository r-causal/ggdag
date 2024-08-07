#  variables used in various NSE calls
utils::globalVariables(
  c(
    ".",
    "x",
    "y",
    "xend",
    "yend",
    "adjusted",
    "collider_line",
    "collider",
    "name",
    "from",
    "to",
    "direction",
    "Var1",
    "Var2",
    "ancestor",
    "children",
    "circular",
    "collider_line",
    "colliders",
    "d_relationship",
    "descendant",
    "direction",
    "e",
    "exogenous",
    "from",
    "independence",
    "instrumental",
    "name",
    "parent",
    "reversable",
    "segment.colour",
    "status",
    "to",
    "to_formula",
    "type",
    "v",
    "w",
    ".ggraph.orig_index",
    ".ggraph.index",
    "from",
    ".from",
    ".to",
    "path",
    "paths",
    "set",
    "adjacent",
    "blanket",
    "collider_path_nodes",
    "ggplot2::expansion"
  )
)

if_not_tidy_daggity <- function(.dagitty, ...) {
  if (!is.tidy_dagitty(.dagitty)) {
    return(tidy_dagitty(.dagitty, ...))
  }
  .dagitty
}

unique_pairs <- function(x, exclude_identical = TRUE) {
  pairs <- expand.grid(x, x) %>% purrr::map_dfc(as.character)
  if (exclude_identical) pairs <- pairs %>% dplyr::filter(Var1 != Var2)
  pairs[!duplicated(t(apply(pairs, 1, sort))), ]
}

formula2char <- function(fmla) {
  #  using default to avoid `formula.tools::as.character.formula()`
  char_fmla <- as.character.default(fmla)
  rhs_vars <- char_fmla[[3]] %>%
    stringr::str_split(" \\+ ") %>%
    purrr::pluck(1)
  bidirectional <- any(stringr::str_detect(rhs_vars, "~"))
  rhs_vars <- stringr::str_replace_all(rhs_vars, "~", "")
  arrows <- ifelse(bidirectional, "<->", "<-")
  rhs_vars_coll <- paste0("{", paste(rhs_vars, collapse = " "), "}")
  paste(char_fmla[[2]], arrows, rhs_vars_coll)
}

edge_type_switch <- function(edge_type) {
  switch(edge_type,
    "link_arc" = geom_dag_edges,
    "link" = geom_dag_edges_link,
    "arc" = geom_dag_edges_arc,
    "diagonal" = geom_dag_edges_diagonal
  )
}

is_empty_or_null <- function(x) {
  is.null(x) || purrr::is_empty(x)
}

is_false <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && !x

has_exposure <- function(x) {
  is_false(purrr::is_empty(dagitty::exposures(pull_dag(x))))
}

has_outcome <- function(x) {
  is_false(purrr::is_empty(dagitty::outcomes(pull_dag(x))))
}

has_latent <- function(x) {
  is_false(purrr::is_empty(dagitty::latents(pull_dag(x))))
}

has_collider_path <- function(x) {
  x <- if_not_tidy_daggity(x)
  suppressWarnings(is_false(is.null(pull_dag_data(x)$collider_line)))
}

n_nodes <- function(x) {
  dplyr::n_distinct(pull_dag_data(x)$name)
}

n_edges <- function(x) {
  sum(!is.na(pull_dag_data(x)$direction)) - n_collder_paths(x)
}

n_collder_paths <- function(x) {
  if (has_collider_path(x)) n <- sum(pull_dag_data(x)$collider_line) else n <- 0
  n
}

collider_paths <- function(x) {
  if (has_collider_path(x)) {
    paths <- pull_dag_data(x) %>%
      dplyr::filter(collider_line) %>%
      dplyr::mutate(collider_path_nodes = paste(name, "<->", to)) %>%
      dplyr::pull(collider_path_nodes)
  } else {
    paths <- c()
  }
  paths
}

#' @noRd
expansion <- function(...) {
  if (ggplot2_version() >= "3.3.0") {
    ggplot2::expansion(...)
  } else {
    ggplot2::expand_scale(...)
  }
}

#' @importFrom utils packageVersion
#' @noRd
ggplot2_version <- function() {
  utils::packageVersion("ggplot2")
}

#' @importFrom utils packageVersion
#' @noRd
dplyr_version <- function() {
  utils::packageVersion("dplyr")
}

ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

ggdag_left_join <- function(...) {
  if (dplyr_version() >= "1.1.1") {
    dplyr::left_join(..., multiple = "all", relationship = "many-to-many")
  } else if (dplyr_version() == "1.1.0") {
    dplyr::left_join(..., multiple = "all")
  } else {
    dplyr::left_join(...)
  }
}

`%nin%` <- Negate(`%in%`)

check_arg_node <- function(node, use_nodes, what = "geom_dag") {
  if (is_present(node)) {
    deprecate_soft("0.3.0", paste0(what, "(node)"), paste0(what, "(use_nodes)"))
    use_nodes <- node
  } else {
    use_nodes
  }
}

check_arg_node <- function(node, use_nodes, what = "geom_dag") {
  if (is_present(node)) {
    deprecate_soft("0.3.0", paste0(what, "(node)"), paste0(what, "(use_nodes)"))
    use_nodes <- node
  }

  use_nodes
}

check_arg_stylized <- function(stylized, use_stylized, what = "geom_dag") {
  if (is_present(stylized)) {
    deprecate_soft("0.3.0", paste0(what, "(stylized)"), paste0(what, "(stylized)"))
    use_stylized <- stylized
  }

  use_stylized
}

ggraph_create_layout <- function(...) {
  .df <- suppressMessages(ggraph::create_layout(...))
  # ggdag doesn't need the igraph object
  attr(.df, "graph") <- NULL

  .df
}
