#' Find colliders
#'
#' Detects any colliders given a DAG.
#' `node_collider` tags colliders and `ggdag_collider` plots all
#' colliders.
#'
#' @inheritParams dag_params
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @inheritParams geom_dag
#'
#' @return a `tidy_dagitty` with a `colliders` column for
#'   colliders or a `ggplot`
#' @export
#'
#' @examples
#' dag <- dagify(m ~ x + y, y ~ x)
#'
#' node_collider(dag)
#' ggdag_collider(dag)
#'
#' @rdname colliders
#' @name Colliders
node_collider <- function(.dag, as_factor = TRUE, ...) {
  .tdy_dag <- if_not_tidy_daggity(.dag, ...)
  # drop the results of an earlier application so the join does not suffix
  .tdy_dag <- dplyr::select(.tdy_dag, -dplyr::any_of("colliders"))
  vars <- unique(pull_dag_data(.tdy_dag)$name)
  colliders <- purrr::map_lgl(vars, \(.x) is_collider(.tdy_dag, .x))
  names(colliders) <- vars
  .tdy_dag <- dplyr::left_join(
    .tdy_dag,
    tibble::enframe(colliders, value = "colliders"),
    by = "name"
  )
  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      colliders = factor(
        as.numeric(colliders),
        levels = 0:1,
        labels = c("Non-Collider", "Collider")
      )
    )
  }

  .tdy_dag
}

#' @rdname colliders
#' @export
ggdag_collider <- function(
  .tdy_dag,
  ...,
  size = 1,
  edge_type = c("link_arc", "link", "arc", "diagonal"),
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option("edge_cap", 8),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  unified_legend = TRUE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  if (missing(edge_type)) {
    edge_type <- ggdag_option("edge_type", "link_arc")
  }

  p <- if_not_tidy_daggity(.tdy_dag, ...) |>
    node_collider() |>
    dplyr::mutate(colliders = forcats::fct_rev(.data$colliders)) |>
    ggplot2::ggplot(aes_dag(color = .data$colliders))

  p <- p +
    geom_dag(
      size = size,
      edge_type = edge_type,
      node_size = node_size,
      text_size = text_size,
      label_size = label_size,
      text_col = text_col,
      label_col = label_col,
      edge_width = edge_width,
      edge_cap = edge_cap,
      arrow_length = arrow_length,
      use_edges = use_edges,
      use_nodes = use_nodes,
      use_stylized = use_stylized,
      use_text = use_text,
      use_labels = use_labels,
      label_geom = label_geom,
      unified_legend = unified_legend,
      key_glyph = draw_key_dag_point,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )

  p
}

#' Activate paths opened by stratifying on a collider
#'
#' Stratifying on colliders can open biasing pathways between variables.
#' `activate_collider_paths` activates any such pathways given a variable
#' or set of variables to adjust for and adds them to the `tidy_dagitty`.
#' A pathway is added for a pair of variables upstream of an adjusted collider
#' only when the adjustment opens a path between them that is closed without
#' it, so variables joined only by a path that the adjustment leaves as it
#' found it are not connected. Openness is judged under the whole of
#' `adjust_for`, so adjusting for a collider and for a variable that blocks the
#' path it opens leaves the pair unconnected.
#'
#' @inheritParams dag_params
#' @param adjust_for a character vector, the variable(s) to adjust for.
#' @param ... additional arguments passed to `tidy_dagitty()`. These are only
#'   used when `.tdy_dag` is not already a `tidy_dagitty`.
#'
#' @return a `tidy_dagitty` with additional rows for collider-activated
#'   pathways
#' @export
#'
#' @examples
#' dag <- dagify(m ~ x + y, x ~ y)
#'
#' collided_dag <- activate_collider_paths(dag, adjust_for = "m")
#' collided_dag
#'
#' @seealso [control_for()], [ggdag_adjust()],
#'   [geom_dag_collider_edges()]
activate_collider_paths <- function(.tdy_dag, adjust_for, ...) {
  if (is.tidy_dagitty(.tdy_dag)) {
    check_tidy_dots_empty(...)
  }
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag, ...)
  vars <- unique(pull_dag_data(.tdy_dag)$name)
  colliders <- purrr::map_lgl(vars, \(.x) is_collider(.tdy_dag, .x))
  downstream_colliders <- purrr::map_lgl(
    vars,
    \(.x) is_downstream_collider(.tdy_dag, .x)
  )
  collider_names <- unique(c(vars[colliders], vars[downstream_colliders]))

  if (!any((collider_names %in% adjust_for))) {
    return(dplyr::mutate(.tdy_dag, collider_line = FALSE))
  }
  adjusted_colliders <- collider_names[collider_names %in% adjust_for]
  .dag <- pull_dag(.tdy_dag)

  candidate_pairs <- adjusted_colliders |>
    purrr::map(\(.x) collider_flanks(.dag, .x, adjust_for)) |>
    purrr::map(unique_pairs) |>
    purrr::list_rbind() |>
    sort_pairs()

  # the openness test is run outside {dplyr}, whose data mask catches a
  # warning raised inside it and re-signals a summary of its own, which drops
  # both the condition's class and the pairs it names
  openness <- purrr::map2(
    candidate_pairs$Var1,
    candidate_pairs$Var2,
    \(.u, .v) path_openness(.dag, .u, .v, adjust_for)
  )

  warn_truncated_pairs(candidate_pairs, purrr::map_lgl(openness, "truncated"))

  activated_pairs <- candidate_pairs[purrr::map_lgl(openness, "opened"), ]

  if (nrow(activated_pairs) == 0) {
    return(dplyr::mutate(.tdy_dag, collider_line = FALSE))
  }

  collider_lines <- dagify_colliders(activated_pairs, .tdy_dag)

  collider_lines$collider_line <- TRUE
  .tdy_dag <- dplyr::mutate(.tdy_dag, collider_line = FALSE)
  update_dag_data(.tdy_dag) <- dplyr::bind_rows(
    pull_dag_data(.tdy_dag),
    collider_lines
  )
  .tdy_dag
}

dagify_colliders <- function(.pairs_df, .tdy_dag) {
  .pairs_df |>
    join_lhs_coords(.tdy_dag) |>
    join_rhs_coords(.tdy_dag) |>
    dplyr::mutate(
      direction = factor("<->", levels = c("<-", "->", "<->"), exclude = NA)
    ) |>
    dplyr::rename(name = "Var1", to = "Var2")
}

join_lhs_coords <- function(.x, .y) {
  ggdag_left_join(
    .x,
    pull_dag_data(.y) |>
      dplyr::select("name", "x", "y") |>
      dplyr::distinct(),
    by = c("Var1" = "name")
  )
}

join_rhs_coords <- function(.x, .y) {
  ggdag_left_join(
    .x,
    pull_dag_data(.y) |>
      dplyr::select("name", xend = "x", yend = "y") |>
      dplyr::distinct(),
    by = c("Var2" = "name")
  )
}

#' The variables flanking a collider
#'
#' Conditioning on a collider can only open a path that runs into it, so the
#' variables an adjusted collider can join are the ones upstream of it: its
#' proper ancestors together with its bidirected partners, which have an
#' arrowhead into it without being ancestors. Variables that are themselves
#' adjusted are dropped, since conditioning on them blocks the path at that
#' point, whether or not they are colliders.
#'
#' @param .dag A `dagitty` object.
#' @param .var A character vector of length 1, the adjusted collider.
#' @param .adjusted A character vector, every adjusted variable.
#' @return A character vector.
#' @noRd
collider_flanks <- function(.dag, .var, .adjusted) {
  flanks <- union(
    dagitty::ancestors(.dag, .var, proper = TRUE),
    dagitty::spouses(.dag, .var)
  )

  setdiff(flanks, .adjusted)
}

#' Order each pair alphabetically and drop repeats
#'
#' Two adjusted colliders can flag the same pair of variables, in either order.
#' Sorting within the pair makes those rows identical so that the pair is drawn
#' once.
#'
#' @param .pairs_df A data frame with `Var1` and `Var2` columns.
#' @return A data frame of the same shape.
#' @noRd
sort_pairs <- function(.pairs_df) {
  sorted <- tibble::tibble(
    Var1 = pmin(.pairs_df$Var1, .pairs_df$Var2),
    Var2 = pmax(.pairs_df$Var1, .pairs_df$Var2)
  )

  dplyr::distinct(sorted)
}

#' The number of paths to enumerate when testing for an activated path
#'
#' `dagitty::paths()` truncates at 100 paths by default, which is low enough
#' for a moderately dense DAG to hide the path an adjustment opens.
#' @noRd
collider_path_limit <- 1000

#' Does adjusting for a set of variables open a path between two variables?
#'
#' A pair is joined by an activated line only when conditioning on the adjusted
#' variables opens a path between the two that is closed without the
#' adjustment. A path that is open either way carries no collider bias, and a
#' path that stays closed carries nothing at all. The two enumerations are
#' matched on the path itself rather than on position, since dagitty need not
#' return them in the same order.
#'
#' @param .dag A `dagitty` object.
#' @param .from,.to A character vector of length 1.
#' @param .adjusted A character vector, the variables adjusted for.
#' @return A list of `opened`, whether the adjustment opens a path, and
#'   `truncated`, whether either enumeration reached its limit.
#' @noRd
path_openness <- function(.dag, .from, .to, .adjusted) {
  adjusted_paths <- dagitty::paths(
    .dag,
    .from,
    .to,
    Z = .adjusted,
    limit = collider_path_limit
  )
  unadjusted_paths <- dagitty::paths(
    .dag,
    .from,
    .to,
    limit = collider_path_limit
  )

  n_paths <- max(
    length(adjusted_paths$paths),
    length(unadjusted_paths$paths)
  )
  truncated <- n_paths >= collider_path_limit

  # `dagitty` returns empty lists, not empty vectors, when no path joins the
  # two variables, and a list is not something `&` can work on
  if (rlang::is_empty(adjusted_paths$paths)) {
    return(list(opened = FALSE, truncated = truncated))
  }

  open_without_adjustment <- as.logical(unlist(unadjusted_paths$open))[
    match(adjusted_paths$paths, unadjusted_paths$paths)
  ]

  opened <- any(
    as.logical(unlist(adjusted_paths$open)) &
      !is.na(open_without_adjustment) &
      !open_without_adjustment
  )

  list(opened = opened, truncated = truncated)
}

#' @rdname path_openness
#' @noRd
adjustment_opens_path <- function(.dag, .from, .to, .adjusted) {
  openness <- path_openness(.dag, .from, .to, .adjusted)
  if (openness$truncated) {
    warn_truncated(.from, .to)
  }

  openness$opened
}

#' Warn once for the pairs whose path enumeration reached its limit
#'
#' `dagitty::paths()` stops at `limit` paths and says nothing about it, so a
#' pair of variables in a dense DAG can silently miss the path the adjustment
#' opens. One warning covers every pair of a single call, since a dense DAG
#' truncates for many pairs at once.
#'
#' @param .pairs_df A data frame with `Var1` and `Var2` columns.
#' @param truncated A logical vector along the rows of `.pairs_df`.
#' @return `NULL`, invisibly.
#' @noRd
warn_truncated_pairs <- function(
  .pairs_df,
  truncated,
  call = rlang::caller_env()
) {
  if (!any(truncated)) {
    return(invisible(NULL))
  }

  warn_truncated(
    .pairs_df$Var1[truncated],
    .pairs_df$Var2[truncated],
    call = call
  )
}

#' @param from,to Character vectors of the same length, the truncated pairs.
#' @rdname warn_truncated_pairs
#' @noRd
warn_truncated <- function(from, to, call = rlang::caller_env()) {
  #  naming both variables is more use than a list holding one pair
  header <- if (length(from) == 1) {
    "Only the first {collider_path_limit} paths between {.val {from}} and {.val {to}} were checked."
  } else {
    pairs <- paste(from, "and", to)
    c(
      "Only the first {collider_path_limit} paths were checked for {length(pairs)} pairs of variables.",
      "!" = "Truncated: {.val {pairs}}"
    )
  }

  warn(
    c(
      header,
      "!" = "A pathway opened by the adjustment may be missing from the plot.",
      "i" = "Consider a smaller DAG or a sparser set of variables to adjust for."
    ),
    warning_class = "ggdag_path_limit_warning",
    call = call
  )

  invisible(NULL)
}

#' Detecting colliders in DAGs
#'
#' A collider is a variable that two edges point into. Bidirected edges count,
#' so a variable with one directed parent and one bidirected partner is a
#' collider, as is a variable with two bidirected partners and no parents.
#'
#' @param .dag an input graph, an object of class `tidy_dagitty` or `dagitty`
#' @param .var a character vector of length 1, the potential collider to check
#' @param downstream Logical. Check for downstream colliders? Default is `TRUE`.
#'
#' @return Logical. Is the variable a collider or downstream collider?
#' @export
#'
#' @examples
#' dag <- dagify(m ~ x + y, m_jr ~ m)
#' is_collider(dag, "m")
#' is_downstream_collider(dag, "m_jr")
#'
#' #  a downstream collider is also treated as a collider
#' is_collider(dag, "m_jr")
#'
#' #  but a direct collider is not treated as a downstream collider
#' is_downstream_collider(dag, "m")
#'
#' @rdname is_collider
#' @name Test if Variable Is Collider
is_collider <- function(.dag, .var, downstream = TRUE) {
  if (is.tidy_dagitty(.dag)) {
    .dag <- pull_dag(.dag)
  }
  validate_nodes_exist(.dag, .var, arg = ".var")
  collider <- has_multiple_arrowheads(.dag, .var)
  if (!downstream || collider) {
    return(collider)
  }

  any_ancestor_is_collider(.dag, .var)
}

#' @rdname is_collider
#' @export
is_downstream_collider <- function(.dag, .var) {
  if (is.tidy_dagitty(.dag)) {
    .dag <- pull_dag(.dag)
  }
  validate_nodes_exist(.dag, .var, arg = ".var")

  any_ancestor_is_collider(.dag, .var)
}

#' Is any proper ancestor of a variable a collider?
#'
#' Ancestry is transitive, so a variable is downstream of a collider exactly
#' when one of its proper ancestors has more than one arrowhead pointing into
#' it. Recursing into the downstream status of each ancestor would revisit the
#' same ancestors repeatedly, which costs exponentially many dagitty calls on
#' deep DAGs.
#'
#' @param .dag A `dagitty` object.
#' @param .var A character vector of length 1.
#' @return Logical.
#' @noRd
any_ancestor_is_collider <- function(.dag, .var) {
  var_ancestors <- dagitty::ancestors(.dag, .var, proper = TRUE)
  any(purrr::map_lgl(var_ancestors, \(.x) has_multiple_arrowheads(.dag, .x)))
}

#' Does more than one arrowhead point into a variable?
#'
#' A collider is a variable that two edges point into, whether those edges are
#' directed or bidirected. dagitty counts only directed edges as parents and
#' reports bidirected partners as spouses, so both sets contribute.
#'
#' @param .dag A `dagitty` object.
#' @param .var A character vector of length 1.
#' @return Logical.
#' @noRd
has_multiple_arrowheads <- function(.dag, .var) {
  n_arrowheads <- length(dagitty::parents(.dag, .var)) +
    length(dagitty::spouses(.dag, .var))

  n_arrowheads > 1
}
