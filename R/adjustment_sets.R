#' Covariate Adjustment Sets
#'
#' See [dagitty::adjustmentSets()] for details.
#'
#' @param .tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param exposure a character vector, the exposure variable. Default is
#'   `NULL`, in which case it will be determined from the DAG.
#' @param outcome a character vector, the outcome variable. Default is
#'   `NULL`, in which case it will be determined from the DAG.
#' @param ... additional arguments to `adjustmentSets`
#' @param shadow logical. Show paths blocked by adjustment?
#' @inheritParams geom_dag
#' @inheritParams expand_plot
#'
#' @return a `tidy_dagitty` with an `adjusted` column and `set`
#'   column, indicating adjustment status and DAG ID, respectively, for the
#'   adjustment sets or a `ggplot`
#' @export
#'
#' @examples
#' dag <- dagify(
#'   y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   w1 ~ ~w2,
#'   exposure = "x",
#'   outcome = "y"
#' )
#'
#' tidy_dagitty(dag) |> dag_adjustment_sets()
#'
#' ggdag_adjustment_set(dag)
#'
#' ggdag_adjustment_set(
#'   dagitty::randomDAG(10, .5),
#'   exposure = "x3",
#'   outcome = "x5"
#' )
#'
#' @rdname adjustment_sets
#' @name Covariate Adjustment Sets
dag_adjustment_sets <- function(
  .tdy_dag,
  exposure = NULL,
  outcome = NULL,
  ...
) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)
  sets <- dagitty::adjustmentSets(
    pull_dag(.tdy_dag),
    exposure = exposure,
    outcome = outcome,
    ...
  )
  is_empty_set <- purrr::is_empty(sets)
  if (is_empty_set) {
    warning(
      "Failed to close backdoor paths. Common reasons include:
            * graph is not acyclic
            * backdoor paths are not closeable with given set of variables
            * necessary variables are unmeasured (latent)"
    )
    sets <- "(No Way to Block Backdoor Paths)"
  } else {
    sets <- extract_sets(sets)
  }

  update_dag_data(.tdy_dag) <-
    purrr::map_df(
      sets,
      \(.x) dplyr::mutate(
        pull_dag_data(.tdy_dag),
        adjusted = ifelse(name %in% .x, "adjusted", "unadjusted"),
        set = paste0("{", paste(.x, collapse = ", "), "}")
      )
    )

  .tdy_dag
}

extract_sets <- function(sets) {
  sets <- unname(as.list(sets))
  sets <- purrr::map_if(
    sets,
    purrr::is_empty,
    ~"(Backdoor Paths Unconditionally Closed)"
  )
}


#' @rdname adjustment_sets
#' @export
ggdag_adjustment_set <- function(
  .tdy_dag,
  exposure = NULL,
  outcome = NULL,
  ...,
  shadow = TRUE,
  size = 1,
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 10,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated(),
  expand_x = expansion(c(0.25, 0.25)),
  expand_y = expansion(c(0.2, 0.2))
) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) |>
    dag_adjustment_sets(exposure = exposure, outcome = outcome, ...)

  p <- ggplot2::ggplot(
    .tdy_dag,
    aes_dag(shape = adjusted, color = adjusted)
  ) +
    ggplot2::facet_wrap(~set) +
    scale_adjusted() +
    expand_plot(expand_x = expand_x, expand_y = expand_y)

  if (shadow) {
    vals <- c("unadjusted" = "black", "adjusted" = "grey80")
  } else {
    vals <- c("unadjusted" = "black", "adjusted" = "#FFFFFF00")
  }

  p <- p +
    geom_dag_edges(
      ggplot2::aes(edge_colour = adjusted),
      show.legend = if (shadow) TRUE else FALSE
    ) +
    ggraph::scale_edge_colour_manual(
      drop = FALSE,
      values = vals,
      limits = names(vals)
    )

  p <- p +
    geom_dag(
      size = size,
      node_size = node_size,
      text_size = text_size,
      label_size = label_size,
      text_col = text_col,
      label_col = label_col,
      edge_width = edge_width,
      edge_cap = edge_cap,
      arrow_length = arrow_length,
      use_edges = FALSE,
      use_nodes = use_nodes,
      use_stylized = use_stylized,
      use_text = use_text,
      use_labels = use_labels,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )

  p
}

#' Assess if a variable confounds a relationship
#'
#' @param .tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param z a character vector, the potential confounder
#' @param x,y a character vector, the variables z may confound.
#' @param direct logical. Only consider direct confounding? Default is
#'   `FALSE`
#'
#' @return Logical. Is the variable a confounder?
#' @export
#'
#' @examples
#' dag <- dagify(y ~ z, x ~ z)
#'
#' is_confounder(dag, "z", "x", "y")
#' is_confounder(dag, "x", "z", "y")
#'
is_confounder <- function(.tdy_dag, z, x, y, direct = FALSE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)
  dag <- pull_dag(.tdy_dag)

  if (direct) {
    z_descendants <- dagitty::children(dag, z)
  } else {
    z_descendants <- dagitty::descendants(dag, z)[-1]
  }
  all(c(x, y) %in% z_descendants)
}

#' Adjust for variables and activate any biasing paths that result
#'
#' @param .tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param var a character vector, the variable(s) to adjust for.
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @inheritParams geom_dag
#' @param collider_lines logical. Should the plot show paths activated by
#'   adjusting for a collider?
#' @param as_factor logical. Should the `adjusted` column be a factor?
#' @param activate_colliders logical. Include colliders activated by adjustment?
#'
#' @return a `tidy_dagitty` with a `adjusted` column for adjusted
#'   variables, as well as any biasing paths that arise, or a `ggplot`
#' @export
#'
#' @examples
#' dag <- dagify(m ~ a + b, x ~ a, y ~ b)
#'
#' control_for(dag, var = "m")
#' ggdag_adjust(dag, var = "m")
#'
#' @rdname control_for
#' @name Adjust for variables
control_for <- function(
  .tdy_dag,
  var,
  as_factor = TRUE,
  activate_colliders = TRUE,
  ...
) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag, ...)
  updated_dag <- pull_dag(.tdy_dag)
  dagitty::adjustedNodes(updated_dag) <- var
  update_dag(.tdy_dag) <- updated_dag
  if (isTRUE(activate_colliders)) {
    .tdy_dag <- activate_collider_paths(.tdy_dag, var)
  }
  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    adjusted = ifelse(name %in% var, "adjusted", "unadjusted")
  )
  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      adjusted = factor(adjusted, exclude = NA)
    )
  }
  .tdy_dag
}

#' @rdname control_for
#' @export
adjust_for <- control_for

#' @rdname control_for
#' @export
ggdag_adjust <- function(
  .tdy_dag,
  var = NULL,
  ...,
  size = 1,
  edge_type = c("link_arc", "link", "arc", "diagonal"),
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 10,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag, ...)
  if (!is.null(var)) {
    .tdy_dag <- .tdy_dag |> control_for(var)
  } else {
    var <- dagitty::adjustedNodes(pull_dag(.tdy_dag))
    if (is.null(var)) {
      stop(
        "an adjusting variable needs to be set, either via `var` or `control_for()`"
      )
    }
    if (is.null(pull_dag_data(.tdy_dag)$adjusted)) {
      .tdy_dag <- .tdy_dag |> control_for(var)
    }
  }

  p <- .tdy_dag |>
    ggplot2::ggplot(aes_dag(col = adjusted, shape = adjusted)) +
    geom_dag_edges(
      ggplot2::aes(edge_alpha = adjusted),
      start_cap = ggraph::circle(edge_cap, "mm"),
      end_cap = ggraph::circle(edge_cap, "mm")
    ) +
    scale_adjusted(include_alpha = TRUE) +
    expand_plot(expand_y = expansion(c(0.2, 0.2)))

  if (collider_lines) {
    p <- p + geom_dag_collider_edges()
  }

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
      use_edges = FALSE,
      use_nodes = use_nodes,
      use_stylized = use_stylized,
      use_text = use_text,
      use_labels = use_labels,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )

  p
}
