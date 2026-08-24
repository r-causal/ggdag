#' Covariate Adjustment Sets
#'
#' See [dagitty::adjustmentSets()] for details.
#'
#' @inheritParams dag_params
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
#'   dagitty::randomDAG(10, 0.5),
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
  endpoints <- resolve_endpoints(pull_dag(.tdy_dag), exposure, outcome)
  sets <- dagitty::adjustmentSets(
    pull_dag(.tdy_dag),
    exposure = endpoints$exposure,
    outcome = endpoints$outcome,
    ...
  )
  is_empty_set <- purrr::is_empty(sets)
  if (is_empty_set) {
    warn(
      c(
        "Failed to close all backdoor paths.",
        "!" = "Common reasons include:",
        "*" = "Graph is not acyclic",
        "*" = "Backdoor paths are not closeable with given set of variables",
        "*" = "Necessary variables are unmeasured (latent)"
      ),
      warning_class = "ggdag_failed_to_close_backdoor_warning"
    )
    sets <- "(No Way to Block Backdoor Paths)"
  } else {
    sets <- extract_sets(sets)
  }

  update_dag_data(.tdy_dag) <-
    purrr::map_df(
      sets,
      \(.x) {
        dplyr::mutate(
          pull_dag_data(.tdy_dag),
          adjusted = ifelse(.data$name %in% .x, "adjusted", "unadjusted"),
          set = paste0("{", paste(.x, collapse = ", "), "}")
        )
      }
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
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option_proportional("edge_cap", 8, 10),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  unified_legend = TRUE,
  key_glyph = draw_key_dag_point,
  label = NULL,
  text = NULL,
  edge_engine = ggdag_option("edge_engine", "ggraph"),
  node = deprecated(),
  stylized = deprecated(),
  expand_x = expansion(c(0.25, 0.25)),
  expand_y = expansion(c(0.2, 0.2))
) {
  edge_engine <- match.arg(edge_engine, c("ggraph", "ggarrow"))

  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) |>
    dag_adjustment_sets(exposure = exposure, outcome = outcome, ...) |>
    dplyr::mutate(
      blocked = ifelse(
        .data$adjusted == "unadjusted",
        NA,
        "blocked by\nadjustment"
      )
    )

  p <- ggplot2::ggplot(
    .tdy_dag,
    aes_dag(shape = .data$adjusted, color = .data$adjusted)
  ) +
    ggplot2::facet_wrap(~set) +
    scale_adjusted() +
    expand_plot(expand_x = expand_x, expand_y = expand_y)

  if (use_edges) {
    if (identical(edge_engine, "ggarrow")) {
      rlang::check_installed(
        "ggarrow",
        reason = "to use edge_engine = \"ggarrow\"."
      )
      resect <- edge_cap * size
      arrow_head <- ggdag_option("arrow_head", NULL) %||%
        ggarrow::arrow_head_wings()
      arrow_fins <- ggdag_option("arrow_fins", NULL)

      blocked_colour <- if (shadow) "grey80" else "#FFFFFF00"
      edge_mapping <- with_edge_curvature(NULL, p$data)

      p <- p +
        quick_plot_arrow_edges(
          mapping = edge_mapping,
          data_directed = function(x) {
            dplyr::filter(x, is.na(.data$blocked), .data$direction == "->")
          },
          data_bidirected = function(x) {
            dplyr::filter(x, is.na(.data$blocked), .data$direction == "<->")
          },
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          resect = resect,
          linewidth = edge_width * size,
          length = arrow_length_unit(arrow_length * size),
          colour = "black",
          show.legend = FALSE
        ) +
        quick_plot_arrow_edges(
          mapping = edge_mapping,
          data_directed = function(x) {
            dplyr::filter(x, !is.na(.data$blocked), .data$direction == "->")
          },
          data_bidirected = function(x) {
            dplyr::filter(x, !is.na(.data$blocked), .data$direction == "<->")
          },
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          resect = resect,
          linewidth = edge_width * size,
          length = arrow_length_unit(arrow_length * size),
          colour = blocked_colour,
          show.legend = FALSE
        )
    } else {
      warn_if_curvature_ignored(p$data)

      vals <- if (shadow) {
        c("blocked by\nadjustment" = "grey80")
      } else {
        c("blocked by\nadjustment" = "#FFFFFF00")
      }

      p <- p +
        drop_empty_edge_layers(
          quick_plot_dag_edges(
            ggplot2::aes(edge_colour = .data$blocked),
            edge_cap = edge_cap,
            edge_width = edge_width,
            arrow_length = arrow_length,
            size = size,
            show.legend = if (shadow) NA else FALSE
          ),
          pull_dag_data(.tdy_dag)
        )

      p <- p +
        ggraph::scale_edge_colour_manual(
          name = "",
          drop = TRUE,
          values = vals,
          limits = names(vals),
          na.value = "black"
        )
    }
  }

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
      edge_engine = edge_engine,
      use_edges = FALSE,
      use_nodes = use_nodes,
      use_stylized = use_stylized,
      use_text = use_text,
      use_labels = use_labels,
      label_geom = label_geom,
      unified_legend = unified_legend,
      key_glyph = key_glyph,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )

  p
}

#' Assess if a variable confounds a relationship
#'
#' @inheritParams dag_params
#' @param z a character vector, the potential confounder
#' @param x,y a character vector, the variables z may confound.
#' @param direct logical. Only consider direct confounding? Default is
#'   `FALSE`
#'
#' @details
#' A confounder is a common cause of `x` and `y`. `z` therefore has to reach
#' `x` by a directed path that does not run through `y`, and reach `y` by a
#' directed path that does not run through `x`. Being a descendant of `z` is
#' not enough: descent is transitive through `x`, so every upstream cause of the
#' exposure, such as an instrument, would qualify even though it opens no
#' backdoor path.
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
    return(all(c(x, y) %in% dagitty::children(dag, z)))
  }

  reaches_avoiding(dag, z, x, .avoid = y) &&
    reaches_avoiding(dag, z, y, .avoid = x)
}

#' Does a directed path run from one set of variables to another?
#'
#' Only directed edges are followed: a bidirected edge marks an unmeasured
#' common cause rather than causation out of `.from`. Nodes in `.avoid` are
#' removed from the graph, so a path that only reaches its target through one
#' of them does not count. `.from` is excluded from the reachable set, so a
#' variable never reaches itself.
#'
#' @param .dag A `dagitty` object.
#' @param .from,.targets,.avoid Character vectors of node names.
#' @return Logical.
#' @noRd
reaches_avoiding <- function(.dag, .from, .targets, .avoid) {
  .edges <- dagitty::edges(.dag)
  from_node <- as.character(.edges$v)
  to_node <- as.character(.edges$w)
  keep <- as.character(.edges$e) == "->" &
    !(from_node %in% .avoid) &
    !(to_node %in% .avoid)
  from_node <- from_node[keep]
  to_node <- to_node[keep]

  visited <- character(0)
  frontier <- setdiff(.from, .avoid)
  while (length(frontier) > 0) {
    visited <- union(visited, frontier)
    frontier <- setdiff(to_node[from_node %in% frontier], visited)
  }

  all(.targets %in% setdiff(visited, .from))
}

#' Adjust for variables and activate any biasing paths that result
#'
#' @inheritParams dag_params
#' @param var the variable(s) to adjust for. This can be a character vector of
#'   variable names or a list of the form `list(c(...))`.
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @inheritParams geom_dag
#' @param collider_lines logical. Should the plot show paths activated by
#'   adjusting for a collider? These paths are drawn as dashed ggraph curves
#'   whatever `edge_engine` is in use: they mark an association rather than an
#'   edge of the DAG, so they stay visibly apart from the arrows the engine
#'   draws.
#' @inheritParams dag_params
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
  var <- flatten_node_names(var)
  validate_nodes_exist(.tdy_dag, var, arg = "var")
  updated_dag <- pull_dag(.tdy_dag)
  dagitty::adjustedNodes(updated_dag) <- var
  update_dag(.tdy_dag) <- updated_dag
  if (isTRUE(activate_colliders)) {
    .tdy_dag <- activate_collider_paths(.tdy_dag, var)
  }
  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    adjusted = ifelse(.data$name %in% var, "adjusted", "unadjusted")
  )
  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      adjusted = factor(.data$adjusted, exclude = NA)
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
  node_size = ggdag_option("node_size", 16),
  text_size = ggdag_option("text_size", 3.88),
  label_size = ggdag_option("label_size", text_size),
  text_col = ggdag_option("text_col", "white"),
  label_col = ggdag_option("label_col", "black"),
  edge_width = ggdag_option("edge_width", 0.6),
  edge_cap = ggdag_option_proportional("edge_cap", 8, 10),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_repel),
  unified_legend = TRUE,
  key_glyph = draw_key_dag_point,
  edge_engine = ggdag_option("edge_engine", "ggraph"),
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
) {
  if (missing(edge_type)) {
    edge_type <- ggdag_option("edge_type", "link_arc")
  }
  edge_engine <- match.arg(edge_engine, c("ggraph", "ggarrow"))
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag, ...)
  if (!is_empty_or_null(var)) {
    .tdy_dag <- .tdy_dag |> control_for(var)
  } else {
    # `dagitty::adjustedNodes()` reports an unadjusted DAG as an empty list
    # rather than as `NULL`
    var <- dagitty::adjustedNodes(pull_dag(.tdy_dag))
    if (is_empty_or_null(var)) {
      abort(
        c(
          "An adjusting variable needs to be set.",
          "i" = "Use {.arg var} or {.fun control_for} to specify adjusting variables."
        ),
        error_class = "ggdag_missing_error"
      )
    }
    if (!"adjusted" %in% names(pull_dag_data(.tdy_dag))) {
      .tdy_dag <- .tdy_dag |> control_for(var)
    }
  }

  p <- .tdy_dag |>
    ggplot2::ggplot(aes_dag(col = .data$adjusted, shape = .data$adjusted)) +
    scale_adjusted(include_alpha = TRUE) +
    expand_plot(expand_y = expansion(c(0.2, 0.2)))

  if (use_edges) {
    if (identical(edge_engine, "ggarrow")) {
      rlang::check_installed(
        "ggarrow",
        reason = "to use edge_engine = \"ggarrow\"."
      )

      p <- p +
        quick_plot_arrow_edges(
          mapping = with_edge_curvature(
            ggplot2::aes(alpha = .data$adjusted),
            p$data
          ),
          data_directed = filter_direction("->"),
          data_bidirected = filter_direction("<->"),
          arrow_head = ggdag_option("arrow_head", NULL) %||%
            ggarrow::arrow_head_wings(),
          arrow_fins = ggdag_option("arrow_fins", NULL),
          resect = edge_cap * size,
          linewidth = edge_width * size,
          length = arrow_length_unit(arrow_length * size),
          show.legend = FALSE
        )
    } else {
      warn_if_curvature_ignored(p$data)

      p <- p +
        drop_empty_edge_layers(
          quick_plot_dag_edges(
            ggplot2::aes(edge_alpha = .data$adjusted),
            edge_type = edge_type,
            edge_cap = edge_cap,
            edge_width = edge_width,
            arrow_length = arrow_length,
            size = size
          ),
          pull_dag_data(.tdy_dag)
        )
    }

    if (collider_lines) {
      p <- p + geom_dag_collider_edges()
    }
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
      edge_engine = edge_engine,
      use_edges = FALSE,
      use_nodes = use_nodes,
      use_stylized = use_stylized,
      use_text = use_text,
      use_labels = use_labels,
      label_geom = label_geom,
      unified_legend = unified_legend,
      key_glyph = key_glyph,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )

  p
}
