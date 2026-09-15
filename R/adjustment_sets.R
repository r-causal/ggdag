#' Covariate Adjustment Sets
#'
#' See [dagitty::adjustmentSets()] for details.
#'
#' @inheritParams dag_params
#' @param ... additional arguments to `adjustmentSets`
#' @param shadow logical. Show paths blocked by adjustment?
#' @param collider_lines Logical or `NULL`. Should the plot show the paths that
#'   adjusting for a collider activates? `NULL`, the default, shows them only
#'   when such paths exist and no adjustment set closes the backdoor paths,
#'   which is the case where they explain why. `TRUE` shows them whenever they
#'   exist, and `FALSE` never shows them. These paths are drawn as dashed
#'   ggraph curves whatever `edge_engine` is in use: they mark an association
#'   rather than an edge of the DAG, so they stay visibly apart from the arrows
#'   the engine draws.
#' @inheritParams path_params
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
#' @inheritSection composite_edge_layers Edge layers of the composite plotters
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
    sets <- no_adjustment_set_label
  } else {
    sets <- extract_sets(sets)
  }

  # Each panel normally reports the membership of the set it is named for. The
  # panel for a DAG with no adjustment set names no set, so it reports the
  # adjustment the DAG already carries instead: that adjustment is what
  # activates the collider paths the plot draws as dashed curves, and blanking
  # it would leave the plot showing their consequence with no sign of the cause.
  adjusted_nodes <- if (is_empty_set) {
    list(as.character(dagitty::adjustedNodes(pull_dag(.tdy_dag))))
  } else {
    sets
  }

  update_dag_data(.tdy_dag) <-
    purrr::map2_df(
      sets,
      adjusted_nodes,
      \(.set, .adjusted) {
        dplyr::mutate(
          pull_dag_data(.tdy_dag),
          adjusted = ifelse(
            .data$name %in% .adjusted,
            "adjusted",
            "unadjusted"
          ),
          set = format_adjustment_set(.set)
        )
      }
    )

  .tdy_dag
}

# The `set` label `dag_adjustment_sets()` writes for a DAG whose backdoor paths
# no adjustment set closes.
no_adjustment_set_label <- "(No Way to Block Backdoor Paths)"

# How the `set` column names one adjustment set: its variables inside braces.
format_adjustment_set <- function(.x) {
  paste0("{", paste(.x, collapse = ", "), "}")
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
  edge_cap = ggdag_option_proportional("edge_cap", 8, 10, unset = NULL),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_auto),
  label_wrap = ggdag_option("label_wrap", NULL),
  unified_legend = TRUE,
  key_glyph = draw_key_dag_point,
  label = NULL,
  text = NULL,
  edge_engine = ggdag_option("edge_engine", "ggraph"),
  node = deprecated(),
  stylized = deprecated(),
  expand_x = expansion(c(0.25, 0.25)),
  expand_y = expansion(c(0.2, 0.2)),
  collider_lines = NULL
) {
  edge_engine <- match.arg(edge_engine, c("ggraph", "ggarrow"))
  check_collider_lines(collider_lines)

  .tdy_dag <- if_not_tidy_daggity(.tdy_dag) |>
    dag_adjustment_sets(exposure = exposure, outcome = outcome, ...) |>
    dplyr::mutate(
      blocked = ifelse(
        .data$adjusted == "unadjusted",
        NA,
        "blocked by\nadjustment"
      )
    ) |>
    shadow_rows_first(\(x) !is.na(x$blocked), panel = "set")

  adjusted_breaks <- present_levels(
    pull_dag_data(.tdy_dag)$adjusted,
    c("adjusted", "unadjusted")
  )

  p <- ggplot2::ggplot(
    .tdy_dag,
    aes_dag(shape = .data$adjusted, color = .data$adjusted)
  ) +
    ggplot2::facet_wrap(~set) +
    scale_adjusted(breaks = adjusted_breaks) +
    expand_dag_plot(.tdy_dag, expand_x = expand_x, expand_y = expand_y)

  if (use_edges) {
    if (identical(edge_engine, "ggarrow")) {
      rlang::check_installed(
        "ggarrow",
        reason = "to use edge_engine = \"ggarrow\"."
      )
      resect <- single_edge_cap(edge_cap, node_size) * size
      arrow_head <- ggdag_option("arrow_head", NULL) %||%
        ggarrow::arrow_head_wings()
      arrow_fins <- ggdag_option("arrow_fins", NULL)

      blocked_colour <- if (shadow) "grey80" else "#FFFFFF00"
      edge_mapping <- with_edge_curvature(NULL, p$data)

      # the blocked edges are the context the open ones are read against, so
      # they are added first and the open ones are drawn over them
      edge_layers <- c(
        quick_plot_arrow_edges(
          mapping = edge_mapping,
          data_directed = filter_blocked_direction("->", blocked = TRUE),
          data_bidirected = filter_blocked_direction("<->", blocked = TRUE),
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          resect = resect,
          linewidth = edge_width * size,
          length = arrow_length_unit(arrow_length * size),
          colour = blocked_colour,
          show.legend = FALSE
        ),
        quick_plot_arrow_edges(
          mapping = edge_mapping,
          data_directed = filter_blocked_direction("->", blocked = FALSE),
          data_bidirected = filter_blocked_direction("<->", blocked = FALSE),
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          resect = resect,
          linewidth = edge_width * size,
          length = arrow_length_unit(arrow_length * size),
          colour = "black",
          show.legend = FALSE
        )
      )
      p <- p + follow_nodes_when_unset(edge_layers, edge_cap, node_size, size)
    } else {
      warn_if_ggarrow_only_ignored(p$data)

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
            node_size = node_size,
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
          breaks = present_levels(pull_dag_data(.tdy_dag)$blocked, names(vals)),
          na.value = "black"
        )
    }

    if (draws_collider_lines(collider_lines, p$data)) {
      p <- p + geom_dag_collider_edges()
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
      label_wrap = label_wrap,
      unified_legend = unified_legend,
      key_glyph = key_glyph,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )

  p
}

# The rows one edge layer of the adjustment set plot draws: the edges of a
# single direction, split by whether adjustment blocks them.
# `filter_direction()` also sets aside the curves that adjusting for a collider
# activates, which mark an association rather than an edge of the DAG and are
# drawn by `geom_dag_collider_edges()` instead.
filter_blocked_direction <- function(.direction, blocked) {
  direction_filter <- filter_direction(.direction)

  function(x) {
    x <- if (blocked) {
      dplyr::filter(x, !is.na(.data$blocked))
    } else {
      dplyr::filter(x, is.na(.data$blocked))
    }

    direction_filter(x)
  }
}

# Do the edge rows include a path that adjusting for a collider has activated?
# A DAG that has not been adjusted for anything carries no `collider_line`
# column at all.
has_activated_collider_paths <- function(.data) {
  "collider_line" %in% names(.data) && any(.data$collider_line, na.rm = TRUE)
}

# Does no adjustment set close the backdoor paths? `dag_adjustment_sets()`
# records that case as the single set `no_adjustment_set_label`, the same case
# it warns about, so the `set` column names it and nothing else.
has_no_adjustment_set <- function(.data) {
  if (!"set" %in% names(.data)) {
    return(FALSE)
  }

  identical(unique(.data$set), format_adjustment_set(no_adjustment_set_label))
}

# Should the plot draw the paths that adjusting for a collider activates?
# `NULL` draws them only where they explain something the adjustment sets
# cannot: a set that closes the backdoor paths leaves the activated paths
# nothing to say, so drawing them is noise.
draws_collider_lines <- function(collider_lines, .data) {
  if (!has_activated_collider_paths(.data)) {
    return(FALSE)
  }

  if (is.null(collider_lines)) {
    return(has_no_adjustment_set(.data))
  }

  collider_lines
}

check_collider_lines <- function(collider_lines, call = rlang::caller_env()) {
  if (is.null(collider_lines)) {
    return(invisible(collider_lines))
  }

  if (
    !is.logical(collider_lines) ||
      length(collider_lines) != 1 ||
      is.na(collider_lines)
  ) {
    abort(
      c(
        "{.arg collider_lines} must be {.code NULL}, {.val {TRUE}}, or {.val {FALSE}}.",
        "x" = "You provided {.obj_type_friendly {collider_lines}}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  invisible(collider_lines)
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
#' @inheritSection composite_edge_layers Edge layers of the composite plotters
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
  update_dag(.tdy_dag) <- keep_layout_direction(updated_dag, pull_dag(.tdy_dag))
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
  edge_cap = ggdag_option_proportional("edge_cap", 8, 10, unset = NULL),
  arrow_length = ggdag_option("arrow_length", 5),
  use_edges = ggdag_option("use_edges", TRUE),
  use_nodes = ggdag_option("use_nodes", TRUE),
  use_stylized = ggdag_option("use_stylized", FALSE),
  use_text = ggdag_option("use_text", TRUE),
  use_labels = ggdag_option("use_labels", FALSE),
  label_geom = ggdag_option("label_geom", geom_dag_label_auto),
  label_wrap = ggdag_option("label_wrap", NULL),
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
  edge_type <- check_edge_type(edge_type)
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
    scale_adjusted(
      include_alpha = TRUE,
      breaks = present_levels(
        pull_dag_data(.tdy_dag)$adjusted,
        c("adjusted", "unadjusted")
      )
    ) +
    expand_dag_plot(.tdy_dag, expand_y = expansion(c(0.2, 0.2)))

  if (use_edges) {
    if (identical(edge_engine, "ggarrow")) {
      rlang::check_installed(
        "ggarrow",
        reason = "to use edge_engine = \"ggarrow\"."
      )

      p <- p +
        follow_nodes_when_unset(
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
            resect = single_edge_cap(edge_cap, node_size) * size,
            linewidth = edge_width * size,
            length = arrow_length_unit(arrow_length * size),
            show.legend = FALSE
          ),
          edge_cap,
          node_size,
          size
        )
    } else {
      warn_if_ggarrow_only_ignored(p$data)

      p <- p +
        drop_empty_edge_layers(
          quick_plot_dag_edges(
            ggplot2::aes(edge_alpha = .data$adjusted),
            edge_type = edge_type,
            edge_cap = edge_cap,
            edge_width = edge_width,
            arrow_length = arrow_length,
            size = size,
            node_size = node_size
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
      label_wrap = label_wrap,
      unified_legend = unified_legend,
      key_glyph = key_glyph,
      text = !!rlang::enquo(text),
      label = !!rlang::enquo(label),
      node = node,
      stylized = stylized
    )

  p
}
