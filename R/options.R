#' Global DAG Options
#'
#' Set, get, and reset global default options for DAG appearance. These options
#' are used as defaults by all `geom_dag()`, `ggdag_*()`, and related functions.
#'
#' @details
#' Options are stored in R's global [options()] as `ggdag.<name>`. When an
#' option is `NULL` (the default), each function uses its own built-in
#' default. Setting a global option overrides the built-in default for all
#' functions that use it. Passing `NULL` to `ggdag_options_set()` leaves that
#' one option unset, which returns it to the built-in default and leaves every
#' other option alone. That also makes the previous values
#' `ggdag_options_set()` returns safe to restore with
#' `do.call(ggdag_options_set, old)`.
#'
#' `edge_cap` is unset by default. Under either edge engine, every `ggdag_*()`
#' plotter, [geom_dag()], and the edge layers a plot is assembled from by
#' hand then stop each edge 2 mm outside the outline of the node at each of
#' its ends, following that node's size and shape, so the arrowheads keep the
#' same distance from nodes of any size and from the sides of square nodes as
#' from circles. Setting the option to a number fixes the cap, in millimetres,
#' at every end.
#'
#' The plotters that draw adjusted nodes as squares ([ggdag_adjustment_set()],
#' [ggdag_adjust()], [ggdag_instrumental()], and the d-relationship plotters)
#' maintain a proportional offset: if you set `ggdag.edge_cap` to a custom
#' value, these functions scale it by `10/8`.
#'
#' `edge_route` chooses how the ggarrow engine draws directed edges.
#' `"straight"`, the default, draws chords. `"spline"` routes each directed
#' edge whose path a node blocks around that node with a smooth curve.
#' `"orthogonal"` draws every directed edge as axis-aligned runs with rounded
#' corners, leaving each node through a port and passing the intermediate
#' layers in the gaps between them or along a channel beyond them.
#' Routing happens when the plot is drawn, in the units of the device, so the
#' same DAG re-routes when the plot is resized. Curvature you set yourself,
#' through [curved()], [curve_edge()], or DAGitty control points, is never
#' rerouted, and bidirected edges keep the arc their edge layer draws them
#' with. The option applies to the ggarrow engine with
#' `edge_type = "link_arc"` or `"link"`; it is a no-op for `"arc"` and
#' `"diagonal"`, which already bend every edge.
#'
#' `edge_route_options` carries the constants the router draws with, built by
#' [edge_route_options()]. It is read only when `edge_route` names a routing
#' mode. A field left unset is derived when the plot is drawn, from the node
#' size the plot uses, so an object set once holds at every plot size.
#' [geom_dag_routed_arrows()] takes the same object, and its own `clearance`,
#' `edge_sep`, and `edge_sep_min` arguments override the object's fields for
#' that layer.
#'
#' `curvature` is the bend of the arcs a packaged plot draws: the bidirected
#' arcs of [geom_dag_edges()], every edge under `edge_type = "arc"`, and, under
#' the ggarrow engine, every edge under `edge_type = "diagonal"`, which that
#' engine draws as arcs. 1 approximates a half circle and 0 a straight line,
#' and a negative value bends the other way. Both edge engines read it, but
#' each draws that value with its own depth and to its own side, and it also
#' sets the bow the time-ordered layout clears its nodes of. An edge layer you
#' build yourself takes the `curvature` argument you give it, so
#' [geom_dag_edges_arc()] called directly keeps its own default of 0.5 rather
#' than the option's 0.3.
#'
#' `layout` is the layout [tidy_dagitty()], [ggdag()], and the quick plotting
#' functions use when a DAG carries no coordinates of its own. The default,
#' `"time_ordered"`, places the nodes in time order with
#' [time_ordered_coords()]. `label_geom` is the geom those functions and
#' [geom_dag()] draw labels with when `use_labels = TRUE`. The default is
#' [geom_dag_label_auto()], which places each label deterministically when
#' the plot is drawn. For the look of earlier versions of ggdag, the
#' `"nicely"` layout with repelled labels, set
#' `ggdag_options_set(layout = "nicely", label_geom = geom_dag_label_repel)`.
#'
#' `label_wrap` is a width in characters that [geom_dag()], [ggdag()], and the
#' quick plotting functions that take `use_labels` hand to the automatic label
#' geoms, [geom_dag_label_auto()] and [geom_dag_text_auto()], which wrap their
#' text to it before the labels are measured and placed. `NULL`, the default,
#' wraps nothing. The repel label geoms do no wrapping of their own and are not
#' given it.
#'
#' `debug_repel_points` is a diagnostic rather than an appearance setting. When
#' it is `TRUE`, every repelling label geom (see [geom_dag_label_repel()]) adds
#' a layer of purple points showing the invisible geometry that labels are
#' repelled from: the points traced along each edge and the disc filling each
#' node. It is useful for understanding why a label came to rest where it did.
#'
#' @param ... Named option values to set. See `ggdag_defaults` for valid names
#'   and types.
#' @param name Character string. The option name (without the `ggdag.` prefix).
#'   If `NULL`, returns all currently-set ggdag options.
#' @param default Default value to return if the option is not set. Defaults
#'   to the entry for `name` in `ggdag_defaults`.
#' @param base_default The base default for this option (e.g., 8 for edge_cap).
#' @param override_default The override default used by certain functions
#'   (e.g., 10 for edge_cap in adjustment set functions).
#' @param unset The value returned when the option is not set. Defaults to
#'   `override_default`. The plotters that draw adjusted nodes as squares pass
#'   `NULL`, so that an unset `edge_cap` is left to follow the nodes.
#'
#' @returns
#' - `ggdag_options_set()`: Invisibly returns a named list of the previous
#'   option values.
#' - `ggdag_options_get()`: The option value, or a named list of all set
#'   options if `name` is `NULL`.
#' - `ggdag_options_reset()`: Called for its side effect; returns `NULL`
#'   invisibly.
#' - `ggdag_option()`: The option value if set, otherwise `default`.
#' - `ggdag_option_proportional()`: The scaled option value if set, otherwise
#'   `unset`.
#'
#' @examples
#' # Set global options
#' old <- ggdag_options_set(node_size = 20, text_size = 5)
#'
#' # Check current value
#' ggdag_options_get("node_size")
#'
#' # Reset to defaults
#' ggdag_options_reset()
#'
#' @export
#' @rdname ggdag_options
ggdag_defaults <- list(
  node_size = 16,
  text_size = 3.88,
  label_size = NULL,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = NULL,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_auto,
  edge_type = "link_arc",
  layout = "time_ordered",
  edge_engine = "ggraph",
  arrow_head = NULL,
  arrow_fins = NULL,
  arrow_mid = NULL,
  edge_route = "straight",
  edge_route_options = NULL,
  label_wrap = NULL,
  curvature = 0.3,
  debug_repel_points = FALSE
)

#' @export
#' @rdname ggdag_options
ggdag_options_set <- function(...) {
  dots <- list(...)
  if (length(dots) == 0) {
    return(invisible(list()))
  }

  # an unnamed value names no option, and left alone it would be stored under
  # the bare `ggdag.` prefix, where nothing ever reads it again
  unnamed <- !nzchar(names(dots) %||% rep("", length(dots)))
  if (any(unnamed)) {
    abort(
      c(
        "Every option passed to {.fun ggdag_options_set} must be named.",
        "x" = "{sum(unnamed)} value{?s} {?has/have} no name.",
        "i" = "Valid options: {.val {names(ggdag_defaults)}}."
      ),
      error_class = "ggdag_type_error"
    )
  }

  unknown <- setdiff(names(dots), names(ggdag_defaults))
  if (length(unknown) > 0) {
    abort(
      c(
        "Unknown ggdag option{?s}: {.val {unknown}}.",
        "i" = "Valid options: {.val {names(ggdag_defaults)}}."
      ),
      error_class = "ggdag_type_error"
    )
  }

  # `options()` unsets an option handed `NULL`, which is how an option goes back
  # to the built-in default a function carries, so `NULL` is a value to pass
  # through rather than one to validate
  for (nm in names(dots)) {
    if (!is.null(dots[[nm]])) {
      validate_ggdag_option(nm, dots[[nm]])
    }
  }

  opt_names <- paste0("ggdag.", names(dots))
  old <- stats::setNames(
    lapply(opt_names, getOption),
    names(dots)
  )
  named_opts <- stats::setNames(dots, opt_names)
  do.call(options, named_opts)
  invisible(old)
}

#' @export
#' @rdname ggdag_options
ggdag_options_get <- function(name = NULL) {
  if (!is.null(name)) {
    return(getOption(paste0("ggdag.", name)))
  }
  all_opts <- options()
  ggdag_opts <- all_opts[grepl("^ggdag\\.", names(all_opts))]
  if (length(ggdag_opts) == 0) {
    return(list())
  }
  names(ggdag_opts) <- sub("^ggdag\\.", "", names(ggdag_opts))
  ggdag_opts
}

#' @export
#' @rdname ggdag_options
ggdag_options_reset <- function() {
  opt_names <- paste0("ggdag.", names(ggdag_defaults))
  null_opts <- stats::setNames(
    rep(list(NULL), length(opt_names)),
    opt_names
  )
  do.call(options, null_opts)
  invisible()
}

#' @export
#' @rdname ggdag_options
ggdag_option <- function(name, default = ggdag_defaults[[name]]) {
  getOption(paste0("ggdag.", name), default = default)
}

#' @export
#' @rdname ggdag_options
ggdag_option_proportional <- function(
  name,
  base_default,
  override_default,
  unset = override_default
) {
  user_val <- getOption(paste0("ggdag.", name))
  if (is.null(user_val)) {
    return(unset)
  }
  user_val * (override_default / base_default)
}

# Validation ---------------------------------------------------------------

#' Validate a single ggdag option value
#' @noRd
validate_ggdag_option <- function(name, value, call = rlang::caller_env()) {
  numeric_opts <- c(
    "node_size",
    "text_size",
    "label_size",
    "edge_width",
    "edge_cap",
    "arrow_length"
  )
  logical_opts <- c(
    "use_edges",
    "use_nodes",
    "use_stylized",
    "use_text",
    "use_labels",
    "debug_repel_points"
  )
  character_opts <- c("text_col", "label_col")
  valid_edge_types <- c("link_arc", "link", "arc", "diagonal")

  # every branch rejects `NA` before it reaches a comparison: an `NA` names no
  # value the option can take, and left alone it either makes an `if` condition
  # missing or passes a type check and misbehaves wherever the option is read
  if (name %in% numeric_opts) {
    if (
      !is.numeric(value) || length(value) != 1 || is.na(value) || value <= 0
    ) {
      abort(
        c(
          "{.arg {name}} must be a single positive number.",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  } else if (name %in% logical_opts) {
    if (!is.logical(value) || length(value) != 1 || is.na(value)) {
      abort(
        c(
          "{.arg {name}} must be a single logical value ({.val {TRUE}} or {.val {FALSE}}).",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  } else if (name %in% character_opts) {
    if (!is.character(value) || length(value) != 1 || is.na(value)) {
      abort(
        c(
          "{.arg {name}} must be a single character string.",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  } else if (name == "edge_type") {
    if (
      !is.character(value) || length(value) != 1 || !value %in% valid_edge_types
    ) {
      abort(
        c(
          "{.arg edge_type} must be one of {.val {valid_edge_types}}.",
          "x" = "You provided {.val {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  } else if (name == "label_geom") {
    if (!is.function(value)) {
      abort(
        c(
          "{.arg label_geom} must be a function.",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  } else if (name == "layout") {
    if (!is.character(value) && !is.function(value)) {
      abort(
        c(
          "{.arg layout} must be a single character string or a function.",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
    if (is.character(value) && (length(value) != 1 || is.na(value))) {
      abort(
        c(
          "{.arg layout} must be a single character string or a function.",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  } else if (name == "edge_engine") {
    valid_engines <- c("ggraph", "ggarrow")
    if (
      !is.character(value) || length(value) != 1 || !value %in% valid_engines
    ) {
      abort(
        c(
          "{.arg edge_engine} must be one of {.val {valid_engines}}.",
          "x" = "You provided {.val {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  } else if (name == "edge_route") {
    valid_routes <- c("straight", "spline", "orthogonal")
    if (
      !is.character(value) || length(value) != 1 || !value %in% valid_routes
    ) {
      abort(
        c(
          "{.arg edge_route} must be one of {.val {valid_routes}}.",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  } else if (name == "edge_route_options") {
    if (!inherits(value, "ggdag_edge_route_options")) {
      abort(
        c(
          "{.arg edge_route_options} must be an object from {.fun edge_route_options}.",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  } else if (name %in% c("arrow_head", "arrow_fins", "arrow_mid")) {
    if (!is.null(value) && !is.function(value) && !is.matrix(value)) {
      abort(
        c(
          "{.arg {name}} must be a function, a matrix, or {.code NULL}.",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  } else if (name == "label_wrap") {
    check_label_wrap(value, arg = "label_wrap", allow_na = FALSE, call = call)
  } else if (name == "curvature") {
    if (!is.numeric(value) || length(value) != 1 || is.na(value)) {
      abort(
        c(
          "{.arg curvature} must be a single number.",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  }
}

#' Check a label wrapping width
#'
#' The width is a count of characters, so it has to be a whole number of them
#' and at least one. The automatic label geoms also accept `NA`, which wraps
#' nothing, because that is what their `wrap` argument has always meant; the
#' option does not, because an option is unset with `NULL`.
#'
#' @param value The width to check.
#' @param arg The name of the argument `value` was given as, for the message.
#' @param allow_na Whether a single `NA` is accepted.
#' @param call The calling environment, for the error message.
#' @return `value`, invisibly.
#' @noRd
check_label_wrap <- function(
  value,
  arg = "label_wrap",
  allow_na = FALSE,
  call = rlang::caller_env()
) {
  if (is.null(value)) {
    return(invisible(value))
  }
  if (allow_na && length(value) == 1 && is.na(value)) {
    return(invisible(value))
  }

  ok <- is.numeric(value) &&
    length(value) == 1 &&
    !is.na(value) &&
    value >= 1 &&
    value == round(value)

  if (!ok) {
    abort(
      c(
        "{.arg {arg}} must be a single positive whole number of characters.",
        "x" = "You provided {.obj_type_friendly {value}}."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }

  invisible(value)
}
