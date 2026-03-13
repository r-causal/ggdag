#' Global DAG Options
#'
#' Set, get, and reset global default options for DAG appearance. These options
#' are used as defaults by all `geom_dag()`, `ggdag_*()`, and related functions.
#'
#' @details
#' Options are stored in R's global [options()] as `ggdag.<name>`. When an
#' option is `NULL` (the default), each function uses its own built-in
#' default. Setting a global option overrides the built-in default for all
#' functions that use it.
#'
#' Functions that normally use `edge_cap = 10` (e.g., [ggdag_adjustment_set()],
#' [ggdag_drelationship()]) maintain a proportional offset. If you set
#' `ggdag.edge_cap` to a custom value, these functions scale it by `10/8`.
#'
#' @param ... Named option values to set. See `ggdag_defaults` for valid names
#'   and types.
#' @param name Character string. The option name (without the `ggdag.` prefix).
#'   If `NULL`, returns all currently-set ggdag options.
#' @param default Default value to return if the option is not set.
#' @param base_default The base default for this option (e.g., 8 for edge_cap).
#' @param override_default The override default used by certain functions
#'   (e.g., 10 for edge_cap in adjustment set functions).
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
#'   `override_default`.
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
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label_geom = geom_dag_label_repel,
  edge_type = "link_arc",
  layout = "nicely"
)

#' @export
#' @rdname ggdag_options
ggdag_options_set <- function(...) {
  dots <- list(...)
  if (length(dots) == 0) {
    return(invisible(list()))
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

  for (nm in names(dots)) {
    validate_ggdag_option(nm, dots[[nm]])
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
ggdag_option <- function(name, default) {
  getOption(paste0("ggdag.", name), default = default)
}

#' @export
#' @rdname ggdag_options
ggdag_option_proportional <- function(name, base_default, override_default) {
  user_val <- getOption(paste0("ggdag.", name))
  if (is.null(user_val)) {
    return(override_default)
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
    "use_labels"
  )
  character_opts <- c("text_col", "label_col")
  valid_edge_types <- c("link_arc", "link", "arc", "diagonal")

  if (name %in% numeric_opts) {
    if (!is.numeric(value) || length(value) != 1 || value <= 0) {
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
    if (!is.logical(value) || length(value) != 1) {
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
    if (!is.character(value) || length(value) != 1) {
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
    if (is.character(value) && length(value) != 1) {
      abort(
        c(
          "{.arg layout} must be a single character string or a function.",
          "x" = "You provided {.obj_type_friendly {value}}."
        ),
        error_class = "ggdag_type_error",
        call = call
      )
    }
  }
}
