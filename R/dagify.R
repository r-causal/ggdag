#' Create a dagitty DAG
#'
#' A convenience wrapper for `dagitty::dagitty()`.
#'
#' @param ... a character vector in the style of dagitty. See
#' \code{dagitty::\link[dagitty]{dagitty}} for details.
#'
#' @return a `dagitty`
#' @export
#'
#' @examples
#' dag("{x m} -> y")
#'
dag <- function(...) {
  #  `c()` flattens both call styles, so a single character vector collapses the
  #  same way several separate arguments do
  dag_string <- paste(c(...), collapse = "; ")
  dagitty::dagitty(paste0("dag{", dag_string, "}"))
}

# for internal use
dag2 <- dag

#' Create a dagitty DAG using R-like syntax
#'
#' `dagify()` creates dagitty DAGs using a more R-like syntax. It currently
#' accepts formulas in the usual R style, e.g. `y ~ x + z`, which gets
#' translated to `y <- {x z}`, as well as using a double tilde (`~~`) to
#' graph bidirected variables, e.g. `x1 ~~ x2` is translated to `x1
#' <-> x2`.
#'
#' A single formula can mix the two: `y ~ x + ~z` gives `x -> y` and `y <-> z`.
#' R's parser lets a unary `~` take in the rest of the right-hand side, so
#' every term after the tilde is bidirected: `y ~ x + ~z + w` gives `x -> y`,
#' `y <-> z`, and `y <-> w`, and `y ~ ~x + z` leaves both `x` and `z`
#' bidirected. Parentheses limit how far the tilde reaches, so
#' `y ~ x + (~z) + w` gives `x -> y`, `y <-> z`, and `w -> y`.
#'
#' A term that is a call contributes one edge for each variable it mentions, so
#' both `y ~ f(x, z)` and `y ~ x:z` give `x -> y` and `z -> y`.
#'
#' @param ... formulas, which are converted to `dagitty` syntax
#' @param exposure a character vector for the exposure (must be a variable name
#'   in the DAG)
#' @param outcome a character vector for the outcome (must be a variable name in
#'   the DAG)
#' @param latent a character vector for any latent variables (must be a variable
#'   name in the DAG)
#' @param labels a named character vector, labels for variables in the DAG
#' @param coords coordinates for the DAG nodes. Can be a named list or a
#'   `data.frame` with columns x, y, and name
#'
#' @return a `dagitty` DAG
#' @export
#'
#' @examples
#'
#' dagify(y ~ x + z, x ~ z)
#'
#' coords <- list(
#'   x = c(A = 1, B = 2, D = 3, C = 3, J = 3, E = 4, G = 5, H = 5, I = 5),
#'   y = c(A = 0, B = 0, D = 1, C = 0, J = -1, E = 0, G = 1, H = 0, I = -1)
#' )
#'
#' dag <- dagify(
#'   G ~ ~H,
#'   G ~ ~I,
#'   I ~ ~G,
#'   H ~ ~I,
#'   D ~ B,
#'   C ~ B,
#'   I ~ C + J,
#'   J ~ B,
#'   B ~ A,
#'   H ~ E,
#'   C ~ E + G,
#'   G ~ D,
#'   coords = coords
#' )
#'
#' dagitty::is.dagitty(dag)
#'
#' ggdag(dag)
#'
#' dag2 <- dagify(
#'   y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   w1 ~ ~w2,
#'   exposure = "x",
#'   outcome = "y"
#' )
#'
#' ggdag(dag2)
#'
#' @seealso [dag()], [coords2df()], [coords2list()]
dagify <- function(
  ...,
  exposure = NULL,
  outcome = NULL,
  latent = NULL,
  labels = NULL,
  coords = NULL
) {
  fmlas <- list(...)

  validate_dag_inputs(
    fmlas,
    exposure,
    outcome,
    latent,
    call = rlang::current_env()
  )

  has_any_curved <- any(vapply(
    fmlas,
    function(f) {
      is.call(f[[3]]) && length(find_curved_calls(f[[3]])) > 0
    },
    logical(1)
  ))

  if (has_any_curved) {
    curved_edges <- extract_curved_edges(fmlas)
    fmlas <- lapply(fmlas, strip_curved)
  } else {
    curved_edges <- tibble::tibble(
      name = character(),
      to = character(),
      edge_curvature = numeric()
    )
  }

  dag_txt <- purrr::map_chr(fmlas, formula2char)
  dag_txt <- paste(dag_txt, collapse = "; ") |>
    (\(x) paste("dag {", x, "}"))()
  dgty <- dagitty::dagitty(dag_txt)
  if (!is.null(exposure)) {
    dagitty::exposures(dgty) <- exposure
  }
  if (!is.null(outcome)) {
    dagitty::outcomes(dgty) <- outcome
  }
  if (!is.null(latent)) {
    dagitty::latents(dgty) <- latent
  }
  if (!is.null(coords)) {
    if (is.data.frame(coords)) {
      dagitty::coordinates(dgty) <- coords2list(coords)
    } else if (is.list(coords)) {
      dagitty::coordinates(dgty) <- coords
    } else if (is.function(coords)) {
      edge_df <- dgty |>
        get_dagitty_edges() |>
        edges2df()
      coord_result <- if ("..." %in% names(formals(coords))) {
        coords(
          edge_df,
          exposure = dagitty::exposures(dgty),
          outcome = dagitty::outcomes(dgty)
        )
      } else {
        coords(edge_df)
      }
      dagitty::coordinates(dgty) <- coords2list(coord_result)
    } else {
      abort(
        c(
          "{.arg coords} must be a named list, data.frame, or function.",
          "x" = "You provided a {.cls {class(coords)}} object."
        ),
        error_class = "ggdag_type_error"
      )
    }
  }
  if (!is.null(labels)) {
    label(dgty) <- labels
  }
  if (nrow(curved_edges) > 0) {
    attr(dgty, "curved_edges") <- curved_edges
  }
  dgty
}

validate_dag_formula_type <- function(fmla, call = rlang::caller_env()) {
  if (rlang::is_formula(fmla, lhs = TRUE)) {
    return(invisible(TRUE))
  }

  detail <- if (rlang::is_formula(fmla)) {
    "{.code {deparse(fmla)}} has no left-hand side."
  } else {
    "You provided {.obj_type_friendly {fmla}}."
  }

  abort(
    c(
      "Each argument to {.fun dagify} must be a two-sided formula.",
      "x" = detail,
      "i" = "For example: {.code dagify(y ~ x + z, x ~ z)}."
    ),
    error_class = "ggdag_type_error",
    call = call
  )
}

validate_dag_formula <- function(fmla, call = rlang::caller_env()) {
  vars <- all.vars(fmla, unique = FALSE)

  if (length(vars) >= 2) {
    lhs <- vars[1]
    rhs <- vars[-1]

    # Check for self-loops
    if (lhs %in% rhs) {
      abort(
        c(
          "Self-loops are not allowed in DAGs.",
          "x" = "Variable {.val {lhs}} cannot depend on itself.",
          "i" = "Remove the self-referencing formula."
        ),
        error_class = "ggdag_dag_error",
        call = call
      )
    }
  }

  invisible(TRUE)
}

validate_dag_inputs <- function(
  fmlas,
  exposure = NULL,
  outcome = NULL,
  latent = NULL,
  call = rlang::caller_env()
) {
  # Every argument must be a two-sided formula before anything indexes into it.
  # A plain loop keeps the condition itself at the top of the chain, rather than
  # under a purrr indexing wrapper.
  for (fmla in fmlas) {
    validate_dag_formula_type(fmla, call = call)
  }

  # Validate each formula
  purrr::walk(fmlas, \(f) validate_dag_formula(f, call = call))

  # Check that exposure and outcome are different
  if (!is.null(exposure) && !is.null(outcome)) {
    if (any(exposure %in% outcome)) {
      abort(
        c(
          "A variable cannot be both exposure and outcome.",
          "x" = "Found: {.val {intersect(exposure, outcome)}}"
        ),
        error_class = "ggdag_dag_error"
      )
    }
  }

  # Check that latent variables aren't also exposure or outcome
  if (!is.null(latent)) {
    if (!is.null(exposure) && any(latent %in% exposure)) {
      abort(
        c(
          "Latent variables cannot also be exposures.",
          "x" = "Found: {.val {intersect(latent, exposure)}}"
        ),
        error_class = "ggdag_dag_error"
      )
    }
    if (!is.null(outcome) && any(latent %in% outcome)) {
      abort(
        c(
          "Latent variables cannot also be outcomes.",
          "x" = "Found: {.val {intersect(latent, outcome)}}"
        ),
        error_class = "ggdag_dag_error"
      )
    }
  }

  # Collect all variables mentioned in formulas
  all_vars_in_dag <- unique(unlist(purrr::map(fmlas, ~ all.vars(.x))))

  # Validate that exposure, outcome, and latent are in the DAG
  if (!is.null(exposure) && !all(exposure %in% all_vars_in_dag)) {
    missing <- setdiff(exposure, all_vars_in_dag)
    abort(
      c(
        "Exposure variable(s) not found in DAG.",
        "x" = "Missing: {.val {missing}}",
        "i" = "Available variables: {.val {all_vars_in_dag}}"
      ),
      error_class = "ggdag_missing_error"
    )
  }

  if (!is.null(outcome) && !all(outcome %in% all_vars_in_dag)) {
    missing <- setdiff(outcome, all_vars_in_dag)
    abort(
      c(
        "Outcome variable(s) not found in DAG.",
        "x" = "Missing: {.val {missing}}",
        "i" = "Available variables: {.val {all_vars_in_dag}}"
      ),
      error_class = "ggdag_missing_error"
    )
  }

  if (!is.null(latent) && !all(latent %in% all_vars_in_dag)) {
    missing <- setdiff(latent, all_vars_in_dag)
    abort(
      c(
        "Latent variable(s) not found in DAG.",
        "x" = "Missing: {.val {missing}}",
        "i" = "Available variables: {.val {all_vars_in_dag}}"
      ),
      error_class = "ggdag_missing_error"
    )
  }

  invisible(TRUE)
}

#' Mark an edge as curved in dagify formulas
#'
#' Use `curved()` inside [dagify()] formulas to specify per-edge curvature.
#' This function should only be used inside `dagify()` formulas; calling it
#' directly will result in an error, similar to [dplyr::n()].
#'
#' @param var A variable name (unquoted) representing the parent node.
#' @param curvature A numeric curvature value. Positive values curve edges
#'   in one direction, negative in the other. Default is `0.3`.
#'
#' @section Curvature sign convention:
#' The curvature value is passed directly to the active edge rendering engine.
#' The **ggraph** engine (default) and **ggarrow** engine interpret the sign
#' differently:
#'
#' - **ggraph**: positive curvature curves *above* (to the left of) a
#'   left-to-right edge.
#' - **ggarrow** / **grid**: positive curvature curves *below* (to the right
#'   of) a left-to-right edge, following `grid::curveGrob()` convention.
#'
#' This means the same `curvature` value will render as a mirror image
#' depending on the engine. ggdag does not negate or transform the value;
#' each engine uses its native convention.
#'
#' @return This function is not intended to be called directly. It is detected
#'   in the formula AST by [dagify()].
#'
#' @examples
#' # Curve the edge from m to y
#' dagify(
#'   y ~ x + curved(m, 0.5),
#'   m ~ x
#' )
#'
#' @export
curved <- function(var, curvature = 0.3) {
  abort(
    c(
      "{.fun curved} can only be used inside {.fun dagify} formulas.",
      "i" = 'Example: {.code dagify(y ~ x + curved(m, 0.5))}'
    ),
    error_class = "ggdag_error"
  )
}

#' Add or update curvature for a single edge
#'
#' `curve_edge()` sets the curvature for a single edge on a `dagitty` or
#' `tidy_dagitty` object. Use [set_curve_edges()] to set multiple edges at once.
#'
#' @param .dag A `dagitty` or `tidy_dagitty` object.
#' @param from Character. The name of the source node.
#' @param to Character. The name of the target node.
#' @param curvature Numeric. The curvature value for the edge.
#'
#' @inheritSection curved Curvature sign convention
#'
#' @return The modified `.dag` object with updated curvature.
#'
#' @examples
#' dag <- dagify(y ~ x + m, m ~ x)
#' dag <- curve_edge(dag, from = "m", to = "y", curvature = 0.5)
#'
#' @export
curve_edge <- function(.dag, from, to, curvature = 0.3) {
  UseMethod("curve_edge")
}

#' @export
curve_edge.dagitty <- function(.dag, from, to, curvature = 0.3) {
  node_names <- names(.dag)
  if (!from %in% node_names) {
    abort(
      c(
        "{.arg from} must be a valid node name.",
        "x" = "{.val {from}} is not in the DAG."
      ),
      error_class = "ggdag_dag_error"
    )
  }
  if (!to %in% node_names) {
    abort(
      c(
        "{.arg to} must be a valid node name.",
        "x" = "{.val {to}} is not in the DAG."
      ),
      error_class = "ggdag_dag_error"
    )
  }
  validate_edges_exist(.dag, from, to)

  # Curvature is kept in an attribute of its own rather than written to the
  # DAG as a dagitty edge control point. Besides being a relative measure
  # rather than an absolute coordinate, a control point at x = 0 does not
  # survive dagitty's DOT writer (see `ctrl_point_to_curvature()` in
  # R/tidy_dag.R), so a curve set here would silently straighten.
  curved_edges <- attr(.dag, "curved_edges") %||%
    tibble::tibble(
      name = character(),
      to = character(),
      edge_curvature = numeric()
    )

  existing <- curved_edges$name == from & curved_edges$to == to
  if (any(existing)) {
    curved_edges$edge_curvature[existing] <- curvature
  } else {
    curved_edges <- dplyr::bind_rows(
      curved_edges,
      tibble::tibble(name = from, to = to, edge_curvature = curvature)
    )
  }

  attr(.dag, "curved_edges") <- curved_edges
  .dag
}

#' @export
curve_edge.tidy_dagitty <- function(.dag, from, to, curvature = 0.3) {
  dag <- pull_dag(.dag)
  dag <- curve_edge.dagitty(dag, from = from, to = to, curvature = curvature)
  update_dag(.dag) <- dag

  dag_data <- pull_dag_data(.dag)
  if ("edge_curvature" %nin% names(dag_data)) {
    dag_data$edge_curvature <- NA_real_
  }
  edge_match <- dag_data$name == from & dag_data$to == to & !is.na(dag_data$to)
  dag_data$edge_curvature[edge_match] <- curvature
  # Non-curved edges should be 0 when any curvature is set
  edge_rows <- !is.na(dag_data$to)
  dag_data$edge_curvature[edge_rows & is.na(dag_data$edge_curvature)] <- 0
  update_dag_data(.dag) <- dag_data

  .dag
}

#' Set curvature for multiple edges at once
#'
#' `set_curve_edges()` replaces all edge curvatures on a `dagitty` or
#' `tidy_dagitty` object from a data frame. Use [curve_edge()] to set a
#' single edge.
#'
#' @param .dag A `dagitty` or `tidy_dagitty` object.
#' @param edges A data frame with columns `from`, `to`, and `curvature`.
#'
#' @inheritSection curved Curvature sign convention
#'
#' @return The modified `.dag` object with updated curvatures.
#'
#' @examples
#' dag <- dagify(y ~ x + m, m ~ x)
#' edges <- data.frame(
#'   from = c("x", "m"),
#'   to = c("y", "y"),
#'   curvature = c(0.3, -0.4)
#' )
#' dag <- set_curve_edges(dag, edges)
#'
#' @export
set_curve_edges <- function(.dag, edges) {
  UseMethod("set_curve_edges")
}

#' @export
set_curve_edges.dagitty <- function(.dag, edges) {
  required_cols <- c("from", "to", "curvature")
  missing_cols <- setdiff(required_cols, names(edges))
  if (length(missing_cols) > 0) {
    abort(
      c(
        "{.arg edges} must have columns {.val {required_cols}}.",
        "x" = "Missing: {.val {missing_cols}}."
      ),
      error_class = "ggdag_type_error"
    )
  }

  node_names <- names(.dag)
  invalid_from <- setdiff(edges$from, node_names)
  if (length(invalid_from) > 0) {
    abort(
      c(
        "Invalid node names in {.arg from}.",
        "x" = "{.val {invalid_from}} not in the DAG."
      ),
      error_class = "ggdag_dag_error"
    )
  }
  invalid_to <- setdiff(edges$to, node_names)
  if (length(invalid_to) > 0) {
    abort(
      c(
        "Invalid node names in {.arg to}.",
        "x" = "{.val {invalid_to}} not in the DAG."
      ),
      error_class = "ggdag_dag_error"
    )
  }
  validate_edges_exist(.dag, edges$from, edges$to)

  curved_edges <- tibble::tibble(
    name = edges$from,
    to = edges$to,
    edge_curvature = edges$curvature
  )

  attr(.dag, "curved_edges") <- curved_edges
  .dag
}

#' @export
set_curve_edges.tidy_dagitty <- function(.dag, edges) {
  dag <- pull_dag(.dag)
  dag <- set_curve_edges.dagitty(dag, edges)
  update_dag(.dag) <- dag

  curved_edges <- attr(dag, "curved_edges")
  dag_data <- pull_dag_data(.dag)

  # Remove existing edge_curvature and re-join
  dag_data$edge_curvature <- NULL
  dag_data <- dplyr::left_join(
    dag_data,
    curved_edges[, c("name", "to", "edge_curvature")],
    by = c("name", "to")
  )
  # Non-curved edges should be 0
  edge_rows <- !is.na(dag_data$to)
  dag_data$edge_curvature[edge_rows & is.na(dag_data$edge_curvature)] <- 0
  update_dag_data(.dag) <- dag_data

  .dag
}

#' Check that every requested edge is actually in the DAG
#'
#' Curvature is stored per edge, so a pair of node names that names no edge
#' would set the curvature of nothing while still flattening every other edge to
#' zero curvature. Bidirected and undirected edges have no direction, so either
#' orientation of one names the same edge.
#'
#' @param .dag A `dagitty` object.
#' @param from,to Character vectors of node names, paired element by element.
#' @param call The calling environment, for the error message.
#' @return `TRUE`, invisibly.
#' @noRd
validate_edges_exist <- function(.dag, from, to, call = rlang::caller_env()) {
  .edges <- dagitty::edges(.dag)

  edge_exists <- function(i) {
    if (nrow(.edges) == 0) {
      return(FALSE)
    }

    directed <- .edges$e == "->" & .edges$v == from[i] & .edges$w == to[i]
    undirected <- .edges$e %in%
      c("<->", "--") &
      ((.edges$v == from[i] & .edges$w == to[i]) |
        (.edges$v == to[i] & .edges$w == from[i]))

    any(directed | undirected)
  }

  found <- vapply(seq_along(from), edge_exists, logical(1))
  if (all(found)) {
    return(invisible(TRUE))
  }

  missing_edges <- paste(from[!found], "->", to[!found])
  abort(
    c(
      "{length(missing_edges)} edge{?s} not found in the DAG.",
      "x" = "Missing: {.val {missing_edges}}",
      "i" = "Did you swap {.arg from} and {.arg to}?"
    ),
    error_class = "ggdag_dag_error",
    call = call
  )
}

#' Extract curved edge specifications from formula list
#'
#' Walks the AST of each formula to find `curved()` calls and extracts
#' the variable name and curvature value.
#'
#' @param fmlas A list of formulas from `dagify()`.
#' @return A tibble with columns `name`, `to`, and `edge_curvature`.
#'   Empty tibble if no `curved()` calls are found.
#' @noRd
extract_curved_edges <- function(fmlas) {
  rows <- list()
  for (fmla in fmlas) {
    lhs <- as.character.default(fmla)[[2]]
    rhs <- fmla[[3]]
    curved_calls <- find_curved_calls(rhs)
    for (cc in curved_calls) {
      rows <- c(
        rows,
        list(tibble::tibble(
          name = cc$var,
          to = lhs,
          edge_curvature = cc$curvature
        ))
      )
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      name = character(),
      to = character(),
      edge_curvature = numeric()
    ))
  }

  dplyr::bind_rows(rows)
}

#' Name the function a call invokes
#'
#' A call head is not always a symbol: `ggdag::curved(m, 0.5)` has a `::` call
#' as its head, and `as.character()` on that returns three elements, which is a
#' hard error in `if ()`. Namespace-qualified `curved()` names the same
#' function, so it resolves to `"curved"`; any other non-symbol head resolves to
#' the empty string, which matches nothing.
#'
#' @param expr A call.
#' @return A length-one character vector.
#' @noRd
call_fn_name <- function(expr) {
  fn <- expr[[1]]

  if (is.name(fn)) {
    return(as.character(fn))
  }

  is_ggdag_qualified <- is.call(fn) &&
    length(fn) == 3 &&
    identical(fn[[1]], quote(`::`)) &&
    identical(fn[[2]], quote(ggdag)) &&
    is.name(fn[[3]])

  if (is_ggdag_qualified) {
    return(as.character(fn[[3]]))
  }

  ""
}

#' Recursively find curved() calls in a formula expression
#' @noRd
find_curved_calls <- function(expr) {
  if (!is.call(expr)) {
    return(list())
  }

  fn_name <- call_fn_name(expr)

  if (fn_name == "curved") {
    var_name <- as.character(expr[[2]])
    curvature <- if (length(expr) >= 3) {
      raw <- expr[[3]]
      val <- if (is.numeric(raw)) {
        raw
      } else if (
        is.call(raw) &&
          identical(raw[[1]], as.name("-")) &&
          length(raw) == 2 &&
          is.numeric(raw[[2]])
      ) {
        -raw[[2]]
      } else {
        abort(
          c(
            "{.arg curvature} in {.fn curved} must be a numeric literal.",
            "i" = "Example: {.code curved(x, 0.5)} or {.code curved(x, -0.3)}"
          ),
          error_class = "ggdag_type_error"
        )
      }
      val
    } else {
      0.3
    }
    return(list(list(var = var_name, curvature = curvature)))
  }

  # Recurse into sub-expressions (e.g., `+`, `~`)
  results <- list()
  for (i in seq_along(expr)[-1]) {
    results <- c(results, find_curved_calls(expr[[i]]))
  }
  results
}

#' Strip curved() wrappers from a formula
#'
#' Recursively walks the formula AST and replaces `curved(x, ...)` with `x`,
#' producing a clean formula for `formula2char()`.
#'
#' @param fmla A formula that may contain `curved()` calls.
#' @return The formula with `curved()` wrappers removed.
#' @noRd
strip_curved <- function(fmla) {
  fmla[[3]] <- strip_curved_expr(fmla[[3]])
  fmla
}

#' Recursively strip curved() from an expression
#' @noRd
strip_curved_expr <- function(expr) {
  if (!is.call(expr)) {
    return(expr)
  }

  fn_name <- call_fn_name(expr)
  if (fn_name == "curved") {
    return(expr[[2]])
  }

  # Recurse into sub-expressions
  for (i in seq_along(expr)[-1]) {
    expr[[i]] <- strip_curved_expr(expr[[i]])
  }
  expr
}

get_dagitty_edges <- function(.dag) {
  # `edge_ctrl_x` is never exactly 0: dagitty drops an edge control point at
  # that coordinate when it writes the DAG out (see `ctrl_point_to_curvature()`
  # in R/tidy_dag.R).
  .edges <- dagitty::edges(.dag)

  # Handle empty edges (DAG with no edges)
  if (nrow(.edges) == 0 || ncol(.edges) == 0) {
    return(tibble::tibble(
      name = character(),
      to = character(),
      direction = character(),
      edge_ctrl_x = numeric(),
      edge_ctrl_y = numeric()
    ))
  }

  .edges |>
    dplyr::rename(
      name = "v",
      to = "w",
      direction = "e",
      edge_ctrl_x = "x",
      edge_ctrl_y = "y"
    )
}

edges2df <- function(.edges) {
  # a DAG with no edges at all can arrive with an all-`NA` logical `to` column,
  # which would otherwise make the node-only rows below logical as well
  .to <- as.character(.edges$to)
  no_outgoing_edges <- unique(.to[!(.to %in% .edges$name)])
  no_outgoing_edges <- no_outgoing_edges[!is.na(no_outgoing_edges)]
  dplyr::bind_rows(
    .edges,
    tibble::tibble(
      name = no_outgoing_edges,
      to = rep(NA_character_, length(no_outgoing_edges))
    )
  )
}

#' Add node-only rows for nodes that take part in no edge
#'
#' The time-ordering pipeline works from an edge list, which never mentions
#' isolated nodes. Adding them as `to = NA` rows is how the rest of the pipeline
#' already represents nodes without outgoing edges.
#'
#' @param .edges_df A data frame with `name` and `to` columns.
#' @param .nodes A character vector of every node in the DAG.
#' @return `.edges_df`, with a row added for each node it did not mention.
#' @noRd
add_isolated_nodes <- function(.edges_df, .nodes) {
  isolated <- setdiff(.nodes, all_node_names(.edges_df))
  if (length(isolated) == 0) {
    return(.edges_df)
  }

  dplyr::bind_rows(
    .edges_df,
    tibble::tibble(name = isolated, to = rep(NA_character_, length(isolated)))
  )
}
