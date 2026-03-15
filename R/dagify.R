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
  dag_string <- paste(..., sep = "; ")
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
#'   x = c(A = 1, B = 2, D = 3, C = 3, F = 3, E = 4, G = 5, H = 5, I = 5),
#'   y = c(A = 0, B = 0, D = 1, C = 0, F = -1, E = 0, G = 1, H = 0, I = -1)
#' )
#'
#' dag <- dagify(
#'   G ~ ~H,
#'   G ~ ~I,
#'   I ~ ~G,
#'   H ~ ~I,
#'   D ~ B,
#'   C ~ B,
#'   I ~ C + F,
#'   F ~ B,
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
#' This function should only be used inside `dagify()` formulas — calling it
#' directly will result in an error, similar to [dplyr::n()].
#'
#' @param var A variable name (unquoted) representing the parent node.
#' @param curvature A numeric curvature value. Positive values curve edges
#'   in one direction, negative in the other. Default is `0.3`.
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

#' Recursively find curved() calls in a formula expression
#' @noRd
find_curved_calls <- function(expr) {
  if (!is.call(expr)) {
    return(list())
  }

  fn_name <- as.character(expr[[1]])

  if (fn_name == "curved") {
    var_name <- as.character(expr[[2]])
    curvature <- if (length(expr) >= 3) eval(expr[[3]]) else 0.3
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

  fn_name <- as.character(expr[[1]])
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
  .edges <- dagitty::edges(.dag)

  # Handle empty edges (DAG with no edges)
  if (nrow(.edges) == 0 || ncol(.edges) == 0) {
    # Return empty tibble with expected columns
    return(tibble::tibble(
      name = character(),
      to = character(),
      direction = character()
    ))
  }

  .edges |>
    dplyr::select(-"x", -"y") |>
    dplyr::rename(name = "v", to = "w", direction = "e")
}

edges2df <- function(.edges) {
  no_outgoing_edges <- unique(.edges$to[!(.edges$to %in% .edges$name)])
  dplyr::bind_rows(
    .edges,
    tibble::tibble(
      name = no_outgoing_edges,
      to = rep(NA_character_, length(no_outgoing_edges))
    )
  )
}
