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
      dagitty::coordinates(dgty) <- dgty |>
        get_dagitty_edges() |>
        edges2df() |>
        coords() |>
        coords2list()
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
      to = rep(NA, length(no_outgoing_edges))
    )
  )
}
