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
#' dag <- dagify(G ~ ~H,
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
#' dag2 <- dagify(y ~ x + z2 + w2 + w1,
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
dagify <- function(..., exposure = NULL, outcome = NULL, latent = NULL, labels = NULL, coords = NULL) {
  fmlas <- list(...)
  dag_txt <- purrr::map_chr(fmlas, formula2char)
  dag_txt <- paste(dag_txt, collapse = "; ") %>%
    paste("dag {", ., "}")
  dgty <- dagitty::dagitty(dag_txt)
  if (!is.null(exposure)) dagitty::exposures(dgty) <- exposure
  if (!is.null(outcome)) dagitty::outcomes(dgty) <- outcome
  if (!is.null(latent)) dagitty::latents(dgty) <- latent
  if (!is.null(coords)) {
    if (is.data.frame(coords)) {
      dagitty::coordinates(dgty) <- coords2list(coords)
    } else if (is.list(coords)) {
      dagitty::coordinates(dgty) <- coords
    } else if (is.function(coords)) {
      dagitty::coordinates(dgty) <- dgty %>%
        get_dagitty_edges() %>%
        edges2df() %>%
        coords() %>%
        coords2list()
    } else {
      stop("`coords` must be of class `list`, `data.frame`, or `function`")
    }
  }
  if (!is.null(labels)) label(dgty) <- labels
  dgty
}

get_dagitty_edges <- function(.dag) {
  .edges <- dagitty::edges(.dag)
  .edges %>%
    dplyr::select(-x, -y) %>%
    dplyr::rename(name = v, to = w, direction = e)
}

edges2df <- function(.edges) {
  no_outgoing_edges <- unique(.edges$to[!(.edges$to %in% .edges$name)])
  dplyr::bind_rows(
    .edges,
    data.frame(
      name = no_outgoing_edges,
      to = rep(NA, length(no_outgoing_edges))
    )
  )
}
