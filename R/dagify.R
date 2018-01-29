#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dag <- function(...) {
  dag_string <- paste(..., sep = "; ")
  dagitty::dagitty(paste0("dag{", dag_string, "}"))
}

#' Title
#'
#' @param ...
#' @param exposure
#' @param outcome
#' @param latent
#' @param labels
#' @param coords
#'
#' @return
#' @export
#'
#' @examples
dagify <- function(..., exposure = NULL, outcome = NULL, latent = NULL, labels = NULL, coords = NULL) {
  fmlas <- list(...)
  dag_txt <- purrr::map_chr(fmlas, formula2char)
  dag_txt <- paste(dag_txt, collapse = "; ") %>%
    paste("dag {", ., "}")
  dgty <- dagitty::dagitty(dag_txt)
  if (!is.null(exposure)) dagitty::exposures(dgty) <- exposure
  if (!is.null(outcome)) dagitty::outcomes(dgty) <- outcome
  if (!is.null(latent)) dagitty::latents(dgty) <- latent
  if (!is.null(labels)) label(dgty) <- labels
  if (!is.null(coords)) {
    if (is.data.frame(coords)) {
      dagitty::coordinates(dgty) <- coords2list(coords)
    } else if (is.list(coords)) {
      dagitty::coordinates(dgty) <- coords
    } else {
      stop("`coords` must be of class `list` or `data.frame`")
    }
  }
  dgty
}
