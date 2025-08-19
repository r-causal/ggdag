#  variables used in various NSE calls
utils::globalVariables(
  c(
    ":=",
    "ggplot2::expansion"
  )
)

if_not_tidy_daggity <- function(.dagitty, ...) {
  if (!is.tidy_dagitty(.dagitty)) {
    return(tidy_dagitty(.dagitty, ...))
  }
  .dagitty
}

unique_pairs <- function(x, exclude_identical = TRUE) {
  pairs <- expand.grid(x, x) |> purrr::map_dfc(as.character)
  if (exclude_identical) {
    pairs <- pairs |> dplyr::filter(.data$Var1 != .data$Var2)
  }
  # Sort each pair and remove names to ensure proper comparison
  sorted_pairs <- apply(pairs, 1, \(row) sort(unname(row)), simplify = FALSE)
  pairs[!duplicated(sorted_pairs), ]
}

formula2char <- function(fmla) {
  #  using default to avoid `formula.tools::as.character.formula()`
  char_fmla <- as.character.default(fmla)
  rhs_vars <- char_fmla[[3]] |>
    stringr::str_split(" \\+ ") |>
    purrr::pluck(1)
  bidirectional <- any(stringr::str_detect(rhs_vars, "~"))
  rhs_vars <- stringr::str_replace_all(rhs_vars, "~", "")
  arrows <- ifelse(bidirectional, "<->", "<-")
  rhs_vars_coll <- paste0("{", paste(rhs_vars, collapse = " "), "}")
  paste(char_fmla[[2]], arrows, rhs_vars_coll)
}

edge_type_switch <- function(edge_type) {
  switch(
    edge_type,
    "link_arc" = geom_dag_edges,
    "link" = geom_dag_edges_link,
    "arc" = geom_dag_edges_arc,
    "diagonal" = geom_dag_edges_diagonal
  )
}

is_empty_or_null <- function(x) {
  is.null(x) || purrr::is_empty(x)
}

is_false <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && !x

has_exposure <- function(x) {
  is_false(purrr::is_empty(dagitty::exposures(pull_dag(x))))
}

has_outcome <- function(x) {
  is_false(purrr::is_empty(dagitty::outcomes(pull_dag(x))))
}

has_latent <- function(x) {
  is_false(purrr::is_empty(dagitty::latents(pull_dag(x))))
}

has_collider_path <- function(x) {
  x <- if_not_tidy_daggity(x)
  suppressWarnings(is_false(is.null(pull_dag_data(x)$collider_line)))
}

n_nodes <- function(x) {
  dplyr::n_distinct(pull_dag_data(x)$name)
}

n_edges <- function(x) {
  sum(!is.na(pull_dag_data(x)$direction)) - n_collider_paths(x)
}

n_collider_paths <- function(x) {
  if (has_collider_path(x)) {
    n <- sum(pull_dag_data(x)$collider_line)
  } else {
    n <- 0
  }
  n
}

collider_paths <- function(x) {
  if (has_collider_path(x)) {
    paths <- pull_dag_data(x) |>
      dplyr::filter(.data$collider_line) |>
      dplyr::mutate(collider_path_nodes = paste(.data$name, "<->", .data$to)) |>
      dplyr::pull(.data$collider_path_nodes)
  } else {
    paths <- c()
  }
  paths
}

#' @noRd
expansion <- function(...) {
  if (ggplot2_version() >= "3.3.0") {
    ggplot2::expansion(...)
  } else {
    ggplot2::expand_scale(...)
  }
}

#' @importFrom utils packageVersion
#' @noRd
ggplot2_version <- function() {
  utils::packageVersion("ggplot2")
}

#' @importFrom utils packageVersion
#' @noRd
dplyr_version <- function() {
  utils::packageVersion("dplyr")
}

ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

ggdag_left_join <- function(...) {
  if (dplyr_version() >= "1.1.1") {
    dplyr::left_join(..., multiple = "all", relationship = "many-to-many")
  } else if (dplyr_version() == "1.1.0") {
    dplyr::left_join(..., multiple = "all")
  } else {
    dplyr::left_join(...)
  }
}

`%nin%` <- Negate(`%in%`)

check_arg_node <- function(node, use_nodes, what = "geom_dag") {
  if (is_present(node)) {
    deprecate_soft("0.3.0", paste0(what, "(node)"), paste0(what, "(use_nodes)"))
    use_nodes <- node
  }

  use_nodes
}

check_arg_stylized <- function(stylized, use_stylized, what = "geom_dag") {
  if (is_present(stylized)) {
    deprecate_soft(
      "0.3.0",
      paste0(what, "(stylized)"),
      paste0(what, "(stylized)")
    )
    use_stylized <- stylized
  }

  use_stylized
}

ggraph_create_layout <- function(...) {
  .df <- suppressMessages(ggraph::create_layout(...))
  # ggdag doesn't need the igraph object
  attr(.df, "graph") <- NULL

  .df
}

# Custom CLI functions ----

#' Custom abort function with ggdag error class
#' @noRd
abort <- function(
  ...,
  error_class = NULL,
  call = rlang::caller_env(),
  .envir = parent.frame()
) {
  cli::cli_abort(
    ...,
    class = c(error_class, "ggdag_error"),
    call = call,
    .envir = .envir
  )
}

#' Custom warn function with ggdag warning class
#' @noRd
warn <- function(
  ...,
  warning_class = NULL,
  call = rlang::caller_env(),
  .envir = parent.frame()
) {
  cli::cli_warn(
    ...,
    class = c(warning_class, "ggdag_warning"),
    call = call,
    .envir = .envir
  )
}

#' Custom inform function for messages
#' @noRd
inform <- function(..., .envir = parent.frame()) {
  cli::cli_inform(
    ...,
    .envir = .envir
  )
}

# Assertion helpers ----

#' Assert input is a tidy_dagitty or dagitty object
#' @noRd
assert_dag_type <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is.tidy_dagitty(x) && !dagitty::is.dagitty(x)) {
    abort(
      c(
        "{.arg {arg}} must be a {.cls tidy_dagitty} or {.cls dagitty} object.",
        "i" = "You provided a {.cls {class(x)}} object."
      ),
      error_class = "ggdag_type_error",
      call = call
    )
  }
}

#' Assert required columns exist in data
#' @noRd
assert_columns_exist <- function(data, columns, call = rlang::caller_env()) {
  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols) > 0) {
    abort(
      c(
        "Required columns are missing from the data.",
        "x" = "Missing columns: {.field {missing_cols}}",
        "i" = "Available columns: {.field {names(data)}}"
      ),
      error_class = "ggdag_columns_error",
      call = call
    )
  }
}

#' Validate that nodes exist in DAG
#' @noRd
validate_nodes_exist <- function(
  .tdy_dag,
  nodes,
  arg = rlang::caller_arg(nodes),
  call = rlang::caller_env()
) {
  # Get all nodes in the DAG
  if (is.tidy_dagitty(.tdy_dag)) {
    all_nodes <- unique(pull_dag_data(.tdy_dag)$name)
  } else if (dagitty::is.dagitty(.tdy_dag)) {
    all_nodes <- names(.tdy_dag)
  } else {
    assert_dag_type(.tdy_dag, call = call)
  }

  # Check which nodes don't exist
  missing_nodes <- setdiff(nodes, all_nodes)
  if (length(missing_nodes) > 0) {
    abort(
      c(
        "{.arg {arg}} not found in DAG.",
        "x" = "Missing: {.val {missing_nodes}}",
        "i" = "Available nodes: {.val {all_nodes}}"
      ),
      error_class = "ggdag_missing_nodes_error",
      call = call
    )
  }

  invisible(TRUE)
}
