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

formula2char <- function(fmla, call = rlang::caller_env()) {
  lhs_vars <- dag_term_names(fmla[[2]])
  lhs <- paste(maybe_quote_dagitty_name(lhs_vars, call = call), collapse = " ")
  if (length(lhs_vars) > 1) {
    lhs <- paste0("{", lhs, "}")
  }

  terms <- split_dag_terms(fmla[[3]])
  is_bidirected <- vapply(terms, \(term) term$bidirected, logical(1))
  directed <- unlist(lapply(terms[!is_bidirected], \(term) term$vars))
  bidirected <- unlist(lapply(terms[is_bidirected], \(term) term$vars))

  statements <- c(
    dag_statement(lhs, "<-", directed, call = call),
    dag_statement(lhs, "<->", bidirected, call = call)
  )

  #  a formula with nothing usable on the right still declares its own node
  if (length(statements) == 0) {
    return(lhs)
  }

  paste(statements, collapse = " ; ")
}

#' Assemble one `dagitty` statement, or nothing when there are no variables
#' @noRd
dag_statement <- function(lhs, arrow, vars, call = rlang::caller_env()) {
  if (length(vars) == 0) {
    return(character(0))
  }

  vars <- paste(maybe_quote_dagitty_name(vars, call = call), collapse = " ")
  paste0(lhs, " ", arrow, " {", vars, "}")
}

#' Split a formula's right-hand side into terms, marking bidirected ones
#'
#' R's parser records the intent of a mixed formula unambiguously: `y ~ x + ~z`
#' parses as `x + (~z)`, so only `z` is bidirected, while `y ~ ~x + z` parses as
#' `~(x + z)`, so both terms are. Splitting the AST rather than the deparsed
#' text keeps each term's arrow type. Parentheses are part of that record:
#' `y ~ x + (~z) + w` is the only single-formula spelling of "`z` bidirected but
#' `w` directed", so the recursion looks through `(` rather than treating a
#' parenthesized term as a leaf.
#'
#' @param expr The right-hand side of a formula.
#' @param bidirected Whether `expr` sits under a unary `~`.
#' @return A list of terms, each a list of `vars` and `bidirected`.
#' @noRd
split_dag_terms <- function(expr, bidirected = FALSE) {
  if (is.call(expr) && length(expr) == 3 && identical(expr[[1]], quote(`+`))) {
    return(c(
      split_dag_terms(expr[[2]], bidirected),
      split_dag_terms(expr[[3]], bidirected)
    ))
  }

  if (is.call(expr) && length(expr) == 2 && identical(expr[[1]], quote(`~`))) {
    return(split_dag_terms(expr[[2]], bidirected = TRUE))
  }

  if (is.call(expr) && length(expr) == 2 && identical(expr[[1]], quote(`(`))) {
    return(split_dag_terms(expr[[2]], bidirected))
  }

  list(list(vars = dag_term_names(expr), bidirected = bidirected))
}

#' The node names a formula term contributes
#'
#' `all.vars()` reaches variables inside calls, so a term such as
#' `curved(x, 0.5)` or a namespace-qualified call still yields its node, and
#' backticks are already stripped. Terms holding no variable at all, such as a
#' literal, fall back to their deparsed form.
#'
#' @param expr A formula term.
#' @return A character vector of node names.
#' @noRd
dag_term_names <- function(expr) {
  vars <- all.vars(expr)
  if (length(vars) > 0) {
    return(vars)
  }

  paste(deparse(expr), collapse = "")
}

#' Quote a node name only when `dagitty` cannot parse it bare
#'
#' Quoting every name would work too, but the unquoted spelling is what the
#' package has always produced and what its tests assert, so only names outside
#' dagitty's bareword class are quoted.
#'
#' @param x A character vector of node names.
#' @param call The calling environment, for the error message.
#' @return A character vector of node names, quoted where necessary.
#' @noRd
maybe_quote_dagitty_name <- function(x, call = rlang::caller_env()) {
  #  perl = TRUE so the ranges are code points rather than a locale's collation
  needs_quoting <- !grepl("^[0-9a-zA-Z_.]+$", x, perl = TRUE)
  x[needs_quoting] <- quote_dagitty_name(x[needs_quoting], call = call)
  x
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

#' Does a value name no variable at all?
#'
#' An `NA` names nothing, so an argument holding one is as unset as an empty
#' one, whatever else it holds.
#'
#' @param x A character vector, or `NULL`.
#' @return Logical.
#' @noRd
is_unset <- function(x) {
  is_empty_or_null(x) || anyNA(x)
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

ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

ggdag_left_join <- function(...) {
  dplyr::left_join(..., multiple = "all", relationship = "many-to-many")
}

`%nin%` <- Negate(`%in%`)

#' Every node mentioned by a DAG data frame
#'
#' Node-only rows (edges with `to = NA`) contribute their `name` alone, so
#' isolated nodes are included.
#'
#' @param .df A data frame with `name` and `to` columns.
#' @return A character vector of unique node names.
#' @noRd
all_node_names <- function(.df) {
  nodes <- c(as.character(.df$name), as.character(.df$to))
  unique(nodes[!is.na(nodes)])
}

#' Quote a node name for use in a `dagitty` string
#'
#' dagitty's unquoted identifiers are limited to `[0-9a-zA-Z_.]`, so names with
#' spaces or accents must be quoted. dagitty re-serializes the parsed DAG, so
#' quoting ordinary names does not change the stored string.
#'
#' Only the double quote is escaped. dagitty's string rule passes a backslash
#' escape through as its two literal characters rather than unescaping it, so
#' doubling backslashes here would corrupt every name that contains one.
#'
#' @param x A character vector of node names.
#' @param call The calling environment, for the error message.
#' @return A character vector of quoted node names.
#' @noRd
quote_dagitty_name <- function(x, call = rlang::caller_env()) {
  if (length(x) == 0) {
    return(character(0))
  }

  check_representable_names(x, call = call)

  paste0('"', gsub('"', '\\\\"', x), '"')
}

#' Reject node names dagitty's grammar cannot hold
#'
#' dagitty's lexer reads the backslash that ends a name as escaping the closing
#' quote, so a name ending in one runs off the end of the string whether it is
#' written raw or escaped. Escaping it is not an option either: dagitty keeps a
#' backslash escape as its two literal characters, so doubling the backslash
#' would rename the node.
#'
#' @param x A character vector of node names.
#' @param call The calling environment, for the error message.
#' @return `x`, invisibly.
#' @noRd
check_representable_names <- function(x, call = rlang::caller_env()) {
  ends_in_backslash <- grepl("\\\\$", x)
  if (!any(ends_in_backslash)) {
    return(invisible(x))
  }

  abort(
    c(
      "A node name can't end in a backslash.",
      "x" = "{.val {unique(x[ends_in_backslash])}} does.",
      "i" = "{.pkg dagitty} reads the last backslash of a name as escaping the
             closing quote, so such a name can't be written down."
    ),
    error_class = "ggdag_dag_error",
    call = call
  )
}

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
      paste0(what, "(use_stylized)")
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

#' Flatten a set of node names to a character vector
#'
#' Arguments such as `controlling_for` are documented as accepting a character
#' vector, a list of the form `list(c(...))`, or `NULL`. Both dagitty and
#' `validate_nodes_exist()` need a plain character vector: a list element
#' holding more than one name reaches them as a deparsed string, or makes an
#' `if` condition longer than one.
#'
#' An input naming no nodes at all, such as `list()` or `character(0)`, becomes
#' `NULL` so that callers can test for it with one condition and treat it the
#' same as an absent argument.
#'
#' @param nodes A character vector, a list of character vectors, or `NULL`.
#' @return A character vector of unique node names, or `NULL` if it names none.
#' @noRd
flatten_node_names <- function(nodes) {
  nodes <- unique(as.character(unlist(nodes, use.names = FALSE)))
  if (length(nodes) == 0) {
    return(NULL)
  }

  nodes
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

#' Reject dots that have nowhere to go
#'
#' Functions that forward `...` to `tidy_dagitty()` have nothing to forward it
#' to when the input is already a `tidy_dagitty`, so an argument passed there
#' would be discarded without a word.
#'
#' @param ... The dots to check.
#' @return `NULL`, invisibly.
#' @noRd
check_tidy_dots_empty <- function(..., call = rlang::caller_env()) {
  dots <- rlang::list2(...)
  if (rlang::is_empty(dots)) {
    return(invisible(NULL))
  }

  dot_names <- rlang::names2(dots)
  dot_names[!nzchar(dot_names)] <- "<unnamed>"

  abort(
    c(
      "{.arg ...} must be empty.",
      "x" = "Unused argument{?s}: {.field {dot_names}}",
      "i" = "{.arg ...} is passed to {.fun tidy_dagitty}, which is not called
             for an input that is already a {.cls tidy_dagitty}."
    ),
    error_class = "ggdag_dots_error",
    call = call
  )
}

#' Resolve the exposure and outcome a dagitty algorithm needs
#'
#' dagitty stops with a bare error when an algorithm that needs endpoints is
#' given none, which carries no ggdag class and names an internal call.
#' Resolving the endpoints first lets the caller raise the package's own error.
#'
#' @param .dag A `dagitty` object.
#' @param exposure,outcome A character vector, or `NULL` to take the value set
#'   on the DAG.
#' @return A list of `exposure` and `outcome`.
#' @noRd
resolve_endpoints <- function(
  .dag,
  exposure,
  outcome,
  call = rlang::caller_env()
) {
  if (is_empty_or_null(exposure)) {
    exposure <- dagitty::exposures(.dag)
  }
  if (is_empty_or_null(outcome)) {
    outcome <- dagitty::outcomes(.dag)
  }

  # `NA` names no variable, and dagitty reports it as one that is missing from
  # the DAG rather than as an endpoint that was never set
  if (is_unset(exposure) || is_unset(outcome)) {
    na_bullet <- if (anyNA(exposure) || anyNA(outcome)) {
      c("x" = "{.code NA} does not name a variable.")
    }

    abort(
      c(
        "Both {.arg exposure} and {.arg outcome} must be set.",
        na_bullet,
        "i" = "Set them in {.fun dagify} or {.fun dagitty::dagitty}.",
        "i" = "Or pass {.arg exposure} and {.arg outcome} directly."
      ),
      error_class = "ggdag_missing_error",
      call = call
    )
  }

  list(exposure = exposure, outcome = outcome)
}

#' Resolve endpoints for an algorithm defined on a single pair
#'
#' `dagitty::instrumentalVariables()` requires exactly one exposure and one
#' outcome, so a DAG carrying several of either reaches it as an error about
#' unset endpoints.
#'
#' @inheritParams resolve_endpoints
#' @return A list of `exposure` and `outcome`, each of length 1.
#' @noRd
resolve_single_endpoints <- function(
  .dag,
  exposure,
  outcome,
  call = rlang::caller_env()
) {
  endpoints <- resolve_endpoints(.dag, exposure, outcome, call = call)
  n_exposure <- length(endpoints$exposure)
  n_outcome <- length(endpoints$outcome)

  if (n_exposure != 1 || n_outcome != 1) {
    abort(
      c(
        "{.arg exposure} and {.arg outcome} must each be a single variable.",
        "x" = "{.arg exposure} names {n_exposure} variable{?s}; {.arg outcome} names {n_outcome}.",
        "i" = "Instrumental variables are defined for one exposure and one outcome."
      ),
      error_class = "ggdag_missing_error",
      call = call
    )
  }

  endpoints
}

#' Check if DAG is acyclic and warn if not
#' @noRd
check_acyclic <- function(.dag, call = rlang::caller_env()) {
  dag_obj <- pull_dag(.dag)

  if (!is_acyclic(dag_obj)) {
    cycle <- dagitty::findCycle(dag_obj)
    cycle_str <- paste(cycle, collapse = " -> ")

    warn(
      c(
        "Graph contains a cycle and is not a valid DAG.",
        "!" = "Cycle detected: {cycle_str}",
        "i" = "Causal diagram algorithms require acyclic graphs.",
        "i" = "Consider revising your DAG specification."
      ),
      warning_class = "ggdag_cyclic_warning",
      call = call
    )
  }
  invisible(NULL)
}
