#' Test DAG properties
#'
#' @description
#' These functions test various properties of DAGs:
#' - `is_acyclic()` tests whether a DAG is acyclic
#' - `is_adjustment_set()` tests whether a set of variables is a valid adjustment set
#' - `is_d_separated()` tests whether two sets of variables are d-separated
#' - `is_d_connected()` tests whether two sets of variables are d-connected
#'
#' @inheritParams dag_params
#' @inheritParams path_params
#' @param Z A set of variables to test or condition on. This can be a character
#'   vector of variable names, a list of the form `list(c(...))`, or `NULL`.
#'
#' @return A logical value indicating whether the tested property holds
#'
#' @examples
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ z,
#'   exposure = "x",
#'   outcome = "y"
#' )
#'
#' is_acyclic(dag)
#' is_adjustment_set(dag, "z")
#' is_d_separated(dag, "x", "y", "z")
#' is_d_connected(dag, "x", "y")
#'
#' @name is_dag_properties
NULL

#' @rdname is_dag_properties
#' @export
is_acyclic <- function(.dag) {
  .dag <- pull_dag(.dag)
  dagitty::isAcyclic(.dag)
}

#' @rdname is_dag_properties
#' @export
is_adjustment_set <- function(.dag, Z, exposure = NULL, outcome = NULL) {
  .dag <- pull_dag(.dag)
  dagitty::isAdjustmentSet(.dag, Z, exposure = exposure, outcome = outcome)
}

# Helper function to handle from/to/controlling_for arguments
.prepare_d_separation_args <- function(.dag, from, to, controlling_for) {
  # Handle NULL values for from/to using exposure/outcome
  if (is.null(from)) {
    from <- dagitty::exposures(.dag)
  }
  if (is.null(to)) {
    to <- dagitty::outcomes(.dag)
  }

  if (is_empty_or_null(from) || is_empty_or_null(to)) {
    abort(
      c(
        "Both {.arg from} and {.arg to} must be set.",
        "i" = "Set {.arg from} to specify the starting variable.",
        "i" = "Set {.arg to} to specify the ending variable."
      ),
      error_class = "ggdag_missing_error"
    )
  }

  # Convert controlling_for to appropriate format for dagitty
  Z <- if (is.null(controlling_for)) {
    list()
  } else if (is.list(controlling_for)) {
    controlling_for
  } else {
    as.list(controlling_for)
  }

  list(from = from, to = to, Z = Z)
}

#' @rdname is_dag_properties
#' @export
is_d_separated <- function(
  .dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL
) {
  .dag <- pull_dag(.dag)
  args <- .prepare_d_separation_args(.dag, from, to, controlling_for)
  dagitty::dseparated(.dag, args$from, args$to, args$Z)
}

#' @rdname is_dag_properties
#' @export
is_d_connected <- function(
  .dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL
) {
  .dag <- pull_dag(.dag)
  args <- .prepare_d_separation_args(.dag, from, to, controlling_for)
  dagitty::dconnected(.dag, args$from, args$to, args$Z)
}

#' Test node properties
#'
#' @description
#' These functions test various properties of nodes in a DAG:
#' - `is_exogenous()` tests whether a variable is exogenous (has no parents)
#' - `is_instrumental()` tests whether a variable is instrumental
#' - `is_exposure()`, `is_outcome()`, `is_latent()` test variable status
#'
#' @inheritParams dag_params
#' @param .var A character string specifying the variable to test
#'
#' @return A logical value indicating whether the tested property holds
#'
#' @examples
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ z,
#'   exposure = "x",
#'   outcome = "y",
#'   latent = "z"
#' )
#'
#' is_exogenous(dag, "z")
#' is_exposure(dag, "x")
#' is_outcome(dag, "y")
#' is_latent(dag, "z")
#'
#' @name is_node_properties
NULL

#' @rdname is_node_properties
#' @export
is_exogenous <- function(.dag, .var) {
  .dag <- pull_dag(.dag)
  validate_nodes_exist(.dag, .var, arg = ".var")
  .var %in% dagitty::exogenousVariables(.dag)
}

#' @rdname is_node_properties
#' @export
is_instrumental <- function(.dag, .var, exposure = NULL, outcome = NULL) {
  .dag <- pull_dag(.dag)
  validate_nodes_exist(.dag, .var, arg = ".var")

  ivs <- dagitty::instrumentalVariables(
    .dag,
    exposure = exposure,
    outcome = outcome
  )
  if (length(ivs) == 0) {
    return(FALSE)
  }

  # instrumentalVariables returns a list with potentially complex structure
  # Check if .var appears anywhere in the results
  any(vapply(ivs, \(iv) .var %in% unlist(iv), logical(1)))
}

#' @rdname is_node_properties
#' @export
is_exposure <- function(.dag, .var) {
  .dag <- pull_dag(.dag)
  validate_nodes_exist(.dag, .var, arg = ".var")
  .var %in% dagitty::exposures(.dag)
}

#' @rdname is_node_properties
#' @export
is_outcome <- function(.dag, .var) {
  .dag <- pull_dag(.dag)
  validate_nodes_exist(.dag, .var, arg = ".var")
  .var %in% dagitty::outcomes(.dag)
}

#' @rdname is_node_properties
#' @export
is_latent <- function(.dag, .var) {
  .dag <- pull_dag(.dag)
  validate_nodes_exist(.dag, .var, arg = ".var")
  .var %in% dagitty::latents(.dag)
}

#' Test node relationships
#'
#' @description
#' These functions test relationships between nodes in a DAG:
#' - `is_parent()` tests whether one node is a parent of another
#' - `is_child()` tests whether one node is a child of another
#' - `is_ancestor()` tests whether one node is an ancestor of another
#' - `is_descendant()` tests whether one node is a descendant of another
#' - `is_adjacent()` tests whether two nodes are adjacent (connected by an edge)
#'
#' @inheritParams dag_params
#' @param .var A character string specifying the variable to test
#' @param .node A character string specifying the reference node
#'
#' @return A logical value indicating whether the relationship holds
#'
#' @examples
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ z
#' )
#'
#' is_parent(dag, "z", "x")
#' is_child(dag, "x", "z")
#' is_ancestor(dag, "z", "y")
#' is_descendant(dag, "y", "z")
#' is_adjacent(dag, "x", "y")
#'
#' @name is_node_relationships
NULL

#' @rdname is_node_relationships
#' @export
is_parent <- function(.dag, .var, .node) {
  .dag <- pull_dag(.dag)
  validate_nodes_exist(.dag, c(.var, .node), arg = c(".var", ".node"))
  .var %in% dagitty::parents(.dag, .node)
}

#' @rdname is_node_relationships
#' @export
is_child <- function(.dag, .var, .node) {
  .dag <- pull_dag(.dag)
  validate_nodes_exist(.dag, c(.var, .node), arg = c(".var", ".node"))
  .var %in% dagitty::children(.dag, .node)
}

#' @rdname is_node_relationships
#' @export
is_ancestor <- function(.dag, .var, .node) {
  .dag <- pull_dag(.dag)
  validate_nodes_exist(.dag, c(.var, .node), arg = c(".var", ".node"))
  ancestors <- dagitty::ancestors(.dag, .node)
  # ancestors includes the node itself, so we need to exclude it
  .var %in% setdiff(ancestors, .node)
}

#' @rdname is_node_relationships
#' @export
is_descendant <- function(.dag, .var, .node) {
  .dag <- pull_dag(.dag)
  validate_nodes_exist(.dag, c(.var, .node), arg = c(".var", ".node"))
  descendants <- dagitty::descendants(.dag, .node)
  # descendants includes the node itself, so we need to exclude it
  .var %in% setdiff(descendants, .node)
}

#' @rdname is_node_relationships
#' @export
is_adjacent <- function(.dag, .var, .node) {
  .dag <- pull_dag(.dag)
  validate_nodes_exist(.dag, c(.var, .node), arg = c(".var", ".node"))
  .var %in% dagitty::adjacentNodes(.dag, .node)
}
