#' Query Functions for DAG Analysis
#'
#' These functions provide direct analytical answers about DAGs, returning
#' tibbles with results rather than tidy_dagitty objects for plotting.
#'
#' @name query_functions
#' @keywords internal
NULL

#' Query Adjustment Sets
#'
#' Find adjustment sets that close backdoor paths between exposure and outcome.
#' Unlike `dag_adjustment_sets()`, this function returns a tibble with the
#' adjustment sets as list columns rather than a tidy_dagitty object.
#'
#' @param .tdy_dag A tidy DAG object.
#' @param exposure A character vector of exposure variable names. If NULL,
#'   uses the exposure defined in the DAG.
#' @param outcome A character vector of outcome variable names. If NULL,
#'   uses the outcome defined in the DAG.
#' @param type Character string specifying the type of adjustment sets to find.
#'   Options are "minimal" (default), "canonical", or "all".
#' @param effect Character string specifying the effect type. Options are
#'   "total" (default) or "direct".
#' @param max.results Maximum number of adjustment sets to return. Default is Inf.
#'
#' @return A tibble with columns:
#'   - `set_id`: Integer identifier for each adjustment set
#'   - `type`: Type of adjustment set (minimal, canonical, or all)
#'   - `effect`: Effect type (total or direct)
#'   - `set`: String representation of the adjustment set (e.g., "\{a, b, c\}")
#'   - `variables`: List column containing the variables in each set
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ z,
#'   exposure = "x",
#'   outcome = "y"
#' )
#'
#' query_adjustment_sets(dag)
query_adjustment_sets <- function(
  .tdy_dag,
  exposure = NULL,
  outcome = NULL,
  type = c("minimal", "canonical", "all"),
  effect = c("total", "direct"),
  max.results = Inf
) {
  .validate_query_input(.tdy_dag)
  type <- match.arg(type)
  effect <- match.arg(effect)

  .dag <- pull_dag(.tdy_dag)

  # Get exposure and outcome from DAG if not provided
  if (is.null(exposure)) {
    exposure <- dagitty::exposures(.dag)
    if (length(exposure) == 0) {
      cli::cli_abort("No exposure variable found in DAG and none provided")
    }
  }

  if (is.null(outcome)) {
    outcome <- dagitty::outcomes(.dag)
    if (length(outcome) == 0) {
      cli::cli_abort("No outcome variable found in DAG and none provided")
    }
  }

  # Get adjustment sets
  adj_sets <- dagitty::adjustmentSets(
    .dag,
    exposure = exposure,
    outcome = outcome,
    effect = effect,
    type = type
  )

  # Convert to tibble
  if (length(adj_sets) == 0) {
    return(tibble::tibble(
      set_id = integer(),
      type = character(),
      effect = character(),
      set = character(),
      variables = list()
    ))
  }

  # Handle max.results
  if (length(adj_sets) > max.results) {
    adj_sets <- adj_sets[seq_len(max.results)]
  }

  # Convert sets to list of character vectors
  variables_list <- purrr::map(adj_sets, \(x) {
    if (length(x) == 0) {
      character()
    } else {
      as.character(x)
    }
  })

  tibble::tibble(
    set_id = seq_along(adj_sets),
    type = type,
    effect = effect,
    set = unname(purrr::map_chr(variables_list, create_set_string)),
    variables = variables_list
  )
}

#' Query Paths in a DAG
#'
#' Find all paths between specified nodes in a DAG and determine if they are
#' open or closed given a conditioning set.
#'
#' @param .tdy_dag A tidy DAG object.
#' @param from Character vector of starting nodes. If NULL, uses exposure from DAG.
#' @param to Character vector of ending nodes. If NULL, uses outcome from DAG.
#' @param directed Logical. If TRUE, only considers directed paths.
#' @param limit Maximum number of paths to return. Default is 100.
#' @param conditioned_on Character vector of variables to condition on.
#'
#' @return A tibble with columns:
#'   - `path_id`: Integer identifier for each path
#'   - `from`: Starting node
#'   - `to`: Ending node
#'   - `path`: Character string representation of the path
#'   - `path_type`: Character classification as "backdoor" or "direct"
#'   - `variables`: List column containing all variables in the path
#'   - `open`: Logical indicating if the path is open
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ w,
#'   z ~ w,
#'   exposure = "x",
#'   outcome = "y"
#' )
#'
#' query_paths(dag)
#' query_paths(dag, conditioned_on = "z")
query_paths <- function(
  .tdy_dag,
  from = NULL,
  to = NULL,
  directed = FALSE,
  limit = 100,
  conditioned_on = NULL
) {
  .validate_query_input(.tdy_dag)

  .dag <- pull_dag(.tdy_dag)

  # Get from and to from DAG if not provided
  if (is.null(from)) {
    from <- dagitty::exposures(.dag)
    if (length(from) == 0) {
      cli::cli_abort("No exposure variable found in DAG and no 'from' provided")
    }
  }

  if (is.null(to)) {
    to <- dagitty::outcomes(.dag)
    if (length(to) == 0) {
      cli::cli_abort("No outcome variable found in DAG and no 'to' provided")
    }
  }

  # Get paths
  paths_obj <- dagitty::paths(
    .dag,
    from = from,
    to = to,
    directed = directed,
    limit = limit,
    Z = conditioned_on
  )

  if (length(paths_obj$paths) == 0) {
    return(tibble::tibble(
      path_id = integer(),
      from = character(),
      to = character(),
      path = character(),
      path_type = character(),
      variables = list(),
      open = logical()
    ))
  }

  # Get directed paths to classify path types (if not already directed)
  if (!directed) {
    directed_paths_obj <- dagitty::paths(
      .dag,
      from = from,
      to = to,
      directed = TRUE,
      limit = limit,
      Z = conditioned_on
    )
    directed_paths <- directed_paths_obj$paths
  } else {
    # If already directed, all paths are direct
    directed_paths <- paths_obj$paths
  }

  # Extract variables from each path
  variables_list <- purrr::map(paths_obj$paths, \(path_str) {
    # Remove all arrow types and split on remaining spaces
    clean_path <- stringr::str_replace_all(path_str, " <->| ->| <-", " ")
    unique(stringr::str_trim(stringr::str_split(clean_path, "\\s+")[[1]]))
  })

  # Classify paths as backdoor or direct
  path_types <- ifelse(
    paths_obj$paths %in% directed_paths,
    "direct",
    "backdoor"
  )

  # Convert paths to tibble
  path_df <- purrr::imap(paths_obj$paths, \(path, idx) {
    tibble::tibble(
      path_id = idx,
      from = from,
      to = to,
      path = path,
      path_type = path_types[idx],
      variables = list(variables_list[[idx]]),
      open = paths_obj$open[idx]
    )
  }) |>
    purrr::list_rbind()

  path_df
}

#' Query Instrumental Variables
#'
#' Identify instrumental variables for a given exposure-outcome pair.
#'
#' @param .tdy_dag A tidy DAG object.
#' @param exposure Character vector of exposure variable names. If NULL,
#'   uses the exposure defined in the DAG.
#' @param outcome Character vector of outcome variable names. If NULL,
#'   uses the outcome defined in the DAG.
#' @param conditioned_on Character vector of variables that must be conditioned on.
#'
#' @return A tibble with columns:
#'   - `instrument`: The instrumental variable
#'   - `exposure`: The exposure variable
#'   - `outcome`: The outcome variable
#'   - `conditioning_set`: String representation of conditioning variables
#'   - `conditioned_on`: List column of required conditioning variables
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   y ~ x + u,
#'   x ~ z + u,
#'   exposure = "x",
#'   outcome = "y",
#'   latent = "u"
#' )
#'
#' query_instrumental(dag)
query_instrumental <- function(
  .tdy_dag,
  exposure = NULL,
  outcome = NULL,
  conditioned_on = NULL
) {
  .validate_query_input(.tdy_dag)

  .dag <- pull_dag(.tdy_dag)

  # Get exposure and outcome from DAG if not provided
  if (is.null(exposure)) {
    exposure <- dagitty::exposures(.dag)
    if (length(exposure) == 0) {
      cli::cli_abort("No exposure variable found in DAG and none provided")
    }
  }

  if (is.null(outcome)) {
    outcome <- dagitty::outcomes(.dag)
    if (length(outcome) == 0) {
      cli::cli_abort("No outcome variable found in DAG and none provided")
    }
  }

  # Get instrumental variables
  ivs <- dagitty::instrumentalVariables(
    .dag,
    exposure = exposure,
    outcome = outcome
  )

  if (length(ivs) == 0) {
    return(tibble::tibble(
      instrument = character(),
      exposure = character(),
      outcome = character(),
      conditioning_set = character(),
      conditioned_on = list()
    ))
  }

  # Convert to tibble
  purrr::map(seq_along(ivs), \(idx) {
    iv_info <- ivs[[idx]]
    cond_vars <- if (is.null(iv_info$Z) || length(iv_info$Z) == 0) {
      character()
    } else {
      iv_info$Z
    }

    tibble::tibble(
      instrument = iv_info$I,
      exposure = exposure,
      outcome = outcome,
      conditioning_set = create_set_string(cond_vars),
      conditioned_on = list(cond_vars)
    )
  }) |>
    purrr::list_rbind()
}

#' Query D-separation
#'
#' Test whether sets of variables are d-separated in a DAG given a conditioning set.
#'
#' @param .tdy_dag A tidy DAG object.
#' @param from Character vector of nodes or a list of node sets.
#' @param to Character vector of nodes or a list of node sets.
#' @param conditioned_on Character vector of conditioning variables.
#'
#' @return A tibble with columns:
#'   - `from_set`: String representation of source nodes
#'   - `from`: List column of source nodes
#'   - `to_set`: String representation of target nodes
#'   - `to`: List column of target nodes
#'   - `conditioning_set`: String representation of conditioning variables
#'   - `conditioned_on`: List column of conditioning variables
#'   - `dseparated`: Logical indicating d-separation
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ w,
#'   z ~ w
#' )
#'
#' query_dseparated(dag, from = "x", to = "z")
#' query_dseparated(dag, from = "x", to = "z", conditioned_on = "w")
query_dseparated <- function(
  .tdy_dag,
  from,
  to,
  conditioned_on = NULL
) {
  .validate_query_input(.tdy_dag)

  .dag <- pull_dag(.tdy_dag)

  # Ensure from and to are character vectors
  if (!is.character(from)) {
    cli::cli_abort("{.arg from} must be a character vector")
  }
  if (!is.character(to)) {
    cli::cli_abort("{.arg to} must be a character vector")
  }

  # Test d-separation
  is_dsep <- dagitty::dseparated(
    .dag,
    X = from,
    Y = to,
    Z = conditioned_on
  )

  # Handle conditioning set
  cond_vars <- if (is.null(conditioned_on) || length(conditioned_on) == 0) {
    character()
  } else {
    conditioned_on
  }

  tibble::tibble(
    from_set = create_set_string(from),
    from = list(from),
    to_set = create_set_string(to),
    to = list(to),
    conditioning_set = create_set_string(cond_vars),
    conditioned_on = list(cond_vars),
    dseparated = is_dsep
  )
}

#' Query D-connection
#'
#' Test whether sets of variables are d-connected in a DAG given a conditioning set.
#' This is the complement of d-separation.
#'
#' @inheritParams query_dseparated
#'
#' @return A tibble with columns:
#'   - `from_set`: String representation of source nodes
#'   - `from`: List column of source nodes
#'   - `to_set`: String representation of target nodes
#'   - `to`: List column of target nodes
#'   - `conditioning_set`: String representation of conditioning variables
#'   - `conditioned_on`: List column of conditioning variables
#'   - `dconnected`: Logical indicating d-connection
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ w,
#'   z ~ w
#' )
#'
#' query_dconnected(dag, from = "x", to = "z")
#' query_dconnected(dag, from = "x", to = "z", conditioned_on = "w")
query_dconnected <- function(
  .tdy_dag,
  from,
  to,
  conditioned_on = NULL
) {
  result <- query_dseparated(.tdy_dag, from, to, conditioned_on)
  result$dconnected <- !result$dseparated
  result$dseparated <- NULL
  result
}

#' Query Collider Nodes
#'
#' Identify all collider nodes in a DAG. A collider is a node with two or more
#' parents.
#'
#' @param .tdy_dag A tidy DAG object.
#'
#' @return A tibble with columns:
#'   - `node`: The collider node
#'   - `parent_set`: String representation of parent nodes
#'   - `parents`: List column containing the parent nodes
#'   - `is_activated`: Logical indicating if the collider is conditioned on
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   z ~ x + y,
#'   w ~ z
#' )
#'
#' query_colliders(dag)
query_colliders <- function(.tdy_dag) {
  .validate_query_input(.tdy_dag)

  .dag <- pull_dag(.tdy_dag)

  # Get all nodes
  all_nodes <- names(.dag)

  # Find colliders by checking parents
  collider_info <- purrr::map(all_nodes, \(node) {
    parents <- dagitty::parents(.dag, node)
    if (length(parents) >= 2) {
      tibble::tibble(
        node = node,
        parent_set = create_set_string(parents),
        parents = list(parents),
        is_activated = node %in% dagitty::adjustedNodes(.dag)
      )
    } else {
      NULL
    }
  }) |>
    purrr::list_rbind()

  if (is.null(collider_info) || nrow(collider_info) == 0) {
    return(tibble::tibble(
      node = character(),
      parent_set = character(),
      parents = list(),
      is_activated = logical()
    ))
  }

  collider_info
}

#' Query Exogenous Variables
#'
#' Identify exogenous (parentless) variables in a DAG.
#'
#' @param .tdy_dag A tidy DAG object.
#'
#' @return A tibble with columns:
#'   - `node`: The exogenous variable
#'   - `n_descendants`: Number of descendant nodes
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ w
#' )
#'
#' query_exogenous(dag)
query_exogenous <- function(.tdy_dag) {
  .validate_query_input(.tdy_dag)

  .dag <- pull_dag(.tdy_dag)

  # Get exogenous variables
  exo_vars <- dagitty::exogenousVariables(.dag)

  if (length(exo_vars) == 0) {
    return(tibble::tibble(
      node = character(),
      n_descendants = integer()
    ))
  }

  # Count descendants for each exogenous variable
  tibble::tibble(
    node = exo_vars,
    n_descendants = purrr::map_int(exo_vars, \(var) {
      descendants <- dagitty::descendants(.dag, var)
      length(setdiff(descendants, var))
    })
  )
}

#' Query Node Parents
#'
#' Find parent nodes for specified variables in a DAG.
#'
#' @param .tdy_dag A tidy DAG object.
#' @param .var Character vector of variables to query. If NULL, returns
#'   parents for all nodes.
#'
#' @return A tibble with columns:
#'   - `node`: The node
#'   - `parent_set`: String representation of parent nodes
#'   - `parents`: List column containing parent nodes
#'   - `n_parents`: Number of parents
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ w
#' )
#'
#' query_parents(dag)
#' query_parents(dag, .var = "y")
query_parents <- function(.tdy_dag, .var = NULL) {
  .validate_query_input(.tdy_dag)

  .dag <- pull_dag(.tdy_dag)

  # Get nodes to query
  nodes <- if (is.null(.var)) {
    names(.dag)
  } else {
    .var
  }

  # Get parents for each node
  purrr::map(nodes, \(node) {
    parents <- dagitty::parents(.dag, node)
    n_par <- length(parents)
    parent_vars <- if (n_par == 0) NA_character_ else parents

    tibble::tibble(
      node = node,
      parent_set = create_set_string(parent_vars),
      parents = list(parent_vars),
      n_parents = n_par
    )
  }) |>
    purrr::list_rbind()
}

#' Query Node Children
#'
#' Find child nodes for specified variables in a DAG.
#'
#' @inheritParams query_parents
#'
#' @return A tibble with columns:
#'   - `node`: The node
#'   - `child_set`: String representation of child nodes
#'   - `children`: List column containing child nodes
#'   - `n_children`: Number of children
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ w
#' )
#'
#' query_children(dag)
#' query_children(dag, .var = "x")
query_children <- function(.tdy_dag, .var = NULL) {
  .validate_query_input(.tdy_dag)

  .dag <- pull_dag(.tdy_dag)

  # Get nodes to query
  nodes <- if (is.null(.var)) {
    names(.dag)
  } else {
    .var
  }

  # Get children for each node
  purrr::map(nodes, \(node) {
    children <- dagitty::children(.dag, node)
    n_child <- length(children)
    child_vars <- if (n_child == 0) NA_character_ else children

    tibble::tibble(
      node = node,
      child_set = create_set_string(child_vars),
      children = list(child_vars),
      n_children = n_child
    )
  }) |>
    purrr::list_rbind()
}

#' Query Node Ancestors
#'
#' Find ancestor nodes for specified variables in a DAG.
#'
#' @inheritParams query_parents
#'
#' @return A tibble with columns:
#'   - `node`: The node
#'   - `ancestor_set`: String representation of ancestor nodes
#'   - `ancestors`: List column containing ancestor nodes
#'   - `n_ancestors`: Number of ancestors
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ w
#' )
#'
#' query_ancestors(dag)
#' query_ancestors(dag, .var = "y")
query_ancestors <- function(.tdy_dag, .var = NULL) {
  .validate_query_input(.tdy_dag)

  .dag <- pull_dag(.tdy_dag)

  # Get nodes to query
  nodes <- if (is.null(.var)) {
    names(.dag)
  } else {
    .var
  }

  # Get ancestors for each node
  purrr::map(nodes, \(node) {
    ancestors <- dagitty::ancestors(.dag, node)
    # Remove self from ancestors
    ancestors <- setdiff(ancestors, node)
    n_anc <- length(ancestors)
    ancestor_vars <- if (n_anc == 0) NA_character_ else ancestors

    tibble::tibble(
      node = node,
      ancestor_set = create_set_string(ancestor_vars),
      ancestors = list(ancestor_vars),
      n_ancestors = n_anc
    )
  }) |>
    purrr::list_rbind()
}

#' Query Node Descendants
#'
#' Find descendant nodes for specified variables in a DAG.
#'
#' @inheritParams query_parents
#'
#' @return A tibble with columns:
#'   - `node`: The node
#'   - `descendant_set`: String representation of descendant nodes
#'   - `descendants`: List column containing descendant nodes
#'   - `n_descendants`: Number of descendants
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ w
#' )
#'
#' query_descendants(dag)
#' query_descendants(dag, .var = "w")
query_descendants <- function(.tdy_dag, .var = NULL) {
  .validate_query_input(.tdy_dag)

  .dag <- pull_dag(.tdy_dag)

  # Get nodes to query
  nodes <- if (is.null(.var)) {
    names(.dag)
  } else {
    .var
  }

  # Get descendants for each node
  purrr::map(nodes, \(node) {
    descendants <- dagitty::descendants(.dag, node)
    # Remove self from descendants
    descendants <- setdiff(descendants, node)
    n_desc <- length(descendants)
    descendant_vars <- if (n_desc == 0) NA_character_ else descendants

    tibble::tibble(
      node = node,
      descendant_set = create_set_string(descendant_vars),
      descendants = list(descendant_vars),
      n_descendants = n_desc
    )
  }) |>
    purrr::list_rbind()
}

#' Query Markov Blanket
#'
#' Find the Markov blanket for specified variables in a DAG. The Markov blanket
#' includes parents, children, and parents of children (co-parents).
#'
#' @inheritParams query_parents
#'
#' @return A tibble with columns:
#'   - `node`: The node
#'   - `blanket`: String representation of Markov blanket nodes
#'   - `blanket_vars`: List column containing Markov blanket nodes
#'   - `blanket_size`: Size of the Markov blanket
#'
#' @export
#' @examples
#' library(ggdag)
#' dag <- dagify(
#'   y ~ x + z,
#'   x ~ w,
#'   z ~ w
#' )
#'
#' query_markov_blanket(dag)
#' query_markov_blanket(dag, .var = "x")
query_markov_blanket <- function(.tdy_dag, .var = NULL) {
  .validate_query_input(.tdy_dag)

  .dag <- pull_dag(.tdy_dag)

  # Get nodes to query
  nodes <- if (is.null(.var)) {
    names(.dag)
  } else {
    .var
  }

  # Get Markov blanket for each node
  purrr::map(nodes, \(node) {
    mb <- dagitty::markovBlanket(.dag, node)
    blanket_vars <- if (length(mb) == 0) NA_character_ else mb

    tibble::tibble(
      node = node,
      blanket = create_set_string(blanket_vars),
      blanket_vars = list(blanket_vars),
      blanket_size = length(mb)
    )
  }) |>
    purrr::list_rbind()
}

#' Internal helper functions
#' @keywords internal
.validate_query_input <- function(.tdy_dag, arg_name = ".tdy_dag") {
  assert_dag_type(.tdy_dag, arg = arg_name)
}

#' @keywords internal
.empty_set_as_list <- function() {
  list(NA_character_)
}

#' @keywords internal
create_set_string <- function(vars) {
  if (is.null(vars) || (length(vars) == 1 && is.na(vars))) {
    return(NA_character_)
  } else if (length(vars) == 0) {
    return("{}")
  } else {
    paste0("{", paste(sort(vars), collapse = ", "), "}")
  }
}

#' @keywords internal
.dagitty_set_to_tibble <- function(set_list, set_name = "variables") {
  if (length(set_list) == 0) {
    return(tibble::tibble(
      !!set_name := list()
    ))
  }

  tibble::tibble(
    !!set_name := purrr::map(set_list, \(x) {
      if (length(x) == 0) NA_character_ else as.character(x)
    })
  )
}
