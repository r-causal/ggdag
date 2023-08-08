#' Familial relationships between variables
#'
#' Parents and children are those nodes that either directly cause or are caused
#' by the variable, respectively. Ancestors and descendants are those nodes that
#' are on the path to or descend from the variable. The `node_*()`
#' functions label variables depending on their relationship. The
#' `ggdag_*()` functions plot the results. See
#' [dagitty::children] for details.
#'
#' @param .tdy_dag input graph, an object of class `tidy_dagitty` or
#'   `dagitty`
#' @param edge_type a character vector, the edge geom to use. One of:
#'   "link_arc", which accounts for directed and bidirected edges, "link",
#'   "arc", or "diagonal"
#' @param ... additional arguments passed to `tidy_dagitty()`
#' @param .var a character vector, the variable to be assessed (must by in DAG)
#' @param node_size size of DAG node
#' @param text_size size of DAG text
#' @param label_size size of label text
#' @param text_col color of DAG text
#' @param label_col color of label text
#' @param node logical. Should nodes be included in the DAG?
#' @param stylized logical. Should DAG nodes be stylized? If so, use
#'   `geom_dag_nodes` and if not use `geom_dag_point`
#' @param text logical. Should text be included in the DAG?
#' @param use_labels a string. Variable to use for
#'   `geom_dag_label_repel()`. Default is `NULL`.
#' @param as_factor logical. Should the relationship variable be a factor?
#'
#' @return a `tidy_dagitty` with an column related to the given
#'   relationship for variable D relationship or a `ggplot`
#' @export
#'
#' @examples
#' library(ggplot2)
#' dag <- dagify(
#'   y ~ x + z2 + w2 + w1,
#'   x ~ z1 + w1,
#'   z1 ~ w1 + v,
#'   z2 ~ w2 + v,
#'   w1 ~ ~w2
#' )
#'
#' ggdag_children(dag, "w1")
#'
#' dag %>%
#'   node_children("w1") %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = children)) +
#'   geom_dag_edges() +
#'   geom_dag_node() +
#'   geom_dag_text(col = "white") +
#'   geom_dag_label_repel(aes(label = children, fill = children), col = "white", show.legend = FALSE) +
#'   theme_dag() +
#'   scale_adjusted() +
#'   scale_color_hue(breaks = c("parent", "child"))
#'
#' ggdag_parents(dag, "y")
#'
#' ggdag_ancestors(dag, "x")
#'
#' ggdag_descendants(dag, "w1")
#'
#' dag %>%
#'   node_parents("y") %>%
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = parent)) +
#'   geom_dag_edges() +
#'   geom_dag_point() +
#'   geom_dag_text(col = "white") +
#'   geom_dag_label_repel(aes(label = parent, fill = parent), col = "white", show.legend = FALSE) +
#'   theme_dag() +
#'   scale_adjusted() +
#'   scale_color_hue(breaks = c("parent", "child"))
#'
#' @rdname variable_family
#' @name Assess familial relationships between variables
node_children <- function(.tdy_dag, .var, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  .children <- dagitty::children(pull_dag(.tdy_dag), .var)
  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    children = dplyr::case_when(
      name %in% .children ~ "child",
      name == .var ~ "parent",
      TRUE ~ NA_character_
    )
  )
  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      children = factor(children, exclude = NA)
    )
  }

  .tdy_dag
}

#' @rdname variable_family
#' @export
node_parents <- function(.tdy_dag, .var, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  .parent <- dagitty::parents(pull_dag(.tdy_dag), .var)
  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    parent = dplyr::case_when(
      name %in% .parent ~ "parent",
      name == .var ~ "child",
      TRUE ~ NA
    )
  )
  if (as_factor) {
    .tdy_dag <- dplyr::mutate(.tdy_dag, parent = factor(parent, exclude = NA))
  }
  .tdy_dag
}

#' @rdname variable_family
#' @export
node_ancestors <- function(.tdy_dag, .var, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  .ancestors <- dagitty::ancestors(pull_dag(.tdy_dag), .var)[-1]
  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    ancestor = dplyr::case_when(
      name %in% .ancestors ~ "ancestor",
      name == .var ~ "descendant",
      TRUE ~ NA_character_
    )
  )
  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      ancestor = factor(ancestor, exclude = NA)
    )
  }
  .tdy_dag
}

#' @rdname variable_family
#' @export
node_descendants <- function(.tdy_dag, .var, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  .descendants <- dagitty::descendants(pull_dag(.tdy_dag), .var)[-1]
  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    descendant = dplyr::case_when(
      name %in% .descendants ~ "descendant",
      name == .var ~ "ancestor",
      TRUE ~ NA_character_
    )
  )
  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      descendant = factor(descendant, exclude = NA)
    )
  }

  .tdy_dag
}

#' @rdname variable_family
#' @export
node_markov_blanket <- function(.tdy_dag, .var, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  .blanket <- dagitty::markovBlanket(pull_dag(.tdy_dag), .var)
  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    blanket = dplyr::case_when(
      name %in% .blanket ~ "Markov blanket",
      name == .var ~ "center variable",
      TRUE ~ NA_character_
    )
  )

  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      blanket = factor(blanket, exclude = NA)
    )
  }

  .tdy_dag
}

#' @rdname variable_family
#' @export
node_adjacent <- function(.tdy_dag, .var, as_factor = TRUE) {
  .tdy_dag <- if_not_tidy_daggity(.tdy_dag)

  .adjacent <- dagitty::adjacentNodes(pull_dag(.tdy_dag), .var)
  .tdy_dag <- dplyr::mutate(
    .tdy_dag,
    adjacent = dplyr::case_when(
      name %in% .adjacent ~ "adjacent",
      name == .var ~ "center variable",
      TRUE ~ NA_character_
    )
  )
  if (as_factor) {
    .tdy_dag <- dplyr::mutate(
      .tdy_dag,
      adjacent = factor(adjacent, exclude = NA)
    )
  }

  .tdy_dag
}

#' @rdname variable_family
#' @export
ggdag_children <- function(.tdy_dag, .var, ..., edge_type = "link_arc",
                           node_size = 16, text_size = 3.88, label_size = text_size,
                           text_col = "white", label_col = text_col,
                           node = TRUE, stylized = FALSE, text = TRUE, use_labels = NULL) {
  edge_function <- edge_type_switch(edge_type)

  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_children(.var) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = children)) +
    edge_function() +
    scale_adjusted() +
    breaks(c("parent", "child"))

  if (node) {
    if (stylized) {
      p <- p + geom_dag_node(size = node_size)
    } else {
      p <- p + geom_dag_point(size = node_size)
    }
  }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) {
    p <- p +
      geom_dag_label_repel(
        ggplot2::aes(
          label = !!rlang::sym(use_labels),
          fill = children
        ),
        size = text_size,
        col = label_col,
        show.legend = FALSE
      )
  }
  p
}

#' @rdname variable_family
#' @export
ggdag_parents <- function(.tdy_dag, .var, ..., edge_type = "link_arc",
                          node_size = 16, text_size = 3.88, label_size = text_size,
                          text_col = "white", label_col = text_col,
                          node = TRUE, stylized = FALSE,
                          text = TRUE, use_labels = NULL) {
  edge_function <- edge_type_switch(edge_type)

  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_parents(.var) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = parent)) +
    edge_function() +
    scale_adjusted() +
    breaks(c("parent", "child"))

  if (node) {
    if (stylized) {
      p <- p + geom_dag_node(size = node_size)
    } else {
      p <- p + geom_dag_point(size = node_size)
    }
  }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) {
    p <- p +
      geom_dag_label_repel(
        ggplot2::aes(
          label = !!rlang::sym(use_labels),
          fill = parent
        ),
        size = text_size,
        col = label_col,
        show.legend = FALSE
      )
  }
  p
}

#' @rdname variable_family
#' @export
ggdag_ancestors <- function(.tdy_dag, .var, ..., edge_type = "link_arc",
                            node_size = 16, text_size = 3.88, label_size = text_size,
                            text_col = "white", label_col = text_col,
                            node = TRUE, stylized = FALSE, text = TRUE, use_labels = NULL) {
  edge_function <- edge_type_switch(edge_type)

  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_ancestors(.var) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = ancestor)) +
    edge_function() +
    scale_adjusted() +
    breaks(c("ancestor", "descendant"))

  if (node) {
    if (stylized) {
      p <- p + geom_dag_node(size = node_size)
    } else {
      p <- p + geom_dag_point(size = node_size)
    }
  }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) {
    p <- p +
      geom_dag_label_repel(
        ggplot2::aes(
          label = !!rlang::sym(use_labels),
          fill = ancestor
        ),
        size = text_size,
        col = label_col,
        show.legend = FALSE
      )
  }
  p
}

#' @rdname variable_family
#' @export
ggdag_descendants <- function(.tdy_dag, .var, ..., edge_type = "link_arc",
                              node_size = 16, text_size = 3.88, label_size = text_size,
                              text_col = "white", label_col = text_col,
                              node = TRUE, stylized = FALSE, text = TRUE, use_labels = NULL) {
  edge_function <- edge_type_switch(edge_type)

  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_descendants(.var) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = descendant)) +
    edge_function() +
    geom_dag_text(col = "white") +
    scale_adjusted() +
    breaks(c("ancestor", "descendant"))

  if (node) {
    if (stylized) {
      p <- p + geom_dag_node(size = node_size)
    } else {
      p <- p + geom_dag_point(size = node_size)
    }
  }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) {
    p <- p +
      geom_dag_label_repel(
        ggplot2::aes(
          label = !!rlang::sym(use_labels),
          fill = descendant
        ),
        size = text_size,
        col = label_col,
        show.legend = FALSE
      )
  }
  p
}

#' @rdname variable_family
#' @export
ggdag_markov_blanket <- function(.tdy_dag, .var, ..., edge_type = "link_arc",
                                 node_size = 16, text_size = 3.88, label_size = text_size,
                                 text_col = "white", label_col = text_col,
                                 node = TRUE, stylized = FALSE, text = TRUE, use_labels = NULL) {
  edge_function <- edge_type_switch(edge_type)

  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_markov_blanket(.var) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = blanket)) +
    edge_function() +
    geom_dag_text(col = "white") +
    scale_adjusted() +
    breaks(c("Markov blanket", "center variable"))

  if (node) {
    if (stylized) {
      p <- p + geom_dag_node(size = node_size)
    } else {
      p <- p + geom_dag_point(size = node_size)
    }
  }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) {
    p <- p +
      geom_dag_label_repel(
        ggplot2::aes(
          label = !!rlang::sym(use_labels),
          fill = blanket
        ),
        size = text_size,
        col = label_col,
        show.legend = FALSE
      )
  }
  p
}

#' @rdname variable_family
#' @export
ggdag_adjacent <- function(.tdy_dag, .var, ..., edge_type = "link_arc",
                           node_size = 16, text_size = 3.88, label_size = text_size,
                           text_col = "white", label_col = text_col,
                           node = TRUE, stylized = FALSE, text = TRUE, use_labels = NULL) {
  edge_function <- edge_type_switch(edge_type)

  p <- if_not_tidy_daggity(.tdy_dag, ...) %>%
    node_adjacent(.var) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = adjacent)) +
    edge_function() +
    geom_dag_text(col = "white") +
    scale_adjusted() +
    breaks(c("adjacent", "center variable"))

  if (node) {
    if (stylized) {
      p <- p + geom_dag_node(size = node_size)
    } else {
      p <- p + geom_dag_point(size = node_size)
    }
  }

  if (text) p <- p + geom_dag_text(col = text_col, size = text_size)

  if (!is.null(use_labels)) {
    p <- p +
      geom_dag_label_repel(
        ggplot2::aes(
          label = !!rlang::sym(use_labels),
          fill = adjacent
        ),
        size = text_size,
        col = label_col,
        show.legend = FALSE
      )
  }
  p
}
