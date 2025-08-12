#' Quickly create a DAGs with common structures of bias
#'
#' base functions create an object of class `dagitty`; `ggdag_* ` functions
#' are wrappers that also call `ggdag()` on the `dagitty` object.
#'
#' The `quartet_*` functions create DAGs that represent the causal quartet,
#' which are four example datasets with identical statistical properties but
#' different causal structures. These are inspired by Anscombe's quartet and
#' demonstrate that statistical summaries alone cannot determine causal
#' relationships. See [Causal Inference in R](https://www.r-causal.org/chapters/05-not-just-a-stats-problem)
#'
#' The four structures represent different relationships between exposure (x),
#' outcome (y), and a covariate (z):
#' \itemize{
#'   \item Collider: z is caused by both x and y (should not adjust for z)
#'   \item Confounder: z causes both x and y (must adjust for z)
#'   \item Mediator: z is on the causal path from x to y (adjust for direct effect only)
#'   \item M-bias: z is a collider with unmeasured confounders u1 and u2 (should not adjust for z)
#' }
#'
#' The time-varying collider (`quartet_time_collider()`) demonstrates how
#' time-ordering can help identify causal relationships when variables are
#' measured at multiple time points.
#'
#' @param x,y,a,b,m,z Character vector. Optional label. Default is `NULL`
#' @param u1,u2 Character vector. Optional label for unmeasured nodes, used in `quartet_m_bias()`. Default is `NULL`
#' @param x0,x1,x2,x3,y1,y2,y3,z1,z2,z3 Character vector. Optional labels for time-indexed nodes, used in `quartet_time_collider()`. Default is `NULL`
#' @param x_y_associated Logical. Are x and y associated? Default is `FALSE`.
#' @inheritParams geom_dag
#'
#' @return a DAG of class `dagitty` or a `ggplot`
#'
#' @references
#' D'Agostino McGowan L, Gerke T, Barrett M (2023). "Causal inference is not just a
#' statistics problem." Journal of Statistics and Data Science Education, 32(1), 1-4.
#' \doi{10.1080/26939169.2023.2276446}
#'
#' @export
#'
#' @examples
#' m_bias() |> ggdag_adjust("m")
#' ggdag_confounder_triangle()
#'
#' # Causal Quartets
#' ggdag_quartet_collider()
#' ggdag_quartet_confounder()
#' ggdag_quartet_mediator()
#' ggdag_quartet_m_bias()
#'
#' # Time-varying collider
#' ggdag_quartet_time_collider()
#'
#' @rdname quick_plot
#' @name Quick Plots for Common DAGs

m_bias <- function(
  x = NULL,
  y = NULL,
  a = NULL,
  b = NULL,
  m = NULL,
  x_y_associated = FALSE
) {
  coords <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    0,
    0,
    "y",
    2,
    0,
    "a",
    0,
    1,
    "b",
    2,
    1,
    "m",
    1,
    .5
  )

  if (x_y_associated) {
    .dag <- dagify(
      m ~ a + b,
      x ~ a,
      y ~ b + x,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  } else {
    .dag <- dagify(
      m ~ a + b,
      x ~ a,
      y ~ b,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  }

  if (!is.null(c(x, y, a, b, m))) {
    label(.dag) <- c(x = x, y = y, a = a, b = b, m = m)
  }

  .dag
}

#' @rdname quick_plot
#' @export
butterfly_bias <- function(
  x = NULL,
  y = NULL,
  a = NULL,
  b = NULL,
  m = NULL,
  x_y_associated = FALSE
) {
  coords <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    0,
    0,
    "y",
    2,
    0,
    "a",
    0,
    1,
    "b",
    2,
    1,
    "m",
    1,
    .5
  )

  if (x_y_associated) {
    .dag <- dagify(
      m ~ a + b,
      x ~ a + m,
      y ~ b + x + m,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  } else {
    .dag <- dagify(
      m ~ a + b,
      x ~ a + m,
      y ~ b + m,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  }

  if (!is.null(c(x, y, a, b, m))) {
    label(.dag) <- c(x = x, y = y, a = a, b = b, m = m)
  }

  .dag
}

#' @rdname quick_plot
#' @export
confounder_triangle <- function(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = FALSE
) {
  coords <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    0,
    0,
    "y",
    2,
    0,
    "z",
    1,
    1
  )

  if (x_y_associated) {
    .dag <- dagify(
      x ~ z,
      y ~ x + z,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  } else {
    .dag <- dagify(
      x ~ z,
      y ~ z,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  }

  if (!is.null(c(x, y, z))) {
    label(.dag) <- c(x = x, y = y, z = z)
  }

  .dag
}

#' @rdname quick_plot
#' @export
collider_triangle <- function(
  x = NULL,
  y = NULL,
  m = NULL,
  x_y_associated = FALSE
) {
  coords <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    0,
    1,
    "y",
    2,
    1,
    "m",
    1,
    0
  )

  if (x_y_associated) {
    .dag <- dagify(
      m ~ x + y,
      y ~ x,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  } else {
    .dag <- dagify(
      m ~ x + y,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  }

  if (!is.null(c(x, y, m))) {
    label(.dag) <- c(x = x, y = y, m = m)
  }

  .dag
}

#' @rdname quick_plot
#' @export
mediation_triangle <- function(
  x = NULL,
  y = NULL,
  m = NULL,
  x_y_associated = FALSE
) {
  coords <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    0,
    0,
    "y",
    2,
    0,
    "m",
    1,
    1
  )

  if (x_y_associated) {
    .dag <- dagify(
      m ~ x,
      y ~ x + m,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  } else {
    .dag <- dagify(
      m ~ x,
      y ~ m,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  }

  if (!is.null(c(x, y, m))) {
    label(.dag) <- c(x = x, y = y, m = m)
  }

  .dag
}

#' @rdname quick_plot
#' @export
quartet_collider <- function(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = TRUE
) {
  coords <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    1,
    1,
    "y",
    2,
    1,
    "z",
    3,
    1.1
  )

  if (x_y_associated) {
    .dag <- dagify(
      z ~ x + y,
      y ~ x,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  } else {
    .dag <- dagify(
      z ~ x + y,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  }

  if (!is.null(c(x, y, z))) {
    label(.dag) <- c(x = x, y = y, z = z)
  }

  .dag
}

#' @rdname quick_plot
#' @export
quartet_confounder <- function(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = TRUE
) {
  coords <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    2,
    1,
    "y",
    3,
    1,
    "z",
    1,
    1.1
  )

  if (x_y_associated) {
    .dag <- dagify(
      x ~ z,
      y ~ x + z,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  } else {
    .dag <- dagify(
      x ~ z,
      y ~ z,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  }

  if (!is.null(c(x, y, z))) {
    label(.dag) <- c(x = x, y = y, z = z)
  }

  .dag
}

#' @rdname quick_plot
#' @export
quartet_mediator <- function(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = FALSE
) {
  coords <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    1,
    1,
    "y",
    3,
    1,
    "z",
    2,
    1.1
  )

  if (x_y_associated) {
    .dag <- dagify(
      z ~ x,
      y ~ z + x,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  } else {
    .dag <- dagify(
      z ~ x,
      y ~ z,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  }

  if (!is.null(c(x, y, z))) {
    label(.dag) <- c(x = x, y = y, z = z)
  }

  .dag
}

#' @rdname quick_plot
#' @export
quartet_m_bias <- function(
  x = NULL,
  y = NULL,
  z = NULL,
  u1 = NULL,
  u2 = NULL,
  x_y_associated = TRUE
) {
  coords <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x",
    3,
    1,
    "y",
    5,
    2,
    "z",
    3,
    2,
    "u1",
    1,
    2,
    "u2",
    2,
    4
  )

  .dag <- dagify(
    z ~ u1 + u2,
    x ~ u1,
    y ~ x + u2,
    exposure = "x",
    outcome = "y",
    coords = coords
  )

  if (!x_y_associated) {
    .dag <- dagify(
      z ~ u1 + u2,
      x ~ u1,
      y ~ u2,
      exposure = "x",
      outcome = "y",
      coords = coords
    )
  }

  if (!is.null(c(x, y, z, u1, u2))) {
    label(.dag) <- c(x = x, y = y, z = z, u1 = u1, u2 = u2)
  }

  .dag
}

#' @rdname quick_plot
#' @export
quartet_time_collider <- function(
  x0 = NULL,
  x1 = NULL,
  x2 = NULL,
  x3 = NULL,
  y1 = NULL,
  y2 = NULL,
  y3 = NULL,
  z1 = NULL,
  z2 = NULL,
  z3 = NULL
) {
  coords <- tibble::tribble(
    ~name,
    ~x,
    ~y,
    "x0",
    1,
    1,
    "x1",
    2,
    1,
    "z1",
    2,
    1.1,
    "y1",
    1.9,
    1.05,
    "x2",
    3,
    1,
    "y2",
    2.9,
    1.05,
    "z2",
    3,
    1.1,
    "x3",
    4,
    1,
    "y3",
    3.9,
    1.05,
    "z3",
    4,
    1.1
  )

  .dag <- dagify(
    y2 ~ x1,
    y3 ~ x2,
    x2 ~ x1,
    z2 ~ x1 + y2,
    z3 ~ x2 + y3 + z2,
    exposure = "x2",
    outcome = "y3",
    coords = coords
  )

  if (!is.null(c(x0, x1, x2, x3, y1, y2, y3, z1, z2, z3))) {
    label(.dag) <- c(
      x0 = x0,
      x1 = x1,
      x2 = x2,
      x3 = x3,
      y1 = y1,
      y2 = y2,
      y3 = y3,
      z1 = z1,
      z2 = z2,
      z3 = z3
    )
  }

  .dag
}

#' @rdname quick_plot
#' @export
ggdag_m_bias <- function(
  x = NULL,
  y = NULL,
  a = NULL,
  b = NULL,
  m = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  ggdag(
    m_bias(x, y, a, b, m, x_y_associated),
    node_size = node_size,
    text_size = text_size,
    label_size,
    edge_type = edge_type,
    text_col = text_col,
    label_col = label_col,
    use_edges = use_edges,
    use_nodes = use_nodes,
    use_stylized = use_stylized,
    use_text = use_text,
    use_labels = use_labels
  )
}

#' @rdname quick_plot
#' @export
ggdag_butterfly_bias <- function(
  x = NULL,
  y = NULL,
  a = NULL,
  b = NULL,
  m = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  ggdag(
    butterfly_bias(x, y, a, b, m, x_y_associated),
    node_size = node_size,
    text_size = text_size,
    label_size,
    edge_type = edge_type,
    text_col = text_col,
    label_col = label_col,
    use_edges = use_edges,
    use_nodes = use_nodes,
    use_stylized = use_stylized,
    use_text = use_text,
    use_labels = use_labels
  )
}

#' @rdname quick_plot
#' @export
ggdag_confounder_triangle <- function(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  ggdag(
    confounder_triangle(x, y, z, x_y_associated),
    node_size = node_size,
    text_size = text_size,
    label_size,
    edge_type = edge_type,
    text_col = text_col,
    label_col = label_col,
    use_edges = use_edges,
    use_nodes = use_nodes,
    use_stylized = use_stylized,
    use_text = use_text,
    use_labels = use_labels
  )
}

#' @rdname quick_plot
#' @export
ggdag_collider_triangle <- function(
  x = NULL,
  y = NULL,
  m = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  ggdag(
    collider_triangle(x, y, m, x_y_associated),
    node_size = node_size,
    text_size = text_size,
    label_size,
    edge_type = edge_type,
    text_col = text_col,
    label_col = label_col,
    use_edges = use_edges,
    use_nodes = use_nodes,
    use_stylized = use_stylized,
    use_text = use_text,
    use_labels = use_labels
  )
}

#' @rdname quick_plot
#' @export
ggdag_mediation_triangle <- function(
  x = NULL,
  y = NULL,
  m = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated()
) {
  ggdag(
    mediation_triangle(x, y, m, x_y_associated),
    node_size = node_size,
    text_size = text_size,
    label_size,
    edge_type = edge_type,
    text_col = text_col,
    label_col = label_col,
    use_edges = use_edges,
    use_nodes = use_nodes,
    use_stylized = use_stylized,
    use_text = use_text,
    use_labels = use_labels
  )
}

#' @rdname quick_plot
#' @export
ggdag_quartet_collider <- function(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = TRUE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL
) {
  ggdag(
    quartet_collider(x, y, z, x_y_associated),
    node_size = node_size,
    text_size = text_size,
    label_size,
    edge_type = edge_type,
    text_col = text_col,
    label_col = label_col,
    use_edges = use_edges,
    use_nodes = use_nodes,
    use_stylized = use_stylized,
    use_text = use_text,
    use_labels = use_labels
  )
}

#' @rdname quick_plot
#' @export
ggdag_quartet_confounder <- function(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = TRUE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL
) {
  ggdag(
    quartet_confounder(x, y, z, x_y_associated),
    node_size = node_size,
    text_size = text_size,
    label_size,
    edge_type = edge_type,
    text_col = text_col,
    label_col = label_col,
    use_edges = use_edges,
    use_nodes = use_nodes,
    use_stylized = use_stylized,
    use_text = use_text,
    use_labels = use_labels
  )
}

#' @rdname quick_plot
#' @export
ggdag_quartet_mediator <- function(
  x = NULL,
  y = NULL,
  z = NULL,
  x_y_associated = FALSE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL
) {
  ggdag(
    quartet_mediator(x, y, z, x_y_associated),
    node_size = node_size,
    text_size = text_size,
    label_size,
    edge_type = edge_type,
    text_col = text_col,
    label_col = label_col,
    use_edges = use_edges,
    use_nodes = use_nodes,
    use_stylized = use_stylized,
    use_text = use_text,
    use_labels = use_labels
  )
}

#' @rdname quick_plot
#' @export
ggdag_quartet_m_bias <- function(
  x = NULL,
  y = NULL,
  z = NULL,
  u1 = NULL,
  u2 = NULL,
  x_y_associated = TRUE,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL
) {
  ggdag(
    quartet_m_bias(x, y, z, u1, u2, x_y_associated),
    node_size = node_size,
    text_size = text_size,
    label_size,
    edge_type = edge_type,
    text_col = text_col,
    label_col = label_col,
    use_edges = use_edges,
    use_nodes = use_nodes,
    use_stylized = use_stylized,
    use_text = use_text,
    use_labels = use_labels
  )
}

#' @rdname quick_plot
#' @export
ggdag_quartet_time_collider <- function(
  x0 = NULL,
  x1 = NULL,
  x2 = NULL,
  x3 = NULL,
  y1 = NULL,
  y2 = NULL,
  y3 = NULL,
  z1 = NULL,
  z2 = NULL,
  z3 = NULL,
  size = 1,
  edge_type = "link_arc",
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 8,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL
) {
  ggdag(
    quartet_time_collider(x0, x1, x2, x3, y1, y2, y3, z1, z2, z3),
    node_size = node_size,
    text_size = text_size,
    label_size,
    edge_type = edge_type,
    text_col = text_col,
    label_col = label_col,
    use_edges = use_edges,
    use_nodes = use_nodes,
    use_stylized = use_stylized,
    use_text = use_text,
    use_labels = use_labels
  )
}
