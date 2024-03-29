% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjustment_sets.R
\name{Adjust for variables}
\alias{Adjust for variables}
\alias{control_for}
\alias{adjust_for}
\alias{ggdag_adjust}
\title{Adjust for variables and activate any biasing paths that result}
\usage{
control_for(.tdy_dag, var, as_factor = TRUE, activate_colliders = TRUE, ...)

adjust_for(.tdy_dag, var, as_factor = TRUE, activate_colliders = TRUE, ...)

ggdag_adjust(
  .tdy_dag,
  var = NULL,
  ...,
  size = 1,
  edge_type = c("link_arc", "link", "arc", "diagonal"),
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 10,
  arrow_length = 5,
  use_edges = TRUE,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  text = NULL,
  label = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
)
}
\arguments{
\item{.tdy_dag}{input graph, an object of class \code{tidy_dagitty} or
\code{dagitty}}

\item{var}{a character vector, the variable(s) to adjust for.}

\item{as_factor}{logical. Should the \code{adjusted} column be a factor?}

\item{activate_colliders}{logical. Include colliders activated by adjustment?}

\item{...}{additional arguments passed to \code{tidy_dagitty()}}

\item{size}{A numeric value scaling the size of all elements in the DAG. This
allows you to change the scale of the DAG without changing the proportions.}

\item{edge_type}{The type of edge, one of "link_arc", "link", "arc",
"diagonal".}

\item{node_size}{The size of the nodes.}

\item{text_size}{The size of the text.}

\item{label_size}{The size of the labels.}

\item{text_col}{The color of the text.}

\item{label_col}{The color of the labels.}

\item{edge_width}{The width of the edges.}

\item{edge_cap}{The size of edge caps (the distance between the arrowheads
and the node borders).}

\item{arrow_length}{The length of arrows on edges.}

\item{use_edges}{A logical value. Include a \verb{geom_dag_edges*()} function? If
\code{TRUE}, which is determined by \code{edge_type}.}

\item{use_nodes}{A logical value. Include \code{geom_dag_point()}?}

\item{use_stylized}{A logical value. Include \code{geom_dag_node()}?}

\item{use_text}{A logical value. Include \code{geom_dag_text()}?}

\item{use_labels}{A logical value. Include \code{geom_dag_label_repel()}?}

\item{text}{The bare name of a column to use for \code{geom_dag_text()}. If
\code{use_text = TRUE}, the default is to use \code{name}.}

\item{label}{The bare name of a column to use for \code{geom_dag_label_repel()}.
If \code{use_labels = TRUE}, the default is to use \code{label}.}

\item{node}{Deprecated.}

\item{stylized}{Deprecated.}

\item{collider_lines}{logical. Should the plot show paths activated by
adjusting for a collider?}
}
\value{
a \code{tidy_dagitty} with a \code{adjusted} column for adjusted
variables, as well as any biasing paths that arise, or a \code{ggplot}
}
\description{
Adjust for variables and activate any biasing paths that result
}
\examples{
dag <- dagify(m ~ a + b, x ~ a, y ~ b)

control_for(dag, var = "m")
ggdag_adjust(dag, var = "m")

}
