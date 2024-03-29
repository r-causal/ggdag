% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drelationship.R
\name{Assess d-separation between variables}
\alias{Assess d-separation between variables}
\alias{node_dconnected}
\alias{node_dseparated}
\alias{node_drelationship}
\alias{ggdag_drelationship}
\alias{ggdag_dseparated}
\alias{ggdag_dconnected}
\title{D-relationship between variables}
\usage{
node_dconnected(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  as_factor = TRUE,
  ...
)

node_dseparated(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  as_factor = TRUE
)

node_drelationship(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  as_factor = TRUE
)

ggdag_drelationship(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  ...,
  edge_type = "link_arc",
  size = 1,
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
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
)

ggdag_dseparated(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  ...,
  edge_type = "link_arc",
  size = 1,
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 10,
  arrow_length = 5,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
)

ggdag_dconnected(
  .tdy_dag,
  from = NULL,
  to = NULL,
  controlling_for = NULL,
  ...,
  edge_type = "link_arc",
  size = 1,
  node_size = 16,
  text_size = 3.88,
  label_size = text_size,
  text_col = "white",
  label_col = "black",
  edge_width = 0.6,
  edge_cap = 10,
  arrow_length = 5,
  use_nodes = TRUE,
  use_stylized = FALSE,
  use_text = TRUE,
  use_labels = FALSE,
  label = NULL,
  text = NULL,
  node = deprecated(),
  stylized = deprecated(),
  collider_lines = TRUE
)
}
\arguments{
\item{.tdy_dag}{input graph, an object of class \code{tidy_dagitty} or
\code{dagitty}}

\item{from}{a character vector, the starting variable (must by in DAG). If
\code{NULL}, checks DAG for exposure variable.}

\item{to}{a character vector, the ending variable (must by in DAG). If
\code{NULL}, checks DAG for outcome variable.}

\item{controlling_for}{a character vector, variables in the DAG to control
for.}

\item{as_factor}{logical. Should the \code{d_relationship} variable be a
factor?}

\item{...}{additional arguments passed to \code{tidy_dagitty()}}

\item{edge_type}{The type of edge, one of "link_arc", "link", "arc",
"diagonal".}

\item{size}{A numeric value scaling the size of all elements in the DAG. This
allows you to change the scale of the DAG without changing the proportions.}

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

\item{label}{The bare name of a column to use for \code{geom_dag_label_repel()}.
If \code{use_labels = TRUE}, the default is to use \code{label}.}

\item{text}{The bare name of a column to use for \code{geom_dag_text()}. If
\code{use_text = TRUE}, the default is to use \code{name}.}

\item{node}{Deprecated.}

\item{stylized}{Deprecated.}

\item{collider_lines}{logical. Should the plot show paths activated by
adjusting for a collider?}
}
\value{
a \code{tidy_dagitty} with a \code{d_relationship} column for
variable D relationship or a \code{ggplot}
}
\description{
D-separation is a key concept in causal structural models. Variables are
d-separated if there are no open paths between them. The \verb{node_d*()}
functions label variables as d-connected or d-separated. The
\verb{ggdag_d*()} functions plot the results. The \verb{*_dconnected()},
\verb{*_dseparated()}, and \verb{*_drelationship()} functions essentially
produce the same output and are just different ways of thinking about the
relationship. See \code{\link[dagitty:dconnected]{dagitty::dseparated()}} for details.
}
\examples{
library(ggplot2)
dag <- dagify(m ~ x + y)
dag \%>\% ggdag_drelationship("x", "y")
dag \%>\% ggdag_drelationship("x", "y", controlling_for = "m")

dag \%>\%
  node_dseparated("x", "y") \%>\%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
  geom_dag_edges() +
  geom_dag_collider_edges() +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() +
  scale_adjusted()

dag \%>\%
  node_dconnected("x", "y", controlling_for = "m") \%>\%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
  geom_dag_edges() +
  geom_dag_collider_edges() +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() +
  scale_adjusted()

dagify(m ~ x + y, m_jr ~ m) \%>\%
  tidy_dagitty(layout = "nicely") \%>\%
  node_dconnected("x", "y", controlling_for = "m_jr") \%>\%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, shape = adjusted, col = d_relationship)) +
  geom_dag_edges() +
  geom_dag_collider_edges() +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() +
  scale_adjusted()
}
