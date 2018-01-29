% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/status.R
\name{Variable Status}
\alias{Variable Status}
\alias{node_status}
\alias{ggdag_status}
\title{Find variable status}
\usage{
node_status(.tdy_dag, as_factor = TRUE, ...)

ggdag_status(.tdy_dag, ...)
}
\arguments{
\item{as_factor}{treat \code{status} variable as factor}

\item{...}{additional arguments passed to \code{tidy_dagitty()}}

\item{.dag, .tdy_dag}{input graph, an object of class \code{tidy_dagitty} or
\code{dagitty}}
}
\value{
a \code{tidy_dagitty} with an \code{status} column for
  variable status or a \code{ggplot}
}
\description{
Detects variable status given a DAG (exposure, outcome, latent). See
\code{dagitty::\link[dagitty]{VariableStatus}} for details.
}
\details{
\code{node_collider} tags variable status and \code{ggdag_collider} plots all
variable statuses.
}
\examples{
dag <- dagify(l ~ x + y,
  y ~ x,
  exposure = "x",
  outcome = "y",
  latent = "l")

node_status(dag)
ggdag_status(dag)

}