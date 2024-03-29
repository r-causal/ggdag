% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull.R
\name{pull_dag}
\alias{pull_dag}
\alias{pull_dag.tidy_dagitty}
\alias{pull_dag.dagitty}
\alias{pull_dag_data}
\alias{pull_dag_data.tidy_dagitty}
\alias{pull_dag_data.dagitty}
\alias{update_dag_data<-}
\alias{update_dag_data<-.tidy_dagitty}
\alias{update_dag}
\alias{update_dag<-}
\alias{update_dag.tidy_dagitty}
\alias{update_dag<-.tidy_dagitty}
\title{Pull components from DAG objects}
\usage{
pull_dag(x, ...)

\method{pull_dag}{tidy_dagitty}(x, ...)

\method{pull_dag}{dagitty}(x, ...)

pull_dag_data(x, ...)

\method{pull_dag_data}{tidy_dagitty}(x, ...)

\method{pull_dag_data}{dagitty}(x, ...)

update_dag_data(x) <- value

\method{update_dag_data}{tidy_dagitty}(x) <- value

update_dag(x, ...)

update_dag(x) <- value

\method{update_dag}{tidy_dagitty}(x, ...)

\method{update_dag}{tidy_dagitty}(x) <- value
}
\arguments{
\item{x}{a \code{tidy_dagitty} or \code{dagitty} object.}

\item{...}{For \code{dagitty} objects, passed to \code{tidy_dagitty()} if needed,
otherwise currently unused.}

\item{value}{a value to set, either a \code{dagitty} or \code{data.frame} object,
depending on the function.}
}
\value{
a DAG object, e.g. \code{dagitty}, or data frame
}
\description{
\code{pull_dag()} and \code{pull_dag_data()} are generic methods to pull components of
DAG objects, e.g. \code{tidy_dagitty}, such as the \code{dagitty} object or the data
frame associated with it. These methods are recommended over extracting
components manually, e.g. \code{my_dag$data}, because the internal structure of
these objects may change over time. Similarly, use \code{update_dag()} if you want
to sync the data back to the DAG object or override it with another DAG; use
\code{update_dag_data()} to do update the data frame. This is useful with
\code{pull_dag_data()}.
}
\examples{

tidy_dagitty_obj <- dagify(y ~ x + z, x ~ z) \%>\%
  tidy_dagitty()
dag <- pull_dag(tidy_dagitty_obj)
dag_data <- pull_dag_data(tidy_dagitty_obj)

tidy_dagitty_obj \%>\%
  dplyr::mutate(name = toupper(name)) \%>\%
  # recreate the DAG component
  update_dag()

dag_data$label <- paste0(dag_data$name, "(observed)")
update_dag_data(tidy_dagitty_obj) <- dag_data

}
