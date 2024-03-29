% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RTI.R
\name{simple_rma}
\alias{simple_rma}
\title{A wrapper for \code{metafor::rma()} with some convenient defaults for my personal
use}
\usage{
simple_rma(x, error_var = 0.5, observed = "obs_score", time_var = NULL)
}
\arguments{
\item{x}{A data.frame, vector, or single value. If not a single value, will
be converted into difference scores.}

\item{error_var}{Variance of the error, not SD or Sdiff}

\item{observed}{Name of the variable used for observations in \code{x}. Must be a
character.}

\item{time_var}{Name of the variable use for time in \code{x}. Must be a
character. NOT FUNCTIONAL AT PRESENT, LEAVE NULL.}
}
\value{
A single object of class \code{rma}.
}
\description{
Not for external use. \code{time_var} not operational at present.
}
\examples{
\dontrun{
# simple entry:
simple_rma(15, 4.74^2)
simple_rma(c(47.5, 32.5), 4.74^2)
# Data.frame entry:
simple_rma(jt_data, error_var = 4.74^2, observed = "obs", time_var = "time")
}
}
