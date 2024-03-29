% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reliableTrend_class.R
\name{rti_to_df}
\alias{rti_to_df}
\title{Convert a \code{reliableTrend} object to a data.frame via rci_to_stripped_list().}
\usage{
rti_to_df(x)
}
\arguments{
\item{x}{An object of class \code{reliableTrend}.}
}
\value{
A \code{data.frame} with 1 row and 12 columns.
}
\description{
Given an object of class \code{reliableTrend}, returns a single-row data.frame.
Loses all non-atomic values of the \code{reliableTrend} object.
}
\examples{
output <- rti_calc_simple(c(98,98,98,99,99,99), .7071068^2)
output
output2 <- reliableTrend(output$rmaObj)
output2
rti_to_df(output2)
}
